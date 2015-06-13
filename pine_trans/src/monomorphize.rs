use pine_typecheck::typed_ast::{TypedCompilationUnit, TypedFunction, TypedIdentifier};
use pine_typecheck::types::{Types, self, Type};
use pine_typecheck::TypedVisitor;

use std::collections::HashMap;
use std::hash::{self, SipHasher};

struct Monomorphizer<'ast> {
    monomorphized_functions: HashMap<String, TypedFunction>,
    environment: HashMap<String, &'ast TypedFunction>,
}

// take a list of functions and return a list of monomorphized
// functions. the returned list of functions will have no type
// variables and their names may be mangled if the same function
// was monomorphized multiple times.
pub fn monomorphize(entry_point: usize, asts: &TypedCompilationUnit) -> TypedCompilationUnit {
    // starting at the entry point, perform a depth-first traversal
    // of all functions that are called. For every generic function
    // that is called, instantiate all of its type parameters
    // clone its AST, mangle the name, and add it to the list of
    // functions being compiled.
    let mut monomorphizer = Monomorphizer {
        monomorphized_functions: HashMap::new(),
        environment: HashMap::new(),
    };
    // first - we build our type environment. This provides
    // the monomorphization process with the knowledge of what
    // type a function has.
    for func in asts {
        monomorphizer.environment.insert(func.name.clone(), func);
    }

    // next - begin monomorphization at the entry point, since it's
    // guaranteed to not have any type parameters
    info!(target: "monomorphization",
          "beginning monomorphization pass");
    let mut entry_point = asts[entry_point].clone();
    info!(target: "monomorphization",
          "beginning to monomorphize function: `{}`",
          entry_point.name);
    monomorphizer.visit_function(&mut entry_point);

    monomorphizer.monomorphized_functions.insert(entry_point.name.clone(), entry_point);
    // finally - take the monomorphized functions and return
    // a vector of them.
    monomorphizer.monomorphized_functions
        .into_iter()
        .map(|(_, v)| v)
        .collect()
}

impl<'ast> TypedVisitor for Monomorphizer<'ast> {
    fn visit_function(&mut self,
                      func: &mut TypedFunction) {
        // just assert that we're not trying to monomorphize something with type variables in it
        if !func.free_type_variables().is_empty() {
            panic!("function `{}` is being monomorphized but it has free type variables: `{:?}`",
                  func.name,
                  func.free_type_variables());
        }
        self.visit_expression(&mut func.body);
    }

    fn visit_identifier(&mut self,
                        ident: &mut TypedIdentifier) {
        // for every identifier, we need to inspect whether or not
        // it refers to a function. If it does, and that function
        // is generic, we need to monomorphize that function.
        info!(target: "monomorphization",
             "inspecting ident `{}` \
              for possible monomorphization",
             ident.data);
        let mut cloned_func = if let Some(func) = self.environment.get(&ident.data) {
            (*func).clone()
        } else {
            // nothing to do here if this identifier isn't a function.
            return;
        };

        info!(target: "monomorphization",
              "identifier `{}` references a function",
              ident.data);
        if !cloned_func.free_type_variables().is_empty() {
            info!(target: "monomorphization",
                  "function `{}` needs to be monomorphized",
                  ident.data);
            // this function needs to be monomorphized.
            // we need to get a type variable substitution,
            // apply it to the function, and figure out
            // if this function has been monomorphized already.

            // TODO - this is pretty inefficient, since we have
            // to actually /do/ some of the monomorphization before
            // we figure out whether or not it's actually necessary.
            let fun_ty = Type::Function(cloned_func.parameter_types.clone(),
                                        Box::new(cloned_func.return_type.clone()));
            info!(target: "monomorphization",
                  "function being invoked type: `{}`",
                  fun_ty);
            info!(target: "monomorphization",
                  "identifier type: `{}`",
                  ident.ty);
            let subst = types::unify(&fun_ty, &ident.ty)
                .ok()
                .expect("this expression passed type resolution \
                         yet is invalid in trans?");
            // now that we have the substitution, apply it
            // to a clone of the function AST.
            cloned_func.apply_subst(&subst);
            info!(target: "monomorphization",
                  "using substitution `{:?}` to monomorphize \
                   function `{}`",
                  subst,
                  ident.data);
            // next, mangle the function name.
            mangle_function_name(&mut cloned_func);
            // check and see if this function has been monomorphized
            // already.
            // either way, we need to rewrite this call site to reference the monomorphized function.
            info!(target: "monomorphization",
                  "rewriting invocation site `{}` to `{}`",
                  ident.data, cloned_func.name);
            ident.data = cloned_func.name.clone();
            if !self.monomorphized_functions.contains_key(&cloned_func.name) {
                info!(target: "monomorphization",
                      "function `{}` has not yet been monomorphized",
                      cloned_func.name);
                // if it hasn't been monomorphized already, do it.
                self.visit_function(&mut cloned_func);
                self.monomorphized_functions.insert(cloned_func.name.clone(), cloned_func);
            }
        } else {
            info!(target: "monomorphization",
                  "function `{}` does not need to be monomorphized",
                  cloned_func.name);
            self.monomorphized_functions.insert(cloned_func.name.clone(), cloned_func);
        }
    }
}

fn mangle_function_name(func: &mut TypedFunction) {
    // our current strategy for mangling functions is to
    // hash the name of the function + the string value
    // of its type.
    let fun_ty = Type::Function(func.parameter_types.clone(),
                                Box::new(func.return_type.clone()));
    let hash = hash::hash::<_, SipHasher>(&format!("{}", fun_ty));
    func.name = format!("{}_{}", func.name, hash);
}
