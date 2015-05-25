use pine_common::{Severity, CompileDiagnostic};
use pine_syntax::visitor::*;
use pine_syntax::ast;

use types::{self, Type, TypeConst};
use symbol_table::SymbolTable;
use typed_ast as typed;
use typed_ast::Typed;

use std::any::Any;

pub struct TypeBinder {
    type_environment: SymbolTable<String, Type>,
    value_environment: SymbolTable<String, Type>,
    level: i32,
    filename: String
}

macro_rules! span_err_and_return {
    ($this:expr, $span:expr, $msg:expr) => {
        {
            let diagnostic = CompileDiagnostic {
                filename: $this.filename.clone(),
                span: $span.span,
                severity: Severity::Error,
                message: $msg
            };
            return Err(diagnostic)
        }
    }
}

impl TypeBinder {
    pub fn new(filename: String) -> TypeBinder {
        TypeBinder {
            type_environment: SymbolTable::new(),
            value_environment: SymbolTable::new(),
            level: 0,
            filename: filename
        }
    }

    pub fn enter_scope(&mut self) {
        self.type_environment.enter_scope();
        self.value_environment.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        self.type_environment.exit_scope();
        self.value_environment.exit_scope();
    }
}

impl Visitor for TypeBinder {
    type Error = CompileDiagnostic;

    fn visit_function<'ast>(&mut self, func: &'ast ast::Function) -> Result<Box<Any>, CompileDiagnostic> {
        // functions in pine allow for recursive bindings by
        // default, in contrast to languages like f#/ocaml which
        // require the `rec` keyword to bind recursively.
        // In order to accomodate this, we create a new type
        // variable for this function and put it into the environment.
        // Once we infer the type of this function, we unify this
        // variable with the inferred type.
        debug!("typechecking function `{}`", &func.name.data);
        self.value_environment.insert_global(func.name.data.clone(), types::new_var(self.level));
        debug!("assigned initial type {:?} to function {}",
               self.value_environment.lookup(&func.name.data),
               &func.name.data);

        // start a new scope, as we will begin by inserting our parameters
        // into the environment.
        self.enter_scope();
        for &ast::Spanned { span: _, data: ref name } in func.parameters.iter() {
            let ty = types::new_var(self.level);
            debug!("assigned initial type {:?} to parameter {}",
                   ty,
                   name);
            self.value_environment.insert_global(name.clone(), ty);
        }

        // with our parameters set up, infer the type of the body of the
        // function.
        let bound_body : Box<typed::TypedBlock> = try!(self.visit_body(&func.body))
            .downcast()
            .ok()
            .expect("return type of visit_body not TypedBody");

        self.exit_scope();

        // at this point we have enough information to infer the type
        // of the function.
        let param_tys = func.parameters.iter()
            .map(|s| self.value_environment.lookup(&s.data).unwrap())
            .cloned()
            .collect::<Vec<_>>();
        debug!("inferred types for parameters of {}: {:?}",
               &func.name.data,
               param_tys);
        let mut fun_ty = Type::Arrow(param_tys.clone(), Box::new(bound_body.ty.clone()));

        let body_expr = match bound_body.data {
            typed::Block(Some(ref expr)) => expr.clone(),
            _ => typed::TypedExpression {
                ty: Type::Const(TypeConst::Unit),
                data: typed::Expression::TupleCreation(vec![])
            }
        };

        // next we can unify this with the type variable we created
        // for the function. This puts the unified type into the global namespace.
        let function_var = self.value_environment.lookup_mut(&func.name.data).unwrap();
        if let Err(ref s) = types::unify(function_var, &mut fun_ty) {
            debug!("failed to unify function types {:?} and {:?}",
                   function_var,
                   fun_ty);
            span_err_and_return!(self, &func.name, s.clone());
        }
        debug!("inferred type {:?} for function {}",
               function_var,
               &func.name.data);

        // now that that's done, return the typed AST for this function.
        return Ok(Box::new(typed::TypedFunction {
            return_type: bound_body.ty,
            parameter_types: param_tys,
            name: func.name.data.clone(),
            parameter_names: func.parameters.iter()
                .map(|x| x.data.clone())
                .collect(),
            body: body_expr
        }))
    }

    fn visit_body<'ast>(&mut self,
                        body: &'ast ast::SpannedBlock) -> Result<Box<Any>, CompileDiagnostic> {
        // a body (or block) consists of one or zero expressions.
        // if the body has zero expressions, it has type unit.
        // otherwise, the type of the block is the same as the
        // type of its expression.
        let typed_expr = match body.data {
            ast::Block(Some(ref expr)) => {
                let expr : Box<typed::TypedExpression> = try!(self.visit_expression(expr))
                    .downcast()
                    .ok()
                    .expect("visit_expression didn't return \
                             a TypedExpression");
                Some(*expr)
            }
            ast::Block(None) => None
        };
        Ok(Box::new(typed::Block(typed_expr)))
    }

    fn visit_expression<'ast>(&mut self,
                              expr: &'ast ast::SpannedExpression) -> Result<Box<Any>, CompileDiagnostic> {
        unimplemented!()
    }

    fn visit_literal<'ast>(&mut self,
                           lit: &'ast ast::SpannedLiteral) -> Result<Box<Any>, CompileDiagnostic> {
        // literals always have a specific type.
        let res = match lit.data {
            ast::Literal::Int(i) => Typed {
                ty: Type::Const(TypeConst::Int),
                data: typed::Literal::Int(i)
            },
            ast::Literal::Bool(b) => Typed {
                ty: Type::Const(TypeConst::Bool),
                data: typed::Literal::Bool(b)
            },
            ast::Literal::Float(f) => Typed {
                ty: Type::Const(TypeConst::Float),
                data: typed::Literal::Float(f)
            },
            ast::Literal::String(ref s) => Typed {
                ty: Type::Const(TypeConst::String),
                data: typed::Literal::String(s.clone())
            }
        };
        Ok(Box::new(res))
    }

    fn visit_identifier<'ast>(&mut self,
                              ident: &'ast ast::SpannedString) -> Result<Box<Any>, CompileDiagnostic> {
        // if the identifier exists in this environment (i.e. it's a parameter or it was bound with a
        // `let` statement) then instantiate the type in the symbol table and return it.
        // Otherwise, it's an error (unbound identifier).
        let ty = match self.value_environment.lookup_mut(&ident.data) {
            Some(ref mut ty) => types::instantiate(self.level, ty),
            None => span_err_and_return!(self, ident,
                                         format!("unbound identifier: {} ", ident.data.clone()))
        };
        Ok(Box::new(Typed {
            ty: ty,
            data: ident.data.clone()
        }))
    }
}
