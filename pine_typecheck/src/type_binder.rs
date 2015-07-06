use pine_common::{Severity, CompileDiagnostic};
use pine_syntax::visitor::*;
use pine_syntax::ast;

use types::{self, Type, TypeConst, TypeEnv, Substitution, compose_subst, Types, Scheme};
use typed_ast as typed;
use typed_ast::Typed;

use std::collections::hash_map::Entry;

pub struct TypeBinder {
    filename: String,
    env: TypeEnv
}

pub type Bound<T> = (Substitution, T);

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
            filename: filename,
            env: TypeEnv::new()
        }
    }
}

impl TypeBinder {
    pub fn visit_function<'ast>(&mut self, func: &'ast ast::Function) -> Result<Bound<typed::TypedFunction>, CompileDiagnostic> {
        info!(target: "type-inference", "beginning type inference for `{}`", func.name.data);
        let saved_env = self.env.clone();
        let param_names : Vec<_> = func.parameters.iter()
            .map(|x| x.data.clone())
            .collect();
        for name in param_names.iter() {
            match self.env.entry(name.clone()) {
                Entry::Occupied(_) => span_err_and_return!(self, func.name,
                    format!("`{}` already exists in this environment", name)),
                Entry::Vacant(v) => {
                    v.insert(Scheme {
                        vars: vec![],
                        ty: types::new_var()
                    });
                }
            }
        }

        match self.env.entry(func.name.data.clone()) {
            Entry::Occupied(_) => span_err_and_return!(self, func.name,
                format!("`{}` already exists in this environment", func.name.data.clone())),
            Entry::Vacant(v) => {
                v.insert(Scheme {
                    vars: vec![],
                    ty: types::new_var()
                });
            }
        }

        let (ref s1, ref t1) = try!(self.visit_body(&func.body));
        info!(target: "type-inference", "return type `{}` inferred for function `{}`", t1.ty, func.name.data);

        // at this point, we have inferred constraints for each one of the parameters here.
        // ultimately, the types of the parameters to this function are the type variables
        // with their substitutions applied.

        let mut param_types : Vec<_> = param_names.iter()
            .map(|x| self.env.get(x).unwrap().clone())
            .map(|mut t| { types::instantiate(&mut t); t.ty })
            .collect();

        param_types.apply_subst(s1);

        // we also need to infer the type of the function itself by unifying the type variable
        // we made earlier with the type we just inferred.
        let inferred_type = Type::Function(param_types.clone(), Box::new(t1.ty.clone()));
        info!(target: "type-inference", "complete type `{}` inferred for function `{}`", inferred_type, func.name.data);

        let mut type_var = self.env.get(&func.name.data).unwrap().clone();
        types::instantiate(&mut type_var);
        type_var.apply_subst(s1);

        info!(target: "type-inference", "inferred type `{}` for recursive invocations of function `{}`", type_var.ty, func.name.data);
        let s2 = try!(self.unify_with_span(&func.name, &inferred_type, &type_var.ty));

        let unifying_subst = compose_subst(&s2, &s1);
        let ret_ty = t1.ty.clone();
        //ret_ty.apply_subst(&unifying_subst);
        //param_types.apply_subst(&unifying_subst);
        // if that was successful, then our function is well-typed.
        let mut function = typed::TypedFunction {
            return_type: ret_ty,
            parameter_types: param_types,
            name: func.name.data.clone(),
            parameter_names: param_names,
            body: t1.clone()
        };
        function.apply_subst(&unifying_subst);

        self.env = saved_env;

        let generalized_fun_type = types::generalize(&self.env, &inferred_type);
        self.env.insert(func.name.data.clone(), generalized_fun_type);
        //types::reset_type_vars();
        Ok(((unifying_subst, function)))
    }

    pub fn visit_item(&mut self,
                      item: &ast::Item) -> Result<Bound<typed::TypedItem>, CompileDiagnostic> {
        match item {
            &ast::Item::Function(ref func) => {
                let (subst, ast) = try!(self.visit_function(&func.data));
                Ok((subst, typed::TypedItem::Function(ast)))
            },
            &ast::Item::ExternFunction(ref extern_fn) => {
                match type_ast_to_type(&extern_fn.data.ty.data) {
                    Ok(ty) => {
                        info!(target: "type-inference", "inserting extern fn `{}` with type `{}` into env",
                              extern_fn.data.name.data, ty);
                        let generalized = types::generalize(&self.env, &ty);
                        self.env.insert(extern_fn.data.name.data.clone(), generalized);
                        Ok((types::empty_subst(),
                            typed::TypedItem::ExternFunction(typed::TypedExternFunction {
                                ty: ty,
                                name: extern_fn.data.name.data.clone()
                            })))
                    },
                    Err(msg) => span_err_and_return!(self, extern_fn.data.ty, msg)
                }
            }
        }
    }

    fn visit_body<'ast>(&mut self,
                        block: &'ast ast::SpannedBlock) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        match block.data.0 {
            Some(ref expr) => self.visit_expression(expr),
            None => panic!("don't do this yet!")
        }
    }

    pub fn visit_expression<'ast>(&mut self,
                              expr: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let res = try!(match expr.data {
            ast::Expression::Literal(ref literal) => self.visit_literal(literal),
            ast::Expression::Identifier(ref string) => self.visit_identifier(string),
            ast::Expression::Ref(ref expr) => self.visit_ref(expr),
            ast::Expression::IfThen(ref cond, ref true_branch) => self.visit_if_then(cond, true_branch),
            ast::Expression::IfThenElse(ref cond, ref true_branch, ref false_branch) => self.visit_if_then_else(cond, true_branch, false_branch),
            ast::Expression::Lambda(ref params, ref expr_or_block) => self.visit_lambda(params, expr_or_block),
            ast::Expression::FunctionCall(ref func, ref params) => self.visit_function_call(func, params),
            ast::Expression::PostfixFunctionCall(ref instance, ref name, ref params) => self.visit_postfix_function_call(instance, name, params),
            ast::Expression::Let(ref pat, ref binding, ref expr) => self.visit_let(pat, binding, expr),
            ast::Expression::Assign(ref target, ref source) => self.visit_assign(target, source),
            ast::Expression::BinaryOperator(ref left, ref right, ref op) => self.visit_binary_op(left, right, op),
            ast::Expression::UnaryOperator(ref operand, ref op) => self.visit_unary_op(operand, op),
            ast::Expression::TupleCreation(ref elements) => self.visit_tuple(elements),
            ast::Expression::Paren(ref expr) => self.visit_paren(expr)
        });
        Ok(res)
    }

    fn visit_literal<'ast>(&mut self,
                           lit: &'ast ast::SpannedLiteral) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let res = match lit.data {
            ast::Literal::Int(i) => typed::Typed {
                ty: Type::Const(TypeConst::Int),
                data: typed::Literal::Int(i)
            },
            ast::Literal::Bool(b) => typed::Typed {
                ty: Type::Const(TypeConst::Bool),
                data: typed::Literal::Bool(b)
            },
            ast::Literal::Float(f) => typed::Typed {
                ty: Type::Const(TypeConst::Float),
                data: typed::Literal::Float(f)
            },
            ast::Literal::String(ref s) => typed::Typed {
                ty: Type::Const(TypeConst::String),
                data: typed::Literal::String(s.clone())
            },
            ast::Literal::Unit => typed::Typed {
                ty: Type::Const(TypeConst::Unit),
                data: typed::Literal::Unit
            }
        };
        Ok((types::empty_subst(), Typed {
            ty: res.ty.clone(),
            data: typed::Expression::Literal(res)
        }))
    }

    fn visit_identifier<'ast>(&mut self,
                              ident: &'ast ast::SpannedString) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        match self.env.get(&ident.data) {
            Some(scheme) => {
                let mut instantiated = scheme.clone();
                types::instantiate(&mut instantiated);
                info!(target: "type-inference", "instantiated `{}` type for identifier `{}`", instantiated.ty, ident.data);
                let ident = Typed {
                    ty: instantiated.ty.clone(),
                    data: typed::Expression::Identifier(Typed {
                        ty: instantiated.ty.clone(),
                        data: ident.data.clone()
                    }
                )};

                Ok((types::empty_subst(), ident))
            },
            None => span_err_and_return!(self, ident, format!("unbound \
                       identifier `{}`", ident.data))
        }
    }

    fn visit_ref<'ast>(&mut self,
                       expr: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        span_err_and_return!(self, expr,
                             "`ref` has not yet been \
                              implemented completely.".to_string())
    }

    fn visit_if_then<'ast>(&mut self,
                           cond: &'ast ast::SpannedExpression,
                           true_branch: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let bound_cond = try!(self.visit_expression(cond));
        // unify the condition's type with bool
        let (ref s1, ref t1) = bound_cond;
        let s2 = try!(self.unify_with_span(cond, &t1.ty, &Type::Const(TypeConst::Bool)));
        let bound_true  = try!(self.visit_expression(true_branch));
        // unify the true branch with unit
        let (ref s3, ref t2) = bound_true;
        let s4 = try!(self.unify_with_span(true_branch, &t2.ty, &Type::Const(TypeConst::Unit)));

        // all is good. Compose all of the substitutions and return.
        // the compose_subst tree kinda sucks, but I translated this
        // from a Haskell example that used it as an infix operator.
        let final_subst = compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1)));
        let mut cond_subst = t1.clone();
        let mut true_subst = t2.clone();
        cond_subst.apply_subst(&final_subst);
        true_subst.apply_subst(&final_subst);

        let result = (final_subst, Typed {
            ty: Type::Const(TypeConst::Unit),
            data: typed::Expression::IfThenElse(
                Box::new(cond_subst),
                Box::new(true_subst),
                None)
        });
        Ok(result)
    }

    fn visit_if_then_else<'ast>(&mut self,
                                cond: &'ast ast::SpannedExpression,
                                true_branch: &'ast ast::SpannedExpression,
                                false_branch: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        // first, unify the condition type with bool
        let bound_cond = try!(self.visit_expression(cond));
        let (ref s1, ref t1) = bound_cond;
        let s2 = try!(self.unify_with_span(cond, &t1.ty, &Type::Const(TypeConst::Bool)));
        // next, infer the types for the true and false branches and
        // unify them
        let bound_true = try!(self.visit_expression(true_branch));
        let bound_false = try!(self.visit_expression(false_branch));

        let (ref s3, ref t2) = bound_true;
        let (ref s4, ref t3) = bound_false;

        let s5 = try!(self.unify_with_span(false_branch, &t2.ty, &t3.ty));

        // finally, compose the substitutions and return.
        let final_subst = compose_subst(&s5, &compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1))));

        let mut cond_subst = t1.clone();
        let mut true_subst = t2.clone();
        let mut false_subst = t3.clone();
        cond_subst.apply_subst(&final_subst);
        true_subst.apply_subst(&final_subst);
        false_subst.apply_subst(&final_subst);

        let result = (final_subst, Typed {
            ty: t2.ty.clone(),
            data: typed::Expression::IfThenElse(
                Box::new(cond_subst),
                Box::new(true_subst),
                Some(Box::new(false_subst)))
        });

        Ok(result)
    }

    fn visit_lambda<'ast>(&mut self,
                          _: &'ast [ast::SpannedString],
                          body: &'ast ast::SpannedExpressionOrBlock) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        span_err_and_return!(self, body,
                             "lambdas have not been implemented yet"
                             .to_string());
    }

    fn visit_function_call<'ast>(&mut self,
                                 func: &'ast ast::SpannedExpression,
                                 args: &'ast [ast::SpannedExpression]) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let mut new_var = types::new_var();
        let saved_env = self.env.clone();
        let (ref s1, ref t1) = try!(self.visit_expression(func));
        self.env.apply_subst(s1);
        info!(target: "type-inference", "function-expression inferred type `{}`", t1.ty);


        let mut typed_params = vec![];
        let mut arg_substs = s1.clone();
        for arg in args {
            let (ref sn, ref tn) = try!(self.visit_expression(arg));
            self.env.apply_subst(sn);
            info!(target: "type-inference", "inferred type `{}` for parameter", tn.ty);
            info!(target: "type-inference", "unifying subst for parameter: `{:?}`", sn);
            arg_substs = compose_subst(sn, &arg_substs);
            typed_params.push(tn.clone());
        }
        self.env = saved_env;

        let called_type = Type::Function(typed_params.iter().map(|t| t.ty.clone()).collect(), Box::new(new_var.clone()));
        info!(target: "type-inference", "signature of called function: `{}` vs. actual type: `{}`", called_type, t1.ty);

        let s2 = try!(self.unify_with_span(func, &called_type, &t1.ty));
        info!(target: "type-inference", "unifying function call substitution: `{:?}`", s2);
        info!(target: "type-inference", "substituted return type: `{}`", new_var);

        let subst = compose_subst(&s2, &compose_subst(&arg_substs, &s1));
        let mut applied = t1.clone();
        applied.apply_subst(&subst);
        new_var.apply_subst(&subst);
        info!(target: "type-inference", "final type for function expression: `{}`", applied.ty);
        typed_params.apply_subst(&subst);
        info!(target: "type-inference", "final type for function call expression: `{}`", new_var);
        Ok((subst,
             Typed {
                 ty: new_var,
                 data: typed::Expression::FunctionCall(
                     Box::new(applied),
                     typed_params)
             }))
    }

    fn visit_postfix_function_call<'ast>(&mut self,
                                         first_arg: &'ast ast::SpannedExpression,
                                         name: &'ast ast::SpannedString,
                                         rest_args: &'ast [ast::SpannedExpression]) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        // this is a quick transform from
        // arg1.function(args...) to function(arg1, args...)
        let function = ast::Spanned {
            span: name.span,
            data: ast::Expression::Identifier(name.clone())
        };
        let mut args = vec![first_arg.clone()];
        for arg in rest_args.iter() {
            args.push(arg.clone());
        }
        self.visit_function_call(&function, &args)
    }

    fn visit_let<'ast>(&mut self,
                       pat: &'ast ast::SpannedPattern,
                       binding: &'ast ast::SpannedExpression,
                       body: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        // TODO move some of this logic to visit_pattern?
        let ident = match pat.data {
            ast::Pattern::Ident(ref s) => s.data.clone(),
            ast::Pattern::Ignored => {
                // patterns can use the _ character to indicate
                // that they don't care about binding the result of
                // the binding subexpression to anything.
                let (ref s1, ref t1) = try!(self.visit_expression(binding));
                let (ref s2, ref t2) = try!(self.visit_expression(body));
                return Ok((compose_subst(s1, s2), Typed {
                    ty: t2.ty.clone(),
                    data: typed::Expression::Let(typed::Pattern::Ignored,
                                                 Box::new(t1.clone()),
                                                 Box::new(t2.clone()))
                }));
            }
            _ => span_err_and_return!(self, pat,
                                      "non-ident patterns have not been \
                                       implemented yet."
                                      .to_string())
        };

        // otherwise, we want to bind this variable to a new scope.
        let bound_binding = try!(self.visit_expression(binding));
        let (ref s1, ref t1) = bound_binding;
        let saved_env = self.env.clone();

        self.env.apply_subst(s1);
        let generalized_type = types::generalize(&self.env, &t1.ty);

        match self.env.entry(ident.clone()) {
            Entry::Occupied(_) => span_err_and_return!(self, pat,
                format!("`{}` already exists in this environment", ident)),
            Entry::Vacant(v) => {
                v.insert(generalized_type);
            }
        }

        self.env.apply_subst(s1);

        let bound_body = try!(self.visit_expression(body));
        let (ref s2, ref t2) = bound_body;
        self.env = saved_env;

        Ok((compose_subst(s1, s2), Typed {
            ty: t2.ty.clone(),
            data: typed::Expression::Let(typed::Pattern::Ident(ident),
                                         Box::new(t1.clone()),
                                         Box::new(t2.clone()))
        }))
    }

    fn visit_assign<'ast>(&mut self,
                          target: &'ast ast::SpannedExpression,
                          _: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        span_err_and_return!(self, target,
                             "assignment has not been implemented yet"
                             .to_string());
    }

    fn visit_binary_op<'ast>(&mut self,
                             left: &'ast ast::SpannedExpression,
                             right: &'ast ast::SpannedExpression,
                             op: &'ast ast::Binop) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let (ref s1, ref t1) = try!(self.visit_expression(left));
        let (ref s2, ref t2) = try!(self.visit_expression(right));
        // try to determine what ops to use.
        match *op {
            op @ ast::Binop::Plus |
            op @ ast::Binop::Minus |
            op @ ast::Binop::Mul |
            op @ ast::Binop::Div => {
                // both the lhs and rhs must be integers (for now!)
                let s3 = try!(self.unify_with_span(left, &t1.ty, &Type::Const(TypeConst::Int)));
                let s4 = try!(self.unify_with_span(right, &t2.ty, &Type::Const(TypeConst::Int)));
                let typed_op = match op {
                    ast::Binop::Plus => typed::Binop::IntegerPlus,
                    ast::Binop::Minus => typed::Binop::IntegerMinus,
                    ast::Binop::Mul => typed::Binop::IntegerMul,
                    ast::Binop::Div => typed::Binop::IntegerDiv,
                    _ => unreachable!()
                };
                Ok(((compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1)))),
                   Typed {
                       ty: Type::Const(TypeConst::Int),
                       data: typed::Expression::BinaryOperator(
                           Box::new(t1.clone()),
                           Box::new(t2.clone()),
                           typed_op)
                   }))
            },
            op @ ast::Binop::Equal |
            op @ ast::Binop::NotEqual => {
                // the lhs and rhs of == and != can be anything,
                // as long as they are the same type.
                let s3 = try!(self.unify_with_span(right, &t1.ty, &t2.ty));
                let typed_op = match op {
                    ast::Binop::Equal => typed::Binop::PointerEq,
                    ast::Binop::NotEqual => typed::Binop::PointerNeq,
                    _ => unreachable!()
                };
                Ok(((compose_subst(&s3, &compose_subst(&s2, &s1)),
                     Typed {
                         ty: Type::Const(TypeConst::Bool),
                         data: typed::Expression::BinaryOperator(
                             Box::new(t1.clone()),
                             Box::new(t2.clone()),
                             typed_op)
                     })))
            },
            op @ ast::Binop::GreaterThan |
            op @ ast::Binop::LessThan |
            op @ ast::Binop::GreaterThanEq |
            op @ ast::Binop::LessThanEq => {
                // comparison ops are for integers only, right now.
                let s3 = try!(self.unify_with_span(left, &t1.ty, &Type::Const(TypeConst::Int)));
                let s4 = try!(self.unify_with_span(right, &t2.ty, &Type::Const(TypeConst::Int)));
                let typed_op = match op {
                    ast::Binop::GreaterThan => typed::Binop::IntegerGT,
                    ast::Binop::LessThan => typed::Binop::IntegerLT,
                    ast::Binop::GreaterThanEq => typed::Binop::IntegerGeq,
                    ast::Binop::LessThanEq => typed::Binop::IntegerLeq,
                    _ => unreachable!()
                };
                Ok(((compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1))),
                     Typed {
                         ty: Type::Const(TypeConst::Bool),
                         data: typed::Expression::BinaryOperator(
                             Box::new(t1.clone()),
                             Box::new(t2.clone()),
                             typed_op)
                     })))
            },
            op @ ast::Binop::And |
            op @ ast::Binop::Or => {
                // and and or are for booleans.
                let s3 = try!(self.unify_with_span(left, &t1.ty, &Type::Const(TypeConst::Bool)));
                let s4 = try!(self.unify_with_span(right, &t2.ty, &Type::Const(TypeConst::Bool)));
                let typed_op = match op {
                    ast::Binop::And => typed::Binop::BooleanAnd,
                    ast::Binop::Or => typed::Binop::BooleanOr,
                    _ => unreachable!()
                };
                Ok(((compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1))),
                     Typed {
                         ty: Type::Const(TypeConst::Bool),
                         data: typed::Expression::BinaryOperator(
                             Box::new(t1.clone()),
                             Box::new(t2.clone()),
                             typed_op)
                     })))

            }
        }
    }

    fn visit_unary_op<'ast>(&mut self,
                            operand: &'ast ast::SpannedExpression,
                            op: &'ast ast::Unop) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        let (ref s1, ref t1) = try!(self.visit_expression(operand));
        match *op {
            ast::Unop::Dereference => span_err_and_return!(self, operand,
                                                           "deref has not been implemented yet"
                                                           .to_string()),
            ast::Unop::Not => {
                // not is for booleans.
                let s2 = try!(self.unify_with_span(operand, &t1.ty, &Type::Const(TypeConst::Bool)));
                Ok(((compose_subst(&s2, s1),
                     Typed {
                         ty: Type::Const(TypeConst::Bool),
                         data: typed::Expression::UnaryOperator(
                             Box::new(t1.clone()),
                             typed::Unop::BooleanNot)
                     })))
            },
            ast::Unop::Negate => {
                // negate is for ints (and later, floats)
                let s2 = try!(self.unify_with_span(operand, &t1.ty, &Type::Const(TypeConst::Int)));
                Ok(((compose_subst(&s2, s1),
                     Typed {
                         ty: Type::Const(TypeConst::Int),
                         data: typed::Expression::UnaryOperator(
                             Box::new(t1.clone()),
                             typed::Unop::IntegerNegate)
                     })))
            }
        }
    }

    fn visit_tuple<'ast>(&mut self,
                         elements: &'ast [ast::SpannedExpression]) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        span_err_and_return!(self, elements[0],
                             "tuples have not been implemented yet"
                             .to_string());
    }

    fn visit_paren<'ast>(&mut self,
                         expr: &'ast ast::SpannedExpression) -> Result<Bound<typed::TypedExpression>, CompileDiagnostic> {
        self.visit_expression(expr)
    }

    #[allow(dead_code)] // this method will be dead code until I implement tuple patterns
    fn visit_pattern<'ast>(&mut self,
                           pat: &'ast ast::SpannedPattern) -> Result<Box<typed::Pattern>, CompileDiagnostic> {
        span_err_and_return!(self, pat,
                             "patterns have not been implemented yet"
                             .to_string());
    }

}

impl TypeBinder {
    fn unify_with_span<T>(&self,
                          span: &ast::Spanned<T>,
                          ty1: &Type,
                          ty2: &Type) -> Result<Substitution, CompileDiagnostic> {
        match types::unify(ty1, ty2) {
            Ok(subst) => Ok(subst),
            Err(msg) => span_err_and_return!(self, span, msg)
        }
    }
}

fn type_ast_to_type(ty: &ast::Type) -> Result<Type, String> {
    match ty {
        &ast::Type::Identifier(ref ident) => match &**ident {
            "int" => Ok(Type::Const(TypeConst::Int)),
            "bool" => Ok(Type::Const(TypeConst::Bool)),
            "string" => Ok(Type::Const(TypeConst::String)),
            "float" => Ok(Type::Const(TypeConst::Float)),
            "unit" => Ok(Type::Const(TypeConst::Unit)),
            _ => Err(format!("Unknown type: `{}`", ident))
        },
        // TODO - type application
        &ast::Type::Application(_, _) => unimplemented!(),
        &ast::Type::Function(ref params, ref ret_ty) => {
            let mut params_ty = vec![];
            for param in params.iter() {
                params_ty.push(try!(type_ast_to_type(&param.data)));
            }
            let return_ty = try!(type_ast_to_type(&ret_ty.data));
            Ok(Type::Function(params_ty, Box::new(return_ty)))
        },
        &ast::Type::Tuple(_) => unimplemented!()
    }
}
