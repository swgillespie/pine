use std::any::Any;
use ast::*;

pub trait Visitor: Sized {
    type Error;

    fn visit_function<'ast>(&mut self, func: &'ast Function) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_function(self, func));
        Ok(Box::new(0))
    }

    fn visit_body<'ast>(&mut self, body: &'ast SpannedBlock) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_body(self, body));
        Ok(Box::new(0))
    }

    fn visit_expression<'ast>(&mut self, expr: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_expression(self, expr));
        Ok(Box::new(0))
    }

    fn visit_literal<'ast>(&mut self, _: &'ast SpannedLiteral) -> Result<Box<Any>, <Self as Visitor>::Error> {
        Ok(Box::new(0))
    }

    fn visit_identifier<'ast>(&mut self, _: &'ast SpannedString) -> Result<Box<Any>, <Self as Visitor>::Error> {
        Ok(Box::new(0))
    }

    fn visit_ref<'ast>(&mut self,
                 expr: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_ref(self, expr));
        Ok(Box::new(0))
    }

    fn visit_if_then<'ast>(&mut self,
                           cond: &'ast SpannedExpression,
                           true_branch: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_if_then(self, cond, true_branch));
        Ok(Box::new(0))
    }

    fn visit_if_then_else<'ast>(&mut self,
                                cond: &'ast SpannedExpression,
                                true_branch: &'ast SpannedExpression,
                                false_branch: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_if_then_else(self, cond, true_branch, false_branch));
        Ok(Box::new(0))
    }

    fn visit_lambda<'ast>(&mut self,
                          params: &'ast [SpannedString],
                          body: &'ast SpannedExpressionOrBlock) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_lambda(self, params, body));
        Ok(Box::new(0))
    }

    fn visit_function_call<'ast>(&mut self,
                                 func: &'ast SpannedExpression,
                                 params: &'ast [SpannedExpression]) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_function_call(self, func, params));
        Ok(Box::new(0))
    }

    fn visit_postfix_function_call<'ast>(&mut self,
                                         base: &'ast SpannedExpression,
                                         name: &'ast SpannedString,
                                         params: &'ast [SpannedExpression]) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_postfix_function_call(self, base, name, params));
        Ok(Box::new(0))
    }

    fn visit_let<'ast>(&mut self,
                       pat: &'ast SpannedPattern,
                       binding: &'ast SpannedExpression,
                       expr: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_let(self, pat, binding, expr));
        Ok(Box::new(0))
    }

    fn visit_assign<'ast>(&mut self,
                          target: &'ast SpannedExpression,
                          source: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_assign(self, target, source));
        Ok(Box::new(0))
    }

    fn visit_binary_op<'ast>(&mut self,
                             left: &'ast SpannedExpression,
                             right: &'ast SpannedExpression,
                             op: &'ast Binop) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_binary_op(self, left, right, op));
        Ok(Box::new(0))
    }

    fn visit_unary_op<'ast>(&mut self,
                            operand: &'ast SpannedExpression,
                            op: &'ast Unop) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_unary_op(self, operand, op));
        Ok(Box::new(0))
    }

    fn visit_tuple<'ast>(&mut self,
                         elements: &'ast [SpannedExpression]) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_tuple(self, elements));
        Ok(Box::new(0))
    }

    fn visit_paren<'ast>(&mut self,
                         expr: &'ast SpannedExpression) -> Result<Box<Any>, <Self as Visitor>::Error> {
        let _ = try!(walk_paren(self, expr));
        Ok(Box::new(0))
    }

    fn visit_pattern<'ast>(&mut self,
                           _: &'ast SpannedPattern) -> Result<Box<Any>, <Self as Visitor>::Error> {
        Ok(Box::new(0))
    }
}

pub fn walk_function<'ast, V: Visitor>(visitor: &mut V,
                                   func: &'ast Function) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_body(&func.body));
    Ok(())
}

pub fn walk_body<'ast, V: Visitor>(visitor: &mut V,
                                   block: &'ast SpannedBlock) -> Result<(), <V as Visitor>::Error> {
    match block.data.0 {
        Some(ref expr) => { let _ = try!(visitor.visit_expression(expr)); },
        None => ()
    };
    Ok(())
}

pub fn walk_expression<'ast, V: Visitor>(visitor: &mut V,
                                           expr: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(match expr.data {
        Expression::Literal(ref literal) => visitor.visit_literal(literal),
        Expression::Identifier(ref string) => visitor.visit_identifier(string),
        Expression::Ref(ref expr) => visitor.visit_ref(expr),
        Expression::IfThen(ref cond, ref true_branch) => visitor.visit_if_then(cond, true_branch),
        Expression::IfThenElse(ref cond, ref true_branch, ref false_branch) => visitor.visit_if_then_else(cond, true_branch, false_branch),
        Expression::Lambda(ref params, ref expr_or_block) => visitor.visit_lambda(params, expr_or_block),
        Expression::FunctionCall(ref func, ref params) => visitor.visit_function_call(func, params),
        Expression::PostfixFunctionCall(ref instance, ref name, ref params) => visitor.visit_postfix_function_call(instance, name, params),
        Expression::Let(ref pat, ref binding, ref expr) => visitor.visit_let(pat, binding, expr),
        Expression::Assign(ref target, ref source) => visitor.visit_assign(target, source),
        Expression::BinaryOperator(ref left, ref right, ref op) => visitor.visit_binary_op(left, right, op),
        Expression::UnaryOperator(ref operand, ref op) => visitor.visit_unary_op(operand, op),
        Expression::TupleCreation(ref elements) => visitor.visit_tuple(elements),
        Expression::Paren(ref expr) => visitor.visit_paren(expr)
    });
    Ok(())
}

pub fn walk_ref<'ast, V: Visitor>(visitor: &mut V,
             expr: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(expr));
    Ok(())
}

pub fn walk_if_then<'ast, V: Visitor>(visitor: &mut V,
                 cond: &'ast SpannedExpression,
                 true_branch: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(cond));
    let _ = try!(visitor.visit_expression(true_branch));
    Ok(())
}

pub fn walk_if_then_else<'ast, V: Visitor>(visitor: &mut V,
                      cond: &'ast SpannedExpression,
                      true_branch: &'ast SpannedExpression,
                      false_branch: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(cond));
    let _ = try!(visitor.visit_expression(true_branch));
    let _ = try!(visitor.visit_expression(false_branch));
    Ok(())
}

pub fn walk_lambda<'ast, V: Visitor>(visitor: &mut V,
                _: &'ast [SpannedString],
                body: &'ast SpannedExpressionOrBlock) -> Result<(), <V as Visitor>::Error> {
    match body.data {
        ExpressionOrBlock::Expr(ref expr) => { let _ = try!(visitor.visit_expression(expr)); },
        ExpressionOrBlock::Block(Some(ref expr)) => { let _ = try!(visitor.visit_expression(expr)); },
        _ => ()
    };
    Ok(())
}

pub fn walk_function_call<'ast, V: Visitor>(visitor: &mut V,
                       func: &'ast SpannedExpression,
                       params: &'ast [SpannedExpression]) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(func));
    for p in params {
        let _ = try!(visitor.visit_expression(p));
    }
    Ok(())
}

pub fn walk_postfix_function_call<'ast, V: Visitor>(visitor: &mut V,
                               base: &'ast SpannedExpression,
                               _: &'ast SpannedString,
                               params: &'ast [SpannedExpression]) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(base));
    for p in params {
        let _ = try!(visitor.visit_expression(p));
    }
    Ok(())
}

pub fn walk_let<'ast, V: Visitor>(visitor: &mut V,
             pat: &'ast SpannedPattern,
             binding: &'ast SpannedExpression,
             expr: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_pattern(pat));
    let _ = try!(visitor.visit_expression(binding));
    let _ = try!(visitor.visit_expression(expr));
    Ok(())
}

pub fn walk_assign<'ast, V: Visitor>(visitor: &mut V,
                target: &'ast SpannedExpression,
                source: &'ast SpannedExpression) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(target));
    let _ = try!(visitor.visit_expression(source));
    Ok(())
}

pub fn walk_binary_op<'ast, V: Visitor>(visitor: &mut V,
                   left: &'ast SpannedExpression,
                   right: &'ast SpannedExpression,
                   _: &'ast Binop) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(left));
    let _ = try!(visitor.visit_expression(right));
    Ok(())
}

pub fn walk_unary_op<'ast, V: Visitor>(visitor: &mut V,
                  operand: &'ast SpannedExpression,
                  _: &'ast Unop) -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(operand));
    Ok(())
}

pub fn walk_tuple<'ast, V: Visitor>(visitor: &mut V,
               elements: &'ast [SpannedExpression])  -> Result<(), <V as Visitor>::Error>{
    for t in elements {
        let _ = try!(visitor.visit_expression(t));
    }
    Ok(())
}

pub fn walk_paren<'ast, V: Visitor>(visitor: &mut V,
               expr: &'ast SpannedExpression)  -> Result<(), <V as Visitor>::Error> {
    let _ = try!(visitor.visit_expression(expr));
    Ok(())
}
