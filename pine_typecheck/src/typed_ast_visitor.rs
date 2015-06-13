use typed_ast::*;

pub trait TypedVisitor: Sized {
    fn visit_function(&mut self, func: &mut TypedFunction) {
        walk_function(self, func);
    }

    fn visit_expression(&mut self, expr: &mut TypedExpression) {
        walk_expression(self, expr);
    }

    fn visit_literal(&mut self, _: &mut TypedLiteral) {
        // do nothing
    }


    fn visit_identifier(&mut self, _: &mut TypedIdentifier) {
        // do nothing
    }

    fn visit_ref(&mut self,
                 expr: &mut TypedExpression) {
        walk_ref(self, expr);
    }

    fn visit_if_then_else(&mut self,
                          cond: &mut TypedExpression,
                          true_branch: &mut TypedExpression,
                          false_branch: &mut Option<Box<TypedExpression>>) {
        walk_if_then_else(self, cond, true_branch, false_branch);
    }

    fn visit_function_call(&mut self,
                           func: &mut TypedExpression,
                           params: &mut [TypedExpression]) {
        walk_function_call(self, func, params);
    }

    fn visit_let(&mut self,
                 pat: &mut Pattern,
                 binding: &mut TypedExpression,
                 expr: &mut TypedExpression) {
        walk_let(self, pat, binding, expr);
    }

    fn visit_assign(&mut self,
                    target: &mut TypedExpression,
                    source: &mut TypedExpression) {
        walk_assign(self, target, source);
    }

    fn visit_binary_op(&mut self,
                       left: &mut TypedExpression,
                       right: &mut TypedExpression,
                       op: &Binop) {
        walk_binary_op(self, left, right, op);
    }

    fn visit_unary_op(&mut self,
                      operand: &mut TypedExpression,
                      op: &Unop) {
        walk_unary_op(self, operand, op);
    }

    fn visit_tuple(&mut self,
                   elements: &mut [TypedExpression]) {
        walk_tuple(self, elements);
    }

    fn visit_paren(&mut self,
                   expr: &mut TypedExpression) {
        walk_paren(self, expr);
    }

    fn visit_pattern(&mut self,
                     _: &Pattern) {
        // do nothing
    }
}

pub fn walk_function<V: TypedVisitor>(visitor: &mut V,
                                      func: &mut TypedFunction) {
    visitor.visit_expression(&mut func.body);
}

pub fn walk_expression<V: TypedVisitor>(visitor: &mut V,
                                        expr: &mut TypedExpression) {
    match expr.data {
        Expression::Literal(ref mut literal) => visitor.visit_literal(literal),
        Expression::Identifier(ref mut string) => visitor.visit_identifier(string),
        Expression::Ref(ref mut expr) => visitor.visit_ref(expr),
        Expression::IfThenElse(ref mut cond, ref mut true_branch, ref mut false_branch) => visitor.visit_if_then_else(cond, true_branch, false_branch),
        Expression::FunctionCall(ref mut func, ref mut params) => visitor.visit_function_call(func, params),
        Expression::Let(ref mut pat, ref mut binding, ref mut expr) => visitor.visit_let(pat, binding, expr),
        Expression::Assign(ref mut target, ref mut source) => visitor.visit_assign(target, source),
        Expression::BinaryOperator(ref mut left, ref mut right, ref mut op) => visitor.visit_binary_op(left, right, op),
        Expression::UnaryOperator(ref mut operand, ref mut op) => visitor.visit_unary_op(operand, op),
        Expression::TupleCreation(ref mut elements) => visitor.visit_tuple(elements),
        Expression::Paren(ref mut expr) => visitor.visit_paren(expr)
    };
}

pub fn walk_ref<V: TypedVisitor>(visitor: &mut V,
                                 expr: &mut TypedExpression) {
    visitor.visit_expression(expr);
}

pub fn walk_if_then_else<V: TypedVisitor>(visitor: &mut V,
                                          cond: &mut TypedExpression,
                                          true_branch: &mut TypedExpression,
                                          false_branch: &mut Option<Box<TypedExpression>>) {
    visitor.visit_expression(cond);
    visitor.visit_expression(true_branch);
    if let &mut Some(ref mut b) = false_branch {
        visitor.visit_expression(b);
    }
}

pub fn walk_function_call<V: TypedVisitor>(visitor: &mut V,
                                            func: &mut TypedExpression,
                                            params: &mut [TypedExpression]) {
    visitor.visit_expression(func);
    for p in params {
        visitor.visit_expression(p);
    }
}

pub fn walk_let<'ast, V: TypedVisitor>(visitor: &mut V,
                                  pat: &Pattern,
                                  binding: &mut TypedExpression,
                                  expr: &mut TypedExpression) {
    visitor.visit_pattern(pat);
    visitor.visit_expression(binding);
    visitor.visit_expression(expr);
}

pub fn walk_assign<V: TypedVisitor>(visitor: &mut V,
                                    target: &mut TypedExpression,
                                    source: &mut TypedExpression) {
    visitor.visit_expression(target);
    visitor.visit_expression(source);
}

pub fn walk_binary_op<V: TypedVisitor>(visitor: &mut V,
                                       left: &mut TypedExpression,
                                       right: &mut TypedExpression,
                                       _: &Binop) {
    visitor.visit_expression(left);
    visitor.visit_expression(right);
}

pub fn walk_unary_op<V: TypedVisitor>(visitor: &mut V,
                                      operand: &mut TypedExpression,
                                      _: &Unop) {
    visitor.visit_expression(operand);
}

pub fn walk_tuple<V: TypedVisitor>(visitor: &mut V,
                                   elements: &mut [TypedExpression]) {
    for t in elements {
        visitor.visit_expression(t);
    }
}

pub fn walk_paren<V: TypedVisitor>(visitor: &mut V,
                                   expr: &mut TypedExpression) {
    visitor.visit_expression(expr);
}
