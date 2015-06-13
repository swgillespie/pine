use rustc_serialize::{Encodable, Encoder};

use std::collections::HashSet;
use types::{Type, Types, Substitution, TypeVar};

pub type TypedCompilationUnit = Vec<TypedFunction>;

#[derive(Clone, Debug, RustcEncodable)]
pub struct TypedFunction {
    pub return_type: Type,
    pub parameter_types: Vec<Type>,
    pub name: String,
    pub parameter_names: Vec<String>,
    pub body: TypedExpression
}

impl Types for TypedFunction {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        self.return_type.free_type_variables()
            .union(&self.parameter_types.free_type_variables()
                   .union(&self.body.free_type_variables())
                   .cloned()
                   .collect())
            .cloned()
            .collect()
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        self.return_type.apply_subst(subst);
        self.parameter_types.apply_subst(subst);
        self.body.apply_subst(subst);
    }
}

#[derive(Clone, Debug, RustcEncodable)]
pub struct Typed<T: Encodable> {
    pub ty: Type,
    pub data: T
}

/*
impl<T: Encodable> Encodable for Typed<T> {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        try!(self.ty.encode(s));
        self.data.encode(s)
    }
}*/

pub type TypedExpression = Typed<Expression>;

#[derive(Clone, Debug, RustcEncodable)]
pub enum Expression {
    Literal(TypedLiteral),
    Identifier(TypedIdentifier),
    Ref(Box<TypedExpression>),
    IfThenElse(Box<TypedExpression>, Box<TypedExpression>, Option<Box<TypedExpression>>),
    FunctionCall(Box<TypedExpression>, Vec<TypedExpression>),
    Let(Pattern, Box<TypedExpression>, Box<TypedExpression>),
    Assign(Box<TypedExpression>, Box<TypedExpression>),
    BinaryOperator(Box<TypedExpression>, Box<TypedExpression>, Binop),
    UnaryOperator(Box<TypedExpression>, Unop),
    TupleCreation(Vec<TypedExpression>),
    Paren(Box<TypedExpression>)
}

impl Types for TypedExpression {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        let free_tys = self.ty.free_type_variables();
        let child_free_tys : HashSet<_> = match self.data {
            Expression::Literal(ref lit) => lit.free_type_variables(),
            Expression::Identifier(ref ident) => ident.free_type_variables(),
            Expression::Ref(ref expr) => expr.free_type_variables(),
            Expression::IfThenElse(ref expr1, ref expr2, Some(ref expr3)) => {
                let temp : HashSet<_> = expr1.free_type_variables()
                    .union(&expr2.free_type_variables())
                    .cloned()
                    .collect();
                temp.union(&expr3.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::IfThenElse(ref expr1, ref expr2, None) => {
                expr1.free_type_variables()
                    .union(&expr2.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::FunctionCall(ref base, ref params) => {
                base.free_type_variables()
                    .union(&params.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::Let(_, ref binder, ref body) => {
                binder.free_type_variables()
                    .union(&body.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::Assign(ref target, ref source) => {
                target.free_type_variables()
                    .union(&source.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::BinaryOperator(ref lhs, ref rhs, _) => {
                lhs.free_type_variables()
                    .union(&rhs.free_type_variables())
                    .cloned()
                    .collect()
            },
            Expression::UnaryOperator(ref operand, _) => operand.free_type_variables(),
            Expression::TupleCreation(ref elements) => elements.free_type_variables(),
            Expression::Paren(ref expr) => expr.free_type_variables()
        };
        free_tys.union(&child_free_tys)
            .cloned()
            .collect()
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        self.ty.apply_subst(subst);
        match self.data {
            Expression::Literal(ref mut lit) => lit.apply_subst(subst),
            Expression::Identifier(ref mut ident) => ident.apply_subst(subst),
            Expression::Ref(ref mut expr) => expr.apply_subst(subst),
            Expression::IfThenElse(ref mut expr1, ref mut expr2, Some(ref mut expr3)) => {
                expr1.apply_subst(subst);
                expr2.apply_subst(subst);
                expr3.apply_subst(subst);
            },
            Expression::IfThenElse(ref mut expr1, ref mut expr2, None) => {
                expr1.apply_subst(subst);
                expr2.apply_subst(subst);
            },
            Expression::FunctionCall(ref mut base, ref mut params) => {
                base.apply_subst(subst);
                params.apply_subst(subst);
            },
            Expression::Let(_, ref mut binder, ref mut body) => {
                binder.apply_subst(subst);
                body.apply_subst(subst);
            },
            Expression::Assign(ref mut target, ref mut source) => {
                target.apply_subst(subst);
                source.apply_subst(subst);
            },
            Expression::BinaryOperator(ref mut lhs, ref mut rhs, _) => {
                lhs.apply_subst(subst);
                rhs.apply_subst(subst);
            },
            Expression::UnaryOperator(ref mut operand, _) => operand.apply_subst(subst),
            Expression::TupleCreation(ref mut elements) => elements.apply_subst(subst),
            Expression::Paren(ref mut expr) => expr.apply_subst(subst)
        };
    }
}

pub type TypedBlock = Typed<Block>;

#[derive(Clone, Debug, RustcEncodable)]
pub struct Block(pub Option<TypedExpression>);

pub type TypedLiteral = Typed<Literal>;

#[derive(Clone, Debug, RustcEncodable)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Float(f32),
    String(String),
    Unit
}

impl Types for TypedLiteral {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        self.ty.free_type_variables()
    }

    fn apply_subst(&mut self, _: &Substitution) {
        // nothing to do
    }
}

pub type TypedIdentifier = Typed<String>;

impl Types for TypedIdentifier {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        self.ty.free_type_variables()
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        self.ty.apply_subst(subst)
    }
}

#[derive(Clone, Debug, RustcEncodable)]
pub enum Pattern {
    Ident(String),
    TupleDestructure(Vec<String>)
}

#[derive(Clone, Debug, Copy, RustcEncodable)]
pub enum Binop {
    IntegerPlus,
    IntegerMinus,
    IntegerMul,
    IntegerDiv,
    IntegerGeq,
    IntegerLeq,
    IntegerLT,
    IntegerGT,
    PointerEq,
    PointerNeq,
    BooleanAnd,
    BooleanOr
}

#[derive(Clone, Debug, Copy, RustcEncodable)]
pub enum Unop {
    PointerDereference,
    BooleanNot,
    IntegerNegate,
}
