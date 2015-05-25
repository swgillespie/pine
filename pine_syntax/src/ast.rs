use pine_common::Span;
use rustc_serialize::{Encodable, Encoder};

pub type CompilationUnit = Vec<SpannedFunction>;

#[derive(PartialEq, Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub data: T
}

impl<T: Encodable> Encodable for Spanned<T> {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        self.data.encode(s)
    }
}

pub type SpannedString = Spanned<String>;
pub type SpannedFunction = Spanned<Function>;
pub type SpannedExpression = Spanned<Expression>;

#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub struct Function {
    pub name: SpannedString,
    pub parameters: Vec<SpannedString>,
    pub body: SpannedBlock
}

#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub struct Block(pub Option<SpannedExpression>);
pub type SpannedBlock = Spanned<Block>;

pub type SpannedExpressionOrBlock = Spanned<ExpressionOrBlock>;
#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub enum ExpressionOrBlock {
    Expr(SpannedExpression),
    Block(Option<SpannedExpression>)
}

pub type SpannedLiteral = Spanned<Literal>;
#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Float(f32),
    String(String)
}

#[derive(PartialEq, Debug, Clone, Copy, RustcEncodable)]
pub enum Binop {
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    NotEqual,
    GreaterThanEq,
    LessThanEq,
    LessThan,
    GreaterThan,
    And,
    Or
}

#[derive(PartialEq, Debug, Clone, Copy, RustcEncodable)]
pub enum Unop {
    Dereference,
    Not,
    Negate
}

#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub enum Expression {
    Literal(SpannedLiteral),
    Identifier(SpannedString),
    Ref(Box<SpannedExpression>),
    IfThen(Box<SpannedExpression>, Box<SpannedExpression>),
    IfThenElse(Box<SpannedExpression>, Box<SpannedExpression>, Box<SpannedExpression>),
    Lambda(Vec<SpannedString>, Box<SpannedExpressionOrBlock>),
    FunctionCall(Box<SpannedExpression>, Vec<SpannedExpression>),
    PostfixFunctionCall(Box<SpannedExpression>, SpannedString, Vec<SpannedExpression>),
    Let(SpannedPattern, Box<SpannedExpression>, Box<SpannedExpression>),
    Assign(Box<SpannedExpression>, Box<SpannedExpression>),
    BinaryOperator(Box<SpannedExpression>, Box<SpannedExpression>, Binop),
    UnaryOperator(Box<SpannedExpression>, Unop),
    TupleCreation(Vec<SpannedExpression>),
    Paren(Box<SpannedExpression>)
}

pub type SpannedPattern = Spanned<Pattern>;
#[derive(PartialEq, Debug, Clone, RustcEncodable)]
pub enum Pattern {
    Ident(SpannedString),
    TupleDestructure(Vec<SpannedString>)
}
