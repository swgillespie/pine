use types::Type;

#[derive(PartialEq, Clone, Debug)]
pub struct TypedFunction {
    pub return_type: Type,
    pub parameter_types: Vec<Type>,
    pub name: String,
    pub parameter_names: Vec<String>,
    pub body: TypedExpression
}

#[derive(PartialEq, Clone, Debug)]
pub struct Typed<T> {
    pub ty: Type,
    pub data: T
}

pub type TypedExpression = Typed<Expression>;

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Literal(TypedLiteral),
    Identifier(TypedIdentifier),
    Ref(Box<TypedExpression>),
    IfThenElse(Box<TypedExpression>, Box<TypedExpression>, Box<Option<TypedExpression>>),
    FunctionCall(Box<TypedExpression>, Vec<TypedExpression>),
    Let(TypedPattern, Box<TypedExpression>, Box<TypedExpression>),
    Assign(Box<TypedExpression>, Box<TypedExpression>),
    BinaryOperator(Box<TypedExpression>, Box<TypedExpression>, TypedBinop),
    UnaryOperator(Box<TypedExpression>, TypedUnop),
    TupleCreation(Vec<TypedExpression>),
    Paren(Box<TypedExpression>)
}

pub type TypedBlock = Typed<Block>;

#[derive(PartialEq, Clone, Debug)]
pub struct Block(pub Option<TypedExpression>);

pub type TypedLiteral = Typed<Literal>;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Float(f32),
    String(String)
}

pub type TypedIdentifier = Typed<String>;

pub type TypedPattern = Typed<Pattern>;

#[derive(PartialEq, Clone, Debug)]
pub enum Pattern {
    Ident(TypedIdentifier),
    TupleDestructure(Vec<TypedIdentifier>)
}


pub type TypedBinop = Typed<Binop>;
#[derive(PartialEq, Clone, Debug, Copy)]
pub enum Binop {
    IntegerPlus,
    IntegerMinus,
    IntegerMul,
    IntegerDiv,
    IntegerEq,
    IntegerNeq,
    IntegerGeq,
    IntegerLeq,
    IntegerLT,
    IntegerGT,
    BooleanAnd,
    BooleanOr
}

pub type TypedUnop = Typed<Unop>;
#[derive(PartialEq, Clone, Debug, Copy)]
pub enum Unop {
    PointerDereference,
    BooleanNot,
    IntegerNegate,
    FloatNegate
}
