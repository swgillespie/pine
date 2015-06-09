use types::Type;

#[derive(Clone, Debug)]
pub struct TypedFunction {
    pub return_type: Type,
    pub parameter_types: Vec<Type>,
    pub name: String,
    pub parameter_names: Vec<String>,
    pub body: TypedExpression
}

#[derive(Clone, Debug)]
pub struct Typed<T> {
    pub ty: Type,
    pub data: T
}

pub type TypedExpression = Typed<Expression>;

#[derive(Clone, Debug)]
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

pub type TypedBlock = Typed<Block>;

#[derive(Clone, Debug)]
pub struct Block(pub Option<TypedExpression>);

pub type TypedLiteral = Typed<Literal>;

#[derive(Clone, Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Float(f32),
    String(String)
}

pub type TypedIdentifier = Typed<String>;

#[derive(Clone, Debug)]
pub enum Pattern {
    Ident(String),
    TupleDestructure(Vec<String>)
}


#[derive(Clone, Debug, Copy)]
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

pub type TypedUnop = Typed<Unop>;
#[derive(Clone, Debug, Copy)]
pub enum Unop {
    PointerDereference,
    BooleanNot,
    IntegerNegate,
}
