//! The "parser" module provides a parser for Pine programs.
//! The parser's job is turning a stream of Tokens (obtained from the lexer module)
//! into an abstract syntax tree (or an error). It does this by utilizing the Lexer
//! struct provided by lexer and utilizing it as an iterator.
//!
//! ## Grammar
//! The grammar that this parser recognizes has not been published yet (as it is in active development),
//! but I am maintaining an ANTLR4 grammar in parallel with this parser that I will publish alongside
//! this source code whenever I have stabilized the grammar somewhat.
//!
//! Each function in the parser roughly corresponds to a production in the grammar, and some of them
//! are documented explicitly with their productions. I will work on expanding the docs as I go.
//!
//! ## The Parser
//! The parser itself is a recursive-descent parser that utilizes one token of lookahead.
//! The lookahead is provided by the Peekable struct wrapped around the lexer, used with
//! the `next_token_one_of` macro. Care has been taken to ensure that this grammar remains LL(1)
//! for ease of parsing.
//!
//! The parser maintains three entry points: `compilation_unit`, `function`, and `expression`.
//! Each one parses multiple functions, a single function, and a single expression respectively.
use pine_common::{CompileDiagnostic, Position, Span, Severity};

use lexer::{Lexer, TokenType, Token};
use lexer::TokenType::*;
use ast::*;

use std::iter::Peekable;
use std::str::FromStr;

pub type ParseResult<T> = Result<T, CompileDiagnostic>;

pub struct Parser<I: Iterator<Item=char>> {
    filename: String,
    lexer: Peekable<Lexer<I>>,
    last_span: Span
}

macro_rules! next_token_one_of {
    ($lexer:expr, $($tok:pat),+) => {
        match $lexer.peek() {
            $(
                Some(&Ok(Token { variant: $tok, span: _})) => true,
            )*
            _ => false
        }
    }
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            filename: FromStr::from_str(lexer.filename()).unwrap(),
            lexer: lexer.peekable(),
            last_span: Span(Position(0, 0), Position(0, 0))
        }
    }

    fn span_err<T>(&self, span: Span, message: String) -> Result<T, CompileDiagnostic> {
        Err(CompileDiagnostic {
            filename: self.filename.clone(),
            span: span,
            severity: Severity::Error,
            message: message
        })
    }

    /* parsing! */
    pub fn compilation_unit(&mut self) -> ParseResult<CompilationUnit> {
        // compilation_unit ::= function compilation_unit
        // compilation_unit ::= ε
        let mut vec = vec![];
        loop {
            let func = match self.lexer.peek() {
                Some(_) => try!(self.function()),
                None => break
            };
            vec.push(func)
        }
        Ok(vec)
    }

    pub fn function(&mut self) -> ParseResult<SpannedFunction> {
        // function ::= DEF IDENTIFIER comma_sep_ident block
        let Span(start, _) = try!(self.parse_token(Def));
        let name = try!(self.identifier());
        let parameters = try!(self.comma_sep_ident());
        let block = try!(self.block());
        Ok(Spanned {
            span: Span(start, block.span.0),
            data: Function {
                name: name,
                parameters: parameters,
                body: block
            }
        })
    }

    fn identifier(&mut self) -> ParseResult<SpannedString> {
        // IDENTIFIER ::= (lexer rule)
        match self.lexer.next() {
            Some(possible_ident) => {
                let ident = try!(possible_ident);
                match ident.variant {
                    Identifier(ref s) => {
                        self.last_span = ident.span;
                        Ok(Spanned {
                            span: ident.span,
                            data: s.clone()
                        })
                    },
                    ref t => self.span_err(ident.span, format!("expected an identifier, got {:?}", t))
                }
            },
            None => self.span_err(self.last_span, format!("expected an identifier, got <EOF>"))
        }
    }

    fn comma_sep_ident(&mut self) -> ParseResult<Vec<SpannedString>> {
        // comma_sep_ident ::= IDENTIFIER
        // comma_sep_ident ::= IDENTIFIER COMMA comma_sep_ident
        // comma_sep_ident ::= ε
        let mut vec = vec![];
        loop {
            if next_token_one_of!(self.lexer, Do, VBar, RParen) {
                break
            }
            // we've eliminated the epsilon case, so we're looking at
            // at least one identifier.
            let ident = try!(self.identifier());
            vec.push(ident);
            match self.lexer.peek() {
                Some(&Ok(Token { variant: Comma, span: _ })) => {
                    let _ = try!(self.parse_token(Comma));
                },
                _ => break
            }
        }
        Ok(vec)
    }

    fn block(&mut self) -> ParseResult<SpannedBlock> {
        // block ::= DO expression END
        // block ::= DO END
        let Span(start, _) = try!(self.parse_token(Do));
        if !next_token_one_of!(self.lexer, End) {
            let expr = try!(self.expression());
            let Span(_, end) = try!(self.parse_token(End));
            Ok(Spanned {
                span: Span(start, end),
                data: Block(Some(expr))
            })
        } else {
            let Span(_, end) = try!(self.parse_token(End));
            Ok(Spanned {
                span: Span(start, end),
                data: Block(None)
            })
        }
    }

    pub fn expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut equality_expr = try!(self.equality_expression());
        loop {
            if !next_token_one_of!(self.lexer,
                                   And,
                                   Or) {
                break;
            }
            let op = op_token_to_binop(try!(self.lexer.next().unwrap()).variant);
            let rhs_expr = try!(self.equality_expression());
            equality_expr = Spanned {
                span: Span(equality_expr.span.0, rhs_expr.span.1),
                data: Expression::BinaryOperator(Box::new(equality_expr), Box::new(rhs_expr), op)
            }
        }
        Ok(equality_expr)
    }

    fn equality_expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut additive_expr = try!(self.additive_expression());
        loop {
            if !next_token_one_of!(self.lexer,
                                   DoubleEq,
                                   NotEqual,
                                   LessThanEq,
                                   LessThan,
                                   GreaterThanEq,
                                   GreaterThan) {
                break;
            }
            let op = op_token_to_binop(try!(self.lexer.next().unwrap()).variant);
            let rhs_expr = try!(self.additive_expression());
            additive_expr = Spanned {
                span: Span(additive_expr.span.0, rhs_expr.span.1),
                data: Expression::BinaryOperator(Box::new(additive_expr), Box::new(rhs_expr), op)
            }
        }
        Ok(additive_expr)
    }

    fn additive_expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut mul_expr = try!(self.multiplicative_expression());
        loop {
            if !next_token_one_of!(self.lexer, Plus, Minus) {
                break;
            }
            let op = op_token_to_binop(try!(self.lexer.next().unwrap()).variant);
            let rhs_expr = try!(self.multiplicative_expression());
            mul_expr = Spanned {
                span: Span(mul_expr.span.0, rhs_expr.span.1),
                data: Expression::BinaryOperator(Box::new(mul_expr), Box::new(rhs_expr), op)
            }
        }
        Ok(mul_expr)
    }

    fn multiplicative_expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut base_expr = try!(self.base_expression());
        loop {
            if !next_token_one_of!(self.lexer, Times, Div) {
                break;
            }
            let op = op_token_to_binop(try!(self.lexer.next().unwrap()).variant);
            let rhs_expr = try!(self.base_expression());
            base_expr = Spanned {
                span: Span(base_expr.span.0, base_expr.span.1),
                data: Expression::BinaryOperator(Box::new(base_expr), Box::new(rhs_expr), op)
            }
        }
        Ok(base_expr)
    }

    fn base_expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut expression =
            if next_token_one_of!(self.lexer,
                                  BooleanLiteral(_),
                                  IntLiteral(_),
                                  FloatLiteral(_),
                                  StringLiteral(_)) {
                let literal = try!(self.literal());
                Spanned {
                    span: literal.span,
                    data: Expression::Literal(literal)
                }
            } else if next_token_one_of!(self.lexer, Identifier(_)) {
                // expression ::= identifier
                let ident = try!(self.identifier());
                Spanned {
                    span: ident.span,
                    data: Expression::Identifier(ident)
                }
            } else if next_token_one_of!(self.lexer, Ref) {
                // expression ::= REF expression
                let Span(start, _) = try!(self.parse_token(Ref));
                let expr = try!(self.expression());
                Spanned {
                    span: Span(start, expr.span.1),
                    data: Expression::Ref(Box::new(expr))
                }
            } else if next_token_one_of!(self.lexer, If) {
                try!(self.if_then_else())
            } else if next_token_one_of!(self.lexer, VBar) {
                try!(self.lambda())
            } else if next_token_one_of!(self.lexer,
                                         Not,
                                         Minus,
                                         Deref) {
                try!(self.unary_op())
            } else if next_token_one_of!(self.lexer, Let) {
                try!(self._let())
            } else if next_token_one_of!(self.lexer, LParen) {
                try!(self.tuple())
            } else {
                let (span, token_str) = match self.lexer.next() {
                    Some(maybe_t) => {
                        let t = try!(maybe_t);
                        (t.span, format!("{:?}", t.variant))
                    },
                    None => (self.last_span, format!("<EOF>"))
                };
                return self.span_err(span, format!("expected one of a literal, identifier, `(`, `ref`, `if`, `|`, `-`, `not`, `!`, or `let`, got {}", token_str));
            };
        // next up are the possible suffixes to an expression.
        loop {
            if next_token_one_of!(self.lexer, LParen) {
                expression = try!(self.function_call(expression))
            } else if next_token_one_of!(self.lexer, Dot) {
                expression = try!(self.postfix_function_call(expression))
            } else if next_token_one_of!(self.lexer, LeftArrow) {
                expression = try!(self.assignment(expression))
            } else {
                // a suffix is optional.
                break;
            }
        }
        Ok(expression)
    }

    fn literal(&mut self) -> ParseResult<SpannedLiteral> {
        // at this point we've verified that the iterator is parked
        // at a literal.
        match try!(self.lexer.next().unwrap()) {
            Token { variant: BooleanLiteral(b), span: s } => {
                self.last_span = s;
                Ok(Spanned {
                    span: s,
                    data: Literal::Bool(b)
                })
            },
            Token { variant: IntLiteral(i), span: s} => {
                self.last_span = s;
                Ok(Spanned {
                    span: s,
                    data: Literal::Int(i)
                })
            },
            Token { variant: FloatLiteral(f), span: s} => {
                self.last_span = s;
                Ok(Spanned {
                    span: s,
                    data: Literal::Float(f)
                })
            },
            Token{ variant: StringLiteral(ref l), span: s} => {
                self.last_span = s;
                Ok(Spanned {
                    span: s,
                    data: Literal::String(l.clone())
                })
            },
            _ => unreachable!()
        }
    }

    fn if_then_else(&mut self) -> ParseResult<SpannedExpression> {
        let Span(start, _) = try!(self.parse_token(If));
        let condition = try!(self.expression());
        let _ = try!(self.parse_token(Then));
        let true_branch = try!(self.expression());
        if next_token_one_of!(self.lexer, End) {
            let Span(_, end) = try!(self.parse_token(End));
            return Ok(Spanned {
                span: Span(start, end),
                data: Expression::IfThen(Box::new(condition), Box::new(true_branch))
            });
        }
        let _ = try!(self.parse_token(Else));
        let false_branch = try!(self.expression());
        let Span(_, end) = try!(self.parse_token(End));
        Ok(Spanned {
            span: Span(start, end),
            data: Expression::IfThenElse(Box::new(condition), Box::new(true_branch), Box::new(false_branch))
        })
    }

    fn lambda(&mut self) -> ParseResult<SpannedExpression> {
        let Span(start, _) = try!(self.parse_token(VBar));
        let params = try!(self.comma_sep_ident());
        let _ = try!(self.parse_token(VBar));
        let expr_or_block = if next_token_one_of!(self.lexer, Do) {
            let SpannedBlock { span: s, data: Block(expr)} = try!(self.block());
            Spanned {
                span: s,
                data: ExpressionOrBlock::Block(expr)
            }
        } else {
            let expr = try!(self.expression());
            Spanned {
                span: expr.span,
                data: ExpressionOrBlock::Expr(expr)
            }
        };
        Ok(Spanned {
            span: Span(start, expr_or_block.span.1),
            data: Expression::Lambda(params, Box::new(expr_or_block))
        })
    }

    fn _let(&mut self) -> ParseResult<SpannedExpression> {
        let Span(start, _) = try!(self.parse_token(Let));
        let pattern = try!(self.pattern());
        let _ = try!(self.parse_token(SingleEq));
        let binding = try!(self.expression());
        let _ = try!(self.parse_token(In));
        let body = try!(self.expression());
        Ok(Spanned {
            span: Span(start, body.span.1),
            data: Expression::Let(pattern, Box::new(binding), Box::new(body))
        })
    }

    fn tuple(&mut self) -> ParseResult<SpannedExpression> {
        let mut vec = vec![];
        let Span(start, _) = try!(self.parse_token(LParen));
        let first = try!(self.expression());
        vec.push(first);
        loop {
            if !next_token_one_of!(self.lexer, Comma) {
                break;
            }
            let _ = try!(self.parse_token(Comma));
            let next = try!(self.expression());
            vec.push(next);
        }
        let Span(_, stop) = try!(self.parse_token(RParen));
        if vec.len() == 1 {
            Ok(Spanned {
                span: Span(start, stop),
                data: Expression::Paren(Box::new(vec.remove(0)))
            })
        } else {
            Ok(Spanned {
                span: Span(start, stop),
                data: Expression::TupleCreation(vec)
            })
        }
    }

    fn pattern(&mut self) -> ParseResult<SpannedPattern> {
        if next_token_one_of!(self.lexer, Identifier(_)) {
            let ident = try!(self.identifier());
            return Ok(Spanned {
                span: ident.span,
                data: Pattern::Ident(ident)
            });
        }
        let Span(start, _) = try!(self.parse_token(LParen));
        let fields = try!(self.comma_sep_ident());
        let Span(_, stop) = try!(self.parse_token(RParen));
        Ok(Spanned {
            span: Span(start, stop),
            data: Pattern::TupleDestructure(fields)
        })
    }

    fn function_call(&mut self, base: SpannedExpression) -> ParseResult<SpannedExpression> {
        let _ = try!(self.parse_token(LParen));
        if next_token_one_of!(self.lexer, RParen) {
            let Span(_, stop) = try!(self.parse_token(RParen));
            return Ok(Spanned {
                span: Span(base.span.0, stop),
                data: Expression::FunctionCall(Box::new(base), vec![])
            });
        }
        // otherwise, there are one or more parameters to parse.
        let parameters = try!(self.nonempty_actual_parameters());
        let Span(_, stop) = try!(self.parse_token(RParen));
        Ok(Spanned {
            span: Span(base.span.0, stop),
            data: Expression::FunctionCall(Box::new(base), parameters)
        })
    }

    fn nonempty_actual_parameters(&mut self) -> ParseResult<Vec<SpannedExpression>> {
        let mut parameters = vec![];
        parameters.push(try!(self.expression()));
        loop {
            if next_token_one_of!(self.lexer, RParen) {
                break;
            }
            let _ = try!(self.parse_token(Comma));
            parameters.push(try!(self.expression()));
        }
        Ok(parameters)
    }

    fn postfix_function_call(&mut self, base: SpannedExpression) -> ParseResult<SpannedExpression> {
        let _ = try!(self.parse_token(Dot));
        let ident = try!(self.identifier());
        if !next_token_one_of!(self.lexer, LParen) {
            return Ok(Spanned {
                span: Span(base.span.0, ident.span.1),
                data: Expression::PostfixFunctionCall(Box::new(base), ident, vec![])
            });
        }
        let _ = try!(self.parse_token(LParen));
        let parameters = try!(self.nonempty_actual_parameters());
        let Span(_, stop) = try!(self.parse_token(RParen));
        Ok(Spanned {
            span: Span(base.span.0, stop),
            data: Expression::PostfixFunctionCall(Box::new(base), ident, parameters)
        })
    }

    fn assignment(&mut self, base: SpannedExpression) -> ParseResult<SpannedExpression> {
        let _ = try!(self.parse_token(LeftArrow));
        let expr = try!(self.expression());
        Ok(Spanned {
            span: Span(base.span.0, expr.span.1),
            data: Expression::Assign(Box::new(base), Box::new(expr))
        })
    }

    fn unary_op(&mut self) -> ParseResult<SpannedExpression> {
        let tok = try!(self.lexer.next().unwrap());
        let op = op_token_to_unop(tok.variant);
        let expr = try!(self.base_expression());
        Ok(Spanned {
            span: Span(tok.span.0, expr.span.1),
            data: Expression::UnaryOperator(Box::new(expr), op)
        })
    }

    fn parse_token(&mut self, tok: TokenType) -> ParseResult<Span> {
        match self.lexer.next() {
            Some(possible_token) => {
                let t = try!(possible_token);
                if t.variant == tok {
                    self.last_span = t.span;
                    Ok(self.last_span)
                } else {
                    self.span_err(t.span, format!("expected {:?}, got {:?}", tok, t.variant))
                }
            },
            None => self.span_err(self.last_span, format!("expected {:?}, got <EOF>", tok))
        }
    }

    pub fn assert_eof(&mut self) -> ParseResult<()> {
        match self.lexer.next() {
            Some(t) => {
                let tok = try!(t);
                self.span_err(tok.span, format!("expected <EOF>, got {:?}", tok.variant))
            },
            None => Ok(())
        }
    }
}

fn op_token_to_binop(t: TokenType) -> Binop {
    match t {
        DoubleEq => Binop::Equal,
        LessThanEq => Binop::LessThanEq,
        LessThan => Binop::LessThan,
        GreaterThanEq => Binop::GreaterThanEq,
        GreaterThan => Binop::GreaterThan,
        Plus => Binop::Plus,
        Minus => Binop::Minus,
        Times => Binop::Mul,
        Div => Binop::Div,
        NotEqual => Binop::NotEqual,
        And => Binop::And,
        Or => Binop::Or,
        _ => unreachable!()
    }
}

fn op_token_to_unop(t: TokenType) -> Unop {
    match t {
        Deref => Unop::Dereference,
        Not => Unop::Not,
        Minus => Unop::Negate,
        _ => unreachable!()
    }
}


