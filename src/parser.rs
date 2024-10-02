use crate::lexer::Token;

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum Expression {
    Constant(u32),
    UnaryOperation(UnaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Program {
    pub function: FunctionDeclaration,
}

pub fn parse_program<'a, I>(tokens: I) -> Program
where
    I: Iterator<Item = &'a Token>,
{
    let (function, _) = parse_function(tokens);
    Program { function }
}

fn parse_function<'a, I>(mut tokens: I) -> (FunctionDeclaration, I)
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.next() {
        Some(Token::IntKeyword) => {}
        _ => panic!("Function must start with int"),
    }

    let identifier = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Function name must be a string literal"),
    };

    match tokens.next() {
        Some(Token::OpenParen) => {}
        _ => panic!("Open paren must follow function name"),
    };

    match tokens.next() {
        Some(Token::CloseParen) => {}
        _ => panic!("Closing paren must follow function name"),
    };

    match tokens.next() {
        Some(Token::OpenBrace) => {}
        _ => panic!("Function body must start with open brace"),
    };

    let (statement, mut tokens) = parse_statement(tokens);

    match tokens.next() {
        Some(Token::CloseBrace) => {}
        _ => panic!("Function body must end with close brace"),
    };

    (
        FunctionDeclaration {
            name: identifier,
            body: statement,
        },
        tokens,
    )
}

fn parse_statement<'a, I>(mut tokens: I) -> (Statement, I)
where
    I: Iterator<Item = &'a Token>,
{
    let (statement, mut tokens) = match tokens.next() {
        Some(Token::ReturnKeyword) => {
            let (expr, tokens) = parse_expr(tokens);
            (Statement::Return(expr), tokens)
        }
        _ => panic!("Statements must contain return"),
    };

    match tokens.next() {
        Some(Token::Semicolon) => {}
        _ => panic!("Statement must end in a semicolon"),
    };

    (statement, tokens)
}

fn parse_expr<'a, I>(mut tokens: I) -> (Expression, I)
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.next() {
        Some(Token::IntegerLiteral(int)) => (Expression::Constant(*int), tokens),
        Some(Token::Negation) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                Expression::UnaryOperation(UnaryOperator::Negation, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::BitwiseComplement) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                Expression::UnaryOperation(UnaryOperator::BitwiseComplement, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::LogicalNegation) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                Expression::UnaryOperation(UnaryOperator::LogicalNegation, Box::new(expr)),
                tokens,
            )
        }
        _ => panic!("Did not find a way to parse the expression"),
    }
}
