use crate::lexer::Token;

#[derive(Debug)]
pub enum Expression {
    Constant(u32),
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

pub fn parse_program<'a, I>(mut tokens: I) -> Program
where
    I: Iterator<Item = &'a Token>,
{
    Program {
        function: parse_function(tokens.by_ref()),
    }
}

fn parse_function<'a, I>(mut tokens: I) -> FunctionDeclaration
where
    I: Iterator<Item = &'a Token>,
{
    let mut current_token = tokens.next();
    match current_token {
        Some(Token::IntKeyword) => {}
        _ => panic!("Function must start with int"),
    }

    current_token = tokens.next();
    let identifier: String = match current_token {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Function name must be a string literal"),
    };

    current_token = tokens.next();
    match current_token {
        Some(Token::OpenParen) => {}
        _ => panic!("Open paren must follow function name"),
    };

    current_token = tokens.next();
    match current_token {
        Some(Token::CloseParen) => {}
        _ => panic!("Closing paren must follow function name"),
    };

    current_token = tokens.next();
    match current_token {
        Some(Token::OpenBrace) => {}
        _ => panic!("Function body must start with open brace"),
    };

    let statement: Statement = parse_statement(tokens.by_ref());

    current_token = tokens.next();
    match current_token {
        Some(Token::CloseBrace) => {}
        _ => panic!("Function body must end with close brace"),
    };

    FunctionDeclaration {
        name: identifier,
        body: statement,
    }
}

fn parse_statement<'a, I>(mut tokens: I) -> Statement
where
    I: Iterator<Item = &'a Token>,
{
    let mut current_token = tokens.next();
    let statement: Statement = match current_token {
        Some(Token::ReturnKeyword) => Statement::Return(parse_expr(tokens.by_ref())),
        _ => panic!("Statements must contain return"),
    };

    current_token = tokens.next();
    match current_token {
        Some(Token::Semicolon) => {}
        _ => panic!("Statement must end in a semicolon"),
    };

    statement
}

fn parse_expr<'a, I>(mut tokens: I) -> Expression
where
    I: Iterator<Item = &'a Token>,
{
    let current_token = tokens.next();
    let expression: Expression = match current_token {
        Some(Token::IntegerLiteral(int)) => Expression::Constant(*int),
        _ => panic!("Expressions must be integers"),
    };

    expression
}
