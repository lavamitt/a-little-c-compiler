use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum ASTUnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum ASTExpression {
    Constant(u32),
    UnaryOperation(ASTUnaryOperator, Box<ASTExpression>),
}

#[derive(Debug)]
pub enum ASTStatement {
    Return(ASTExpression),
}

#[derive(Debug)]
pub struct ASTFunctionDefinition {
    pub name: String,
    pub body: ASTStatement,
}

#[derive(Debug)]
pub struct ASTProgram {
    pub function: ASTFunctionDefinition,
}

pub fn parse_program<'a, I>(tokens: I) -> ASTProgram
where
    I: Iterator<Item = &'a Token>,
{
    let (function, _) = parse_function(tokens);
    ASTProgram { function }
}

fn parse_function<'a, I>(mut tokens: I) -> (ASTFunctionDefinition, I)
where
    I: Iterator<Item = &'a Token>,
{
    expect_token(tokens.next(), &Token::IntKeyword);
    let identifier = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Function name must be a string literal"),
    };
    expect_token(tokens.next(), &Token::OpenParen);
    expect_token(tokens.next(), &Token::CloseParen);
    expect_token(tokens.next(), &Token::OpenBrace);

    let (statement, mut tokens) = parse_statement(tokens);

    expect_token(tokens.next(), &Token::CloseBrace);

    (
        ASTFunctionDefinition {
            name: identifier,
            body: statement,
        },
        tokens,
    )
}

fn parse_statement<'a, I>(mut tokens: I) -> (ASTStatement, I)
where
    I: Iterator<Item = &'a Token>,
{
    expect_token(tokens.next(), &Token::ReturnKeyword);
    let (expr, mut tokens) = parse_expr(tokens);
    expect_token(tokens.next(), &Token::Semicolon);

    (ASTStatement::Return(expr), tokens)
}

fn parse_expr<'a, I>(mut tokens: I) -> (ASTExpression, I)
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.next() {
        Some(Token::IntegerLiteral(int)) => (ASTExpression::Constant(*int), tokens),
        Some(Token::Negation) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                ASTExpression::UnaryOperation(ASTUnaryOperator::Negation, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::BitwiseComplement) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                ASTExpression::UnaryOperation(ASTUnaryOperator::BitwiseComplement, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::LogicalNegation) => {
            let (expr, tokens) = parse_expr(tokens);
            (
                ASTExpression::UnaryOperation(ASTUnaryOperator::LogicalNegation, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::OpenParen) => {
            let (expr, mut tokens) = parse_expr(tokens);
            expect_token(tokens.next(), &Token::CloseParen);
            (expr, tokens)
        }
        _ => panic!("Did not find a way to parse the expression"),
    }
}

fn expect_token(token: Option<&Token>, expected: &Token) {
    match token {
        Some(token) if token == expected => (),
        _ => panic!(
            "Found unexpectated token: {:?}, expected: {:?}",
            token, expected
        ),
    }
}
