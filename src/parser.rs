use crate::lexer::Token;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum ASTUnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum ASTBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub enum ASTExpression {
    Constant(u32),
    UnaryOperation(ASTUnaryOperator, Box<ASTExpression>),
    BinaryOperation(ASTBinaryOperator, Box<ASTExpression>, Box<ASTExpression>),
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

pub fn parse_program<'a, I>(tokens: Peekable<I>) -> ASTProgram
where
    I: Iterator<Item = &'a Token>,
{
    let (function, _) = parse_function(tokens);
    ASTProgram { function }
}

fn parse_function<'a, I>(mut tokens: Peekable<I>) -> (ASTFunctionDefinition, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    expect_token(tokens.next(), &Token::IntKeyword);
    let identifier = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Function name must be a string literal"),
    };
    expect_token(tokens.next(), &Token::OpenParen);
    expect_token(tokens.next(), &Token::Identifier("void".to_string()));
    expect_token(tokens.next(), &Token::CloseParen);
    expect_token(tokens.next(), &Token::OpenBrace);

    let (statement, mut tokens) = parse_statement(tokens);

    expect_token(tokens.next(), &Token::CloseBrace);

    // Check for remaining tokens
    if tokens.next().is_some() {
        panic!("Unexpected tokens after function definition");
    }

    (
        ASTFunctionDefinition {
            name: identifier,
            body: statement,
        },
        tokens,
    )
}

fn parse_statement<'a, I>(mut tokens: Peekable<I>) -> (ASTStatement, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    expect_token(tokens.next(), &Token::ReturnKeyword);
    let (expr, mut tokens) = parse_expr(tokens, 0);
    expect_token(tokens.next(), &Token::Semicolon);

    (ASTStatement::Return(expr), tokens)
}

// <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
fn parse_factor<'a, I>(mut tokens: Peekable<I>) -> (ASTExpression, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    let next_token = tokens.peek();
    match next_token {
        Some(Token::IntegerLiteral(int)) => {
            tokens.next();
            (ASTExpression::Constant(*int), tokens)
        }
        Some(token @ (Token::Negation | Token::BitwiseComplement | Token::LogicalNegation)) => {
            let operator = match token {
                Token::Negation => ASTUnaryOperator::Negation,
                Token::BitwiseComplement => ASTUnaryOperator::BitwiseComplement,
                Token::LogicalNegation => ASTUnaryOperator::LogicalNegation,
                _ => unreachable!(), // This should never happen
            };

            tokens.next();
            let (expr, tokens) = parse_factor(tokens);
            (
                ASTExpression::UnaryOperation(operator, Box::new(expr)),
                tokens,
            )
        }
        Some(Token::OpenParen) => {
            tokens.next();
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::CloseParen);
            (expr, tokens)
        }
        _ => panic!("Did not find a way to parse the expression"),
    }
}

// <exp> ::= <factor> | <exp> <binop> <exp>
fn parse_expr<'a, I>(mut tokens: Peekable<I>, min_precedence: u32) -> (ASTExpression, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    let (mut left, mut tokens) = parse_factor(tokens);
    while let Some(token) = tokens.peek() {
        if let Some(binary_op) = token_to_binary_op(token) {
            let curr_precedence = precedence(&binary_op);
            if curr_precedence >= min_precedence {
                tokens.next(); // snip off binary token
                let right_and_tokens = parse_expr(tokens, curr_precedence + 1);
                let right = right_and_tokens.0;
                tokens = right_and_tokens.1;
                left = ASTExpression::BinaryOperation(binary_op, Box::new(left), Box::new(right));
            } else {
                break;
            }
        } else {
            break;
        }
    }

    (left, tokens)
}

fn token_to_binary_op(token: &Token) -> Option<ASTBinaryOperator> {
    match token {
        Token::Addition => Some(ASTBinaryOperator::Add),
        Token::Negation => Some(ASTBinaryOperator::Subtract),
        Token::Multiplication => Some(ASTBinaryOperator::Multiply),
        Token::Division => Some(ASTBinaryOperator::Divide),
        Token::Remainder => Some(ASTBinaryOperator::Remainder),
        _ => None,
    }
}

fn precedence(binary_op: &ASTBinaryOperator) -> u32 {
    match binary_op {
        ASTBinaryOperator::Add => 45,
        ASTBinaryOperator::Subtract => 45,
        ASTBinaryOperator::Multiply => 50,
        ASTBinaryOperator::Divide => 50,
        ASTBinaryOperator::Remainder => 50,
        _ => unreachable!(), // This should never happen
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
