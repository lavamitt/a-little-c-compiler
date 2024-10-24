use crate::lexer::Token;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum ASTUnaryOperator {
    Negation,
    BitwiseComplement,
    Not,
}

#[derive(Debug)]
pub enum ASTBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    IsEqual,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Equal
}

#[derive(Debug)]
pub enum ASTExpression {
    Constant(u32),
    Var(String),
    UnaryOperation(ASTUnaryOperator, Box<ASTExpression>),
    BinaryOperation(ASTBinaryOperator, Box<ASTExpression>, Box<ASTExpression>),
    Assignment(Box<ASTExpression>, Box<ASTExpression>), // lvalue = expression
}

#[derive(Debug)]
pub enum ASTStatement {
    Return(ASTExpression),
    Expression(ASTExpression),
    Null,
}

#[derive(Debug)]
pub struct ASTVariableDeclaration {
    pub name: String,
    pub init: Option<ASTExpression>
}

#[derive(Debug)]
pub enum ASTBlockItem {
    Statement(ASTStatement),
    VariableDeclaration(ASTVariableDeclaration),
}

#[derive(Debug)]
pub struct ASTFunctionDefinition {
    pub name: String,
    pub body: Vec<ASTBlockItem>,
}

#[derive(Debug)]
pub struct ASTProgram {
    pub function: ASTFunctionDefinition,
}

// <program> ::= <function>
pub fn parse_program<'a, I>(tokens: Peekable<I>) -> ASTProgram
where
    I: Iterator<Item = &'a Token>,
{
    let (function, _) = parse_function(tokens);
    ASTProgram { function }
}

// <function> ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
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

    let mut function_body: Vec<ASTBlockItem> = Vec::new();
    while let Some(token) = tokens.peek() {
        if token == &&Token::CloseBrace {
            break;
        }
    
        let (next_item, new_tokens) = parse_block_item(tokens);
    
        function_body.push(next_item);
        tokens = new_tokens;
    }

    expect_token(tokens.next(), &Token::CloseBrace);

    // Check for remaining tokens
    if tokens.next().is_some() {
        panic!("Unexpected tokens after function definition");
    }

    (
        ASTFunctionDefinition {
            name: identifier,
            body: function_body,
        },
        tokens,
    )
}

// <block-item> ::= <statement> | <declaration>
fn parse_block_item<'a, I>(mut tokens: Peekable<I>) -> (ASTBlockItem, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    if tokens.peek() == Some(&&Token::IntKeyword) {
        let (declaration, tokens) = parse_declaration(tokens);
        (ASTBlockItem::VariableDeclaration(declaration), tokens)
    } else {
        let (statement, tokens) = parse_statement(tokens);
        (ASTBlockItem::Statement(statement), tokens)
    }
}

// <declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
fn parse_declaration<'a, I>(mut tokens: Peekable<I>) -> (ASTVariableDeclaration, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    expect_token(tokens.next(), &Token::IntKeyword);
    let variable_name = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Variable name must be a string literal"),
    };
    match tokens.peek() {
        Some(Token::Equal) => {
            tokens.next();
            let (expr, tokens) = parse_expr(tokens, 0);
            (ASTVariableDeclaration {
                name: variable_name,
                init: Some(expr)
            }, tokens)
        }
        Some(Token::Semicolon) => {
            tokens.next();
            (ASTVariableDeclaration {
                name: variable_name,
                init: None
            }, tokens)
        }
        unknown_token => panic!("Unexpected token after variable declaration: {:?}", unknown_token)
    }
}

// <statement> ::= "return" <exp> ";" | <exp> ";" | ";"
fn parse_statement<'a, I>(mut tokens: Peekable<I>) -> (ASTStatement, Peekable<I>)
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.peek() {
        Some(&Token::ReturnKeyword) => {
            tokens.next();
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTStatement::Return(expr), tokens)
        }
        Some(&Token::Semicolon) => {
            tokens.next();
            (ASTStatement::Null, tokens)
        }
        _ => {
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTStatement::Expression(expr), tokens)
        }
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
                match binary_op {
                    ASTBinaryOperator::Equal => {
                        let right_and_tokens = parse_expr(tokens, curr_precedence);
                        let right = right_and_tokens.0;
                        tokens = right_and_tokens.1;
                        left = ASTExpression::Assignment(Box::new(left), Box::new(right));
                    }
                    _ => {
                        let right_and_tokens = parse_expr(tokens, curr_precedence + 1);
                        let right = right_and_tokens.0;
                        tokens = right_and_tokens.1;
                        left = ASTExpression::BinaryOperation(binary_op, Box::new(left), Box::new(right));
                    }
                }
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
        Token::LogicalAnd => Some(ASTBinaryOperator::And),
        Token::LogicalOr => Some(ASTBinaryOperator::Or),
        Token::EqualTo => Some(ASTBinaryOperator::IsEqual),
        Token::NotEqualTo => Some(ASTBinaryOperator::NotEqual),
        Token::LessThan => Some(ASTBinaryOperator::LessThan),
        Token::LessThanOrEqualTo => Some(ASTBinaryOperator::LessOrEqual),
        Token::GreaterThan => Some(ASTBinaryOperator::GreaterThan),
        Token::GreaterThanOrEqualTo => Some(ASTBinaryOperator::GreaterOrEqual),
        Token::Equal => Some(ASTBinaryOperator::Equal),
        _ => None,
    }
}

fn precedence(binary_op: &ASTBinaryOperator) -> u32 {
    match binary_op {
        ASTBinaryOperator::Multiply => 50,
        ASTBinaryOperator::Divide => 50,
        ASTBinaryOperator::Remainder => 50,
        ASTBinaryOperator::Add => 45,
        ASTBinaryOperator::Subtract => 45,
        ASTBinaryOperator::LessThan => 35,
        ASTBinaryOperator::LessOrEqual => 35,
        ASTBinaryOperator::GreaterThan => 35,
        ASTBinaryOperator::GreaterOrEqual => 35,
        ASTBinaryOperator::IsEqual => 30,
        ASTBinaryOperator::NotEqual => 30,
        ASTBinaryOperator::And => 10,
        ASTBinaryOperator::Or => 5,
        ASTBinaryOperator::Equal => 1,
        _ => unreachable!(), // This should never happen
    }
}

// <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"
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
                Token::LogicalNegation => ASTUnaryOperator::Not,
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
        Some(Token::Identifier(name)) => {
            tokens.next();
            (ASTExpression::Var(name.clone()), tokens)
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
