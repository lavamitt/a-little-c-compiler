use crate::lexer::Token;
use itertools::peek_nth;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum ASTUnaryOperator {
    Negation,
    BitwiseComplement,
    Not,
}

#[derive(Debug, Clone)]
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
    Equal,
    QuestionMark, // a ? b : c
}

#[derive(Debug, Clone)]
pub enum ASTExpression {
    Constant(u32),
    Var(String),
    UnaryOperation(ASTUnaryOperator, Box<ASTExpression>),
    BinaryOperation(ASTBinaryOperator, Box<ASTExpression>, Box<ASTExpression>),
    Assignment(Box<ASTExpression>, Box<ASTExpression>), // lvalue = expression
    Conditional(Box<ASTExpression>, Box<ASTExpression>, Box<ASTExpression>), // condition, then, else
    FunctionCall(String, Vec<ASTExpression>),                                // identifier, args
}

#[derive(Debug, Clone)]
pub enum ASTStatement {
    Return(ASTExpression),
    Expression(ASTExpression),
    If(ASTExpression, Box<ASTStatement>, Option<Box<ASTStatement>>), // condition, then, else
    Compound(ASTBlock),
    DoWhile(Box<ASTStatement>, ASTExpression, Option<String>), // body, condition, label
    While(ASTExpression, Box<ASTStatement>, Option<String>),   // condition, body, label
    For(
        ASTForInit,
        Option<ASTExpression>,
        Option<ASTExpression>,
        Box<ASTStatement>,
        Option<String>,
    ), // init, condition, post, body, label
    Break(Option<String>),                                     // label
    Continue(Option<String>),                                  // label
    Null,
}

#[derive(Debug, Clone)]
pub enum ASTForInit {
    InitDecl(ASTVariableDeclaration),
    InitExpr(Option<ASTExpression>),
}

#[derive(Debug, Clone)]
pub struct ASTVariableDeclaration {
    pub name: String,
    pub init: Option<ASTExpression>,
}

#[derive(Debug, Clone)]
pub enum ASTBlockItem {
    Statement(ASTStatement),
    VariableDeclaration(ASTVariableDeclaration),
    FunctionDeclaration(ASTFunctionDeclaration),
}

#[derive(Debug, Clone)]
pub struct ASTBlock {
    pub items: Vec<ASTBlockItem>,
}

#[derive(Debug, Clone)]
pub struct ASTFunctionDeclaration {
    pub name: String,
    pub args: Vec<String>,
    pub body: Option<ASTBlock>,
}

#[derive(Debug)]
pub struct ASTProgram {
    pub functions: Vec<ASTFunctionDeclaration>,
}

// <program> ::= <function>
pub fn parse_program<'a, I>(mut tokens: Peekable<I>) -> ASTProgram
where
    I: Iterator<Item = &'a Token> + Clone,
{
    let mut functions: Vec<ASTFunctionDeclaration> = Vec::new();
    while tokens.peek().is_some() {
        let (function, _tokens) = parse_function_declaration(tokens);
        functions.push(function);
        tokens = _tokens;
    }

    ASTProgram { functions }
}

// <function> ::= "int" <identifier> "(" <param-list> ")" ( <block> | ";")
// <param-list> ::= "void" | "int" <identifier> { "," "int" <identifier> }
fn parse_function_declaration<'a, I>(
    mut tokens: Peekable<I>,
) -> (ASTFunctionDeclaration, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    expect_token(tokens.next(), &Token::IntKeyword);
    let identifier = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Function name must be a string literal"),
    };
    expect_token(tokens.next(), &Token::OpenParen);

    let mut args: Vec<String> = Vec::new();
    if tokens.peek() == Some(&&Token::Identifier("void".to_string())) {
        tokens.next(); // eat up void
    } else {
        loop {
            expect_token(tokens.next(), &Token::IntKeyword);
            let identifier = match tokens.next() {
                Some(Token::Identifier(function_name)) => function_name.to_string(),
                _ => panic!("Argument name must be a string literal"),
            };
            args.push(identifier);

            if tokens.peek() == Some(&&Token::CloseParen) {
                break;
            }
            expect_token(tokens.next(), &Token::Comma);
        }
    }

    expect_token(tokens.next(), &Token::CloseParen);

    let mut function_body = None;
    if tokens.peek() == Some(&&Token::Semicolon) {
        tokens.next(); // eat the semicolon
    } else {
        let (_body, _tokens) = parse_block(tokens);
        function_body = Some(_body);
        tokens = _tokens;
    }

    (
        ASTFunctionDeclaration {
            name: identifier,
            args,
            body: function_body,
        },
        tokens,
    )
}

// <block> ::= "{" { <block-item>} "}"
fn parse_block<'a, I>(mut tokens: Peekable<I>) -> (ASTBlock, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    let mut items: Vec<ASTBlockItem> = Vec::new();

    expect_token(tokens.next(), &Token::OpenBrace);
    while let Some(token) = tokens.peek() {
        if token == &&Token::CloseBrace {
            break;
        }

        let (next_item, rest_of_tokens) = parse_block_item(tokens);

        items.push(next_item);
        tokens = rest_of_tokens;
    }
    expect_token(tokens.next(), &Token::CloseBrace);

    (ASTBlock { items }, tokens)
}

// <block-item> ::= <statement> | <variable_declaration> | <function_declaration>
fn parse_block_item<'a, I>(mut tokens: Peekable<I>) -> (ASTBlockItem, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    if tokens.peek() == Some(&&Token::IntKeyword) {
        let is_function = peek_nth(tokens.clone()).peek_nth(2) == Some(&&Token::OpenParen);
        if is_function {
            let (declaration, tokens) = parse_function_declaration(tokens);
            (ASTBlockItem::FunctionDeclaration(declaration), tokens)
        } else {
            let (declaration, tokens) = parse_variable_declaration(tokens);
            (ASTBlockItem::VariableDeclaration(declaration), tokens)
        }
    } else {
        let (statement, tokens) = parse_statement(tokens);
        (ASTBlockItem::Statement(statement), tokens)
    }
}

// <variable_declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
fn parse_variable_declaration<'a, I>(
    mut tokens: Peekable<I>,
) -> (ASTVariableDeclaration, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    expect_token(tokens.next(), &Token::IntKeyword);
    let variable_name = match tokens.next() {
        Some(Token::Identifier(function_name)) => function_name.to_string(),
        _ => panic!("Variable name must be a string literal"),
    };
    match tokens.peek() {
        Some(Token::Equal) => {
            tokens.next();
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::Semicolon);
            (
                ASTVariableDeclaration {
                    name: variable_name,
                    init: Some(expr),
                },
                tokens,
            )
        }
        Some(Token::Semicolon) => {
            tokens.next();
            (
                ASTVariableDeclaration {
                    name: variable_name,
                    init: None,
                },
                tokens,
            )
        }
        unknown_token => panic!(
            "Unexpected token after variable declaration: {:?}",
            unknown_token
        ),
    }
}

// <statement> ::= "return" <exp> ";" | <exp> ";" | <block> | ";"
fn parse_statement<'a, I>(mut tokens: Peekable<I>) -> (ASTStatement, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    match tokens.peek() {
        Some(&Token::ReturnKeyword) => {
            tokens.next();
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTStatement::Return(expr), tokens)
        }
        Some(&Token::IfKeyword) => {
            tokens.next();
            expect_token(tokens.next(), &Token::OpenParen);
            let (condition, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::CloseParen);

            let (then, mut tokens) = parse_statement(tokens);
            if tokens.peek() == Some(&&Token::ElseKeyword) {
                tokens.next();
                let (or_else, tokens) = parse_statement(tokens);
                return (
                    ASTStatement::If(condition, Box::new(then), Some(Box::new(or_else))),
                    tokens,
                );
            }
            (ASTStatement::If(condition, Box::new(then), None), tokens)
        }
        Some(&Token::OpenBrace) => {
            let (block, tokens) = parse_block(tokens);
            (ASTStatement::Compound(block), tokens)
        }
        Some(&Token::ForKeyword) => {
            tokens.next();
            expect_token(tokens.next(), &Token::OpenParen);
            let (init, mut tokens) = parse_for_init(tokens);
            let (condition, mut tokens) = match tokens.peek() {
                Some(&Token::Semicolon) => {
                    tokens.next();
                    (None, tokens)
                }
                _ => {
                    let (expr, mut tokens) = parse_expr(tokens, 0);
                    expect_token(tokens.next(), &Token::Semicolon);
                    (Some(expr), tokens)
                }
            };
            let (post, mut tokens) = match tokens.peek() {
                Some(&Token::CloseParen) => {
                    tokens.next();
                    (None, tokens)
                }
                _ => {
                    let (expr, mut tokens) = parse_expr(tokens, 0);
                    expect_token(tokens.next(), &Token::CloseParen);
                    (Some(expr), tokens)
                }
            };
            let (body, tokens) = parse_statement(tokens);
            (
                ASTStatement::For(init, condition, post, Box::new(body), None),
                tokens,
            )
        }

        Some(&Token::WhileKeyword) => {
            tokens.next();
            expect_token(tokens.next(), &Token::OpenParen);
            let (condition, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::CloseParen);
            let (body, tokens) = parse_statement(tokens);
            (ASTStatement::While(condition, Box::new(body), None), tokens)
        }
        Some(&Token::DoKeyword) => {
            tokens.next();
            let (body, mut tokens) = parse_statement(tokens);
            expect_token(tokens.next(), &Token::WhileKeyword);
            expect_token(tokens.next(), &Token::OpenParen);
            let (condition, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::CloseParen);
            expect_token(tokens.next(), &Token::Semicolon);
            (
                ASTStatement::DoWhile(Box::new(body), condition, None),
                tokens,
            )
        }
        Some(&Token::BreakKeyword) => {
            tokens.next();
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTStatement::Break(None), tokens)
        }
        Some(&Token::ContinueKeyword) => {
            tokens.next();
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTStatement::Continue(None), tokens)
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

fn parse_for_init<'a, I>(mut tokens: Peekable<I>) -> (ASTForInit, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    match tokens.peek() {
        Some(&Token::IntKeyword) => {
            let (decl, tokens) = parse_variable_declaration(tokens);
            (ASTForInit::InitDecl(decl), tokens)
        }
        Some(&Token::Semicolon) => {
            tokens.next();
            (ASTForInit::InitExpr(None), tokens)
        }
        _ => {
            let (expr, mut tokens) = parse_expr(tokens, 0);
            expect_token(tokens.next(), &Token::Semicolon);
            (ASTForInit::InitExpr(Some(expr)), tokens)
        }
    }
}

// <exp> ::= <factor> | <exp> <binop> <exp>
fn parse_expr<'a, I>(mut tokens: Peekable<I>, min_precedence: u32) -> (ASTExpression, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
{
    let (mut left, mut tokens) = parse_factor(tokens);
    while let Some(token) = tokens.peek() {
        if let Some(binary_op) = token_to_binary_op(token) {
            let curr_precedence = precedence(&binary_op);
            if curr_precedence >= min_precedence {
                tokens.next(); // snip off binary token
                match binary_op {
                    ASTBinaryOperator::Equal => {
                        let (right, new_tokens) = parse_expr(tokens, curr_precedence);
                        tokens = new_tokens;
                        left = ASTExpression::Assignment(Box::new(left), Box::new(right));
                    }
                    ASTBinaryOperator::QuestionMark => {
                        let (then, new_tokens) = parse_expr(tokens, 0);
                        tokens = new_tokens;
                        expect_token(tokens.next(), &Token::Colon);
                        let (right, new_tokens) = parse_expr(tokens, curr_precedence);
                        tokens = new_tokens;
                        left = ASTExpression::Conditional(
                            Box::new(left),
                            Box::new(then),
                            Box::new(right),
                        );
                    }
                    _ => {
                        let (right, new_tokens) = parse_expr(tokens, curr_precedence + 1);
                        tokens = new_tokens;
                        left = ASTExpression::BinaryOperation(
                            binary_op,
                            Box::new(left),
                            Box::new(right),
                        );
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
        Token::QuestionMark => Some(ASTBinaryOperator::QuestionMark),
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
        ASTBinaryOperator::QuestionMark => 3,
        ASTBinaryOperator::Equal => 1,
        _ => unreachable!(), // This should never happen
    }
}

// <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"
// | <identifier> "(" [ <argument-list> ] ")"
fn parse_factor<'a, I>(mut tokens: Peekable<I>) -> (ASTExpression, Peekable<I>)
where
    I: Iterator<Item = &'a Token> + Clone,
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

            if tokens.peek() == Some(&&Token::OpenParen) {
                tokens.next(); // eat up open paren
                let mut args: Vec<ASTExpression> = Vec::new();
                if tokens.peek() != Some(&&Token::CloseParen) {
                    loop {
                        let (new_arg, _tokens) = parse_expr(tokens, 0);
                        args.push(new_arg);
                        tokens = _tokens;
                        if tokens.peek() == Some(&&Token::CloseParen) {
                            break;
                        }
                        expect_token(tokens.next(), &Token::Comma);
                    }
                }
                tokens.next(); // eat up close paren

                (ASTExpression::FunctionCall(name.clone(), args), tokens)
            } else {
                (ASTExpression::Var(name.clone()), tokens)
            }
        }
        _ => panic!(
            "Did not find a way to parse the expression: {:?}",
            next_token
        ),
    }
}

fn expect_token(token: Option<&Token>, expected: &Token) {
    match token {
        Some(token) if token == expected => (),
        _ => panic!(
            "Found unexpected token: {:?}, expected: {:?}",
            token, expected
        ),
    }
}
