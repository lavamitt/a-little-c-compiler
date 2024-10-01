use std::env;
use std::fs;
use std::iter::Peekable;
use std::slice::Iter;
use std::str::Chars;

// LEXER

#[derive(Debug)]
enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    IntKeyword,
    ReturnKeyword,
    Identifier(String),
    IntegerLiteral(u32),
    Unknown(String), // To handle unexpected tokens
}

fn is_keyword(word: &str) -> Option<Token> {
    match word {
        "int" => Some(Token::IntKeyword),
        "return" => Some(Token::ReturnKeyword),
        _ => None,
    }
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn lex_identifier(&mut self, first_char: char) -> Token {
        let mut identifier = first_char.to_string();

        while let Some(&c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(self.next_char().unwrap());
            } else {
                break;
            }
        }

        if let Some(keyword_token) = is_keyword(&identifier) {
            keyword_token
        } else {
            Token::Identifier(identifier)
        }
    }

    fn lex_integer_literal(&mut self, first_digit: char) -> Token {
        let mut literal = first_digit.to_string();

        while let Some(&c) = self.peek_char() {
            if c.is_numeric() {
                literal.push(self.next_char().unwrap());
            } else {
                break;
            }
        }

        match literal.parse::<u32>() {
            Ok(num) => Token::IntegerLiteral(num),
            Err(_) => Token::Unknown(literal), // Handle any unexpected cases
        }
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.next_char() {
            let token = match c {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                ';' => Token::Semicolon,
                c if c.is_whitespace() => continue,
                c if c.is_alphabetic() => self.lex_identifier(c),
                c if c.is_numeric() => self.lex_integer_literal(c),
                c => Token::Unknown(c.to_string()),
            };
            tokens.push(token);
        }

        tokens
    }
}

// PARSER

#[derive(Debug)]
enum Expression {
    Constant(u32),
}

#[derive(Debug)]
enum Statement {
    Return(Expression),
}

#[derive(Debug)]
struct FunctionDeclaration {
    name: String,
    body: Statement,
}

#[derive(Debug)]
struct Program {
    function: FunctionDeclaration,
}

fn parse_program<'a, I>(mut tokens: I) -> Program
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

// CODEGEN

fn codegen(program: Program, assembly: &mut String) {
    codegen_function(program.function, assembly);
}

fn codegen_function(function: FunctionDeclaration, assembly: &mut String) {
    assembly.push_str(format!(".globl {}\n", function.name).as_str());
    assembly.push_str(format!("_{}:\n", function.name).as_str());

    codegen_statement(function.body, assembly);
}

fn codegen_statement(statement: Statement, assembly: &mut String) {
    match statement {
        Statement::Return(expr) => {
            assembly.push_str("movl ");
            codegen_expression(expr, assembly);
            assembly.push_str(", %eax\n");
            assembly.push_str("retq\n");
        }
        _ => panic!("Found unknown statement type"),
    };
}

fn codegen_expression(expression: Expression, assembly: &mut String) {
    let constant = match expression {
        Expression::Constant(num) => {
            format!("${}", num)
        }
        _ => panic!("Found unknown expression"),
    };

    assembly.push_str(constant.as_str());
}

// MAIN

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let code = fs::read_to_string(file_path).unwrap();
    let mut lexer = Lexer::new(&code);
    let mut tokens = lexer.lex();

    for token in &tokens {
        println!("{:?}", token);
    }

    let program: Program = parse_program(tokens.iter());
    println!("{:?}", program);

    let mut assembly = String::new();
    codegen(program, &mut assembly);
    println!("{assembly}");
}
