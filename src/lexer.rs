use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    IntKeyword,
    ReturnKeyword,
    DoKeyword,
    WhileKeyword,
    ForKeyword,
    BreakKeyword,
    ContinueKeyword,
    IfKeyword,
    ElseKeyword,
    QuestionMark,
    Colon,
    Identifier(String),
    IntegerLiteral(u32),
    Equal,                // =
    Addition,             // +
    Multiplication,       // *
    Division,             // /
    Remainder,            // %
    Negation,             // -
    Decrement,            // --
    BitwiseComplement,    // ~
    LogicalNegation,      // !
    LogicalAnd,           // &&
    LogicalOr,            // ||
    EqualTo,              // ==
    NotEqualTo,           // !=
    LessThan,             // <
    GreaterThan,          // >
    LessThanOrEqualTo,    // <=
    GreaterThanOrEqualTo, // >=
}

pub fn is_keyword(word: &str) -> Option<Token> {
    match word {
        "int" => Some(Token::IntKeyword),
        "return" => Some(Token::ReturnKeyword),
        "do" => Some(Token::DoKeyword),
        "while" => Some(Token::WhileKeyword),
        "for" => Some(Token::ForKeyword),
        "break" => Some(Token::BreakKeyword),
        "continue" => Some(Token::ContinueKeyword),
        "if" => Some(Token::IfKeyword),
        "else" => Some(Token::ElseKeyword),
        _ => None,
    }
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
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
            } else if c.is_alphanumeric() || c == '_' {
                panic!("Expected integer literal to end with a word boundary but found character: {:?}", c.to_string())
            } else {
                break;
            }
        }

        match literal.parse::<u32>() {
            Ok(num) => Token::IntegerLiteral(num),
            Err(_) => panic!("Bad identifier: {:?}", literal), // Handle any unexpected cases
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.next_char() {
            let token = match c {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                ';' => Token::Semicolon,
                '?' => Token::QuestionMark,
                ':' => Token::Colon,
                '+' => Token::Addition,
                '*' => Token::Multiplication,
                '/' => Token::Division,
                '%' => Token::Remainder,
                '-' if self.peek_char() == Some(&'-') => {
                    self.next_char();
                    Token::Decrement
                }
                '-' => Token::Negation,
                '~' => Token::BitwiseComplement,
                '&' if self.peek_char() == Some(&'&') => {
                    self.next_char();
                    Token::LogicalAnd
                }
                '|' if self.peek_char() == Some(&'|') => {
                    self.next_char();
                    Token::LogicalOr
                }
                '=' if self.peek_char() == Some(&'=') => {
                    self.next_char();
                    Token::EqualTo
                }
                '=' => Token::Equal,
                '!' if self.peek_char() == Some(&'=') => {
                    self.next_char();
                    Token::NotEqualTo
                }
                '!' => Token::LogicalNegation,
                '<' if self.peek_char() == Some(&'=') => {
                    self.next_char();
                    Token::LessThanOrEqualTo
                }
                '<' => Token::LessThan,
                '>' if self.peek_char() == Some(&'=') => {
                    self.next_char();
                    Token::GreaterThanOrEqualTo
                }
                '>' => Token::GreaterThan,
                c if c.is_whitespace() => continue,
                c if c.is_alphabetic() => self.lex_identifier(c),
                c if c.is_numeric() => self.lex_integer_literal(c),
                c => panic!("Found unexpected character: {}", c.to_string()),
            };
            tokens.push(token);
        }

        tokens
    }
}
