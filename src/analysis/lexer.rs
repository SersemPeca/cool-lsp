const MAX_STRING_LENGTH: usize = 1024;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Class,
    Else,
    Fi,
    If,
    In,
    Inherits,
    Isvoid,
    Let,
    Loop,
    Pool,
    Then,
    While,
    Case,
    Esac,
    New,
    Of,
    Not,

    Bool(bool),
    Int(String),
    Str(String),

    TypeId(String),
    ObjectId(String),

    LParen,
    RParen,
    LBrace,
    RBrace,
    Colon,
    Semicolon,
    Comma,
    Dot,
    At,

    Assign, // <-
    Arrow,  // =>

    Plus,
    Minus,
    Star,
    Slash,
    Tilde,

    Lt,
    Le,
    Eq,

    Eof,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexToken {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LexResult {
    pub tokens: Vec<LexToken>,
    pub errors: Vec<LexError>,
}

#[allow(dead_code)]
pub fn lex(input: &str) -> LexResult {
    let mut lexer = Lexer::new(input);
    lexer.scan();
    LexResult {
        tokens: lexer.tokens,
        errors: lexer.errors,
    }
}

#[allow(dead_code)]
struct Lexer<'src> {
    src: &'src str,
    pos: usize,
    line: usize,
    column: usize,
    tokens: Vec<LexToken>,
    errors: Vec<LexError>,
}

impl<'src> Lexer<'src> {
    fn new(src: &'src str) -> Self {
        Self {
            src,
            pos: 0,
            line: 1,
            column: 1,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn scan(&mut self) {
        while self.pos < self.src.len() {
            if self.skip_whitespace_or_comment() {
                continue;
            }

            let Some(ch) = self.peek() else {
                break;
            };

            let line = self.line;
            let column = self.column;

            match ch {
                '"' => match self.lex_string() {
                    Ok((token, token_line, token_column)) => {
                        self.add_token(token_line, token_column, token);
                    }
                    Err(err) => {
                        self.errors.push(err);
                    }
                },
                c if c.is_ascii_digit() => {
                    let token = self.lex_int();
                    self.add_token(line, column, Token::Int(token));
                }
                c if c.is_ascii_alphabetic() => {
                    let token = self.lex_identifier();
                    self.add_token(line, column, token);
                }
                '_' => {
                    let message = format!("Invalid symbol \"{}\"", display_invalid_char('_'));
                    self.advance();
                    self.errors.push(LexError {
                        line,
                        column,
                        message,
                    });
                }
                '(' => {
                    if self.starts_with("(*") {
                        // Comment handled by skip_whitespace_or_comment
                        // but reaching here means nested comment inside token loop.
                        // Consume and continue to next iteration.
                        let _ = self.advance();
                        let _ = self.advance();
                        if let Err(err) = self.skip_block_comment() {
                            self.errors.push(err);
                            break;
                        }
                        continue;
                    } else {
                        self.advance();
                        self.add_token(line, column, Token::LParen);
                    }
                }
                ')' => {
                    self.advance();
                    self.add_token(line, column, Token::RParen);
                }
                '{' => {
                    self.advance();
                    self.add_token(line, column, Token::LBrace);
                }
                '}' => {
                    self.advance();
                    self.add_token(line, column, Token::RBrace);
                }
                ':' => {
                    self.advance();
                    self.add_token(line, column, Token::Colon);
                }
                ';' => {
                    self.advance();
                    self.add_token(line, column, Token::Semicolon);
                }
                ',' => {
                    self.advance();
                    self.add_token(line, column, Token::Comma);
                }
                '.' => {
                    self.advance();
                    self.add_token(line, column, Token::Dot);
                }
                '@' => {
                    self.advance();
                    self.add_token(line, column, Token::At);
                }
                '+' => {
                    self.advance();
                    self.add_token(line, column, Token::Plus);
                }
                '-' => {
                    if self.starts_with("--") {
                        self.advance();
                        self.advance();
                        self.skip_line_comment();
                    } else {
                        self.advance();
                        self.add_token(line, column, Token::Minus);
                    }
                }
                '*' => {
                    if self.starts_with("*)") {
                        self.advance();
                        self.advance();
                        self.errors.push(LexError {
                            line,
                            column,
                            message: String::from("Unmatched *)"),
                        });
                    } else {
                        self.advance();
                        self.add_token(line, column, Token::Star);
                    }
                }
                '/' => {
                    self.advance();
                    self.add_token(line, column, Token::Slash);
                }
                '~' => {
                    self.advance();
                    self.add_token(line, column, Token::Tilde);
                }
                '<' => {
                    self.advance();
                    match self.peek() {
                        Some('-') => {
                            self.advance();
                            self.add_token(line, column, Token::Assign);
                        }
                        Some('=') => {
                            self.advance();
                            self.add_token(line, column, Token::Le);
                        }
                        _ => {
                            self.add_token(line, column, Token::Lt);
                        }
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        self.add_token(line, column, Token::Arrow);
                    } else {
                        self.add_token(line, column, Token::Eq);
                    }
                }
                '\0'..='\u{1F}' | '\u{7F}' => {
                    let ch = self.advance().unwrap();
                    let message = format!("Invalid symbol \"{}\"", display_invalid_char(ch));
                    self.errors.push(LexError {
                        line,
                        column,
                        message,
                    });
                }
                _ => {
                    let ch = self.advance().unwrap();
                    let message = format!("Invalid symbol \"{}\"", display_invalid_char(ch));
                    self.errors.push(LexError {
                        line,
                        column,
                        message,
                    });
                }
            }
        }
    }

    fn skip_whitespace_or_comment(&mut self) -> bool {
        let mut skipped = false;
        loop {
            let Some(ch) = self.peek() else {
                break;
            };

            if ch.is_whitespace() {
                skipped = true;
                self.advance();
                continue;
            }

            if self.starts_with("--") {
                skipped = true;
                self.advance();
                self.advance();
                self.skip_line_comment();
                continue;
            }

            if self.starts_with("(*") {
                skipped = true;
                self.advance();
                self.advance();
                if let Err(err) = self.skip_block_comment() {
                    self.errors.push(err);
                    self.pos = self.src.len();
                    break;
                }
                continue;
            }

            break;
        }

        skipped
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        let mut depth = 1usize;

        while let Some(ch) = self.peek() {
            if self.starts_with("(*") {
                self.advance();
                self.advance();
                depth += 1;
                continue;
            }

            if self.starts_with("*)") {
                self.advance();
                self.advance();
                depth -= 1;
                if depth == 0 {
                    return Ok(());
                }
                continue;
            }

            self.advance();
            match ch {
                '\n' => continue,
                _ => {}
            }
        }

        let (line, column) = if self.column == 1 && self.line > 1 {
            (self.line - 1, self.column)
        } else {
            (self.line, self.column)
        };

        Err(LexError {
            line,
            column,
            message: String::from("Unmatched (*"),
        })
    }

    fn lex_int(&mut self) -> String {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
        self.src[start..self.pos].to_string()
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos;
        let first_char = self.advance().unwrap();

        while let Some(ch) = self.peek() {
            if is_identifier_continue(ch) {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = &self.src[start..self.pos];
        let lower = lexeme.to_ascii_lowercase();

        if let Some(token) = keyword_token(&lower) {
            return token;
        }

        if is_bool_literal(first_char, &lower) {
            return Token::Bool(lower == "true");
        }

        if first_char.is_ascii_uppercase() {
            Token::TypeId(lexeme.to_string())
        } else {
            Token::ObjectId(lexeme.to_string())
        }
    }

    fn lex_string(&mut self) -> Result<(Token, usize, usize), LexError> {
        self.advance(); // consume opening quote
        let mut buffer = String::new();
        let mut length = 0usize;

        loop {
            let Some(ch) = self.peek() else {
                return Err(LexError {
                    line: self.line,
                    column: self.column,
                    message: String::from("Unterminated string at EOF"),
                });
            };

            match ch {
                '"' => {
                    self.advance();
                    let line = self.line;
                    let column = if self.column > 1 { self.column - 1 } else { 1 };
                    if self.pos >= self.src.len() {
                        return Err(LexError {
                            line,
                            column,
                            message: String::from("Unterminated string at EOF"),
                        });
                    }
                    return Ok((Token::Str(buffer), line, column));
                }
                '\0' => {
                    let line = self.line;
                    let column = self.column;
                    self.advance();
                    self.skip_until_string_terminator();
                    return Err(LexError {
                        line,
                        column,
                        message: String::from("String contains null character"),
                    });
                }
                '\n' => {
                    let line = self.line;
                    let column = self.column;
                    self.advance();
                    let message = if self.pos >= self.src.len() {
                        "Unterminated string at EOF"
                    } else {
                        "String contains unescaped new line"
                    };
                    return Err(LexError {
                        line,
                        column,
                        message: String::from(message),
                    });
                }
                '\\' => {
                    self.advance();
                    let Some(escaped) = self.peek() else {
                        return Err(LexError {
                            line: self.line,
                            column: self.column,
                            message: String::from("Unterminated string at EOF"),
                        });
                    };

                    match escaped {
                        'b' => {
                            self.advance();
                            buffer.push('\u{0008}');
                            length += 1;
                        }
                        't' => {
                            self.advance();
                            buffer.push('\t');
                            length += 1;
                        }
                        'n' => {
                            self.advance();
                            buffer.push('\n');
                            length += 1;
                        }
                        '\\' => {
                            self.advance();
                            buffer.push('\\');
                            length += 1;
                        }
                        '"' => {
                            self.advance();
                            buffer.push('"');
                            length += 1;
                        }
                        '\0' => {
                            let line = self.line;
                            let column = self.column;
                            self.advance();
                            self.skip_until_string_terminator();
                            return Err(LexError {
                                line,
                                column,
                                message: String::from("String contains escaped null character"),
                            });
                        }
                        '\n' => {
                            self.advance();
                            buffer.push('\n');
                            length += 1;
                        }
                        other => {
                            self.advance();
                            buffer.push(other);
                            length += 1;
                        }
                    }
                }
                other => {
                    self.advance();
                    buffer.push(other);
                    length += 1;
                }
            }

            if length > MAX_STRING_LENGTH {
                let line = self.line;
                let column = if self.column > 0 { self.column } else { 1 };
                self.skip_until_string_terminator();
                return Err(LexError {
                    line,
                    column,
                    message: String::from("String constant too long"),
                });
            }
        }
    }

    fn skip_until_string_terminator(&mut self) {
        while let Some(ch) = self.peek() {
            self.advance();
            match ch {
                '"' => break,
                '\\' => {
                    if let Some(next) = self.peek() {
                        if next == '\n' {
                            self.advance();
                        } else {
                            self.advance();
                        }
                    }
                }
                '\n' => break,
                _ => {}
            }
        }
    }

    fn add_token(&mut self, line: usize, column: usize, token: Token) {
        self.tokens.push(LexToken {
            token,
            line,
            column,
        });
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn starts_with(&self, pattern: &str) -> bool {
        self.src[self.pos..].starts_with(pattern)
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        let len = ch.len_utf8();
        self.pos += len;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }
}

fn keyword_token(lower: &str) -> Option<Token> {
    match lower {
        "class" => Some(Token::Class),
        "else" => Some(Token::Else),
        "fi" => Some(Token::Fi),
        "if" => Some(Token::If),
        "in" => Some(Token::In),
        "inherits" => Some(Token::Inherits),
        "isvoid" => Some(Token::Isvoid),
        "let" => Some(Token::Let),
        "loop" => Some(Token::Loop),
        "pool" => Some(Token::Pool),
        "then" => Some(Token::Then),
        "while" => Some(Token::While),
        "case" => Some(Token::Case),
        "esac" => Some(Token::Esac),
        "new" => Some(Token::New),
        "of" => Some(Token::Of),
        "not" => Some(Token::Not),
        _ => None,
    }
}

fn is_bool_literal(first_char: char, lower: &str) -> bool {
    match (first_char, lower) {
        ('t', "true") => true,
        ('f', "false") => true,
        _ => false,
    }
}

fn is_identifier_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn display_invalid_char(ch: char) -> String {
    match ch {
        '\0'..='\u{1F}' | '\u{7F}' => format!("<0x{:02X}>", ch as u32),
        '\\' => String::from("\\\\"),
        '"' => String::from("\\\""),
        _ => ch.to_string(),
    }
}
