use logos::{Lexer, Logos};
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Clone, Copy)]
pub struct LexConfig {
    pub emit_trivia: bool,
}
impl Default for LexConfig {
    fn default() -> Self {
        Self { emit_trivia: false }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Tok,
    pub range: Range<usize>, // byte offsets
}

#[derive(Debug, Error, Clone)]
pub enum LexDiag {
    #[error("unterminated string")]
    UnterminatedString { start: usize },
    #[error("string contains NUL")]
    StringContainsNul { at: usize },
    #[error("unescaped newline inside string")]
    NewlineInString { at: usize },
    #[error("unterminated block comment")]
    UnterminatedComment { start: usize },
}

#[derive(Default, Debug)]
pub struct Extras {
    pub cfg: LexConfig,
    pub diags: Vec<LexDiag>,
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(extras = Extras)]
pub enum Tok {
    // Literals / Identifiers
    #[regex(r"[0-9]+")]
    Integer,

    // Strings handled by a custom callback to enforce COOL rules
    #[regex(r#""#, lex_string)]
    String,

    // Special identifiers
    #[token("self")]
    SelfId,
    #[token("SELF_TYPE")]
    SelfTypeId,

    // Booleans: first letter MUST be lowercase, rest case-insensitive
    #[regex(r"t(?i:rue)")]
    TrueConst,
    #[regex(r"f(?i:alse)")]
    FalseConst,

    // Keywords (case-insensitive). Apparently COOL treats keywords case-insensitively
    #[regex(r"(?i:class)")]
    KwClass,
    #[regex(r"(?i:else)")]
    KwElse,
    #[regex(r"(?i:fi)")]
    KwFi,
    #[regex(r"(?i:if)")]
    KwIf,
    #[regex(r"(?i:in)")]
    KwIn,
    #[regex(r"(?i:inherits)")]
    KwInherits,
    #[regex(r"(?i:isvoid)")]
    KwIsvoid,
    #[regex(r"(?i:let)")]
    KwLet,
    #[regex(r"(?i:loop)")]
    KwLoop,
    #[regex(r"(?i:pool)")]
    KwPool,
    #[regex(r"(?i:then)")]
    KwThen,
    #[regex(r"(?i:while)")]
    KwWhile,
    #[regex(r"(?i:case)")]
    KwCase,
    #[regex(r"(?i:esac)")]
    KwEsac,
    #[regex(r"(?i:new)")]
    KwNew,
    #[regex(r"(?i:of)")]
    KwOf,
    #[regex(r"(?i:not)")]
    KwNot,

    // Identifiers: distinguished by first char case
    #[regex(r"[A-Z][A-Za-z0-9_]*")]
    TypeId,
    #[regex(r"[a-z_][A-Za-z0-9_]*")]
    ObjectId,

    // Operators / Punctuation
    #[token("<-")]
    Assign,
    #[token("<=")]
    Le,
    #[token("=>")]
    CaseArrow,
    #[token("<")]
    Lt,
    #[token("=")]
    Eq,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("~")]
    Tilde,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("@")]
    At,

    // Whitespace (space, tab, CR, LF, FF, VT). Skip unless emit_trivia=true
    #[regex(r"[ \t\r\n\f\v]+", lex_ws)]
    Whitespace,

    // Line comments: `--` to end of line/EOF
    #[regex(r"--[^\n]*", lex_line_comment)]
    LineComment,

    // Nested block comments: handled in callback; supports nesting
    #[regex(r"\(\*", lex_block_comment)]
    BlockComment,

    Error,
}

// Callbacks for tricky tokens
fn lex_ws(lex: &mut Lexer<Tok>) -> Option<()> {
    if lex.extras.cfg.emit_trivia {
        Some(())
    } else {
        None
    }
}

fn lex_line_comment(lex: &mut Lexer<Tok>) -> Option<()> {
    if lex.extras.cfg.emit_trivia {
        Some(())
    } else {
        None
    }
}

fn lex_block_comment(lex: &mut Lexer<Tok>) -> Option<()> {
    // We’re at the '(' of "(*"
    let bytes = lex.source().as_bytes();
    let mut i = lex.span().start; // current position
    let len = bytes.len();
    let mut depth = 0usize;

    // Consume "(*"
    i += 2;

    while i < len {
        if i + 1 < len && bytes[i] == b'(' && bytes[i + 1] == b'*' {
            depth += 1;
            i += 2;
            continue;
        }
        if i + 1 < len && bytes[i] == b'*' && bytes[i + 1] == b')' {
            if depth == 0 {
                i += 2;
                lex.bump(i - lex.span().end);
                return if lex.extras.cfg.emit_trivia {
                    Some(())
                } else {
                    None
                };
            } else {
                depth -= 1;
                i += 2;
                continue;
            }
        }
        i += 1;
    }

    // EOF before closing
    lex.extras.diags.push(LexDiag::UnterminatedComment {
        start: lex.span().start,
    });
    if lex.extras.cfg.emit_trivia {
        Some(())
    } else {
        None
    }
}

fn lex_string(lex: &mut Lexer<Tok>) -> Option<()> {
    // We’re at the opening quote
    let src = lex.source().as_bytes();
    let mut i = lex.span().start; // index of the opening quote
    let len = src.len();

    i += 1; // consume opening "

    while i < len {
        match src[i] {
            b'"' => {
                // closing quote
                i += 1;
                lex.bump(i - lex.span().end);
                return Some(());
            }
            b'\0' => {
                lex.extras.diags.push(LexDiag::StringContainsNul { at: i });
                // keep consuming until we hit a quote or line end to avoid looping
                i += 1;
            }
            b'\n' => {
                // raw newline is illegal unless escaped by backslash in previous char
                lex.extras.diags.push(LexDiag::NewlineInString { at: i });
                // Stop at newline; COOL strings cannot span raw lines
                // We don't advance to a closing quote; mark token to here
                lex.bump(i - lex.span().end);
                return Some(());
            }
            b'\\' => {
                // Escape sequence: consume backslash and the next char if any
                i += 1;
                if i >= len {
                    // EOF after backslash → unterminated
                    lex.extras.diags.push(LexDiag::UnterminatedString {
                        start: lex.span().start,
                    });
                    lex.bump(i - lex.span().end);
                    return Some(());
                }
                // allow \b \t \n \f \" \\ and also backslash + newline (line continuation)
                // Even unknown escapes are allowed as literal per spec; we just consume one char
                i += 1;
            }
            _ => i += 1,
        }
    }

    // EOF without closing quote
    lex.extras.diags.push(LexDiag::UnterminatedString {
        start: lex.span().start,
    });
    // bump to EOF
    lex.bump(i.saturating_sub(lex.span().end));
    Some(())
}

// Driver

/// Lex the whole input into tokens and diagnostics
/// If `cfg.emit_trivia=false`, trivia tokens are skipped
pub fn lex(input: &str, cfg: LexConfig) -> (Vec<Token>, Vec<LexDiag>) {
    let mut lexer = Tok::lexer(input);
    lexer.extras.cfg = cfg;

    let mut tokens = Vec::new();
    while let Some(kind) = lexer.next() {
        let span = lexer.span();
        tokens.push(Token {
            kind: kind.unwrap(),
            range: span.start..span.end,
        });
    }
    (tokens, lexer.extras.diags.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords_and_ids() {
        let (toks, diags) = lex(
            "Class Main inherits Object let x <- 1;",
            LexConfig::default(),
        );
        assert!(diags.is_empty());
        assert!(toks.iter().any(|t| matches!(t.kind, Tok::KwClass)));
        assert!(toks.iter().any(|t| matches!(t.kind, Tok::TypeId)));
        assert!(toks.iter().any(|t| matches!(t.kind, Tok::KwInherits)));
        assert!(toks.iter().any(|t| matches!(t.kind, Tok::ObjectId)));
        assert!(toks.iter().any(|t| matches!(t.kind, Tok::Assign)));
    }

    #[test]
    fn booleans_case_rule() {
        let (toks, _) = lex("true FALSE False fAlSe tRuE", LexConfig::default());
        // first token is TrueConst; tokens starting with uppercase F should be identifiers.
        assert!(matches!(toks[0].kind, Tok::TrueConst));
        assert!(matches!(toks[1].kind, Tok::TypeId | Tok::ObjectId)); // not a boolean
        assert!(matches!(toks[2].kind, Tok::FalseConst));
        assert!(matches!(toks[3].kind, Tok::FalseConst));
        assert!(matches!(toks[4].kind, Tok::TrueConst));
    }

    #[test]
    fn strings_and_errors() {
        let (_t, d) = lex(r#""hi\n""#, LexConfig::default());
        assert!(d.is_empty());

        let (_t, d2) = lex("\"bad\nstring\"", LexConfig::default());
        assert!(
            d2.iter()
                .any(|e| matches!(e, LexDiag::NewlineInString { .. }))
        );

        let (_t, d3) = lex("\"unfinished", LexConfig::default());
        assert!(
            d3.iter()
                .any(|e| matches!(e, LexDiag::UnterminatedString { .. }))
        );
    }

    #[test]
    fn comments_nested() {
        let src = "(* a (* nested *) ok *) x";
        let (t, d) = lex(src, LexConfig::default());
        assert!(d.is_empty());
        // Last token should be an identifier 'x'
        assert!(matches!(t.last().unwrap().kind, Tok::ObjectId));
    }
}
