use std::ops::Range;

use chumsky::{
    IterParser, Parser,
    error::Simple,
    extra,
    prelude::{any, choice, end, just, none_of, recursive},
    text,
};

// Will be used later for diagnostics
pub type Span = Range<usize>;
pub type Spanned<T> = (T, Span);

const KEYWORDS: &[&str] = &[
    "class", "else", "fi", "if", "in", "inherits", "isvoid", "let", "loop", "pool", "then",
    "while", "case", "esac", "new", "of", "not",
];

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
    Int(i64),
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

    LineComment(String),  // "-- ..."
    BlockComment(String), // "(* ... *( ... )* ... *)", optionally nested

    Eof,
    Error(String),
}

macro_rules! make_kw_lexer {
    ($fn_name:ident, $kw:literal, $Variant:ident) => {
        pub fn $fn_name<'src>()
        -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>> {
            just($kw)
                .map(|_| Token::$Variant)
                .labelled(concat!($kw, " keyword"))
        }
    };
}

macro_rules! make_kw_lexers {
    ($( $fn_name:ident : $kw:literal => $Variant:ident ),+ $(,)?) => {
        $( make_kw_lexer!($fn_name, $kw, $Variant); )+
    };
}

make_kw_lexers! {
    // Keywords
    lex_class   : "class"    => Class,
    lex_else    : "else"     => Else,
    lex_fi      : "fi"       => Fi,
    lex_if      : "if"       => If,
    lex_in      : "in"       => In,
    lex_inherits: "inherits" => Inherits,
    lex_isvoid  : "isvoid"   => Isvoid,
    lex_let     : "let"      => Let,
    lex_loop    : "loop"     => Loop,
    lex_pool    : "pool"     => Pool,
    lex_then    : "then"     => Then,
    lex_while   : "while"    => While,
    lex_case    : "case"     => Case,
    lex_esac    : "esac"     => Esac,
    lex_new     : "new"      => New,
    lex_of      : "of"       => Of,
    lex_not     : "not"      => Not,

    // Symbols / Punctuation
    lex_lparen    : "("   => LParen,
    lex_rparen    : ")"   => RParen,
    lex_lbrace    : "{"   => LBrace,
    lex_rbrace    : "}"   => RBrace,
    lex_colon     : ":"   => Colon,
    lex_semicolon : ";"   => Semicolon,
    lex_comma     : ","   => Comma,
    lex_dot       : "."   => Dot,
    lex_at        : "@"   => At,

    // Multi-char operators
    lex_assign    : "<-"  => Assign,
    lex_arrow     : "=>"  => Arrow,

    // Arithmetic operators
    lex_plus      : "+"   => Plus,
    lex_minus     : "-"   => Minus,
    lex_star      : "*"   => Star,
    lex_slash     : "/"   => Slash,
    lex_tilde     : "~"   => Tilde,

    // Comparison operators
    lex_lt        : "<"   => Lt,
    lex_le        : "<="  => Le,
    lex_eq        : "="   => Eq,
}

pub fn lex_type_id<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>> {
    let tail = any()
        .filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_')
        .repeated()
        .collect::<String>();

    any()
        .filter(char::is_ascii_uppercase)
        .then(tail)
        .map(|(head, rest)| {
            let mut s = String::new();
            s.push(head);
            s.push_str(&rest);
            Token::TypeId(s)
        })
}

// ObjectId: Lowercase letter, then [A-Za-z0-9_]*
pub fn lex_object_id<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>>
{
    let tail = any()
        .filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_')
        .repeated()
        .collect::<String>();

    any()
        .filter(char::is_ascii_lowercase)
        .then(tail)
        .map(|(head, rest)| {
            let mut s = String::new();
            s.push(head);
            s.push_str(&rest);
            Token::ObjectId(s)
        })
}

// End-of-file token
pub fn lex_eof<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>> {
    end().to(Token::Eof).labelled("end of file")
}

fn escape<'src>() -> impl Parser<'src, &'src str, char, extra::Err<Simple<'src, char>>> {
    just('\\').ignore_then(choice((
        just('t').to('\t'),
        just('b').to('\u{0008}'),
        just('n').to('\n'),
        just('f').to('\u{000C}'),
        none_of("tbnf"),
    )))
}

fn lex_string<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>> {
    let escape = escape();

    let normal_char = none_of("\\\"\n");

    let ch = choice((escape, normal_char));

    just('"')
        .ignore_then(ch.repeated().collect::<String>())
        .then_ignore(just('"'))
        .map(|captured| Token::Str(captured))
        .labelled("string literal")
}

pub fn block_comment_ignored<'src>()
-> impl Parser<'src, &'src str, (), extra::Err<Simple<'src, char>>> {
    recursive(|comment| {
        // A "chunk" is either another nested comment, or a single char that
        // *cannot* close "*)" or start "(*"
        let chunk = choice((
            comment.ignored(),
            none_of("*()").ignored(),
            just('*').then_ignore(none_of(')')).ignored(), // '*' not followed by ')'
            just('(').then_ignore(none_of('*')).ignored(), // '(' not followed by '*'
        ));

        just("(*")
            .ignore_then(chunk.repeated())
            .then_ignore(just("*)"))
            .ignored()
    })
    .labelled("block comment")
}

pub fn line_comment_ignored<'src>()
-> impl Parser<'src, &'src str, (), extra::Err<Simple<'src, char>>> {
    just("--")
        .ignore_then(none_of('\n').repeated()) // everything until a '\n'
        .then_ignore(choice((text::newline().ignored(), end().ignored())))
        .ignored()
        .labelled("line comment")
}

pub fn lex<'src>() -> impl Parser<'src, &'src str, Vec<Token>, extra::Err<Simple<'src, char>>> {
    // whitespace OR comments (ignored) — a function so we never need to clone a parser
    fn ws_or_comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Simple<'src, char>>> {
        choice((
            text::whitespace().ignored(),
            line_comment_ignored(),
            block_comment_ignored(),
        ))
        .repeated()
        .ignored()
    }

    // Keywords (word-bounded). If you prefer your macro fns, replace this block with choice((lex_class(), ...))
    let keywords = choice((
        text::keyword("class").to(Token::Class),
        text::keyword("else").to(Token::Else),
        text::keyword("fi").to(Token::Fi),
        text::keyword("if").to(Token::If),
        text::keyword("in").to(Token::In),
        text::keyword("inherits").to(Token::Inherits),
        text::keyword("isvoid").to(Token::Isvoid),
        text::keyword("let").to(Token::Let),
        text::keyword("loop").to(Token::Loop),
        text::keyword("pool").to(Token::Pool),
        text::keyword("then").to(Token::Then),
        text::keyword("while").to(Token::While),
        text::keyword("case").to(Token::Case),
        text::keyword("esac").to(Token::Esac),
        text::keyword("new").to(Token::New),
        text::keyword("of").to(Token::Of),
        text::keyword("not").to(Token::Not),
    ));

    // Symbols / operators (longer first)
    let symbols = choice((
        // multi-char first
        lex_le(),
        lex_assign(),
        lex_arrow(),
        // singles
        lex_lparen(),
        lex_rparen(),
        lex_lbrace(),
        lex_rbrace(),
        lex_colon(),
        lex_semicolon(),
        lex_comma(),
        lex_dot(),
        lex_at(),
        lex_lt(),
        lex_eq(),
        lex_plus(),
        lex_minus(),
        lex_star(),
        lex_slash(),
        lex_tilde(),
    ));

    // One token (spanned)
    let token = choice((
        keywords,
        lex_string(),
        symbols,
        lex_type_id(),
        lex_object_id(),
    ))
    .labelled("token");

    let comments = ws_or_comment().repeated();
    let comments1 = ws_or_comment().repeated();

    token.delimited_by(comments, comments1).repeated().collect()
}

#[test]
fn test_escape() {
    assert_eq!(escape().parse(r"\c").into_result(), Ok('c'));

    assert_eq!(escape().parse(r"\b").into_result(), Ok('\u{0008}'));
    assert_eq!(escape().parse(r"\t").into_result(), Ok('\t'));
    assert_eq!(escape().parse(r"\n").into_result(), Ok('\n'));
    assert_eq!(escape().parse(r"\f").into_result(), Ok('\u{000C}'));
}

#[test]
fn test_string() {
    assert_eq!(
        lex_string().parse("\"TEST\"").into_result(),
        Ok(Token::Str(String::from("TEST")))
    );

    let err = lex_string().parse("\"T\nEST\"").into_result();

    assert!(err.is_err_and(|arr| !arr.is_empty()));

    let ok = lex_string().parse("\"T\\nEST\"").into_result();

    assert!(ok.is_ok_and(|x| x == Token::Str(String::from("T\nEST"))));

    assert_eq!(
        lex_string().parse("\"T\u{0045}ST\"").into_result(),
        Ok(Token::Str(String::from("TEST")))
    );
}

#[test]
fn test_block_comment_ignored() {
    let test_str_1 = "(*\n*)";

    assert_eq!(
        block_comment_ignored().parse(test_str_1).into_result(),
        Ok(())
    );

    let test_str_2 = "(*\n(*\n*)\n*)";

    assert_eq!(
        block_comment_ignored().parse(test_str_2).into_result(),
        Ok(())
    );

    let test_str_3 = "(*  (* *)  *)";

    assert_eq!(
        block_comment_ignored().parse(test_str_3).into_result(),
        Ok(())
    );
}

#[test]
fn test_line_commend_ignored() {
    let test_str_1 = "-- TEST";

    assert_eq!(
        line_comment_ignored().parse(test_str_1).into_result(),
        Ok(())
    );

    let test_str_2 = "-- -- TEST";

    assert_eq!(
        line_comment_ignored().parse(test_str_2).into_result(),
        Ok(())
    );

    let test_str_3 = "-- TEST -- TEST";

    assert_eq!(
        line_comment_ignored().parse(test_str_3).into_result(),
        Ok(())
    );

    let test_str_3 = "-";

    assert!(
        line_comment_ignored()
            .parse(test_str_3)
            .into_result()
            .is_err(),
    );
}

#[test]
fn test_keywords() {
    assert_eq!(lex_class().parse("class").into_result(), Ok(Token::Class));
    assert!(lex_class().parse("classy").into_result().is_err());
}

#[test]
fn test_object_id() {
    assert_eq!(
        lex_object_id().parse("test").into_result(),
        Ok(Token::ObjectId(String::from("test")))
    );

    assert!(lex_object_id().parse("Test").into_result().is_err());
}

#[test]
fn test_type_id() {
    assert!(lex_type_id().parse("test").into_result().is_err());

    assert_eq!(
        lex_type_id().parse("Test").into_result(),
        Ok(Token::TypeId(String::from("Test")))
    );
}

#[test]
fn test_lex() {
    let test = r#"(*

    *)
    test"#;

    assert_eq!(
        lex().parse(test).into_result(),
        Ok(vec![Token::ObjectId(String::from("test"))])
    );
}
