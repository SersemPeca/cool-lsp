use std::ops::Range;

use chumsky::{
    IterParser, Parser,
    error::Simple,
    extra,
    prelude::{choice, end, just, none_of, recursive},
    text,
};

// Will be used later for diagnostics
pub type Span = Range<usize>;
pub type Spanned<T> = (T, Span);

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

    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    Colon,     // :
    Semicolon, // ;
    Comma,     // ,
    Dot,       // .
    At,        // @

    Assign, // <-
    Arrow,  // =>

    Plus,  // +
    Minus, // -
    Star,  // *
    Slash, // /
    Tilde, // ~

    Lt, // <
    Le, // <=
    Eq, // =

    LineComment(String),  // "-- ...\n"
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

fn lex_class_keyword<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Simple<'src, char>>>
{
    just("Class")
        .map(|_| Token::Class)
        .labelled("class literal")
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
