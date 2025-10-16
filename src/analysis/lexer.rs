use chumsky::{
    Parser,
    prelude::{choice, end, just, none_of},
};

fn parser<'src>() -> impl Parser<'src, &'src str, ()> {
    end() // --(5)
}

fn escape_char<'src>() -> impl Parser<'src, &'src str, ()> {
    // just('\\').ignore_then(
    //     choice((
    //         just('t').to('\t'),
    //         just('b').to('\u{0008}'),
    //         just('n').to('\n'),
    //         just('f').to('\u{000C}'),
    //         // Any other escaped char: return it as-is
    //         none_of("tbnf"),
    //     ))
    //     .labelled("escape sequence"),
    // )
}

fn test<'src>() -> impl Parser<'src, &'src str, ()> {
    just('\\').ignore_then(choice((
        just('t').to('\t'),
        just('b').to('\u{0008}'),
        just('n').to('\n'),
        just('f').to('\u{000C}'),
        // Any other escaped char: return it as-is
        none_of("tbnf").map(|x| ()),
    )))
}

#[test]
fn test_parser() {
    // Our parser expects empty strings, so this should parse successfully
    assert_eq!(parser().parse("").into_result(), Ok(()));

    // Anything other than an empty string should produce an error
    assert!(parser().parse("123").has_errors());
}
