use chumsky::Parser;
use cool_lsp::analysis::lexer::{lex, Token};
use std::fs;
use std::path::Path;

fn token_strings(tokens: &[Token]) -> Vec<String> {
    tokens
        .iter()
        .filter_map(|tok| match tok {
            Token::TypeId(s) => Some(format!("TYPEID {s}")),
            Token::ObjectId(s) => Some(format!("OBJECTID {s}")),
            Token::Int(n) => Some(format!("INT_CONST {n}")),
            Token::Str(s) => Some(format!("STR_CONST {:?}", s)),
            Token::Bool(b) => Some(format!("BOOL_CONST {}", if *b { "true" } else { "false" })),
            Token::Class => Some(String::from("CLASS")),
            Token::Else => Some(String::from("ELSE")),
            Token::Fi => Some(String::from("FI")),
            Token::If => Some(String::from("IF")),
            Token::In => Some(String::from("IN")),
            Token::Inherits => Some(String::from("INHERITS")),
            Token::Isvoid => Some(String::from("ISVOID")),
            Token::Let => Some(String::from("LET")),
            Token::Loop => Some(String::from("LOOP")),
            Token::Pool => Some(String::from("POOL")),
            Token::Then => Some(String::from("THEN")),
            Token::While => Some(String::from("WHILE")),
            Token::Case => Some(String::from("CASE")),
            Token::Esac => Some(String::from("ESAC")),
            Token::New => Some(String::from("NEW")),
            Token::Of => Some(String::from("OF")),
            Token::Not => Some(String::from("NOT")),
            Token::LParen => Some(String::from("'('")),
            Token::RParen => Some(String::from("')'")),
            Token::LBrace => Some(String::from("'{'")),
            Token::RBrace => Some(String::from("'}'")),
            Token::Colon => Some(String::from("':'")),
            Token::Semicolon => Some(String::from("';'")),
            Token::Comma => Some(String::from("','")),
            Token::Dot => Some(String::from("'.'")),
            Token::At => Some(String::from("'@'")),
            Token::Assign => Some(String::from("ASSIGN")),
            Token::Arrow => Some(String::from("DARROW")),
            Token::Plus => Some(String::from("'+'")),
            Token::Minus => Some(String::from("'-'")),
            Token::Star => Some(String::from("'*'")),
            Token::Slash => Some(String::from("'/'")),
            Token::Tilde => Some(String::from("'~'")),
            Token::Lt => Some(String::from("'<'")),
            Token::Le => Some(String::from("LE")),
            Token::Eq => Some(String::from("'='")),
            Token::Eof => Some(String::from("EOF")),
            Token::Error(e) => Some(format!("ERROR: {e}")),
            Token::LineComment(_) | Token::BlockComment(_) => None,
        })
        .collect()
}

fn parse_expected_output(expected: &str) -> (Vec<String>, Vec<String>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for line in expected.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let rest = match trimmed.split_once(' ') {
            Some((prefix, tail)) if prefix.starts_with('#') => tail.trim_start(),
            _ => trimmed,
        };

        if rest.starts_with("ERROR") {
            errors.push(rest.to_string());
        } else {
            tokens.push(rest.to_string());
        }
    }

    (tokens, errors)
}

#[test]
fn lexer_directory_tests() {
    let dir = Path::new("./tests/lexer");
    let entries = fs::read_dir(dir).expect("Cannot read tests/lexer directory");

    let mut failed = false;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("in") {
            continue;
        }

        let test_name = path.file_stem().unwrap().to_string_lossy();
        let expected_path = path.with_extension("out");

        println!("→ Testing {test_name}");

        let input = fs::read_to_string(&path).expect("Failed to read input file");
        let expected_output = fs::read_to_string(&expected_path).unwrap_or_else(|_| {
            panic!(
                "❌ Expected output file missing: {}",
                expected_path.display()
            )
        });

        let (expected_tokens, expected_errors) = parse_expected_output(&expected_output);

        let parse_result = lex().parse(&input).into_result();

        match parse_result {
            Ok(tokens) => {
                let actual_strings = token_strings(&tokens);

                if !expected_errors.is_empty() {
                    failed = true;
                    eprintln!(
                        "❌ {test_name} expected lexing error(s) but succeeded.\nExpected errors:\n{}",
                        expected_errors.join("\n")
                    );
                    continue;
                }

                if actual_strings != expected_tokens {
                    failed = true;
                    eprintln!(
                        "❌ {test_name} tokens differ:\nExpected:\n{}\nGot:\n{}",
                        expected_tokens.join("\n"),
                        actual_strings.join("\n")
                    );
                } else {
                    println!("✅ {test_name} passed");
                }
            }
            Err(errs) => {
                if expected_errors.is_empty() {
                    failed = true;
                    eprintln!("❌ {test_name}: unexpected lexing error(s):\n{:#?}", errs);
                } else {
                    println!("✅ {test_name} produced expected lexing error(s)");
                }
            }
        }
    }

    if failed {
        panic!("Some lexer tests failed");
    }
}
