use cool_lsp::analysis::lexer::{LexError, LexToken, Token, lex};
use std::fs;
use std::path::Path;

fn stringify_cool_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000C}' => out.push_str("<0x0c>"),
            _ if ch.is_control() => out.push_str(&format!("<0x{:02x}>", ch as u32)),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn format_token(tok: &LexToken) -> String {
    let kind = match &tok.token {
        Token::TypeId(s) => format!("TYPEID {s}"),
        Token::ObjectId(s) => format!("OBJECTID {s}"),
        Token::Int(s) => format!("INT_CONST {s}"),
        Token::Str(s) => format!("STR_CONST {}", stringify_cool_string(s)),
        Token::Bool(b) => format!("BOOL_CONST {}", if *b { "true" } else { "false" }),
        Token::Class => "CLASS".to_string(),
        Token::Else => "ELSE".to_string(),
        Token::Fi => "FI".to_string(),
        Token::If => "IF".to_string(),
        Token::In => "IN".to_string(),
        Token::Inherits => "INHERITS".to_string(),
        Token::Isvoid => "ISVOID".to_string(),
        Token::Let => "LET".to_string(),
        Token::Loop => "LOOP".to_string(),
        Token::Pool => "POOL".to_string(),
        Token::Then => "THEN".to_string(),
        Token::While => "WHILE".to_string(),
        Token::Case => "CASE".to_string(),
        Token::Esac => "ESAC".to_string(),
        Token::New => "NEW".to_string(),
        Token::Of => "OF".to_string(),
        Token::Not => "NOT".to_string(),
        Token::LParen => "'('".to_string(),
        Token::RParen => "')'".to_string(),
        Token::LBrace => "'{'".to_string(),
        Token::RBrace => "'}'".to_string(),
        Token::Colon => "':'".to_string(),
        Token::Semicolon => "';'".to_string(),
        Token::Comma => "','".to_string(),
        Token::Dot => "'.'".to_string(),
        Token::At => "'@'".to_string(),
        Token::Assign => "ASSIGN".to_string(),
        Token::Arrow => "DARROW".to_string(),
        Token::Plus => "'+'".to_string(),
        Token::Minus => "'-'".to_string(),
        Token::Star => "'*'".to_string(),
        Token::Slash => "'/'".to_string(),
        Token::Tilde => "'~'".to_string(),
        Token::Lt => "'<'".to_string(),
        Token::Le => "LE".to_string(),
        Token::Eq => "'='".to_string(),
        Token::Eof => "EOF".to_string(),
    };

    format!("#{} {}", tok.line, kind)
}

fn format_error(err: &LexError) -> String {
    format!("#{} ERROR: {}", err.line, err.message)
}

fn parse_expected_output(expected: &str) -> (Vec<String>, Vec<String>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for line in expected.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        if trimmed.contains("ERROR: ") {
            errors.push(trimmed.to_string());
        } else {
            tokens.push(trimmed.to_string());
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

        let actual = lex(&input);
        let actual_tokens: Vec<String> = actual.tokens.iter().map(format_token).collect();
        let actual_errors: Vec<String> = actual.errors.iter().map(format_error).collect();

        let mut test_failed = false;

        if actual_tokens != expected_tokens {
            test_failed = true;
            eprintln!(
                "❌ {test_name} tokens differ:\nExpected:\n{}\nGot:\n{}",
                expected_tokens.join("\n"),
                actual_tokens.join("\n")
            );
        }

        if actual_errors != expected_errors {
            test_failed = true;
            eprintln!(
                "❌ {test_name} errors differ:\nExpected:\n{}\nGot:\n{}",
                expected_errors.join("\n"),
                actual_errors.join("\n")
            );
        }

        if test_failed {
            failed = true;
        } else {
            println!("✅ {test_name} passed");
        }
    }

    if failed {
        panic!("Some lexer tests failed");
    }
}
