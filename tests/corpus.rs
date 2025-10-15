use glob::glob;
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::{fs, path::Path};

// --- import your lexer API ---

use cool_lsp::analysis::lexer::{LexConfig, LexDiag, Tok, Token, lex};

// ------------ RENDERING ---------------
// Make the output match your .out files exactly.
// Tweak `render_token` / `render_diag` until it matches your format.
fn render_case(input: &str, toks: &[Token], diags: &[LexDiag]) -> String {
    let lines = line_starts(input);

    let mut out = String::new();
    // 1) tokens
    for t in toks {
        let line_no = line_of(t.range.start, &lines);
        out.push_str(&render_token(line_no, input, t));
        out.push('\n');
    }
    // 2) diagnostics
    for d in diags {
        let (line_no, msg) = render_diag(input, &lines, d);
        out.push_str(&format!("#{line_no} {msg}\n"));
    }
    // Trim final newline if your .out files don’t end with one
    if out.ends_with('\n') {
        out.pop();
    }
    out
}

fn render_token(line_no: usize, input: &str, t: &Token) -> String {
    match t.kind {
        Tok::String => {
            // Lexeme as written (including quotes). Your reference output might want the
            // *processed* payload instead; if so, adjust this to decode escapes as needed.
            let lexeme = &input[t.range.clone()];
            // Example matches like: `#3 STR_CONST "\\-"`
            format!("#{line_no} STR_CONST {}", lexeme)
        }
        // Example: single quotes in your reference for '-' — include whatever you need:
        Tok::Minus => format!("#{line_no} '-'"),
        Tok::Plus => format!("#{line_no} '+'"),
        Tok::Slash => format!("#{line_no} '/'"),
        Tok::Star => format!("#{line_no} '*'"),
        Tok::Eq => format!("#{line_no} '='"),
        Tok::Lt => format!("#{line_no} '<'"),
        Tok::Le => format!("#{line_no} '<='"),
        Tok::Assign => format!("#{line_no} '<-'"),
        Tok::CaseArrow => format!("#{line_no} '=>'"),
        // Add whatever else your .out expects. For non-string tokens that should not appear,
        // you can skip or format differently.
        _ => {
            // Fallback: show the lexeme (handy while aligning with the reference format)
            let lexeme = &input[t.range.clone()];
            format!("#{line_no} {}", lexeme)
        }
    }
}

fn render_diag(input: &str, lines: &[usize], d: &LexDiag) -> (usize, String) {
    match d {
        LexDiag::StringContainsNul { at } => {
            (line_of(*at, lines), format!("ERROR: String contains NUL"))
        }
        LexDiag::NewlineInString { at } => (
            line_of(*at, lines),
            format!("ERROR: String contains unescaped new line"),
        ),
        LexDiag::UnterminatedString { start } => (
            line_of(*start, lines),
            format!("ERROR: Unterminated string"),
        ),
        LexDiag::UnterminatedComment { start } => (
            line_of(*start, lines),
            format!("ERROR: Unterminated comment"),
        ),
    }
}

// ------------ LINE MAP ---------------
fn line_starts(src: &str) -> Vec<usize> {
    // Byte offsets of the start of each line
    let mut v = vec![0];
    for (i, b) in src.bytes().enumerate() {
        if b == b'\n' {
            v.push(i + 1);
        }
    }
    v
}

fn line_of(byte_off: usize, line_starts: &[usize]) -> usize {
    // 1-based line number
    match line_starts.binary_search(&byte_off) {
        Ok(i) => i + 1,
        Err(i) => i, // index of the preceding line start + 1
    }
}

// ------------ TEST HARNESS -----------
fn read_to_string(p: &Path) -> String {
    fs::read_to_string(p).unwrap_or_else(|e| panic!("reading {}: {e}", p.display()))
}

#[test]
fn corpus() {
    // Adjust the glob to your directory
    for entry in glob("tests/cases/*.in").expect("glob") {
        let in_path = entry.expect("path");
        let stem = in_path.file_stem().unwrap().to_string_lossy();
        let out_path = in_path.with_extension("out");

        let input = read_to_string(&in_path);
        let (toks, diags) = lex(&input, LexConfig { emit_trivia: false });

        let actual = render_case(&input, &toks, &diags);
        let expected = read_to_string(&out_path);

        // Handy: write an .actual file when it mismatches for quick diffing in editors
        if actual != expected {
            let actual_path = in_path.with_extension("actual");
            fs::write(&actual_path, &actual).ok();
            eprintln!(
                "Mismatch for case '{}'. Wrote {}",
                stem,
                actual_path.display()
            );
        }

        assert_eq!(expected, actual, "case '{}'", stem);
    }
}
