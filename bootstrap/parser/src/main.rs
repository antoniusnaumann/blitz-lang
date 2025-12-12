use std::env::args;

use parser::Lexer;
use type_macro::*;

anti_types!();

fn main() {
    let path = args().skip(1).next().unwrap();
    let content = std::fs::read_to_string(path).unwrap();
    let mut lexer = Lexer::new(&content);

    let token = lexer.next_token();
    println!(
        "{:#?} : '{}'",
        token.kind,
        &content[token.span.start..=token.span.end]
    );

    for token in lexer {
        println!(
            "{:#?} : '{}'",
            token.kind,
            &content[token.span.start..=token.span.end].replace('\n', "\\n")
        );
    }
}
