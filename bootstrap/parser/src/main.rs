use std::env::args;

use parser::{Parser, Print};

fn main() {
    let path = args().skip(1).next().unwrap();
    let content = std::fs::read_to_string(path).unwrap();
    let parser = Parser::new(&content);

    for item in parser {
        println!(
            "{}", // " <== '{}'\n",
            item.print(),
            // &content[item.span().start..=item.span().end].replace('\n', "\\n")
        );
    }
}
