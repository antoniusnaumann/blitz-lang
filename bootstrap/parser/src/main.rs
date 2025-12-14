use std::env::args;

use parser::Parser;

fn main() {
    let path = args().skip(1).next().unwrap();
    let content = std::fs::read_to_string(path).unwrap();
    let mut parser = Parser::new(&content);

    for item in parser {
        println!(
            "{:#?} : '{}'",
            item,
            &content[item.span().start..=item.span().end].replace('\n', "\\n")
        );
    }
}
