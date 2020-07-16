use regex::Regex;

mod ast;
use ast::*;

type Tokens = Vec<Command>;

fn extract_command(command: &str) -> Command {
    let re = Regex::new(r"(@|foo)").unwrap();
    let extracted = re.captures("@").unwrap();

    return Command::Address(5)
}

// fn tokenize(code: String) -> Tokens {
    
// }

// struct Parser{
//     current_command: Option<Command>,
//     tokens: Tokens,
//     input: str
// }

// impl Parser<'_> {
//     pub fn advance(mut self) {
//         self.current_command = parse_command()
//     }
// }