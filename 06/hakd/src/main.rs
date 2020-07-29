mod parser;

fn main() {
    println!("{:#?}", parser::parse_command("5=M+1"))
}