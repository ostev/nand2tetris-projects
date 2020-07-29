mod parser;

fn main() {
    println!("{:#?}", parser::parse_command("M=1+0"))
}