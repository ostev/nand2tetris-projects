mod parser;

fn main() {
    println!("{:#?}", parser::parse_command("M=M+1;JEQ"))
}