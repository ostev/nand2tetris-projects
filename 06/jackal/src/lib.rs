mod parser;

pub fn compile(code: &str) -> &str {
    return code;
}

#[cfg(test)]
mod tests {
    use super::*;

    const ADD_PROGRAM: &str =
        "@2
        D=A
        @3
        D=D+A
        @0
        M=D";
    
    const ADD_PROGRAM_RESULT: &str =
        "0000000000000010
        1110110000010000
        0000000000000011
        1110000010010000
        0000000000000000
        1110001100001000";

    #[test]
    fn compiles_with_no_symbols() {
        let program = compile(ADD_PROGRAM);
        assert_eq!(program, ADD_PROGRAM_RESULT);
    }
}