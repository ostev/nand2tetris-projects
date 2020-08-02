use regex::Regex;

mod ast;
use ast::*;

pub type ParsingResult = Result<Command, ParseError>;

pub type LineNumber = usize;
pub type ColNumber = usize;

// TODO: Implement Copy trait

#[derive(PartialEq, std::fmt::Debug, Clone)]
pub enum ParseError {
    Unexpected(String, ColNumber),
    RegexConstructionError(regex::Error),
    IntegerParsingError(std::num::ParseIntError),
    InputExpected,
    CommandExpected,
    GenericParseError
}

fn parse_input(input: regex::Match) -> Result<Input, ParseError> {
    return match input.as_str() {
        "0" => Ok(Input::Zero),
        "1" => Ok(Input::One),
        "-1" => Ok(Input::NegativeOne),
        "A" => Ok(Input::Register(Register::A)),
        "D" => Ok(Input::Register(Register::D)),
        "M" => Ok(Input::Memory),
        "" => Err(ParseError::InputExpected),
        other => Err(ParseError::Unexpected(other.to_string(), input.start()))
    };
}

fn parse_destination(destination: regex::Match)
    -> Result<ComputationDestinations, ParseError> {
    match destination.as_str() {
        "A" => Ok(ComputationDestinations::One(ComputationDestination::Register(Register::A))),
        "D" => Ok(ComputationDestinations::One(ComputationDestination::Register(Register::D))),
        "M" => Ok(ComputationDestinations::One(ComputationDestination::Memory)),
        other
            => Err(ParseError::Unexpected(
                other.to_string(), destination.start()
            ))
    }
}

fn parse_jump(jump: regex::Match)
    -> Result<Jump, ParseError> {
        let new = Jump::new;

        return match jump.as_str() {
            "JGT" => Ok(new(
                false, 
                false, 
                false
            )),
            "JEQ" => Ok(new(
                false,
                true,
                false
            )),
            "JGE" => Ok(new(
                false,
                true,
                true
            )),
            "JLT" => Ok(new(
                true,
                false,
                false
            )),
            "JNE" => Ok(new(
                true,
                false,
                true
            )),
            "JLE" => Ok(new(
                true,
                true,
                false
            )),
            "JMP" => Ok(new(
                true,
                true,
                true
            )),
            "" => Ok(new(
                false,
                false,
                false
            )),
            other
                => Err(ParseError::Unexpected(
                    other.to_string(),
                    jump.start()
                ))
        };
    }

fn parse_address_command(address: regex::Match) -> Result<Command, ParseError> {
    match address.as_str().parse::<u16>() {
        Ok(a)
            => Ok(Command::Address(a)),
        Err(err)
            => Err(ParseError::IntegerParsingError(err))
    }
}

fn match_destinations(destinations:
    Option<Result<ComputationDestinations, ParseError>>) -> ComputationDestinations {
    match destinations {
        Some(Ok(dest))
            => dest,
        Some(Err(_)) => ComputationDestinations::None,
        None => ComputationDestinations::None
    }
}

pub fn parse_command(command: &str) -> ParsingResult {
    // TODO: Implement jump parsing

    if command == "" {
        return Err(ParseError::CommandExpected);
    }

    let re_option = Regex::new(
        r"^((@(?P<address>.*)|((?P<destination>.)=)?((?P<x>.)((-|\+)(?P<y>.))?))(;(?P<jmp>...))?)");

    let command_no_whitespace: String = command.chars().filter(|c| !c.is_whitespace()).collect();

    let re: Regex = match re_option {
        Ok(valid_re) => valid_re,
        Err(err) => return Err(ParseError::RegexConstructionError(err))
    };

    let captures = re.captures(&(command_no_whitespace));

    let command: Option<Result<Command, ParseError>> = captures.and_then(|cap| {
        let extracted_address_command =
                cap.name("address")
                .map(parse_address_command);

        let extracted_destinations
            = cap.name("destination")
            .map(parse_destination);

        let extracted_jump
            = cap.name("jmp")
            .map(parse_jump);
        
        let extracted_x = cap.name("x").map(parse_input);
        
        let extracted_y = cap.name("y").map(parse_input);

        if let Some(addr) = extracted_address_command {
            return Some(addr);
        } else if let Some(result_x) = extracted_x {
            let command: Result<Command, ParseError> = result_x.and_then(
                |x| {
                    let dest_with_default
                        = extracted_destinations.unwrap_or(
                            Ok(ComputationDestinations::None)
                        );
                    
                    let jump_with_default 
                        = extracted_jump.unwrap_or(
                            Ok(Jump::new(
                                false, 
                                false, 
                                false)));

                    if let Err(err) = extracted_y.clone().unwrap_or(Ok(Input::Memory)) {
                        return Err(err);
                    }

                    if let Err(err) = dest_with_default {
                        return Err(err);
                    }

                    if let Err(err) = jump_with_default {
                        return Err(err);
                    }

                    // This code is unreachable if there is an error state
                    let unwrapped_y = extracted_y.map(|y| y.unwrap());
                    let unwrapped_jump = jump_with_default.unwrap();
                    let unwrapped_dest = dest_with_default.unwrap();

                    return Ok(Command::Compute(Computation {
                        x,
                        y: unwrapped_y,
                        jump: unwrapped_jump
                    }, unwrapped_dest));
                });
            return Some(command);
        } else {
            return None;
        }
    });
    
    return command.unwrap_or(Err(ParseError::GenericParseError));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_addressing_command() {
        for i in (0..200).step_by(4) {
            for j in ["", " ", "  "].iter() {
                let parsed =
                    parse_command(
                        &("@".to_string() + j + &i.to_string())
                    );
                assert_eq!(parsed, Ok(Command::Address(i)));
            }
        }
    }

    #[test]
    fn parses_computation_command() {
        assert_eq!(
            parse_command("M=M+1"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Memory,
                        y: Some(
                            Input::One,
                        ),
                        jump: Jump::new(false, false, false)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Memory
                    ),
                ),
            )
        );
        assert_eq!(
            parse_command("D=M+0"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Memory,
                        y: Some(
                            Input::Zero,
                        ),
                        jump: Jump::new(false, false, false)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    )
                ),
            )
        );
        assert_eq!(
            parse_command("D=A+1;JEQ"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Register(Register::A),
                        y: Some(
                            Input::One,
                        ),
                        jump: Jump::new(false, true, false)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    )
                ),
            )
        );
        assert_eq!(
            parse_command("D=A+1;JMP"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Register(Register::A),
                        y: Some(
                            Input::One,
                        ),
                        jump: Jump::new(true, true, true)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    )
                ),
            )
        );
        assert_eq!(
            parse_command("D=A+1;JLT"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Register(Register::A),
                        y: Some(
                            Input::One,
                        ),
                        jump: Jump::new(true, false, false)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    )
                ),
            )
        );
        assert_eq!(
            parse_command("D=A+1;JLE"),
            Ok(
                Command::Compute(
                    Computation {
                        x: Input::Register(Register::A),
                        y: Some(
                            Input::One,
                        ),
                        jump: Jump::new(true, true, false)
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    )
                ),
            )
        );
    }
}