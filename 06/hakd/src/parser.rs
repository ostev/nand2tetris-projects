use regex::Regex;

mod ast;
use ast::*;

pub type ParsingResult = Result<Command, ParseError>;

pub type LineNumber = usize;
pub type ColNumber = usize;

#[derive(PartialEq, std::fmt::Debug)]
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
        r"^((@(?P<address>.*)|((?P<destination>.)=)?((?P<x>.)((-|\+)(?P<y>.))?))(;(?P<jmp>...)?))");

    let command_no_whitespace: String = command.chars().filter(|c| !c.is_whitespace()).collect();

    let re: Regex = match re_option {
        Ok(valid_re) => valid_re,
        Err(err) => return Err(ParseError::RegexConstructionError(err))
    };

    let captures = re.captures(&(command_no_whitespace));

    let command: Option<Result<Command, ParseError>> = captures.and_then(|cap| {
        let address = cap.name("address")
                    .map(|address| match address.as_str().parse::<u16>() {
                        Ok(a) => Ok(Command::Address(a)),
                        Err(err) => Err(ParseError::IntegerParsingError(err))
                    });

        let destinations = cap.name("destination")
                        .map(|destination| match destination.as_str() {
                            "A" => Ok(ComputationDestinations::One(ComputationDestination::Register(Register::A))),
                            "D" => Ok(ComputationDestinations::One(ComputationDestination::Register(Register::D))),
                            "M" => Ok(ComputationDestinations::One(ComputationDestination::Memory)),
                            other => Err(ParseError::Unexpected(other.to_string(), destination.start()))
                        });
        
        let x = cap.name("x").map(parse_input);
        
        let y = cap.name("y").map(parse_input);

        if let Some(addr) = address {
            return Some(addr);
        } else if let Some(maybe_x) = x {
            match maybe_x {
                Ok(x_value) =>
                    return match y {
                        Some(Ok(y_value)) =>
                            Some(
                                Ok(
                                    Command::Compute(
                                        Computation {
                                            x: x_value, y: Some(y_value)
                                        },
                                        match_destinations(destinations),
                                        Jump::new(false, false, false)
                                    )
                                )
                            ),
                        Some(Err(err)) => return Some(Err(err)),
                        None =>
                            Some(
                                Ok(
                                    Command::Compute(
                                        Computation {
                                            x: x_value, y: None
                                        },
                                        match_destinations(destinations),
                                        Jump::new(false, false, false)
                                    )
                                )
                            )
                    },   
                Err(err) => return Some(Err(err))
            }
            
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
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Memory
                    ),
                    Jump::new(false, false, false),
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
                    },
                    ComputationDestinations::One(
                        ComputationDestination::Register(Register::D)
                    ),
                    Jump::new(false, false, false),
                ),
            )
        );
    }
}