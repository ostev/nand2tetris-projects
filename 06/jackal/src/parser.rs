use regex::Regex;

mod ast;
use ast::*;

pub type ParsingResult = Result<Command, ParseError>;

pub type LineNumber = isize;
pub type ColNumber = isize;

#[derive(PartialEq, std::fmt::Debug)]
pub enum ParseError {
    Unexpected(String, LineNumber, ColNumber),
    RegexConstructionError(regex::Error),
    IntegerParsingError(std::num::ParseIntError),
    CommandExpected,
    GenericParseError
}

pub fn parse_command(command: &str) -> ParsingResult {
    if command == "" {
        return Err(ParseError::CommandExpected);
    }

    let re_option = Regex::new(
        r"^((@(?P<address>\d+)|((?P<destination>D|M|A)=)?((?P<x>0|1|-1|D|M|A)((-|\+)(?P<y>0|1|-1|D|M|A))?)))");

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
                            "A" => ComputationDestinations::One(ComputationDestination::Register(Register::A)),
                            "D" => ComputationDestinations::One(ComputationDestination::Register(Register::D)),
                            "M" => ComputationDestinations::One(ComputationDestination::Memory),
                            &_ => todo!(" ")
                        });
        
        let x = cap.name("x")
                .map(|x| match x.as_str() {
                    "0" => Input::Zero,
                    "1" => Input::One,
                    "-1" => Input::NegativeOne,
                    "A" => Input::Register(Register::A),
                    "D" => Input::Register(Register::D),
                    "M" => Input::Memory,
                    &_ => unreachable!()
                });
        
        let y = cap.name("y")
                .map(|y| match y.as_str() {
                    "0" => Input::Zero,
                    "1" => Input::One,
                    "-1" => Input::NegativeOne,
                    "A" => Input::Register(Register::A),
                    "D" => Input::Register(Register::D),
                    "M" => Input::Memory,
                    &_ => unreachable!()
                });
        
        if let Some(Ok(a)) = address {
            // Addressing command
            return Some(Ok(a));
        } else if let Some(x) = x {
            println!("hello");
            if let Some(outputs) = destinations {
                return
                    Some(
                        Ok(
                            Command::Compute(
                                Computation {
                                    x,
                                    y
                                },
                                outputs,
                                Jump::new(false,false,false)
                            )
                        )
                    );
            } else {
                return
                    Some(
                        Ok(
                            Command::Compute(
                                Computation {
                                    x,
                                    y
                                },
                                ComputationDestinations::None,
                                Jump::new(false,false,false)
                            )
                        )
                    );
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

    // #[test]
    // fn parses_computation_command() {
    //     for j in ["M=", "D=", "A=", ""].iter() {
    //         for k in ["M", "D", "A", "-1", "0", "1"].iter() {
    //             for l in ["M", "M", "D", "A", "-1", "0", "1"].iter() {
    //                 let parsed = parse_command(&(l));
    //             }
    //         }
    //     }
    // }
}