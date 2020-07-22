

pub type Number = u16;

#[derive(PartialEq, std::fmt::Debug)]
pub enum Symbol {
    SP,
    LCL,
    ARG,
    THIS,
    THAT,
    R0, R1, R2, R3, R4, R5, R6, R7,
    R8, R9, R10, R11, R12, R13, R14, R15,
    SCREEN,
    KBD,
    Custom(String)
}

#[derive(PartialEq, std::fmt::Debug)]
pub enum Register {
    A,
    D
}

#[derive(PartialEq, std::fmt::Debug)]
pub enum ComputationDestination {
    Register(Register),
    Memory
}

#[derive(PartialEq, std::fmt::Debug)]
pub enum ComputationDestinations {
    None,
    One(ComputationDestination),
    Two(ComputationDestination,
        ComputationDestination),
    Three(ComputationDestination,
        ComputationDestination,
        ComputationDestination)
}

#[derive(PartialEq, std::fmt::Debug)]
pub enum Input {
    Register(Register),
    Memory,
    Zero,
    One,
    NegativeOne
}

#[derive(PartialEq, std::fmt::Debug)]
pub struct Computation {
    pub x: Input,
    pub y: Option<Input>
}

#[derive(PartialEq, std::fmt::Debug)]
pub struct Jump {
    if_greater_than: bool,
    if_equal_to: bool,
    if_less_than: bool
}

impl Jump {
    pub fn new(if_greater_than: bool,
        if_equal_to: bool,
        if_less_than: bool) -> Jump {
            return Jump {
                if_greater_than,
                if_equal_to,
                if_less_than
            }
    }
}

#[derive(PartialEq, std::fmt::Debug)]
pub enum Command {
    Address(Number),
    Compute(Computation,
        ComputationDestinations,
        Jump)
}