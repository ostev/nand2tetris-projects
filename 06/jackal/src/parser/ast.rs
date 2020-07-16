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

pub enum Register {
    A,
    D
}

pub enum ComputationDestination {
    Register(Register),
    Memory
}

pub enum ComputationDestinations {
    None,
    One(ComputationDestination),
    Two(ComputationDestination,
        ComputationDestination),
    Three(ComputationDestination,
        ComputationDestination,
        ComputationDestination)
}

pub struct Computation {
    use_a_register: bool,
    c1: bool,
    c2: bool,
    c3: bool,
    c4: bool,
    c5: bool,
    c6: bool
}

pub enum Jump {
    None,
    Jump {
        if_greater_than: bool,
        if_equal_to: bool,
        if_less_than: bool
    }
}

pub enum Command {
    Address(i16),
    Compute(Computation,
        ComputationDestinations,
        Jump)
}