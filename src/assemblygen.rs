#[derive(Debug)]
pub enum Reg {
	AX,
    R10
}

#[derive(Debug)]
pub enum Operand {
	Imm(u32),
    Register(Reg),
    Pseudo(String),
    Stack(u32),
}

#[derive(Debug)]
pub enum AssemblyUnaryOperator {
    Neg,
    Not
}

#[derive(Debug)]
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Unary(AssemblyUnaryOperator, Operand),
    AllocateStack(u32),
    Ret
}

#[derive(Debug)]
pub struct AssemblyFunctionDefinition {
    pub name: String,
    pub instructions: Vec<AssemblyInstruction>,
}

#[derive(Debug)]
pub struct AssemblyProgram {
    pub function: TACKYFunctionDefinition,
}

pub fn assemblygen(program: TACKYProgram) -> AssemblyProgram {
    let function = assemblygen_function(program.function);
    AssemblyProgram { function }
}

fn assemblygen_function(function: TACKYFunctionDefinition) -> AssemblyFunctionDefinition {
    let name = function.name;
    let instructions = assemblygen_body(function.instructions);

    AssemblyFunctionDefinition { name, instructions }
}



