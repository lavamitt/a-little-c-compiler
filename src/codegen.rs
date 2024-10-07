use crate::tackygen::{TACKYFunctionDefinition, TACKYProgram, TACKYInstruction, TACKYVal, TACKYUnaryOperator};

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
    Stack(i32), // Stack(-4) == -4(%rbp)
}

#[derive(Debug)]
pub enum AssemblyUnaryOperator {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Unary(AssemblyUnaryOperator, Operand),
    AllocateStack(u32), // ex: subq $n, %rsp
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
    let instructions = assemblygen_body(&function.instructions);

    AssemblyFunctionDefinition { name, instructions }
}

fn assemblygen_body(instructions: &Vec<TACKYInstruction>) -> Vec<AssemblyInstruction> {
    let mut assembly_instructions: Vec<AssemblyInstruction> = Vec::new();

    for tacky_instruction in instruction {
        match tacky_instruction {
            TACKYInstruction::Unary(unop, src, dst) => {
                let assembly_src: Operand = assemblygen_operand(src);
                let assembly_dst: Operand = assemblygen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Mov(assembly_src, assembly_dst));
                let assembly_unop: AssemblyUnaryOperator = assembly_unop(unop);
                assembly_instructions.push(AssemblyInstruction::Unary(assembly_unop, assembly_dst));
            },
            TACKYInstruction::Return(val) => {
                let operand: Operand = assemblygen_operand(val);
                let ret_reg: Operand = Operand::Register(Reg::AX);
                assembly_instructions.push(AssemblyInstruction::Mov(operand, ret_reg));
                assembly_instructions.push(AssemblyInstruction::Ret);
            }
            _ => panic!("Found unknown TACKYInstruction type"),
        };
    }
    assembly_instructions
}

fn assemblygen_operand(val: TACKYVal) -> Operand {
    match val {
        TACKYVal::Constant(num) => {Operand::Imm(num)},
        TACKYVal::Var(identifier) => {Operand::Pseudo(identifier)}
    }
}

fn assemblygen_unop(unop: TACKYUnaryOperator) -> AssemblyUnaryOperator {
    match unop {
        TACKYUnaryOperator::Complement => {AssemblyUnaryOperator::Not},
        TACKYUnaryOperator::Negate => {AssemblyUnaryOperator::Neg}
    }
}



