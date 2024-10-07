use crate::tackygen::{TACKYFunctionDefinition, TACKYProgram, TACKYInstruction, TACKYVal, TACKYUnaryOperator};

#[derive(Debug, Clone)]
pub enum Reg {
	AX,
    R10
}

#[derive(Debug, Clone)]
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
    pub function: AssemblyFunctionDefinition,
}

pub fn codegen(program: TACKYProgram) -> AssemblyProgram {
    let function = codegen_function(program.function);
    AssemblyProgram { function }
}

fn codegen_function(function: TACKYFunctionDefinition) -> AssemblyFunctionDefinition {
    let name = function.name;
    let instructions = codegen_body(&function.instructions);

    AssemblyFunctionDefinition { name, instructions }
}

fn codegen_body(instructions: &Vec<TACKYInstruction>) -> Vec<AssemblyInstruction> {
    let mut assembly_instructions: Vec<AssemblyInstruction> = Vec::new();

    for tacky_instruction in instructions {
        match tacky_instruction {
            TACKYInstruction::Unary(unop, src, dst) => {
                let assembly_src: Operand = codegen_operand(src);
                let assembly_dst: Operand = codegen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Mov(assembly_src, assembly_dst.clone()));
                let assembly_unop: AssemblyUnaryOperator = codegen_unop(unop);
                assembly_instructions.push(AssemblyInstruction::Unary(assembly_unop, assembly_dst));
            },
            TACKYInstruction::Return(val) => {
                let operand: Operand = codegen_operand(val);
                let ret_reg: Operand = Operand::Register(Reg::AX);
                assembly_instructions.push(AssemblyInstruction::Mov(operand, ret_reg));
                assembly_instructions.push(AssemblyInstruction::Ret);
            }
            _ => panic!("Found unknown TACKYInstruction type"),
        };
    }
    assembly_instructions
}

fn codegen_operand(val: &TACKYVal) -> Operand {
    match val {
        TACKYVal::Constant(num) => {Operand::Imm(*num)},
        TACKYVal::Var(identifier) => {Operand::Pseudo(identifier.clone())}
    }
}

fn codegen_unop(unop: &TACKYUnaryOperator) -> AssemblyUnaryOperator {
    match unop {
        TACKYUnaryOperator::Complement => {AssemblyUnaryOperator::Not},
        TACKYUnaryOperator::Negate => {AssemblyUnaryOperator::Neg}
    }
}



