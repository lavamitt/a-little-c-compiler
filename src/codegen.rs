use crate::tackygen::{
    TACKYFunctionDefinition, TACKYInstruction, TACKYProgram, TACKYUnaryOperator, TACKYVal,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Reg {
    AX,
    R10,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(u32),
    Register(Reg),
    Pseudo(String),
    Stack(i32), // Stack(-4) == -4(%rbp)
}

#[derive(Debug, Clone)]
pub enum AssemblyUnaryOperator {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Unary(AssemblyUnaryOperator, Operand),
    AllocateStack(u32), // ex: subq $n, %rsp
    Ret,
}

#[derive(Debug, Clone)]
pub struct AssemblyFunctionDefinition {
    pub name: String,
    pub instructions: Vec<AssemblyInstruction>,
}

#[derive(Debug, Clone)]
pub struct AssemblyProgram {
    pub function: AssemblyFunctionDefinition,
}

pub fn codegen(program: TACKYProgram) -> AssemblyProgram {
    let function = codegen_function(program.function);
    let mut assembly_program = AssemblyProgram { function };
    println!("BEFORE FIXES:");
    println!("{:?}", assembly_program);
    let offset = replace_pseudo(&mut assembly_program);
    fix_instructions(&mut assembly_program, offset);
    assembly_program
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
                assembly_instructions
                    .push(AssemblyInstruction::Mov(assembly_src, assembly_dst.clone()));
                let assembly_unop: AssemblyUnaryOperator = codegen_unop(unop);
                assembly_instructions.push(AssemblyInstruction::Unary(assembly_unop, assembly_dst));
            }
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
        TACKYVal::Constant(num) => Operand::Imm(*num),
        TACKYVal::Var(identifier) => Operand::Pseudo(identifier.clone()),
    }
}

fn codegen_unop(unop: &TACKYUnaryOperator) -> AssemblyUnaryOperator {
    match unop {
        TACKYUnaryOperator::Complement => AssemblyUnaryOperator::Not,
        TACKYUnaryOperator::Negate => AssemblyUnaryOperator::Neg,
    }
}

pub fn replace_pseudo(assembly_program: &mut AssemblyProgram) -> i32 {
    let mut pseudoregister_map: HashMap<String, i32> = HashMap::new();
    let mut stack_offset: i32 = 0;

    for instruction in &mut assembly_program.function.instructions {
        match instruction {
            AssemblyInstruction::Mov(src, dst) => {
                if let Operand::Pseudo(pseudo_identifier) = src {
                    let offset = pseudoregister_map
                        .entry(pseudo_identifier.clone())
                        .or_insert_with(|| {
                            stack_offset -= 4;
                            stack_offset
                        });
                    *src = Operand::Stack(*offset as i32);
                }

                if let Operand::Pseudo(pseudo_identifier) = dst {
                    let offset = pseudoregister_map
                        .entry(pseudo_identifier.clone())
                        .or_insert_with(|| {
                            stack_offset -= 4;
                            stack_offset
                        });
                    *dst = Operand::Stack(*offset as i32);
                }
            }
            AssemblyInstruction::Unary(unop, operand) => {
                if let Operand::Pseudo(pseudo_identifier) = operand {
                    let offset = pseudoregister_map
                        .entry(pseudo_identifier.clone())
                        .or_insert_with(|| {
                            stack_offset -= 4;
                            stack_offset
                        });
                    *operand = Operand::Stack(*offset as i32);
                }
            }
            _ => {}
        }
    }
    stack_offset
}

pub fn fix_instructions(assembly_program: &mut AssemblyProgram, offset: i32) {
    let mut fixed_assembly_instructions: Vec<AssemblyInstruction> = Vec::new();
    fixed_assembly_instructions.push(AssemblyInstruction::AllocateStack(offset.abs() as u32));

    for instruction in &mut assembly_program.function.instructions {
        match instruction {
            AssemblyInstruction::Mov(src, dst) => {
                if let Operand::Stack(src_offset) = src {
                    if let Operand::Stack(dst_offset) = dst {
                        fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                            src.clone(),
                            Operand::Register(Reg::R10),
                        ));
                        fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                            Operand::Register(Reg::R10),
                            dst.clone(),
                        ));
                    } else {
                        fixed_assembly_instructions.push(instruction.clone());
                    }
                } else {
                    fixed_assembly_instructions.push(instruction.clone());
                }
            }
            _ => fixed_assembly_instructions.push(instruction.clone()),
        }
    }

    assembly_program.function.instructions = fixed_assembly_instructions;
}
