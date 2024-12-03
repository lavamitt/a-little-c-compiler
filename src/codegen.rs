use crate::tackygen::{
    TACKYBinaryOperator, TACKYFunctionDefinition, TACKYInstruction, TACKYProgram,
    TACKYUnaryOperator, TACKYVal,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
}

#[derive(Debug, Clone)]
pub enum ConditionalCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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
    Complement,
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum AssemblyBinaryOperator {
    Add,
    Sub,
    Mult,
}

#[derive(Debug, Clone)]
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Unary(AssemblyUnaryOperator, Operand),
    Binary(AssemblyBinaryOperator, Operand, Operand),
    Idiv(Operand), // stores quotient in AX and remainder in DX, needs dividend to be across DX AX.
    Cmp(Operand, Operand),
    Cdq, // sign extends AX into DX
    Jmp(String),
    JmpCC(ConditionalCode, String),
    SetCC(ConditionalCode, Operand),
    Label(String),
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
    // TMP UNDO CHANGES HERE
    // let function = codegen_function(program.function);
    let mut assembly_program = AssemblyProgram {
        function: AssemblyFunctionDefinition {
            name: String::new(),
            instructions: Vec::new(),
        },
    };

    // println!("BEFORE FIXES:");
    // println!("{:?}", assembly_program);
    // let offset = replace_pseudo(&mut assembly_program);

    // fix_instructions(&mut assembly_program, offset);
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
            TACKYInstruction::Unary(TACKYUnaryOperator::Not, src, dst) => {
                let src: Operand = codegen_operand(src);
                let dst: Operand = codegen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Cmp(Operand::Imm(0), src.clone()));
                assembly_instructions.push(AssemblyInstruction::Mov(Operand::Imm(0), dst.clone()));
                assembly_instructions
                    .push(AssemblyInstruction::SetCC(ConditionalCode::E, dst.clone()));
            }
            TACKYInstruction::Unary(unop, src, dst) => {
                let src: Operand = codegen_operand(src);
                let dst: Operand = codegen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Mov(src, dst.clone()));
                let assembly_unop: AssemblyUnaryOperator = codegen_unop(unop);
                assembly_instructions.push(AssemblyInstruction::Unary(assembly_unop, dst.clone()));
            }
            TACKYInstruction::Binary(TACKYBinaryOperator::Divide, src1, src2, dst) => {
                let src1: Operand = codegen_operand(src1);
                let src2: Operand = codegen_operand(src2);
                let dst: Operand = codegen_operand(dst);

                assembly_instructions
                    .push(AssemblyInstruction::Mov(src1, Operand::Register(Reg::AX)));
                assembly_instructions.push(AssemblyInstruction::Cdq);
                assembly_instructions.push(AssemblyInstruction::Idiv(src2));
                assembly_instructions
                    .push(AssemblyInstruction::Mov(Operand::Register(Reg::AX), dst));
            }
            TACKYInstruction::Binary(TACKYBinaryOperator::Remainder, src1, src2, dst) => {
                let src1: Operand = codegen_operand(src1);
                let src2: Operand = codegen_operand(src2);
                let dst: Operand = codegen_operand(dst);

                assembly_instructions
                    .push(AssemblyInstruction::Mov(src1, Operand::Register(Reg::AX)));
                assembly_instructions.push(AssemblyInstruction::Cdq);
                assembly_instructions.push(AssemblyInstruction::Idiv(src2));
                assembly_instructions
                    .push(AssemblyInstruction::Mov(Operand::Register(Reg::DX), dst));
            }
            TACKYInstruction::Binary(binop, src1, src2, dst) => {
                let src1: Operand = codegen_operand(src1);
                let src2: Operand = codegen_operand(src2);
                let dst: Operand = codegen_operand(dst);
                let was_comparative_binop = match binop {
                    TACKYBinaryOperator::Equal
                    | TACKYBinaryOperator::NotEqual
                    | TACKYBinaryOperator::GreaterThan
                    | TACKYBinaryOperator::GreaterOrEqual
                    | TACKYBinaryOperator::LessThan
                    | TACKYBinaryOperator::LessOrEqual => {
                        assembly_instructions
                            .push(AssemblyInstruction::Cmp(src2.clone(), src1.clone()));
                        assembly_instructions
                            .push(AssemblyInstruction::Mov(Operand::Imm(0), dst.clone()));
                        assembly_instructions.push(AssemblyInstruction::SetCC(
                            codegen_comparative_binop(binop),
                            dst.clone(),
                        ));
                        true
                    }
                    _ => false,
                };

                if !was_comparative_binop {
                    let binop = match binop {
                        TACKYBinaryOperator::Add => AssemblyBinaryOperator::Add,
                        TACKYBinaryOperator::Subtract => AssemblyBinaryOperator::Sub,
                        TACKYBinaryOperator::Multiply => AssemblyBinaryOperator::Mult,
                        _ => panic!(
                            "No instruction for converting TACKY to Assembly for binary operator: {:?}",
                            binop
                        ),
                    };
                    assembly_instructions.push(AssemblyInstruction::Mov(src1.clone(), dst.clone()));
                    assembly_instructions.push(AssemblyInstruction::Binary(
                        binop,
                        src2.clone(),
                        dst.clone(),
                    ));
                }
            }
            TACKYInstruction::Jump(target) => {
                assembly_instructions.push(AssemblyInstruction::Jmp(target.clone()))
            }
            TACKYInstruction::JumpIfZero(val, target) => {
                let val = codegen_operand(val);
                assembly_instructions.push(AssemblyInstruction::Cmp(Operand::Imm(0), val));
                assembly_instructions.push(AssemblyInstruction::JmpCC(
                    ConditionalCode::E,
                    target.clone(),
                ));
            }
            TACKYInstruction::JumpIfNotZero(val, target) => {
                let val = codegen_operand(val);
                assembly_instructions.push(AssemblyInstruction::Cmp(Operand::Imm(0), val));
                assembly_instructions.push(AssemblyInstruction::JmpCC(
                    ConditionalCode::NE,
                    target.clone(),
                ));
            }
            TACKYInstruction::Copy(src, dst) => {
                let src: Operand = codegen_operand(src);
                let dst: Operand = codegen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Mov(src, dst));
            }
            TACKYInstruction::Label(identifier) => {
                assembly_instructions.push(AssemblyInstruction::Label(identifier.clone()));
            }
            TACKYInstruction::Return(val) => {
                let operand: Operand = codegen_operand(val);
                let ret_reg: Operand = Operand::Register(Reg::AX);
                assembly_instructions.push(AssemblyInstruction::Mov(operand, ret_reg));
                assembly_instructions.push(AssemblyInstruction::Ret);
            }
            // temp TODO REMOVE
            _ => {}
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
        TACKYUnaryOperator::Complement => AssemblyUnaryOperator::Complement,
        TACKYUnaryOperator::Negate => AssemblyUnaryOperator::Neg,
        TACKYUnaryOperator::Not => AssemblyUnaryOperator::Not,
    }
}

fn codegen_comparative_binop(binop: &TACKYBinaryOperator) -> ConditionalCode {
    match binop {
        TACKYBinaryOperator::Equal => ConditionalCode::E,
        TACKYBinaryOperator::NotEqual => ConditionalCode::NE,
        TACKYBinaryOperator::GreaterThan => ConditionalCode::G,
        TACKYBinaryOperator::GreaterOrEqual => ConditionalCode::GE,
        TACKYBinaryOperator::LessThan => ConditionalCode::L,
        TACKYBinaryOperator::LessOrEqual => ConditionalCode::LE,
        _ => panic!(
            "Tried to convert non-comparative binop to conditional code: {:?}",
            binop
        ),
    }
}

pub fn replace_pseudo(assembly_program: &mut AssemblyProgram) -> i32 {
    let mut pseudoregister_map: HashMap<String, i32> = HashMap::new();
    let mut stack_offset: i32 = 0;

    for instruction in &mut assembly_program.function.instructions {
        match instruction {
            AssemblyInstruction::Mov(src, dst) => {
                maybe_replace_identifier_with_register(
                    src,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
                maybe_replace_identifier_with_register(
                    dst,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            AssemblyInstruction::Unary(_, operand) => {
                maybe_replace_identifier_with_register(
                    operand,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            AssemblyInstruction::Binary(_, src, dst) => {
                maybe_replace_identifier_with_register(
                    src,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
                maybe_replace_identifier_with_register(
                    dst,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            AssemblyInstruction::Idiv(operand) => {
                maybe_replace_identifier_with_register(
                    operand,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            AssemblyInstruction::Cmp(src, dst) => {
                maybe_replace_identifier_with_register(
                    src,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
                maybe_replace_identifier_with_register(
                    dst,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            AssemblyInstruction::SetCC(_, operand) => {
                maybe_replace_identifier_with_register(
                    operand,
                    &mut pseudoregister_map,
                    &mut stack_offset,
                );
            }
            _ => {}
        }
    }
    stack_offset
}

pub fn maybe_replace_identifier_with_register(
    operand: &mut Operand,
    pseudoregister_map: &mut HashMap<String, i32>,
    curr_stack_offset: &mut i32,
) {
    if let Operand::Pseudo(pseudo_identifier) = operand {
        let offset = pseudoregister_map
            .entry(pseudo_identifier.clone())
            .or_insert_with(|| {
                *curr_stack_offset -= 4;
                *curr_stack_offset
            });
        *operand = Operand::Stack(*offset as i32);
    }
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
                        continue;
                    }
                }
                fixed_assembly_instructions.push(instruction.clone());
            }
            AssemblyInstruction::Binary(AssemblyBinaryOperator::Mult, src, dst) => {
                if let Operand::Stack(dst_offset) = dst {
                    fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                        dst.clone(),
                        Operand::Register(Reg::R11),
                    ));
                    fixed_assembly_instructions.push(AssemblyInstruction::Binary(
                        AssemblyBinaryOperator::Mult,
                        src.clone(),
                        Operand::Register(Reg::R11),
                    ));
                    fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                        Operand::Register(Reg::R11),
                        dst.clone(),
                    ));
                    continue;
                }

                // maybe this doesnt make sense to put here..
                // if let Operand::Imm(_) = dst {
                //     fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                //         dst.clone(),
                //         Operand::Register(Reg::R11),
                //     ));
                //     fixed_assembly_instructions.push(AssemblyInstruction::Binary(AssemblyBinaryOperator::Mult, src.clone(), Operand::Register(Reg::R11)));
                //     continue;
                // }

                fixed_assembly_instructions.push(instruction.clone());
            }
            AssemblyInstruction::Binary(binop, src, dst) => {
                if let Operand::Stack(src_offset) = src {
                    if let Operand::Stack(dst_offset) = dst {
                        fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                            src.clone(),
                            Operand::Register(Reg::R10),
                        ));
                        fixed_assembly_instructions.push(AssemblyInstruction::Binary(
                            binop.clone(),
                            Operand::Register(Reg::R10),
                            dst.clone(),
                        ));
                        continue;
                    }
                }

                // maybe this doesnt make sense to put here..
                // if let Operand::Imm(_) = dst {
                //     fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                //         dst.clone(),
                //         Operand::Register(Reg::R11),
                //     ));
                //     fixed_assembly_instructions.push(AssemblyInstruction::Binary(binop.clone(), src.clone(), Operand::Register(Reg::R11)));
                //     continue;
                // }
                fixed_assembly_instructions.push(instruction.clone());
            }
            AssemblyInstruction::Idiv(operand) => {
                if let Operand::Imm(num) = operand {
                    fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                        operand.clone(),
                        Operand::Register(Reg::R10),
                    ));
                    fixed_assembly_instructions
                        .push(AssemblyInstruction::Idiv(Operand::Register(Reg::R10)));
                    continue;
                }
                fixed_assembly_instructions.push(instruction.clone());
            }
            AssemblyInstruction::Cmp(src, dst) => {
                if let Operand::Stack(src_offset) = src {
                    if let Operand::Stack(dst_offset) = dst {
                        fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                            src.clone(),
                            Operand::Register(Reg::R10),
                        ));
                        fixed_assembly_instructions.push(AssemblyInstruction::Cmp(
                            Operand::Register(Reg::R10),
                            dst.clone(),
                        ));
                        continue;
                    }
                }

                if let Operand::Imm(_) = dst {
                    fixed_assembly_instructions.push(AssemblyInstruction::Mov(
                        dst.clone(),
                        Operand::Register(Reg::R11),
                    ));
                    fixed_assembly_instructions.push(AssemblyInstruction::Cmp(
                        src.clone(),
                        Operand::Register(Reg::R11),
                    ));
                    continue;
                }
                fixed_assembly_instructions.push(instruction.clone());
            }
            _ => fixed_assembly_instructions.push(instruction.clone()),
        }
    }

    assembly_program.function.instructions = fixed_assembly_instructions;
}
