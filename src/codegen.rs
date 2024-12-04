use crate::tackygen::{
    TACKYBinaryOperator, TACKYFunctionDefinition, TACKYInstruction, TACKYProgram,
    TACKYUnaryOperator, TACKYVal,
};
use std::collections::HashMap;

/// caller-saved == the caller should save the vals in these registers before calling a function
/// all other registers are callee-saved, we can assume the vals will stay the same when returning to us.
#[derive(Debug, Clone)]
pub enum Reg {
    /// typically stores return values, also used in math, caller-saved
    AX,
    /// fourth param, loop counting, string ops, repeated instructions, caller-saved
    CX,
    /// third param, I/O, overflow in math, caller-saved
    DX,
    /// first param, destination pointer in string ops, caller-saved
    DI,
    /// second param, source pointer in string ops, caller-saved
    SI,
    /// fifth param, caller-saved
    R8,
    /// sixth param, caller-saved
    R9,
    /// caller-saved
    R10,
    /// caller-saved
    R11,
}

static ARG_REGISTERS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

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
    AllocateStack(u32),   // ex: subq $n, %rsp
    DeallocateStack(u32), // ex: addq $n, %rsp
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, Clone)]
pub struct AssemblyFunctionDefinition {
    pub name: String,
    pub instructions: Vec<AssemblyInstruction>,
    pub stack_size: Option<i32>,
}

#[derive(Debug, Clone)]
pub struct AssemblyProgram {
    pub functions: Vec<AssemblyFunctionDefinition>,
}

pub fn codegen(program: TACKYProgram) -> AssemblyProgram {
    let mut codegen_functions: Vec<AssemblyFunctionDefinition> = Vec::new();
    for function in program.functions {
        let codegen_function = codegen_function(function);
        codegen_functions.push(codegen_function);
    }

    let mut assembly_program = AssemblyProgram {
        functions: codegen_functions,
    };

    println!("BEFORE FIXES:");
    println!("{:?}", assembly_program);
    replace_pseudo(&mut assembly_program);

    fix_instructions(&mut assembly_program);
    assembly_program
}

fn codegen_function(function: TACKYFunctionDefinition) -> AssemblyFunctionDefinition {
    let name = function.name;
    let num_args = function.args.len();

    let mut instructions: Vec<AssemblyInstruction> = Vec::new();

    let i = 0;
    while i < num_args {
        let stack_pseudo_reg = Operand::Pseudo(format!("param{}", i));
        if i < ARG_REGISTERS.len() {
            let current_param_reg = Operand::Register(ARG_REGISTERS[i].clone());
            instructions.push(AssemblyInstruction::Mov(
                current_param_reg,
                stack_pseudo_reg,
            ));
        } else {
            let offset = (i - (ARG_REGISTERS.len() - 1)) * 8 + 8;
            let current_param_stack = Operand::Stack(offset as i32);
            instructions.push(AssemblyInstruction::Mov(
                current_param_stack,
                stack_pseudo_reg,
            ));
        }
    }

    instructions.extend(codegen_body(&function.instructions));

    AssemblyFunctionDefinition {
        name,
        instructions,
        stack_size: None,
    }
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
            TACKYInstruction::FuncCall(name, args, dst) => {
                // adjust stack alignment
                let register_args = &args[0..6];
                let stack_args = &args[6..];

                let mut stack_padding = 0;
                if stack_args.len() % 2 == 1 {
                    stack_padding = 8;
                }

                assembly_instructions.push(AssemblyInstruction::AllocateStack(stack_padding));
                for (i, register_arg) in register_args.iter().enumerate() {
                    let reg = Operand::Register(ARG_REGISTERS[i].clone());
                    let assembly_arg = codegen_operand(register_arg);
                    assembly_instructions.push(AssemblyInstruction::Mov(assembly_arg, reg));
                }

                for (i, stack_arg) in stack_args.iter().enumerate().rev() {
                    let assembly_arg = codegen_operand(stack_arg);
                    match assembly_arg {
                        Operand::Imm(_) | Operand::Register(_) => {
                            assembly_instructions.push(AssemblyInstruction::Push(assembly_arg))
                        }
                        _ => {
                            assembly_instructions.push(AssemblyInstruction::Mov(
                                assembly_arg,
                                Operand::Register(Reg::AX),
                            ));
                            assembly_instructions
                                .push(AssemblyInstruction::Push(Operand::Register(Reg::AX)));
                        }
                    }
                }

                assembly_instructions.push(AssemblyInstruction::Call(name.clone()));

                let bytes_to_remove = 8 * (stack_args.len() as u32) + stack_padding;
                if bytes_to_remove != 0 {
                    assembly_instructions
                        .push(AssemblyInstruction::DeallocateStack(bytes_to_remove))
                }

                let assembly_dst = codegen_operand(dst);
                assembly_instructions.push(AssemblyInstruction::Mov(
                    Operand::Register(Reg::AX),
                    assembly_dst,
                ));
            }
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

pub fn replace_pseudo(assembly_program: &mut AssemblyProgram) {
    for function in &mut assembly_program.functions {
        let mut pseudoregister_map: HashMap<String, i32> = HashMap::new();
        let mut stack_offset: i32 = 0;

        for instruction in &mut function.instructions {
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
                AssemblyInstruction::Push(operand) => {
                    maybe_replace_identifier_with_register(
                        operand,
                        &mut pseudoregister_map,
                        &mut stack_offset,
                    );
                }
                _ => {}
            }
        }
        function.stack_size = Some(stack_offset);
    }
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

pub fn fix_instructions(assembly_program: &mut AssemblyProgram) {
    for function in &mut assembly_program.functions {
        let mut fixed_assembly_instructions: Vec<AssemblyInstruction> = Vec::new();
        let offset = function.stack_size.expect(&format!("Found function without calculated stack size: {}", function.name));

        fixed_assembly_instructions.push(AssemblyInstruction::AllocateStack(offset.abs() as u32));

        for instruction in &mut function.instructions {
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

        function.instructions = fixed_assembly_instructions;
    }
}
