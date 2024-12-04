use crate::codegen::{
    AssemblyBinaryOperator, AssemblyFunctionDefinition, AssemblyInstruction, AssemblyProgram,
    AssemblyUnaryOperator, ConditionalCode, Operand, Reg,
};
const INDENT: &str = "    ";

pub fn emit_code(program: AssemblyProgram) -> String {
    let mut assembly = String::new();
    for function in program.functions {
        emit_function(&function, &mut assembly);
        add_epilogue(&mut assembly);
    }

    assembly
}

pub fn add_epilogue(assembly: &mut String) {}

fn emit_function(function: &AssemblyFunctionDefinition, assembly: &mut String) {
    assembly.push_str(&format!("{}.globl _{}\n", INDENT, function.name));
    assembly.push_str(&format!("_{}:\n", function.name));
    assembly.push_str(&format!("{}pushq %rbp\n", INDENT));
    assembly.push_str(&format!("{}movq %rsp, %rbp\n", INDENT));
    emit_instructions(&function.instructions, assembly);
}

fn emit_instructions(instructions: &[AssemblyInstruction], assembly: &mut String) {
    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Mov(src, dst) => {
                assembly.push_str(INDENT);
                assembly.push_str("movl ");
                emit_operand(src, assembly, OperandNumBytes::Four);
                assembly.push_str(", ");
                emit_operand(dst, assembly, OperandNumBytes::Four);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Unary(unop, operand) => {
                assembly.push_str(&format!("{}{} ", INDENT, unop_to_assembly_str(unop)));
                emit_operand(operand, assembly, OperandNumBytes::Four);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Binary(binop, src, dst) => {
                assembly.push_str(&format!("{}{} ", INDENT, binop_to_assembly_str(binop)));
                emit_operand(src, assembly, OperandNumBytes::Four);
                assembly.push_str(", ");
                emit_operand(dst, assembly, OperandNumBytes::Four);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Idiv(operand) => {
                assembly.push_str("idivl ");
                emit_operand(operand, assembly, OperandNumBytes::Four);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Cdq => {
                assembly.push_str("cdq\n");
            }
            AssemblyInstruction::Cmp(op1, op2) => {
                assembly.push_str(&format!("{}cmpl ", INDENT));
                emit_operand(op1, assembly, OperandNumBytes::Four);
                assembly.push_str(", ");
                emit_operand(op2, assembly, OperandNumBytes::Four);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Jmp(label) => {
                let local_label = &format!(".L{}", label);
                assembly.push_str(&format!("{}jmp {}\n", INDENT, local_label));
            }
            AssemblyInstruction::JmpCC(cond_code, label) => {
                let local_label = &format!(".L{}", label);
                let code = cond_code_to_assembly_str(cond_code);
                assembly.push_str(&format!("{}j{} {}\n", INDENT, code, local_label));
            }
            AssemblyInstruction::SetCC(cond_code, operand) => {
                let code = cond_code_to_assembly_str(cond_code);
                assembly.push_str(&format!("{}set{} ", INDENT, code));
                emit_operand(operand, assembly, OperandNumBytes::One);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Label(label) => {
                assembly.push_str(&format!(".L{}:\n", label));
            }
            AssemblyInstruction::AllocateStack(offset) => {
                assembly.push_str(&format!("{}subq ${}, %rsp\n", INDENT, offset));
            }
            AssemblyInstruction::DeallocateStack(offset) => {
                assembly.push_str(&format!("{}addq ${}, %rsp\n", INDENT, offset));
            }
            AssemblyInstruction::Push(operand) => {
                assembly.push_str(&format!("{}pushq ", INDENT));
                emit_operand(operand, assembly, OperandNumBytes::Eight);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Call(func_name) => {
                assembly.push_str(&format!("{}call _{}\n", INDENT, func_name));
            }
            AssemblyInstruction::Ret => {
                assembly.push_str(&format!("{}movq %rbp, %rsp\n", INDENT));
                assembly.push_str(&format!("{}popq %rbp\n", INDENT));
                assembly.push_str(&format!("{}retq\n", INDENT));
            }
        };
    }
}

pub enum OperandNumBytes {
    One,
    Four,
    Eight,
}

fn emit_operand(operand: &Operand, assembly: &mut String, num_bytes: OperandNumBytes) {
    let constant = match operand {
        Operand::Imm(num) => {
            format!("${}", num)
        }
        Operand::Register(reg) => reg_to_assembly_str(reg, num_bytes).to_string(),
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        _ => panic!("Found unknown operand"),
    };

    assembly.push_str(&constant);
}

fn unop_to_assembly_str(unop: &AssemblyUnaryOperator) -> &str {
    match unop {
        AssemblyUnaryOperator::Complement => "notl",
        AssemblyUnaryOperator::Neg => "negl",
        AssemblyUnaryOperator::Not => "notl", // FIX
    }
}

fn binop_to_assembly_str(binop: &AssemblyBinaryOperator) -> &str {
    match binop {
        AssemblyBinaryOperator::Add => "addl",
        AssemblyBinaryOperator::Sub => "subl",
        AssemblyBinaryOperator::Mult => "imull",
    }
}

fn reg_to_assembly_str(reg: &Reg, num_bytes: OperandNumBytes) -> &str {
    match reg {
        Reg::AX => match num_bytes {
            OperandNumBytes::Eight => "%rax",
            OperandNumBytes::Four => "%eax",
            OperandNumBytes::One => "%al",
        },
        Reg::DX => match num_bytes {
            OperandNumBytes::Eight => "%rdx",
            OperandNumBytes::Four => "%edx",
            OperandNumBytes::One => "%dl",
        },
        Reg::CX => match num_bytes {
            OperandNumBytes::Eight => "%rcx",
            OperandNumBytes::Four => "%ecx",
            OperandNumBytes::One => "%cl",
        },
        Reg::DI => match num_bytes {
            OperandNumBytes::Eight => "%rdi",
            OperandNumBytes::Four => "%edi",
            OperandNumBytes::One => "%dil",
        },
        Reg::SI => match num_bytes {
            OperandNumBytes::Eight => "%rsi",
            OperandNumBytes::Four => "%esi",
            OperandNumBytes::One => "%sil",
        },
        Reg::R8 => match num_bytes {
            OperandNumBytes::Eight => "%r8",
            OperandNumBytes::Four => "%r8d",
            OperandNumBytes::One => "%r8b",
        },
        Reg::R9 => match num_bytes {
            OperandNumBytes::Eight => "%r9",
            OperandNumBytes::Four => "%r9d",
            OperandNumBytes::One => "%r9b",
        },
        Reg::R10 => match num_bytes {
            OperandNumBytes::Eight => "%r10",
            OperandNumBytes::Four => "%r10d",
            OperandNumBytes::One => "%r10b",
        },
        Reg::R11 => match num_bytes {
            OperandNumBytes::Eight => "%r11",
            OperandNumBytes::Four => "%r11d",
            OperandNumBytes::One => "%r11b",
        },
    }
}

fn cond_code_to_assembly_str(code: &ConditionalCode) -> &str {
    match code {
        ConditionalCode::E => "e",
        ConditionalCode::NE => "ne",
        ConditionalCode::G => "g",
        ConditionalCode::GE => "ge",
        ConditionalCode::L => "l",
        ConditionalCode::LE => "le",
    }
}
