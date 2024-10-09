use crate::codegen::{
    AssemblyFunctionDefinition, AssemblyInstruction, AssemblyProgram, AssemblyUnaryOperator,
    Operand, Reg,
};
const INDENT: &str = "    ";

pub fn emit_code(program: AssemblyProgram) -> String {
    let mut assembly = String::new();
    let function = emit_function(&program.function, &mut assembly);
    add_epilogue(&mut assembly);
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
                emit_operand(src, assembly);
                assembly.push_str(", ");
                emit_operand(dst, assembly);
                assembly.push_str("\n");
            }
            AssemblyInstruction::Unary(unop, operand) => {
                assembly.push_str(&format!("{}{} ", INDENT, unop_to_assembly_str(unop)));
                emit_operand(operand, assembly);
                assembly.push_str("\n");
            }
            AssemblyInstruction::AllocateStack(offset) => {
                assembly.push_str(&format!("{}subq ${}, %rsp\n", INDENT, offset));
            }
            AssemblyInstruction::Ret => {
                assembly.push_str(&format!("{}movq %rbp, %rsp\n", INDENT));
                assembly.push_str(&format!("{}popq %rbp\n", INDENT));
                assembly.push_str(&format!("{}retq\n", INDENT));
            }
            _ => panic!("Found unknown instruction"),
        };
    }
}

fn emit_operand(operand: &Operand, assembly: &mut String) {
    let constant = match operand {
        Operand::Imm(num) => {
            format!("${}", num)
        }
        Operand::Register(reg) => reg_to_assembly_str(reg).to_string(),
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        _ => panic!("Found unknown operand"),
    };

    assembly.push_str(&constant);
}

fn unop_to_assembly_str(unop: &AssemblyUnaryOperator) -> &str {
    match unop {
        AssemblyUnaryOperator::Not => "notl",
        AssemblyUnaryOperator::Neg => "negl",
    }
}

fn reg_to_assembly_str(reg: &Reg) -> &str {
    match reg {
        Reg::AX => "%eax",
        Reg::R10 => "%r10d",
    }
}
