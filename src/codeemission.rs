use crate::codegen::{TACKYFunctionDefinition, TACKYInstruction, TACKYProgram, TACKYVal};
const INDENT: &str = "    ";

pub fn emit_code(program: AssemblyProgram) -> String {
    let mut assembly = String::new();
    let function = emit_function(&program.function, &mut assembly);
    assembly
}

fn emit_function(function: &AssemblyFunctionDefinition, assembly: &mut String) {
    assembly.push_str(&format!("{}.globl _{}\n", INDENT, function.name));
    assembly.push_str(&format!("_{}:\n", function.name));
    emit_instructions(&function.instructions, assembly);
}

fn emit_instructions(instructions: &[Instruction], assembly: &mut String) {
    for instruction in instructions {
        match instruction {
            Instruction::Mov(src, dst) => {
                assembly.push_str(INDENT);
                assembly.push_str("movl ");
                emit_operand(src, assembly);
                assembly.push_str(", ");
                emit_operand(dst, assembly);
                assembly.push_str("\n");
            }
            Instruction::Ret => {
                assembly.push_str(INDENT);
                assembly.push_str("retq\n");
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
        Operand::Register => "%eax".to_string(),
        _ => panic!("Found unknown operand"),
    };

    assembly.push_str(&constant);
}
