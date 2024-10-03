use crate::parser::{Expression, FunctionDeclaration, Program, Statement};

#[derive(Debug)]
pub enum Operand {
    Imm(u32),
    Register,
}

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand), // src dst
    Ret,
}

#[derive(Debug)]
pub struct AssemblyFunctionDefinition {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct AssemblyProgram {
    pub function: AssemblyFunctionDefinition,
}

pub fn codegen(program: Program) -> AssemblyProgram {
    let function = codegen_function(program.function);
    AssemblyProgram { function }
}

fn codegen_function(function: FunctionDeclaration) -> AssemblyFunctionDefinition {
    let name = function.name;
    let instructions = codegen_body(function.body);

    AssemblyFunctionDefinition { name, instructions }
}

fn codegen_body(statement: Statement) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();

    match statement {
        Statement::Return(expr) => {
            let return_operand = codegen_expression(expr);
            instructions.push(Instruction::Mov(return_operand, Operand::Register));
            instructions.push(Instruction::Ret);
        }
        _ => panic!("Found unknown statement type"),
    };

    instructions
}

fn codegen_expression(expression: Expression) -> Operand {
    let operand = match expression {
        Expression::Constant(num) => Operand::Imm(num),
        _ => panic!("Found unknown expression"),
    };

    operand
}
