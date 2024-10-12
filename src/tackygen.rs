use crate::parser::{
    ASTExpression, ASTFunctionDefinition, ASTProgram, ASTStatement, ASTUnaryOperator, ASTBinaryOperator
};

#[derive(Debug, Clone)]
pub enum TACKYVal {
    Constant(u32),
    Var(String),
}

#[derive(Debug)]
pub enum TACKYUnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum TACKYBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub enum TACKYInstruction {
    Unary(TACKYUnaryOperator, TACKYVal, TACKYVal), // src dst
    Binary(TACKYBinaryOperator, TACKYVal, TACKYVal, TACKYVal), // src1 src2 dst
    Return(TACKYVal),
}

#[derive(Debug)]
pub struct TACKYFunctionDefinition {
    pub name: String,
    pub instructions: Vec<TACKYInstruction>,
}

#[derive(Debug)]
pub struct TACKYProgram {
    pub function: TACKYFunctionDefinition,
}

pub struct TACKYHelperFunctions {
    tmp_register_counter: u32,
}

impl TACKYHelperFunctions {
    fn make_temporary_register(&mut self) -> String {
        // let new_temporary_register = format!("_{}_tmp.{}", function_name, self.tmp_register_counter.to_string());
        let new_temporary_register = format!("tmp.{}", self.tmp_register_counter.to_string());
        self.tmp_register_counter += 1;
        new_temporary_register
    }
}

static mut helper: TACKYHelperFunctions = TACKYHelperFunctions {
    tmp_register_counter: 0,
};

pub fn tackygen(program: ASTProgram) -> TACKYProgram {
    let function = tackygen_function(program.function);
    TACKYProgram { function }
}

fn tackygen_function(function: ASTFunctionDefinition) -> TACKYFunctionDefinition {
    let name = function.name;
    let instructions = tackygen_body(function.body);

    TACKYFunctionDefinition { name, instructions }
}

fn tackygen_body(statement: ASTStatement) -> Vec<TACKYInstruction> {
    let mut instructions: Vec<TACKYInstruction> = Vec::new();

    match statement {
        ASTStatement::Return(expr) => {
            let return_val = tackygen_expression(expr, &mut instructions);
            instructions.push(TACKYInstruction::Return(return_val))
            // instructions.push(Instruction::Mov(return_operand, Operand::Register));
            // instructions.push(Instruction::Ret);
        }
        _ => panic!("Found unknown statement type"),
    };

    instructions
}

fn tackygen_expression(
    expression: ASTExpression,
    instructions: &mut Vec<TACKYInstruction>,
) -> TACKYVal {
    let operand = match expression {
        ASTExpression::Constant(num) => TACKYVal::Constant(num),
        ASTExpression::UnaryOperation(ast_unop, expr) => {
            let src = tackygen_expression(*expr, instructions);
            let dst_name = unsafe { helper.make_temporary_register() }; // we're not running a multithreaded app
            let dst = TACKYVal::Var(dst_name);
            let tacky_unop = convert_unop(ast_unop);
            instructions.push(TACKYInstruction::Unary(tacky_unop, src, dst.clone()));
            dst
        },
        ASTExpression::BinaryOperation(ast_binop, expr1, expr2) => {
            let src1 = tackygen_expression(*expr1, instructions);
            let src2 = tackygen_expression(*expr2, instructions);
            let dst_name = unsafe { helper.make_temporary_register() };
            let dst = TACKYVal::Var(dst_name);
            let tacky_binop = convert_binop(ast_binop);
            instructions.push(TACKYInstruction::Binary(tacky_binop, src1, src2, dst.clone()));
            dst
        }
        _ => panic!("Found unknown expression"),
    };

    operand
}

fn convert_unop(ast_unop: ASTUnaryOperator) -> TACKYUnaryOperator {
    match ast_unop {
        ASTUnaryOperator::Negation => TACKYUnaryOperator::Negate,
        ASTUnaryOperator::BitwiseComplement => TACKYUnaryOperator::Complement,
        _ => panic!("Found unimplemented unary operator"),
    }
}

fn convert_binop(ast_binop: ASTBinaryOperator) -> TACKYBinaryOperator {
    match ast_binop {
        ASTBinaryOperator::Add => TACKYBinaryOperator::Add,
        ASTBinaryOperator::Subtract => TACKYBinaryOperator::Subtract,
        ASTBinaryOperator::Multiply => TACKYBinaryOperator::Multiply,
        ASTBinaryOperator::Divide => TACKYBinaryOperator::Divide,
        ASTBinaryOperator::Remainder => TACKYBinaryOperator::Remainder,
        _ => panic!("Found unimplemented binary operator"),
    }
}
