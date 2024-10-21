use crate::parser::{
    ASTBinaryOperator, ASTExpression, ASTFunctionDefinition, ASTProgram, ASTStatement,
    ASTUnaryOperator,
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
    Not,
}

#[derive(Debug)]
pub enum TACKYBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug)]
pub enum TACKYInstruction {
    Return(TACKYVal),
    Unary(TACKYUnaryOperator, TACKYVal, TACKYVal), // src dst
    Binary(TACKYBinaryOperator, TACKYVal, TACKYVal, TACKYVal), // src1 src2 dst
    Copy(TACKYVal, TACKYVal),                      // src dst
    Jump(String),
    JumpIfZero(TACKYVal, String),
    JumpIfNotZero(TACKYVal, String),
    Label(String),
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
    label_counter: u32,
}

impl TACKYHelperFunctions {
    fn make_temporary_register(&mut self) -> String {
        let new_temporary_register = format!("tmp.{}", self.tmp_register_counter.to_string());
        self.tmp_register_counter += 1;
        new_temporary_register
    }

    fn make_labels_at_same_counter(&mut self, prefixes: Vec<String>) -> Vec<String> {
        let mut new_labels: Vec<String> = Vec::new();
        for prefix in prefixes {
            new_labels.push(format!("{}.{}", prefix, self.label_counter.to_string()))
        }
        self.label_counter += 1;
        new_labels
    }
}

static mut helper: TACKYHelperFunctions = TACKYHelperFunctions {
    tmp_register_counter: 0,
    label_counter: 0,
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
        }
        ASTExpression::BinaryOperation(ASTBinaryOperator::And, expr1, expr2) => {
            let src1 = tackygen_expression(*expr1, instructions);
            let labels = unsafe {
                helper
                    .make_labels_at_same_counter(vec!["and_false_".to_string(), "end_".to_string()])
            };
            let false_label = &labels[0];
            let end = &labels[1];
            instructions.push(TACKYInstruction::JumpIfZero(src1, false_label.clone()));
            let src2 = tackygen_expression(*expr2, instructions);
            instructions.push(TACKYInstruction::JumpIfZero(src2, false_label.clone()));
            let dst_name = unsafe { helper.make_temporary_register() };
            let dst = TACKYVal::Var(dst_name);
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(1), dst.clone()));
            instructions.push(TACKYInstruction::Jump(end.clone()));
            instructions.push(TACKYInstruction::Label(false_label.clone()));
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(0), dst.clone()));
            instructions.push(TACKYInstruction::Label(end.clone()));
            dst
        }
        ASTExpression::BinaryOperation(ASTBinaryOperator::Or, expr1, expr2) => {
            let src1 = tackygen_expression(*expr1, instructions);
            let labels = unsafe {
                helper.make_labels_at_same_counter(vec!["or_true_".to_string(), "end_".to_string()])
            };
            let true_label = &labels[0];
            let end = &labels[1];
            instructions.push(TACKYInstruction::JumpIfNotZero(src1, true_label.clone()));
            let src2 = tackygen_expression(*expr2, instructions);
            instructions.push(TACKYInstruction::JumpIfNotZero(src2, true_label.clone()));
            let dst_name = unsafe { helper.make_temporary_register() };
            let dst = TACKYVal::Var(dst_name);
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(0), dst.clone()));
            instructions.push(TACKYInstruction::Jump(end.clone()));
            instructions.push(TACKYInstruction::Label(true_label.clone()));
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(1), dst.clone()));
            instructions.push(TACKYInstruction::Label(end.clone()));
            dst
        }
        ASTExpression::BinaryOperation(ast_binop, expr1, expr2) => {
            let src1 = tackygen_expression(*expr1, instructions);
            let src2 = tackygen_expression(*expr2, instructions);
            let dst_name = unsafe { helper.make_temporary_register() };
            let dst = TACKYVal::Var(dst_name);
            let tacky_binop = convert_binop(ast_binop);
            instructions.push(TACKYInstruction::Binary(
                tacky_binop,
                src1,
                src2,
                dst.clone(),
            ));
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
        ASTUnaryOperator::Not => TACKYUnaryOperator::Not,
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
        ASTBinaryOperator::IsEqual => TACKYBinaryOperator::Equal,
        ASTBinaryOperator::NotEqual => TACKYBinaryOperator::NotEqual,
        ASTBinaryOperator::LessThan => TACKYBinaryOperator::LessThan,
        ASTBinaryOperator::LessOrEqual => TACKYBinaryOperator::LessOrEqual,
        ASTBinaryOperator::GreaterThan => TACKYBinaryOperator::GreaterThan,
        ASTBinaryOperator::GreaterOrEqual => TACKYBinaryOperator::GreaterOrEqual,
        _ => panic!("Found unimplemented binary operator"),
    }
}
