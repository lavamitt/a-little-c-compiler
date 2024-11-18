use core::panic;

use crate::parser::{
    ASTBinaryOperator, ASTBlockItem, ASTExpression, ASTFunctionDefinition, ASTProgram,
    ASTStatement, ASTUnaryOperator, ASTVariableDeclaration, ASTBlock
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
    pub(crate) tmp_register_counter: u32,
    pub(crate) label_counter: u32,
}

impl TACKYHelperFunctions {
    pub fn make_temporary_register(&mut self) -> String {
        let new_temporary_register = format!("tmp.{}", self.tmp_register_counter.to_string());
        self.tmp_register_counter += 1;
        new_temporary_register
    }

    pub fn make_labels_at_same_counter(&mut self, prefixes: Vec<String>) -> Vec<String> {
        let mut new_labels: Vec<String> = Vec::new();
        for prefix in prefixes {
            new_labels.push(format!("{}.{}", prefix, self.label_counter.to_string()))
        }
        self.label_counter += 1;
        new_labels
    }
}

pub struct TACKYContext {
    pub helper: TACKYHelperFunctions,
}

impl TACKYContext {
    pub fn new() -> Self {
        Self {
            helper: TACKYHelperFunctions {
                tmp_register_counter: 0,
                label_counter: 0,
            },
        }
    }
}

pub fn tackygen(context: &mut TACKYContext, program: ASTProgram) -> TACKYProgram {
    let function = tackygen_function(context, program.function);
    TACKYProgram { function }
}

fn tackygen_function(
    context: &mut TACKYContext,
    function: ASTFunctionDefinition,
) -> TACKYFunctionDefinition {
    let name = function.name;

    let mut instructions: Vec<TACKYInstruction> = Vec::new();
    tackygen_block(context, function.body, &mut instructions);

    // just in case the function did not provide a return statement
    instructions.push(TACKYInstruction::Return(TACKYVal::Constant(0)));


    TACKYFunctionDefinition { name, instructions }
}

fn tackygen_block(context: &mut TACKYContext, block: ASTBlock, instructions: &mut Vec<TACKYInstruction>) {
    for item in block.items {
        match item {
            ASTBlockItem::Statement(statement) => {
                tackygen_statement(context, statement, instructions)
            }
            ASTBlockItem::VariableDeclaration(decl) => {
                tackygen_declaration(context, decl, instructions)
            }
        }
    }
}

fn tackygen_statement(
    context: &mut TACKYContext,
    statement: ASTStatement,
    instructions: &mut Vec<TACKYInstruction>,
) {
    match statement {
        ASTStatement::Expression(expr) => {
            tackygen_expression(context, expr, instructions);
        }
        ASTStatement::If(condition, then, or_else) => {
            let condition_val = tackygen_expression(context, condition, instructions);

            match or_else {
                Some(else_statement) => {
                    let labels = context.helper.make_labels_at_same_counter(vec![
                        "if_else_".to_string(),
                        "end_".to_string(),
                    ]);
                    let else_label = labels[0].clone();
                    let end_label = labels[1].clone();

                    instructions.push(TACKYInstruction::JumpIfZero(
                        condition_val,
                        else_label.clone(),
                    ));

                    tackygen_statement(context, *then, instructions);
                    instructions.push(TACKYInstruction::Jump(end_label.clone()));

                    instructions.push(TACKYInstruction::Label(else_label));
                    tackygen_statement(context, *else_statement, instructions);

                    instructions.push(TACKYInstruction::Label(end_label));
                }
                None => {
                    let labels = context
                        .helper
                        .make_labels_at_same_counter(vec!["end_".to_string()]);
                    let end_label = labels[0].clone();

                    instructions.push(TACKYInstruction::JumpIfZero(
                        condition_val,
                        end_label.clone(),
                    ));

                    tackygen_statement(context, *then, instructions);
                    instructions.push(TACKYInstruction::Jump(end_label.clone()));

                    instructions.push(TACKYInstruction::Label(end_label));
                }
            }
        }
        ASTStatement::Compound(block) => {
            tackygen_block(context, block, instructions);
        }  
        ASTStatement::Return(expr) => {
            let return_val = tackygen_expression(context, expr, instructions);
            instructions.push(TACKYInstruction::Return(return_val));
        }
        ASTStatement::Null => {}
    }
}

fn tackygen_declaration(
    context: &mut TACKYContext,
    decl: ASTVariableDeclaration,
    instructions: &mut Vec<TACKYInstruction>,
) {
    match decl.init {
        Some(expr) => {
            let val = tackygen_expression(context, expr, instructions);
            instructions.push(TACKYInstruction::Copy(
                val,
                TACKYVal::Var(decl.name.clone()),
            ));
        }
        None => {}
    }
}

fn tackygen_expression(
    context: &mut TACKYContext,
    expression: ASTExpression,
    instructions: &mut Vec<TACKYInstruction>,
) -> TACKYVal {
    let operand = match expression {
        ASTExpression::Constant(num) => TACKYVal::Constant(num),
        ASTExpression::Var(name) => TACKYVal::Var(name),
        ASTExpression::Assignment(lvalue, expr) => {
            match *lvalue {
                ASTExpression::Var(lvalue_name) => {
                    let result = tackygen_expression(context, *expr, instructions);
                    instructions.push(TACKYInstruction::Copy(result, TACKYVal::Var(lvalue_name.clone())));
                    TACKYVal::Var(lvalue_name)
                }
                _ => panic!("Expected LHS of assignment to be a Variable, this should have been caught in the semantic pass. {:?}", *lvalue)
            }
        }
        ASTExpression::Conditional(condition, then, or_else) => {
            let condition_val = tackygen_expression(context, *condition, instructions);

            let labels = context
                .helper
                .make_labels_at_same_counter(vec!["if_else_".to_string(), "end_".to_string()]);
            let else_label = labels[0].clone();
            let end_label = labels[1].clone();
            let result_name = context.helper.make_temporary_register();
            let result = TACKYVal::Var(result_name);

            instructions.push(TACKYInstruction::JumpIfZero(condition_val, else_label.clone()));

            let then_val = tackygen_expression(context, *then, instructions);
            instructions.push(TACKYInstruction::Copy(then_val, result.clone()));
            instructions.push(TACKYInstruction::Jump(end_label.clone()));

            instructions.push(TACKYInstruction::Label(else_label));
            let else_val = tackygen_expression(context, *or_else, instructions);
            instructions.push(TACKYInstruction::Copy(else_val, result.clone()));

            instructions.push(TACKYInstruction::Label(end_label));
            result
        }
        ASTExpression::UnaryOperation(ast_unop, expr) => {
            let src = tackygen_expression(context, *expr, instructions);
            let dst_name = context.helper.make_temporary_register();
            let dst = TACKYVal::Var(dst_name);
            let tacky_unop = convert_unop(ast_unop);
            instructions.push(TACKYInstruction::Unary(tacky_unop, src, dst.clone()));
            dst
        }
        ASTExpression::BinaryOperation(ASTBinaryOperator::And, expr1, expr2) => {
            let src1 = tackygen_expression(context, *expr1, instructions);
            let labels = context
                .helper
                .make_labels_at_same_counter(vec!["and_false_".to_string(), "end_".to_string()]);
            let false_label = &labels[0];
            let end = &labels[1];
            instructions.push(TACKYInstruction::JumpIfZero(src1, false_label.clone()));
            let src2 = tackygen_expression(context, *expr2, instructions);
            instructions.push(TACKYInstruction::JumpIfZero(src2, false_label.clone()));
            let dst_name = context.helper.make_temporary_register();
            let dst = TACKYVal::Var(dst_name);
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(1), dst.clone()));
            instructions.push(TACKYInstruction::Jump(end.clone()));
            instructions.push(TACKYInstruction::Label(false_label.clone()));
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(0), dst.clone()));
            instructions.push(TACKYInstruction::Label(end.clone()));
            dst
        }
        ASTExpression::BinaryOperation(ASTBinaryOperator::Or, expr1, expr2) => {
            let src1 = tackygen_expression(context, *expr1, instructions);
            let labels = context
                .helper
                .make_labels_at_same_counter(vec!["or_true_".to_string(), "end_".to_string()]);
            let true_label = &labels[0];
            let end = &labels[1];
            instructions.push(TACKYInstruction::JumpIfNotZero(src1, true_label.clone()));
            let src2 = tackygen_expression(context, *expr2, instructions);
            instructions.push(TACKYInstruction::JumpIfNotZero(src2, true_label.clone()));
            let dst_name = context.helper.make_temporary_register();
            let dst = TACKYVal::Var(dst_name);
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(0), dst.clone()));
            instructions.push(TACKYInstruction::Jump(end.clone()));
            instructions.push(TACKYInstruction::Label(true_label.clone()));
            instructions.push(TACKYInstruction::Copy(TACKYVal::Constant(1), dst.clone()));
            instructions.push(TACKYInstruction::Label(end.clone()));
            dst
        }
        ASTExpression::BinaryOperation(ast_binop, expr1, expr2) => {
            let src1 = tackygen_expression(context, *expr1, instructions);
            let src2 = tackygen_expression(context, *expr2, instructions);
            let dst_name = context.helper.make_temporary_register();
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
