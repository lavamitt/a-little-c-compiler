use core::panic;

use crate::global_context::CompilerContext;
use crate::parser::{
    ASTBinaryOperator, ASTBlock, ASTBlockItem, ASTExpression, ASTForInit, ASTFunctionDeclaration,
    ASTProgram, ASTStatement, ASTUnaryOperator, ASTVariableDeclaration,
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
    FuncCall(String, Vec<TACKYVal>, TACKYVal), // func_name args dst
}

#[derive(Debug)]
pub struct TACKYFunctionDefinition {
    pub name: String,
    pub args: Vec<String>,
    pub instructions: Vec<TACKYInstruction>,
}

#[derive(Debug)]
pub struct TACKYProgram {
    pub functions: Vec<TACKYFunctionDefinition>,
}

pub fn tackygen(context: &mut CompilerContext, program: ASTProgram) -> TACKYProgram {
    let mut functions: Vec<TACKYFunctionDefinition> = Vec::new();

    for function in program.functions {
        match tackygen_function(context, function) {
            Some(tacky_function) => {
                functions.push(tacky_function);
            }
            None => {}
        }
    }

    TACKYProgram { functions }
}

fn tackygen_function(
    context: &mut CompilerContext,
    function: ASTFunctionDeclaration,
) -> Option<TACKYFunctionDefinition> {
    let name = function.name;

    let mut instructions: Vec<TACKYInstruction> = Vec::new();
    match function.body {
        Some(body) => {
            tackygen_block(context, body, &mut instructions);
        }
        None => return None,
    }

    // just in case the function did not provide a return statement
    instructions.push(TACKYInstruction::Return(TACKYVal::Constant(0)));

    Some(TACKYFunctionDefinition {
        name,
        args: function.args,
        instructions,
    })
}

fn tackygen_block(
    context: &mut CompilerContext,
    block: ASTBlock,
    instructions: &mut Vec<TACKYInstruction>,
) {
    for item in block.items {
        match item {
            ASTBlockItem::Statement(statement) => {
                tackygen_statement(context, statement, instructions)
            }
            ASTBlockItem::VariableDeclaration(decl) => {
                tackygen_declaration(context, decl, instructions)
            }
            ASTBlockItem::FunctionDeclaration(_) => {
                // if this is valid, it would just be a declaration without a body, so do nothing.
            }
        }
    }
}

fn tackygen_statement(
    context: &mut CompilerContext,
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

        ASTStatement::DoWhile(body, condition, label) => {
            let do_while_label =
                label.unwrap_or_else(|| panic!("Expected do while loop to be labeled."));
            let continue_label = format!("{}_{}", "continue", do_while_label);
            let break_label = format!("{}_{}", "break", do_while_label);

            instructions.push(TACKYInstruction::Label(do_while_label.clone()));
            tackygen_statement(context, *body, instructions);
            instructions.push(TACKYInstruction::Label(continue_label));
            let condition_val = tackygen_expression(context, condition, instructions);
            instructions.push(TACKYInstruction::JumpIfNotZero(
                condition_val,
                do_while_label,
            ));
            instructions.push(TACKYInstruction::Label(break_label));
        }
        ASTStatement::While(condition, body, label) => {
            let while_label = label.unwrap_or_else(|| panic!("Expected while loop to be labeled."));
            let continue_label = format!("{}_{}", "continue", while_label);
            let break_label = format!("{}_{}", "break", while_label);

            instructions.push(TACKYInstruction::Label(while_label)); // we don't need this.. but I like it for readability in the assembly
            instructions.push(TACKYInstruction::Label(continue_label.clone()));
            let condition_val = tackygen_expression(context, condition, instructions);
            instructions.push(TACKYInstruction::JumpIfZero(
                condition_val,
                break_label.clone(),
            ));
            tackygen_statement(context, *body, instructions);
            instructions.push(TACKYInstruction::Jump(continue_label));
            instructions.push(TACKYInstruction::Label(break_label));
        }
        ASTStatement::For(init, condition, post, body, label) => {
            let for_label = label.unwrap_or_else(|| panic!("Expected for loop to be labeled."));
            let continue_label = format!("{}_{}", "continue", for_label);
            let break_label = format!("{}_{}", "break", for_label);

            tackygen_for_init(context, init, instructions);
            instructions.push(TACKYInstruction::Label(for_label.clone()));
            match condition {
                Some(expr) => {
                    let conditional_val = tackygen_expression(context, expr, instructions);
                    instructions.push(TACKYInstruction::JumpIfZero(
                        conditional_val,
                        break_label.clone(),
                    ));
                }
                None => {}
            }
            tackygen_statement(context, *body, instructions);
            instructions.push(TACKYInstruction::Label(continue_label));
            match post {
                Some(expr) => {
                    tackygen_expression(context, expr, instructions);
                }
                None => {}
            }
            instructions.push(TACKYInstruction::Jump(for_label));
            instructions.push(TACKYInstruction::Label(break_label));
        }
        ASTStatement::Break(label) => {
            let for_label = label.unwrap_or_else(|| {
                panic!("Found break improperly unlabeled or used outside of loop context.")
            });
            let break_label = format!("{}_{}", "break", for_label);
            instructions.push(TACKYInstruction::Jump(break_label));
        }
        ASTStatement::Continue(label) => {
            let for_label = label.unwrap_or_else(|| {
                panic!("Found continue improperaly unlabeled or used outside of loop context.")
            });
            let continue_label = format!("{}_{}", "continue", for_label);
            instructions.push(TACKYInstruction::Jump(continue_label));
        }
        ASTStatement::Null => {}
    }
}

fn tackygen_for_init(
    context: &mut CompilerContext,
    init: ASTForInit,
    instructions: &mut Vec<TACKYInstruction>,
) {
    match init {
        ASTForInit::InitDecl(decl) => {
            tackygen_declaration(context, decl, instructions);
        }
        ASTForInit::InitExpr(expr) => match expr {
            Some(inner_expr) => {
                tackygen_expression(context, inner_expr, instructions);
            }
            None => {}
        },
    }
}

fn tackygen_declaration(
    context: &mut CompilerContext,
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
    context: &mut CompilerContext,
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
        ASTExpression::FunctionCall(name, args) => {
            let mut arg_srcs: Vec<TACKYVal> = Vec::new();
            for arg in args {
                let arg_src = tackygen_expression(context, arg, instructions);
                arg_srcs.push(arg_src);
            }

            let dst_name = context.helper.make_temporary_register();
            let dst = TACKYVal::Var(dst_name);

            instructions.push(TACKYInstruction::FuncCall(name.clone(), arg_srcs, dst.clone()));
            dst
        }
    };

    operand
}

fn convert_unop(ast_unop: ASTUnaryOperator) -> TACKYUnaryOperator {
    match ast_unop {
        ASTUnaryOperator::Negation => TACKYUnaryOperator::Negate,
        ASTUnaryOperator::BitwiseComplement => TACKYUnaryOperator::Complement,
        ASTUnaryOperator::Not => TACKYUnaryOperator::Not,
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
