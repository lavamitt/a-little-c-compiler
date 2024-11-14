use crate::parser::{
    ASTBlockItem, ASTExpression, ASTFunctionDefinition, ASTProgram, ASTStatement,
    ASTVariableDeclaration,
};
use crate::tackygen::TACKYContext;
use std::collections::HashMap;

pub fn semantic_pass(context: &mut TACKYContext, program: ASTProgram) -> ASTProgram {
    let mut variable_map: HashMap<String, String> = HashMap::new();

    let mut resolved_function_body: Vec<ASTBlockItem> = Vec::new();
    for item in program.function.body {
        match item {
            ASTBlockItem::Statement(statement) => {
                let resolved_statement = resolve_statement(context, &statement, &mut variable_map);
                resolved_function_body.push(ASTBlockItem::Statement(resolved_statement))
            }
            ASTBlockItem::VariableDeclaration(decl) => {
                let resolved_declaration = resolve_declaration(context, &decl, &mut variable_map);
                resolved_function_body.push(ASTBlockItem::VariableDeclaration(resolved_declaration))
            }
        }
    }

    ASTProgram {
        function: ASTFunctionDefinition {
            name: program.function.name.clone(), // should we resolve the function name too?
            body: resolved_function_body,
        },
    }
}

pub fn resolve_statement(
    context: &mut TACKYContext,
    statement: &ASTStatement,
    variable_map: &mut HashMap<String, String>,
) -> ASTStatement {
    match statement {
        ASTStatement::Return(expr) => {
            ASTStatement::Return(resolve_expr(context, expr, variable_map))
        }
        ASTStatement::Expression(expr) => {
            ASTStatement::Expression(resolve_expr(context, expr, variable_map))
        }
        ASTStatement::Null => ASTStatement::Null,
    }
}

pub fn resolve_declaration(
    context: &mut TACKYContext,
    decl: &ASTVariableDeclaration,
    variable_map: &mut HashMap<String, String>,
) -> ASTVariableDeclaration {
    if variable_map.contains_key(&decl.name) {
        panic!("Duplicate variable declaration!: {:?}", decl.name);
    }

    let unique_name = &context
        .helper
        .make_labels_at_same_counter(vec![decl.name.clone()])[0];
    variable_map.insert(decl.name.clone(), unique_name.clone());

    let resolved_init = match &decl.init {
        Some(expr) => Some(resolve_expr(context, &expr, variable_map)),
        None => None,
    };

    ASTVariableDeclaration {
        name: unique_name.clone(),
        init: resolved_init,
    }
}

pub fn resolve_expr(
    context: &mut TACKYContext,
    expr: &ASTExpression,
    variable_map: &mut HashMap<String, String>,
) -> ASTExpression {
    match expr {
        ASTExpression::Assignment(left, right) => match **left {
            ASTExpression::Var(_) => {
                return ASTExpression::Assignment(
                    Box::new(resolve_expr(context, left, variable_map)),
                    Box::new(resolve_expr(context, right, variable_map)),
                )
            }
            _ => panic!(
                "Left side of assignment must be a var! Got {:?} instead.",
                left
            ),
        },

        ASTExpression::Var(name) => match variable_map.get(name) {
            Some(new_name) => {
                return ASTExpression::Var(new_name.clone());
            }
            None => panic!("Undeclared variable! Found: {:?}", name),
        },

        ASTExpression::UnaryOperation(op, operated_on_expr) => {
            return ASTExpression::UnaryOperation(
                op.clone(),
                Box::new(resolve_expr(context, operated_on_expr, variable_map)),
            )
        }

        ASTExpression::BinaryOperation(op, left_expr, right_expr) => {
            return ASTExpression::BinaryOperation(
                op.clone(),
                Box::new(resolve_expr(context, left_expr, variable_map)),
                Box::new(resolve_expr(context, right_expr, variable_map)),
            )
        }

        ASTExpression::Constant(num) => ASTExpression::Constant(num.clone()),
    }
}
