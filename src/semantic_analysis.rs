use crate::parser::{
    ASTBlock, ASTBlockItem, ASTExpression, ASTForInit, ASTFunctionDefinition, ASTProgram,
    ASTStatement, ASTVariableDeclaration,
};
use crate::tackygen::TACKYContext;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct VariableMapEntry {
    new_name: String,
    from_current_block: bool,
}

pub fn semantic_pass(context: &mut TACKYContext, program: ASTProgram) -> ASTProgram {
    // resolve variables
    let mut variable_map: HashMap<String, VariableMapEntry> = HashMap::new();
    let mut resolved_function_body =
        resolve_block(context, &program.function.body, &mut variable_map);

    // annotate labels
    annotate_block(context, &mut resolved_function_body, None);

    ASTProgram {
        function: ASTFunctionDefinition {
            name: program.function.name.clone(), // should we resolve the function name too?
            body: resolved_function_body,
        },
    }
}

pub fn resolve_block(
    context: &mut TACKYContext,
    block: &ASTBlock,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTBlock {
    let mut resolved_block_items: Vec<ASTBlockItem> = Vec::new();

    for item in &block.items {
        match item {
            ASTBlockItem::Statement(statement) => {
                let resolved_statement = resolve_statement(context, &statement, variable_map);
                resolved_block_items.push(ASTBlockItem::Statement(resolved_statement))
            }
            ASTBlockItem::VariableDeclaration(decl) => {
                let resolved_declaration = resolve_declaration(context, &decl, variable_map);
                resolved_block_items.push(ASTBlockItem::VariableDeclaration(resolved_declaration))
            }
        }
    }

    ASTBlock {
        items: resolved_block_items,
    }
}

pub fn resolve_statement(
    context: &mut TACKYContext,
    statement: &ASTStatement,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTStatement {
    match statement {
        ASTStatement::Return(expr) => {
            ASTStatement::Return(resolve_expr(context, expr, variable_map))
        }
        ASTStatement::If(condition, then, or_else) => {
            let resolved_condition = resolve_expr(context, condition, variable_map);
            let resolved_then = resolve_statement(context, then, variable_map);
            let resolved_else = or_else
                .as_ref()
                .map(|else_stmt| resolve_statement(context, else_stmt, variable_map));

            ASTStatement::If(
                resolved_condition,
                Box::new(resolved_then),
                resolved_else.map(Box::new),
            )
        }
        ASTStatement::Expression(expr) => {
            ASTStatement::Expression(resolve_expr(context, expr, variable_map))
        }
        ASTStatement::Compound(block) => {
            let mut new_scope_variable_map = copy_variable_map(variable_map);
            let resolved_block = resolve_block(context, block, &mut new_scope_variable_map);
            ASTStatement::Compound(resolved_block)
        }
        ASTStatement::DoWhile(body, condition, label) => {
            let resolved_body = resolve_statement(context, body, variable_map);
            let resolved_condition = resolve_expr(context, condition, variable_map);
            ASTStatement::DoWhile(Box::new(resolved_body), resolved_condition, label.clone())
        }
        ASTStatement::While(condition, body, label) => {
            let resolved_condition = resolve_expr(context, condition, variable_map);
            let resolved_body = resolve_statement(context, body, variable_map);
            ASTStatement::While(resolved_condition, Box::new(resolved_body), label.clone())
        }
        ASTStatement::For(init, condition, post, body, label) => {
            let mut new_scope_variable_map = copy_variable_map(variable_map);
            let resolved_for_init = match init {
                ASTForInit::InitDecl(decl) => ASTForInit::InitDecl(resolve_declaration(
                    context,
                    decl,
                    &mut new_scope_variable_map,
                )),
                ASTForInit::InitExpr(maybe_expr) => match maybe_expr {
                    Some(expr) => ASTForInit::InitExpr(Some(resolve_expr(
                        context,
                        expr,
                        &mut new_scope_variable_map,
                    ))),
                    None => ASTForInit::InitExpr(None),
                },
            };

            let resolved_condition = match condition {
                Some(expr) => Some(resolve_expr(context, expr, &mut new_scope_variable_map)),
                None => None,
            };

            let resolved_post = match post {
                Some(expr) => Some(resolve_expr(context, expr, &mut new_scope_variable_map)),
                None => None,
            };

            let resolved_body = resolve_statement(context, body, &mut new_scope_variable_map);

            ASTStatement::For(
                resolved_for_init,
                resolved_condition,
                resolved_post,
                Box::new(resolved_body),
                label.clone(),
            )
        }
        ASTStatement::Break(label) => ASTStatement::Break(label.clone()),
        ASTStatement::Continue(label) => ASTStatement::Continue(label.clone()),
        ASTStatement::Null => ASTStatement::Null,
    }
}

pub fn resolve_declaration(
    context: &mut TACKYContext,
    decl: &ASTVariableDeclaration,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTVariableDeclaration {
    if variable_map.contains_key(&decl.name)
        && variable_map.get(&decl.name).unwrap().from_current_block
    {
        panic!("Duplicate variable declaration!: {:?}", decl.name);
    }

    let unique_name = &context
        .helper
        .make_labels_at_same_counter(vec![decl.name.clone()])[0];
    let new_entry = VariableMapEntry {
        new_name: unique_name.clone(),
        from_current_block: true,
    };
    variable_map.insert(decl.name.clone(), new_entry);

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
    context: &mut TACKYContext, // technically this isn't used...
    expr: &ASTExpression,
    variable_map: &mut HashMap<String, VariableMapEntry>,
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

        ASTExpression::Conditional(condition, then, or_else) => {
            return ASTExpression::Conditional(
                Box::new(resolve_expr(context, condition, variable_map)),
                Box::new(resolve_expr(context, then, variable_map)),
                Box::new(resolve_expr(context, or_else, variable_map)),
            )
        }

        ASTExpression::Var(name) => match variable_map.get(name) {
            Some(entry) => {
                return ASTExpression::Var(entry.new_name.clone());
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

fn copy_variable_map(
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> HashMap<String, VariableMapEntry> {
    let mut new_map = variable_map.clone();
    for (_, value) in new_map.iter_mut() {
        value.from_current_block = false;
    }
    return new_map;
}

pub fn annotate_block(
    context: &mut TACKYContext,
    block: &mut ASTBlock, // Change to &mut
    current_label: Option<&str>,
) {
    for item in &mut block.items {
        match item {
            ASTBlockItem::Statement(statement) => {
                annotate_statement(context, statement, current_label);
            }
            _ => {}
        }
    }
}

pub fn annotate_statement(
    context: &mut TACKYContext,
    statement: &mut ASTStatement,
    current_label: Option<&str>,
) {
    match statement {
        ASTStatement::Compound(block) => {
            annotate_block(context, block, current_label);
        }
        ASTStatement::DoWhile(body, condition, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            annotate_statement(context, body, Some(new_label.as_str()));
        }
        ASTStatement::While(condition, body, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            annotate_statement(context, body, Some(new_label.as_str()));
        }
        ASTStatement::For(init, condition, post, body, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            annotate_statement(context, body, Some(new_label.as_str()));
        }
        ASTStatement::Break(label) => match current_label {
            Some(new_label) => {
                *label = Some(new_label.to_string());
            }
            None => {
                panic!("Break used outside of loop context.")
            }
        },
        ASTStatement::Continue(label) => match current_label {
            Some(new_label) => {
                *label = Some(new_label.to_string());
            }
            None => {
                panic!("Continue used outside of loop context.")
            }
        },
        _ => {}
    }
}
