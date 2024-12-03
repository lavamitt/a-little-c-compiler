use crate::parser::{
    ASTBlock, ASTBlockItem, ASTExpression, ASTForInit, ASTFunctionDeclaration, ASTProgram,
    ASTStatement, ASTVariableDeclaration,
};

use crate::global_context::CompilerContext;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct VariableMapEntry {
    new_name: String,
    from_current_scope: bool,
    has_external_linkage: bool,
}

pub fn semantic_pass(context: &mut CompilerContext, program: ASTProgram) -> ASTProgram {
    // resolve variables
    let mut variable_map: HashMap<String, VariableMapEntry> = HashMap::new();

    let mut resolved_functions: Vec<ASTFunctionDeclaration> = Vec::new();

    for function in program.functions {
        let resolved_function = resolve_function_declaration(context, &function, &mut variable_map);
        resolved_functions.push(resolved_function);
    }

    // annotate labels
    for function in &mut resolved_functions {
        if let Some(ref mut body) = function.body {
            annotate_block(context, body, None);
        }
    }

    ASTProgram {
        functions: resolved_functions,
    }
}

fn resolve_block(
    context: &mut CompilerContext,
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
                let resolved_declaration =
                    resolve_variable_declaration(context, &decl, variable_map);
                resolved_block_items.push(ASTBlockItem::VariableDeclaration(resolved_declaration))
            }
            ASTBlockItem::FunctionDeclaration(decl) => {
                if decl.body.is_some() {
                    panic!("Functions can only be defined in the global scope. Found local defined function: {:?}", decl.name)
                }

                let resolved_declaration =
                    resolve_function_declaration(context, &decl, variable_map);
                resolved_block_items.push(ASTBlockItem::FunctionDeclaration(resolved_declaration))
            }
        }
    }

    ASTBlock {
        items: resolved_block_items,
    }
}

fn resolve_statement(
    context: &mut CompilerContext,
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
            let resolved_for_init =
                match init {
                    ASTForInit::InitDecl(decl) => ASTForInit::InitDecl(
                        resolve_variable_declaration(context, decl, &mut new_scope_variable_map),
                    ),
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

fn resolve_function_declaration(
    context: &mut CompilerContext,
    decl: &ASTFunctionDeclaration,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTFunctionDeclaration {
    if variable_map.contains_key(&decl.name)
        && variable_map.get(&decl.name).unwrap().from_current_scope
        && !variable_map.get(&decl.name).unwrap().has_external_linkage
    {
        panic!("Duplicate function declaration!: {:?}", decl.name);
    }

    let new_entry = VariableMapEntry {
        new_name: decl.name.clone(),
        from_current_scope: true,
        has_external_linkage: true, // will this always be true?
    };
    variable_map.insert(decl.name.clone(), new_entry);

    let mut inner_function_scope_variable_map = copy_variable_map(variable_map);
    let mut resolved_args: Vec<String> = Vec::new();

    for arg in &decl.args {
        let new_name =
            resolve_var_identifier(context, &arg, &mut inner_function_scope_variable_map);
        resolved_args.push(new_name)
    }

    let resolved_body = match &decl.body {
        Some(body) => {
            let _body = resolve_block(context, &body, &mut inner_function_scope_variable_map);
            Some(_body)
        }
        None => None,
    };

    ASTFunctionDeclaration {
        name: decl.name.clone(),
        args: resolved_args,
        body: resolved_body,
    }
}

fn resolve_var_identifier(
    context: &mut CompilerContext,
    name: &String,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> String {
    if variable_map.contains_key(name) && variable_map.get(name).unwrap().from_current_scope {
        panic!("Duplicate variable declaration!: {:?}", name);
    }

    let unique_name = &context
        .helper
        .make_labels_at_same_counter(vec![name.clone()])[0];
    let new_entry = VariableMapEntry {
        new_name: unique_name.clone(),
        from_current_scope: true,
        has_external_linkage: false,
    };

    variable_map.insert(name.clone(), new_entry);
    unique_name.clone()
}

fn resolve_variable_declaration(
    context: &mut CompilerContext,
    decl: &ASTVariableDeclaration,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTVariableDeclaration {
    let new_var_name = resolve_var_identifier(context, &decl.name, variable_map);

    let resolved_init = match &decl.init {
        Some(expr) => Some(resolve_expr(context, &expr, variable_map)),
        None => None,
    };

    ASTVariableDeclaration {
        name: new_var_name.clone(),
        init: resolved_init,
    }
}

fn resolve_expr(
    context: &mut CompilerContext, // technically this isn't used...
    expr: &ASTExpression,
    variable_map: &mut HashMap<String, VariableMapEntry>,
) -> ASTExpression {
    match expr {
        ASTExpression::FunctionCall(name, args) => {
            if variable_map.contains_key(name) {
                let mut resolved_args: Vec<ASTExpression> = Vec::new();
                for arg in args {
                    resolved_args.push(resolve_expr(context, arg, variable_map))
                }
                let new_name = &variable_map.get(name).unwrap().new_name;
                return ASTExpression::FunctionCall(new_name.clone(), resolved_args);
            } else {
                panic!("Tried to call undeclared function!: {:?}", name)
            }
        }
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
        value.from_current_scope = false;
    }
    return new_map;
}

pub fn annotate_block(
    context: &mut CompilerContext,
    block: &mut ASTBlock, // Change to &mut
    current_label: Option<&str>,
) {
    for item in &mut block.items {
        match item {
            ASTBlockItem::Statement(statement) => {
                annotate_statement(context, statement, current_label);
            }
            ASTBlockItem::VariableDeclaration(decl) => {
                annotate_variable_declaration(context, decl, current_label);
            }
            ASTBlockItem::FunctionDeclaration(decl) => {
                annotate_function_declaration(context, decl, current_label);
            }
        }
    }
}

pub fn annotate_statement(
    context: &mut CompilerContext,
    statement: &mut ASTStatement,
    current_label: Option<&str>,
) {
    match statement {
        ASTStatement::Return(expr) => {
            annotate_expr(context, expr, current_label);
        }
        ASTStatement::If(condition, then, or_else) => {
            annotate_expr(context, condition, current_label);
            annotate_statement(context, &mut *then, current_label);
            or_else
                .as_mut()
                .map(|else_stmt| annotate_statement(context, else_stmt, current_label));
        }
        ASTStatement::Expression(expr) => {
            annotate_expr(context, expr, current_label);
        }
        ASTStatement::Compound(block) => {
            annotate_block(context, block, current_label);
        }
        ASTStatement::DoWhile(body, condition, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            annotate_statement(context, body, Some(new_label.as_str()));
            annotate_expr(context, condition, Some(new_label.as_str()));
        }
        ASTStatement::While(condition, body, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            annotate_expr(context, condition, Some(new_label.as_str()));
            annotate_statement(context, body, Some(new_label.as_str()));
        }
        ASTStatement::For(init, condition, post, body, label) => {
            let new_label = &context
                .helper
                .make_labels_at_same_counter(vec!["do_while_".to_string()])[0];
            *label = Some(new_label.clone());

            match init {
                ASTForInit::InitDecl(decl) => {
                    annotate_variable_declaration(context, decl, Some(new_label.as_str()))
                }
                ASTForInit::InitExpr(maybe_expr) => match maybe_expr {
                    Some(expr) => annotate_expr(context, expr, Some(new_label.as_str())),
                    None => {}
                },
            };
            condition
                .as_mut()
                .map(|cond_expr| annotate_expr(context, cond_expr, Some(new_label.as_str())));
            post.as_mut()
                .map(|post_expr| annotate_expr(context, post_expr, Some(new_label.as_str())));

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
        ASTStatement::Null => {}
    }
}

fn annotate_variable_declaration(
    context: &mut CompilerContext,
    decl: &mut ASTVariableDeclaration,
    current_label: Option<&str>,
) {
    match &mut decl.init {
        Some(expr) => Some(annotate_expr(context, expr, current_label)),
        None => None,
    };
}

fn annotate_function_declaration(
    context: &mut CompilerContext,
    decl: &mut ASTFunctionDeclaration,
    current_label: Option<&str>,
) {
    match &mut decl.body {
        Some(body) => Some(annotate_block(context, body, current_label)),
        None => None,
    };
}

fn annotate_expr(
    context: &mut CompilerContext,
    expr: &mut ASTExpression,
    current_label: Option<&str>,
) {
    match expr {
        ASTExpression::Assignment(left, right) => {
            annotate_expr(context, left, current_label);
            annotate_expr(context, right, current_label);
        }

        ASTExpression::Conditional(condition, then, or_else) => {
            annotate_expr(context, condition, current_label);
            annotate_expr(context, then, current_label);
            annotate_expr(context, or_else, current_label);
        }

        ASTExpression::UnaryOperation(_, operated_on_expr) => {
            annotate_expr(context, operated_on_expr, current_label);
        }
        ASTExpression::BinaryOperation(_, left_expr, right_expr) => {
            annotate_expr(context, left_expr, current_label);
            annotate_expr(context, right_expr, current_label);
        }
        _ => {}
    }
}
