use crate::parser::{Expression, FunctionDeclaration, Program, Statement};

pub fn codegen(program: Program, assembly: &mut String) {
    codegen_function(program.function, assembly);
}

fn codegen_function(function: FunctionDeclaration, assembly: &mut String) {
    assembly.push_str(format!(".globl _{}\n", function.name).as_str());
    assembly.push_str(format!("_{}:\n", function.name).as_str());

    codegen_statement(function.body, assembly);
}

fn codegen_statement(statement: Statement, assembly: &mut String) {
    match statement {
        Statement::Return(expr) => {
            assembly.push_str("movl ");
            codegen_expression(expr, assembly);
            assembly.push_str(", %eax\n");
            assembly.push_str("retq\n");
        }
        _ => panic!("Found unknown statement type"),
    };
}

fn codegen_expression(expression: Expression, assembly: &mut String) {
    let constant = match expression {
        Expression::Constant(num) => {
            format!("${}", num)
        }
        _ => panic!("Found unknown expression"),
    };

    assembly.push_str(constant.as_str());
}


