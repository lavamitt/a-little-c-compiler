mod lexer;
mod parser;
mod codegen;

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use crate::lexer::Lexer;
use crate::parser::parse_program;
use crate::codegen::codegen;


// MAIN

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let code = fs::read_to_string(file_path).unwrap();
    let mut lexer = Lexer::new(&code);
    let mut tokens = lexer.lex();

    for token in &tokens {
        println!("{:?}", token);
    }

    let program = parse_program(tokens.iter());
    println!("{:?}", program);

    let mut assembly = String::new();
    codegen(program, &mut assembly);
    println!("{assembly}");

    let path = Path::new(file_path);
    let output_assembly_path = format!(
        "src/{}_assembly.s",
        path.file_name().unwrap().to_str().unwrap()
    );
    fs::write(output_assembly_path.to_string(), assembly).expect("Unable to write file");

    let gcc_output = Command::new("gcc")
        .arg(output_assembly_path)
        .arg("-o")
        .arg("out")
        .output()
        .expect("Failed to execute gcc");

    if gcc_output.status.success() {
        println!("gcc executed successfully");
    } else {
        eprintln!("gcc failed to execute");
        eprintln!("stderr: {}", String::from_utf8_lossy(&gcc_output.stderr));
    }
}
