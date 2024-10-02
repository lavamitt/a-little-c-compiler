mod codegen;
mod lexer;
mod parser;

use crate::codegen::codegen;
use crate::lexer::Lexer;
use crate::parser::parse_program;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

// MAIN

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("{}", file_path);
    let code = fs::read_to_string(file_path).unwrap();

    // LEXING
    let mut lexer = Lexer::new(&code);
    let mut tokens = lexer.lex();

    println!("############## LEXING DEBUG INFO ##############");
    for token in &tokens {
        println!("{:?}", token);
    }
    println!("");

    // PARSING
    let program = parse_program(tokens.iter());
    println!("############## PARSING DEBUG INFO ##############");
    println!("{:?}", program);
    println!("");

    // CODEGEN
    let mut assembly = String::new();
    codegen(program, &mut assembly);
    println!("############## CODEGEN DEBUG INFO ##############");
    println!("{assembly}");
    println!("");

    println!("############## OUTPUT DEBUG INFO ##############");
    let path = Path::new(file_path);
    let stem = path.file_stem().unwrap().to_str().unwrap();
    let directory = path.parent().unwrap().to_str().unwrap();
    println!("{}", stem);
    println!("{}", directory);
    let output_assembly_path = format!("{}/{}_assembly.s", directory, stem);
    fs::write(output_assembly_path.to_string(), assembly).expect("Unable to write file");

    let gcc_output = Command::new("gcc")
        .arg(&output_assembly_path)
        .arg("-o")
        .arg(format!("{}/{}", directory, stem))
        .output()
        .expect("Failed to execute gcc");

    if gcc_output.status.success() {
        println!("gcc executed successfully");
        fs::remove_file(&output_assembly_path).expect("Unable to delete assembly file");
    } else {
        eprintln!("gcc failed to execute");
        eprintln!("stderr: {}", String::from_utf8_lossy(&gcc_output.stderr));
    }
}
