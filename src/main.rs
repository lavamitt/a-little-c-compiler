mod codeemission;
mod tackygen;
mod lexer;
mod parser;
mod codegen;

use crate::codeemission::emit_code;
use crate::tackygen::tackygen;
use crate::lexer::Lexer;
use crate::parser::parse_program;
use crate::codegen::codegen;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
	Lexing,
	Parsing,
    Tacky,
    Codegen,
    CodeEmission,
}

// MAIN

fn main() {
    let args: Vec<String> = env::args().collect();

    let (stage, file_path) = parse_args(&args);
    let input_path = Path::new(&file_path);
    let preprocessed_path = preprocess_file(input_path);
    let code = fs::read_to_string(&preprocessed_path);

    match stage {
        Some(stage) => run_up_to_stage(&code, input_path, stage),
        None => run_all_stages(&code, input_path),
    }

    fs::remove_file(preprocessed_path).unwrap_or_else(|e| panic!("Failed to remove preprocessed file: {}", e));

    // // let file_path = &args[1];
    // println!("{}", file_path);
    // let code = fs::read_to_string(file_path).unwrap();

    // // PREPROCESS FILE
    // let preprocessed_file_path = "hi";
    // let gcc_status = Command::new("gcc")
    //     .args(&["-E", "-P", file_path, "-o", preprocessed_file_path])
    //     .status()
    //     .expect("Failed to execute gcc");

    // // LEXING
    // let mut lexer = Lexer::new(&code);
    // let mut tokens = lexer.lex();

    // println!("############## LEXING DEBUG INFO ##############");
    // for token in &tokens {
    //     println!("{:?}", token);
    // }
    // println!("");

    // // PARSING
    // let program = parse_program(tokens.iter());
    // println!("############## PARSING DEBUG INFO ##############");
    // println!("{:?}", program);
    // println!("");

    // // TACKYGEN
    // let tackygen = tackygen(program);
    // println!("############## TACKYGEN DEBUG INFO ##############");
    // println!("{:?}", tackygen);
    // println!("");

    // // CODEGEN
    // let codegen = codegen(tackygen);
    // println!("############## CODEGEN DEBUG INFO ##############");
    // println!("{:?}", codegen);
    // println!("");

    // // CODE EMISSION
    // let assembly = emit_code(codegen);
    // println!("############## CODEEMISSION DEBUG INFO ##############");
    // println!("{assembly}");
    // println!("");

    // println!("############## OUTPUT DEBUG INFO ##############");
    // let path = Path::new(file_path);
    // let stem = path.file_stem().unwrap().to_str().unwrap();
    // let directory = path.parent().unwrap().to_str().unwrap();
    // println!("{}", stem);
    // println!("{}", directory);
    // let output_assembly_path = format!("{}/{}_assembly.s", directory, stem);
    // fs::write(output_assembly_path.to_string(), assembly).expect("Unable to write file");

    // let gcc_output = Command::new("gcc")
    //     .arg(&output_assembly_path)
    //     .arg("-o")
    //     .arg(format!("{}/{}", directory, stem))
    //     .output()
    //     .expect("Failed to execute gcc");

    // if gcc_output.status.success() {
    //     println!("gcc executed successfully");
    //     fs::remove_file(&output_assembly_path).expect("Unable to delete assembly file");
    // } else {
    //     eprintln!("gcc failed to execute");
    //     eprintln!("stderr: {}", String::from_utf8_lossy(&gcc_output.stderr));
    // }
}



fn parse_args(args: &[String]) -> (Option<Stage>, String) {
    if args.len() < 2 {
        panic!("Usage: program [--stage] <file_path>");
    }

    if args.len() == 2 {
        // No stage provided, run all stages
        return (None, args[1].clone());
    }

    let stage = match args[1].as_str() {
        "--lex" => Some(Stage::Lexing),
        "--parse" => Some(Stage::Parsing),
        "--tacky" => Some(Stage::Tacky),
        "--codegen" => Some(Stage::Codegen),
        "--emit" => Some(Stage::CodeEmission),
        _ => return panic!("Invalid stage argument"),
    };

    (stage, args[2].clone())
}

fn preprocess_file(input_path: &Path) -> PathBuf {
    let preprocessed_path = input_path.with_extension("i");
    let status = Command::new("gcc")
        .args(&["-E", "-P", &input_path.to_string_lossy(), "-o", &preprocessed_path.to_string_lossy()])
        .status()
        .unwrap_or_else(|e| panic!("Failed to execute gcc: {}", e));

    if !status.success() {
        panic!("Failed to preprocess file");
    }

    preprocessed_path
}

fn run_lexer(code: &str) -> Vec<lexer::Token> {
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex();
    println!("############## LEXING DEBUG INFO ##############");
    for token in &tokens {
        println!("{:?}", token);
    }
    tokens
}

fn run_parser(code: &str) -> parser::ASTProgram {
    let tokens = run_lexer(code);
    let program = parse_program(tokens.iter());
    println!("############## PARSING DEBUG INFO ##############");
    println!("{:?}", program);
    program
}

fn run_tackygen(code: &str) -> tackygen::TACKYProgram {
    let program = run_parser(code);
    let tackygen = tackygen(program);
    println!("############## TACKYGEN DEBUG INFO ##############");
    println!("{:?}", tackygen);
    tackygen
}

fn run_codegen(code: &str) -> codegen::AssemblyProgram {
    let tackygen = run_tackygen(code);
    let codegen = codegen(tackygen);
    println!("############## CODEGEN DEBUG INFO ##############");
    println!("{:?}", codegen);
    codegen
}

fn run_code_emission(code: &str, input_path: &Path) {
    let codegen = run_codegen(code);
    let assembly = emit_code(codegen);

    println!("############## CODEEMISSION DEBUG INFO ##############");
    println!("{assembly}");

    let stem = input_path.file_stem().unwrap().to_str().unwrap();
    let directory = input_path.parent().unwrap();
    let output_assembly_path = directory.join(format!("{}_assembly.s", stem));
    fs::write(&output_assembly_path, &assembly).unwrap_or_else(|e| panic!("Failed to write assembly file: {}", e));

    let output_executable = directory.join(stem);
    let gcc_output = Command::new("gcc")
        .arg(&output_assembly_path)
        .arg("-o")
        .arg(&output_executable)
        .output()
        .unwrap_or_else(|e| panic!("Failed to execute gcc: {}", e));

    if gcc_output.status.success() {
        println!("gcc executed successfully");
        fs::remove_file(&output_assembly_path).unwrap_or_else(|e| panic!("Failed to remove assembly file: {}", e));
    } else {
        panic!("gcc failed to execute: {}", String::from_utf8_lossy(&gcc_output.stderr));
    }
}

fn run_up_to_stage(code: &str, input_path: &Path, stage: Stage) {
    run_lexer(code);
    if stage == Stage::Lexing {
        return;
    }

    run_parser(code);
    if stage == Stage::Parsing {
        return;
    }

    run_tackygen(code);
    if stage == Stage::Tacky {
        return;
    }

    run_codegen(code);
    if stage == Stage::Codegen {
        return;
    }

    run_code_emission(code, input_path);
}

fn run_all_stages(code: &str, input_path: &Path) {
    run_up_to_stage(code, input_path, Stage::CodeEmission);
}