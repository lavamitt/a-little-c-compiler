mod codeemission;
mod codegen;
mod global_context;
mod lexer;
mod parser;
mod semantic_analysis;
mod tackygen;

use crate::codeemission::emit_code;
use crate::codegen::codegen;
use crate::global_context::CompilerContext;
use crate::lexer::Lexer;
use crate::parser::parse_program;
use crate::semantic_analysis::semantic_pass;
use crate::tackygen::tackygen;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    Lexing,
    Parsing,
    Validate,
    Tacky,
    Codegen,
    CodeEmission,
}

// MAIN

fn main() {
    let args: Vec<String> = env::args().collect();

    let (generate_object_file, stage, file_path) = parse_args(&args);
    let input_path = Path::new(&file_path);
    let preprocessed_path = preprocess_file(input_path);
    let code = fs::read_to_string(&preprocessed_path).unwrap();

    let maybe_assembly = match stage {
        Some(stage) => run_up_to_stage(&code, stage),
        None => run_all_stages(&code),
    };

    match maybe_assembly {
        Some(assembly) => output_compiled_language(assembly, input_path, generate_object_file),
        None => {}
    }

    fs::remove_file(preprocessed_path)
        .unwrap_or_else(|e| panic!("Failed to remove preprocessed file: {}", e));
}

fn parse_args(args: &[String]) -> (bool, Option<Stage>, String) {
    if args.len() < 2 || args.len() > 4 {
        panic!("Usage: program [-c] [--stage] <file_path>");
    }

    let mut stage = None;
    let mut generate_object_file = false;

    for arg in &args[1..args.len() - 1] {
        match arg.as_str() {
            "--lex" => stage = Some(Stage::Lexing),
            "--parse" => stage = Some(Stage::Parsing),
            "--validate" => stage = Some(Stage::Validate),
            "--tacky" => stage = Some(Stage::Tacky),
            "--codegen" => stage = Some(Stage::Codegen),
            "--emit" => stage = Some(Stage::CodeEmission),
            "-c" => generate_object_file = true,
            _ => panic!("Invalid stage argument"),
        }
    }

    (generate_object_file, stage, args[2].clone())
}

fn preprocess_file(input_path: &Path) -> PathBuf {
    let preprocessed_path = input_path.with_extension("i");
    let status = Command::new("gcc-14")
        .args(&[
            "-E",
            "-P",
            &input_path.to_string_lossy(),
            "-o",
            &preprocessed_path.to_string_lossy(),
        ])
        .status()
        .unwrap_or_else(|e| panic!("Failed to execute gcc: {}", e));

    if !status.success() {
        panic!("Failed to preprocess file");
    }

    preprocessed_path
}

fn output_compiled_language(assembly: String, input_path: &Path, generate_object_file: bool) {
    let stem = input_path.file_stem().unwrap().to_str().unwrap();
    let directory = input_path.parent().unwrap();
    let output_assembly_path = directory.join(format!("{}_assembly.s", stem));
    fs::write(&output_assembly_path, &assembly)
        .unwrap_or_else(|e| panic!("Failed to write assembly file: {}", e));

    let output_executable = directory.join(stem);
    let mut gcc_command = Command::new("gcc");
    gcc_command
        .arg(&output_assembly_path)
        .arg("-o")
        .arg(&output_executable);

    if generate_object_file {
        gcc_command.arg("-c");
    }

    let gcc_output = gcc_command
        .output()
        .unwrap_or_else(|e| panic!("Failed to execute gcc: {}", e));

    if gcc_output.status.success() {
        println!("gcc executed successfully");
        fs::remove_file(&output_assembly_path)
            .unwrap_or_else(|e| panic!("Failed to remove assembly file: {}", e));
    } else {
        panic!(
            "gcc failed to execute: {}",
            String::from_utf8_lossy(&gcc_output.stderr)
        );
    }
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
    let program = parse_program(tokens.iter().peekable());
    println!("############## PARSING DEBUG INFO ##############");
    println!("{:?}", program);
    program
}

fn run_validate(code: &str) -> (parser::ASTProgram, CompilerContext) {
    let program = run_parser(code);
    let mut context = CompilerContext::new();
    let resolved_program = semantic_pass(&mut context, program);
    println!("############## VAIDATE DEBUG INFO ##############");
    println!("{:?}", resolved_program);
    (resolved_program, context)
}

fn run_tackygen(code: &str) -> tackygen::TACKYProgram {
    let (program, mut context) = run_validate(code);
    let tackygen = tackygen(&mut context, program);
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

fn run_code_emission(code: &str) -> String {
    let codegen = run_codegen(code);
    let assembly = emit_code(codegen);
    println!("############## CODEEMISSION DEBUG INFO ##############");
    println!("{assembly}");
    assembly
}

fn run_up_to_stage(code: &str, stage: Stage) -> Option<String> {
    match stage {
        Stage::Lexing => {
            run_lexer(code);
            None
        }
        Stage::Parsing => {
            run_parser(code);
            None
        }
        Stage::Validate => {
            run_validate(code);
            None
        }
        Stage::Tacky => {
            run_tackygen(code);
            None
        }
        Stage::Codegen => {
            run_codegen(code);
            None
        }
        Stage::CodeEmission => {
            let assembly = run_code_emission(code);
            Some(assembly)
        }
    }
}

fn run_all_stages(code: &str) -> Option<String> {
    run_up_to_stage(code, Stage::CodeEmission)
}
