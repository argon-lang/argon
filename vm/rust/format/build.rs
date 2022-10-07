use std::fs;
use std::path::PathBuf;
use std::env;
use std::error::Error;
use std::io::Write;

use protobuf_codegen::Codegen;
use serde_json::{Value, Map};


fn main() {
    println!("cargo:rerun-if-changed=../../protobuf/argonvm.proto");
    Codegen::new()
        .protoc()
        .cargo_out_dir("proto")
        .input("../../protobuf/argonvm.proto")
        .include("../../protobuf/")
        .run_from_script();

    println!("cargo:rerun-if-changed=../../bytecode.json");
    generate_instructions().unwrap();
}

fn generate_instructions() -> Result<(), Box<dyn Error>> {
    let bytecode_json: serde_json::Value = {
        let file = fs::File::open("../../bytecode.json")?;
        serde_json::from_reader(file)?
    };


    let mut path = PathBuf::from(env::var("OUT_DIR")?);
    path.push("instructions.rs");

    let mut f = fs::File::create(path)?;

    writeln!(f, "use crate::instruction_util::*;")?;
    writeln!(f, "pub enum Instruction {{")?;

    for (name, insn) in bytecode_json.as_object().unwrap() {
        let insn = normalize_instruction(insn);
        write!(f, "\t{}", format_identifier(name))?;

        let params = insn.get("parameters").unwrap().as_array().unwrap();
        if let Some(param) = params.first() {
            write!(f, "({}", insn_param_type(param.get("type").unwrap().as_str().unwrap()))?;
            for param in params.iter().skip(1) {
                write!(f, ", {}", insn_param_type(param.get("type").unwrap().as_str().unwrap()))?;
            }
            write!(f, ")")?;
        }
        
        
        writeln!(f, ",")?;
    }

    writeln!(f, "}}")?;

    

    writeln!(f, "impl Instruction {{")?;


    writeln!(f, "\tpub fn write<F: std::io::Write>(&self, f: &mut F) -> std::io::Result<()> {{")?;
    writeln!(f, "\t\tmatch self {{")?;

    for (name, insn) in bytecode_json.as_object().unwrap() {
        let insn = normalize_instruction(insn);
        


        write!(f, "\t\t\tInstruction::{}", format_identifier(name))?;
        let params = insn.get("parameters").unwrap().as_array().unwrap();
        if let Some(param) = params.first() {
            write!(f, "({}", param.get("name").unwrap().as_str().unwrap())?;
            for param in params.iter().skip(1) {
                write!(f, ", {}", param.get("name").unwrap().as_str().unwrap())?;
            }
            write!(f, ")")?;
        }
        writeln!(f, " => {{")?;
        writeln!(f, "\t\t\t\tf.write(std::slice::from_ref(&{}))?;", insn.get("opcode").unwrap().as_u64().unwrap())?;
        for param in params {
            write!(f, "\t\t\t\t")?;
            write_insn_param_write(&mut f, param.get("type").unwrap().as_str().unwrap(), param.get("name").unwrap().as_str().unwrap())?;
            writeln!(f, ";")?;
        }
        writeln!(f, "\t\t\t}}")?;
    }
    
    writeln!(f, "\t\t}}")?;
    writeln!(f, "\t\tOk(())")?;
    writeln!(f, "\t}}")?;


    writeln!(f, "\tpub fn read<F: std::io::Read>(f: &mut F) -> std::io::Result<Option<Self>> {{")?;
    writeln!(f, "\t\tlet mut opcode: u8 = 0;")?;
    writeln!(f, "\t\tf.read_exact(std::slice::from_mut(&mut opcode))?;")?;
    writeln!(f, "\t\tOk(match opcode {{")?;

    for (name, insn) in bytecode_json.as_object().unwrap() {
        let insn = normalize_instruction(insn);
        


        write!(f, "\t\t\t{} => {{", insn.get("opcode").unwrap().as_u64().unwrap())?;
        let params = insn.get("parameters").unwrap().as_array().unwrap();
        for param in params {
            write!(f, "\t\t\t\tlet {} = ", param.get("name").unwrap().as_str().unwrap())?;
            write_insn_param_read(&mut f, param.get("type").unwrap().as_str().unwrap())?;
            writeln!(f, ";")?;
        }
        write!(f, "\t\t\t\tSome(Instruction::{}", format_identifier(name))?;
        if let Some(param) = params.first() {
            write!(f, "({}", param.get("name").unwrap().as_str().unwrap())?;
            for param in params.iter().skip(1) {
                write!(f, ", {}", param.get("name").unwrap().as_str().unwrap())?;
            }
            write!(f, ")")?;
        }
        writeln!(f, ")")?;
        writeln!(f, "\t\t\t}},")?;
    }

    writeln!(f, "\t\t\t_ => None,")?;
    
    writeln!(f, "\t\t}})")?;
    writeln!(f, "\t}}")?;


    writeln!(f, "}}")?;


    Ok(())
}


fn normalize_instruction(insn: &Value) -> Map<String, Value> {
    match insn {
        Value::Number(opcode) => {
            let mut map = Map::new();
            map.insert(String::from("opcode"), Value::Number(opcode.clone()));
            map.insert(String::from("parameters"), Value::Array(Vec::new()));
            map
        },
        Value::Object(obj) => obj.clone(),
        _ => panic!("Invalid instruction"),
    }
}

fn format_identifier(name: &str) -> String {
    let mut s = String::new();

    let mut use_upper = true;
    for ch in name.chars() {
        if use_upper {
            s.push(ch);
            use_upper = false;
            continue;
        }
        else if ch == '_' {
            use_upper = true;
        }
        else {
            s.push(ch.to_ascii_lowercase());
        }
    }

    s
}

fn insn_param_type(t: &str) -> &str {
    match t {
        "index" => "usize",
        "int8" => "i8",
        _ => panic!("Unsupported type {}", t),
    }
}

fn write_insn_param_write<F: Write>(f: &mut F, t: &str, name: &str) -> Result<(), Box<dyn Error>> {
    match t {
        "index" => write!(f, "write_index(f, *{})?", name)?,
        "int8" => write!(f, "write_int8(f, *{})?", name)?,
        _ => panic!("Unsupported type {}", t),
    }

    Ok(())
}

fn write_insn_param_read<F: Write>(f: &mut F, t: &str) -> Result<(), Box<dyn Error>> {
    match t {
        "index" => write!(f, "read_index(f)?")?,
        "int8" => write!(f, "read_int8(f)?")?,
        _ => panic!("Unsupported type {}", t),
    }

    Ok(())
}

