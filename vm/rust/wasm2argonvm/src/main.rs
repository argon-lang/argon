use std::{error::Error, io::Write};
use std::path::Path;
use std::ffi::OsString;
use std::collections::HashMap;
use wasmparser::{Parser, Payload, TypeRef, Type, ValType, Operator, BlockType, FunctionBody, DataKind, OperatorsReader};
use protobuf::{MessageField, EnumOrUnknown, Message, CodedOutputStream};

use argon_vm_format::{proto::argonvm, instructions::Instruction};

struct InvalidWasm(String);

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<OsString> = std::env::args_os().skip(1).collect();
    let wasmpath = Path::new(&args[0]);
    let outpath = Path::new(&args[1]);

    let wasmbytes = std::fs::read(wasmpath)?;


    let mut converter = Converter {
        ..Default::default()
    };

    converter.load(&wasmbytes)?;

    let mut outfile = std::fs::File::create(outpath)?;
    converter.write(&mut outfile)?;


    Ok(())
}


#[derive(Default)]
struct Converter {
    wasm_types: Vec<Type>,
    function_types: Vec<u32>,

    next_function_index: u32,
    decl_function_indexes: Vec<u32>,
    next_code_index: u32,
    function_indexes: HashMap<u32, usize>,
    next_memory_index: u32,
    memory_global_indexes: HashMap<u32, usize>,


    functions: Vec<argonvm::Function>,
    global_types: Vec<argonvm::ValueType>,
    native_functions: HashMap<String, usize>,

    start_function_index: Option<u32>,
    entrypoint: argonvm::Chunk,
}

impl Converter {
    fn load(&mut self, wasmbytes: &[u8]) -> Result<(), Box<dyn Error>> {
        for payload in Parser::new(0).parse_all(wasmbytes) {
            let payload = payload?;
            println!("Payload: {payload:?}");
            match payload {
                Payload::Version { .. } => {
                    println!("====== Module");
                }
                Payload::TypeSection(types) => {
                    for t in types {
                        self.wasm_types.push(t?);
                    }
                }
                Payload::ExportSection(s) => {
                    for export in s {
                        let export = export?;
                        if export.name == "_start" {
                            self.start_function_index = Some(export.index);
                        }
                        println!("  Export {} {:?}", export.name, export.kind);
                    }
                }
                Payload::ImportSection(s) => {
                    for import in s {
                        let import = import?;
                        println!("  Import {}::{}", import.module, import.name);
                        match import.ty {
                            TypeRef::Func(t) => {
                                let index = self.next_function_index;
                                self.next_function_index += 1;

                                let arvm_index = self.get_native_function(format!("{}::{}", import.module, import.name));

                                self.function_indexes.insert(index, arvm_index);
                                self.function_types.push(t);
                            }
                            _ => panic!("Unsupported import"),
                        }
                        // remappedNames.insert( format!("{}::{}", import.module, import.name))
                    }
                }
                Payload::FunctionSection(funcs) => {
                    for func_type in funcs {
                        let func_type = func_type?;
                        let t = match &self.wasm_types[func_type as usize] {
                            Type::Func(t) => t,
                        };

                        let index = self.next_function_index;
                        self.next_function_index += 1;
                        self.function_indexes.insert(index, self.functions.len());

                        self.decl_function_indexes.push(index);

    
                        self.functions.push(argonvm::Function {
                            func: Some(argonvm::function::Func::Bytecode(
                                argonvm::BytecodeFunction {
                                    parameterTypes: t.params().into_iter().map(|t| wasm_type_to_argonvm(*t)).collect(),
                                    returnType: MessageField::some(wasm_types_to_argonvm_tuple(t.results())),
                                    body: MessageField::none(),
                                    special_fields: Default::default(),
                                }
                            )),
                            special_fields: Default::default(),
                        });
                        self.function_types.push(func_type);
                    }
                }
                Payload::MemorySection(memories) => {
                    for memory in memories {
                        let memory = memory?;
                        let index = self.next_memory_index;
                        self.next_memory_index += 1;

                        let global_index = self.global_types.len();
                        self.global_types.push(wasm_type_to_argonvm(ValType::ExternRef));
                        
                        self.memory_global_indexes.insert(index, global_index);
                        
                        if memory.memory64 { panic!("Memory64 not supported"); }

                        let create_memory_func = self.get_native_function("wasm_memory_new".to_string());
                        
                        let bc = get_bytecode(&mut self.entrypoint.bytecode);
                        Instruction::Constant(add_constant(&mut self.entrypoint.constants, argonvm::constant_value::Value::Int32(memory.initial as i32))).write(bc)?;
                        Instruction::Call(create_memory_func).write(bc)?;
                        Instruction::StGlobal(global_index).write(bc)?;
                    }
                }
                Payload::DataSection(data_reader) => {
                    for data in data_reader {
                        let data = data?;
                        match data.kind {
                            DataKind::Passive => {
                                panic!("Not implemented");
                            }
                            DataKind::Active { memory_index, offset_expr } => {
                                let global_index = self.memory_global_indexes[&memory_index];

                                let store_buffer = self.get_native_function("wasm_memory_store_buffer".to_string());

                                let offset = const_expr_evaluator(offset_expr.get_operators_reader())?;
                                let data_vec = data.data.to_vec();

                                let bc = get_bytecode(&mut self.entrypoint.bytecode);
                                Instruction::LdGlobal(global_index).write(bc)?;
                                Instruction::Constant(add_constant(&mut self.entrypoint.constants, argonvm::constant_value::Value::Int32(offset))).write(bc)?;
                                Instruction::Constant(add_constant(&mut self.entrypoint.constants, argonvm::constant_value::Value::BytesLiteral(data_vec))).write(bc)?;
                                Instruction::Call(store_buffer).write(bc)?;
                                Instruction::Pop.write(bc)?;
                            }
                        }
                    }
                }
                Payload::CodeSectionEntry(code) => {
                    let index = self.decl_function_indexes[self.next_code_index as usize];
                    self.next_code_index += 1;
                    self.emit_function(index, code)?;
                }
                _other => {
                    // println!("found payload {:?}", _other);
                }
            }
        }

        let bc = get_bytecode(&mut self.entrypoint.bytecode);
        if let Some(start_index) = self.start_function_index {
            Instruction::ReturnCall(self.function_indexes[&start_index]).write(bc)?;
        }
        else {
            Instruction::Return.write(bc)?;
        }


        Ok(())
    }

    fn write<F: Write>(self, f: &mut F) -> Result<(), Box<dyn Error>> {
        let mut f = CodedOutputStream::new(f);

        let program = argonvm::Program {
            classes: Vec::new(),
            functions: self.functions,
            globalTypes: self.global_types,
            entrypoint: MessageField::some(self.entrypoint),
            special_fields: Default::default(),
        };

        program.write_to(&mut f)?;

        f.flush()?;


        Ok(())
    }

    fn get_native_function(&mut self, name: String) -> usize {
        *self.native_functions.entry(name).or_insert_with_key(|name| {
            let index = self.functions.len();
            self.functions.push(argonvm::Function {
                func: Some(argonvm::function::Func::Native(name.clone())),
                special_fields: Default::default(),
            });
            index
        })
    }

    fn emit_function(&mut self, index: u32, code: FunctionBody) -> Result<(), Box<dyn Error>> {
        let mut chunk = argonvm::Chunk::default();
        

        for local in code.get_locals_reader()? {
            let (count, var_type) = local?;
            let t = wasm_type_to_argonvm(var_type);
            for _ in 0..count {
                chunk.variableTypes.push(t.clone());
            }
        }


        let mut emitter = FunctionEmitter {
            converter: self,
            chunk: &mut chunk,
        };

        for operator in code.get_operators_reader()? {
            let operator = operator?;
            emitter.emit_operator(operator)?;
        }

        match &mut self.functions[self.function_indexes[&index]].func {
            Some(argonvm::function::Func::Bytecode(bytecode_func)) => {
                match &bytecode_func.returnType.get_or_default().type_ {
                    Some(argonvm::value_type::Type::Tuple(tuple)) => {
                        Instruction::ConstructTuple(tuple.elements.len()).write(get_bytecode(&mut chunk.bytecode))?;
                    }
                    _ => (),
                }

                Instruction::Return.write(get_bytecode(&mut chunk.bytecode))?;
                
                bytecode_func.body = MessageField::some(chunk);
            }
            _ => panic!("Tried to set bytecode of non-bytecode function"),
        }


        
        
        Ok(())
    }
}


struct FunctionEmitter<'a> {
    converter: &'a mut Converter,
    chunk: &'a mut argonvm::Chunk,
}

impl <'a> FunctionEmitter<'a> {
    fn emit_operator(&mut self, operator: Operator) -> Result<(), Box<dyn Error>> {
        println!("Operator: {operator:?}");
        match operator {
            Operator::End => {},
            Operator::Drop => {
                self.write_insn(Instruction::Pop)?;
            }

            Operator::I32Const { value } => {
                self.write_constant(argonvm::constant_value::Value::Int32(value))?;
            }

            Operator::I32Store { memarg } => {
                let store = self.converter.get_native_function("wasm_memory_store_i32".to_string());
                self.write_insn(Instruction::ConstantInt8(memarg.align as i8))?;
                self.write_constant(argonvm::constant_value::Value::Int32(memarg.offset as i32))?;
                self.write_insn(Instruction::LdGlobal(self.converter.memory_global_indexes[&memarg.memory]))?;
                self.write_insn(Instruction::Call(store))?;
                self.write_insn(Instruction::Pop)?;
            }

            Operator::Call { function_index } => {
                self.write_insn(Instruction::Call(self.converter.function_indexes[&function_index]))?;
            }

            _ => panic!("Unsupported operator: {operator:?}"),
        }

        Ok(())
    }

    fn write_insn(&mut self, insn: Instruction) -> Result<(), Box<dyn Error>> {
        let bc = get_bytecode(&mut self.chunk.bytecode);
        insn.write(bc)?;
        Ok(())
    }

    fn write_constant(&mut self, value: argonvm::constant_value::Value) -> Result<(), Box<dyn Error>> {
        let index = add_constant(&mut self.chunk.constants, value);
        self.write_insn(Instruction::Constant(index))?;
        Ok(())
    }
}

fn wasm_type_to_argonvm(t: ValType) -> argonvm::ValueType {
    argonvm::ValueType {
        type_: Some(argonvm::value_type::Type::Simple(EnumOrUnknown::new(
            match t {
                ValType::I32 => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_INT32,
                ValType::I64 => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_INT64,
                ValType::F32 => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_FLOAT32,
                ValType::F64 => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_FLOAT64,
                ValType::V128 => panic!("Unsupported value type"),
                ValType::FuncRef => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_OBJECT_REFERENCE,
                ValType::ExternRef => argonvm::ValueTypeSimple::VALUE_TYPE_SIMPLE_OBJECT_REFERENCE,
            }
        ))),
        special_fields: Default::default(),
    }
}

fn wasm_types_to_argonvm_tuple(types: &[ValType]) -> argonvm::ValueType {
    argonvm::ValueType {
        type_: Some(argonvm::value_type::Type::Tuple(
            argonvm::ValueTypeTuple {
                elements: types.into_iter().map(|t| wasm_type_to_argonvm(*t)).collect(),
                special_fields: Default::default(),
            }
        )),
        special_fields: Default::default(),
    }
}


fn get_bytecode(bytecode: &mut Option<Vec<u8>>) -> &mut Vec<u8> {
    if let Some(bytecode) = bytecode {
        bytecode
    }
    else {
        *bytecode = Some(Vec::new());
        get_bytecode(bytecode)
    }
}


fn add_constant(constants: &mut Vec<argonvm::ConstantValue>, value: argonvm::constant_value::Value) -> usize {
    let index = constants.len();
    constants.push(argonvm::ConstantValue {
        value: Some(value),
        special_fields: Default::default(),
    });
    index
}

fn const_expr_evaluator(reader: OperatorsReader) -> Result<i32, Box<dyn Error>> {
    let mut stack = Vec::new();
    for op in reader {
        let op = op?;
        match op {
            Operator::End => {}
            Operator::I32Const { value } => {
                stack.push(value);
            }
            _ => {
                panic!("Unsupported operator");
            }
        }
    }
    Ok(stack.pop().unwrap())
}


