use crate::chunk::Chunk;
use crate::value::{Value, ValueType};


pub struct Function {
    pub parameters: Vec<ValueType>,
    pub return_type: ValueType,
    pub implementation: FunctionImplementation,
}

pub enum NativeTrampoline {
    Result(Value),
    DelayFunction(usize, Vec<Value>, Box<dyn FnOnce(Value) -> NativeTrampoline>),
    Delay(Box<dyn FnOnce() -> NativeTrampoline>),
}

pub enum FunctionImplementation {
    FromChunk(Chunk),
    Native(Box<dyn Fn(Vec<Value>) -> NativeTrampoline>)
}

