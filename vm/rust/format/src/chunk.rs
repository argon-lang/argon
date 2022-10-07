use crate::value::{Value, ValueType};
use crate::instructions::Instruction;

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    variable_types: Vec<ValueType>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            variable_types: Vec::new(),
        }
    }

    pub fn size(&self) -> usize {
        self.code.len()
    }

    pub fn write_instruction(&mut self, insn: &Instruction) -> std::io::Result<()> {
        insn.write(&mut self.code)
    }

    pub fn read_index(&self, offset: &mut usize) -> usize {
        let mut result: usize = 0;

        loop {
            let b = self.code[*offset];
            *offset += 1;

            result = (result << 7) | (b & 0x7F) as usize;
            if (b & 0x80) == 0 {
                break;
            }
        }

        result
    }

    pub fn get_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    pub fn get_variables(&self) -> &Vec<ValueType> {
        &self.variable_types
    }

}

