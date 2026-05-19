use crate::flags::{Flags, FlagsUnpacked, Mode, TypeIndexKind};

pub struct FunctionBody {
    instructions: Vec<InstructionEntry>,
}

impl FunctionBody {
    pub fn instructions(&self) -> &[InstructionEntry] {
        &self.instructions
    }
}

#[derive(Debug, Clone)]
pub struct InstructionReference {
    index: usize,
}

pub struct InstructionEntry {
    instruction: Instruction,
    flags: Flags,
    result_type: usize,
}

impl InstructionEntry {
    pub fn instruction(&self) -> &Instruction {
        &self.instruction
    }

    pub fn mode(&self) -> Mode {
        self.flags.get_mode()
    }

    pub fn result_type(&self) -> usize {
        self.result_type
    }
}

pub struct InstructionEntryUnpacked {
    pub instruction: Instruction,
    pub mode: Mode,
    pub result_type: InstructionType,
}

#[derive(Debug, Clone)]
pub enum InstructionType {
    Local(InstructionReference),
    TypeN(usize),
    BigTypeN(usize),
    Builtin(BuiltinType),
}

#[derive(Debug, Clone)]
#[repr(usize)]
pub enum BuiltinType {
    Error = 0,
    EmptyTuple = 1,
    Nothing = 2,
    Bool = 3,
    Int = 4,
    String = 5,
}

pub enum Instruction {
    LoadLocal(usize),
    StoreLocal(usize),
    LoadTypeN(usize),
    LoadBigTypeN(usize),
    LoadBuiltinType(BuiltinType),
    Return(usize),
}

pub struct FunctionBuilder {
    body: FunctionBody,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        FunctionBuilder {
            body: FunctionBody {
                instructions: Vec::new(),
            },
        }
    }

    pub fn add_instruction(&mut self, entry: InstructionEntryUnpacked) -> InstructionReference {
        let type_index_kind;
        let result_type;

        match entry.result_type {
            InstructionType::Local(index) => {
                type_index_kind = TypeIndexKind::Local;
                result_type = index.index;
            }
            InstructionType::TypeN(index) => {
                type_index_kind = TypeIndexKind::Type;
                result_type = index;
            }
            InstructionType::BigTypeN(index) => {
                type_index_kind = TypeIndexKind::BigType;
                result_type = index;
            }
            InstructionType::Builtin(builtin) => {
                type_index_kind = TypeIndexKind::Builtin;
                result_type = builtin as usize;
            }
        }

        let flags = Flags::pack(FlagsUnpacked {
            mode: entry.mode,
            type_index_kind,
        });

        let entry = InstructionEntry {
            instruction: entry.instruction,
            flags,
            result_type,
        };

        self.body.instructions.push(entry);

        InstructionReference {
            index: self.body.instructions.len(),
        }
    }

    pub fn build(self) -> FunctionBody {
        self.body
    }
}
