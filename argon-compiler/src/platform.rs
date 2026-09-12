use alloc::string::String;
use esexpr::ESExpr;
use hashbrown::HashMap;

pub struct PlatformMetadata {
    pub metadata: ESExpr<'static>,
    pub externs: HashMap<String, Extern>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExternType {
    Function,
    Method,
    StaticMethod,
}

impl ExternType {
    pub fn name(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Method => "method",
            Self::StaticMethod => "static method",
        }
    }
}

pub struct Extern {
    pub extern_type: ExternType,
    pub implementation: ESExpr<'static>,
}

pub struct PlatformExtern {
    // Externs by platform.
    pub externs: HashMap<String, ESExpr<'static>>,
}
