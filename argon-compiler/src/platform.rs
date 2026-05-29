use alloc::string::String;
use esexpr::ESExpr;
use hashbrown::HashMap;

pub struct PlatformMetadata {
    pub metadata: ESExpr<'static>,
    pub externs: HashMap<String, Extern>,
}

pub enum ExternType {
    Function,
}

pub struct Extern {
    pub extern_type: ExternType,
    pub implementation: ESExpr<'static>,
}

pub struct PlatformExtern {
    // Externs by platform.
    pub externs: HashMap<String, ESExpr<'static>>,
}
