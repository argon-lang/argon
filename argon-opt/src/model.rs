use std::collections::HashMap;

use argon_format_vm::vm as vf;
use num_bigint::BigUint;

pub struct TubeModel {
    pub header: vf::TubeHeader,
    pub metadata: vf::TubeMetadata,
    pub modules: Vec<ModuleModel>,

    pub module_info: HashMap<BigUint, ModuleInfo>,
    pub function_info: HashMap<BigUint, FunctionInfo>,
    pub record_info: HashMap<BigUint, RecordInfo>,
    pub record_field_info: HashMap<BigUint, RecordFieldInfo>,
    pub enum_info: HashMap<BigUint, EnumInfo>,
    pub enum_variant_info: HashMap<BigUint, EnumVariantInfo>,
    pub trait_info: HashMap<BigUint, TraitInfo>,
    pub method_info: HashMap<BigUint, MethodInfo>,
    pub instance_info: HashMap<BigUint, InstanceInfo>,
}

pub struct ModuleModel {
    pub path: vf::ModulePath,
    pub exports: Vec<ModuleExportEntry>,
}

pub enum ModuleExportEntry {
    FunctionDefinition(vf::FunctionDefinition),
    RecordDefinition(vf::RecordDefinition),
    EnumDefinition(vf::EnumDefinition),
    TraitDefinition(vf::TraitDefinition),
    InstanceDefinition(vf::InstanceDefinition),
}

pub struct ModuleInfo {
    pub tube_id: BigUint,
    pub path: vf::ModulePath,
}

pub struct FunctionInfo {
    pub import_specifier: vf::ImportSpecifier,
}

pub struct RecordInfo {
    pub import_specifier: vf::ImportSpecifier,
}

pub enum RecordFieldOwner {
    Record,
    EnumVariant,
}

pub struct RecordFieldInfo {
    pub owner: RecordFieldOwner,
    pub record_id: BigUint,
    pub name: vf::Identifier,
}

pub struct EnumInfo {
    pub import_specifier: vf::ImportSpecifier,
}

pub struct EnumVariantInfo {
    pub enum_id: BigUint,
    pub name: vf::Identifier,
}

pub struct TraitInfo {
    pub import_specifier: vf::ImportSpecifier,
}

pub struct MethodInfo {
    pub parent_import_specifier: vf::ImportSpecifier,
    pub name: vf::Identifier,
    pub signature: vf::ErasedSignature,
    pub definition: Option<vf::MethodDefinition>,
}

pub struct InstanceInfo {
    pub import_specifier: vf::ImportSpecifier,
}

pub struct ProgramIR<'a> {
    pub tubes: HashMap<vf::TubeName, &'a TubeModel>,
    pub current_tube: &'a TubeModel,
}
