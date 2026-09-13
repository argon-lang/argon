use crate::names::{mangled, module_package};
use argon_format_vm::vm as vf;
use argon_vm::model::TubeModel;

pub struct Program<'a> {
    pub model: &'a TubeModel,
    pub root: Vec<String>,
}
impl<'a> Program<'a> {
    pub fn new(model: &'a TubeModel, root: Vec<String>) -> Result<Self, String> {
        let p = Self { model, root };
        for (id, x) in &model.function_info {
            p.callable_name(&x.import_specifier)
                .map_err(|e| format!("function {id}: {e}"))?;
        }
        for (id, x) in &model.record_info {
            p.type_name(&x.import_specifier)
                .map_err(|e| format!("record {id}: {e}"))?;
        }
        for (id, x) in &model.enum_info {
            p.type_name(&x.import_specifier)
                .map_err(|e| format!("enum {id}: {e}"))?;
        }
        for (id, x) in &model.trait_info {
            p.type_name(&x.import_specifier)
                .map_err(|e| format!("trait {id}: {e}"))?;
        }
        for (id, x) in &model.instance_info {
            p.type_name(&x.import_specifier)
                .map_err(|e| format!("instance {id}: {e}"))?;
        }
        Ok(p)
    }
    pub(crate) fn package_for_module(&self, id: &num_bigint::BigUint) -> Result<String, String> {
        let info = self
            .model
            .module_info
            .get(id)
            .ok_or_else(|| format!("unknown module id {id}"))?;
        let tube = self
            .model
            .tube_name_for_id(&info.tube_id)
            .ok_or_else(|| format!("unknown tube id {}", info.tube_id))?;
        let root = if info.tube_id == 0u32.into() {
            self.root.clone()
        } else {
            let parts = core::iter::once(tube.head.as_str())
                .chain(tube.tail.iter().map(String::as_str))
                .collect::<Vec<_>>();
            let mut x = vec!["Argon".into(), "Tube".into(), format!("T{}", parts.len())];
            x.extend(parts.into_iter().map(crate::encode_component));
            x
        };
        Ok(module_package(
            &root,
            &info
                .path
                .path
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>(),
        ))
    }
    pub fn import_symbol(&self, x: &vf::ImportSpecifier) -> Result<String, String> {
        match x {
            vf::ImportSpecifier::Global {
                module_id,
                name,
                sig,
            } => Ok(format!(
                "{}::{}",
                self.package_for_module(module_id)?,
                mangled(name, sig, &|i| self.import_symbol(i))?
            )),
            vf::ImportSpecifier::Local { parent, index } => {
                Ok(format!("{}__k{index}", self.import_symbol(parent)?))
            }
        }
    }
    pub fn callable_name(&self, x: &vf::ImportSpecifier) -> Result<String, String> {
        self.import_symbol(x)
    }
    pub fn type_name(&self, x: &vf::ImportSpecifier) -> Result<String, String> {
        let s = self.import_symbol(x)?;
        let (p, n) = s.rsplit_once("::").ok_or("malformed import symbol")?;
        Ok(format!("{p}::Type::{n}"))
    }
}
