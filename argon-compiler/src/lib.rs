pub mod scope;
pub mod type_checker;

use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use argon_expr::ExprContext;
use argon_util::{CompileError, ErrorReporter};

pub trait Context {
    type Reporter: ErrorReporter<CompileError> + ErrorReporter<std::io::Error>;

    fn reporter(&self) -> &Self::Reporter;
}

struct DefaultExprContext;

impl ExprContext for DefaultExprContext {
    type Hole = EmptyHole;
    type Function = Rc<dyn Function>;
    type Method = Rc<dyn Method>;
    type Record = Rc<dyn Record>;
    type Enum = Rc<dyn Enum>;
    type EnumCase = Rc<dyn EnumCase>;
    type Trait = Rc<dyn Trait>;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum EmptyHole {}

trait Function {}

trait Method {}

trait Record {}

trait Enum {}

trait EnumCase {}

trait Trait {}

macro_rules! impl_dyn_stub_traits {
    ($trait_name:ident) => {
        impl Debug for dyn $trait_name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($trait_name)).finish()
            }
        }

        impl PartialEq for dyn $trait_name {
            fn eq(&self, other: &Self) -> bool {
                std::ptr::addr_eq(self, other)
            }
        }

        impl Eq for dyn $trait_name {}

        impl Hash for dyn $trait_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                std::ptr::from_ref(self).hash(state);
            }
        }
    };
}

impl_dyn_stub_traits!(Function);
impl_dyn_stub_traits!(Method);
impl_dyn_stub_traits!(Record);
impl_dyn_stub_traits!(Enum);
impl_dyn_stub_traits!(EnumCase);
impl_dyn_stub_traits!(Trait);
