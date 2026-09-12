//! Safe, explicit-context bindings for the subset of Z3 used by Argon.
//!
//! The public API deliberately contains no raw Z3 handles and no implicit or
//! thread-local context. Every Z3 value is tied to the lifetime of its context.

#![no_std]
#![allow(
    clippy::missing_panics_doc,
    reason = "programmer contract violations panic"
)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

use crate::ast::Ast as _;
use crate::backend::sys;
use alloc::ffi::CString;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use core::ffi::CStr;
use core::fmt;
use core::marker::PhantomData;
use sys::{Z3_context, Z3_func_decl, Z3_params, Z3_pattern, Z3_solver, Z3_sort};

pub mod ast;
mod backend;
mod datatype;

pub use datatype::{DatatypeAccessor, DatatypeBuilder, DatatypeSort, DatatypeVariant};
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SortKind {
    Bool,
    Int,
    Bv,
    Seq,
    Uninterpreted,
    Other,
}

fn sort_kind(kind: sys::SortKind) -> SortKind {
    match kind {
        sys::SortKind::Bool => SortKind::Bool,
        sys::SortKind::Int => SortKind::Int,
        sys::SortKind::Bv => SortKind::Bv,
        sys::SortKind::Seq => SortKind::Seq,
        sys::SortKind::Uninterpreted => SortKind::Uninterpreted,
        _ => SortKind::Other,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Int(u32),
    String(String),
}

impl From<u32> for Symbol {
    fn from(value: u32) -> Self {
        Self::Int(value)
    }
}
impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}
impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

fn symbol(ctx: &Context, value: impl Into<Symbol>) -> sys::Z3_symbol {
    match value.into() {
        Symbol::Int(value) => unsafe {
            sys::Z3_mk_int_symbol(
                ctx.raw,
                value.try_into().expect("integer symbol is too large"),
            )
            .expect("Z3 failed to create symbol")
        },
        Symbol::String(value) => {
            let value = CString::new(value).expect("Z3 symbol must not contain a NUL byte");
            unsafe {
                sys::Z3_mk_string_symbol(ctx.raw, value.as_ptr())
                    .expect("Z3 failed to create symbol")
            }
        }
    }
}

/// An independent Z3 logical context.
pub struct Context {
    pub(crate) raw: Z3_context,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl Context {
    #[must_use]
    pub fn new() -> Self {
        unsafe {
            let config = sys::Z3_mk_config().expect("Z3 failed to create a configuration");
            let raw = sys::Z3_mk_context_rc(config).expect("Z3 failed to create a context");
            sys::Z3_del_config(config);
            sys::Z3_set_error_handler(raw, None);
            Self {
                raw,
                _not_send_sync: PhantomData,
            }
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context").finish_non_exhaustive()
    }
}
impl Drop for Context {
    fn drop(&mut self) {
        unsafe { sys::Z3_del_context(self.raw) }
    }
}

fn same_context(left: &Context, right: &Context) {
    assert_eq!(
        left.raw, right.raw,
        "cannot combine values from different Z3 contexts"
    );
}

pub struct Sort<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) raw: Z3_sort,
}

impl<'ctx> Sort<'ctx> {
    pub(crate) unsafe fn wrap(ctx: &'ctx Context, raw: Z3_sort) -> Self {
        let ast =
            unsafe { sys::Z3_sort_to_ast(ctx.raw, raw).expect("Z3 returned a null sort AST") };
        unsafe { sys::Z3_inc_ref(ctx.raw, ast) };
        Self { ctx, raw }
    }
    #[must_use]
    pub fn uninterpreted(ctx: &'ctx Context, name: Symbol) -> Self {
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_uninterpreted_sort(ctx.raw, symbol(ctx, name))
                    .expect("Z3 failed to create sort"),
            )
        }
    }
    #[must_use]
    pub fn bool(ctx: &'ctx Context) -> Self {
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_bool_sort(ctx.raw).expect("Z3 failed to create bool sort"),
            )
        }
    }
    #[must_use]
    pub fn int(ctx: &'ctx Context) -> Self {
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_int_sort(ctx.raw).expect("Z3 failed to create int sort"),
            )
        }
    }
    #[must_use]
    pub fn string(ctx: &'ctx Context) -> Self {
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_string_sort(ctx.raw).expect("Z3 failed to create string sort"),
            )
        }
    }
    #[must_use]
    pub fn bitvector(ctx: &'ctx Context, bits: u32) -> Self {
        assert!(bits > 0);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_bv_sort(ctx.raw, bits).expect("Z3 failed to create bit-vector sort"),
            )
        }
    }
    #[must_use]
    pub fn seq(element: &Self) -> Self {
        unsafe {
            Self::wrap(
                element.ctx,
                sys::Z3_mk_seq_sort(element.ctx.raw, element.raw)
                    .expect("Z3 failed to create sequence sort"),
            )
        }
    }
    #[must_use]
    pub fn kind(&self) -> SortKind {
        sort_kind(unsafe { sys::Z3_get_sort_kind(self.ctx.raw, self.raw) })
    }
    #[must_use]
    pub fn context(&self) -> &'ctx Context {
        self.ctx
    }
}
impl Clone for Sort<'_> {
    fn clone(&self) -> Self {
        unsafe { Self::wrap(self.ctx, self.raw) }
    }
}
impl Drop for Sort<'_> {
    fn drop(&mut self) {
        unsafe {
            sys::Z3_dec_ref(
                self.ctx.raw,
                sys::Z3_sort_to_ast(self.ctx.raw, self.raw).expect("valid sort"),
            );
        }
    }
}
impl PartialEq for Sort<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ctx.raw == other.ctx.raw
            && unsafe { sys::Z3_is_eq_sort(self.ctx.raw, self.raw, other.raw) }
    }
}
impl Eq for Sort<'_> {}
impl fmt::Debug for Sort<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_ptr(unsafe { sys::Z3_sort_to_string(self.ctx.raw, self.raw) }, f)
    }
}

pub struct FuncDecl<'ctx> {
    ctx: &'ctx Context,
    raw: Z3_func_decl,
}
impl<'ctx> FuncDecl<'ctx> {
    pub(crate) unsafe fn wrap(ctx: &'ctx Context, raw: Z3_func_decl) -> Self {
        unsafe {
            sys::Z3_inc_ref(
                ctx.raw,
                sys::Z3_func_decl_to_ast(ctx.raw, raw).expect("valid function declaration"),
            );
        };
        Self { ctx, raw }
    }
    #[must_use]
    pub fn new(name: impl Into<Symbol>, domain: &[&Sort<'ctx>], range: &Sort<'ctx>) -> Self {
        for sort in domain {
            same_context(sort.ctx, range.ctx);
        }
        let domain: Vec<_> = domain.iter().map(|sort| sort.raw).collect();
        unsafe {
            Self::wrap(
                range.ctx,
                sys::Z3_mk_func_decl(
                    range.ctx.raw,
                    symbol(range.ctx, name),
                    u32::try_from(domain.len()).expect("too many arguments"),
                    domain.as_ptr(),
                    range.raw,
                )
                .expect("Z3 failed to create function"),
            )
        }
    }
    #[must_use]
    pub fn apply(&self, args: &[&dyn ast::Ast<'ctx>]) -> ast::Dynamic<'ctx> {
        for arg in args {
            same_context(self.ctx, arg.context());
        }
        let args: Vec<_> = args.iter().map(|arg| arg.raw()).collect();
        unsafe {
            ast::Dynamic::wrap(
                self.ctx,
                sys::Z3_mk_app(
                    self.ctx.raw,
                    self.raw,
                    u32::try_from(args.len()).expect("too many arguments"),
                    args.as_ptr(),
                )
                .expect("Z3 function application failed"),
            )
        }
    }
    #[must_use]
    pub fn domain(&self, index: usize) -> Option<SortKind> {
        if index >= unsafe { sys::Z3_get_domain_size(self.ctx.raw, self.raw) } as usize {
            return None;
        }
        let index = u32::try_from(index).ok()?;
        let raw = unsafe { sys::Z3_get_domain(self.ctx.raw, self.raw, index)? };
        Some(sort_kind(unsafe {
            sys::Z3_get_sort_kind(self.ctx.raw, raw)
        }))
    }
    #[must_use]
    pub fn context(&self) -> &'ctx Context {
        self.ctx
    }
}
impl Drop for FuncDecl<'_> {
    fn drop(&mut self) {
        unsafe {
            sys::Z3_dec_ref(
                self.ctx.raw,
                sys::Z3_func_decl_to_ast(self.ctx.raw, self.raw).expect("valid declaration"),
            );
        }
    }
}
impl fmt::Debug for FuncDecl<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_ptr(
            unsafe { sys::Z3_func_decl_to_string(self.ctx.raw, self.raw) },
            f,
        )
    }
}

pub struct Params<'ctx> {
    ctx: &'ctx Context,
    raw: Z3_params,
}
impl<'ctx> Params<'ctx> {
    #[must_use]
    pub fn new(ctx: &'ctx Context) -> Self {
        unsafe {
            let raw = sys::Z3_mk_params(ctx.raw).expect("Z3 failed to create parameters");
            sys::Z3_params_inc_ref(ctx.raw, raw);
            Self { ctx, raw }
        }
    }
    pub fn set_u32(&mut self, key: impl Into<Symbol>, value: u32) {
        unsafe { sys::Z3_params_set_uint(self.ctx.raw, self.raw, symbol(self.ctx, key), value) }
    }
}
impl Drop for Params<'_> {
    fn drop(&mut self) {
        unsafe { sys::Z3_params_dec_ref(self.ctx.raw, self.raw) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SatResult {
    Unsat,
    Unknown,
    Sat,
}
pub struct Solver<'ctx> {
    ctx: &'ctx Context,
    raw: Z3_solver,
}
impl<'ctx> Solver<'ctx> {
    #[must_use]
    pub fn new(ctx: &'ctx Context) -> Self {
        unsafe {
            let raw = sys::Z3_mk_solver(ctx.raw).expect("Z3 failed to create solver");
            sys::Z3_solver_inc_ref(ctx.raw, raw);
            Self { ctx, raw }
        }
    }
    pub fn assert(&self, value: impl core::borrow::Borrow<ast::Bool<'ctx>>) {
        let value = value.borrow();
        same_context(self.ctx, value.context());
        unsafe { sys::Z3_solver_assert(self.ctx.raw, self.raw, value.raw()) }
    }
    pub fn set_params(&self, params: &Params<'ctx>) {
        same_context(self.ctx, params.ctx);
        unsafe { sys::Z3_solver_set_params(self.ctx.raw, self.raw, params.raw) }
    }
    #[must_use]
    pub fn check(&self) -> SatResult {
        match unsafe { sys::Z3_solver_check(self.ctx.raw, self.raw) } {
            sys::Z3_L_FALSE => SatResult::Unsat,
            sys::Z3_L_TRUE => SatResult::Sat,
            _ => SatResult::Unknown,
        }
    }
}
impl Drop for Solver<'_> {
    fn drop(&mut self) {
        unsafe { sys::Z3_solver_dec_ref(self.ctx.raw, self.raw) }
    }
}

pub struct Pattern<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) raw: Z3_pattern,
}
impl<'ctx> Pattern<'ctx> {
    #[must_use]
    pub fn new(terms: &[&dyn ast::Ast<'ctx>]) -> Self {
        let first = terms.first().expect("Z3 pattern must not be empty");
        let ctx = first.context();
        for term in terms {
            same_context(ctx, term.context());
        }
        let terms: Vec<_> = terms.iter().map(|term| term.raw()).collect();
        unsafe {
            let raw = sys::Z3_mk_pattern(
                ctx.raw,
                terms.len().try_into().expect("too many terms"),
                terms.as_ptr(),
            )
            .expect("Z3 failed to create pattern");
            sys::Z3_inc_ref(
                ctx.raw,
                sys::Z3_pattern_to_ast(ctx.raw, raw).expect("valid pattern"),
            );
            Self { ctx, raw }
        }
    }
}
impl Drop for Pattern<'_> {
    fn drop(&mut self) {
        unsafe {
            sys::Z3_dec_ref(
                self.ctx.raw,
                sys::Z3_pattern_to_ast(self.ctx.raw, self.raw).expect("valid pattern"),
            );
        }
    }
}

fn display_ptr(ptr: *const core::ffi::c_char, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if ptr.is_null() {
        return Err(fmt::Error);
    }
    f.write_str(
        unsafe { CStr::from_ptr(ptr) }
            .to_str()
            .map_err(|_| fmt::Error)?,
    )
}

#[cfg(test)]
mod tests {
    extern crate std;

    use super::*;
    use crate::ast::{Ast, Bool, Dynamic, Int};
    use std::vec;

    #[test]
    fn solves_basic_constraints() {
        let context = Context::new();
        let solver = Solver::new(&context);
        let value = Int::from_i64(&context, 1);
        solver.assert(value.equals(&Int::from_i64(&context, 2)));
        assert_eq!(solver.check(), SatResult::Unsat);
    }

    #[test]
    fn creates_and_uses_datatypes() {
        let context = Context::new();
        let datatype = DatatypeBuilder::new(&context, "OptionalInt")
            .variant("none", vec![])
            .variant(
                "some",
                vec![("value", DatatypeAccessor::sort(Sort::int(&context)))],
            )
            .finish();
        let some = datatype.variants[1]
            .constructor
            .apply(&[&Int::from_i64(&context, 7)]);
        let is_some = datatype.variants[1]
            .tester
            .apply(&[&some])
            .as_bool()
            .expect("tester is boolean");
        let solver = Solver::new(&context);
        solver.assert(is_some.not());
        assert_eq!(solver.check(), SatResult::Unsat);
    }

    #[test]
    #[should_panic(expected = "different Z3 contexts")]
    fn rejects_mixed_contexts() {
        let first = Context::new();
        let second = Context::new();
        let _ = Int::from_i64(&first, 1).equals(&Int::from_i64(&second, 1));
    }

    #[test]
    fn contexts_are_independent_across_threads() {
        let handles: Vec<_> = (0..4)
            .map(|_| {
                std::thread::spawn(|| {
                    let context = Context::new();
                    let solver = Solver::new(&context);
                    solver.assert(Bool::from_bool(&context, true));
                    assert_eq!(solver.check(), SatResult::Sat);
                    Dynamic::new_const("local", &Sort::bool(&context))
                        .get_sort()
                        .kind()
                })
            })
            .collect();
        for handle in handles {
            assert_eq!(handle.join().expect("thread should finish"), SortKind::Bool);
        }
    }
}
