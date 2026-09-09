use crate::backend::sys;
use crate::{Context, Sort, SortKind, display_ptr, same_context, symbol};
use alloc::ffi::{CString, NulError};
use alloc::string::ToString;
use alloc::vec::Vec;
use core::fmt;
use sys::Z3_ast;

#[derive(Copy, Clone)]
#[doc(hidden)]
pub struct AstHandle(Z3_ast);

/// Common behavior for Z3 abstract syntax trees.
pub trait Ast<'ctx>: fmt::Debug {
    #[doc(hidden)]
    fn context(&self) -> &'ctx Context;
    #[doc(hidden)]
    fn handle(&self) -> AstHandle;

    #[must_use]
    fn get_sort(&self) -> Sort<'ctx> {
        unsafe {
            Sort::wrap(
                self.context(),
                sys::Z3_get_sort(self.context().raw, self.raw()).expect("Z3 AST has no sort"),
            )
        }
    }

    #[must_use]
    fn equals(&self, other: &dyn Ast<'ctx>) -> Bool<'ctx> {
        same_context(self.context(), other.context());
        unsafe {
            Bool::wrap(
                self.context(),
                sys::Z3_mk_eq(self.context().raw, self.raw(), other.raw())
                    .expect("Z3 equality failed"),
            )
        }
    }

    #[doc(hidden)]
    fn raw(&self) -> Z3_ast {
        self.handle().0
    }
}

macro_rules! ast_type {
    ($name:ident) => {
        pub struct $name<'ctx> {
            pub(crate) ctx: &'ctx Context,
            pub(crate) raw: Z3_ast,
        }
        impl<'ctx> $name<'ctx> {
            pub(crate) unsafe fn wrap(ctx: &'ctx Context, raw: Z3_ast) -> Self {
                unsafe { sys::Z3_inc_ref(ctx.raw, raw) };
                Self { ctx, raw }
            }
        }
        impl<'ctx> Ast<'ctx> for $name<'ctx> {
            fn context(&self) -> &'ctx Context {
                self.ctx
            }
            fn handle(&self) -> AstHandle {
                AstHandle(self.raw)
            }
        }
        impl Clone for $name<'_> {
            fn clone(&self) -> Self {
                unsafe { Self::wrap(self.ctx, self.raw) }
            }
        }
        impl Drop for $name<'_> {
            fn drop(&mut self) {
                unsafe { sys::Z3_dec_ref(self.ctx.raw, self.raw) }
            }
        }
        impl PartialEq for $name<'_> {
            fn eq(&self, other: &Self) -> bool {
                self.ctx.raw == other.ctx.raw
                    && unsafe { sys::Z3_is_eq_ast(self.ctx.raw, self.raw, other.raw) }
            }
        }
        impl Eq for $name<'_> {}
        impl fmt::Debug for $name<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                display_ptr(unsafe { sys::Z3_ast_to_string(self.ctx.raw, self.raw) }, f)
            }
        }
    };
}

ast_type!(Dynamic);
ast_type!(Bool);
ast_type!(Int);
ast_type!(BV);
ast_type!(Seq);
ast_type!(String);

impl<'ctx> From<Bool<'ctx>> for Dynamic<'ctx> {
    fn from(value: Bool<'ctx>) -> Self {
        unsafe { Self::wrap(value.ctx, value.raw) }
    }
}
impl<'ctx> From<Int<'ctx>> for Dynamic<'ctx> {
    fn from(value: Int<'ctx>) -> Self {
        unsafe { Self::wrap(value.ctx, value.raw) }
    }
}
impl<'ctx> From<BV<'ctx>> for Dynamic<'ctx> {
    fn from(value: BV<'ctx>) -> Self {
        unsafe { Self::wrap(value.ctx, value.raw) }
    }
}
impl<'ctx> From<Seq<'ctx>> for Dynamic<'ctx> {
    fn from(value: Seq<'ctx>) -> Self {
        unsafe { Self::wrap(value.ctx, value.raw) }
    }
}
impl<'ctx> From<String<'ctx>> for Dynamic<'ctx> {
    fn from(value: String<'ctx>) -> Self {
        unsafe { Self::wrap(value.ctx, value.raw) }
    }
}

impl<'ctx> Dynamic<'ctx> {
    #[must_use]
    pub fn new_const(name: impl Into<crate::Symbol>, sort: &Sort<'ctx>) -> Self {
        unsafe {
            Self::wrap(
                sort.ctx,
                sys::Z3_mk_const(sort.ctx.raw, symbol(sort.ctx, name), sort.raw)
                    .expect("Z3 failed to create constant"),
            )
        }
    }
    #[must_use]
    pub fn fresh_const(prefix: &str, sort: &Sort<'ctx>) -> Self {
        let prefix = CString::new(prefix).expect("prefix contains NUL");
        unsafe {
            Self::wrap(
                sort.ctx,
                sys::Z3_mk_fresh_const(sort.ctx.raw, prefix.as_ptr(), sort.raw)
                    .expect("Z3 failed to create fresh constant"),
            )
        }
    }
    #[must_use]
    pub fn sort_kind(&self) -> SortKind {
        self.get_sort().kind()
    }
    #[must_use]
    pub fn as_bool(&self) -> Option<Bool<'ctx>> {
        (self.sort_kind() == SortKind::Bool).then(|| unsafe { Bool::wrap(self.ctx, self.raw) })
    }
    #[must_use]
    pub fn as_int(&self) -> Option<Int<'ctx>> {
        (self.sort_kind() == SortKind::Int).then(|| unsafe { Int::wrap(self.ctx, self.raw) })
    }
    #[must_use]
    pub fn as_bv(&self) -> Option<BV<'ctx>> {
        (self.sort_kind() == SortKind::Bv).then(|| unsafe { BV::wrap(self.ctx, self.raw) })
    }
    #[must_use]
    pub fn as_seq(&self) -> Option<Seq<'ctx>> {
        (self.sort_kind() == SortKind::Seq).then(|| unsafe { Seq::wrap(self.ctx, self.raw) })
    }
    #[must_use]
    pub fn as_string(&self) -> Option<String<'ctx>> {
        let sort = unsafe { sys::Z3_get_sort(self.ctx.raw, self.raw)? };
        unsafe {
            sys::Z3_is_string_sort(self.ctx.raw, sort).then(|| String::wrap(self.ctx, self.raw))
        }
    }
}

fn raw_values<'ctx, T: Ast<'ctx>>(values: &[&T]) -> (&'ctx Context, Vec<Z3_ast>) {
    let first = values
        .first()
        .expect("Z3 variadic operation requires at least one operand");
    let ctx = first.context();
    for value in values {
        same_context(ctx, value.context());
    }
    (ctx, values.iter().map(|value| value.raw()).collect())
}

impl<'ctx> Bool<'ctx> {
    #[must_use]
    pub fn from_bool(ctx: &'ctx Context, value: bool) -> Self {
        unsafe {
            Self::wrap(
                ctx,
                if value {
                    sys::Z3_mk_true(ctx.raw)
                } else {
                    sys::Z3_mk_false(ctx.raw)
                }
                .expect("Z3 failed to create boolean"),
            )
        }
    }
    #[must_use]
    pub fn and(values: &[&Self]) -> Self {
        let (ctx, values) = raw_values(values);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_and(
                    ctx.raw,
                    values.len().try_into().expect("too many operands"),
                    values.as_ptr(),
                )
                .expect("Z3 and failed"),
            )
        }
    }
    #[must_use]
    pub fn or(values: &[&Self]) -> Self {
        let (ctx, values) = raw_values(values);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_or(
                    ctx.raw,
                    values.len().try_into().expect("too many operands"),
                    values.as_ptr(),
                )
                .expect("Z3 or failed"),
            )
        }
    }
    #[must_use]
    pub fn not(&self) -> Self {
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_not(self.ctx.raw, self.raw).expect("Z3 not failed"),
            )
        }
    }
    #[must_use]
    pub fn implies(&self, other: &Self) -> Self {
        same_context(self.ctx, other.ctx);
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_implies(self.ctx.raw, self.raw, other.raw)
                    .expect("Z3 implication failed"),
            )
        }
    }
    #[must_use]
    pub fn pb_le(values: &[(&Self, i32)], bound: i32) -> Self {
        let first = values
            .first()
            .expect("pseudo-boolean expression must not be empty");
        let ctx = first.0.ctx;
        for (value, _) in values {
            same_context(ctx, value.ctx);
        }
        let asts: Vec<_> = values.iter().map(|v| v.0.raw).collect();
        let weights: Vec<_> = values.iter().map(|v| v.1).collect();
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_pble(
                    ctx.raw,
                    values.len().try_into().expect("too many operands"),
                    asts.as_ptr(),
                    weights.as_ptr(),
                    bound,
                )
                .expect("Z3 pseudo-boolean expression failed"),
            )
        }
    }
}

impl<'ctx> Int<'ctx> {
    fn numeral(ctx: &'ctx Context, value: &str) -> Result<Self, NulError> {
        let value = CString::new(value)?;
        let sort = Sort::int(ctx);
        Ok(unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_numeral(ctx.raw, value.as_ptr(), sort.raw)
                    .expect("invalid integer numeral"),
            )
        })
    }
    pub fn from_str(ctx: &'ctx Context, value: &str) -> Result<Self, NulError> {
        Self::numeral(ctx, value)
    }
    #[must_use]
    pub fn from_i64(ctx: &'ctx Context, value: i64) -> Self {
        Self::numeral(ctx, &value.to_string()).expect("integer has no NUL")
    }
    #[must_use]
    pub fn from_u64(ctx: &'ctx Context, value: u64) -> Self {
        Self::numeral(ctx, &value.to_string()).expect("integer has no NUL")
    }
    #[must_use]
    pub fn from_bv(value: &BV<'ctx>, signed: bool) -> Self {
        unsafe {
            Self::wrap(
                value.ctx,
                sys::Z3_mk_bv2int(value.ctx.raw, value.raw, signed).expect("Z3 conversion failed"),
            )
        }
    }
    #[must_use]
    pub fn add(values: &[&Self]) -> Self {
        nary_int(values, sys::Z3_mk_add)
    }
    #[must_use]
    pub fn sub(values: &[&Self]) -> Self {
        nary_int(values, sys::Z3_mk_sub)
    }
    #[must_use]
    pub fn mul(values: &[&Self]) -> Self {
        nary_int(values, sys::Z3_mk_mul)
    }
    #[must_use]
    pub fn unary_minus(&self) -> Self {
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_unary_minus(self.ctx.raw, self.raw).expect("Z3 negation failed"),
            )
        }
    }
    #[must_use]
    pub fn lt(&self, other: &Self) -> Bool<'ctx> {
        int_cmp(self, other, sys::Z3_mk_lt)
    }
    #[must_use]
    pub fn le(&self, other: &Self) -> Bool<'ctx> {
        int_cmp(self, other, sys::Z3_mk_le)
    }
    #[must_use]
    pub fn gt(&self, other: &Self) -> Bool<'ctx> {
        int_cmp(self, other, sys::Z3_mk_gt)
    }
    #[must_use]
    pub fn ge(&self, other: &Self) -> Bool<'ctx> {
        int_cmp(self, other, sys::Z3_mk_ge)
    }
}

type Nary = unsafe extern "C" fn(sys::Z3_context, u32, *const Z3_ast) -> Option<Z3_ast>;
fn nary_int<'ctx>(values: &[&Int<'ctx>], operation: Nary) -> Int<'ctx> {
    let (ctx, values) = raw_values(values);
    unsafe {
        Int::wrap(
            ctx,
            operation(
                ctx.raw,
                values.len().try_into().expect("too many operands"),
                values.as_ptr(),
            )
            .expect("Z3 integer operation failed"),
        )
    }
}
type Binary = unsafe extern "C" fn(sys::Z3_context, Z3_ast, Z3_ast) -> Option<Z3_ast>;
fn int_cmp<'ctx>(left: &Int<'ctx>, right: &Int<'ctx>, operation: Binary) -> Bool<'ctx> {
    same_context(left.ctx, right.ctx);
    unsafe {
        Bool::wrap(
            left.ctx,
            operation(left.ctx.raw, left.raw, right.raw).expect("Z3 comparison failed"),
        )
    }
}

macro_rules! bv_bin {
    ($name:ident, $z3:ident) => {
        #[must_use]
        pub fn $name(&self, other: &Self) -> Self {
            same_context(self.ctx, other.ctx);
            unsafe {
                Self::wrap(
                    self.ctx,
                    sys::$z3(self.ctx.raw, self.raw, other.raw)
                        .expect("Z3 bit-vector operation failed"),
                )
            }
        }
    };
}
macro_rules! bv_un {
    ($name:ident, $z3:ident) => {
        #[must_use]
        pub fn $name(&self) -> Self {
            unsafe {
                Self::wrap(
                    self.ctx,
                    sys::$z3(self.ctx.raw, self.raw).expect("Z3 bit-vector operation failed"),
                )
            }
        }
    };
}
macro_rules! bv_cmp {
    ($name:ident, $z3:ident) => {
        #[must_use]
        pub fn $name(&self, other: &Self) -> Bool<'ctx> {
            same_context(self.ctx, other.ctx);
            unsafe {
                Bool::wrap(
                    self.ctx,
                    sys::$z3(self.ctx.raw, self.raw, other.raw)
                        .expect("Z3 bit-vector comparison failed"),
                )
            }
        }
    };
}

impl<'ctx> BV<'ctx> {
    fn numeral(ctx: &'ctx Context, value: &str, bits: u32) -> Self {
        let value = CString::new(value).expect("number contains NUL");
        let sort = Sort::bitvector(ctx, bits);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_numeral(ctx.raw, value.as_ptr(), sort.raw)
                    .expect("invalid bit-vector numeral"),
            )
        }
    }
    #[must_use]
    pub fn from_i64(ctx: &'ctx Context, value: i64, bits: u32) -> Self {
        Self::numeral(ctx, &value.to_string(), bits)
    }
    #[must_use]
    pub fn from_u64(ctx: &'ctx Context, value: u64, bits: u32) -> Self {
        Self::numeral(ctx, &value.to_string(), bits)
    }
    #[must_use]
    pub fn from_int(value: &Int<'ctx>, bits: u32) -> Self {
        unsafe {
            Self::wrap(
                value.ctx,
                sys::Z3_mk_int2bv(value.ctx.raw, bits, value.raw).expect("Z3 conversion failed"),
            )
        }
    }
    bv_bin!(bvadd, Z3_mk_bvadd);
    bv_bin!(bvsub, Z3_mk_bvsub);
    bv_bin!(bvmul, Z3_mk_bvmul);
    bv_bin!(bvand, Z3_mk_bvand);
    bv_bin!(bvor, Z3_mk_bvor);
    bv_bin!(bvxor, Z3_mk_bvxor);
    bv_bin!(bvshl, Z3_mk_bvshl);
    bv_bin!(bvlshr, Z3_mk_bvlshr);
    bv_bin!(bvashr, Z3_mk_bvashr);
    bv_un!(bvneg, Z3_mk_bvneg);
    bv_un!(bvnot, Z3_mk_bvnot);
    bv_cmp!(bvult, Z3_mk_bvult);
    bv_cmp!(bvule, Z3_mk_bvule);
    bv_cmp!(bvugt, Z3_mk_bvugt);
    bv_cmp!(bvuge, Z3_mk_bvuge);
    bv_cmp!(bvslt, Z3_mk_bvslt);
    bv_cmp!(bvsle, Z3_mk_bvsle);
    bv_cmp!(bvsgt, Z3_mk_bvsgt);
    bv_cmp!(bvsge, Z3_mk_bvsge);
    #[must_use]
    pub fn extract(&self, high: u32, low: u32) -> Self {
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_extract(self.ctx.raw, high, low, self.raw).expect("Z3 extract failed"),
            )
        }
    }
    #[must_use]
    pub fn sign_ext(&self, bits: u32) -> Self {
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_sign_ext(self.ctx.raw, bits, self.raw).expect("Z3 extension failed"),
            )
        }
    }
    #[must_use]
    pub fn zero_ext(&self, bits: u32) -> Self {
        unsafe {
            Self::wrap(
                self.ctx,
                sys::Z3_mk_zero_ext(self.ctx.raw, bits, self.raw).expect("Z3 extension failed"),
            )
        }
    }
}

impl<'ctx> Seq<'ctx> {
    #[must_use]
    pub fn empty(element: &Sort<'ctx>) -> Self {
        unsafe {
            Self::wrap(
                element.ctx,
                sys::Z3_mk_seq_empty(
                    element.ctx.raw,
                    sys::Z3_mk_seq_sort(element.ctx.raw, element.raw).expect("sequence sort"),
                )
                .expect("empty sequence"),
            )
        }
    }
    #[must_use]
    pub fn unit(value: &impl Ast<'ctx>) -> Self {
        unsafe {
            Self::wrap(
                value.context(),
                sys::Z3_mk_seq_unit(value.context().raw, value.raw()).expect("sequence unit"),
            )
        }
    }
    #[must_use]
    pub fn concat(values: &[&Self]) -> Self {
        let (ctx, values) = raw_values(values);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_seq_concat(
                    ctx.raw,
                    values.len().try_into().expect("too many operands"),
                    values.as_ptr(),
                )
                .expect("sequence concatenation"),
            )
        }
    }
    #[must_use]
    pub fn nth(&self, index: u64) -> Dynamic<'ctx> {
        let index = Int::from_u64(self.ctx, index);
        unsafe {
            Dynamic::wrap(
                self.ctx,
                sys::Z3_mk_seq_nth(self.ctx.raw, self.raw, index.raw).expect("sequence index"),
            )
        }
    }
    #[must_use]
    pub fn length(&self) -> Int<'ctx> {
        unsafe {
            Int::wrap(
                self.ctx,
                sys::Z3_mk_seq_length(self.ctx.raw, self.raw).expect("sequence length"),
            )
        }
    }
}

impl<'ctx> String<'ctx> {
    pub fn from_str(ctx: &'ctx Context, value: &str) -> Result<Self, NulError> {
        let value = CString::new(value)?;
        Ok(unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_string(ctx.raw, value.as_ptr()).expect("invalid Z3 string"),
            )
        })
    }
    #[must_use]
    pub fn concat(values: &[&Self]) -> Self {
        let (ctx, values) = raw_values(values);
        unsafe {
            Self::wrap(
                ctx,
                sys::Z3_mk_seq_concat(
                    ctx.raw,
                    values.len().try_into().expect("too many operands"),
                    values.as_ptr(),
                )
                .expect("string concatenation"),
            )
        }
    }
}

#[must_use]
pub fn forall_const<'ctx>(
    bounds: &[&dyn Ast<'ctx>],
    patterns: &[&crate::Pattern<'ctx>],
    body: &Bool<'ctx>,
) -> Bool<'ctx> {
    let ctx = body.ctx;
    for bound in bounds {
        same_context(ctx, bound.context());
    }
    for pattern in patterns {
        same_context(ctx, pattern.ctx);
    }
    let bounds: Vec<_> = bounds.iter().map(|bound| bound.raw()).collect();
    let patterns: Vec<_> = patterns.iter().map(|pattern| pattern.raw).collect();
    unsafe {
        Bool::wrap(
            ctx,
            sys::Z3_mk_forall_const(
                ctx.raw,
                0,
                bounds.len().try_into().expect("too many bounds"),
                bounds.as_ptr().cast(),
                patterns.len().try_into().expect("too many patterns"),
                patterns.as_ptr(),
                body.raw,
            )
            .expect("Z3 quantifier failed"),
        )
    }
}
