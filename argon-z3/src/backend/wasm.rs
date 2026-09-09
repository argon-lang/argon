//! Z3 bindings for `wasm32-unknown-unknown`.
//!
//! Z3 is loaded as a separate WebAssembly module.  Its C API functions and
//! allocator are imported from the `z3` module, while the memory bridge is
//! imported from `argon-memory`.

#![allow(
    non_camel_case_types,
    reason = "These names intentionally match Z3's C API type names."
)]
#![allow(
    non_snake_case,
    reason = "The imported functions intentionally retain Z3's C API spelling."
)]
#![allow(
    dead_code,
    reason = "Some imported entry points are only referenced by target-specific callers; the complete API remains available for the wasm backend."
)]

use alloc::boxed::Box;
use alloc::vec;
use core::ffi::CStr;
use core::ffi::{c_char, c_int, c_uint};
use core::ptr::NonNull;
use core::{mem, ptr};

macro_rules! opaque {
    ($raw:ident, $name:ident) => {
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct $raw {
            _private: [u8; 0],
        }
        pub type $name = NonNull<$raw>;
    };
}

opaque!(_Z3_symbol, Z3_symbol);
opaque!(_Z3_config, Z3_config);
opaque!(_Z3_context, Z3_context);
opaque!(_Z3_sort, Z3_sort);
opaque!(_Z3_func_decl, Z3_func_decl);
opaque!(_Z3_ast, Z3_ast);
opaque!(_Z3_app, Z3_app);
opaque!(_Z3_pattern, Z3_pattern);
opaque!(_Z3_constructor, Z3_constructor);
opaque!(_Z3_params, Z3_params);
opaque!(_Z3_solver, Z3_solver);

pub type Z3_string = *const c_char;
pub type Z3_lbool = c_int;
pub const Z3_L_FALSE: Z3_lbool = -1;
pub const Z3_L_UNDEF: Z3_lbool = 0;
pub const Z3_L_TRUE: Z3_lbool = 1;

#[repr(u32)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SortKind {
    Uninterpreted = 0,
    Bool = 1,
    Int = 2,
    Real = 3,
    Bv = 4,
    Array = 5,
    Datatype = 6,
    Relation = 7,
    FiniteDomain = 8,
    FloatingPoint = 9,
    RoundingMode = 10,
    Seq = 11,
    Re = 12,
    Char = 13,
    TypeVar = 14,
    Unknown = 1000,
}
pub type Z3_sort_kind = SortKind;

pub type Z3_error_code = c_uint;
pub type Z3_error_handler = Option<unsafe extern "C" fn(Z3_context, Z3_error_code)>;

mod raw {
    use super::*;

    #[link(wasm_import_module = "z3")]
    unsafe extern "C" {
        pub fn malloc(size: usize) -> *mut u8;
        pub fn free(ptr: *mut u8);

        pub fn Z3_ast_to_string(c: Z3_context, a: Z3_ast) -> Z3_string;
        pub fn Z3_dec_ref(c: Z3_context, a: Z3_ast);
        pub fn Z3_del_config(c: Z3_config);
        pub fn Z3_del_constructor(c: Z3_context, constr: Z3_constructor);
        pub fn Z3_del_context(c: Z3_context);
        pub fn Z3_func_decl_to_ast(c: Z3_context, f: Z3_func_decl) -> Option<Z3_ast>;
        pub fn Z3_func_decl_to_string(c: Z3_context, d: Z3_func_decl) -> Z3_string;
        pub fn Z3_get_datatype_sort_constructor(
            c: Z3_context,
            t: Z3_sort,
            idx: c_uint,
        ) -> Option<Z3_func_decl>;
        pub fn Z3_get_datatype_sort_constructor_accessor(
            c: Z3_context,
            t: Z3_sort,
            idx_c: c_uint,
            idx_a: c_uint,
        ) -> Option<Z3_func_decl>;
        pub fn Z3_get_datatype_sort_recognizer(
            c: Z3_context,
            t: Z3_sort,
            idx: c_uint,
        ) -> Option<Z3_func_decl>;
        pub fn Z3_get_domain(c: Z3_context, d: Z3_func_decl, i: c_uint) -> Option<Z3_sort>;
        pub fn Z3_get_domain_size(c: Z3_context, d: Z3_func_decl) -> c_uint;
        pub fn Z3_get_sort(c: Z3_context, a: Z3_ast) -> Option<Z3_sort>;
        pub fn Z3_get_sort_kind(c: Z3_context, t: Z3_sort) -> Z3_sort_kind;
        pub fn Z3_inc_ref(c: Z3_context, a: Z3_ast);
        pub fn Z3_is_eq_ast(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> bool;
        pub fn Z3_is_eq_sort(c: Z3_context, s1: Z3_sort, s2: Z3_sort) -> bool;
        pub fn Z3_is_string_sort(c: Z3_context, s: Z3_sort) -> bool;
        pub fn Z3_mk_add(c: Z3_context, num_args: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_and(c: Z3_context, num_args: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_app(
            c: Z3_context,
            d: Z3_func_decl,
            num_args: c_uint,
            args: *const Z3_ast,
        ) -> Option<Z3_ast>;
        pub fn Z3_mk_bool_sort(c: Z3_context) -> Option<Z3_sort>;
        pub fn Z3_mk_bv2int(c: Z3_context, t1: Z3_ast, is_signed: bool) -> Option<Z3_ast>;
        pub fn Z3_mk_bvadd(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvsub(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvmul(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvand(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvor(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvxor(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvshl(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvlshr(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvashr(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvneg(c: Z3_context, t1: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvnot(c: Z3_context, t1: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvult(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvule(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvugt(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvuge(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvslt(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvsle(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvsgt(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bvsge(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_bv_sort(c: Z3_context, sz: c_uint) -> Option<Z3_sort>;
        pub fn Z3_mk_config() -> Option<Z3_config>;
        pub fn Z3_mk_const(c: Z3_context, s: Z3_symbol, ty: Z3_sort) -> Option<Z3_ast>;
        pub fn Z3_mk_constructor(
            c: Z3_context,
            name: Z3_symbol,
            recognizer: Z3_symbol,
            num_fields: c_uint,
            field_names: *const Z3_symbol,
            sorts: *const Option<Z3_sort>,
            sort_refs: *mut c_uint,
        ) -> Option<Z3_constructor>;
        pub fn Z3_mk_context_rc(c: Z3_config) -> Option<Z3_context>;
        pub fn Z3_mk_datatype(
            c: Z3_context,
            name: Z3_symbol,
            num_constructors: c_uint,
            constructors: *mut Z3_constructor,
        ) -> Option<Z3_sort>;
        pub fn Z3_mk_eq(c: Z3_context, l: Z3_ast, r: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_extract(
            c: Z3_context,
            high: c_uint,
            low: c_uint,
            t1: Z3_ast,
        ) -> Option<Z3_ast>;
        pub fn Z3_mk_false(c: Z3_context) -> Option<Z3_ast>;
        pub fn Z3_mk_forall_const(
            c: Z3_context,
            weight: c_uint,
            num_bound: c_uint,
            bound: *const Z3_app,
            num_patterns: c_uint,
            patterns: *const Z3_pattern,
            body: Z3_ast,
        ) -> Option<Z3_ast>;
        pub fn Z3_mk_fresh_const(c: Z3_context, prefix: Z3_string, ty: Z3_sort) -> Option<Z3_ast>;
        pub fn Z3_mk_func_decl(
            c: Z3_context,
            s: Z3_symbol,
            domain_size: c_uint,
            domain: *const Z3_sort,
            range: Z3_sort,
        ) -> Option<Z3_func_decl>;
        pub fn Z3_mk_ge(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_gt(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_implies(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_int2bv(c: Z3_context, n: c_uint, t1: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_int_sort(c: Z3_context) -> Option<Z3_sort>;
        pub fn Z3_mk_int_symbol(c: Z3_context, i: c_int) -> Option<Z3_symbol>;
        pub fn Z3_mk_le(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_lt(c: Z3_context, t1: Z3_ast, t2: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_mul(c: Z3_context, num_args: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_not(c: Z3_context, a: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_numeral(c: Z3_context, numeral: Z3_string, ty: Z3_sort) -> Option<Z3_ast>;
        pub fn Z3_mk_or(c: Z3_context, num_args: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_params(c: Z3_context) -> Option<Z3_params>;
        pub fn Z3_mk_pattern(
            c: Z3_context,
            num_patterns: c_uint,
            terms: *const Z3_ast,
        ) -> Option<Z3_pattern>;
        pub fn Z3_mk_pble(
            c: Z3_context,
            num_args: c_uint,
            args: *const Z3_ast,
            coeffs: *const c_int,
            k: c_int,
        ) -> Option<Z3_ast>;
        pub fn Z3_mk_seq_concat(c: Z3_context, n: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_seq_empty(c: Z3_context, seq: Z3_sort) -> Option<Z3_ast>;
        pub fn Z3_mk_seq_length(c: Z3_context, s: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_seq_nth(c: Z3_context, s: Z3_ast, index: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_seq_sort(c: Z3_context, s: Z3_sort) -> Option<Z3_sort>;
        pub fn Z3_mk_seq_unit(c: Z3_context, a: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_sign_ext(c: Z3_context, i: c_uint, t1: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_solver(c: Z3_context) -> Option<Z3_solver>;
        pub fn Z3_mk_string(c: Z3_context, s: Z3_string) -> Option<Z3_ast>;
        pub fn Z3_mk_string_sort(c: Z3_context) -> Option<Z3_sort>;
        pub fn Z3_mk_string_symbol(c: Z3_context, s: Z3_string) -> Option<Z3_symbol>;
        pub fn Z3_mk_sub(c: Z3_context, num_args: c_uint, args: *const Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_true(c: Z3_context) -> Option<Z3_ast>;
        pub fn Z3_mk_unary_minus(c: Z3_context, arg: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_mk_uninterpreted_sort(c: Z3_context, s: Z3_symbol) -> Option<Z3_sort>;
        pub fn Z3_mk_zero_ext(c: Z3_context, i: c_uint, t1: Z3_ast) -> Option<Z3_ast>;
        pub fn Z3_params_dec_ref(c: Z3_context, p: Z3_params);
        pub fn Z3_params_inc_ref(c: Z3_context, p: Z3_params);
        pub fn Z3_params_set_uint(c: Z3_context, p: Z3_params, k: Z3_symbol, v: c_uint);
        pub fn Z3_pattern_to_ast(c: Z3_context, p: Z3_pattern) -> Option<Z3_ast>;
        pub fn Z3_set_error_handler(c: Z3_context, h: Z3_error_handler);
        pub fn Z3_solver_assert(c: Z3_context, s: Z3_solver, a: Z3_ast);
        pub fn Z3_solver_check(c: Z3_context, s: Z3_solver) -> Z3_lbool;
        pub fn Z3_solver_dec_ref(c: Z3_context, s: Z3_solver);
        pub fn Z3_solver_inc_ref(c: Z3_context, s: Z3_solver);
        pub fn Z3_solver_set_params(c: Z3_context, s: Z3_solver, p: Z3_params);
        pub fn Z3_sort_to_ast(c: Z3_context, s: Z3_sort) -> Option<Z3_ast>;
        pub fn Z3_sort_to_string(c: Z3_context, s: Z3_sort) -> Z3_string;
    }
}

pub use raw::{
    Z3_dec_ref, Z3_del_config, Z3_del_constructor, Z3_del_context, Z3_func_decl_to_ast,
    Z3_get_datatype_sort_constructor, Z3_get_datatype_sort_constructor_accessor,
    Z3_get_datatype_sort_recognizer, Z3_get_domain, Z3_get_domain_size, Z3_get_sort,
    Z3_get_sort_kind, Z3_inc_ref, Z3_is_eq_ast, Z3_is_eq_sort, Z3_is_string_sort, Z3_mk_bool_sort,
    Z3_mk_bv_sort, Z3_mk_bv2int, Z3_mk_bvadd, Z3_mk_bvand, Z3_mk_bvashr, Z3_mk_bvlshr, Z3_mk_bvmul,
    Z3_mk_bvneg, Z3_mk_bvnot, Z3_mk_bvor, Z3_mk_bvsge, Z3_mk_bvsgt, Z3_mk_bvshl, Z3_mk_bvsle,
    Z3_mk_bvslt, Z3_mk_bvsub, Z3_mk_bvuge, Z3_mk_bvugt, Z3_mk_bvule, Z3_mk_bvult, Z3_mk_bvxor,
    Z3_mk_config, Z3_mk_const, Z3_mk_context_rc, Z3_mk_eq, Z3_mk_extract, Z3_mk_false, Z3_mk_ge,
    Z3_mk_gt, Z3_mk_implies, Z3_mk_int_sort, Z3_mk_int_symbol, Z3_mk_int2bv, Z3_mk_le, Z3_mk_lt,
    Z3_mk_not, Z3_mk_params, Z3_mk_seq_empty, Z3_mk_seq_length, Z3_mk_seq_nth, Z3_mk_seq_sort,
    Z3_mk_seq_unit, Z3_mk_sign_ext, Z3_mk_solver, Z3_mk_true, Z3_mk_unary_minus,
    Z3_mk_uninterpreted_sort, Z3_mk_zero_ext, Z3_params_dec_ref, Z3_params_inc_ref,
    Z3_params_set_uint, Z3_pattern_to_ast, Z3_set_error_handler, Z3_solver_assert, Z3_solver_check,
    Z3_solver_dec_ref, Z3_solver_inc_ref, Z3_solver_set_params, Z3_sort_to_ast,
};

#[link(wasm_import_module = "argon-memory")]
unsafe extern "C" {
    pub fn copy_argonc_to_z3(source: *const u8, destination: *mut u8, length: usize);
    pub fn copy_z3_to_argonc(source: *const u8, destination: *mut u8, length: usize);
    pub fn z3_string_length(source: *const u8) -> usize;
    pub fn copy_z3_string_to_argonc(
        source: *const u8,
        destination: *mut u8,
        capacity: usize,
    ) -> usize;
}

unsafe fn z3_alloc<T>(count: usize) -> *mut T {
    unsafe {
        if count == 0 {
            return ptr::null_mut();
        }
        let bytes = count
            .checked_mul(mem::size_of::<T>())
            .expect("Z3 argument buffer is too large");
        let destination = raw::malloc(bytes).cast::<T>();
        assert!(!destination.is_null(), "Z3 allocation failed");
        destination
    }
}

unsafe fn z3_copy_from_argon<T: Copy>(source: *const T, count: usize) -> *mut T {
    unsafe {
        let destination = z3_alloc(count);
        if count == 0 {
            return destination;
        }
        let bytes = count
            .checked_mul(mem::size_of::<T>())
            .expect("Z3 argument buffer is too large");
        copy_argonc_to_z3(source.cast(), destination.cast(), bytes);
        destination
    }
}

unsafe fn z3_free<T>(value: *mut T) {
    unsafe {
        if !value.is_null() {
            raw::free(value.cast());
        }
    }
}

unsafe fn z3_copy_string(source: Z3_string) -> *mut c_char {
    unsafe {
        assert!(!source.is_null(), "Z3 string argument must not be null");
        let bytes = CStr::from_ptr(source).to_bytes_with_nul();
        z3_copy_from_argon(bytes.as_ptr(), bytes.len()).cast()
    }
}

unsafe fn argon_string_from_z3(source: Z3_string) -> Z3_string {
    unsafe {
        assert!(!source.is_null(), "Z3 returned a null string");
        let length = z3_string_length(source.cast());
        let mut result = vec![0_u8; length.saturating_add(1)];
        let copied = copy_z3_string_to_argonc(source.cast(), result.as_mut_ptr(), result.len());
        assert!(
            copied <= result.len(),
            "Z3 string copy exceeded its capacity"
        );
        Box::leak(result.into_boxed_slice()).as_ptr().cast()
    }
}

pub unsafe extern "C" fn Z3_ast_to_string(c: Z3_context, a: Z3_ast) -> Z3_string {
    unsafe { argon_string_from_z3(raw::Z3_ast_to_string(c, a)) }
}

pub unsafe extern "C" fn Z3_func_decl_to_string(c: Z3_context, d: Z3_func_decl) -> Z3_string {
    unsafe { argon_string_from_z3(raw::Z3_func_decl_to_string(c, d)) }
}

pub unsafe extern "C" fn Z3_sort_to_string(c: Z3_context, s: Z3_sort) -> Z3_string {
    unsafe { argon_string_from_z3(raw::Z3_sort_to_string(c, s)) }
}

macro_rules! ast_array_call {
    ($name:ident, $raw:ident) => {
        pub unsafe extern "C" fn $name(
            c: Z3_context,
            num_args: c_uint,
            args: *const Z3_ast,
        ) -> Option<Z3_ast> {
            unsafe {
                let args = z3_copy_from_argon(args, num_args as usize);
                let result = raw::$raw(c, num_args, args);
                z3_free(args);
                result
            }
        }
    };
}

ast_array_call!(Z3_mk_add, Z3_mk_add);
ast_array_call!(Z3_mk_and, Z3_mk_and);
ast_array_call!(Z3_mk_mul, Z3_mk_mul);
ast_array_call!(Z3_mk_or, Z3_mk_or);
ast_array_call!(Z3_mk_sub, Z3_mk_sub);
ast_array_call!(Z3_mk_seq_concat, Z3_mk_seq_concat);

pub unsafe extern "C" fn Z3_mk_app(
    c: Z3_context,
    d: Z3_func_decl,
    num_args: c_uint,
    args: *const Z3_ast,
) -> Option<Z3_ast> {
    unsafe {
        let args = z3_copy_from_argon(args, num_args as usize);
        let result = raw::Z3_mk_app(c, d, num_args, args);
        z3_free(args);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_fresh_const(
    c: Z3_context,
    prefix: Z3_string,
    ty: Z3_sort,
) -> Option<Z3_ast> {
    unsafe {
        let prefix = z3_copy_string(prefix);
        let result = raw::Z3_mk_fresh_const(c, prefix.cast(), ty);
        z3_free(prefix);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_numeral(
    c: Z3_context,
    numeral: Z3_string,
    ty: Z3_sort,
) -> Option<Z3_ast> {
    unsafe {
        let numeral = z3_copy_string(numeral);
        let result = raw::Z3_mk_numeral(c, numeral.cast(), ty);
        z3_free(numeral);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_string(c: Z3_context, value: Z3_string) -> Option<Z3_ast> {
    unsafe {
        let value = z3_copy_string(value);
        let result = raw::Z3_mk_string(c, value.cast());
        z3_free(value);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_string_symbol(c: Z3_context, value: Z3_string) -> Option<Z3_symbol> {
    unsafe {
        let value = z3_copy_string(value);
        let result = raw::Z3_mk_string_symbol(c, value.cast());
        z3_free(value);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_string_sort(c: Z3_context) -> Option<Z3_sort> {
    unsafe { raw::Z3_mk_string_sort(c) }
}

pub unsafe extern "C" fn Z3_mk_func_decl(
    c: Z3_context,
    s: Z3_symbol,
    domain_size: c_uint,
    domain: *const Z3_sort,
    range: Z3_sort,
) -> Option<Z3_func_decl> {
    unsafe {
        let domain = z3_copy_from_argon(domain, domain_size as usize);
        let result = raw::Z3_mk_func_decl(c, s, domain_size, domain, range);
        z3_free(domain);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_pattern(
    c: Z3_context,
    num_patterns: c_uint,
    terms: *const Z3_ast,
) -> Option<Z3_pattern> {
    unsafe {
        let terms = z3_copy_from_argon(terms, num_patterns as usize);
        let result = raw::Z3_mk_pattern(c, num_patterns, terms);
        z3_free(terms);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_pble(
    c: Z3_context,
    num_args: c_uint,
    args: *const Z3_ast,
    coeffs: *const c_int,
    k: c_int,
) -> Option<Z3_ast> {
    unsafe {
        let args = z3_copy_from_argon(args, num_args as usize);
        let coeffs = z3_copy_from_argon(coeffs, num_args as usize);
        let result = raw::Z3_mk_pble(c, num_args, args, coeffs, k);
        z3_free(args);
        z3_free(coeffs);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_forall_const(
    c: Z3_context,
    weight: c_uint,
    num_bound: c_uint,
    bound: *const Z3_app,
    num_patterns: c_uint,
    patterns: *const Z3_pattern,
    body: Z3_ast,
) -> Option<Z3_ast> {
    unsafe {
        let bound = z3_copy_from_argon(bound, num_bound as usize);
        let patterns = z3_copy_from_argon(patterns, num_patterns as usize);
        let result =
            raw::Z3_mk_forall_const(c, weight, num_bound, bound, num_patterns, patterns, body);
        z3_free(bound);
        z3_free(patterns);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_constructor(
    c: Z3_context,
    name: Z3_symbol,
    recognizer: Z3_symbol,
    num_fields: c_uint,
    field_names: *const Z3_symbol,
    sorts: *const Option<Z3_sort>,
    sort_refs: *mut c_uint,
) -> Option<Z3_constructor> {
    unsafe {
        let field_names = z3_copy_from_argon(field_names, num_fields as usize);
        let sorts = z3_copy_from_argon(sorts, num_fields as usize);
        let sort_refs_z3 = z3_alloc::<c_uint>(num_fields as usize);
        let result = raw::Z3_mk_constructor(
            c,
            name,
            recognizer,
            num_fields,
            field_names,
            sorts,
            sort_refs_z3,
        );
        if num_fields != 0 {
            copy_z3_to_argonc(
                sort_refs_z3.cast(),
                sort_refs.cast(),
                (num_fields as usize) * mem::size_of::<c_uint>(),
            );
        }
        z3_free(field_names);
        z3_free(sorts);
        z3_free(sort_refs_z3);
        result
    }
}

pub unsafe extern "C" fn Z3_mk_datatype(
    c: Z3_context,
    name: Z3_symbol,
    num_constructors: c_uint,
    constructors: *mut Z3_constructor,
) -> Option<Z3_sort> {
    unsafe {
        let constructors_z3 = z3_copy_from_argon(constructors, num_constructors as usize);
        let result = raw::Z3_mk_datatype(c, name, num_constructors, constructors_z3);
        z3_free(constructors_z3);
        result
    }
}
