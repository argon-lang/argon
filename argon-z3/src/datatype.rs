use crate::backend::sys;
use crate::{Context, FuncDecl, Sort, Symbol, same_context, symbol};
use std::ffi::CString;
use sys::Z3_constructor;

#[derive(Debug)]
pub enum DatatypeAccessor<'ctx> {
    Sort(Sort<'ctx>),
}
impl<'ctx> DatatypeAccessor<'ctx> {
    #[must_use]
    pub fn sort(sort: Sort<'ctx>) -> Self {
        Self::Sort(sort)
    }
}

#[derive(Debug)]
pub struct DatatypeVariant<'ctx> {
    pub constructor: FuncDecl<'ctx>,
    pub tester: FuncDecl<'ctx>,
    pub accessors: Vec<FuncDecl<'ctx>>,
}
#[derive(Debug)]
pub struct DatatypeSort<'ctx> {
    pub sort: Sort<'ctx>,
    pub variants: Vec<DatatypeVariant<'ctx>>,
}

pub struct DatatypeBuilder<'ctx> {
    ctx: &'ctx Context,
    name: Symbol,
    variants: Vec<(String, Vec<(String, DatatypeAccessor<'ctx>)>)>,
}

impl<'ctx> DatatypeBuilder<'ctx> {
    #[must_use]
    pub fn new(ctx: &'ctx Context, name: impl Into<Symbol>) -> Self {
        Self {
            ctx,
            name: name.into(),
            variants: Vec::new(),
        }
    }
    #[must_use]
    pub fn variant(mut self, name: &str, fields: Vec<(&str, DatatypeAccessor<'ctx>)>) -> Self {
        for (_, DatatypeAccessor::Sort(sort)) in &fields {
            same_context(self.ctx, sort.ctx);
        }
        self.variants.push((
            name.into(),
            fields
                .into_iter()
                .map(|(name, value)| (name.into(), value))
                .collect(),
        ));
        self
    }
    #[must_use]
    pub fn finish(self) -> DatatypeSort<'ctx> {
        assert!(!self.variants.is_empty(), "datatype requires a variant");
        let mut constructors: Vec<Z3_constructor> = Vec::with_capacity(self.variants.len());
        unsafe {
            for (variant, fields) in &self.variants {
                let recognizer = format!("is-{variant}");
                let variant = CString::new(variant.as_str()).expect("variant name contains NUL");
                let recognizer = CString::new(recognizer).expect("recognizer name contains NUL");
                let field_names: Vec<_> = fields
                    .iter()
                    .map(|(name, _)| symbol(self.ctx, name.clone()))
                    .collect();
                let field_sorts: Vec<_> = fields
                    .iter()
                    .map(|(_, DatatypeAccessor::Sort(sort))| Some(sort.raw))
                    .collect();
                let mut sort_refs = vec![0_u32; fields.len()];
                constructors.push(
                    sys::Z3_mk_constructor(
                        self.ctx.raw,
                        sys::Z3_mk_string_symbol(self.ctx.raw, variant.as_ptr())
                            .expect("invalid variant symbol"),
                        sys::Z3_mk_string_symbol(self.ctx.raw, recognizer.as_ptr())
                            .expect("invalid recognizer symbol"),
                        fields.len().try_into().expect("too many fields"),
                        field_names.as_ptr(),
                        field_sorts.as_ptr(),
                        sort_refs.as_mut_ptr(),
                    )
                    .expect("Z3 failed to create constructor"),
                );
            }
            let mut raw_constructors = constructors.clone();
            let raw_sort = sys::Z3_mk_datatype(
                self.ctx.raw,
                symbol(self.ctx, self.name),
                raw_constructors
                    .len()
                    .try_into()
                    .expect("too many variants"),
                raw_constructors.as_mut_ptr(),
            )
            .expect("Z3 failed to create datatype");
            let sort = Sort::wrap(self.ctx, raw_sort);
            let variants = self
                .variants
                .iter()
                .enumerate()
                .map(|(variant_index, (_, fields))| {
                    let variant_index =
                        u32::try_from(variant_index).expect("too many datatype variants");
                    let constructor = FuncDecl::wrap(
                        self.ctx,
                        sys::Z3_get_datatype_sort_constructor(
                            self.ctx.raw,
                            raw_sort,
                            variant_index,
                        )
                        .expect("missing constructor"),
                    );
                    let tester = FuncDecl::wrap(
                        self.ctx,
                        sys::Z3_get_datatype_sort_recognizer(self.ctx.raw, raw_sort, variant_index)
                            .expect("missing tester"),
                    );
                    let accessors = fields
                        .iter()
                        .enumerate()
                        .map(|(field_index, _)| {
                            let field_index =
                                u32::try_from(field_index).expect("too many datatype fields");
                            FuncDecl::wrap(
                                self.ctx,
                                sys::Z3_get_datatype_sort_constructor_accessor(
                                    self.ctx.raw,
                                    raw_sort,
                                    variant_index,
                                    field_index,
                                )
                                .expect("missing accessor"),
                            )
                        })
                        .collect();
                    DatatypeVariant {
                        constructor,
                        tester,
                        accessors,
                    }
                })
                .collect();
            for constructor in constructors {
                sys::Z3_del_constructor(self.ctx.raw, constructor);
            }
            DatatypeSort { sort, variants }
        }
    }
}

impl core::fmt::Debug for DatatypeBuilder<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("DatatypeBuilder")
            .field("name", &self.name)
            .field("variants", &self.variants)
            .finish()
    }
}
