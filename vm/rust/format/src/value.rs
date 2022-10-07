use std::{fmt::Display, alloc::Layout};
use itertools::Itertools;

#[derive(Clone, PartialEq)]
pub enum Value {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    Tuple(Vec<Value>),
    ObjectReference(*mut u8)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int8(i) => write!(f, "{}", i)?,
            Value::Int16(i) => write!(f, "{}", i)?,
            Value::Int32(i) => write!(f, "{}", i)?,
            Value::Int64(i) => write!(f, "{}", i)?,
            Value::Float32(x) => write!(f, "{}", x)?,
            Value::Float64(x) => write!(f, "{}", x)?,
            Value::Tuple(values) => write!(f, "({})", values.iter().format(", "))?,
            Value::ObjectReference(addr) => write!(f, "{:?}", addr)?,
        }

        Ok(())
    }
}

#[derive(Clone)]
pub enum ValueType {
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    Tuple(Vec<ValueType>),
    ObjectReference,
}

impl ValueType {
    pub fn default_value(&self) -> Value {
        match self {
            ValueType::Int8 => Value::Int8(0),
            ValueType::Int16 => Value::Int16(0),
            ValueType::Int32 => Value::Int32(0),
            ValueType::Int64 => Value::Int64(0),
            ValueType::Float32 => Value::Float32(0.0),
            ValueType::Float64 => Value::Float64(0.0),
            ValueType::Tuple(elems) =>
                Value::Tuple(elems.iter().map(|elem| elem.default_value()).collect()),
            ValueType::ObjectReference => Value::ObjectReference(std::ptr::null_mut()),
        }
    }

    pub fn get_layout(&self) -> Layout {
        match self {
            ValueType::Int8 => Layout::new::<i8>(),
            ValueType::Int16 => Layout::new::<i16>(),
            ValueType::Int32 => Layout::new::<i32>(),
            ValueType::Int64 => Layout::new::<i64>(),
            ValueType::Float32 => Layout::new::<f32>(),
            ValueType::Float64 => Layout::new::<f64>(),
            ValueType::Tuple(elems) => {
                let mut layout = Layout::from_size_align(0, 1).unwrap();
                for elem in elems {
                    layout = layout.extend(elem.get_layout()).unwrap().0;
                }
                layout
            },
            ValueType::ObjectReference => Layout::new::<*mut u8>(),
        }
    }

    #[cfg(target_pointer_width = "64")]
    pub const fn for_usize() -> ValueType {
        ValueType::Int64
    }

    #[cfg(target_pointer_width = "32")]
    pub const fn for_usize() -> ValueType {
        ValueType::Int32
    }
    
    #[cfg(target_pointer_width = "16")]
    pub const fn for_usize() -> ValueType {
        ValueType::Int16
    }
}
