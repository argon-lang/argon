use alloc::{
    format,
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use argon_util::InternalCompilerError;
use core::cell::RefCell;
use embedded_io_async::{ErrorType, Read, Write};
use js_sys::{Array, Function, Object, Promise, Reflect, Uint8Array};
use wasm_bindgen::{JsCast, JsValue, closure::Closure};
use wasm_bindgen_futures::{JsFuture, future_to_promise};

use crate::{InputFile, InputStream, OutputDirectory, OutputFile, OutputStream};

fn message(value: JsValue) -> String {
    value.as_string().unwrap_or_else(|| format!("{value:?}"))
}

fn error(value: JsValue) -> InternalCompilerError {
    InternalCompilerError::JavaScript(message(value))
}

fn method(value: &JsValue, name: &str) -> Result<Function, InternalCompilerError> {
    Reflect::get(value, &JsValue::from_str(name))
        .map_err(error)?
        .dyn_into::<Function>()
        .map_err(|_| InternalCompilerError::JavaScript(format!("{name} is not a function")))
}

async fn call_async(
    this: &JsValue,
    name: &str,
    argument: Option<&JsValue>,
) -> Result<JsValue, InternalCompilerError> {
    let function = method(this, name)?;
    let result = match argument {
        Some(argument) => function.call1(this, argument),
        None => function.call0(this),
    }
    .map_err(error)?;
    JsFuture::from(Promise::resolve(&result))
        .await
        .map_err(error)
}

fn object_with_method(value: JsValue, name: &str) -> Result<JsValue, InternalCompilerError> {
    if !value.is_object() {
        return Err(InternalCompilerError::JavaScript(
            "expected an object".into(),
        ));
    }
    method(&value, name)?;
    Ok(value)
}

pub struct JsInputStream(JsValue);

impl TryFrom<JsValue> for JsInputStream {
    type Error = InternalCompilerError;
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        object_with_method(value.clone(), "read")?;
        object_with_method(value.clone(), "close")?;
        Ok(Self(value))
    }
}

impl ErrorType for JsInputStream {
    type Error = InternalCompilerError;
}
impl Read for JsInputStream {
    async fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let array = Uint8Array::new_with_length(buf.len() as u32);
        let result = call_async(&self.0, "read", Some(array.as_ref())).await?;
        let numeric_count = result
            .as_f64()
            .filter(|n| n.is_finite() && *n >= 0.0)
            .ok_or_else(|| {
                InternalCompilerError::JavaScript("read returned an invalid byte count".into())
            })?;
        let count = numeric_count as usize;
        if numeric_count != count as f64 {
            return Err(InternalCompilerError::JavaScript(
                "read returned a non-integer byte count".into(),
            ));
        }
        if count > buf.len() {
            return Err(InternalCompilerError::JavaScript(
                "read returned a byte count larger than the buffer".into(),
            ));
        }
        array.subarray(0, count as u32).copy_to(&mut buf[..count]);
        Ok(count)
    }
}
impl InputStream for JsInputStream {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        call_async(&self.0, "close", None).await.map(|_| ())
    }
    fn into_js_value(self) -> JsValue {
        self.0
    }
}

pub struct JsOutputStream(JsValue);
impl TryFrom<JsValue> for JsOutputStream {
    type Error = InternalCompilerError;
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        object_with_method(value.clone(), "write")?;
        object_with_method(value.clone(), "close")?;
        Ok(Self(value))
    }
}
impl ErrorType for JsOutputStream {
    type Error = InternalCompilerError;
}
impl Write for JsOutputStream {
    async fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        let array = Uint8Array::from(buf);
        call_async(&self.0, "write", Some(array.as_ref())).await?;
        Ok(buf.len())
    }
    async fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}
impl OutputStream for JsOutputStream {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        call_async(&self.0, "close", None).await.map(|_| ())
    }
    fn into_js_value(self) -> JsValue {
        self.0
    }
}

pub struct JsInputFile {
    value: JsValue,
    name: String,
    #[cfg(feature = "std")]
    path: std::path::PathBuf,
}
impl TryFrom<JsValue> for JsInputFile {
    type Error = InternalCompilerError;
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        object_with_method(value.clone(), "open")?;
        let file_name = Reflect::get(&value, &JsValue::from_str("fileName")).map_err(error)?;
        let name = if let Some(name) = file_name.as_string() {
            name
        } else {
            method(&value, "fileName")?
                .call0(&value)
                .map_err(error)?
                .as_string()
                .ok_or_else(|| {
                    InternalCompilerError::JavaScript("fileName did not return a string".into())
                })?
        };
        Ok(Self {
            #[cfg(feature = "std")]
            path: name.clone().into(),
            name,
            value,
        })
    }
}
impl InputFile for JsInputFile {
    type Reader = JsInputStream;
    #[cfg(feature = "std")]
    fn path(&self) -> &std::path::Path {
        &self.path
    }
    #[cfg(not(feature = "std"))]
    fn location_file(&self) -> &parse18_runtime::LocationFileView {
        &self.name
    }
    async fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
        call_async(&self.value, "open", None).await?.try_into()
    }
    fn into_js_value(self) -> JsValue {
        self.value
    }
}

pub struct JsOutputFile(JsValue);
impl TryFrom<JsValue> for JsOutputFile {
    type Error = InternalCompilerError;
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        object_with_method(value.clone(), "open")?;
        object_with_method(value.clone(), "delete")?;
        Ok(Self(value))
    }
}
impl OutputFile for JsOutputFile {
    type Writer = JsOutputStream;
    #[cfg(feature = "std")]
    fn path(&self) -> &std::path::Path {
        std::path::Path::new("javascript-output")
    }
    async fn open(&self) -> Result<Self::Writer, InternalCompilerError> {
        call_async(&self.0, "open", None).await?.try_into()
    }
    async fn delete(&self) -> Result<(), InternalCompilerError> {
        call_async(&self.0, "delete", None).await.map(|_| ())
    }
    fn into_js_value(self) -> JsValue {
        self.0
    }
}

pub struct JsOutputDirectory(JsValue);
impl TryFrom<JsValue> for JsOutputDirectory {
    type Error = InternalCompilerError;
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        object_with_method(value.clone(), "getFile")?;
        Ok(Self(value))
    }
}
impl OutputDirectory for JsOutputDirectory {
    type File = JsOutputFile;
    fn create_file(&self, name: &str) -> Result<Self::File, InternalCompilerError> {
        let args = Array::new();
        for part in name.split('/') {
            args.push(&JsValue::from_str(part));
        }
        method(&self.0, "getFile")?
            .apply(&self.0, &args)
            .map_err(error)?
            .try_into()
    }
    fn into_js_value(self) -> JsValue {
        self.0
    }
}

fn set(object: &Object, name: &str, value: &JsValue) {
    Reflect::set(object, &JsValue::from_str(name), value).expect("new object is extensible");
}

pub fn rust_input_file<I: InputFile + 'static>(file: I) -> JsValue
where
    I::Reader: 'static,
{
    let file = Rc::new(file);
    #[cfg(feature = "std")]
    let name = file.location_file().to_string_lossy().into_owned();
    #[cfg(not(feature = "std"))]
    let name = file.location_file().to_string();
    let state = file.clone();
    let open = Closure::<dyn FnMut() -> Promise>::new(move || {
        let state = state.clone();
        future_to_promise(async move {
            state
                .open()
                .await
                .map(InputStream::into_js_value)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        })
    });
    let object = Object::new();
    set(&object, "fileName", &JsValue::from_str(&name));
    set(&object, "open", open.as_ref());
    open.forget();
    object.into()
}

pub fn rust_input_stream<R: InputStream + 'static>(reader: R) -> JsValue {
    let state = Rc::new(RefCell::new(Some(reader)));
    let read_state = state.clone();
    let read = Closure::<dyn FnMut(Uint8Array) -> Promise>::new(move |array: Uint8Array| {
        let state = read_state.clone();
        future_to_promise(async move {
            let mut reader = state
                .borrow_mut()
                .take()
                .ok_or_else(|| JsValue::from_str("stream is closed"))?;
            let mut bytes = vec![0; array.length() as usize];
            let result = reader.read(&mut bytes).await;
            state.borrow_mut().replace(reader);
            let count = result.map_err(|e| JsValue::from_str(&e.to_string()))?;
            array.set(&Uint8Array::from(&bytes[..count]), 0);
            Ok::<JsValue, JsValue>(JsValue::from_f64(count as f64))
        })
    });
    let close_state = state;
    let close = Closure::<dyn FnMut() -> Promise>::new(move || {
        let state = close_state.clone();
        future_to_promise(async move {
            let mut reader = state
                .borrow_mut()
                .take()
                .ok_or_else(|| JsValue::from_str("stream is closed"))?;
            reader
                .close()
                .await
                .map_err(|e| JsValue::from_str(&e.to_string()))?;
            Ok::<JsValue, JsValue>(JsValue::UNDEFINED)
        })
    });
    let object = Object::new();
    set(&object, "read", read.as_ref());
    set(&object, "close", close.as_ref());
    read.forget();
    close.forget();
    object.into()
}

pub fn rust_output_file<O: OutputFile + 'static>(file: O) -> JsValue
where
    O::Writer: 'static,
{
    let file = Rc::new(file);
    let open_state = file.clone();
    let delete_state = file;
    let open = Closure::<dyn FnMut() -> Promise>::new(move || {
        let state = open_state.clone();
        future_to_promise(async move {
            state
                .open()
                .await
                .map(OutputStream::into_js_value)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        })
    });
    let delete = Closure::<dyn FnMut() -> Promise>::new(move || {
        let state = delete_state.clone();
        future_to_promise(async move {
            state
                .delete()
                .await
                .map(|()| JsValue::UNDEFINED)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        })
    });
    let object = Object::new();
    set(&object, "open", open.as_ref());
    set(&object, "delete", delete.as_ref());
    open.forget();
    delete.forget();
    object.into()
}

pub fn rust_output_stream<W: OutputStream + 'static>(writer: W) -> JsValue {
    let state = Rc::new(RefCell::new(Some(writer)));
    let write_state = state.clone();
    let write = Closure::<dyn FnMut(Uint8Array) -> Promise>::new(move |array: Uint8Array| {
        let state = write_state.clone();
        future_to_promise(async move {
            let mut writer = state
                .borrow_mut()
                .take()
                .ok_or_else(|| JsValue::from_str("stream is closed"))?;
            let mut bytes = vec![0; array.length() as usize];
            array.copy_to(&mut bytes);
            let result = writer.write_all(&bytes).await;
            state.borrow_mut().replace(writer);
            result
                .map(|()| JsValue::UNDEFINED)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        })
    });
    let close_state = state;
    let close = Closure::<dyn FnMut() -> Promise>::new(move || {
        let state = close_state.clone();
        future_to_promise(async move {
            let mut writer = state
                .borrow_mut()
                .take()
                .ok_or_else(|| JsValue::from_str("stream is closed"))?;
            writer
                .flush()
                .await
                .map_err(|e| JsValue::from_str(&e.to_string()))?;
            writer
                .close()
                .await
                .map_err(|e| JsValue::from_str(&e.to_string()))?;
            Ok::<JsValue, JsValue>(JsValue::UNDEFINED)
        })
    });
    let object = Object::new();
    set(&object, "write", write.as_ref());
    set(&object, "close", close.as_ref());
    write.forget();
    close.forget();
    object.into()
}

pub fn rust_output_directory<O: OutputDirectory + 'static>(directory: O) -> JsValue
where
    O::File: 'static,
    <O::File as OutputFile>::Writer: 'static,
{
    let directory = Rc::new(directory);
    let get_file =
        Closure::<dyn FnMut(Array) -> Result<JsValue, JsValue>>::new(move |parts: Array| {
            let mut values = Vec::new();
            for part in parts.iter() {
                values.push(
                    part.as_string()
                        .ok_or_else(|| JsValue::from_str("path part is not a string"))?,
                );
            }
            directory
                .create_file(&values.join("/"))
                .map(OutputFile::into_js_value)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        });
    let wrapper =
        Function::new_with_args("f", "return { getFile(...parts) { return f(parts); } };");
    let object = wrapper
        .call1(&JsValue::UNDEFINED, get_file.as_ref())
        .expect("wrapper function succeeds");
    get_file.forget();
    object
}
