use alloc::{format, string::ToString, vec, vec::Vec};
use argon_io::{InputDirectory, InputFile, InputStream, OutputDirectory, OutputFile, OutputStream};
use argon_util::InternalCompilerError;
use embedded_io::ErrorType;
use js_sys::{Array, Function, Promise, Uint8Array};
use std::path::{Path, PathBuf};
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;

fn js_error(value: JsValue) -> InternalCompilerError {
    InternalCompilerError::JavaScript(value.as_string().unwrap_or_else(|| format!("{value:?}")))
}

async fn node_call(source: &str, args: &[JsValue]) -> Result<JsValue, InternalCompilerError> {
    let names = (0..args.len())
        .map(|index| format!("a{index}"))
        .collect::<Vec<_>>()
        .join(",");
    let function = Function::new_with_args(&names, source);
    let args = Array::from_iter(args.iter());
    let value = function
        .apply(&JsValue::UNDEFINED, &args)
        .map_err(js_error)?;
    JsFuture::from(value.dyn_into::<Promise>().map_err(js_error)?)
        .await
        .map_err(js_error)
}

#[derive(Clone, Debug)]
pub struct LocalInputFile {
    path: PathBuf,
}
impl LocalInputFile {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}
impl From<PathBuf> for LocalInputFile {
    fn from(path: PathBuf) -> Self {
        Self::new(path)
    }
}

pub struct LocalFileReader {
    bytes: Vec<u8>,
    offset: usize,
}
impl ErrorType for LocalFileReader {
    type Error = InternalCompilerError;
}
impl argon_io::Read for LocalFileReader {
    async fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let count = core::cmp::min(buf.len(), self.bytes.len().saturating_sub(self.offset));
        buf[..count].copy_from_slice(&self.bytes[self.offset..self.offset + count]);
        self.offset += count;
        Ok(count)
    }
}
impl InputStream for LocalFileReader {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        Ok(())
    }
}
impl InputFile for LocalInputFile {
    type Reader = LocalFileReader;
    fn path(&self) -> &Path {
        &self.path
    }
    async fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
        let value = node_call(
            "return import('node:fs/promises').then(fs => fs.readFile(a0));",
            &[JsValue::from_str(&self.path.to_string_lossy())],
        )
        .await?;
        let array = Uint8Array::new(&value);
        let mut bytes = vec![0; array.length() as usize];
        array.copy_to(&mut bytes);
        Ok(LocalFileReader { bytes, offset: 0 })
    }
}

#[derive(Clone, Debug)]
pub struct LocalOutputFile {
    path: PathBuf,
}
impl LocalOutputFile {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}
impl From<PathBuf> for LocalOutputFile {
    fn from(path: PathBuf) -> Self {
        Self::new(path)
    }
}
pub struct LocalFileWriter {
    path: PathBuf,
    bytes: Vec<u8>,
}
impl ErrorType for LocalFileWriter {
    type Error = InternalCompilerError;
}
impl argon_io::Write for LocalFileWriter {
    async fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        self.bytes.extend_from_slice(buf);
        Ok(buf.len())
    }
    async fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}
impl OutputStream for LocalFileWriter {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        let array = Uint8Array::from(self.bytes.as_slice());
        node_call("return import('node:fs/promises').then(async fs => { const p = await import('node:path'); await fs.mkdir(p.dirname(a0), {recursive:true}); await fs.writeFile(a0,a1); });", &[JsValue::from_str(&self.path.to_string_lossy()), array.into()]).await.map(|_| ())
    }
}
impl OutputFile for LocalOutputFile {
    type Writer = LocalFileWriter;
    fn path(&self) -> &Path {
        &self.path
    }
    async fn open(&self) -> Result<Self::Writer, InternalCompilerError> {
        Ok(LocalFileWriter {
            path: self.path.clone(),
            bytes: Vec::new(),
        })
    }
    async fn delete(&self) -> Result<(), InternalCompilerError> {
        node_call(
            "return import('node:fs/promises').then(fs => fs.rm(a0,{force:true}));",
            &[JsValue::from_str(&self.path.to_string_lossy())],
        )
        .await
        .map(|_| ())
    }
}

#[derive(Clone, Debug)]
pub struct LocalSourceDirectory {
    path: PathBuf,
}
impl LocalSourceDirectory {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}
impl From<PathBuf> for LocalSourceDirectory {
    fn from(path: PathBuf) -> Self {
        Self::new(path)
    }
}
impl InputDirectory for LocalSourceDirectory {
    type File = LocalInputFile;
    type Files = Vec<Result<Self::File, InternalCompilerError>>;
    async fn list_files(&self) -> Self::Files {
        let value = node_call("return Promise.all([import('node:fs/promises'),import('node:path')]).then(async ([fs,p]) => { const out=[]; async function walk(d){ for(const e of await fs.readdir(d,{withFileTypes:true})){const q=p.join(d,e.name); if(e.isDirectory()) await walk(q); else if(e.isFile() && q.endsWith('.argon')) out.push(q);} } await walk(a0); return out; });", &[JsValue::from_str(&self.path.to_string_lossy())]).await;
        match value {
            Ok(value) => Array::from(&value)
                .iter()
                .map(|value| {
                    value
                        .as_string()
                        .map(|path| LocalInputFile::new(path.into()))
                        .ok_or_else(|| {
                            InternalCompilerError::JavaScript(
                                "directory listing contained a non-string".to_string(),
                            )
                        })
                })
                .collect(),
            Err(err) => vec![Err(err)],
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocalOutputDirectory {
    path: PathBuf,
}
impl LocalOutputDirectory {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}
impl From<PathBuf> for LocalOutputDirectory {
    fn from(path: PathBuf) -> Self {
        Self::new(path)
    }
}
impl OutputDirectory for LocalOutputDirectory {
    type File = LocalOutputFile;
    fn create_file(&self, name: &str) -> Result<Self::File, InternalCompilerError> {
        Ok(LocalOutputFile::new(self.path.join(name)))
    }
}

pub struct StdIoWrite<W> {
    inner: W,
}
impl<W> StdIoWrite<W> {
    pub fn new(inner: W) -> Self {
        Self { inner }
    }
    pub fn into_inner(self) -> W {
        self.inner
    }
}
impl<W> ErrorType for StdIoWrite<W>
where
    W: std::io::Write,
{
    type Error = std::io::Error;
}
impl<W: std::io::Write> embedded_io::Write for StdIoWrite<W> {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        std::io::Write::write(&mut self.inner, buf)
    }
    fn flush(&mut self) -> Result<(), Self::Error> {
        std::io::Write::flush(&mut self.inner)
    }
}
