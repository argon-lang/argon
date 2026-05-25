use argon_util::InternalCompilerError;
use embedded_io::Error;
use std::io;
use std::path::Path;

pub use embedded_io::{Read, Write};

pub trait InputFile {
    type Reader: Read<Error = InternalCompilerError>;

    fn path(&self) -> &Path;
    fn open(&self) -> Result<Self::Reader, InternalCompilerError>;
}

pub trait InputDirectory {
    type File: InputFile;
    type Files: IntoIterator<Item = Result<Self::File, InternalCompilerError>>;

    fn list_files(&self) -> Self::Files;
}

pub trait OutputFile {
    type Writer: Write<Error = InternalCompilerError>;

    fn path(&self) -> &Path;
    fn open(&self) -> Result<Self::Writer, InternalCompilerError>;
    fn delete(&self) -> Result<(), InternalCompilerError>;
}

pub trait OutputDirectory {
    type File: OutputFile;

    fn create_file(&self, name: &str) -> Result<Self::File, InternalCompilerError>;
}

pub struct EmbeddedIoRead<R> {
    inner: R,
}

impl<R> EmbeddedIoRead<R> {
    pub fn new(inner: R) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> R {
        self.inner
    }
}

impl<R> io::Read for EmbeddedIoRead<R>
where
    R: Read<Error = InternalCompilerError>,
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.inner
            .read(buf)
            .map_err(|err| io::Error::new(io::ErrorKind::from(err.kind()), err.to_string()))
    }
}

pub struct EmbeddedIoWrite<W> {
    inner: W,
}

impl<W> EmbeddedIoWrite<W> {
    pub fn new(inner: W) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> W {
        self.inner
    }
}

impl<W> io::Write for EmbeddedIoWrite<W>
where
    W: Write<Error = InternalCompilerError>,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner
            .write(buf)
            .map_err(|err| io::Error::new(io::ErrorKind::from(err.kind()), err.to_string()))
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner
            .flush()
            .map_err(|err| io::Error::new(io::ErrorKind::from(err.kind()), err.to_string()))
    }
}
