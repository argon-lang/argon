#![no_std]

#[cfg(feature = "std")]
extern crate std;

use argon_util::InternalCompilerError;
#[cfg(feature = "std")]
use embedded_io::Error;
use parse18_runtime::LocationFileView;
#[cfg(feature = "std")]
use std::{io, path::Path, string::ToString};

pub use embedded_io::{Read, Write};

pub trait InputFile {
    type Reader: Read<Error = InternalCompilerError>;

    #[cfg(feature = "std")]
    fn path(&self) -> &Path;

    #[cfg(feature = "std")]
    fn location_file(&self) -> &LocationFileView {
        self.path()
    }

    #[cfg(not(feature = "std"))]
    fn location_file(&self) -> &LocationFileView;

    fn open(&self) -> Result<Self::Reader, InternalCompilerError>;
}

pub trait InputDirectory {
    type File: InputFile;
    type Files: IntoIterator<Item = Result<Self::File, InternalCompilerError>>;

    fn list_files(&self) -> Self::Files;
}

pub trait OutputFile {
    type Writer: Write<Error = InternalCompilerError>;

    #[cfg(feature = "std")]
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

#[cfg(feature = "std")]
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

impl<R> esexpr_binary::io::Read<InternalCompilerError> for EmbeddedIoRead<R>
where
    R: Read<Error = InternalCompilerError>,
{
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, InternalCompilerError> {
        self.inner.read(buf)
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

#[cfg(feature = "std")]
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

impl<W> esexpr_binary::io::Write<InternalCompilerError> for EmbeddedIoWrite<W>
where
    W: Write<Error = InternalCompilerError>,
{
    fn write(&mut self, mut buf: &[u8]) -> Result<(), InternalCompilerError> {
        while !buf.is_empty() {
            let count = self.inner.write(buf)?;
            if count == 0 {
                return Err(InternalCompilerError::WriteZero);
            }
            buf = &buf[count..];
        }

        Ok(())
    }
}
