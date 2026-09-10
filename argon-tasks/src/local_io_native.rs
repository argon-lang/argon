use alloc::{vec, vec::Vec};
use argon_io::{InputDirectory, InputFile, InputStream, OutputDirectory, OutputFile, OutputStream};
use argon_util::InternalCompilerError;
use embedded_io::{ErrorType, Write};
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufReader, BufWriter};

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

#[derive(Clone, Debug)]
pub struct LocalOutputDirectory {
    path: PathBuf,
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

impl<W> ErrorType for StdIoWrite<W> {
    type Error = std::io::Error;
}

impl<W> Write for StdIoWrite<W>
where
    W: std::io::Write,
{
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        std::io::Write::write(&mut self.inner, buf)
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        std::io::Write::flush(&mut self.inner)
    }
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

pub struct LocalFileReader {
    file: BufReader<tokio::fs::File>,
    path: PathBuf,
}

pub struct LocalFileWriter {
    file: BufWriter<tokio::fs::File>,
    path: PathBuf,
}

impl ErrorType for LocalFileReader {
    type Error = InternalCompilerError;
}

impl argon_io::Read for LocalFileReader {
    async fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        self.file
            .read(buf)
            .await
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}
impl InputStream for LocalFileReader {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        Ok(())
    }
}

impl ErrorType for LocalFileWriter {
    type Error = InternalCompilerError;
}

impl argon_io::Write for LocalFileWriter {
    async fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        self.file
            .write(buf)
            .await
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }

    async fn flush(&mut self) -> Result<(), Self::Error> {
        self.file
            .flush()
            .await
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}
impl OutputStream for LocalFileWriter {
    async fn close(&mut self) -> Result<(), InternalCompilerError> {
        self.file
            .shutdown()
            .await
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl InputFile for LocalInputFile {
    type Reader = LocalFileReader;

    fn path(&self) -> &Path {
        &self.path
    }

    async fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
        tokio::fs::File::open(&self.path)
            .await
            .map(|file| LocalFileReader {
                file: BufReader::new(file),
                path: self.path.clone(),
            })
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl InputDirectory for LocalSourceDirectory {
    type File = LocalInputFile;
    type Files = Vec<Result<Self::File, InternalCompilerError>>;

    async fn list_files(&self) -> Self::Files {
        let mut results = Vec::new();
        let mut pending = vec![self.path.clone()];
        while let Some(path) = pending.pop() {
            let mut entries = match tokio::fs::read_dir(&path).await {
                Ok(entries) => entries,
                Err(err) => {
                    results.push(Err(InternalCompilerError::IoError(path, err)));
                    continue;
                }
            };
            loop {
                let entry = match entries.next_entry().await {
                    Ok(Some(entry)) => entry,
                    Ok(None) => break,
                    Err(err) => {
                        results.push(Err(InternalCompilerError::IoError(path.clone(), err)));
                        break;
                    }
                };
                let entry_path = entry.path();
                match entry.file_type().await {
                    Ok(kind) if kind.is_dir() => pending.push(entry_path),
                    Ok(kind)
                        if kind.is_file()
                            && entry_path.extension() == Some(OsStr::new("argon")) =>
                    {
                        results.push(Ok(LocalInputFile { path: entry_path }))
                    }
                    Ok(_) => {}
                    Err(err) => results.push(Err(InternalCompilerError::IoError(entry_path, err))),
                }
            }
        }
        results
    }
}

impl OutputFile for LocalOutputFile {
    type Writer = LocalFileWriter;

    fn path(&self) -> &Path {
        &self.path
    }

    async fn open(&self) -> Result<Self::Writer, InternalCompilerError> {
        if let Some(parent) = self.path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(|err| InternalCompilerError::IoError(parent.to_path_buf(), err))?;
        }
        tokio::fs::File::create(&self.path)
            .await
            .map(|file| LocalFileWriter {
                file: BufWriter::new(file),
                path: self.path.clone(),
            })
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }

    async fn delete(&self) -> Result<(), InternalCompilerError> {
        match tokio::fs::remove_file(&self.path).await {
            Ok(()) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(err) => Err(InternalCompilerError::IoError(self.path.clone(), err)),
        }
    }
}

impl OutputDirectory for LocalOutputDirectory {
    type File = LocalOutputFile;

    fn create_file(&self, name: &str) -> Result<Self::File, InternalCompilerError> {
        Ok(LocalOutputFile {
            path: self.path.join(name),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_io::{InputDirectory, InputFile, InputStream, OutputFile, OutputStream, Read, Write};

    #[tokio::test]
    async fn local_files_are_async_and_deletable() {
        let temp = tempfile::tempdir().expect("create temp directory");
        let path = temp.path().join("nested/data.bin");
        let output = LocalOutputFile::new(path.clone());
        let mut writer = output.open().await.expect("open output");
        writer.write_all(b"argon").await.expect("write output");
        writer.flush().await.expect("flush output");
        writer.close().await.expect("close output");

        let mut reader = LocalInputFile::new(path.clone())
            .open()
            .await
            .expect("open input");
        let mut bytes = [0; 5];
        reader.read_exact(&mut bytes).await.expect("read input");
        reader.close().await.expect("close input");
        assert_eq!(&bytes, b"argon");

        output.delete().await.expect("delete output");
        assert!(!path.exists());
    }

    #[tokio::test]
    async fn source_listing_is_recursive_and_filtered() {
        let temp = tempfile::tempdir().expect("create temp directory");
        tokio::fs::create_dir_all(temp.path().join("nested"))
            .await
            .expect("create nested directory");
        tokio::fs::write(temp.path().join("root.argon"), b"")
            .await
            .expect("write source");
        tokio::fs::write(temp.path().join("nested/child.argon"), b"")
            .await
            .expect("write source");
        tokio::fs::write(temp.path().join("nested/ignored.txt"), b"")
            .await
            .expect("write ignored file");
        let files = LocalSourceDirectory::new(temp.path().to_path_buf())
            .list_files()
            .await;
        assert_eq!(
            files
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .expect("list sources")
                .len(),
            2
        );
    }
}
