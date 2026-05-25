use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_util::InternalCompilerError;
use embedded_io::{ErrorType, Read, Write};
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

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
    file: std::fs::File,
    path: PathBuf,
}

pub struct LocalFileWriter {
    file: std::fs::File,
    path: PathBuf,
}

impl ErrorType for LocalFileReader {
    type Error = InternalCompilerError;
}

impl Read for LocalFileReader {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        std::io::Read::read(&mut self.file, buf)
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl ErrorType for LocalFileWriter {
    type Error = InternalCompilerError;
}

impl Write for LocalFileWriter {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        std::io::Write::write(&mut self.file, buf)
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        std::io::Write::flush(&mut self.file)
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl InputFile for LocalInputFile {
    type Reader = LocalFileReader;

    fn path(&self) -> &Path {
        &self.path
    }

    fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
        std::fs::File::open(&self.path)
            .map(|file| LocalFileReader {
                file,
                path: self.path.clone(),
            })
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl InputDirectory for LocalSourceDirectory {
    type File = LocalInputFile;
    type Files = Vec<Result<Self::File, InternalCompilerError>>;

    fn list_files(&self) -> Self::Files {
        WalkDir::new(&self.path)
            .into_iter()
            .filter_map(|entry| match entry {
                Ok(entry) => {
                    if entry.file_type().is_file()
                        && entry.path().extension() == Some(OsStr::new("argon"))
                    {
                        Some(Ok(LocalInputFile {
                            path: entry.path().to_path_buf(),
                        }))
                    } else {
                        None
                    }
                }
                Err(err) => Some(Err(InternalCompilerError::WalkDirError(err))),
            })
            .collect()
    }
}

impl OutputFile for LocalOutputFile {
    type Writer = LocalFileWriter;

    fn path(&self) -> &Path {
        &self.path
    }

    fn open(&self) -> Result<Self::Writer, InternalCompilerError> {
        std::fs::File::create(&self.path)
            .map(|file| LocalFileWriter {
                file,
                path: self.path.clone(),
            })
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }

    fn delete(&self) -> Result<(), InternalCompilerError> {
        match std::fs::remove_file(&self.path) {
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
