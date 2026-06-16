use argon_util::InternalCompilerError;
use parse18_runtime::LocationFileView;
use std::path::Path;

pub use embedded_io::{Read, Write};

pub trait InputFile {
    type Reader: Read<Error = InternalCompilerError>;

    fn path(&self) -> &Path;

    fn location_file(&self) -> &LocationFileView {
        self.path()
    }

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
