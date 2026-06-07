use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct WorkspacePaths {
    root: PathBuf,
}

impl WorkspacePaths {
    pub fn from_cargo_manifest_dir() -> Result<Self, std::env::VarError> {
        let mut root = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?);
        root.pop();

        Ok(Self { root })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn libraries_dir(&self) -> PathBuf {
        self.root.join("libraries")
    }

    pub fn backend_dir(&self) -> PathBuf {
        self.root.join("backend")
    }

    pub fn argon_bin(&self) -> PathBuf {
        self.root.join("dist/bin/argonc")
    }

    pub fn testcases_dir(&self) -> PathBuf {
        self.root.join("argon-testcases/testcases")
    }
}
