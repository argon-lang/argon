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
        let executable_name = if cfg!(windows) {
            "argonc.exe"
        } else {
            "argonc"
        };

        self.root.join("dist").join("bin").join(executable_name)
    }

    pub fn testcases_dir(&self) -> PathBuf {
        self.root.join("argon-testcases/testcases")
    }
}
