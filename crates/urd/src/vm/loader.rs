//! # File Loader Module
//!
//! Defines the [`FileLoader`] trait that consumers implement to resolve
//! module paths used in `import "path/to/file" as alias` statements.
//!
//! The loader is consulted at compile time (not at runtime) — the entire
//! multi-file graph is merged into a single [`crate::ir::IrGraph`] before
//! the VM even starts.

/// Abstraction for resolving and loading Urd source files by path.
///
/// Consumers implement this trait to integrate module loading with their
/// environment (e.g. Godot's `res://` or `uid://` paths, native filesystem
/// paths, or in-memory maps for testing).
///
/// # Example
///
/// ```rust
/// use urd::vm::loader::FileLoader;
///
/// #[derive(Debug)]
/// struct FsLoader {
///     base_dir: std::path::PathBuf,
/// }
///
/// impl FileLoader for FsLoader {
///     fn load(&self, path: &str) -> Result<String, String> {
///         let full = self.base_dir.join(path);
///         std::fs::read_to_string(&full)
///             .map_err(|e| format!("cannot load '{}': {}", path, e))
///     }
/// }
/// ```
pub trait FileLoader: std::fmt::Debug {
    /// Load the source text at `path`.
    ///
    /// Returns the raw Urd source as a `String` on success, or a human-readable
    /// error message on failure.
    ///
    /// The `path` value is exactly the string literal written in the `import`
    /// statement, unmodified.
    fn load(&self, path: &str) -> Result<String, String>;
}

/// A [`FileLoader`] backed by the native filesystem.
///
/// Paths are resolved relative to `base_dir`. Useful for standalone tools,
/// tests, and any environment where the native FS is available.
#[derive(Debug)]
pub struct FsLoader {
    /// The root directory used when resolving relative import paths.
    pub base_dir: std::path::PathBuf,
}

impl FsLoader {
    /// Creates an `FsLoader` rooted at `base_dir`.
    pub fn new(base_dir: impl Into<std::path::PathBuf>) -> Self {
        FsLoader {
            base_dir: base_dir.into(),
        }
    }
}

impl FileLoader for FsLoader {
    fn load(&self, path: &str) -> Result<String, String> {
        let full = self.base_dir.join(path);
        std::fs::read_to_string(&full).map_err(|e| format!("cannot load '{}': {}", path, e))
    }
}

/// A [`FileLoader`] backed by an in-memory map, useful for unit tests.
///
/// Keys are module paths (e.g. `"lib/helpers.urd"`), values are raw source strings.
#[derive(Debug, Default, Clone)]
pub struct MemLoader {
    files: std::collections::HashMap<String, String>,
}

impl MemLoader {
    /// Creates an empty `MemLoader`.
    pub fn new() -> Self {
        MemLoader::default()
    }

    /// Registers a source string under `path`.
    pub fn add(&mut self, path: impl Into<String>, source: impl Into<String>) -> &mut Self {
        self.files.insert(path.into(), source.into());
        self
    }
}

impl FileLoader for MemLoader {
    fn load(&self, path: &str) -> Result<String, String> {
        self.files
            .get(path)
            .cloned()
            .ok_or_else(|| format!("module '{}' not found in MemLoader", path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mem_loader_add_and_load() {
        let mut loader = MemLoader::new();
        loader.add("foo.urd", "label start { }");
        assert_eq!(loader.load("foo.urd"), Ok("label start { }".to_string()));
    }

    #[test]
    fn mem_loader_missing_key_returns_error() {
        let loader = MemLoader::new();
        assert!(loader.load("missing.urd").is_err());
    }

    #[test]
    fn mem_loader_method_chaining() {
        let mut loader = MemLoader::new();
        loader.add("a.urd", "").add("b.urd", "");
        assert!(loader.load("a.urd").is_ok());
        assert!(loader.load("b.urd").is_ok());
    }

    #[test]
    fn mem_loader_error_message_contains_path() {
        let loader = MemLoader::new();
        let err = loader.load("some/missing/module.urd").unwrap_err();
        assert!(
            err.contains("some/missing/module.urd"),
            "error message should contain the requested path, got: {err}"
        );
    }

    #[test]
    fn mem_loader_overwrite_existing_key() {
        let mut loader = MemLoader::new();
        loader.add("x.urd", "version one");
        loader.add("x.urd", "version two");
        assert_eq!(loader.load("x.urd"), Ok("version two".to_string()));
    }

    #[test]
    fn fs_loader_new_stores_base_dir() {
        let loader = FsLoader::new("/tmp/scripts");
        assert_eq!(loader.base_dir, std::path::PathBuf::from("/tmp/scripts"));
    }

    #[test]
    fn fs_loader_missing_file_returns_error() {
        let loader = FsLoader::new("/nonexistent_dir_that_cannot_exist_xyzzy");
        let result = loader.load("module.urd");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.contains("module.urd"),
            "error message should contain the path, got: {err}"
        );
    }

    #[test]
    fn fs_loader_reads_existing_file() {
        use std::io::Write;

        let dir = std::env::temp_dir();
        let file_path = dir.join("urd_test_loader_reads_existing.urd");
        let content = "label hello { }";

        {
            let mut f = std::fs::File::create(&file_path).expect("create temp file");
            write!(f, "{}", content).expect("write temp file");
        }

        let loader = FsLoader::new(&dir);
        let result = loader.load("urd_test_loader_reads_existing.urd");

        // Clean up before asserting so the file is removed even on failure.
        let _ = std::fs::remove_file(&file_path);

        assert_eq!(result, Ok(content.to_string()));
    }
}
