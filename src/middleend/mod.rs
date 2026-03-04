pub mod hir;
pub mod intermediate;
pub use hir::{DeclarationId, declarations, error, id, names, symbols, types};
pub use intermediate::*;
