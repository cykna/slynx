use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub slynx, "/grammar/slynx.rs");
pub mod ast;
pub mod checker;
pub mod compiler;

pub mod hir;
pub mod intermediate;
