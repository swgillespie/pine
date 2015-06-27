#![feature(hash)]
extern crate llvm_sys;
extern crate pine_typecheck;
#[macro_use]
extern crate log;

mod monomorphize;
mod llvm_trans;

pub use monomorphize::monomorphize;
pub use llvm_trans::{translate, TranslatedModule};
