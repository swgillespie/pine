#![feature(hash)]

extern crate pine_typecheck;
#[macro_use]
extern crate log;

mod monomorphize;

pub use monomorphize::monomorphize;



