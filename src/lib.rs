#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
#![feature(min_const_generics)]

mod binary;
mod embedding;
mod execution;
mod interpretation;
mod leb128;
mod lexer;
mod module;
mod parser;
mod script;
mod structure;
mod token;
mod validation;

pub use embedding::*;
