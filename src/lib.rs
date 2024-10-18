// #![feature(trace_macros)]

mod r#macro;
mod parser;
mod rule;
mod utils;

pub use self::{
    parser::{AtomParser, Error, FormatSpecifier, PartParser},
    rule::Rule,
    utils::check_unique,
};
