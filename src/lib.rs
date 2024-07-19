mod r#macro;
mod parser;
mod utils;

pub use self::{
    parser::{Error, FormatSpecifier, PartParser},
    utils::check_unique,
};
