#![warn(clippy::nursery)]
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::uninlined_format_args)]

mod completer;
mod context;
mod error;

pub use completer::KeywordsCompleter;
pub use context::{BindingShift, Context};
pub use error::{Error, Result};
pub use util_derive::RcTerm;
