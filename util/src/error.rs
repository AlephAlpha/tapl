use std::result::Result as StdResult;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Identifier {0} is unbound")]
    IdentifierUnbound(String),
    #[error("Variable lookup failure: offset: {0}, ctx size: {1}")]
    VariableLookupFailure(usize, usize),
    #[error("No rule applies")]
    NoRuleApplies,
    #[error("Type of binding does not match declared type")]
    TypeMismatch,
    #[error("Scoping error")]
    ScopingError,
    #[error("{0}")]
    TypeError(String),
    #[error("{0}")]
    UnificationError(String),
    #[error("{0}")]
    KindError(String),
}

pub type Result<T, E = Error> = StdResult<T, E>;
