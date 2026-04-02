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
    #[error("Linear variable {0} has already been used")]
    LinearVariableUsed(String),
    #[error("Unused linear variable {0}")]
    UnusedLinearVariable(String),
    #[error("Ordered variable {0} and {1} used out of order")]
    OrderedVariableUsedOutOfOrder(String, String),
}

pub type Result<T, E = Error> = StdResult<T, E>;
