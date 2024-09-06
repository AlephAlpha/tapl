use std::result::Result as StdResult;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("No rule applies")]
    NoRuleApplies,
}

pub type Result<T, E = Error> = StdResult<T, E>;
