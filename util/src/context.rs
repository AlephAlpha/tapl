use crate::error::{Error, Result};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Context<T> {
    bindings: Vec<(String, T)>,
}

impl<T> Default for Context<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Context<T> {
    pub const fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    pub const fn len(&self) -> usize {
        self.bindings.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    pub fn add_binding(&mut self, name: impl Into<String>, data: T) {
        self.bindings.push((name.into(), data));
    }

    pub fn add_name(&mut self, name: impl Into<String>)
    where
        T: Default,
    {
        self.add_binding(name, T::default());
    }

    pub fn drop_binding(&mut self) {
        self.bindings.pop();
    }

    pub fn drop_linear_binding(&mut self) -> Result<()>
    where
        T: BindingLinear,
    {
        if let Some((name, b)) = self.bindings.pop()
            && b.is_unused_lin()
        {
            return Err(Error::UnusedLinearVariable(name));
        }
        Ok(())
    }

    pub fn with_binding<U>(
        &mut self,
        name: impl Into<String>,
        data: T,
        f: impl FnOnce(&mut Self) -> U,
    ) -> U {
        self.add_binding(name, data);
        let result = f(self);
        self.drop_binding();
        result
    }

    pub fn with_linear_binding<U>(
        &mut self,
        name: impl Into<String>,
        data: T,
        f: impl FnOnce(&mut Self) -> Result<U>,
    ) -> Result<U>
    where
        T: BindingLinear,
    {
        self.add_binding(name, data);
        match f(self) {
            Ok(result) => {
                self.drop_linear_binding()?;
                Ok(result)
            }
            Err(e) => {
                self.drop_binding();
                Err(e)
            }
        }
    }

    pub fn with_name<U>(&mut self, name: impl Into<String>, f: impl FnOnce(&mut Self) -> U) -> U
    where
        T: Default,
    {
        self.with_binding(name, T::default(), f)
    }

    pub fn is_name_bound(&self, name: &str) -> bool {
        self.bindings.iter().any(|(n, _)| n == name)
    }

    pub fn pick_fresh_name(&self, name: &str) -> String {
        let mut name = name.to_string();
        while self.is_name_bound(&name) {
            name.push('\'');
        }
        name
    }

    pub fn index_to_name(&self, i: usize) -> Result<&str> {
        self.bindings
            .iter()
            .rev()
            .nth(i)
            .map(|(n, _)| n.as_str())
            .ok_or_else(|| Error::VariableLookupFailure(i, self.len()))
    }

    pub fn name_to_index(&self, name: &str) -> Result<usize> {
        self.bindings
            .iter()
            .rev()
            .position(|(n, _)| n == name)
            .ok_or_else(|| Error::IdentifierUnbound(name.to_string()))
    }

    pub fn get_binding(&self, i: usize) -> Result<&T> {
        self.bindings
            .iter()
            .rev()
            .nth(i)
            .map(|(_, b)| b)
            .ok_or_else(|| Error::VariableLookupFailure(i, self.len()))
    }

    pub fn get_binding_shifting(&self, i: usize) -> Result<T>
    where
        T: BindingShift,
    {
        self.get_binding(i).and_then(|b| b.shift(i as isize + 1))
    }

    pub fn use_binding(&mut self, i: usize) -> Result<T>
    where
        T: BindingLinear + Clone,
    {
        if i >= self.len() {
            return Err(Error::VariableLookupFailure(i, self.len()));
        }

        let (name, b) = &self.bindings[self.len() - 1 - i];
        if b.is_used() {
            return Err(Error::LinearVariableUsed(name.clone()));
        }

        if b.is_ord()
            && let Some((name_, _)) = self.bindings.iter().rev().take(i).find(|(_, b)| b.is_ord())
        {
            return Err(Error::OrderedVariableUsedOutOfOrder(
                name.clone(),
                name_.clone(),
            ));
        }

        let b_ = b.clone();
        let len = self.len();
        self.bindings[len - 1 - i].1 = b_.used();
        Ok(b_)
    }

    pub fn use_binding_shifting(&mut self, i: usize) -> Result<T>
    where
        T: BindingLinear + BindingShift + Clone,
    {
        self.use_binding(i).and_then(|b| b.shift(i as isize + 1))
    }

    pub fn check_unused_lin(&self) -> Result<()>
    where
        T: BindingLinear,
    {
        if let Some((x, _)) = self.bindings.iter().find(|(_, b)| b.is_unused_lin()) {
            Err(Error::UnusedLinearVariable(x.clone()))
        } else {
            Ok(())
        }
    }
}

pub trait BindingShift: Sized {
    fn shift(&self, d: isize) -> Result<Self>;
}

// In linear types, we simply mark a variable as used,
// instead of removing it from the context,
// so that the de Bruijn indices are not affected.
pub trait BindingLinear: Sized {
    fn used(&self) -> Self;

    fn is_used(&self) -> bool;

    fn is_unused_lin(&self) -> bool;

    fn is_ord(&self) -> bool {
        false
    }
}
