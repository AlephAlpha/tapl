use rustyline::{completion::Completer, Context, Helper, Highlighter, Hinter, Result, Validator};
use std::collections::BTreeSet;

/// A rustyline completer that completes based on a list of keywords.
///
/// Commands are also suggested when the cursor is at the beginning of the line.
#[derive(Helper, Highlighter, Hinter, Validator)]
pub struct KeywordsCompleter {
    keywords: BTreeSet<String>,
    commands: BTreeSet<String>,
}

impl KeywordsCompleter {
    pub fn new(
        keywords: impl IntoIterator<Item = impl Into<String>>,
        commands: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Self {
            keywords: keywords.into_iter().map(Into::into).collect(),
            commands: commands.into_iter().map(Into::into).collect(),
        }
    }

    pub fn add_keyword(&mut self, keyword: impl Into<String>) {
        self.keywords.insert(keyword.into());
    }

    pub fn remove_keyword(&mut self, keyword: impl AsRef<str>) {
        self.keywords.retain(|k| k != keyword.as_ref());
    }
}

impl Completer for KeywordsCompleter {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context) -> Result<(usize, Vec<String>)> {
        let mut start = line[..pos]
            .rfind(|c: char| !c.is_alphanumeric())
            .map_or(0, |i| i + 1);
        let mut prefix: &str = &line[start..pos];

        if prefix.starts_with(|c: char| !c.is_alphabetic()) {
            start += prefix.find(char::is_alphabetic).unwrap_or(prefix.len());
            prefix = &line[start..pos];
        }

        let mut completions = self
            .keywords
            .iter()
            .filter(|k| k.starts_with(prefix))
            .cloned()
            .collect::<Vec<_>>();

        if pos == 0 {
            completions.extend(self.commands.iter().map(|c| format!(":{c}")));
        }

        if start == 1 && line.starts_with(':') {
            completions.extend(
                self.commands
                    .iter()
                    .filter(|c| c.starts_with(prefix))
                    .cloned(),
            );
        }

        Ok((start, completions))
    }
}
