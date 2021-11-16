use ulc_types::{
    errors::{FiledError, SyntaxError},
    Filed, Spanned,
};

pub struct CheckerContext<'input> {
    defined: Vec<Filed<'input, Spanned<&'input str>>>,
    used: Vec<Filed<'input, Spanned<&'input str>>>,
}

impl<'input> Default for CheckerContext<'input> {
    fn default() -> Self {
        Self {
            defined: Default::default(),
            used: Default::default(),
        }
    }
}

impl<'input> CheckerContext<'input> {
    pub fn add_defined(&mut self, defined_ident: Filed<'input, Spanned<&'input str>>) {
        self.defined.push(defined_ident)
    }

    pub fn add_used(&mut self, used_ident: Filed<'input, Spanned<&'input str>>) {
        self.used.push(used_ident)
    }

    pub fn check(&self) -> anyhow::Result<(), FiledError<'input>> {
        for used_token in &self.used {
            if !self.defined.contains(used_token) {
                return Err(FiledError {
                    filename: used_token.filename,
                    contents: used_token.contents,
                    error: SyntaxError::InvalidIdent(Spanned {
                        node: used_token.node.node.to_owned(),
                        span: used_token.node.span,
                    }),
                });
            }
        }
        Ok(())
    }
}
