use std::collections::HashMap;

use ulc_types::ULCType;

use crate::FuncData;

pub fn create_std() -> HashMap<String, FuncData> {
    vec![(
        "puts".to_owned(),
        FuncData {
            ident: ("puts".to_owned(), None),
            ret_ty: ULCType::Unit,
            param_tys: vec![(ULCType::String, None)],
        },
    )]
    .into_iter()
    .collect()
}
