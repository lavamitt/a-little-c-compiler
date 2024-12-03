use std::collections::HashMap;
pub struct HelperFunctions {
    pub(crate) tmp_register_counter: u32,
    pub(crate) label_counter: u32,
}

impl HelperFunctions {
    pub fn make_temporary_register(&mut self) -> String {
        let new_temporary_register = format!("tmp.{}", self.tmp_register_counter.to_string());
        self.tmp_register_counter += 1;
        new_temporary_register
    }

    pub fn make_labels_at_same_counter(&mut self, prefixes: Vec<String>) -> Vec<String> {
        let mut new_labels: Vec<String> = Vec::new();
        for prefix in prefixes {
            new_labels.push(format!("{}.{}", prefix, self.label_counter.to_string()))
        }
        self.label_counter += 1;
        new_labels
    }
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Int,
    Func(u32, bool) // number of params, defined
}

pub struct CompilerContext {
    pub helper: HelperFunctions,
    pub symbol_table: HashMap<String, SymbolType>
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            helper: HelperFunctions {
                tmp_register_counter: 0,
                label_counter: 0,
            },
            symbol_table: HashMap::new()
        }
    }
}