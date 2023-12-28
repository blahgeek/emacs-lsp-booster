use std::collections::BTreeMap;

use serde_json as json;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum LispObject {
    Symbol(String),
    Keyword(String),
    Str(String),
    Int(i64),
    Float(String),  // use string for Eq and Ord
    Nil,
    T,
}

impl LispObject {
    fn to_repl(&self) -> String {
        match self {
            LispObject::Symbol(s) => s.clone(),
            LispObject::Keyword(s) => format!(":{}", s),
            // TODO: properly quote
            LispObject::Str(s) => format!("\"{}\"", s.replace("\"", "\\\"").replace("\\", "\\\\")),
            LispObject::Int(i) => i.to_string(),
            LispObject::Float(s) => s.clone(),
            LispObject::Nil => "nil".into(),
            LispObject::T => "t".into(),
        }
    }
}


// Only for generating json. Sequential execution only.
struct BytecodeBuilder {
    code: Vec<u8>,
    constants: BTreeMap<LispObject, u32>,

    current_stack_size: i32,
    max_stack_size: i32,
}

lazy_static! {
    static ref CONSTANT_FUNC_MAKE_VECTOR: LispObject = LispObject::Symbol("make-vector".to_string());
}

impl BytecodeBuilder {
    fn create_or_get_constant(&mut self, obj: LispObject) -> u32 {
        if let Some(&idx) = self.constants.get(&obj) {
            return idx;
        }
        let next_id = self.constants.len() as u32;
        self.constants.insert(obj, next_id);
        return next_id;
    }

    fn add_opcode<const N: usize>(&mut self, opcode: [u8; N], stack_delta: i32) {
        assert!(self.current_stack_size + stack_delta >= 0);
        self.current_stack_size += stack_delta;
        self.max_stack_size = self.max_stack_size.max(self.current_stack_size);
        self.code.extend(&opcode);
    }

    fn add_opcode_constant(&mut self, obj: LispObject) {
        let idx = self.create_or_get_constant(obj);
        if idx < 64 {
            self.add_opcode([(192 + idx) as u8], 1);
        } else if idx < (2<<16) {
            // constant2
            self.add_opcode([129, (idx & 0xff) as u8, (idx >> 8) as u8], 1);
        } else {
            unimplemented!();
        }
    }

    fn add_opcode_call(&mut self, n_args: u16) {
        // https://github.com/rocky/elisp-bytecode/issues/79
        let delta = -(n_args as i32 + 1) + 1;
        if n_args <= 5 {
            self.add_opcode([(32 + n_args) as u8], delta);
        } else if n_args < (1 << 8) {
            self.add_opcode([(32 + 6), n_args as u8], delta);
        } else {
            self.add_opcode([(32 + 7), (n_args & 0xff) as u8, (n_args >> 8) as u8], delta);
        }
    }

    fn build_one_value_array(&mut self, arr: &[json::Value]) {
        if arr.len() < (1 << 16) {
            // use "vector" call
            self.add_opcode_constant(LispObject::Symbol("vector".into()));
            for value in arr {
                self.build_one_value(value);
            }
            self.add_opcode_call(arr.len() as u16);
        } else {
            // fallback to make-vector & aset
            self.add_opcode_constant(CONSTANT_FUNC_MAKE_VECTOR.clone());
            self.add_opcode_constant(LispObject::Int(arr.len() as i64));
            self.add_opcode_constant(LispObject::Nil);
            self.add_opcode([32 + 2], -3 + 1);  // call

            self.add_opcode_constant(LispObject::Int(0)); // index for aset

            for value in arr {
                self.add_opcode([1], 1);  // stack ref 1, the vector
                self.add_opcode([1], 1);  // stack ref 1, the index
                self.build_one_value(value);
                self.add_opcode([73], -3+1);  // aset
                self.add_opcode([136], -1);   // discard aset result
                self.add_opcode([84], 0);     // add1
            }
            self.add_opcode([136], -1);   // discard index
            // the vector remains
        }
    }

    fn build_one_value_map(&mut self, map: &json::Map<String, json::Value>) {
        // list
        let list_len = map.len() * 2;
        let use_list_call = list_len < (1 << 16);
        if use_list_call {
            self.add_opcode_constant(LispObject::Symbol("list".into()));
        }

        for (key, value) in map {
            self.add_opcode_constant(LispObject::Keyword(key.clone()));
            self.build_one_value(value);
        }

        if use_list_call {
            self.add_opcode_call(list_len as u16);
        } else {
            self.add_opcode_constant(LispObject::Nil);
            for _ in 0..list_len {
                self.add_opcode([66], -2 + 1);  // cons
            }
        }
    }

    // current only support:
    // object-type: plist,  null-object: nil,  false-object: nil,  array-type: vector
    fn build_one_value(&mut self, value: &json::Value) {
        match value {
            &json::Value::Null | &json::Value::Bool(false) => {
                self.add_opcode_constant(LispObject::Nil);
            },
            &json::Value::Bool(true) => {
                self.add_opcode_constant(LispObject::T);
            },
            &json::Value::Number(ref num) => {
                if num.is_f64() {
                    self.add_opcode_constant(LispObject::Float(num.to_string()));
                } else {
                    self.add_opcode_constant(LispObject::Int(num.as_i64().unwrap()));
                }
            },
            &json::Value::String(ref s) => {
                self.add_opcode_constant(LispObject::Str(s.clone()));
            },
            &json::Value::Array(ref arr) => {
                self.build_one_value_array(&arr);
            },
            &json::Value::Object(ref map) => {
                self.build_one_value_map(&map);
            },
        }
    }

    fn build(&mut self, value: &json::Value) {
        self.build_one_value(value);
        self.add_opcode([135], -1);   // return
    }

    fn into_repl(self) -> String {
        let mut result: String = "#[0 \"".into();
        for c in self.code {
            result.push_str(&format!("\\{:o}", c));
        }
        result += "\" [";

        let mut constants_array = self.constants.into_iter().collect::<Vec<_>>();
        constants_array.sort_by_key(|(_, idx)| *idx);
        result += &constants_array.into_iter()
            .map(|(obj, _)| obj.to_repl())
            .collect::<Vec<_>>().join(" ");

        result += &format!("] {}]", self.max_stack_size);
        return result;
    }
}

pub fn generate_bytecode_repl(value: &json::Value) -> String {
    let mut builder = BytecodeBuilder {
        code: Vec::new(),
        constants: BTreeMap::new(),
        current_stack_size: 0,
        max_stack_size: 0,
    };
    builder.build(value);
    return builder.into_repl();
}
