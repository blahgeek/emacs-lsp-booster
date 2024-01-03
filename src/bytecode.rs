use std::collections::BTreeMap;

use anyhow::{Result, bail};
use serde_json as json;
use smallvec::smallvec;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LispObject {
    Symbol(String),
    Keyword(String),
    Str(String),
    Int(i64),
    Float(String),  // use string for Eq and Ord
    Nil,
    T,
    Vector(Vec<LispObject>),
}

impl LispObject {
    fn to_repl(&self) -> String {
        match self {
            LispObject::Symbol(s) => s.clone(),
            LispObject::Keyword(s) => format!(":{}", s),
            LispObject::Str(s) => {
                let mut result = String::new();
                result.push('"');
                let mut last_is_escape = false;
                for c in s.chars() {
                    if c == '"' || c == '\\' {
                        result.push('\\');
                        result.push(c);
                        last_is_escape = false;
                    } else if (c as u32) < 32 || ((c as u32) > 126 && (c as u32) < 256) {
                        result += &format!("\\{:o}", c as u32);
                        last_is_escape = true;
                    } else {
                        // https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
                        if last_is_escape && ('0'..='8').contains(&c) {
                            result += "\\ ";
                        }
                        result.push(c);
                        last_is_escape = false;
                    }
                }
                result.push('"');
                result
            },
            LispObject::Int(i) => i.to_string(),
            LispObject::Float(s) => s.clone(),
            LispObject::Nil => "nil".into(),
            LispObject::T => "t".into(),
            LispObject::Vector(v) =>
                format!("[{}]", v.iter().map(|x| x.to_repl()).collect::<Vec<_>>().join(" "))
        }
    }
}

// to support constants more than 65536 elements:
// - for first 63536 slots, use it as normal
// - in 63536-64536, put numbers 0-1000, for indexing
// - for last 1000 slots (64536-65536), use two-level vector, 1000*1000, each is a 1000-element vector

// cv: constant vector
const CV_TWO_LEVEL_VECTOR_SIZE: u32 = 1000;
const CV_NORMAL_SLOT_COUNT: u32 = (1 << 16) - CV_TWO_LEVEL_VECTOR_SIZE * 2;
const CV_TWO_LEVEL_IDX_BEGIN: u32 = CV_NORMAL_SLOT_COUNT;
const CV_TWO_LEVEL_DATA_BEGIN: u32 = CV_NORMAL_SLOT_COUNT + CV_TWO_LEVEL_VECTOR_SIZE;

enum Op {
    PushConstant(u32),  // support more than u16, will expand to multiple ops
    Call(u16),
    StackRef(u16),
    List(u8),
    Discard,
    ASet,
    Add1,
    Cons,
    Return,
}

impl Op {
    fn get_stack_delta(&self) -> i32 {
        match self {
            &Self::PushConstant(_) => 1,
            &Self::Call(n) => -(n as i32 + 1) + 1,
            &Self::StackRef(_) => 1,
            &Self::List(n) => -(n as i32) + 1,
            &Self::Discard => -1,
            &Self::ASet => -3 + 1,
            &Self::Add1 => 0,
            &Self::Cons => -2 + 1,
            &Self::Return => -1,
        }
    }

    fn to_code(&self) -> Result<smallvec::SmallVec<[u8; 3]>> {
        match self {
            &Self::PushConstant(v) if v < 64 =>
                Ok(smallvec![(192 + v) as u8]),
            &Self::PushConstant(v) if v < CV_NORMAL_SLOT_COUNT =>
                Ok(smallvec![129, (v & 0xff) as u8, (v >> 8) as u8]),
            &Self::PushConstant(v) if v < (CV_NORMAL_SLOT_COUNT + CV_TWO_LEVEL_VECTOR_SIZE * CV_TWO_LEVEL_VECTOR_SIZE) => {
                let mut result = smallvec![];
                let two_level_i = (v - CV_NORMAL_SLOT_COUNT) / CV_TWO_LEVEL_VECTOR_SIZE;
                let two_level_j = (v - CV_NORMAL_SLOT_COUNT) % CV_TWO_LEVEL_VECTOR_SIZE;

                // get vector
                let index_for_i = two_level_i + CV_TWO_LEVEL_DATA_BEGIN;
                result.extend_from_slice(&[129, (index_for_i & 0xff) as u8, (index_for_i >> 8) as u8]);
                // get index
                let index_for_j = two_level_j + CV_TWO_LEVEL_IDX_BEGIN;
                result.extend_from_slice(&[129, (index_for_j & 0xff) as u8, (index_for_j >> 8) as u8]);
                // aref
                result.push(72);

                Ok(result)
            },
            &Self::PushConstant(v) =>
                bail!("Too many constants! {}", v),
            &Self::Call(v) if v <= 5 =>
                Ok(smallvec![(32 + v) as u8]),
            &Self::Call(v) if v < (1 << 8) =>
                Ok(smallvec![(32 + 6) as u8, v as u8]),
            &Self::Call(v) =>
                Ok(smallvec![(32 + 7) as u8, (v & 0xff) as u8, (v >> 8) as u8]),
            &Self::StackRef(v) if (1..=4).contains(&v) =>
                Ok(smallvec![v as u8]),
            &Self::StackRef(_) => unimplemented!(),
            &Self::List(v) if v == 0 => unreachable!(),
            &Self::List(v) if (1..=4).contains(&v) => Ok(smallvec![66 + v]),
            &Self::List(v) => Ok(smallvec![175, v]),
            &Self::Discard => Ok(smallvec![136]),
            &Self::ASet => Ok(smallvec![73]),
            &Self::Add1 => Ok(smallvec![84]),
            &Self::Cons => Ok(smallvec![66]),
            &Self::Return => Ok(smallvec![135]),
        }
    }
}

#[derive(Clone, Copy)]
pub enum ObjectType {
    Plist,
    Hashtable,
    Alist,
}

pub struct BytecodeOptions {
    pub object_type: ObjectType,
    // TODO: array_type
    pub null_object: LispObject,
    pub false_object: LispObject,
}

impl Default for BytecodeOptions {
    fn default() -> Self {
        Self {
            object_type: ObjectType::Plist,
            null_object: LispObject::Nil,
            false_object: LispObject::Nil,
        }
    }
}

// Only for generating json. Sequential execution only.
struct BytecodeCompiler {
    options: BytecodeOptions,

    ops: Vec<Op>,
    constants: BTreeMap<LispObject, (u32, u32)>,  // (index, count)
}

impl BytecodeCompiler {
    fn compile_constant_op(&mut self, obj: LispObject) {
        let idx = {
            if let Some(idx_and_count) = self.constants.get_mut(&obj) {
                idx_and_count.1 += 1;
                idx_and_count.0
            } else {
                let next_id = self.constants.len() as u32;
                self.constants.insert(obj, (next_id, 1));
                next_id
            }
        };
        self.ops.push(Op::PushConstant(idx))
    }

    fn compile_value_array(&mut self, arr: &[json::Value]) {
        if arr.len() < (1 << 16) {
            // use "vector" call
            self.compile_constant_op(LispObject::Symbol("vector".into()));
            for value in arr {
                self.compile_value(value);
            }
            self.ops.push(Op::Call(arr.len() as u16));
        } else {
            // fallback to make-vector & aset
            self.compile_constant_op(LispObject::Symbol("make-vector".into()));
            self.compile_constant_op(LispObject::Int(arr.len() as i64));
            self.compile_constant_op(LispObject::Nil);
            self.ops.push(Op::Call(2));

            self.compile_constant_op(LispObject::Int(0)); // index for aset

            for value in arr {
                self.ops.push(Op::StackRef(1));  // the vector
                self.ops.push(Op::StackRef(1));  // the index
                self.compile_value(value);
                self.ops.push(Op::ASet);
                self.ops.push(Op::Discard);  // discard aset result
                self.ops.push(Op::Add1);
            }
            self.ops.push(Op::Discard);   // discard index
            // the vector remains
        }
    }

    fn compile_value_map_plist_or_alist(&mut self, map: &json::Map<String, json::Value>, alist: bool) {
        let list_len = if alist { map.len() } else { map.len() * 2 };
        // see below
        if list_len < (1 << 16) && list_len >= (1 << 8) {
            self.compile_constant_op(LispObject::Symbol("list".into()));
        }

        for (key, value) in map {
            if alist {
                self.compile_constant_op(LispObject::Symbol(key.clone()));
                self.compile_value(value);
                self.ops.push(Op::Cons);
            } else {
                self.compile_constant_op(LispObject::Keyword(key.clone()));
                self.compile_value(value);
            }
        }

        // four modes: 0. (empty) just nil 1. list op; 2. list call; 3. recursive cons
        if list_len == 0 {
            self.compile_constant_op(LispObject::Nil);
        } else if list_len < (1 << 8) {
            self.ops.push(Op::List(list_len as u8));
        } else if list_len < (1 << 16) {
            self.ops.push(Op::Call(list_len as u16));
        } else {
            self.compile_constant_op(LispObject::Nil);
            for _ in 0..list_len {
                self.ops.push(Op::Cons);
            }
        }
    }

    fn compile_value_map_hashtable(&mut self, map: &json::Map<String, json::Value>) {
        self.compile_constant_op(LispObject::Symbol("make-hash-table".into()));
        self.compile_constant_op(LispObject::Keyword("test".into()));
        self.compile_constant_op(LispObject::Symbol("equal".into()));
        self.compile_constant_op(LispObject::Keyword("size".into()));
        self.compile_constant_op(LispObject::Int(map.len() as i64));
        self.ops.push(Op::Call(4));

        for (key, value) in map {
            self.compile_constant_op(LispObject::Symbol("puthash".into()));
            self.compile_constant_op(LispObject::Str(key.clone()));
            self.compile_value(value);
            self.ops.push(Op::StackRef(3));
            self.ops.push(Op::Call(3));
            self.ops.push(Op::Discard);
        }
    }

    fn compile_value(&mut self, value: &json::Value) {
        match value {
            &json::Value::Null => {
                self.compile_constant_op(self.options.null_object.clone());
            },
            &json::Value::Bool(false) => {
                self.compile_constant_op(self.options.false_object.clone());
            },
            &json::Value::Bool(true) => {
                self.compile_constant_op(LispObject::T);
            },
            &json::Value::Number(ref num) => {
                if num.is_f64() {
                    self.compile_constant_op(LispObject::Float(num.to_string()));
                } else {
                    self.compile_constant_op(LispObject::Int(num.as_i64().unwrap()));
                }
            },
            &json::Value::String(ref s) => {
                self.compile_constant_op(LispObject::Str(s.clone()));
            },
            &json::Value::Array(ref arr) => {
                self.compile_value_array(&arr);
            },
            &json::Value::Object(ref map) => {
                match self.options.object_type {
                    ObjectType::Plist => self.compile_value_map_plist_or_alist(&map, false),
                    ObjectType::Alist => self.compile_value_map_plist_or_alist(&map, true),
                    ObjectType::Hashtable => self.compile_value_map_hashtable(&map),
                };
            },
        }
    }

    fn compile(&mut self, value: &json::Value) {
        self.compile_value(value);
        self.ops.push(Op::Return);
    }

    // return (code, constants, max_stack_size)
    fn into_bytecode(self) -> Result<(Vec<u8>, Vec<LispObject>, i32)> {
        // optimize constants vector, sort by usage
        let mut constants_array = self.constants.into_iter().collect::<Vec<_>>();
        constants_array.sort_by_key(
            // if count is same, still sort by the old idx, to increase locality
            |(_, idx_and_count)| (-(idx_and_count.1 as i32), idx_and_count.0));
        let index_remap = constants_array.iter().enumerate()
            .map(|(new_idx, (_, idx_and_count))| (idx_and_count.0, new_idx as u32))
            .collect::<BTreeMap<_, _>>();

        let mut constants_array = constants_array.into_iter().map(|(obj, _)| obj).collect::<Vec<_>>();
        // rearrange constants
        let mut two_level_vectors: Vec<LispObject> = Vec::new();
        // collect two level vectors from the end (reverse order)
        while constants_array.len() > CV_NORMAL_SLOT_COUNT as usize {
            let len = {
                let remaining = (constants_array.len() - CV_NORMAL_SLOT_COUNT as usize) % CV_TWO_LEVEL_VECTOR_SIZE as usize;
                if remaining == 0 {
                    CV_TWO_LEVEL_VECTOR_SIZE as usize
                } else {
                    remaining
                }
            };
            let v = constants_array
                .splice((constants_array.len() - len)..constants_array.len(), Vec::new())
                .collect();
            two_level_vectors.push(LispObject::Vector(v));
        }
        two_level_vectors.reverse();

        if !two_level_vectors.is_empty() {
            debug_assert_eq!(constants_array.len(), CV_NORMAL_SLOT_COUNT as usize);
            for i in 0..CV_TWO_LEVEL_VECTOR_SIZE {
                constants_array.push(LispObject::Int(i as i64));
            }
            constants_array.extend(two_level_vectors);
        }

        let mut code: Vec<u8> = Vec::new();
        let mut current_stack_size = 0;
        let mut max_stack_size = 0;
        for op in self.ops {
            let op = if let Op::PushConstant(v) = op {
                Op::PushConstant(index_remap[&v])
            } else {
                op
            };
            code.extend(op.to_code()?);
            current_stack_size += op.get_stack_delta();
            debug_assert!(current_stack_size >= 0);
            max_stack_size = i32::max(current_stack_size, max_stack_size);
        }

        // some buffer for max stack size (for the PushConstant variant)
        Ok((code, constants_array, max_stack_size + 8))
    }

    fn into_repl(self) -> Result<String> {
        let (code, constants, max_stack_size) = self.into_bytecode()?;
        Ok(format!("#[0 {} {} {}]",
                   LispObject::Str(code.into_iter().map(|x| x as char).collect()).to_repl(),
                   LispObject::Vector(constants).to_repl(),
                   max_stack_size))
    }
}

pub fn generate_bytecode_repl(value: &json::Value, options: BytecodeOptions) -> Result<String> {
    let mut compiler = BytecodeCompiler {
        options,
        ops: Vec::new(),
        constants: BTreeMap::new(),
    };
    compiler.compile(value);
    compiler.into_repl()
}
