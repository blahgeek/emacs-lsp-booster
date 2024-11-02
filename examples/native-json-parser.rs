use emacs::{defun, Env, IntoLisp, Result, Value};
use sonic_rs as json;
use sonic_rs::{JsonContainerTrait, JsonValueTrait};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

fn json_to_lisp<'a>(env: &'a Env, val: &json::Value) -> Result<Value<'a>> {
    match val.get_type() {
        json::JsonType::Null => ().into_lisp(env),
        json::JsonType::Boolean => {
            if val.is_true() {
                true.into_lisp(env)
            } else {
                ().into_lisp(env)
            }
        }
        json::JsonType::Number => {
            if val.is_f64() {
                val.as_f64().unwrap().into_lisp(env)
            } else if val.is_i64() {
                val.as_i64().unwrap().into_lisp(env)
            } else {
                val.as_u64().unwrap().into_lisp(env)
            }
        }
        json::JsonType::String => val.as_str().unwrap().to_string().into_lisp(env),
        json::JsonType::Array => {
            let vals = val
                .as_array()
                .unwrap()
                .as_slice()
                .iter()
                .map(|x| json_to_lisp(env, x))
                .collect::<Result<Vec<_>>>()?;
            env.vector(&vals)
        }
        json::JsonType::Object => {
            let map = val.as_object().unwrap();
            let mut vals: Vec<Value<'a>> = Vec::new();
            for (k, v) in map {
                vals.push(env.intern(&format!(":{}", k))?);
                vals.push(json_to_lisp(env, v)?);
            }
            env.list(&vals)
        }
    }
}

#[defun]
fn parse(env: &Env, s: String) -> Result<Value<'_>> {
    let start = std::time::Instant::now();
    let json_val = json::from_str(&s)?;
    let end = std::time::Instant::now();
    env.message(format!("json parsing took {:?}", end.duration_since(start)))?;

    json_to_lisp(env, &json_val)
}
