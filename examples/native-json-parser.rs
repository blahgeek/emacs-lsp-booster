use emacs::{defun, Env, Result, Value, IntoLisp};
use serde_json as json;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

fn json_to_lisp<'a>(env: &'a Env, val: &json::Value) -> Result<Value<'a>> {
    match val {
        &json::Value::Null | &json::Value::Bool(false) =>
            ().into_lisp(env),
        &json::Value::Bool(true) =>
            true.into_lisp(env),
        &json::Value::Number(ref num) =>
            if num.is_f64() {
                num.as_f64().unwrap().into_lisp(env)
            } else if num.is_i64() {
                num.as_i64().unwrap().into_lisp(env)
            } else {
                num.as_u64().unwrap().into_lisp(env)
            },
        &json::Value::String(ref s) =>
            s.into_lisp(env),
        &json::Value::Array(ref arr) => {
            let vals = arr.iter().map(|x| json_to_lisp(env, x)).collect::<Result<Vec<_>>>()?;
            env.vector(&vals)
        },
        &json::Value::Object(ref map) => {
            let mut vals: Vec<Value<'a>> = Vec::new();
            for (k, v) in map {
                vals.push(env.intern(&format!(":{}", k))?);
                vals.push(json_to_lisp(env, v)?);
            }
            env.list(&vals)
        },
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
