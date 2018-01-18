
use Functions;
use value::{Value,EValue};
use callable::Callable;
use callable::Convertible;

fn len(v: Value) -> i32 {
  match v.evalue() {
    EValue::Int(_v) => 1,
    EValue::Str(s) => s.len() as i32,
    EValue::Err(_e) => 1,
    EValue::Vec(v) => v.len() as i32,
    EValue::Obj(_o) => 1,
    EValue::Fun(_f) => 1,
    EValue::Pri(_p) => 1,
  }
}

fn reversed(v: Vec<Value>) -> Vec<Value> {
  let mut r = v.clone();
  r.reverse();
  r
}

fn push(v: Value, what: Value) -> Value {
  match v.evalue() {
    EValue::Vec(v) => v.push(what),
    _ => {}
  };
  v
}

fn print(v: Value) -> Value {
  println!("{:?}", v);
  v
}

pub fn load_stdlib(fs: &mut Functions) {
  fs.insert("len".to_string(), Value::from_pri(Box::new(len as fn(Value) -> i32)));
  fs.insert("reversed".to_string(), Value::from_pri(Box::new(reversed as fn(Vec<Value>) -> Vec<Value>)));
  fs.insert("push".to_string(), Value::from_pri(Box::new(push as fn(Value, Value) -> Value)));
  fs.insert("print".to_string(), Value::from_pri(Box::new(print as fn(Value) -> Value)));
  let fptr: fn(&mut Vec<Value>)->(Option<Value>) = Vec::<Value>::pop;
  fs.insert("pop".to_string(), Value::from_pri(Box::new(fptr)));
}