
use std::collections::HashMap;

use Functions;
use Classes;
use Class;
use value::{Value,EValue};
use callable::Callable;
use callable::Convertible;
use callable::Boxable;


fn len(v: Value) -> i32 {
  match v.evalue() {
    EValue::Int(_v) => 1,
    EValue::Flt(_v) => 1,
    EValue::Str(s) => s.len() as i32,
    EValue::Err(_e) => 1,
    EValue::Vec(v) => v.len() as i32,
    EValue::Obj(_o) => 1,
    EValue::Fun(_f) => 1,
    EValue::Pri(_p) => 1,
    EValue::Box(_p) => 1,
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

#[derive(Clone, Debug)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x:i32, y:i32) -> Point {
    Point{x:x, y:y}
  }
  fn l1(&mut self) -> i32 {
    self.x + self.y
  }
  fn translate(&mut self, x:i32, y:i32) -> i32 {
    self.x += x;
    self.y += y;
    self.l1()
  }
}

impl Boxable for Point {
  fn class_name() -> String {
    "Point".to_string()
  }
}

pub fn load_stdlib(fs: &mut Functions, clss: &mut Classes) {
  fs.insert("len".to_string(), Value::from_pri(Box::new(len as fn(Value) -> i32)));
  fs.insert("reversed".to_string(), Value::from_pri(Box::new(reversed as fn(Vec<Value>) -> Vec<Value>)));
  fs.insert("push".to_string(), Value::from_pri(Box::new(push as fn(Value, Value) -> Value)));
  fs.insert("print".to_string(), Value::from_pri(Box::new(print as fn(Value) -> Value)));
  let fptr: fn(&mut Vec<Value>)->(Option<Value>) = Vec::<Value>::pop;
  fs.insert("pop".to_string(), Value::from_pri(Box::new(fptr)));
 
  let mut cPoint = Class {name: "Point".to_string(), fields: hashmap!{"box".to_string() => 0}, funcs: HashMap::new()};
  fs.insert("newPoint".to_string(), Value::from_pri(Box::new(Point::new as fn(i32, i32)->Point)));
  cPoint.funcs.insert("translate".to_string(), Value::from_pri(Box::new(Point::translate as fn(&mut Point, i32, i32)->i32)));
  cPoint.funcs.insert("l1".to_string(), Value::from_pri(Box::new(Point::l1 as fn(&mut Point)->i32)));
  clss.insert("Point".to_string(), Box::new(cPoint));
}