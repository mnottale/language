

use value::{Value,T_ARR};

pub trait Convertible {
  fn to_value(&self) -> Value;
  fn from_value(&Value) -> Self;
}

impl Convertible for i32 {
  fn to_value(&self) -> Value { Value::from_int(*self)}
  fn from_value(v: &Value) -> i32 { v.as_int()}
}

impl Convertible for Value {
  fn to_value(&self) -> Value { self.clone()}
  fn from_value(v: &Value) -> Value { v.clone()}
}

impl Convertible for Vec<Value> {
  fn to_value(&self) -> Value { Value::from_vec(self.clone())}
  fn from_value(v: &Value) -> Vec<Value> {
    if v.vtype() == T_ARR {
      v.as_vec().clone()
    } else {
      Vec::new()
    }
  }
}

pub trait Callable {
  fn call(&self, args: Vec<Value>) -> Value;
}

/*
impl Callable for fn(i32) -> i32 {
  fn call(&self, args: Vec<Value>) -> Value {
    Value::from_int(self(args[0].as_int()))
  }
}*/

impl<R: Convertible, P1:Convertible> Callable for fn(P1) -> R {
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value(&args[0])).to_value()
  }
}

impl<R: Convertible, P1:Convertible, P2: Convertible> Callable for fn(P1, P2) -> R {
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value(&args[0]), Convertible::from_value(&args[1])).to_value()
  }
}

use std;
use std::fmt::Debug;
impl Debug for Callable {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    "<Prim>".fmt(f)
 }
}

