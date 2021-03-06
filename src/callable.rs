
use Object;
use get_class;
use value::{Value,T_ARR, T_BOX, T_OBJ};
use std::marker::Sized;

pub trait Boxable {
  fn class_name() -> String;
}

pub trait Convertible {
  fn to_value(self) -> Value;
  fn from_value(&Value) -> Self;
  fn to_value_ref(&self) -> Value { panic!("to_value_ref not implemented")}
  fn from_value_refmut(&Value) -> &mut Self {panic!("from_value_refmut not implemented")}
}

/*
pub trait CloneOrDie: Sized {
  fn clone_or_die(&self) -> Self { panic!("clone not implemented")}
}

impl<T> CloneOrDie for T {}
impl<T> CloneOrDie for T where T:Clone {
  fn clone_or_die(&self) -> Self { self.clone()}
}

fn cod<T>(me: &T) -> T { panic!("type not clonable")}
fn cod<T:Clone>(me: &T) -> T {
  me.clone()
}*/

impl<T> Convertible for T where T:Boxable+Sized+'static {
  fn to_value(self) -> Value {
    let b = Value::from_any(Box::new(self));
    Value::from_obj(Object{
        class: get_class(T::class_name()),
        fields: vec![b],
    })
  }
  fn from_value(v: &Value) -> T {
    panic!("Impossible conversion");
    /*
    if v.vtype() == T_BOX {
      (*(*v).as_box().downcast_ref::<T>().unwrap()).clone_or_die()
    } else if v.vtype() == T_OBJ {
      (*v).as_obj().fields[0].as_box().downcast_ref::<T>().unwrap().clone_or_die()
    } else {
      panic!("Not an object nor a box")
    }*/
  }
  /*
  fn to_value_ref(&self) -> Value {
    let b = Value::from_any(Box::new((*self).clone_or_die()));
    Value::from_obj(Object{
        class: get_class(T::class_name()),
        fields: vec![b],
    })
  }*/
  fn from_value_refmut(v: &Value) -> &'static mut Self {
    if v.vtype() == T_BOX {
      (*v).as_box().downcast_mut::<T>().unwrap()
    } else if v.vtype() == T_OBJ {
      (*v).as_obj().fields[0].as_box().downcast_mut::<T>().unwrap()
    } else {
      panic!("Not an object nor a box")
    }
  }
}

/*
impl<'a,T> Convertible for &'a mut T where T:Boxable+?Sized+'static {
  fn to_value(self) -> Value {
    let b = Value::from_any(Box::new((*self).clone_or_die()));
    Value::from_obj(Object{
        class: get_class(T::class_name()),
        fields: vec![b],
    })
  }
  fn from_value(v: &Value) -> Self {
    if v.vtype() == T_BOX {
      (*v).as_box().downcast_mut::<T>().unwrap()
    } else if v.vtype() == T_OBJ {
      (*v).as_obj().fields[0].as_box().downcast_mut::<T>().unwrap()
    } else {
      panic!("Not an object nor a box")
    }
  }
}
*/
impl Convertible for i32 {
  fn to_value(self) -> Value { Value::from_int(self)}
  fn from_value(v: &Value) -> i32 { v.as_int()}
}

impl Convertible for f64 {
  fn to_value(self) -> Value { Value::from_flt(self)}
  fn from_value(v: &Value) -> f64 { v.as_flt()}
}

impl Convertible for Value {
  fn to_value(self) -> Value { self.clone()}
  fn from_value(v: &Value) -> Value { v.clone()}
}

impl Convertible for String {
  fn to_value(self) -> Value { Value::from_str(self)}
  fn from_value(v: &Value) -> String { v.as_str().clone()}
}

impl Convertible for Vec<Value> {
  fn to_value(self) -> Value { Value::from_vec(self)}
  fn from_value(v: &Value) -> Vec<Value> {
    if v.vtype() == T_ARR {
      v.as_vec().clone()
    } else {
      Vec::new()
    }
  }
  fn from_value_refmut(v: &Value) -> &'static mut Vec<Value> {
    v.as_vec()
  }
}

impl Convertible for Option<Value> {
  fn to_value(self) -> Value {
    match self {
      Some(v) => v,
      None => Value::from_err("No value".to_string())
    }
  }
  fn from_value(v: &Value) -> Option<Value> {
    Some(v.clone())
  }
}

/*
impl<'a> Convertible for &'a mut Vec<Value> {
  fn to_value(& self) -> Value {
    Value::from_vec(**self)
  }
  fn from_value(v: &Value) -> &'a mut Vec<Value> {
    v.as_vec()
  }
}
*/

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

impl<R: Convertible, P1> Callable for fn(&mut P1) -> R where P1:Convertible{
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value_refmut(&args[0])).to_value()
  }
}


impl<R: Convertible, P1:Convertible, P2: Convertible> Callable for fn(P1, P2) -> R {
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value(&args[0]), Convertible::from_value(&args[1])).to_value()
  }
}

impl<'a, R: Convertible, P1:'a, P2:Convertible> Callable for fn(&mut P1, P2) -> R where P1: Convertible{
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value_refmut(&args[0]), Convertible::from_value(&args[1])).to_value()
  }
}

impl<'a, R: Convertible, P1:'a, P2:Convertible, P3:Convertible> Callable for fn(&mut P1, P2, P3) -> R where P1: Convertible{
  fn call(&self, args: Vec<Value>) -> Value {
    self(Convertible::from_value_refmut(&args[0]), Convertible::from_value(&args[1]), Convertible::from_value(&args[2])).to_value()
  }
}

use std;
use std::fmt::Debug;
impl Debug for Callable {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    "<Prim>".fmt(f)
 }
}

