use std::collections::HashMap;

use cpython::{Python, PyObject, PythonObject, PyDict, PyResult, PyModule, ToPyObject};
use callable::{Boxable, Convertible};
use value::Value;


pub fn test_python() {
  let gil = Python::acquire_gil();
  let python = gil.python();
  let sys = python.import("sys").unwrap();
  let version = sys.get(python, "version").unwrap();
  let vtype = version.get_type(python);
  println!("vtype: {}", vtype.name(python));
}

pub fn get_module(_: Value, modname: String) -> PyModule {
  Python::acquire_gil().python().import(modname.as_str()).unwrap()
}

impl Boxable for PyModule {
  fn class_name() -> String { "PyModule".to_string()}
}

impl Convertible for PyObject {
  fn to_value(self) -> Value {
    Value::from_str(self.extract(Python::acquire_gil().python()).unwrap())
  }
  fn from_value(v: &Value) -> PyObject {
    let pl = "foo".to_py_object(Python::acquire_gil().python());
    let po: PyObject = pl.into_object();
    po
  }
}

fn pymodule_get(me: &mut PyModule, attr: String) -> PyObject {
  let gil = Python::acquire_gil();
  let python = gil.python();
  me.get(python, attr.as_str()).unwrap()
}


use Functions;
use Classes;
use Class;

pub fn load_python(fs: &mut Functions, clss: &mut Classes) {
  fs.insert("pymodule".to_string(), Value::from_pri(Box::new(get_module as fn(Value, String)->PyModule)));
  let mut cModule = Class {name: "PyModule".to_string(), fields: hashmap!{"box".to_string() => 0}, funcs: HashMap::new()};
  cModule.funcs.insert("get".to_string(), Value::from_pri(Box::new(pymodule_get as fn(&mut PyModule, String)->PyObject)));
  clss.insert("PyModule".to_string(), Box::new(cModule));
}