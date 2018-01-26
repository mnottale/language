use std::collections::HashMap;

use cpython::{ObjectProtocol, Python, PyObject, PyList, PyTuple, PythonObject, PyDict, PyResult, PyModule, ToPyObject};
use callable::{Boxable, Convertible, Callable};
use value::{Value, EValue};
use get_class;
use Object;


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

fn eval(v: String) -> PyObject {
  Python::acquire_gil().python().eval(v.as_str(), None, None).unwrap()
}

impl Boxable for PyModule {
  fn class_name() -> String { "PyModule".to_string()}
}

fn pyobject_to_value(me: PyObject, python: &Python) -> Value {
  match &*me.get_type(*python).name(*python) {
    "str" => Value::from_str(me.extract(*python).unwrap()),
    "int" => Value::from_int(me.extract(*python).unwrap()),
    "float" => Value::from_flt(me.extract(*python).unwrap()),
    "list" => {
      let pl: PyList = me.extract(*python).unwrap();
      let mut res = Vec::new();
      for i in 0..pl.len(*python) {
        res.push(pyobject_to_value(pl.get_item(*python, i), python))
      }
      Value::from_vec(res)
    },
    _ => {
      let b = Value::from_any(Box::new(me));
      Value::from_obj(Object{
          class: get_class("PyObject".to_string()),
          fields: vec![b],
      })
    },
  }
}

fn value_to_pyobject(me: &Value, python: &Python) -> PyObject {
  match me.evalue() {
    EValue::Int(i) => i.to_py_object(*python).into_object(),
    EValue::Flt(f) => f.to_py_object(*python).into_object(),
    EValue::Str(s) => s.to_py_object(*python).into_object(),
    EValue::Vec(v) => {
      let mut res = PyList::new(*python, &[]);
      for i in 0..v.len() {
        res.insert_item(*python, i, value_to_pyobject(&v[i], python));
      }
      res.into_object()
    },
    EValue::Box(b) => unsafe {
      PyObject::from_borrowed_ptr(*python,
        b.downcast_mut::<PyObject>().unwrap().as_ptr())
    },
    EValue::Obj(o) => unsafe {
      PyObject::from_borrowed_ptr(*python,
        o.fields[0].as_box().downcast_mut::<PyObject>().unwrap().as_ptr())
    },
    _ => panic!("cannot cast this to pyobject"),
  }
}

impl Convertible for PyObject {
  fn to_value(self) -> Value {
    let gil = Python::acquire_gil();
    let python = gil.python();
    pyobject_to_value(self, &python)
  }
  fn from_value(v: &Value) -> PyObject {
    let gil = Python::acquire_gil();
    let python = gil.python();
    value_to_pyobject(v, &python)
  }
}

fn pymodule_get(me: &mut PyModule, attr: String) -> PyObject {
  let gil = Python::acquire_gil();
  let python = gil.python();
  me.get(python, attr.as_str()).unwrap()
}

struct PyCall
{
}

impl Callable for PyCall {
  fn call(&self, args: Vec<Value>) -> Value {
    // self, slot, args
    let gil = Python::acquire_gil();
    let python = gil.python();
    let target: PyObject = Convertible::from_value(&args[0]);
    let mut pargs = Vec::<PyObject>::new();
    for i in 2..args.len() {
      pargs.push(Convertible::from_value(&args[i]));
    }
    target.call_method(python, args[1].as_str().as_str(), PyTuple::new(python, pargs.as_slice()), None).unwrap().to_value()
  }
}

struct PyModCall
{
}

impl Callable for PyModCall {
  fn call(&self, args: Vec<Value>) -> Value {
    // self, slot, args
    let gil = Python::acquire_gil();
    let python = gil.python();
    let target: &mut PyModule = Convertible::from_value_refmut(&args[0]);
    let mut pargs = Vec::<PyObject>::new();
    for i in 2..args.len() {
      pargs.push(Convertible::from_value(&args[i]));
    }
    target.call(python, args[1].as_str().as_str(), PyTuple::new(python, pargs.as_slice()), None).unwrap().to_value()
  }
}

use Functions;
use Classes;
use Class;

pub fn load_python(fs: &mut Functions, clss: &mut Classes) {
  let mut cModules = Class {name: "PyModules".to_string(), fields: HashMap::new(), funcs: HashMap::new()};
  cModules.funcs.insert("fallback".to_string(), Value::from_pri(Box::new(get_module as fn(Value, String)->PyModule)));
  // we cannot call get_class() to construct the object as it would deadlock
  let bcm = Box::new(cModules);
  let cptr : *const Class = &*bcm;
  let cref: &'static Class = unsafe { &*cptr};
  clss.insert("PyModules".to_string(), bcm);
  fs.insert("pymodule".to_string(), Value::from_obj(Object{class: cref, fields: Vec::new()}));
  
  fs.insert("pyeval".to_string(), Value::from_pri(Box::new(eval as fn(String) -> PyObject)));
  let mut cModule = Class {name: "PyModule".to_string(), fields: hashmap!{"box".to_string() => 0}, funcs: HashMap::new()};
  cModule.funcs.insert("get".to_string(), Value::from_pri(Box::new(pymodule_get as fn(&mut PyModule, String)->PyObject)));
  cModule.funcs.insert("fallback".to_string(), Value::from_pri(Box::new(PyModCall{})));
 
  clss.insert("PyModule".to_string(), Box::new(cModule));
  
  let mut cObject = Class {name: "PyObject".to_string(), fields: hashmap!{"box".to_string() => 0}, funcs: HashMap::new()};
  cObject.funcs.insert("fallback".to_string(), Value::from_pri(Box::new(PyCall{})));
  clss.insert("PyObject".to_string(), Box::new(cObject));
}