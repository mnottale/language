
use std;
use std::fmt::Debug;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum EValue {
  Int(i32),
  Str(&'static mut String),
  Err(&'static mut String),
  Vec(&'static mut List),
  Obj(&'static mut Object),
  Fun(&'static mut Function),
}

pub struct Value {
  v: u64,
}

use Function;
use Object;
use List;

static AMASK:u64 = 0x0000FFFFFFFFFFFF;
static TMASK:u64 = 0xFFFF000000000000;
static TSHIFT:i32 = 48;

pub static T_INT:u64 = 1;
pub static T_STR:u64 = 2;
pub static T_ARR:u64 = 3;
pub static T_ERR:u64 = 4;
pub static T_OBJ:u64 = 5;
pub static T_FUN:u64 = 6;


impl Value {
  pub fn from_int(val: i32) -> Value {
    Value{v: (T_INT << TSHIFT) + ((val as u32) as u64)}
  }
  pub fn from_str(val: String) -> Value {
    let sptr = Box::into_raw(Box::new(val));
    Value{v: (T_STR << TSHIFT) + (sptr as u64)}
  }
  pub fn from_err(val: String) -> Value {
    let sptr = Box::into_raw(Box::new(val));
    Value{v: (T_ERR << TSHIFT) + (sptr as u64)}
  }
  pub fn from_vec(val: List) -> Value {
    let l =  Box::into_raw(Box::new(val));
    //println!("{:x}", l as u64);
    let cp = Box::into_raw(Box::new((l as u64) | ((1 as u64) << TSHIFT)));
    //println!("{:x}", cp as u64);
    Value{v: (T_ARR << TSHIFT) + (cp as u64)}
  }
  pub fn from_obj(val: Object) -> Value {
    let l =  Box::into_raw(Box::new(val));
    //println!("{:x}", l as u64);
    let cp = Box::into_raw(Box::new((l as u64) | ((1 as u64) << TSHIFT)));
    //println!("{:x}", cp as u64);
    Value{v: (T_OBJ << TSHIFT) + (cp as u64)}
  }
  pub fn from_fun(val: Function) -> Value {
    let l =  Box::into_raw(Box::new(val));
    //println!("{:x}", l as u64);
    let cp = Box::into_raw(Box::new((l as u64) | ((1 as u64) << TSHIFT)));
    //println!("{:x}", cp as u64);
    Value{v: (T_FUN << TSHIFT) + (cp as u64)}
  }
  pub fn vtype(&self) -> u64 {
    ((self.v &TMASK) >> TSHIFT)
  }
  pub fn as_str(& self) -> &'static mut String {
    unsafe {
      &mut*((self.v&AMASK) as *mut String)
    }
  }
  pub fn as_err(& self) -> &'static mut String {
    unsafe {
      &mut*((self.v&AMASK) as *mut String)
    }
  }
  pub fn as_vec(& self) -> &'static mut List {
    unsafe {
      //println!("{:x}", self.v & AMASK);
      let cp = * ((self.v&AMASK) as *mut u64);
      //println!("{:x}", cp);
      &mut*((cp & AMASK) as *mut List)
    }
  }
  pub fn as_obj(&self) -> &'static mut Object {
    unsafe {
      //println!("{:x}", self.v & AMASK);
      let cp = * ((self.v&AMASK) as *mut u64);
      //println!("{:x}", cp);
      &mut*((cp & AMASK) as *mut Object)
    }
  }
  pub fn as_fun(&self) -> &'static mut Function {
    unsafe {
      //println!("{:x}", self.v & AMASK);
      let cp = * ((self.v&AMASK) as *mut u64);
      //println!("{:x}", cp);
      &mut*((cp & AMASK) as *mut Function)
    }
  }
  pub fn as_int(& self) -> i32 {
    ((self.v & AMASK) as u32) as i32
  }
  pub fn evalue(& self) -> EValue {
    let t = self.vtype();
    if t == T_INT { return EValue::Int(self.as_int()); }
    if t == T_STR { return EValue::Str(self.as_str()); }
    if t == T_ERR { return EValue::Err(self.as_err()); }
    if t == T_ARR { return EValue::Vec(self.as_vec()); }
    if t == T_OBJ { return EValue::Obj(self.as_obj()); }
    if t == T_FUN { return EValue::Fun(self.as_fun()); }
    unreachable!()
  }
  pub fn to_bool(& self) -> bool {
    match self.evalue() {
      EValue::Int(i) => i != 0 as i32,
      EValue::Str(s) => s.len() != 0,
      EValue::Vec(v) => v.len() != 0,
      EValue::Obj(_o) => true,
      EValue::Err(_e) => true,
      EValue::Fun(_f) => true,
    }
  }
}

impl Drop for Value {
  fn drop(&mut self) {
    if self.vtype() == T_STR {
      unsafe {
        Box::from_raw( (self.v & AMASK) as *mut String);
      }
    }
    if self.vtype() == T_ARR {
      // decrease refcount
      unsafe {
        let cp = * ((self.v&AMASK) as *mut u64);
        let mut count = (cp & TMASK) >> TSHIFT;
        count-= 1;
        if count == 0 {
          Box::from_raw((cp & AMASK) as *mut List);
          Box::from_raw((self.v&AMASK) as *mut u64);
        }
        else {
          * ((self.v&AMASK) as *mut u64) -= 1 << TSHIFT;
        }
      }
    }
    if self.vtype() == T_OBJ {
      // decrease refcount
      unsafe {
        let cp = * ((self.v&AMASK) as *mut u64);
        let mut count = (cp & TMASK) >> TSHIFT;
        count-= 1;
        if count == 0 {
          Box::from_raw((cp & AMASK) as *mut Object);
          Box::from_raw((self.v&AMASK) as *mut u64);
        }
        else {
          * ((self.v&AMASK) as *mut u64) -= 1 << TSHIFT;
        }
      }
    }
    if self.vtype() == T_FUN {
      // decrease refcount
      unsafe {
        let cp = * ((self.v&AMASK) as *mut u64);
        let mut count = (cp & TMASK) >> TSHIFT;
        count-= 1;
        if count == 0 {
          Box::from_raw((cp & AMASK) as *mut Function);
          Box::from_raw((self.v&AMASK) as *mut u64);
        }
        else {
          * ((self.v&AMASK) as *mut u64) -= 1 << TSHIFT;
        }
      }
    }
  }
}

impl Clone for Value {
  fn clone(&self) -> Value {
    if self.vtype() == T_STR {
      return Value::from_str(self.as_str().clone());
    }
    if self.vtype() == T_ERR {
      return Value::from_err(self.as_err().clone());
    }
    if self.vtype() == T_INT {
      return Value{v: self.v};
    }
    if self.vtype() == T_ARR {
      unsafe {
        // increase refcount
        * ((self.v&AMASK) as *mut u64) += 1 << TSHIFT;
        return Value{v: self.v};
      }
    }
    if self.vtype() == T_OBJ {
      unsafe {
        // increase refcount
        * ((self.v&AMASK) as *mut u64) += 1 << TSHIFT;
        return Value{v: self.v};
      }
    }
    if self.vtype() == T_FUN {
      unsafe {
        // increase refcount
        * ((self.v&AMASK) as *mut u64) += 1 << TSHIFT;
        return Value{v: self.v};
      }
    }
    unreachable!();
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    self.evalue().fmt(f)
 }
}