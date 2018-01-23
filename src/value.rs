
use std;
use std::fmt::Debug;
use std::fmt::Formatter;
use callable::Callable;
use std::any::Any;

#[derive(Debug)]
pub enum EValue {
  Int(i32),
  Str(&'static mut String),
  Err(&'static mut String),
  Vec(&'static mut List),
  Obj(&'static mut Object),
  Fun(&'static mut Function),
  Pri(&'static Callable),
  Box(&'static mut Any),
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
pub static T_PRI:u64 = 7;
pub static T_BOX:u64 = 8;

use std::ops::Deref;
use std::ops::DerefMut;

fn f64_repr(v: f64) -> u64 {
  unsafe {
    let iptr: *const f64 = &v;
    let jptr = iptr as *const u64;
    let j = *jptr;
    j
  }
}

fn f64_from_repr(v: u64) -> f64 {
  unsafe {
    let iptr: *const u64 = &v;
    let jptr = iptr as *const f64;
    *jptr
  }
}

pub fn box_f64(v: f64) -> u64 {
  let repr = f64_repr(v);
  let sgn = repr >> 63;
  let mut exp = (repr & 0b01111111_11110000_00000000_00000000_00000000_00000000_00000000_00000000) >> (63-11);
  let mut man = repr & 0x000FFFFFFFFFFFFF;
  // exp is +1023   inf is s,2047,0   NaN is s,2047,nonzero
  if exp == 2047 {
    exp = 1023;
  }
  else {
    if (exp as i64) -1023 > 511 { // too big, INF
      exp = 1023;
      man = 0;
    } else {
      if (exp as i64) - 1023 < -511 { // too small, 0
        exp = 0;
        man = 0;
      } else {
        exp = ((exp as i64) - 1023 + 511) as u64;
      }
    }
  }
  man + (exp << 52) + (sgn << 62) + (1<<63)
}

pub fn unbox_f64(repr:u64) -> f64 {
  let sgn = (repr >> 62) & 1;
  let mut exp = (repr >> 52) & 0b1111111111;
  let man = repr & 0x000FFFFFFFFFFFFF;
  if exp == 1023 {
    exp = 2047;
  } else {
    if exp != 0 {
      exp = ((exp as i64) - 511 + 1023) as u64;
    }
  }
  let ieee = (sgn << 63) + (exp << 52) + man;
  f64_from_repr(ieee)
}




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
  pub fn from_pri(val: Box<Callable>) -> Value {
    //let b:Box<Callable> = Box::new(*val);
    let l = Box::into_raw(Box::new(val));
    //println!("{:x}", l as u64);
    let cp = Box::into_raw(Box::new((l as u64) | ((1 as u64) << TSHIFT)));
    //println!("{:x}", cp as u64);
    Value{v: (T_PRI << TSHIFT) + (cp as u64)}
  }
  pub fn from_any(val: Box<Any>) -> Value {
    //let b:Box<Callable> = Box::new(*val);
    let l = Box::into_raw(Box::new(val));
    //println!("{:x}", l as u64);
    let cp = Box::into_raw(Box::new((l as u64) | ((1 as u64) << TSHIFT)));
    //println!("{:x}", cp as u64);
    Value{v: (T_BOX << TSHIFT) + (cp as u64)}
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
  pub fn as_pri(&self) -> &'static Callable {
  unsafe {
      //println!("{:x}", self.v & AMASK);
      let cp = * ((self.v&AMASK) as *mut u64);
      //println!("{:x}", cp);
      let b = &mut*((cp & AMASK) as *mut Box<Callable>);
      let c = b.deref().deref() as *const Callable;
      &*c
    }
  }
  pub fn as_box(&self) -> &'static mut Any {
  unsafe {
      //println!("{:x}", self.v & AMASK);
      let cp = * ((self.v&AMASK) as *mut u64);
      //println!("{:x}", cp);
      let b = &mut*((cp & AMASK) as *mut Box<Any>);
      let c = (*b).deref_mut() as *mut Any;
      &mut *c
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
    if t == T_PRI { return EValue::Pri(self.as_pri()); }
    if t == T_BOX { return EValue::Box(self.as_box()); }
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
      EValue::Pri(_p) => true,
      EValue::Box(_p) => true,
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
    if self.vtype() == T_PRI {
      // decrease refcount
      unsafe {
        let cp = * ((self.v&AMASK) as *mut u64);
        let mut count = (cp & TMASK) >> TSHIFT;
        count-= 1;
        if count == 0 {
          Box::from_raw((cp & AMASK) as *mut Box<Callable>);
          Box::from_raw((self.v&AMASK) as *mut u64);
        }
        else {
          * ((self.v&AMASK) as *mut u64) -= 1 << TSHIFT;
        }
      }
    }
    if self.vtype() == T_BOX {
      // decrease refcount
      unsafe {
        let cp = * ((self.v&AMASK) as *mut u64);
        let mut count = (cp & TMASK) >> TSHIFT;
        count-= 1;
        if count == 0 {
          Box::from_raw((cp & AMASK) as *mut Box<Any>);
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
    if self.vtype() == T_PRI {
      unsafe {
        // increase refcount
        * ((self.v&AMASK) as *mut u64) += 1 << TSHIFT;
        return Value{v: self.v};
      }
    }
    if self.vtype() == T_BOX {
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
