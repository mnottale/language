//#![feature(underscore_lifetimes)]


#[macro_use]
extern crate lazy_static;

extern crate pest;

#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct LParser;

use std::collections::HashMap;
use std::sync::Mutex;
use std::io::{self, BufRead};
use std::sync::Arc;

use std::ops::Deref;
use std::ops::DerefMut;

use std::fmt::Debug;
mod value;

use value::Value;
use value::EValue;

use value::T_INT;

lazy_static! {
  static ref variables : Mutex<HashMap<String, Value>> = Mutex::new(HashMap::new());
  static ref arrays : Mutex<Allocator<Vec<Value>>> = Mutex::new(Allocator::new(Vec::new()));
  //static ref functions : Arc<HashMap<String, Box<Function>>> = Arc::new(HashMap::new());
}

struct Allocator<T: Clone> {
  template: T,
  avail: Vec<Box<T>>,
  frees: Vec<u32>,
}

impl<T> Allocator<T> where T: Clone {
  fn new(template: T) -> Allocator<T> {
    Allocator::<T> {template: template, avail: Vec::new(), frees: Vec::new()}
  }
  fn alloc(&mut self) -> u32 {
    if self.frees.len() != 0 {
      let res = self.frees[self.frees.len()-1];
      self.frees.pop();
      return res;
    } else {
      self.avail.push(Box::new(self.template.clone()));
      return self.avail.len() as u32;
    }
  }
  fn free(&mut self, idx: u32) {
    self.avail[idx as usize] = Box::new(self.template.clone());
    self.frees.push(idx);
  }
  fn get(&mut self, idx: u32) -> *mut T {
    self.avail[idx as usize].deref_mut()
  }
}



type List = Vec<Value>;



pub struct Object {
  class: &'static Class,
  fields: Vec<Value>,
}

impl Object {
  fn new(class: &Class) -> Object {
    unsafe {
      Object{class: &*(class as *const Class), fields: Vec::new()}
    }
  }
}

impl Debug for Object {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
   self.class.name.fmt(f)
 }
}

impl Debug for Function {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
   "<func>".fmt(f)
 }
}


type Functions = HashMap<String, Box<Function>>;
type ExprList = Vec<Box<Expression>>;
type IdentList = Vec<String>;

#[derive(Clone)]
enum Expression {
  Constant(i32),
  GlobalVariable(String),
  StackVariable(i32),
  Array(ExprList),
  Lambda(Box<Function>),
  Dot{lhs: Box<Expression>, rhs: String, cacheClass: *const Class, cacheIndex: i32},
  ObjDef{cls: String, init: Vec<Box<Expression>>},
  SubScript{v: Box<Expression>, idx: Box<Expression>},
  Operator{lhs: Box<Expression>, rhs: Box<Expression>, op: String},
  FunctionCall{function: Box<Expression>, args: Vec<Box<Expression>>},
}

#[derive(Clone)]
enum Statement {
  GlobalAssignment{target: String, rhs: Box<Expression>},
  StackAssignment{target: i32, rhs: Box<Expression>},
  ObjAssignment{target: Box<Expression>, slot: String, rhs: Box<Expression>, cacheClass: *const Class, cacheIndex: i32},
  If{cond: Box<Expression>, block: Box<Block>, blockelse: Option<Box<Block>>},
  Expression(Box<Expression>),
}

// variable name -> stack index
type Stack = HashMap<String, i32>;

struct Class {
  name: String,
  fields: HashMap<String, i32>,
}

#[derive(Clone)]
pub struct Function {
  formals: Vec<String>,
  code: Box<Block>,
  stack: Box<Stack>,
  closureBuild: Vec<i32>, // parent stack indexes
  closurePut: Vec<i32>, // stack index where to put closed values
  closure: Vec<Value>,
}

#[derive(Clone)]
struct Block {
  statements: Vec<Box<Statement>>,
}

struct ParseContext {
  stack: Option<Box<Stack>>,
  parentStack: Option<Box<Stack>>,
  close: HashMap<i32, i32>, // parent stack index -> self stack index
}

type Classes = HashMap<String, Box<Class>>;

struct ExecContext {
  stack: Vec<Value>,
  functions: Arc<Functions>,
  classes: Arc<Classes>,
}

// DIE BEARCLAW DIE
fn bearclaw<T>(t: &T) -> &'static mut T {
  unsafe {
    let tt = t as *const T;
    let tt2 = tt as u64;
    let tt3 = tt2 as *mut T;
    return &mut *tt3;
  }
}

fn parse_classdecl(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, _ctx: &mut ParseContext) -> Class {
  let name = pairs.next().unwrap().into_span().as_str().to_string();
  let mut fields = HashMap::new();
  for p in pairs {
    let fname = p.into_span().as_str().to_string();
    let l = fields.len();
    fields.insert(fname, l as i32);
  }
  Class{name: name, fields: fields}
}

fn parse_variable(name: String, ctx: &mut ParseContext) -> Expression {
  match ctx.stack {
    None => Expression::GlobalVariable(name),
    Some(ref mut bcf) => {
      match bcf.get(&name) {
        Some(idx) => {return Expression::StackVariable(*idx);}
        _ => {}
      }
      match ctx.parentStack {
        None => Expression::GlobalVariable(name),
        Some(ref pstack) => match pstack.get(&name) {
          None => Expression::GlobalVariable(name),
          Some(idx) => {
            let sidx = bcf.len() as i32;
            bcf.insert(name.clone(), sidx);
            ctx.close.insert(*idx, sidx);
            Expression::StackVariable(sidx)
          }
        }
      }
    }
  }
}

fn parse_objdef(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let name = pairs.next().unwrap().into_span().as_str().to_string();
  let init = parse_exprlist(pairs.next().unwrap().into_inner(), ctx);
  Expression::ObjDef{cls: name, init: init}
}

fn parse_binary(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let noie = pairs.next().unwrap();
  let op = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let v1 = match noie.as_rule() {
    Rule::number => Expression::Constant(noie.into_span().as_str().to_string().parse::<i32>().unwrap()),
    Rule::identchain => parse_identchain(noie.into_inner(), ctx),
    Rule::expr => parse_expr(noie.into_inner(), ctx),
    _ => unreachable!(),
  };
  let v2 = parse_expr(expr.into_inner(), ctx);
  return Expression::Operator{lhs: Box::new(v1), rhs: Box::new(v2), op: op.into_span().as_str().to_string()}
}

fn parse_exprlist(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> ExprList {
  let mut res = ExprList::new();
  for p in pairs {
    res.push(Box::new(parse_expr(p.into_inner(), ctx)))
  }
  println!("Parsed exprlist of len {}", res.len());
  res
}

fn parse_identlist(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, _ctx: &mut ParseContext) -> IdentList {
  let mut res = IdentList::new();
  for p in pairs {
    res.push(p.into_span().as_str().to_string())
  }
  res
}

fn parse_identchain(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let first = pairs.next().unwrap().into_span().as_str().to_string();
  let mut init = parse_variable(first, ctx);
  for p in pairs {
    let next = p.into_span().as_str().to_string();
    init = Expression::Dot{lhs: Box::new(init), rhs: next, cacheClass: 0 as *const Class, cacheIndex: 0};
  }
  init
}

fn parse_expr(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let content = pairs.next().unwrap();
  let mut v = match content.as_rule() {
      Rule::binary => parse_binary(content.into_inner(), ctx),
      Rule::number => Expression::Constant(content.into_span().as_str().to_string().parse::<i32>().unwrap()),
      Rule::ident =>  parse_variable(content.into_span().as_str().to_string(), ctx),
      Rule::identchain => parse_identchain(content.into_inner(), ctx),
      Rule::expr => parse_expr(content.into_inner(), ctx),
      Rule::array => {
        Expression::Array(parse_exprlist(content.into_inner().next().unwrap().into_inner(), ctx))
      },
      Rule::objdef => parse_objdef(content.into_inner(), ctx),
      Rule::lambda => parse_lambda(content.into_inner(), ctx),
      _ => unreachable!(),
  };
  // parse subsequents [expr] or .ident or (exprlist)
  for p in pairs {
    v = match p.as_rule() {
      Rule::ident => Expression::Dot{lhs: Box::new(v), rhs: p.into_span().as_str().to_string(), cacheClass: 0 as *const Class, cacheIndex:0},
      Rule::expr => Expression::SubScript{v: Box::new(v), idx: Box::new(parse_expr(p.into_inner(), ctx))},
      Rule::exprlist => Expression::FunctionCall{function: Box::new(v), args: parse_exprlist(p.into_inner(), ctx)},
      _ => unreachable!(),
    }
  };
  v
}

fn parse_statement(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let inner_pair = pair.into_inner().next().unwrap();
  //println!("  Rule:    {:?}", inner_pair.as_rule());
  //process_statement(inner_pair, &mut state);
  match inner_pair.as_rule() {
    Rule::assign => parse_assign(inner_pair.into_inner(), ctx),
    Rule::IF => parse_if(inner_pair.into_inner(), ctx),
    Rule::expr => Statement::Expression(Box::new(parse_expr(inner_pair.into_inner(), ctx))),
    Rule::vardecl => {
      let mut vd = inner_pair.into_inner();
      let ident = vd.next().unwrap().into_span().as_str().to_string();
      let mut idx = 0;
      match ctx.stack {
        None => {},
        Some(ref mut cf) => { let len = cf.deref().len(); cf.deref_mut().insert(ident, len as i32);idx = len;}
      };
      let rhs = parse_expr(vd.next().unwrap().into_inner(), ctx);
      Statement::StackAssignment{target: idx as i32, rhs: Box::new(rhs)}
    }
    other => {println!("Unhandled:    {:?}", other); Statement::Expression(Box::new(Expression::Constant(100000)))},
  }
}


fn parse_if(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let expr = pairs.next().unwrap();
  let block = pairs.next().unwrap();
  println!("Block:    {:?}", block.as_rule());
  println!("Block:    {}", block.clone().into_span().as_str());
  let cond =  parse_expr(expr.into_inner(), ctx);
  let block = parse_block(block.into_inner(), ctx);
  let belse = match pairs.next() {
    None => None,
    Some(b) => Some(Box::new(parse_block(b.into_inner(), ctx))),
  };
  Statement::If{cond: Box::new(cond), block: Box::new(block), blockelse: belse}
}

fn parse_assign(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let ident = parse_identchain(pairs.next().unwrap().into_inner(), ctx);
  let expr = pairs.next().unwrap();
  let rrhs = parse_expr(expr.into_inner(), ctx);
  match ident {
    Expression::GlobalVariable(v) => Statement::GlobalAssignment{target: v, rhs: Box::new(rrhs)},
    Expression::StackVariable(v) => Statement::StackAssignment{target: v, rhs: Box::new(rrhs)},
    Expression::Dot{lhs, rhs, cacheClass, cacheIndex} => Statement::ObjAssignment{target: lhs, slot: rhs, rhs: Box::new(rrhs), cacheClass: 0 as *const Class, cacheIndex: 0},
    _ => {unreachable!()},
  }
}

fn parse_block(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Block {
  let mut res = Vec::new();
  for r in pairs {
    //println!("blockstatement:    {}", r.clone().into_span().as_str());
    res.push(Box::new(parse_statement(r, ctx)));
  }
  return Block{statements: res};
}

fn parse_lambda(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let pformals = pairs.next().unwrap();
  let formals = parse_identlist(pformals.into_inner(), ctx);
  let mut stack = Stack::new();
  for i in 0..formals.len() {
    stack.insert(formals[i].clone(), i as i32);
  }
  let body = pairs.next().unwrap();
  let mut newctx = ParseContext{stack: Some(Box::new(stack)), parentStack: ctx.stack.clone(), close: HashMap::new()};
  let mut block = parse_block(body.into_inner(), &mut newctx);
  let mut closureBuild = Vec::new();
  let mut closurePut = Vec::new();
  for (p, s) in newctx.close {
    closureBuild.push(p);
    closurePut.push(s);
  }
  Expression::Lambda(Box::new(Function{
      formals: formals,
      code: Box::new(block),
      stack: newctx.stack.unwrap(),
      closureBuild: closureBuild,
      closurePut: closurePut,
      closure: Vec::new(),
  }))
}

fn process_toplevel(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>, mut funcs: &mut Arc<Functions>, mut classes: &mut Arc<Classes>) -> Value {
  let item = pair.into_inner().next().unwrap();
  let mut ctx: ParseContext = ParseContext{stack: None, parentStack: None, close: HashMap::new()};
  match item.as_rule() {
    Rule::assign => exec_statement(&parse_assign(item.into_inner(), &mut ctx), &mut ExecContext{stack: Vec::new(), functions: Arc::clone(funcs), classes: Arc::clone(classes)}),
    Rule::funcdef => {
      
      let mut comps = item.into_inner();
      let name = comps.next().unwrap();
      let pformals = comps.next().unwrap();
      let formals = parse_identlist(pformals.into_inner(), &mut ctx);
      let mut stack = Stack::new();
      for i in 0..formals.len() {
        stack.insert(formals[i].clone(), i as i32);
      }
      ctx.stack = Some(Box::new(stack));
      let body = comps.next().unwrap();
      let mut block = parse_block(body.into_inner(), &mut ctx);
      Arc::get_mut(&mut funcs).unwrap().insert(name.clone().into_span().as_str().to_string(),
        Box::new(Function{formals: formals, code: Box::new(block), stack: ctx.stack.unwrap(), closure: Vec::new(), closureBuild:Vec::new(), closurePut: Vec::new()}));
      Value::from_int(0)
    },
    Rule::classdecl => {
      let cls = parse_classdecl(item.into_inner(), &mut ctx);
      let cname = cls.name.clone();
      Arc::get_mut(classes).unwrap().insert(cls.name.clone(), Box::new(cls));
      Value::from_str(cname)
    }
    _ => Value::from_int(-1000000),
  }
}

fn exec_statement(s: &Statement, ctx: &mut ExecContext) -> Value {
  match *s {
    Statement::GlobalAssignment{ref target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      //println!("assigning {} to {}", val, target);
      let mut v = variables.lock().unwrap();
      let entry = v.entry(target.clone()).or_insert(Value::from_int(0));
      *entry = val;
      (*entry).clone()
    },
    Statement::StackAssignment{target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      ctx.stack[target as usize] = val;
      ctx.stack[target as usize].clone()
    }
    Statement::Expression(ref e) => {
      exec_expr(&*e, ctx)
    },
    Statement::If{ref cond, ref block, ref blockelse} => {
      let cond = exec_expr(&*cond, ctx);
      //println!("Condition is {}", cond.to_bool());
      match cond.to_bool() {
       true => exec_block(&*block, ctx),
       false => match *blockelse {
          None => Value::from_int(0),
          Some(ref b) => exec_block(&*b, ctx),
        }
      }
    },
    Statement::ObjAssignment{ref target, ref slot, ref rhs, ref cacheClass, ref cacheIndex} => {
      let tgt = exec_expr(&*target, ctx);
      let v = exec_expr(&*rhs, ctx);
      match tgt.evalue() {
        EValue::Obj(o) => {
          if o.class as *const Class == *cacheClass {
            o.fields[*cacheIndex as usize] = v.clone();
            v
          } else {
            match o.class.fields.get(slot) {
              Some(idx) => {
                *bearclaw(cacheClass) = o.class;
                *bearclaw(cacheIndex) = *idx;
                o.fields[*idx as usize] = v.clone();
                v
              },
              None => Value::from_err(String::new() + "no such field: " + &slot),
            }
          }
        },
        _ => Value::from_err("assignment to field of non-object".to_string()),
      }
    }
  }
}

fn exec_block(b: &Block, ctx: &mut ExecContext) -> Value {
  let mut val: Value = Value::from_int(0);
  for ref s in &b.statements {
    val = exec_statement(&*s, ctx)
  }
  return val;
}

fn exec_expr(e: &Expression, ctx: &mut ExecContext) -> Value {
  match *e {
    Expression::Constant(c) => Value::from_int(c),
    Expression::GlobalVariable(ref name) => variables.lock().unwrap()[name].clone(),
    Expression::StackVariable(idx) => ctx.stack[idx as usize].clone(),
    Expression::Operator {ref lhs, ref rhs, ref op} => {
      let l = exec_expr(&*lhs, ctx).as_int();
      let r = exec_expr(&*rhs, ctx).as_int();
      let r = match op.as_str() {
        "+" => l+r,
        "-" => l-r,
        ">" => {
          // println!("lhs: {}, rhs: {}", l, r);
          if l > r {1} else {0}
        },
        _ => 1000000,
      };
      Value::from_int(r)
    },
    Expression::SubScript{ref v, ref idx} => {
      let val = exec_expr(&*v, ctx);
      let i = exec_expr(&*idx, ctx);
      if i.vtype() != T_INT {
       Value::from_err("index must be integer".to_string())
      }
      else {
        match val.evalue() {
          EValue::Int(_ii) => Value::from_err("int cannot be indexed".to_string()),
          EValue::Str(data) => Value::from_int(data.clone().into_bytes()[i.as_int() as usize] as i32),
          EValue::Err(e) => Value::from_err(e.clone()),
          EValue::Fun(_o) => Value::from_err("functions cannot be indexed".to_string()),
          EValue::Obj(_o) => Value::from_err("objects cannot be indexed".to_string()),
          EValue::Vec(v) => if v.len() <= (i.as_int() as usize) || i.as_int() < 0 {
            Value::from_err("invalid index value".to_string())
          } else {
            v[i.as_int() as usize].clone()
          },
        }
      }
    },
    Expression::Array(ref exprlist) => {
      let mut v = List::new();
      for e in exprlist {
        v.push(exec_expr(e, ctx));
      }
      Value::from_vec(v)
    },
    Expression::FunctionCall{ref function, ref args} => {
      //println!("Entering function {}", function);
      let f = exec_expr(&*function, ctx);
      match f.evalue() {
        EValue::Fun(ref fun) => {
          if fun.formals.len() != args.len() {
            Value::from_err("wrong number of arguments".to_string())
          } else {
            let mut fctx = ExecContext{stack: Vec::new(), functions: Arc::clone(&ctx.functions), classes: Arc::clone(&ctx.classes)};
            fctx.stack.resize(fun.stack.len(), Value::from_int(0));
            for i in 0..args.len() {
              fctx.stack[i] = exec_expr(&*args[i], ctx);
            }
            for i in 0..fun.closure.len() {
              fctx.stack[fun.closurePut[i] as usize] = fun.closure[i].clone();
            }
            exec_block(&*fun.code, &mut fctx)
          }
        },
        _ => Value::from_err("call of not a function".to_string()),
      }
    },
    Expression::ObjDef{ref cls, ref init} => {
      if ctx.classes[cls].fields.len() != init.len() {
        Value::from_err(String::new() + "Initializer list has wrong size for " + cls)
      } else {
        let mut obj = Object::new(&*ctx.classes[cls]);
        for i in 0..init.len() {
          let v = exec_expr(&*init[i], ctx);
          obj.fields.push(v);
        }
        Value::from_obj(obj)
      }
    },
    Expression::Dot{ref lhs, ref rhs, ref cacheClass, ref cacheIndex} => {
      let val = exec_expr(&*lhs, ctx);
      match val.evalue() {
        EValue::Obj(o) => {
          if o.class as *const Class == *cacheClass {
            o.fields[*cacheIndex as usize].clone()
          } else {
            match o.class.fields.get(rhs) {
              Some(idx) => {
                *bearclaw(cacheClass) = o.class;
                *bearclaw(cacheIndex) = *idx;
                o.fields[*idx as usize].clone()
              },
              None => Value::from_err(String::new() + "missing field: " + rhs),
            }
          }
        },
        _ => Value::from_err("value left of '.' is not an object".to_string()),
      }
    },
    Expression::Lambda(ref f) => {
      let mut func = (**f).clone();
      for i in func.closureBuild.clone() /*DF?*/ {
        func.closure.push(ctx.stack[i as usize].clone());
      }
      Value::from_fun(func)
    }
  }
}

fn test_value() {
  let vi = Value::from_int(-42);
  let vs = Value::from_str("foo".to_string());
  let vv = Value::from_vec(vec![Value::from_int(1),Value::from_int(2),Value::from_int(3)]);
  println!("vi: {}", vi.as_int());
  println!("vs: {}", vs.as_str());
  println!("vv: {:?}", vv.as_vec());
  {
    let vi2 = vi.clone();
    let vs2 = vs.clone();
    let vv2 = vv.clone();
    println!("vi: {}", vi2.as_int());
    println!("vs: {}", vs2.as_str());
    println!("vv: {:?}", vv2.as_vec());
  }
  {
    let vi2 = vi.clone();
    let vs2 = vs.clone();
    let vv2 = vv.clone();
    println!("vi: {}", vi2.as_int());
    println!("vs: {}", vs2.as_str());
    println!("vv: {:?}", vv2.as_vec());
  }
}

use std::time::Instant;

fn main() {
  test_value();
  let mut state : HashMap<String, i32> = HashMap::new();
  let mut functions = Arc::new(Functions::new());
  let mut classes = Arc::new(Classes::new());
  let stdin = io::stdin();
  loop {
    let mut line = String::new();
    stdin.lock().read_line(&mut line).unwrap();
    //println!("Text:    {}", line);
    let sline = line.as_str();
    let pres = LParser::parse_str(Rule::toplevel, sline);
    if pres.is_err() {
      println!("Error: {}", pres.err().unwrap());
      continue;
    }
    let pairs = pres.unwrap();
    for pair in pairs {
      //println!("Rule:    {:?}", pair.as_rule());
      //println!("Span:    {:?}", pair.clone().into_span());
      println!("Text:    {}", pair.clone().into_span().as_str());
      /*for inner_pair in pair.into_inner() {
        println!("  Rule:    {:?}", inner_pair.as_rule());
      }*/
      let now = Instant::now();
      let val = process_toplevel(pair, &mut functions, &mut classes);
      println!("={:?}    in {:?}", val, Instant::now()-now);
      for (k, v) in &state {
        println!("{} = {:?}", k, v);
      }
    }
  }
}


/*
BASIC FIBO
func fibo(x) { if x > 2 { (fibo(x-1))+(fibo(x-2));} else {x;};}

FIBO ON OBJECT
class obj{x;}
func fibo(x) { if x.x > 2 { var va = fibo(obj{x.x-1}); var vb = fibo(obj{x.x-2});  obj{va.x+vb.x};} else {x;};}
func fibo(x) { if x.x > 2 { obj{(fibo(obj{x.x-1}).x) + fibo(obj{x.x-2}).x};} else {x;};}
z=fibo(obj{34}):
base 6.1s
cache index read: 5.1
cache index write: 5.1  <--dummy we don't use those but object initializer
python equivalent: 9s

*/