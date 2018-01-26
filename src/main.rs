//#![feature(underscore_lifetimes)]


extern crate cpython;

#[macro_use] extern crate maplit;

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
use std::cell::Cell;

use std::ops::Deref;
use std::ops::DerefMut;

use std::fmt::Debug;
mod value;

use value::Value;
use value::EValue;
use value::box_f64;
use value::unbox_f64;

use value::{T_INT, T_PRI, T_ARR};

mod callable;
use callable::Callable;
use callable::Convertible;

mod python;
use python::test_python;

mod libstdlib;
use libstdlib::load_stdlib;

lazy_static! {
  static ref variables : Mutex<HashMap<String, Value>> = Mutex::new(HashMap::new());
  static ref classes : Mutex<HashMap<String, Box<Class>>> = Mutex::new(HashMap::new());
}

pub fn get_class(cn: String) -> &'static Class {
  unsafe {
    &*(&**classes.lock().unwrap().get(&cn).unwrap() as *const Class)
  }
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
   "<func>".fmt(f);
   self.code.fmt(f)
 }
}

impl Debug for Block {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    self.statements.fmt(f)
    /*
    for i in 0..self.statements.len() {
      self[i].fmt(f)
    }
    Result::Ok(())*/
  }
}


type Functions = HashMap<String, Value>;
type ExprList = Vec<Box<Expression>>;
type IdentList = Vec<String>;

#[derive(Clone, Debug)]
enum Expression {
  Constant(i32),
  Constantf(f64),
  Constants(String),
  GlobalVariable(String),
  StackVariable(i32),
  Array(ExprList),
  Lambda(Box<Function>),
  Dot{lhs: Box<Expression>, rhs: String, cacheClass: Cell<*const Class>, cacheIndex: Cell<i32>},
  ObjDef{cls: String, init: Vec<Box<Expression>>},
  SubScript{v: Box<Expression>, idx: Box<Expression>},
  Operator{lhs: Box<Expression>, rhs: Box<Expression>, op: String},
  FunctionCall{function: Box<Expression>, args: Vec<Box<Expression>>},
}

#[derive(Clone,Debug)]
enum Statement {
  GlobalAssignment{target: String, rhs: Box<Expression>},
  StackAssignment{target: i32, rhs: Box<Expression>},
  GlobalIndexAssignment{target: String, rhs: Box<Expression>, index: Box<Expression>},
  StackIndexAssignment{target: i32, rhs: Box<Expression>, index: Box<Expression>},
  ObjAssignment{target: Box<Expression>, slot: String, rhs: Box<Expression>, cacheClass: Cell<*const Class>, cacheIndex: Cell<i32>},
  ObjIndexAssignment{target: Box<Expression>, slot: String, rhs: Box<Expression>, index: Box<Expression>, cacheClass: Cell<*const Class>, cacheIndex: Cell<i32>},
  If{cond: Box<Expression>, block: Box<Block>, blockelse: Option<Box<Block>>},
  While{cond: Box<Expression>, block: Box<Block>},
  Block(Box<Block>),
  Expression(Box<Expression>),
}

// variable name -> stack index
type Stack = HashMap<String, i32>;

pub struct Class {
  name: String,
  fields: HashMap<String, i32>,
  funcs: HashMap<String, Value>,
}

#[derive(Clone)]
pub struct Function {
  formals: Vec<String>,
  catchall: Option<String>,
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
  expressions: HashMap<String, Box<Expression>>, // for parametric asts
  statements: HashMap<String, Box<Statement>>,
  identifiers: HashMap<String, String>,
}

type Classes = HashMap<String, Box<Class>>;

struct ExecContext {
  stack: Vec<Value>,
  functions: Arc<Functions>,
}

fn parse_classdecl(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, _ctx: &mut ParseContext) -> Class {
  let name = pairs.next().unwrap().into_span().as_str().to_string();
  let mut fields = HashMap::new();
  for p in pairs {
    let fname = p.into_span().as_str().to_string();
    let l = fields.len();
    fields.insert(fname, l as i32);
  }
  Class{name: name, fields: fields, funcs: HashMap::new()}
}

fn parse_variable(mut name: String, ctx: &mut ParseContext) -> Expression {
  if name.get(0..1).unwrap() == "%" {
    name = ctx.identifiers[&name].clone();
  }
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
    Rule::string => {
      let val = noie.into_span().as_str().to_string();
      Expression::Constants(val[1..val.len()-1].to_string())
    },
    Rule::number =>  {
        let val = noie.into_span().as_str().to_string().parse::<f64>().unwrap();
        if val == val.floor() {
          Expression::Constant(val as i32)
        } else {
          Expression::Constantf(val)
        }
      },
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

fn parse_identlist(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, _ctx: &mut ParseContext) -> (IdentList, Option<String>) {
  let mut res = IdentList::new();
  let mut catchall = None;
  for p in pairs {
    match p.as_rule() {
      Rule::ident => res.push(p.into_span().as_str().to_string()),
      Rule::catchall => catchall = Some(p.into_inner().next().unwrap().into_span().as_str().to_string()),
      _ => unreachable!(),
    }
  }
  (res, catchall)
}

fn parse_identchain(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let first = pairs.next().unwrap().into_span().as_str().to_string();
  let mut init = parse_variable(first, ctx);
  for p in pairs {
    let next = p.into_span().as_str().to_string();
    init = Expression::Dot{lhs: Box::new(init), rhs: next, cacheClass: Cell::new(0 as *const Class), cacheIndex: Cell::new(0)};
  }
  init
}

fn parse_expr(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let content = pairs.next().unwrap();
  let mut v = match content.as_rule() {
      Rule::binary => parse_binary(content.into_inner(), ctx),
      Rule::number => {
        let val = content.into_span().as_str().to_string().parse::<f64>().unwrap();
        if val == val.floor() {
          Expression::Constant(val as i32)
        } else {
          Expression::Constantf(val)
        }
      },
      Rule::string => {
        let val = content.into_span().as_str().to_string();
        Expression::Constants(val[1..val.len()-1].to_string())
      },
      Rule::ident =>  parse_variable(content.into_span().as_str().to_string(), ctx),
      Rule::identchain => parse_identchain(content.into_inner(), ctx),
      Rule::expr => parse_expr(content.into_inner(), ctx),
      Rule::array => {
        Expression::Array(parse_exprlist(content.into_inner().next().unwrap().into_inner(), ctx))
      },
      Rule::objdef => parse_objdef(content.into_inner(), ctx),
      Rule::lambda => parse_lambda(content.into_inner(), ctx),
      Rule::parametric => {
        let pname = content.into_span().as_str().to_string();
        (*ctx.expressions[&pname]).clone()
      },
      _ => unreachable!(),
  };
  // parse subsequents [expr] or .ident or (exprlist)
  for p in pairs {
    v = match p.as_rule() {
      Rule::ident => Expression::Dot{lhs: Box::new(v), rhs: p.into_span().as_str().to_string(), cacheClass: Cell::new(0 as *const Class), cacheIndex: Cell::new(0)},
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
    Rule::block => Statement::Block(Box::new(parse_block(inner_pair.into_inner(), ctx))),
    Rule::assign => parse_assign(inner_pair.into_inner(), ctx),
    Rule::IF => parse_if(inner_pair.into_inner(), ctx),
    Rule::WHILE => parse_while(inner_pair.into_inner(), ctx),
    Rule::FORINT => parse_forint(inner_pair.into_inner(), ctx),
    Rule::FORC => parse_forc(inner_pair.into_inner(), ctx),
    Rule::expr => Statement::Expression(Box::new(parse_expr(inner_pair.into_inner(), ctx))),
    Rule::vardecl => {
      let mut vd = inner_pair.into_inner();
      let mut ident = vd.next().unwrap().into_span().as_str().to_string();
      if ident.get(0..1).unwrap() == "%" {
        println!("PARAMETRIC {}", ident);
        ident = ctx.identifiers[&ident].clone();
      }
      let mut idx = 0;
      match ctx.stack {
        None => {},
        Some(ref mut cf) => { let len = cf.deref().len(); cf.deref_mut().insert(ident, len as i32);idx = len;}
      };
      let rhs = parse_expr(vd.next().unwrap().into_inner(), ctx);
      Statement::StackAssignment{target: idx as i32, rhs: Box::new(rhs)}
    },
    Rule::parametric => {
      let pname = inner_pair.into_span().as_str().to_string();
      (*ctx.statements[&pname]).clone()
    },
    other => {println!("Unhandled:    {:?}", other); Statement::Expression(Box::new(Expression::Constant(100000)))},
  }
}

fn parse_while(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let expr = pairs.next().unwrap();
  let block = pairs.next().unwrap();
  let cond =  parse_expr(expr.into_inner(), ctx);
  let block = parse_block(block.into_inner(), ctx);
  Statement::While{cond: Box::new(cond), block: Box::new(block)}
}

fn parse_forint(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let ident = pairs.next().unwrap().into_span().as_str().to_string();
  let limit = parse_expr(pairs.next().unwrap().into_inner(), ctx);
  // We will likely use our counter variable 'ident' in the block, so we need
  // to create it before parsing block.
  match ctx.stack {
    Some(ref mut s) => {
      let l = s.len();
      s.insert(ident.clone(), l as i32);
    },
    _ => {}
  }
  let block = parse_block(pairs.next().unwrap().into_inner(), ctx);
  let p = "{%v = 0; while %v < $e { $b; %v = %v + 1;};}";
  let rule = LParser::parse_str(Rule::block, p);
  ctx.expressions.insert("$e".to_string(), Box::new(limit));
  ctx.statements.insert("$b".to_string(), Box::new(Statement::Block(Box::new(block))));
  ctx.identifiers.insert("%v".to_string(), ident);
  Statement::Block(Box::new(parse_block(rule.unwrap().next().unwrap().into_inner(), ctx)))
}

fn parse_forc(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let init = parse_statement(pairs.next().unwrap(), ctx);
  let limit = parse_expr(pairs.next().unwrap().into_inner(), ctx);
  let increment = parse_statement(pairs.next().unwrap(), ctx);
  let block = parse_block(pairs.next().unwrap().into_inner(), ctx);
  let p = "{$binit; while $e { $b; $inc;};}";
  let rule = LParser::parse_str(Rule::block, p);
  ctx.expressions.insert("$e".to_string(), Box::new(limit));
  ctx.statements.insert("$b".to_string(), Box::new(Statement::Block(Box::new(block))));
  ctx.statements.insert("$binit".to_string(), Box::new(init));
  ctx.statements.insert("$inc".to_string(), Box::new(increment));
  Statement::Block(Box::new(parse_block(rule.unwrap().next().unwrap().into_inner(), ctx)))
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
  match pairs.next() {
    None => {
      let rrhs = parse_expr(expr.into_inner(), ctx);
      match ident {
        Expression::GlobalVariable(v) => Statement::GlobalAssignment{target: v, rhs: Box::new(rrhs)},
        Expression::StackVariable(v) => Statement::StackAssignment{target: v, rhs: Box::new(rrhs)},
        Expression::Dot{lhs, rhs, cacheClass, cacheIndex} => Statement::ObjAssignment{target: lhs, slot: rhs, rhs: Box::new(rrhs), cacheClass: Cell::new(0 as *const Class), cacheIndex: Cell::new(0)},
        _ => {unreachable!()},
        }
      },
    Some(e) => {
      let index = parse_expr(expr.into_inner(), ctx);
      let rrhs = parse_expr(e.into_inner(), ctx);   
      match ident {
        Expression::GlobalVariable(v) => Statement::GlobalIndexAssignment{target: v, rhs: Box::new(rrhs), index: Box::new(index)},
        Expression::StackVariable(v) => Statement::StackIndexAssignment{target: v, rhs: Box::new(rrhs), index: Box::new(index)},
        Expression::Dot{lhs, rhs, cacheClass, cacheIndex} => Statement::ObjIndexAssignment{target: lhs, slot: rhs, rhs: Box::new(rrhs), index: Box::new(index), cacheClass: Cell::new(0 as *const Class), cacheIndex: Cell::new(0)},
        _ => {unreachable!()},

      }
    }
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
  let (formals, catchall) = parse_identlist(pformals.into_inner(), ctx);
  let mut stack = Stack::new();
  for i in 0..formals.len() {
    stack.insert(formals[i].clone(), i as i32);
  }
  match catchall {
    Some(ref v) => {
      let l = stack.len();
      stack.insert(v.clone(), l as i32);
    }
    None => {}
  }
  let body = pairs.next().unwrap();
  let mut newctx = ParseContext{stack: Some(Box::new(stack)), parentStack: ctx.stack.clone(), close: HashMap::new(), expressions: HashMap::new(), statements: HashMap::new(), identifiers: HashMap::new()};
  let mut block = parse_block(body.into_inner(), &mut newctx);
  let mut closureBuild = Vec::new();
  let mut closurePut = Vec::new();
  for (p, s) in newctx.close {
    closureBuild.push(p);
    closurePut.push(s);
  }
  Expression::Lambda(Box::new(Function{
      formals: formals,
      catchall: catchall,
      code: Box::new(block),
      stack: newctx.stack.unwrap(),
      closureBuild: closureBuild,
      closurePut: closurePut,
      closure: Vec::new(),
  }))
}

fn process_toplevel(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>, mut funcs: &mut Arc<Functions>) -> Value {
  let item = pair.into_inner().next().unwrap();
  let mut ctx: ParseContext = ParseContext{stack: None, parentStack: None, close: HashMap::new(), expressions: HashMap::new(), statements: HashMap::new(), identifiers: HashMap::new()};
  match item.as_rule() {
    Rule::assign => {
      let ast = parse_assign(item.into_inner(), &mut ctx);
      println!("AST {:?}", ast);
      exec_statement(&ast, &mut ExecContext{stack: Vec::new(), functions: Arc::clone(funcs)})
    },
    Rule::memfuncdef => {
      
      let mut comps = item.into_inner();
      let rcname = comps.next().unwrap();
      let cname = rcname.into_span().as_str().to_string();
      let rname = comps.next().unwrap();
      let name = rname.into_span().as_str().to_string();
      let pformals = comps.next().unwrap();
      let (formals, catchall) = parse_identlist(pformals.into_inner(), &mut ctx);
      let mut stack = Stack::new();
      for i in 0..formals.len() {
        stack.insert(formals[i].clone(), i as i32);
      }
      match catchall {
        Some(ref v) => {
          let l = stack.len();
          stack.insert(v.clone(), l as i32);
        }
        None => {}
      }
      ctx.stack = Some(Box::new(stack));
      let body = comps.next().unwrap();
      let mut block = parse_block(body.into_inner(), &mut ctx);
      let vf = Value::from_fun(Function{
        formals: formals,
        catchall: catchall,
        code: Box::new(block),
        stack: ctx.stack.unwrap(),
        closure: Vec::new(),
        closureBuild:Vec::new(),
        closurePut: Vec::new()
      });
      classes.lock().unwrap().get_mut(&cname).unwrap().funcs.insert(name, vf);
      Value::from_int(0)
    },
    Rule::classdecl => {
      let cls = parse_classdecl(item.into_inner(), &mut ctx);
      let cname = cls.name.clone();
      classes.lock().unwrap().insert(cls.name.clone(), Box::new(cls));
      Value::from_str(cname)
    }
    _ => Value::from_int(-1000000),
  }
}

fn assign_index(v: &mut Value, idx: &Value, val: Value) -> Value {
	if idx.vtype() != T_INT {
		return Value::from_err("Index is not an int".to_string());
	}
	if v.vtype() != T_ARR {
		return Value::from_err("Variable is not an array".to_string());
	}
	let arr = v.as_vec();
	if (arr.len() as i32) <= idx.as_int() || idx.as_int() < 0 {
		return Value::from_err("Invalid index".to_string());
	}
	arr[idx.as_int() as usize] = val.clone();
	val
}

fn exec_statement(s: &Statement, ctx: &mut ExecContext) -> Value {
  match *s {
    Statement::GlobalAssignment{ref target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      let mut v = variables.lock().unwrap();
      let entry = v.entry(target.clone()).or_insert(Value::from_int(0));
      *entry = val;
      (*entry).clone()
    },
    Statement::GlobalIndexAssignment{ref target, ref rhs, ref index} => {
    	let val = exec_expr(&*rhs, ctx);
    	let idx = exec_expr(&*index, ctx);
      let mut v = variables.lock().unwrap();
      let entry = v.entry(target.clone()).or_insert(Value::from_int(0));
      assign_index(&mut *entry, &idx, val)
    },
    Statement::StackAssignment{target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      ctx.stack[target as usize] = val;
      ctx.stack[target as usize].clone()
    }
    Statement::StackIndexAssignment{target, ref rhs, ref index} => {
    	let val = exec_expr(&*rhs, ctx);
    	let idx = exec_expr(&*index, ctx);
    	assign_index(&mut ctx.stack[target as usize], &idx, val)
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
    Statement::While{ref cond, ref block} => {
      let mut val = Value::from_int(0);
      while true {
        let cond = exec_expr(&*cond, ctx);
        match cond.to_bool() {
          false => return val,
          true => val = exec_block(&*block, ctx),
        }
      }
      val
    },
    Statement::Block(ref block) => {
      exec_block(&*block, ctx)
    },
    Statement::ObjAssignment{ref target, ref slot, ref rhs, ref cacheClass, ref cacheIndex} => {
      let tgt = exec_expr(&*target, ctx);
      let v = exec_expr(&*rhs, ctx);
      match tgt.evalue() {
        EValue::Obj(o) => {
          if o.class as *const Class == cacheClass.get() {
            o.fields[cacheIndex.get() as usize] = v.clone();
            v
          } else {
            match o.class.fields.get(slot) {
              Some(idx) => {
                cacheClass.set(o.class);
                cacheIndex.set(*idx);
                o.fields[*idx as usize] = v.clone();
                v
              },
              None => Value::from_err(String::new() + "no such field: " + &slot),
            }
          }
        },
        _ => Value::from_err("assignment to field of non-object".to_string()),
      }
    },
    Statement::ObjIndexAssignment{ref target, ref slot, ref rhs, ref index, ref cacheClass, ref cacheIndex} => {
      let tgt = exec_expr(&*target, ctx);
      let v = exec_expr(&*rhs, ctx);
      let aidx = exec_expr(&*index, ctx);
      match tgt.evalue() {
        EValue::Obj(o) => {
          if o.class as *const Class == cacheClass.get() {
          	assign_index(&mut o.fields[cacheIndex.get() as usize], &aidx, v)
          } else {
            match o.class.fields.get(slot) {
              Some(idx) => {
                cacheClass.set(o.class);
                cacheIndex.set(*idx);
                assign_index(&mut o.fields[cacheIndex.get() as usize], &aidx, v)
              },
              None => Value::from_err(String::new() + "no such field: " + &slot),
            }
          }
        },
        _ => Value::from_err("assignment to field of non-object".to_string()),
      }
    },
  }
}

fn exec_block(b: &Block, ctx: &mut ExecContext) -> Value {
  let mut val: Value = Value::from_int(0);
  for ref s in &b.statements {
    val = exec_statement(&*s, ctx)
  }
  return val;
}

fn exec_op_f (v1: f64, v2: f64, op: &str) -> Value {
  match op {
    "+" => Convertible::to_value(v1 + v2),
    "-" => Convertible::to_value(v1 - v2),
    "/" => Convertible::to_value(v1 / v2),
    "*" => Convertible::to_value(v1 * v2),
    "%" => Convertible::to_value(v1 % v2),
    "<" => { if v1 < v2 {Value::from_int(1)} else {Value::from_int(0)}},
    ">" => { if v1 > v2 {Value::from_int(1)} else {Value::from_int(0)}},
    "==" => { if v1 == v2 {Value::from_int(1)} else {Value::from_int(0)}},
    _ => Value::from_err("Operator not implemented".to_string()),
  }
}

fn exec_op_i (v1: i32, v2: i32, op: &str) -> Value {
  match op {
    "+" => Convertible::to_value(v1 + v2),
    "-" => Convertible::to_value(v1 - v2),
    "/" => Convertible::to_value(v1 / v2),
    "*" => Convertible::to_value(v1 * v2),
    "%" => Convertible::to_value(v1 % v2),
    "<" => { if v1 < v2 {Value::from_int(1)} else {Value::from_int(0)}},
    ">" => { if v1 > v2 {Value::from_int(1)} else {Value::from_int(0)}},
    "==" => { if v1 == v2 {Value::from_int(1)} else {Value::from_int(0)}},
    "<<" => Convertible::to_value(v1 << v2),
    ">>" => Convertible::to_value(v1 >> v2),
    _ => Value::from_err("Operator not implemented".to_string()),
  }
}

fn exec_funcall(fun: &Function, args: Vec<Value>, ctx: &mut ExecContext) -> Value{
  //let &mut fun = f.as_fun();
  if fun.catchall.is_none() {
    if fun.formals.len() != args.len() {
      return Value::from_err("wrong number of arguments".to_string());
    }
    let mut fctx = ExecContext{stack: args, functions: Arc::clone(&ctx.functions)};
    fctx.stack.resize(fun.stack.len(), Value::from_int(0));
    for i in 0..fun.closure.len() {
      fctx.stack[fun.closurePut[i] as usize] = fun.closure[i].clone();
    }
    exec_block(&*fun.code, &mut fctx)
  } else {
    if fun.formals.len() > args.len() {
      return Value::from_err("wrong number of arguments".to_string());
    }
    let mut fctx = ExecContext{stack: Vec::new(), functions: Arc::clone(&ctx.functions)};
    fctx.stack.resize(fun.stack.len(), Value::from_int(0));
    for i in 0..fun.formals.len() {
      fctx.stack[i] = args[i].clone();
    }
    let mut ca = Vec::new();
    for i in fun.formals.len()..args.len() {
      ca.push(args[i].clone());
    }
    // catchall is always right after formals in stack
    fctx.stack[fun.formals.len()] = Value::from_vec(ca);
    for i in 0..fun.closure.len() {
      fctx.stack[fun.closurePut[i] as usize] = fun.closure[i].clone();
    }
    exec_block(&*fun.code, &mut fctx)
  }
}


fn exec_expr(e: &Expression, ctx: &mut ExecContext) -> Value {
  match *e {
    Expression::Constant(c) => Value::from_int(c),
    Expression::Constantf(c) => Value::from_flt(c),
    Expression::Constants(ref s) => Value::from_str(s.clone()),
    Expression::GlobalVariable(ref name) => {
      match variables.lock().unwrap().get(name) {
        Some(v) => v.clone(),
        None => Value::from_err(String::new() + "no such variable " + name),
      }
    },
    Expression::StackVariable(idx) => ctx.stack[idx as usize].clone(),
    Expression::Operator {ref lhs, ref rhs, ref op} => {
      let l = exec_expr(&*lhs, ctx);
      let r = exec_expr(&*rhs, ctx);
      match (l.evalue(), r.evalue()) {
        (EValue::Int(a), EValue::Int(b)) => exec_op_i(a, b, op.as_str()),
        (EValue::Flt(a), EValue::Flt(b)) => exec_op_f(a, b, op.as_str()),
        (EValue::Flt(a), EValue::Int(b)) => exec_op_f(a, b as f64, op.as_str()),
        (EValue::Int(a), EValue::Flt(b)) => exec_op_f(a as f64, b, op.as_str()),
        _ => Value::from_err("Operator not supported on this types".to_string()),
      }
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
          EValue::Flt(_ii) => Value::from_err("float cannot be indexed".to_string()),
          EValue::Str(data) => Value::from_int(data.clone().into_bytes()[i.as_int() as usize] as i32),
          EValue::Err(e) => Value::from_err(e.clone()),
          EValue::Fun(_o) => Value::from_err("functions cannot be indexed".to_string()),
          EValue::Pri(_o) => Value::from_err("primitives cannot be indexed".to_string()),
          EValue::Obj(_o) => Value::from_err("objects cannot be indexed".to_string()),
          EValue::Box(_o) => Value::from_err("boxed cannot be indexed".to_string()),
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
      let f = exec_expr(&*function, ctx);
      //println!("Entering function {:?}", f);
      match f.evalue() {
        EValue::Fun(ref fun) => {
          let mut vargs = Vec::new();
          for i in 0..args.len() {
            vargs.push(exec_expr(&*args[i], ctx));
          }
          exec_funcall(fun, vargs, ctx)
        },
        EValue::Pri(ref c) => {
          let mut a = Vec::new();
          for i in 0..args.len() {
              a.push(exec_expr(&*args[i], ctx));
          }
          c.call(a)
        }
        EValue::Vec(ref v) => {
          if v[0].vtype() == T_PRI {
            let mut a = Vec::new();
            for i in 1..v.len() {
              a.push(v[i].clone());
            }
            for i in 0..args.len() {
              a.push(exec_expr(&*args[i], ctx));
            }
            v[0].as_pri().call(a)
          } else {
            let mut vargs = Vec::new();
            for i in 1..v.len() {
              vargs.push(v[i].clone());
            }
            for i in 0..args.len() {
              vargs.push(exec_expr(&*args[i], ctx));
            }
            exec_funcall(v[0].as_fun(), vargs, ctx)
          }
        }
        _ => Value::from_err("call of not a function".to_string()),
      }
    },
    Expression::ObjDef{ref cls, ref init} => {
      if classes.lock().unwrap()[cls].fields.len() != init.len() {
        Value::from_err(String::new() + "Initializer list has wrong size for " + cls)
      } else {
        let mut obj = Object::new(&*classes.lock().unwrap()[cls]);
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
          if o.class as *const Class == cacheClass.get() {
            o.fields[cacheIndex.get() as usize].clone()
          } else {
            match o.class.fields.get(rhs) {
              Some(idx) => {
                cacheClass.set(o.class);
                cacheIndex.set(*idx);
                return o.fields[*idx as usize].clone();
              },
              None => {}
            }
            // try functions
            match o.class.funcs.get(rhs) {
              Some(ref f) => return Value::from_vec(vec![(*f).clone(), val.clone()]),
              None => {}
            }
            // try fallback
            match o.class.funcs.get(&"fallback".to_string()) {
              Some(ref f) => return Value::from_vec(vec![(*f).clone(), val.clone(), Value::from_str(rhs.clone())]),
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
  println!("{} = \n{}", 63.2345, unbox_f64(box_f64(63.2345)));
  println!("{} = \n{}", 0.5, unbox_f64(box_f64(0.5)));
  println!("{} = \n{}", 632345.0, unbox_f64(box_f64(632345.0)));
  println!("{} = \n{}", 1e1000, unbox_f64(box_f64(1e1000)));
  println!("{} = \n{}", -1e1000, unbox_f64(box_f64(-1e1000)));
   println!("{} = \n{}", 1e300, unbox_f64(box_f64(1e300)));
   println!("{} = \n{}", 1e-50, unbox_f64(box_f64(1e-50)));
  println!("{} = \n{}", 1e-300, unbox_f64(box_f64(1e-300)));
  println!("{} = \n{}", 0.0/0.0, unbox_f64(box_f64(0.0/0.0)));
}

use std::time::Instant;

fn main() {
  //test_value();
  test_python();
  let mut state : HashMap<String, i32> = HashMap::new();
  let mut functions = Arc::new(Functions::new());
  load_stdlib(&mut variables.lock().unwrap(), &mut *classes.lock().unwrap());
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
      let val = process_toplevel(pair, &mut functions);
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


BENCH binary-trees
class Tree{l;r;}
makeTree = func(d) { if d > 0 { Tree{makeTree(d-1),makeTree(d-1)};} else { Tree{0, 0};};}
checksumTree = func(t) { if t.l == 0 { 1;} else { (1 + (checksumTree(t.l)) + checksumTree(t.r));};}
step = func(d, c) { var res = 0; forint i in c { res = res + checksumTree(makeTree(d));};res;}
do = func(maxdepth) { for var i=4;i < maxdepth; i = i+2 { var cnt = 1 << ((maxdepth + 4) - i); var ck = step(i, cnt); print(i);print(cnt); print(ck); print("--");};}
bench = func(maxdepth) { print(checksumTree(makeTree(maxdepth+1))); var lltree = makeTree(maxdepth); do(maxdepth);print(checksumTree(lltree));}

*/
