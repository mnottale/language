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


lazy_static! {
  static ref variables : Mutex<HashMap<String, i32>> = Mutex::new(HashMap::new());
  //static ref functions : Arc<HashMap<String, Box<Function>>> = Arc::new(HashMap::new());
}

type Functions = HashMap<String, Box<Function>>;

enum Expression {
  Constant(i32),
  GlobalVariable(String),
  StackVariable(i32),
  Operator{lhs: Box<Expression>, rhs: Box<Expression>, op: String},
  FunctionCall{function: String},
}

enum Statement {
  GlobalAssignment{target: String, rhs: Box<Expression>},
  StackAssignment{target: i32, rhs: Box<Expression>},
  If{cond: Box<Expression>, block: Box<Block>},
  Expression(Box<Expression>),
}

// variable name -> stack index
type Stack = HashMap<String, i32>;

struct Function {
  code: Box<Block>,
  stack: Box<Stack>,
}

struct Block {
  statements: Vec<Box<Statement>>,
}

struct ParseContext {
  stack: Option<Box<Stack>>,
}

struct ExecContext {
  stack: Vec<i32>,
  functions: Arc<Functions>,
}

fn parse_variable(name: String, ctx: &mut ParseContext) -> Expression {
  match ctx.stack {
    None => Expression::GlobalVariable(name),
    Some(ref bcf) => match bcf.deref().get(&name) {
      None => Expression::GlobalVariable(name),
      Some(idx) => Expression::StackVariable(*idx)
    }
  }
}

fn parse_binary(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let noie = pairs.next().unwrap();
  let op = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let v1 = match noie.as_rule() {
    Rule::number => Expression::Constant(noie.into_span().as_str().to_string().parse::<i32>().unwrap()),
    Rule::ident =>  parse_variable(noie.into_span().as_str().to_string(), ctx),
    Rule::expr => parse_expr(noie.into_inner(), ctx),
    _ => Expression::Constant(1000000),
  };
  let v2 = parse_expr(expr.into_inner(), ctx);
  return Expression::Operator{lhs: Box::new(v1), rhs: Box::new(v2), op: op.into_span().as_str().to_string()}
}

fn parse_funccall(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let content = pairs.next().unwrap();
  return Expression::FunctionCall{function: content.into_span().as_str().to_string()};
}

fn parse_expr(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Expression {
  let content = pairs.next().unwrap();
  match content.as_rule() {
      Rule::binary => parse_binary(content.into_inner(), ctx),
      Rule::funccall => parse_funccall(content.into_inner(), ctx),
      Rule::number => Expression::Constant(content.into_span().as_str().to_string().parse::<i32>().unwrap()),
      Rule::ident =>  parse_variable(content.into_span().as_str().to_string(), ctx),
      Rule::expr => parse_expr(content.into_inner(), ctx),
      _ => Expression::Constant(1000001),
  }
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
      let ident = inner_pair.clone().into_inner().next().unwrap().into_span().as_str().to_string();
      match ctx.stack {
        None => {},
        Some(ref mut cf) => { let len = cf.deref().len(); cf.deref_mut().insert(ident, len as i32);}
      };
      parse_assign(inner_pair.into_inner(), ctx)
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
  Statement::If{cond: Box::new(cond), block: Box::new(block)}
}

fn parse_assign(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Statement {
  let ident = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let rhs = parse_expr(expr.into_inner(), ctx);
  let target = ident.clone().into_span().as_str().to_string();
  match ctx.stack {
    None => Statement::GlobalAssignment{target: target, rhs: Box::new(rhs)},
    Some(ref s) => match s.deref().get(&target) {
      None => Statement::GlobalAssignment{target: target, rhs: Box::new(rhs)},
      Some(idx) => Statement::StackAssignment{target: *idx, rhs: Box::new(rhs)},
    }
  }
}

fn parse_block(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, ctx: &mut ParseContext) -> Block {
  let mut res = Vec::new();
  for r in pairs {
    println!("blockstatement:    {}", r.clone().into_span().as_str());
    res.push(Box::new(parse_statement(r, ctx)));
  }
  return Block{statements: res};
}

fn process_toplevel(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>, mut funcs: &mut Arc<Functions>) -> i32 {
  let item = pair.into_inner().next().unwrap();
  let mut ctx: ParseContext = ParseContext{stack: None};
  match item.as_rule() {
    Rule::assign => exec_statement(&parse_assign(item.into_inner(), &mut ctx), &mut ExecContext{stack: Vec::new(), functions: Arc::clone(funcs)}),
    Rule::funccall => exec_expr(&parse_funccall(item.into_inner(), &mut ctx), &mut ExecContext{stack: Vec::new(), functions: Arc::clone(funcs)}),
    Rule::funcdef => {
      ctx.stack = Some(Box::new(Stack::new()));
      let mut comps = item.into_inner();
      let name = comps.next().unwrap();
      let body = comps.next().unwrap();
      let mut block = parse_block(body.into_inner(), &mut ctx);
      Arc::get_mut(&mut funcs).unwrap().insert(name.clone().into_span().as_str().to_string(),
        Box::new(Function{code: Box::new(block), stack: ctx.stack.unwrap()}));
      0
    },
    _ => -1000000,
  }
}

fn exec_statement(s: &Statement, ctx: &mut ExecContext) -> i32 {
  match *s {
    Statement::GlobalAssignment{ref target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      //println!("assigning {} to {}", val, target);
      let mut v = variables.lock().unwrap();
      let entry = v.entry(target.clone()).or_insert(0);
      *entry = val;
      val
    },
    Statement::StackAssignment{target, ref rhs} => {
      let val = exec_expr(&*rhs, ctx);
      ctx.stack[target as usize] = val;
      val
    }
    Statement::Expression(ref e) => {
      exec_expr(&*e, ctx)
    },
    Statement::If{ref cond, ref block} => {
      let cond = exec_expr(&*cond, ctx);
      let mut res: i32 = 0;
      if cond != 0 {
        res = exec_block(&*block, ctx);
      };
      res
    }
  }
}

fn exec_block(b: &Block, ctx: &mut ExecContext) -> i32 {
  let mut val: i32 = 0;
  for ref s in &b.statements {
    val = exec_statement(&*s, ctx)
  }
  return val;
}

fn exec_expr(e: &Expression, ctx: &mut ExecContext) -> i32 {
  match *e {
    Expression::Constant(c) => c,
    Expression::GlobalVariable(ref name) => variables.lock().unwrap()[name],
    Expression::StackVariable(idx) => ctx.stack[idx as usize],
    Expression::Operator {ref lhs, ref rhs, ref op} => {
      let l = exec_expr(&*lhs, ctx);
      let r = exec_expr(&*rhs, ctx);
      match op.as_str() {
        "+" => l+r,
        "-" => l-r,
        _ => 1000000,
      }
    },
    Expression::FunctionCall{ref function} => {
      println!("Entering function {}", function);
      let ref f = ctx.functions[function];
      let mut fctx = ExecContext{stack: Vec::new(), functions: Arc::clone(&ctx.functions)};
      fctx.stack.resize(f.stack.len(), 0);
      exec_block(f.deref().code.deref(), &mut fctx)
    }
  }
}

fn main() {
  let mut state : HashMap<String, i32> = HashMap::new();
  let mut functions = Arc::new(Functions::new());
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
      let val = process_toplevel(pair, &mut functions);
      println!("={}", val);
      for (k, v) in &state {
        println!("{} = {}", k, v);
      }
    }
  }
}