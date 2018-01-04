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

lazy_static! {
  static ref variables : Mutex<HashMap<String, i32>> = Mutex::new(HashMap::new());
  static ref functions : Mutex<HashMap<String, Box<Block>>> = Mutex::new(HashMap::new());
}

enum Expression {
  Constant(i32),
  Variable(String),
  Operator{lhs: Box<Expression>, rhs: Box<Expression>, op: String},
  FunctionCall{function: String},
}

enum Statement {
  Assignment{target: String, rhs: Box<Expression>},
  If{cond: Box<Expression>, block: Box<Block>},
  Expression(Box<Expression>),
}

struct Block {
  statements: Vec<Box<Statement>>,
}

fn parse_binary(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Expression {
  let noie = pairs.next().unwrap();
  let op = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let v1 = match noie.as_rule() {
    Rule::number => Expression::Constant(noie.into_span().as_str().to_string().parse::<i32>().unwrap()),
    Rule::ident =>  Expression::Variable(noie.into_span().as_str().to_string()),
    Rule::expr => parse_expr(noie.into_inner()),
    _ => Expression::Constant(1000000),
  };
  let v2 = parse_expr(expr.into_inner());
  return Expression::Operator{lhs: Box::new(v1), rhs: Box::new(v2), op: op.into_span().as_str().to_string()}
}

fn parse_funccall(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Expression {
  let content = pairs.next().unwrap();
  return Expression::FunctionCall{function: content.into_span().as_str().to_string()};
}

fn parse_expr(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Expression {
  let content = pairs.next().unwrap();
  match content.as_rule() {
      Rule::binary => parse_binary(content.into_inner()),
      Rule::funccall => parse_funccall(content.into_inner()),
      Rule::number => Expression::Constant(content.into_span().as_str().to_string().parse::<i32>().unwrap()),
      Rule::ident =>  Expression::Variable(content.into_span().as_str().to_string()),
      Rule::expr => parse_expr(content.into_inner()),
      _ => Expression::Constant(1000001),
  }
}

fn parse_statement(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>) -> Statement {
  let inner_pair = pair.into_inner().next().unwrap();
  //println!("  Rule:    {:?}", inner_pair.as_rule());
  //process_statement(inner_pair, &mut state);
  match inner_pair.as_rule() {
    Rule::assign => parse_assign(inner_pair.into_inner()),
    Rule::IF => parse_if(inner_pair.into_inner()),
    Rule::expr => Statement::Expression(Box::new(parse_expr(inner_pair.into_inner()))),
    other => {println!("Unhandled:    {:?}", other); Statement::Expression(Box::new(Expression::Constant(100000)))},
  }
}

fn parse_if(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Statement {
  let expr = pairs.next().unwrap();
  let block = pairs.next().unwrap();
  println!("Block:    {:?}", block.as_rule());
  println!("Block:    {}", block.clone().into_span().as_str());
  let cond =  parse_expr(expr.into_inner());
  let block = parse_block(block.into_inner());
  Statement::If{cond: Box::new(cond), block: Box::new(block)}
}

fn parse_assign(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Statement {
  let ident = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let rhs = parse_expr(expr.into_inner());
  let target = ident.clone().into_span().as_str().to_string();
  return Statement::Assignment{target: target, rhs: Box::new(rhs)};
}

fn parse_block(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>) -> Block {
  let mut res = Vec::new();
  for r in pairs {
    println!("blockstatement:    {}", r.clone().into_span().as_str());
    res.push(Box::new(parse_statement(r)));
  }
  return Block{statements: res};
}

fn process_toplevel(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>) -> i32 {
  let item = pair.into_inner().next().unwrap();
  match item.as_rule() {
    Rule::assign => exec_statement(&parse_assign(item.into_inner())),
    Rule::funccall => exec_expr(&parse_funccall(item.into_inner())),
    Rule::funcdef => {
      let mut comps = item.into_inner();
      let name = comps.next().unwrap();
      let body = comps.next().unwrap();
      let block = parse_block(body.into_inner());
      functions.lock().unwrap().insert(name.clone().into_span().as_str().to_string(), Box::new(block));
      0
    },
    _ => -1000000,
  }
}

fn exec_statement(s: &Statement) -> i32 {
  match *s {
    Statement::Assignment{ref target, ref rhs} => {
      let val = exec_expr(&*rhs);
      //println!("assigning {} to {}", val, target);
      let mut v = variables.lock().unwrap();
      let entry = v.entry(target.clone()).or_insert(0);
      *entry = val;
      val
    },
    Statement::Expression(ref e) => {
      exec_expr(&*e)
    },
    Statement::If{ref cond, ref block} => {
      let cond = exec_expr(&*cond);
      let mut res: i32 = 0;
      if cond != 0 {
        res = exec_block(&*block);
      };
      res
    }
  }
}

fn exec_block(b: &Block) -> i32 {
  let mut val: i32 = 0;
  for ref s in &b.statements {
    val = exec_statement(&*s)
  }
  return val;
}

fn exec_expr(e: &Expression) -> i32 {
  match *e {
    Expression::Constant(c) => c,
    Expression::Variable(ref name) => variables.lock().unwrap()[name],
    Expression::Operator {ref lhs, ref rhs, ref op} => {
      let l = exec_expr(&*lhs);
      let r = exec_expr(&*rhs);
      match op.as_str() {
        "+" => l+r,
        "-" => l-r,
        _ => 1000000,
      }
    },
    Expression::FunctionCall{ref function} => {
      exec_block(&*functions.lock().unwrap()[function])
    }
  }
}


fn process_binary(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) -> i32 {
  let noie = pairs.next().unwrap();
  let op = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let v1 = match noie.as_rule() {
    Rule::number => noie.into_span().as_str().to_string().parse::<i32>().unwrap(),
    Rule::ident =>  state[&noie.into_span().as_str().to_string()],
    Rule::expr => process_expr(noie.into_inner(), &mut state),
    default => 1000000,
  };
  let v2 = process_expr(expr.into_inner(), &mut state);
  match op.into_span().as_str() {
    "+" => v1 + v2,
    "-" => v1 - v2,
    default => 1000000,
  }
}

fn process_expr(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) -> i32 {
  let content = pairs.next().unwrap();
  match content.as_rule() {
      Rule::binary => process_binary(content.into_inner(), &mut state),
      Rule::number => content.into_span().as_str().to_string().parse::<i32>().unwrap(),
      Rule::ident =>  state[&content.into_span().as_str().to_string()],
      Rule::expr => process_expr(content.into_inner(), &mut state),
      default => -1,
  }
}
fn process_assign(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) {
  // ident expr
  let ident = pairs.next().unwrap();
  let expr = pairs.next().unwrap();
  let val = process_expr(expr.into_inner(), &mut state);
  let entry = state.entry(ident.clone().into_span().as_str().to_string()).or_insert(0);
  *entry = val;
}

fn process_block(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) {
  for r in pairs {
    println!("blockstatement:    {}", r.clone().into_span().as_str());
    process_statement(r, &mut state)
  }
}

fn process_if(mut pairs: pest::iterators::Pairs<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) {
  let expr = pairs.next().unwrap();
  let block = pairs.next().unwrap();
  println!("Block:    {:?}", block.as_rule());
  println!("Block:    {}", block.clone().into_span().as_str());
  if process_expr(expr.into_inner(), &mut state) != 0 {
    process_block(block.into_inner(), &mut state)
  }
}

  
fn process_statement(pair: pest::iterators::Pair<Rule, pest::inputs::StrInput>, mut state: &mut HashMap<String, i32>) {
  for inner_pair in pair.into_inner() {
    //println!("  Rule:    {:?}", inner_pair.as_rule());
    //process_statement(inner_pair, &mut state);
    match inner_pair.as_rule() {
      Rule::assign => process_assign(inner_pair.into_inner(), &mut state),
      Rule::IF => process_if(inner_pair.into_inner(), &mut state),
      default => {println!("Unhandled:    {:?}", default)},
    }
  }
}

fn main() {
  let mut state : HashMap<String, i32> = HashMap::new();
  
  let stdin = io::stdin();
  loop {
    let mut line = String::new();
    stdin.lock().read_line(&mut line).unwrap();
    //println!("Text:    {}", line);
    let sline = line.as_str();
    let pairs = LParser::parse_str(Rule::toplevel, sline).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      //println!("Rule:    {:?}", pair.as_rule());
      //println!("Span:    {:?}", pair.clone().into_span());
      println!("Text:    {}", pair.clone().into_span().as_str());
      /*for inner_pair in pair.into_inner() {
        println!("  Rule:    {:?}", inner_pair.as_rule());
      }*/
      let val = process_toplevel(pair);
      println!("={}", val);
      for (k, v) in &state {
        println!("{} = {}", k, v);
      }
    }
  }
}