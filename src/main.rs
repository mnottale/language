//#![feature(underscore_lifetimes)]


extern crate pest;
#[macro_use]
extern crate pest_derive;


use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct LParser;

use std::collections::HashMap;
use std::io::{self, BufRead};

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
  /*
  if process_expr(expr.into_inner(), &mut state) != 0 {
    loop {
      let p = pairs.next();
      match p {
        Some(s) => process_statement(s, &mut state),
        None => break,
      }
    }
  }*/
  
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
    let pairs = LParser::parse_str(Rule::statement, sline).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      println!("Rule:    {:?}", pair.as_rule());
      println!("Span:    {:?}", pair.clone().into_span());
      println!("Text:    {}", pair.clone().into_span().as_str());
      /*for inner_pair in pair.into_inner() {
        println!("  Rule:    {:?}", inner_pair.as_rule());
      }*/
      process_statement(pair, &mut state);
      for (k, v) in &state {
        println!("{} = {}", k, v);
      }
    }
  }
}