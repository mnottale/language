# What is language

Language is a demo of a scripting language written in Rust, written mainly to explore Rust and the various techniques used in implementing a language.

## Unboxed int and doubles, packed representation

Language supports unboxed 32bit integers and 63 bit floats (10 bit exponent) for maximum efficiency.

## Classes and objects

Objects are ref-counted and stored on the heap (with a limit of 65535 references per object).

## Parametric AST

Some language constructs are implemented by transforming them in other simpler constructs to minimize the complexity of the execution engine. For instance 'for' loops are rewritten using 'while'.

# A stack for local variables, with support for closure by value

Closure by reference is planed, using the 'slot' technique.

# Python bindings

Because I'm not going to implement a feature-complete stdlib :) . The following works fine:

    pymodule.re().compile("foo").match("foobar").group(0)

# Native function bindings

There is no need to implement wrappers for every Rust function we want exposed in the language, thanks to a Callable trait implemented for all function types taking/returning Convertible arguments.
