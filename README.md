# Macquarie University, Department of Computing

## COMP332 Programming Languages 2018

## Lintilla, a simple functional programming language.

### Introduction

[Lintilla](http://hitchhikers.wikia.com/wiki/Lintilla) is a language that contains elements from mainstream functional languages such as [ML](https://en.wikipedia.org/wiki/Standard_ML), [Haskell](https://www.haskell.org/), [Scala](https://www.scala-lang.org/) and [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)). It uses a syntax that has been borrowed from Dom's favourite systems programming language [Rust](https://www.rust-lang.org/).

The description here provides a brief overview of the Lintilla language and its syntax. Further detail about aspects such as checking the validity of names or types and translating a program into an executable form will be added when we work on the Lintilla code generator in assignment 3.

### The Lintilla language

The basic unit of a Lintilla program is the *expression*. There are no statements, their place is taken by expressions whose type is the special type `unit`. You can think of unit as a type which has only one value, called `()`, and it is this value that is returned by those constructs which otherwise might be regarded as returning nothing (like a procedure or a `while` loop).

A program is essentially just a single expression. For example, here is a simple program that returns the result of a simple arithmetic operation:

    2 + 3 * 4

When this program is run it will print the value of the expression: the number
14.

*Block expressions* are used to build programs out of smaller expressions. A block expression is a pair of curly braces containing zero or more expressions separated by semicolons (`;`). The idea is that when a block is executed each expression in that block is executed in turn, and the value computed by the last expression becomes the value of that block. For example, here is a program consisting of a block expression that declares two variables and uses their values in the expression whose values if returned from the block:

    {
       let a = 5;
       let b = a + 1;
       a * b
    }

This program will print the result of multiplying `a` by `b`, so 30 will be printed. Here the name `a` can be used in the definition of `b` since `b` is defined later, but that is a name analysis issue, so we don't need to worry about it for the moment. Notice also that the semicolon is used as an expression separator, not a line terminator; so the last line in this block isn't followed by a semicolon. An empty block `{}` returns the unique value `()` of type `unit`.

A Lintilla program is a file with name ending `.lin` which we can think of as a single block. It isn't surrounded by curly braces, because the beginning and end of the file serve the same purpose, but each expression in the file must be separated from the next by a semicolon. It is the value computed by the last expression in the file that is returned as the result of the program, and printed to the terminal.

*Variables* come in two flavours *mutable* and *immutable*. The default flavour of variable is immutable, these are bound to a value when they are declared and they maintain the same value from there to the point where they go out of scope. An immutable variable is declared and bound to a value in a `let` expression:

    let doms_var = 10;

We can also declare a mutable variable; think of these as being bound to a memory cell that contains a value that can be changed. Mutable variables are bound and their values are initialised in `let mut` expressions:

    let mut doms_mutable_var = 20;

The value stored in the memory cell bound to a mutable variable can be changed using an assignment expression (`:=`):

    doms_mutable_var := doms_mutable_var + 1;

The scope of a variable extends from the point where it is declared to the end of the smallest enclosing block. Here again, however, this is a matter to handled by the semantic analysis phase of the compiler, which we will consider in a later assignment.

*Functions* may also be declared, both at top level or within a block. For example here is the familiar factorial function given as a recursive function in Lintilla:

    fn factorial(n: int) -> int {
      if n = 0 { 1 } else { n * factorial(n-1) }
    }

This declaration defines a new function which takes a single parameter of type `int` and returns an `int` as its result. It introduces a new immutable variable called `factorial` which, as a first order approximation, we can think of as pointing to the code generated by compiling the function body. The result returned by a function is the value returned when its body, a block, is executed.

Functions may take more than one parameter:

    fn mod(n: int, m: int) -> int {
      n - (n / m) * m
    };
    
    mod(23,10)  // program prints `3`, the remainder when `23` is divided by `10`.

A *procedure* is a function which returns no value, or more precisely it returns the unique value `()` of type `unit`. Procedures are declared by omitting the return type specification in a `fn` declaration:

    let mut x = 20;

    fn add_n_to_x(n : int) {
      x := x + n
    };

    add_n_to_x(23);    // now the mutable variable `x` contains the value `43`
    
    x                  // program prints the value `43`

The same effect can be gained by using the the explicit return type `unit`:

    fn add_n_to_x(n : int) -> unit {
      x := x + n
    }

The scope of a function name extends **throughout** the smallest block enclosing its declaration. This seems like a strange rule, but it allows us to declare (mutually) recursive functions

    fn f(n : int) -> int {
      g(n-3)
    };
    
    fn g(n : int) -> int {
      if 0 < n { f(2*n) } else { n }
    }
    
    // Even though `g` is declared at a point after the declaration of `f` we can
    // still refer to it in the body of `f`.
    
We do not distinguish declarations from other kinds of expressions for the purposes of parsing. That doesn't mean that it will always make sense to use a declaration wherever any other kind of expression is appropriate, but just that this isn't a decision that the parser will make. The places where declarations can legally occur will be determined, and enforced, by the Lintilla type checker.
    
*Control expressions* we have already seen the principle control expression provided by Lintilla, the `if` expression. We might note that the language specifies that `if` expressions must have both a _then_ and an _else_ clause and that these must both be blocks enclosed in curly braces.

The language also provides `while` loops:

    fn fibonacci(n : int) -> int {
      let mut res1 = 0;
      let mut res2 = 1;
      let mut count = n;
      
      while 0 < count {
        let temp = res1;
        res1 := res2;
        res2 := temp + res1;
        count := count - 1
      };
      
      res1
    }

Here again the body of a `while` must be a block enclosed in curly braces.

Finally Lintilla also has a `return` expression, which can be used to exit early from a block. A `return` may optionally take a parameter, and it is the value of that parameter which is returned as the result of the block expression when that `return` is executed. For example, in our Fibonacci function we might want to exit from the function early if the parameter passed to that function is negative:

    fn fibonacci(n : int) -> int {
      if n < 0 { return -1 } else {}; // If `n` is negative return the error value -1.
      
      let mut res1 = 0;
      let mut res2 = 1;
      let mut count = n;
      
      while 0 < count {
        let temp = res1;
        res1 := res2;
        res2 := temp + res1;
        count := count - 1
      };
      
      res1
    }


*Expression* forms are interchangeable as long as they have the correct type. E.g., anywhere we can put a number can also take a block or some other kind of expression that evaluates to a number. For example, here is an artificial program that uses blocks nested inside an arithmetic operation:

    {
      let a = 3;
      a + 1
    } *
    {
      let b = 10;
      b - 1
    }
    
Or more concisely:

    { let a = 3; a + 1 } * { let b = 10; b - 1 }

We've seen a few different forms of expression: numbers, addition expressions, multiplication expressions and function call expressions. There are also other arithmetic operations, boolean values, boolean literals, relational and logical operators, and conditional expressions. The complete syntax of Lintilla is given below.

Finally Lintilla allows programmers to insert comments into their code. These begin with two slashes `//` and extend from there to the end of the line. Comments are ignored by the compiler, which treats them just like white space.

### Lintilla syntax

Here is a complete context-free grammar for the Lintilla language, upon which the parser of a Lintilla compiler should be based:

    program : (exp ";")* exp.
    
    exp : assign
        | pexp
        | ifexp
        | whileexp
        | returnexp
        | letdecl
        | fndecl.

    pexp : pexp "=" pexp
         | pexp "<" pexp
         | pexp "+" pexp
         | pexp "-" pexp
         | pexp "*" pexp
         | pexp "/" pexp
         | "-" pexp
         | "false"
         | "true"
         | integer
         | app
         | "(" exp ")".
         | block. 

    app : app "(" ((exp ",")* exp)? ")"
        | idnuse.

    assign : idnuse ":=" exp.

    block : "{" ((exp ";")* exp)? "}".

    ifexp : "if" exp block "else" block.
    
    whileexp : "while" exp block.
    
    returnexp : "return" exp?.
    
    letdecl : "let" "mut"? idndef "=" exp.
    
    fndecl : "fn" idndef "(" ((paramdecl ",")* paramdecl)? ")" ("->" tipe)? block.
    
    paramdecl : idndef ":" tipe.

Finally, the syntax of types:

    tipe : "unit"
         | "bool"
         | "int"
         | "fn" "(" ((tipe ",")* tipe)? ")" ("->" tipe)?
         | "(" tipe ")".

We use the word `tipe` instead of `type` since the latter is a Scala keyword which prevents us from using it as the name of a parser in our code. A function type is specified using the syntax

    fn(type_1, type_2, ..., type_n) -> res_type
    
which denotes the type of a function that takes `n` parameters of types `type_1`,..., `type_n` and returns a result of type `res_type`. We might note that a procedure has a type of the form

    fn(type_1, type_2, ..., type_n) -> unit

and that Lintilla does not allow variables or parameters to have type `unit`. This, however, is a matter for the type analysis phase of the compiler which we don't (yet) consdier here.

#### Precedence and associativity

The grammar above is not immediately suitable for encoding as a parser. The `pexp` non-terminal is ambiguous since it makes no allowance for precedence and associativity of the operators. You should rewrite that grammar production to implement the following precedence and associativity rules:

-   The following expression constructs have precedence as shown from
    lowest to highest with constructs on the same line having the same
    precedence:

    1.  equal and less than
    2.  addition and subtraction
    3.  multiplication and division
    4.  all other kinds of expression
    
    The constructs in the highest precedence category include unary negation, blocks in curly braces and expressions in parentheses.
    
-   All binary expression operators are left associative, except for the
    relational operators (equality and less than) which are not
    associative.

---
[Dominic Verity](http://orcid.org/0000-0002-4137-6982)  
Last modified: 12 September 2018  
[Copyright (c) 2018 by Dominic Verity and Anthony Sloane. Macquarie University. All rights reserved.](http://mozilla.org/MPL/2.0/)

