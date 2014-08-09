# Lambda Doge


A language for those who waste their time.

## Usage

  To build, requires [sbt](http://scala-sbt.org).  Once sbt is installed, cd into the doge directory and type:


    $ sbt
    > runMain doge.compiler.Compiler test.doge


You should see output like the following:

    [info] Compiling 1 Scala source to /home/jsuereth/projects/personal/doge/target/scala-2.11/classes...
    [info] Running doge.compiler.Compiler test.doge
     -- Compiling --
    WOW
    Big
    SUCH Int
    SO numbers
    MUCH Plus numbers 1!
 
    WOW
    Doge
    MUCH IS 5!
 
    WOW
    Katz
    VERY Big Doge!
 
    WOW
    Rain
    VERY Katz!

    WOW
    main
    MUCH PrintLn
    VERY Big Doge!
    MUCH Rain!
    VERY Katz!!
     -- Parsed --
    let Big(numbers: Int) = (<Plus> <numbers> 1)
    let Doge() = (<IS> 5)
    let Katz() = (<Big> <Doge>)
    let Rain() = <Katz>
    let main() = (<PrintLn> (<Big> <Doge>) <Rain> <Katz>)
     -- Typed --
    let Big(numbers: Int) = (<Plus> <numbers> 1)
    let Doge() = (<IS> 5)
    let Katz() = (<Big> <Doge>)
    let Rain() = <Katz>
    let main() = (<PrintLn> (<Big> <Doge>) <Rain> <Katz>)
    Creating method: Big (I)I
    Creating method: Doge ()I
    Creating method: Katz ()I
    Creating method: Rain ()I
     -- Compiled --
    /home/jsuereth/projects/personal/doge/test.class


Once the file has been compiled, you can run the doge script via:

    $ java -cp . test


It's quite limited now, but Enjoy! Hack! Contribute!


Below follows the specifications, including built-in "standard library".

## Parsing


Any whitespace acts as a token delimiter.

    type-declr := SUCH <type>*
    arg-list := SO <id>*
    let-expr :=  WOW <id> <type-declr>? <arg-list>? (<application-expr>)
    application-expr := (MANY | VERY | MUCH) <application> !
    expr := (<let-expr> | <application-expr> | <literal> | <id>)

Examples:

    WOW
    Big
    SUCH Int
    SO numbers
    MUCH Plus numbers 1 !

    WOW
    Doge
    MUCH 5 !

    WOW
    main
    MUCH PrintLn
    VERY Big Doge !!

Semantics (in pseudo language):

    let Big(numbers: Int) = (<Plus> <numbers> 1)
    let Doge() = <5>
    let main() = (<PrintLn> (<Big> <Doge>))

Expected execution output:

    6

### Parser TODOs

* Refined the AST so we get better positions on type errors.
* More literal types (e.g. String, List)

## Type System

The DOGE type system is composed of two types:

    * Type Constructor  (TCons)
    * Type Variable     (TVar)

A type constructor (TCons) is composed of a term (string) identifier and a set of type arguments, e.g.

    type term = String
    data TCons term [Type]

Simple types are encoded as type constructors of no arguments. e.g. integers are defined as:

    TInteger = TCons "int"

And there is a single function type, with constructor:

     TFunc in out =  TCons "->" in out

So, type constructors with a term of "->" are treated specially in the system.  Any multi-argument function is turned
into a curried function type.  For example, a method:

    let x(Int, Int, Int): Int

would be turned into the type

    Int => (Int => (Int => Int))


### Type Inference

Doge does a let-local inference algorithm, where all types in a given top-level let expression must be unified.
Currently all user-specified types in the syntax are ignored.  We're using a pretty naive Hindley-Milner adaptation.
Basically, all unknown types are assigned a type variable, and all expressions are unified until as many known types
that can be discovered, are discovered.

e.g.

    let x(a) = Plus a 1

with the following known types :

    Plus :: Int => Int => Int

would result in the following associations :

    x :: Int => Int
    a :: Int

Nothing super complicated, just brute force unification.

### Possible Improvements

* Refine let-expression semantics, including scoping issues and nesting.
* Support for Typeclasses
* Some kind of mechanism for handling JDK types.
* Support for Kinds
* Performance





## JVM Encoding

This section details the encoding to the JVM.

Examples:


### Outer lets

Concrete methods:

    let x :: Int => Int => Int

encodes as:

    public static int x(name: Int, arg2: Int) { ... }


Concrete members:


     let x :: Int

encode as:

     public static int x() { ... }

### Lambdas

Not supported at this time


### BuiltIn function calls

    Plus :: Int => Int => Int

encodes as IADD bytecode operation.


    IS :: a => a

is erased completely (the argument expression is compiled directly inline).


    PrintLn :: Int => Int => Int => Int

Yes, the type is super lame.  Right now we encode this pretty dumb to call `System.out.print` a bunch of times, then a `println`.









