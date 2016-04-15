# Lambda Doge


A language for those who take themselves seriously.

## Installation

  For a prebuilt package, simple download:  [ ![Download](https://api.bintray.com/packages/jsuereth/doge/dogec/images/download.png) ](https://bintray.com/jsuereth/doge/dogec/_latestVersion) 

  To build, requires [sbt](http://scala-sbt.org) and a java JRE.  Once sbt is installed, cd into the doge directory and type:


    $ sbt
    > stage
    
Now, you should see a `target/universal/stage` directory which contains all the files needed to run the DOGE compiler,
including a `bin/` directory with utilities and a `lib/` directory with all the necessary jar files to run.

It's quite limited now, but Enjoy! Hack! Contribute!


Below follows a minor user guide.

# Getting Started

    WOW
    SUCH language!
    Much typing!
    Very Welcome!

DOGE is a statically typed language based, loosely, on lambda calculus with several ideas taken from Haskell/Scala.

It has the following features:

* Type Inference
* Tuples
* Lists
* Curried functions
* Compiles to JVM bytecode


## Your first module.

In DOGE, any file is treated as a DOGE module.   That is, all let expressions within the module are encoded into the
same JVM .class file.   One function may see those previously defined, but are unable to call other let expressions not 
yet defined.   Additionally, any expression labelled as main will be evaluated as an appliation on the JVM.

For example, let's create a file called `test.doge` with the following contents:


    WOW
    main
    MUCH PrintLn 1!

Now, we can compile this file via the following command line:

    dogec test.doge

And "run" the module via the following command:

    java -cp . test

Which will print the output `1`.


# Basics

The entire DOGE language is composed of named expressions.  We can define a new named expression using the WOW statement.  For example:

    WOW
    <name>
    <function application expr>
    
Expressions can be one of four things:

1. An integer literal, e.g. `1` or `451`
2. A boolean literal, i.e. `true` or `false`
3. A function application, e.g. `MUCH <func> <args> !` **Note: the ! ends the function application**
4. A Lambda expression, e.g. `MANY <arg names> <function-application>`

Named expressions may also have 'holes', i.e. function arguments.   These are denoted using the `SO` syntax, for example here is a method which adds one to any integer it is given:

    WOW
    addOne
    SO number
    MUCH Plus number 1!

Additionally, all functions in DOGE are curried by default.  That is, we can pass as many arguments as we wish to methods.
For example, `Plus` has the type `Int => Int => Int`.
This means it is a function which takes two integers and returns an integer, however you can supply just one integer to get back
a function `Int => Int`.

    WOW
    partialAdd
    MUCH Plus 1!
    
    WOW
    fullAdd
    MUCH partialAdd 2!
    
The `partialAdd` method will return a function `Int => Int` as its result.  The `fullAdd` method supplies the remaining
arguments, and its result would be `3`.




Besides this basic syntax, there are a set of built in functions/methods you can use to define your program.

Additionally, any zero-argument expression named `main` will be evaluated as the "application" on the JVM.  This can
be passed to the `java` executable to "run" a lambda-doge program.


# Standard Library

The standard library supports several primitive functions currently:

## Raw

    IS :: a => a

Is a method which just returns its argument.  It is actually completely erased in bytecode, but exists
as a syntactic sugar to ensure that expressions which require a function call can just return a value.

## Booleans


    ifs :: Boolean => a => a => a

Encodes if statements. If the first argument is true, returns the second argument, otherwise returns the last argument.
For evaluation semantics, the second/third argument are evaluated lazily.


## Integers

    Plus :: Int => Int => Int
    
Adds two numbers together.


    Minus:: Int => Int => Int
    
Minus two numbers together.

    Multiply :: Int => Int => Int
    
Multiply two numbers together.

    Divide :: Int => Int => Int
    
Divide two numbers together.


## Tuples

    tuple2 :: a -> b -> (a,b)
    
Creates a tuple of the two values

    fst :: (a,b) -> a
    
Returns the first part of a tuple

    snd :: (a,b) -> b
    
Returns the second part of a tuple

## Lists

    Nil :: List a
    
Creates an empty list with unspecified contents.

    cons :: a -> List a -> List a

Prepends an element to a list.

     hd :: List a -> a
   
Grabs the top of a list.  Note: This may throw an exception.

     tl :: List a -> List a
   
Grabs a new list with all but the first element.

## Stdout

    Print :: a -> Unit

Prints the value passed (using .toString) to the console

    PrintLn :: a -> Unit

Prints the value passed (using .toString) to the console, as well as an end-of-line.









