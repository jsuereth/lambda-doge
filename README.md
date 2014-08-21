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

DOGE is a staticly typed language based, loosely, on lambda calculus with several ideas taken from Haskell.

It has the following features:

* Type Inference
* Tuples
* Compiles to JVM bytecode


## Your first module.

In DOGE, any file is treated as a DOGE module.   That is, all let expressions within the module are encoded into the
same JVM .class file.   One function may see those previously defined, but are unable to call other let expressions not 
yet defined.   Additionally, any expression labelled as main will be evaluated as an appliation on the JVM.

For example, let's create a file called `test.doge` with the following contents:


    WOW
    main
    MUCH PrintLn
    1 2 3!

Now, we can compile this file via the following command line:

    dogec test.doge

And "run" the module via the following command:

    java -cp . test

Which will print the output `123`.



# Standard Library

The standard library supports several primitive functions currently:

## Raw

    IS :: a => a

Is a method which just returns its argument.  It is actually completely erased in bytecode, but exists
as a syntactic sugar to ensure that expressions which require a function call can just return a value.

## Booleans


    ifs :: Boolean => a => a => a

Encodes if statements. If the first argument is true, returns the second argument, otherwise returns the last argument.
For evaluation semantics, the first/second argument are evaluated lazily.


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


## Lame hacks

    PrintLn :: Int => Int => Int => Int

Yes, the type is super lame.  Right now we encode this pretty dumb to call `System.out.print` a bunch of times, then a `println`.









