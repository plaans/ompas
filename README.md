# Unnamed FactBase
Author: Jérémy Turi

Last update of the readme: 30/03/2021

***

## Overview
This project aim to develop a factbase to handle facts in a RAE-like system.
The model and representation used are based on the project aries from Arthur-Bit-Monnot.
This project is part of the Thesis I am currently working on at LAAS-CNRS is RIS Team.

***
## Features

### FactBase language
- `(read <name>.txt)`: Read a list of lisp commands to reconstruct the factbase and the environment.
- `(write <name>.txt)` : write the commands to reconstruct later the factbase and the environment.
- `(define <symbol> <body>)`  : Define a new symbol, the body is different in function of the kind.
    - `(list <val_1> ... <val_n> )` : define a list of elements
    - `(quote <value>)` : define a LValue that will not be evaluated during parsing.
    - `(map (list <pair_value_1> ... <pair_value_2>)) : create a map with a list of pair of LValue.   
      Types must match with predicate types.
- `(set <sym> <value>)` : set to a LValue a value and return a new LValue
- `(get <sym>)` : return the LValue associated to the symbol
- `(get map_1 <key>)` : return the value associated to the key in the map



### Default types
By default, the factbase as the root types **int**, **float**, **boolean** and **object**

***

## Run

You have to install rust on your computer previously.
You can find intallation instructions on [rust website](https://www.rust-lang.org/tools/install)
- close the project in your repository
- build with `cargo build` in a terminal
- run with `cargo run -- -r`, it will launch the repl.

  
## Dependencies
- aries_model
- aries_planning
- aries_utils
- aries_collections
- rustyline
- im-rs

## TODO
- add multiple world, multi context support.
