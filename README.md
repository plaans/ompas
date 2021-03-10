# Unnamed FactBase
Author: Jérémy Turi

Last update of the readme: 10/03/2021

***

## Overview
This project aim to develop a factbase to handle facts in a RAE-like system.
The model and representation used are based on the project aries from Arthur-Bit-Monnot.
This project is part of the Thesis I am currently working on at LAAS-CNRS is RIS Team.

***
## Features

### FactBase language
- `(read <name>.txt)`: Read a list of lisp commands to reconstruct the factbase and the environment.
- `((write <name>.txt))` : write the commands to reconstruct later the factbase and the environment.
- `(define <symbol> <body>)`  : Define a new symbol, the body is different in function of the kind.
    - `(type <parent-type>)`: Define a new type with a parent type
    - `(obj <type>)` : Define an object with a type.
    - `(sf <t_p_1> ... <t_p_n> <t_value>))` : Define a new state function with n parameters types and a value type.
    - `(var <type> <value>} ` : Define a new var with a type and a value that matches the type.
    - `(sv <p_1> ... <p_n> <value>)` : Define a new state-variable with n parameters and a value.
      Types must match with predicate types.
    - `(factbase <sv_1>...<sv_n>)`: Define a factbase with n state variables
- `(define ?fb_j (set ?fb_i <sv_1> ... <sv_n> )`: define a new factbase ?fb_j from the facts of ?fb_i and new facts.
- `(get <sym>)` : print the value of the symbol in a pretty way



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
-im-rs

## TODO
- add multiple world, multi context support.