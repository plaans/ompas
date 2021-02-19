# Unnamed FactBase
Author: Jérémy Turi

Last update of the readme: 19/02/2021

***

## Overview
This project aim to develop a factbase to handle facts in a RAE-like system.
The model and representation used are based on the project aries from Arthur-Bit-Monnot.
This project is part of the Thesis I am currently working on at LAAS-CNRS is RIS Team.

***
## Featues

### FactBase language
- `((help))` : print the help
- `((hist))` : Print a list of all commands.
- `((get-all))`: Print all the fact-base in a pretty way.
- `((close))` or `((exit))` : Exit the repl.
- `((path)) ` : Print the current repository.
- `((read ?fb))`                     : Read a factbase from a file x.fb.
- `((write ?fb))`                     : Write factbase to file x.fb.
- `((let (?f)))`               : Define a new fact. Here are the different values for **?f**
    - `type t - parent_type{optional}`: Define a new type
    - `object o - t {optional}` : Define a new object of type t. By default the object will be of type **object**
    - `pred sv t1 ... tn tv)))` : Define a new state function with n parameters types and a value type.
    - `const{optional} var x - type{optional} = value{optional} ` : Define a new var with a type and a value of the right type.
      Can be a constant.
    - `const{optional} sv n_sv p1...pn value` : Define a new state-variable with n parameters and a value.
      Types must match with predicate types.
- `((set (?f))) ` and `((test(?f)))` : respectively set a value and test a value of a fact: ?f has the following format:
    - `n_sv p1...pn value` : set a value to a already defined state-variable. Types must match.
    - `x value` : set a value to a already defined variable. Types must match.
- `((get (?f))) ` and `((del (?f)))`: respectively get the value of a fact and del a fact.
    - `x` : access a variable
    - `n_sv p1 ... pn` : access a state variable



### Default types
By default, the factbase as the root types **int**, **boolean** and **object**
### Type inference
The fact base can infer types when declaring a variable with no type or a state var with no definition of the state function:
- inference of the type of variable: if we define a variable as `((let (var ?r = bedroom)))`
  and **bedroom** is of type **room**, then **?r** will be of type **room**.
- inference of types of a state variable: if we define a state variable like `((let (sv loc bob bedroom)))`, 
and **loc** has not been previously defined, then the state function **loc** will infer its from **bob** and **bedroom**.
  If **bob** is of type **robot** and **bedroom** is of type **room**,
  then the factbase will define **loc[robot] <- location**

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

## TODO
- add multiple world, multi context support.