# Operationnal Model Planning and Acting System (OMPAS)
Author: Jérémy Turi

Last update of the readme: 13/04/2021

***

## Overview
This project aim to develop a factbase to handle facts in a RAE-like system.
The model and representation used are based on the project aries from Arthur Bit-Monnot.
This project is part of the Thesis I am currently working on at LAAS-CNRS is RIS Team.

## Documentation
The documentation is available at this gitbook: https://yirmandias.gitbook.io/ompas/

***

## Run

You have to install rust on your computer previously.
You can find intallation instructions on [rust website](https://www.rust-lang.org/tools/install)
- close the project in your repository
- build with `cargo build` in a terminal
- run with `cargo run`, it will launch the ompas binary

  
## Dependencies
- aries_model
- aries_planning
- aries_utils
- aries_collections
- rustyline
- im-rs

## TODO
- add multiple world, multi context support.
