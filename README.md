# AssemblerInHaskell

## What is this project?

This project is an assembler for the Hack architecture based on the [Nand2Tetris](https://www.nand2tetris.org/) project. It is written in the Haskell programming language for as a final project for Dr. Yorgey's CSCI 365 Functional Programming course.

## How do I compile it?

You can compile this program using this command.

> ghc --make assembler.hs
## Usage

You can use the program as such:

> ./assembler [input file.asm] [output file.hack]

## Project Layout

The project is laid out as such:

**AParser** contains most of the primordial parsers that were written by Dr. Yorgey prior to this project. 

**DataStructures** contains most of the GADTs for the project, and is where the project takes form. 

**Parser** is where Strings get turned into these data structures.

**ToBinary** contains functions vital in converting parsed data into binary instructions in the form of strings. 

**Assembler** "glues together" the Parser and ToBinary steps, and acts can act as the entire assembler, although you won't have any IO.  It also contains the main function.

**FileIO** contains the functions for IO, and wraps around the Assembler.


