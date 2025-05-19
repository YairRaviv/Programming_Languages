# Programming Languages - Exercise 2

This folder contains the implementation for Exercise 2 of the Programming Languages course.

## Contents

- `316283043_315708370_3.rkt`: The main Racket file containing the solutions to exercise questions.

## Topics Covered

### Question 1: Postfix AE Interpreter

Implementation of an interpreter for postfix notation Arithmetic Expressions (AE). The implementation includes:

- A modified BNF grammar for postfix notation
- Updated parser to handle operators at the end of expressions
- Support for power and square operations
- Tail-recursive implementation of the power function

### Question 2: List Expressions (LE) Parser and Evaluator

Implementation of a parser and evaluator for List Expressions (LE) with the following features:

- Support for lists, append, and cons operations
- Handling of numbers and symbols
- Full evaluation of nested list expressions

## Running the Code

This code requires the Racket programming language with the PL package installed. To run the code:

1. Open the file in DrRacket or another Racket environment
2. Run the file to execute the tests

## Implementation Notes

The implementation uses typed Racket with the PL language support. It includes detailed comments explaining the approach and solution for each problem.