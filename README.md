In this project we implement a lexer, parser and interpreter
for an imperative language supporting loops, conditionals, 
and recursive function definitions. 
This is an extension of a previous parser project 
for a similar language that did not support function definitions. 

The interpreter, combined with the lexer and parser, 
takes a string and produces a program embedded in Haskell. 
The interpreted programs are functions of type

  Store -> Store 

where a Store is a function from variables to values 
(variables can be mapped to ints or function definitions)
representing a program state. 

We test each component of the interpreter and 
provide sample programs in the test directory. 
