# <p align ="center">Compiler for a C-like Language 💻</p>

## <p align ="center">This project is a compiler for a simple procedural language similar to C. It is implemented using Lex, Yacc, and the C programming language. The compiler performs various tasks such as parsing, scanning, semantic analysis, generating Three-Address Code (3AC), and supporting features like short circuit evaluation.</p>
<br> 

### <p align ="center"> Implemented using: </p>
<p align ="center">
<a href="https://en.wikipedia.org/wiki/C_(programming_language)" target="_blank" rel="noreferrer">   <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/C_Programming_Language.svg/380px-C_Programming_Language.svg.png?20201031132917" width="32" height="32" /></a></p>

<br>

### <p align ="center"> Do remember to star ⭐ the repository if you like what you see!</p>

<br>

## Language Description 📝
The language supports the following keywords: bool, char, int, real, string, int*, char*, real*, if, else, while, for, var, return, null, void, do.

It also includes various operator lexemes such as &&, /, =, ==, >, >=, <, <=, -, !, !=, ||, +, *, &.

Literal lexemes include boolean values (true or false), characters enclosed in single quotes, integers (decimal or hex), reals (similar to doubles in C), strings (sequences of characters enclosed in double quotes), and identifiers (variable or function names).

The language supports pointers, which are variables that store memory addresses. Pointers can be dereferenced using the * operator and can be assigned the address of a variable using the & operator. The keyword null represents a pointer that points nowhere.

Other lexemes include semicolons, commas, pipes (used for string length computation), curly braces (for code blocks), parentheses (for function calls and parameter lists), and square brackets (for string indexing).
<br>

## Program Structure 🏗️
A program in this language consists of multiple functions listed one after another. Every program should have one main() procedure. User-defined functions can also be included, and they must be defined before they are called.

Functions are declared with a return type, function name, parameter list, and a body containing variable declarations and statements. Nested functions are also supported.

Statements and Expressions 💬
Statements in the language can be assignment statements, function call statements, if statements, if-else statements, while statements, for statements, and code blocks.

Expressions are formed using operators with the same precedence as in C/C++. Function calls can be used as expressions.
<br>

## Return Statement ↩️
Non-void functions must end with a return statement. The return statement is followed by an expression that evaluates to the return value.
<br>

## Variables and Strings 📚
Variables are declared using the var keyword, followed by the type and one or more identifiers separated by commas. Variables can be assigned values in the declaration.

Strings, which are character arrays, can be declared using the string keyword followed by one or more identifiers and their lengths in square brackets. String elements can be assigned character values or used in expressions.
<br>

## Usage 🚀
To use the compiler, run the Lex and Yacc programs with the provided language description. The compiler will perform lexical analysis, syntax parsing, semantic analysis, and generate Three-Address Code (3AC) for the input code.
<br>

## <p align ="center">Enjoy using the compiler and exploring the features of this new language! 🎉</p>

### <p align ="center"> Do remember to star ⭐ the repository if you like what you see!</p>
