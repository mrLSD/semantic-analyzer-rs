[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lints](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml)
[![Tests](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml)
[![Crates.io version](https://img.shields.io/crates/v/semantic-analyzer.svg?style=flat-square)](https://crates.io/crates/semantic-analyzer)
[![codecov](https://codecov.io/gh/mrLSD/semantic-analyzer-rs/graph/badge.svg?token=ZQ8FCYSSZX)](https://codecov.io/gh/mrLSD/semantic-analyzer-rs)

<div style="text-align: center;">
    <h1>mrLSD<code>/semantic-analyzer-rs</code></h1>
</div>

Semantic analyzer is an open source semantic analyzer for programming languages 
that makes it easy to build your own efficient compilers.

## What is the library for and what tasks does it solve

Creating a compilers for a programming language is process that involves several key 
stages. Most commonly it is:

- **Lexical Analysis (Lexer)**: This stage involves breaking down the input stream 
of characters into a series of tokens. Tokens are the atomic elements of the programming language, such as identifiers, keywords, operators, etc.

- **Syntax Analysis (Parsing)**: At this stage, the tokens obtained in the previous 
stage are grouped according to the grammar rules of the programming language. The result 
of this process is an **Abstract Syntax Tree (AST)**, which represents a hierarchical structure of the code.

- **Semantic Analysis**: This stage involves checking the semantic correctness of the code. This can include 
type checking, scope verification of variables, etc.

- **Intermediate Code Optimization**: At this stage, the compiler tries to improve the intermediate representation of the code to make it more efficient. 
This can include dead code elimination, expression simplification, etc.

- **Code Generation**: This is the final stage where the compiler transforms the optimized intermediate representation (IR) into 
machine code specific to the target architecture.

This library represent **Semantic Analysis** stage.

### Features

- **Name Binding and Scope Checking**: The analyzer verifies that all variables, constants, functions are declared before they're used, 
and that they're used within their scope. It also checks for name collisions, where variables, constants, functions, types in the same scope have the same name.

- **Checking Function Calls**: The analyzer verifies that functions are called with the number of parameters and that the type of 
arguments matches the type expected by the function.

- **Scope Rules**: Checks that variables, functions, constants, types are used within their scope, and available in the visibility scope.

- **Type Checking**: The analyzer checks that operations are performed on compatible types for expressions, functions, constant, bindings.
For operations in expressions. It is the process of verifying that the types of expressions are consistent with their usage in the context.

- **Flow Control Checking**: The analyzer checks that the control flow statements (if-else, loop, return, break, continue) are used correctly. 
Supported condition expressions and condition expression correctness check.

- **Building the Symbol Table**: For analyzing used the symbol table as data structure used by the semantic analyzer to keep track of 
symbols (variables, functions, constants) in the source code. Each entry in the symbol table contains the symbol's name, type, and scope related for block state, and other relevant information.

### Semantic State Tree

The result of executing and passing stages of the semantic analyzer is: **Semantic State Tree**.

This can be used for Intermediate Code Generation, for further passes
semantic tree optimizations, linting, backend codegen (like LLVM) to target machine.

#### Structure of Semantic State Tree 

- **blocks state** and related block state child branches. It's a basic
entity for scopes: variables, blocks (function, if, loop). 
Especially it makes sense for expressions. This allows you to granularly separate the visibility scope 
and its visibility limits. In particular - all child elements can access parent elements.
However, parent elements cannot access child elements, which effectively limits the visibility scope and entity usage.

  - **variables state**: block state entity, contains properties of variable in current
  state like: name, type, mutability, allocation, mallocation.

  - **inner variables state**: block state entity, contains inner variables names.
  It's useful for Intermediate Representation for codegen backends like LLVM.
  Where shadowed name variables should have different inner names. It means inner variables
  always unique.

  - labels state: block state entity, that contains all information about control flow labels.

- **Global state**: contains global state of constants, declared functions and types.

- **State entity**: contains: 
  - Global State 
  - Errors results
  - Semantic tree results

All of that source data, that can be used for Intermediate Representation for next optimizations and compilers codegen.

### Subset of programming languages

The input parameter for the analyzer is a predefined
AST (abstract syntax tree). As a library for building AST and the only dependency
used [nom_locate](https://github.com/fflorent/nom_locate) - which allows getting
all the necessary information about the source code, for further semantic analysis
and generating relevant and informative error messages. Currently
decided that the AST is a fixed structure because it is a fundamental
element that defines the lexical representation of a programming language.

On the other hand, it allows you to implement any subset of the programming language that matches
syntax tree. It also implies a subset of lexical representations from which an AST can be generated 
that meets the initial requirements of the semantic analyzer. As a library for lexical 
analysis and source code parsing, it is recommended to use: [nom is a parser combinators library](https://github.com/rust-bakery/nom).

AST displays the **Turing complete** programming language and contains all the necessary elements for this.

## MIT [LICENSE](LICENSE)
