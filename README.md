[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build](https://github.com/mrLSD/z-rose/actions/workflows/builds.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/builds.yml)
[![Lints](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml)
[![Tests](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml)

<div style="text-align: center;">
    <h1><code>mrLSD/<b>semantic-analyzer-rs</b></code></h1>
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

## MIT [LICENSE](LICENSE)
