[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lints](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml)
[![Tests](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/tests.yml)
[![Crates.io version](https://img.shields.io/crates/v/semantic-analyzer.svg?style=flat-square)](https://crates.io/crates/semantic-analyzer)
[![codecov](https://codecov.io/gh/mrLSD/semantic-analyzer-rs/graph/badge.svg?token=ZQ8FCYSSZX)](https://codecov.io/gh/mrLSD/semantic-analyzer-rs)

<div style="text-align: center;">
    <h1>mrLSD<code>/semantic-analyzer-rs</code></h1>
</div>

Semantic analyzer is an open source semantic analyzer for programming languages 
that makes it easy to build your own efficient compilers with extensibility in mind.

## 🌀 What the library is for and what tasks it solves

Creating a compilers for a programming language is process that involves several key 
stages. Most commonly they are:

▶️ **Lexical Analysis (Lexer)**: This stage involves breaking down the input stream 
of characters into a series of tokens. Tokens are the atomic elements of the programming language, such as identifiers, keywords, operators, etc.

▶️ **Syntax Analysis (Parsing)**: At this stage, the tokens obtained in the previous 
stage are grouped according to the grammar rules of the programming language. The result 
of this process is an **Abstract Syntax Tree (AST)**, which represents a hierarchical structure of the code.

⏩ **Semantic Analysis**: This stage involves checking the semantic correctness of the code. This can include 
type checking, scope verification of variables, etc.

▶️ **Intermediate Code Optimization**: At this stage, the compiler tries to improve the intermediate representation of the code to make it more efficient. 
This can include dead code elimination, expression simplification, etc.

▶️ **Code Generation**: This is the final stage where the compiler transforms the optimized intermediate representation (IR) into 
machine code specific to the target architecture.

This library represents **Semantic Analysis** stage.

### 🌻 Features

✅ **Name Binding and Scope Checking**: The analyzer verifies that all variables, constants, functions are declared before they're used, 
and that they're used within their scope. It also checks for name collisions, where variables, constants, functions, types in the same scope have the same name.

✅ **Checking Function Calls**: The analyzer verifies that functions are called with the number of parameters and that the type of 
arguments matches the type expected by the function.

✅ **Scope Rules**: Checks that variables, functions, constants, types are used within their scope, and available in the visibility scope.

✅ **Type Checking**: The analyzer checks that operations are performed on compatible types for expressions, functions, constant, bindings.
For operations in expressions. It is the process of verifying that the types of expressions are consistent with their usage in the context.

✅ **Flow Control Checking**: The analyzer checks that the control flow statements (if-else, loop, return, break, continue) are used correctly. 
Supported condition expressions and condition expression correctness check.

✅ **Building the Symbol Table**: For analyzing used the symbol table as data structure used by the semantic analyzer to keep track of 
symbols (variables, functions, constants) in the source code. Each entry in the symbol table contains the symbol's name, type, and scope related for block state, and other relevant information.

✅ **Generic expression value**: The ability to expand custom expressions for AST,
according to compiler requirements. And the ability to implement custom instructions 
for these custom expressions in the **Semantic Stack Context**.

### 🌳 Semantic State Tree

The result of executing and passing stages of the semantic analyzer is: **Semantic State Tree**.

This can be used for Intermediate Code Generation, for further passes
semantic tree optimizations, linting, backend codegen (like LLVM) to target machine.

#### 🌲 Structure of Semantic State Tree 

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

### 🧺 Subset of programming languages

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

## 🔋 🔌 Extensibility

Since `AST` is predefined, but in real conditions it may be necessary to expand the 
functionality for the specific needs of the `compiler`, has been added the functionality 
of the `AST` extensibility and the additional generated set of `Instructions` for 
the **Semantic Stack Context**.

- [x] 🚨 **Genetic expression value**: The ability to expand custom expressions for z, according to compiler requirements. 
The ability to implement custom instructions for these custom expressions in the 
**Semantic Stack Context**.

## 🛋️ Examples

- 🔎 There is the example implementation separate project [💾 Toy Codegen](https://github.com/mrLSD/toy-codegen).
The project uses the `SemanticStack` results and converts them into **Code Generation** logic which clearly shows the 
possibilities of using the results of the `semantic-analyzer-rs` `SemanticStackContext` results. LLVM is used as a 
backend, [inkwell](https://github.com/TheDan64/inkwell) as a library for LLVM codegen, and compiled into an executable 
program. The source of data is the AST structure itself.

## 📶 Features

Available library rust features:
- `codec` - 💮 enable serialization and deserialization with `Serde`.
  This is especially convenient in the process of forming AST, Codegen, 
  a serialized representation of the `SemanticState`. Another important 
  nuance is that any library that implements `Serde` can act as a 
  serializer `codec`. For example formats: `json`, `toml`, `yaml`, 
  `binary`, and many others that can use `serde` library.
  The main entities, which apply the `codec` feature are:
  - [x] `AST` ↪️ AST data source can be presented with serialized source.
    This is especially useful for designing and testing `Codegen`, AST data 
    transfer pipeline, and also for use as a data generation source for 
    AST - any programming language that can generate serialized AST data.
  - [x] `State` ↪️ `SematniсState` can be obtained in the form of 
    serialized data. This is especially convenient for storing state 
    before code generation with different parameters, post-analysis, 
    optimizations - which will allow to work with already analyzed 
    data.
  - [x] `SemanticStack` ↪️ contains a set of instructions for `Codegen`. 
    Representation in serialized form may be convenient for cases: code 
    generation without repeated semantic analysis, only based on 
    instructions for the code generator generated by the `semantic analyzer`. 
    Serialized data represented `SemanticStack` - opens up wide 
    possibilities for using any third-party code generators and compilers 
    implemented in any programming language.

## MIT [LICENSE](LICENSE)
