# <sup>z</sup>rose Programming Language

The ThatRose is an open source programming language that makes it easy to 
build fast, reliable, and efficient software.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build](https://github.com/mrLSD/z-rose/actions/workflows/builds.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/builds.yml)
[![Lints](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml/badge.svg)](https://github.com/mrLSD/z-rose/actions/workflows/lints.yml)


![ThatRose](img/that-rose.png)

Compiled general-purpose programming language, statically typed with LLVM backend. 
The main focus of the language is simplicity, reliability, speed, maintainability.

ThatRose in research stage:
[x] EBNF representation
[x] Syntax parser to AST
[] Semantic analyzer **[in progress]**
[] LLVM Codegen
[] Memory model
[] Type system
[] std libraries
[] package management

Current implementation based on Rust language. For backend currently
used **LLVM 15**.

Part of research is:
- Pure LLVM IR as intermediate codegen
- clang libraries as intermediate backend for codegen

### MIT [LICENSE](LICENSE)
