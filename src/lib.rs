#![deny(clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
//! # Semantic Analyzer
//! The semantic analyzer consists of the following basic elements:
//! - AST is an abstract syntax tree that implements a predefined set of
//!   representations in a programming language. This is the basis for
//!   semantic analysis.
//! - Semantic analyzer - AST based semantic analyzes generates a
//!   Semantic State Stack and semantic representation context for
//!   logical semantic blocks. Contains all the necessary results of
//!   semantic analysis, including:
//!   - constants
//!   - types
//!   - functions
//!
//! For the body of functions, the analysis of the semantic logic of the
//! function and the generation of Block State context trees are fully implemented.
//!
//! Based on this Semantic context data, additional analysis in the form of linters,
//! optimizers, and code generation can be implemented.

/// AST representation
pub mod ast;
/// Semantic analyzer and State related functions
pub mod semantic;
/// Semantic analyzer common types
pub mod types;
