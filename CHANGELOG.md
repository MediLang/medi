# Changelog

All notable changes to the Medi programming language will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Lexer Benchmarks and File Extension Standardization (2025-05-30)
- **Added**
  - Comprehensive benchmarking suite for lexer performance
  - Documentation for lexer performance characteristics and usage guidelines
  - Support for `.medi` file extension as the standard for Medi source files
- **Changed**
  - Updated all documentation and examples to use `.medi` extension
  - Refactored lexer benchmarks to handle large files more efficiently
- **Fixed**
  - Memory usage in chunked lexer implementation
  - File handling in benchmark utilities

### PR #11: Nested Expressions & Statements (2025-05-24)
- **Added**
  - Support for complex nested binary and block expressions
  - Medical-specific operators: `of` and `per` with custom precedence
  - Comprehensive test coverage for edge cases
- **Fixed**
  - Handling of extra semicolons in block expressions
  - Error handling for chained comparisons

### PR #9, #10: Operator Precedence (2025-05-23, 2025-05-18)
- **Added**
  - Operator precedence rules for all operators
  - Detailed documentation for operator behavior
  - Docstrings for better code documentation
- **Changed**
  - Refactored expression parsing logic
  - Improved error messages for operator precedence issues

### PR #6, #7, #8: Recursive Descent Parser (2025-05-17, 2025-05-15, 2025-05-15)
- **Added**
  - Comprehensive recursive descent parser implementation
  - Detailed documentation for parser components
  - Test cases for various language constructs
- **Changed**
  - Multiple iterations to improve parser accuracy
  - Enhanced error recovery mechanisms

### PR #5: Core Parser Implementation (2025-05-12)
- **Added**
  - Initial recursive descent parser structure
  - Basic expression and statement parsing
  - Foundation for future language features

### PR #3, #4: Documentation Setup (2025-05-11)
- **Added**
  - MkDocs configuration with Material theme
  - Initial documentation structure
  - Project roadmap and contributing guidelines
- **Fixed**
  - Documentation build configuration
  - Broken links and formatting issues

### PR #1: Project Initialization (2025-05-10)
- **Added**
  - Core language syntax parser
  - Lexer and basic parser infrastructure
  - Initial test suite
  - Project structure and build configuration

## [Pre-release Development]

### Initial Commit
- Project initialization
- Basic language design documentation
- Development environment setup
