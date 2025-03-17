# COBTRAN Architecture

This document describes the architecture of the COBTRAN system and how its components interact.

## High-Level Architecture

COBTRAN follows a pipeline architecture with the following main components:

```
Input COBOL Code → Parser → AST → Transformer → Target Model → Generator → Output Code
```

### Components

1. **Parser**: Analyzes COBOL source code and creates an abstract syntax tree (AST)
2. **AST (Abstract Syntax Tree)**: In-memory representation of the COBOL program structure
3. **Transformer**: Converts the COBOL AST to a target language model
4. **Target Model**: Intermediate representation that bridges COBOL and the target language
5. **Generator**: Produces code in the target language from the intermediate representation

## Package Structure

The COBTRAN codebase is organized into the following packages:

- `cobtran/` - Main package
  - `__init__.py` - Package initialization
  - `cli.py` - Command-line interface
  - `core.py` - Core functionality and orchestration
  - `parser/` - COBOL parsing
    - `__init__.py` - Package exports
    - `parser.py` - COBOL parser implementation
    - `cobol_grammar.lark` - Lark grammar for COBOL
  - `ast/` - Abstract Syntax Tree
    - `__init__.py` - Package exports
    - `ast.py` - AST data structures
  - `transformers/` - Code transformation
    - `__init__.py` - Package exports
    - `transformer.py` - AST to target model transformation
  - `generators/` - Code generation
    - `__init__.py` - Package exports
    - `generator.py` - Target model to code generation
  - `utils/` - Utility functions
    - `__init__.py` - Package exports
    - `logging.py` - Logging utilities

## Data Flow

1. The user provides COBOL source code via the CLI
2. The `core.py` module orchestrates the migration process
3. The COBOL source is parsed into an AST using the `parser` module
4. The AST is transformed to a target model using the `transformers` module
5. The target model is used to generate code in the target language using the `generators` module
6. The generated code is written to the specified output location

## Parser Module

The parser uses the Lark parsing library to analyze COBOL source code. The grammar is defined in `cobol_grammar.lark`. The parser handles:

- Tokenization of COBOL source
- Handling of COBOL's fixed format (columns with specific meanings)
- Construction of an AST that represents the program structure

## AST Module

The AST module defines data structures that represent COBOL program elements:

- `CobolAST`: Root of the AST
- `Division`: COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- `Section`: Sections within divisions
- `Paragraph`: Named groups of statements
- `Statement`: Individual COBOL statements

## Transformer Module

The transformer converts the COBOL AST to a target model that's closer to the target language:

- Analyzes COBOL data definitions and creates equivalent target language variables
- Transforms COBOL procedural logic to a structure that maps to the target language
- Handles COBOL-specific constructs and idioms

## Generator Module

The generator produces code in the target language based on the intermediate representation:

- For Python: Generates classes, methods, and appropriate Python syntax
- For Java: Generates classes, methods, and Java-specific syntax
- Applies formatting and organization conventions of the target language

## Extension Points

COBTRAN is designed with extensibility in mind. Key extension points include:

1. **Additional COBOL dialects**: Extend the grammar and parser to support more COBOL variants
2. **Additional target languages**: Add new generator implementations for other languages
3. **Enhanced transformations**: Improve the transformation of COBOL constructs to more idiomatic target language code
