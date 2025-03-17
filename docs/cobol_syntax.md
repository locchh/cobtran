# COBOL Syntax Support

This document details the COBOL syntax features currently supported by COBTRAN.

## COBOL Structure

COBTRAN supports the standard COBOL program structure:

### Divisions

- ✅ IDENTIFICATION DIVISION
- ✅ ENVIRONMENT DIVISION
- ✅ DATA DIVISION
- ✅ PROCEDURE DIVISION

### Sections

#### Identification Division
- ✅ PROGRAM-ID
- ✅ AUTHOR
- ✅ INSTALLATION
- ✅ DATE-WRITTEN
- ✅ DATE-COMPILED
- ✅ SECURITY

#### Environment Division
- ✅ CONFIGURATION SECTION
- ✅ INPUT-OUTPUT SECTION
- ✅ FILE-CONTROL
- ✅ I-O-CONTROL

#### Data Division
- ✅ FILE SECTION
- ✅ WORKING-STORAGE SECTION
- ✅ LINKAGE SECTION

## Data Types and Descriptions

### Data Items

- ✅ Level numbers (01-49, 66, 77, 88)
- ✅ Group items
- ✅ Elementary items
- ✅ REDEFINES clause
- ✅ OCCURS clause
- ✅ VALUE clause
- ✅ PICTURE/PIC clause
- ✅ Basic data types (numeric, alphanumeric)
- ⚠️ Complex data types (partial support)

### PICTURE Clause Symbols

- ✅ 9 - Numeric
- ✅ A - Alphabetic
- ✅ X - Alphanumeric
- ✅ V - Implied decimal point
- ✅ S - Sign
- ✅ Z - Leading zero suppression
- ✅ . - Actual decimal point
- ✅ + - Sign
- ✅ - - Sign
- ⚠️ Other PICTURE symbols (partial support)

## Procedure Division

### Statements

- ✅ MOVE
- ✅ IF/ELSE
- ✅ PERFORM
- ✅ DISPLAY
- ✅ ACCEPT
- ✅ COMPUTE
- ✅ ADD
- ✅ SUBTRACT
- ✅ MULTIPLY
- ✅ DIVIDE
- ✅ CALL
- ✅ STOP RUN
- ✅ GO TO
- ✅ EXIT PROGRAM
- ⚠️ Other statements (partial support)

### Expressions

- ✅ Arithmetic expressions
- ✅ Conditional expressions
- ✅ Comparison operators
- ✅ Logical operators (AND, OR, NOT)
- ⚠️ Complex expressions (partial support)

## File Handling

- ✅ Basic file operations (OPEN, CLOSE, READ, WRITE)
- ✅ Sequential files
- ⚠️ Indexed files (partial support)
- ⚠️ Relative files (partial support)
- ⚠️ Advanced file operations (partial support)

## Special Features

- ✅ Formatted COBOL (fixed column format)
- ✅ Free-format COBOL
- ✅ COPY statement (basic support)
- ⚠️ REPLACING option in COPY
- ⚠️ EXEC SQL/CICS/DLI (limited support)
- ⚠️ Compiler directives (limited support)

## Legend

- ✅ Fully Supported
- ⚠️ Partially Supported
- ❌ Not Supported (yet)

## Limitations and Known Issues

1. **Complex Data Structures**: Some complex data structures with nested OCCURS or REDEFINES may require manual adjustment after migration.

2. **Platform-Specific Features**: COBOL code that relies on platform-specific features (e.g., mainframe-specific calls) may require additional integration work.

3. **Database Integration**: EXEC SQL statements are translated as comments with placeholders. Database access will need to be reimplemented using target language database libraries.

4. **Special Registers**: COBOL special registers (e.g., CURRENT-DATE) are mapped to equivalent functions in the target language where possible.

5. **COBOL Dialects**: COBTRAN primarily supports ANSI COBOL. Dialect-specific features may not be fully supported.

## Future Enhancements

The COBTRAN team is actively working to expand support for additional COBOL features:

1. Better support for indexed and relative files
2. Enhanced CICS/DB2 integration
3. Support for more COBOL dialects
4. Improved handling of complex data structures

## Contributing

If you need support for a specific COBOL feature not currently covered, please consider contributing to the project. See [Contributing](contributing.md) for more information.
