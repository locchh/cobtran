# COBOL Syntax Support

This document details the COBOL syntax features currently supported by COBTRAN.

## COBOL Structure

COBTRAN supports the standard COBOL program structure:

### Divisions

- ✅ IDENTIFICATION DIVISION – Describes metadata about the program, such as its name, author, and creation date.
- ✅ ENVIRONMENT DIVISION – Specifies the system-dependent aspects of the program, including input/output device details.
- ✅ DATA DIVISION – Declares the data structures and storage areas used by the program.
- ✅ PROCEDURE DIVISION – Contains the executable code or logic of the program.

### Sections

#### Identification Division
- ✅ PROGRAM-ID– Names the program and acts as its identifier.
- ✅ AUTHOR – Documents the name of the person who wrote the program.
- ✅ INSTALLATION – Describes the system or organization where the program was developed or is intended to run.
- ✅ DATE-WRITTEN – Indicates when the program was created.
- ✅ DATE-COMPILED – States the last compilation date of the program.
- ✅ SECURITY – Provides security classification or comments related to the program’s use or handling.

#### Environment Division
- ✅ CONFIGURATION SECTION – Defines hardware and software environment details (e.g., source/computer system).
- ✅ INPUT-OUTPUT SECTION – Declares external files/devices and how they are to be handled.
- ✅ FILE-CONTROL – Lists the files that the program will use and their access methods.
- ✅ I-O-CONTROL – Controls the input-output operations such as buffering strategies.

#### Data Division
- ✅ FILE SECTION – Declares the structure of external files used in the program.
- ✅ WORKING-STORAGE SECTION – Defines temporary variables and data structures used during execution.
- ✅ LINKAGE SECTION – Defines parameters passed between the main program and called subprograms (used for inter-program communication).

## Data Types and Descriptions

### Data Items

- ✅ Level numbers (01-49, 66, 77, 88) – Indicate the hierarchy of data items in the Data Division; 01 typically defines a group item, 77 is for standalone items, 66 is for RENAMES, and 88 defines condition names.
- ✅ Group items – A data item that contains other items (like a struct or record); declared with level number 01 or higher.
- ✅ Elementary items – The smallest indivisible data item (i.e., does not contain other items); used to hold individual values.
- ✅ REDEFINES clause – Allows multiple data definitions to share the same memory area, supporting different interpretations of the same data.
- ✅ OCCURS clause – Used to define arrays or tables by specifying how many times a data item repeats.
- ✅ VALUE clause – Assigns a default or initial value to a data item when the program begins execution.
- ✅ PICTURE/PIC clause – Describes the data type, size, and format of the variable (e.g., numeric, alphanumeric).
- ✅ Basic data types (numeric, alphanumeric) – COBOL supports simple data types like numbers (with or without signs/decimals) and character strings.


- ⚠️ Complex data types (partial support)

### PICTURE Clause Symbols

- ✅ 9 - Numeric – Numeric digit placeholder; each 9 represents a position for one digit.
- ✅ A - Alphabetic – Alphabetic character placeholder; accepts only letters.
- ✅ X - Alphanumeric – Alphanumeric character placeholder; accepts letters, digits, and symbols.
- ✅ V - Implied decimal point – Implied decimal point; does not occupy space in memory but affects how numbers are interpreted.
- ✅ S - Sign ; specifies whether the value is positive or negative.
- ✅ Z - Leading zero suppression – Suppresses leading zeros in numeric display.
- ✅ . - Actual decimal point
- ✅ + - Sign
- ✅ - - Sign
- ⚠️ Other PICTURE symbols (partial support)

## Procedure Division

### Statements

- ✅ MOVE – Copies a value from one variable to another.
- ✅ IF/ELSE – Implements conditional logic for branching code execution.
- ✅ PERFORM – Executes a block of code, like a loop or procedure call.
- ✅ DISPLAY – Outputs text or data to the screen or console.
- ✅ ACCEPT – Receives input from the user or system.
- ✅ COMPUTE – Performs arithmetic operations and assigns the result.
- ✅ ADD – Adds one or more numbers.
- ✅ SUBTRACT – Subtracts one or more numbers.
- ✅ MULTIPLY – Multiplies numbers.
- ✅ DIVIDE – Divides numbers.
- ✅ CALL – Invokes a subprogram or external routine.
- ✅ STOP RUN – Terminates the program.
- ✅ GO TO – Unconditional jump to another section of code.
- ✅ EXIT PROGRAM – Exits a subprogram and returns control to the calling program.
- ⚠️ Other statements (partial support)

### Expressions

- ✅ Arithmetic expressions – Support basic math operations (e.g., A + B * C).
- ✅ Conditional expressions
- ✅ Comparison operators
- ✅ Logical operators (AND, OR, NOT)
- ⚠️ Complex expressions (partial support)

## File Handling

- ✅ Basic file operations (OPEN, CLOSE, READ, WRITE)
- ✅ Sequential files – Boolean expressions used in IF, EVALUATE, etc.
- ⚠️ Indexed files (partial support) – =, <, >, <=, >=, <> to compare values.
- ⚠️ Relative files (partial support) – Combine multiple conditions for decision-making.
- ⚠️ Advanced file operations (partial support)

## Special Features

- ✅ Formatted COBOL (fixed column format) – Traditional COBOL format with strict column placement (e.g., Area A/B rules).
- ✅ Free-format COBOL – More flexible syntax introduced in newer COBOL versions; less strict about indentation and column alignment.
- ✅ COPY statement (basic support) – Reuses code/data definitions by copying from external files or libraries.
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
