# Frequently Asked Questions (FAQ)

## General Questions

### What is COBTRAN?

COBTRAN is a tool designed to migrate legacy COBOL code to modern programming languages such as Python and Java. It parses COBOL source code, builds an abstract syntax tree (AST), and generates equivalent code in the target language while preserving the original business logic.

### Why would I want to migrate from COBOL?

There are several reasons organizations choose to migrate from COBOL:
- Shrinking pool of COBOL developers as many reach retirement age
- Difficulty integrating COBOL with modern technologies
- High maintenance costs for mainframe systems
- Challenges in recruiting new developers for COBOL positions
- Need for greater agility and faster development cycles

### Is COBTRAN free to use?

Yes, COBTRAN is an open-source project available under the MIT license. You are free to use, modify, and distribute it according to the terms of the license.

### What programming languages can COBTRAN migrate to?

Currently, COBTRAN supports migration to:
- Python
- Java

Support for additional languages is planned for future releases.

## Technical Questions

### Does COBTRAN support all COBOL features?

COBTRAN supports many common COBOL features, but not all. The current focus is on the most widely used constructs. See [COBOL Syntax Support](cobol_syntax.md) for details on which features are supported.

### How accurate is the migrated code?

COBTRAN aims to preserve the business logic and functionality of the original COBOL code. However, due to fundamental differences between COBOL and modern languages, the migrated code might not be a line-by-line translation. Some constructs might be implemented differently to follow best practices in the target language.

### Do I need to modify the generated code?

While COBTRAN generates working code, you might want to refine it for:
- Better alignment with target language idioms and patterns
- Performance optimizations
- Integration with existing systems
- Enhanced readability and maintainability

### Can COBTRAN migrate CICS, DB2, or other mainframe-specific code?

COBTRAN has limited support for mainframe-specific extensions like CICS and DB2. These are typically added as comments in the generated code, and you'll need to implement their functionality using appropriate libraries in the target language.

### How does COBTRAN handle COBOL copybooks?

COBTRAN processes COBOL copybooks by expanding them during the parsing phase. The definitions from copybooks are integrated into the AST and included in the generated code.

### What about COBOL data types and numeric operations?

COBTRAN maps COBOL data types to their closest equivalents in the target language. However, exact decimal precision in COBOL may not always map perfectly to native data types in modern languages. For critical financial calculations, you might need to use specialized decimal libraries in the target language.

## Migration Process

### How long does migration take?

The time required for migration depends on several factors:
- The size and complexity of your COBOL codebase
- The number of non-standard or mainframe-specific extensions used
- The level of post-migration refinement required
- Integration requirements with existing systems

For large systems, migration is typically a phased process rather than a one-time event.

### What is the recommended approach for migrating a large COBOL system?

For large systems, we recommend:
1. Start with a small, self-contained module to gain experience with the migration process
2. Develop comprehensive tests for the COBOL code before migration
3. Use these tests to verify the correctness of the migrated code
4. Migrate in phases, focusing on one module or function at a time
5. Run old and new systems in parallel during the transition period

### What if I find bugs or missing features in COBTRAN?

If you encounter bugs or find that COBTRAN is missing features you need, please:
1. Check the [GitHub issues](https://github.com/yourusername/cobtran/issues) to see if it's already reported
2. If not, open a new issue with a detailed description
3. Consider contributing a fix or enhancement if you can (see [Contributing](contributing.md))

## Performance and Maintenance

### Is the generated code efficient?

The primary focus of COBTRAN is correctness rather than optimization. While the generated code is functional, you might want to optimize performance-critical sections manually after migration.

### How maintainable is the generated code?

COBTRAN strives to generate readable, well-structured code that follows conventions of the target language. The code includes comments linking back to the original COBOL where appropriate. However, as with any automated translation, some constructs might be less idiomatic than hand-written code.

### Can I customize how the code is generated?

Yes, COBTRAN is designed to be extensible. You can customize the code generation process by:
1. Extending the `TargetModel` class
2. Creating custom generators
3. Modifying the transformation process

See the [Architecture](architecture.md) document for more details on extending COBTRAN.

## Getting Help

### Where can I get support for COBTRAN?

Support options include:
- Opening an issue on GitHub
- Consulting the documentation
- Reaching out to the community of users
- Contacting the project maintainers

### I have a specific question not covered here. What should I do?

If your question isn't covered in this FAQ, please:
1. Check the rest of the documentation
2. Search existing GitHub issues
3. Open a new issue with your question
4. Reach out to the project maintainers directly for complex inquiries
