# COBTRAN Usage Guide

This guide provides instructions on how to use COBTRAN to migrate COBOL code to modern programming languages.

## Command-Line Interface

COBTRAN provides a command-line interface (CLI) for easy migration of COBOL code.

### Basic Usage

The basic command to migrate a single COBOL file is:

```bash
python -m cobtran.cli migrate --source path/to/cobol/file.cob --target python --output path/to/output
```

For example, to migrate the example COBOL file to Python:

```bash
python -m cobtran.cli migrate --source examples/hello.cob --target python --output output/
```

### Command Options

The `migrate` command supports the following options:

- `--source`, `-s`: Path to the COBOL source file or directory (required)
- `--target`, `-t`: Target language for migration, either `python` or `java` (required)
- `--output`, `-o`: Output directory for migrated code (required)
- `--verbose`, `-v`: Enable verbose output

### Migrating Multiple Files

To migrate all COBOL files in a directory:

```bash
python -m cobtran.cli migrate --source path/to/cobol/directory --target python --output path/to/output
```

COBTRAN will recursively search for files with `.cob`, `.cbl`, or `.cobol` extensions and migrate each one.

## Output Structure

When migrating a single file, COBTRAN will create a file in the output directory with the same name but with an appropriate extension for the target language:

- For Python: `.py` extension
- For Java: `.java` extension

When migrating a directory, COBTRAN will preserve the directory structure in the output directory.

## Examples

### Migrating a Single File to Python

```bash
python -m cobtran.cli migrate --source examples/hello.cob --target python --output output/
```

This will create `output/hello.py` with the Python equivalent of the COBOL code.

### Migrating a Single File to Java

```bash
python -m cobtran.cli migrate --source examples/hello.cob --target java --output output/
```

This will create `output/Hello.java` with the Java equivalent of the COBOL code.

### Migrating Multiple Files with Verbose Output

```bash
python -m cobtran.cli migrate --source examples/ --target python --output output/ --verbose
```

This will migrate all COBOL files in the `examples/` directory to Python, with detailed output about the migration process.

## Handling Migration Results

After migration, you should:

1. Review the generated code to ensure it correctly implements the business logic
2. Run tests to verify the functionality matches the original COBOL program
3. Make any necessary adjustments or optimizations to the generated code

## Advanced Usage

### Programmatic API

COBTRAN can also be used programmatically in your Python code:

```python
from cobtran.core import migrate_file

# Migrate a single file
output_path = migrate_file(
    source_file="path/to/cobol/file.cob",
    output_dir="path/to/output",
    target_language="python",
    verbose=True
)

print(f"Migrated file saved to: {output_path}")
```

### Customization

COBTRAN is designed to be extensible. You can customize various aspects of the migration process by extending or modifying the core classes:

- Custom COBOL dialect support by extending the parser
- Custom target language generators
- Custom transformations for specific COBOL patterns

See the [Architecture](architecture.md) document for more details on how to extend COBTRAN.

## Limitations

Be aware of the following limitations when using COBTRAN:

1. Not all COBOL features are supported
2. Complex or platform-specific features may require manual adjustment
3. The generated code might need optimization for performance
4. Some COBOL idioms might not have direct equivalents in the target language

## Troubleshooting

If you encounter issues during migration:

1. Use the `--verbose` flag to get more detailed output
2. Check that your COBOL code follows standard COBOL syntax
3. Ensure the output directory exists and is writable
4. Refer to the [FAQ](faq.md) for common issues and solutions
