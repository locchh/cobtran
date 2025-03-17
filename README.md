# COBTRAN - COBOL to Modern Language Translator

A tool for migrating legacy COBOL code to modern programming languages such as Python and Java.

## Overview

COBTRAN is designed to help organizations modernize their legacy COBOL applications by providing a streamlined migration path to contemporary programming languages. The tool analyzes COBOL source code, builds an abstract syntax tree (AST), and generates equivalent code in the target language while preserving the original business logic.

## Features

- COBOL source code parsing and analysis
- AST (Abstract Syntax Tree) generation
- Code transformation to Python/Java
- Support for common COBOL constructs and patterns
- Detailed migration reports
- Comprehensive test suite for verifying migration accuracy

## Getting Started

### Prerequisites

- Python 3.8+
- Additional dependencies specified in `requirements.txt`

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/cobtran.git
cd cobtran

# Create a virtual environment
python -m venv venv
source venv/bin/activate  # On Windows, use: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### Usage

```bash
python -m cobtran.cli --source path/to/cobol/files --target python --output path/to/output
```

## Project Structure

- `cobtran/` - Main package
  - `parser/` - COBOL parsing modules
  - `ast/` - Abstract Syntax Tree representations
  - `transformers/` - Code transformation logic
  - `generators/` - Target language code generators
  - `utils/` - Utility functions
- `tests/` - Test suite
- `examples/` - Example COBOL programs and their translations

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- Tool Usage and Installation
  - [Overview](docs/overview.md)
  - [Installation Guide](docs/installation.md)
  - [Usage Guide](docs/usage.md)

- Technical References
  - [Architecture](docs/architecture.md)
  - [COBOL Syntax Support](docs/cobol_syntax.md)
  - [Target Languages](docs/target_languages.md)

- Migration and Modernization
  - [Migration Guide](docs/migration_guide.md)
  - [Modernization Approaches](docs/modernization_approaches.md)
  - [COBOL to Modern Mapping](docs/cobol_to_modern_mapping.md)
  - [Modernization Best Practices](docs/modernization_best_practices.md)
  - [Migration Automation](docs/migration_automation.md)
  - [Case Studies](docs/case_studies.md)

- Community
  - [Contributing Guide](docs/contributing.md)
  - [FAQ](docs/faq.md)

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
