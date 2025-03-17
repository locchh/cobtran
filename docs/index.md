# COBTRAN Documentation

Welcome to the COBTRAN documentation. This documentation provides comprehensive information about the COBTRAN tool, which is designed to migrate legacy COBOL code to modern programming languages such as Python and Java.

## Contents

- [Overview](overview.md) - Introduction to COBTRAN and its purpose
- [Architecture](architecture.md) - The architectural design of COBTRAN
- [Installation](installation.md) - How to install and set up COBTRAN
- [Usage Guide](usage.md) - How to use COBTRAN for code migration
- [COBOL Syntax Support](cobol_syntax.md) - Detailed information about supported COBOL syntax
- [Target Languages](target_languages.md) - Information about supported target languages
- [Examples](examples.md) - Example migrations using COBTRAN
- [Contributing](contributing.md) - How to contribute to the COBTRAN project
- [FAQ](faq.md) - Frequently Asked Questions

## Migration & Modernization Guides

- [Migration Guide](migration_guide.md) - Comprehensive guide to migrating COBOL applications
- [Modernization Approaches](modernization_approaches.md) - Various approaches to modernizing COBOL applications
- [COBOL to Modern Mapping](cobol_to_modern_mapping.md) - Technical mapping between COBOL and modern languages
- [Modernization Best Practices](modernization_best_practices.md) - Best practices for COBOL modernization
- [Case Studies](case_studies.md) - Real-world examples of successful COBOL migrations
- [Migration Automation](migration_automation.md) - How to automate COBOL migration with COBTRAN

## Quick Start

```bash
# Install COBTRAN
pip install -e .

# Run a basic migration
python -m cobtran.cli migrate --source examples/hello.cob --target python --output output/
```

## Support

If you encounter any issues or have questions, please file an issue on the project repository or contact the project maintainers.
