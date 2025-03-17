# Contributing to COBTRAN

Thank you for your interest in contributing to COBTRAN! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Development Workflow](#development-workflow)
- [Submitting Contributions](#submitting-contributions)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Issue Tracking](#issue-tracking)
- [Contact](#contact)

## Code of Conduct

We expect all contributors to adhere to our Code of Conduct. Please be respectful, inclusive, and considerate in all interactions.

## Getting Started

1. Fork the repository on GitHub
2. Clone your fork to your local machine:
   ```bash
   git clone https://github.com/yourusername/cobtran.git
   cd cobtran
   ```
3. Add the original repository as an upstream remote:
   ```bash
   git remote add upstream https://github.com/originalowner/cobtran.git
   ```
4. Create a virtual environment and install the development dependencies:
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows, use: venv\Scripts\activate
   pip install -e ".[dev]"
   ```

## Development Setup

### Install Development Dependencies

In addition to the regular dependencies, you'll need development tools:

```bash
pip install -r requirements-dev.txt
```

This installs:
- pytest - For running tests
- black - For code formatting
- isort - For import sorting
- flake8 - For linting
- mypy - For type checking

### Configure Pre-commit Hooks

We recommend setting up pre-commit hooks to ensure your code meets our standards:

```bash
pip install pre-commit
pre-commit install
```

## Development Workflow

1. Create a branch for your work:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes and commit them with clear, descriptive messages:
   ```bash
   git add .
   git commit -m "Add feature X that does Y"
   ```

3. Keep your branch updated with the upstream repository:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

4. Run tests to make sure your changes don't break existing functionality:
   ```bash
   pytest
   ```

5. Format your code:
   ```bash
   black .
   isort .
   ```

## Submitting Contributions

1. Push your changes to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

2. Create a pull request (PR) from your branch to the upstream main branch.

3. Provide a clear description of the changes in your PR.

4. Be responsive to feedback and comments on your PR.

## Coding Standards

We follow these coding standards:

- Use [Black](https://black.readthedocs.io/) for code formatting
- Follow [PEP 8](https://www.python.org/dev/peps/pep-0008/) style guidelines
- Use type hints as specified in [PEP 484](https://www.python.org/dev/peps/pep-0484/)
- Write clear docstrings following [Google style](https://google.github.io/styleguide/pyguide.html#38-comments-and-docstrings)
- Keep functions and methods small and focused
- Use meaningful variable and function names

## Testing

All new features and bug fixes should include tests:

1. Write unit tests for your code using pytest
2. Place tests in the `tests/` directory
3. Ensure all tests pass before submitting a PR
4. Aim for high test coverage of your code

To run tests:

```bash
# Run all tests
pytest

# Run tests with coverage
pytest --cov=cobtran

# Run a specific test file
pytest tests/test_parser.py
```

## Documentation

Good documentation is crucial for this project:

1. Update or add docstrings for all public functions, classes, and methods
2. Update relevant documentation files in the `docs/` directory
3. Include examples for new features
4. Document any changes to the API or behavior

## Areas for Contribution

Here are some areas where contributions are particularly welcome:

1. **COBOL Feature Support**: Expanding the range of COBOL features and dialects supported
2. **Target Languages**: Adding support for additional target languages
3. **Code Optimization**: Improving the quality and efficiency of generated code
4. **Documentation**: Enhancing documentation, especially examples and tutorials
5. **Testing**: Adding test cases, especially for edge cases and complex COBOL constructs
6. **Performance**: Improving the speed and memory usage of the migration process

## Issue Tracking

- Check the project's issue tracker for open issues
- Comment on an issue if you're working on it
- Reference issues in your PR with keywords like "Fixes #123" or "Closes #456"

## Contact

If you have questions or need help, you can:

- Open an issue on GitHub
- Contact the project maintainers via email
- Join our community chat (if applicable)

Thank you for contributing to COBTRAN!
