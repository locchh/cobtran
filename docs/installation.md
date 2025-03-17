# COBTRAN Installation Guide

This guide provides instructions on how to install and set up COBTRAN on your system.

## Prerequisites

Before installing COBTRAN, ensure your system meets the following requirements:

- Python 3.8 or higher
- pip (Python package manager)
- Git (for cloning the repository)

## Installation Options

### Option 1: Install from Source (Recommended for Development)

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/cobtran.git
   cd cobtran
   ```

2. Create and activate a virtual environment (optional but recommended):
   ```bash
   # On Linux/macOS
   python -m venv venv
   source venv/bin/activate

   # On Windows
   python -m venv venv
   venv\Scripts\activate
   ```

3. Install in development mode:
   ```bash
   pip install -e .
   ```

   This installs COBTRAN in "editable" mode, which means changes to the source code will take effect without reinstalling.

### Option 2: Install from PyPI (Coming Soon)

```bash
pip install cobtran
```

### Option 3: Install Using Requirements File

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/cobtran.git
   cd cobtran
   ```

2. Create and activate a virtual environment (optional but recommended):
   ```bash
   # On Linux/macOS
   python -m venv venv
   source venv/bin/activate

   # On Windows
   python -m venv venv
   venv\Scripts\activate
   ```

3. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

## Verifying the Installation

To verify that COBTRAN was installed correctly, run:

```bash
python -m cobtran.cli --version
```

You should see the current version of COBTRAN displayed.

## Development Setup

If you plan to contribute to COBTRAN, you may want to install additional development dependencies:

```bash
pip install -r requirements-dev.txt
```

This will install tools like pytest, flake8, black, and mypy that are useful for development and testing.

## Troubleshooting

### Common Issues

1. **Missing Dependencies**:
   If you encounter errors about missing dependencies, try installing them manually:
   ```bash
   pip install lark-parser click jinja2
   ```

2. **Python Version**:
   COBTRAN requires Python 3.8 or higher. Check your Python version with:
   ```bash
   python --version
   ```

3. **Permission Issues**:
   If you encounter permission errors during installation, try:
   ```bash
   # On Linux/macOS
   pip install --user -e .

   # Or use sudo (not recommended for security reasons)
   sudo pip install -e .
   ```

4. **Path Issues**:
   If the `cobtran` command is not found after installation, ensure that your Python scripts directory is in your PATH.

### Getting Help

If you continue to experience issues with installation, please:

1. Check the [FAQ](faq.md) for common problems and solutions
2. File an issue on the GitHub repository
3. Contact the project maintainers for assistance
