# COBOL Migration Automation with COBTRAN

This guide demonstrates how to use COBTRAN for automating the migration of COBOL code to modern programming languages, covering both individual file migrations and large-scale projects.

## Table of Contents

- [Automation Fundamentals](#automation-fundamentals)
- [Command Line Usage](#command-line-usage)
- [Batch Processing](#batch-processing)
- [Customization Options](#customization-options)
- [Integration with DevOps](#integration-with-devops)
- [Handling Migration Failures](#handling-migration-failures)
- [Performance Optimization](#performance-optimization)
- [Post-Processing](#post-processing)

## Automation Fundamentals

COBTRAN is designed to be a key component in an automated COBOL migration pipeline. The tool converts COBOL code to modern languages through several phases:

1. **Parsing**: Converting COBOL source into an Abstract Syntax Tree (AST)
2. **Transformation**: Converting the COBOL AST into a target language model
3. **Generation**: Producing source code in the target language

Each of these phases can be customized and automated for large-scale migrations.

## Command Line Usage

### Basic Migration Command

The simplest way to use COBTRAN is through its command-line interface:

```bash
python -m cobtran.cli migrate --source path/to/source.cob --target python --output output/
```

This command:
- Parses the COBOL file at `path/to/source.cob`
- Transforms it to a Python model
- Generates Python code in the `output/` directory

### Migration Options

COBTRAN provides several options to customize the migration process:

```bash
python -m cobtran.cli migrate \
  --source path/to/source.cob \
  --target python \
  --output output/ \
  --dialect ibm \
  --preserve-comments \
  --generate-tests \
  --logging-level INFO
```

These options:
- Specify the COBOL dialect as IBM COBOL
- Preserve comments from the original code
- Generate unit tests for the migrated code
- Set the logging level to INFO for more detailed output

### Getting Help

For a complete list of options:

```bash
python -m cobtran.cli --help
python -m cobtran.cli migrate --help
```

## Batch Processing

For migrating multiple COBOL files, COBTRAN provides batch processing capabilities.

### Directory Migration

To migrate all COBOL files in a directory:

```bash
python -m cobtran.cli batch --source-dir path/to/cobol/files/ \
  --target python \
  --output output/ \
  --pattern "*.cob" \
  --preserve-directory-structure
```

This command:
- Processes all files with the `.cob` extension in the source directory
- Converts them to Python
- Maintains the same directory structure in the output directory

### Processing a File List

For more selective batch processing, you can use a file list:

```bash
python -m cobtran.cli batch --file-list migration_list.txt \
  --target java \
  --output output/ \
  --config config.json
```

Where `migration_list.txt` contains paths to COBOL files, one per line:

```
path/to/file1.cob
path/to/file2.cbl
path/to/subdir/file3.cob
```

### Parallel Processing

For large codebases, COBTRAN can utilize multiple processor cores:

```bash
python -m cobtran.cli batch --source-dir path/to/cobol/files/ \
  --target python \
  --output output/ \
  --workers 8 \
  --timeout 300
```

This command:
- Uses 8 worker processes for parallel migration
- Sets a timeout of 300 seconds per file

## Customization Options

### Configuration Files

For complex migrations, you can use a JSON configuration file:

```bash
python -m cobtran.cli migrate --config migration_config.json
```

Example `migration_config.json`:

```json
{
  "source": "path/to/source.cob",
  "target": "python",
  "output": "output/",
  "dialect": "ibm",
  "preserve_comments": true,
  "generate_tests": true,
  "custom_transformers": [
    "path/to/custom_transformer.py"
  ],
  "variable_naming": "snake_case",
  "include_original_code": true,
  "logging_level": "INFO"
}
```

### Custom Transformers

You can extend COBTRAN's capabilities with custom transformers:

```python
# custom_transformer.py
from cobtran.transformers.transformer import Transformer

class CustomTransformer(Transformer):
    def transform_program(self, cobol_ast):
        # Custom transformation logic
        model = super().transform_program(cobol_ast)
        
        # Add custom modifications
        model.metadata["migrated_by"] = "CustomTransformer"
        
        return model
```

And use it in your migration:

```bash
python -m cobtran.cli migrate --source file.cob --target python \
  --custom-transformer path/to/custom_transformer.py:CustomTransformer
```

### Custom Code Generators

Similarly, you can create custom code generators:

```python
# custom_generator.py
from cobtran.generators.generator import Generator

class CustomGenerator(Generator):
    def generate_program(self, model):
        # Custom generation logic
        code = super().generate_program(model)
        
        # Add custom header
        header = "# Generated by CustomGenerator\n\n"
        return header + code
```

## Integration with DevOps

### Continuous Integration

COBTRAN can be integrated into CI/CD pipelines. Here's an example GitHub Actions workflow:

```yaml
# .github/workflows/migrate.yml
name: COBOL Migration

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  migrate:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Set up Python
      uses: actions/setup-python@v2
      with:
        python-version: '3.8'
    
    - name: Install dependencies
      run: |
        python -m pip install --upgrade pip
        pip install ./cobtran
    
    - name: Migrate COBOL code
      run: |
        python -m cobtran.cli batch \
          --source-dir src/cobol/ \
          --target python \
          --output src/python/ \
          --report migration_report.json
    
    - name: Upload migration report
      uses: actions/upload-artifact@v2
      with:
        name: migration-report
        path: migration_report.json
```

### Automated Testing

After migration, you can automatically test the generated code:

```yaml
    - name: Run tests on migrated code
      run: |
        cd src/python/
        python -m pytest --cov=.
```

## Handling Migration Failures

### Error Reporting

COBTRAN provides detailed error reporting to help diagnose migration failures:

```bash
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --report migration_report.json \
  --continue-on-error
```

The `migration_report.json` will contain information about successful and failed migrations:

```json
{
  "summary": {
    "total": 100,
    "successful": 95,
    "failed": 5
  },
  "successful_files": [
    {
      "source": "cobol/file1.cob",
      "target": "python/file1.py",
      "time_taken": 2.3
    },
    ...
  ],
  "failed_files": [
    {
      "source": "cobol/problem_file.cob",
      "error": "Syntax error at line 42: Unexpected token",
      "time_taken": 1.5
    },
    ...
  ]
}
```

### Fallback Strategies

For files that fail automatic migration, you can implement fallback strategies:

1. **Manual Intervention**: Flag files for manual review
2. **Partial Migration**: Migrate parts of the code that can be handled automatically
3. **Alternative Approaches**: Try different migration settings or approaches

Example script for handling failed migrations:

```python
import json
import os
import subprocess

# Load migration report
with open('migration_report.json', 'r') as f:
    report = json.load(f)

# Process failed files
for failed in report['failed_files']:
    source = failed['source']
    
    # Try with a different dialect
    print(f"Attempting migration of {source} with alternative settings...")
    
    result = subprocess.run([
        'python', '-m', 'cobtran.cli', 'migrate',
        '--source', source,
        '--target', 'python',
        '--output', 'python/',
        '--dialect', 'ansi85',  # Try a different dialect
        '--preserve-problematic-code'  # Preserve code that can't be translated
    ])
    
    if result.returncode != 0:
        print(f"Second attempt failed for {source}, marking for manual review")
        with open('manual_review_list.txt', 'a') as review_file:
            review_file.write(f"{source}: {failed['error']}\n")
```

## Performance Optimization

### Memory Management

For large-scale migrations, memory management is crucial:

```bash
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --batch-size 20 \  # Process 20 files per batch
  --memory-limit 4096  # Limit memory usage to 4GB
```

### Performance Profiling

You can enable performance profiling to identify bottlenecks:

```bash
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --profile \
  --profile-output profiling_results.prof
```

Analyze the results with tools like `snakeviz`:

```bash
pip install snakeviz
snakeviz profiling_results.prof
```

## Post-Processing

### Code Formatting

After migration, it's often beneficial to format the generated code according to language standards:

```bash
# For Python
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --post-process "black {file}"

# For Java
python -m cobtran.cli batch --source-dir cobol/ --target java --output java/ \
  --post-process "google-java-format -r {file}"
```

### Static Analysis

You can integrate static analysis tools to identify potential issues in the generated code:

```bash
# For Python
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --post-process "pylint {file} >> pylint_report.txt"

# For Java
python -m cobtran.cli batch --source-dir cobol/ --target java --output java/ \
  --post-process "pmd -d {file} -R rulesets/java/quickstart.xml -f text >> pmd_report.txt"
```

### Documentation Generation

Generate documentation from the migrated code:

```bash
# For Python
python -m cobtran.cli batch --source-dir cobol/ --target python --output python/ \
  --post-process "cd python && pdoc --html {relative_file}"

# For Java
python -m cobtran.cli batch --source-dir cobol/ --target java --output java/ \
  --post-process "cd java && javadoc -d docs {relative_file}"
```

## Conclusion

COBTRAN provides a comprehensive set of tools for automating COBOL code migration. By leveraging these capabilities, organizations can:

1. Efficiently migrate large COBOL codebases
2. Maintain consistency across migrated code
3. Integrate migration into existing development workflows
4. Handle complex migration scenarios with customization

For large migrations, a combination of automation, customization, and careful validation provides the best approach.
