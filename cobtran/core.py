"""Core functionality for COBTRAN."""

import os
from pathlib import Path
from typing import List, Tuple, Optional, Dict, Any

from cobtran.parser import parse_cobol
from cobtran.ast import CobolAST
from cobtran.transformers import transform_to_target
from cobtran.generators import generate_code


def migrate_file(
    source_file: Path,
    output_dir: Path,
    target_language: str,
    verbose: bool = False,
) -> str:
    """
    Migrate a single COBOL file to the target language.
    
    Args:
        source_file: Path to the source COBOL file
        output_dir: Directory where migrated code will be saved
        target_language: Target language for migration (python or java)
        verbose: Whether to print verbose output
    
    Returns:
        Path to the generated file
    """
    if verbose:
        print(f"Processing {source_file}...")
    
    # Read COBOL source
    with open(source_file, "r", encoding="utf-8") as f:
        cobol_source = f.read()
    
    # Parse COBOL to AST
    cobol_ast = parse_cobol(cobol_source)
    
    # Transform AST to target language model
    target_model = transform_to_target(cobol_ast, target_language)
    
    # Generate code in target language
    if target_language == "python":
        extension = ".py"
        output_filename = source_file.stem.lower() + extension
    else:  # java
        extension = ".java"
        # Convert to PascalCase for Java class name
        class_name = "".join(word.capitalize() for word in source_file.stem.split("-"))
        output_filename = class_name + extension
    
    output_path = output_dir / output_filename
    
    # Generate and write the code
    generated_code = generate_code(target_model, target_language)
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(generated_code)
    
    if verbose:
        print(f"Generated {output_path}")
    
    return str(output_path)


def migrate_directory(
    source_dir: Path,
    output_dir: Path,
    target_language: str,
    verbose: bool = False,
) -> List[Tuple[str, str]]:
    """
    Migrate all COBOL files in a directory to the target language.
    
    Args:
        source_dir: Directory containing source COBOL files
        output_dir: Directory where migrated code will be saved
        target_language: Target language for migration (python or java)
        verbose: Whether to print verbose output
    
    Returns:
        List of (source, target) file path pairs
    """
    results = []
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    for root, _, files in os.walk(source_dir):
        rel_path = os.path.relpath(root, source_dir)
        current_output_dir = output_dir if rel_path == "." else output_dir / rel_path
        os.makedirs(current_output_dir, exist_ok=True)
        
        for file in files:
            if file.lower().endswith((".cob", ".cbl", ".cobol")):
                source_file = Path(root) / file
                try:
                    output_path = migrate_file(
                        source_file, current_output_dir, target_language, verbose
                    )
                    results.append((str(source_file), output_path))
                except Exception as e:
                    if verbose:
                        print(f"Error processing {source_file}: {e}")
    
    return results
