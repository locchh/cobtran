#!/usr/bin/env python3
"""Command-line interface for COBTRAN."""

import os
import sys
from pathlib import Path
from typing import List, Optional

import click

from cobtran.core import migrate_file, migrate_directory


@click.group()
@click.version_option()
def cli():
    """COBTRAN - COBOL to Modern Language Translator."""
    pass


@cli.command()
@click.option(
    "--source",
    "-s",
    required=True,
    type=click.Path(exists=True),
    help="Path to COBOL source file or directory",
)
@click.option(
    "--target",
    "-t",
    required=True,
    type=click.Choice(["python", "java"]),
    help="Target language for migration",
)
@click.option(
    "--output",
    "-o",
    required=True,
    type=click.Path(),
    help="Output directory for migrated code",
)
@click.option(
    "--verbose",
    "-v",
    is_flag=True,
    help="Enable verbose output",
)
def migrate(source: str, target: str, output: str, verbose: bool):
    """Migrate COBOL code to the specified target language."""
    source_path = Path(source)
    output_path = Path(output)
    
    if not output_path.exists():
        os.makedirs(output_path, exist_ok=True)
    
    if source_path.is_file():
        if not source_path.name.lower().endswith((".cob", ".cbl", ".cobol")):
            click.echo(f"Warning: {source_path} does not have a COBOL extension.")
        
        result = migrate_file(source_path, output_path, target, verbose)
        click.echo(f"Migration completed: {result}")
    
    elif source_path.is_dir():
        results = migrate_directory(source_path, output_path, target, verbose)
        click.echo(f"Migration completed for {len(results)} files")
        if verbose:
            for src, dest in results:
                click.echo(f"  {src} -> {dest}")
    
    else:
        click.echo("Error: Source path must be a file or directory")
        sys.exit(1)


if __name__ == "__main__":
    cli()
