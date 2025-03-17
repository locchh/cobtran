"""
Transformers for converting COBOL AST to target language intermediate representation.
"""

from .transformer import transform_to_target

__all__ = ["transform_to_target"]
