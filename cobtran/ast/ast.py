"""AST class definitions for representing COBOL programs."""

from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional


@dataclass
class Statement:
    """A single COBOL statement."""
    text: str
    # In a full implementation, this would have properties for the type 
    # of statement and its specific attributes


@dataclass
class Paragraph:
    """A COBOL paragraph, which is a named group of statements."""
    name: str
    statements: List[Statement] = field(default_factory=list)


@dataclass
class Section:
    """A COBOL section, which is a named group of paragraphs and statements."""
    name: str
    paragraphs: List[Paragraph] = field(default_factory=list)
    statements: List[Statement] = field(default_factory=list)


@dataclass
class Division:
    """A COBOL division, which is a major structural component of a COBOL program."""
    name: str
    sections: List[Section] = field(default_factory=list)
    paragraphs: List[Paragraph] = field(default_factory=list)
    statements: List[Statement] = field(default_factory=list)


@dataclass
class CobolAST:
    """
    The root of the Abstract Syntax Tree for a COBOL program.
    
    Attributes:
        divisions: List of divisions in the program
        statements: Any statements at the program level
    """
    divisions: List[Division] = field(default_factory=list)
    statements: List[Statement] = field(default_factory=list)
    
    def get_division(self, name: str) -> Optional[Division]:
        """Get a division by name."""
        for division in self.divisions:
            if name in division.name:
                return division
        return None
    
    def get_data_division(self) -> Optional[Division]:
        """Get the DATA DIVISION if it exists."""
        return self.get_division("DATA DIVISION")
    
    def get_procedure_division(self) -> Optional[Division]:
        """Get the PROCEDURE DIVISION if it exists."""
        return self.get_division("PROCEDURE DIVISION")
