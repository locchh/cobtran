"""
Code generation from intermediate representation to target language.
"""

from typing import Dict, Any, List, Optional
import textwrap

from cobtran.transformers.transformer import TargetModel


def generate_code(model: TargetModel, target_language: str) -> str:
    """
    Generate code in the target language from the intermediate model.
    
    Args:
        model: The target model to generate code from
        target_language: The target language (python or java)
        
    Returns:
        Generated code as a string
    """
    if target_language == "python":
        return generate_python_code(model)
    elif target_language == "java":
        return generate_java_code(model)
    else:
        raise ValueError(f"Unsupported target language: {target_language}")


def generate_python_code(model: TargetModel) -> str:
    """
    Generate Python code from the intermediate model.
    
    Args:
        model: The target model to generate code from
        
    Returns:
        Generated Python code as a string
    """
    lines = [
        f'"""{model.class_name} - Migrated from COBOL"""',
        ""
    ]
    
    # Add imports
    for import_stmt in model.imports:
        lines.append(import_stmt)
    
    lines.append("")
    lines.append("")
    
    # Add class definition
    lines.append(f"@dataclass")
    lines.append(f"class {model.class_name}:")
    
    # Add class docstring
    lines.append(f'    """')
    lines.append(f'    {model.class_name} - Migrated from COBOL')
    lines.append(f'    """')
    lines.append("")
    
    # Add fields
    if model.fields:
        for field in model.fields:
            if field["value"] is not None:
                lines.append(f"    {field['name']}: {field['type']} = {field['value']}")
            else:
                lines.append(f"    {field['name']}: {field['type']}")
        lines.append("")
    
    # Add methods
    for method in model.methods:
        method_params = ", ".join(f"{p.get('name')}" for p in method["params"])
        if method_params and not method_params.startswith("self"):
            method_params = f"self, {method_params}"
        else:
            method_params = "self"
        
        lines.append(f"    def {method['name']}({method_params}):")
        for line in method["body"]:
            lines.append(f"        {line}")
        lines.append("")
    
    # Add main code blocks
    for code_block in model.main_code:
        if code_block["type"] == "if_main":
            for line in code_block["code"]:
                lines.append(line)
        lines.append("")
    
    return "\n".join(lines)


def generate_java_code(model: TargetModel) -> str:
    """
    Generate Java code from the intermediate model.
    
    Args:
        model: The target model to generate code from
        
    Returns:
        Generated Java code as a string
    """
    lines = [
        "/**",
        f" * {model.class_name} - Migrated from COBOL",
        " */",
        ""
    ]
    
    # Add imports
    for import_stmt in model.imports:
        lines.append(import_stmt)
    
    lines.append("")
    
    # Add class definition
    lines.append(f"public class {model.class_name} {{")
    
    # Add fields
    if model.fields:
        for field in model.fields:
            if field["value"] is not None:
                lines.append(f"    private {field['type']} {field['name']} = {field['value']};")
            else:
                lines.append(f"    private {field['type']} {field['name']};")
        lines.append("")
    
    # Add constructor
    lines.append(f"    public {model.class_name}() {{")
    lines.append(f"        // Constructor")
    lines.append(f"    }}")
    lines.append("")
    
    # Add methods
    for method in model.methods:
        return_type = method.get("return_type", "void")
        params_str = ", ".join(f"{p['type']} {p['name']}" for p in method["params"])
        
        lines.append(f"    public {return_type} {method['name']}({params_str}) {{")
        for line in method["body"]:
            lines.append(f"        {line}")
        lines.append("    }")
        lines.append("")
    
    # Close class definition
    lines.append("}")
    
    return "\n".join(lines)
