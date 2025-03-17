# Target Languages

COBTRAN currently supports migration to the following target languages:

## Python

Python is a high-level, interpreted programming language known for its readability and versatility. It has become one of the most popular programming languages for a wide range of applications, from web development to data science.

### Python Migration Details

When migrating COBOL to Python, COBTRAN:

- Creates Python classes to represent COBOL programs
- Uses Python data types that closely match COBOL data types
- Converts COBOL paragraphs to Python methods
- Implements COBOL data structures using appropriate Python constructs
- Uses Python's string handling for COBOL text manipulation
- Leverages Python libraries for file I/O operations

### Example Migration

**Original COBOL:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING            PIC X(20) VALUE "Hello, world!".
       01 COUNTER             PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY GREETING.
           PERFORM COUNT-UP 5 TIMES.
           STOP RUN.
           
       COUNT-UP.
           ADD 1 TO COUNTER.
           DISPLAY "Counter: " COUNTER.
```

**Migrated Python:**
```python
"""Hello - Migrated from COBOL"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional


@dataclass
class Hello:
    """
    Hello - Migrated from COBOL
    """
    
    greeting: str = "Hello, world!"
    counter: int = 0
    
    def main_logic(self):
        print(self.greeting)
        for _ in range(5):
            self.count_up()
        return
    
    def count_up(self):
        self.counter += 1
        print(f"Counter: {self.counter}")


if __name__ == '__main__':
    program = Hello()
    program.main_logic()
```

### Python Version Support

COBTRAN generates Python code compatible with Python 3.8 and newer.

## Java

Java is a class-based, object-oriented programming language designed to have as few implementation dependencies as possible. It's widely used in enterprise applications, web development, and Android app development.

### Java Migration Details

When migrating COBOL to Java, COBTRAN:

- Creates Java classes to represent COBOL programs
- Uses Java data types that closely match COBOL data types
- Converts COBOL paragraphs to Java methods
- Implements COBOL data structures using appropriate Java constructs
- Uses Java's string handling for COBOL text manipulation
- Leverages Java standard libraries for file I/O operations

### Example Migration

**Original COBOL:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING            PIC X(20) VALUE "Hello, world!".
       01 COUNTER             PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY GREETING.
           PERFORM COUNT-UP 5 TIMES.
           STOP RUN.
           
       COUNT-UP.
           ADD 1 TO COUNTER.
           DISPLAY "Counter: " COUNTER.
```

**Migrated Java:**
```java
/**
 * Hello - Migrated from COBOL
 */

import java.util.ArrayList;
import java.util.HashMap;

public class Hello {
    private String greeting = "Hello, world!";
    private int counter = 0;
    
    public Hello() {
        // Constructor
    }
    
    public void mainLogic() {
        System.out.println(greeting);
        for (int i = 0; i < 5; i++) {
            countUp();
        }
    }
    
    public void countUp() {
        counter += 1;
        System.out.println("Counter: " + counter);
    }
    
    public static void main(String[] args) {
        Hello program = new Hello();
        program.mainLogic();
    }
}
```

### Java Version Support

COBTRAN generates Java code compatible with Java 8 and newer.

## Mapping COBOL to Target Languages

### Data Types

| COBOL Type | Python Type | Java Type |
|------------|-------------|-----------|
| PIC X(n) | str | String |
| PIC 9(n) | int | int |
| PIC 9(n)V9(m) | float | double |
| PIC S9(n) | int | int |
| 01 (group item) | class | class |

### Control Structures

| COBOL Statement | Python Equivalent | Java Equivalent |
|-----------------|-------------------|-----------------|
| IF/ELSE | if/else | if/else |
| PERFORM n TIMES | for _ in range(n) | for loop |
| PERFORM UNTIL | while loop | while loop |
| PERFORM procedure | method call | method call |
| GO TO | (avoided, restructured) | (avoided, restructured) |

### I/O Operations

| COBOL Statement | Python Equivalent | Java Equivalent |
|-----------------|-------------------|-----------------|
| DISPLAY | print() | System.out.println() |
| ACCEPT | input() | Scanner input |
| OPEN/READ/WRITE | file operations | FileInputStream/FileOutputStream |

## Planned Target Languages

In future releases, COBTRAN aims to support additional target languages:

- JavaScript/TypeScript
- C#
- Go

## Customizing Target Language Output

COBTRAN is designed to be extensible. You can customize the code generation for target languages by:

1. Extending the `TargetModel` class in `transformers/transformer.py`
2. Implementing a new generator in `generators/generator.py`
3. Adding support for the new language in the CLI
