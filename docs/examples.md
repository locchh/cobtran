# COBTRAN Examples

This document provides examples of COBOL code migrations using COBTRAN. Each example shows the original COBOL code and the resulting Python and Java code after migration.

## Basic Example: Hello World

### Original COBOL
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Hello, World!".
           STOP RUN.
```

### Migrated Python
```python
"""Hello - Migrated from COBOL"""

from dataclasses import dataclass, field


@dataclass
class Hello:
    """
    Hello - Migrated from COBOL
    """
    
    def main_logic(self):
        print("Hello, World!")
        return


if __name__ == '__main__':
    program = Hello()
    program.main_logic()
```

### Migrated Java
```java
/**
 * Hello - Migrated from COBOL
 */

public class Hello {
    
    public Hello() {
        // Constructor
    }
    
    public void mainLogic() {
        System.out.println("Hello, World!");
    }
    
    public static void main(String[] args) {
        Hello program = new Hello();
        program.mainLogic();
    }
}
```

## Counter Example

### Original COBOL
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER             PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM COUNT-UP 5 TIMES.
           STOP RUN.
           
       COUNT-UP.
           ADD 1 TO COUNTER.
           DISPLAY "Counter: " COUNTER.
```

### Migrated Python
```python
"""Counter - Migrated from COBOL"""

from dataclasses import dataclass, field


@dataclass
class Counter:
    """
    Counter - Migrated from COBOL
    """
    
    counter: int = 0
    
    def main_logic(self):
        for _ in range(5):
            self.count_up()
        return
    
    def count_up(self):
        self.counter += 1
        print(f"Counter: {self.counter}")


if __name__ == '__main__':
    program = Counter()
    program.main_logic()
```

### Migrated Java
```java
/**
 * Counter - Migrated from COBOL
 */

public class Counter {
    private int counter = 0;
    
    public Counter() {
        // Constructor
    }
    
    public void mainLogic() {
        for (int i = 0; i < 5; i++) {
            countUp();
        }
    }
    
    public void countUp() {
        counter += 1;
        System.out.println("Counter: " + counter);
    }
    
    public static void main(String[] args) {
        Counter program = new Counter();
        program.mainLogic();
    }
}
```

## Conditional Logic Example

### Original COBOL
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDITION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM                 PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter a number: ".
           ACCEPT NUM.
           
           IF NUM > 50
               DISPLAY "Number is greater than 50"
           ELSE IF NUM = 50
               DISPLAY "Number is equal to 50"
           ELSE
               DISPLAY "Number is less than 50"
           END-IF.
           
           STOP RUN.
```

### Migrated Python
```python
"""Condition - Migrated from COBOL"""

from dataclasses import dataclass, field


@dataclass
class Condition:
    """
    Condition - Migrated from COBOL
    """
    
    num: int = 0
    
    def main_logic(self):
        print("Enter a number: ")
        self.num = int(input())
        
        if self.num > 50:
            print("Number is greater than 50")
        elif self.num == 50:
            print("Number is equal to 50")
        else:
            print("Number is less than 50")
        
        return


if __name__ == '__main__':
    program = Condition()
    program.main_logic()
```

### Migrated Java
```java
/**
 * Condition - Migrated from COBOL
 */

import java.util.Scanner;

public class Condition {
    private int num = 0;
    
    public Condition() {
        // Constructor
    }
    
    public void mainLogic() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Enter a number: ");
        num = scanner.nextInt();
        
        if (num > 50) {
            System.out.println("Number is greater than 50");
        } else if (num == 50) {
            System.out.println("Number is equal to 50");
        } else {
            System.out.println("Number is less than 50");
        }
        
        scanner.close();
    }
    
    public static void main(String[] args) {
        Condition program = new Condition();
        program.mainLogic();
    }
}
```

## File Handling Example

### Original COBOL
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEREAD.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
          05 EMPLOYEE-ID       PIC 9(5).
          05 EMPLOYEE-NAME     PIC X(30).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF               PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT EMPLOYEE-FILE.
           PERFORM READ-EMPLOYEE-RECORD.
           PERFORM PROCESS-EMPLOYEE-RECORDS UNTIL WS-EOF = "Y".
           CLOSE EMPLOYEE-FILE.
           STOP RUN.
       
       READ-EMPLOYEE-RECORD.
           READ EMPLOYEE-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ.
       
       PROCESS-EMPLOYEE-RECORDS.
           DISPLAY "ID: " EMPLOYEE-ID ", Name: " EMPLOYEE-NAME.
           PERFORM READ-EMPLOYEE-RECORD.
```

### Migrated Python
```python
"""FileRead - Migrated from COBOL"""

from dataclasses import dataclass, field
from typing import Optional


@dataclass
class EmployeeRecord:
    employee_id: int = 0
    employee_name: str = ""


@dataclass
class FileRead:
    """
    FileRead - Migrated from COBOL
    """
    
    employee_record: EmployeeRecord = field(default_factory=EmployeeRecord)
    ws_eof: str = "N"
    employee_file = None
    
    def main_logic(self):
        self.employee_file = open("employees.dat", "r")
        self.read_employee_record()
        
        while self.ws_eof != "Y":
            self.process_employee_records()
        
        self.employee_file.close()
        return
    
    def read_employee_record(self):
        line = self.employee_file.readline()
        if not line:
            self.ws_eof = "Y"
            return
        
        # Parse fixed-width format
        if len(line) >= 35:  # 5 + 30 characters
            self.employee_record.employee_id = int(line[0:5])
            self.employee_record.employee_name = line[5:35].strip()
    
    def process_employee_records(self):
        print(f"ID: {self.employee_record.employee_id}, Name: {self.employee_record.employee_name}")
        self.read_employee_record()


if __name__ == '__main__':
    program = FileRead()
    program.main_logic()
```

### Migrated Java
```java
/**
 * FileRead - Migrated from COBOL
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class FileRead {
    private int employeeId = 0;
    private String employeeName = "";
    private String wsEof = "N";
    private BufferedReader employeeFile = null;
    
    public FileRead() {
        // Constructor
    }
    
    public void mainLogic() {
        try {
            employeeFile = new BufferedReader(new FileReader("employees.dat"));
            readEmployeeRecord();
            
            while (!wsEof.equals("Y")) {
                processEmployeeRecords();
            }
            
            employeeFile.close();
        } catch (IOException e) {
            System.err.println("Error processing file: " + e.getMessage());
        }
    }
    
    public void readEmployeeRecord() throws IOException {
        String line = employeeFile.readLine();
        if (line == null) {
            wsEof = "Y";
            return;
        }
        
        // Parse fixed-width format
        if (line.length() >= 35) {  // 5 + 30 characters
            employeeId = Integer.parseInt(line.substring(0, 5).trim());
            employeeName = line.substring(5, Math.min(line.length(), 35)).trim();
        }
    }
    
    public void processEmployeeRecords() throws IOException {
        System.out.println("ID: " + employeeId + ", Name: " + employeeName);
        readEmployeeRecord();
    }
    
    public static void main(String[] args) {
        FileRead program = new FileRead();
        program.mainLogic();
    }
}
```

## Running the Examples

1. Create the COBOL source files with the code above
2. Run COBTRAN to migrate them:

```bash
# Migrate to Python
python -m cobtran.cli migrate --source hello.cob --target python --output output/

# Migrate to Java
python -m cobtran.cli migrate --source hello.cob --target java --output output/
```

3. Run the migrated code:

```bash
# Run Python code
python output/hello.py

# Compile and run Java code
javac output/Hello.java
java -cp output Hello
```
