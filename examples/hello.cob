       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MY-COMPUTER.
       OBJECT-COMPUTER. MY-COMPUTER.
       
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
