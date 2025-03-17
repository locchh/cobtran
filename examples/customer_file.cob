       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTFILE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMER.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID      PIC 9(5).
           05 CUSTOMER-NAME    PIC X(30).
           05 CUSTOMER-ADDRESS PIC X(50).
           05 CUSTOMER-PHONE   PIC X(15).
           05 BALANCE          PIC 9(7)V99.
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX VALUE SPACES.
       01 WS-EOF               PIC X VALUE "N".
       01 DISPLAY-BALANCE      PIC $,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILE.
           PERFORM READ-NEXT-RECORD.
           PERFORM PROCESS-RECORDS UNTIL WS-EOF = "Y".
           PERFORM CLOSE-FILE.
           STOP RUN.

       OPEN-FILE.
           OPEN INPUT CUSTOMER-FILE.
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file: " FILE-STATUS
               MOVE "Y" TO WS-EOF
           END-IF.

       READ-NEXT-RECORD.
           READ CUSTOMER-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ.
           IF FILE-STATUS NOT = "00" AND FILE-STATUS NOT = "10"
               DISPLAY "Error reading file: " FILE-STATUS
               MOVE "Y" TO WS-EOF
           END-IF.

       PROCESS-RECORDS.
           MOVE BALANCE TO DISPLAY-BALANCE.
           DISPLAY "Customer: " CUSTOMER-ID 
                   " - " CUSTOMER-NAME.
           DISPLAY "Balance: " DISPLAY-BALANCE.
           DISPLAY "-----------------------------------".
           PERFORM READ-NEXT-RECORD.

       CLOSE-FILE.
           CLOSE CUSTOMER-FILE.
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error closing file: " FILE-STATUS
           END-IF.
