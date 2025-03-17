       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVOICECALC.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INVOICE-DATA.
          05 INVOICE-NUMBER    PIC X(10).
          05 INVOICE-DATE      PIC X(10).
          05 CUSTOMER-DATA.
             10 CUSTOMER-ID    PIC X(8).
             10 CUSTOMER-NAME  PIC X(30).
             10 CUSTOMER-TYPE  PIC X VALUE SPACES.
                88 REGULAR     VALUE "R".
                88 PREMIUM     VALUE "P".
                88 WHOLESALE   VALUE "W".
          05 ITEM-TABLE.
             10 ITEMS OCCURS 10 TIMES INDEXED BY ITEM-IDX.
                15 ITEM-ID     PIC X(8).
                15 ITEM-DESC   PIC X(20).
                15 ITEM-PRICE  PIC 9(5)V99.
                15 ITEM-QTY    PIC 9(3).
                15 ITEM-TOTAL  PIC 9(7)V99.
          05 SUBTOTAL          PIC 9(7)V99 VALUE ZEROS.
          05 DISCOUNT-RATE     PIC V99 VALUE ZEROS.
          05 DISCOUNT-AMOUNT   PIC 9(7)V99 VALUE ZEROS.
          05 TAX-RATE          PIC V99 VALUE 0.08.
          05 TAX-AMOUNT        PIC 9(7)V99 VALUE ZEROS.
          05 TOTAL-AMOUNT      PIC 9(7)V99 VALUE ZEROS.
       
       01 DISPLAY-FIELDS.
          05 DISPLAY-SUBTOTAL    PIC $,$$$,$$9.99.
          05 DISPLAY-DISCOUNT    PIC $,$$$,$$9.99.
          05 DISPLAY-TAX         PIC $,$$$,$$9.99.
          05 DISPLAY-TOTAL       PIC $,$$$,$$9.99.
          05 DISPLAY-ITEM-PRICE  PIC $,$$$,$$9.99.
          05 DISPLAY-ITEM-TOTAL  PIC $,$$$,$$9.99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-INVOICE.
           PERFORM PROCESS-ITEMS.
           PERFORM CALCULATE-TOTALS.
           PERFORM DISPLAY-INVOICE.
           STOP RUN.
       
       INITIALIZE-INVOICE.
           MOVE "INV-12345" TO INVOICE-NUMBER.
           MOVE "2025-03-18" TO INVOICE-DATE.
           MOVE "CUST-789" TO CUSTOMER-ID.
           MOVE "ACME CORPORATION" TO CUSTOMER-NAME.
           MOVE "P" TO CUSTOMER-TYPE.
           
           MOVE "ITEM-001" TO ITEM-ID(1).
           MOVE "Widget A" TO ITEM-DESC(1).
           MOVE 19.99 TO ITEM-PRICE(1).
           MOVE 5 TO ITEM-QTY(1).
           
           MOVE "ITEM-002" TO ITEM-ID(2).
           MOVE "Widget B" TO ITEM-DESC(2).
           MOVE 29.99 TO ITEM-PRICE(2).
           MOVE 3 TO ITEM-QTY(2).
           
           MOVE "ITEM-003" TO ITEM-ID(3).
           MOVE "Widget C" TO ITEM-DESC(3).
           MOVE 49.99 TO ITEM-PRICE(3).
           MOVE 2 TO ITEM-QTY(3).
           
           MOVE "ITEM-004" TO ITEM-ID(4).
           MOVE "Widget D" TO ITEM-DESC(4).
           MOVE 99.99 TO ITEM-PRICE(4).
           MOVE 1 TO ITEM-QTY(4).
       
       PROCESS-ITEMS.
           PERFORM VARYING ITEM-IDX FROM 1 BY 1 
               UNTIL ITEM-IDX > 10
               
               IF ITEM-ID(ITEM-IDX) NOT = SPACES
                   COMPUTE ITEM-TOTAL(ITEM-IDX) = 
                       ITEM-PRICE(ITEM-IDX) * ITEM-QTY(ITEM-IDX)
                   ADD ITEM-TOTAL(ITEM-IDX) TO SUBTOTAL
               END-IF
           END-PERFORM.
       
       CALCULATE-TOTALS.
           EVALUATE TRUE
               WHEN REGULAR
                   IF SUBTOTAL > 1000
                       MOVE 0.05 TO DISCOUNT-RATE
                   ELSE
                       MOVE 0.00 TO DISCOUNT-RATE
                   END-IF
               WHEN PREMIUM
                   IF SUBTOTAL > 1000
                       MOVE 0.10 TO DISCOUNT-RATE
                   ELSE
                       MOVE 0.05 TO DISCOUNT-RATE
                   END-IF
               WHEN WHOLESALE
                   IF SUBTOTAL > 5000
                       MOVE 0.15 TO DISCOUNT-RATE
                   ELSE IF SUBTOTAL > 1000
                       MOVE 0.10 TO DISCOUNT-RATE
                   ELSE
                       MOVE 0.07 TO DISCOUNT-RATE
                   END-IF
           END-EVALUATE.
           
           COMPUTE DISCOUNT-AMOUNT = SUBTOTAL * DISCOUNT-RATE.
           COMPUTE TAX-AMOUNT = (SUBTOTAL - DISCOUNT-AMOUNT) * TAX-RATE.
           COMPUTE TOTAL-AMOUNT = SUBTOTAL - DISCOUNT-AMOUNT + TAX-AMOUNT.
       
       DISPLAY-INVOICE.
           DISPLAY "INVOICE: " INVOICE-NUMBER.
           DISPLAY "DATE: " INVOICE-DATE.
           DISPLAY "CUSTOMER: " CUSTOMER-NAME.
           DISPLAY "----------------------------------------".
           DISPLAY "ITEMS:".
           
           PERFORM VARYING ITEM-IDX FROM 1 BY 1 
               UNTIL ITEM-IDX > 10
               
               IF ITEM-ID(ITEM-IDX) NOT = SPACES
                   MOVE ITEM-PRICE(ITEM-IDX) TO DISPLAY-ITEM-PRICE
                   MOVE ITEM-TOTAL(ITEM-IDX) TO DISPLAY-ITEM-TOTAL
                   DISPLAY ITEM-DESC(ITEM-IDX) " x " ITEM-QTY(ITEM-IDX)
                           " @ " DISPLAY-ITEM-PRICE
                           " = " DISPLAY-ITEM-TOTAL
               END-IF
           END-PERFORM.
           
           MOVE SUBTOTAL TO DISPLAY-SUBTOTAL.
           MOVE DISCOUNT-AMOUNT TO DISPLAY-DISCOUNT.
           MOVE TAX-AMOUNT TO DISPLAY-TAX.
           MOVE TOTAL-AMOUNT TO DISPLAY-TOTAL.
           
           DISPLAY "----------------------------------------".
           DISPLAY "SUBTOTAL: " DISPLAY-SUBTOTAL.
           DISPLAY "DISCOUNT: " DISPLAY-DISCOUNT.
           DISPLAY "TAX: " DISPLAY-TAX.
           DISPLAY "TOTAL: " DISPLAY-TOTAL.
