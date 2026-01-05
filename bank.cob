       IDENTIFICATION DIVISION.
       PROGRAM-ID. bank.
       AUTHOR. Darl Floresca.
       DATE-WRITTEN.January 5, 2025.

       ENVIRONMENT DIVISION. 

       DATA DIVISION. 
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CARDHOLDER.
           02 HOLDERID PIC 9(10) VALUE 10000.
           02 FULLNAME PIC AAA VALUE "Darl Floresca". 
           02 DATEOFBIRTH.
               03 MONTH PIC 99.
               03 DAYOFBIRTH PIC 99.
               03 YEAROFBIRTH PIC 9(4).
       01 CARDNUMBER.
           02 CVV PIC 999.
       01 SAVINGS  PIC 9(8)V99 VALUE 10000.
       01 WITHDRAW PIC 9(8)V99 VALUE ZEROS.
       01 TOTAL PIC 9(8)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
           DISPLAY "WELCOME TO BANK"
           DISPLAY "Input your credit card number: " WITH NO ADVANCING.
           DISPLAY "WELCOME ", FULLNAME
           
           ACCEPT CARDNUMBER.
           DISPLAY "Current Savings: ", SAVINGS.
           DISPLAY "Enter withdraw number: " WITH NO ADVANCING.
           ACCEPT WITHDRAW.
           COMPUTE TOTAL = SAVINGS - WITHDRAW.
           DISPLAY "Balance remaining: ", TOTAL.
           DISPLAY "Withdrawed ", WITHDRAW.

           STOP RUN.
