       IDENTIFICATION DIVISION.
       PROGRAM-ID. EZT-TO-COBOL-PRACTICE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DARL-PC.
       OBJECT-COMPUTER. XXX.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OUT-FILE ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01 IN-RECORD           PIC X(26).

       FD  OUT-FILE.
       01 OUT-RECORD          PIC X(40).

       WORKING-STORAGE SECTION.
       01 WS-IN-DATA.
          05 WS-IN-ID         PIC X(05).
          05 FILLER           PIC X(01).
          05 WS-IN-NAME       PIC X(20).

       01 WS-OUT-DATA.
          05 WS-OUT-ID        PIC X(05).
          05 FILLER           PIC X(01).
          05 WS-OUT-NAME      PIC X(20).
          05 WS-OUT-STATUS    PIC X(10).

       01 WS-COUNTERS.
          05 WS-TOTAL-COUNT   PIC 9(03).
          05 WS-ALUMNI-COUNT  PIC 9(03).
          05 WS-ACTIVE-COUNT  PIC 9(03).

       01 WS-HEADER.
          05 FILLER           PIC X(11) VALUE "---STUDENT ".
          05 FILLER           PIC X(16) VALUE "STATUS REPORT---".

       01 WS-FOOTER.
          05 FILLER           PIC X(08) VALUE "ACTIVE: ".
          05 WS-PRINT-ACTIVE  PIC ZZ9.
          05 FILLER           PIC X(03) VALUE " | ".
          05 FILLER           PIC X(08) VALUE "ALUMNI: ".
          05 WS-PRINT-ALUMNI  PIC ZZ9.
          05 FILLER           PIC X(03) VALUE " | ".
          05 FILLER           PIC X(07) VALUE "TOTAL: ".
          05 WS-PRINT-TOTAL   PIC ZZ9.

       01 WS-FLAGS.
          05 WS-EOF-FLAG      PIC X(01) VALUE 'N'.
             88 END-OF-FILE             VALUE 'Y'.
          05 WS-IN-STATUS     PIC X(02).

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY "--- PROGRAM START ---"

           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE

           IF WS-IN-STATUS NOT = "00"
              DISPLAY "ERROR: Input file status is: " WS-IN-STATUS
              STOP RUN
           ELSE
              DISPLAY "SUCCESS: Input file opened."
           END-IF.

           WRITE OUT-RECORD FROM WS-HEADER

           *> read file
           READ IN-FILE INTO WS-IN-DATA
           AT END
              SET END-OF-FILE TO TRUE
           END-READ

           IF END-OF-FILE
              DISPLAY "WARNING: File is empty/read failed immediately!"
           END-IF.

           PERFORM 1000-PROCESS-RECORDS UNTIL END-OF-FILE

           PERFORM 4000-MOVE-DATA-TO-PRINT

           WRITE OUT-RECORD FROM WS-FOOTER

           CLOSE IN-FILE OUT-FILE
           DISPLAY "--- PROGRAM END ---"
           STOP RUN.

       1000-PROCESS-RECORDS.
           DISPLAY "Checking ID: [" WS-IN-ID "]"

           MOVE SPACES TO WS-OUT-DATA

           IF WS-IN-ID >= "10000"
              PERFORM 2000-WRITE-ACTIVE-RECORDS
           ELSE
              IF WS-IN-ID < "10000"
                 PERFORM 3000-WRITE-ALUMNI-RECORDS
              END-IF
           END-IF
           ADD 1 TO WS-TOTAL-COUNT *> increment total count

           READ IN-FILE INTO WS-IN-DATA
           AT END
              SET END-OF-FILE TO TRUE
           END-READ.

       2000-WRITE-ACTIVE-RECORDS.
           ADD 1 TO WS-ACTIVE-COUNT *> increment active count
           MOVE "ACTIVE" TO WS-OUT-STATUS
           MOVE WS-IN-ID TO WS-OUT-ID
           MOVE WS-IN-NAME TO WS-OUT-NAME
           WRITE OUT-RECORD FROM WS-OUT-DATA.

       3000-WRITE-ALUMNI-RECORDS.
           ADD 1 TO WS-ALUMNI-COUNT *> increment alumni count
           MOVE "ALUMNI" TO WS-OUT-STATUS
           MOVE WS-IN-ID TO WS-OUT-ID
           MOVE WS-IN-NAME TO WS-OUT-NAME
           WRITE OUT-RECORD FROM WS-OUT-DATA.

       4000-MOVE-DATA-TO-PRINT.
           MOVE WS-ACTIVE-COUNT TO WS-PRINT-ACTIVE
           MOVE WS-ALUMNI-COUNT TO WS-PRINT-ALUMNI
           MOVE WS-TOTAL-COUNT TO WS-PRINT-TOTAL.
