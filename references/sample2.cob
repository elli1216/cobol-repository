       IDENTIFICATION DIVISION.
        PROGRAM-ID.   ACICACR8.
        AUTHOR.       KRYSTLER CATANIAG.
      * REMARKS.      ACR8
      ******************************************************************
      * AUTHOR: KRYSTLER CATANIAG.
      * DATE: 10/06/2025
      * PURPOSE: STEP 100 - ACR8
      * LAST UPDATED: 10/16/2025 - KRYSTLER CATANIAG.
      ******************************************************************
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
 
           SELECT IFILEA ASSIGN TO IFILEA
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS FILKEY
               FILE STATUS IS WS-FS1.
 
           SELECT OFILEA ASSIGN TO SYS011-UR-1403-S.
 
       DATA DIVISION.
       FILE SECTION.
       FD IFILEA
           RECORD CONTAINS 160 CHARACTERS
           DATA RECORD IS IFILEA-REC.
 
 
       01 IFILEA-REC.
          05 FILKEY.
             10 ACCTNO             PIC 9(10).
             10 ACCTNO-CTL3 REDEFINES ACCTNO.
                15 CTL3            PIC 9(03).
                15 FILLER          PIC 9(07).
             10 ACCTNO-REDEF REDEFINES ACCTNO.
                15 ACCTNO-PART1    PIC 9(04).
                15 ACCTNO-PART2    PIC 9(04).
                15 ACCTNO-PART3    PIC 9(02).
             10 CHKNO              PIC 9(10).
             10 TRNDTE             PIC 9(08).
             10 EMPL               PIC X(04).
             10 TERM               PIC X(04).
             10 FILTIME            PIC 9(06).
          05 FILLER                PIC 9(04).
          05 AMT                   PIC 9(10)V99.
          05 PAYEE                 PIC X(20).
          05 FILLER                PIC 9(10).
          05 FILLER                PIC 9(10).
          05 CHKDATE               PIC 9(08).
          05 STAT                  PIC 9(02).
          05 CHKNEW                PIC X.
          05 CHKUPDT               PIC X.
          05 TRXNCD                PIC 9(03).
          05 OLDVAL                PIC X(06).
 
       FD OFILEA
           RECORDING MODE IS F.
       01 OFILEA-REC               PIC X(131).
 
       WORKING-STORAGE SECTION.
       COPY SIWSCNTL.
       COPY STWSLU.
       01 WS-REPORT-LINES.
          05 HEADER1.
             10 FILLER             PIC X(01)           VALUE SPACE.
             10 FILLER             PIC X(04)           VALUE 'BANK'.
             10 FILLER             PIC X(07)           VALUE SPACES.
             10 FILLER             PIC X(03)           VALUE '51'.
             10 FILLER             PIC X(36)           VALUE SPACES.
             10 FILLER             PIC X(28)           VALUE
                   'LAND BANK OF THE PHILIPPINES'.
             10 FILLER             PIC X(18)           VALUE SPACES.
             10 FILLER             PIC X(09)           VALUE '*ACAURPT*'
           .
             10 FILLER             PIC X(14)           VALUE SPACES.
             10 FILLER             PIC X(05)           VALUE 'PAGE '.
             10 PAGENUM            PIC ZZ,ZZ9.
 
          05 HEADER2.
             10 FILLER             PIC X(01)           VALUE SPACE.
             10 FILLER             PIC X(08)           VALUE 'CURRENCY'.
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 FILLER             PIC X(03)           VALUE '000'.
             10 FILLER             PIC X(38)           VALUE SPACES.
             10 FILLER             PIC X(25)           VALUE
                   'DEMAND DEPOSIT ACCOUNTING'.
 
          05 HEADER3.
             10 FILLER             PIC X(01)           VALUE SPACE.
             10 FILLER             PIC X(06)           VALUE 'BRANCH'.
             10 FILLER             PIC X(05)           VALUE SPACES.
             10 DSP-CTL3           PIC 9(03).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 BRNAME             PIC X(30).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 FILLER             PIC X(28)           VALUE
                   'ACICREG STATUS CHANGE REPORT'.
 
          05 HEADER4.
             10 FILLER             PIC X(58)           VALUE SPACES.
             10 FILLER             PIC X(06)           VALUE 'AS OF '.
             10 TLE-MM             PIC 9(02).
             10 TLE-DD             PIC 9(02).
             10 TLE-YYYY           PIC X(04).
 
          05 HEADER5.
             10 FILLER             PIC X(05)           VALUE SPACES.
             10 FILLER             PIC X(07)           VALUE 'TRNDESC'.
             10 FILLER             PIC X(07)           VALUE SPACES.
             10 FILLER             PIC X(06)           VALUE 'ACCTNO'.
             10 FILLER             PIC X(08)           VALUE SPACES.
             10 FILLER             PIC X(05)           VALUE 'CHKNO'.
             10 FILLER             PIC X(07)           VALUE SPACES.
             10 FILLER             PIC X(07)           VALUE 'CHKDATE'.
             10 FILLER             PIC X(13)           VALUE SPACES.
             10 FILLER             PIC X(03)           VALUE 'AMT'.
             10 FILLER             PIC X(12)           VALUE SPACES.
             10 FILLER             PIC X(04)           VALUE 'STAT'.
             10 FILLER             PIC X(10)           VALUE SPACES.
             10 FILLER             PIC X(05)           VALUE 'PAYEE'.
             10 FILLER             PIC X(11)           VALUE SPACES.
             10 FILLER             PIC X(04)           VALUE 'EMPL'.
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 FILLER             PIC X(04)           VALUE 'TERM'.
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 FILLER             PIC X(06)           VALUE 'OLDVAL'.
 
          05 DETAIL-LINE.
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-TRNDESC         PIC X(09) JUSTIFIED RIGHT.
             10 FILLER             PIC X(04)           VALUE SPACES.
             10 WS-ACCTNO.
                15 ACCNO-1         PIC X(04).
                15 FILLER          PIC X               VALUE '-'.
                15 ACCNO-2         PIC X(04).
                15 FILLER          PIC X               VALUE '-'.
                15 ACCNO-3         PIC X(02).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-CHKNO           PIC X(10).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-CHKDATE         PIC 99/99/9999.
             10 FILLER             PIC X(06)           VALUE SPACES.
             10 WS-AMT             PIC Z,ZZZ,ZZZ,ZZ9.99.
             10 FILLER             PIC X(05)           VALUE SPACES.
             10 WS-STAT            PIC X(02).
             10 FILLER             PIC X(04)           VALUE SPACES.
             10 WS-PAYEE           PIC X(20).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-EMPL            PIC X(04).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-TERM            PIC X(04).
             10 FILLER             PIC X(03)           VALUE SPACES.
             10 WS-OLDVAL          PIC X(02).
 
          05 BLKLINE               PIC X(01)           VALUE SPACE.
 
       01 WS-CONTROL-FIELDS.
          05 WS-EOF                PIC 9               VALUE 0.
          05 WS-EOF-LKU            PIC 9               VALUE 0.
          05 WS-CURRENT-BRANCH     PIC 9(03)           VALUE ZEROES.
          05 WS-PREV-BRANCH        PIC 9(03)           VALUE ZEROES.
          05 WS-PAGENEW            PIC 9               VALUE 0.
          05 WS-PAGES              PIC 9(04)           VALUE 0.
          05 WS-PAGE-LIMIT         PIC 9(02)           VALUE 0.
          05 WS-SETPAGE-LIMIT      PIC 9(02)           VALUE 50.
          05 WS-FIRST-PAGE         PIC X               VALUE 'Y'.
 
       01 WS-CURRDATE-YMD.
          05 WS-CURRDATE-YMD-YYYY  PIC 9(04).
          05 WS-CURRDATE-YMD-MM    PIC 9(02).
          05 WS-CURRDATE-YMD-DD    PIC 9(02).
 
       01 WS-CURRDTE.
          05 WS-CURRDTE-MM         PIC 9(02).
          05 WS-CURRDTE-DD         PIC 9(02).
          05 WS-CURRDTE-YYYY       PIC 9(04).
 
       01 PG-COUNT                 PIC 9(03)           VALUE 1.
 
       01 WS-FS1                   PIC X(02)           VALUE ZERO.
       01 WS-FS2                   PIC X(02)           VALUE ZERO.
 
       PROCEDURE DIVISION.
       0000-MAIN-RTN SECTION.
       0000-MAIN.
           ACCEPT WS-CURRDATE-YMD FROM SYSIN
 
           MOVE WS-CURRDATE-YMD-MM TO WS-CURRDTE-MM
           MOVE WS-CURRDATE-YMD-DD TO WS-CURRDTE-DD
           MOVE WS-CURRDATE-YMD-YYYY TO WS-CURRDTE-YYYY
 
           MOVE WS-CURRDATE-YMD-YYYY TO TLE-YYYY
           MOVE WS-CURRDATE-YMD-MM TO TLE-MM
           MOVE WS-CURRDATE-YMD-DD TO TLE-DD
 
           PERFORM 0100-INITIALIZE-RTN THRU 0100-EXIT
           PERFORM 0200-PROCESS-FILE-RTN THRU 0200-EXIT UNTIL WS-EOF = 1
           PERFORM 0900-FINALIZE-RTN THRU 0900-EXIT
 
           STOP RUN.
       0000-EXIT.
           EXIT.
 
       0100-INITIALIZE-RTN SECTION.
       0100-INITIALIZE.
 
           OPEN INPUT IFILEA
                OUTPUT OFILEA
 
           MOVE ZEROS TO FILKEY
           START IFILEA KEY IS >= FILKEY
           INVALID KEY
                   MOVE '1' TO WS-EOF
           END-START.
 
      *    IF WS-FS1 = '00'
      *        DISPLAY 'READ SUCCESS FOR IFILEA'
      *    ELSE
      *        DISPLAY 'IFILEA FILE STATUS CODE: ' WS-FS1
      *        STOP RUN
      *    END-IF
 
           MOVE 'I' TO I-O-CONTROL-ACCESS.
           MOVE 'O' TO I-O-CONTROL-OPERATOR.
           CALL 'STLKPMV' USING I-O-CONTROL-AREA,
                                STWS-LOOKUP-RECORD.
 
       0100-EXIT.
           EXIT.
 
       0175-BRANCH-LU-RTN SECTION.
       0175-BRANCH-LU.
 
           MOVE SPACES TO STWS-LU-CONTROL.
           MOVE '51' TO STWS-LU-CTL1.
           MOVE '000' TO STWS-LU-CTL2.
           MOVE CTL3 TO STWS-LU-CTL3
                        STWS-LOOKUP
                        STWS-LU-VALUE5.
           MOVE 'L' TO STWS-LU-REC-ID.
           MOVE '02' TO STWS-LU-FIELD.
           MOVE SPACES TO STWS-LU-LANGUAGE.
 
           MOVE 'K' TO I-O-CONTROL-OPERATOR.
 
           CALL 'STLKPMV' USING I-O-CONTROL-AREA,
                                STWS-LOOKUP-RECORD.
 
           IF I-O-88-NOT-FOUND
              MOVE 'BRANCH NOT FOUND' TO BRNAME OF HEADER3
           ELSE
              MOVE STWS-LU-DESC-30 TO BRNAME OF HEADER3
              MOVE CTL3 TO DSP-CTL3
           END-IF.
 
       0175-EXIT.
           EXIT.
 
       0200-PROCESS-FILE-RTN SECTION.
       0200-PROCESS-FILE.
 
           READ IFILEA NEXT RECORD
           AT END
              MOVE 1 TO WS-EOF
           NOT AT END
               IF TRNDTE = WS-CURRDTE
 
                  IF WS-PREV-BRANCH NOT = CTL3
                     MOVE 1 TO WS-PAGENEW
                     PERFORM 0175-BRANCH-LU-RTN THRU 0175-EXIT
                     MOVE CTL3 TO WS-PREV-BRANCH
                  END-IF
 
                  EVALUATE TRUE
                  WHEN TRXNCD = 002
                       MOVE 'DEL ACIC' TO WS-TRNDESC
                  WHEN TRXNCD = 004
                       MOVE 'UPD STAT' TO WS-TRNDESC
                  WHEN TRXNCD = 005
                       MOVE 'UPDT AMT' TO WS-TRNDESC
                  WHEN TRXNCD = 006
                       MOVE 'UPD DATE' TO WS-TRNDESC
                  WHEN TRXNCD = 007
                       MOVE 'UPD PAYEE' TO WS-TRNDESC
                  END-EVALUATE
 
                  IF TRXNCD = 002 OR TRXNCD = 004 OR
                     TRXNCD = 005 OR TRXNCD = 006 OR
                     TRXNCD = 007
 
                     PERFORM 0250-PROCESS-RECORD-RTN
                  END-IF
               END-IF
           END-READ.
 
       0200-EXIT.
           EXIT.
 
       0250-PROCESS-RECORD-RTN SECTION.
       0250-PROCESS-RECORD.
 
           MOVE ACCTNO-PART1 TO ACCNO-1
           MOVE ACCTNO-PART2 TO ACCNO-2
           MOVE ACCTNO-PART3 TO ACCNO-3
 
           MOVE CHKNO TO WS-CHKNO
           MOVE CHKDATE TO WS-CHKDATE
 
           MOVE AMT TO WS-AMT
 
           MOVE STAT TO WS-STAT
           MOVE PAYEE TO WS-PAYEE
           MOVE EMPL TO WS-EMPL
           MOVE TERM TO WS-TERM
           MOVE OLDVAL TO WS-OLDVAL
 
           IF WS-PAGENEW = 1
              PERFORM 0300-PRINT-HEADERS-RTN
              MOVE 0 TO WS-PAGENEW
              MOVE 0 TO WS-PAGE-LIMIT
              MOVE 1 TO WS-PAGES
           END-IF
 
           WRITE OFILEA-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO WS-PAGE-LIMIT
 
           IF WS-PAGE-LIMIT >= WS-SETPAGE-LIMIT
              ADD 1 TO PG-COUNT
              PERFORM 0300-PRINT-HEADERS-RTN
              MOVE 1 TO WS-PAGE-LIMIT
           END-IF.
 
       0250-EXIT.
           EXIT.
 
       0300-PRINT-HEADERS-RTN SECTION.
       0300-PRINT-HEADERS.
 
           MOVE PG-COUNT TO PAGENUM
 
           IF WS-FIRST-PAGE = 'Y'
              WRITE OFILEA-REC FROM HEADER1
              MOVE 'N' TO WS-FIRST-PAGE
           ELSE
              WRITE OFILEA-REC FROM HEADER1 AFTER 1
           END-IF
           WRITE OFILEA-REC FROM HEADER2 AFTER 1
           WRITE OFILEA-REC FROM HEADER3 AFTER 1
           WRITE OFILEA-REC FROM HEADER4 AFTER 1
           WRITE OFILEA-REC FROM BLKLINE AFTER 1
           WRITE OFILEA-REC FROM HEADER5 AFTER 1.
 
       0300-EXIT.
           EXIT.
 
       0900-FINALIZE-RTN SECTION.
       0900-FINALIZE.
           CLOSE IFILEA
                 OFILEA.
 
       0900-EXIT.
           EXIT.
 
      *
      *         E N D   O F   P R O G R A M    A C A U D R P T