      ******************************************************************
      * AUTHOR: LORIEL ANN G. CONICONDE
      * DATE: 9/7/2025
      * PURPOSE: STEP 300 - REPORT FOR NEGOTIATED FOR THE MONTH
      * TECTONICS: COBC
      * UPDATED BY: KRYSTLER CATANIAG
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MCCNMRPT.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IMCREGFL ASSIGN TO SYS012-DA-FBA1-S-MCREGFL
           ORGANIZATION IS SEQUENTIAL.
 
           SELECT OMCREGFL ASSIGN TO SYS011-UR-1403-S.
 
       DATA DIVISION.
       FILE SECTION.
       FD IMCREGFL
           RECORD CONTAINS 560 CHARACTERS
           RECORDING IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS WK-MC-MCHKFLE.
           COPY MCHKREG.
 
       FD OMCREGFL
           RECORD CONTAINS 133 CHARACTERS
           RECORDING IS F
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS OMCREGFL-REC.
 
       01 OMCREGFL-REC                     PIC X(133).
 
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
       COPY IMWSLKUP.
       COPY SIWSCNTL.
      *-------------------------------------------------------------*
       01 WS-REPORT.
           05 BLKLINE                      PIC X(01)  VALUE SPACES.
           05 HEADER1.
               10 FILLER                   PIC X(46)  VALUE SPACES.
               10 FILLER                   PIC X(28)  VALUE
                  'LAND BANK OF THE PHILIPPINES'.
               10 FILLER                   PIC X(28)  VALUE SPACES.
               10 FILLER                   PIC X(09)  VALUE
                  '*MCRBTC9*'.
 
           05 HEADER2.
               10 FILLER                   PIC X(08)  VALUE
                  'BRANCH: '.
               10 MC-CTL3                  PIC X(03).
               10 FILLER                   PIC X      VALUE SPACES.
               10 MC-BRNAME                PIC X(30).
               10 FILLER                   PIC X      VALUE SPACES.
               10 FILLER                   PIC X(34)  VALUE
                  'LIST OF CANCELLED MC FOR THE MONTH'.
               10 FILLER                   PIC X(61)  VALUE SPACES.
 
           05 HEADER3.
               10 FILLER                   PIC X(53)  VALUE SPACES.
               10 TLE-MM                   PIC X(09).
               10 FILLER                   PIC X      VALUE SPACE.
               10 TLE-Y                    PIC 9(04).
 
           05 HEADER4.
               10 FILLER                   PIC X(03)  VALUE SPACES.
               10 FILLER                   PIC X(11)  VALUE
                  'DATE ISSUED'.
               10 FILLER                   PIC X(09)  VALUE SPACES.
               10 FILLER                   PIC X(08)  VALUE
                  'CHECK NO'.
               10 FILLER                   PIC X(18)  VALUE SPACES.
               10 FILLER                   PIC X(06)  VALUE
                  'AMOUNT'.
               10 FILLER                   PIC X(16)  VALUE SPACES.
               10 FILLER                   PIC X(14)  VALUE
                  'DATE CANCELLED'.
               10 FILLER                   PIC X(06)  VALUE SPACES.
               10 FILLER                   PIC X(04)  VALUE
                  'USER'.
               10 FILLER                   PIC X(08)  VALUE SPACES.
               10 FILLER                   PIC X(07)  VALUE
                  'BR CODE'.
 
           05 TOTAL-LINE.
               10 FILLER                   PIC X(15)  VALUE
                  'TOTAL CHECKS:  '.
               10 MC-CHKTALLY              PIC Z,ZZ9.
               10 FILLER                   PIC X(05)  VALUE SPACES.
               10 FILLER                   PIC X(15)  VALUE
                  'TOTAL AMOUNT:  '.
               10 FILLER                   PIC X(02)  VALUE SPACES.
               10 MC-TOTAL-AMOUNT          PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
               10 FILLER                   PIC X(74)  VALUE SPACES.
 
           05 DETAIL-LINE.
               10 FILLER                   PIC X(03)  VALUE SPACES.
               10 MC-CHKDATE               PIC 99/99/9999.
               10 FILLER                   PIC X(09)  VALUE SPACES.
               10 MC-CHKNO                 PIC 9(10).
               10 FILLER                   PIC X(10)  VALUE SPACES.
               10 MC-CHKAMT                PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
               10 FILLER                   PIC X(10)  VALUE SPACES.
               10 MC-DATEUPD               PIC 99/99/9999.
               10 FILLER                   PIC X(08)  VALUE SPACES.
               10 MC-USERADD               PIC XXXX.
               10 FILLER                   PIC X(10)  VALUE SPACES.
               10 MC-BRCODE                PIC XXX.
 
       01 WS-ACCESSORY-STORAGES.
           05 WS-EOF1                      PIC 9      VALUE 0.
           05 WS-EOF2                      PIC 9      VALUE 0.
           05 WS-CURRENT-BRANCH            PIC 9(03)  VALUE 0.
           05 WS-PAGENEW                   PIC 9      VALUE 0.
           05 WS-PAGES                     PIC 9999   VALUE 0.
           05 WS-TOTAL-CHECKS              PIC 9(04)  VALUE 0.
           05 WS-TOTAL-AMOUNT              PIC 9(14)V99 VALUE 0.
           05 WS-CHK-DATE                  PIC 9(08).
           05 WS-PAGE-LIMIT                PIC 99     VALUE 0.
 
       01 ACCT-NUM1                        PIC 9(10).
       01 ACCT-NUM2 REDEFINES ACCT-NUM1.
           05 WS-ACCT-BRANCH               PIC 9(03).
 
       01 WS-DATEUPD                       PIC 9(08).
       01 WS-UPDMY REDEFINES WS-DATEUPD.
           05 WS-DATEUMM                   PIC 9(02).
           05 WS-DATEUDD                   PIC 9(02).
           05 WS-DATEUYY                   PIC 9(04).
 
       01 WS-PARAM-DATE.
           05 WS-PARAM-MM                  PIC 99.
           05 WS-PARAM-DD                  PIC 99.
           05 WS-PARAM-CCYY                PIC 9999.
 
       PROCEDURE DIVISION.
       0000-MAIN-RTN SECTION.
       0000-MAIN.
           ACCEPT WS-PARAM-DATE FROM SYSIN
           OPEN INPUT IMCREGFL
           OPEN OUTPUT OMCREGFL
 
           MOVE 'I'                 TO        I-O-CONTROL-ACCESS.
           MOVE 'O'                 TO        I-O-CONTROL-OPERATOR
           CALL 'IMLKPMV'           USING     I-O-CONTROL-AREA,
                                              IMWS-LOOKUP-RECORD.
 
           READ IMCREGFL AT END MOVE 1 TO WS-EOF1.
           PERFORM 0200-DSP-RPRT-RTN THRU 0200-EXIT UNTIL WS-EOF1 = 1
 
           CLOSE IMCREGFL
           CLOSE OMCREGFL
           STOP RUN.
 
       0000-EXIT.
           STOP RUN.
 
       0100-BRANCH-LU-RTN SECTION.
       0100-BRANCH-LU.
           MOVE SPACES      TO IM-LKUP-KEY.
           MOVE '51'        TO IM-LKUP-CTL1.
           MOVE '000'       TO IM-LKUP-CTL2.
           MOVE '000'       TO IM-LKUP-CTL3.
           MOVE WS-ACCT-BRANCH TO IM-LKUP-VALUE.
           MOVE '03'        TO IM-LKUP-FIELD.
 
           MOVE 'I' TO I-O-CONTROL-ACCESS.
           MOVE 'K' TO I-O-CONTROL-OPERATOR.
 
           CALL 'IMLKPMV'   USING     I-O-CONTROL-AREA,
                                      IMWS-LOOKUP-RECORD.
 
           IF I-O-88-NOT-FOUND
               MOVE 'BRANCH NOT FOUND' TO MC-BRNAME
           ELSE
               MOVE IM-LKUP-NAME TO MC-BRNAME
           END-IF.
 
       0100-EXIT.
           EXIT.
 
       0200-DSP-RPRT-RTN SECTION.
       0200-DSP-RPRT.
           MOVE WK-MC-MCHK-DATEUPD TO WS-DATEUPD
           IF WK-MC-CHKAMT NUMERIC
               AND WK-MC-MCHK-CHKSTAT = 02
               AND WS-DATEUMM = WS-PARAM-MM
               AND WS-DATEUYY = WS-PARAM-CCYY
 
               MOVE WK-MC-ACCTNO TO ACCT-NUM1
               IF WS-ACCT-BRANCH NOT = WS-CURRENT-BRANCH
                   MOVE 1 TO WS-PAGENEW
                   MOVE WS-ACCT-BRANCH TO WS-CURRENT-BRANCH
                   PERFORM 0100-BRANCH-LU-RTN THRU 0100-EXIT
                   MOVE WS-ACCT-BRANCH TO MC-CTL3
               END-IF
 
               PERFORM 0003-DATE
 
               MOVE WS-PARAM-CCYY TO TLE-Y
 
               MOVE WK-MC-MCHK-CHKDTE TO WS-CHK-DATE
               MOVE WS-CHK-DATE TO MC-CHKDATE
               MOVE WK-MC-CHKNO TO MC-CHKNO
               MOVE WK-MC-CHKAMT TO MC-CHKAMT
               MOVE WK-MC-MCHK-DATEUPD TO WS-DATEUPD
               MOVE WS-DATEUPD TO MC-DATEUPD
               MOVE WK-MC-MCHK-USERUPD TO MC-USERADD
               MOVE WK-MC-MCHK-BRCODE TO MC-BRCODE
 
               IF WS-PAGENEW  = 1
                   IF WS-PAGES NOT = 0
                       MOVE WS-TOTAL-CHECKS TO MC-CHKTALLY
                       MOVE WS-TOTAL-AMOUNT TO MC-TOTAL-AMOUNT
                       WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                       WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                       WRITE OMCREGFL-REC FROM TOTAL-LINE AFTER 1
                   END-IF
 
                   MOVE 0 TO WS-PAGENEW
                   MOVE 0 TO WS-PAGE-LIMIT
                   WRITE OMCREGFL-REC FROM HEADER1 AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER2 AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER3 AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER4 AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
 
                   MOVE 0 TO WS-TOTAL-CHECKS
                   MOVE 0 TO WS-TOTAL-AMOUNT
 
                   MOVE 1 TO WS-PAGES
               END-IF
 
               ADD 1 TO WS-TOTAL-CHECKS
               ADD 1 TO WS-PAGE-LIMIT
               ADD WK-MC-CHKAMT TO WS-TOTAL-AMOUNT
 
               IF WS-PAGE-LIMIT = 51
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER PAGE
                   WRITE OMCREGFL-REC FROM HEADER1 AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER2 AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER3 AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
                   WRITE OMCREGFL-REC FROM HEADER4 AFTER 1
                   WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
 
                  MOVE 1 TO WS-PAGE-LIMIT
              END-IF
 
              WRITE OMCREGFL-REC FROM DETAIL-LINE AFTER 1
           END-IF.
 
           IF WS-EOF1 NOT = 1
               READ IMCREGFL
                   AT END MOVE 1 TO WS-EOF1
               END-READ
           END-IF
 
           IF WS-EOF1 = 1 AND WS-PAGES = 1
               MOVE WS-TOTAL-CHECKS TO MC-CHKTALLY
               MOVE WS-TOTAL-AMOUNT TO MC-TOTAL-AMOUNT
               WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
               WRITE OMCREGFL-REC FROM BLKLINE AFTER 1
               WRITE OMCREGFL-REC FROM TOTAL-LINE AFTER 1
           END-IF.
 
       0200-EXIT.
           EXIT.
 
       0003-DATE.
           EVALUATE WS-PARAM-MM
               WHEN 01 MOVE '  JANUARY' TO TLE-MM
               WHEN 02 MOVE ' FEBRUARY' TO TLE-MM
               WHEN 03 MOVE '    MARCH' TO TLE-MM
               WHEN 04 MOVE '    APRIL' TO TLE-MM
               WHEN 05 MOVE '      MAY' TO TLE-MM
               WHEN 06 MOVE '     JUNE' TO TLE-MM
               WHEN 07 MOVE '     JULY' TO TLE-MM
               WHEN 08 MOVE '   AUGUST' TO TLE-MM
               WHEN 09 MOVE 'SEPTEMBER' TO TLE-MM
               WHEN 10 MOVE '  OCTOBER' TO TLE-MM
               WHEN 11 MOVE ' NOVEMBER' TO TLE-MM
               WHEN 12 MOVE ' DECEMBER' TO TLE-MM
               WHEN OTHER MOVE SPACES   TO TLE-MM
           END-EVALUATE.