# The Professional COBOL Processing Model
**Author:** Student / Intern
**Context:** Easytrieve to COBOL Migration

This document outlines the standard "Batch Processing" flow used in professional mainframe development. It emphasizes data stability, error handling, and logical isolation.

---

## 1. The Mental Model (The 4 Phases)

Every COBOL program follows this strictly ordered lifecycle:

### Phase A: The Setup (Environment & Data)
* **Connect:** Link the physical file (PC/Mainframe disk) to a logical name (`SELECT`).
* **Buffer:** Define the "Mailbox" where raw data arrives (`FD` / `FILE SECTION`).
* **Workspace:** Define the "Desk" where you actually process the data (`WORKING-STORAGE`).

### Phase B: The Initialization (Procedure Start)
* **Open:** Open the files (`OPEN INPUT/OUTPUT`).
* **Check:** Immediately check `FILE STATUS` to ensure the file exists.
* **Prime:** Perform the **"Prime Read"** (Read the first record *before* the loop starts).

### Phase C: The Loop (The Engine)
* **Perform:** Start the loop `UNTIL` the "End of File" flag is TRUE.
* **Clean:** Clear your output workspace (`INITIALIZE` or `MOVE SPACES`) to prevent "garbage" data.
* **Process:** Move data from Input Workspace $\to$ Output Workspace. Apply logic (IF/ELSE/MATH).
* **Write:** Push the clean data from Workspace $\to$ File.
* **Next:** Read the *next* record at the bottom of the loop.

### Phase D: The Shutdown (Cleanup)
* **Close:** Close all files explicitly to save data to disk.
* **Stop:** End the run.

---

## 2. The Data Flow Diagram (READ INTO / WRITE FROM)

This method separates the "Physical" layer from the "Logical" layer, preventing crashes like `Status 71`.

```mermaid
graph TD
    subgraph "FILE SECTION"
        A[Input File on Disk] -->|READ| B(FD IN-BUFFER)
        F(FD OUT-BUFFER) -->|WRITE| G[Output File on Disk]
    end

    subgraph "WORKING-STORAGE"
        B -->|INTO| C{WS-IN-DATA}
        C -->|Logic / Moves| D{WS-OUT-DATA}
        D -->|FROM| F
    end

    style C fill:#000000,stroke:#333,stroke-width:2px,padding:4px
    style D fill:#000000,stroke:#333,stroke-width:2px

```

---

## 3. The "Golden Rules"

1. **Never Process in the Buffer:** Always use `READ ... INTO` and `WRITE ... FROM`. Don't touch the `FD` variables directly if you can avoid it.
2. **The "Prime Read" Pattern:** You must read *once* before the loop, and *again* at the very bottom of the loop.
* *Why?* If you put the READ at the top of the loop, you will process the last record twice or crash on an empty file.


3. **Close the Door:** Always finish your `IF/ELSE` logic with `END-IF` before triggering the next `READ`.
4. **Clean Your Desk:** Always `MOVE SPACES` or `INITIALIZE` your output structure at the start of every loop iteration.

---

## 4. The Master Skeleton Code

Use this template for every new task.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMPLATE-PROG.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OUT-FILE ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-BUFFER           PIC X(100). *> Match actual record length

       FD  OUT-FILE.
       01  OUT-BUFFER          PIC X(100). *> Match actual record length

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 WS-EOF           PIC X(01) VALUE 'N'.
              88 END-OF-FILE             VALUE 'Y'.
           05 WS-STATUS        PIC X(02).

       01  WS-IN-DATA.
           *> Define your Input Fields Here
           05 WS-IN-ID         PIC X(05).
           05 WS-IN-REST       PIC X(95).

       01  WS-OUT-DATA.
           *> Define your Output Fields Here
           05 WS-OUT-ID        PIC X(05).
           05 WS-OUT-REST      PIC X(95).

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT IN-FILE OUTPUT OUT-FILE
           
           IF WS-STATUS NOT = "00"
               DISPLAY "Error Opening File: " WS-STATUS
               STOP RUN
           END-IF

           *> 1. The Prime Read
           READ IN-FILE INTO WS-IN-DATA
               AT END SET END-OF-FILE TO TRUE
           END-READ

           *> 2. The Main Loop
           PERFORM 1000-PROCESS UNTIL END-OF-FILE

           CLOSE IN-FILE OUT-FILE
           STOP RUN.

       1000-PROCESS.
           *> A. Clean
           MOVE SPACES TO WS-OUT-DATA

           *> B. Logic
           MOVE WS-IN-DATA TO WS-OUT-DATA
           
           *> C. Write
           WRITE OUT-BUFFER FROM WS-OUT-DATA

           *> D. Read Next
           READ IN-FILE INTO WS-IN-DATA
               AT END SET END-OF-FILE TO TRUE
           END-READ.

```

Or this template with pagination, depends on your needs. This structure was from the cobol program of the last interns in Landbank.


```cobol
      ******************************************************************
      * AUTHOR: [YOUR NAME]
      * DATE: [CURRENT DATE]
      * PURPOSE: [REPORT DESCRIPTION]
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTTEMPL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Input File (Sorted by the Key you want to group by!)
           SELECT IN-FILE ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           *> Output Report File
           SELECT OUT-FILE ASSIGN TO "OUTPUT.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01 IN-RECORD.
          05 IN-KEY-GROUP       PIC X(05).     *> The field triggers a break (e.
          05 IN-AMOUNT          PIC 9(05)V99.
          05 IN-DATA            PIC X(68).

       FD  OUT-FILE
           RECORD CONTAINS 133 CHARACTERS.
       01 OUT-RECORD            PIC X(133).

       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      * REPORT LAYOUTS (Hardcoded Print Lines)
      *-------------------------------------------------------------*
       01 WS-REPORT-LINES.
          05 BLANK-LINE         PIC X(133)         VALUE SPACES.

          05 HEADER-1.
             10 FILLER          PIC X(01)          VALUE SPACE.
             10 FILLER          PIC X(20)          VALUE
                   "BANK NAME/COMPANY".
             10 FILLER          PIC X(90)          VALUE SPACES.
             10 FILLER          PIC X(10)          VALUE "PAGE:".
             10 HDR-PAGE-NUM    PIC ZZZ9.

          05 HEADER-GROUP.
             10 FILLER          PIC X(01)          VALUE SPACE.
             10 FILLER          PIC X(15)          VALUE
                   "BRANCH CODE:   ".
             10 HDR-GROUP-ID    PIC X(05).
             10 FILLER          PIC X(112)         VALUE SPACES.

          05 DETAIL-LINE.
             10 FILLER          PIC X(05)          VALUE SPACES.
             10 DET-DATA        PIC X(50).
             10 FILLER          PIC X(05)          VALUE SPACES.
             10 DET-AMOUNT      PIC ZZ,ZZ9.99.

          05 GROUP-TOTAL-LINE.
             10 FILLER          PIC X(40)          VALUE SPACES.
             10 FILLER          PIC X(15)          VALUE
                   "BRANCH TOTAL:  ".
             10 GRP-TOTAL-AMT   PIC ZZZ,ZZZ,ZZ9.99.

      *-------------------------------------------------------------*
      * CONTROL FLAGS & COUNTERS
      *-------------------------------------------------------------*
       01 WS-CONTROLS.
          05 WS-EOF             PIC X(01)          VALUE 'N'.
          05 WS-FIRST-REC       PIC X(01)          VALUE 'Y'.
           
           *> "Hold" variable to compare against current record
          05 WS-HOLD-GROUP      PIC X(05)          VALUE SPACES.

       01 WS-COUNTERS.
          05 WS-PAGE-COUNT      PIC 9(04)          VALUE 0.
          05 WS-LINE-COUNT      PIC 9(02)          VALUE 55.
                                                        *> Force header on start
          05 WS-LINES-PER-PAGE  PIC 9(02)          VALUE 50.
           
          05 WS-SUBTOTAL        PIC 9(12)V99       VALUE 0.
          05 WS-GRAND-TOTAL     PIC 9(14)V99       VALUE 0.

       01 WS-PARAMS.
          05 WS-PARAM-DATE      PIC X(08).

       PROCEDURE DIVISION.
       
       *> ==========================================================
       *> MAIN DRIVER SECTION
       *> ==========================================================
       0000-MAIN-RTN SECTION.
       0000-MAIN.
           *> 1. Setup
           ACCEPT WS-PARAM-DATE FROM SYSIN
           OPEN INPUT IN-FILE OUTPUT OUT-FILE
           
           *> 2. Prime Read
           READ IN-FILE
           AT END
              MOVE 'Y' TO WS-EOF.

           *> 3. Main Loop
           PERFORM 0100-PROCESS-RTN UNTIL WS-EOF = 'Y'.

           *> 4. Final Cleanup (Process last group totals)
           IF WS-FIRST-REC = 'N'
              PERFORM 0300-PRINT-TOTALS
           END-IF.

           CLOSE IN-FILE OUT-FILE
           STOP RUN.

       *> ==========================================================
       *> PROCESSING SECTION (The "Heart" of the Logic)
       *> ==========================================================
       0100-PROCESS-RTN SECTION.
       0100-PROCESS.
           
           *> A. Check for Control Break (Did the Branch change?)
           IF WS-FIRST-REC = 'Y'
              MOVE IN-KEY-GROUP TO WS-HOLD-GROUP
              MOVE 'N' TO WS-FIRST-REC
              PERFORM 0200-NEW-PAGE-RTN
           ELSE
              IF IN-KEY-GROUP NOT = WS-HOLD-GROUP
                   *> 1. Print Totals for Old Group
                 PERFORM 0300-PRINT-TOTALS
                   *> 2. Reset for New Group
                 MOVE IN-KEY-GROUP TO WS-HOLD-GROUP
                 MOVE 0 TO WS-SUBTOTAL
                   *> 3. Force New Page/Header for New Group
                 PERFORM 0200-NEW-PAGE-RTN
              END-IF
           END-IF.

           *> B. Check Pagination (Is page full?)
           IF WS-LINE-COUNT > WS-LINES-PER-PAGE
              PERFORM 0200-NEW-PAGE-RTN
           END-IF.

           *> C. Process Detail
           MOVE IN-DATA TO DET-DATA
           MOVE IN-AMOUNT TO DET-AMOUNT
           
           ADD IN-AMOUNT TO WS-SUBTOTAL.

           WRITE OUT-RECORD FROM DETAIL-LINE AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINE-COUNT.

           *> D. Read Next
           READ IN-FILE
           AT END
              MOVE 'Y' TO WS-EOF.

       0100-EXIT.
           EXIT.

       *> ==========================================================
       *> PAGINATION SECTION
       *> ==========================================================
       0200-NEW-PAGE-RTN SECTION.
       0200-NEW-PAGE.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE WS-PAGE-COUNT TO HDR-PAGE-NUM.
           MOVE WS-HOLD-GROUP TO HDR-GROUP-ID.

           WRITE OUT-RECORD FROM HEADER-1 AFTER ADVANCING PAGE.
           WRITE OUT-RECORD FROM HEADER-GROUP AFTER ADVANCING 2 LINES.
           WRITE OUT-RECORD FROM BLANK-LINE AFTER ADVANCING 1 LINE.

           MOVE 0 TO WS-LINE-COUNT.

       0200-EXIT.
           EXIT.

       *> ==========================================================
       *> TOTALS SECTION
       *> ==========================================================
       0300-PRINT-TOTALS SECTION.
       0300-TOTALS.
           MOVE WS-SUBTOTAL TO GRP-TOTAL-AMT.
           
           WRITE OUT-RECORD FROM BLANK-LINE AFTER ADVANCING 1 LINE.
           WRITE OUT-RECORD FROM GROUP-TOTAL-LINE AFTER ADVANCING 1 LINE.
           WRITE OUT-RECORD FROM BLANK-LINE AFTER ADVANCING 1 LINE.
           
           ADD 3 TO WS-LINE-COUNT.

       0300-EXIT.
           EXIT.
```
