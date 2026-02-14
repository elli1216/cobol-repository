# COBOL File Definitions: Fixed (FB) vs. Variable (VB)

**Context:** Migrating Easytrieve (`.ezt`) file definitions to IBM Enterprise COBOL.

---

## Quick Comparison

| Feature | **FB (Fixed Block)** | **VB (Variable Block)** |
| :--- | :--- | :--- |
| **Analogy** | A wall of identical bricks. | A mailbag of different-sized letters. |
| **Structure** | Every record has the **exact same length**. Short records are padded with spaces. | Records vary in length to save space. Contains a hidden 4-byte descriptor (RDW). |
| **COBOL Mode** | `RECORDING MODE IS F` | `RECORDING MODE IS V` |
| **Efficiency** | Faster processing (predictable). | Saves disk space (no padding). |

---

## How to Identify in Easytrieve

Look at the `FILE` statement in your `.ezt` source code.

### 1. Fixed Block (FB)

```easytrieve
FILE MYFILE FB(80 800)
```

- **FB**: Explicitly states "Fixed Block".
- **80**: The Record Length (Size of `01` level).
- **800**: The Block Size (Ignore this in COBOL, use `0`).

### 2. Variable Block (VB)

```easytrieve
FILE STACTMI VB(18054 27998)
```

- **VB**: Explicitly states "Variable Block".
- **18054**: The **Maximum** Record Length.
- **27998**: The Block Size.

---

## COBOL Implementation Guide

The difference is entirely in the **FD (File Description)**. The `PROCEDURE DIVISION` logic (`READ`/`WRITE`) remains mostly the same.

### Scenario A: Fixed Block (FB) Implementation

**Rule:** Definition must match the exact byte size.

```cobol
       FD  INPUT-FILE
           RECORDING MODE IS F         *> "Fixed"
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.   *> Let system handle block size
       01  INPUT-REC      PIC X(80).   *> EXACT size match (e.g., 80)
```

### Scenario B: Variable Block (VB) Implementation

**Rule:** Definition must match the **Maximum** possible size.

```cobol
       FD  STACTMI-FILE
           RECORDING MODE IS V         *> "Variable"
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  STACTMI-REC.
           05  REC-DATA   PIC X(18054). *> MAX size from EZT
```

> [!IMPORTANT]
> **Mainframe Note:** In Mainframe COBOL, do **NOT** manually define the 4-byte RDW (Record Descriptor Word) at the start of the record. The operating system handles it invisibly. Just define the data payload.

---

## Troubleshooting Common Errors

| Error Code | Meaning | Cause | Fix |
| :--- | :--- | :--- | :--- |
| **Abend S013** | Record Length Mismatch | Defined `FD ... PIC X(80)` but file is length 100. | Match your COBOL `FD` to dataset attributes. |
| **Status 39** | Attribute Mismatch | Used `RECORDING MODE F` for a Variable file. | Check EZT; if `VB`, use `RECORDING MODE V`. |
| **IGYDS1089-S** | Syntax Error | `RECORDING MODE` was placed inside `SELECT`. | Move `RECORDING MODE` to the `FD` section. |
| **IGYPS0086-I** | EXIT PARAGRAPH Trap | `EXIT PARAGRAPH` is not a valid COBOL command. | Use the `GO TO [Label]-EXIT` pattern. |
| **Severe Error** | Invalid WRITE FROM SPACES | `WRITE ... FROM SPACES` literal used. | Use a variable (e.g., `WS-BLANK-LINE`). |
| **ERROR_TOKEN** | Missing Period | Previous line is missing a period (`.`). | Add `.` to the end of the previous line. |

---

## Common Logic & Syntax Fixes

### 1. The "EXIT PARAGRAPH" Trap
The mainframe ignores `EXIT PARAGRAPH`, causing code to "fall through."

```cobol
*> WRONG
IF ERROR-FOUND
   EXIT PARAGRAPH.  *> Computer ignores this!
END-IF.

*> CORRECT
IF ERROR-FOUND
   GO TO 2100-EXIT. *> Jumps to the end
END-IF.
...
2100-EXIT.
   EXIT.
```

### 2. Writing Spaces Properly
The compiler needs a defined variable to know how many spaces to write.

IN WORKING-STORAGE IF FILE IS **FD**:
```cobol
FILE SECTION.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
                               *> Size is 132
       01  RPT-REC             PIC X(132). 

WORKING-STORAGE SECTION.
                               *> MATCH EXACTLY:
       01  WS-BLANK-LINE       PIC X(132) VALUE SPACES.

*> IN PROCEDURE:
WRITE PRT-REC FROM WS-BLANK-LINE AFTER ADVANCING 1 LINE.
```
---
IN WORKING-STORAGE IF FILE IS **VB**:
```cobol
FILE SECTION.
       FD  DATA-FILE
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS.
       01  DATA-REC            PIC X(18054). <-- Max Size

WORKING-STORAGE SECTION.
                               *> MATCH MAX/JUST ONE SPACE
       01  WS-BLANK-LINE       PIC X(18054 or 1) VALUE SPACES.

*> IN PROCEDURE:
WRITE PRT-REC FROM WS-BLANK-LINE AFTER ADVANCING 1 LINE.
```

### 3. Missing Periods (The "Run-on Sentence")
Always check the line **above** the error for a missing period.

```cobol
*> WRONG
05 FILLER PIC X(10) VALUE 'ACCOUNT'   <-- Missing dot!
05 FILLER PIC X(10) VALUE 'NUMBER'.

*> CORRECT
05 FILLER PIC X(10) VALUE 'ACCOUNT'.  <-- Added dot
05 FILLER PIC X(10) VALUE 'NUMBER'.
```

