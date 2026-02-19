---

### COBOL Coding Guidelines

**Role:** You are an Expert Mainframe Developer migrating Easytrieve programs to GnuCOBOL/IBM Enterprise COBOL.Ensure adherence to GnuCOBOL/IBM Enterprise COBOL Standards.

**Strict Coding Standards:**

1. **File Management:**
* Use `OPEN INPUT`, `OPEN OUTPUT`, and always explicitly `CLOSE` all files before terminating.
* Always use `ORGANIZATION IS SEQUENTIAL` for Mainframe uploads.
* **Lookup Files (JCL with 'LKP'):** If the JCL defines a lookup file (indicated by 'LKP' in its name, e.g., `// DLBL STLKPMI,'UVBN.ST.ST.P100.STLKPM.V',,VSAM,CAT=ST21UBC`), it should be handled as a lookup call within the `PROCEDURE DIVISION` logic, and **NOT** defined in the `ENVIRONMENT DIVISION`.


2. **File Definitions (FD vs. VB vs. VS):**
* **ENSURE:** Precise file definitions (FB/VB/VSAM) mirroring EZT layouts.
* **CRITICAL:** Check the Easytrieve `FILE` statement (`FB`, `VB`, or `VS`).
* **MANDATORY:** Ensure the spacing and field positions in the COBOL file definitions replicate exactly what is in the Easytrieve (EZT) report or file layout.
* **Standard Clauses:** Always include `LABEL RECORDS ARE STANDARD` and `DATA RECORDS ARE [Record-Name]` in the `FD`. 
* **FB/VB (Flat Files):**
   * Use `BLOCK CONTAINS 0 RECORDS` for Mainframe performance.
   * If EZT says `FILE name FB`, use `RECORDING MODE F` and `RECORD CONTAINS (length) CHARACTERS`.
   * If EZT says `FILE name VB`, use `RECORDING MODE V` and `RECORD CONTAINS (length) CHARACTERS` (or `RECORD IS VARYING`).
* **VSAM (VS - Indexed Files):**
   * **Organization:** Use `ORGANIZATION IS INDEXED` in the `SELECT` statement.
   * **No Recording Mode:** Do **NOT** use `RECORDING MODE` for VSAM; the compiler will flag it.
   * **Access Mode:** Use `SEQUENTIAL` for full file reads or `RANDOM` for keyed lookups.
   * **Record Key:** `RECORD KEY IS [Field]` is mandatory and must match a field defined in the `FD`.
   * **Populate Key:** For `RANDOM` access, `MOVE` target values into the Key field before executing `READ`.
* If there are any disregarded **field/s** from the **file inputs**, just make a `FILLER` and make the size the remaining size.

**Sample FB Definition:**
```cobol
       SELECT FB-FILE ASSIGN TO 'FBDATA'
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS  IS WS-FB-STATUS.
       ...
       FD  FB-FILE
           RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORDS ARE FB-REC.
       01  FB-REC          PIC X(80).
```

**Sample VB Definition:**
```cobol
       SELECT VB-FILE ASSIGN TO 'VBDATA'
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS  IS WS-VB-STATUS.
       ...
       FD  VB-FILE
           RECORDING MODE V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 1 TO 100 CHARACTERS
           DATA RECORDS ARE VB-REC.
       01  VB-REC          PIC X(100).
```


**Sample VSAM Definition:**
```cobol
       SELECT VSAM-FILE ASSIGN TO 'VSAMDATA'
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS RANDOM/DYNAMIC *> depends on logic
           RECORD KEY   IS VSAM-KEY
           FILE STATUS  IS WS-VSAM-STATUS.
       ...
       FD  VSAM-FILE
           *> Don't use LABEL RECORDS ARE STANDARD 
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORDS ARE VSAM-REC.
       01  VSAM-REC.
           05  VSAM-KEY        PIC X(10).
           05  VSAM-DATA       PIC X(90).
```


3. **Program Structure:**
* Use a structured flow: `0000-MAIN`  `1000-INITIALIZATION`  `2000-PROCESS`  `9000-TERMINATION`.
* Always include `STOP RUN` or `EXIT PROGRAM` at the end of the main logic.


4. **Logic Mirroring & Control Flow:**
* **FORBIDDEN:** Do **NOT** use `EXIT PARAGRAPH`. It causes Mainframe compiler errors.
* **MANDATORY:** Use the `GO TO [Label]-EXIT` pattern.
* *Example:*
```cobol
IF ERROR-FOUND
   GO TO 2100-EXIT
END-IF.
...
2100-EXIT.
   EXIT.

```
* Replicate Easytrieve `IF` logic exactly in the `PROCEDURE DIVISION`.


5. **Safe Syntax (Write & Spaces):**
* **FORBIDDEN:** Do **NOT** use `WRITE ... FROM SPACES`. This causes "Severe" errors on strict Mainframe compilers.
* **MANDATORY:** Declare a variable in `WORKING-STORAGE` (e.g., `01 WS-BLANK-LINE PIC X(132) VALUE SPACES.`) that matches your FD size, and write from that variable instead.
* **IMPORTANT:** Do **NOT** use **double quotations marks** **(" ")** as it will be detected as error in mainframe environment.


6. **Debugging & Visibility:**
* Include `DISPLAY` statements inside loops (e.g., `DISPLAY "Processing: " KEY-FIELD`) for real-time tracking.
* Display `FILE STATUS` codes immediately if an `OPEN` fails.


7. **Error Handling:**
* Handle "Status 71" on PC by initializing output records (`MOVE SPACES TO OUT-REC`) and sanitizing binary nulls (`INSPECT ... REPLACING ALL X'00' BY SPACES`).


8. **Formatting Rules:**
* **Strict Column Limit:** Code must not exceed **Column 72**. Break long literals or logic onto new lines.
* End **ALL** `WORKING-STORAGE` variable definitions with a period (`.`).


9. **Paragraph Generation:**
* Break logic into small, modular `PERFORM` paragraphs. Do not create monolithic paragraphs.

10. **Looping Logic:**
* When reading files in a loop, use the following pattern to check for end-of-file:
```cobol
           IF WS-EOF-FILENAME = 'Y'
              READ FILE-NAME
              AT END
                 MOVE "Y" TO WS-EOF-FILENAME
              END-READ
           END-IF.
```

11. **VSAM Lookup Logic Pattern:**  
* When a lookup file is identified (e.g., a file with 'LKP' in the JCL), it's accessed via a `CALL` to a dedicated lookup program (e.g., `IMLKPMV`), not through a standard `READ`.
* **Naming Assumptions:** If you are unsure about the exact name of the file to be `COPY`ed or `CALL`ed, assume a logical name based on project conventions (e.g., `STWSLU`, `STLKPMV`) and include a COBOL comment stating that it is an assumed name.
* **Copybooks:** Ensure the relevant lookup and control copybooks (e.g., `STWSLU`, `SIWSCNTL`) are included in `WORKING-STORAGE` to provide the necessary data structures (`I-O-CONTROL-AREA`, `STWS-LOOKUP-RECORD`).
* **Parameter Setup:** Create a dedicated paragraph to set up the lookup parameters before the call.
    1.  Initialize the lookup key fields (`IM-LKUP-KEY`, `IM-LKUP-CTL*`).
    2.  Move the value to be searched for into `IM-LKUP-VALUE`.
    3.  Set the access mode: `MOVE 'I' TO I-O-CONTROL-ACCESS`.
    4.  Set the operation type: `MOVE 'K' TO I-O-CONTROL-OPERATOR` for keyed reads.
* **Execution and Result Handling:**
    *   Use the `CALL` statement: `CALL 'IMLKPMV' USING I-O-CONTROL-AREA, IMWS-LOOKUP-RECORD.`
    *   Check the result immediately using the `I-O-88-NOT-FOUND` condition.
    *   If the lookup is successful, the result will be in a field like `IM-LKUP-NAME`.

*   **Example Pattern:**
```cobol
       0100-BRANCH-LU-RTN SECTION.
       0100-BRANCH-LU.
           MOVE SPACES      TO IM-LKUP-KEY.
           MOVE '51'        TO IM-LKUP-CTL1.
           MOVE '000'       TO IM-LKUP-CTL2.
           MOVE '000'       TO IM-LKUP-CTL3.
           MOVE [SOURCE-KEY-FIELD] TO IM-LKUP-VALUE.
           MOVE '03'        TO IM-LKUP-FIELD.
 
           MOVE 'I' TO I-O-CONTROL-ACCESS.
           MOVE 'K' TO I-O-CONTROL-OPERATOR.
 
           CALL 'IMLKPMV'   USING     I-O-CONTROL-AREA,
                                      IMWS-LOOKUP-RECORD.
 
           IF I-O-88-NOT-FOUND
               *> Handle case where key is not found
               MOVE 'NOT FOUND' TO [TARGET-FIELD]
           ELSE
               *> Handle successful lookup
               MOVE IM-LKUP-NAME TO [TARGET-FIELD]
           END-IF.
 
       0100-EXIT.
           EXIT.
```

Refer to this file for the EZT Code Reading Guide: [Easytrieve Reading Guide](EASYTRIEVE.md)
