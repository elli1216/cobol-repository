---
---

## Program Structure (The 4 Divisions)

| Keyword | Description |
| --- | --- |
| **`IDENTIFICATION DIVISION`** | The first line of every program. It marks the start of the code metadata. |
| **`PROGRAM-ID`** | Specifies the name the computer uses to identify this program (e.g., `MCCNMRPT`). |
| **`ENVIRONMENT DIVISION`** | The section that links your code to the outside world (files, computers, printers). |
| **`DATA DIVISION`** | The section where **all** variables, files, and memory structures must be defined. |
| **`PROCEDURE DIVISION`** | The section containing the executable logic (Read, Write, Add, If/Else). |

---

## File Handling (Environment & File Section)

| Keyword | Description |
| --- | --- |
| **`INPUT-OUTPUT SECTION`** | A subsection of Environment Division specifically for file definitions. |
| **`FILE-CONTROL`** | The paragraph where you map internal file names to external system files. |
| **`SELECT ... ASSIGN TO`** | Connects your code name (`IMCREGFL`) to the Mainframe system name (`SYS012...`). |
| **`ORGANIZATION IS SEQUENTIAL`** | Tells the computer to read the file line-by-line from start to finish. |
| **`FD`** | **File Description**. Describes the physical properties of a file (length, type). |
| **`RECORD CONTAINS`** | A safety check defining exactly how many characters are in one line of the file. |
| **`RECORDING MODE F`** | **Fixed**. Indicates every line in the file has the exact same length. |
| **`COPY`** | Imports external code (Copybooks). Used to share standard layouts across teams. |

---

## Data Definition (Working-Storage)

| Keyword | Description |
| --- | --- |
| **`WORKING-STORAGE SECTION`** | The place to define variables (counters, flags, print lines) used during processing. |
| **`PIC` (Picture)** | Defines the data type and size. <br>

<br>• `PIC X(10)` = Text (10 chars).<br>

<br>• `PIC 9(05)` = Number (5 digits).<br>

<br>• `PIC ZZ9` = Number with zero suppression (prints `  5` instead of `005`). |
| **`VALUE`** | Sets the initial data in a variable (e.g., `VALUE 'LAND BANK'`). |
| **`SPACES`** | A special keyword representing blank space characters. Used to clear memory. |
| **`FILLER`** | A placeholder variable name used when you don't need to reference the field (like empty space between columns). |
| **`REDEFINES`** | Allows you to look at the same piece of memory in two different ways (e.g., viewing a full date `20230515` or just the year `2023`). |

---

## Logic & Control Flow (Procedure Division)

| Keyword | Description |
| --- | --- |
| **`OPEN INPUT / OUTPUT`** | Prepares a file for reading (Input) or writing (Output). Must be done before processing. |
| **`READ ... AT END`** | Reads the next line from a file. If the file is empty, executes the `AT END` logic (setting a flag). |
| **`PERFORM ... UNTIL`** | A loop. Runs a specific paragraph of code repeatedly until a condition is met (like End of File). |
| **`IF ... ELSE ... END-IF`** | Standard decision logic. `END-IF` is crucial to close the logic block explicitly. |
| **`EVALUATE ... WHEN`** | A "Switch" statement. Checks one variable against multiple possible values (used for Month conversion). |
| **`CALL ... USING`** | Runs an external sub-program (like the Lookup Module `IMLKPMV`) and passes variables to it. |
| **`STOP RUN`** | Immediately terminates the program. The "Hard Stop." |
| **`EXIT`** | A dummy command often used as a target for `GO TO` or `PERFORM ... THRU`. It does nothing but mark the end. |

---

## Data Manipulation

| Keyword | Description |
| --- | --- |
| **`MOVE`** | Copies data from one place to another. **Does not delete** the source data. |
| **`ADD ... TO`** | Performs addition. `ADD 1 TO COUNTER`. |
| **`WRITE ... FROM`** | Writes a record to an output file using data from Working-Storage. |
| **`AFTER ADVANCING`** | Controls vertical spacing on a printed report.<br>

<br>• `AFTER 1` = Single Space (Next line).<br>

<br>• `AFTER PAGE` = Page Break (Top of next page). |
| **`ACCEPT ... FROM SYSIN`** | Reads parameters (like the reporting date) typed into the system console or JCL. |

### Advanced Math & Data Types (Crucial for Banking)

| Keyword | Description |
| --- | --- |
| **`COMP-3`** | **Packed Decimal**. This is the #1 most important data type in Mainframes. It stores 2 digits in 1 byte of memory to save space. <br>

<br>• *Example:* `PIC S9(7) COMP-3.` (Stores a 7-digit number in just 4 bytes). |
| **`COMP`** | **Binary**. Stores numbers in pure binary format (Computer native format). Used for loop counters or array indexes because it's fast. |
| **`COMPUTE`** | The "Calculator" command. Allows complex math in one line. <br>

<br>• *Example:* `COMPUTE TOTAL = (AMT-1 + AMT-2) * .10` |
| **`ROUNDED`** | Automatically rounds the result of math to the nearest decimal. <br>

<br>• *Example:* `ADD TAX TO TOTAL ROUNDED.` |
| **`INITIALIZE`** | Wipes a variable clean. Sets numbers to `0` and text to `SPACES` instantly. Much safer than `MOVE 0 TO...`. |

### Loops & Arrays (Data Structures)

| Keyword | Description |
| --- | --- |
| **`OCCURS`** | Defines an **Array** (List). <br>

<br>• *Example:* `05 MONTH-NAME PIC X(10) OCCURS 12 TIMES.` (Creates a list of 12 months). |
| **`INDEXED BY`** | Creates a special internal counter used *only* for that array. Faster than using a normal number variable. |
| **`SEARCH`** | Automatically scans an array to find a value. No need to write a manual loop! |
| **`PERFORM ... VARYING`** | The "For Loop". <br>

<br>• *Example:* `PERFORM 200-LOOP VARYING I FROM 1 BY 1 UNTIL I > 10.` |

---

### String Manipulation (Text Cleaning)

| Keyword | Description |
| --- | --- |
| **`INSPECT`** | Used to count or replace characters in a string. <br>

<br>• *Example:* `INSPECT NAME REPLACING ALL '@' WITH ' '` (Cleans dirty data). |
| **`STRING`** | Concatenates (joins) text together. <br>

<br>• *Example:* `STRING FIRST-NAME DELIMITED BY SPACE, LAST-NAME INTO FULL-NAME.` |
| **`UNSTRING`** | Splits text apart (like Python's `.split()`). Useful for parsing CSV files. |

---

### Advanced Logic & Conditionals

| Keyword | Description |
| --- | --- |
| **`88` Level** | **Condition Names**. Defines a "True/False" state for a variable. <br>

<br>• *Code:* `88 IS-ACTIVE VALUE 'A'.` <br>

<br>• *Usage:* `IF IS-ACTIVE` (Instead of writing `IF STATUS = 'A'`). |
| **`CONTINUE`** | A placeholder that means "Do nothing and keep going." Often used inside complex `IF` statements to handle the "Else" side nicely. |
| **`NEXT SENTENCE`** | An older version of `CONTINUE` that jumps completely out of the current logical block (like a `break`). *Be careful using this, it can be tricky.* |
| **`GO TO`** | **The Danger Zone.** Jumps to a specific paragraph. Modern coding standards forbid this, but you **will** see it in old legacy code. |

---

### Database & VSAM (File Handling)

Since you mentioned "Report for Negotiated", you might eventually touch **VSAM** (Indexed) files.

| Keyword | Description |
| --- | --- |
| **`INVALID KEY`** | Used when reading an Indexed file. "If I can't find the Record ID you asked for, do this..." |
| **`START`** | Positions the file pointer to a specific record *without* reading it yet. Used to "jump" to the middle of a file. |
| **`REWRITE`** | Updates an *existing* record in a file (instead of adding a new one). |
| **`DELETE`** | Removes a record from a file. |

---