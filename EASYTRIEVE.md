# Easytrieve (EZT) Reading Guide for COBOL Migration

**Purpose:** A cheat sheet for decoding Easytrieve source code, focusing on file definitions, variable types, and logic patterns for Mainframe COBOL migration.

---

## 1. Decoding File Definitions
Easytrieve defines data fields using a columnar format. Unlike COBOL, it does not strictly require level numbers (01, 05) or sequential order.

### The Syntax Structure
```easytrieve
FILE IMACTMI VB(27000 27998)
* [Field Name]  [Start]  [Len]  [Type]  [Decimals/Mask]
  CTL1             1       2       N
  OSBAL          369       8       P       2

```

| Column | Meaning | Notes for COBOL |
| --- | --- | --- |
| **Field Name** | The variable name. | Use this as your data name (e.g., `05 CTL1...`). |
| **Start** | The **Start Position** (Offset). | EZT allows jumping around. **COBOL requires sequential order.** You must calculate `FILLER` sizes to bridge gaps between fields. |
| **Len** | The **Physical Length** in bytes. | **CRITICAL:** For Packed Decimal (P), this is *storage size*, not digit count. |
| **Type** | Data Format (A, N, P, B). | See "Data Types" section below. |
| **Decimals** | Number of decimal places. | Used for `PIC 9(x)V99`. |

---

## 2. Data Types & COBOL Equivalents

| EZT Code | Type Name | Meaning | COBOL Equivalent |
| --- | --- | --- | --- |
| **A** | **Alphameric** | Text, numbers, spaces, symbols. | `PIC X(length)` |
| **N** | **Numeric** | "Zoned" decimal (readable text). | `PIC 9(length)` |
| **P** | **Packed** | Packed Decimal (COMP-3). | `PIC S9(...) COMP-3` |
| **B** | **Binary** | Binary Integer. | `PIC S9(...) COMP` |

### The "Packed Decimal" (P) Formula

In EZT, a length of `8` for a Packed field does **not** mean `PIC 9(08)`. It refers to physical bytes.

**Formula to find COBOL Digits:**


**Example:** `OSBAL 369 8 P 2`

1. **Calculate Digits:**  digits total.
2. **Apply Decimals:** The definition says `2` decimal places.
3. **COBOL Result:** `PIC S9(13)V99 COMP-3.` (13 integer + 2 decimal = 15 total).

---

## 3. File Header Analysis

Look at the `FILE` statement at the top of the definition.

```easytrieve
FILE IMACTMI VB(27000 27998)

```

1. **Format (VB vs FB):**
* `VB` = **Variable Blocked**  COBOL `RECORDING MODE V`.
* `FB` = **Fixed Blocked**  COBOL `RECORDING MODE F`.


2. **Record Length (First Number):**
* `27000` = The **Maximum Record Length**.
* **COBOL:** Use this for your `FD` (e.g., `01 REC-DATA PIC X(27000).`).


3. **Block Size (Second Number):**
* `27998` = Block Size.
* **COBOL:** Ignore this. Always use `BLOCK CONTAINS 0 RECORDS` and let the JCL handle it.



---

## 4. Logic Interpretation Tips

### The "Invisible" Loop

* **EZT:** The statement `JOB INPUT FILE-A` automatically creates a read loop. You won't see a `READ` command.
* **COBOL:** You must explicitly code the loop:
```cobol
PERFORM UNTIL FILE-EOF = 'Y'
    READ FILE-A ...
    (Process Logic)
END-PERFORM.

```



### Implicit Redefines (Overlays)

EZT often defines multiple fields starting at the **same position**.

* **EZT:**
```easytrieve
FULL-DATE   10   6   N
MM-DATE     10   2   N  * Starts at 10
DD-DATE     12   2   N
YY-DATE     14   2   N

```


* **COBOL:** You must structure this as a Group Item:
```cobol
05 FULL-DATE.
   10 MM-DATE  PIC 9(02).
   10 DD-DATE  PIC 9(02).
   10 YY-DATE  PIC 9(02).

```



### MASKs are for Printing Only

* **EZT:** `ACCTNO ... MASK ('99-999')`
* **Reality:** The data in the file is just `12345`. The hyphens do not exist in the database.
* **COBOL:** Define the input variable as pure numeric (`PIC 9(05)`). Only add the hyphens when moving it to a report line variable.

---

```

```
