# Working with COBOL

This repository contains basic information and concepts of COBOL. It also includes some practice programs I completed while learning COBOL. Why COBOL? I had a brain fart and decided to learn the basics of it, so here we are. Enjoy!

### Column Structure of COBOL

![image](https://github.com/Gaurav-Van/Working-With-COBOL/assets/50765800/e6a780b9-18c8-4f91-ae70-641b4256bd0d)

1. **Sequence Number Area (Columns 1-6)**:
   
   * This area is used for numbering the lines of code within a COBOL program. It helps with reference, sorting, and debugging.
   * Typically, each line of code starts with a sequence number (e.g., 100, 200, etc.). These numbers are essential for maintaining program flow and readability.

2. **Indicator Area (Column 7)**:
   
   * Column 7 is reserved for specific indicators:
     * `*`: Denotes a comment line.
     * `-`: Indicates continuation from the previous line.
     * `/`: Marks a new page or form feed in printed reports.
   * These indicators provide additional context and control over the program.

3. **Area A (Columns 8-11)**:
   
   * Also known as the “A Area,” this section is where you define division, section, and paragraph names.
   * It’s essential for organizing your COBOL program into logical sections.
   * Statements related to program structure (e.g., `IDENTIFICATION DIVISION`, `DATA DIVISION`, etc.) often appear here.

4. **Area B (Columns 12-72)**:
   
   * The bulk of your COBOL code resides in this area.
   * It includes statements, data manipulation, calculations, and logic.
   * Always adhere to the strict column-based formatting rules within this area.

5. **Identification Area (Columns 73-80)**:
   
   * Historically, this area was used for identification purposes (e.g., program name, author, date).
   * In modern COBOL, it’s less commonly used due to changes in coding practices.
   * Some legacy systems still rely on this area for specific information

<hr>

### Heirarchical Code Structure of COBOL

![image](https://github.com/Gaurav-Van/Working-With-COBOL/assets/50765800/645a84fc-0118-41ce-a4ee-617997c22245)

* **Divisions**: The topmost level of the hierarchy, divisions are the largest units in a COBOL program. There are four main divisions:
  
  * **Identification Division**: Provides the program’s name and other identification details.
  * **Environment Division**: Specifies the computer and peripheral devices the program will use.
  * **Data Division**: Defines all the variables and data structures the program will manipulate.
  * **Procedure Division**: Contains the actual code that performs the program’s tasks.

* **Sections**: Within each division, sections break down the code into smaller, logical groupings. Each section serves a specific purpose related to its division, such as the `FILE SECTION` in the Data Division, which describes file layouts.

* **Paragraphs**: Sections are further divided into paragraphs, which are named blocks of code that perform a particular function. Paragraphs make the code easier to navigate and modify.

* **Sentences**: A sentence is a string of COBOL statements terminated by a period. Sentences are the executable instructions within paragraphs.

* **Statements**: The smallest unit of the hierarchy, statements are the individual COBOL commands that carry out operations, like `MOVE`, `ADD`, or `READ`.

<hr>

### Data Types

we use `Picture Clause` or `PIC`. It sets the Datatype and length of the variable. `PIC Datatype(Length)`

#### Numeric

`PIC 9` Single numeric value. *9* here represents numeric datatype. 

`PIC 9(4)` four numeric values 

#### Alphabetic

`PIC A` Single Alphabetic Character. 

#### Alphanumeric

`PIC X` Single Alphanumeric Character 

`PIC X(8)` Eight Alphanumeric Character

#### Decimal Position

`V` represents decimal position

#### Represent Symbols in the value

`PIC 9(4)V99` used to represents number like 2857.96 or 1234.56

-----

### COBOL Data Types

COBOL primarily deals with two types of data: numeric and alphanumeric.

1. **Numeric Data**: These are numbers that can be used for arithmetic operations.
2. **Alphanumeric Data**: These can contain letters, numbers, and special characters.

#### Picture Clause (PIC)

The PIC clause is used to define the data type and size. It specifies the type and format of the data item.

* **9**: Represents a numeric digit. 
* **X**: Represents an alphanumeric character.
* **A**: Represents an alphabetic character (A-Z and a-z).
* **S**: Indicates a signed numeric value.
* **V**: Implied decimal point.

Examples:

* `PIC 9(5)`: A numeric value with up to 5 digits.
* `PIC X(10)`: An alphanumeric string with up to 10 characters.
* `PIC S9(4)V99`: A signed numeric value with 4 digits before the decimal and 2 digits after.

### COMP (Computational) Usage

COBOL uses several types of computational storage formats, each optimized for specific types of data processing.

#### COMP-1 (Single-precision floating-point)

* **Usage**: For single-precision floating-point numbers.
* **Storage**: Typically 4 bytes.
* **Representation**: Usually conforms to the IEEE 754 standard for floating-point representation.
* **Range**: Approximately 7 decimal digits of precision.

#### COMP-2 (Double-precision floating-point)

* **Usage**: For double-precision floating-point numbers.
* **Storage**: Typically 8 bytes.
* **Representation**: Usually conforms to the IEEE 754 standard for double-precision floating-point representation.
* **Range**: Approximately 15-16 decimal digits of precision.

#### COMP-3 (Packed Decimal)

* **Usage**: For numeric data stored in a packed decimal format.
* **Storage**: Each digit is stored in a half-byte (4 bits), with the sign in the last nibble.
* **Representation**: BCD (Binary Coded Decimal) format.
* **Example**: The number 1234 might be stored as 0x123C (C indicates a positive sign).

#### COMP-4 (Binary - similar to COMP)

* **Usage**: For numeric data stored in a binary format.
* **Storage**: The size is typically determined by the compiler (e.g., 2, 4, or 8 bytes).
* **Representation**: Pure binary format.
* **Note**: Often considered synonymous with COMP in many COBOL implementations.

#### COMP-5 (Native Binary)

* **Usage**: For numeric data stored in the native binary format of the machine.
* **Storage**: Similar to COMP-4 but often optimized for the specific hardware.
* **Representation**: Pure binary format, ensuring more efficient computation.
* **Note**: Designed for better performance on specific hardware.

### Character Encoding: EBCDIC vs. ASCII

#### EBCDIC (Extended Binary Coded Decimal Interchange Code)

* **Usage**: Primarily used in IBM mainframes.
* **Character Set**: An 8-bit character encoding.
* **Storage**: Each character is stored in 1 byte.
* **Range**: 256 possible characters.
* **Example**: The letter 'A' is represented as 0xC1.

#### ASCII (American Standard Code for Information Interchange)

* **Usage**: Widely used in various computing environments.
* **Character Set**: A 7-bit character encoding (often stored as 8-bit with a leading zero).
* **Storage**: Each character is stored in 1 byte.
* **Range**: 128 possible characters (standard ASCII), extended ASCII uses the full 256 range.
* **Example**: The letter 'A' is represented as 0x41.

<hr>

### Literals

Values that have multiple use cases but needs to be define and initialized only once. They do not change. Cobol has some figurative literals built-in 

`ZERO / ZEROES`

`SPACE / SPACES`

`LOW-VALUE`

`HIGH-VALUE`

`NULL / NULLS`

<hr>

### Keywords and Paragraphs

1. **COBOL Keywords (Top 15)**:
   
   * **ACCEPT**: Reads input data into a variable.
   * **ADD**: Performs addition.
   * **DISPLAY**: Outputs data to the screen.
   * **DIVIDE**: Performs division.
   * **IF**: Conditional statement for branching.
   * **MOVE**: Assigns a value to a variable.
   * **PERFORM**: Executes a sequence of statements (similar to a loop or function call).
   * **READ**: Reads records from files.
   * **SUBTRACT**: Performs subtraction.
   * **COMPUTE**: Evaluates arithmetic expressions.
   * **DISPLAY-1**: Used for formatted output.
   * **STOP**: Halts program execution.
   * **STRING**: Concatenates strings.
   * **UNSTRING**: Splits a string into components.
   * **INITIALIZE**: Sets variables to initial values.

2. **Paragraphs in COBOL**:
   
   * A paragraph is a named block of code within a COBOL program.
   * It’s similar to a function or method in other languages but without parameters or return values.
   * Paragraphs provide a way to organize code logically.
   * Each paragraph has a unique name and contains a sequence of COBOL statements.
   * You can invoke a paragraph using the `PERFORM` keyword.
   * Unlike functions, paragraphs don’t have parameters or local variables.

<hr>

### Paragraphs and Perform

##### `COBOL Paragraphs`

* **What are Paragraphs?**
  
  * In COBOL, a paragraph is a named block of code within a program.
  * Think of it as a reusable code segment that performs a specific task.
  * Unlike functions in other languages, paragraphs don’t have parameters or return values.
  * Each paragraph has a unique name (similar to a function name).

* **Purpose of Paragraphs:**
  
  * Organize code logically: You can group related statements together in a paragraph.
  * Improve readability: Descriptive paragraph names make code easier to understand.
  * Encapsulate functionality: A paragraph represents a specific action or operation.

* **Example of a COBOL Paragraph:**
  ```cobol
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MYPROGRAM.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NUM1 PIC 9(3) VALUE 100.
      01 NUM2 PIC 9(3) VALUE 50.
      
      PROCEDURE DIVISION.
          PERFORM CALCULATE-SUM
          DISPLAY "Sum: " NUM1
          STOP RUN.
      
      CALCULATE-SUM.
          ADD NUM2 TO NUM1.
  ```
  

`PERFORM` Statement (Similar to Loops):
---------------------------------------

* **What is `PERFORM`?**
  
  * The `PERFORM` statement executes a sequence of statements (like a loop).
  * It invokes a paragraph by name, causing its code to run.
  * Acts as a control flow mechanism.

* **Usage of `PERFORM`:**
  
  * Execute a paragraph once or repeatedly (like a loop).
  * Specify the paragraph name after `PERFORM`.

* **Example of `PERFORM`:**
  ```cobol
      IDENTIFICATION DIVISION.
      PROGRAM-ID. LOOPSAMPLE.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 COUNTER PIC 9(2) VALUE 1.
      
      PROCEDURE DIVISION.
          PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 10
              DISPLAY "Counter: " COUNTER
          END-PERFORM
          STOP RUN.
  ```

<hr>

### Program Linkage

`CALL` keyword. 

* **What is Program Linkage?**
  
  * Program linkage refers to the interaction between different COBOL programs or subprograms.
  * It allows one program (the **calling program**) to invoke another program (the **called program** or **subprogram**).
  * The purpose is to modularize code, improve maintainability, and reuse common functionality.

* **How Does Program Linkage Work?**
  
  * The **CALL** statement is used to invoke a subprogram from the calling program.
  * The called program can be a separate COBOL program or a specific section within the same program.
  * Data can be passed between the calling program and the called program using parameters.

* **Types of Program Linkage:**<br><br>
 `Static Linkage`

     * The called program is a separate COBOL program with its own source code.
     
     * The calling program references the called program by name.
     
     * The linkage is established during compilation.
     
     * Example:
       
       ```
       CALL 'MYPROG' USING data-parameters
       ```

  `Dynamic Linkage`
   
   * The called program is dynamically loaded at runtime (e.g., as a shared library or DLL).
   
   * The calling program specifies the program name or library.
   
   * The linkage is resolved during execution.
   
   * Example:
     
     ```
     CALL 'MYLIBRARY' USING data-parameters
     ```

---------------

#### Linkage Section in COBOL

* The **Linkage Section** serves as an interface between different COBOL programs or subprograms.
* Its primary role is to declare variables that can be accessed by another program (usually the calling program).
* When one program calls another using the `CALL` statement, data can be passed between them through the linkage section.
* It helps in:
  * Sending data from the calling program to the called program.
  * Receiving data from the called program back to the calling program.

![image](https://github.com/Gaurav-Van/Working-With-COBOL/assets/50765800/122c5279-3d57-455a-87b0-9883f43e2618)

<hr>

### File Output and Reports

`FILLER` Add things in between. Keyword in cobol. 
`FILLER     PIC X(05) VALUE SPACES` adds 5 spaces. 

![image](https://github.com/Gaurav-Van/Working-With-COBOL/assets/50765800/b6fdba13-a0c3-4f04-bcb2-95d7fb0ecd32)

<hr>

### Conditional Expressions

```
IF Condition DO/DISPLAY/ACTION_KEYWORD action
    ELSE DO/DISPLAY/ACTION_KEYWORD action
END-IF
```

##### Evaluate-When (Switch-case)

In COBOL, the **WHEN** keyword is commonly used within the **EVALUATE** statement to handle multiple conditions. Let’s explore how it works:

1. **EVALUATE Statement**:
   
   * The **EVALUATE** statement is similar to a switch or case statement in other languages.
   * It allows you to evaluate an expression and perform different actions based on its value.
   * Within the **EVALUATE**, you can use the **WHEN** keyword to specify conditions.

```cobol
EVALUATE expression
    WHEN condition-1
        PERFORM action-1
    WHEN condition-2
        PERFORM action-2
    ...
    WHEN OTHER
        PERFORM default-action
END-EVALUATE
```

<hr>

### Intrinsic Functions

intrinsic functions (also known as built-in functions) provide a set of predefined operations that allow you to perform specific tasks. These functions simplify coding by providing a shorthand way to execute complex operations.

`ABS` Returns the absolute value of a numeric argument. `ANNUITY` Calculates the ratio of an annuity payment to an initial value. `Upper-case` `Lower-case` | `FV` Calculates the future value of an investment. `PV` Computes the present value of future cash flows. `RATE` Determines the interest rate for an investment.

![image](https://github.com/Gaurav-Van/Working-With-COBOL/assets/50765800/1bdcd6b0-6470-444b-be0f-e5cfaa6d7084)

<hr>

### Extra

```cobol
ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
```

The **RECORDING MODE F** in COBOL refers to a fixed-length format for physical records in a QSAM (Queued Sequential Access Method) file. Let me break it down for you:

1. **RECORDING MODE F (Fixed)**:
   
   * All records in the file have the same fixed length.
   * Each record is wholly contained within one block.
   * Blocks can contain more than one record.
   * There are no record-length or block-descriptor fields.
   * This mode is commonly used when records have a consistent size.
   * It’s ideal for situations where you know the exact length of each record.
   * In your example, the `FD PRINT-LINE RECORDING MODE F` specifies that the file “PRINT-LINE” contains fixed-length records.

2. **FD (File Description)**:
   
   * The `FD` (File Description) section defines the characteristics of a file.
   * It includes information about the file’s organization, access mode, and record format.
   * In your code snippet, `FD PRINT-LINE` declares the file named “PRINT-LINE.”
   * The `RECORDING MODE F` indicates that the records in this file are of fixed length.
   
   <hr>
