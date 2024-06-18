000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. MergeFiles.
000300* Example program demonstrating the use of the MERGE.
000400* The program merges the file Students.Dat and 
000500* Transins.Dat to create a new file Students.New.
000600* A problem with using the MERGE for inserting records is that 
000700* duplicate records are not detected.
000800
000900 ENVIRONMENT DIVISION.
001000 INPUT-OUTPUT SECTION.
001100 FILE-CONTROL.
001200     SELECT StudentFile ASSIGN TO "STUDENTS.DAT"
001300              ORGANIZATION IS LINE SEQUENTIAL.
001400 
001500     SELECT InsertionsFile ASSIGN TO "TRANSINS.DAT"
001600              ORGANIZATION IS LINE SEQUENTIAL.
001700 
001800     SELECT NewStudentFile    ASSIGN TO "STUDENTS.NEW"
001900              ORGANIZATION IS LINE SEQUENTIAL.
002000 
002100     SELECT WorkFile ASSIGN TO "WORK.TMP".
002200 
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  StudentFile.
002600 01  StudentRec             PIC X(30).
002700 
002800 FD  InsertionsFile.
002900 01  InsertionRec           PIC X(30).
003000 
003100 FD  NewStudentFile.
003200 01  NewStudentRec          PIC X(30).
003300 
003400 SD  WorkFile.
003500 01  WorkRec.
003600     02 WStudentId          PIC 9(7).
003700     02 FILLER              PIC X(23).
003800 
003900 PROCEDURE DIVISION.
004000 Begin.
004100     MERGE WorkFile
004200        ON ASCENDING KEY WStudentId
004300        USING InsertionsFile,  StudentFile
004400        GIVING NewStudentFile.
004500     STOP RUN.
004600     