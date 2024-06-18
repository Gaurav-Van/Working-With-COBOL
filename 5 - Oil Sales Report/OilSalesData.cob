000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. OilSalesData.
000300*produce a summary sales report from an unsorted sequential file 
000400*containing the details of sales of essential and base oils to 
000500*Aromamora customers.  
000600 ENVIRONMENT DIVISION.
000700 INPUT-OUTPUT SECTION.
000800 FILE-CONTROL.
000900        SELECT Sales-File ASSIGN TO "SALES.DAT"
001000                  ORGANIZATION IS LINE SEQUENTIAL.
001100 
001200        SELECT Work-File ASSIGN TO "SORT.TMP".
001300 
001400        SELECT Summary-Report ASSIGN TO "AROMASALES.RPT"
001500                  ORGANIZATION IS LINE SEQUENTIAL.
001600 
001700 
001800        SELECT Sorted-File ASSIGN TO "SORTSALE.DAT"
001900                  ORGANIZATION IS LINE SEQUENTIAL.
002000 
002100 DATA DIVISION.
002200 FILE SECTION.
002300 FD Sales-File.
002400 01  Sales-Rec.
002500     88 End-Of-Sales-File    VALUE HIGH-VALUES.
002600     02  SF-Cust-Id              PIC X(5).
002700     02  SF-Cust-Name            PIC X(20).
002800     02  SF-Oil-Id.
002900         03  FILLER              PIC X.
003000             88 Essential-Oil   VALUE "E".
003100         03  SF-Oil-Name         PIC 99.
003200     02 SF-Unit-Size             PIC 99.
003300     02 SF-Units-Sold            PIC 999.
003400 
003500 
003600 SD Work-File.
003700 01 Work-Rec.
003800    88 End-Of-Work-File VALUE HIGH-VALUES.
003900     02  WF-Cust-Id              PIC X(5).
004000     02  WF-Cust-Name            PIC X(20).
004100     02  WF-Oil-Id.
004200         03 FILLER               PIC X.
004300         03 WF-Oil-Num           PIC 99.
004400     02 WF-Unit-Size             PIC 99.
004500     02 WF-Units-Sold            PIC 999.
004600 
004700 
004800 FD Summary-Report.
004900 01 Print-Line                   PIC X(64).
005000 
005100 FD Sorted-File.
005200 01 Sorted-Rec                   PIC X(33).
005300 
005400 
005500 
005600 WORKING-STORAGE SECTION.
005700 
005800 01  Oils-Table.
005900     02  Oil-Cost-Values.
006000         03 FILLER               PIC X(40) 
006100                 VALUE "0041003200450050002910250055003900650075".
006200         03 FILLER               PIC X(40) 
006300                 VALUE "0080004400500063006500550085004812500065".
006400         03 FILLER               PIC X(40) 
006500                 VALUE "0060005500670072006501250085006511150105".
006600     02  FILLER REDEFINES Oil-Cost-VALUES.
006700         03 OIL-COST           PIC 99V99 OCCURS 30 TIMES.
006800 
006900 01  Report-Heading-Line         PIC X(44)
007000             VALUE "              AROMAMORA SUMMARY SALES REPORT". 
007100 
007200 01  Report-Heading-Underline.
007300     02  FILLER                  PIC X(13) VALUE SPACES.
007400     02  FILLER                  PIC X(32) VALUE ALL "-".
007500 
007600 01  Topic-Heading-Line.
007700     02  FILLER                  PIC BX(13) VALUE "CUSTOMER NAME".
007800     02  FILLER                  PIC X(8) VALUE SPACES.
007900     02  FILLER                  PIC X(10) VALUE "CUST-ID   ".
008000     02  FILLER                  PIC X(8) VALUE "SALES   ".
008100     02  FILLER                  PIC X(11) VALUE "QTY SOLD   ".
008200     02  FILLER                  PIC X(11) VALUE "SALES VALUE".
008300 
008400 01  Cust-Sales-Line.
008500     02  Prn-Cust-Name           PIC X(20).
008600     02  Prn-Cust-Id             PIC BBB9(5).
008700     02  Prn-Cust-Sales          PIC BBBBBZZ9.
008800     02  Prn-Qty-Sold            PIC BBBBBZZ,ZZ9.
008900     02  Prn-Sales-Value         PIC BBBB$$$,$$9.99.                 
009000 
009100 01  Total-Sales-Line.
009200     02  FILLER                  PIC X(33) VALUE SPACES.
009300     02  FILLER                  PIC X(19) VALUE "TOTAL SALES :".
009400     02  Prn-Total-Sales         PIC BBBBBBZZ,ZZ9.
009500 
009600 
009700 01  Total-Qty-Sold-Line.
009800     02  FILLER                PIC X(33) VALUE SPACES.
009900     02  FILLER                PIC X(19) VALUE "TOTAL QTY SOLD :".
010000     02  Prn-Total-Qty-Sold    PIC BBBBBZZZ,ZZ9.
010100 
010200 01  Total-Sales-Value-Line.
010300     02  FILLER                  PIC X(33) VALUE SPACES.
010400     02  FILLER             PIC X(19) VALUE "TOTAL SALES VALUE :".
010500     02  Prn-Total-Sales-Value   PIC B$$$$,$$9.99.
010600 
010700 01  Cust-Totals.
010800     02  Cust-Sales              PIC 999.
010900     02  Cust-Qty-Sold           PIC 9(5).
011000     02  Cust-Sales-Value        PIC 9(5)V99.
011100 
011200 01  Final-Totals.
011300     02  Total-Sales             PIC 9(5)    VALUE ZEROS.
011400     02  Total-Qty-Sold          PIC 9(6)    VALUE ZEROS.
011500     02  Total-Sales-Value       PIC 9(6)V99 VALUE ZEROS.
011600 
011700 01  Temp-Variables.
011800     02  Sale-Qty-Sold           PIC 99999.
011900     02  Value-Of-Sale           PIC 999999V99. 
012000     02  Prev-Cust-Id            PIC X(5).
012100 
012200 PROCEDURE DIVISION.
012300 Produce-Summary-Report.
012400     SORT Work-File ON ASCENDING WF-Cust-Name
012500          INPUT PROCEDURE IS Select-Essential-Oils
012600          OUTPUT PROCEDURE IS Print-Summary-Report.
012700 
012800     STOP RUN. 
012900 
013000 Select-Essential-Oils.
013100     OPEN INPUT Sales-File.
013200     READ Sales-File
013300         AT END SET End-Of-Sales-File TO TRUE
013400     END-READ.
013500     
013600     PERFORM UNTIL End-Of-Sales-File
013700         IF Essential-Oil 
013800             RELEASE Work-Rec FROM Sales-Rec
013900         END-IF
014000         READ Sales-File
014100             AT END SET End-Of-Sales-File TO TRUE
014200         END-READ
014300     END-PERFORM.
014400 
014500     CLOSE Sales-File.
014600 
014700 Print-Summary-Report.
014800     OPEN OUTPUT Summary-Report.
014900     OPEN OUTPUT Sorted-File.
015000     WRITE Print-Line FROM Report-Heading-Line AFTER 
015100     ADVANCING 1 LINE.
015200     WRITE Print-Line FROM Report-Heading-Underline AFTER 
015300     ADVANCING 1 LINE
015400     WRITE Print-Line FROM Topic-Heading-Line AFTER ADVANCING 
015500     3 LINES.
015600 
015700     RETURN Work-File
015800         AT END SET End-Of-Work-File TO TRUE
015900     END-RETURN.
016000 
016100     PERFORM Print-Customer-Lines UNTIL End-Of-Work-File
016200 
016300 
016400     MOVE Total-Sales TO Prn-Total-Sales.
016500     WRITE Print-Line FROM Total-Sales-Line AFTER ADVANCING 
016600     3 LINES.
016700 
016800     MOVE Total-Qty-Sold TO Prn-Total-Qty-Sold.
016900     WRITE Print-Line FROM Total-Qty-Sold-Line AFTER ADVANCING 
017000     2 LINES.
017100 
017200     MOVE Total-Sales-Value TO Prn-Total-Sales-Value.
017300     WRITE Print-Line FROM Total-Sales-Value-Line AFTER 
017400     ADVANCING 2 LINES.
017500 
017600     CLOSE Summary-Report, Sorted-File.
017700 
017800 Print-Customer-Lines.
017900     MOVE ZEROS TO Cust-Totals.
018000     MOVE WF-Cust-Id TO Prn-Cust-Id, Prev-Cust-Id.
018100     MOVE WF-Cust-Name TO Prn-Cust-Name.
018200 
018300     PERFORM UNTIL WF-Cust-Id NOT = Prev-Cust-Id
018400         WRITE Sorted-Rec FROM Work-Rec
018500         ADD 1 TO Cust-Sales, Total-Sales
018600 
018700         COMPUTE Sale-Qty-Sold = WF-Unit-Size * WF-Units-Sold
018800         ADD Sale-Qty-Sold TO Cust-Qty-Sold, Total-Qty-Sold
018900 
019000     COMPUTE Value-Of-Sale = Sale-Qty-Sold * Oil-Cost(WF-Oil-Num)
019100         ADD Value-Of-Sale TO Cust-Sales-Value, Total-Sales-Value
019200         
019300         RETURN Work-File
019400             AT END SET End-Of-Work-File TO TRUE
019500         END-RETURN
019600     END-PERFORM.
019700 
019800     MOVE Cust-Sales TO Prn-Cust-Sales.
019900     MOVE Cust-Qty-Sold TO Prn-Qty-Sold.
020000     MOVE Cust-Sales-Value TO Prn-Sales-Value.
020100 
020200     WRITE Print-Line FROM Cust-Sales-Line AFTER ADVANCING 
020300     2 LINES.
020400     
020500  
020600 
020700 
020800     