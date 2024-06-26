       IDENTIFICATION DIVISION.
       PROGRAM-ID. TicTacToe.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PLAYER PIC A(1).
                   88 PLAYER-ONE VALUE "X".
                   88 PLAYER-TWO VALUE "O".
       01 WS-STATE PIC A(5).
                   88 GAME-OVER VALUES "WIN", "LOSE", "STALE".
       01 WS-MOVE-OUTCOME PIC A(5).
                   88 MOVE-COMPLETE VALUES "WIN", "LOSE", "FAIL".
       01 WS-MASK-DETECTED PIC 9(1).
                   88 WIN-DETECTED VALUES 3, 4, 5, 6, 7, 8, 9.
       01 WS-COMPUTER-MOVED PIC 9(1).
                   88 COMPUTER-MOVED VALUE 1.
       01 WS-EOF PIC 9(1).
                   88 EOF VALUE 1.
       01 WS-SWAP-PLAYERS PIC 9(1).
                   88 SWAP-PLAYERS VALUE 1.
        01 WS-NEXT-MOVE PIC X(2).
            88 FINISHED-PLAYING VALUES "N", "n".
        01 WS-GAME-GRID.
            05 WS-GAME-GRID-ROW OCCURS 3 TIMES.
                10 WS-GAME-GRID-COL OCCURS 3 TIMES.
                    15 WS-CELL PIC X(1).
        01 WS-WIN-MASK.
            05 WS-WIN-MASK-ROW PIC X(9) OCCURS 8 TIMES.
        01 WS-CURRENT-WIN-MASK PIC X(9).
        01 WS-MASK-COUNT PIC 9(1).
        01 WS-COL PIC 9(1).
        01 WS-ROW PIC 9(1).
        01 WS-WINS PIC 9(2).
        01 WS-MOVES PIC 9(2).
        01 WS-GAMES PIC 9(2).
        01 WS-COMPUTER-MOVE PIC 9(1).
        01 WS-DETECT-LOOP-COUNT PIC 9(1).
        01 WS-MESSAGE PIC X(128).
        01 WS-INSTRUCTION PIC X(16).
        01 WS-FLAT-GAME-GRID PIC X(9).
       
       PROCEDURE DIVISION.
           MOVE "X" TO WS-PLAYER
           MOVE "111000000" TO WS-WIN-MASK-ROW(1)
           MOVE "000111000" TO WS-WIN-MASK-ROW(2)
           MOVE "000000111" TO WS-WIN-MASK-ROW(3)
           MOVE "100010001" TO WS-WIN-MASK-ROW(4)
           MOVE "001010100" TO WS-WIN-MASK-ROW(5)
           MOVE "100100100" TO WS-WIN-MASK-ROW(6)
           MOVE "010010010" TO WS-WIN-MASK-ROW(7)
           MOVE "001001001" TO WS-WIN-MASK-ROW(8)
           PERFORM GAME-LOOP-PARAGRAPH
               WITH TEST AFTER UNTIL FINISHED-PLAYING
           STOP RUN.
       
       GAME-LOOP-PARAGRAPH.
               INITIALIZE WS-GAME-GRID
               INITIALIZE WS-STATE
               INITIALIZE WS-MOVES
               MOVE "Make a move like 'A2'" TO WS-MESSAGE
               PERFORM GAME-FRAME-PARAGRAPH
                   WITH TEST AFTER UNTIL GAME-OVER
               ADD 1 TO WS-GAMES END-ADD
               IF WS-STATE IS EQUAL TO "WIN"
                   ADD 1 TO WS-WINS END-ADD
                   DISPLAY "WINNER! (^_^)" 
                   IF PLAYER-ONE
                       MOVE "PLAYER-TWO WINS (O)" TO WS-MESSAGE
                   ELSE
                       MOVE "PLAYER-ONE WINS (X)" TO WS-MESSAGE
                   END-IF
               END-IF
               DISPLAY WS-MESSAGE 
               DISPLAY "GAME " WS-STATE 
               MOVE "n" TO WS-NEXT-MOVE
           .
       
       GAME-FRAME-PARAGRAPH.
           MOVE "Move to square: " TO WS-INSTRUCTION
           INITIALIZE WS-MOVE-OUTCOME
           INITIALIZE WS-NEXT-MOVE
           DISPLAY "   +---+---+---+   " 
           DISPLAY " A | "WS-CELL(1,1)"|"WS-CELL(1,2)"| "WS-CELL(1,3)"|" 
           DISPLAY "   +---+---+---+   " 
           DISPLAY " B | "WS-CELL(2,1)"| "WS-CELL(2,2)"|"WS-CELL(2,3)"|" 
           DISPLAY "   +---+---+---+   " 
           DISPLAY " C | "WS-CELL(3,1)"| "WS-CELL(3,2)"|"WS-CELL(3,3)"|" 
           DISPLAY "   +---+---+---+   " 
           DISPLAY "     1   2   3     " 
           DISPLAY " " 
           DISPLAY "Stats:" 
           DISPLAY " > Moves played = " WS-MOVES 
           DISPLAY " > Games won = " WS-WINS "/" WS-GAMES 
           DISPLAY " "
           DISPLAY "Message: " WS-MESSAGE 
           DISPLAY WS-INSTRUCTION
           ACCEPT WS-NEXT-MOVE
           DISPLAY "Accepted " WS-NEXT-MOVE " from STDIN"
               DISPLAY " "
               EVALUATE FUNCTION UPPER-CASE(WS-NEXT-MOVE(1:1))
                   WHEN "A" SET WS-ROW TO 1
                   WHEN "B" SET WS-ROW TO 2
                   WHEN "C" SET WS-ROW TO 3
                   WHEN OTHER MOVE "FAIL" TO WS-MOVE-OUTCOME
               END-EVALUATE
               SET WS-COL TO WS-NEXT-MOVE(2:1)
               IF
                   WS-MOVE-OUTCOME IS NOT EQUAL TO "FAIL"
                   AND WS-COL IS GREATER THAN 0
                   AND WS-COL IS LESS THAN 4
                   AND WS-CELL(WS-ROW,WS-COL) = " "
               THEN
                   MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
               ELSE
                   MOVE "FAIL" TO WS-MOVE-OUTCOME
               END-IF
               MOVE WS-GAME-GRID TO WS-FLAT-GAME-GRID
               IF PLAYER-ONE
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "1"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "0"
               ELSE
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "0"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "1"
               END-IF
               INSPECT WS-FLAT-GAME-GRID REPLACING ALL " " BY "0"
               INITIALIZE WS-MASK-COUNT
               PERFORM 8 TIMES
                   ADD 1 TO WS-MASK-COUNT END-ADD
             MOVE WS-WIN-MASK-ROW(WS-MASK-COUNT) TO WS-CURRENT-WIN-MASK
                   PERFORM VALIDATE-WIN-PARAGRAPH
               END-PERFORM
               
               IF NOT MOVE-COMPLETE AND WS-MOVES IS EQUAL TO 8
                   MOVE "STALE" TO WS-MOVE-OUTCOME
               END-IF
               INITIALIZE WS-SWAP-PLAYERS
               EVALUATE WS-MOVE-OUTCOME
               WHEN "WIN"
                   MOVE "WINNER! (^_^)" TO WS-MESSAGE
                   MOVE "WIN" TO WS-STATE
                   SET WS-SWAP-PLAYERS TO 1
               WHEN "LOSE"
                   MOVE "YOU DIED (x_x)" TO WS-MESSAGE
                   MOVE "LOSE" TO WS-STATE
                   SET WS-SWAP-PLAYERS TO 1
               WHEN "STALE"
                   MOVE "Stalemate! (>_<)" TO WS-MESSAGE
                   MOVE "STALE" TO WS-STATE
               WHEN "FAIL"
                   MOVE "Invalid move... (o_O)" TO WS-MESSAGE
               WHEN OTHER
                   MOVE "Enter a move" TO WS-MESSAGE
                   SET WS-SWAP-PLAYERS TO 1
                   ADD 1 TO WS-MOVES END-ADD
               END-EVALUATE
               IF SWAP-PLAYERS
                   IF PLAYER-ONE
                       MOVE "O" TO WS-PLAYER
                   ELSE
                       MOVE "X" TO WS-PLAYER
                   END-IF
               END-IF
           .
           
       VALIDATE-WIN-PARAGRAPH.
               INITIALIZE WS-MASK-DETECTED
               SET WS-DETECT-LOOP-COUNT TO 1
               PERFORM 9 TIMES
                   IF
                       WS-CURRENT-WIN-MASK(WS-DETECT-LOOP-COUNT:1)
                       IS EQUAL TO
                       WS-FLAT-GAME-GRID(WS-DETECT-LOOP-COUNT:1)
                       AND IS EQUAL TO 1
                   THEN
                       ADD 1 TO WS-MASK-DETECTED END-ADD
                   END-IF
                   ADD 1 TO WS-DETECT-LOOP-COUNT END-ADD
               END-PERFORM
               IF WIN-DETECTED
                   IF PLAYER-ONE
                       MOVE "WIN" TO WS-MOVE-OUTCOME
                       MOVE "PLAYER-ONE WINS (X)" TO WS-MESSAGE
                   ELSE
                       MOVE "WIN" TO WS-MOVE-OUTCOME
                       MOVE "PLAYER-TWO WINS (X)" TO WS-MESSAGE
                   END-IF
               END-IF
           .