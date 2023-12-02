       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02P1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-REC            PIC X(192).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 PZLINPUT-FS     PIC XX.
           05 GAME            PIC X(8).
           05 SUBSETS         PIC X(192).
           05 GAME-TEXT       PIC X(4).
           05 GAME-ID         PIC 999.
           05 CUBES-LIM.
              10 RED          PIC 999 VALUE 12.
              10 GREEN        PIC 999 VALUE 13.
              10 BLUE         PIC 999 VALUE 14.
           05 SUBSETS-TBL OCCURS 0 TO 500 DEPENDING ON SUBSETS-LEN.
              10 NUM          PIC 999.
              10 COLOR        PIC X(5).
           05 SUBSETS-LEN     PIC S9(4) COMP.
           05 SUBSETS-PTR     PIC S9(4) COMP.
           05 CNT             PIC S9(4) COMP.
           05 GAME-SUM        PIC S9(4) COMP.
              
       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X   VALUE 'C'.
              88 PZLINPUT-EOF         VALUE 'E'.
           05 GAME-FLAG       PIC X   VALUE 'N'.
              88 GAME-IMPOSSIBLE      VALUE 'Y'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES
           MOVE 12 TO RED OF CUBES-LIM
           MOVE 13 TO GREEN OF CUBES-LIM
           MOVE 14 TO BLUE OF CUBES-LIM

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM PARSE-GAME
             PERFORM CHECK-GAME
             PERFORM READ-PZLINPUT
           END-PERFORM

           DISPLAY GAME-SUM

           CLOSE PZLINPUT

           GOBACK
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .

       PARSE-GAME SECTION.
           UNSTRING PZL-REC DELIMITED BY ': ' INTO GAME SUBSETS
           UNSTRING GAME DELIMITED BY SPACE INTO GAME-TEXT GAME-ID

           INSPECT SUBSETS REPLACING ALL ',' BY SPACE
           INSPECT SUBSETS REPLACING ALL ';' BY SPACE
           
           *> Blank out table.
           PERFORM VARYING CNT FROM 1 BY 1
                     UNTIL CNT > SUBSETS-LEN
             MOVE ZEROES TO NUM(CNT)
             MOVE SPACES TO COLOR(CNT)
           END-PERFORM

           MOVE 1 TO SUBSETS-LEN SUBSETS-PTR
           PERFORM VARYING CNT FROM 1 BY 1
                     UNTIL CNT GREATER THAN 500

             UNSTRING SUBSETS DELIMITED BY ALL SPACE
                 INTO NUM(SUBSETS-LEN) COLOR(SUBSETS-LEN)
                 WITH POINTER SUBSETS-PTR
             END-UNSTRING
             IF COLOR(CNT) = SPACES
               EXIT PERFORM
             END-IF

             ADD 1 TO SUBSETS-LEN
           END-PERFORM
           .

       CHECK-GAME SECTION.
           MOVE 'N' TO GAME-FLAG
           MOVE 1 TO CNT

           PERFORM VARYING CNT FROM 1 BY 1
                     UNTIL CNT > SUBSETS-LEN
                        OR GAME-IMPOSSIBLE
             EVALUATE FUNCTION TRIM(COLOR(CNT))
               WHEN 'red'
                 IF NUM(CNT) > RED OF CUBES-LIM
                   SET GAME-IMPOSSIBLE TO TRUE
                 END-IF
               WHEN 'green'
                 IF NUM(CNT) > GREEN OF CUBES-LIM
                   SET GAME-IMPOSSIBLE TO TRUE
                 END-IF
               WHEN 'blue'
                 IF NUM(CNT) > BLUE OF CUBES-LIM
                   SET GAME-IMPOSSIBLE TO TRUE
                 END-IF
             END-EVALUATE
           END-PERFORM

           IF NOT GAME-IMPOSSIBLE
             ADD GAME-ID TO GAME-SUM
           END-IF
           .