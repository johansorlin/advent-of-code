       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY04P1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-REC            PIC X(140).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 PZLINPUT-FS     PIC XX.
           05 CARD-POINTS     PIC S9(9) COMP.
           05 POINTS-SUM      PIC S9(9) COMP.
           05 CNT             PIC S9(4) COMP.
           05 CN2             PIC S9(4) COMP.
           05 WI-CNT          PIC S9(4) COMP.
           05 MY-CNT          PIC S9(4) COMP.
           *> Current card.
           05 CARD            PIC X(8).
           05 NUMBERS         PIC X(192).
           05 CARD-TEXT       PIC X(4).
           05 CARD-ID         PIC 999.
           05 WI-NUM          PIC X(29).
           05 MY-NUM          PIC X(74).
           05 WI-TBL OCCURS 10 TIMES.
              10 T-WI-NUM     PIC 99.
           05 MY-TBL OCCURS 25 TIMES.
              10 T-MY-NUM     PIC 99.

       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X   VALUE 'C'.
              88 PZLINPUT-EOF         VALUE 'E'.
           05 MATCH-FLAG      PIC X   VALUE 'N'.
              88 FIRST-MATCH          VALUE 'Y'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM PARSE-GAME
             DISPLAY CARD-TEXT CARD-ID WI-NUM MY-NUM
             DISPLAY 'WI-NUM table: ' T-WI-NUM(1) T-WI-NUM(5)
             PERFORM CHECK-GAME
             PERFORM READ-PZLINPUT
           END-PERFORM

           CLOSE PZLINPUT

           DISPLAY POINTS-SUM

           GOBACK
           .

       PARSE-GAME SECTION.
           MOVE SPACES TO CARD NUMBERS CARD-TEXT WI-NUM MY-NUM
           MOVE ZEROES TO CARD-ID WI-CNT MY-CNT

           UNSTRING PZL-REC DELIMITED BY ': ' INTO CARD NUMBERS
           UNSTRING CARD DELIMITED BY SPACE INTO CARD-TEXT CARD-ID
           UNSTRING NUMBERS DELIMITED BY ' | ' INTO WI-NUM MY-NUM

           *> Ingest winning numbers.
           PERFORM VARYING CNT FROM 1 BY 3
                     UNTIL CNT > 29
                        OR WI-NUM(CNT:2) IS = SPACES *> For test input.
             ADD 1 TO WI-CNT
             MOVE WI-NUM(CNT:2) TO T-WI-NUM(WI-CNT)
           END-PERFORM

           *> Ingest my numbers.
           PERFORM VARYING CNT FROM 1 BY 3
                     UNTIL CNT > 74
                        OR MY-NUM(CNT:2) IS = SPACES *> For test input.
             ADD 1 TO MY-CNT
             MOVE MY-NUM(CNT:2) TO T-MY-NUM(MY-CNT)
           END-PERFORM
           .

       CHECK-GAME SECTION.
           MOVE 'N' TO MATCH-FLAG
           MOVE ZEROES TO CARD-POINTS

           *> Check my numbers against winning.
           PERFORM VARYING CNT FROM 1 BY 1
                     UNTIL CNT > 25
                        OR T-MY-NUM(CNT) = ZEROES *> For test input.
             PERFORM VARYING CN2 FROM 1 BY 1
                       UNTIL CN2 > 10
                          OR T-WI-NUM(CN2) = ZEROES *> For test input.
               IF T-MY-NUM(CNT) = T-WI-NUM(CN2)
                 IF MATCH-FLAG = 'N'
                   SET FIRST-MATCH TO TRUE
                   MOVE 1 TO CARD-POINTS
                 ELSE
                   COMPUTE CARD-POINTS = CARD-POINTS * 2
                 END-IF
               END-IF
             END-PERFORM
           END-PERFORM

           ADD CARD-POINTS TO POINTS-SUM
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .