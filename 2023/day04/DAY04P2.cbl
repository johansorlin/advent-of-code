       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY04P2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO '20.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-REC            PIC X(140).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 PZLINPUT-FS     PIC XX.
           05 CARD-MATCHES    PIC 9(2).
           05 CARDS-SUM       PIC 9(16).
           05 CNT             PIC 9(6).
           05 CN2             PIC 9(6).
           05 CN3             PIC 9(6).
           05 CN4             PIC 9(6).
           05 CN5             PIC 9(2).
           05 CARDS-CNT       PIC 9(3).
           05 WI-CNT          PIC 9(6).
           05 MY-CNT          PIC 9(6).
           *> Current card.
           05 CARD            PIC X(8).
           05 NUMBERS         PIC X(192).
           05 CARD-TEXT       PIC X(4).
           05 CARD-ID         PIC 999.
           05 WI-NUM          PIC X(29).
           05 MY-NUM          PIC X(74).
           *> All cards.
           05 CARDS-TBL OCCURS 201.
             10 INSTANCES     PIC 9(6).
             10 WI-TBL OCCURS 10.
                15 T-WI-NUM   PIC 99.
             10 MY-TBL OCCURS 25.
                15 T-MY-NUM   PIC 99.

       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X   VALUE 'C'.
              88 PZLINPUT-EOF         VALUE 'E'.
           05 MATCH-FLAG      PIC X   VALUE 'N'.
              88 FIRST-MATCH          VALUE 'Y'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES

           OPEN INPUT PZLINPUT

           *> Ingest cards.
           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM PARSE-GAME
             PERFORM READ-PZLINPUT
           END-PERFORM

           CLOSE PZLINPUT

           *> Original cards.
           MOVE CARDS-CNT TO CARDS-SUM

           *> Go through cards.
           PERFORM VARYING CNT FROM 1 BY 1
                     UNTIL CNT > 10000
                        OR T-WI-NUM(1, CNT) IS ZERO
             *> Instances of card.
             PERFORM VARYING CN2 FROM 1 BY 1
                       UNTIL CN2 > INSTANCES(CNT)
               *> Check card, tallying matches.
               PERFORM CHECK-GAME
               IF CARD-MATCHES > 0
                 PERFORM VARYING CN5 FROM 1 BY 1
                           UNTIL CN5 > CARD-MATCHES
                   ADD 1 TO INSTANCES(CNT + CN5) CARDS-SUM
                 END-PERFORM
               END-IF
             END-PERFORM
           END-PERFORM

           DISPLAY CARDS-SUM

           GOBACK
           .

       PARSE-GAME SECTION.
           MOVE SPACES TO CARD NUMBERS CARD-TEXT WI-NUM MY-NUM
           MOVE ZEROES TO CARD-ID WI-CNT MY-CNT

           UNSTRING PZL-REC DELIMITED BY ': ' INTO CARD NUMBERS
           UNSTRING CARD DELIMITED BY SPACE INTO CARD-TEXT CARD-ID
           UNSTRING NUMBERS DELIMITED BY ' | ' INTO WI-NUM MY-NUM

           ADD 1 TO CARDS-CNT
           MOVE 1 TO INSTANCES(CARDS-CNT)

           *> Ingest winning numbers.
           PERFORM VARYING CNT FROM 1 BY 3
                     UNTIL CNT > 29
                        OR WI-NUM(CNT:2) IS = SPACES *> For test input.
             ADD 1 TO WI-CNT
             MOVE WI-NUM(CNT:2) TO T-WI-NUM(WI-CNT, CARDS-CNT)
           END-PERFORM

           *> Ingest my numbers.
           PERFORM VARYING CNT FROM 1 BY 3
                     UNTIL CNT > 74
                        OR MY-NUM(CNT:2) IS = SPACES *> For test input.
             ADD 1 TO MY-CNT
             MOVE MY-NUM(CNT:2) TO T-MY-NUM(MY-CNT, CARDS-CNT)
           END-PERFORM
           .

       CHECK-GAME SECTION.
           MOVE 'N' TO MATCH-FLAG
           MOVE ZEROES TO CARD-MATCHES

           *> Check my numbers against winning.
           PERFORM VARYING CN3 FROM 1 BY 1
                     UNTIL CN3 > 25
                        OR T-MY-NUM(CN3, CNT) =
                           ZEROES *> For test input.
             PERFORM VARYING CN4 FROM 1 BY 1
                       UNTIL CN4 > 10
                          OR T-WI-NUM(CN4, CNT) =
                             ZEROES *> For test input.
               IF T-MY-NUM(CN3, CNT) = T-WI-NUM(CN4, CNT)
                 ADD 1 TO CARD-MATCHES
               END-IF
             END-PERFORM
           END-PERFORM
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .