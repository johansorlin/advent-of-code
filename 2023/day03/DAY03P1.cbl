       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY03P1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS NUMBERS IS '0123456789'
           CLASS SYMBOLS IS '+*=-#/%$&@'.
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
           05 REC             PIC X(140).
           05 REC-LEN         PIC 999.
           05 ORG-REC-LEN     PIC 999.
           05 ROWS            PIC 999.
           05 NUM             PIC 999.
           05 NUM-CHAR        PIC XXX.
           05 NUM-LEN         PIC 9.
           05 NUM-PTR         PIC S9(4) COMP.
           05 SYM-PTR         PIC S9(4) COMP.
           05 CNT             PIC S9(4) COMP.
           05 CN2             PIC S9(4) COMP.
           05 SYM-CNT         PIC S9(4) COMP.
           05 ROW             PIC S9(4) COMP.
           05 PARTS-SUM       PIC S9(9) COMP.
           05 NUM-TBL OCCURS 150 TIMES.
              10 NUM-REC      PIC X(142).
           05 SYM-TBL OCCURS 150 TIMES.
              10 SYM-REC      PIC X(142).
              
       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X   VALUE 'C'.
              88 PZLINPUT-EOF         VALUE 'E'.
           05 NUM-FLAG        PIC X   VALUE 'N'.
              88 NUM-FOUND            VALUE 'Y'.
              88 END-OF-NUMBER        VALUE 'N'.
           05 SYM-FLAG        PIC X   VALUE 'N'.
              88 SYM-FOUND            VALUE 'Y'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES
           MOVE 1 TO NUM-PTR SYM-PTR ROW
           MOVE 140 TO ORG-REC-LEN
           COMPUTE REC-LEN = ORG-REC-LEN + 2

           MOVE SPACES TO NUM-REC(NUM-PTR) SYM-REC(NUM-PTR)

           ADD 1 TO NUM-PTR SYM-PTR

           OPEN INPUT PZLINPUT

           *> Read file and split records into one table of numbers 
           *> and one table of symbols.
           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM PARSE-SCHM
             PERFORM READ-PZLINPUT
             ADD 1 TO NUM-PTR SYM-PTR ROW
           END-PERFORM

           CLOSE PZLINPUT

           MOVE SPACES TO NUM-REC(NUM-PTR) SYM-REC(NUM-PTR)

           COMPUTE ROWS = ROW
           MOVE 1 TO CNT ROW

           *> Find and add up part numbers.
           PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > ROWS
             PERFORM VARYING CNT FROM 1 BY 1 UNTIL CNT > REC-LEN
               IF NUM-REC(ROW)(CNT:1) IS NUMBERS
                 *> Start of number.
                 IF NUM-FLAG NOT = 'Y'
                   SET NUM-FOUND TO TRUE
                 END-IF
                 ADD 1 TO NUM-LEN
                 MOVE NUM-REC(ROW)(CNT:1) TO NUM-CHAR(NUM-LEN:1)
               ELSE
                 *> End of number.
                 IF NUM-FOUND
                   SET END-OF-NUMBER TO TRUE
                   MOVE FUNCTION NUMVAL(NUM-CHAR) TO NUM
                   PERFORM CHECK-NUMBER
                   IF SYM-FOUND
                     ADD NUM TO PARTS-SUM
                   END-IF
                   MOVE ZEROES TO NUM NUM-LEN
                   MOVE SPACES TO NUM-CHAR
                 END-IF
               END-IF
             END-PERFORM

             SET END-OF-NUMBER TO TRUE
             MOVE ZEROES TO NUM NUM-LEN
             MOVE SPACES TO NUM-CHAR
           END-PERFORM

           DISPLAY PARTS-SUM

           GOBACK
           .

       CHECK-NUMBER SECTION.
           MOVE 'N' TO SYM-FLAG
           MOVE 0 TO SYM-CNT

           PERFORM VARYING CN2 FROM 0 BY 1 UNTIL CN2 > NUM-LEN + 1
                                              OR SYM-FOUND
           COMPUTE SYM-CNT = CNT - NUM-LEN - 1 + CN2
             *> Check above for symbols.
             IF ROW NOT <= 1
               IF CNT NOT <= 1
                 IF SYM-REC(ROW - 1)(SYM-CNT:1) IS SYMBOLS
                   SET SYM-FOUND TO TRUE
                 END-IF
               END-IF
             END-IF
             *> Check current row for symbols.
             IF CNT NOT <= 1
               IF SYM-REC(ROW)(SYM-CNT:1) IS SYMBOLS
                 SET SYM-FOUND TO TRUE
               END-IF
             END-IF
             *> Check below for symbols.
             IF ROW NOT >= ROWS
               IF CNT NOT > REC-LEN
                 IF SYM-REC(ROW + 1)(SYM-CNT:1) IS SYMBOLS
                   SET SYM-FOUND TO TRUE
                 END-IF
               END-IF
             END-IF
           END-PERFORM
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .

       PARSE-SCHM SECTION.
           MOVE SPACES TO REC
           MOVE PZL-REC TO REC
           INSPECT REC REPLACING ALL '.' BY SPACE

           *> Replace symbols.
           MOVE 0 TO CNT
           PERFORM VARYING CNT FROM 1 BY 1 UNTIL CNT > ORG-REC-LEN
             IF REC(CNT:1) IS SYMBOLS
               MOVE SPACE TO REC(CNT:1)
             END-IF
           END-PERFORM

           STRING ' ' REC ' ' INTO NUM-REC(NUM-PTR)

           MOVE SPACES TO REC
           MOVE PZL-REC TO REC
           INSPECT REC REPLACING ALL '.' BY SPACE
           
           *> Replace numbers.
           MOVE 0 TO CNT
           PERFORM VARYING CNT FROM 1 BY 1 UNTIL CNT > ORG-REC-LEN
             IF REC(CNT:1) IS NUMBERS
               MOVE SPACE TO REC(CNT:1)
             END-IF
           END-PERFORM

           STRING ' ' REC ' ' INTO SYM-REC(SYM-PTR)
           .

   