       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01P2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-RECORD              PIC X(128).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 PZL-REC-C            PIC S9(4) COMP.
           05 FIRST-DIGIT          PIC X.
           05 LAST-DIGIT           PIC X.
           05 PZLINPUT-FS          PIC XX.
           05 PZL-REC-NC           PIC XX.
           05 PZL-REC-ND           PIC 99.
           05 CALIBRATION-SUM      PIC S9(18) COMP.
           05 REC PIC X(128).
           05 FND PIC X.
           05 CNT PIC S9(4).
           05 LTH PIC S9(4).
           05 CN2 PIC S9(4).
       01  SWITCHES.
           05 PZLINPUT-FLAG        PIC X    VALUE 'C'.
              88 PZLINPUT-EOF               VALUE 'E'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE
                      SWITCHES

           MOVE 0 TO CALIBRATION-SUM

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           MOVE PZL-RECORD TO REC
           PERFORM UNTIL PZLINPUT-EOF
             COMPUTE LTH = FUNCTION LENGTH(FUNCTION TRIM(REC))
             PERFORM CONVERT-LETTERS
             
             PERFORM GET-FIRST-AND-LAST

             

             MOVE SPACES TO REC
             PERFORM READ-PZLINPUT
             MOVE PZL-RECORD TO REC
           END-PERFORM

           CLOSE PZLINPUT


           DISPLAY CALIBRATION-SUM



           GOBACK
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-RECORD
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
             CONTINUE
           END-IF
           .

       GET-FIRST-AND-LAST SECTION.
           COMPUTE PZL-REC-C = 1
           MOVE SPACES TO PZL-REC-NC
           MOVE SPACES TO FIRST-DIGIT
           MOVE SPACES TO LAST-DIGIT
           MOVE 0 TO PZL-REC-ND

           *> Find first digit.
           PERFORM UNTIL PZL-REC-C > LTH
                      OR FIRST-DIGIT IS NOT = SPACES

             IF REC(PZL-REC-C:1) IS NUMERIC
               MOVE REC(PZL-REC-C:1) TO FIRST-DIGIT
               CONTINUE
             END-IF

             ADD 1 TO PZL-REC-C

           END-PERFORM

           *> Find last digit.
           COMPUTE PZL-REC-C = 0
           MOVE LTH TO PZL-REC-C
           PERFORM UNTIL PZL-REC-C < 1
                      OR LAST-DIGIT IS NOT = SPACES
             IF REC(PZL-REC-C:1) IS NUMERIC
               MOVE REC(PZL-REC-C:1) TO LAST-DIGIT
               CONTINUE
             END-IF
             SUBTRACT 1 FROM PZL-REC-C
           END-PERFORM
           
           STRING FIRST-DIGIT LAST-DIGIT INTO PZL-REC-NC
           COMPUTE PZL-REC-ND = FUNCTION NUMVAL(PZL-REC-NC)

           *>ADD PZL-REC-ND TO CALIBRATION-SUM
           COMPUTE CALIBRATION-SUM = CALIBRATION-SUM + PZL-REC-ND
           .

       CONVERT-LETTERS.
           *> Replace first
           MOVE SPACE TO FND
           MOVE 1 TO CNT
           MOVE 0 TO CN2
           PERFORM UNTIL FND = 'Y' OR CNT > LTH
             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'one'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'one' by 'o1e'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'two'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'two' by 't2o'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'three'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'three' by 'th3ee'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'four'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'four' by 'fo4r'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'five'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'five' by 'fi5e'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'six'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'six' by 's6x'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'seven'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'seven' by 'se7en'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'eight'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'eight' by 'ei8ht'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(1:CNT) TALLYING CN2 FOR ALL 'nine'
             IF CN2 > 0
               INSPECT REC(1:CNT) REPLACING ALL 'nine' by 'n9ne'
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             ADD 1 TO CNT
           END-PERFORM

           *> Replace last
           MOVE SPACE TO FND
           MOVE 1 TO CNT
           MOVE 0 TO CN2
           PERFORM UNTIL FND = 'Y' OR CNT > LTH
             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'one'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'one' by '1  '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'two'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'two' by '2  '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'three'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH)
               REPLACING ALL 'three' by '3    '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'four'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'four' by '4   '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'five'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'five' by '5   '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'six'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'six' by '6  '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'seven'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH)
               REPLACING ALL 'seven' by '7    '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'eight'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH)
               REPLACING ALL 'eight' by '8    '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             INSPECT REC(LTH - CNT:LTH) TALLYING CN2 FOR ALL 'nine'
             IF CN2 > 0
               INSPECT REC(LTH - CNT:LTH) REPLACING ALL 'nine' by '9   '
               MOVE 'Y' TO FND
               CONTINUE
             END-IF
           MOVE 0 TO CN2

             ADD 1 TO CNT
           END-PERFORM
           .

