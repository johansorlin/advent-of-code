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
       01  PZL-REC            PIC X(128).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 PZLINPUT-FS     PIC XX.
           05 REC             PIC X(128).
           05 CNT             PIC 9(4).
           05 TAL             PIC 9(4).
           05 LEN             PIC 9(4).
           05 FIRST-DIGIT     PIC X.
           05 LAST-DIGIT      PIC X.
           05 CALIBRATION-VAL PIC 99.
           05 CALIBRATION-SUM PIC 9(9) COMP.

       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X    VALUE 'C'.
              88 PZLINPUT-EOF          VALUE 'E'.
           05 LTR-FND-FLAG    PIC X    VALUE 'N'.
              88 LETTER-FOUND          VALUE 'Y'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           MOVE PZL-REC TO REC
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM CONVERT-LETTERS
             PERFORM GET-FIRST-AND-LAST
             ADD CALIBRATION-VAL TO CALIBRATION-SUM
             PERFORM READ-PZLINPUT
             MOVE PZL-REC TO REC
           END-PERFORM

           CLOSE PZLINPUT

           DISPLAY CALIBRATION-SUM

           GOBACK
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
             CONTINUE
           END-IF
           .

       GET-FIRST-AND-LAST SECTION.
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(PZL-REC))
           MOVE 1 TO CNT
           MOVE SPACES TO FIRST-DIGIT LAST-DIGIT

           *> Find first digit.
           PERFORM UNTIL CNT > LEN OR FIRST-DIGIT IS NOT = SPACE
             IF REC(CNT:1) IS NUMERIC
               MOVE REC(CNT:1) TO FIRST-DIGIT
             END-IF
             ADD 1 TO CNT
           END-PERFORM

           *> Find last digit.
           MOVE LEN TO CNT
           PERFORM UNTIL CNT > LEN OR LAST-DIGIT IS NOT = SPACE
             IF REC(CNT:1) IS NUMERIC
               MOVE REC(CNT:1) TO LAST-DIGIT
             END-IF
             SUBTRACT 1 FROM CNT
           END-PERFORM
           
           MOVE FIRST-DIGIT TO CALIBRATION-VAL(1:1)
           MOVE LAST-DIGIT TO CALIBRATION-VAL(2:1)
           .

       CONVERT-LETTERS SECTION.
           *> This section should really rely on some structure of all
           *> digits spelled, the replace value and its length, then 
           *> loop through the record forwards and backwards and use
           *> the structure to replace stuff using only one inspect 
           *> replacing. I might come back to fix this...
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(REC))

           *> Replace first
           MOVE 1 TO CNT
           MOVE 0 TO TAL
           MOVE 'N' TO LTR-FND-FLAG
           PERFORM UNTIL LETTER-FOUND OR CNT > LEN
             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'one'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'one' BY 'o1e'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'two'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'two' BY 't2o'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'three'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'three' BY 'th3ee'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'four'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'four' BY 'fo4r'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'five'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'five' BY 'fi5e'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'six'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'six' BY 's6x'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'seven'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'seven' BY 'se7en'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'eight'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'eight' BY 'ei8ht'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(1:CNT) TALLYING TAL FOR ALL 'nine'
               IF TAL > 0
                 INSPECT REC(1:CNT) REPLACING ALL 'nine' BY 'n9ne'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             ADD 1 TO CNT
           END-PERFORM

           *> Replace last
           MOVE 1 TO CNT
           MOVE 0 TO TAL
           MOVE 'N' TO LTR-FND-FLAG
           PERFORM UNTIL LETTER-FOUND OR CNT > LEN
             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'one'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'one' BY 'o1e'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'two'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'two' BY 't2o'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'three'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'three' BY 'th3ee'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'four'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'four' BY 'fo4r'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'five'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'five' BY 'fi5e'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'six'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'six' BY 's6x'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'seven'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'seven' BY 'se7en'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'eight'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'eight' BY 'ei8ht'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             IF NOT LETTER-FOUND
               INSPECT REC(LEN - CNT:LEN) TALLYING TAL FOR ALL 'nine'
               IF TAL > 0
                 INSPECT REC(LEN - CNT:LEN)
                 REPLACING ALL 'nine' BY 'n9ne'
                 SET LETTER-FOUND TO TRUE
               END-IF
             END-IF

             ADD 1 TO CNT
           END-PERFORM
           .

