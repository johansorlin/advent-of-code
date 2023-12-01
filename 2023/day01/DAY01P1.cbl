       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01P1.

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
           05 LEN             PIC 9(4) COMP.
           05 CNT             PIC 9(4) COMP.
           05 FIRST-DIGIT     PIC X.
           05 LAST-DIGIT      PIC X.
           05 CALIBRATION-VAL PIC 99.
           05 CALIBRATION-SUM PIC 9(9) COMP.
       01  SWITCHES.
           05 PZLINPUT-FLAG   PIC X    VALUE 'C'.
              88 PZLINPUT-EOF          VALUE 'E'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE SWITCHES

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM GET-FIRST-AND-LAST
             ADD CALIBRATION-VAL TO CALIBRATION-SUM
             PERFORM READ-PZLINPUT
           END-PERFORM

           DISPLAY CALIBRATION-SUM

           CLOSE PZLINPUT

           GOBACK
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-REC
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .

       GET-FIRST-AND-LAST SECTION.
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(PZL-REC))
           MOVE 1 TO CNT
           MOVE SPACES TO FIRST-DIGIT LAST-DIGIT

           *> Find first digit.
           PERFORM UNTIL CNT > LEN OR FIRST-DIGIT IS NOT = SPACE
             IF PZL-REC(CNT:1) IS NUMERIC
               MOVE PZL-REC(CNT:1) TO FIRST-DIGIT
             END-IF
             ADD 1 TO CNT
           END-PERFORM

           *> Find last digit.
           MOVE LEN TO CNT
           PERFORM UNTIL CNT > LEN OR LAST-DIGIT IS NOT = SPACE
             IF PZL-REC(CNT:1) IS NUMERIC
               MOVE PZL-REC(CNT:1) TO LAST-DIGIT
             END-IF
             SUBTRACT 1 FROM CNT
           END-PERFORM
           
           MOVE FIRST-DIGIT TO CALIBRATION-VAL(1:1)
           MOVE LAST-DIGIT TO CALIBRATION-VAL(2:1)
           .