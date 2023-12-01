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
       01  PZL-RECORD              PIC X(128).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 DEPTH-INCR-CNT       PIC 9(4) COMP.
           05 PZL-REC-L            PIC 9(4) COMP.
           05 PZL-REC-C            PIC 9(4) COMP.
           05 PZL-REC-FD           PIC X.
           05 PZL-REC-LD           PIC X.
           05 PZL-PREV-RECORD      PIC X(128).
           05 PZLINPUT-FS          PIC XX.
           05 PZL-REC-NC           PIC XX.
           05 PZL-REC-ND           PIC 99.
           05 CALIBRATION-SUM      PIC 9(9) COMP.
       01  SWITCHES.
           05 PZLINPUT-FLAG        PIC X    VALUE 'C'.
              88 PZLINPUT-EOF               VALUE 'E'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE
                      SWITCHES

           OPEN INPUT PZLINPUT

           PERFORM READ-PZLINPUT
           MOVE PZL-RECORD TO PZL-PREV-RECORD
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM GET-FIRST-AND-LAST
             PERFORM READ-PZLINPUT
               MOVE PZL-RECORD TO PZL-PREV-RECORD
           END-PERFORM

           DISPLAY CALIBRATION-SUM

           CLOSE PZLINPUT

           GOBACK
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-RECORD
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .

       GET-FIRST-AND-LAST SECTION.
           COMPUTE PZL-REC-L = FUNCTION LENGTH(
                               FUNCTION TRIM(PZL-RECORD))
           COMPUTE PZL-REC-C = 1
           MOVE SPACES TO PZL-REC-FD
           MOVE SPACES TO PZL-REC-LD

           *> Find first digit.
           PERFORM UNTIL PZL-REC-C > PZL-REC-L
                      OR PZL-REC-FD IS NOT = SPACES
             IF PZL-RECORD(PZL-REC-C:1) IS NUMERIC
               MOVE PZL-RECORD(PZL-REC-C:1) TO PZL-REC-FD
             END-IF
             ADD 1 TO PZL-REC-C
           END-PERFORM

           *> Find last digit.
           MOVE PZL-REC-L TO PZL-REC-C
           PERFORM UNTIL PZL-REC-C > PZL-REC-L
                      OR PZL-REC-LD IS NOT = SPACES
             IF PZL-RECORD(PZL-REC-C:1) IS NUMERIC
               MOVE PZL-RECORD(PZL-REC-C:1) TO PZL-REC-LD
             END-IF
             SUBTRACT 1 FROM PZL-REC-C
           END-PERFORM
           
           STRING PZL-REC-FD PZL-REC-LD INTO PZL-REC-NC
           MOVE PZL-REC-NC TO PZL-REC-ND

           ADD PZL-REC-ND TO CALIBRATION-SUM
           .