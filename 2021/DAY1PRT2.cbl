       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY1PZL2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO 'PZLINPUT.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-RECORD              PIC X(5).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.
           05 WNDW-INCR-CNT        PIC S9(4) COMP.
           05 PZLINPUT-FS          PIC XX.
           05 PZLINPUT-TBL.
              10 PZL-REC PIC 9(5) OCCURS 2000 TIMES INDEXED IX.
       01  SWITCHES.
           05 PZLINPUT-FLAG        PIC X     VALUE 'C'.
              88 PZLINPUT-EOF                VALUE 'E'.

       PROCEDURE DIVISION.
       
       MAIN SECTION.
           INITIALIZE WORKSPACE
                      SWITCHES

           OPEN INPUT PZLINPUT
           PERFORM INIT-PZLINPUT-TBL
           CLOSE PZLINPUT
           PERFORM CALC-INCR

           DISPLAY WNDW-INCR-CNT
           GOBACK
           .

       INIT-PZLINPUT-TBL SECTION.
           SET IX TO 1
           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             COMPUTE PZL-REC(IX) = FUNCTION NUMVAL(PZL-RECORD)
             SET IX UP BY 1
             PERFORM READ-PZLINPUT
           END-PERFORM
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-RECORD
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .

       CALC-INCR SECTION.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 1997
             IF (PZL-REC(IX) + PZL-REC(IX + 1) + PZL-REC(IX + 2))
              < (PZL-REC(IX + 1) + PZL-REC(IX + 2) +PZL-REC(IX + 3))
               ADD 1 TO WNDW-INCR-CNT
             END-IF
           END-PERFORM
           .