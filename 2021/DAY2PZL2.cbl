       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY2PZL2.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PZLINPUT ASSIGN TO 'PZLINPUT.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PZLINPUT-FS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  PZLINPUT.
       01  PZL-RECORD              PIC X(15).

       WORKING-STORAGE SECTION.
       01  WORKSPACE.                             
           05 PZLINPUT-FS          PIC XX.
           05 CMD                  PIC X(7).
           05 NUM                  PIC 9(5).
           05 POS-X                PIC 9(5).
           05 NUM-STR              PIC X(5).
           05 AIM                  PIC 9(5).
           05 DEPTH                PIC 9(6).
           05 PRDCT                PIC 9(11).
       01  SWITCHES.
           05 PZLINPUT-FLAG        PIC X     VALUE 'C'.
              88 PZLINPUT-EOF                VALUE 'E'.

       PROCEDURE DIVISION.

       MAIN SECTION.
           INITIALIZE WORKSPACE
                      SWITCHES

           OPEN INPUT PZLINPUT
           PERFORM READ-PZLINPUT
           PERFORM UNTIL PZLINPUT-EOF
             PERFORM UNPACK-PZLINPUT
             PERFORM PROCESS-PZLINPUT
           END-PERFORM
           CLOSE PZLINPUT

           MULTIPLY POS-X BY DEPTH GIVING PRDCT
           DISPLAY PRDCT
           GOBACK
           .

       PROCESS-PZLINPUT SECTION.
           COMPUTE NUM = FUNCTION NUMVAL(NUM-STR)
           EVALUATE CMD
             WHEN 'forward'
               ADD NUM TO POS-X
               COMPUTE DEPTH = DEPTH + AIM * NUM
             WHEN 'down'
               ADD NUM TO AIM
             WHEN 'up'
               SUBTRACT NUM FROM AIM
           END-EVALUATE
           PERFORM READ-PZLINPUT
           .

       UNPACK-PZLINPUT SECTION.
           UNSTRING PZL-RECORD DELIMITED BY ALL spaces
               INTO CMD
                    NUM-STR
           .

       READ-PZLINPUT SECTION.
           READ PZLINPUT INTO PZL-RECORD
           IF PZLINPUT-FS NOT = '00'
             SET PZLINPUT-EOF TO TRUE
           END-IF
           .