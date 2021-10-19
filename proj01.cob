       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ01.
       AUTHOR. Addyson Sisemore
      * PROJECT  1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'COB1-EMPLOYEE'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(100).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(125).
       WORKING-STORAGE SECTION.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 INPUT-DATA.
         03 I-EID        PIC X(7).
         03 I-LAST       PIC X(15).
         03 I-FIRST      PIC X(15).
         03 I-TYPE       PIC X(2).
         03 I-TITLE      PIC X(17).
         03 I-SSN-FIRST  PIC X(3).
         03 I-SSN-MID    PIC X(2).
         03 I-SSN-LAST   PIC X(4).
         03 FILLER       PIC X(24)   VALUE '.'.
         03 I-DATE-M     PIC X(2).
         03 I-DATE-D     PIC X(2).
         03 I-DATE-Y     PIC X(4).
         03 FILLER       PIC X(3)    VALUE SPACES.
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER        PIC X(2)     VALUE SPACES.
         03 L-SSN-FIRST1  PIC X(3).
         03 FILLER        PIC X(1)     VALUE '-'.
         03 L-SSN-MID1    PIC X(2).
         03 FILLER        PIC X(1)     VALUE '-'.
         03 L-SSN-LAST1   PIC X(4).
         03 FILLER        PIC X(2)     VALUE SPACES.
         03 L-EID1        PIC X(7).
         03 FILLER        PIC X(2)     VALUE SPACES.
         03 L-LAST1       PIC X(15).
         03 FILLER        PIC X(1)     VALUE SPACES.
         03 L-FIRST1      PIC X(15).
         03 FILLER        PIC X(10).
         03 L-TITLE1      PIC X(17).
         03 FILLER        PIC X(5)     VALUE SPACES.
         03 L-TYPE1       PIC X(2).
         03 FILLER        PIC X(5)     VALUE SPACES.
         03 L-DATE-M1     PIC X(2).
         03 FILLER        PIC X(1)     VALUE '/'.
         03 L-DATE-D1     PIC X(2).
         03 FILLER        PIC X(1)     VALUE '/'.
         03 L-DATE-Y1     PIC X(4).
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 FILLER      PIC X(3)    VALUE 'SSN'.
         03 FILLER      PIC X(10)   VALUE SPACES.
         03 FILLER      PIC X(6)    VALUE 'EMP ID'.
         03 FILLER      PIC X(3)    VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'LAST'.
         03 FILLER      PIC X(12)   VALUE SPACES.
         03 FILLER      PIC X(5)    VALUE 'FIRST'.
         03 FILLER      PIC X(20)   VALUE SPACES.
         03 FILLER      PIC X(5)    VALUE 'TITLE'.
         03 FILLER      PIC X(17)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'TYPE'.
         03 FILLER      PIC X(3)    VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'DATE'.
         03 FILLER      PIC X(4)    VALUE SPACES.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
             OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEAD.
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING PAGE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.
       1500-LOOP.
           PERFORM 1600-PRINT-DATA.
           PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-DATA.
           MOVE I-SSN-FIRST     TO L-SSN-FIRST1.
           MOVE I-SSN-MID       TO L-SSN-MID1.
           MOVE I-SSN-LAST      TO L-SSN-LAST1.
           MOVE I-EID           TO L-EID1.
           MOVE I-LAST          TO L-LAST1.
           MOVE I-FIRST         TO L-FIRST1.
           MOVE I-TITLE         TO L-TITLE1.
           MOVE I-TYPE          TO L-TYPE1.
           MOVE I-DATE-M        TO L-DATE-M1.
           MOVE I-DATE-D        TO L-DATE-D1.
           MOVE I-DATE-Y        TO L-DATE-Y1.
             WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
             AT END MOVE 1 TO EOF-I.                                        
