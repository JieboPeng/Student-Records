      ******************************************************************
      * Author:Jiebo Peng(040918844)
      * Date: August 05, 2023
      * Purpose:Use screen for user to search student number in an
      *         indexed sequential student file. If student number match
      *         then display all of this student's info to the screen.
      *         If no match, then display error message. Use the screen
      *         to input information that need to be updated. Accept the
      *         the screen and update the indexed sequential student file.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Project3-p2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INDEXED-STUDENT-FILE
               ASSIGN TO "C:\STUFILE3OUT.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STUDENT-NUMBER
               FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD  INDEXED-STUDENT-FILE.
       01  STUDENT-RECORD-IN.
           05 STUDENT-NUMBER PIC 9(6).
           05 TUITION-OWED PIC 9(4)V99.
           05 STUDENT-NAME PIC X(40).
           05 PROGRAM-CODE   PIC X(5).
           05 COURSE-CODE-1 PIC X(7).
           05 COURSE-AVERAGE-1 PIC 9(3).
           05 COURSE-CODE-2 PIC X(7).
           05 COURSE-AVERAGE-2 PIC 9(3).
           05 COURSE-CODE-3 PIC X(7).
           05 COURSE-AVERAGE-3 PIC 9(3).
           05 COURSE-CODE-4 PIC X(7).
           05 COURSE-AVERAGE-4 PIC 9(3).
           05 COURSE-CODE-5 PIC X(7).
           05 COURSE-AVERAGE-5 PIC 9(3).
       WORKING-STORAGE SECTION.
       01  STUDENT-RECORD-WS.
           05 STUDENT-NUMBER-WS PIC 9(6).
           05 TUITION-OWED-WS PIC 9(4)V99.
           05 STUDENT-NAME-WS PIC X(40).
           05 PROGRAM-CODE-WS PIC X(5).
           05 COURSE-CODE-1-WS PIC X(7).
           05 COURSE-AVERAGE-1-WS PIC 9(3).
           05 COURSE-CODE-2-WS PIC X(7).
           05 COURSE-AVERAGE-2-WS PIC 9(3).
           05 COURSE-CODE-3-WS PIC X(7).
           05 COURSE-AVERAGE-3-WS PIC 9(3).
           05 COURSE-CODE-4-WS PIC X(7).
           05 COURSE-AVERAGE-4-WS PIC 9(3).
           05 COURSE-CODE-5-WS PIC X(7).
           05 COURSE-AVERAGE-5-WS PIC 9(3).
       01  CONTROL-FIELDS.
           05 STATUS-FIELD PIC 9(2).
           05 TUITION-PAYMENT PIC 9(4)V99.
       SCREEN SECTION.
       01  STUDENT-INPUT-SCREEN.
           05 VALUE "STUDENT SEARCH" BLANK SCREEN LINE 1 COL 35.
           05 VALUE "STUDENT NUMBER" LINE 5 COL 5.
           05 STUDENT-NUMBER-INPUT LINE 5 COL 25
               PIC 9(6) TO STUDENT-NUMBER.
       01  STUDENT-OUTPUT-SCREEN.
           05 VALUE "STUDENT RECORD" BLANK SCREEN LINE 1 COL 35.
           05 VALUE "STUDENT NUMBER" LINE 3 COL 5.
           05 STUDENT-NUMBER-OUTPUT LINE 3 COL 25
               PIC 9(6) FROM STUDENT-NUMBER.
           05 VALUE "STUDENT NAME" LINE 5 COL 5.
           05 SUTDENT-NAME-OUTPUT LINE 5 COL 25
                PIC X(48) FROM STUDENT-NAME.
           05 VALUE "PROGRAM" LINE 7 COL 5.
           05 PROGRAM-OUTPUT LINE 7 COL 25
                PIC X(5) FROM PROGRAM-CODE.
           05 VALUE "TUITION OWED" LINE 9 COL 5.
           05 TUITION-OWED-OUTPUT LINE 9 COL 25
               PIC $9999.99 FROM TUITION-OWED.
           05 VALUE "PAYMENT" LINE 11 COL 5.
           05 PAYMENT-OUTPUT LINE 11 COL 25
               PIC $9999.99 TO TUITION-PAYMENT.

       PROCEDURE DIVISION.
       100-UPDATE-STUDENT-FILE.
           PERFORM 201-INITIANIZE.
           PERFORM 202-UPDATE-FILE.
           PERFORM 203-TERMINATE-PROGRAM.
           STOP RUN.
       201-INITIANIZE.
           PERFORM 301-OPEN-FILE.
           PERFORM 302-PROMPT-SEARCH-SCREEN.
       202-UPDATE-FILE.
           PERFORM 303-READ-FILE.
           PERFORM 304-UPDATE-STUDENT-RECORD.
       203-TERMINATE-PROGRAM.
           CLOSE INDEXED-STUDENT-FILE.

       301-OPEN-FILE.
           OPEN I-O INDEXED-STUDENT-FILE.
      * show a scrren for user to input.
       302-PROMPT-SEARCH-SCREEN.
           DISPLAY STUDENT-INPUT-SCREEN.
           ACCEPT STUDENT-INPUT-SCREEN.
       303-READ-FILE.
           READ INDEXED-STUDENT-FILE
              INVALID KEY
                 DISPLAY "STUDENT RECORD IS NOT FOUND:" STUDENT-NUMBER
              NOT INVALID KEY
                 DISPLAY STUDENT-OUTPUT-SCREEN.
                 ACCEPT STUDENT-OUTPUT-SCREEN.
       304-UPDATE-STUDENT-RECORD.
           MOVE STUDENT-RECORD-IN TO STUDENT-RECORD-WS.
           PERFORM 401-COMPUTE-TUITION-OWED.
           PERFORM 402-REWRITE-STUDENT-RECORD.

       401-COMPUTE-TUITION-OWED.
           COMPUTE TUITION-OWED-WS = TUITION-OWED-WS - TUITION-PAYMENT.
       402-REWRITE-STUDENT-RECORD.
           REWRITE STUDENT-RECORD-IN FROM STUDENT-RECORD-WS
               INVALID KEY
                  DISPLAY "STUDENT RECORD IS NOT FOUND:" STUDENT-NUMBER
               NOT INVALID KEY
                  DISPLAY "RECORD SAVED!".

       END PROGRAM Project3-p2.
