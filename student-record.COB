      ******************************************************************
      * Author:Jiebo Peng
      * Date:June 1, 2023
      * Purpose:Project 1
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-RECORDS-PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-RECORDS-OUT
               ASSIGN TO "C:\STUDENT-RECORD.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-RECORDS-OUT.
       01 STUDENT-RECORDS.
           05 STUDENT-NUMBER               PIC 9(7).
           05 TUITION-OWED                 PIC 9(5).
           05 STUDENT-NAME                PIC X(40).

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
              05 PTOMPT-FLG PIC X.

       PROCEDURE DIVISION.

       101-PRODUCE-STUDENT-RECORDS.
           PERFORM 201-INITIATE-STUDENT-RECORDS.
           PERFORM 202-PRODUCE-STUDENT-RECORDS
               UNTIL PTOMPT-FLG EQUALS 'N' OR 'n'.
           PERFORM 203-CLOSE-STUDENT-RECORDS.
           STOP RUN.

       201-INITIATE-STUDENT-RECORDS.
           PERFORM 301-OPEN-STUDENT-RECORDS-FILE.
           PERFORM 302-GET-CONTINUE-FLAG.

       202-PRODUCE-STUDENT-RECORDS.
           PERFORM 303-GET-STUDENT-NUMBER.
           PERFORM 304-GET-TUITION-OWED.
           PERFORM 305-GET-STUDENT-NAME.
           PERFORM 306-WRITE-STUDENT-RECORDS.
           PERFORM 302-GET-CONTINUE-FLAG.

       203-CLOSE-STUDENT-RECORDS.
           CLOSE STUDENT-RECORDS-OUT.

       301-OPEN-STUDENT-RECORDS-FILE.
           OPEN OUTPUT STUDENT-RECORDS-OUT.

       302-GET-CONTINUE-FLAG.
           DISPLAY'DO YOU WANT TO GENERATE A RECORD?'
           ACCEPT PTOMPT-FLG.

       303-GET-STUDENT-NUMBER.
           DISPLAY "PLEASE ENTER THE STUDENT-NUMBER".
           ACCEPT STUDENT-NUMBER.

       304-GET-TUITION-OWED.
           DISPLAY "PLEASE ENTER THE TUITION-OWED".
           ACCEPT TUITION-OWED.

       305-GET-STUDENT-NAME.
           DISPLAY "PLEASE ENTER THE STUDENT-NAME".
           ACCEPT STUDENT-NAME.

       306-WRITE-STUDENT-RECORDS.
           WRITE STUDENT-RECORDS.


       END PROGRAM STUDENT-RECORDS-PROGRAM.
