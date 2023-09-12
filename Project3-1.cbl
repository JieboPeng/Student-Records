      ******************************************************************
      * Author:Jiebo Peng(040918844)
      * Date: August 05, 2023
      * Purpose:Convert sequential student file into indexed sequential student file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-IN ASSIGN TO "C:\STUFILE3.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-FILE-OUT ASSIGN TO "C:\STUFILE3OUT.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS STUDENT-NUMBER-OUT
               FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE-IN.
           01 STUDENT-RECORD-IN.
               05 STUDENT-NUMBER PIC 9(6).
               05 TUITION-OWED PIC 9(4)99.
               05 STUDENT-NAME PIC X(40).
               05 PROGRAM-OF-STUDY PIC X(5).
               05 COURSE-CODE1 PIC X(7).
               05 COURSE-AVERAGE1 PIC 9(3).
               05 COURSE-CODE2 PIC X(7).
               05 COURSE-AVERAGE2 PIC 9(3).
               05 COURSE-CODE3 PIC X(7).
               05 COURSE-AVERAGE3 PIC 9(3).
               05 COURSE-CODE4 PIC X(7).
               05 COURSE-AVERAGE4 PIC 9(3).
               05 COURSE-CODE5 PIC X(7).
               05 COURSE-AVERAGE5 PIC 9(3).

       FD STUDENT-FILE-OUT.
           01 STUDENT-RECORD-OUT.
               05 STUDENT-NUMBER-OUT PIC 9(6).
               05 TUITION-OWED-OUT PIC 9(4)99.
               05 STUDENT-NAME-OUT PIC X(40).
               05 PROGRAM-OF-STUDY-OUT PIC X(5).
               05 COURSE-CODE1-OUT PIC X(7).
               05 COURSE-AVERAGE1-OUT PIC 9(3).
               05 COURSE-CODE2-OUT PIC X(7).
               05 COURSE-AVERAGE2-OUT PIC 9(3).
               05 COURSE-CODE3-OUT PIC X(7).
               05 COURSE-AVERAGE3-OUT PIC 9(3).
               05 COURSE-CODE4-OUT PIC X(7).
               05 COURSE-AVERAGE4-OUT PIC 9(3).
               05 COURSE-CODE5-OUT PIC X(7).
               05 COURSE-AVERAGE5-OUT PIC 9(3).


       WORKING-STORAGE SECTION.
           01 EOF-FLAG PIC X(3).
           01 STATUS-FIELD PIC X(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       100-CONVERT-TO-INDEXFILE.
           PERFORM 201-INITIAL-FILE.
           PERFORM 202-CREAT-FILE-OUT UNTIL EOF-FLAG = "YES".
           PERFORM 203-TERMINAL-FILE
           STOP RUN.

       201-INITIAL-FILE.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-READ-FILE.

       301-OPEN-FILES.
           OPEN INPUT STUDENT-FILE-IN.
           OPEN OUTPUT STUDENT-FILE-OUT.

       302-READ-FILE.
           READ STUDENT-FILE-IN AT END MOVE "YES" TO EOF-FLAG
               NOT AT END
               MOVE STUDENT-RECORD-IN TO STUDENT-RECORD-OUT.

       202-CREAT-FILE-OUT.
           PERFORM 303-WRITE-FILE-OUT.
           PERFORM 302-READ-FILE.

       303-WRITE-FILE-OUT.
           WRITE STUDENT-RECORD-OUT
           INVALID KEY
               DISPLAY "INVALID KEY"
           NOT INVALID KEY
               DISPLAY "RECORD FOUND"
           END-WRITE.

       203-TERMINAL-FILE.
           CLOSE STUDENT-FILE-IN.
           CLOSE STUDENT-FILE-OUT.

       END PROGRAM PROJECT3-1.
