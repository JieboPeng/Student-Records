      ******************************************************************
      * Author:Jiebo Peng(040918844)
      * Date: August 05, 2023
      * PurpoJulyse:Read student records and courses from external fiels,
      * Then output the records to a file and audit the records from
      * read and write.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-P3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROGRAM-FILE-IN ASSIGN TO "C:\PROGRAM.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT STUDENT-FILE-IN ASSIGN TO "C:\STUFILE3OUT.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS STUDENT-NUMBER.

           SELECT STUDENT-REPORT-OUT ASSIGN TO "C:\STU-REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE-IN.
       01 STUDENT-RECORD-IN.
           05 STUDENT-NUMBER PIC 9(6).
           05 TUITION-OWED PIC 9(4)V99.
           05 STUDENT-NAME PIC X(40).
           05 PROGRAM-OF-STUDY PIC X(5).
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

       FD PROGRAM-FILE-IN.
       01 PROGRAM-RECORD.
           05 PROGRAM-CODE-IN PIC X(5).
           05 PROGRAM-X PIC X(1).
           05 PROGRAM-NAME-IN PIC X(20).


       FD STUDENT-REPORT-OUT.
       01 STUDNT-REPORT-RECORD-OUT PIC X(90).

       WORKING-STORAGE SECTION.
       01 STUDENT-REPROT-RECORD.
           05 STUDENT-NAME-OUT PIC X(40).
           05 FILLER PIC X(3) VALUE SPACES.
           05 STUDENT-AVERAGE-OUT PIC 9(3).
           05 FILLER PIC X(4) VALUE SPACES.
           05 PROGRAM-NAME-OUT PIC X(20).
           05 FILLER PIC X(7) VALUE SPACES.
           05 TUITION-OWED-OUT PIC Z,ZZ9.99.

       01 REPORT-HEADER.
           05 FILLER PIC X(40) VALUE "NAME".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "AVERAGE".
           05 FILLER PIC X(4) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "PROGRAM".
           05 FILLER PIC X(4) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "TUITION".

       01 HEADER-LINE.
           05 FILLER PIC X(90) VALUE ALL "-".

       01 CONTROL-FIELDS.
           05 EOF-FLAG PIC A(1).
           05 EOF-FLAG-PROG PIC A(1).
           05 READ-COUNTER PIC 9(3).
           05 WRITE-COUNTER PIC 9(3).
           05 SUB-1 PIC 9(2).
           05 FOUND-FLAG PIC A(1).
      * Copy the file which contain the structure of the program table
       COPY "./PROGRAM-TBL.DAT".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       100-PRODUCE-STUDENT-REPORT.
           PERFORM 201-INITIALIZE.
           PERFORM 202-CREAT-REPORT-RECORDS
                   UNTIL EOF-FLAG = "Y" OR "y".
           PERFORM 203-TERMINATE.
           STOP RUN.

       201-INITIALIZE.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-LOAD-PROGRAM-TABLE VARYING
                  SUB-1 FROM 1 BY 1 UNTIL SUB-1 > 20 OR
                  EOF-FLAG-PROG = "Y" OR "y".
           PERFORM 303-READ-A-STUDENT-RECORD.
           PERFORM 304-WRITE-REPORT-HEADER.

       202-CREAT-REPORT-RECORDS.
           MOVE "N" TO FOUND-FLAG.
           PERFORM 305-SEARCH-PROGRAM-TABLE VARYING
                  SUB-1 FROM 1 BY 1 UNTIL SUB-1 > 20 OR
                  FOUND-FLAG = "Y" OR "y".
           PERFORM 306-CALCULATE-COURSES-AVERAGE.
           PERFORM 307-WRITE-A-REPORT-RECORD.
           PERFORM 303-READ-A-STUDENT-RECORD.

       203-TERMINATE.
           PERFORM 308-DISPLAY-AUDIT.
           PERFORM 309-CLOSE-FILES.

       301-OPEN-FILES.
           OPEN INPUT STUDENT-FILE-IN PROGRAM-FILE-IN.
           OPEN OUTPUT STUDENT-REPORT-OUT.

       302-LOAD-PROGRAM-TABLE.
           READ PROGRAM-FILE-IN AT END MOVE "Y" TO EOF-FLAG-PROG
              NOT AT END
                 MOVE PROGRAM-CODE-IN TO PROGRAM-CODE-TBL(SUB-1)
                 MOVE PROGRAM-NAME-IN TO PROGRAM-NAME-TBL(SUB-1).

       303-READ-A-STUDENT-RECORD.
           READ STUDENT-FILE-IN AT END MOVE "Y" TO EOF-FLAG
              NOT AT END ADD 1 TO READ-COUNTER.

       304-WRITE-REPORT-HEADER.
           WRITE STUDNT-REPORT-RECORD-OUT FROM REPORT-HEADER.
           WRITE STUDNT-REPORT-RECORD-OUT FROM HEADER-LINE.

       305-SEARCH-PROGRAM-TABLE.
           MOVE "N" TO FOUND-FLAG.
               IF PROGRAM-OF-STUDY EQUAL PROGRAM-CODE-TBL(SUB-1)
                   MOVE PROGRAM-NAME-TBL(SUB-1) TO PROGRAM-NAME-OUT
                   MOVE "Y" TO FOUND-FLAG
               END-IF.

       306-CALCULATE-COURSES-AVERAGE.
      *  Call an external function that calculate the average of five
      *  courses.
           CALL "./Project3_P3_Sub"
           USING STUDENT-AVERAGE-OUT,
               COURSE-AVERAGE-1,COURSE-AVERAGE-2,COURSE-AVERAGE-3,
               COURSE-AVERAGE-4,COURSE-AVERAGE-5.

       307-WRITE-A-REPORT-RECORD.
           MOVE STUDENT-NAME TO STUDENT-NAME-OUT.
           MOVE TUITION-OWED TO TUITION-OWED-OUT.
           WRITE STUDNT-REPORT-RECORD-OUT
                 FROM STUDENT-REPROT-RECORD.
                 ADD 1 TO WRITE-COUNTER.

       308-DISPLAY-AUDIT.
           DISPLAY "THE NUMBER OF STUDENT RECORDS READ: " READ-COUNTER.
           DISPLAY "THE NUMBER OF STUDENT RECORDS WRITTEN: "
                WRITE-COUNTER.

       309-CLOSE-FILES.
           CLOSE STUDENT-FILE-IN.
           CLOSE PROGRAM-FILE-IN.
           CLOSE STUDENT-REPORT-OUT.

       END PROGRAM PROJECT3-P3.
