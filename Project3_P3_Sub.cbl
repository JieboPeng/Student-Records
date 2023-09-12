      ******************************************************************
      * Author:Jiebo Peng(040918844)
      * Date: August 05, 2023
      * Purpose:Calculate the average grade of five courses.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Project3_P3_Sub.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AVERAGE-GRADE PIC 9(3).
       01  LS-COURSE-AVERAGE-1 PIC 9(3).
       01  LS-COURSE-AVERAGE-2 PIC 9(3).
       01  LS-COURSE-AVERAGE-3 PIC 9(3).
       01  LS-COURSE-AVERAGE-4 PIC 9(3).
       01  LS-COURSE-AVERAGE-5 PIC 9(3).

       PROCEDURE DIVISION USING
           LS-AVERAGE-GRADE,LS-COURSE-AVERAGE-1,LS-COURSE-AVERAGE-2,
           LS-COURSE-AVERAGE-3,LS-COURSE-AVERAGE-4,LS-COURSE-AVERAGE-5.

           COMPUTE LS-AVERAGE-GRADE ROUNDED= (LS-COURSE-AVERAGE-1
                   + LS-COURSE-AVERAGE-2 + LS-COURSE-AVERAGE-3
                   + LS-COURSE-AVERAGE-4 + LS-COURSE-AVERAGE-5) / 5.

           GOBACK.
       END PROGRAM Project3_P3_Sub.
