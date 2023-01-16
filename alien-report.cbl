      ******************************************************************
      * Author: Jordan
      * Date: 12/15/2022
      * Purpose: Learning purpose
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALIEN-SIGHTINGS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT WorkFile ASSIGN TO WorkData.

          SELECT UFODATA ASSIGN TO
          "C:\Users\jorda\Documents\Datasets\archive\complete-csv.csv"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT UFODATA-OUT-Fixed ASSIGN TO
          "C:\Users\jorda\Documents\Datasets\archive\tempfile.csv"
          ORGANIZATION IS LINE SEQUENTIAL.
          SELECT UFODATA-IN-Fixed ASSIGN TO
          "C:\Users\jorda\Documents\Datasets\archive\tempfile.csv"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT SortedFile ASSIGN TO
           "C:\Users\jorda\Documents\Datasets\COBOL-REPORTS\
      -     "ALIEN-REPORT.csv"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT Sorted-Data-Out ASSIGN TO
          "C:\Users\jorda\Documents\Datasets\COBOL-REPORTS\outfile.csv"
          ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       SD WorkFile.
       01 WorkData.
          02 WD-DateTime PIC X(20).
          02 WD-Location.
             03 WD-City PIC X(30) VALUE SPACES.
             03 WD-State PIC XX VALUE SPACES.
             03 WD-Country PIC XX VALUE SPACES.
             03 WD-Shape PIC X(15) VALUE SPACES.
             03 WD-Duration PIC X(10) VALUE ZEROS.

       FD UFODATA.
       01 InputBuffer PIC X(300) VALUE SPACES.

       FD UFODATA-IN-Fixed.
       01 UFO-IN.
          02 UFO-IN-DateTime PIC X(20).
          02 UFO-IN-Location.
             03 UFO-IN-City PIC X(30) VALUE SPACES.
             03 UFO-IN-State PIC XX VALUE SPACES.
             03 UFO-IN-Country PIC XX VALUE SPACES.
             03 UFO-IN-Shape PIC X(15) VALUE SPACES.
             03 UFO-IN-Duration PIC X(10) VALUE ZEROS.

       FD UFODATA-OUT-Fixed.
       01 PrintLine PIC X(300) VALUE SPACES.

       FD SortedFile.
       01 SortedUFO-Data.
          02 Sorted-DateTime PIC X(20).
          02 Sorted-Location.
             03 Sorted-City PIC X(30) VALUE SPACES.
             03 Sorted-State PIC XX VALUE SPACES.
             03 Sorted-Country PIC XX VALUE SPACES.
             03 Sorted-Shape PIC X(15) VALUE SPACES.
             03 Sorted-Duration PIC X(10) VALUE ZEROS.
          02 Sorted-DateTime PIC X(20).

       WORKING-STORAGE SECTION.
       01 Prn-Data.
          02 Prn-DateTime PIC X(20).
          02 Prn-Location.
             03 Prn-City PIC X(30)BB VALUE SPACES.
             03 Prn-State PIC XXBB VALUE SPACES.
             03 Prn-Country PIC XXBB VALUE SPACES.
             03 Prn-Shape PIC X(15)BB VALUE SPACES.
             03 Prn-Duration PIC X(10) VALUE ZEROS.
       01 BOB.
          88 bob-EOF VALUE HIGH-VALUE.
          02 bob-2 PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "******STARTING PARSE AND SORT*****"
            PERFORM ParseCSV THRU SortCSVFile
            STOP RUN.

       ParseCSV.
            OPEN INPUT UFODATA
            OPEN OUTPUT UFODATA-OUT-Fixed
            READ UFODATA
             AT END SET bob-EOF TO TRUE
            END-READ
      *>       MOVE SPACES TO InputBuffer
            PERFORM UNTIL bob-EOF
             UNSTRING InputBuffer DELIMITED BY ","
                INTO Prn-DateTime, Prn-City, Prn-State, Prn-Country,
                Prn-Shape, Prn-Duration
             END-UNSTRING
             WRITE PrintLine FROM Prn-Data AFTER ADVANCING 1 LINE
             READ UFODATA
                AT END SET bob-EOF TO TRUE
             END-READ
            END-PERFORM
            CLOSE UFODATA-OUT-Fixed
            CLOSE UFODATA.

       SortCSVFile.
            SORT WorkFile ON ASCENDING KEY WD-Country
             ON ASCENDING KEY WD-State
             ON ASCENDING KEY WD-City
             WITH DUPLICATES IN ORDER
             USING UFODATA-IN-Fixed GIVING SortedFile.

       END PROGRAM ALIEN-SIGHTINGS.
