       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX2.
       AUTHOR."xxx"
       DATE-WRITTEN."27/09/18".
       DATE-COMPILED."27/09/18"
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.M8.
       OBJECT-COMPUTER.M8.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT STUD1 ASSIGN TO DISK
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL.
       SELECT STUD2 ASSIGN TO DISK
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL.
       SELECT OUT ASSIGN TO DISK
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL.
       SELECT WORK ASSIGN TO DISK
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD STUD1
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "STUD1.DAT".
       01 STUD1-REC.
       02 R-NO1 PIC 99.
       02 NAME1 PIC X(10).
       02 ATT1 PIC 99.
       02 PER1 PIC 99.
       02 OVERALL1 PIC 999.
       FD STUD2
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "STUD2.DAT".
       01 STUD2-REC.
       02 R-NO2 PIC 99.
       02 NAME2 PIC X(10).
       02 ATT2 PIC 99.
       02 PER2 PIC 99.
       02 OVERALL2 PIC 999.
       FD OUT
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "MERGE.DAT".
       01 OUT-REC.
       02 R-NO3 PIC 99.
       02 NAME3 PIC X(10).
       02 ATT3 PIC 99.
       02 PER3 PIC 99.
       02 OVERALL3 PIC 999.
       SD WORK.
       01 WORK-REC.
       02 R-NO4 PIC 99.
       02 NAME4 PIC X(10).
       02 ATT4 PIC 99.
       02 PER4 PIC 99.
       02 OVERALL4 PIC 999.
       WORKING-STORAGE SECTION.
       77 N PIC X.
       PROCEDURE DIVISION.
       OPEN OUTPUT STUD1,STUD2.
       PARA-1.
       DISPLAY "---FOR FILE1---".
       DISPLAY "Enter roll no".
       ACCEPT R-NO1.
       DISPLAY "enter name".
       ACCEPT NAME1.
       DISPLAY "Enter Mark1".
       ACCEPT ATT1.
       DISPLAY "Enter Mark2".
       ACCEPT PER1.
       COMPUTE OVERALL1=ATT1+PER1;
       WRITE STUD1-REC.
       DISPLAY "Do u want to continue... PRESS x".
       ACCEPT N.
       IF N = "Y"
       GO TO PARA-1.
       PARA-2.
       DISPLAY "---FOR FILE2---".
       DISPLAY "Enter roll no".
       ACCEPT R-NO2.
       DISPLAY "Enter name".
       ACCEPT NAME2.
       DISPLAY "Enter Mark1".
       ACCEPT ATT2.
       DISPLAY "Enter Mark2".
       ACCEPT PER2.
       COMPUTE OVERALL2=ATT2+PER2;
       WRITE STUD2-REC.
       DISPLAY "TO CONTINUE...PRESS x".
       ACCEPT N.
       IF N="Y"
       GO TO PARA-2.
       END-PARA.
       CLOSE STUD1,STUD2.
       MERGE-PARA.
       MERGE WORK ON ASCENDING KEY R-NO4 USING STUD1,STUD2
       GIVING OUT.
       OPEN INPUT OUT.
       DISPLAY "------------------------------------".
       DISPLAY "ROLL NAME MARK1 MARK2 OVERALL".
       DISPLAY "------------------------------------".
       READ-PARA.
       READ OUT AT END GO TO LAST-PARA CLOSE OUT.
       DISPLAY R-NO3 " "NAME3" "ATT3" "PER3" "OVERALL3.
       GO TO READ-PARA.
       LAST-PARA.
       STOP RUN.
