       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ex4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT StudentFile ASSIGN TO DISK
       ORGANIZATION IS RELATIVE.
       DATA DIVISION.
           FILE SECTION.
       FD StudentFile
       LABEL RECORDS ARE STANDARD
       DATA RECORD IS StudentRec
       VALUE OF FILE-ID IS "TEST1.txt".
       01 StudentRec.
       02 StudentId PIC 9(7).
       02 StudentName.
       03 Surname PIC X(8).
       03 Initials PIC XX.
       02 Department PIC X(5).
       02 mailid PIC X(30).
       02 mailid2 PIC X(30).
       WORKING-STORAGE SECTION.
       77 N PIC 99.
       77 C PIC 99 VALUE ZERO.
       77 I PIC 99 VALUE ZERO.
       77 sfname PIC X(8).
       77 slname PIC X(8).
       77 sname PIC X(30).
       77 sdept PIC X(3).
       77 smail PIC X(30).
       77 cop PIC X(30).
       77 destr PIC X(90).
       77 fnd PIC X(50).
       77 rpl PIC X(50).
       77 ovrlap PIC 9 VALUE ZERO.
       77 uname PIC X(30).
       77 len PIC 99 VALUE ZEROS.
       01 st PIC X(1).
       01 en PIC X(1).

       PROCEDURE DIVISION.

       Begin.
       DISPLAY "Enter total number of records in first file:".
       ACCEPT N.
       OPEN OUTPUT StudentFile.
       PERFORM GetStudentRecord N TIMES.
       CLOSE StudentFile.
       DISPLAY "Records are successfully written".
       PERFORM FindReplaceAll.
       Stop RUN.
       FRFile.
       IF ovrlap=0
       OPEN I-O StudentFile.
       DISPLAY " ".
       ADD 1 ovrlap GIVING ovrlap.
       PERFORM PutStudentRecord.

       GetStudentRecord.
       DISPLAY "Enter student details:".
       DISPLAY "first name,last name,department,mailid".
       ACCEPT sfname.
       ACCEPT slname.

       STRING sfname DELIMITED BY SPACES
       ' 'DELIMITED BY SIZE
       INTO sname.
       STRING sname DELIMITED BY SPACES
       INTO sname.

       MOVE sname TO StudentName OF StudentFile.
       ACCEPT Department OF StudentFile.
       ACCEPT mailid OF StudentFile.
       WRITE StudentRec.
       MOVE mailid to cop.

       PutStudentRecord.
       READ StudentFile RECORD AT END GO TO EndOperation.
       PERFORM IntoAnotherFile.
       GO TO PutStudentRecord.

       IntoAnotherFile.

       IF I<C
       DISPLAY " ".

       MOVE mailid OF StudentFile TO smail.

       INSPECT smail REPLACING ALL 'gmail' BY 'ymail'.
       MOVE smail TO mailid OF StudentFile.
       MOVE StudentName OF StudentFile TO sname.
       MOVE Department OF StudentFile TO sdept.
       REWRITE StudentRec.

       UNSTRING smail DELIMITED BY '@'
       INTO uname

       INSPECT smail TALLYING len FOR CHARACTERS BEFORE INITIAL ' '.
       DISPLAY "First name : " sfname.
       DISPLAY "Last name :"slname.
       DISPLAY "Name(CONCATENAE):" sname.
       DISPLAY "Department :" sdept.
       DISPLAY "Mail-ID :" smail.
       DISPLAY "Username :" uname.
       DISPLAY "Mail ID Length :" len.
       DISPLAY "Copied MAilid :" cop.
       DISPLAY "repalced mailid string values".
       ACCEPT st.
       ACCEPT en.
       DISPLAY "OLD STRING BEFORE REPLACING:" mailid.
       INSPECT mailid REPLACING ALL st BY en.
       DISPLAY "NEW STRING AFTER REPLACING :"mailid.

       DISPLAY " ".
       ADD 1 I GIVING I.

       EndOperation.
       DISPLAY " ".
       DISPLAY "Total no of records in file:"N.
       DISPLAY " ".
       CLOSE StudentFile.

       FindReplaceAll.
       DISPLAY " ".
       IF ovrlap=0
       GO TO FRFile.
