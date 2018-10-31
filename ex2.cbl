       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sequencefile.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT StudentFile ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD StudentFile
       LABEL RECORDS ARE STANDARD
       DATA RECORD IS StudentRec
       VALUE OF FILE-ID IS "C:\Users\student\TEXT.txt".
       01 StudentRec.
       02 StudentId PIC 9(7).
       02 StudentName.
       03 Surname PIC X(8).
       03 Initials PIC XX.
       02 Department PIC X(5).
       02 mailid PIC X(30).

       WORKING-STORAGE SECTION.
       77 N PIC 99.
       77 C PIC 99 VALUE ZERO.
       77 I PIC 99 VALUE ZERO.
       77 sfname PIC X(3000).
       77 slname PIC X(3000).
       77 sname PIC X(6000).
       77 sdept PIC X(3000).
       77 smail PIC X(3000).
       77 destr PIC X(9000).
       77 fnd PIC X(5000).
       77 rpl PIC X(5000).
       77 ovrlap PIC 9 VALUE ZERO.
       77 uname PIC X(30000).
       77 len PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.
       Begin.
       DISPLAY "Enter total number of records in first file:".
       ACCEPT N.
       OPEN OUTPUT StudentFile.
       PERFORM GetStudentRecord N TIMES.
       DISPLAY " ".
       CLOSE StudentFile.
       DISPLAY "Records are Succesfully Written".
       PERFORM FindReplaceAll.
       STOP RUN.


       FRFile.
       IF ovrlap=0
       OPEN I-O StudentFile.
       DISPLAY " ".
       ADD 1 ovrlap GIVING ovrlap.
       PERFORM PutStudentRecord.


       GetStudentRecord.
       DISPLAY "Enter Student Details:".
       DISPLAY "First Name, Last Name, Department, MailID".
       ACCEPT sfname.
       ACCEPT slname.

      *String Function - Concatenation
      *sname = sfname + slname
       STRING sfname DELIMITED BY SPACES
       SPACE DELIMITED BY SIZE
       INTO sname.
       STRING sname DELIMITED BY SPACES
       INTO sname.

       MOVE sname TO StudentName OF StudentFile.
       ACCEPT Department OF StudentFile.
       ACCEPT mailid OF StudentFile.
       WRITE StudentRec.

       PutStudentRecord.
       READ StudentFile RECORD AT END GO TO EndOperation.
       PERFORM IntoAnotherFile.
       GO TO PutStudentRecord.


       IntoAnotherFile.
       IF I<C
       DISPLAY " ".
       MOVE 0 to len.
       MOVE mailid OF StudentFile TO smail.


      *String Function - Find and Replace All
      *String gmail will be replaced by ymail
      *INSPECT smail REPLACING ALL 'gmail' BY 'ymail'.
       MOVE smail TO mailid OF StudentFile.
       MOVE StudentName OF StudentFile TO sname.
       MOVE Department OF StudentFile TO sdept.
       REWRITE StudentRec.

      *String Function - Split
      *String before @ will be stored in unmae
       UNSTRING smail DELIMITED BY '@'
       INTO uname

      *String Function - Length
      *Length of the Mail ID
       INSPECT smail TALLYING len FOR CHARACTERS BEFORE INITIAL' '.

       DISPLAY "Name:" sname.
       DISPLAY "Department:" sdept.
       DISPLAY "Mail ID:" smail.
       DISPLAY "UserName:" uname.
       DISPLAY "Mail ID Length:" len.
       DISPLAY " ".
       ADD 1 I GIVING I.

       EndOperation.
       DISPLAY " ".
       DISPLAY "Total No of Record in File:" N.
       DISPLAY " ".
       CLOSE StudentFile.

       FindReplaceAll.
       DISPLAY " ".
       IF ovrlap=0
       GO TO FRFile.
       //tocopy
       IDENTIFICATION DIVISION.
       PROGRAM-ID. seq1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INFILE ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OUTFILE ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD INFILE
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "TEXT.txt".
       01 INTEXT.
       02 ITEXT PIC X(79).
       FD OUTFILE
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "OUTTEXT.txt".
       01 OUTTEXT.
       02 OTEXT PIC X(79).
       WORKING-STORAGE SECTION.
       77 EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PARA.
       OPEN INPUT INFILE.
       OPEN OUTPUT OUTFILE.
       READ INFILE RECORD AT END MOVE 1 TO EOF.
       PERFORM X-PARA UNTIL EOF = 1.
       DISPLAY "SEE OUTTEXT.txt FILE FOR THE TEXT".
       CLOSE INFILE OUTFILE.
       STOP RUN.
       X-PARA.
       WRITE OUTTEXT FROM INTEXT.
       READ INFILE RECORD AT END MOVE 1 TO EOF.
