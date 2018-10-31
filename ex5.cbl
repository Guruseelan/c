       IDENTIFICATION DIVISION.
       PROGRAM-ID. HHH.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EMP1 ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD EMP1
       LABEL RECORDS ARE STANDARD
       VALUE OF FILE-ID IS "REC"
       DATA RECORD IS DT.
       01 DT.
       02 NAM PIC A(20).
       02 IDNO PIC X(20).
       02 BPAY PIC 9(6).
       02 DA PIC 9(6)V99.
       02 HRA PIC 9(6)V99.
       02 PFI PIC 9(6)V99.
       02 GPAY PIC 9(6)V99.
       02 NPAY PIC 9(6)V99.
       WORKING-STORAGE SECTION.
       77 N PIC 9(6).
       77 GP PIC ZZZZ99.99.
       77 NP PIC ZZZZ99.99.
       77 NOI PIC X(10).
       77 Z PIC ZZZZ9.99.
       77 CHI PIC 9.
       77 CHOICE PIC 99.
       77 R1 PIC 99.
       PROCEDURE DIVISION.
       P1.
       DISPLAY "CHOICE BOARD".
       DISPLAY "1.CREATE".
       DISPLAY "2.APPEND".
       DISPLAY "3.UPDATE".
       DISPLAY "4.SHOW".
       DISPLAY "5.EXIT".
       DISPLAY "ENTER YOUR CHOICE".
       ACCEPT CHI.
       IF CHI=1
       PERFORM P2
       ELSE IF CHI=2
       PERFORM P3
       ELSE IF CHI=3
       PERFORM P4
       ELSE IF CHI=4
       MOVE 0 TO R1
       PERFORM P5
       ELSE IF CHI=5
       STOP RUN.
       GO TO P1.
       P2.
       DISPLAY "ENTER NO OF RECORDS".
       DISPLAY "HOW MANY RECORDS".
       ACCEPT N.
       OPEN OUTPUT EMP1.
       PERFORM P2A THRU P2B N TIMES.
       CLOSE EMP1.
       P2A.
       DISPLAY "EMPLOYEE NAME"
       ACCEPT NAM.
       DISPLAY "EMPLOYEE NUMBER".
       ACCEPT IDNO.
       DISPLAY "BASIC PAY".
       ACCEPT BPAY.
       COMPUTE DA=25/100*BPAY.
       COMPUTE HRA=15/100*BPAY.
       COMPUTE GPAY=BPAY+DA+HRA.
       COMPUTE PFI=2/100*BPAY.
       COMPUTE NPAY = GPAY - PFI.
       MOVE GPAY TO GP.
       MOVE NPAY TO NP.
       P2B.
       WRITE DT.
       P3.
       DISPLAY "ENTER NO. OF RECORDS".
       DISPLAY "HOW MANY RECORDS".
       ACCEPT N.
       OPEN EXTEND EMP1.
       PERFORM P2A THRU P2B N TIMES.
       CLOSE EMP1.
       P4.
       DISPLAY "EMPLOYEE NUMBER TO UPDATE".
       ACCEPT NOI.
       DISPLAY "UPDATING A FILE".
       OPEN I-O EMP1.
       PERFORM P4A.
       P4A.
       READ EMP1 RECORD AT END DISPLAY "NOT FOUND";

       CLOSE EMP1 GO TO P1.
       IF NOI=IDNO
       DISPLAY "FOUND"
       PERFORM P2A
       REWRITE DT
       CLOSE EMP1
       GO TO P1.
       GO TO P4A.
       P5.
       DISPLAY "DISPLAYING FILE".
       OPEN INPUT EMP1.
       PERFORM P5A.
       P5A.
       DISPLAY "****************************".
       DISPLAY "BP DA HRA PF GP NP".
       DISPLAY "----------------------------".
       READ EMP1 RECORD AT END CLOSE EMP1 GO TO P1.
       ADD 1 TO R1.
       DISPLAY "NAME:" NAM.
       DISPLAY "RECORD NO:" IDNO.
       MOVE BPAY TO Z.
       DISPLAY "BASIC PAY:" Z.
       MOVE DA TO Z.
       DISPLAY "DEARNESS ALLOWANCE:" Z.
       MOVE HRA TO Z.
       DISPLAY "HOUSE RENT ALLOWANCE:" Z.
       MOVE PFI TO Z.
       DISPLAY "PROVIDENT FUND:" Z.
       DISPLAY "GROSS PAY:" GP.
       DISPLAY "NET PAY" NP.
       DISPLAY "PLEASE ENTER TO CONTINUE...".
       ACCEPT CHOICE.
       GO TO P5A.
