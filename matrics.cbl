       IDENTIFICATION DIVISION.
       PROGRAM-ID. matrics.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A.
       02 ADR OCCURS 10 TIMES.
       03 AD PIC S9(2) OCCURS 10 TIMES.
       01 B.
       02 BDR OCCURS 10 TIMES.
       03 BD PIC S9(2) OCCURS 10 TIMES.
       01 C.
       02 CDR OCCURS 10 TIMES.
       03 CDD PIC 9(3) OCCURS 10 TIMES.
       77 I PIC 9(2).
       77 J PIC 9(2).
       77 K PIC 9(2).
       77 R1 PIC 9(2).
       77 C1 PIC 9(2).
       77 M PIC S9(3).
       77 LIN PIC 9(3).
       77 COLL PIC 9(3).
       PROCEDURE DIVISION.
       P1.
       DISPLAY  "MATRIX OPERATIONS".
       DISPLAY " ENTER THE NO.of ROW :".
       ACCEPT R1.
       DISPLAY " ENTER THE No.OF COLUMN :".
       ACCEPT C1.
       DISPLAY " ENTER FIRST MATRIX VALUES(ONE by ONE) ".
       PERFORM P2 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J > C1.
       DISPLAY "ENTER SECOND MATRIX VALUES(ONE by ONE) :".
       PERFORM P3 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J > C1.
       PERFORM P4 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J > C1.

       DISPLAY " MATRIX ADDITION".
       MOVE 5 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P5 VARYING I FROM 1 BY 1 UNTIL I > R1.

       PERFORM P7 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J > C1.
       DISPLAY  "MATRIX SUBTRACTION".
       MOVE 10 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P8 VARYING I FROM 1 BY 1 UNTIL I > R1.
       PERFORM P10 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J> C1.
       DISPLAY "MATRIX MULTIPLICATION ".
       MOVE 15 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P12 VARYING I FROM 1 BY 1 UNTIL I > R1.

       PERFORM P14 VARYING I FROM 1 BY 1 UNTIL I > R1
       AFTER J FROM 1 BY 1 UNTIL J > C1.
       DISPLAY " MATRIX INVERSE ".
       MOVE 20 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P15 VARYING I FROM 1 BY 1 UNTIL I > R1.
       STOP RUN.
       P2.
       ACCEPT AD(I J).
       P3.
       ACCEPT BD(I J).
       P4.
       COMPUTE CDD(I J) = AD(I J) + BD(I J).
       P5.
       ADD 1 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P6 VARYING J FROM 1 BY 1 UNTIL J > C1.
       P6.
       DISPLAY LIN , COLL  CDD(I J).
       ADD 5 TO COLL.
       P7.
       COMPUTE CDD(I J) = AD(I J) - BD(I J).
       P8.
       ADD 1 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P9 VARYING J FROM 1 BY 1 UNTIL J > C1.
       P9.
       IF BD(I J) > AD(I J)

       DISPLAY  LIN , COLL  CDD(I J)
       ELSE
       DISPLAY LIN , COLL  CDD(I J).
       ADD 5 TO COLL.
       P10.
       MOVE 0 TO M.
       PERFORM P11 VARYING K FROM 1 BY 1 UNTIL K > C1.
       P11.
       COMPUTE M = M + AD(I K) * BD(K J).
       MOVE M TO CDD(I J).
       P12.
       ADD 1 TO LIN.
       MOVE 30 TO COLL.
       PERFORM P13 VARYING J FROM 1 BY 1 UNTIL J > C1.
       P13.
       DISPLAY  LIN , COLL  CDD(I J).
       ADD 5 TO COLL.

       P14.
       COMPUTE M = AD(J I) + 0.
       MOVE M TO CDD(I J).
       P15.
       ADD 5 TO LIN.
       MOVE 35 TO COLL.
       PERFORM P16 VARYING J FROM 1 BY 1 UNTIL J > C1.
       P16.
       DISPLAY LIN , COLL  CDD(I J).
       ADD 5 TO COLL.
