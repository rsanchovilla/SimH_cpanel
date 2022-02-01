* $$JOB DEMOCOBF,,,BG
// JOB DEMOCOBF SAMPLE ANS COBOL PROGRAM
// OPTION LINK,SYM,LISTX
// EXEC FCOBOL
 CBL BUF=1024,SUPMAP,NOTRUNC
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       AUTHOR. A PROGRAMMER.
       REMARKS. THIS IS A SAMPLE ANS COBOL PROGRAM FOR A TYPICAL
           BUSINESS REPORT.  IT ALSO DEMONSTRATES A CONTROL BREAK,
           USING 'SEARCH' FOR A TABLE, CALLING A SUBROUTINE,
           AND USES BOTH A LINE COUNTER AND THE COBOL USE OF CH.12
           ON THE PRINTER CARRIAGE TAPE.  IT IS WRITTEN USING
           STRUCTURED PROGRAMMING TECHNIQUES.

           THIS PROGRAM SHOWS THE PRINTING METHOD OF SETTING UP
           ALL LINES IN WORKING STORAGE AND THEN USING THE 'FROM'
           OPTION ON THE 'WRITE' TO SEND EACH ONE TO THE PRINTER.
           SKIP1
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-360.
       OBJECT-COMPUTER. IBM-360.
       SPECIAL-NAMES.
           C01 IS TOP-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAY-FILE   ASSIGN TO SYS007-UR-2501-S.
           SELECT PRINT-FILE ASSIGN TO SYS009-UR-1403-S
               RESERVE NO ALTERNATE AREA.
           SKIP1
       DATA DIVISION.
       FILE SECTION.
       FD  PAY-FILE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 75 CHARACTERS
           DATA RECORD IS EMPLOYEE-RECORD.
       01  EMPLOYEE-RECORD.
           05  EM-DEPARTMENT           PIC XX.
           05  FILLER                  PIC XXX.
           05  EM-NAME                 PIC X(20).
           05  FILLER                  PIC X(8).
           05  EM-OTH-DED-X.
               10  EM-OTH-DED          PIC S9(5)V99.
           05  FILLER                  PIC X(16).
           05  EM-SSN13                PIC XXX.
           05  EM-SSN45                PIC XX.
           05  EM-SSN69                PIC XXXX.
           05  EM-SALARY-X.
               10  EM-SALARY           PIC S9(5)V99.
           05  FILLER                  PIC X(3).
       FD  PRINT-FILE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           DATA RECORD IS PRINT-LINE.
       01  PRINT-LINE                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  FILLER.
           02 FILLER  COMP-3.
               05  WS-PAGE             PIC S999        VALUE ZERO.
               05  LINE-CNTR           PIC S999.
               05  WS-FIT-WITH         PIC S9(5)V99.
               05  WS-SIT-WITH         PIC S9(5)V99.
               05  WS-NET-PAY          PIC S9(5)V99.
               05  WS-DT-SALARY        PIC S9(5)V99    VALUE ZERO.
               05  WS-DT-FIT-WITH      PIC S9(5)V99    VALUE ZERO.
               05  WS-DT-SIT-WITH      PIC S9(5)V99    VALUE ZERO.
               05  WS-DT-OTH-DED       PIC S9(5)V99    VALUE ZERO.
               05  WS-DT-NET-PAY       PIC S9(5)V99    VALUE ZERO.
               05  WS-FT-SALARY        PIC S9(7)V99    VALUE ZERO.
               05  WS-FT-FIT-WITH      PIC S9(7)V99    VALUE ZERO.
               05  WS-FT-SIT-WITH      PIC S9(7)V99    VALUE ZERO.
               05  WS-FT-OTH-DED       PIC S9(7)V99    VALUE ZERO.
               05  WS-FT-NET-PAY       PIC S9(7)V99    VALUE ZERO.
           02  FILLER.
               05  WS-SW-EOF           PIC XXX         VALUE 'NO'.
                   88  NO-MORE-RECORDS                 VALUE 'YES'.
               05  EOP-SW              PIC X.
               05  WS-DEPARTMENT       PIC XX.
               05  DEPTS-TABLE.
                   10  FILLER          PIC X(12) VALUE '10     SALES'.
                   10  FILLER          PIC X(12) VALUE '20      MFG.'.
                   10  FILLER          PIC X(12) VALUE '30  BUSINESS'.
                   10  FILLER          PIC X(12) VALUE '40    TRANS.'.
                   10  FILLER          PIC X(12) VALUE '50 ENGINEER.'.
               05  DEPT-TABLE REDEFINES DEPTS-TABLE OCCURS 5 TIMES
                       INDEXED BY IDX.
                   10  DEPT-CODE       PIC XX.
                   10  DEPT-DESC       PIC X(10).
           02  BLANK-LINE              PIC X           VALUE SPACE.
           02  HEADING1. 
               05  FILLER              PIC X.
               05  H1-TODAY            PIC X(8).
               05  FILLER              PIC X(40)       VALUE SPACES.
               05  FILLER    PIC X(70) VALUE 'ITPRG 248 SAMPLE PROGRAM'.
               05  FILLER              PIC X(5)        VALUE 'PAGE '.
               05  H1-PAGE             PIC ZZ9.
           02  HEADING2.
               05  FILLER              PIC X(14)  VALUE ' DEPT.   NAME'.
               05  FILLER              PIC X(15)       VALUE SPACES.
               05  FILLER              PIC X(100)      VALUE
                      'SOC.SEC.NO.        SALARY         F.I.T.
      -               'S.I.T.          OTHER           NET'.
           02  UNDER-LINE.
               05  FILLER              PIC X(14) VALUE ' _____    ____'.
               05  FILLER              PIC X(15)       VALUE SPACES.
               05  FILLER              PIC X(100)      VALUE
                      '___________        ______         ______
      -               '______          _____           ___'.
           02  DETAIL-LINE.
               05  FILLER              PIC X           VALUE SPACE.
               05  DL-DEPARTMENT       PIC XX.
               05  FILLER              PIC X(4)        VALUE SPACES.
               05  DL-NAME             PIC X(20).
               05  FILLER              PIC XX          VALUE SPACES.
               05  DL-SSN13            PIC XXX.
               05  DL-SSN-HYP1         PIC X.
               05  DL-SSN45            PIC XX.
               05  DL-SSN-HYP2         PIC X.
               05  DL-SSN69            PIC XXXX.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DL-SALARY           PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DL-FIT-WITH         PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DL-SIT-WITH         PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DL-OTH-DED          PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DL-NET-PAY          PIC ZZ,ZZZ.99-.
           02  ERROR-LINE.
               05  FILLER              PIC X           VALUE SPACE.
               05  EL-DEPARTMENT       PIC XX.
               05  FILLER              PIC X(4)        VALUE SPACES.
               05  EL-NAME             PIC X(20).
               05  FILLER              PIC XX          VALUE SPACES.
               05  EL-SSN13            PIC XXX.
               05  EL-SSN-HYP1         PIC X.
               05  EL-SSN45            PIC XX.
               05  EL-SSN-HYP2         PIC X.
               05  EL-SSN69            PIC XXXX.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  FILLER              PIC X(13)  VALUE 'ERROR IN DATA'.
           02  DEPARTMENT-LINE.
               05  FILLER              PIC X(12)       VALUE SPACES.
               05  DT-DEPT-DESC        PIC X(10)       VALUE SPACES.
               05  FILLER              PIC X(12) VALUE ' DEPARTMENT'.
               05  DT-DEPARTMENT       PIC X(2).
               05  FILLER              PIC X(9) VALUE ' TOTALS'.
               05  DT-SALARY           PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DT-FIT-WITH         PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DT-SIT-WITH         PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DT-OTH-DED          PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X(5)        VALUE SPACES.
               05  DT-NET-PAY          PIC ZZ,ZZZ.99-.
               05  FILLER              PIC X           VALUE '*'.
           02  FINAL-TOTAL-LINE.
               05  FILLER              PIC X(28)       VALUE SPACES.
               05  FILLER              PIC X(15)   VALUE 'FINAL TOTALS'.
               05  FT-SALARY           PIC ZZZZ,ZZZ.99-.
               05  FILLER              PIC X(3)        VALUE SPACES.
               05  FT-FIT-WITH         PIC ZZZZ,ZZZ.99-.
               05  FILLER              PIC X(3)        VALUE SPACES.
               05  FT-SIT-WITH         PIC ZZZZ,ZZZ.99-.
               05  FILLER              PIC X(3)        VALUE SPACES.
               05  FT-OTH-DED          PIC ZZZZ,ZZZ.99-.
               05  FILLER              PIC X(3)        VALUE SPACES.
               05  FT-NET-PAY          PIC ZZZZ,ZZZ.99-.
               05  FILLER              PIC XX          VALUE '**'.
           SKIP1
       PROCEDURE DIVISION.
           OPEN INPUT PAY-FILE, OUTPUT PRINT-FILE
           MOVE CURRENT-DATE TO H1-TODAY
           PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT
           READ PAY-FILE AT END MOVE 'YES' TO WS-SW-EOF.
           MOVE EM-DEPARTMENT TO WS-DEPARTMENT
           PERFORM DETAIL-ROUTINE THRU DETAIL-ROUTINE-EXIT
               UNTIL NO-MORE-RECORDS
           PERFORM FINAL-TOTALS THRU FINAL-TOTALS-EXIT
           CLOSE PAY-FILE, PRINT-FILE
           STOP RUN.
           SKIP1
       HEADING-ROUTINE.
           ADD 1 TO WS-PAGE
           MOVE WS-PAGE TO H1-PAGE
           WRITE PRINT-LINE FROM HEADING1 AFTER ADVANCING TOP-PAGE
           WRITE PRINT-LINE FROM HEADING2 AFTER ADVANCING 2
           WRITE PRINT-LINE FROM UNDER-LINE AFTER ADVANCING 0
           WRITE PRINT-LINE FROM BLANK-LINE AFTER ADVANCING 1
           MOVE 4 TO LINE-CNTR.
           MOVE '0' TO EOP-SW.
       HEADING-ROUTINE-EXIT.
           EXIT.
           SKIP1
       DETAIL-ROUTINE.
           IF EM-DEPARTMENT NOT = WS-DEPARTMENT
               PERFORM DEPARTMENT-TOTAL THRU DEPARTMENT-TOTAL-EXIT.
           IF EOP-SW = '1' OR LINE-CNTR > 56
               PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT.
           EXAMINE EM-SALARY-X  REPLACING LEADING SPACES BY ZEROS
           EXAMINE EM-OTH-DED-X REPLACING LEADING SPACES BY ZEROS
           IF EM-SALARY IS NUMERIC AND EM-OTH-DED IS NUMERIC
                    PERFORM DATA-IS-GOOD THRU DATA-IS-GOOD-EXIT
               ELSE PERFORM DATA-IS-BAD  THRU DATA-IS-BAD-EXIT.
           READ PAY-FILE AT END MOVE 'YES' TO WS-SW-EOF.
       DETAIL-ROUTINE-EXIT.
           EXIT.
           SKIP1
       DATA-IS-GOOD.
           MULTIPLY EM-SALARY BY .14  GIVING WS-FIT-WITH ROUNDED
           MULTIPLY EM-SALARY BY .025 GIVING WS-SIT-WITH ROUNDED
           COMPUTE WS-NET-PAY = EM-SALARY - WS-FIT-WITH - WS-SIT-WITH -
               EM-OTH-DED
           ADD EM-SALARY      TO WS-DT-SALARY
           ADD WS-FIT-WITH    TO WS-DT-FIT-WITH
           ADD WS-SIT-WITH    TO WS-DT-SIT-WITH
           ADD EM-OTH-DED     TO WS-DT-OTH-DED
           ADD WS-NET-PAY     TO WS-DT-NET-PAY
           MOVE EM-DEPARTMENT TO DL-DEPARTMENT
           MOVE EM-NAME       TO DL-NAME
           MOVE EM-SSN13      TO DL-SSN13
           MOVE EM-SSN45      TO DL-SSN45
           MOVE EM-SSN69      TO DL-SSN69
           MOVE '-'           TO DL-SSN-HYP1, DL-SSN-HYP2
           MOVE EM-SALARY     TO DL-SALARY
           MOVE WS-FIT-WITH   TO DL-FIT-WITH
           MOVE WS-SIT-WITH   TO DL-SIT-WITH
           MOVE EM-OTH-DED    TO DL-OTH-DED
           MOVE WS-NET-PAY    TO DL-NET-PAY
           WRITE PRINT-LINE FROM DETAIL-LINE AFTER ADVANCING 1
               EOP MOVE '1' TO EOP-SW.
           ADD 1 TO LINE-CNTR.
       DATA-IS-GOOD-EXIT.
           EXIT.
       DATA-IS-BAD.
           MOVE EM-DEPARTMENT TO EL-DEPARTMENT
           MOVE EM-NAME       TO EL-NAME
           MOVE EM-SSN13      TO EL-SSN13
           MOVE EM-SSN45      TO EL-SSN45
           MOVE EM-SSN69      TO EL-SSN69
           MOVE '-'           TO EL-SSN-HYP1, EL-SSN-HYP2
           WRITE PRINT-LINE FROM ERROR-LINE AFTER ADVANCING 1
               EOP MOVE '1' TO EOP-SW.
           ADD 1 TO LINE-CNTR.
       DATA-IS-BAD-EXIT.
           EXIT.
           SKIP1
       DEPARTMENT-TOTAL.
           SET IDX TO 1
           SEARCH DEPT-TABLE VARYING IDX
               AT END MOVE 'UNKNOWN' TO DT-DEPT-DESC
               WHEN WS-DEPARTMENT = DEPT-CODE (IDX)
                    MOVE DEPT-DESC (IDX) TO DT-DEPT-DESC.
           MOVE WS-DEPARTMENT  TO DT-DEPARTMENT
           MOVE WS-DT-SALARY   TO DT-SALARY
           MOVE WS-DT-FIT-WITH TO DT-FIT-WITH
           MOVE WS-DT-SIT-WITH TO DT-SIT-WITH
           MOVE WS-DT-OTH-DED  TO DT-OTH-DED
           MOVE WS-DT-NET-PAY  TO DT-NET-PAY
           WRITE PRINT-LINE FROM DEPARTMENT-LINE AFTER ADVANCING 1
               EOP MOVE '1' TO EOP-SW.
           WRITE PRINT-LINE FROM BLANK-LINE AFTER ADVANCING 1
               EOP MOVE '1' TO EOP-SW.
           ADD 2 TO LINE-CNTR
           ADD WS-DT-SALARY   TO WS-FT-SALARY
           ADD WS-DT-FIT-WITH TO WS-FT-FIT-WITH
           ADD WS-DT-SIT-WITH TO WS-FT-SIT-WITH
           ADD WS-DT-OTH-DED  TO WS-FT-OTH-DED
           ADD WS-DT-NET-PAY  TO WS-FT-NET-PAY
           MOVE ZEROS TO WS-DT-SALARY, WS-DT-FIT-WITH, WS-DT-SIT-WITH,
               WS-DT-OTH-DED, WS-DT-NET-PAY
           MOVE EM-DEPARTMENT TO WS-DEPARTMENT.
       DEPARTMENT-TOTAL-EXIT.
           EXIT.
           SKIP1
       FINAL-TOTALS.
           PERFORM DEPARTMENT-TOTAL THRU DEPARTMENT-TOTAL-EXIT
           MOVE WS-FT-SALARY   TO FT-SALARY
           MOVE WS-FT-FIT-WITH TO FT-FIT-WITH
           MOVE WS-FT-SIT-WITH TO FT-SIT-WITH
           MOVE WS-FT-OTH-DED  TO FT-OTH-DED
           MOVE WS-FT-NET-PAY  TO FT-NET-PAY
           WRITE PRINT-LINE FROM FINAL-TOTAL-LINE AFTER ADVANCING 1.
       FINAL-TOTALS-EXIT.
           EXIT.
/*
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// EXEC
10004ACHER, WILLIAM C.          00675241004            3679874321120000034
10185DONNEMAN, THOMAS M.        00900191003            1043781234130000040
10300FELDMAN, MIKE R.           00300000004            2156278643115000026
10325HATFIELD, MARK I.          00205390002            2225723628090000030
10730REEDE, OWEN W.             01051440001            2115897234105000021
10960WINGLAND, KEITH E.         00350000003            4215679672085000026
20111CARTOLER, VIOLET B.        00750060004            3455667381140000032
20304FROMM, STEVE V.            01200005002            2300267832122500037
20590NEIL, CLARENCE N.          00950230001            4016787112135000040
20801SCHEIBER, HARRY T.         00325080002            6198842236112500046
20956WANGLEY, THEO. A.          00150000003            1723456124120000050
30030ALLOREN, RUTH W.           00000000002            7647982367130000055
30181DELBERT, EDWARD D.         01305541015            6419182773110000051
30318HANEY, CAROL S.            01450005008            5533266791100000058
30487KING, MILDRED J.           01804290010            8711322487090500033
30834TRAWLEY, HARRIS T.         00550000009            7623561471100250032
40171COSTA, NAN S.              00560000005            1241963182122000046
40027ALHOUER, ELAINE E.         00220660006            6381469783079500022
40317HANBEE, ALETTA O.          00395000008            1136719674139000050
40721RASSMUSEN, JOHN J.         01000000004            2064397865129600040
50040ATKINSON, CHARLES          00675241004            3679874321120000034
50060BASEL, DEBORAH L           00900191003            1043781234130000040
50105BETTINARDI, RONALD J       01500500003            1125666601110000022
/*
/&
* $$EOJ