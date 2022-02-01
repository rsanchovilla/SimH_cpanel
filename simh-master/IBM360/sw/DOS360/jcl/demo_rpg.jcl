// JOB DEMORPG - SAMPLE RPG PROGRAM
// OPTION LINK
// EXEC RPG
      * TYPICAL BUSINESS APPLICATION
      * DEMONSTRATES TABLES AND CONTROL BREAK
      * CC 75-80 of the H card is the CSECT name.
     H                                                                    PAYREG
     FSALARY  IT  F  80  80           EREAD01 SYSIPT
     FFEDWITH IT  F  80  80           EREAD01 SYSIPT
     FCARDS   IPE F  80  80    2       READ01 SYSIPT
     FREPORT  O   V 120 120     OF    LPRINTERSYSLST
     FNEDECAR O   F  80  80            READ40 SYSPCH
     E    SALARY          TABARG 10  10  2  ATABFUN  5 2
     E    FEDWITH         TABAAA  4   5  5 2ATABFFF 12  
     LREPORT  001010070201303019040250503106037070430804609049100531105712
     ICARDS   AA  12  80 CN
     I       OR   01  80 CR
     I                                        1   20DEPTNOL1
     I                                        3   50EMPNO
     I                                        6  25 EMPLOE
     I                                       26  322YTDPAY
     I                                       33  392CURSAL
     I                                       40  412COMRAT
     I                                       42  43 YREMP
     I                                       50  552DEDUCT          06
     I                                       58  580EXEMPT
     I        BB  02
     I                                        1  80 BADCRD
     C   12                SETON                     01
     C*          GROSS PAY = COMMISSION + SALARY
     C   01      CURSAL    MULT COMRAT    COMPAY  72H
     C   01      YREMP     LOKUPTABARG    TABFUN         03
     C   01 03   TABFUN    ADD  COMPAY    GRSPAY  72
     C*                    F I C A   T A X
     C   01      YTDPAY    ADD  GRSPAY    YTDPA   72
     C   01      YTDPA     COMP 10800.               09
     C   01N09   GRSPAY    MULT .0585     FICA    52H      ALL PAY TAXED
     C   01 09   10800.    SUB  YTDPAY    RESULT  72 10
     C   01 09 10RESULT    MULT .0585     FICA      H      SOME PAY TAXED
     C   01 09N10          Z-ADD0         FICA             NO PAY TAXED
     C*          F E D E R A L   I N C O M E   T A X
     C   01      EXEMPT    MULT 62.5      EXEMPA  52
     C   01      GRSPAY    SUB  EXEMPA    TAXPAY  52   07
     C   01 07             Z-ADD0         FIT     52
     C   01 07             GOTO ENDFIT
     C   01      TAXPAY    LOKUPTABAAA    TABFFF 12  04  04
     C   01 04             MOVELTABFFF    FIT
     C   01 04             MOVE TABFFF    WK1     7
     C   01 04             MOVELWK1       PERCNT  22
     C   01 04             MOVE WK1       INCRMT  52
     C   01 04   TAXPAY    SUB  INCRMT    WK2     72
     C   01 04   WK2       MULT PERCNT    EXFIT   52H
     C   01 04   FIT       ADD  EXFIT     FIT     52
     C           ENDFIT    TAG
     C*          I L L I N O I S   I N C O M E   T A X
     C   01N12   GRSPAY    MULT 12.       YGRSPA  72
     C   01N12   EXEMPT    MULT 1000.     EXSPAY  72
     C   01N12   YGRSPA    SUB  EXSPAY    TASPAY  72   08
     C   01N12 08          Z-ADD0         IIT     52
     C   01N12N08TASPAY    MULT .025      WK3     52H
     C   01N12N08WK3       DIV  12        IIT     52H
     C   01N12             Z-ADDIIT       SIT     52
     C   01N12   CIIT      ADD  IIT       CIIT    52
     C*          I N D I A N A   I N C O M E   T A X
     C   01 12   EXEMPT    SUB  1         EXEMP   10   14
     C   01 12N14EXEMP     MULT 75        WK5     52
     C   01 12N14WK5       ADD  116.67    EXNPAY  52
     C   01 12 14          Z-ADD0         EXNPAY
     C   01 12   GRSPAY    SUB  EXNPAY    TANPAY  72   13
     C   01 12N13TANPAY    MULT .02       NIT     52H
     C   01 12 13          Z-ADD0         NIT
     C   01 12             Z-ADDNIT       SIT
     C   01 12   CNIT      ADD  NIT       CNIT    52
     C*                    N E T   P A Y
     C   01      GRSPAY    SUB  FICA      WK4     72
     C   01      WK4       SUB  FIT       WK4
     C   01      WK4       SUB  SIT       WK4
     C   01      WK4       SUB  DEDUCT    NETPAY  72   05
     C   01 05             Z-SUBNETPAY    NETPA   62
     C   01 05             Z-ADDNETPA     NDEDUC  62
     C   01 05   DEDUCT    SUB  NETPA     DEDUCT
     C   01 05   WK4       SUB  DEDUCT    NETPAY
     C   01 05             SETON                     11
     C   01 05             MOVE DEPTNO    ADEPNO  2
     C   01 05             MHLZOADEPNO    ADEPNO           CORRECT SIGN
     C   01      GRSPAY    ADD  DGSPAY    DGSPAY  72
     C   01      FICA      ADD  DFICA     DFICA   72
     C   01      FIT       ADD  DFIT      DFIT    72
     C   01      SIT       ADD  DSIT      DSIT    72
     C   01      DEDUCT    ADD  DDDUCT    DDDUCT  72
     C   01      NETPAY    ADD  DNTPAY    DNTPAY  72
     CL1         DGSPAY    ADD  CGSPAY    CGSPAY  72
     CL1         DFICA     ADD  CFICA     CFICA   72
     CL1         DFIT      ADD  CFIT      CFIT    72
     CL1         DSIT      ADD  CSIT      CSIT    72
     CL1         DDDUCT    ADD  CDDUCT    CDDUCT  72
     CL1         DNTPAY    ADD  CNTPAY    CNTPAY  72
     OREPORT  H  2 1   1P
     O                                   37 'NADIR  TELEVISION'
     O                                   46 'COMPANY'
     O        H  3     1P
     O       OR  3 1   OF
     O                         UDATE      8 ' 0/  /  '
     O                                   42 'PAYROLL  REGISTER  FOR'
     O                                   58 'JANUARY  1973'
     O                                  110 'PAGE'
     O                         PAGE  Z  114
     O        H  1     1P
     O       OR        OF
     O                                   21 'DEPARTMENT  EMPLOYEE'
     O                                   58 'GROSS'
     O                                  102 'OTHER'
     O        H  2     1P
     O       OR        OF
     O                                   20 'NUMBER      NUMBER'
     O                                   37 'EMPLOYEE'
     O                                   58 'PAY'
     O                                   80 'FICA        FIT'
     O                                  102 'SIT     DEDUCT'
     O                                  114 'NET PAY'
     O        D  1     01
     O                 L1      DEPTNOZ    6
     O                         EMPNO Z   18
     O                         EMPLOE    46
     O                         GRSPAY    59 '  ,  0.  -'
     O                         FICA      70 '  0.  -'
     O                         FIT       81 '  0.  -'
     O                N12      IIT       92 '  0.  -'
     O                 12      NIT       93 '  0.  -N'
     O                N06      DEDUCT   103 ' ,  0.  -'
     O                         NETPAY   115 '  ,  0.  -'
     O                 05               116 '*'
     O        D  1     02
     O                         BADCRD    80
     O                                   95 '*BAD RECORD*'
     O        T  2     L1
     O                                   30 'DEPARTMENT    TOTALS'
     O                         DEPTNOZ   23
     O                         DGSPAY B  59 '$  ,  0.  -'
     O                         DFICA  B  70 '$  ,  0.  -'
     O                         DFIT   B  81 '$  ,  0.  -'
     O                         DSIT   B  92 '$  ,  0.  -'
     O                         DDDUCT B 103 '$  ,  0.  -'
     O                         DNTPAY B 115 '$  ,  0.  -'
     O        T 11     LR
     O                                   24 'COMPANY TOTALS'
     O                         CGSPAY    59 '$  ,  0.  -'
     O                         CFICA     70 '$  ,  0.  -'
     O                         CFIT      81 '$  ,  0.  -'
     O                         CSIT      92 '$  ,  0.  -'
     O                         CDDUCT   103 '$  ,  0.  -'
     O                         CNTPAY   115 '$  ,  0.  -'
     O        T  1     LR
     O                                   83 'ILL'
     O                         CIIT      92 '  0.  -'
     O        T  0     LR
     O                                   83 'IND'
     O                         CNIT      92 '  0.  -
     ONEDECAR D2       01 05
     O                                   80 'R'
     O                         NDEDUC    55 '0      '
     O                         EMPLOE    25
     O                         EMPNO      5 '0   '
     O                         ADEPNO     2
     O        T2       LR 11
/*
// EXEC LNKEDT
// EXEC
0120000022500003275000430000053250006350000737500084000009450001050000
04600000000000000171000000014046007250001750161710089600106142072500
99999140342489600
10004ACHER, WILLIAM C.   000000000325241004      000000  1                     R
10185DONNEMAN, THOMAS M. 000000000900191003      001020  2                     R
10300FELDMAN, MIKE R.    000000000300000004      000000  3                     R
10325HATFIELD, MARK I.   000000000205390002      000220  2                     R
10730REEDE, OWEN W.      000000001051440001      002310  3                     R
10960WINGLAND, KEITH E.  000000000350000003      000000  4                     R
20111CARTOLER, VIOLET B. 000000000750060004      001140  5                     N
20304FROMM, STEVE V.     000000001200005002      001823  4                     R
20590NEIL, CLARENCE N.   000000000950230001      002324  4                     R
20801SCHEIBER, HARRY T.  000000000325080002      000520  1                     R
20956WANGLEY, THEO. A.   000000000150000003      000275  4                     R
30030ALLOREN, RUTH W.    110000000000000002      000000  3                     R
30181DELBERT, EDWARD D.  110000001305541015      000000  1                     R
30311GROLER, GRACE B.    1100000         10      048369  2                     R
30318HANEY, CAROL S.     000000001450005008      006019  5                     R
30487KING, MILDRED J.    110000001804290010      005322  5                     R
30834TRAWLEY, HARRIS T.  000000000550000009      001329  1                     R
40027ALHOUER, ELAINE E.  000000000220660006      000400  2                     R
40171COSTA, NAN S.       000000000560000005      000903  4                     R
40317HANBEE, ALETTA O.   000000000395000008      001180  4                     R
40721RASSMUSEN, JOHN J.  000000001000000004      002346  2                     R
40739RIDEL, ROBERT R.    0000000         03      007224  2                     N
40806STOCKTON, NORMAN Q. 0000000         03      013380  3                     R
50122CENNA, DICK L.      0000000         07      000000  3                     R
50207EBERHARDT, RON G.   0000000         02      000980  5                     R
50308GLEASON, JAMES E.   0000000         04      000000  2                     R
50568LYNNE, GERALD H.    0000000         06      001428  2                     R
50909UDSON, DORIS M.     0000000         08      000000  4                     R
60100BATES, TONY F.      0000000         09      005170  5                     R
60179DAMSON, ERIC C.     0000000         07      000223  2                     R
60292EVERLY, DONNA M.    0000000         05      000322  4                     R
60409ICK, MICK W.        0000000         03      000590  5                     R
60607ODELLE, NICHOLAS P. 0000000         01      006220  5                     R
60825TILLMAN, DON M.     1100000         08      004665  4                     R
70214EDMONSON, RICK T.   0000000         06      000000  1                     R
70310GORMALLY, MARIE N.  0000000         03      000866  1                     R
70332HELD, ANNA J.       0000000         05      000926  1                     R
70689OWNEY, REED M.      0000000         08      001132  1                     R
70802SHEA, MICHAEL H.    0000000         02      004680  1                     R
80102BELLSLEY, ARTHUR A. 0000000         04      002762  1                     R
80282ESTEBAN, JUAN L.    0000000         07      000000  1                     R
80322HARLETON, JEAN H.   0000000         08      000000  1                     N
80505LAMBERT, JERRY O.   0000000         06      000000  1                     R
80921ULL, GEORGE A.      0000000         04      012080  1                     R
90105BOYLE, RALPH P.     0000000         02      007787  1                     R
90215EDSON, WILBUR S.    0000000         01      001575  1                     R
90315HALE, ALAN A.       0000000         03      003829  1                     R
90574MELTZ, FRANK K.     0000000         05      000000  1                     R
90740RIDGEFIELD, SUZY S. 0000000         07      002310  1                     R
90820TELLER, STEPHEN U.  0000000         09      032222  1                     R
/*
/&
