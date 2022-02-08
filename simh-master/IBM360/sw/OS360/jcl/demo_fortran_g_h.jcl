//FORTRANG JOB   MSGLEVEL=1                                             00020000
//FTNGTEST EXEC  FORTGCLG,PARM.FORT='EBCDIC,LIST,MAP'                   00040000
//FORT.SYSIN  DD  *                                                     00060000
C   ********************************************************************00080000
C   *                                                                  *00100000
C   * THIS PROGRAM IS A TEST CASE DESIGNED TO VERIFY THAT THE FORTRAN  *00120000
C   * IV (G) COMPILER HAS BEEN PROPERLY INSTALLED IN YOUR SYSTEM. THE  *00140000
C   * REQUIRED JOB CONTROL LANGUAGE STATEMENTS AND THE DATA CARD       *00160000
C   * ARE INCLUDED AS PART OF THE DECK.                                *00180000
C   *                                                                  *00200000
C   * THE PROGRAM GENERATES A TABLE OF BINOMIAL COEFFICIENTS WHICH IS  *00220000
C   * THEN PRINTED ON THE SYSOUT DEVICE.  ALL DATA EXCEPT THE FIRST    *00240000
C   * LINE OF OUTPUT IS PROGRAM GENERATED.                             *00260000
C   *                                                                  *00280000
C *                                                                    *00300000
C *   THE OUTPUT SHOULD BE ---                                         *00320000
C *                                                                    *00340000
C *      I---------------------------------------------------------I   *00360000
C *      I                           K                             I   *00380000
C * I----I---------------------------------------------------------I   *00400000
C * I  N I 1    2    3    4     5     6     7      8      9     10 I   *00420000
C * I----I---------------------------------------------------------I   *00440000
C * I  1 I  1    0    0    0     0     0     0      0      0     0 I   *00460000
C * I  2 I  2    1    0    0     0     0     0      0      0     0 I   *00480000
C * I  3 I  3    3    1    0     0     0     0      0      0     0 I   *00500000
C * I  4 I  4    6    4    1     0     0     0      0      0     0 I   *00520000
C * I  5 I  5   10   10    5     1     0     0      0      0     0 I   *00540000
C * I  6 I  6   15   20   15     6     1     0      0      0     0 I   *00560000
C * I  7 I  7   21   35   35    21     7     1      0      0     0 I   *00580000
C * I  8 I  8   28   56   70    56    28     8      1      0     0 I   *00600000
C * I  9 I  9   36   84  126   126    84    36      9      1     0 I   *00620000
C * I 10 I 10   45  120  210   252   210   120     45     10     1 I   *00640000
C * I 11 I 11   55  165  330   462   462   330    165     55    11 I   *00660000
C * I 12 I 12   66  220  495   792   924   792    495    220    66 I   *00680000
C * I 13 I 13   78  286  715  1287  1716  1716   1287    715   286 I   *00700000
C * I 14 I 14   91  364 1001  2002  3003  3432   3003   2002  1001 I   *00720000
C * I 15 I 15  105  455 1365  3003  5005  6435   6435   5005  3003 I   *00740000
C * I 16 I 16 120  560 1820  4368  8008 11440  12870  11440   8008 I   *00760000
C * I 17 I 17 136  680 2380  6188 12376 19448  24310  24310  19448 I   *00780000
C * I 18 I 18 153  816 3060  8568 18564 31824  43758  48620  43758 I   *00800000
C * I 19 I 19 171  969 3876 11624 27132 50388  75582  92378  92378 I   *00820000
C * I 20 I 20 190 1140 4845 15504 38760 77520 125970 167960 184756 I   *00840000
C * I----I---------------------------------------------------------I   *00860000
C *                                                                    *00880000
C   ********************************************************************00900000
      DIMENSION NBYK(20,10),HEAD(20)                                    00920000
      DO 10 K=1,10                                                      00940000
      NBYK(1,K) = K                                                     00960000
   10 CONTINUE                                                          00980000
      READ (5,1) HEAD                                                   01000000
      WRITE (6,2) HEAD                                                  01020000
      WRITE (6,3) (NBYK(1,K),K=1,10)                                    01040000
      DO 30 N=1,20                                                      01060000
      DO 20 K=1,10                                                      01080000
      NBYK(N,K) = KBINCO(N,K)                                           01100000
   20 CONTINUE                                                          01120000
      WRITE (6,4) N,(NBYK(N,K),K=1,10)                                  01140000
   30 CONTINUE                                                          01160000
      WRITE (6,5)                                                       01180000
      STOP                                                              01200000
    1 FORMAT (20A4)                                                     01220000
    2 FORMAT ('1   FORTRAN G SAMPLE TEST CASE'///////1X,20A4/////)      01240000
    3 FORMAT (T7,'I',57('-'),'I'/T7,'I',T35,'K',T65,'I'/T2,'I----I',    01260000
     X        57('-'),'I'/T2,'I  N I',I2,I4,2I5,3I6,3I7,'  I'/T2,       01280000
     X        'I----I',57('-'),'I')                                     01300000
    4 FORMAT (' I',I3,' I',I3,I4,2I5,3I6,3I7,' I')                      01320000
    5 FORMAT (' I----I',57('-'),'I')                                    01340000
      END                                                               01360000
C                                                                       01380000
C   ************************************************                    01400000
C   * KBINCO COMPUTES THE BINOMIAL COEFFICIENT,    *                    01420000
C   * C(N,K) = (N*(N-1)***(N-K+1))/(K*(K-1)***1),  *                    01440000
C   * WHERE N AND K ARE THE INTEGER ARGUEMENTS TO  *                    01460000
C   * THE FUNCTION.  INTERMEDIATE CALCULATIONS ARE *                    01480000
C   * PERFORMED IN REAL ARITHMETIC.  IN THE CASE   *                    01500000
C   * WHERE K .GT. N, A VALUE OF ZERO IS RETURNED. *                    01520000
C   * THE VALUES OF N AND K ARE LEFT UNCHANGED.    *                    01540000
C   * THE FUNCTION HAS BEEN CHECKED FOR ALL COM-   *                    01560000
C   * BINATIONS OF N=1,2,...,20 AND K=1,2,...,10.  *                    01580000
C   ************************************************                    01600000
C                                                                       01620000
      FUNCTION KBINCO (N,K)                                             01640000
CHECK FOR TRIVIAL CASES                                                 01660000
      IF (K .GT. N) GO TO 50                                            01680000
      IF (K .EQ. 0  .OR.  N .EQ. K) GO TO 60                            01700000
      IF (K .EQ. 1  .OR.  N-K .EQ. 1) GO TO 70                          01720000
CONVERT TO REAL FOR INT. CALCULATIONS                                   01740000
      P = N                                                             01760000
      Q = K                                                             01780000
CHECK FOR LOWER 'DENOMINATOR'                                           01800000
      IF (P-Q .LT. Q) Q = P-Q                                           01820000
COMPUTE DENOMINATOR                                                     01840000
      MAX = Q                                                           01860000
      BOT = 1.0                                                         01880000
      DO 30 I=2,MAX                                                     01900000
      BOT = I*BOT                                                       01920000
   30 CONTINUE                                                          01940000
COMPUTE NUMERATOR                                                       01960000
      MAX = P                                                           01980000
      MIN = P - Q + 1.0                                                 02000000
      TOP = 1.0                                                         02020000
      DO 40 I=MIN,MAX                                                   02040000
      TOP = I*TOP                                                       02060000
   40 CONTINUE                                                          02080000
CALCULATE AND ROUND BIN. COEFF.                                         02100000
      KBINCO = TOP/BOT + 0.5                                            02120000
      RETURN                                                            02140000
   50 KBINCO = 0                                                        02160000
      RETURN                                                            02180000
   60 KBINCO = 1                                                        02200000
      RETURN                                                            02220000
   70 KBINCO = N                                                        02240000
      RETURN                                                            02260000
      END                                                               02280000
/*                                                                      02290000
//GO.SYSIN DD *                                                         03000000
          BINOMIAL COEFFICIENTS                                         03200000
/*                                                                      03400000
//