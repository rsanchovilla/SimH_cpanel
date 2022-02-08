//IDS  JOB  400565,S.EGGETT,MSGLEVEL=1,CLASS=A,REGION=108K,PRTY=8       01000020
//X EXEC PL1LFCLG,PARM.PL1L='SIZE=45056,S,NL,A,X,LD,ND,SM=(2,72),EB,C', 02000020
//             PARM.LKED='LET'                                          03000020
//PL1L.SYSIN DD *                                                       04000020
 IEMSP2 : PROCEDURE OPTIONS(MAIN);                                      05000020
 /* THIS PROGRAM TRANSFERS CARD IMAGES FROM ONE FILE TO ANOTHER.        06000020
 /* EACH IMAGE HAS A TYPE NUMBER AS FIRST CHARACTER.THE PROGRAM         07000020
 /* RECORDS THE NUMBER OF CARDS THERE ARE OF EACH TYPE. */              08000020
    DECLARE CARDS INPUT ENVIRONMENT(F(80)) ,                            09000020
            BUILD OUTPUT ENVIRONMENT(F(480,80)),                        10000020
          1 CARD,                                                       11000020
          2 TYP CHAR(1),                                                12000020
          2 BODY CHAR(79),                                              13000020
            T FIXED BINARY,                                             14000020
            TYPE(0:9) FIXED(3) STATIC INITIAL((10) 0 );                 15000020
    PUT EDIT('OUTPUT OF PL/1 SAMPLE PROGRAM')(COLUMN(45),A) PAGE;       16000020
    ON ENDFILE(CARDS) GOTO FINISH;                                      17000020
    ON CONVERSION BEGIN;PUT EDIT                                        18000020
 ('THIS CARD IGNORED BECAUSE TYPE IS NOT NUMERIC := ',CARD)(A) SKIP(2); 19000020
    GOTO LOOP; END;                                                     20000020
 LOOP: READ FILE(CARDS) INTO(CARD);                                     21000020
    T=TYP;TYPE(T) = TYPE(T) + 1;                                        22000020
    WRITE FROM(CARD) FILE(BUILD);                                       23000020
    GOTO LOOP;                                                          24000020
 FINISH: PUT DATA(TYPE) SKIP(3);                                        25000020
    PUT EDIT('END OF SAMPLE OUTPUT')(COLUMN(47),A) SKIP(2);             26000020
    END IEMSP2;                                                         27000020
/*                                                                      28000020
//GO.SYSABEND  DD  SYSOUT=A                                             29000020
//GO.BUILD DD UNIT=SYSDA,SPACE=(480,(10,5))                             30000020
//GO.CARDS DD *,DCB=BLKSIZE=80                                          31000020
2    FIRST OF DATA FOR PL1 SAMPLE PROGRAM                               32000020
6                                                                       33000020
6 THE                                                                   34000020
8  CONTENTS                                                             35000020
6   OF                                                                  36000020
2   THESE                                                               37000020
4     CARDS                                                             38000020
7      IMMATERIAL                                                       39000020
2       SINCE                                                           40000020
0       THEY                                                            41000020
6        ARE                                                            42000020
6         ONLY                                                          43000020
6          TRANSMITTED                                                  44000020
6           NOT                                                         45000020
6            PRINTED                                                    46000020
6                                                                       47000020
&       DELIBERATE DUD CARD                                             48000020
6                                                                       49000020
2   LAST OF SOURCE DATA FOR PL1 SAMPLE                                  50000020
/*                                                                      51000020
//