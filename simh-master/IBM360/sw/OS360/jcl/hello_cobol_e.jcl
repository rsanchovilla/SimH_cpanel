//HLOCOBE  JOB  1234,COMLINKEX,MSGLEVEL=1                               0002
//STEPCMPL EXEC     COBECLG,REGION=128K
//SYSUT3        DD  UNIT=SYSDA,SPACE=(CYL,(8,5))
//COB.SYSIN     DD *                                                        0006
000000 IDENTIFICATION DIVISION.                                         0008BP01
000010 PROGRAM-ID. 'TCECBP01'.                                          0010BP01
000020 REMARKS.                                                         0012BP01
000030     MINIMUM ENVIRONMENT REQUIRED TO COMPILE AND EXECUTE          0014BP01
000040         SYSTEM/360 MODEL 30 WITH 32K STORAGE                     0016BP01
000050         CONTROL PROGRAM                                          0018BP01
000060         COBOL COMPILER E                                         0020BP01
000380 ENVIRONMENT DIVISION.                                            0084BP01
000390 CONFIGURATION SECTION.                                           0086BP01
000400 SOURCE-COMPUTER. IBM-360 E50.                                    0088BP01
000410 OBJECT-COMPUTER. IBM-360 E50.                                    0090BP01
000420 INPUT-OUTPUT SECTION.                                            0092BP01
000430 FILE-CONTROL.                                                    0094BP01
000620 DATA DIVISION.                                                   0132BP01
000630 FILE SECTION.                                                    0134BP01
002500 WORKING-STORAGE SECTION.                                         0508BP01
       77 MESSAGE-1 PICTURE X(19) VALUE                                 0510
              'HELLO WORLD COBOL-E'.                                    0512
003080 PROCEDURE DIVISION.                                              0646BP01
  100      DISPLAY MESSAGE-1
  110      STOP RUN.
//LKED.SYSUT1 DD  UNIT=SYSDA
/*                                                                      1328
//