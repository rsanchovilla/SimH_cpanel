//RPGSMPL JOB  CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=96K
//     EXEC RPGECLG
//RPG.SYSUT3  DD   UNIT=SYSDA
//RPG.SYSUT2  DD   UNIT=SYSDA
//RPG.SYSUT1  DD   UNIT=SYSDA
//RPG.SYSGO   DD   UNIT=SYSDA
//RPG.SYSIN   DD   *
00000H                                                                    SAMPL1
01010FINPUT   IPE F  80  80            READ40                             SAMPL1
01020FOUTPUT  O   V 132 132     OF     PRINTER                            SAMPL1
01010IINPUT   AA  01   1 Z-                                               SAMPL1
01020I                                        8  29 NAME                  SAMPL1
01030I                                       30  310MONTH                 SAMPL1
01040I                                       32  330DAY                   SAMPL1
01050I                                       34  380INVNO                 SAMPL1
01060I                                       39  430CUSTNOL1              SAMPL1
01070I                                       44  450STATE                 SAMPL1
01080I                                       46  480CITY                  SAMPL1
01090I                                       74  802INVAMT                SAMPL1
01010C   01      INVAMT    ADD  TOTAL     TOTAL   72                      SAMPL1
01020C   01      INVAMT    ADD  GRPTOT    GRPTOT  72                      SAMPL1
01010OOUTPUT  H  201   1P                                                 SAMPL1
01020O       OR        OF                                                 SAMPL1
01030O                                   53 '      A C C O U N T S  R'    SAMPL1
01040O                                   77 ' E C E I V A B L E  R E '    SAMPL1
01050O                                   88 'G I S T E R'                 SAMPL1
01060O        H  1     1P                                                 SAMPL1
01070O       OR        OF                                                 SAMPL1
01080O                                   25 'CUSTOMER'                    SAMPL1
01090O                                   80 'LOCATION       INVOICE'      SAMPL1
01100O                                  109 'INVOICE DATE    INVOICE'     SAMPL1
01110O        H  2     1P                                                 SAMPL1
01120O       OR        OF                                                 SAMPL1
01130O                                   42 'NUMBER         CUSTOMER '    SAMPL1
01140O                                   46 'NAME'                        SAMPL1
01150O                                   79 ' STATE   CITY     NUMBER'    SAMPL1
01160O                                  108 ' MO    DAY     AMOUNT'       SAMPL1
02010O        D  2     01                                                 SAMPL1
02020O                         CUSTNOZ   23                               SAMPL1
02030O                         NAME      53                               SAMPL1
02040O                         STATE Z   59                               SAMPL1
02050O                         CITY  Z   67                               SAMPL1
02060O                         INVNO Z   79                               SAMPL1
02070O                         MONTH Z   90                               SAMPL1
02080O                         DAY   Z   96                               SAMPL1
02090O                         INVAMT   109 '$  ,  0.  '                  SAMPL1
02100O        T  2     L1                                                 SAMPL1
02110O                         GRPTOT B 109 '$  ,  0.  '                  SAMPL1
02120O                                  110 '*'                           SAMPL1
02130O        T  2     LR                                                 SAMPL1
02140O                         TOTAL    109 '$  ,  0.  '                  SAMPL1
02150O                                  111 '**'                          SAMPL1
//LKED.SYSUT1 DD   UNIT=SYSDA
//GO.OUTPUT   DD   SYSOUT=A
//GO.INPUT    DD   *
K      AMALGAMATED CORP      1110116031071233 61                           38925
K      BROWN WHOLESALE       1228123241131530231                           80208
K      BROWN WHOLESALE       1214995881131530231                           26117
K      FARM IMPLEMENTS       1018109011189747 77                            2763
K      BLACK OIL             11 8115091853016 67                           59295
K      BLACK OIL             1223122921853016 67                           95097
K      LEATHER BELT CO       11 8115112071636471                           33563
K      LEATHER BELT CO       1217122632071636471                           12175
K      GENERAL MFG CO        11141161529017 6 63                           44012
K      GENERAL MFG CO        11231167629017 6 63                           72222
K      A-B-C DIST CO          911 96892905425 39                           64540
K      A-B-C DIST CO         1111116052905425 39                           27169
K      A-B-C DIST CO         1214122342905425 39                           55933
//