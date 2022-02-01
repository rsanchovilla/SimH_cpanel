// JOB DEMORPG
// OPTION LINK,LIST
     ACTION MAP
// EXEC RPG
00000H                                                                    RPG001
01010FINPUT   IPE F  80  80            READ40 SYSIPT                      RPG002
01020FOUTPUT  O   V 132 132     OF     PRINTERSYSLST                      RPG003
01010IINPUT   AA  01   1 Z-                                               RPG004
01020I                                        8  29 NAME                  RPG005
01030I                                       30  310MONTH                 RPG006
01040I                                       32  330DAY                   RPG007
01050I                                       34  380INVNO                 RPG008
01060I                                       39  430CUSTNOL1              RPG009
01070I                                       44  450STATE                 RPG010
01080I                                       46  480CITY                  RPG011
01090I                                       74  802INVAMT                RPG012
01010C   01      INVAMT    ADD  TOTAL     TOTAL   72                      RPG013
01020C   01      INVAMT    ADD  GRPTOT    GRPTOT  72                      RPG014
01010OOUTPUT  H  201   1P                                                 RPG015
01020O       OR        OF                                                 RPG016
01030O                                   53 '      A C C O U N T S  R'    RPG017
01040O                                   77 ' E C E I V A B L E  R E '    RPG018
01050O                                   88 'G I S T E R'                 RPG019
01060O        H  1     1P                                                 RPG020
01070O       OR        OF                                                 RPG021
01080O                                   25 'CUSTOMER'                    RPG022
01090O                                   80 'LOCATION       INVOICE'      RPG023
01100O                                  109 'INVOICE DATE    INVOICE'     RPG024
01110O        H  2     1P                                                 RPG025
01120O       OR        OF                                                 RPG026
01130O                                   42 'NUMBER         CUSTOMER '    RPG027
01140O                                   46 'NAME'                        RPG028
01150O                                   79 ' STATE   CITY     NUMBER'    RPG029
01160O                                  108 ' MO    DAY     AMOUNT'       RPG030
02010O        D  2     01                                                 RPG031
02020O                         CUSTNOZ   23                               RPG032
02030O                         NAME      53                               RPG033
02040O                         STATE Z   59                               RPG034
02050O                         CITY  Z   67                               RPG035
02060O                         INVNO Z   79                               RPG036
02070O                         MONTH Z   90                               RPG037
02080O                         DAY   Z   96                               RPG038
02090O                         INVAMT   109 '$  ,  0.  '                  RPG039
02100O        T  2     L1                                                 RPG040
02110O                         GRPTOT B 109 '$  ,  0.  '                  RPG041
02120O                                  110 '*'                           RPG042
02130O        T  2     LR                                                 RPG043
02140O                         TOTAL    109 '$  ,  0.  '                  RPG044
02150O                                  111 '**'                          RPG045
/*
// EXEC LNKEDT
// EXEC
-01    AMALGAMATED CORP      1110116031071233061                         0038925
-02    BROWN WHOLESALE       1228123241131530231                         0080208
-03    BROWN WHOLESALE       1214995881131530231                         0026117
-04    FARM IMPLEMENTS       1018109011189747077                         0002763
-05    BLACK OIL             1108115091853016067                         0059295
-06    BLACK OIL             1223122921853016067                         0095097
-07    LEATHER BELT CO       1108115112071636471                         0033563
-08    LEATHER BELT CO       1217122632071636471                         0012175
-09    GENERAL MFG CO        1114116152901706063                         0044012
-10    GENERAL MFG CO        1123116762901706063                         0072222
-11    A-B-C DIST CO         0911096892905425039                         0064540
-12    A-B-C DIST CO         1111116052905425039                         0027169
-13    A-B-C DIST CO         1214122342905425039                         0055933
/*
/&
