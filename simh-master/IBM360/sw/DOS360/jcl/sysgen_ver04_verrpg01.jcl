* $$JOB VERRPG01,,,BG
// JOB VERRPG01
// OPTION LINK
// EXEC RPG
00000H                                                                    $460
01010FINPUT   IPE F  80  80            READ40 SYSIPT                      $460
01020FOUTPUT  O   V 132 132     OF     PRINTERSYSLST                      $460
01010IINPUT   AA  01   1 Z-                                               $460
01020I                                        8  29 NAME                  $460
01030I                                       30  310MONTH                 $460
01040I                                       32  330DAY                   $460
01050I                                       34  380INVNO                 $460
01060I                                       39  430CUSTNOL1              $460
01070I                                       44  450STATE                 $460
01080I                                       46  480CITY                  $460
01090I                                       74  802INVAMT                $460
01010C   01      INVAMT    ADD  TOTAL     TOTAL   72                      $460
01020C   01      INVAMT    ADD  GRPTOT    GRPTOT  72                      $460
01010OOUTPUT  H  201   1P                                                 $460
01020O       OR        OF                                                 $460
01030O                                   53 '      A C C O U N T S  R'    $460
01040O                                   77 ' E C E I V A B L E  R E '    $460
01050O                                   88 'G I S T E R'                 $460
01060O        H  1     1P                                                 $460
01070O       OR        OF                                                 $460
01080O                                   25 'CUSTOMER'                    $460
01090O                                   80 'LOCATION       INVOICE'      $460
01100O                                  109 'INVOICE DATE    INVOICE'     $460
01110O        H  2     1P                                                 $460
01120O       OR        OF                                                 $460
01130O                                   42 'NUMBER         CUSTOMER '    $460
01140O                                   46 'NAME'                        $460
01150O                                   79 ' STATE   CITY     NUMBER'    $460
01160O                                  108 ' MO    DAY     AMOUNT'       $460
02010O        D  2     01                                                 $460
02020O                         CUSTNOZ   23                               $460
02030O                         NAME      53                               $460
02040O                         STATE Z   59                               $460
02050O                         CITY  Z   67                               $460
02060O                         INVNO Z   79                               $460
02070O                         MONTH Z   90                               $460
02080O                         DAY   Z   96                               $460
02090O                         INVAMT   109 '$  ,  0.  '                  $460
02100O        T  2     L1                                                 $460
02110O                         GRPTOT B 109 '$  ,  0.  '                  $460
02120O                                  110 '*'                           $460
02130O        T  2     LR                                                 $460
02140O                         TOTAL    109 '$  ,  0.  '                  $460
02150O                                  111 '**'                          $460
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
* $$EOJ