//EXECUTE  JOB   1234,COMLINKEX,MSGLEVEL=1                              0002
//STEPX    EXEC     COBECLG                                             0004
//COB.SYSIN DD *                                                        0006
000000 IDENTIFICATION DIVISION.                                         0008BP01
000010 PROGRAM-ID. 'TCECBP01'.                                          0010BP01
000020 REMARKS.                                                         0012BP01
000030     MINIMUM ENVIRONMENT REQUIRED TO COMPILE AND EXECUTE          0014BP01
000040         SYSTEM/360 MODEL 30 WITH 32K STORAGE                     0016BP01
000050         CONTROL PROGRAM                                          0018BP01
000060         COBOL COMPILER E                                         0020BP01
000070     SPECIAL OPERATOR INSTRUCTIONS-NONE                           0022BP01
000080     PURPOSE                                                      0024BP01
000090         TEST THE COBOL COMPILERS ABILITY TO READ AND             0026BP01
000100         WRITE FROM TAPE CHECKING PROPER DATA ALIGNMENT           0028BP01
000110         WHERE DATA IS A MIXTURE OF DISPLAY COMPUTATIONAL         0030BP01
000120         AND COMPUTATIONAL-3.                                     0032BP01
000130         FILES ARE HOMOGENEOUS HETROGENEOUS EQUAL AND DIFFERING.  0034BP01
000140     PREREQUISITES                                                0036BP01
000150         ALL PREVIOUS GROUP A AND B TEST CASES OPERATIONAL.       0038BP01
000160     INDIVIDUAL TEST CASES IN THIS PROGRAM                        0040BP01
000170         GROUP B                                                  0042BP01
000180         LEVEL P                                                  0044BP01
000190         TEST CASE 1                                              0046BP01
000200     NUMBER OF SOURCE STATEMENTS                                  0048BP01
000210         575                                                      0050BP01
000220     EXPECTED RESULTS                                             0052BP01
000230         HEADING                                                  0054BP01
000240         END OF TEST                                              0056BP01
000250     ERROR INDICATIONS                                            0058BP01
000260         ********** (PARAGRAPH-NAME)  FAILED                      0060BP01
000270     DATE STARTED TEST WRITING                                    0062BP01
000280         JANUARY 25 1965                                          0064BP01
000290     DATE FINISHED TEST WRITING                                   0066BP01
000300         JANUARY 28 1965                                          0068BP01
000310     DATE DEBUGGED ON CURRENT SYSTEM                              0070BP01
000320                                                                  0072BP01
000330     DATE ADDED TO ATP SYSTEM                                     0074BP01
000340                                                                  0076BP01
000350 AUTHOR.                                                          0078BP01
000360         D50 TEST AND RELEASE PROCESSOR A GROUP                   0080BP01
000370         P J MITCHELL                                             0082BP01
000380 ENVIRONMENT DIVISION.                                            0084BP01
000390 CONFIGURATION SECTION.                                           0086BP01
000400 SOURCE-COMPUTER. IBM-360 E50.                                    0088BP01
000410 OBJECT-COMPUTER. IBM-360 E50.                                    0090BP01
000420 INPUT-OUTPUT SECTION.                                            0092BP01
000430 FILE-CONTROL.                                                    0094BP01
000440     SELECT HOMO-WRITE-FILE1-FIXED ASSIGN TO                      0096BP01
           'SYS004'                                                     0098
000460     UTILITY.                                                     0100BP01
000470     SELECT HOMO-READ-FILE1-FIXED ASSIGN TO                       0102BP01
           'SYS004'                                                     0104
000490     UTILITY.                                                     0106BP01
000500     SELECT HETRO-WRITE-FILE1-EQUAL ASSIGN TO                     0108BP01
           'SYS005'                                                     0110
000520     UTILITY.                                                     0112BP01
000530     SELECT  HETRO-READ-FILE1-EQUAL ASSIGN TO                     0114BP01
           'SYS005'                                                     0116
000550     UTILITY.                                                     0118BP01
000560     SELECT  HETRO-WRITE-FILE1-DIFF ASSIGN TO                     0120BP01
           'SYS006'                                                     0122
000580     UTILITY.                                                     0124BP01
000590     SELECT  HETRO-READ-FILE1-DIFF ASSIGN TO                      0126BP01
           'SYS006'                                                     0128
000610     UTILITY.                                                     0130BP01
000620 DATA DIVISION.                                                   0132BP01
000630 FILE SECTION.                                                    0134BP01
000640 FD  HOMO-READ-FILE1-FIXED                                        0136BP01
000650     RECORD CONTAINS 24 CHARACTERS                                0138BP01
000660     RECORDING MODE IS F                                          0140BP01
000670     LABEL RECORDS ARE OMITTED                                    0142BP01
000680     DATA RECORD IS REC-1.                                        0144BP01
000690 01  REC-1.                                                       0146BP01
000700 02  FORMAT-1-REC-1.                                              0148BP01
000710 03  NUM-1-REC-1 PICTURE 9 DISPLAY.                               0150BP01
000720 03  FILLER PICTURE 9(1).                                         0152BP01
000730 03  NUM-2-REC-1     PICTURE S9(4)                  COMPUTATIONAL.0154BP01
000740 03  NUM-3-REC-1 PICTURE 9 DISPLAY.                               0156BP01
000750 03  FILLER PICTURE 9(3).                                         0158BP01
000760 03  NUM-4-REC-1     PICTURE S9(9)                  COMPUTATIONAL.0160BP01
000770 03  NUM-5-REC-1   PICTURE 999 COMPUTATIONAL-3.                   0162BP01
000780 03  FILLER PICTURE 9(2).                                         0164BP01
000790 03  NUM-6-REC-1     PICTURE S9(18)                 COMPUTATIONAL.0166BP01
000800 02  FORMAT-2-REC-1 REDEFINES FORMAT-1-REC-1.                     0168BP01
000810 03  NUM-7-REC-1   PICTURE 9(15) DISPLAY.                         0170BP01
000820 03  NUM-8-REC-1  REDEFINES NUM-7-REC-1.                          0172BP01
000830 04  NUM-9-REC-1   PICTURE 9(7) DISPLAY.                          0174BP01
000840 04  NUM-10-REC-1   PICTURE 9(8)  DISPLAY.                        0176BP01
000850 04  NUM-11-REC-1 REDEFINES NUM-10-REC-1.                         0178BP01
000860 05  NUM-12-REC-1   PICTURE 9(4) DISPLAY.                         0180BP01
000870 05  NUM-13-REC-1   PICTURE 9(4) DISPLAY.                         0182BP01
000880 03  FILLER PICTURE 9(1).                                         0184BP01
000890 03  NUM-14-REC-1    PICTURE S9(18)                 COMPUTATIONAL.0186BP01
000900 02  FORMAT-3-REC-1   REDEFINES FORMAT-2-REC-1.                   0188BP01
000910 03  NUM-15-REC-1 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0190BP01
000920 FD  HOMO-WRITE-FILE1-FIXED                                       0192BP01
000930     RECORD CONTAINS 24 CHARACTERS                                0194BP01
000940     RECORDING MODE IS F                                          0196BP01
000950     LABEL RECORDS ARE OMITTED                                    0198BP01
000960     DATA RECORD IS REC-2.                                        0200BP01
000970 01  REC-2.                                                       0202BP01
000980 02  FORMAT-1-REC-2.                                              0204BP01
000990 03  NUM-1-REC-2 PICTURE 9 DISPLAY.                               0206BP01
001000 03  NUM-2-REC-2     PICTURE S9(4)                  COMPUTATIONAL.0208BP01
001010 03  NUM-3-REC-2 PICTURE 9 DISPLAY.                               0210BP01
001020 03  NUM-4-REC-2     PICTURE S9(9)                  COMPUTATIONAL.0212BP01
001030 03  NUM-5-REC-2   PICTURE 999 COMPUTATIONAL-3.                   0214BP01
001040 03  NUM-6-REC-2     PICTURE S9(18)                 COMPUTATIONAL.0216BP01
001050 02  FORMAT-2-REC-2 REDEFINES FORMAT-1-REC-2.                     0218BP01
001060 03  NUM-7-REC-2   PICTURE 9(15) DISPLAY.                         0220BP01
001070 03  NUM-8-REC-2  REDEFINES NUM-7-REC-2.                          0222BP01
001080 04  NUM-9-REC-2   PICTURE 9(7) DISPLAY.                          0224BP01
001090 04  NUM-10-REC-2   PICTURE 9(8)  DISPLAY.                        0226BP01
001100 04  NUM-11-REC-2 REDEFINES NUM-10-REC-2.                         0228BP01
001110 05  NUM-12-REC-2   PICTURE 9(4) DISPLAY.                         0230BP01
001120 05  NUM-13-REC-2   PICTURE 9(4) DISPLAY.                         0232BP01
001130 03  NUM-14-REC-2    PICTURE S9(18)                 COMPUTATIONAL.0234BP01
001140 02  FORMAT-3-REC-2   REDEFINES FORMAT-2-REC-2.                   0236BP01
001150 03  NUM-15-REC-2 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0238BP01
001160 FD  HETRO-READ-FILE1-EQUAL                                       0240BP01
001170     RECORD CONTAINS 24 CHARACTERS                                0242BP01
001180     RECORDING MODE IS F                                          0244BP01
001190     LABEL RECORDS ARE OMITTED                                    0246BP01
001200     DATA RECORDS ARE REC-3 REC-5.                                0248BP01
001210 01  REC-3.                                                       0250BP01
001220 02  FORMAT-1-REC-3.                                              0252BP01
001230 03  NUM-1-REC-3 PICTURE 9 DISPLAY.                               0254BP01
001240 03  FILLER PICTURE 9(1).                                         0256BP01
001250 03  NUM-2-REC-3     PICTURE S9(4)                  COMPUTATIONAL.0258BP01
001260 03  NUM-3-REC-3 PICTURE 9 DISPLAY.                               0260BP01
001270 03  FILLER PICTURE 9(3).                                         0262BP01
001280 03  NUM-4-REC-3     PICTURE S9(9)                  COMPUTATIONAL.0264BP01
001290 03  NUM-5-REC-3   PICTURE 999 COMPUTATIONAL-3.                   0266BP01
001300 03  FILLER PICTURE 9(2).                                         0268BP01
001310 03  NUM-6-REC-3     PICTURE S9(18)                 COMPUTATIONAL.0270BP01
001320 02  FORMAT-2-REC-3 REDEFINES FORMAT-1-REC-3.                     0272BP01
001330 03  NUM-7-REC-3   PICTURE 9(15) DISPLAY.                         0274BP01
001340 03  NUM-8-REC-3  REDEFINES NUM-7-REC-3.                          0276BP01
001350 04  NUM-9-REC-3   PICTURE 9(7) DISPLAY.                          0278BP01
001360 04  NUM-10-REC-3   PICTURE 9(8)  DISPLAY.                        0280BP01
001370 04  NUM-11-REC-3 REDEFINES NUM-10-REC-3.                         0282BP01
001380 05  NUM-12-REC-3   PICTURE 9(4) DISPLAY.                         0284BP01
001390 05  NUM-13-REC-3   PICTURE 9(4) DISPLAY.                         0286BP01
001400 03  FILLER PICTURE 9(1).                                         0288BP01
001410 03  NUM-14-REC-3    PICTURE S9(18)                 COMPUTATIONAL.0290BP01
001420 02  FORMAT-3-REC-3   REDEFINES FORMAT-2-REC-3.                   0292BP01
001430 03  NUM-15-REC-3 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0294BP01
001440 01  REC-5.                                                       0296BP01
001450 03  NUM-6-REC-5     PICTURE S9(18)                 COMPUTATIONAL.0298BP01
001460 03  NUM-5-REC-5   PICTURE 999 COMPUTATIONAL-3.                   0300BP01
001470 03  FILLER PICTURE 9(2).                                         0302BP01
001480 03  NUM-4-REC-5     PICTURE S9(9)                  COMPUTATIONAL.0304BP01
001490 03  NUM-3-REC-5 PICTURE 9 DISPLAY.                               0306BP01
001500 03  FILLER PICTURE 9(1).                                         0308BP01
001510 03  NUM-2-REC-5     PICTURE S9(4)                  COMPUTATIONAL.0310BP01
001520 03  NUM-1-REC-5 PICTURE 9(4) DISPLAY.                            0312BP01
001530 FD  HETRO-WRITE-FILE1-EQUAL                                      0314BP01
001540     RECORD CONTAINS 24 CHARACTERS                                0316BP01
001550     RECORDING MODE IS F                                          0318BP01
001560     LABEL RECORDS ARE OMITTED                                    0320BP01
001570     DATA RECORDS ARE REC-4 REC-6.                                0322BP01
001580 01  REC-4.                                                       0324BP01
001590 02  FORMAT-1-REC-4.                                              0326BP01
001600 03  NUM-1-REC-4 PICTURE 9 DISPLAY.                               0328BP01
001610 03  NUM-2-REC-4     PICTURE S9(4)                  COMPUTATIONAL.0330BP01
001620 03  NUM-3-REC-4 PICTURE 9 DISPLAY.                               0332BP01
001630 03  NUM-4-REC-4     PICTURE S9(9)                  COMPUTATIONAL.0334BP01
001640 03  NUM-5-REC-4   PICTURE 999 COMPUTATIONAL-3.                   0336BP01
001650 03  NUM-6-REC-4     PICTURE S9(18)                 COMPUTATIONAL.0338BP01
001660 02  FORMAT-2-REC-4 REDEFINES FORMAT-1-REC-4.                     0340BP01
001670 03  NUM-7-REC-4   PICTURE 9(15) DISPLAY.                         0342BP01
001680 03  NUM-8-REC-4  REDEFINES NUM-7-REC-4.                          0344BP01
001690 04  NUM-9-REC-4   PICTURE 9(7) DISPLAY.                          0346BP01
001700 04  NUM-10-REC-4   PICTURE 9(8)  DISPLAY.                        0348BP01
001710 04  NUM-11-REC-4 REDEFINES NUM-10-REC-4.                         0350BP01
001720 05  NUM-12-REC-4   PICTURE 9(4) DISPLAY.                         0352BP01
001730 05  NUM-13-REC-4   PICTURE 9(4) DISPLAY.                         0354BP01
001740 03  NUM-14-REC-4    PICTURE S9(18)                 COMPUTATIONAL.0356BP01
001750 02  FORMAT-3-REC-4   REDEFINES FORMAT-2-REC-4.                   0358BP01
001760 03  NUM-15-REC-4 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0360BP01
001770 01  REC-6.                                                       0362BP01
001780 03  NUM-6-REC-6     PICTURE S9(18)                 COMPUTATIONAL.0364BP01
001790 03  NUM-5-REC-6   PICTURE 999 COMPUTATIONAL-3.                   0366BP01
001800 03  NUM-4-REC-6     PICTURE S9(9)                  COMPUTATIONAL.0368BP01
001810 03  NUM-3-REC-6 PICTURE 9 DISPLAY.                               0370BP01
001820 03  NUM-2-REC-6     PICTURE S9(4)                  COMPUTATIONAL.0372BP01
001830 03  NUM-1-REC-6 PICTURE 9(4) DISPLAY.                            0374BP01
001840 FD  HETRO-READ-FILE1-DIFF                                        0376BP01
001850     RECORD CONTAINS 21 TO 24 CHARACTERS                          0378BP01
001860     LABEL RECORDS ARE OMITTED                                    0380BP01
001870     DATA RECORDS ARE REC-7 REC-9.                                0382BP01
001880 01  REC-7.                                                       0384BP01
001890 02  FORMAT-1-REC-7.                                              0386BP01
001900 03  NUM-1-REC-7 PICTURE 9 DISPLAY.                               0388BP01
001910 03  FILLER PICTURE 9(1).                                         0390BP01
001920 03  NUM-2-REC-7     PICTURE S9(4)                  COMPUTATIONAL.0392BP01
001930 03  NUM-3-REC-7 PICTURE 9 DISPLAY.                               0394BP01
001940 03  FILLER PICTURE 9(3).                                         0396BP01
001950 03  NUM-4-REC-7     PICTURE S9(9)                  COMPUTATIONAL.0398BP01
001960 03  NUM-5-REC-7   PICTURE 999 COMPUTATIONAL-3.                   0400BP01
001970 03  FILLER PICTURE 9(2).                                         0402BP01
001980 03  NUM-6-REC-7     PICTURE S9(18)                 COMPUTATIONAL.0404BP01
001990 02  FORMAT-2-REC-7 REDEFINES FORMAT-1-REC-7.                     0406BP01
002000 03  NUM-7-REC-7 PICTURE 9(15) DISPLAY.                           0408BP01
002010 03  NUM-8-REC-7  REDEFINES NUM-7-REC-7.                          0410BP01
002020 04  NUM-9-REC-7   PICTURE 9(7) DISPLAY.                          0412BP01
002030 04  NUM-10-REC-7   PICTURE 9(8)  DISPLAY.                        0414BP01
002040 04  NUM-11-REC-7 REDEFINES NUM-10-REC-7.                         0416BP01
002050 05  NUM-12-REC-7   PICTURE 9(4) DISPLAY.                         0418BP01
002060 05  NUM-13-REC-7   PICTURE 9(4) DISPLAY.                         0420BP01
002070 03  FILLER PICTURE 9(1).                                         0422BP01
002080 03  NUM-14-REC-7    PICTURE S9(18)                 COMPUTATIONAL.0424BP01
002090 02  FORMAT-3-REC-7   REDEFINES FORMAT-2-REC-7.                   0426BP01
002100 03  NUM-15-REC-7 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0428BP01
002110 01  REC-9.                                                       0430BP01
002120 03  NUM-6-REC-9     PICTURE S9(18)                 COMPUTATIONAL.0432BP01
002130 03  NUM-5-REC-9   PICTURE 999 COMPUTATIONAL-3.                   0434BP01
002140 03  FILLER PICTURE 9(2).                                         0436BP01
002150 03  NUM-4-REC-9     PICTURE S9(9)                  COMPUTATIONAL.0438BP01
002160 03  NUM-3-REC-9 PICTURE 9 DISPLAY.                               0440BP01
002170 03  FILLER PICTURE 9(1).                                         0442BP01
002180 03  NUM-2-REC-9     PICTURE S9(4)                  COMPUTATIONAL.0444BP01
002190 03  NUM-1-REC-9 PICTURE 9 DISPLAY.                               0446BP01
002200 FD  HETRO-WRITE-FILE1-DIFF                                       0448BP01
002210     RECORD CONTAINS 21 TO 24 CHARACTERS                          0450BP01
002220     LABEL RECORDS ARE OMITTED                                    0452BP01
002230     DATA RECORDS ARE REC-8 REC-10.                               0454BP01
002240 01  REC-8.                                                       0456BP01
002250 02  FORMAT-1-REC-8.                                              0458BP01
002260 03  NUM-1-REC-8 PICTURE 9 DISPLAY.                               0460BP01
002270 03  NUM-2-REC-8     PICTURE S9(4)                  COMPUTATIONAL.0462BP01
002280 03  NUM-3-REC-8 PICTURE 9 DISPLAY.                               0464BP01
002290 03  NUM-4-REC-8     PICTURE S9(9)                  COMPUTATIONAL.0466BP01
002300 03  NUM-5-REC-8   PICTURE 999 COMPUTATIONAL-3.                   0468BP01
002310 03  NUM-6-REC-8     PICTURE S9(18)                 COMPUTATIONAL.0470BP01
002320 02  FORMAT-2-REC-8 REDEFINES FORMAT-1-REC-8.                     0472BP01
002330 03  NUM-7-REC-8   PICTURE 9(15) DISPLAY.                         0474BP01
002340 03  NUM-8-REC-8  REDEFINES NUM-7-REC-8.                          0476BP01
002350 04  NUM-9-REC-8   PICTURE 9(7) DISPLAY.                          0478BP01
002360 04  NUM-10-REC-8   PICTURE 9(8)  DISPLAY.                        0480BP01
002370 04  NUM-11-REC-8 REDEFINES NUM-10-REC-8.                         0482BP01
002380 05  NUM-12-REC-8   PICTURE 9(4) DISPLAY.                         0484BP01
002390 05  NUM-13-REC-8   PICTURE 9(4) DISPLAY.                         0486BP01
002400 03  NUM-14-REC-8    PICTURE S9(18)                 COMPUTATIONAL.0488BP01
002410 02  FORMAT-3-REC-8   REDEFINES FORMAT-2-REC-8.                   0490BP01
002420 03  NUM-15-REC-8 OCCURS 24 TIMES PICTURE   9 DISPLAY.            0492BP01
002430 01  REC-10.                                                      0494BP01
002440 03  NUM-6-REC-10    PICTURE S9(18)                 COMPUTATIONAL.0496BP01
002450 03  NUM-5-REC-10  PICTURE 999 COMPUTATIONAL-3.                   0498BP01
002460 03  NUM-4-REC-10    PICTURE S9(9)                  COMPUTATIONAL.0500BP01
002470 03  NUM-3-REC-10 PICTURE 9 DISPLAY.                              0502BP01
002480 03  NUM-2-REC-10    PICTURE S9(4)                  COMPUTATIONAL.0504BP01
002490 03  NUM-1-REC-10 PICTURE 9 DISPLAY.                              0506BP01
002500 WORKING-STORAGE SECTION.                                         0508BP01
       77 MESSAGE-1 PICTURE X(50) VALUE                                 0510
              'INFORMATION AS EXISTS IN BUFFERS / WORKING-STORAGE'.     0512
       77 MESSAGE-2 PICTURE X(41) VALUE                                 0514
              'RESULTS ARE FOLLOWED BY RESULTS SHOULD BE'.              0516
       77 MESSAGE-3 PICTURE X(48) VALUE                                 0518
              ' 0                 1                   2        '.       0520
       77 MESSAGE-4 PICTURE X(48) VALUE                                 0522
              ' 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4'.       0524
002510 77  SUB-1 PICTURE 99 VALUE 00 DISPLAY.                           0526BP01
002520 77  SUB-2 PICTURE 99 VALUE 00 DISPLAY.                           0528BP01
002530 77  ERROR1   PICTURE X(11) VALUE '********** ' DISPLAY.          0530BP01
       01 PARAGRAPH-NOS.                                                0532
        02 NAME-1 PICTURE X(10) VALUE 'PARAGRAPH-'.                     0534
        02 PARAGRAPH-NO PICTURE 99 VALUE ZERO.                          0536
002540 01  PRIMARY-DATA.                                                0538BP01
002550 02  NUMBER-1 PICTURE 9 VALUE 9 DISPLAY.                          0540BP01
002560 02  FILLER   PICTURE 9 VALUE 0 DISPLAY.                          0542BP01
002570 02  NUMBER-2        PICTURE S9(4) VALUE 9999       COMPUTATIONAL.0544BP01
002580 02  NUMBER-3 PICTURE 9 VALUE 9 DISPLAY.                          0546BP01
002590 02  FILLER   PICTURE 999 VALUE 000 DISPLAY.                      0548BP01
002600 02  NUMBER-4        PICTURE S9(9) VALUE 999999999  COMPUTATIONAL.0550BP01
002610 02  NUMBER-5 PICTURE 999  VALUE 999 COMPUTATIONAL-3.             0552BP01
002620 02  FILLER   PICTURE 99 VALUE 00 DISPLAY.                        0554BP01
002630 02  NUMBER-6        PICTURE S9(18) VALUE 999999999999999999      0556BP01
002640         COMPUTATIONAL.                                           0558BP01
002650 01  PRIMARY-DATA-1.                                              0560BP01
002660 02  NUMBER-7 PICTURE 9(15) VALUE 888888888888888 DISPLAY.        0562BP01
002670 02  FILLER   PICTURE 9 VALUE 0 DISPLAY.                          0564BP01
002680 02  NUMBER-14       PICTURE S9(18) VALUE 888888888888888888      0566BP01
002690         COMPUTATIONAL.                                           0568BP01
002700 01  PRIMARY-DATA-2.                                              0570BP01
002710 02  NUMBER-9  PICTURE 9(7)  VALUE 7777777  DISPLAY.              0572BP01
002720 02  NUMBER-10 PICTURE 9(8)  VALUE 77777777 DISPLAY.              0574BP01
002730 02  FILLER    PICTURE 9     VALUE 0        DISPLAY.              0576BP01
002740 02  NUMBER-14       PICTURE S9(18) VALUE 777777777777777777      0578BP01
002750         COMPUTATIONAL.                                           0580BP01
002760 01  PRIMARY-DATA-3.                                              0582BP01
002770 02  NUMBER-9  PICTURE 9(7)  VALUE 6666666  DISPLAY.              0584BP01
002780 02  NUMBER-12 PICTURE 9(4)  VALUE 6666     DISPLAY.              0586BP01
002790 02  NUMBER-13 PICTURE 9(4)  VALUE 6666     DISPLAY.              0588BP01
002800 02  FILLER    PICTURE 9     VALUE 0        DISPLAY.              0590BP01
002810 02  NUMBER-14       PICTURE S9(18) VALUE 666666666666666666      0592BP01
002820         COMPUTATIONAL.                                           0594BP01
002830 01  PRIMARY-DATA-4.                                              0596BP01
002840 02  NUMBER-06       PICTURE S9(18) VALUE 999999999999999999      0598BP01
002850         COMPUTATIONAL.                                           0600BP01
002860 02  NUMBER-05 PICTURE 999   VALUE 999 COMPUTATIONAL-3.           0602BP01
002870 02  FILLER    PICTURE 99 VALUE 00 DISPLAY.                       0604BP01
002880 02  NUMBER-04       PICTURE S9(9)  VALUE 999999999 COMPUTATIONAL.0606BP01
002890 02  NUMBER-03 PICTURE 9 VALUE 9  DISPLAY.                        0608BP01
002900 02  FILLER    PICTURE 9 VALUE 0  DISPLAY.                        0610BP01
002910 02  NUMBER-02       PICTURE S9(4)  VALUE 9999      COMPUTATIONAL.0612BP01
002920 02  NUMBER-01 PICTURE 9999 VALUE 9999 DISPLAY.                   0614BP01
002930 01  PRIMARY-DATA-5.                                              0616BP01
002940 02  NUMBER-06       PICTURE S9(18) VALUE 888888888888888888      0618BP01
002950         COMPUTATIONAL.                                           0620BP01
002960 02  NUMBER-05 PICTURE 999 VALUE 888 COMPUTATIONAL-3.             0622BP01
002970 02      FILLER    PICTURE 99 VALUE 00 DISPLAY.                   0624BP01
002980 02  NUMBER-04       PICTURE S9(9)  VALUE 888888888 COMPUTATIONAL.0626BP01
002990 02  NUMBER-03 PICTURE 9 VALUE 8 DISPLAY.                         0628BP01
003000 02  FILLER    PICTURE 9 VALUE 0 DISPLAY.                         0630BP01
003010 02  NUMBER-02       PICTURE S9(4)  VALUE 8888      COMPUTATIONAL.0632BP01
003020 02  NUMBER-01 PICTURE 9 VALUE 8 DISPLAY.                         0634BP01
003030 01  TEST-AREAS.                                                  0636BP01
003040 02  TEST-AREA-RD.                                                0638BP01
003050 03  TRD OCCURS 24 TIMES PICTURE X.                               0640BP01
003060 02  TEST-AREA-WT.                                                0642BP01
003070 03  TWT OCCURS 48 TIMES PICTURE X.                               0644BP01
003080 PROCEDURE DIVISION.                                              0646BP01
003090 DISPLAY-HEADER.                                                  0648BP01
003100     DISPLAY '          GROUP B LEVEL P TEST CASE 1'.             0650BP01
003110     DISPLAY ' '.                                                 0652BP01
003120 01-FDSLACKTEST.                                                  0654BP01
003130     PERFORM 31-OPEN-OUT-FILES.                                   0656BP01
       PARAGRAPH-01.                                                    0658
003140     PERFORM 50-SETUP-FORMAT-1-REC-2-4-8 THRU 51-EXIT.            0660BP01
           IF FORMAT-1-REC-2 NOT = PRIMARY-DATA   PERFORM 62-MESSAGE    0662
           MOVE FORMAT-1-REC-2 TO TEST-AREA-RD PERFORM                  0664
           27-PRINT-ROUTINE THRU 28-EXIT MOVE PRIMARY-DATA    TO        0666
           TEST-AREA-RD PERFORM 27-PRINT-ROUTINE THRU 28-EXIT           0668
           ELSE NEXT SENTENCE.                                          0670
       PARAGRAPH-02.                                                    0672
003170     PERFORM 52-SETUP-FORMAT-2-RD THRU 53-EXIT.                   0674BP01
           IF FORMAT-2-REC-2 NOT = PRIMARY-DATA-1 PERFORM 62-MESSAGE    0676
           MOVE FORMAT-2-REC-2 TO TEST-AREA-RD PERFORM                  0678
           27-PRINT-ROUTINE THRU 28-EXIT MOVE PRIMARY-DATA-1  TO        0680
           TEST-AREA-RD PERFORM 27-PRINT-ROUTINE THRU 28-EXIT           0682
           ELSE NEXT SENTENCE.                                          0684
       PARAGRAPH-03.                                                    0686
           PERFORM 54-SETUP-FORMAT-2-RD2 THRU 55-EXIT.                  0688
           IF FORMAT-2-REC-2 NOT = PRIMARY-DATA-2 PERFORM 62-MESSAGE    0690
           MOVE FORMAT-2-REC-2 TO TEST-AREA-RD PERFORM                  0692
           27-PRINT-ROUTINE THRU 28-EXIT MOVE PRIMARY-DATA-2  TO        0694
           TEST-AREA-RD PERFORM 27-PRINT-ROUTINE THRU 28-EXIT           0696
           ELSE NEXT SENTENCE.                                          0698
       PARAGRAPH-04.                                                    0700
003230     PERFORM 56-SETUP-FORMAT-3-RD3 THRU 57-EXIT.                  0702BP01
           IF FORMAT-2-REC-2 NOT = PRIMARY-DATA-3 PERFORM 62-MESSAGE    0704
           MOVE FORMAT-2-REC-2 TO TEST-AREA-RD PERFORM                  0706
           27-PRINT-ROUTINE THRU 28-EXIT MOVE PRIMARY-DATA-3  TO        0708
           TEST-AREA-RD PERFORM 27-PRINT-ROUTINE THRU 28-EXIT           0710
           ELSE NEXT SENTENCE.                                          0712
003260     PERFORM 33-CLOSE-OUT-FILES.                                  0714BP01
003270 02-FORMAT-1-REC-1-2-CHK.                                         0716BP01
003280     OPEN OUTPUT HOMO-WRITE-FILE1-FIXED.                          0718BP01
003290     WRITE REC-2 FROM PRIMARY-DATA.                               0720BP01
003300     CLOSE HOMO-WRITE-FILE1-FIXED.                                0722BP01
003310     OPEN INPUT HOMO-READ-FILE1-FIXED.                            0724BP01
003320     READ HOMO-READ-FILE1-FIXED AT END GO TO 11-EARLY-END-FILE.   0726BP01
003340     IF FORMAT-1-REC-1 EQUAL TO PRIMARY-DATA NEXT SENTENCE ELSE   0728BP01
           PERFORM 19-HOMO-FD-ERROR.                                    0730
003330     CLOSE HOMO-READ-FILE1-FIXED.                                 0732BP01
003360 03-FORMAT-2-RD-CHK.                                              0734BP01
003370     OPEN OUTPUT HOMO-WRITE-FILE1-FIXED.                          0736BP01
003380     WRITE REC-2 FROM PRIMARY-DATA-1.                             0738BP01
003390     CLOSE HOMO-WRITE-FILE1-FIXED.                                0740BP01
003400     OPEN INPUT HOMO-READ-FILE1-FIXED.                            0742BP01
003410     READ HOMO-READ-FILE1-FIXED AT END GO TO 11-EARLY-END-FILE.   0744BP01
003430     IF FORMAT-2-REC-1 EQUAL TO PRIMARY-DATA-1 NEXT SENTENCE ELSE 0746BP01
           PERFORM 20-HOMO-FD-ERROR.                                    0748
003420     CLOSE HOMO-READ-FILE1-FIXED.                                 0750BP01
003450 04-FORMAT-2-RD2-CHK.                                             0752BP01
003460     OPEN OUTPUT HOMO-WRITE-FILE1-FIXED.                          0754BP01
003470     WRITE REC-2 FROM PRIMARY-DATA-2.                             0756BP01
003480     CLOSE HOMO-WRITE-FILE1-FIXED.                                0758BP01
003490     OPEN INPUT  HOMO-READ-FILE1-FIXED.                           0760BP01
003500     READ HOMO-READ-FILE1-FIXED AT END GO TO 11-EARLY-END-FILE.   0762BP01
003520     IF FORMAT-2-REC-1 EQUAL TO PRIMARY-DATA-2 NEXT SENTENCE ELSE 0764BP01
           PERFORM 21-HOMO-FD-ERROR.                                    0766
003510     CLOSE HOMO-READ-FILE1-FIXED.                                 0768BP01
003540 05-FORMAT-2-RD3-CHK.                                             0770BP01
003550     OPEN OUTPUT HOMO-WRITE-FILE1-FIXED.                          0772BP01
003560     WRITE REC-2 FROM PRIMARY-DATA-3.                             0774BP01
003570     CLOSE HOMO-WRITE-FILE1-FIXED.                                0776BP01
003580     OPEN  INPUT HOMO-READ-FILE1-FIXED.                           0778BP01
003590     READ  HOMO-READ-FILE1-FIXED AT END GO TO 11-EARLY-END-FILE.  0780BP01
003610     IF FORMAT-2-REC-1 EQUAL TO PRIMARY-DATA-3 NEXT SENTENCE ELSE 0782BP01
           PERFORM 22-HOMO-FD-ERROR.                                    0784
003600     CLOSE HOMO-READ-FILE1-FIXED.                                 0786BP01
003630 06-RD-IMP-EQU-REC-CHK.                                           0788BP01
003640     PERFORM 31-OPEN-OUT-FILES.                                   0790BP01
003650     WRITE REC-4 FROM PRIMARY-DATA.                               0792BP01
003660     WRITE REC-4 FROM PRIMARY-DATA-1.                             0794BP01
003670     WRITE REC-4 FROM PRIMARY-DATA-2.                             0796BP01
003680     WRITE REC-4 FROM PRIMARY-DATA-3.                             0798BP01
003690      WRITE REC-6 FROM PRIMARY-DATA-4.                            0800BP01
003700     PERFORM 33-CLOSE-OUT-FILES.                                  0802BP01
003710     PERFORM 31-OPEN-IN-FILES.                                    0804BP01
003720     READ HETRO-READ-FILE1-EQUAL AT END GO TO 12-EARLY-END-FILE.  0806BP01
           IF FORMAT-1-REC-3 NOT = PRIMARY-DATA                         0808
004950     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED'            0810
           PERFORM 63-MESSAGE                                           0812
           MOVE FORMAT-1-REC-3 TO TEST-AREA-RD PERFORM 27-PRINT-ROUTINE 0814
           THRU 28-EXIT MOVE PRIMARY-DATA   TO TEST-AREA-RD PERFORM     0816
           27-PRINT-ROUTINE THRU 28-EXIT ELSE NEXT SENTENCE.            0818
003750     READ HETRO-READ-FILE1-EQUAL AT END GO TO 12-EARLY-END-FILE.  0820BP01
           IF FORMAT-2-REC-3 NOT = PRIMARY-DATA-1                       0822
004950     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED'            0824
           PERFORM 63-MESSAGE                                           0826
           MOVE FORMAT-2-REC-3 TO TEST-AREA-RD PERFORM 27-PRINT-ROUTINE 0828
           THRU 28-EXIT MOVE PRIMARY-DATA-1 TO TEST-AREA-RD PERFORM     0830
           27-PRINT-ROUTINE THRU 28-EXIT ELSE NEXT SENTENCE.            0832
003780     READ HETRO-READ-FILE1-EQUAL AT END GO TO 12-EARLY-END-FILE.  0834BP01
           IF FORMAT-2-REC-3 NOT = PRIMARY-DATA-2                       0836
004950     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED'            0838
           PERFORM 63-MESSAGE                                           0840
           MOVE FORMAT-2-REC-3 TO TEST-AREA-RD PERFORM 27-PRINT-ROUTINE 0842
           THRU 28-EXIT MOVE PRIMARY-DATA-2 TO TEST-AREA-RD PERFORM     0844
           27-PRINT-ROUTINE THRU 28-EXIT ELSE NEXT SENTENCE.            0846
003810     READ HETRO-READ-FILE1-EQUAL AT END GO TO 12-EARLY-END-FILE.  0848BP01
           IF FORMAT-2-REC-3 NOT = PRIMARY-DATA-3                       0850
004950     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED'            0852
           PERFORM 63-MESSAGE                                           0854
           MOVE FORMAT-2-REC-3 TO TEST-AREA-RD PERFORM 27-PRINT-ROUTINE 0856
           THRU 28-EXIT MOVE PRIMARY-DATA-3 TO TEST-AREA-RD PERFORM     0858
           27-PRINT-ROUTINE THRU 28-EXIT ELSE NEXT SENTENCE.            0860
003840     READ HETRO-READ-FILE1-EQUAL AT END GO TO 12-EARLY-END-FILE.  0862BP01
           IF REC-5          NOT = PRIMARY-DATA-4                       0864
004950     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED'            0866
           PERFORM 63-MESSAGE                                           0868
           MOVE REC-5          TO TEST-AREA-RD PERFORM 27-PRINT-ROUTINE 0870
           THRU 28-EXIT MOVE PRIMARY-DATA-4 TO TEST-AREA-RD PERFORM     0872
           27-PRINT-ROUTINE THRU 28-EXIT ELSE NEXT SENTENCE.            0874
003870     READ HETRO-READ-FILE1-EQUAL AT END GO TO 07-CLOSE-HETRO.     0876BP01
003880     GO TO 13-NO-END-FILE.                                        0878BP01
003890 07-CLOSE-HETRO.                                                  0880BP01
003900     PERFORM 33-CLOSE-IN-FILES.                                   0882BP01
003910 08-RD-IMP-DIFF-REC-CHK.                                          0884BP01
003920     PERFORM 31-OPEN-OUT-FILES.                                   0886BP01
003930     PERFORM 50-SETUP-FORMAT-1-REC-2-4-8 THRU 51-EXIT.            0888BP01
003940     WRITE REC-8.                                                 0890BP01
003950     WRITE REC-8 FROM PRIMARY-DATA-1.                             0892BP01
003960     WRITE REC-8 FROM PRIMARY-DATA-2.                             0894BP01
003970     PERFORM 56-SETUP-FORMAT-3-RD3.                               0896BP01
003980     WRITE REC-8.                                                 0898BP01
003990     WRITE REC-10 FROM PRIMARY-DATA-5.                            0900BP01
004000     PERFORM 60-SETUP-REC-10-DIFF.                                0902BP01
004010     WRITE REC-10.                                                0904BP01
004020     PERFORM 33-CLOSE-OUT-FILES.                                  0906BP01
004030     PERFORM 31-OPEN-IN-FILES.                                    0908BP01
004040     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0910BP01
004050     IF FORMAT-1-REC-7 EQUAL TO PRIMARY-DATA NEXT SENTENCE ELSE   0912BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0914
           PERFORM 63-MESSAGE                                           0916
           MOVE FORMAT-1-REC-7 TO TEST-AREA-RD                          0918
           PERFORM 24-HETRO-DIFF-ERROR                                  0920
           MOVE PRIMARY-DATA   TO TEST-AREA-RD                          0922
           PERFORM 24-HETRO-DIFF-ERROR.                                 0924
004070     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0926BP01
004080     IF FORMAT-2-REC-7 EQUAL TO PRIMARY-DATA-1 NEXT SENTENCE ELSE 0928BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0930
           PERFORM 63-MESSAGE                                           0932
           MOVE FORMAT-2-REC-7 TO TEST-AREA-RD                          0934
           PERFORM 24-HETRO-DIFF-ERROR                                  0936
           MOVE PRIMARY-DATA-1 TO TEST-AREA-RD                          0938
           PERFORM 24-HETRO-DIFF-ERROR.                                 0940
004100     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0942BP01
004110     IF FORMAT-2-REC-7 EQUAL TO PRIMARY-DATA-2 NEXT SENTENCE ELSE 0944BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0946
           PERFORM 63-MESSAGE                                           0948
           MOVE FORMAT-2-REC-7 TO TEST-AREA-RD                          0950
           PERFORM 24-HETRO-DIFF-ERROR                                  0952
           MOVE PRIMARY-DATA-2 TO TEST-AREA-RD                          0954
           PERFORM 24-HETRO-DIFF-ERROR.                                 0956
004130     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0958BP01
004140     IF FORMAT-2-REC-7 EQUAL TO PRIMARY-DATA-3 NEXT SENTENCE ELSE 0960BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0962
           PERFORM 63-MESSAGE                                           0964
           MOVE FORMAT-2-REC-7 TO TEST-AREA-RD                          0966
           PERFORM 24-HETRO-DIFF-ERROR                                  0968
           MOVE PRIMARY-DATA-3 TO TEST-AREA-RD                          0970
           PERFORM 24-HETRO-DIFF-ERROR.                                 0972
004160     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0974BP01
004170     IF REC-9 EQUAL TO PRIMARY-DATA-5 NEXT SENTENCE ELSE          0976BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0978
           PERFORM 63-MESSAGE                                           0980
           MOVE REC-9          TO TEST-AREA-RD                          0982
           PERFORM 24-HETRO-DIFF-ERROR                                  0984
           MOVE PRIMARY-DATA-5 TO TEST-AREA-RD                          0986
           PERFORM 24-HETRO-DIFF-ERROR.                                 0988
004190     READ HETRO-READ-FILE1-DIFF AT END GO TO 15-EARLY-END-FILE.   0990BP01
004200     IF REC-9 EQUAL TO PRIMARY-DATA-5 NEXT SENTENCE ELSE          0992BP01
           DISPLAY ERROR1 '08-RD-IMP-DIFF-REC-CHK FAILED'               0994
           PERFORM 63-MESSAGE                                           0996
           MOVE REC-9          TO TEST-AREA-RD                          0998
           PERFORM 24-HETRO-DIFF-ERROR                                  1000
           MOVE PRIMARY-DATA-5 TO TEST-AREA-RD                          1002
           PERFORM 24-HETRO-DIFF-ERROR.                                 1004
004220     READ HETRO-READ-FILE1-DIFF AT END GO TO 09-CLOSE-HETRO.      1006BP01
004230     GO TO 16-NO-END-FILE.                                        1008BP01
004240 09-CLOSE-HETRO.                                                  1010BP01
004250     PERFORM 33-CLOSE-IN-FILES.                                   1012BP01
004260 10-END-PROGRAM.                                                  1014BP01
004270     DISPLAY '          END OF PROGRAM'.                          1016BP01
004280     STOP RUN.                                                    1018BP01
004290 11-EARLY-END-FILE.                                               1020BP01
004300     DISPLAY ERROR1     '02-FORMAT-1-REC-2-CHK FAILED DUE TO EARLY1022BP01
004310     ' END OF FILE'.                                              1024BP01
004320     CLOSE HOMO-READ-FILE1-FIXED.                                 1026BP01
004330     GO TO 10-END-PROGRAM.                                        1028BP01
004340 12-EARLY-END-FILE.                                               1030BP01
004350     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED DUE TO EARLY1032BP01
004360     ' END OF FILE'.                                              1034BP01
004370     PERFORM 33-CLOSE-IN-FILES.                                   1036BP01
004380     GO TO 10-END-PROGRAM.                                        1038BP01
004390 13-NO-END-FILE.                                                  1040BP01
004400     DISPLAY ERROR1     '06-RD-IMP-EQU-REC-CHK FAILED DUE TO NO EN1042BP01
004410     'D OF FILE'.                                                 1044BP01
004420     PERFORM 33-CLOSE-IN-FILES.                                   1046BP01
004430     GO TO 10-END-PROGRAM.                                        1048BP01
004440 15-EARLY-END-FILE.                                               1050BP01
004450     DISPLAY ERROR1     '08-RD-IMP-DIFF-REC-CHK FAILED DUE TO EARL1052BP01
004460     'Y END OF FILE'.                                             1054BP01
004470     PERFORM 33-CLOSE-IN-FILES.                                   1056BP01
004480     GO TO 10-END-PROGRAM.                                        1058BP01
004490 16-NO-END-FILE.                                                  1060BP01
004500     DISPLAY ERROR1     '08-RD-IMP-DIFF-REC-CHK FAILED DUE TO NO E1062BP01
004510     'ND OF FILE'.                                                1064BP01
004520     PERFORM 33-CLOSE-IN-FILES.                                   1066BP01
004530     GO TO 10-END-PROGRAM.                                        1068BP01
004660 19-HOMO-FD-ERROR.                                                1070BP01
004670     DISPLAY ERROR1     '02-FORMAT-1-REC-1-2-CHK FAILED'.         1072BP01
           PERFORM 63-MESSAGE.                                          1074
004680     MOVE FORMAT-1-REC-1 TO TEST-AREA-RD.                         1076BP01
                               PERFORM 27-PRINT-ROUTINE THRU 28-EXIT.   1078
           MOVE PRIMARY-DATA   TO TEST-AREA-RD. PERFORM 27-PRINT-ROUTINE1080
           THRU 28-EXIT.                                                1082
004710 20-HOMO-FD-ERROR.                                                1084BP01
004720     DISPLAY ERROR1     '03-FORMAT-2-RD-CHK FAILED'.              1086BP01
           PERFORM 63-MESSAGE.                                          1088
004740     MOVE FORMAT-2-REC-1 TO TEST-AREA-RD.                         1090BP01
                               PERFORM 27-PRINT-ROUTINE THRU 28-EXIT.   1092
           MOVE PRIMARY-DATA-1 TO TEST-AREA-RD. PERFORM 27-PRINT-ROUTINE1094
           THRU 28-EXIT.                                                1096
004760 21-HOMO-FD-ERROR.                                                1098BP01
004770     DISPLAY ERROR1     '04-FORMAT-2-RD2-CHK FAILED'.             1100BP01
           PERFORM 63-MESSAGE.                                          1102
           MOVE FORMAT-2-REC-1 TO TEST-AREA-RD.                         1104
                               PERFORM 27-PRINT-ROUTINE THRU 28-EXIT.   1106
           MOVE PRIMARY-DATA-2 TO TEST-AREA-RD. PERFORM 27-PRINT-ROUTINE1108
           THRU 28-EXIT.                                                1110
004790 22-HOMO-FD-ERROR.                                                1112BP01
004800     DISPLAY ERROR1     '05-FORMAT-2-RD3-CHK FAILED'.             1114BP01
           PERFORM 63-MESSAGE.                                          1116
           MOVE FORMAT-2-REC-1 TO TEST-AREA-RD.                         1118
                               PERFORM 27-PRINT-ROUTINE THRU 28-EXIT.   1120
           MOVE PRIMARY-DATA-3 TO TEST-AREA-RD. PERFORM 27-PRINT-ROUTINE1122
           THRU 28-EXIT.                                                1124
005000 24-HETRO-DIFF-ERROR.                                             1126BP01
005040     PERFORM 27-PRINT-ROUTINE.                                    1128BP01
005150 27-PRINT-ROUTINE.                                                1130BP01
005160     MOVE SPACES TO TEST-AREA-WT.                                 1132BP01
005190     MOVE 1 TO SUB-1.                                             1134BP01
005200     MOVE 2 TO SUB-2.                                             1136BP01
005210     PERFORM 29-PRINT-ROUTINE THRU 30-EXIT 24 TIMES.              1138BP01
005220     DISPLAY TEST-AREA-WT.                                        1140BP01
005230 28-EXIT.                                                         1142BP01
005240     EXIT.                                                        1144BP01
005250 29-PRINT-ROUTINE.                                                1146BP01
005260     MOVE TRD (SUB-1) TO TWT (SUB-2).                             1148BP01
005270     ADD 1 SUB-1 GIVING SUB-1.                                    1150BP01
005280     ADD 2 SUB-2 GIVING SUB-2.                                    1152BP01
005290 30-EXIT.                                                         1154BP01
005300     EXIT.                                                        1156BP01
005310 31-OPEN-OUT-FILES.                                               1158BP01
005320     OPEN OUTPUT HOMO-WRITE-FILE1-FIXED HETRO-WRITE-FILE1-EQUAL   1160BP01
005330     HETRO-WRITE-FILE1-DIFF.                                      1162BP01
005340 31A-EXIT.                                                        1164BP01
005350     EXIT.                                                        1166BP01
005360 31-OPEN-IN-FILES.                                                1168BP01
005370     OPEN INPUT HOMO-READ-FILE1-FIXED HETRO-READ-FILE1-EQUAL      1170BP01
005380     HETRO-READ-FILE1-DIFF.                                       1172BP01
005390 32-EXIT.                                                         1174BP01
005400     EXIT.                                                        1176BP01
005410 33-CLOSE-OUT-FILES.                                              1178BP01
005420     CLOSE                                                        1180BP01
005430     HOMO-WRITE-FILE1-FIXED                                       1182BP01
005440     HETRO-WRITE-FILE1-EQUAL                                      1184BP01
005450     HETRO-WRITE-FILE1-DIFF.                                      1186BP01
005460 33A-EXIT.                                                        1188BP01
005470     EXIT.                                                        1190BP01
005480 33-CLOSE-IN-FILES.                                               1192BP01
005490     CLOSE                                                        1194BP01
005500     HOMO-READ-FILE1-FIXED                                        1196BP01
005510     HETRO-READ-FILE1-EQUAL                                       1198BP01
005520     HETRO-READ-FILE1-DIFF.                                       1200BP01
005530 34-EXIT.                                                         1202BP01
005540     EXIT.                                                        1204BP01
005550 50-SETUP-FORMAT-1-REC-2-4-8.                                     1206BP01
005560     MOVE ZERO TO REC-2 REC-4 REC-8.                              1208BP01
005570     MOVE NUMBER-1 TO NUM-1-REC-2 NUM-1-REC-4 NUM-1-REC-8.        1210BP01
005580     MOVE NUMBER-2 TO NUM-2-REC-2 NUM-2-REC-4 NUM-2-REC-8.        1212BP01
005590     MOVE NUMBER-3 TO NUM-3-REC-2 NUM-3-REC-4 NUM-3-REC-8.        1214BP01
005600     MOVE NUMBER-4 TO NUM-4-REC-2 NUM-4-REC-4 NUM-4-REC-8.        1216BP01
005610     MOVE NUMBER-5 TO NUM-5-REC-2 NUM-5-REC-4 NUM-5-REC-8.        1218BP01
005620     MOVE NUMBER-6 TO NUM-6-REC-2 NUM-6-REC-4 NUM-6-REC-8.        1220BP01
005630 51-EXIT.                                                         1222BP01
005640     EXIT.                                                        1224BP01
005650 52-SETUP-FORMAT-2-RD.                                            1226BP01
005660     MOVE ZERO TO REC-2 REC-4 REC-8.                              1228BP01
005670     MOVE NUMBER-7  TO NUM-7-REC-2 NUM-7-REC-4 NUM-7-REC-8.       1230BP01
005680     MOVE NUMBER-14 IN PRIMARY-DATA-1 TO NUM-14-REC-2             1232BP01
005690     NUM-14-REC-4 NUM-14-REC-8.                                   1234BP01
005700 53-EXIT.                                                         1236BP01
005710     EXIT.                                                        1238BP01
005720 54-SETUP-FORMAT-2-RD2.                                           1240BP01
005730     MOVE ZERO TO REC-2 REC-4 REC-8.                              1242BP01
005740     MOVE NUMBER-10 TO NUM-10-REC-2 NUM-10-REC-4 NUM-10-REC-8.    1244BP01
005750     MOVE NUMBER-14 IN PRIMARY-DATA-2 TO                          1246BP01
005760     NUM-14-REC-2 NUM-14-REC-4 NUM-14-REC-8.                      1248BP01
005770     MOVE NUMBER-9 IN PRIMARY-DATA-2 TO                           1250BP01
005780     NUM-9-REC-2 NUM-9-REC-4 NUM-9-REC-8.                         1252BP01
005790 55-EXIT.                                                         1254BP01
005800     EXIT.                                                        1256BP01
005810 56-SETUP-FORMAT-3-RD3.                                           1258BP01
005820     MOVE ZERO TO REC-2 REC-4 REC-8.                              1260BP01
005830     MOVE NUMBER-9 IN PRIMARY-DATA-3 TO                           1262BP01
005840     NUM-9-REC-2 NUM-9-REC-4 NUM-9-REC-8.                         1264BP01
005850     MOVE NUMBER-12 TO NUM-12-REC-2 NUM-12-REC-4 NUM-12-REC-8.    1266BP01
           MOVE NUMBER-13 TO NUM-13-REC-2 NUM-13-REC-4 NUM-13-REC-8.    1268
005880     MOVE NUMBER-14 IN PRIMARY-DATA-3 TO                          1270BP01
005890     NUM-14-REC-2 NUM-14-REC-4 NUM-14-REC-8.                      1272BP01
005900 57-EXIT.                                                         1274BP01
005910     EXIT.                                                        1276BP01
005920 58-SETUP-REC-6-EQU.                                              1278BP01
005930     MOVE ZERO TO REC-6.                                          1280BP01
005940     MOVE NUMBER-06 IN PRIMARY-DATA-4 TO NUM-6-REC-6.             1282BP01
005950     MOVE NUMBER-05 IN PRIMARY-DATA-4 TO NUM-5-REC-6.             1284BP01
005960     MOVE NUMBER-04 IN PRIMARY-DATA-4 TO NUM-4-REC-6.             1286BP01
005970     MOVE NUMBER-03 IN PRIMARY-DATA-4 TO NUM-3-REC-6.             1288BP01
005980     MOVE NUMBER-02 IN PRIMARY-DATA-4 TO NUM-2-REC-6.             1290BP01
005990     MOVE NUMBER-01 IN PRIMARY-DATA-4 TO NUM-1-REC-6.             1292BP01
006000 59-EXIT.                                                         1294BP01
006010     EXIT.                                                        1296BP01
006020 60-SETUP-REC-10-DIFF.                                            1298BP01
006030     MOVE ZERO TO REC-10.                                         1300BP01
006040     MOVE NUMBER-06 IN PRIMARY-DATA-5 TO NUM-6-REC-10.            1302BP01
006050     MOVE NUMBER-05 IN PRIMARY-DATA-5 TO NUM-5-REC-10.            1304BP01
006060     MOVE NUMBER-04 IN PRIMARY-DATA-5 TO NUM-4-REC-10.            1306BP01
006070     MOVE NUMBER-03 IN PRIMARY-DATA-5 TO NUM-3-REC-10.            1308BP01
006080     MOVE NUMBER-02 IN PRIMARY-DATA-5 TO NUM-2-REC-10.            1310BP01
006090     MOVE NUMBER-01 IN PRIMARY-DATA-5 TO NUM-1-REC-10.            1312BP01
006100 61-EXIT.                                                         1314BP01
006110     EXIT.                                                        1316BP01
       62-MESSAGE.                                                      1318
           ADD 1 TO PARAGRAPH-NO. DISPLAY ERROR1 '01-FDSLACKTEST FAILED'1320
           'IN' PARAGRAPH-NOS. PERFORM 63-MESSAGE.                      1322
       63-MESSAGE. DISPLAY MESSAGE-1. DISPLAY MESSAGE-2. DISPLAY        1324
           ' '. DISPLAY MESSAGE-3. DISPLAY MESSAGE-4.                   1326
/*                                                                      1328
//GO.SYSOUT DD SYSOUT=A,DCB=(,BLKSIZE=120,LRECL=120)                    1330
//GO.SYS004    DD  UNIT=183,LABEL=(,NL),VOLUME=SER=BBBBBB               1332
//GO.SYS005    DD  UNIT=182,LABEL=(,NL),VOLUME=SER=AAAAAA               1334
//GO.SYS006    DD  UNIT=282,LABEL=(,NL),VOLUME=SER=CCCCCC               1336
/*
//