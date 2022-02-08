//IKFSAMP JOB   CB545,COBOL,MSGLEVEL=1                                  01000021
// EXEC COBUCLG,PARM.COB='QUOTE'                                        02000021
//COB.SYSIN DD *                                                        03000021
100010 IDENTIFICATION DIVISION.                                         04000021
100020 PROGRAM-ID.  TESTRUN.                                            05000021
100030   AUTHOR. PROGRAMMER NAME.                                       06000021
100040   INSTALLATION. NEW YORK PROGRAMMING CENTER.                     07000021
100050   DATE-WRITTEN. JULY 12, 1968.                                   08000021
100060   DATE-COMPILED.                                                 09000021
100070   REMARKS. THIS PROGRAM HAS BEEN WRITTEN AS A SAMPLE PROGRAM FOR 10000021
100080     COBOL USERS. IT CREATES AN OUTPUT FILE AND READS IT BACK AS  11000021
100090     INPUT.                                                       12000021
100100 ENVIRONMENT DIVISION.                                            13000021
100110 CONFIGURATION SECTION.                                           14000021
100120   SOURCE-COMPUTER. IBM-360-H50.                                  15000021
100130   OBJECT-COMPUTER. IBM-360-H50.                                  16000021
100131 SPECIAL-NAMES.
100132     CONSOLE IS CNSL.
100140 INPUT-OUTPUT SECTION.                                            17000021
100150 FILE-CONTROL.                                                    18000021
100160     SELECT FILE-1 ASSIGN TO UT-2400-S-SAMPLE.                    19000021
100170     SELECT FILE-2 ASSIGN TO UT-2400-S-SAMPLE.                    20000021
100180 DATA DIVISION.                                                   21000021
100190 FILE SECTION.                                                    22000021
100200 FD  FILE-1                                                       23000021
100210     LABEL RECORDS ARE OMITTED                                    24000021
100220     BLOCK CONTAINS 100 CHARACTERS                                25000021
100225     RECORD CONTAINS 20 CHARACTERS                                26000021
100230     RECORDING MODE IS F                                          27000021
100240     DATA RECORD IS RECORD-1.                                     28000021
100250   01 RECORD-1.                                                   29000021
100260     02 FIELD-A PICTURE IS X(20).                                 30000021
100270 FD  FILE-2                                                       31000021
100280     LABEL RECORDS ARE OMITTED                                    32000021
100290     BLOCK CONTAINS 5 RECORDS                                     33000021
100300     RECORD CONTAINS 20 CHARACTERS                                34000021
100310     RECORDING MODE IS F                                          35000021
100320     DATA RECORD IS RECORD-2.                                     36000021
100330   01 RECORD-2.                                                   37000021
100340     02 FIELD-A PICTURE IS X(20).                                 38000021
100350 WORKING-STORAGE SECTION.                                         39000021
100351     77 HOLA1 PICTURE X(5) VALUE "Hola1".
100352     77 HOLA2 PICTURE X(5) VALUE "Hola2".
100353     77 HOLA3 PICTURE X(5) VALUE "Hola3".
100354     77 HOLA4 PICTURE X(5) VALUE "Hola4".
100355     77 HOLA5 PICTURE X(5) VALUE "Hola5".
100360     77 COUNT PICTURE S99 COMP SYNC.                              40000021
100370     77 NOMBER  PICTURE S99 COMP SYNC.                            41000021
100375   01 FILLER.                                                     42000021
100380     02 ALPHABET PICTURE X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".43000021
100395     02 ALPHA REDEFINES ALPHABET PICTURE X OCCURS 26 TIMES.       44000021
100405     02 DEPENDENTS PICTURE X(26)  VALUE "012340123401234012340123445000021
100410-    "0".                                                         46000021
100420     02 DEPEND REDEFINES DEPENDENTS PICTURE X OCCURS 26 TIMES.    47000021
100440   01 WORK-RECORD.                                                48000021
100450     02 NAME-FIELD  PICTURE X.                                    49000021
100460     02 FILLER PICTURE X VALUE SPACE.                             50000021
100470     02 RECORD-NO PICTURE 9999.                                   51000021
100480     02 FILLER PICTURE X VALUE SPACE.                             52000021
100490     02 LOCATION PICTURE AAA VALUE "NYC".                         53000021
100500     02 FILLER PICTURE X VALUE SPACE.                             54000021
100510     02 NO-OF-DEPENDENTS PICTURE XX.                              55000021
100520     02 FILLER PICTURE X(7) VALUE SPACES.                         56000021
100530 PROCEDURE DIVISION.                                              57000021
100540 BEGIN. READY TRACE.                                              58000021
100550     NOTE THAT THE FOLLOWING OPENS THE OUTPUT FILE TO BE CREATED  59000021
100560     AND INITIALIZES COUNTERS.                                    60000021
100561     DISPLAY HOLA1 UPON CNSL.
100570 STEP-1. OPEN OUTPUT FILE-1. MOVE ZERO TO COUNT NOMBER.           61000021
100571     DISPLAY HOLA2 UPON CNSL.
100580     NOTE THAT THE FOLLOWING CREATES INTERNALLY THE RECORDS TO BE 62000021
100590     CONTAINED IN THE FILE, WRITES THEM ON TAPE, AND DISPLAYS     63000021
100600     THEM ON THE CONSOLE.                                         64000021
100610 STEP-2. ADD 1 TO COUNT, ADD 1 TO NOMBER, MOVE ALPHA (COUNT) TO   65000021
100620     NAME-FIELD.                                                  66000021
100630     MOVE DEPEND (COUNT) TO NO-OF-DEPENDENTS.                     67000021
100640     MOVE NOMBER TO RECORD-NO.                                    68000021
100650 STEP-3. DISPLAY WORK-RECORD UPON CONSOLE. WRITE RECORD-1 FROM    69000021
100660     WORK-RECORD.                                                 70000021
100670 STEP-4. PERFORM STEP-2 THRU STEP-3 UNTIL COUNT IS EQUAL TO 26.   71000021
100680     NOTE THAT THE FOLLOWING CLOSES OUTPUT AND REOPENS IT AS      72000021
100690     INPUT.                                                       73000021
100700 STEP-5. CLOSE FILE-1. OPEN INPUT FILE-2.                         74000021
100710     NOTE THAT THE FOLLOWING READS BACK THE FILE AND SINGLES OUT  75000021
100720     EMPLOYEES WITH NO DEPENDENTS.                                76000021
100730 STEP-6. READ FILE-2 RECORD INTO WORK-RECORD AT END GO TO STEP-8. 77000021
100740 STEP-7. IF NO-OF-DEPENDENTS IS EQUAL TO "0" MOVE "Z" TO          78000021
100750     NO-OF-DEPENDENTS. EXHIBIT NAMED WORK-RECORD. GO TO           79000021
100760     STEP-6.                                                      80000021
100770 STEP-8. CLOSE FILE-2.                                            81000021
100780     STOP RUN.                                                    82000021
//LKED.SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR
//            DD DSNAME=SYS1.LINKLIB,DISP=SHR
//SYSUT1    DD UNIT=SYSDA
//GO.SYSPRINT DD SYSOUT=A
//GO.SAMPLE DD   UNIT=TAPE,LABEL=(,NL)                                  83000021
//GO.SYSOUT DD   SYSOUT=A                                               84000021
//