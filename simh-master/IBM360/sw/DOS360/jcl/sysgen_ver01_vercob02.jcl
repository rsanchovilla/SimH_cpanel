* $$JOB VERCOB02,,,BG
// JOB VERCOB02
// OPTION LINK
// EXEC FCOBOL
010010 IDENTIFICATION DIVISION.                                         $4820001
010020 PROGRAM-ID. C360SAMP.                                            $4820002
010030 REMARKS.                                                         $4820003
010040     EXAMPLE OF A 360 FCOBOL PROGRAM                              $4820004
010050         TO COMPUTE SALARIES.                                     $4820005
020060 ENVIRONMENT DIVISION.                                            $4820006
020070 CONFIGURATION SECTION.                                           $4820007
020080   SOURCE-COMPUTER. IBM-360-F30.                                  $4820008
020090   OBJECT-COMPUTER. IBM-360-F30.                                  $4820009
020100 INPUT-OUTPUT SECTION.                                            $4820010
020110   FILE-CONTROL.                                                  $4820011
020120     SELECT SALARY-FILE                                           $4820012
020130         ASSIGN TO SYS004-UR-1403-S.                              $4820013
020140                                                                  $4820014
030150 DATA DIVISION.                                                   $4820015
030160 FILE SECTION.                                                    $4820016
030170   FD SALARY-FILE                                                 $4820017
030180     LABEL RECORDS ARE OMITTED                                    $4820018
030190         DATA RECORD IS SALARY-RECORD.                            $4820019
030200   01 SALARY-RECORD  PICTURE X(100).                              $4820020
031010 WORKING-STORAGE SECTION.                                         $4820021
031020   77 TOTAL-A      PIC 9(6)V99   VALUE ZERO.                      $4820022
031030   77 TOTAL-B      PIC 9(6)V99   VALUE ZERO.                      $4820023
031040   77 TOTAL-C      PIC 9(6)V99   VALUE ZERO.                      $4820024
031050   77 WEEKLY-PAY   PIC 999V99.                                    $4820025
031060   77 MONTHLY-PAY  PIC 9999V99.                                   $4820026
031070   77 ANNUAL-PAY   PIC 99999V99.                                  $4820027
031080   77 CON-A        PIC 9(6)V99   VALUE 008826.69.                 $4820028
031090   77 CON-B        PIC 9(6)V99   VALUE 038250.00.                 $4820029
031100   77 CON-C        PIC 9(6)V99   VALUE 459000.00.                 $4820030
031110   01 SALARIES.                                                   $4820031
031120      02 FILLER    PIC X(46)     VALUE SPACES.                    $4820032
031130      02 WEEKLY    PIC ZZZ.99.                                    $4820033
031140      02 FILLER    PIC X(3)      VALUE SPACES.                    $4820034
031150      02 MONTHLY   PIC ZZZZ.99.                                   $4820035
031160      02 FILLER    PIC X(3)      VALUE SPACES.                    $4820036
031170      02 ANNUAL    PIC ZZZZZ.99.                                  $4820037
031180      02 FILLER    PIC X(27)     VALUE SPACES.                    $4820038
031190   01 MESG.                                                       $4820039
031200      02 FILLER    PIC X(40)     VALUE SPACES.                    $4820040
032010      02 SHOW      PIC A(26).                                     $4820041
032015      02 FILLER PIC X(34) VALUE SPACES.                           $4820042
032020   01 DSPY.                                                       $4820043
032030      02 FILLER    PIC X(40)     VALUE SPACES.                    $4820044
032040      02 PRSNT     PIC A(33).                                     $4820045
032045      02 FILLER PIC X(27) VALUE SPACES.                           $4820046
032050   01 PG-HDING.                                                   $4820047
032060      02 FILLER    PIC X(46)     VALUE SPACES.                    $4820048
032070      02 WEEKLY    PIC A(6)      VALUE 'WEEKLY'.                  $4820049
032080      02 FILLER    PIC X(3)      VALUE SPACES.                    $4820050
032090      02 MONTHLY   PIC A(7)      VALUE 'MONTHLY'.                 $4820051
032100      02 FILLER    PIC X(3)      VALUE SPACES.                    $4820052
032110      02 ANNUAL    PIC A(6)      VALUE 'ANNUAL'.                  $4820053
032120      02 FILLER    PIC X(29)     VALUE SPACES.                    $4820054
040130                                                                  $4820055
040140 PROCEDURE DIVISION.                                              $4820056
040150   START-PROG.                                                    $4820057
040160     OPEN OUTPUT SALARY-FILE.                                     $4820058
040170     WRITE SALARY-RECORD FROM PG-HDING AFTER ADVANCING 0 LINES.   $4820059
040180     PERFORM CALCULATIONS                                         $4820060
040190       VARYING MONTHLY-PAY FROM 500 BY 10                         $4820061
040200       UNTIL MONTHLY-PAY IS GREATER THAN 1000.                    $4820062
041010     IF TOTAL-A = CON-A AND TOTAL-B = CON-B AND TOTAL-C = CON-C   $4820063
041020         MOVE 'TABLE VALUES ARE CORRECT' TO SHOW                  $4820064
041030         WRITE SALARY-RECORD FROM MESG AFTER ADVANCING 2 LINES    $4820065
041040       ELSE                                                       $4820066
041050         MOVE 'TABLE VALUES ARE NOT CORRECT' TO PRSNT             $4820067
041060         WRITE SALARY-RECORD FROM DSPY AFTER ADVANCING 2 LINES.   $4820068
041070     CLOSE SALARY-FILE.                                           $4820069
041080     STOP RUN.                                                    $4820070
041090   CALCULATIONS.                                                  $4820071
041100     COMPUTE WEEKLY-PAY = 3 * MONTHLY-PAY / 13.                   $4820072
041110     COMPUTE ANNUAL-PAY = 12 * MONTHLY-PAY.                       $4820073
041120     MOVE WEEKLY-PAY TO WEEKLY IN SALARIES.                       $4820074
041130     MOVE MONTHLY-PAY TO MONTHLY IN SALARIES.                     $4820075
041140     MOVE ANNUAL-PAY TO ANNUAL IN SALARIES.                       $4820076
041150     ADD WEEKLY-PAY TO TOTAL-A.                                   $4820077
041160     ADD MONTHLY-PAY TO TOTAL-B.                                  $4820078
041170     ADD ANNUAL-PAY TO TOTAL-C.                                   $4820079
041180     WRITE SALARY-RECORD FROM SALARIES AFTER ADVANCING 1 LINES.   $4820080
/*
// EXEC LNKEDT
// ASSGN SYSLST,UA
// ASSGN SYS004,X'00E'
// EXEC
/*
/&
* $$EOJ