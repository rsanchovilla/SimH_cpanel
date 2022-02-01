* $$JOB VERCOB01,,,BG
// JOB VERCOB01
// OPTION LINK
// EXEC COBOL
001010 IDENTIFICATION DIVISION.                                         $4520001
001020 PROGRAM-ID. 'C360SAMP'.                                          $4520002
001030 REMARKS.                                                         $4520003
001040     EXAMPLE OF A 360 COBOL PROGRAM TO                            $4520004
001050     COMPUTE SALARIES.                                            $4520005
001060 ENVIRONMENT DIVISION.                                            $4520006
001070 CONFIGURATION SECTION.                                           $4520007
001080   SOURCE-COMPUTER. IBM-360.                                      $4520008
001090   OBJECT-COMPUTER. IBM-360.                                      $4520009
001100 INPUT-OUTPUT SECTION.                                            $4520010
001110 FILE-CONTROL.                                                    $4520011
001120     SELECT SALARY-FILE                                           $4520012
001130     ASSIGN TO 'SYS004' UNIT-RECORD 1403.                         $4520013
001140                                                                  $4520014
001150 DATA DIVISION.                                                   $4520015
001160 FILE SECTION.                                                    $4520016
001170  FD SALARY-FILE                                                  $4520017
001180     LABEL RECORDS ARE OMITTED                                    $4520018
001185     RECORDING MODE IS F                                          $4520019
001190     DATA RECORD IS SALARY-RECORD.                                $4520020
001200  01 SALARY-RECORD                                                $4520021
001210     PICTURE X(100).                                              $4520022
001220 WORKING-STORAGE SECTION.                                         $4520023
001230  77 TOTAL-A      PICTURE 9(6)V99    VALUE ZERO.                  $4520024
001240  77 TOTAL-B      PICTURE 9(6)V99    VALUE ZERO.                  $4520025
001250  77 TOTAL-C      PICTURE 9(6)V99    VALUE ZERO.                  $4520026
002010  77 WEEKLY-PAY   PICTURE 999V99.                                 $4520027
002020  77 MONTHLY-PAY  PICTURE 9999V99.                                $4520028
002030  77 ANNUAL-PAY   PICTURE 99999V99.                               $4520029
002040  77 CON-A  PICTURE 9(6)V99    VALUE IS 008826.69.                $4520030
002050  77 CON-B  PICTURE 9(6)V99    VALUE IS 038250.00 .               $4520031
002060  77 CON-C  PICTURE 9(6)V99    VALUE IS 459000.00 .               $4520032
002070  01 SALARIES.                                                    $4520033
002080     02 FILLER    PICTURE A(46)  VALUE SPACE.                     $4520034
002090     02 WEEKLY    PICTURE ZZZ.99 .                                $4520035
002100     02 FILLER    PICTURE AAA    VALUE SPACE.                     $4520036
002110     02 MONTHLY   PICTURE ZZZZ.99 .                               $4520037
002120     02 FILLER    PICTURE AAA    VALUE SPACE.                     $4520038
002130     02 ANNUAL    PICTURE ZZZZZ.99 .                              $4520039
002140     02 FILLER    PICTURE A(27)  VALUE SPACE.                     $4520040
002150  01 MESG.                                                        $4520041
002160     02 FILLER    PICTURE A(40)  VALUE SPACES.                    $4520042
002170     02 SHOW      PICTURE A(26).                                  $4520043
002180  01 DSPY.                                                        $4520044
002190     02 FILLER    PICTURE A(40)  VALUE SPACES.                    $4520045
002200     02 PRSNT     PICTURE A(33).                                  $4520046
002210  01 HEADING.                                                     $4520047
002220     02 FILLER    PICTURE A(46)  VALUE SPACES.                    $4520048
002230     02 WEEKLY    PICTURE A(6)   VALUE IS 'WEEKLY'.               $4520049
002240     02 FILLER    PICTURE A(3)   VALUE IS SPACES.                 $4520050
002250     02 MONTHLY   PICTURE A(7)   VALUE IS 'MONTHLY'.              $4520051
003010     02 FILLER    PICTURE A(3)   VALUE IS SPACES.                 $4520052
003020     02 ANNUAL    PICTURE A(6)   VALUE IS 'ANNUAL'.               $4520053
003030     02 FILLER    PICTURE A(29)  VALUE IS SPACES.                 $4520054
003040                                                                  $4520055
003050 PROCEDURE DIVISION.                                              $4520056
003060 START.                                                           $4520057
003070     OPEN OUTPUT SALARY-FILE.                                     $4520058
003080     WRITE SALARY-RECORD FROM HEADING AFTER ADVANCING 0 LINES.    $4520059
003090     PERFORM CALCULATIONS                                         $4520060
003100       VARYING MONTHLY-PAY FROM 500 BY 10                         $4520061
003110       UNTIL MONTHLY-PAY IS GREATER THAN 1000.                    $4520062
003120       IF TOTAL-A = CON-A AND TOTAL-B = CON-B AND TOTAL-C = CON-C $4520063
003130       MOVE 'TABLE VALUES ARE CORRECT' TO SHOW                    $4520064
003140       WRITE SALARY-RECORD FROM MESG AFTER ADVANCING 2 LINES      $4520065
003150       ELSE                                                       $4520066
003160       MOVE 'TABLE VALUES ARE NOT CORRECT' TO PRSNT               $4520067
003170       WRITE SALARY-RECORD FROM DSPY AFTER ADVANCING 2 LINES.     $4520068
003180     CLOSE SALARY-FILE.                                           $4520069
003190     STOP RUN.                                                    $4520070
003200                                                                  $4520071
003210 CALCULATIONS.                                                    $4520072
003220     COMPUTE WEEKLY-PAY = 3 * MONTHLY-PAY / 13                    $4520073
003230     COMPUTE ANNUAL-PAY = 12 * MONTHLY-PAY                        $4520074
003240     MOVE WEEKLY-PAY TO WEEKLY IN SALARIES                        $4520075
003250     MOVE MONTHLY-PAY TO MONTHLY IN SALARIES                      $4520076
004010     MOVE ANNUAL-PAY TO ANNUAL IN SALARIES                        $4520077
004020     ADD WEEKLY-PAY TO TOTAL-A                                    $4520078
004030     ADD MONTHLY-PAY TO TOTAL-B                                   $4520079
004040     ADD ANNUAL-PAY TO TOTAL-C                                    $4520080
004050     WRITE SALARY-RECORD FROM SALARIES  AFTER ADVANCING 1 LINES.  $4520081
/*
// EXEC LNKEDT
// ASSGN SYS004,X'00E'
// EXEC
/*
/&
* $$EOJ