// JOB DEMOCBL
// OPTION LIST,LINK
    ACTION MAP
// EXEC COBOL
001010 IDENTIFICATION DIVISION.                                         $4020001
001020 PROGRAM-ID. 'C360SAMP'.                                          $4020002
001030 REMARKS.                                                         $4020003
001040     EXAMPLE OF A 360 COBOL PROGRAM TO                            $4020004
001050     COMPUTE SALARIES.                                            $4020005
001060 ENVIRONMENT DIVISION.                                            $4020006
001070 CONFIGURATION SECTION.                                           $4020007
001080   SOURCE-COMPUTER. IBM-360.                                      $4020008
001090   OBJECT-COMPUTER. IBM-360.                                      $4020009
001100 INPUT-OUTPUT SECTION.                                            $4020010
001110 FILE-CONTROL.                                                    $4020011
001120     SELECT SALARY-FILE                                           $4020012
001130     ASSIGN TO 'SYS004' UNIT-RECORD 1403.                         $4020013
001140                                                                  $4020014
001150 DATA DIVISION.                                                   $4020015
001160 FILE SECTION.                                                    $4020016
001170  FD SALARY-FILE                                                  $4020017
001180     LABEL RECORDS ARE OMITTED                                    $4020018
001185     RECORDING MODE IS F                                          $4020019
001190     DATA RECORD IS SALARY-RECORD.                                $4020020
001200  01 SALARY-RECORD                                                $4020021
001210     PICTURE X(100).                                              $4020022
001220 WORKING-STORAGE SECTION.                                         $4020023
001230  77 TOTAL-A      PICTURE 9(6)V99    VALUE ZERO.                  $4020024
001240  77 TOTAL-B      PICTURE 9(6)V99    VALUE ZERO.                  $4020025
001250  77 TOTAL-C      PICTURE 9(6)V99    VALUE ZERO.                  $4020026
002010  77 WEEKLY-PAY   PICTURE 999V99.                                 $4020027
002020  77 MONTHLY-PAY  PICTURE 9999V99.                                $4020028
002030  77 ANNUAL-PAY   PICTURE 99999V99.                               $4020029
002040  77 CON-A  PICTURE 9(6)V99    VALUE IS 008826.69.                $4020030
002050  77 CON-B  PICTURE 9(6)V99    VALUE IS 038250.00 .               $4020031
002060  77 CON-C  PICTURE 9(6)V99    VALUE IS 459000.00 .               $4020032
002070  01 SALARIES.                                                    $4020033
002080     02 FILLER    PICTURE A(46)  VALUE SPACE.                     $4020034
002090     02 WEEKLY    PICTURE ZZZ.99 .                                $4020035
002100     02 FILLER    PICTURE AAA    VALUE SPACE.                     $4020036
002110     02 MONTHLY   PICTURE ZZZZ.99 .                               $4020037
002120     02 FILLER    PICTURE AAA    VALUE SPACE.                     $4020038
002130     02 ANNUAL    PICTURE ZZZZZ.99 .                              $4020039
002140     02 FILLER    PICTURE A(27)  VALUE SPACE.                     $4020040
002150  01 MESG.                                                        $4020041
002160     02 FILLER    PICTURE A(40)  VALUE SPACES.                    $4020042
002170     02 SHOW      PICTURE A(26).                                  $4020043
002180  01 DSPY.                                                        $4020044
002190     02 FILLER    PICTURE A(40)  VALUE SPACES.                    $4020045
002200     02 PRSNT     PICTURE A(33).                                  $4020046
002210  01 HEADING.                                                     $4020047
002220     02 FILLER    PICTURE A(46)  VALUE SPACES.                    $4020048
002230     02 WEEKLY    PICTURE A(6)   VALUE IS 'WEEKLY'.               $4020049
002240     02 FILLER    PICTURE A(3)   VALUE IS SPACES.                 $4020050
002250     02 MONTHLY   PICTURE A(7)   VALUE IS 'MONTHLY'.              $4020051
003010     02 FILLER    PICTURE A(3)   VALUE IS SPACES.                 $4020052
003020     02 ANNUAL    PICTURE A(6)   VALUE IS 'ANNUAL'.               $4020053
003030     02 FILLER    PICTURE A(29)  VALUE IS SPACES.                 $4020054
003040                                                                  $4020055
003050 PROCEDURE DIVISION.                                              $4020056
003060 START.                                                           $4020057
003070     OPEN OUTPUT SALARY-FILE.                                     $4020058
003080     WRITE SALARY-RECORD FROM HEADING AFTER ADVANCING 0 LINES.    $4020059
003090     PERFORM CALCULATIONS                                         $4020060
003100       VARYING MONTHLY-PAY FROM 500 BY 10                         $4020061
003110       UNTIL MONTHLY-PAY IS GREATER THAN 1000.                    $4020062
003120       IF TOTAL-A = CON-A AND TOTAL-B = CON-B AND TOTAL-C = CON-C $4020063
003130       MOVE 'TABLE VALUES ARE CORRECT' TO SHOW                    $4020064
003140       WRITE SALARY-RECORD FROM MESG AFTER ADVANCING 2 LINES      $4020065
003150       ELSE                                                       $4020066
003160       MOVE 'TABLE VALUES ARE NOT CORRECT' TO PRSNT               $4020067
003170       WRITE SALARY-RECORD FROM DSPY AFTER ADVANCING 2 LINES.     $4020068
003180     CLOSE SALARY-FILE.                                           $4020069
003190     STOP RUN.                                                    $4020070
003200                                                                  $4020071
003210 CALCULATIONS.                                                    $4020072
003220     COMPUTE WEEKLY-PAY = 3 * MONTHLY-PAY / 13                    $4020073
003230     COMPUTE ANNUAL-PAY = 12 * MONTHLY-PAY                        $4020074
003240     MOVE WEEKLY-PAY TO WEEKLY IN SALARIES                        $4020075
003250     MOVE MONTHLY-PAY TO MONTHLY IN SALARIES                      $4020076
004010     MOVE ANNUAL-PAY TO ANNUAL IN SALARIES                        $4020077
004020     ADD WEEKLY-PAY TO TOTAL-A                                    $4020078
004030     ADD MONTHLY-PAY TO TOTAL-B                                   $4020079
004040     ADD ANNUAL-PAY TO TOTAL-C                                    $4020080
004050     WRITE SALARY-RECORD FROM SALARIES  AFTER ADVANCING 1 LINES.  $4020081
/*
// EXEC LNKEDT
// ASSGN SYS004,X'00E'
// EXEC
/&
