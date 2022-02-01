// JOB OPTMODS CORRECT AND DELETE STUFF ON TOS SYSRES
* DELETE UNUSED 10K ASSEMBLY PHASES
* DELETE OCR, PAPER TAPE, AND BOS/360 COMPATABILITY
* DELETE SYSGEN RELOCATABLE MODULES
* CORRECT AUTOTEST SAMPLE
// EXEC MAINT
 DELETC $$ANERR9                OCR
 DELETC $$BOOR01                OCR
 DELETC ASSEM00A,ASSEM00B       SMALL ASSEMBLER
 DELETR IJA.ALL          JC,LE,SYS.UTIL
 DELETR IJP.ALL          TAPE SORT
 DELETR IJQ.ALL          ASSEMBLER
 DELETR IJR.ALL          RPG COMPILER
 DELETR IJS.ALL          COBOL COMPILER AND DEBUG
 DELETR IJTFO,IJTFO1,IJTFO2,IJTFO3,IJTFO4 FORTRAN COMPILER
 DELETR IJV.ALL          AUTOTEST
 DELETR IJW.ALL          UTILITIES
 DELETR IJX.ALL          PL/I COMPILER
 DELETR IJZ.ALL          OLTEP
 CATALS A.AT1
 BKEND A.AT1
         REPRO                                                          $4070001
         PHASE AAAAAAAA,S                                               $4070002
CSECT1   START 0                                                        $4070003
R2       EQU   2                                                        $4070004
R3       EQU   3                                                        $4070005
R10      EQU   10                                                       $4070006
HERE     BALR  R10,0                                                    $4070007
         USING *,R10                                                    $4070008
BAL1     BAL   R2,READ                                                  $4070009
MVC1     MVC   SYMA(4),IN+10                                            $4070010
MVC2     MVC   SYMC(4),IN+75                                            $4070011
THERE    LH    R3,SYMC                                                  $4070012
BC1      BC    15,HERE+2                                                $4070013
READ     EQU   *                                                        $4070014
CHAN     EXCP  CARDCCB                                                  $4070015
WAIT1    WAIT  CARDCCB                                                  $4070016
TEST2    TM    CARDCCB+4,1                                              $4070017
BC2      BCR   1,R2                                                     $4070018
BCR1     BCR   15,R2                                                    $4070019
EOJ      EOJ                                                            $4070020
IN       DC    CL80' '                                                  $4070021
SYMA     DC    2F'1'                                                    $4070022
SYMB     DC    F'0'                                                     $4070023
SYMC     DC    2H'0'                                                    $4070024
CARDCCB  CCB   SYSIPT,CARDCCW                                           $4070025
CARDCCW  CCW   2,IN,32,80                                               $4070026
         END   HERE                                                     $4070027
          0001                                                             AAAA
          0002                                                             BBBB
          0003                                                             CCCC
          0004                                                             DDDD
          0005                                                             EEEE
 BKEND A.AT1
 DELETS A.CHNG           BOS/360 COMPATABILITY
 DELETS A.DSPLY          OCR
 DELETS A.DTFEN          BOS/360 COMPATABILITY
 DELETS A.DTFOR          OCR
 DELETS A.DTFPT          PAPER TAPE
 DELETS A.DTFSR          BOS/360 COMPATABILITY
 DELETS A.ORDC           OCR
 DELETS A.ORJT           OCR
 DELETS A.ORMOD          OCR
 DELETS A.PTMOD          PAPER TAPE
 DELETS A.RDLNE          OCR
 DELETS A.RESCN          OCR
 DELETS A.WAITF          OCR
/*
/* X'182' (sys002.aws) now has the updated SYSRES
/&
