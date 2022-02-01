* $$JOB DEMOECLP,,,F2
// JOB DEMOECLP ECLIPTIC COORDINATE TABLE
// OPTION LINK
// EXEC ASSEMBLY
ECLP     TITLE 'ECLIPTIC COORDINATE TABLE'
* SAMPLE DOS ASSEMBLER LANGUAGE PROGRAM
* DEMONSTRATES TRAPPING PROGRAM AND INTERVAL TIMER INTERRUPTIONS
* DEMONSTRATES DOUBLE PRECISION FLOATING POINT INSTRUCTIONS
* DEMONSTRATES CALLING A FORTRAN IV FUNCTION
* USES PRINTER ASA CHARACTERS
*        PRINT NOGEN
ECLIPTIC START X'5000'
         ENTRY ILFIBCOM,IBCOM#,INTSW
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
FR0      EQU   0
FR2      EQU   2
FR4      EQU   4
FR6      EQU   6
         BALR  R12,0
         USING *,R12
         STXIT PC,THATSALL,SAVEXIT     TRAP PROGRAM CHECK INTERRUPTIONS
         STXIT IT,THATSALL,SAVEXIT     END PROGRAM WITH DUMP AFTER
         SETIME 15                       15 SECONDS IF NOT DONE
         LD    FR0,TWOPI               PI 
         MD    FR0,=D'2'                TIMES 2
         STD   FR0,TWOPI                 IS 2 PI
         OPEN  PRINTER
         MVI   PRINTOUT,C' '           CLEAR PRINT LINE
         MVC   PRINTOUT+1(132),PRINTOUT
         MVC   PRINTOUT+53(27),=C'ECLIPTIC  COORDINATE  TABLE'
         MVI   PRINTOUT,C'1'           START ON NEW PAGE
         BAL   R2,PRINT                PRINT HEADING
         MVC   PRINTOUT+10(118),HEADING2
         MVI   PRINTOUT,C'-'           TRIPLE SPACE
         BAL   R2,PRINT                PRINT COLUMN HEADINGS
         MVC   PRINTOUT+4(5),=C'HOURS'
         BAL   R2,PRINT
*
         LA    R10,24                  LOOP FOR 24 HOURS
LOOP     EQU   *
         LA    R8,PRINTOUT+11          START AT BEGINNING OF LINE
         LA    R9,12                   LOOP TWELVE PARTS OF HOUR
         MVC   PRINTOUT+3(6),=X'40202021F0F0'  MOVE HOURS
         ED    PRINTOUT+3(4),HOURS
LOOP1    EQU   *
         LD    FR6,MIN
         DD    FR6,=D'12'              GET FRAC. OF HOUR
         AD    FR6,HRS                 PLUS HOURS
         MD    FR6,TWOPI               TIMES TWO PI
         DD    FR6,=D'24'              GET RADIAN VALUE FROM 0 TO 2 PI
         STD   FR6,DOUBLE              STORE RADIAN VALUE
         LA    R13,SAVEAREA            GET SINE
         CALL  DSIN,(DOUBLE)             ANSWER RETURNED IN FPR0
         MD    FR0,=D'23.44305'        ALLOW FOR EARTH'S TILT
         MD    FR0,=D'3600'            TO GET DEGREE SECONDS (60 X 60)
         LTDR  FR0,FR0                 ROUND OFF
         BM    *+12
         AD    FR0,=D'.5'
         B     *+8
         SD    FR0,=D'.5'
         LDR   FR2,FR0                 SAVE SIGN
         LPDR  FR0,FR0            GET ABSOLUTE VALUE (ENSURE POSITIVE)
         AW    FR0,DCONS               CONVERT FP TO BINARY
         STD   FR0,DOUBLE
         L     R1,DOUBLE+4
         LTDR  FR2,FR2                 IF FP NEGATIVE,
         BNM   *+6
         LNR   R1,R1                     MAKE BINARY NEGATIVE
         LR    R4,R1                   SAVE SIGN
         LPR   R1,R1              GET ABSOLUTE VALUE (ENSURE POSITIVE)
         XR    R0,R0
         D     R0,=F'60'
         CVD   R0,DOUBLE               GET DECL. DEGREE SECONDS
         MVC   RESULT+2(2),DOUBLE+6    XXXX0SSF
         XR    R0,R0
         D     R0,=F'60'
         CVD   R0,DOUBLE               GET DECL. DEGREE MINUTES
         MVO   RESULT(3),DOUBLE+6(2)   00MMFSSF      
         MVO   RESULT(3),RESULT(2)     000MMSSF
         CVD   R1,DOUBLE               GET DECL. DEGREES
         L     R1,DOUBLE+4             0000DDDF
         SRL   R1,4                    00000DDD
         SLL   R1,20                   DDD00000
         O     R1,RESULT               DDDMMSSF
         ST    R1,RESULT               STORE PACKED RESULT
         LTR   R4,R4                   IF BINARY NEGATIVE,
         BNM   *+10
         MVN   RESULT+3(1),=X'0D'        INDICATE PACKED NEGATIVE
         MVC   0(10,R8),=X'402021204B20204B2020'
         LA    R1,3(R8)
         EDMK  0(10,R8),RESULT
         BNM   *+10                    IF NEGATIVE,
         BCTR  R1,0
         MVI   0(R1),C'-'                PUT MINUS IN FRONT
         LA    R8,10(R8)               POINT TO NEXT SLOT ON PRINT LINE
         LD    FR4,MIN          BUMP MINUTES TO NEXT TWELFTH OF AN HOUR
         AD    FR4,=D'1'
         STD   FR4,MIN
         BCT   R9,LOOP1
         MVI   PRINTOUT,C'0'           DOUBLE SPACE
         BAL   R2,PRINT                PRINT THE HOUR LINE
         LD    FR4,HRS                 BUMP HOUR BY ONE
         AD    FR4,=D'1'
         STD   FR4,HRS
         XC    MIN,MIN                 START MINUTES AT ZERO
         AP    HOURS,=P'1'             SET FOR NEXT HOUR
         BCT   R10,LOOP                LOOP FOR NEXT HOUR
*
         CLOSE PRINTER
         EOJ
PRINT    EQU   *
         PRTOV PRINTER,12              IF EOP, SKIP TO NEW PAGE
         PUT   PRINTER,PRINTOUT        PRINT LINE
         MVI   PRINTOUT,C' '           CLEAR PRINT AREA
         MVC   PRINTOUT+1(132),PRINTOUT
         BR    R2
ILFIBCOM EQU   *-60
IBCOM#   EQU   ILFIBCOM
THATSALL BALR  R12,0
         USING *,R12
         PDUMP ECLIPTIC,ECLIPTIC+4096  DUMP PROGRAM ONLY, NOT SUPVR.
         CANCEL
HRS      DC    D'0'                    START AT ZERO HOURS
MIN      DC    D'0'                    START AT ZERO MINUTES
TWOPI    DC    D'3.1415926535897932'   PI X 2 IN INIT.
DCONS    DC    X'4E00000000000000'
RESULT   DC    F'0'
INTSW    DC    X'0'
HOURS    DC    PL2'0'                  START AT ZERO HOURS
HEADING2 DC    C'MINS  00        05        10        15        20      -
                 25        30        35        40        45        50 '
         DC    C'      55'
         LTORG
PRINTOUT DS    CL133
DOUBLE   DS    D                       WORK AREA
SAVEAREA DS    9D                      REG. SAVE AREA FOR SINE ROUTINE
SAVEXIT  DS    9D                      PROGRAM CHECK OR I.T. SAVE AREA
PRINTER  DTFPR BLKSIZE=133,DEVADDR=SYSLST,IOAREA1=PRNTOUT1,            -
               WORKA=YES,PRINTOV=YES,CTLCHR=ASA
PRNTOUT1 DS    CL133
         PRMOD WORKA=YES,CTLCHR=ASA,PRINTOV=YES
         END
/*
 INCLUDE ILFLSCN   FORTRAN IV SINE,COSINE ROUTINE
// EXEC LNKEDT
// EXEC
/&
* $$EOJ