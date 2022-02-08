//ASMFSPL  JOB                                                                   
//STEP1    EXEC  PROC=ASMFCLG,REGION=128K
//ASM.SYSUT1 DD UNIT=SYSDA
//ASM.SYSUT2 DD UNIT=SYSDA
//ASM.SYSUT3 DD UNIT=SYSDA
//ASM.SYSIN DD *
EXAM     TITLE 'SAMPLE PROGRAM'                                         00500019
         PRINT DATA                                                     01000019
*                                                                       01500019
*        THIS IS THE MACRO DEFINITION                                   02000019
*                                                                       02500019
         MACRO                                                          03000019
         MOVE  &TO,&FROM                                                03500019
.*                                                                      04000019
.*       DEFINE SETC SYMBOL                                             04500019
.*                                                                      05000019
         LCLC  &TYPE                                                    05500019
.*                                                                      06000019
.*       CHECK NUMBER OF OPERANDS                                       06500019
.*                                                                      07000019
         AIF   (N'&SYSLIST NE 2).ERROR1                                 07500019
.*                                                                      08000019
.*       CHECK TYPE ATTRIBUTES OF OPERANDS                              08500019
.*                                                                      09000019
         AIF   (T'&TO NE T'&FROM).ERROR2                                09500019
         AIF   (T'&TO EQ 'C' OR T'&TO EQ 'G' OR T'&TO EQ 'K').TYPECGK   10000019
         AIF   (T'&TO EQ 'D' OR T'&TO EQ 'E' OR T'&TO EQ 'H').TYPEDEH   10500019
         AIF   (T'&TO EQ 'F').MOVE                                      11000019
         AGO   .ERROR3                                                  11500019
.TYPEDEH ANOP                                                           12000019
.*                                                                      12500019
.*       ASSIGN TYPE ATTRIBUTE TO SETC SYMBOL                           13000019
.*                                                                      13500019
&TYPE    SETC  T'&TO                                                    14000019
.MOVE    ANOP                                                           14500019
*        NEXT TWO STATEMENTS GENERATED FOR MOVE MACRO                   15000019
         L&TYPE   2,&FROM                                               15500019
         ST&TYPE  2,&TO                                                 16000019
         MEXIT                                                          16500019
.*                                                                      17000019
.*       CHECK LENGTH ATTRIBUTES OF OPERANDS                            17500019
.*                                                                      18000019
.TYPECGK AIF   (L'&TO NE L'&FROM OR L'&TO GT 256).ERROR4                18500019
*        NEXT STATEMENT GENERATED FOR MOVE MACRO                        19000019
         MVC   &TO,&FROM                                                19500019
         MEXIT                                                          20000019
.*                                                                      20500019
.*       ERROR MESSAGES FOR INVALID MOVE MACRO INSTRUCTIONS             21000019
.*                                                                      21500019
.ERROR1  MNOTE 1,'IMPROPER NUMBER OF OPERANDS, NO STATEMENTS GENERATED' 22000019
         MEXIT                                                          22500019
.ERROR2  MNOTE 1,'OPERAND TYPES DIFFERENT, NO STATEMENTS GENERATED'     23000019
         MEXIT                                                          23500019
.ERROR3  MNOTE 1,'IMPROPER OPERAND TYPES, NO STATEMENTS GENERATED'      24000019
         MEXIT                                                          24500019
.ERROR4  MNOTE 1,'IMPROPER OPERAND LENGTHS, NO STATEMENTS GENERATED'    25000019
         MEND                                                           25500019
*                                                                       26000019
*        MAIN ROUTINE                                                   26500019
*                                                                       27000019
SAMPLR   CSECT                                                          27500019
BEGIN    SAVE  (14,12),,*                                               28000019
         BALR  R12,0         ESTABLISH ADDRESSABILITY OF PROGRAM        28500019
         USING *,R12         AND TELL THE ASSEMBLER WHAT BASE TO USE    29000019
         ST    13,SAVE13                                                29500019
         LM    R5,R7,=A(LISTAREA,16,LISTEND)  LOAD LIST AREA PARAMETERS 30000019
         USING LIST,R5      REGISTER 5 POINTS TO THE LIST               30500019
MORE     BAL   R14,SEARCH   FIND LIST ENTRY IN TABLE                    31000019
         TM    SWITCH,NONE  CHECK TO SEE IF NAME WAS FOUND              31500019
         BO    NOTTHERE     BRANCH IF NOT                               32000019
         USING TABLE,R1     REGISTER 1 NOW POINTS TO TABLE ENTRY        32500019
         MOVE  TSWITCH,LSWITCH          MOVE FUNCTIONS                  33000019
         MOVE  TNUMBER,LNUMBER               FROM LIST ENTRY            33500019
         MOVE  TADDRESS,LADDRESS                  TO TABLE ENTRY        34000019
LISTLOOP BXLE  R5,R6,MORE   LOOP THROUGH THE LIST                       34500019
         CLC   TESTTABL(240),TABLAREA                                   35000019
         BNE   NOTRIGHT                                                 35500019
         CLC   TESTLIST(96),LISTAREA                                    36000019
         BNE   NOTRIGHT                                                 36500019
         WTO   'ASSEMBLER SAMPLE PROGRAM SUCCESSFUL'                    37000019
EXIT     L     R13,SAVE13                                               37500019
         RETURN (14,12),RC=0                                            38000019
*                                                                       38500019
NOTRIGHT WTO   'ASSEMBLER SAMPLE PROGRAM UNSUCCESSFUL'                  39000019
         B     EXIT                                                     39500019
NOTTHERE OI    LSWITCH,NONE TURN ON SWITCH IN LIST ENTRY                40000019
         B     LISTLOOP     GO BACK AND LOOP                            40500019
SAVE13   DC    F'0'                                                     41000019
SWITCH   DC    X'00'                                                    41500019
NONE     EQU   X'80'                                                    42000019
*                                                                       42500019
*        BINARY SEARCH ROUTINE                                          43000019
*                                                                       43500019
SEARCH   NI    SWITCH,255-NONE TURN OFF NOT FOUND SWITCH                44000019
         LM    R1,R3,=F'128,4,128' LOAD TABLE PARAMETERS                44500019
         LA    R1,TABLAREA-16(R1)  GET ADDRESS OF MIDDLE ENTRY          45000019
LOOP     SRL   R3,1                DIVIDE INCREMENT BY 2                45500019
         CLC   LNAME,TNAME         COMPARE LIST ENTRY WITH TABLE ENTRY  46000019
         BH    HIGHER              BRANCH IF SHOULD BE HIGHER IN TABLE  46500019
         BCR   8,R14               EXIT IF FOUND                        47000019
         SR    R1,R3               OTHERWISE IT IS LOWER IN THE TABLE  X47500019
                                     SO SUBTRACT INCREMENT              48000019
         BCT   R2,LOOP             LOOP 4 TIMES                         48500019
         B     NOTFOUND            ARGUMENT IS NOT IN THE TABLE         49000019
HIGHER   AR    R1,R3               ADD INCREMENT                        49500019
         BCT   R2,LOOP             LOOP 4 TIMES                         50000019
NOTFOUND OI    SWITCH,NONE         TURN ON NOT FOUND SWITCH             50500019
         BR    R14                 EXIT                                 51000019
*                                                                       51500019
*        THIS IS THE TABLE                                              52000019
*                                                                       52500019
         DS    0D                                                       53000019
TABLAREA DC    XL8'0',CL8'ALPHA'                                        53500019
         DC    XL8'0',CL8'BETA'                                         54000019
         DC    XL8'0',CL8'DELTA'                                        54500019
         DC    XL8'0',CL8'EPSILON'                                      55000019
         DC    XL8'0',CL8'ETA'                                          55500019
         DC    XL8'0',CL8'GAMMA'                                        56000019
         DC    XL8'0',CL8'IOTA'                                         56500019
         DC    XL8'0',CL8'KAPPA'                                        57000019
         DC    XL8'0',CL8'LAMBDA'                                       57500019
         DC    XL8'0',CL8'MU'                                           58000019
         DC    XL8'0',CL8'NU'                                           58500019
         DC    XL8'0',CL8'OMICRON'                                      59000019
         DC    XL8'0',CL8'PHI'                                          59500019
         DC    XL8'0',CL8'SIGMA'                                        60000019
         DC    XL8'0',CL8'ZETA'                                         60500019
*                                                                       61000019
*        THIS IS THE LIST                                               61500019
*                                                                       62000019
LISTAREA DC    CL8'LAMBDA',X'0A',FL3'29',A(BEGIN)                       62500019
         DC    CL8'ZETA',X'05',FL3'5',A(LOOP)                           63000019
         DC    CL8'THETA',X'02',FL3'45',A(BEGIN)                        63500019
         DC    CL8'TAU',X'00',FL3'0',A(1)                               64000019
         DC    CL8'LIST',X'1F',FL3'465',A(0)                            64500019
LISTEND  DC    CL8'ALPHA',X'00',FL3'1',A(123)                           65000019
*                                                                       65500019
*        THIS IS THE CONTROL TABLE                                      66000019
*                                                                       66500019
         DS    0D                                                       67000019
TESTTABL DC    FL3'1',X'00',A(123),CL8'ALPHA'                           67500019
         DC    XL8'0',CL8'BETA'                                         68000019
         DC    XL8'0',CL8'DELTA'                                        68500019
         DC    XL8'0',CL8'EPSILON'                                      69000019
         DC    XL8'0',CL8'ETA'                                          69500019
         DC    XL8'0',CL8'GAMMA'                                        70000019
         DC    XL8'0',CL8'IOTA'                                         70500019
         DC    XL8'0',CL8'KAPPA'                                        71000019
         DC    FL3'29',X'0A',A(BEGIN),CL8'LAMBDA'                       71500019
         DC    XL8'0',CL8'MU'                                           72000019
         DC    XL8'0',CL8'NU'                                           72500019
         DC    XL8'0',CL8'OMICRON'                                      73000019
         DC    XL8'0',CL8'PHI'                                          73500019
         DC    XL8'0',CL8'SIGMA'                                        74000019
         DC    FL3'5',X'05',A(LOOP),CL8'ZETA'                           74500019
*                                                                       75000019
*        THIS IS THE CONTROL LIST                                       75500019
*                                                                       76000019
TESTLIST DC    CL8'LAMBDA',X'0A',FL3'29',A(BEGIN)                       76500019
         DC    CL8'ZETA',X'05',FL3'5',A(LOOP)                           77000019
         DC    CL8'THETA',X'82',FL3'45',A(BEGIN)                        77500019
         DC    CL8'TAU',X'80',FL3'0',A(1)                               78000019
         DC    CL8'LIST',X'9F',FL3'465',A(0)                            78500019
         DC    CL8'ALPHA',X'00',FL3'1',A(123)                           79000019
*                                                                       79500019
*        THESE ARE THE SYMBOLIC REGISTERS                               80000019
*                                                                       80500019
R0       EQU   0                                                        81000019
R1       EQU   1                                                        81500019
R2       EQU   2                                                        82000019
R3       EQU   3                                                        82500019
R5       EQU   5                                                        83000019
R6       EQU   6                                                        83500019
R7       EQU   7                                                        84000019
R12      EQU   12                                                       84500019
R13      EQU   13                                                       85000019
R14      EQU   14                                                       85500019
R15      EQU   15                                                       86000019
*                                                                       86500019
*        THIS IS THE FORMAT DEFINITION OF LIST ENTRYS                   87000019
*                                                                       87500019
LIST     DSECT                                                          88000019
LNAME    DS    CL8                                                      88500019
LSWITCH  DS    C                                                        89000019
LNUMBER  DS    FL3                                                      89500019
LADDRESS DS    F                                                        90000019
*                                                                       90500019
*        THIS IS THE FORMAT DEFINITION OF TABLE ENTRYS                  91000019
*                                                                       91500019
TABLE    DSECT                                                          92000019
TNUMBER  DS    FL3                                                      92500019
TSWITCH  DS    C                                                        93000019
TADDRESS DS    F                                                        93500019
TNAME    DS    CL8                                                      94000019
         END   BEGIN                                                    94500019
/*
//LKED.SYSUT1 DD  UNIT=SYSDA
//GO.OUTPUT   DD  SYSOUT=A
//GO.SYSPRINT DD  SYSOUT=A
//