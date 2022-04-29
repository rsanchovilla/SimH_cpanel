******************************************************
******************************************************
*
*      THIS IS A TOTAL DIAGNOSTIC COMPOSED OF 6 SMALLER
*      DIAGNOSTICS FOR THE 6802 MICRO-PROCESSOR
*
*      AUSTIN,TEXAS - MICROPROCESSOR CAPITAL OF THE WORLD
*
*      MODIFIED FOR SIMH SWTPC 6800 EMULATOR AND SWTBUG
*      BY ROBERTO SANCHO, APR 2022
*
*      PROGRAM TAKEN FROM MOTOROLA USER GROUP 
*      http://test.dankohn.info/~myhome/projects/68HC11/AXIOM_HC11/Source/Users%20Group/UG137
*
*      THIS DIAGNOSTIC ATTEMPTS TO VERIFY ALL INSTRUCTIONS
*      FOR THE 6802 PROCESSOR. THE VARIOUS INSTRUCTION
*      GROUPS ARE TESTED SEPARATELY AND IN A SPECIFIED
*      ORDER (I.E. A GROUP FOLLOWING A PREVIOUS GROUP
*      ASSUMES THE PREVIOUS GROUPS FUNCTION). THE GROUPS
*      AND THEIR ORDER IS:
*         GROUP 3   BRANCHES
*         GROUP 1   LOADS,STORES,ETC
*         GROUP 6   NOP,TAP,SEV,ETC
*         GROUP 2   NEG,COM,INC,SHIFTS,ETC
*         GROUP 4   TAB,TBA,SBA,ETC
*         GROUP 5   TSX,INS,PULA,ETC
*
*      THE DIAGNOSTIC ASSUMES THE FOLLOWING INSTRUCTIONS 
*      HAVE BEEN HAND TESTED BEFORE EXECUTION:
*         LDAA   IMMEDIATE
*         TAP
*         JMP    EXTENDED
*         EOR    DIRECT
*         PSHA
*         PULA
*
*      ONCE THESE INSTRUCTIONS ARE TESTED THE
*      DIAGNOSTIC WILL NOT USE ANY INSTRUCTION
*      UNTIL IT HAS BEEN THROUGHLY TESTED.
*
*
*      WRITTEN TO RUN UNDER EXBUG 1.2
*      TO LOAD FROM MDOS TYPE: LOAD BIGTST;V
*
*      THE PROGRAM TYPES 'OK' AT THE END OF EACH COMPLETE
*      TEST OF THE INSTRUCTION TEST.
*      HANGS IF TEST FAILS
*
*      REQUIRES RAM AT LOCATIONS 0-$200
*      REQUIRES ROM OR RAM AT LOCATIONS $1000-$2FFF
*
******STARTAT LOCATION $1000
*
*      WRITTEN BY C.D.HUNTSMAN
*
*
*****************************************************
*****************************************************
*
       ORG $20
*       
*
*      DATA IN BASE PAGE FOR DIRECT ADDRESSING
*
BOTTOM RMB    $30 STACK AREA
STACK  RMB    8
DATA   FCB    @252,@125,@252,@0,@0,@377,@100,@124
       FCB    $80,$81,$BF,$FE
DATA1  FCB    @175,@176,@177,@001
TMP    RMB    2
*
*      DOUBLE PRECISION CONSTANTS
*
HAAAA  FDB $AAAA
H5555  FDB $5555
H5554  FDB $5554
H8001  FDB $8001
HBFFF  FDB $BFFF
HFFFF  FDB $FFFF
H8000  FDB $8000
HFFFE  FDB $FFFE
H0000  FDB $0000
H4000  FDB $4000
*
*      BASE PAGE VARIABLES
*
TEMP   RMB 2
OSP    RMB 2 OLD SP SAVE
NSP    RMB 2 NEW SP SAVE
*
*
*      EXTENDED VARIABLES
*
       ORG $100
TMP2   RMB 2
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 3 ****
*
*****************************************************
*
*      G R P 3
*
*****************************************************
*REV   1.0
*REV   1.1 GMCM REVISION - SEPT 77
*
*      GROUP 3 INSTRUCTIONS INCLUDE ALL BRANCHES 
*      EXCEPT BSR.
*
*PROGRAMTO TEST GP3 INST
*
*
*      THIS TEST ASSUMES THAT THE FOLLOWING INSTRUCTIONS
*      ARE ALREADY OPERATIONAL:
*      LDAA IMMEDIATE
*      TAP
*      JMP EXTENDED
*
       ORG $1000
ST     BRA B1
       BRA *
B1     JMP B2
       ORG $108E
C1     JMP C2
*
*      TEST ALL PATHS OF BRANCH
*
*      I.E., CHECK FORWARD AND REVERSE OFFSETS
*
       ORG $10C0
B2     BRA B3
       BRA *
B4     BRA B5
       BRA *
       BRA *
B3     BRA B4
*
*      FINAL PATH
*
       ORG $1106
B5     CLC
       BCS *
       BRA C1
*
*      FROM HERE ON IT IS ASSUMED THAT IF BRA CAN BE
*      EXECUTED WITH PLUS AND MINUS OFFSETS SO CAN
*      THE OTHER BRANCHES.
*
*
*      TEST COMBINATIONS OF BRANCH
*
C2     LDAA #$01
       TAP             C=1,Z=0
       BCS T1
       BRA *
T1     BLS T2
       BRA *
T2     BCC *
       BHI *
       LDAA #$00
       TAP             C=0,Z=0
       BCC T3
       BRA *
T3     BHI T4
       BRA *
T4     BCS *
       BLS *
       LDAA #$04
       TAP             C=0,Z=1
       BLS T5
       BRA *
T5     BEQ T6
       BRA *
T6     BNE *
       BHI *
       LDAA #$05
       TAP             C=1,Z=1
       BLS T7
       BRA *
T7     BHI *
       LDAA #$08
       TAP             N=1,V=0,Z=0
       BMI T8
       BRA *
T8     BPL *
       BGE *
       BLT T9
       BRA *
T9     BGT *
       BLE T10
       BRA *
T10    BVS *
       BEQ *
       BCC TA
       BRA *
TA     LDAA #$02
       TAP              N=0,V=1,Z=0
       BVS T11
       BRA *
T11    BVC *
       BLT T12
       BRA *
T12    BGE *
       BGT *
       BLE T13
       BRA *
T13    BPL T14
       BRA *
T14    BMI *
       LDAA #$00
       TAP              N=0,V=0,Z=0
       BGE T15
       BRA *
T15    BLT *
       BLE *
       BGT T16
       BRA *
T16    LDAA #$0A
       TAP              N=1,V=1,Z=0
       BGE T17
       BRA *
T17    BLT *
       BLE *
       BGT T18
       BRA *
T18    LDAA #$04
       TAP              N=0,V=0,Z=1
       BLE T19
       BRA *
T19    BGT * 
       BRA GRP1         GO DO GROUP 1
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 1 ****
*
******************************************************
*
*      G R P 1
*
*****************************************************
*
*      GMCM   REVISION 1.1 - SEPT 77
*
*      GROUP I INSTRUCTIONS INCLUDE:
*       SUB
*       CMP
*       SBC
*       AND
*       BIT
*       LDA
*       STA
*       EOR
*       ADC
*       ORA
*       ADD
*       CPX
*       BSR
*       JSR
*       LDS
*       LDX
*       STS
*       STX
*
*
DATA2  FCB $55,$AA
*
*
*                         LOAD ACCUMULATOR A (LDA)
*
*        ASSUMES BRANCHES AND EOR DIRECT WORK!!!!!!!!!!!!!!
*
********************************************
*
*               LOAD ACCUMULATOR a (LDA)
*
GRP1   LDAA DATA     DATA=@252,N=1,Z=0,V=0
A1     BPL  A1
A2     BEQ  A2
A3     BVS  A3
       EORA DATA
A4     BNE  A4
       LDAA DATA+1   DATA+1=@125,N=0,Z=0,V=0
A5     BMI  A5
       EORA DATA+1
A6     BNE  A6
       LDAA DATA+3   DATA+3=@0,N=0,Z=1,V=0
A7     BNE  A7
********************************************
*
*               LOAD ACCUMULATOR B (LDA)
*
       LDAB DATA     DATA=@252,N=1,Z=0,V=0
A10    BPL  A10
A11    BEQ  A11
A12    BVS  A12
       EORB DATA
A13    BNE  A13
       LDAB DATA+1   DATA+1=@125,N=0,Z=0,V=0
A14    BMI  A14
       EORB DATA+1
A15    BNE  A15
       LDAB DATA+3   DATA+3=@0,J=0,Z=1,V=0
A16    BNE  A16
********************************************
*
*               STORE ACCUMULATOR A (STA)
*
       LDAA DATA     DATA=@252,N=1,Z=0,V=0
       STAA TMP
A20    BPL  A20
A21    BEQ  A21
       BVS  *
       EORA TMP
A23    BNE  A23
       LDAA DATA+1   DATA+1=@125,N=0,Z=0,V=0
       STAA TMP
A24    BMI  A24
       EORA TMP
A25    BNE  A25
********************************************
*
*               STORE ACCUMULATOR B (STA)
*
       LDAB DATA     DATA=@252,N=1,Z=0,V=0
       STAB TMP
A30    BPL  A30
A31    BEQ  A31
       BVS  *
       EORB TMP
A33    BNE  A33
       LDAB DATA+1     DATA+1=@125,N=0,Z=0,V=0
       STAB TMP
       BMI  *
       EORB TMP
       BNE  *
*******************************************
*
*               LOAD AND STORE STACK POINTER (LDS)
*
       STS  STACK    SAVE STACK POINTER
       LDS  DATA     DATA=@252,DATA+1=@125,N=1,Z
A40    BPL  A40
A41    BEQ  A41
       BVS  *
       STS  TMP
       LDAA DATA
       EORA TMP
A43    BNE  A43
       LDAA DATA+1   DATA+1=@125
       EORA TMP+1
A44    BNE  A44
       LDS  DATA+1   DATA+1=@125,DATA+2=@252,N=0
A45    BMI  A45
       STS  TMP
       LDAA DATA+1
       EORA TMP
A46    BNE  A46
       LDAA DATA+2
       EORA TMP+1
A47    BNE  A47
       LDS  DATA+3
A48    BNE  A48
       LDS  STACK
**************************************
*
*               LOAD AND STORE INDEX REGISTORS (LDX&STX)
*
       LDX  DATA     DATA=@252,DATA+1=@125,N=1,Z
A50    BPL  A50
A51    BEQ  A51
A52    BVS  A52
       STX  TMP
       LDAA DATA
       EORA TMP
A53    BNE  A53
       BMI  *        N=1?
       LDAA DATA+1
       EORA TMP+1
A54    BNE  A54
       LDX  DATA+1   DATA+1=@125,DATA+2=@252
A55    BMI  A55
       STX  TMP
       LDAA DATA+1
       EORA TMP
A56    BNE  A56
       LDAA DATA
       EORA TMP+1
A57    BNE  A57
       LDX  DATA+3   DATA+3,Z=1
A58    BNE  A58
*
***************************************************:
*
*            COMPARE INDEX REGISTER
*
*
*
*      CASE 1:
*      X = $AA55
*      DATA = $AA55
*      RESULT = $0
*      Z = 1, N = 0, V = 0, C = 0
*
       LDX DATA
       CPX DATA    DO IT
       BNE *
       BMI *
       BVS *
       BCS *
       BEQ CP20
       BRA *
CP20   BPL CP22
       BRA *
CP22   BVC CP23
       BRA *
CP23   BCC CP1
       BRA *
*
*      CASE 2:
*      X = AA55
*      DATA+1 = 55AA
*      RESULT = 54AB
*      N=0, Z=0, V=1, C=0
CP1    CPX DATA+1
       BLS *
       BVC *
       BCS *
       BEQ *
       BMI *
       BNE CP2
       BRA *
CP2    BGE *
       BGT *
       BHI CP3
       BRA *
CP3    BLE CP4
       BRA *
CP4    BLS *
       BLT CP5
       BRA *
CP5    BVS CP6
       BRA *
CP6    BPL CP7
       BRA *
*
*      CASE 3:
*      X = 55AA
*      DATA = AA55
*      RESULT = AB55
*      Z=0, N=1, V=1, C=1
*
CP7    LDX DATA+1
       CPX DATA
       BPL *
       BVC *
       BEQ *
       BMI CP9
       BRA *
CP9    BVS CP10
       BRA *
CP10   BNE CP11
       BRA *
*
*      CASE 4:
*      X = 55AA
*      DATA + 3 = 0000
*      RESULT = 55AA
*      Z=0, N=0, V=0, C=0
*
CP11   LDX DATA+1
       CPX DATA+3 
       BEQ *
       BMI *
       BVS *
       BCS *
       BNE CP12
       BRA *
CP12   BPL CP13
       BRA *
CP13   BVC CP14
       BRA *
CP14   BCC CP15 
       BRA *
*
*      CASE 5:
*      X = AA00
*      DATA = AA55
*      RESULT = FFAB
*      Z=0, N=1, V=0, C=0
*
CP15   LDX DATA+2   X=AA00
       CPX DATA
       BEQ *
       BPL *
       BVS *
       BNE CP16
       BRA *
CP16   BMI CP17
       BRA *
CP17   BVC CP19
       BRA *
CP19   NOP
**************************************
*
*               LOGICAL AND ACCUMULATOR A (AND)
*
       LDAA DATA     DATA+1=@252,DATA+1=@125,N=0
       ANDA DATA+1
       BMI  *
       BNE  *
A62    BVS  A62
       LDAA DATA+1
       ANDA DATA     N=0,Z=1,V=0
A63    BMI  A63
       BNE  *
A65    BVS  A65
       LDAA DATA+5   DATA+5=@377,N=1,Z=0,V=0
       ANDA DATA+5
A66    BPL  A66
A67    BEQ  A67
       EORA DATA+5   Z=1
       BNE  *
       LDAA DATA+4   DATA+4=0
       ANDA DATA+4
       BNE  *
*********************************************
*
*               LOGICAL AND ACCUMULATOR B (AND)
*
       LDAB DATA     DATA=@252,DATA+1=@125
       ANDB DATA+1   N=0,Z=1,V=0
A70    BMI  A70
       BNE  *
A72    BVS  A72
       LDAB DATA+1
       ANDB DATA     N=0,Z=1,V=0
A73    BMI  A73
       BNE  *
A75    BVS  A75
       LDAB DATA+5   DATA+5=@377
       ANDB DATA+5   N=1,Z=0,V=0
A76    BPL  A76
A77    BEQ  A77
       EORB DATA+5
A78    BNE  A78
       LDAB DATA+4   DATA+4=0
       ANDB DATA+4   Z=1
A79    BNE  A79
*********************************************
*
*               LOGICAL OR ACCUMULATOR A (ORA)
*
       LDAA DATA     DATA=@252
       ORAA DATA+1   DATA+1=@125,N=1,Z=0,V=0
A801   BPL  A801
A802   BEQ  A802
A803   BVS  A803
       EORA DATA+5   DATA+5=@377
A804   BNE  A804
       LDAA DATA+1
       ORAA DATA
A805   BPL  A805
A806   BEQ  A806
A807   BVS  A807
       EORA DATA+5
A808   BNE  A808
       LDAA DATA+5
       ORAA DATA+5
       EORA DATA+5
A809   BNE  A809
       LDAA DATA+3   DATA+3=0
       ORAA DATA+3   N=0,Z=1,V=0
A810   BMI  A810
A811   BNE  A811
A812   BVS  A812
       EORA DATA+3
       BNE  *
*********************************************
*
*               LOGICAL OR ACCUMULATOR B (ORA)
*
       LDAB DATA     DATA=@252,DATA+1=@125
       ORAB DATA+1   N=1,Z=0,V=0
A901   BPL  A901
A902   BEQ  A902
A903   BVS  A903
       EORB DATA+5   DATA+5=@377
A904   BNE  A904
       LDAB DATA+1
       ORAB DATA
A905   BPL  A905
A906   BEQ  A906
A907   BVS  A907
       EORB DATA+5
A908   BNE  A908
       LDAB DATA+5
       ORAB DATA+5
       EORB DATA+5
A909   BNE  A909
       LDAB DATA+3   DATA+3=0
       ORAB DATA+3   N=0,Z=1,V=0
A910   BMI  A910
A911   BNE  A911
       BVS  *
       EORB DATA+3
A913   BNE  A913
*********************************************
*
*               ADD ACCUMULATOR A TO MEMORY (ADD)
*
       LDAA DATA+1   DATA+1=@125
       ADDA DATA+1   N=1,Z=0,V=1,C=0
A100   BPL  A100
A101   BEQ  A101
A102   BVC  A102
A103   BCS  A103
       EORA DATA     DATA=@252
A104   BNE  A104
       LDAA DATA
       ADDA DATA     N=0,Z=0,V=1,C=1
A105   BMI  A105
A106   BEQ  A106
       BVC  *
A108   BCC  A108
       EORA DATA+7   DATA+7=@124
A109   BNE  A109
       LDAA DATA+9   DATA+9=@201(-127)
       ADDA DATA+10   DATA+10=@277(-65),N=0,Z=0
A110   BMI  A110
A111   BEQ  A111
A112   BVC  A112
A113   BCC  A113
       EORA DATA+6   DATA+6=@100
A114   BNE  A114
       LDAA DATA+9   DATA+9=@201
       ADDA DATA+5   DATA+5=@377,N=1,Z=0,V=0,C=1
A115   BPL  A115
A116   BEQ  A116
A117   BVS  A117
A118   BCC  A118
       EORA DATA+8   DATA+8=@200
A119   BNE  A119
       LDAA DATA+5   DATA+5=@377
       ADDA DATA+3   DATA+3=0,N=1,Z=0,V=0,C=0
A120   BPL  A120
A121   BEQ  A121
A122   BVS  A122
A123   BCS  A123
       EORA DATA+5   DATA+5=@377
A124   BNE  A124
       LDAA DATA+5   DATA+5=@377
       ADDA DATA+5   N=1,Z=0,V=0,C=1
A125   BPL  A125
A126   BEQ  A126
A127   BVS  A127
A128   BCC  A128
       EORA DATA+11   DATA+11=@376
A129   BNE  A129
       LDAA DATA+3   DATA+3=0
       ADDA DATA+3   N=0,Z=1,V=0,C=0
A130   BMI  A130
A131   BNE  A131
A132   BVS  A132
A133   BCS  A133
       EORA DATA+3
A134   BNE  A134
*********************************************
*
*               ADD ACCUMULATOR B TO MEMORY
*
       LDAB DATA+1   DATA+1=@125
       ADDB DATA+1   N=1,Z=0V=1,C=0
A200   BPL  A200
A201   BEQ  A201
A202   BVC  A202
A203   BCS  A203
       EORB DATA     DATA=@252
A204   BNE  A204
       LDAB DATA     DATA=@252
       ADDB DATA     N=0,Z=0,V=1,C=1
A205   BMI  A205
A206   BEQ  A206
A207   BVC  A207
A208   BCC  A208
       EORB DATA+7   DATA+7=@124
A209   BNE  A209
       LDAB DATA+9   DATA+9=@201(-127)
       ADDB DATA+10  DATA+10=@277(-65),N=0,Z=0
A210   BMI  A210
A211   BEQ  A211
A212   BVC  A212
A213   BCC  A213
       EORB DATA+6   DATA+6=@100
A214   BNE  A214
       LDAB DATA+9   DATA+9"=""@201
       ADDB DATA+5   DATA+5=@377,N=1,Z=0,V=0,C=1
A215   BPL  A215
A216   BEQ  A216
A217   BVS  A217
A218   BCC  A218
       EORB DATA+8   DATA+8=@200
A219   BNE  A219
       LDAB DATA+5   DATA+5=@377
       ADDB DATA+3   DATA+3=0,N=1,Z=0,V=0,C=0
A220   BPL  A220
A221   BEQ  A221
A222   BVS  A222
A223   BCS  A223
       EORB DATA+5   DATA+5=@377
A224   BNE  A224
       LDAB DATA+5   DATA+5=@377
       ADDB DATA+5   N=1,Z=0,V=0,C=1
A225   BPL  A225
A226   BEQ  A226
A227   BVS  A227
A228   BCC  A228
       EORB DATA+11  DATA+11=@376
A229   BNE  A229
       LDAB DATA+3   DATA+3=@0
       ADDB DATA+3   N=0,Z=1,V=0,C=0
A230   BMI  A230
A231   BNE  A231
A232   BVS  A232
A233   BCS  A233
       EORB DATA+3
A234   BNE  A234
*********************************************
*
*               SUBTRACT MEMORY FROM ACCUMULATOR A (SUB)
*
       LDAA DATA+5   DATA+5=@377
       SUBA DATA+5   N=0,Z=1,V=0,C=0
A140   BMI  A140
A141   BNE  A141
       BVS  *
A143   BCS  A143
       LDAA DATA+10  DATS"+10""=@277
       SUBA DATA+3   DATA+3=0,N=1,Z=0,V=0,C=0
A144   BPL  A144
A145   BEQ  A145
A146   BVS  A146
A147   BCS  A147
       EORA DATA+10
A148   BNE  A148
*********************************************
*
*               SUBTRACT MEMORY FROM ACCUMULATOR B (SUB)
*
       LDAB DATA+5   DATA+5=@377
       SUBB DATA+5   N=0,Z=1,V=0,C=0
A240   BMI  A240
A241   BNE  A241
A242   BVS  A242
A243   BCS  A243
       LDAB DATA+10  DATA+10=@277
       SUBB DATA+3   DATA+3=0.N=1,Z=0,V=0,C=0
A244   BPL  A244
A245   BEQ  A245
A246   BVS  A246
A247   BCS  A247
       EORB DATA+10
A248   BNE   A248
*********************************************
*
*               ADD WITH CARRY,ACCUMULATOR A (ADC)
*
       LDAA DATA+1   DATA+1=@125
       ADDA DATA+3   DATA+3=0,RESULT=@125,
*                    CARRY SET TO ZERO
       ADCA DATA+5   DATA+5=@377,RESULT=@124,
*                    N=0,Z=0,V=0,C=1
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       STAA TMP      NO,SAVE RESULT
       EORA DATA+7   DATA+7=@124.SET UP
*                    EXEC TEST
       BNE  *        RESULT=@124?
       LDAA TMP      YES RESTORE ACCUM A
       BCC  *        VERIFY C=1
       ADCA DATA+3   DATA+3=0,RESULT=@125
*                    Z=0,C=0
       BEQ  *        Z=1?
       BCS  *        NO,C=1?
       EORA DATA+1   DATA+1=@125,SET
*                    UP EXEC TEST
       BNE  *        RESULT=@125?
       LDAA DATA+15  YES,SET UP NEXT TEST,
*                    DATA+15=@001,C=0,Z=0
       ADCA DATA+5   DATA+5=@377,C=0,SO RESULT
*                   =0,N=0,Z=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       EORA DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT=0?
       CLC           YES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       ADCA DATA+5   ADD @377& C=0 TO
*                    ZERO,RESULT=@377,N=1,Z=0,V
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       EORA DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAA DATA+5   YES,RESTORE ACCUM A
       ADCA DATA+8   DATA+8=@200. ADDIN
*                 @200,C=0.RESULT=@177,N=0,V=1,
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       ADCA DATA+8   NO,DATA+8=@200.ADD IN
*                    @200 & C=1.RESULT
*                    =0,V=0
       BVS  *        V=1?
       EORA DATA+3   NO.DATA+3=0,SET UP EXEC TES
*                    =0,V=0
       BNE  *        RESULT=ZERO?
       NOP           YES
*********************************************
*
*               ADD WITH CARRY,ACCUMULATOR B (ADC)
*
       LDAB DATA+1   DATA+1=@125
       ADDB DATA+3   DATA+3=0,RESULT=@125,
*                    CARRY SET TO ZERO
       ADCB DATA+5   DATA+5=@377,RESULT=@124,
*                    N=0,V=0,C=1
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       STAB TMP      NO, SAVE RESULT
       EORB DATA+7   DATA+7=@124.SET UP
*                    EXEC TEST
       BNE  *        RESULT=@124?
       LDAB TMP      YES,RESTORE ACCUM B
       BCC  *        VERIFY C=0
       ADCB DATA+3   DATA+3=0,RESULT=
*                    Z=0,C=0
       BEQ  *        Z=1?
       BCS  *        NO,C=1?
       EORB DATA+1   NO,DATA+1=@125,
*                    SET UP EXEC TEST
       BNE  *        RESULT=@125?
       LDAB DATA+15  YES,SET UP NEXT TEST,
*                    DATA+15=@001,C=0,Z=0
       ADCB DATA+5   DATA+5=@377,RESULT=0,
*                    N=0,Z=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       EORB DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT=0?
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       ADCB DATA+5   YES,ADD @377&C=0,RESULT=
*                    @377,N=1,Z=0,V=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       EORB DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAB DATA+5   YES,RESTORE ACCUM B
       ADCB DATA+8   DATA+8 =@200. ADD IN
*                    WITH C=0.RESULT=@177,N=0,V
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       ADCB DATA+8   NO,DATA+8=@200. ADD IN
*                    @200 WITH C=1. RESULT
*                    =0,V=0
       BVS  *        V=1?
       EORB DATA+3   NO,DATA+3=0. SET UP
*                    EXEC TEST
       BNE  *        RESULT=0?
       NOP           YES
*********************************************
*
*               SUBTRACT WITH CARRY,ACCUMULATOR A (SBC)
*
       LDAA DATA+14  DATA+14=@177,SET UP TEST,
       SEC           SET CARRY,
       BCC  *        AND VERIFY CARRY SET.N=0,Z=
       SBCA DATA+13  DATA+13=@176,ACCUM A=0
*                         N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0? ALSO EXEC TEST.
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       SBCA DATA+15  DATA+15=@001,ACCUM A=A377,
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA DATA+5   NO,DATA+5=@377,SET UP
*                   EXEC TEST
       BNE  *        SUBTRACTION RESULT=@377?
       LDAA DATA+5   YES,DATA+5=@377,SET
*                   UP NEXT TEST,N=1.Z=0,V=0,C=
       SBCA DATA+14  DATA+14=@177;ACCUM A=@177,
*                   N=0,Z=0,V=1,C=0
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORA DATA+14  NO,DATA+14=@177,SET UP
*                   EXEC TEST
       BNE  *        SUBTRACTION RESULT=@177?
       LDAA DATA+14  YES,SET UP NEXT TEST,
       SEV           SET OVERFLOW,
       BVC  *        AND VERIFY OVERFLOW SET.V=1
       SBCA DATA+3   DATA+3,V=0
       BVS  *        V=1?
       EORA DATA+14  NO,SET UP EXEC TEST
       BNE  *        SUBTRACTED RESULT=@177?
       NOP           YES
*********************************************
*
*               SUBTRACT WITH CARRY ACCUM B (SBC)
*
       LDAB DATA+14  DATA+14=@177,SET UP TEST
       SEC           SET CARRY,
       BCC  *        AND VERIFY CARRY SET.N=0,Z=
       SBCB DATA+13  DATA+13=@176,ACCUM B=0,
*                   N=0,Z=1,V=0,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0? ALSO EXEC TEST
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       SBCB DATA+15  DATA+15=@001,ACCUM B=@377,
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB DATA+5   NO,DATA+5=@377,SET UP
*                   EXEC TEST
       BNE  *        SUBTRACTION RESULT=@377?
       LDAB DATA+5   YES,DATA+5=@377,SET
*                   UP NEXT TEST,N=1.Z=0,V=0,C=
       SBCB DATA+14  DATA+14=@177;ACCUM A=@177,
*                   N=0,Z=0,V=1,C=0
       BMI    *      N=1?
       BEQ    *      NO,Z=1?
       BVC    *      NO,V=0?
       BCS    *      NO,C=1?
       EORB DATA+14  NO,DATA+14=@177,SET UP
*                   EXEC TEST
       BNE  *        SUBTRACTION RESULT=@177?
       LDAB DATA+14  YES,SET UP NEXT TEST,
       SEV           SET OVERFLOW,
       BVC  *        AND VERIFY OVERFLOW SET.V=1
       SBCB DATA+3   DATA+3=0,V=0
       BVS  *        V=1?
       EORB DATA+14  NO,SET UP EXEC TEST
       BNE  *        SUBTRACTED RESULT=@177?
       NOP           YES
*********************************************
*
*               COMPARE ACCUMULATOR A TO MEMORY (CMP)
*
       LDAA DATA+14  DATA+14=@177,SET UP TEST
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
*                   N=0,Z=0,V=0,C=0
       CMPA DATA+14  ACCUM A=@177,N=0,Z=1,V=0,C
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORA DATA+14  NO,SEY UP EXEC TEST
       BNE  *        WAS ACCUM A STILL=@177?
*                   YES.N=0,Z=1,V=0,C"=1,ACCUM
       CMPA DATA+15  DATA+15=@001,N=1,Z=0,V=0,C
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA DATA+3   NO,SET UP EXEC TEST,DATA+3=
       BNE  *        WAS ACCUM A STILL=0?
       LDAA DATA+11  YES,SET UP NEXT TEST
*                   DATA+11=@376,N=1,Z=0,V=0,C=
       CMPA DATA+14  DATA+14=@177,N=0,Z=0,V=1,C
       BMI  *        N=1?
       BVC  *        NO,V=0?
       EORA DATA+11  NO,SET UP EXEC TEST
       BNE  *        WAS ACCUM A STILL=@376?
       SEV           YES,SET OVERFLOW AND
       BVC  *        VERIFY.NEXT TEST SET UP,V=1
       CMPA DATA+5   DATA+5=@377,V=0
       BVS  *        V=1?
       EORA DATA+3   NO,SET UP EXEC TEST
       BNE  *        WAS ACCUM A STILL=0?
       NOP           YES
*********************************************
*
*               COMPARE ACCUMULATOR B TO MEMORY (CMP)
*
       LDAB DATA+14  DATA+14=@177,SET UP TEST
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
*                   N=0,Z=0,V=0,C=0
       CMPB DATA+14  ACCUM A=@177,N=0,Z=1,V=0,C
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORB DATA+14  NO,SEY UP EXEC TEST
       BNE  *        WAS ACCUM A STILL=@177?
*                          YES.N=0,Z=1,V=0,C"=1,ACCUM
       CMPB DATA+15  DATA+15=@001,N=1,Z=0,V=0,C
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB DATA+3   NO,SET UP EXEC TEST,DATA+3=
       BNE  *        WAS ACCUM B STILL=0?
       LDAB DATA+11  YES,SET UP EXEC TEST
*                 DATA+11=@376,N=1,Z=0,V=0,C=0
       CMPB DATA+14  DATA+14=@177,N=0,Z=0,V=1,C
       BMI  *        N=1?
       BVC  *        NO,V=0?
       EORB DATA+11  NO,SET UP EXEC TEST
       BNE  *        WAS ACCUM B STILL=@376?
       SEV           YES,SET OVERFLOW AND
       BVC  *        VERIFY.NEXT TEST SET UP,V=1
       CMPB DATA+5   DATA+5=@377,V=0
       BVS  *        V=1?
       EORB DATA+3   NO,SET UP EXEC TEST
       BNE  *        WAS ACCUM A STILL=0?
       NOP           YES
*********************************************
*
*               BIT TEST ACCUMULATOR A (BIT)
*
       LDAA DATA+5   DATA+5=@377,SET UP TEST
       SEV           SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       SEC           SET CARRY
       BCC  *        VERIFY CARRY SET
       BITA DATA+3   DATA+3=0,N=0,Z=1,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAA DATA+3   YES,SET UP NEXT TEST
       BITA DATA+5   DATA+3=0 & DATA+5=@377,
*                   N=0,Z=1,V=0,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT =0?
       LDAA DATA+5   YES,DATA+5=@377,
*                   SET UP NEXT TEST
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       SEV           SET OVERFLOW
       BVC  *        LDA A  DATA+3   DATA+3=0
       EORA DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAA DATA+3   DATA+3=0
       BITA DATA+3   N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT=0?
       NOP           YES
*********************************************
*
*               BIT TEST ACCUMULATOR B (BIT)
*
       LDAB DATA+5   DATA+5=@377,SET E    *        NO,Z=0?
       SEV           SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       SEC           SET CARRY
       BCC  *        VERIFY CARRY SET
       BITB DATA+3   DATA+3=0,N=0,Z=1,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAB DATA+3   YES,SET UP NEXT TEST
       BITB DATA+5   DATA+3=0 & DATA+5=@377,
*                   N=0,Z=1,V=0,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT=0?
       LDAB DATA+5   YES,DATA+5=@377,
*                 SET UP NEXT TEST
       CLC           CLEAR CA=1?
       BCS  *        VERIFY CARRY CLEARED
       CLV           CLEAR OVERFLOW
       BVS  *        NO,V=1?
       EORB DATA+5   NO,SET UP EXEC TEST
       BNE  *        RESULT=@377?
       LDAB DATA+3   DATA+3=0
       BITB DATA+3   DATA+3=0,N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        N/,C=1?
       EORB DATA+3   NO,SET UP EXEC TEST
       BNE  *        RESULT=0?
       NOP           YES
******************************************************
*
*      CHECK ADDRESSING MODES FOR GROUP I INSTRUCTIONS
*        ASSUMES DIRECT MODE FOR EACH INSTRUCTION HAS
*        BEEN CHECKED ABOVE.
*
******************************************************
*
       LDAA #$AA     CHECK IMMEDIATE MODE - 1 BYTE
       CMPA DATA     
       BNE *         
       LDAA #$55     
       CMPA DATA     
       BEQ *         
*               
       LDAB #$AA     CHECK IMMEDIATE MODE - 1 BYTE
       CMPB DATA     
       BNE *         
       LDAB #$55     
       CMPB DATA     
       BEQ *         
*               
       LDX #$AA55    IMMEDIATE MODE - 2 BYTE
       CPX DATA      
       BNE *         
       LDX #$55AA    
       CPX DATA      
       BEQ *         
*               
       LDAA DATA2    EXTENDED MODE - 1 BYTE
       CMPA #$55     
       BNE *         
*               
       LDAB DATA2    EXTENDED MODE - 1 BYTE
       CMPB #$55     
       BNE *         
*               
       LDX #$3344    EXTENDED MODE - 2 BYTE
       STX TMP2      
       LDAA TMP2     
       LDAB TMP2+1   
       CMPA #$33     
       BNE *         
       CMPB #$44     
       BNE *         
* 
*      INDEXED MODE - ZERO OFFSET
* 
       LDX #DATA2    INDEXED MODE - 1 BYTE
       LDAA 0,X
       CMPA 0,X
       BNE *
* 
       LDX 0,X       INDEXED - 2 BYTE
       CPX #$55AA
       BNE *
* 
*      INDEXED MODE - NON ZERO OFFSET
* 
       LDX #DATA2-128 ONE BYTE
       LDAA 128,X
       INX           BUMP SO THE OFFSET IS DIFFERENT
       CMPA 127,X
       BNE *
       INX
* 
       LDX #DATA2-128 ONE BYTE
       LDAB 128,X
       INX           BUMP SO THE OFFSET IS DIFFERENT
       CMPB 127,X
       BNE *
       INX
* 
       LDX 126,X     TWO BYTES
       CPX #$55AA
       BNE *
*
**************************************************
*
*    THE BSR AND JSR INSTRUCTIONS ARE NOT GROUP I
*      TYPE INSTRUCTIONS BUT THEY DO OCCUPY SPACE
*      IN THE GROUP I SECTION OF THE OPCODE MAP
*      AND SO ARE INCLUDED HERE.
*
**************************************************
*
*          BRANCH TO SUBROUTINE
*
*
       LDS #STACK    SETUP STACK
       BSR BR1       GO TO SUB
BR2    BRA *         SHOULD NEVER GET HERE
BR1    STS TMP       SEE WHAT SP IS NOW
       LDX TMP       SHOULD BE DECREMENTED BY 2
       CPX #STACK-2
       BNE *
       LDX 1,X       GET RETURN ADDRESS OFF OF STACK
       CPX #BR2      IS IT OKAY?
       BNE *
*
**************************************************
*
*        JUMP TO SUBROUTINE
*
*
       LDS #STACK    RE-INIT STACK
       JSR JS1       GO TO SUB
JS2    BRA *         SHOULD NEVER GET HERE
JS1    STS TMP       SEE IF SP IS DECREMENTED BY 2
       LDX TMP
       CPX #STACK-2
       BNE *
       LDX 1,X       CHECK RETURN ADDRESS ON STACK
       CPX #JS2
       BNE *
*      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*           GROUP 1 TEST COMPLETE
*
*      * * * * * * * * * * * * * * * * * * * * * * * * * * *
       NOP
       NOP
       NOP
       BRA GRP6
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 6 ****
*
*********************************************************
*
*      G R P 6
*
********************************************************
*
*
*      GROUP VI INSTRUCTIONS INCLUDE:
*        NOP
*        TAP
*        TPA
*        INX
*        DEX
*        CLV
*        SEV
*        CLC
*        SEC
*        CLI
*        SEI
*
*
*      ASSUMES FOLLOWING ALREADY HAVE BEEN TESTED:
*        GROUP I
*        BRANCHES (GROUP III)
*
*           GMCM REVISION 1.0 - SEPT 1977
*
GRP6   LDAA #0
       BNE  *
       TAP
       TPA
       EORA #$C0
       BNE  *        TEST TAP,TPA FOR 00
       LDAA #$FF
       TAP
*
       TPA
       EORA #$FF
       BNE  *        TEST TAP,TPA FOR FF
       LDAA #$AA
       TAP
*
       TPA
       EORA #$EA
       BNE  *        TEST TPA,TAP FOR AA
*
*     TEST CLV SEV
*
       LDAA #$FF
       TAP
       CLV
       TPA
       EORA #$FD     TEST CLV WITH 1 S
       BNE  *
       LDAA #$02
       TAP
       CLV
       TPA
       EORA #$C0     TEST CLV WITH 0 S
       BNE  *
       LDAA #$FD
       TAP
       SEV
       TPA
       EORA #$FF     TEST SEV WITH 1 S
       BNE  *
       LDAA #$00
       TAP
       SEV
       TPA
       EORA #$C2     TEST SEV WITH 0 S
       BNE  *
*
*     TEST CLC SEC
*
       LDAA #$FF
       TAP
       CLC
       TPA
       EORA #$FE
       BNE  *
       LDAA #$01
       TAP
       CLC
       TPA
       EORA #$C0     TEST CLC WITH 0 S
       BNE  *
       LDAA #$FE
       TAP
       SEC
       TPA
       EORA #$FF
       BNE  *
       LDAA #$00
       TAP
       SEC
       TPA
       EORA #$C1
       BNE  *        TEST SEC9 WITH 0 S
*
*     TEST CLI SEI
*
       LDAA #$FF
       TAP
       CLI
       TPA
       EORA #$EF
       BNE  *        TEST CLI WITH 1 S
       LDAA #$10
       TAP
       CLI
       TPA
       EORA #$C0
       BNE  *
       LDAA #$00
       TAP
       SEI
       TPA
       EORA #$D0
       BNE  *
       LDAA #$EF
       TAP
       SEI
       TPA
       EORA #$FF     TEST SEI WITH 1 S
       BNE  *
*
*     TEST INX
*
       LDAA #$00
       LDX  #$FFFF
       TAP
       INX
       BNE  *
       BMI  *
       BVS  *
       LDX  #$00EF
       INX
       BEQ  *
       BMI  *
       BVS  *
       TPA
       EORA #$C0
       BNE  *
       LDX  #$8080
       INX
       BVS  *
*
*     TEST FOR DEX
*
       LDAA #$00
       LDX  #$0001
       TAP
       DEX
       BNE  *
       BMI  *
       BVS  *
       DEX
       TPA
       EORA #$C0
       BNE  *
       LDX  #$8081
       DEX
       BVS  *
*
*     TEST NOP
*
Z1     LDAA #$00
       LDAB Z1+1
Z2     LDX  #$0000
       LDS  #$0FFF
       TAP
       NOP
       EORA #$00
       BNE  *
       TPA
       EORA #$C4
       BNE  *
       EORB #$00
       BNE  *
       CPX  Z2+1
       BNE  *
       TSX
       DEX
       CPX  #$0FFF
       BNE  *
       LDAA #$FF
       TAP
       NOP
       TPA
       EORA #$FF
       BNE  *
*
*           GROUP 6 TEST COMPLETE-RETURN TO START
*
       NOP
       NOP
       BRA GRP2
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 2 ****
*
************************************************************
*
*      G R P 2
*
*************************************************************
*
*
*
*      REVISION 1.1 - GMCM - SEPT 1977
*
*      GROUP II INSTRUCTIONS INCLUDE:
*       NEG
*       COM
*       LSR
*       ROR
*       ASR
*       ASL
*       ROL
*       DEC
*       INC
*       TST
*       JMP
*       CLR
*
*      PROGRAM TO TEST MPU GROUP 2 INSTRUCTIONS
*
*      THIS TEST ASSUMES THE FOLLOWING HAVE ALREADY
*      BEEN TESTED:
*       GROUP I
*       BRANCHES (GROUP III)
*       GROUP VI
*
************************************************
*
*               NEGATE ACCUMULATOR A (NEG)
*
GRP2   LDAA OC377
       NEGA          NEGATE @377 TO @001,N=0,Z=0
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA OC001    NO,SET UP EXECUTION TEST.
       BNE  *        WAS NEGATED NUMBER = @001
       LDAA ZERO     YES
       NEGA          NEGATE ZERO TO ZERO,N=0,Z=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA ZERO     NO,SET UP EXECUTION TEST.
       BNE  *        WAS NEGATED NUMBER = ZERO?
       LDAA OC200    YES
       NEGA          NEGATE @200 TO @200.N=1,Z=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC200    NO,SET UP EXEC TEST
       BNE  *        WAS NEGATED NUMBER = @200?
       NOP           YES
*******************************************
*
*             NEGATE ACCUMULATOR B (NEG)
*
       LDAB OC377
       NEGB          NEGATE @377 TO @001,N=0,Z=0
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB OC001    NO,SET UP EXECUTION TEST.
       BNE  *        WAS NEGATED NUMBER = @001?
       LDAB ZERO     YES
       NEGB          NEGATE ZERO TO ZERO,N=0,Z=|
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB ZERO     NO,SET UP EXECUTION TEST.
       BNE  *        WAS NEGATED NUMBER = ZERO?
       LDAB OC200    YES
       NEGB          NEGATE @200 TO @200.N=1,Z=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC200    NO,SET UP EXECUTION TEST
       BNE  *        WAS NEGATED NUMBER = @200?
       NOP           YES
*******************************************
*
*             NEGATE MEMORY (NEG)
*
       LDAA OC377
       STAA TMP      SET MEMORY LOCATION TO BE N
       NEG  TMP      NEGATE @377 TO @100,N=0,Z=0
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,GET NEGATED NUMBER
       EORA OC001    SET UP EXECUTION TEST
       BNE  *        IS NDGATED NUMBER = @001?
       NOP           YES
*******************************************
*
*             COMPLIMENT ACCUMULATOR A (COM)
*
       LDAA OC377
       COMA          COMPLIMENT @377 TO ZERO,N=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA ZERO     NO,SET  UP EXECUTION TEST
       BNE  *        WAS COMPLIMENTED NUMBER =ZE
       COMA          COMPLIMENT ZERO TO @377,N=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA OC377    NO,SET UP EXECUTION TEST
       BNE  *        WAS COMPLIMENTED NUMBER = @
       NOP           YES
*******************************************
*
*             COMPLIMENT ACCUMULATOR B (COM)
*
       LDAB OC377
       COMB          CMPLMNT @377 TO ZERO,
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        COMPLMNTD NUMBER=ZERO?
       COMB          YES,CMPLMNT ZERO TO @377
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB OC377    NO,SET UP EXEC TEST
       BNE  *        COMPLMNTD NUMBER=@377?
       NOP           YES
*******************************************
*
*             COMPLIMENT MEMORY (COM)
*
       LDAA OC377
       STAA TMP      SET UP MEMORY LOCATION
*                          TO BE COMPLIMENTED
       COM  TMP      CMPLMNT @377 TO ZERO,
*                         N=0,Z=1,V=0,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,GET CMPLMNTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        COMPLMNTD NUMBER=@377?
       NOP           YES
*******************************************
*
*             LOGICAL SHIFT RIGHT ACCUM A (LSR)
*
       LDAA OC377    SET UP ACCUM A
       LSRA          SHIFT ONCE TO @177,
*                         N=0,Z=0,V=1,C=1
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC177    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@177?
       LDAA OC177    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @077
       EORA OC077    SET UP EXEC TES"T
       BNE  *        SHFTD NUMB=@077
       LDAA OC077    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @037
       EORA OC037    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@037?
       LDAA OC037    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @017
       EORA OC017    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@017?
       LDAA OC017    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @007
       EORA OC007    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@007?
       LDAA OC007    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @003
       EORA OC003    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@003?
       LDAA OC003    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO @001
       EORA OC001    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@001?
       LDAA OC001    YES,RESTORE ACCUM A
       LSRA          SHIFT ONCE TO ZERO
*                    N=0,Z=1,V=1,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=0?
       LSRA          YES,SHIFT ONCE AGAIN TO
*                     ZERO,N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       NOP           YES
*******************************************
*
*             LOGICAL SHIFT RIGHT ACCUM B (LSR)
*
       LDAB OC377    SET UP ACCUM B
       LSRB          SHIFT ONCE TO @177,
*                   N=0,Z=0,V=1,C=1
       BMI  *        N=1?
       BEQ  *        NO,Z=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC177    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@177?
       LDAB OC177    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @077
       EORB OC077    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@077
       LDAB OC077    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @037
       EORB OC037    SET UP EXEC TEST
       BNE  *        SHIFTD NUMB=@037?
       LDAB OC037    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @017
       EORB OC017    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@017
       LDAB OC017    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @007
       EORB OC007    SET UP EXEC TEST
       BNE  *        SHIFTD NUMB=@007?
       LDAB OC007    YES, RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @003
       EORB OC003    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=003?
       LDAB OC003    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO @001
       EORB OC001    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@001?
       LDAB OC001    YES,RESTORE ACCUM B
       LSRB          SHIFT ONCE TO ZERO,
*                   N=0,Z=1,V=1,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=ZERO?
       LSRB          YES,SHIFT ONCE ZERO STAYS,
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       NOP           NO
*******************************************
*
*             LOGICAL SHIFT RIGHT MEMORY (LSR)
*
       LDAA OC377
       STAA TMP      SET UP MEM LOCATION
*                          TO BE SHIFTED
       LSR  TMP      SHIFT TMP TO @177,
*                          N=0,Z=0,V=1,C=1
       BMI  *        N=1?
       BEQ  *        N0,Z=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH SHFTD NUMBER
       EORA OC177    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@177?
       LDAA OC001    YES
       STAA TMP      SET UP MEM LOCATION
*                    TO BE SHIFTED
       LSR  TMP      SHIFT TMP TO ZERO.,
*                    N=0,Z=1,V=1,C=1
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        SHFTD NUMB=ZERO?
       LSR  TMP      YES,SHFT TMP AGAIN TO ZERO
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        SHFTD NUMB=0?
       NOP           YES
*******************************************
*
*             ROTATE ACCUMULATOR A RIGHT (ROR)
*

       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC126    SET UP TEST
       RORA          ROTATE @126 TO @053,
*                 N=0,Z=0,V=0,C=0 
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA OC053    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@053?

       LDAA OC053    SET UP TEST
       RORA          ROTATE @053 TO @025,
*                 N=0,Z=0,V=1,C=1 
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC025    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@025?
       LDAA OC025    YES,RESTORE ACCUM A &
       SEV           SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       RORA          ROTATE @025+C TO @212,
*                   N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA OC212    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMF=@212?
       LDAA OC212    YES,RESTORE ACCUM A
       RORA          ROTATE @212 TO @305,
*                    N=1,Z=0,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORA OC305    NO,SET UP EXEC TEST
       BNE  *        ES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC001    SET UP NEXT TEST
       RORA          ROTATE @001 TO
*                   ZERO,Z=1
       BNE  *        Z=0?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        Z=0?
       RORA          YES,ROTATE TO
*            CLEARY CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAB OC126    SET UP TEST
       RORB          ROTATE @126 TO @053,
*                 N=0,Z=0,V=0,C=0 
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB OC053    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@053?

       LDAB OC053    SET UP TEST
       RORB          ROTATE @053 TO @025,
*                 N=0,Z=0,V=1,C=1 
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC025    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@025?
       LDAB OC025    YES,RESTORE ACCUM A &
       SEV           SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       RORB          ROTATE @025+C TO @212,
*                   N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB OC212    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMF=@212?
       LDAB OC212    YES,RESTORE ACCUM A
       RORB          ROTATE @212 TO @305,
*                    N=1,Z=0,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORB OC305    NO,SET UP EXEC TEST
       BNE  *        ES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAB OC001    SET UP NEXT TEST
       RORB          ROTATE @001 TO
*                   ZERO,Z=1
       BNE  *        Z=0?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        Z=0?
       RORB          YES,ROTATE TO
*            CLEARY CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC126    SET UP TEST
       STAA TMP      SET UP MEM LOCATION TO BE S
       ROR  TMP      ROTATE @126 TO @053
*                   N=0,Z=0,V=0,C=0
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH RORATED NUMB
       EORA OC053    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@053?
       ROR  TMP      ROTATE @053 TO @025,
*                   N=0,Z=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      FETCH ROTATED NUMB
       EORA OC025    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@025?
       SEV           YES,SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ROR  TMP      ROTATE @025 TO @212,
*                   N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC212    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@212?
       ROR  TMP      ROTATE @212 TO @305,
*                   N=1,Z=0,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC305    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@305?
       ROR  TMP      ROTATE @305 TO @142,N=0
       BMI  *        N=1?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC142    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@142?
       CLC           YES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC001    SET UP EXEC TEST
       STAA TMP      SET UP MEM LOCATION TO BE R
       ROR  TMP      ROTATE @001 TO
*                   ZERO,Z=1
       BNE  *        Z=0?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        ROTATED NUMB=ZERO?
       ROR  TMP      YES,ROTATE ZERO TO
*                   @200,Z=0
       BEQ  *        Z=1?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC200    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@200?
       NOP           YES
*******************************************
*
*             ARITHMETICALLY SHIFT RIGHT ACCUMULATOR
*
       LDAA OC125    SET UP TEST
       ASRA          SHIFT @125 TO @052,
*                   N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC052    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@052?
       LDAA OC052    YES,RESTORE ACCUM A
       SEV           & SET (OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ASRA          SHIFT @052 TO @025,
*                   N=0,V=0,C=0
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA OC025    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@025?
       LDAA OC252    YES,SET UP NEXT TEST
       ASRA          SHIFT @252 TO @325,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORA OC325    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=A325?
       LDAA OC325    YES,RESTORE ACCUM A &
       SEV           SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ASRA          SHIFT @325 TO @352,
*                   N=1,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA OC352    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@352?
       LDAA OC001    YES,SET UP NEXT TEST
       ASRA          SHIFT @001 TO ZERO,Z=1
       BNE  *        Z=0?
       NOP           NO
*******************************************
*
*             ARITHMETICALLY SHIFT RIGHT ACCUM B (ASR)
*
       LDAB OC125    SET UP TEST
       ASRB          SHIFT @125 TO @052,
*                   N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC052    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@052?
       LDAB OC052    YES,RESTORE ACCUM B
       SEV           & OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ASRB          SHIFT @052 TO @025
*                          N=0,V=0,C=0
       BMI  *        N=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB OC025    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@025?
       LDAB OC252    YES,SET UP NEXT TEST
       ASRB          SHIFT @252 TO @325,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORB OC325    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@325?
       LDAB OC325    YES,RESTORE ACCUM B
       SEV           &SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ASRB          SHIFT @325 TO @352,
*                   N=1,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB OC352    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@352
       LDAB OC001    YES,SET UP NEXT TEST
       ASRB          SHIFT @001 TO ZERO,Z=1
       BNE  *        Z=0?
       NOP           NO
*******************************************
*
*             ARITHMETICALLY SHIFT RIGHT MEMORY (ASR)
*
       LDAA OC125    SET UP TEST
       STAA TMP
       ASR  TMP      SHIFT @125 TO @052,
*                   N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC052    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@052?
       SEV           SHFTD NUMB =@025?
       LDAA OC252    YES,SET UP NEXT TEST
       STAA TMP
       ASR  TMP      SHIFT @252 TO @325,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC325    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@325?
       SEV           YES,SET OVERFLOW
       BVC  *        VERIFY OVERFLOW SET
       ASR  TMP      SHIFT@325 TO @352
*                   N=1,V=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1          NO
*******************************************
*
*             ARITHMETICALLY SHIFT LEFT ACCUM A (ASL)
*
       LDAA OC135
       ASLA          SHIFT @135 TO @272,
*                   N=1,Z=0,V=1,C=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVC  *        64 NO,SET UP EXEC TEST
       BCS  *        NO,C=1?
       
       EORA OC272    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@272?
       LDAA OC272    YES,RESTORE ACCUM A
       ASLA          SHIFT @272 TO @164
*                          N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC164    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@164?
       LDAA OC164    YES,RESTORE ACCUM A
       ASLA          SHIFT @164 TO @350,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORA OC350    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@350?
       LDAA OC350    YES,RESTORE ACCUM A
       ASLA
*
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        C=0?
       EORA OC320    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@320?
       LDAA OC320    YES,RESTORE ACCUM A
       ASLA          SHIFT @320 TO
*                   @240,V=0
       BVS  *        V=1?
       ASLA          NO,SHIFT @240 TO
*                   @100,V=1
       BVC  *         =0?
       ASLA          NO,SHIFT @100
*                    TO @200,Z=0
       BEQ  *        Z=1?
       ASLA          NO,SHIFT @200
*                    TO ZERO,Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       EORA ZERO     NO,SET UP EXEC TEST
*******************************************
*
*       ARITHMETICALLY SHIFT LEFT ACCUM B (ASL)
*
       LDAB OC135
       ASLB          SHIFT @135 TO @272,
*                   N=1,Z=0,V=1,C=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVC  *        64 NO,SET UP EXEC TEST
       BCS  *        NO,C=1?
       
       EORB OC272    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@272?
       LDAB OC272    YES,RESTORE ACCUM B
       ASLB          SHIFT @272 TO @164
*                          N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC164    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@164?
       LDAB OC164    YES,RESTORE ACCUM B
       ASLB          SHIFT @164 TO @350,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORB OC350    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@350?
       LDAB OC350    YES,RESTORE ACCUM B
       ASLB
*
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        C=0?
       EORB OC320    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@320?
       LDAB OC320    YES,RESTORE ACCUM B
       ASLB          SHIFT @320 TO
*                   @240,V=0
       BVS  *        V=1?
       ASLB          NO,SHIFT @240 TO
*                   @100,V=1
       BVC  *         =0?
       ASLB          NO,SHIFT @100
*                    TO @200,Z=0
       BEQ  *        Z=1?
       ASLB          NO,SHIFT @200
*                    TO ZERO,Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       EORB ZERO     NO,SET UP EXEC TEST
****************
*
*             ARITHMETICALLY SHIFT LEFT MEMORY (ASL)
*
       LDAA OC135
       STAA TMP      SET UP MEM LOCATION TO BE S
       ASL  TMP      SHIFT @135 TO @272,
*                     N=1,Z=0,V=1,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVC  *        NO,NO,V=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC272    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@272
       ASL  TMP      YES,SHIFT @272 TO @164,
*                   N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC164    NO,SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@164?
       ASL  TMP      YES,SHIFT @164 TO @350,
*                   N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC350    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@350?
       ASL  TMP      YES,SHIFT @350 TO @320
*                 N=1,Z=0,C=1
       BPL  *        N=0?
       BVS  *        NO,V=1?
       BCC  *        C=0?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA OC320    SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@320?
       ASL  TMP      YES,SHIFT @320 TO
*                    @240,V=0
       BVS  *        V=1?
       ASL  TMP      NO,SHIFT @240
*                    TO @100,V=1
       BVC  *        V=1?
       ASL  TMP      NO,SHIFT @100
*                 TO @200,Z=0
       BEQ  *        Z=1?
       ASL  TMP      NO,SHIFT @200
*                    TO ZERO,Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       LDAA TMP      NO,FETCH SHFTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        SHFTD NUMB=@200?
       ASL  TMP      YES,SHIFT,CLEARING C BIT,
*                    Z=1,V=0,C=0
       BNE  *        Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       NOP           NO
*******************************************
*
*             ROTATE LEFT ACCUMULATOR A (ROL)
*
       LDAA OC252    NO,RESTORE ACCUM A       
       ROL A         ROTATE @252 TO @124,
*               N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORA OC124    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@124?
       LDAA OC124    YES,RESTORE ACCUM A
       ROLA          ROTATE @124 TO @251,
*                    N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORA OC251    NO,SET UP EXECTEST
       BNE  *        ROTATED NUMB=@251?
       LDAA OC251    YES,RESTORE ACCUM A
       ROLA          ROTATE @251 TO @122,
*                    N=0,C=1
       BMI  *        N=1?
       BCC  *        NO,C=0?
       EORA OC122    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@122?
       LDAA OC200    YES,SET UP NEXT TEST
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       ROLA          ROTATE @200 TO ZERO,
*                    Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       ROLA          NO,ROTATE ZERO TO @001,
*                    Z=0,V=0
       BEQ  *        Z=1?
       BVS  *        NO,V=1?
       LDAA OC300    NO,SET UP NEXT TEST
       ROLA          ROTATE @300 TO
*                    @200,V=0
       BVS  *        V=1?
       NOP           NO
*******************************************
*
*             ROTATE LEFT ACCUMULATOR B (ROL)
*
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY IS CLEARED
       LDAB OC252    SET UP TEST
       ROLB          ROTATE @252 TO @124,
*                    N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       EORB OC124    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@124?
       LDAB OC124    YES,RESTORE ACCUM B
       ROLB          ROTATE @124 TO 251,
*                    N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       EORB OC251    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@251?
       LDAB OC251    YES,RESTORE ACCUM B
       ROLB          ROTATE @251 TO @122,
*                    N=0,C=1
       BMI  *        N=|?
       BCC  *        NO,C=0?
       EORB OC122    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@122?
       LDAB OC200    YES,SET UP NEXT TEST
       CLC           CLEAR CARRY
       BCS  *        CLEARED
       ROLB          ROTATE @200 TO ZERO,
*                    Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       ROLB          NO,ROTATE ZERO TO @001,
*                          Z=0,V=0
       BEQ  *        Z=1?
       BVS  *        NO,V=1?
       LDAB OC300    NO,SET UP NEXT TEST
       ROLB          ROTATE @300 TO
*                    @200,V=0
       BVS  *        V=1?
       NOP           NO
*******************************************
*
*             ROTATE LEFT MEMORY (ROL)
*
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC252    SET UP TEST
       STAA TMP      SET UP MEM LOCATION TO BE R
       ROL  TMP      ROTATE @252 TO @124,
*                 N=0,V=1,C=1
       BMI  *        N=1?
       BVC  *        NO,V=0?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH ROTATED NUMB       
       EORA OC124    NO,SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@124?
       ROL  TMP      ROTATE @124 TO 251,
*                    N=1,V=1,C=0
       BPL  *        N=0?
       BVC  *        NO,V=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC251    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@251?
       ROL  TMP      YES,ROTATE @251 TO @122,
*                 N=0,C=1
       BMI  *        N=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH ROTATED NUMB
       EORA OC122    SET UP EXEC TEST
       BNE  *        ROTATED NUMB=@122?
       LDAA OC200    YES, SET UP NEXT TEST
       STAA TMP      SET MEM LOCATION TO BE ROTA
       CLC           CLEAR CARRY
       BCS  *        CLEARED
       ROL  TMP      ROTATE @200 TO ZERO,
*                    Z=1,V=1
       BNE  *        Z=0?
       BVC  *        NO,V=0?
       ROL  TMP      NO,ROTATE ZERO TO @001,
*                          Z=0,V=0
       BEQ  *        Z=1?
       BVS  *        NO, V=1?
       LDAA OC300    NO,SET UP NEXT TEST
       STAA TMP      SET UP MEM LOCATION TO BE R
       ROL  TMP      ROTATE @300 TO
*                 @200,V=0
       BVS  *        V=1?
       NOP           NO
********************************************
*
*             DECREMENT ACCUMULATOR A (DEC)
*
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC001    SET UP TEST
       DECA          DCRMT @001 TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=ZERO?
       SEC           YES,SET CARRY
       BCC  *        VERIFY CARRY SET
       DECA          DCRMT ZERO TO -1(@377),
*                    N=1,Z=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BCC  *        NO,C=0?
       EORA OC377    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB= -1(377)?
       LDAA OC377    YES,RESTORE ACCUM A
       DECA          DCRMT TO -2(@376)
*                    V=0,C=1
       BVS  *        V=1?
       BCC  *        NO,C=0?
       EORA OC376    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB = -2(@376)?
       LDAA OC200    YES,SET UP NEXT TEST
       DECA          DCRMT @200 TO
*                    @177,V=1
       BVC  *        V=0?
       EORA OC177    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=@177?
       NOP           YES
********************************************
*
*             DECREMENT ACCUMULATOR B (DEC)
*
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAB OC001    SET UPPTEST
       DECB          DCRMT @001 TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=ZERO?
       SEC           YES,SET CARRY
       BCC  *        VERIFY CARRY SET
       DECB          DCRMT ZERO TO -1(@377)
*                    N=1,Z=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BCC  *        NO,C=0?
       EORB OC377    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB= -1(@377)?
       LDAB OC377    YES,RESTORE ACCUM B
       DECB          DCRMT TO -2(@376),
*                    V=0,C=1
       BVS  *        V=1?
       BCC  *        NO,C=0?
       EORB OC376    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB= -2(@376)?
       LDAB OC200    YES,SET UP NEXT TEST
       DECB          DCRMT @200 TO
*                    @177,V=1
       BVC  *        V=0?
       EORB OC177    NO,SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=@177?
       NOP           YES
********************************************
*
*             DECREMENT MEMORY (DEC)
*
       CLC           CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC001    SET UP TEST
       STAA TMP      SET UP MEM LOCATION TO BE D
       DEC  TMP      DCRMT @001 TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH DCRMTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=ZERO?
       SEC           YES,SET CARRY
       BCC  *        VERIFY CARRY SET
       DEC  TMP      DCRMT ZERO TO -1(@377),
*                    N=1,Z=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH DCRMTD NUMB
       EORA OC377    SET UP EXEC TEST
       BNE  *        DCRMTD NUMB= -1(@377)?
       DEC  TMP      YES,DCRMT TO -2(@376),
*                    V=0,C=1
       BVS  *        V=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH DCRMTD NUMB
       EORA OC376    SET UP EXEC TEST
       BNE  *        DCRMTD NUMB= -2(@376)?
       LDAA OC200    YES,SET UP NEXT TEST
       STAA TMP      SET UP MEM LOCATION TO BE D
       DEC  TMP      DCRMT @200 TO
*                    @177,V=1
       BVC  *        V=0?
       LDAA TMP      NO,FETCH DCRMTD NUMB
       EORA OC177    SET UP EXEC TEST
       BNE  *        DCRMTD NUMB=@177?
       NOP           YES
********************************************
*
*             INCREMENT ACCUMULATOR A (INC)
*
       SEC           SET CARRY
       BCC  *        VERIFY CARRY SET
       LDAA OC376    ACCUM A= -2(@376)
       INCA          ICRMT TO -1(@377),
*                    N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORA OC377    NO,SET UP EXEC TEST
       BNE  *        INCRMTD NUMB=@377?
       CLC           YES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAA OC377    RESTORE ACCUM A
       INCA          ICRMT TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=ZERO
       LDAA OC177    YES,SET UP NEXT TEST
       BVS  *        VERIFY OVERFLOW= 0
       INCA          ICRMT TO @200,V=1
       BVC  *        V=0?
       EORA OC200    NO,SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=@200
       NOP           YES
********************************************
*
*             INCREMENT ACCUMULATOR B (INC)
*
       SEC           SET CARRY
       BCC  *        VERIFY CARRY SET
       LDAB OC376    ACCUM B= -2(@376)
       INCB          ICRMT TO -1(@377),
*                    N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       EORB OC377    NO,SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=@377?
       CLC           YES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       LDAB OC377    RESTORE ACCUM B
       INCB          ICRMT TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=ZERO?
       LDAB OC177    YES,SET UP NEXT TEST
       BVS  *        VERIFY OVERFLOW=0
       INCB          ICRMT TO @200,V=1
       BVC  *        V=0?
       EORB OC200    NO,SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=@200?
       NOP           YES
********************************************
*
*             INCREMENT MEMORY (INC)
*
       SEC           SET CARRY
       BCC  *        VERIFY CARRY SET
       LDAA OC376
       STAA TMP      SET UP MEM LOCATION TO BE I
       INC  TMP      ICRMT TO -1(@377),
*                    N=1,Z=0,V=0,C=1
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCC  *        NO,C=0?
       LDAA TMP      NO,FETCH ICRMTD NUMB
       EORA OC377    SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=@377
       CLC           YES,CLEAR CARRY
       BCS  *        VERIFY CARRY CLEARED
       INC  TMP      ICRMT TO ZERO,
*                    N=0,Z=1,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH ICRMTD NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        ICRMTD NUMB= ZERO?
       LDAA OC177    YES,SET UP NEXT TEST
       STAA TMP      SET UP MEM LOCATION TO BE I
       BVS  *        VERIFY OVERFLOW=0
       INC  TMP      ICRMT TO @200,V=1
       BVC  *        V=0?
       LDAA TMP      NO,FETCH ICRMTD NUMB
       EORA OC200    SET UP EXEC TEST
       BNE  *        ICRMTD NUMB=@200?
       NOP           YES
********************************************
*
*             TEST ACCUMULATOR A (TST)
*
       LDAA OC377    SET UP TEST
       TSTA          N=1,Z=0,V=0,C=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA OC377    NO,SET UP EXEC TEST
       BNE  *        ACCUM A=@377?
       LDAA ZERO     YES,SET UP NEXT TEST
       TSTA          N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        ACCUM A=ZERO?
       NOP           YES
********************************************
*
*             TEST ACCUMULATOR B (TST)
*
       LDAB OC377    SET UP TEST
       TSTB          N=1,Z=0,V=0,C=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB OC377    NO,SET UP EXEC TEST
       BNE  *        TSTED NUMB=@377?
       LDAB ZERO     YES,SET UP NEXT TEST
       TSTB          N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        TESTED NUMB=0?
       NOP           YES
********************************************
*
*             TEST MEMORY (TST)
*
       LDAA OC377    SET UP TEST
       STAA TMP      SET UP MEM LOCATION TO BE T
       TST  TMP      N=1,Z=0,V=0,C=0
       BPL  *        N=0?
       BEQ  *        NO,Z=1?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH TESTED NUMB
       EORA OC377    SET UP EXEC TEST
       BNE  *        TESTED NUMB=@377?
       LDAA ZERO     YES,SET UP NEXT TEST
       STAA TMP      SET UP MEM LOCATION TO BE T
       TST  TMP      N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH TESTED NUMB
       BNE  *        TESTED NUMB=0?
       NOP           YES
********************************************
*
*             CLEAR ACCUMULATOR A (CLR)
*
       LDAA OC377    SET UP TEST
*       N=1,Z=0
       BPL  *        VERIFY N=1
       BEQ  *        VERIFY Z=0
       CLRA          CLEAR @377 TO ZERO,
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA ZERO     NO,ZET UP EXEC TEST
       BNE  *        ACCUM A=0?
       CLRA          CLEAR ZERO TO ZERO,
*                 N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORA ZERO     NO,SET UP EXEC TEST
       BNE  *        ACCUM A=ZERO?
       NOP           YES
********************************************
*
*             CLEAR ACCUMULATOR B (CLR)
*
       LDAB OC377    SET UP TEST
*                    N=1,Z=0
       BPL  *        VERIFY N=1
       BEQ  *        VERIFY Z=0
       CLRB          CLEAR @377 TO ZERO,
*                 N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB ZERO     NO,SET UP EXEC TEST
       BNE  *        ACCUM B=ZERO?
       CLRB          CLEAR ZERO TO ZERO,
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       EORB ZERO     NO, SET UP EXEC TEST
       BNE  *        ACCUM B=ZERO?
       NOP           YES
********************************************
*
*             CLEAR MEMORY (CLR)
*
       LDAA OC377    SET UP TEST
       STAA TMP      SET UP MEM LOCATION TO BE C
*                    N=1,Z=0
       BPL  *        VERIFY N=1
       BEQ  *        VERIFY Z=0
       CLR  TMP      CLEAR @377 TO ZERO,
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH CLEARED NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        CLEARED NUMB=0?
       CLR  TMP      CLEAR ZERO TO ZERO,
*                    N=0,Z=1,V=0,C=0
       BMI  *        N=1?
       BNE  *        NO,Z=0?
       BVS  *        NO,V=1?
       BCS  *        NO,C=1?
       LDAA TMP      NO,FETCH CLEARED NUMB
       EORA ZERO     SET UP EXEC TEST
       BNE  *        CLEARED NUMB=0?
       NOP           YES
*
***************************************************
*
*              JUMP
*
*
       JMP JP1          FORWARD JUMP
       BRA *
JP1    CLRA             FLAG
JP3    TSTA             TEST FLAG
       BNE JP2          ONLY GO THRU LOOP ONCE
       INCA
       JMP JP3          REVERSE JUMP
       BRA *
JP2    NOP
*
*********************************************************
*
*      CHECK GROUP II INDEX ADDRESSING MODE.
*        IT IS ASSUMED AT THIS POINT THAT ALL GROUP II
*        INSTRUCTIONS HAVE BEEN TESTED USING EXTENDED
*        AND ACUMULATOR ADDRESSING
*
*
       LDX #TMP         SETUP INDEX REG
       LDAA #$FF        PUT -1 IN TMP
       STAA TMP
       NEG 0,X          NEG USING INDEXED MODE
       LDAA TMP         SHOULD BE +1 NOW
       CMPA #1
       BNE *
       COM 0,X          COM USING INDEX MODE
       LDAA TMP         SHOULD BE $FE NOW
       CMPA #$FE
       BNE *
       LSR 0,X          SHIFT USING INDEX MODE
       LDAA TMP         SHOULD BE $7F NOW
       CMPA #$7F
       BNE *
       SEC              SET TO ROTATE IN 1
       ROR 0,X          ROR USING INDEX MODE
       LDAA TMP         SHOULD BE $BF NOW
       CMPA #$BF
       BNE *
       ASR 0,X          ASR USING INDEX MODE
       LDAA TMP         SHOULD BE $DF NOW
       CMPA #$DF
       BNE *
       ASL 0,X          ASL USING INDEX MODE
       LDAA TMP         SHOULD BE $BE NOW
       CMPA #$BE
       BNE *
       SEC              SET TO ROTATE IN 1
       ROL 0,X          ROL USING INDEX MODE
       LDAA TMP         SHOULD BE $7D NOW
       CMPA #$7D
       BNE *
       DEC 0,X          DEC USING INDEX MODE
       LDAA TMP         SHOULD BE $7C NOW
       CMPA #$7C
       BNE *
       INC 0,X          INC USING INDEX MODE
       LDAA TMP         SHOULD BE $7D AGAIN
       CMPA #$7D
       BNE *
       CLR 0,X          CLR USING INDEX MODE
       LDAA TMP         SHOULD BE ZERO
       BNE *
       TST 0,X          TST USING INDEX MODE
       BNE *
       LDX #JP4         DO INDEXXED JUMP
       JMP 0,X
       BRA *
JP4    NOP
       LDX #TMP-128     TRY INDEX WITH NON-ZERO OFFSET
       CLR 128,X
       INX              BUMP INDEX TO CHANGE OFFSET
       COM 127,X
       LDAA TMP
       CMPA #-1
       BNE *
*
**********************************************
*      ALL DONE - 
*
       NOP
       NOP 
       NOP
       BRA GRP4
*
*      DATA
*
ZERO   FCB @0
OC001  FCB @001
OC003  FCB @003
OC007  FCB @007
OC017  FCB @017
OC025  FCB @025
OC037  FCB @037
OC052  FCB @052
OC053  FCB @053
OC077  FCB @077
OC122  FCB @122
OC124  FCB @124
OC125  FCB @125
OC126  FCB @126
OC127  FCB @127
OC135  FCB @135
OC142  FCB @142
OC164  FCB @164
OC177  FCB @177
OC200  FCB @200
OC212  FCB @212
OC225  FCB @225
OC251  FCB @251
OC252  FCB @252
OC254  FCB @254
OC272  FCB @272
OC300  FCB @300
OC305  FCB @305
OC320  FCB @320
OC325  FCB @325
OC350  FCB @350
OC352  FCB @352
OC376  FCB @376
OC377  FCB @377
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 4 ****
*
*
*****************************************************
*
*      G R P 4
*
*****************************************************
*
*
*      REV 1.0
*      GMCM REVISION 1.1 - SEPT 1977
*
*      GROUP IV INSTRUCTIONS INCLUDE:
*        TAB
*        TBA
*        SBA
*        CBA
*        DAA
*        ABA
*
*      ASSUMES THE FOLLOWING HAVE ALREADY BEEN CHECKED
*      AND WORK:
*        BRANCHES (GROUP III)
*        GROUP I
*        GROUP VI
*        PSHA,PULA
*
*
*      PROGRAM TO TEST GP4 INSTRUCTIONS
*
*      TEST TAB
*
GRP4   LDAA #$00
       TAP
       TAB
       TPA
       EORA #$C4
       BNE  *        DATA 00
       EORB #$00
       BNE  *
       LDAA #$AA
       TAB
       TPA
       EORA #$C8     DATA AA
       BNE  *
       EORB #$AA
       BNE  *
       LDAA #$FF
       TAB
       TPA
       EORA #$C8
       BNE  *        DATA FF
       EORB #$FF
       BNE  *
*
*     TEST TBA
*
       LDAB #$00
       TAP
       TBA
       BCS  *
       BVS  *        DATA 00
       ABA
       BMI  *
       EORA #$00
       BNE  *
       TPA
       EORA #$C4
       BNE  *
       LDAB #$AA
       TBA
       BCS  *        DATA AA
       BVS  *
       BPL  *
       BEQ  *
       EORA #$AA
       BNE  *
       TPA
       EORA #$C4
       BNE  *
       LDAB #$FF
       TBA
       BCS  *
       BVS  *
       BPL  *
       BEQ  *
       EORA #$FF     DATA FF
       BNE  *
       TPA
       EORA #$C4
       BNE  *
*
*     TEST ABA
*
       LDS  #STACK
       LDAA #$00
       LDAB #$05
       ABA
       PSHA
       TPA
       EORA #$C0     DATA #1
       BNE  *
       PULA
       EORA #$05
       BNE  *
*
       LDAA #$7A
       LDAB #$7A
       ABA
       PSHA
       TPA
       EORA #$EA     DATA #2
       BNE  *
       PULA
       EORA #$F4
       BNE  *
*
       LDAA #$40
       LDAB #$C0
       ABA
       PSHA
       TPA           DATA 3
       EORA #$C5
       BNE  *
       PULA
       EORA #$00
       BNE  *
*
       LDAA #$00
       LDAB #$80
       ABA
       PSHA
       TPA           DATA 4
       EORA #$C8
       BNE  *
       PULA
       EORA #$80
       BNE  *
*
       LDAA #$C0
       LDAB #$40
       ABA
       PSHA
       TPA
       EORA #$C5     DATA 5
       BNE  *
       PULA
       EORA #$00
       BNE  *
*
       LDAA #$80
       LDAB #$00
       ABA
       PSHA
       TPA           DATA 6
       EORA #$C8
       BNE  *
       PULA
       EORA #$80
       BNE  *
*
       LDAA #$80
       LDAB #$80
       ABA
       PSHA
       TPA           DATA 7
       EORA #$C7
       BNE  *
       PULA
       EORA #$00
       BNE  *
*
       LDAA #$C0
       LDAB #$C0
       ABA
       PSHA
       TPA
       EORA #$C9
       BNE  *
       PULA          DATA 8
       EORA #$80
       BNE  *
*
*     TEST SBA
*
       LDAA #$0A
       LDAB #$0A
       SBA
       PSHA
       TPA
       EORA #$C4     DATA 1
       BNE  *
       PULA
       EORA #$00
       BNE  *
*
       LDAA #$70
       LDAB #$7F
       SBA
       PSHA
       TPA           DATA 2
       EORA #$C9
       BNE  *
       PULA
       EORA #$F1
       BNE  *
*
       LDAA #$48
       LDAB #$F7
       SBA
       PSHA
       TPA           DATA 3
       EORA #$C1
       BNE  *
       PULA
       EORA #$51
       BNE  *
*
       LDAA #$48
       LDAB #$B7
       SBA
       PSHA          DATA 4
       TPA
       EORA #$CB
       BNE  *
       PULA
       EORA #$91
       BNE  *
*
       LDAA #$8A
       LDAB #$7F
       SBA
       PSHA
       TPA
       EORA #$C2
       BNE  *        DATA 5
       PULA
       EORA #$0B
       BNE  *
       EORB #$7F
       BNE  *
*
       LDAA #$CB
       LDAB #$0F
       SBA
       PSHA
       TPA
       EORA #$C8
       BNE  *        DATA 6
       PULA
       EORA #$BC
       BNE  *
*
       LDAA #$CB
       LDAB #$81
       SBA
       PSHA
       TPA           DATA 7
       EORA #$C0
       BNE  *
       PULA
       EORA #$4A
       BNE  *
*
       LDAA #$8B
       LDAB #$F0
       SBA
       PSHA
       TPA           DATA 8
       EORA #$C9
       BNE  *
       PULA
       EORA #$9B
       BNE  *
       EORB #$F0
       BNE  *
*
*     TEST CBA
*
       LDAA #$0A
       LDAB #$0A
       CBA
       PSHA
       TPA           DATA 1
       EORA #$C4
       BNE  *
       PULA
       EORA #$0A
       BNE  *
       EORB #$0A
       BNE  *
*
       LDAA #$48
       LDAB #$B7
       CBA
       PSHA
       TPA
       EORA #$CB     DATA 4
       BNE  *
       PULA
       EORA #$48
       BNE  *
       EORB #$B7
       BNE  *
*
*     TEST DAA
*
       LDAA #$E1
       TAP
       LDAA #$00
       DAA           DATA 1
       PSHA
       TPA
       EORA #$E1
       BNE  *
       PULA
       EORA #$66
       BNE  *
*
       LDAA #$C0
       TAP
       LDAA #$99
       DAA           DATA 2
       PSHA
       TPA
       EORA #$C8
       BNE  *
       PULA
       EORA #$99
       BNE  *
*
       LDAA #$C0
       TAP
       LDAA #$AA
       DAA
       PSHA          DATA 3
       TPA
       EORA #$C1
       BNE  *
       PULA
       EORA #$10
       BNE  *
*
       LDAA #$C0
       TAP
       LDAA #$9A
       DAA           DATA 4
       PSHA
       TPA
       EORA #$C5
       BNE  *
       PULA
       EORA #$00
       BNE  *
*
       NOP
       NOP
       NOP           GO BACK TO START
       BRA GRP5
       TTL **** 6802 TOTAL INSTRUCTION TEST - GROUP 5 ****
*
***********************************************
*
*      G R P 5
*
************************************************
*
*
*
*
*      GROUP V INSTRUCTIONS ARE:
*       TSX
*       INS
*       PULA
*       PULB
*       DES
*       TXS
*       PSHA
*       PSHB
*       RTS
*       WAI*
*
*        * WAI WILL BE TESTED IN A SEPARATE INTERUPT DIAGNOSTIC
*          SWI AND RTI ARE NOT INCLUDED IN THIS DIAGNOSTIC
*
*      THE GROUP V TESTS ASSUME ALL OTHER GROUPS ARE ALREADY
*      WORKING AND TESTED.
*
*
*
**********************************************************
*      
*          TRANSFER SP TO X-REG (TSX)
*
*
GRP5   LDS #TEMP     LOAD STACK REG
       TSX           TRANSFER SP+1 -> X
       CPX #TEMP+1   OK?
       BNE *
       LDS #$1FFF    TRY A 16 BIT VERSION
       TSX           SP + 1 --> X
       CPX #$2000
       BNE *
       CLRA          MAKE SURE TSX DOESN'T CHANGE ANY CC'S
       TAP
       TSX
       TPA
       CMPA #$C0
       BNE *
*
**********************************************************
*
*          TRANSFER X-REG TO SP (TXS)
*
*
       LDX #TEMP+1
       LDS #0        CLEAR SP
       TXS           X-1 -> SP
       STS TEMP      SAVE NEW SP
       LDX TEMP      GET IT IN X
       CPX #TEMP     OK?
       BNE *         HANG IF NOT
       CLRA          MAKE SURE TXS DOESN'T CHANGE ANY CC'S
       TAP
       TXS
       TPA
       CMPA #$C0
       BNE *
*
***********************************************************
*
*          INCREMENT STACK POINTER (INS)
*
*
       LDS #TEMP
       INS           INC SP
       TSX           SP+1 -> X
       CPX #TEMP+2   OK?
       BNE *         HANG IF NOT
       LDS #$FF
       INS           FF+1 -> 100
       TSX           SP+1 -> X
       CPX #$101     OK?
       BNE *         HANG IF NOT
       CLRA          MAKE SURE INS DOESN'T CHANGE ANY CC'S
       TAP
       INS
       TPA
       CMPA #$C0
       BNE *
*
***********************************************************
*
*          DECREMENT STACK POINTER (DES)
*
*
DS1    LDS #$100     INIT SP
       DES           SP-1 -> SP
       TSX           SP+1 -> X
       CPX #$100     OK?
       BNE *
       CLRA          MAKE SURE DES DOESN'T CHANGE ANY CC'S
       TAP
       TSX
       TPA
       CMPA #$C0
       BNE *
*
************************************************************
*
*          PUSH A-REGISTER ON STACK (PSHA)
*
*
PSA1   LDS #STACK    INIT STACK
       LDAA #$55
       PSHA          PUSH IT
       TSX           POINT X TO LAST ON STACK
       CMPA 0,X      WAS IT PUSHED
       BNE *         HANG IF NOT
       LDAA #$AA     TRY WITH DIFFERENT PATTERN
       PSHA
       TSX
       CMPA 0,X
       BNE *
       CLRA          MAKE SURE NO CC'S ARE CHANGED
       TAP
       PSHA          DO A PSHA
       TPA           GET CC'S
       CMPA #$C0     OK?
       BNE *         HANG IF NOT
       INS
       INS
       INS           CLEAN UP STACK
*
**************************************************************
*
*          PUSH B-REGISTER ON STACK (PSHB)
*
*
PSB1   LDS #STACK    INIT STACK
       LDAB #$AA     LOAD A CONSTANT
       PSHB          PUSH IT
       TSX           POINT X TO LAST ON STACK
       CMPB 0,X      WAS IT PUSHED?
       BNE *         HANG IF NOT
I      LDAB #$55     DO ANOTHER PATTERN
       PSHB
       TSX
       CMPB 0,X
       BNE *
       CLRA          MAKE SURE NO CC'S ARE SET
       TAP
       PSHB          DO A PSHB
       TPA           GET BACK CC'S
       CMPA #$C0     OK?
       BNE *         HANG IF NOT
       INS
       INS           CLEAN STACK
       INS
*
******************************************************
*
*          PULL FROM STACK INTO A (PULA)
*
*
PLA1   LDS #STACK   INIT SP
       CLRA         PUSH ZERO
       PSHA
       COMA         SCREW UP A
       PULA         RETRIEVE A
       TSTA         SHOULD BE ZERO
       BNE *
       LDAA #$FF    PUSH ALL 1'S
       PSHA
       CLRA         CHANGE A
       PULA         SHOULD BE ALL 1'S
       CMPA #$FF
       BNE *
       CLRA         MAKE SURE NO CC'S ARE CHANGED
       TAP          BY PSHA OR PULA
       PSHA
       PULA
       TPA
       CMPA #$C0    SHOULD BE $C0
       BNE *
*
******************************************************
*
*          PULL FROM STACK INTO B (PULB)
*
*
PLB1   LDS #STACK   INIT SP
       CLRB         PUSH ZERO
       PSHB
       COMB         SCREW IT UP
       PULB         RETRIEVE IT
       TSTB         SHOULD BE ZERO
       BNE *
       LDAB #$FF    PUSH ALL 1'S
       PSHB
       CLRB         CHANGE B
       PULB         SHOULD BE ALL 1'S
       CMPB #$FF
       BNE *
       CLRA         MAKE SURE NO CC'S ARE CHANGED 
       TAP          BY PSHB OR PULB
       PSHB
       PULB
       TPA          SHOULD BE $C0
       CMPA #$C0
       BNE *
*
********************************************************
*
*          RETURN FROM SUBROUTINE (RTS)
*
*
       LDS #STACK   INIT SP
       JSR RT1      GO TO SUB
       BRA *        SHOULD'T GET HERE
       TPA          CHECK CC'S AFTER RTS
       CMPA #$C0
       BNE *
       BRA RT2      OKAY,SKIP AROUND
RT1    TSX          MODIFY RETURN ADDRESS TO SKIP BRA *
       LDAA 0,X     LOAD RETURN ADDRESS
       LDAB 1,X
       ADDB #2      FIX IT
       ADCA #0
       STAA 0,X
       STAB 1,X
       CLRA         CLEAR CC'S TO MAKE SURE
       TAP          RTS DOESN'T CHANGE ANY
       RTS
*
RT2    LDX #RT3     TRY RTS WITHOUT JSR
       STX TMP      PUT DESTINATION (RT3)
       LDAA TMP+1   ON STACK
       PSHA
       LDAB TMP
       PSHB         RT3 ON STACK
       RTS
       BRA *        SHOULDN'T GET HERE
RT3    NOP          OKAY!
*
*
********************************************************
*
PDATA1 EQU $E07E    SWTBUG PRINT STRING
MON    EQU $E0E3    SWTBUG RETURN TO MONITOR
*
*      CAN PUT BREAKPOINT AFTER HERE
BRKPT  NOP          EVERYTHINGS OKAY !!!!!!!!!!!!!!!!!!!!!!!!!
       NOP
       NOP
       LDX #OKMSG   PRINT 'OK' ON THE TERMINAL
       JSR PDATA1
       NOP
       NOP
       NOP
       JMP MON      RETURN TO SWTBUG MONITOR
*
OKMSG  FCC ' CPU TEST OK'
       FCB 4
*
*
       END 
