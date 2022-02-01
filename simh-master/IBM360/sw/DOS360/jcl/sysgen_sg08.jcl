// JOB SYSGEN8A  DITTO ASSEMBLE AND CATALOG                             DITT0001
// OPTION CATAL,LOG                                                     DITT0002
   PHASE DITTO,+0                                                       DITT0003
// EXEC ASSEMBLY                                                        DITT0004
         TITLE '* * * * * * * *     D  I  T  T  O     * * * * * * * *'  DITT0005
         SPACE 6                                                        DITT0006
*********************************************************************** DITT0007
*                                                                     * DITT0008
*                                                                     * DITT0009
* * * * * * * * * * * *      D I T T O        * * * * * * * * * * * * * DITT0010
*                                                                     * DITT0011
*                                                                     * DITT0012
*        DD        DISK TO PRINTER IN CHAR. AND HEX FORMAT UNBLOCKED  * DITT0013
*        SD        DISK TO PRINTER - SPLIT CYLINDER                   * DITT0014
*        DDR       DISK TO PRINTER IN CHAR. AND HEX FORMAT REBLOCKED  * DITT0015
*        SDR       DISK TO PRINTER REBLOCKED - SPLIT CYLINDER         * DITT0016
*        DRL       DISK RECORD LOAD                                   * DITT0017
*        DRS       DISK RECORD SCAN                                   * DITT0018
*        SRS       DISK RECORD SCAN - SPLIT CYLINDER                  * DITT0019
*        EOF       WRITE DISK END OF FILE RECORD                      * DITT0020
*        DID       ALTER DISK IDENTIFICATION VOLUME NUMBER            * DITT0021
*        CC        CARD TO CARD                                       * DITT0022
*        CCS       CARD TO CARD WITH SEQ. NUMBERS AND DECK NAME       * DITT0023
*        CP        CARD TO PRINTER IN CHARACTER FORMAT                * DITT0024
*        CH        CARD TO PRINTER IN CHARACTER AND HEX FORMAT        * DITT0025
*        CT        CARD TO TAPE UNBLOCKED                             * DITT0026
*        CTR       CARD TO TAPE REBLOCKED                             * DITT0027
*        TC        TAPE TO CARD BLOCKED OR UNBLOCKED                  * DITT0028
*        TP        TAPE TO PRINTER UNBLOCKED IN CHAR. FORMAT          * DITT0029
*        TPR       TAPE TO PRINTER REBLOCKED IN CHAR. FORMAT          * DITT0030
*        TH        TAPE TO PRINTER UNBLOCKED IN CHAR. AND HEX FORMAT  * DITT0031
*        THR       TAPE TO PRINTER REBLOCKED IN CHAR. AND HEX FORMAT  * DITT0032
*        TFA       PRINT SYSLST TAPES TYPE A FORMS CONTROL            * DITT0033
*        TFD       PRINT SYSLST TAPES TYPE D FORMS CONTROL            * DITT0034
*        TT        TAPE TO TAPE  (01 TO 99) FILES                     * DITT0035
*        TRS       TAPE RECORD SCAN                                   * DITT0036
*        TRL       TAPE RECORD LOAD                                   * DITT0037
*        INT       INTIALIZE TAPE                                     * DITT0038
*        WTM       WRITE TAPE MARK                                    * DITT0039
*        REW       REWIND TAPE                                        * DITT0040
*        RUN       REWIND AND UNLOAD TAPE                             * DITT0041
*        FSR       FORWARD SPACE RECORD                               * DITT0042
*        BSR       BACK SPACE RECORD                                  * DITT0043
*        FSF       FORWARD SPACE FILE                                 * DITT0044
*        BSF       BACK SPACE FILE                                    * DITT0045
*        EOJ       END OF JOB                                         * DITT0046
*                                                                     * DITT0047
*                                                                     * DITT0048
*             C. A. ALEXANDER      IBM     DETROIT, MICHIGAN          * DITT0049
*********************************************************************** DITT0050
         EJECT                                                          DITT0051
         ISEQ  77,80                                                    DITT0052
DITTO    START                                                          DITT0053
         BALR  6,0                                                      DITT0054
         USING *,6,14,15                                                DITT0055
         L     7,BUMP              SET UP                               DITT0056
         LR    14,6                BASE                                 DITT0057
         AR    14,7                REGISTERS                            DITT0058
         LR    15,14                                                    DITT0059
         AR    15,7                                                     DITT0060
         LA    1,SAVPSWS           INTIALIZE INTERRUPT                  DITT0061
         LA    0,INTRP             ROUTINE                              DITT0062
         SVC   20                                                       DITT0063
         B     RELOCAT             GO RE-LOCATE PROGRAM                 DITT0064
BUMP     DC    F'4096'                                                  DITT0065
         SPACE 2                                                        DITT0066
*********************************************************************** DITT0067
**********         INTERRUPT ROUTINE & CONSOLE INPUT        *********** DITT0068
*********************************************************************** DITT0069
         SPACE 1                                                        DITT0070
INTRP    BC    15,XIT              FIRST TIME SWITCH                    DITT0071
         LA    8,CONSOLE           TO ENSURE                            DITT0072
         ST    8,SAVREGS           RELOCATION ROUTINE                   DITT0073
         LA    9,WRTCON            IS COMPLETE.                         DITT0074
         ST    9,SAVREGS+4                                              DITT0075
         CLC   SAVREGS+5(3),SAVPSWS+5  ARE WE IN I/O ROUTINE ?          DITT0076
         BH    INTRP1              NO,  BRANCH                          DITT0077
         ST    8,SAVPSWS+40        YES, MODIFY RETURN REG TO CONSOLE.   DITT0078
         B     INTRP1+6                                                 DITT0079
INTRP1   MVC   SAVPSWS+5(3),SAVREGS+1  SET PSW = RETURN REG             DITT0080
         MVC   CNTRLSW,OFF         TURN OFF CONTROL CARD SW             DITT0081
XIT      SVC   21                  EXIT INTERRUPT                       DITT0082
CONSOLE  MVC   SCALE(16),BLANKS    BLANK SCALE AREA                     DITT0083
         LA    8,SKCCW             RESET SEEK CCW                       DITT0084
         ST    8,DSKCCB+8          IN DISK CCB.                         DITT0085
         LA    8,DTPIO             RESET IO POINTER IN                  DITT0086
         ST    8,RWCCW             DISK CCW.                            DITT0087
         MVI   BLCT,X'FF'          RESET                                DITT0088
         MVI   BLCT+1,X'FF'        SIMULATED END OF FILE SW.            DITT0089
         MVC   INTPCCW+6(2),DSL+2  RESET TAPE LNGTH                     DITT0090
         MVC   OTTPCCW+6(2),DSL+2  IN TAPE CCWS.                        DITT0091
         MVC   OFF+1(30),OFF       RESET SWITCHES AND CONSTANTS         DITT0092
         MVC   HOLD(16),OFF        TO ZERO.                             DITT0093
         MVI   OTTPCCW,X'01'       RESET OUT TAPE TO WRITE.             DITT0094
         MVI   INTPCCB+2,X'00'     TURN OFF ERROR HANDLING - IN  TAPE.  DITT0095
         MVC   HH2,HH1+7           SET HEADING FORMAT.                  DITT0096
         MVC   ADRSLO(216),BLANKS  BLANK OUT HEADING BUFFERS.           DITT0097
         MVC   CDCCB+6,CCBSAV      RESET CARD CCB TO READ FROM SYSIPT.  DITT0098
         CLC   CNTRLSW,ON          DO WE READ CONTROL CARDS ?           DITT0099
         BE    CNTRLCD             YES,  GO TO CONTROL CARD ROUTINE.    DITT0100
         MVI   INTPCCB+2,X'02'     NO, SET IN  TAPE TO HANDLE ERRORS.   DITT0101
         MVC   CONIO(16),=C'DITTO FUNCTION ?'                           DITT0102
         MVI   WCNCCW+7,X'10'                                           DITT0103
         MVI   RCNCCW+7,X'03'      SET FOR REPLY.                       DITT0104
         BAL   8,WRTCON                                                 DITT0105
         LA    4,CONIO                                                  DITT0106
         BAL   8,LOOKUP            GO LOOK UP FUNCTION.                 DITT0107
         B     CONSOLE             INVALID FUNCTION , RETURN TO CONSOLE DITT0108
         SPACE 2                                                        DITT0109
*********************************************************************** DITT0110
**********               CONTROL CARD INPUT                 *********** DITT0111
*********************************************************************** DITT0112
         SPACE 1                                                        DITT0113
CNTRLCD  BAL   8,OVFLO             SKIP TO CHAN 1                       DITT0114
         BAL   8,CDRD              READ A CARD                          DITT0115
         MVC   PL,EIGHTY+1                                              DITT0116
         BAL   8,PRNT              PRINT THE CONTROL CARD.              DITT0117
         CLC   CONIO(7),CTL        CC 1-7 = $$DITTO ?                   DITT0118
         BNE   BADCTL              NO, GO TO BAD CONTROL CARD ROUTINE.  DITT0119
         LA    11,CONIO+15         YES,  SET UP FOR CONTROL CARD SCAN.  DITT0120
         CLI   0(11),X'40'         PARAMETERS GIVEN ?                   DITT0121
         BE    CHKID               NO,  GO CHECK ID.                    DITT0122
CKBG     CLC   BG,0(11)            PARAM = BEGIN ?                      DITT0123
         BNE   CKEN                NO, BRANCH.                          DITT0124
         LA    11,6(11)                                                 DITT0125
         MVC   ADRSLO,0(11)        YES, SAVE IT.                        DITT0126
         LA    11,5(11)                                                 DITT0127
         OI    CT,X'02'            SET BEGIN SW ON.                     DITT0128
         B     BUMPIT                                                   DITT0129
CKEN     CLC   EN,0(11)            PARAM = END ?                        DITT0130
         BNE   CKIN                NO, BRANCH                           DITT0131
         LA    11,4(11)                                                 DITT0132
         MVC   ADRSUP,0(11)        YES, SAVE IT.                        DITT0133
         LA    11,5(11)                                                 DITT0134
         OI    CT,X'01'            SET END SW ON.                       DITT0135
         B     BUMPIT                                                   DITT0136
CKIN     CLC   IN,0(11)            PARAM = INPUT ?                      DITT0137
         BNE   CKOT                NO, BRANCH.                          DITT0138
         LA    11,9(11)                                                 DITT0139
         LA    12,3(0)                                                  DITT0140
         BAL   8,TSTNUM            YES, TEST FOR NUMERIC                DITT0141
         B     BADCTL                                                   DITT0142
         CLC   SYSMAX,0(11)        TEST FOR UPPER LIMIT.                DITT0143
         BNH   BADCTL                                                   DITT0144
         MVC   INL,0(11)           SAVE IT.                             DITT0145
         MVC   HOLD+5(3),0(11)                                          DITT0146
         BAL   8,BINCVT            CONVERT TO BINARY                    DITT0147
         STC   1,INPUT             AND STORE.                           DITT0148
         LA    11,3(11)                                                 DITT0149
         OI    CT,X'80'            SET INPUT SW ON.                     DITT0150
         B     BUMPIT                                                   DITT0151
CKOT     CLC   OT,0(11)            PARAM = OUTPUT ?                     DITT0152
         BNE   CKNB                NO, BRANCH.                          DITT0153
         LA    11,10(11)                                                DITT0154
         LA    12,3(0)                                                  DITT0155
         BAL   8,TSTNUM            YES, TEST FOR NUMERIC.               DITT0156
         B     BADCTL                                                   DITT0157
         CLC   SYSMAX,0(11)        TEST UPPER LIMIT.                    DITT0158
         BNH   BADCTL                                                   DITT0159
         MVC   OTL,0(11)           SAVE IT.                             DITT0160
         MVC   HOLD+5(3),0(11)                                          DITT0161
         BAL   8,BINCVT            CONVERT TO BINARY.                   DITT0162
         STC   1,OTPUT             AND STORE.                           DITT0163
         LA    11,3(11)                                                 DITT0164
         OI    CT,X'40'            SET OUTPUT SW ON.                    DITT0165
         B     BUMPIT                                                   DITT0166
CKNB     CLC   NB,0(11)            PARAM = NBLKS ?                      DITT0167
         BNE   CKRS                NO, BRANCH.                          DITT0168
         LA    11,6(11)                                                 DITT0169
         MVC   HNB,0(11)           SAVE IT.                             DITT0170
         LA    12,4(0)                                                  DITT0171
         BAL   8,TSTNUM            TEST FOR NUMERIC.                    DITT0172
         B     BADCTL                                                   DITT0173
         MVC   HOLD+4(4),0(11)                                          DITT0174
         BAL   8,BINCVT            CONVERT TO BINARY                    DITT0175
         STH   1,BLCT              AND STORE.                           DITT0176
         LA    11,4(11)                                                 DITT0177
         OI    CT,X'20'            SET NBLKS SW ON.                     DITT0178
         B     BUMPIT                                                   DITT0179
CKRS     CLC   RS,0(11)            PARAM = RECSIZE ?                    DITT0180
         BNE   CKDT                NO, BRANCH.                          DITT0181
         LA    11,8(11)                                                 DITT0182
         MVC   HRS,0(11)           YES, SAVE IT.                        DITT0183
         LA    11,5(11)                                                 DITT0184
         OI    CT,X'10'            SET RECSIZE SW ON.                   DITT0185
         B     BUMPIT                                                   DITT0186
CKDT     CLC   DT,0(11)            PARAM = DECKTYPE ?                   DITT0187
         BNE   CKBF                NO, BRANCH.                          DITT0188
         LA    11,9(11)                                                 DITT0189
         MVC   DECKTYPE,0(11)      YES, SAVE IT.                        DITT0190
         LA    11,3(11)                                                 DITT0191
         OI    CT,X'08'            SET DECKTYPE SW ON.                  DITT0192
         B     BUMPIT                                                   DITT0193
CKBF     CLC   BF,0(11)            PARAM = BLKFACTOR ?                  DITT0194
         BNE   CKDN                NO, BRANCH.                          DITT0195
         LA    11,10(11)                                                DITT0196
         MVC   HBF(3),0(11)        YES, SAVE IT.                        DITT0197
         LA    11,3(11)                                                 DITT0198
         OI    CT,X'04'            SET BLKFACTOR SW ON.                 DITT0199
         B     BUMPIT                                                   DITT0200
CKDN     CLC   DN,0(11)            PARAM = DECKNAME ?                   DITT0201
         BNE   BUMPIT              NO, BRANCH.                          DITT0202
         LA    11,9(11)                                                 DITT0203
         LR    13,11               YES, SET                             DITT0204
         SR    12,12               UP                                   DITT0205
         CLC   COMMA,0(11)         TO SCAN FOR COMMA                    DITT0206
         BE    BUMPIT              OR                                   DITT0207
         CLC   BLANKS(1),0(11)     BLANK AND SAVE.                      DITT0208
         BE    BUMPIT                                                   DITT0209
DNLOOP   LA    11,1(11)                                                 DITT0210
         CLC   BLANKS(1),0(11)                                          DITT0211
         BE    FILLIT                                                   DITT0212
         CLC   COMMA,0(11)                                              DITT0213
         BE    FILLIT                                                   DITT0214
         LA    12,1(12)                                                 DITT0215
         B     DNLOOP                                                   DITT0216
FILLIT   LA    1,7(0)                                                   DITT0217
         CR    12,1                                                     DITT0218
         BH    BADCTL                                                   DITT0219
         STC   12,*+5                                                   DITT0220
         MVC   DECKNAME,0(13)                                           DITT0221
BUMPIT   CLC   0(1,11),BLANKS      CHECK FOR BLANK                      DITT0222
         BE    CHKID                                                    DITT0223
         CLC   COMMA,0(11)         OR COMMA .                           DITT0224
         BNE   BADCTL                                                   DITT0225
         LA    11,1(11)                                                 DITT0226
         B     CKBG                                                     DITT0227
CHKID    LA    4,CONIO+9                                                DITT0228
         BAL   8,LOOKUP                                                 DITT0229
BADCTL   MVC   CONIO(80),BLANKS                                         DITT0230
         MVC   CONIO(31),=C'MISSING OR INVALID CONTROL CARD'            DITT0231
         BAL   8,PRNT                                                   DITT0232
         SVC   6                                                        DITT0233
         SPACE 2                                                        DITT0234
*********************************************************************** DITT0235
**********         VALIDATE AND TRANSLATE TAPE MODE SET     *********** DITT0236
*********************************************************************** DITT0237
         SPACE 1                                                        DITT0238
CHKMOD   LA    3,MSICCW            SET IN-TAPE CCB TO                   DITT0239
         ST    3,INTPCCB+8         CHAIN TO MODE SET CCW.               DITT0240
         LA    3,MSOCCW            SET OUT-TAPE CCB TO                  DITT0241
         ST    3,OTTPCCB+8         CHAIN TO MODE SET CCW.               DITT0242
         LA    3,MODTBL            SET UP FOR                           DITT0243
         LA    9,MODTBL+35         TABLE SEARCH.                        DITT0244
         CLC   CONIO+3(2),ZEROS    WAS MODE SET ZEROS ?                 DITT0245
         BNE   *+10                NO, BRANCH.                          DITT0246
         MVC   CONIO+3(2),MODTBL   YES, SET MODE = C0.                  DITT0247
         CLC   CONIO+3(2),BLANKS   MODE SET BLANKS ?                    DITT0248
         BNE   *+10                NO, BRANCH.                          DITT0249
         MVC   CONIO+3(2),MODTBL   YES, SET MODE = C0                   DITT0250
MODLOOP  CLC   CONIO+3(2),0(3)     OPERATOR MODE SET MATCH TABLE ?      DITT0251
         BE    GOODMOD             YES, GO SET MODE.                    DITT0252
         CLI   0(3),X'FF'          NO, ARE WE AT END OF TABLE ?         DITT0253
         BE    BADMOD              YES, BAD MODE GIVEN.                 DITT0254
         LA    3,2(3)              NO, BUMP TABLE                       DITT0255
         LA    9,1(9)              LOOKUP.                              DITT0256
         B     MODLOOP                                                  DITT0257
GOODMOD  MVI   CONIO+5,X'09'       SET 9 TRK SW.                        DITT0258
         TM    0(9),X'C0'          IS MODE FOR 9 TRK ?                  DITT0259
         BO    4(8)                YES, BRANCH.                         DITT0260
         MVI   CONIO+5,X'07'       NO, SET 7 TRK SW.                    DITT0261
         B     4(8)                RETURN.                              DITT0262
BADMOD   MVC   CONIO(16),=C'INVALID MODE SET'                           DITT0263
         MVI   WCNCCW+7,X'10'                                           DITT0264
         B     WCON2                                                    DITT0265
MODTBL   DC    CL34'C01020283038506068707890A0A8B0B8C8'                 DITT0266
         DC    X'FF'                                                    DITT0267
         DC    X'C313232B333B53636B737B93A3ABB3BBCB'                    DITT0268
         SPACE 2                                                        DITT0269
*********************************************************************** DITT0270
**********               VERTICAL HEX ROUTINE               *********** DITT0271
*********************************************************************** DITT0272
         SPACE 1                                                        DITT0273
HEXDP    MVC   HEXIO+20(100),0(10)     MOVE DATA TO HEX AREA.           DITT0274
         SR    8,8                 BUMP PRINT LENGTH                    DITT0275
         IC    8,PRLTH             BY                                   DITT0276
         LA    8,32(8)             32 POSITIONS                         DITT0277
         STC   8,PL                FOR HEX HEADINGS.                    DITT0278
         MVC   CONIO(7),BLANKS                                          DITT0279
         MVI   INPUT,X'FF'         SET IGNORE OVERFLOW SW ON.           DITT0280
         MVC   CONIO(26),HDG1      SET UP FIRST                         DITT0281
         MVC   HEXIO+14(6),DET1    HEADING.                             DITT0282
         MVI   CC,X'09'            SINGLE SPACE.                        DITT0283
         BAL   8,EDIT              EDIT AND PRINT.                      DITT0284
         MVC   HEXIO+20(1),0(10)       MOVE                             DITT0285
         PACK  HEXIO+20(0),HEXIO+20(0) ZONES AND                        DITT0286
         MVO   HEXIO+21(16),0(16,10)   WITH OFFSET TO                   DITT0287
         MVO   HEXIO+36(16),15(16,10)  NUMERIC POSITIONS.               DITT0288
         MVO   HEXIO+51(16),30(16,10)                                   DITT0289
         MVO   HEXIO+66(16),45(16,10)                                   DITT0290
         MVO   HEXIO+81(16),60(16,10)                                   DITT0291
         MVO   HEXIO+96(16),75(16,10)                                   DITT0292
         MVO   HEXIO+111(10),90(10,10)                                  DITT0293
         NC    HEXIO+20(100),SET   AND OFF ZONE S.                      DITT0294
         TR    HEXIO+20(100),HEXTB TRANSLATE TO HEX CHAR.               DITT0295
         MVC   CONIO(26),HDG2      SET UP ZONE                          DITT0296
         MVC   HEXIO+14(6),DET2    HEADINGS.                            DITT0297
         BAL   8,PRNT              PRINT ZONES.                         DITT0298
         MVC   HEXIO+20(100),0(10) REFILL HEX AREA.                     DITT0299
         NC    HEXIO+20(100),SET   AND OFF ZONES.                       DITT0300
         TR    HEXIO+20(100),HEXTB TRANSLATE TO HEX CHAR.               DITT0301
         MVC   CONIO(26),HDG3      SET UP NUMERIC                       DITT0302
         MVC   HEXIO+14(6),DET3    HEADINGS.                            DITT0303
         BAL   8,PRNT              PRINT NUMERIC LINE.                  DITT0304
         MVC   CONIO(33),BLANKS                                         DITT0305
         MVC   HEXIO(120),SCALE    MOVE SCALE TO PRINT AREA.            DITT0306
         MVI   CC,X'11'            SET TO DOUBLE SPACE.                 DITT0307
         MVI   INPUT,X'00'         TURN OFF IGNORE OVERFLOW SW.         DITT0308
         BAL   8,PRNT              PRINT SCALE.                         DITT0309
         BR    9                   RETURN.                              DITT0310
PRLTH    DC    CL1' '                                                   DITT0311
HEXTB    DC    CL16'0123456789ABCDEF'                                   DITT0312
SET      DC    X'0F'                                                    DITT0313
         DS    CL99                                                     DITT0314
SCALE    DC    CL20' '                                                  DITT0315
         DC    CL50'1...5...10...15...20...25...30...35...40...45...50' DITT0316
         DC    CL50'...55...60...65...70...75...80...85...90...95.....' DITT0317
TRTBL    DC    CL50' '                                                  DITT0318
         DC    CL30'                        Ö.<(+×'                     DITT0319
         DC    X'50'                                                    DITT0320
         DC    CL20'         !$*);^-/   '                               DITT0321
         DC    CL24'      ,%_>?          :#@'                           DITT0322
         DC    X'7D'                                                    DITT0323
         DC    CL25'="                       '                          DITT0324
         DC    CL50'                                          ABCDEFGH' DITT0325
         DC    CL50'I       JKLMNOPQR        STUVWXYZ      0123456789 ' DITT0326
         DC    CL5' '                                                   DITT0327
         SPACE 2                                                        DITT0328
*********************************************************************** DITT0329
**********         NUMERIC FUNCTIONS FOR CONTROL DATA       *********** DITT0330
*********************************************************************** DITT0331
         SPACE 1                                                        DITT0332
TSTNUMV  LR    13,12               RIGHT JUSTIFY NUMERIC                DITT0333
         BCT   13,*+4                  CONSOLE REPLY.                   DITT0334
         MVC   CONIO+50(5),ZEROS                                        DITT0335
         MVC   CONIO+55(10),CONIO                                       DITT0336
         LA    1,CONIO+54                                               DITT0337
         AR    1,12                                                     DITT0338
RTJUST   CLI   0(1),C' '                                                DITT0339
         BNE   MOVBAC                                                   DITT0340
         BCT   1,*+4                                                    DITT0341
         BCT   13,RTJUST                                                DITT0342
MOVBAC   SR    1,12                                                     DITT0343
         MVC   CONIO(10),1(1)                                           DITT0344
TSTNUM   LR    13,11               TEST FOR NUMERIC.                    DITT0345
         TM    0(13),X'F0'                                              DITT0346
         BNO   NUMBAD                                                   DITT0347
         LA    13,1(13)                                                 DITT0348
         BCT   12,TSTNUM+2                                              DITT0349
         B     4(8)                                                     DITT0350
NUMBAD   CLC   CNTRLSW,ON                                               DITT0351
         BE    BADCTL                                                   DITT0352
         B     0(8)                                                     DITT0353
BINCVT   PACK  HOLD,HOLD           CONVERT HOLD TO BINARY               DITT0354
         CVB   1,HOLD              RESULTS IN REG 1.                    DITT0355
         MVC   HOLD(1),OFF                                              DITT0356
         MVC   HOLD+1(7),HOLD                                           DITT0357
         BR    8                                                        DITT0358
DECCVT   CVD   7,HOLD              CONVERT BINARY NUMBER                DITT0359
         UNPK  HOLDSEQ,HOLD        IN REG 7                             DITT0360
         MVZ   HOLDSEQ+7(1),ON     TO DECIMAL.                          DITT0361
         MVI   HOLD,X'00'                                               DITT0362
         MVC   HOLD+1(7),HOLD                                           DITT0363
         CLI   ESW,X'00'           DO WE SUPPRESS LEADING ZEROS ?       DITT0364
         MVI   ESW,X'00'                                                DITT0365
         BNE   0(8)                NO, RETURN.                          DITT0366
         LA    13,HOLDSEQ          YES,                                 DITT0367
         LA    1,7(0)              REPLACE WITH                         DITT0368
DECLOP   CLI   0(13),C'0'          BLANKS.                              DITT0369
         BNE   0(8)                                                     DITT0370
         MVI   0(13),C' '                                               DITT0371
         LA    13,1(13)                                                 DITT0372
         BCT   1,DECLOP                                                 DITT0373
         BR    8                                                        DITT0374
         SPACE 2                                                        DITT0375
*********************************************************************** DITT0376
**********         DEBLOCK  RECORDS FOR PRINTING OR DUMP    *********** DITT0377
*********************************************************************** DITT0378
         SPACE 1                                                        DITT0379
DEBLOK   LA    4,100(0)                                                 DITT0380
         SR    7,7                                                      DITT0381
DB1      MVI   CC,X'09'                                                 DITT0382
         CR    3,4                 IS RECORD LNTH GREATER THAN 100 ?    DITT0383
         BNH   LSBLK               NO, SET UP FOR LAST BLOCK.           DITT0384
         SR    3,4                                                      DITT0385
DB2      CLI   DPSW,X'FF'          DUMP IN HEX ?                        DITT0386
         BNE   LST                 NO, GO TO LIST                       DITT0387
         STC   4,PRLTH             YES, SET                             DITT0388
         BAL   8,DECCVT            UP TO                                DITT0389
         MVC   SCALE+12(8),HOLDSEQ                                      DITT0390
         BAL   9,HEXDP             HEX PRINT 100 POS.                   DITT0391
         MVC   HDG1(78),BLANKS                                          DITT0392
         AR    10,4                BUMP I/O POINTER BY 100.             DITT0393
         LA    7,10(7)                                                  DITT0394
         B     DB1                                                      DITT0395
LST      LR    1,4                                                      DITT0396
         LA    1,32(1)             BUMP PRINT LENGTH                    DITT0397
         STC   1,PL                BY 32 POS ( FOR HDGS)                DITT0398
         MVC   CONIO(32),HDG1      MOVE HDG1 TO PRINT AREA              DITT0399
         MVC   CONIO+32(100),0(10)                                      DITT0400
         AR    10,4                BUMP I/O AREA POINTER BY 100.        DITT0401
         BAL   8,EDIT              EDIT AND PRINT.                      DITT0402
         MVC   HDG1(78),BLANKS                                          DITT0403
         B     DB1                                                      DITT0404
LSBLK    LTR   3,3                 ARE WE DONE ?                        DITT0405
         BZ    0(11)               YES, RETURN.                         DITT0406
         LR    4,3                 NO, SET PRINT LNGTH TO REMAINING     DITT0407
         SR    3,3                 RECORD LENGTH.                       DITT0408
         B     DB2                                                      DITT0409
DPSW     DC    X'00'                                                    DITT0410
         SPACE 2                                                        DITT0411
*********************************************************************** DITT0412
**********         EDIT AND CONVERT DISK ADDRESS TO BINARY   ********** DITT0413
*********************************************************************** DITT0414
         SPACE 1                                                        DITT0415
EDTADR   LA    12,5(0)                                                  DITT0416
         BAL   8,TSTNUM            IS ADDRESS NUMERIC ?                 DITT0417
         B     EDTBAD              NO, BRANCH.                          DITT0418
         MVI   UPPERH,C'0'                                              DITT0419
         MVI   ENDHD,X'09'                                              DITT0420
         CLI   SW2314,X'FF'        IS DEVICE A 2314 ?                   DITT0421
         BNE   *+12                NO, BRANCH.                          DITT0422
         MVI   ENDHD,X'13'                                              DITT0423
         MVI   UPPERH,C'1'         YES, SET UPPER HEAD LIMIT TO 19.     DITT0424
         CLC   0(3,11),UPPERC      IS ADDRESS GREATER THAN UPPER LIMIT. DITT0425
         BH    EDTBAD              YES, BAD ADDRESS.                    DITT0426
         PACK  HOLD,0(3,11)        NO, CONVERT CYLINDER NUMBERS         DITT0427
         CVB   13,HOLD             TO BINARY.                           DITT0428
         MVC   HOLD(1),OFF                                              DITT0429
         MVC   HOLD+1(7),HOLD                                           DITT0430
         CLC   3(2,11),UPPERH      IS HEAD NUMBER VALID ?               DITT0431
         BH    EDTBAD              NO, BAD ADDRESS.                     DITT0432
         PACK  HOLD,3(2,11)        YES, CONVERT                         DITT0433
         CVB   1,HOLD              HEAD NUMBER TO BINARY.               DITT0434
         MVC   HOLD(1),OFF                                              DITT0435
         MVC   HOLD+1(7),HOLD                                           DITT0436
         B     4(9)                RETURN, CYL IN R13, HEAD IN R1       DITT0437
EDTBAD   CLC   CNTRLSW,ON                                               DITT0438
         BE    BADCTL                                                   DITT0439
         B     0(9)                                                     DITT0440
BEGHD    DS    CL1                                                      DITT0441
ENDHD    DS    CL1                                                      DITT0442
         EJECT                                                          DITT0443
*********************************************************************** DITT0444
*********           SUPERVISOR  PUB-TO-LUB CONVERSION       *********** DITT0445
**********         IF A LUB IS NOT ASSGNED TO THE DESIRED   *********** DITT0446
**********         PUB, AND IF A LUB AND A PUB ARE          *********** DITT0447
**********         AVAILABLE TO THIS PARTITION, THE         *********** DITT0448
**********         $$BDITAN TRANSIENT WILL BE FETCHED AND   *********** DITT0449
**********         A TEMPORARY ASSIGNMENT MADE.             *********** DITT0450
*********************************************************************** DITT0451
         SPACE 1                                                        DITT0452
LUBPUB   LH    1,22                GET COMM. REGION ADDRESS             DITT0453
         LH    2,62(1)             FOCL ADDRESS IN R2.                  DITT0454
         LH    3,64(1)             PUB ADDRESS IN R3.                   DITT0455
         LH    13,74(1)             NICL ADDRESS IN R13                 DITT0456
         LH    10,76(1)             LUB ADDRESS IN R10                  DITT0457
         MVN   CHAN(1),CONIO                                            DITT0458
         LA    11,CONIO+1                                               DITT0459
         LA    12,2(0)                                                  DITT0460
CHKUNT   TM    0(11),X'F0'         UNIT ADDRESS NUMERIC ?               DITT0461
         BO    GOODUNT             YES, BRANCH                          DITT0462
         TM    0(11),X'C0'         IS IT ALPHA ?                        DITT0463
         BZ    BADCHAN             NO, BAD ADDRESS.                     DITT0464
         NI    0(11),X'0F'         AND THE ZONES OFF.                   DITT0465
         CLI   0(11),X'00'         IS IT LESS THAN 'A' ?                DITT0466
         BE    BADCHAN             YES, BAD HEX CHAR.                   DITT0467
         CLI   0(11),X'06'         IS IT GREATER THAN 'F' ?             DITT0468
         BH    BADCHAN             YES, BAD ADDRESS.                    DITT0469
         TR    0(1,11),ADTBL-1     TRANSLATE TO FORM 0X.                DITT0470
         LA    11,1(11)                                                 DITT0471
         BCT   12,CHKUNT                                                DITT0472
GOODUNT  MVC   CHAN+1(1),CONIO+1   CONVERT 2 POS. UNIT                  DITT0473
         PACK  CHAN+1(0),CHAN+1(0)     ADDRESS TO 1 POS. HEX.           DITT0474
         MVN   CHAN+1(1),CONIO+2                                        DITT0475
         CLI   CHAN,X'06'          IS CHANNEL =  0-6 ?                  DITT0476
         BH    BADCHAN             NO, BAD CHANNEL NUMBER.              DITT0477
         SR    1,1                                                      DITT0478
         IC    1,CHAN                                                   DITT0479
         AR    2,1                 ADD CHANNEL TO FOCL ADDRESS.         DITT0480
         CLI   0(2),X'FF'          IS CHANNEL SUPPORTED ?               DITT0481
         BE    BADCHAN             NO,  BAD ADDRESS.                    DITT0482
         SR    1,1                 YES,                                 DITT0483
         IC    1,0(2)              LOAD FOCL VALUE                      DITT0484
         LA    4,8(0)                                                   DITT0485
         AR    3,1                 BUMP TO CORRECT CHANNEL PUB          DITT0486
         BCT   4,*-2                                                    DITT0487
         LH    2,22                                                     DITT0488
         LH    4,72(2)             LOAD FICL ADDRESS IN R4.             DITT0489
PUBLOP   CLC   CHAN,0(3)           DO PUBS MATCH ?                      DITT0490
         BE    CHKDSK              YES, BRANCH.                         DITT0491
         CLI   0(3),X'FF'           IS UNIT SUPPORTED ?                 DITT0492
         BE    BADCHAN             NO, BAD ADDRESS.                     DITT0493
         LA    1,1(1)              YES,  BUMP LUB POINTER.              DITT0494
         LA    3,8(3)              BUMP PUB POINTER AND                 DITT0495
         B     PUBLOP              LOOP FOR NEXT LOOK.                  DITT0496
CHKDSK   MVI   SW2314,X'00'        TURN OFF 2314 SW.                    DITT0497
         MVI   DEVTYP,C'0'         RESET DEVICE TYPE CODE.              DITT0498
         CLI   4(3),X'60'          IS DEVICE 2311 ?                     DITT0499
         BNE   *+12                NO, BRANCH.                          DITT0500
         MVI   DEVTYP,C'D'         YES, SET DEVICE TYPE CODE FOR DISK.  DITT0501
         B     LUBLOK                                                   DITT0502
         MVI   SW2314,X'FF'        NO, SET SW FOR 2314.                 DITT0503
         CLI   4(3),X'62'          IS DEVICE 2314 ?                     DITT0504
         BNE   *+12                NO, BRANCH.                          DITT0505
         MVI   DEVTYP,C'D'         YES, SET DEVICE TYPE CODE FOR DISK.  DITT0506
         B     LUBLOK                                                   DITT0507
         CLI   4(3),X'50'          IS DEVICE TAPE ?                     DITT0508
         BNE   LUBLOK              NO, BRANCH.                          DITT0509
         TM    6(3),X'01'          IS IT 7 TRK TAPE ?                   DITT0510
         BZ    CHKTP1              NO, BRANCH.                          DITT0511
         CLI   CONIO+5,X'07'       YES, WAS 7 TRK MODE SET GIVEN ?      DITT0512
         BNE   BADTPTK9            NO, ISSUE 'UNIT IS NOT 9 TRK' MSG.   DITT0513
         B     LUBLOK              YES, BRANCH.                         DITT0514
CHKTP1   CLI   CONIO+5,X'09'       WAS 9 TRK MODE SET GIVEN ?           DITT0515
         BNE   BADTPTK7            NO, ISSUE 'UNIT IS NOT 7 TRK' MSG    DITT0516
LUBLOK   STH   3,PUBADRS                                                DITT0517
         STH   3,NEWLUB                                                 DITT0518
         AH    4,FICL        BUMP FICL TO CORRECT PARTITION UNITS.      DITT0519
         STC   1,PUBX              STORE LUB POINTER.                   DITT0520
         SR    1,1                                                      DITT0521
         IC    1,0(4)              LOAD FICL INDEX VALUE.               DITT0522
         AR    10,1                CALCULATE PROPER                     DITT0523
         AR    10,1                LUB ENTRY POINT.                     DITT0524
         SR    2,2                                                      DITT0525
         SR    1,1                                                      DITT0526
         AH    13,FICL                                                  DITT0527
         IC    1,0(13)             LOAD NICL FOR PROGRAMMER UNITS.      DITT0528
LUBLOP   CLC   PUBX,0(10)          DO PUB POINTERS MATCH ?              DITT0529
         BE    FNDLUB              YES, BRANCH.                         DITT0530
         CLC   0(2,10),=X'FFFF'    IS LUB UNASSGNED ?                   DITT0531
         BNE   LUBLOP1             NO, BRANCH.                          DITT0532
         STH   10,NEWLUB           YES, SAVE LUB ADDRESS                DITT0533
         STC   2,INPUT             AND LUB                              DITT0534
         STC   2,OTPUT             VALUE.                               DITT0535
         ST    10,LUBADRS                                               DITT0536
LUBLOP1  LA    10,2(10)            NO, BUMP TO NEXT LUB.                DITT0537
         LA    2,1(2)              BUMP SYSTEM PROG. UNIT NUMBER.       DITT0538
         BCT   1,LUBLOP            BRANCH TO LOOP FOR NEXT LOOK.        DITT0539
         CLC   NEWLUB,PUBADRS      IS THERE A FREE LUB ?                DITT0540
         BNE   CHEKTYP             YES, BRANCH.                         DITT0541
         MVC   CONIO(20),=C'NO AVAIL PROGRAM LUB'                       DITT0542
         B     NOTAVAIL+6                                               DITT0543
CHEKTYP  LH    1,PUBADRS                                                DITT0544
         CLI   DEVTYP,C'D'                                              DITT0545
         BE    CHKDOWN                                                  DITT0546
DEVOWNER EQU   *+1                                                      DITT0547
         TM    7(1),X'03'          IS DEVICE OWNED BY ANOTHER PARTITION DITT0548
         BNZ   NOTAVAIL            YES, BRANCH.                         DITT0549
CHKDOWN  TM    7(1),X'F8'          IS DEVICE DOWN ?                     DITT0550
         BNZ   FETCH               NO, BRANCH                           DITT0551
NOTAVAIL MVC   CONIO(20),=C'DEVICE NOT AVAILABLE'                       DITT0552
         MVI   WCNCCW+7,X'14'                                           DITT0553
         LR    8,9                                                      DITT0554
         B     WCON2                                                    DITT0555
FETCH    LA    0,NEWLUB            ADDRESS OF PASS FIELD                DITT0556
         LA    1,ASSGN             B-TRANSIENT NAME                     DITT0557
         MVC   LUBPTR,PUBX                                              DITT0558
         SVC   2                   FETCH THE TRANSIENT                  DITT0559
         CLI   LUBPTR,X'FF'        ANY JIBS ?                           DITT0560
         BE    NOJIBS              NO, BRANCH.                          DITT0561
         B     4(9)                YES, RETURN                          DITT0562
NEWLUB   DS    1H                                                       DITT0563
PUBADRS  DS    1H                                                       DITT0564
LUBPTR   DS    CL1                                                      DITT0565
PUBFLAG  DC    X'04'                                                    DITT0566
         DS    0F                                                       DITT0567
ASSGN    DC    CL8'$$BDITAN'                                            DITT0568
BADTPTK7 MVC   CONIO(17),=C'UNIT IS NOT 9 TRK'                          DITT0569
         MVI   CONIO+12,C'7'                                            DITT0570
         B     BADTPTK9+6                                               DITT0571
BADTPTK9 MVC   CONIO(17),=C'UNIT IS NOT 9 TRK'                          DITT0572
         MVI   WCNCCW+7,X'11'                                           DITT0573
         B     BADCHAN+10                                               DITT0574
BADCHAN  MVC   CONIO(15),=C'INVALID ADDRESS'                            DITT0575
         MVI   WCNCCW+7,X'0F'                                           DITT0576
         LR    8,9                                                      DITT0577
         B     WCON2                                                    DITT0578
FNDLUB   STC   2,INPUT             INPUT OR OUTPUT LOGICAL PROGRAMMER   DITT0579
         ST    10,LUBADRS                                               DITT0580
         STC   2,OTPUT             UNIT NUMBER STORED FOR RETURN.       DITT0581
         B     4(9)                                                     DITT0582
         SPACE 2                                                        DITT0583
*********************************************************************** DITT0584
*********           SUPERVISOR  LUB-TO-PUB CONVERSION       *********** DITT0585
*********************************************************************** DITT0586
         SPACE 1                                                        DITT0587
GETPUB   LH    1,22                GET COMM. REGION ADDRESS IN R2.      DITT0588
         LH    10,76(1)            LUB ADDRESS IN R10.                  DITT0589
         LH    4,72(1)             FICL ADDRESS IN R4.                  DITT0590
         SR    7,7                                                      DITT0591
         AH    4,FICL              BUMP FICL POINTER TO CORRECT         DITT0592
         IC    7,0(4)              PARTITION.                           DITT0593
         SR    8,8                                                      DITT0594
         IC    8,INPUT             GET SYS NUMBER.                      DITT0595
         AR    7,8                 ADD SYS NUMBER TO FICL VALUE.        DITT0596
         AR    10,7                ADD RESULT FICL TO LUB               DITT0597
         AR    10,7                ADDRESS. ( 2 BYTES PER LUB).         DITT0598
         SR    7,7                                                      DITT0599
         ST    10,LUBADRS                                               DITT0600
         IC    7,0(10)             GET LUB POINTER.                     DITT0601
         LH    4,64(1)             GET PUB POINTER.                     DITT0602
         LA    1,8(0)              ADD LUB POINTER X 8                  DITT0603
         AR    4,7                 TO PUB ADDRESS.                      DITT0604
         BCT   1,*-2                                                    DITT0605
         MVC   PUBX,1(4)           GET DESIRED                          DITT0606
         PACK  PUBX,PUBX           PUB (0CUU)                           DITT0607
         MVC   PHYUNT(1),0(4)      AND                                  DITT0608
         MVC   PHYUNT+1(1),PUBX    CONVERT                              DITT0609
         MVC   PHYUNT+2(1),1(4)    TO                                   DITT0610
         MVZ   PHYUNT(3),ADTBL     DECIMAL VALUE.                       DITT0611
         TR    PHYUNT(3),HEXTB                                          DITT0612
         MVI   SW2314,X'00'                                             DITT0613
         MVI   DEVTYP,C'0'         RESET DEVICE TYPE.                   DITT0614
         CLI   4(4),X'60'          IS DEVICE TYPE = 2311 ?              DITT0615
         BNE   *+12                NO, BRANCH.                          DITT0616
         MVI   DEVTYP,C'D'         SET DEVICE TYPE = DISK.              DITT0617
         B     GOODSS+6                                                 DITT0618
         MVI   SW2314,X'FF'        NO, SET SW FOR 2314.                 DITT0619
         CLI   4(4),X'62'          IS DEVICE TYPE = 2314 ?              DITT0620
         BNE   *+12                NO, BRANCH.                          DITT0621
         MVI   DEVTYP,C'D'         SET DEVICE TYPE = DISK.              DITT0622
         B     GOODSS+6                                                 DITT0623
         CLI   4(4),X'50'          IS DEVICE TYPE = TAPE ?              DITT0624
         BNE   GOODSS+6            NO, BRANCH.                          DITT0625
         LA    1,35(0)                                                  DITT0626
         LA    7,MODTBL+35         YES,                                 DITT0627
         LA    8,MODTBL                                                 DITT0628
PRNTSS   CLC   5(1,4),0(7)         LOOP THRU MODTBL                     DITT0629
         BE    GOODSS              UNTIL MATCH IS FOUND                 DITT0630
         LA    7,1(7)              EQUAL TO EXIST.                      DITT0631
         LA    8,2(8)              MODE SET GIVEN.                      DITT0632
         BCT   1,PRNTSS                                                 DITT0633
         LA    8,BLANKS                                                 DITT0634
GOODSS   MVC   HH2+27(2),0(8)      MOVE DECIMAL MODE SET TO HEADING.    DITT0635
         MVC   HH2+8(3),PHYUNT     MOVE PUB TO HEADING.                 DITT0636
         B     0(9)                                                     DITT0637
CHAN     DC    X'0000'                                                  DITT0638
ADTBL    DC    X'0A0B0C0D0E0F'                                          DITT0639
PUBX     DC    CL1' '                                                   DITT0640
PHYUNT   DC    CL3' '                                                   DITT0641
         EJECT                                                          DITT0642
*********************************************************************** DITT0643
**********         CONSOLE MESSAGE WRITER                   *********** DITT0644
*********************************************************************** DITT0645
         SPACE 1                                                        DITT0646
ITMSG    MVC   CONIO(18),SYSITMSG  GET IN-TAPE ADDRESS.                 DITT0647
         MVI   WCNCCW+7,X'12'      AND MODE SET FROM SYSLOG.            DITT0648
         MVI   RCNCCW+7,X'05'                                           DITT0649
         BAL   8,WRTCON                                                 DITT0650
         BAL   8,CHKMOD            VALIDATE MODE SET GIVEN.             DITT0651
         B     ITMSG                                                    DITT0652
         MVC   MSICCW(1),0(9)                                           DITT0653
         BAL   9,LUBPUB            VALIDATE PUB ADDRESS.                DITT0654
         B     ITMSG                                                    DITT0655
         B     0(7)                                                     DITT0656
OTMSG    MVC   CONIO(19),SYSOTMSG  GET OUT-TAPE ADDRESS.                DITT0657
         MVI   WCNCCW+7,X'13'      AND MODE SET FROM SYSLOG.            DITT0658
         MVI   RCNCCW+7,X'05'                                           DITT0659
         BAL   8,WRTCON                                                 DITT0660
         BAL   8,CHKMOD            VALIDATE MODE SET.                   DITT0661
         B     OTMSG                                                    DITT0662
         MVC   MSOCCW(1),0(9)                                           DITT0663
         BAL   9,LUBPUB            VALIDATE PUB ADDRESS.                DITT0664
         B     OTMSG                                                    DITT0665
         B     0(7)                                                     DITT0666
IDMSG    MVC   CONIO(10),SYSDSK    GET DISK ADDRESS AND VALIDATE PUB.   DITT0667
         MVI   WCNCCW+7,X'0A'                                           DITT0668
         MVI   RCNCCW+7,X'03'                                           DITT0669
         BAL   8,WRTCON                                                 DITT0670
         BAL   9,LUBPUB                                                 DITT0671
         B     IDMSG                                                    DITT0672
         MVC   DSKCCB+7(1),INPUT                                        DITT0673
         B     0(7)                                                     DITT0674
         SPACE 2                                                        DITT0675
*********************************************************************** DITT0676
**********               DISK  DEBLOCK  ROUTINE             *********** DITT0677
*********************************************************************** DITT0678
         SPACE 1                                                        DITT0679
DSKDEBL  MVI   RWCCW,X'1E'         SET TO READ COUNT, KEY, DATA.        DITT0680
         MVC   RWCCW+6(2),DSL+2    SET MAX. RECORD LNTH.                DITT0681
         LA    8,1(0)              DECREMENT RECORD #                   DITT0682
         IC    7,LOWLIM+6              TO SEARCH FOR                    DITT0683
         SR    7,8                     R-1 RECORD.                      DITT0684
         STC   7,LOWLIM+6                                               DITT0685
         MVI   SHCCW,X'31'                                              DITT0686
         BAL   8,DISK              GET RECORD.                          DITT0687
         LA    7,1(7)                                                   DITT0688
         STC   7,LOWLIM+6                                               DITT0689
         TM    3(1),X'08'          NO RECORDS ON TRACK ?                DITT0690
         BO    0(4)                YES, RETURN.                         DITT0691
         CLC   DTPIO(5),LOWLIM+2   WAS RECORD R0 ?                      DITT0692
         BL    0(4)                YES, TREAT AS NO REC.                DITT0693
         CLI   EOF,X'10'           IS FUNCTION WRITE EOF ?              DITT0694
         BE    DRCDEBL             YES, BRANCH.                         DITT0695
         CLC   DSKCCB(2),DSL       IS RESIDUAL CT ZERO ??               DITT0696
         BE    WRGLNTC             YES, I/O AREA TOO SMALL.             DITT0697
DRCDEBL  MVI   DPSW,X'FF'          SET HEX DUMP SW ON.                  DITT0698
         LA    10,DTPIO+8                                               DITT0699
         STM   3,4,SAV                                                  DITT0700
         SR    7,7                                                      DITT0701
         IC    7,DTPIO+4           GET RECORD # FROM COUNT FIELD.       DITT0702
         BAL   8,DECCVT            CONVERT TO DECIMAL                   DITT0703
         MVC   HDG1(8),DCYL                                             DITT0704
         MVC   HDG2(8),DHED        MOVE REC # IN HEADING.               DITT0705
         MVC   HDG3(8),DREC                                             DITT0706
         MVC   HDG3+5(3),HOLDSEQ+5                                      DITT0707
         IC    7,DTPIO+5           GET KEY LNTH FROM COUNT.             DITT0708
         LR    2,7                 KEY LNTH RETURNED IN R2.             DITT0709
         LTR   7,7                 IS KEY LNTH ZERO ?                   DITT0710
         BZ    NOKEY               YES, BRANCH.                         DITT0711
         BAL   8,DECCVT            NO, PUT KEY                          DITT0712
         MVC   KEYH+4(5),HOLDSEQ+3 LNTH IN HEADING.                     DITT0713
         MVC   HDG1+13(9),KEYH                                          DITT0714
         LR    3,7                                                      DITT0715
         BAL   11,DEBLOK           GO PRINT KEY RECORD.                 DITT0716
         SR    7,7                                                      DITT0717
NOKEY    IC    7,DTPIO+6           GET DATA LNTH FROM COUNT FIELD.      DITT0718
         SLL   7,8(0)                                                   DITT0719
         IC    7,DTPIO+7                                                DITT0720
         LR    5,7                 RETURN DATA LNTH IN R5.              DITT0721
         LTR   7,7                 IS DATA LNTH ZERO ?                  DITT0722
         BZ    ENDFL               YES, GO PRINT EOF RECORD.            DITT0723
         BAL   8,DECCVT            NO, PUT LNTH IN HEADING.             DITT0724
         MVC   DATH+5(4),HOLDSEQ+4                                      DITT0725
         MVC   HDG1+13(9),DATH                                          DITT0726
         CLC   DBLSW,OFF           IS DATA TO BE DEBLOCKED ?            DITT0727
         BE    DEBLK2              NO, BRANCH.                          DITT0728
DEBLK1   C     7,SAVREGS           IS DATA LNTH GREATER THAN LOG. LNTH? DITT0729
         BNH   DEBLK2              NO, BRANCH.                          DITT0730
         L     3,SAVREGS           YES, GO                              DITT0731
         SR    7,3                 DEBLOCK FIRST LOGICAL PORTION.       DITT0732
         ST    7,SAVREGS+4                                              DITT0733
         BAL   11,DEBLOK                                                DITT0734
         L     7,SAVREGS+4         DECREMENT DATA LNTH AND LOOP         DITT0735
         B     DEBLK1              UNTIL FINISHED.                      DITT0736
DEBLK2   LR    3,7                 LOAD LNTH IN R3.                     DITT0737
         BAL   11,DEBLOK           GO PRINT DATA RECORD.                DITT0738
         MVI   CC,X'13'                                                 DITT0739
         BAL   8,PRNT                                                   DITT0740
         LM    3,4,SAV                                                  DITT0741
         B     4(4)                RETURN.                              DITT0742
ENDFL    MVC   CONIO(26),HDG1      *****    EOF ROUTINE    *****        DITT0743
         MVC   DATH+5(4),BLANKS                                         DITT0744
         MVI   DATH+8,C'0'         DATA LNTH = 0.                       DITT0745
         MVC   CONIO+13(9),DATH                                         DITT0746
         MVC   CONIO+22(20),BLANKS                                      DITT0747
         MVC   CONIO+32(18),=C'END OF FILE RECORD'                      DITT0748
         MVI   CC,X'09'                                                 DITT0749
         MVI   PL,X'32'                                                 DITT0750
         BAL   8,PRNT                                                   DITT0751
         MVC   CONIO+13(50),BLANKS                                      DITT0752
         MVC   CONIO(26),HDG2                                           DITT0753
         BAL   8,PRNT                                                   DITT0754
         MVC   CONIO(26),HDG3                                           DITT0755
         MVI   CC,X'19'                                                 DITT0756
         BAL   8,PRNT                                                   DITT0757
         LM    3,4,SAV                                                  DITT0758
         B     4(4)                                                     DITT0759
         SPACE 2                                                        DITT0760
*********************************************************************** DITT0761
**********         THIS ROUTINE WILL BE ENTERED IF THE      *********** DITT0762
**********         USER OPERATES DISK IN A DASD FILE        *********** DITT0763
**********         PROTECTED ENVIRONMENT.  A TRANSIENT WILL *********** DITT0764
**********         BE FETCHED TO PLACE THE FILE LIMITS IN   *********** DITT0765
**********         A JIB.   SEE $$BDITFP TRANSIENT SUPPLIED *********** DITT0766
*********************************************************************** DITT0767
         SPACE 1                                                        DITT0768
DASDFP   BC    15,0(8)             ALTERED TO NOP IF DASDFP=YES         DITT0769
         CLI   DEVTYP,C'D'         IS DEVICE TYPE = 2311 OR 2314 ?      DITT0770
         BNE   0(8)                NO, RETURN.                          DITT0771
         LA    1,DEVTAB            YES, GET ADDRS OF DEVICE TABLE.      DITT0772
         LA    2,15                                                     DITT0773
DEVLOP   CLC   INPUT,0(1)          HAS TRANSIENT BEEN FETCHED FOR LUB ? DITT0774
         BE    0(8)                YES,  RETURN.                        DITT0775
         CLI   0(1),X'FF'          END OF ACTIVE TABLE ?                DITT0776
         BE    STUFF               YES, BRANCH.                         DITT0777
         LA    1,1(1)              NO, BUMP TO NEXT TABLE               DITT0778
         BCT   2,DEVLOP                ENTRY AND LOOP.                  DITT0779
STUFF    MVC   0(1,1),INPUT        PLACE LUB IN TABLE                   DITT0780
         LA    1,TRANS             LOAD TRANSIENT NAME                  DITT0781
         LA    0,LUBADRS           LOAD ADDRESS OF LUB FOR TRANSIENT.   DITT0782
         SVC   2                   FETCH THE B TRANSIENT.               DITT0783
         CLI   FILEMASK,X'FF'      WERE THERE ENOUGH JIBS ?             DITT0784
         BNE   0(8)                YES, RETURN.                         DITT0785
         MVI   INTRP+1,X'F0'       DISABLE INTERRUPT.                   DITT0786
NOJIBS   MVC   CONIO(23),=C'4890I NO AVAILABLE JIBS'                    DITT0787
         MVI   WCNCCW+7,X'17'                                           DITT0788
         BAL   8,WCON2                                                  DITT0789
         SVC   6                   CANCEL JOB                           DITT0790
LUBADRS  DS    1F                                                       DITT0791
FILEMASK DC    X'00CA24FF'                                              DITT0792
DEVTAB   DC    X'FFFFFFFFFFFFFFFF'                                      DITT0793
         DC    X'FFFFFFFFFFFFFFFF'                                      DITT0794
TRANS    DC    CL8'$$BDITFP'                                            DITT0795
         SPACE 2                                                        DITT0796
*********************************************************************** DITT0797
**********               ALTER RECORD FROM CONSOLE          *********** DITT0798
*********************************************************************** DITT0799
         SPACE 1                                                        DITT0800
CHNG     MVI   WCNCCW+7,X'22'      REG 9 = BEGIN OF RECORD AT ENTER     DITT0801
         MVI   RCNCCW+7,X'02'      REG 10 = END OF RECORD AT ENTER      DITT0802
         MVC   DECKNAME(5),ALTRMSG                                      DITT0803
         MVC   CONIO(34),PRTIO     # TYPE BYTES TO BE CHANGED           DITT0804
         BAL   8,WRTCON            GET REPLY.                           DITT0805
         LA    12,2(0)                                                  DITT0806
         LA    11,CONIO                                                 DITT0807
         BAL   8,TSTNUMV           IS REPLY NUMERIC ?                   DITT0808
         B     CHNG                                                     DITT0809
         MVC   HOLD+6(2),CONIO                                          DITT0810
         BAL   8,BINCVT            CONVERT TO BINARY, RESULTS IN R1.    DITT0811
         MVC   ALTCT+6(2),CONIO    STORE DECIMAL VALUE.                 DITT0812
         LA    8,35(0)                                                  DITT0813
         CR    1,8                 AMOUNT OF CHANGE = 1-35 ?            DITT0814
         BH    CHNG                NO, RESTART.                         DITT0815
         LTR   1,1                 REPLY 0 ?                            DITT0816
         BZ    CHNG                YES, RESTART.                        DITT0817
         LR    3,1                 NO,  SAVE AMOUNT IN R3.              DITT0818
GETLTH   MVC   CONIO(29),SDPMSG    GET STARTING                         DITT0819
         MVI   WCNCCW+7,X'1D'      POSITION OF CHANGE.                  DITT0820
         MVI   RCNCCW+7,X'04'                                           DITT0821
         BAL   8,WRTCON            REPLY 001-9999.                      DITT0822
         LA    11,CONIO                                                 DITT0823
         LA    12,4(0)                                                  DITT0824
         BAL   8,TSTNUMV           IS REPLY NUMERIC ?                   DITT0825
         B     GETLTH                                                   DITT0826
         MVC   HOLD+4(4),CONIO                                          DITT0827
         BAL   8,BINCVT            CONVERT TO BINARY.                   DITT0828
         LTR   1,1                 REPLY GREATER THAN 0000 ?            DITT0829
         BZ    GETLTH              NO, RESTART.                         DITT0830
         LA    8,1(0)                                                   DITT0831
         SR    1,8                                                      DITT0832
         LR    13,1                                                     DITT0833
         LR    8,9                 RECORD LNTH IN R8.                   DITT0834
         AR    8,1                 ADD STARTING ALTER POSITION.         DITT0835
         AR    8,3                 ADD # BYTES OF CHANGE.               DITT0836
         CR    8,10                IS TOTAL GREATER THAN REC LNTH ?     DITT0837
         BNH   LNTHOK              NO, LENGTH OK.                       DITT0838
         MVC   CONIO(28),=C'LENGTH EXCEEDS END OF RECORD'               DITT0839
         MVI   WCNCCW+7,X'1C'                                           DITT0840
         BAL   8,WCON2                                                  DITT0841
         B     CHNG                                                     DITT0842
LNTHOK   MVC   CONIO(26),ALTRMSG   LOG MSSG,                            DITT0843
         MVC   CONIO(5),DECKNAME   ENTER OR ALTER                       DITT0844
         MVI   WCNCCW+7,X'1A'                                           DITT0845
         MVI   RCNCCW+7,X'01'                                           DITT0846
         BAL   8,WRTCON                                                 DITT0847
         LA    12,CONIO                                                 DITT0848
         CLI   CONIO,C'C'          IS ALTER IN CHAR. MODE ?             DITT0849
         BE    CHARIN              YES, BRANCH.                         DITT0850
         CLI   CONIO,C'H'          IS ALTER IN HEX MODE ?               DITT0851
         BNE   LNTHOK              NO, INVALID REPLY, RESTART.          DITT0852
HEXIN    MVC   CONIO(31),ALTCT     MOVE SCALE TO SYSLOG.                DITT0853
         MVI   WCNCCW+7,X'1F'                                           DITT0854
         MVI   CONIO+16,C'2'                                            DITT0855
         LR    7,3                                                      DITT0856
         AR    7,3                                                      DITT0857
         BAL   8,WCON2                                                  DITT0858
         MVC   CONIO(75),SCALE+20                                       DITT0859
         STC   7,WCNCCW+7                                               DITT0860
         STC   7,RCNCCW+7                                               DITT0861
         BAL   8,WRTCON            GET ALTER DATA, HEX FORM.            DITT0862
HLOP     TM    0(12),C'0'          HEX CHAR = 0-9 ?                     DITT0863
         BO    HEND                YES, BRANCH.                         DITT0864
         TM    0(12),X'C0'         HEX CHAR = A-F ?                     DITT0865
         BNO   BDHEX               NO, BAD HEX CHAR.                    DITT0866
         NI    0(12),X'0F'                                              DITT0867
         CLI   0(12),X'06'                                              DITT0868
         BH    BDHEX                                                    DITT0869
         TR    0(1,12),ADTBL-1     SHIFT ZONE TO FORM 0X.               DITT0870
HEND     LA    12,1(12)                                                 DITT0871
         BCT   7,HLOP                                                   DITT0872
         PACK  CONIO+80(8),CONIO(15)   PACK DATA                        DITT0873
         PACK  CONIO+87(8),CONIO+14(15)    2 HEX POS. PER BYTE.         DITT0874
         PACK  CONIO+94(8),CONIO+28(15)                                 DITT0875
         PACK  CONIO+101(8),CONIO+42(15)                                DITT0876
         PACK  CONIO+108(8),CONIO+56(15)                                DITT0877
         LA    12,CONIO+80                                              DITT0878
         B     MOVLOPS                                                  DITT0879
CHARIN   MVC   CONIO(31),ALTCT     GET ALTER                            DITT0880
         MVI   WCNCCW+7,X'1F'      DATA IN                              DITT0881
         MVI   CONIO+16,C'1'       CHARACTER                            DITT0882
         BAL   8,WCON2             FORM FROM                            DITT0883
         MVC   CONIO(75),SCALE+20  SYSLOG.                              DITT0884
         STC   3,WCNCCW+7                                               DITT0885
         STC   3,RCNCCW+7                                               DITT0886
         BAL   8,WRTCON                                                 DITT0887
MOVLOPS  LR    8,9                                                      DITT0888
         AR    8,13                                                     DITT0889
MOVLOP   MVC   0(1,8),0(12)                                             DITT0890
         LA    8,1(8)              MOVE NEW DATA                        DITT0891
         LA    12,1(12)            1 BYTE AT A TIME                     DITT0892
         BCT   3,MOVLOP            TO RECORD AND OVERLAY.               DITT0893
         B     0(4)                RETURN FOR OTHER FUNCTIONS.          DITT0894
BDHEX    MVC   CONIO(16),=C'INVALID HEX CHAR'                           DITT0895
         MVI   WCNCCW+7,X'10'                                           DITT0896
         BAL   8,WCON2                                                  DITT0897
         LA    12,CONIO                                                 DITT0898
         B     HEXIN                                                    DITT0899
         SPACE 2                                                        DITT0900
*********************************************************************** DITT0901
**********         VALIDATE FUNCTION CODE AND BRANCH ACCORDINGLY ****** DITT0902
*********************************************************************** DITT0903
         SPACE 1                                                        DITT0904
LOOKUP   LA    5,FUNCT             FUNCTION TABLE POINTER IN R5.        DITT0905
         LA    9,FUNCTA            VECTOR BRANCH TABLE IN R9.           DITT0906
         CLC   CNTRLSW,ON          CONTROL CARD OPERATION ?             DITT0907
         BE    FILLBL              YES, BRANCH.                         DITT0908
         CLI   CONIO,X'50'         IS OP CODE = &XX ?                   DITT0909
         BNE   FILLBL              NO, BRANCH.                          DITT0910
         MVI   CONIO,C'C'          OVERLAY & WITH 'C'.                  DITT0911
         CLI   CDCCB+6,X'01'       IS READER = SYSNNN ?                 DITT0912
         BE    FILLBL              YES, BRANCH.                         DITT0913
         LH    2,22                GET SYSIPT                           DITT0914
         LH    1,76(2)                 LUB FOR                          DITT0915
         AH    1,SYSUNT                THIS PARTITION                   DITT0916
         MVC   CONIO+5(1),2(1)     AND STORE IN CONIO+5.                DITT0917
         CLI   CONIO+5,X'FD'       IS SYSIPT ASSGNED ?                  DITT0918
         BH    FILLBL              NO, BRANCH.                          DITT0919
         LH    12,74(2)                                                 DITT0920
         AH    12,FICL             GET ADDRESS OF                       DITT0921
         SR    3,3                     THIS PARTITION'S                 DITT0922
         IC    3,0(12)                 PROGRAMMER                       DITT0923
         LH    12,72(2)                LUBS AND                         DITT0924
         AH    12,FICL                 THE NUMBER OF LUBS.              DITT0925
         SR    7,7                                                      DITT0926
         IC    7,0(12)                                                  DITT0927
         LH    1,76(2)                                                  DITT0928
         AR    1,7                                                      DITT0929
         AR    1,7                                                      DITT0930
         SR    7,7                                                      DITT0931
NXLUB    CLC   0(1,1),CONIO+5      IS THERE A PROGRAMMER                DITT0932
         BE    MOVCIN                  LUB ASSIGNED TO                  DITT0933
         LA    7,1(7)                  THE SAME DEVICE ?                DITT0934
         LA    1,2(1)                                                   DITT0935
         BCT   3,NXLUB                                                  DITT0936
         LH    3,64(2)             GET ADDRESS OF PUB TABLE.            DITT0937
         SR    7,7                                                      DITT0938
         IC    7,CONIO+5           LUB POINTER VALUE                    DITT0939
         LR    1,7                                                      DITT0940
         SLL   7,3                 MULTIPLY BY 8 AND                    DITT0941
         AR    3,7                 ADD TO GIVE DESIRED PUB.             DITT0942
         CLI   4(3),X'60'          IS SYSIPT = 2311 ?                   DITT0943
         BE    FILLBL                                                   DITT0944
         CLI   4(3),X'62'          IS SYSIPT = 2314 ?                   DITT0945
         BE    FILLBL                                                   DITT0946
         LH    4,72(2)             LOAD FICL ADDRESS.                   DITT0947
         LH    13,74(2)            LOAD NICL ADDRESS.                   DITT0948
         LH    10,76(2)            LOAD LUB  ADDRESS.                   DITT0949
         BAL   9,LUBLOK            GO ASSGN A PROGRAMMER                DITT0950
         B     CONSOLE              LUB TO SYSIPT.                      DITT0951
         SR    7,7                                                      DITT0952
         IC    7,INPUT                                                  DITT0953
         LA    4,CONIO                                                  DITT0954
         LA    9,FUNCTA                                                 DITT0955
MOVCIN   STC   7,CDCCB+7           ALTER CDCCB TO READ FROM THIS        DITT0956
         MVI   CDCCB+6,X'01'           SYSXXX SO THAT /& CARDS          DITT0957
FILLBL   CLI   2(4),X'40'              MAY BE READ.                     DITT0958
         BNE   LKLOOP                                                   DITT0959
         MVI   2(4),C'U'           OVERLAY POS 3 OP CODE IF BLANK.      DITT0960
LKLOOP   CLC   0(3,4),0(5)         OP CODE MATCH FUNCTION TABLE ?       DITT0961
         BE    0(9)                YES, BRANCH TO VECTOR TABLE          DITT0962
         LA    5,3(5)                                                   DITT0963
         LA    9,4(9)              BUMP POINTERS.                       DITT0964
         CLC   0(1,5),ON           END OF TABLE ?                       DITT0965
         BE    0(8)                YES, RETURN                          DITT0966
         B     LKLOOP              NO, LOOP FOR NEXT LOOK.              DITT0967
FUNCT    DC    CL48'CCUCPUCTUCTRTCUTPUTPRTHUTHRTTUWTMFSFBSFFSRBSRREW'   DITT0968
         DC    CL48'DDUDRLDIDEOJCCSRUNCHUCCLINTTFAERGTRLTRSTCRTFDDRS'   DITT0969
         DC    CL15'DDREOFSDUSDRSRS'                                    DITT0970
         DC    X'FF'                                                    DITT0971
FUNCTA   B     CDCD                CARD - CARD                          DITT0972
         B     CDPR                CARD - PRINTER                       DITT0973
         B     CDTPU               CARD - TAPE UNBLOCKED.               DITT0974
         B     CDTPR               CARD - TAPE REBLOCKED.               DITT0975
         B     TPCD                TAPE - CARD UNBLOCKED.               DITT0976
         B     TPPRU               TAPE - LIST PRINT UNBLOCKED.         DITT0977
         B     TPPRR               TAPE - LIST PRINT REBLOCKED.         DITT0978
         B     TPDPU               TAPE - PRINT HEX UNBLOCKED.          DITT0979
         B     TPDPR               TAPE - PRINT HEX REBLOCKED.          DITT0980
         B     TPTP                TAPE - TAPE.                         DITT0981
         B     WTMK                WRITE TAPE MARK.                     DITT0982
         B     FSPF                FORWARD SPACE FILE.                  DITT0983
         B     BSPF                BACK SPACE FILE.                     DITT0984
         B     FSPR                FORWARD SPACE RECORD.                DITT0985
         B     BSPR                BACK SPACE RECORD.                   DITT0986
         B     REWTP               REWIND TAPE.                         DITT0987
         B     DISKDP              DISK PRINT UNBLOCKED.                DITT0988
         B     DISKRLD             DISK RECORD LOAD.                    DITT0989
         B     DSID                CHANGE DISK VOL #.                   DITT0990
         B     EOJ                 EOJ.                                 DITT0991
         B     CDCDS               CARD - CARD WITH SEQ. #'S.           DITT0992
         B     RUNTP               REWIND AND UNLOAD TAPE.              DITT0993
         B     CDDP                CARD - PRINTER IN HEX.               DITT0994
         B     CANC                CANCEL INPUT CARD DATA.              DITT0995
         B     INITP               INITIALIZE TAPE.                     DITT0996
         B     TPFOA               TAPE - PRINT FORMS TYPE A.           DITT0997
         B     ERGAP               ERASE TAPE RECORDS                   DITT0998
         B     TPRLD               TAPE RECORD LOAD.                    DITT0999
         B     TRSCAN              TAPE RECORD SCAN.                    DITT1000
         B     TPCD                TAPE - CARD.                         DITT1001
         B     TPFOD               TAPE - PRINT FORMS TYPE D            DITT1002
         B     DRSCAN              DISK RECORD SCAN.                    DITT1003
         B     DISKDR              DISK - PRINT REBLOCKED.              DITT1004
         B     WRTEOF              WRITE DISK EOF RECORD.               DITT1005
         B     SPLITDD             DISK PRINT - SPLIT CYLINDER.         DITT1006
         B     SPLITDR             DISK PRINT REBLOCKED - SPLIT CYL.    DITT1007
         B     SPLITRS             DISK RECORD SCAN - SPLIT CYL.        DITT1008
         EJECT                                                          DITT1009
*********************************************************************** DITT1010
*                                                                     * DITT1011
*                        UTILITY  ROUTINES                            * DITT1012
*                                                                     * DITT1013
*********************************************************************** DITT1014
         SPACE 2                                                        DITT1015
*********************************************************************** DITT1016
**********      CARD TO CARD WITH OR WITHOUT SEQ.  NUM.     *********** DITT1017
*********************************************************************** DITT1018
         SPACE 1                                                        DITT1019
CDCD     MVI   SEQSW+1,X'F0'                                            DITT1020
         B     CDCD1                                                    DITT1021
CDCDS    MVI   SEQSW+1,X'00'                                            DITT1022
         LA    7,1010(0)                                                DITT1023
         LA    10,HOLDSEQ                                               DITT1024
         LA    2,CONIO                                                  DITT1025
         CLC   CNTRLSW,ON                                               DITT1026
         BNE   GETDT                                                    DITT1027
         LA    9,CDCD1                                                  DITT1028
         MVC   CONIO(3),DECKTYPE                                        DITT1029
         B     CHK1                                                     DITT1030
GETDT    MVC   CONIO(15),=C'XXX - DECK TYPE'                            DITT1031
         MVI   WCNCCW+7,X'0F'                                           DITT1032
         MVI   RCNCCW+7,X'03'                                           DITT1033
         LA    9,GETDK                                                  DITT1034
         BAL   8,WRTCON                                                 DITT1035
CHK1     CLC   CONIO(3),=C'BAL'                                         DITT1036
         BNE   CHK2                                                     DITT1037
         LA    7,10(0)                                                  DITT1038
         LA    3,76(2)                                                  DITT1039
         MVI   ALT2+1,X'03'                                             DITT1040
         LA    5,72(2)                                                  DITT1041
         MVI   ALT1+1,X'03'                                             DITT1042
         MVI   DCKMSG+10,C'4'                                           DITT1043
         MVI   GETDK+11,X'04'                                           DITT1044
         LA    10,3(10)                                                 DITT1045
         B     0(9)                                                     DITT1046
CHK2     CLC   CONIO(3),=C'RPG'                                         DITT1047
         BNE   CHK3                                                     DITT1048
         LR    3,2                                                      DITT1049
         MVI   ALT2+1,X'04'                                             DITT1050
         LA    5,74(2)                                                  DITT1051
         MVI   ALT1+1,X'05'                                             DITT1052
         MVI   DCKMSG+10,C'6'                                           DITT1053
         MVI   GETDK+11,X'06'                                           DITT1054
         LA    10,3(10)                                                 DITT1055
         B     0(9)                                                     DITT1056
CHK3     CLC   CONIO(3),=C'COB'                                         DITT1057
         BNE   BADDT                                                    DITT1058
         LR    3,2                                                      DITT1059
         MVI   ALT2+1,X'05'                                             DITT1060
         LA    5,72(2)                                                  DITT1061
         MVI   ALT1+1,X'07'                                             DITT1062
         LA    10,2(10)                                                 DITT1063
         MVI   DCKMSG+10,C'8'                                           DITT1064
         MVI   GETDK+11,X'08'                                           DITT1065
         B     0(9)                                                     DITT1066
GETDK    MVC   CONIO(18),DCKMSG                                         DITT1067
         MVI   WCNCCW+7,X'12'                                           DITT1068
         MVI   RCNCCW+7,X'08'                                           DITT1069
         BAL   8,WRTCON                                                 DITT1070
         MVC   DECKNAME,CONIO                                           DITT1071
CDCD1    BAL   8,CDRD                                                   DITT1072
         CLC   EOF,ON                                                   DITT1073
         BE    CCEND                                                    DITT1074
SEQSW    BC    15,PNCHIT                                                DITT1075
ALT1     MVC   0(8,5),DECKNAME                                          DITT1076
         MVI   ESW,X'FF'                                                DITT1077
         BAL   8,DECCVT                                                 DITT1078
         CLI   ALT2+1,X'03'                                             DITT1079
         BE    ALT2                                                     DITT1080
         CLI   HOLDSEQ+5,C'2'                                           DITT1081
         BNE   ALT2                                                     DITT1082
         LA    7,800(7)                                                 DITT1083
ALT2     MVC   0(4,3),0(10)                                             DITT1084
         LA    7,10(7)                                                  DITT1085
PNCHIT   BAL   8,CDPN                                                   DITT1086
         B     CDCD1                                                    DITT1087
BADDT    CLC   CNTRLSW,ON                                               DITT1088
         BE    BADCTL                                                   DITT1089
         B     CDCDS                                                    DITT1090
CCEND    BC    0,CONSOLE                                                DITT1091
         MVC   CONIO(80),BLANKS                                         DITT1092
         BAL   8,CDPN                                                   DITT1093
         B     CONSOLE                                                  DITT1094
         SPACE 2                                                        DITT1095
*********************************************************************** DITT1096
**********         CARD TO PRINTER   LIST OR HEX            *********** DITT1097
*********************************************************************** DITT1098
         SPACE 1                                                        DITT1099
CDPR     BAL   8,OVFLO                                                  DITT1100
         MVI   PL,X'50'                                                 DITT1101
         BAL   8,CDRD                                                   DITT1102
         CLC   EOF,ON                                                   DITT1103
         BE    CONSOLE                                                  DITT1104
         MVI   CC,X'09'                                                 DITT1105
         BAL   8,EDIT              EDIT AND PRINT.                      DITT1106
         B     CDPR+8                                                   DITT1107
CDDP     BAL   8,OVFLO                                                  DITT1108
         SR    7,7                                                      DITT1109
         MVC   HDG1(10),DREC                                            DITT1110
         MVC   HDG1+13(7),DATH                                          DITT1111
         MVI   HDG1+18,C'8'                                             DITT1112
         MVI   HDG1+19,C'0'                                             DITT1113
         LA    10,DTPIO                                                 DITT1114
CDDP1    BAL   8,CDRD                                                   DITT1115
         CLC   EOF,ON                                                   DITT1116
         BE    CONSOLE                                                  DITT1117
         MVC   DTPIO(80),CONIO                                          DITT1118
         LA    7,1(7)                                                   DITT1119
         BAL   8,DECCVT                                                 DITT1120
         MVC   HDG1+5(5),HOLDSEQ+3                                      DITT1121
         MVI   PRLTH,X'50'                                              DITT1122
         BAL   9,HEXDP                                                  DITT1123
         B     CDDP1                                                    DITT1124
         SPACE 2                                                        DITT1125
*********************************************************************** DITT1126
**********         CARD TO TAPE - BLOCKED OR UNBLOCKED      *********** DITT1127
*********************************************************************** DITT1128
         SPACE 1                                                        DITT1129
CDTPR    MVI   SWT1+1,X'F0'                                             DITT1130
         CLC   CNTRLSW,OFF                                              DITT1131
         BE    CDTPR1                                                   DITT1132
         TM    CT,X'44'                                                 DITT1133
         BNO   BADCTL                                                   DITT1134
         MVC   CONIO(3),HBF                                             DITT1135
         B     CTRCOM                                                   DITT1136
CDTPR1   MVC   CONIO(10),=C'BLK FACTOR'                                 DITT1137
         MVI   WCNCCW+7,X'0A'                                           DITT1138
         MVI   RCNCCW+7,X'03'                                           DITT1139
         BAL   8,WRTCON                                                 DITT1140
CTRCOM   LA    12,3(0)                                                  DITT1141
         LA    11,CONIO                                                 DITT1142
         BAL   8,TSTNUMV                                                DITT1143
         B     CDTPR1                                                   DITT1144
         CLC   CONIO(3),ZEROS                                           DITT1145
         BH    BFOK                                                     DITT1146
         CLC   CNTRLSW,ON                                               DITT1147
         BE    BADCTL                                                   DITT1148
         B     CDTPR1                                                   DITT1149
BFOK     MVC   HOLD+5(3),CONIO                                          DITT1150
         BAL   8,BINCVT                                                 DITT1151
         STH   1,HBF                                                    DITT1152
         B     CDTPU+4                                                  DITT1153
CDTPU    MVI   SWT1+1,X'00'                                             DITT1154
         CLC   CNTRLSW,OFF                                              DITT1155
         BE    CDTPU1                                                   DITT1156
         TM    CT,X'40'                                                 DITT1157
         BNO   BADCTL                                                   DITT1158
         B     CTUCOM                                                   DITT1159
CDTPU1   BAL   7,OTMSG                                                  DITT1160
CTUCOM   MVC   OTTPCCB+7(1),OTPUT                                       DITT1161
SWT1     BC    0,REBLOK                                                 DITT1162
         MVC   OTTPCCW+6(2),EIGHTY                                      DITT1163
CTLOOP   BAL   8,CDRD                                                   DITT1164
         CLC   EOF,ON                                                   DITT1165
         BE    LTMK                                                     DITT1166
         MVC   DTPIO(80),CONIO                                          DITT1167
         BAL   8,TPOT                                                   DITT1168
         B     CTLOOP                                                   DITT1169
REBLOK   SR    4,4                                                      DITT1170
         LA    5,DTPIO                                                  DITT1171
         LH    2,HBF                                                    DITT1172
         MH    2,EIGHTY                                                 DITT1173
         STH   2,OTTPCCW+6                                              DITT1174
         C     2,DSL                                                    DITT1175
         BNH   MORCD                                                    DITT1176
         MVC   CONIO(6),SYSOTMSG+6                                      DITT1177
         B     WRGLNTC+6                                                DITT1178
MORCD    BAL   8,CDRD                                                   DITT1179
         CLC   EOF,ON                                                   DITT1180
         BE    LASTOT                                                   DITT1181
         MVC   0(80,5),CONIO                                            DITT1182
         LA    5,80(5)                                                  DITT1183
         LA    4,80(4)                                                  DITT1184
         CH    4,OTTPCCW+6                                              DITT1185
         BL    MORCD                                                    DITT1186
         LA    5,DTPIO                                                  DITT1187
         SR    4,4                                                      DITT1188
         BAL   8,TPOT                                                   DITT1189
         B     MORCD                                                    DITT1190
LASTOT   LTR   4,4                                                      DITT1191
         BZ    LTMK                                                     DITT1192
         STH   4,OTTPCCW+6                                              DITT1193
         BAL   8,TPOT                                                   DITT1194
         B     LTMK                                                     DITT1195
         SPACE 2                                                        DITT1196
*********************************************************************** DITT1197
**********         TAPE TO CARD - BLOCKED OR UNBLOCKED      *********** DITT1198
*********************************************************************** DITT1199
         SPACE 1                                                        DITT1200
TPCD     CLC   CNTRLSW,OFF                                              DITT1201
         BE    TC1                                                      DITT1202
         TM    CT,X'80'                                                 DITT1203
         BNO   BADCTL                                                   DITT1204
         MVI   CT,X'00'                                                 DITT1205
         B     TCCOM                                                    DITT1206
TC1      BAL   7,ITMSG                                                  DITT1207
TCCOM    MVC   INTPCCB+7(1),INPUT                                       DITT1208
         BAL   8,INTP                                                   DITT1209
         TM    4(1),1                                                   DITT1210
         BZ    MORTP+12                                                 DITT1211
MORTP    BAL   8,INTP                                                   DITT1212
         TM    4(1),1                                                   DITT1213
         BO    CCEND                                                    DITT1214
         SR    2,2                                                      DITT1215
         LA    4,DTPIO                                                  DITT1216
         L     5,DSL                                                    DITT1217
         SH    5,INTPCCB                                                DITT1218
         CLC   INTPCCB(2),DSL                                           DITT1219
         BE    WRGLNTC                                                  DITT1220
PUNCHMOR CH    5,=X'0051'                                               DITT1221
         BNE   MOVPNCH                                                  DITT1222
         LA    2,1(2)                                                   DITT1223
         LA    4,1(4)                                                   DITT1224
MOVPNCH  MVC   CONIO(80),0(4)                                           DITT1225
         LA    2,80(2)                                                  DITT1226
         LA    4,80(4)                                                  DITT1227
         BAL   8,CDPN                                                   DITT1228
         CR    2,5                                                      DITT1229
         BL    PUNCHMOR                                                 DITT1230
         BE    MORTP                                                    DITT1231
         MVC   CONIO(60),BLANKS                                         DITT1232
         MVC   CONIO(19),TLMSG                                          DITT1233
         B     WRGLNTC+12                                               DITT1234
WRGLNTC  MVC   CONIO(6),SYSITMSG+8                                      DITT1235
         MVC   CONIO+6(54),BADLTH                                       DITT1236
         CLC   CNTRLSW,ON                                               DITT1237
         BE    WMSGP                                                    DITT1238
         MVI   WCNCCW+7,X'3C'                                           DITT1239
         LA    8,CONSOLE                                                DITT1240
         B     WCON2                                                    DITT1241
WMSGP    MVI   PL,X'3C'                                                 DITT1242
         BAL   8,OVFLO                                                  DITT1243
         BAL   8,PRNT                                                   DITT1244
         SVC   6                                                        DITT1245
         SPACE 2                                                        DITT1246
*********************************************************************** DITT1247
**********     TAPE TO PRINTER - LIST OR HEX, BLK OR UNBLK  *********** DITT1248
*********************************************************************** DITT1249
         SPACE 1                                                        DITT1250
TPPRU    MVI   RSZSW+5,X'F0'                                            DITT1251
         MVI   DPSW,X'00'                                               DITT1252
         B     TP0                                                      DITT1253
TPPRR    MVI   RSZSW+5,X'00'                                            DITT1254
         MVI   DPSW,X'00'                                               DITT1255
         B     TP0                                                      DITT1256
TPDPU    MVI   RSZSW+5,X'F0'                                            DITT1257
         MVI   DPSW,X'FF'                                               DITT1258
         B     TP0                                                      DITT1259
TPDPR    MVI   RSZSW+5,X'00'                                            DITT1260
         MVI   DPSW,X'FF'                                               DITT1261
TP0      SR    2,2                                                      DITT1262
         MVC   HH2(29),DEVH                                             DITT1263
         CLC   CNTRLSW,OFF                                              DITT1264
         BE    TP1                                                      DITT1265
         TM    CT,X'80'                                                 DITT1266
         BNO   BADCTL                                                   DITT1267
         MVC   INTPCCB+7(1),INPUT                                       DITT1268
         MVC   HH2+16(3),INL                                            DITT1269
         BAL   9,GETPUB                                                 DITT1270
         B     RSZSW                                                    DITT1271
TP1      BAL   7,ITMSG                                                  DITT1272
         MVC   HH2+8(3),CONIO                                           DITT1273
         MVC   HH2+27(2),CONIO+3                                        DITT1274
         MVC   INTPCCB+7(1),INPUT                                       DITT1275
         SR    7,7                                                      DITT1276
         IC    7,INPUT                                                  DITT1277
         MVI   ESW,X'FF'                                                DITT1278
         BAL   8,DECCVT                                                 DITT1279
         MVC   HH2+16(3),HOLDSEQ+5                                      DITT1280
RSZSW    MVI   TPLPSW+1,X'F0'                                           DITT1281
         BC    0,TPCOM                                                  DITT1282
         MVI   TPLPSW+1,X'00'                                           DITT1283
         CLC   CNTRLSW,ON                                               DITT1284
         BE    TP3                                                      DITT1285
TP2      MVC   CONIO(16),RECSIZ                                         DITT1286
         MVI   WCNCCW+7,X'10'                                           DITT1287
         MVI   RCNCCW+7,X'05'                                           DITT1288
         BAL   8,WRTCON                                                 DITT1289
         LA    12,5(0)                                                  DITT1290
         LA    11,CONIO                                                 DITT1291
         BAL   8,TSTNUMV                                                DITT1292
         B     TP2                                                      DITT1293
         B     TP4                                                      DITT1294
TP3      TM    CT,X'10'                                                 DITT1295
         BZ    BADCTL                                                   DITT1296
         MVC   CONIO(5),HRS                                             DITT1297
TP4      LA    12,5(0)                                                  DITT1298
         CLC   CONIO(5),ZEROS                                           DITT1299
         BNE   *+8                                                      DITT1300
         MVI   CONIO,C'X'                                               DITT1301
         LA    11,CONIO                                                 DITT1302
         BAL   8,TSTNUM                                                 DITT1303
         B     TP2                                                      DITT1304
         MVC   HOLD+3(5),CONIO                                          DITT1305
         BAL   8,BINCVT                                                 DITT1306
         CLC   DBLSW,ON                                                 DITT1307
         BE    DISKDR1                                                  DITT1308
         LR    7,1                                                      DITT1309
         LR    12,1                                                     DITT1310
         BAL   8,DECCVT                                                 DITT1311
         MVC   DATH+5(5),HOLDSEQ+3                                      DITT1312
TPCOM    BAL   8,OVFLO                                                  DITT1313
         MVC   CONIO,HH1                                                DITT1314
         MVI   PL,X'84'                                                 DITT1315
         MVI   CC,X'19'                                                 DITT1316
         CLI   RSZSW+5,X'00'                                            DITT1317
         BE    *+8                                                      DITT1318
         BAL   8,PRNT                                                   DITT1319
         SR    2,2                                                      DITT1320
         ST    2,SAV                                                    DITT1321
         BAL   8,INTP                                                   DITT1322
         TM    4(1),1                                                   DITT1323
         BZ    TPLOOP+12                                                DITT1324
TPLOOP   BAL   8,INTP                                                   DITT1325
         TM    4(1),1                                                   DITT1326
         BO    CONSOLE                                                  DITT1327
         LA    10,DTPIO                                                 DITT1328
         L     5,DSL                                                    DITT1329
         SH    5,INTPCCB                                                DITT1330
         CLC   INTPCCB(2),DSL                                           DITT1331
         BE    WRGLNTC                                                  DITT1332
         LR    7,5                                                      DITT1333
         BAL   8,DECCVT                                                 DITT1334
         MVC   HDG1(10),TBLK                                            DITT1335
         MVC   HDG1+13(10),DATH                                         DITT1336
         MVC   HDG1+18(5),HOLDSEQ+3                                     DITT1337
         L     7,SAV                                                    DITT1338
         LA    7,1(7)                                                   DITT1339
         ST    7,SAV                                                    DITT1340
         BAL   8,DECCVT                                                 DITT1341
         MVC   HDG1+5(5),HOLDSEQ+3                                      DITT1342
TPLPSW   BC    15,TDU                                                   DITT1343
         MVI   CC,X'19'                                                 DITT1344
         MVI   PL,X'84'                                                 DITT1345
         MVC   CONIO,HH1                                                DITT1346
         MVC   CONIO+50(23),HDG1                                        DITT1347
         BAL   8,PRNT                                                   DITT1348
         B     TDRLOP                                                   DITT1349
TDU      LR    3,5                                                      DITT1350
         BAL   11,DEBLOK                                                DITT1351
         LA    1,100(0)                                                 DITT1352
         CR    5,1                                                      DITT1353
         BNH   TPLOOP                                                   DITT1354
         MVI   CC,X'13'                                                 DITT1355
         BAL   8,PRNT                                                   DITT1356
         B     TPLOOP                                                   DITT1357
TDRLOP   LR    3,12                                                     DITT1358
         MVC   HDG1(10),DREC                                            DITT1359
         MVC   HDG1+13(10),DATH                                         DITT1360
         LA    2,1(2)                                                   DITT1361
         LR    7,2                                                      DITT1362
         BAL   8,DECCVT                                                 DITT1363
         MVC   HDG1+5(5),HOLDSEQ+3                                      DITT1364
         CR    5,12                                                     DITT1365
         BNL   FULBLK                                                   DITT1366
         LR    3,5                                                      DITT1367
         LR    5,12                                                     DITT1368
         LR    7,3                                                      DITT1369
         BAL   8,DECCVT                                                 DITT1370
         MVC   HDG1+18(5),HOLDSEQ+3                                     DITT1371
FULBLK   BAL   11,DEBLOK                                                DITT1372
         SR    5,12                                                     DITT1373
         BNZ   TDRLOP                                                   DITT1374
         MVI   CC,X'13'                                                 DITT1375
         BAL   8,PRNT                                                   DITT1376
         B     TPLOOP                                                   DITT1377
         SPACE 2                                                        DITT1378
*********************************************************************** DITT1379
**********                    TAPE TO TAPE                  *********** DITT1380
*********************************************************************** DITT1381
         SPACE 1                                                        DITT1382
TPTP     MVI   TRLSW+1,X'00'                                            DITT1383
         MVI   LEADTM+1,X'00'                                           DITT1384
         CLC   CNTRLSW,OFF                                              DITT1385
         BE    TPTP1                                                    DITT1386
         TM    CT,X'C0'                                                 DITT1387
         BNO   BADCTL                                                   DITT1388
         MVI   CT,X'00'                                                 DITT1389
         MVC   OTTPCCB+7(1),OTPUT                                       DITT1390
         MVC   INTPCCB+7(1),INPUT                                       DITT1391
         B     FILELP                                                   DITT1392
TPTP1    BAL   7,ITMSG                                                  DITT1393
         MVC   INTPCCB+7(1),INPUT                                       DITT1394
         BAL   7,OTMSG                                                  DITT1395
         MVC   OTTPCCB+7(1),OTPUT                                       DITT1396
FILECT   MVC   CONIO(7),FILNUM                                          DITT1397
         MVI   WCNCCW+7,X'07'                                           DITT1398
         MVI   RCNCCW+7,X'02'                                           DITT1399
         BAL   8,WRTCON                                                 DITT1400
         LA    12,2(0)                                                  DITT1401
         LA    11,CONIO                                                 DITT1402
         BAL   8,TSTNUMV                                                DITT1403
         B     FILECT                                                   DITT1404
         MVC   HOLD+6(2),CONIO                                          DITT1405
         BAL   8,BINCVT                                                 DITT1406
         LTR   1,1                                                      DITT1407
         BZ    FILECT                                                   DITT1408
         LR    3,1                                                      DITT1409
FILELP   SR    7,7                                                      DITT1410
TTCOM    BAL   8,INTP                                                   DITT1411
         TM    4(1),1                                                   DITT1412
         BZ    TTMORE                                                   DITT1413
LEADTM   BC    0,*+8                                                    DITT1414
         LA    3,1(3)                                                   DITT1415
         MVC   CONIO+8(21),=C' BLKS AND T.M. COPIED'                    DITT1416
         MVI   ESW,X'FF'                                                DITT1417
         BAL   8,DECCVT                                                 DITT1418
         MVC   CONIO(8),HOLDSEQ                                         DITT1419
         CLC   CNTRLSW,ON                                               DITT1420
         BE    PRNTTC                                                   DITT1421
         MVI   WCNCCW+7,X'1D'                                           DITT1422
         BAL   10,LTMK+4                                                DITT1423
         BAL   8,WCON2                                                  DITT1424
         MVI   OTTPCCW,X'01'                                            DITT1425
         BCT   3,FILELP                                                 DITT1426
         B     CONSOLE                                                  DITT1427
PRNTTC   MVI   PL,X'1D'                                                 DITT1428
         BAL   8,PRNT                                                   DITT1429
         B     LTMK                                                     DITT1430
TTMORE   LA    7,1(7)                                                   DITT1431
         MVI   LEADTM+1,X'F0'                                           DITT1432
         L     5,DSL                                                    DITT1433
         SH    5,INTPCCB                                                DITT1434
         CLC   INTPCCB(2),DSL                                           DITT1435
         BE    WRGLNTC                                                  DITT1436
         STH   5,OTTPCCW+6                                              DITT1437
         BAL   8,TPOT                                                   DITT1438
TRLSW    BC    0,TRLRT                                                  DITT1439
         B     TTCOM                                                    DITT1440
         SPACE 2                                                        DITT1441
*********************************************************************** DITT1442
**********               INITIALIZE  TAPE                   *********** DITT1443
*********************************************************************** DITT1444
         SPACE 1                                                        DITT1445
INITP    CLC   CNTRLSW,ON                                               DITT1446
         BE    BADCTL                                                   DITT1447
         BAL   7,OTMSG                                                  DITT1448
         MVC   OTTPCCB+7(1),OTPUT                                       DITT1449
         MVC   CONIO(15),NVOLMSG                                        DITT1450
         MVI   WCNCCW+7,X'0F'                                           DITT1451
         BAL   8,WCON2                                                  DITT1452
         MVC   CONIO(06),XX                                             DITT1453
         MVI   WCNCCW+7,X'06'                                           DITT1454
         MVI   RCNCCW+7,X'06'                                           DITT1455
         BAL   8,WRTCON                                                 DITT1456
         MVC   DTPIO(80),BLANKS                                         DITT1457
         MVC   DTPIO(4),=C'VOL1'                                        DITT1458
         MVC   DTPIO+4(6),CONIO                                         DITT1459
         LA    8,DTPIO                                                  DITT1460
         MVC   41(10,8),USERNAME                                        DITT1461
         MVC   OTTPCCW+6(2),EIGHTY                                      DITT1462
         BAL   8,TPOT                                                   DITT1463
         MVC   DTPIO(61),BLANKS                                         DITT1464
         MVC   DTPIO(4),=C'HDR1'                                        DITT1465
         BAL   8,TPOT                                                   DITT1466
         B     LTMK                                                     DITT1467
         SPACE 2                                                        DITT1468
*********************************************************************** DITT1469
**********     PRINT SYSLST TAPES - TYPE A OR D UNBLOCKED   *********** DITT1470
*********************************************************************** DITT1471
         SPACE 1                                                        DITT1472
TPFOA    MVI   FORMSW+1,X'F0'                                           DITT1473
         B     TPFOM                                                    DITT1474
TPFOD    MVI   FORMSW+1,X'00'                                           DITT1475
TPFOM    CLC   CNTRLSW,OFF                                              DITT1476
         BE    TPF0                                                     DITT1477
         TM    CT,X'80'                                                 DITT1478
         BNO   BADCTL                                                   DITT1479
         B     TPF1                                                     DITT1480
TPF0     BAL   7,ITMSG                                                  DITT1481
TPF1     MVC   INTPCCB+7(1),INPUT                                       DITT1482
         MVI   CT,X'00'                                                 DITT1483
         MVI   DTPIO,C' '                                               DITT1484
         MVC   DTPIO+1(132),DTPIO                                       DITT1485
         MVC   INTPCCW+6(2),=X'0085'                                    DITT1486
         LA    9,TPF2                                                   DITT1487
TPF2     BAL   8,INTP                                                   DITT1488
         TM    4(1),1                                                   DITT1489
         BO    0(9)                                                     DITT1490
         LA    9,CONSOLE                                                DITT1491
         LA    1,PRCCB                                                  DITT1492
         CLC   INTPCCB(2),=X'000C'                                      DITT1493
         BE    FORMSW+4                                                 DITT1494
FORMSW   BC    15,TYPA                                                  DITT1495
         LA    10,15(0)                                                 DITT1496
         LA    7,CCTAB                                                  DITT1497
TRNCC    CLC   0(1,7),DTPIO                                             DITT1498
         BE    MOVCC                                                    DITT1499
         LA    7,2(7)                                                   DITT1500
         BCT   10,TRNCC                                                 DITT1501
         LA    7,CCTAB                                                  DITT1502
MOVCC    TM    2(1),X'80'                                               DITT1503
         BO    *+6                                                      DITT1504
         SVC   7                                                        DITT1505
         MVC   PRTCCW(1),1(7)                                           DITT1506
         SVC   0                                                        DITT1507
         MVI   DTPIO,X'01'                                              DITT1508
TYPA     TM    2(1),X'80'                                               DITT1509
         BO    *+6                                                      DITT1510
         SVC   7                                                        DITT1511
         MVC   PRTCCW(1),DTPIO                                          DITT1512
         MVC   PRTIO(132),DTPIO+1                                       DITT1513
         SVC   0                                                        DITT1514
         MVI   DTPIO,C' '                                               DITT1515
         MVC   DTPIO+1(132),DTPIO                                       DITT1516
         B     TPF2                                                     DITT1517
CCTAB    DC    X'400BF013601BF18BF293F39BF4A3F5ABF6B3F7BBF8C3F9C9'      DITT1518
         DC    X'C1D3C2D9C3E3'                                          DITT1519
         SPACE 3                                                        DITT1520
*********************************************************************** DITT1521
**********              TAPE RECORD LOAD                    *********** DITT1522
*********************************************************************** DITT1523
         SPACE 1                                                        DITT1524
TPRLD    CLC   CNTRLSW,ON                                               DITT1525
         BE    BADCTL                                                   DITT1526
         BAL   7,ITMSG                                                  DITT1527
         MVC   INTPCCB+7(1),INPUT                                       DITT1528
         BAL   7,OTMSG                                                  DITT1529
         MVC   OTTPCCB+7(1),OTPUT                                       DITT1530
         SR    7,7                                                      DITT1531
         MVI   LEADTM+1,X'00'                                           DITT1532
HOWFAR   MVC   CONIO(6),BLKMSG                                          DITT1533
         MVI   WCNCCW+7,X'06'                                           DITT1534
         MVI   RCNCCW+7,X'04'                                           DITT1535
         BAL   8,WRTCON                                                 DITT1536
         LA    12,4(0)                                                  DITT1537
         LA    11,CONIO                                                 DITT1538
         BAL   8,TSTNUMV                                                DITT1539
         B     HOWFAR                                                   DITT1540
         MVC   HOLD+4(4),CONIO                                          DITT1541
         BAL   8,BINCVT                                                 DITT1542
         LR    2,1                                                      DITT1543
         CLC   CONIO(4),ZEROS                                           DITT1544
         BE    HOWFAR                                                   DITT1545
WHICHWAY MVC   CONIO(24),DIRECT                                         DITT1546
         MVI   WCNCCW+7,X'18'                                           DITT1547
         MVI   RCNCCW+7,X'01'                                           DITT1548
         BAL   8,WRTCON                                                 DITT1549
         CLI   CONIO,C'B'                                               DITT1550
         BE    COPYB                                                    DITT1551
         CLI   CONIO,C'F'                                               DITT1552
         BNE   WHICHWAY                                                 DITT1553
COPYF    MVI   INTPCCW,X'02'                                            DITT1554
         MVI   OTTPCCW,X'01'                                            DITT1555
         MVI   TRLSW+1,X'F0'                                            DITT1556
         LA    3,1(0)                                                   DITT1557
FLOOP    B     TTCOM                                                    DITT1558
TRLRT    BCT   2,FLOOP                                                  DITT1559
         MVI   OTTPCCW,X'27'                                            DITT1560
         BAL   8,OVFLO                                                  DITT1561
         BAL   8,TPOT                                                   DITT1562
         MVI   OTTPCCW,X'01'                                            DITT1563
         LR    3,5                                                      DITT1564
         MVI   DPSW,X'FF'                                               DITT1565
         LA    10,DTPIO                                                 DITT1566
         BAL   8,DECCVT                                                 DITT1567
         MVC   TBLK+5(5),HOLDSEQ+3                                      DITT1568
         MVC   HDG1(10),TBLK                                            DITT1569
         ST    7,SAV                                                    DITT1570
         LR    7,5                                                      DITT1571
         BAL   8,DECCVT                                                 DITT1572
         MVC   HDG1+13(10),DATB                                         DITT1573
         MVC   HDG1+18(5),HOLDSEQ+3                                     DITT1574
         BAL   11,DEBLOK                                                DITT1575
RIGHTREC MVC   CONIO(11),DESIRED                                        DITT1576
         MVC   CONIO+11(10),CHNGMSG+16                                  DITT1577
         MVI   WCNCCW+7,X'15'                                           DITT1578
         MVI   RCNCCW+7,X'01'                                           DITT1579
         BAL   8,WRTCON                                                 DITT1580
         L     7,SAV                                                    DITT1581
         CLI   CONIO,C'N'                                               DITT1582
         BE    TRLRET                                                   DITT1583
         CLI   CONIO,C'Y'                                               DITT1584
         BNE   RIGHTREC                                                 DITT1585
         LA    1,TRLRET                                                 DITT1586
         STM   1,14,SAVREGS                                             DITT1587
         B     CHNGIT                                                   DITT1588
TRLRET   NOP   *+4                                                      DITT1589
         STH   5,OTTPCCW+6                                              DITT1590
         BAL   8,TPOT                                                   DITT1591
         B     HOWFAR                                                   DITT1592
COPYB    MVI   INTPCCW,X'27'                                            DITT1593
         MVI   OTTPCCW,X'27'                                            DITT1594
         SR    7,2                                                      DITT1595
BLOOP    BAL   8,INTP                                                   DITT1596
         BAL   8,TPOT                                                   DITT1597
         BCT   2,BLOOP                                                  DITT1598
         LA    2,1(0)                                                   DITT1599
         B     COPYF                                                    DITT1600
         SPACE 2                                                        DITT1601
*********************************************************************** DITT1602
**********               TAPE ERROR CORRECTION              *********** DITT1603
*********************************************************************** DITT1604
         SPACE 1                                                        DITT1605
TPERR    MVC   CONIO(24),TAPERR                                         DITT1606
         MVI   WCNCCW+7,X'18'                                           DITT1607
         MVC   TBLK+5(5),TAPERR+5                                       DITT1608
         BAL   8,WCON2                                                  DITT1609
TPE      MVC   CONIO(50),TPERRMSG                                       DITT1610
         MVI   WCNCCW+7,X'32'                                           DITT1611
         MVI   RCNCCW+7,X'01'                                           DITT1612
         BAL   8,WRTCON                                                 DITT1613
         CLI   CONIO,C'B'                                               DITT1614
         BE    RTTPER                                                   DITT1615
         CLI   CONIO,C'I'                                               DITT1616
         BE    TAPOK                                                    DITT1617
         CLI   CONIO,C'C'                                               DITT1618
         BNE   TPE                                                      DITT1619
         MVI   REPEAT+1,X'F0'                                           DITT1620
TPERLP   BAL   8,OVFLO                                                  DITT1621
         L     5,DSL                                                    DITT1622
         SH    5,INTPCCB                                                DITT1623
         CLC   INTPCCB(2),DSL                                           DITT1624
         BE    WRGLNTC                                                  DITT1625
         LR    7,5                                                      DITT1626
         BAL   8,DECCVT                                                 DITT1627
         MVC   HDG1(10),TBLK                                            DITT1628
         MVC   HDG1+13(10),DATB                                         DITT1629
         MVC   HDG1+18(5),HOLDSEQ+3                                     DITT1630
         LR    3,5                                                      DITT1631
         LA    10,DTPIO                                                 DITT1632
         LR    2,5                                                      DITT1633
         MVI   DPSW,X'FF'                                               DITT1634
         BAL   11,DEBLOK                                                DITT1635
REPEAT   BC    15,CHNGIT                                                DITT1636
         MVC   CONIO(26),CHNGMSG                                        DITT1637
         MVI   WCNCCW+7,X'1A'                                           DITT1638
         MVI   RCNCCW+7,X'01'                                           DITT1639
         BAL   8,WRTCON                                                 DITT1640
         CLI   CONIO,C'Y'                                               DITT1641
         BE    TAPOK                                                    DITT1642
         CLI   CONIO,C'N'                                               DITT1643
         BNE   REPEAT+4                                                 DITT1644
CHNGIT   MVI   REPEAT+1,X'00'                                           DITT1645
         MVC   CONIO(26),CHNGMSG                                        DITT1646
         MVC   CONIO+6(10),=C' TO LENGTH'                               DITT1647
         MVI   WCNCCW+7,X'1A'                                           DITT1648
         MVI   RCNCCW+7,X'01'                                           DITT1649
         BAL   8,WRTCON                                                 DITT1650
         CLI   CONIO,C'N'                                               DITT1651
         BE    OKLNTH                                                   DITT1652
         CLI   CONIO,C'Y'                                               DITT1653
         BNE   CHNGIT                                                   DITT1654
NEWLNTH  MVC   CONIO(16),RECSIZ                                         DITT1655
         MVC   CONIO(7),DESIRED                                         DITT1656
         MVI   WCNCCW+7,X'10'                                           DITT1657
         MVI   RCNCCW+7,X'05'                                           DITT1658
         BAL   8,WRTCON                                                 DITT1659
         LA    12,5(0)                                                  DITT1660
         LA    11,CONIO                                                 DITT1661
         BAL   8,TSTNUMV                                                DITT1662
         B     NEWLNTH                                                  DITT1663
         MVC   HOLD+3(5),CONIO                                          DITT1664
         BAL   8,BINCVT                                                 DITT1665
         CR    1,5                                                      DITT1666
         BNH   NOBLK                                                    DITT1667
         C     1,DSL                                                    DITT1668
         BH    WRGLNTC                                                  DITT1669
         LA    10,DTPIO                                                 DITT1670
         AR    10,5                                                     DITT1671
         LR    8,1                                                      DITT1672
         SR    8,5                                                      DITT1673
         MVI   0(10),C' '                                               DITT1674
         LA    10,1(10)                                                 DITT1675
         BCT   8,*-8                                                    DITT1676
NOBLK    ST    1,SAVREGS                                                DITT1677
         L     5,DSL                                                    DITT1678
         SR    5,1                                                      DITT1679
         STH   5,INTPCCB                                                DITT1680
         B     TPERLP                                                   DITT1681
OKLNTH   MVI   PRTIO,C'#'                                               DITT1682
         MVC   PRTIO+1(28),BYTMSG                                       DITT1683
         MVC   PRTIO+29(10),BLANKS                                      DITT1684
         LA    9,DTPIO                                                  DITT1685
         LR    10,9                                                     DITT1686
         AR    10,5                                                     DITT1687
         BAL   4,CHNG                                                   DITT1688
         B     TPERLP                                                   DITT1689
TAPOK    LM    1,14,SAVREGS                                             DITT1690
         B     4(1)                                                     DITT1691
RTTPER   LM    1,14,SAVREGS                                             DITT1692
         B     0(1)                                                     DITT1693
         SPACE 2                                                        DITT1694
*********************************************************************** DITT1695
**********                   TAPE RECORD SCAN               *********** DITT1696
*********************************************************************** DITT1697
         SPACE 1                                                        DITT1698
TRSCAN   CLC   CNTRLSW,ON                                               DITT1699
         BE    BADCTL                                                   DITT1700
         CLC   TYP,OFF                                                  DITT1701
         BNE   *+8                                                      DITT1702
         BAL   7,ITMSG                                                  DITT1703
         MVC   INTPCCB+7,INPUT                                          DITT1704
         MVC   CONIO(16),RECSIZ                                         DITT1705
         MVI   WCNCCW+7,X'10'                                           DITT1706
         MVI   RCNCCW+7,X'05'                                           DITT1707
         BAL   8,WRTCON                                                 DITT1708
         LA    11,CONIO                                                 DITT1709
         LA    12,5(0)                                                  DITT1710
         BAL   8,TSTNUMV                                                DITT1711
         B     TRSCAN                                                   DITT1712
         MVC   HOLD+3(5),CONIO                                          DITT1713
         BAL   8,BINCVT                                                 DITT1714
         LR    2,1                                                      DITT1715
SCANLTH  MVC   CONIO(25),=C'LNTH SCAN ARGUMENT (1-35)'                  DITT1716
         MVI   WCNCCW+7,X'19'                                           DITT1717
         LA    11,CONIO                                                 DITT1718
         MVI   RCNCCW+7,X'02'                                           DITT1719
         BAL   8,WRTCON                                                 DITT1720
         LA    12,2(0)                                                  DITT1721
         BAL   8,TSTNUMV                                                DITT1722
         B     SCANLTH                                                  DITT1723
         MVC   ALTCT+6(2),CONIO                                         DITT1724
         MVC   HDG1(10),TBLK                                            DITT1725
         MVC   HDG1+5(5),=C' SCAN'                                      DITT1726
         MVC   HDG1+13(7),DATH                                          DITT1727
         MVC   HDG1+18(2),CONIO                                         DITT1728
         MVC   HOLD+6(2),CONIO                                          DITT1729
         BAL   8,BINCVT                                                 DITT1730
         LA    8,35(0)                                                  DITT1731
         CR    1,8                                                      DITT1732
         BH    SCANLTH                                                  DITT1733
         LTR   1,1                                                      DITT1734
         BZ    SCANLTH                                                  DITT1735
         LR    3,1                                                      DITT1736
         LR    5,3                                                      DITT1737
SCANST   MVC   CONIO(29),SDPMSG                                         DITT1738
         MVI   WCNCCW+7,X'1D'                                           DITT1739
         MVI   RCNCCW+7,X'05'                                           DITT1740
         BAL   8,WRTCON                                                 DITT1741
         LA    12,5(0)                                                  DITT1742
         LA    11,CONIO                                                 DITT1743
         BAL   8,TSTNUMV                                                DITT1744
         B     SCANST                                                   DITT1745
         MVC   HOLD+3(5),CONIO                                          DITT1746
         BAL   8,BINCVT                                                 DITT1747
         LTR   1,1                                                      DITT1748
         BZ    SCANST                                                   DITT1749
         LR    4,1                                                      DITT1750
         SR    13,13                                                    DITT1751
         LA    9,PRTIO                                                  DITT1752
         LA    8,1(0)                                                   DITT1753
         SR    4,8                                                      DITT1754
         LR    10,4                                                     DITT1755
         CLC   TYP,OFF                                                  DITT1756
         BNE   SCANOK                                                   DITT1757
         AR    4,5                                                      DITT1758
         CR    2,4                                                      DITT1759
         BNL   SCANOK                                                   DITT1760
         MVC   CONIO(28),=C'LENGTH EXCEEDS END OF RECORD'               DITT1761
         MVI   WCNCCW+7,X'1C'                                           DITT1762
         BAL   8,WCON2                                                  DITT1763
         CLI   TYP,X'02'                                                DITT1764
         BE    SCANLTH                                                  DITT1765
         B     TRSCAN                                                   DITT1766
SCANOK   MVC   DECKNAME(5),ALTCT                                        DITT1767
         BAL   4,LNTHOK                                                 DITT1768
         CLC   TYP,OFF                                                  DITT1769
         BNE   DRS1                                                     DITT1770
         STC   5,PRLTH                                                  DITT1771
         BAL   8,OVFLO                                                  DITT1772
         MVC   DTPIO(35),PRTIO                                          DITT1773
         LA    1,1(0)                                                   DITT1774
         SR    5,1                                                      DITT1775
         STC   5,SCANCHK+1                                              DITT1776
         LR    5,10                                                     DITT1777
         LA    10,DTPIO                                                 DITT1778
         BAL   9,HEXDP                                                  DITT1779
         LR    10,5                                                     DITT1780
         MVC   PRTIO(35),DTPIO                                          DITT1781
         SR    7,7                                                      DITT1782
         BAL   8,INTP                                                   DITT1783
         TM    4(1),1                                                   DITT1784
         BZ    SCANIN2                                                  DITT1785
SCANIN   BAL   8,INTP                                                   DITT1786
         TM    4(1),1                                                   DITT1787
         BZ    SCANIN2                                                  DITT1788
         MVC   CONIO(14),=C'NO MATCH FOUND'                             DITT1789
         MVI   WCNCCW+7,X'0E'                                           DITT1790
         BAL   8,WCON2                                                  DITT1791
         B     CONSOLE                                                  DITT1792
SCANIN2  LA    7,1(7)                                                   DITT1793
         SR    4,4                                                      DITT1794
         L     5,DSL                                                    DITT1795
         SH    5,INTPCCB                                                DITT1796
         CLC   INTPCCB(2),DSL                                           DITT1797
         BE    WRGLNTC                                                  DITT1798
         LA    1,DTPIO                                                  DITT1799
SCANLOOP LR    3,1                                                      DITT1800
         AR    3,10                                                     DITT1801
SCANCHK  CLC   0(1,3),PRTIO                                             DITT1802
         BE    SCANHIT                                                  DITT1803
         AR    1,2                                                      DITT1804
         AR    4,2                                                      DITT1805
         CR    5,4                                                      DITT1806
         BH    SCANLOOP                                                 DITT1807
         B     SCANIN                                                   DITT1808
SCANHIT  MVI   DPSW,X'FF'                                               DITT1809
         BAL   8,DECCVT                                                 DITT1810
         MVC   HDG1(10),TBLK                                            DITT1811
         MVC   HDG1+5(5),HOLDSEQ+3                                      DITT1812
         LR    7,5                                                      DITT1813
         BAL   8,DECCVT                                                 DITT1814
         MVC   HDG1+13(10),DATH                                         DITT1815
         MVC   HDG1+18(5),HOLDSEQ+3                                     DITT1816
         LR    3,5                                                      DITT1817
         BAL   8,OVFLO                                                  DITT1818
         LA    10,DTPIO                                                 DITT1819
         BAL   11,DEBLOK                                                DITT1820
         B     CONSOLE                                                  DITT1821
         SPACE 2                                                        DITT1822
*********************************************************************** DITT1823
**********         TAPE CONTROL AND MISC. FUNCTIONS         *********** DITT1824
*********************************************************************** DITT1825
         SPACE 1                                                        DITT1826
LTMK     LA    10,CONSOLE                                               DITT1827
         MVI   OTTPCCW,X'1F'                                            DITT1828
         BAL   8,TPOT                                                   DITT1829
         BR    10                                                       DITT1830
REWTP    MVI   OTTPCCW,X'07'                                            DITT1831
         B     TPCT                                                     DITT1832
RUNTP    MVI   OTTPCCW,X'0F'                                            DITT1833
         B     TPCT                                                     DITT1834
ERGAP    MVI   OTTPCCW,X'17'                                            DITT1835
         B     TPCT                                                     DITT1836
WTMK     MVI   OTTPCCW,X'1F'                                            DITT1837
         B     TPCT                                                     DITT1838
BSPF     MVI   OTTPCCW,X'2F'                                            DITT1839
         B     TPCT                                                     DITT1840
FSPF     MVI   OTTPCCW,X'3F'                                            DITT1841
         B     TPCT                                                     DITT1842
FSPR     MVI   OTTPCCW,X'37'                                            DITT1843
         B     NBLKMSG                                                  DITT1844
BSPR     MVI   OTTPCCW,X'27'                                            DITT1845
NBLKMSG  CLC   CNTRLSW,ON                                               DITT1846
         BE    TCNTSAV                                                  DITT1847
         MVC   CONIO(6),BLKMSG                                          DITT1848
         MVI   WCNCCW+7,X'06'                                           DITT1849
         MVI   RCNCCW+7,X'04'                                           DITT1850
         BAL   8,WRTCON                                                 DITT1851
         LA    12,4(0)                                                  DITT1852
         LA    11,CONIO                                                 DITT1853
         BAL   8,TSTNUMV                                                DITT1854
         B     NBLKMSG                                                  DITT1855
         B     TCNTCOM                                                  DITT1856
TCNTSAV  MVC   CONIO(4),HNB                                             DITT1857
         TM    CT,X'60'                                                 DITT1858
         BNO   BADCTL                                                   DITT1859
TCNTCOM  LA    12,4(0)                                                  DITT1860
         CLC   CONIO(4),ZEROS                                           DITT1861
         BNE   *+8                                                      DITT1862
         MVI   CONIO,C'X'                                               DITT1863
         LA    11,CONIO                                                 DITT1864
         BAL   8,TSTNUM                                                 DITT1865
         B     NBLKMSG                                                  DITT1866
         MVC   HOLD+4(4),CONIO                                          DITT1867
         BAL   8,BINCVT                                                 DITT1868
         LR    5,1                                                      DITT1869
         B     TPCT+4                                                   DITT1870
TPCT     LA    5,1(0)                                                   DITT1871
         MVI   OTTPCCW+7,X'01'                                          DITT1872
         CLC   CNTRLSW,OFF                                              DITT1873
         BE    TPCT1                                                    DITT1874
         LA    8,OTTPCCW                                                DITT1875
         ST    8,OTTPCCB+8                                              DITT1876
         TM    CT,X'40'                                                 DITT1877
         BNO   BADCTL                                                   DITT1878
         B     GOTTCT                                                   DITT1879
TPCT1    BAL   7,OTMSG                                                  DITT1880
GOTTCT   MVC   OTTPCCB+7(1),OTPUT                                       DITT1881
         SR    7,7                                                      DITT1882
TPCTLP   BAL   8,TPOT                                                   DITT1883
         BCT   5,TPCTLP                                                 DITT1884
         B     CONSOLE                                                  DITT1885
CANC     CLC   CNTRLSW,ON                                               DITT1886
         BE    BADCTL                                                   DITT1887
         CLC   EOF,ON                                                   DITT1888
         BE    CONSOLE                                                  DITT1889
         BAL   8,CDRD                                                   DITT1890
         B     CANC+10                                                  DITT1891
         SPACE 2                                                        DITT1892
*********************************************************************** DITT1893
**********               CHANGE DISK VOLUME NUMBER          *********** DITT1894
*********************************************************************** DITT1895
         SPACE 1                                                        DITT1896
DSID     CLC   CNTRLSW,ON                                               DITT1897
         BE    BADCTL                                                   DITT1898
         BAL   7,IDMSG                                                  DITT1899
         MVI   RWCCW,X'0E'                                              DITT1900
         MVI   SHCCW,X'31'                                              DITT1901
         LA    3,3(0)                                                   DITT1902
         STC   3,LOWLIM+6                                               DITT1903
         MVI   RWCCW+6,X'00'                                            DITT1904
         MVI   RWCCW+7,X'54'                                            DITT1905
         BAL   8,DASDFP                                                 DITT1906
         BAL   8,DISK                                                   DITT1907
         TM    3(1),X'08'                                               DITT1908
         BO    NOID                                                     DITT1909
         CLC   DTPIO(4),=C'VOL1'                                        DITT1910
         BNE   NOID                                                     DITT1911
         MVC   CONIO(16),=C'EXIST. VOL # IS '                           DITT1912
         MVC   CONIO+16(6),DTPIO+8                                      DITT1913
         MVI   WCNCCW+7,X'16'                                           DITT1914
         BAL   8,WCON2                                                  DITT1915
         MVC   CONIO(15),NVOLMSG                                        DITT1916
         MVI   WCNCCW+7,X'0F'                                           DITT1917
         BAL   8,WCON2                                                  DITT1918
         MVC   CONIO(6),XX                                              DITT1919
         MVI   WCNCCW+7,X'06'                                           DITT1920
         MVI   RCNCCW+7,X'06'                                           DITT1921
         BAL   8,WRTCON                                                 DITT1922
         MVC   DTPIO+8(6),CONIO                                         DITT1923
         MVI   RWCCW,X'0D'                                              DITT1924
         BAL   8,DISK                                                   DITT1925
         MVI   WCNCCW+7,X'13'                                           DITT1926
         MVC   CONIO(9),NVOLMSG+6                                       DITT1927
         MVC   CONIO+9(4),BPTRK+40                                      DITT1928
         MVC   CONIO+13(6),DTPIO+8                                      DITT1929
         BAL   8,WCON2                                                  DITT1930
         B     CONSOLE                                                  DITT1931
NOID     MVC   CONIO(20),NOREC                                          DITT1932
         MVI   WCNCCW+7,X'14'                                           DITT1933
         BAL   8,WCON2                                                  DITT1934
         B     CONSOLE                                                  DITT1935
         SPACE 2                                                        DITT1936
*********************************************************************** DITT1937
**********                    DISK RECORD LOAD              *********** DITT1938
*********************************************************************** DITT1939
         SPACE 1                                                        DITT1940
DISKRLD  CLC   CNTRLSW,ON                                               DITT1941
         BE    BADCTL                                                   DITT1942
         BAL   7,IDMSG                                                  DITT1943
         MVC   DEVH+8(3),CONIO                                          DITT1944
         SR    7,7                                                      DITT1945
         IC    7,OTPUT                                                  DITT1946
         MVI   ESW,X'FF'                                                DITT1947
         BAL   8,DECCVT                                                 DITT1948
         MVC   DEVH+16(3),HOLDSEQ+5                                     DITT1949
         MVC   HH2(19),DEVH                                             DITT1950
DRECAD   MVC   CONIO(23),RECADDRS                                       DITT1951
         MVI   WCNCCW+7,X'17'                                           DITT1952
         MVI   RCNCCW+7,X'0A'                                           DITT1953
         BAL   8,WRTCON                                                 DITT1954
         LA    11,CONIO+7                                               DITT1955
         LA    12,3(0)                                                  DITT1956
         BAL   8,TSTNUM                                                 DITT1957
         B     DRECAD                                                   DITT1958
         MVC   HOLD+5(3),CONIO+7                                        DITT1959
         BAL   8,BINCVT                                                 DITT1960
         LTR   1,1                                                      DITT1961
         BZ    DRECAD                                                   DITT1962
         LA    8,255(0)                                                 DITT1963
         CR    1,8                                                      DITT1964
         BH    DRECAD                                                   DITT1965
         STC   1,LOWLIM+6                                               DITT1966
         MVC   CONIO+3(2),CONIO+4                                       DITT1967
         LA    11,CONIO                                                 DITT1968
         BAL   9,EDTADR                                                 DITT1969
         B     DRECAD                                                   DITT1970
         STC   13,LOWLIM+3                                              DITT1971
         STC   1,LOWLIM+5                                               DITT1972
         MVC   DCYL+5(3),CONIO                                          DITT1973
         MVC   DHED+6(2),CONIO+3                                        DITT1974
         MVC   CONIO,HH1                                                DITT1975
         BAL   8,DASDFP                                                 DITT1976
         BAL   8,OVFLO                                                  DITT1977
         MVI   PL,X'84'                                                 DITT1978
         BAL   8,PRNT                                                   DITT1979
         MVI   CC,X'13'                                                 DITT1980
         BAL   8,PRNT                                                   DITT1981
         CLI   EOF,X'10'                                                DITT1982
         BE    WRTEOFR                                                  DITT1983
         BAL   4,DSKDEBL                                                DITT1984
         B     NOID                                                     DITT1985
ALTKEY   LA    9,DTPIO+8                                                DITT1986
         LTR   2,2                                                      DITT1987
         BZ    ALTDAT                                                   DITT1988
         LTR   5,5                                                      DITT1989
         BZ    KEYONLY                                                  DITT1990
         MVC   CONIO(5),ALTRMSG                                         DITT1991
         MVC   CONIO+5(18),SCANMSG+4                                    DITT1992
         MVI   WCNCCW+7,X'17'                                           DITT1993
         MVI   RCNCCW+7,X'01'                                           DITT1994
         BAL   8,WRTCON                                                 DITT1995
         CLI   CONIO,C'D'                                               DITT1996
         BE    ALTDAT                                                   DITT1997
         CLI   CONIO,C'K'                                               DITT1998
         BNE   ALTKEY                                                   DITT1999
KEYONLY  MVC   PRTIO(5),=C'# KEY'                                       DITT2000
         MVC   PRTIO+5(28),BYTMSG                                       DITT2001
         MVC   PRTIO+33(10),BLANKS                                      DITT2002
         LR    10,9                                                     DITT2003
         AR    10,2                                                     DITT2004
         BAL   4,CHNG                                                   DITT2005
         B     DRLPRT                                                   DITT2006
ALTDAT   LTR   5,5                                                      DITT2007
         BZ    CONSOLE                                                  DITT2008
         AR    9,2                                                      DITT2009
         LR    10,9                                                     DITT2010
         AR    10,5                                                     DITT2011
         MVC   PRTIO(6),=C'# DATA'                                      DITT2012
         MVC   PRTIO+6(28),BYTMSG                                       DITT2013
         BAL   4,CHNG                                                   DITT2014
DRLPRT   BAL   8,OVFLO                                                  DITT2015
         MVC   CONIO(31),=C'ALTERED  DISK  RECORD  IN  CORE'            DITT2016
         MVI   PL,X'1F'                                                 DITT2017
         BAL   8,PRNT                                                   DITT2018
         BAL   4,DRCDEBL                                                DITT2019
         B     DRCMSG                                                   DITT2020
DRCMSG   MVC   CONIO(26),CHNGMSG                                        DITT2021
         MVI   WCNCCW+7,X'1A'                                           DITT2022
         MVI   RCNCCW+7,X'01'                                           DITT2023
         BAL   8,WRTCON                                                 DITT2024
         CLI   CONIO,C'Y'                                               DITT2025
         BE    WRTDRC                                                   DITT2026
         CLI   CONIO,C'N'                                               DITT2027
         BNE   DRCMSG                                                   DITT2028
         B     ALTKEY                                                   DITT2029
WRTDRC   LA    8,DTPIO+8                                                DITT2030
         ST    8,RWCCW                                                  DITT2031
         MVI   RWCCW,X'0D'                                              DITT2032
         BAL   8,DISK                                                   DITT2033
         TM    3(1),X'08'                                               DITT2034
         BO    NOID                                                     DITT2035
         B     CONSOLE                                                  DITT2036
         SPACE 2                                                        DITT2037
*********************************************************************** DITT2038
***********             DISK DUMP  BLK OR UNBLK             *********** DITT2039
*********************************************************************** DITT2040
         SPACE 1                                                        DITT2041
SPLITDD  MVI   EOF,X'01'                                                DITT2042
         B     DISKDP                                                   DITT2043
SPLITDR  MVI   EOF,X'01'                                                DITT2044
DISKDR   MVI   DBLSW,X'FF'                                              DITT2045
         CLC   CNTRLSW,OFF                                              DITT2046
         BE    DISKDP                                                   DITT2047
         TM    CT,X'93'                                                 DITT2048
         BO    DISKDP                                                   DITT2049
         MVI   CT,X'00'                                                 DITT2050
DISKDP   MVC   HH2(22),DEVH                                             DITT2051
         MVC   HH2+22(24),DSKH                                          DITT2052
         CLC   CNTRLSW,OFF                                              DITT2053
         BE    DSKCON                                                   DITT2054
         TM    CT,X'83'                                                 DITT2055
         BNO   BADCTL                                                   DITT2056
         MVC   DSKCCB+7(1),INPUT                                        DITT2057
         MVC   HH2+16(3),INL                                            DITT2058
         BAL   9,GETPUB                                                 DITT2059
         MVC   CONIO(5),ADRSLO                                          DITT2060
         B     DSPR1                                                    DITT2061
DSKCON   BAL   7,IDMSG                                                  DITT2062
         MVC   HH2+8(3),CONIO                                           DITT2063
         SR    7,7                                                      DITT2064
         IC    7,INPUT                                                  DITT2065
         MVI   ESW,X'FF'                                                DITT2066
         BAL   8,DECCVT                                                 DITT2067
         MVC   HH2+16(3),HOLDSEQ+5                                      DITT2068
ADDBG    MVC   CONIO(13),=C'CCCHH - BEGIN'                              DITT2069
         MVI   WCNCCW+7,X'0D'                                           DITT2070
         MVI   RCNCCW+7,X'05'                                           DITT2071
         BAL   8,WRTCON                                                 DITT2072
DSPR1    LA    11,CONIO                                                 DITT2073
         BAL   9,EDTADR                                                 DITT2074
         B     ADDBG                                                    DITT2075
         STC   13,LOWLIM+3                                              DITT2076
         MVI   BEGHD,X'00'                                              DITT2077
         CLI   EOF,X'01'                                                DITT2078
         BNE   *+8                                                      DITT2079
         STC   1,BEGHD                                                  DITT2080
         STC   1,LOWLIM+5                                               DITT2081
         MVC   CONIO(5),ADRSUP                                          DITT2082
         CLC   CNTRLSW,ON                                               DITT2083
         BE    DSPR2                                                    DITT2084
ADDEN    MVC   CONIO(11),=C'CCCHH - END'                                DITT2085
         MVI   WCNCCW+7,X'0B'                                           DITT2086
         MVI   RCNCCW+7,X'05'                                           DITT2087
         BAL   8,WRTCON                                                 DITT2088
DSPR2    BAL   9,EDTADR                                                 DITT2089
         B     ADDEN                                                    DITT2090
         STC   13,UPPLIM+3                                              DITT2091
         STC   1,UPPLIM+5                                               DITT2092
         CLI   EOF,X'01'                                                DITT2093
         BNE   *+8                                                      DITT2094
         STC   1,ENDHD                                                  DITT2095
         BAL   8,DASDFP                                                 DITT2096
         BAL   8,OVFLO                                                  DITT2097
         CLC   DBLSW,ON                                                 DITT2098
         BNE   RO                                                       DITT2099
         MVI   CT,X'10'                                                 DITT2100
         B     RSZSW+12                                                 DITT2101
DISKDR1  ST    1,SAVREGS                                                DITT2102
RO       CLC   UPPLIM,LOWLIM                                            DITT2103
         BL    CONSOLE                                                  DITT2104
         LA    8,SKHA                                                   DITT2105
         ST    8,DSKCCB+8                                               DITT2106
         BAL   8,DISK                                                   DITT2107
         TM    3(1),X'08'              NO REC                           DITT2108
         BZ    FOUNDHA                                                  DITT2109
         MVC   CONIO(17),=C'NO HOME ADDRS REC'                          DITT2110
         MVI   WCNCCW+7,X'11'                                           DITT2111
         BAL   8,WCON2                                                  DITT2112
         B     CONSOLE                                                  DITT2113
FOUNDHA  SR    7,7                                                      DITT2114
         IC    7,LOWLIM+3                                               DITT2115
         MVI   ESW,X'FF'                                                DITT2116
         BAL   8,DECCVT                                                 DITT2117
         MVC   HH2+31(3),HOLDSEQ+5                                      DITT2118
         MVC   DCYL+5(3),HOLDSEQ+5                                      DITT2119
         IC    7,LOWLIM+5                                               DITT2120
         MVI   ESW,X'FF'                                                DITT2121
         BAL   8,DECCVT                                                 DITT2122
         MVC   HH2+41(2),HOLDSEQ+6                                      DITT2123
         MVC   DHED+6(2),HOLDSEQ+6                                      DITT2124
         MVC   HH2+52(65),BLANKS                                        DITT2125
         LA    8,SKCCW                                                  DITT2126
         ST    8,DSKCCB+8                                               DITT2127
         MVC   RWCCW+6(2),DSL+2                                         DITT2128
         MVC   LLIMSAVE,LOWLIM                                          DITT2129
         CLI   DTPIO,X'03'                                              DITT2130
         BNE   TRDP                                                     DITT2131
         MVC   HH2+46(25),GATRK                                         DITT2132
         MVC   HH2+46(9),BPTRK                                          DITT2133
         MVI   ALTSW,X'FF'                                              DITT2134
         B     PRTRO                                                    DITT2135
TRDP     CLI   DTPIO,X'02'                                              DITT2136
         BNE   TROA                                                     DITT2137
         MVC   HH2+46(61),BPTRK                                         DITT2138
         SR    7,7                                                      DITT2139
         IC    7,DTPIO+6                                                DITT2140
         MVI   ESW,X'FF'                                                DITT2141
         BAL   8,DECCVT                                                 DITT2142
         MVC   HH2+94(3),HOLDSEQ+5                                      DITT2143
         IC    7,DTPIO+8                                                DITT2144
         MVI   ESW,X'FF'                                                DITT2145
         BAL   8,DECCVT                                                 DITT2146
         MVC   HH2+105(2),HOLDSEQ+6                                     DITT2147
         B     PRTRO                                                    DITT2148
TROA     CLI   DTPIO,X'01'                                              DITT2149
         BNE   TROP                                                     DITT2150
         MVC   HH2+46(64),GATRK                                         DITT2151
         SR    7,7                                                      DITT2152
         IC    7,DTPIO+6                                                DITT2153
         MVI   ESW,X'FF'                                                DITT2154
         BAL   8,DECCVT                                                 DITT2155
         MVC   HH2+97(3),HOLDSEQ+5                                      DITT2156
         MVC   DCYL+5(3),HOLDSEQ+5                                      DITT2157
         IC    7,DTPIO+8                                                DITT2158
         MVI   ESW,X'FF'                                                DITT2159
         BAL   8,DECCVT                                                 DITT2160
         MVC   HH2+108(2),HOLDSEQ+6                                     DITT2161
         MVC   DHED+6(2),HOLDSEQ+6                                      DITT2162
         MVC   LOWLIM+2(4),DTPIO+5                                      DITT2163
         B     PRTRO                                                    DITT2164
TROP     MVC   HH2+46(23),BPTRK                                         DITT2165
         MVC   HH2+46(9),GATRK                                          DITT2166
PRTRO    MVI   PL,X'84'                                                 DITT2167
         MVI   CC,X'19'                                                 DITT2168
         MVC   CONIO(132),HH1                                           DITT2169
         BAL   8,PRNT                                                   DITT2170
         SR    3,3                                                      DITT2171
         CLI   ALTSW,X'FF'                                              DITT2172
         BE    BUMPTK                                                   DITT2173
RN       LA    3,1(3)                                                   DITT2174
         STC   3,LOWLIM+6                                               DITT2175
         BAL   4,DSKDEBL                                                DITT2176
         B     BUMPTK                                                   DITT2177
         CLC   TYP,OFF                                                  DITT2178
         BNE   DRS2                                                     DITT2179
         B     RN                                                       DITT2180
BUMPTK   MVC   LOWLIM,LLIMSAVE                                          DITT2181
         MVI   ALTSW,X'00'                                              DITT2182
         CLC   LOWLIM+5(1),ENDHD                                        DITT2183
         BL    BUMPHD                                                   DITT2184
         IC    3,LOWLIM+3                                               DITT2185
         LA    3,1(3)                                                   DITT2186
         STC   3,LOWLIM+3                                               DITT2187
         MVC   LOWLIM+5(1),BEGHD                                        DITT2188
         B     RO                                                       DITT2189
BUMPHD   IC    3,LOWLIM+5                                               DITT2190
         LA    3,1(3)                                                   DITT2191
         STC   3,LOWLIM+5                                               DITT2192
         B     RO                                                       DITT2193
         SPACE 2                                                        DITT2194
*********************************************************************** DITT2195
**********               DISK  RECORD  SCAN                 *********** DITT2196
*********************************************************************** DITT2197
         SPACE 1                                                        DITT2198
SPLITRS  MVI   EOF,X'01'                                                DITT2199
DRSCAN   CLC   CNTRLSW,ON                                               DITT2200
         BE    BADCTL                                                   DITT2201
         MVI   DRS2+1,X'00'                                             DITT2202
         MVI   WCNCCW+7,X'1F'                                           DITT2203
         MVI   RCNCCW+7,X'01'                                           DITT2204
         MVC   CONIO(31),SCANMSG                                        DITT2205
         BAL   8,WRTCON                                                 DITT2206
         MVI   TYP,X'01'                                                DITT2207
         CLI   CONIO,C'E'                                               DITT2208
         BE    DISKDP                                                   DITT2209
         MVI   TYP,X'02'                                                DITT2210
         LA    2,255(0)                                                 DITT2211
         CLI   CONIO,C'K'                                               DITT2212
         BE    SCANLTH                                                  DITT2213
         MVI   TYP,X'03'                                                DITT2214
         CLI   CONIO,C'D'                                               DITT2215
         BE    TRSCAN                                                   DITT2216
         B     DRSCAN                                                   DITT2217
DRS1     ST    2,SAVREGS+8                                              DITT2218
         ST    5,SAVREGS                                                DITT2219
         ST    10,SAVREGS+4                                             DITT2220
         B     DISKDP                                                   DITT2221
DRS2     BC    0,DRS3                                                   DITT2222
         MVI   WCNCCW+7,X'0A'                                           DITT2223
         MVC   CONIO(10),=C'RECORDS AT'                                 DITT2224
         BAL   8,WCON2                                                  DITT2225
         MVC   CONIO(10),RECADDRS                                       DITT2226
         BAL   8,WCON2                                                  DITT2227
         LA    1,1(0)                                                   DITT2228
         L     4,SAVREGS                                                DITT2229
         SR    4,1                                                      DITT2230
         STC   4,KEYLNTH+1                                              DITT2231
         STC   4,DATLNTH+1                                              DITT2232
         MVI   DRS2+1,X'F0'                                             DITT2233
DRS3     CLI   TYP,X'02'                                                DITT2234
         BE    CKKEY                                                    DITT2235
         BH    CKDAT                                                    DITT2236
         LTR   5,5                                                      DITT2237
         BZ    HITIT                                                    DITT2238
         B     RN                                                       DITT2239
CKKEY    LTR   2,2                                                      DITT2240
         BZ    RN                                                       DITT2241
         L     0,SAVREGS+4                                              DITT2242
         L     1,SAVREGS                                                DITT2243
         AR    1,0                                                      DITT2244
         CR    2,1                                                      DITT2245
         BL    RN                                                       DITT2246
         LA    1,DTPIO+8                                                DITT2247
         AR    1,0                                                      DITT2248
KEYLNTH  CLC   PRTIO,0(1)                                               DITT2249
         BE    HITIT                                                    DITT2250
         B     RN                                                       DITT2251
CKDAT    LA    1,DTPIO+8                                                DITT2252
         AR    1,2                                                      DITT2253
         L     2,SAVREGS+4                                              DITT2254
         AR    1,2                                                      DITT2255
         L     4,SAVREGS+8                                              DITT2256
         LR    0,4                                                      DITT2257
DATLOOP  CR    0,5                                                      DITT2258
         BH    RN                                                       DITT2259
DATLNTH  CLC   PRTIO,0(1)                                               DITT2260
         BE    HITIT                                                    DITT2261
         AR    1,4                                                      DITT2262
         AR    0,4                                                      DITT2263
         B     DATLOOP                                                  DITT2264
HITIT    SR    7,7                                                      DITT2265
         IC    7,DTPIO+1                                                DITT2266
         MVI   ESW,X'FF'                                                DITT2267
         BAL   8,DECCVT                                                 DITT2268
         MVC   CONIO(3),HOLDSEQ+5                                       DITT2269
         IC    7,DTPIO+3                                                DITT2270
         MVI   ESW,X'FF'                                                DITT2271
         BAL   8,DECCVT                                                 DITT2272
         MVC   CONIO+4(2),HOLDSEQ+6                                     DITT2273
         IC    7,DTPIO+4                                                DITT2274
         MVI   ESW,X'FF'                                                DITT2275
         BAL   8,DECCVT                                                 DITT2276
         MVC   CONIO+7(3),HOLDSEQ+5                                     DITT2277
         MVI   WCNCCW+7,X'0A'                                           DITT2278
         BAL   8,WCON2                                                  DITT2279
         B     RN                                                       DITT2280
         SPACE 2                                                        DITT2281
*********************************************************************** DITT2282
**********                   WRITE EOF RECORD               *********** DITT2283
*********************************************************************** DITT2284
         SPACE 1                                                        DITT2285
WRTEOF   MVI   EOF,X'10'                                                DITT2286
         B     DISKRLD                                                  DITT2287
WRTEOFR  MVI   DTPIO+5,X'00'                                            DITT2288
         MVC   DTPIO+6(2),DTPIO+5                                       DITT2289
         MVI   RWCCW+6,X'00'                                            DITT2290
         MVI   RWCCW+7,X'08'                                            DITT2291
         MVI   RWCCW,X'1D'                                              DITT2292
         BAL   4,DSKDEBL+10                                             DITT2293
         B     NOID                                                     DITT2294
         B     CONSOLE                                                  DITT2295
         SPACE 2                                                        DITT2296
*********************************************************************** DITT2297
**********                        EOJ                       *********** DITT2298
*********************************************************************** DITT2299
         SPACE 1                                                        DITT2300
EOJ      MVI   INTRP+1,X'F0'                                            DITT2301
         LA    1,PRCCB                                                  DITT2302
         BAL   8,WAIT                                                   DITT2303
         CLI   SPOLPRT,X'FF'                                            DITT2304
         BNE   CHKPNSP                                                  DITT2305
         MVI   SPLCCW,X'1F'                                             DITT2306
         SVC   0                                                        DITT2307
         BAL   8,WAIT                                                   DITT2308
         MVI   SPLCCW,X'27'                                             DITT2309
         SVC   0                                                        DITT2310
CHKPNSP  CLI   SPOLPCH,X'FF'                                            DITT2311
         BNE   DONE                                                     DITT2312
         LA    1,PNCCB                                                  DITT2313
         BAL   8,WAIT                                                   DITT2314
         MVI   PNCCW,X'1F'                                              DITT2315
         BAL   8,CDPN                                                   DITT2316
         MVI   PNCCW,X'27'                                              DITT2317
         BAL   8,CDPN                                                   DITT2318
DONE     SVC   14                                                       DITT2319
WAIT     TM    2(1),X'80'                                               DITT2320
         BO    *+6                                                      DITT2321
         SVC   7                                                        DITT2322
         BR    8                                                        DITT2323
SPOLPRT  DC    X'00'                                                    DITT2324
SPOLPCH  DC    X'00'                                                    DITT2325
         EJECT                                                          DITT2326
*********************************************************************** DITT2327
**********         PHYSICAL  IOCS  ROUTINES                ************ DITT2328
*********************************************************************** DITT2329
         SPACE 1                                                        DITT2330
******************************    WRITE CONSOLE AND READ REPLY          DITT2331
WRTCON   LA    1,WCNCCB                                                 DITT2332
         EXCP  (1)                                                      DITT2333
         WAIT  (1)                                                      DITT2334
REREAD   MVC   CONIO(80),BLANKS                                         DITT2335
         LA    1,RCNCCB                                                 DITT2336
         EXCP  (1)                                                      DITT2337
         WAIT  (1)                                                      DITT2338
         TM    RCNCCB+4,1                                               DITT2339
         BO    REREAD                                                   DITT2340
         OC    CONIO(80),BLANKS                                         DITT2341
         BR    8                                                        DITT2342
******************************    WRITE CONSOLE ONLY                    DITT2343
WCON2    LA    1,WCNCCB                                                 DITT2344
         EXCP  (1)                                                      DITT2345
         WAIT  (1)                                                      DITT2346
         BR    8                                                        DITT2347
******************************    READ CARD                             DITT2348
CDRD     LA    1,CDCCB                                                  DITT2349
         EXCP  (1)                                                      DITT2350
         WAIT  (1)                                                      DITT2351
         CLI   CDCCB+6,X'00'                                            DITT2352
         BNE   CHKBL                                                    DITT2353
         TM    4(1),1                                                   DITT2354
         BZ    0(8)                                                     DITT2355
         MVC   EOF,ON                                                   DITT2356
         BR    8                                                        DITT2357
CHKBL    CLC   CONIO(80),BLANKS                                         DITT2358
         BNE   0(8)                                                     DITT2359
         MVC   EOF,ON                                                   DITT2360
         BR    8                                                        DITT2361
******************************    PUNCH CARD                            DITT2362
CDPN     LA    1,PNCCB                                                  DITT2363
         WAIT  (1)                                                      DITT2364
         MVC   PRTIO(80),CONIO                                          DITT2365
         EXCP  (1)                                                      DITT2366
         OI    SPOLPCH,X'0F'                                            DITT2367
         BR    8                                                        DITT2368
******************************    PRINT AND CARR. CONTROL               DITT2369
EDIT     TR    CONIO(132),TRTBL                                         DITT2370
PRNT     CLC   TYP,OFF                                                  DITT2371
         BNE   0(8)                                                     DITT2372
         OI    SPOLPRT,X'0F'                                            DITT2373
         SR    1,1                                                      DITT2374
         IC    1,CC                                                     DITT2375
         SRL   1,3(0)                                                   DITT2376
         AH    1,LINCT                                                  DITT2377
         STH   1,LINCT                                                  DITT2378
         LA    1,PRCCB                                                  DITT2379
         WAIT  (1)                                                      DITT2380
         LA    0,1(0)                                                   DITT2381
         IC    1,PL                                                     DITT2382
         SR    1,0                                                      DITT2383
         STC   1,PR1+1                                                  DITT2384
         MVI   PRTIO,C' '                                               DITT2385
         MVC   PRTIO+1(131),PRTIO                                       DITT2386
         LA    1,PRCCB                                                  DITT2387
         CLI   INPUT,X'FF'                                              DITT2388
         BE    PR1                                                      DITT2389
         CLC   DOSLCT,LINCT+1                                           DITT2390
         BNL   PR1                                                      DITT2391
         MVI   LINCT+1,X'01'                                            DITT2392
         CLI   CC,X'13'                                                 DITT2393
         MVI   CC,X'89'                                                 DITT2394
         BE    PR1+6                                                    DITT2395
PR1      MVC   PRTIO,CONIO                                              DITT2396
         MVC   PRTCCW(1),CC                                             DITT2397
         EXCP  (1)                                                      DITT2398
         BR    8                                                        DITT2399
OVFLO    CLC   TYP,OFF                                                  DITT2400
         BNE   0(8)                                                     DITT2401
         MVI   LINCT+1,X'01'                                            DITT2402
         MVI   INPUT,X'00'                                              DITT2403
         LA    1,PRCCB                                                  DITT2404
         WAIT  (1)                                                      DITT2405
PRTCC    MVI   PRTCCW,X'8B'                                             DITT2406
         EXCP  (1)                                                      DITT2407
TPCC     MVI   CC,X'11'                                                 DITT2408
         BR    8                                                        DITT2409
******************************    WRITE OUTPUT TAPE                     DITT2410
TPOT     LA    1,OTTPCCB                                                DITT2411
         EXCP  (1)                                                      DITT2412
         WAIT  (1)                                                      DITT2413
         BR    8                                                        DITT2414
******************************    READ INPUT TAPE                       DITT2415
INTP     TM    CT,X'20'                                                 DITT2416
         BZ    INTP1                                                    DITT2417
         LH    1,LOWLIM                                                 DITT2418
         LA    1,1(1)                                                   DITT2419
         STH   1,LOWLIM                                                 DITT2420
         CLC   BLCT,LOWLIM                                              DITT2421
         BNL   INTP1                                                    DITT2422
         LA    1,INTPCCB                                                DITT2423
         OI    4(1),1                                                   DITT2424
         BR    8                                                        DITT2425
INTP1    LA    1,INTPCCB                                                DITT2426
         EXCP  (1)                                                      DITT2427
         WAIT  (1)                                                      DITT2428
         TM    3(1),X'10'                                               DITT2429
         BZ    0(8)                                                     DITT2430
         LA    1,INTPRT                                                 DITT2431
         STM   1,14,SAVREGS                                             DITT2432
         B     TPERR                                                    DITT2433
INTPRT   B     INTP                                                     DITT2434
         LA    1,INTPCCB                                                DITT2435
         BR    8                                                        DITT2436
******************************    READ - WRITE DISK                     DITT2437
DISK     LA    1,DSKCCB                                                 DITT2438
         EXCP  (1)                                                      DITT2439
         WAIT  (1)                                                      DITT2440
         BR    8                                                        DITT2441
         SPACE 2                                                        DITT2442
*********************************************************************** DITT2443
**********               CHANNEL COMMAND WORDS              *********** DITT2444
*********************************************************************** DITT2445
         SPACE 1                                                        DITT2446
         CNOP  0,8                                                      DITT2447
RCNCCB   CCB   SYSLOG,RCNCCW                                            DITT2448
RCNCCW   CCW   X'0A',CONIO,X'00',72                                     DITT2449
WCNCCB   CCB   SYSLOG,WCNCCW                                            DITT2450
WCNCCW   CCW   X'09',CONIO,X'00',72                                     DITT2451
CDCCB    CCB   SYSIPT,CDCCW                                             DITT2452
CDCCW    CCW   X'02',CONIO,X'00',80                                     DITT2453
PNCCB    CCB   SYSPCH,PNCCW,X'8400'                                     DITT2454
PNCCW    CCW   X'41',PRTIO,X'00',80                                     DITT2455
PRCCB    CCB   SYSLST,PRTCCW,X'8400'                                    DITT2456
SPLCCW   CCW   X'01',CC,X'00',133                                       DITT2457
PRTCCW   CCW   X'09',PRTIO,X'00',132                                    DITT2458
INTPCCB  CCB   SYS000,INTPCCW,X'0200'                                   DITT2459
MSICCW   CCW   X'00',DTPIO,X'40',1                                      DITT2460
INTPCCW  CCW   X'02',DTPIO,X'00',1                                      DITT2461
OTTPCCB  CCB   SYS100,OTTPCCW                                           DITT2462
MSOCCW   CCW   X'00',DTPIO,X'40',1                                      DITT2463
OTTPCCW  CCW   X'01',DTPIO,X'00',1                                      DITT2464
DSKCCB   CCB   SYS100,SKCCW                                             DITT2465
SKCCW    CCW   X'07',LOWLIM,X'40',6                                     DITT2466
SHCCW    CCW   X'31',LOWLIM+2,X'40',5                                   DITT2467
TIC      CCW   X'08',*-8,X'40',1                                        DITT2468
RWCCW    CCW   X'1E',DTPIO,X'00',4001                                   DITT2469
SKHA     CCW   X'07',LOWLIM,X'40',6                                     DITT2470
HACCW    CCW   X'1A',DTPIO,X'40',5                                      DITT2471
ROCCW    CCW   X'16',DTPIO+5,X'00',16                                   DITT2472
         SPACE 2                                                        DITT2473
*********************************************************************** DITT2474
**********                   CONSTANTS                      *********** DITT2475
*********************************************************************** DITT2476
         SPACE 1                                                        DITT2477
SAVPSWS  DC    9D'0'                                                    DITT2478
DSL      DC    X'00007FFF'                                              DITT2479
FICL     DC    H'0'      VALUE BG=1,F2=2,F1=3                           DITT2480
SYSUNT   DC    X'0000'                                                  DITT2481
EIGHTY   DC    X'0050'                                                  DITT2482
LINCT    DC    X'0000'                                                  DITT2483
BLCT     DC    CL2' '                                                   DITT2484
HBF      DC    X'000000'                                                DITT2485
DOSLCT   DC    X'00'                                                    DITT2486
ON       DC    X'FF'                                                    DITT2487
DCYL     DC    CL8'CYL  XXX'                                            DITT2488
DHED     DC    CL8'HEAD  XX'                                            DITT2489
DREC     DC    CL10'REC  XXXXX'                                         DITT2490
KEYH     DC    CL9'KEY XXX  '                                           DITT2491
DATB     DC    CL10'DATA XXXXX'                                         DITT2492
DATH     DC    CL10'DATA XXXXX'                                         DITT2493
TBLK     DC    CL10'BLOCKXXXXX'                                         DITT2494
IN       DC    CL9'INPUT=SYS'                                           DITT2495
OT       DC    CL10'OUTPUT=SYS'                                         DITT2496
BG       DC    CL6'BEGIN='                                              DITT2497
EN       DC    CL4'END='                                                DITT2498
NB       DC    CL6'NBLKS='                                              DITT2499
RS       DC    CL8'RECSIZE='                                            DITT2500
BF       DC    CL10'BLKFACTOR='                                         DITT2501
DT       DC    CL9'DECKTYPE='                                           DITT2502
DN       DC    CL9'DECKNAME='                                           DITT2503
SYSMAX   DC    CL3'221'                                                 DITT2504
CCBSAV   DS    CL2                                                      DITT2505
ZEROS    DC    CL5'00000'                                               DITT2506
DEVH     DC    CL22'DEVICE  XXX  SYSXXX,  '                             DITT2507
         DC    CL7'MODE XX'                                             DITT2508
BYTMSG   DC    CL28' BYTES TO BE CHANGED, (1-35)'                       DITT2509
TAPERR   DC    CL24'TAPE ERROR ON INPUT TAPE'                           DITT2510
SDPMSG   DC    CL29'STARTING DATA POSITION IN REC'                      DITT2511
DSKH     DC    CL24'CYLINDER XXX, HEAD  X,  '                           DITT2512
BPTRK    DC    CL47'DEFECTIVE PRIMARY TRACK, ALTERNATE TRACK IS CYL'    DITT2513
         DC    CL14' XXX, TRACK  X'                                     DITT2514
GATRK    DC    CL50'OPERATIVE ALTERNATE TRACK, DEFECTIVE TRACK WAS CYL' DITT2515
         DC    CL14' XXX, TRACK  X'                                     DITT2516
DET1     DC    CL6'CHAR  '                                              DITT2517
DET2     DC    CL6'ZONE  '                                              DITT2518
DET3     DC    CL6'NUMR  '                                              DITT2519
CTL      DC    CL7'$$DITTO'                                             DITT2520
DCKMSG   DC    CL18'DECK NAME X CHARS.'                                 DITT2521
SYSOTMSG DC    CL19'CUUMM - OUTPUT TAPE'                                DITT2522
SYSITMSG DC    CL18'CUUMM - INPUT TAPE'                                 DITT2523
SYSDSK   DC    CL10'CUU - DISK'                                         DITT2524
BLKMSG   DC    CL6'# BLKS'                                              DITT2525
RECADDRS DC    CL23'CCC-HH-RRR  REC ADDRESS'                            DITT2526
SCANMSG  DC    CL31'SCAN KEY - K, DATA - D, EOF - E'                    DITT2527
DESIRED  DC    CL11'DESIRED REC'                                        DITT2528
DIRECT   DC    CL24'COPY FWD OR BACK, F OR B'                           DITT2529
FILNUM   DC    CL7'# FILES'                                             DITT2530
TPERRMSG DC    CL50'BYPASS REC - B, IGNORE ERROR - I, USER CORRECT - C' DITT2531
NOREC    DC    CL20'NO DISK RECORD FOUND'                               DITT2532
NVOLMSG  DC    CL15'ENTER NEW VOL #'                                    DITT2533
BADLTH   DC    CL40' RECORD EXCEEDS BUFFER SIZE, REALLOCATE '           DITT2534
         DC    CL14'PARTITION SIZE'                                     DITT2535
ALTRMSG  DC    CL26'ALTER IN HEX - H, CHAR - C'                         DITT2536
ALTCT    DC    CL31'ENTER XX BYTES, X CHAR PER BYTE'                    DITT2537
RECSIZ   DC    CL16'LOGICAL REC LNTH'                                   DITT2538
TLMSG    DC    CL19'TAPE NOT CARD IMAGE'                                DITT2539
CHNGMSG  DC    CL26'CHANGES COMPLETE ?, Y OR N'                         DITT2540
XX       DC    CL6'XXXXXX'                                              DITT2541
COMMA    DC    CL1','                                                   DITT2542
UPPERC   DC    CL3'202'                                                 DITT2543
UPPERH   DC    CL2'09'                                                  DITT2544
USERNAME DC    CL10'IBM  S/360'                                         DITT2545
CNTRLSW  DC    X'FF'                                                    DITT2546
         LTORG                                                          DITT2547
HH1      DC    CL8'* * * * '                                            DITT2548
HH2      DC    CL117' '                                                 DITT2549
HH3      DC    CL7' '                                                   DITT2550
BLANKS   DC    CL80' '                                                  DITT2551
ADRSLO   DC    CL5' '                                                   DITT2552
ADRSUP   DC    CL5' '                                                   DITT2553
INPUT    DC    CL1' '                                                   DITT2554
OTPUT    DC    CL1' '                                                   DITT2555
HNB      DC    CL4' '                                                   DITT2556
HRS      DC    CL5' '                                                   DITT2557
HDG1     DC    CL26' '                                                  DITT2558
HDG2     DC    CL26' '                                                  DITT2559
HDG3     DC    CL26' '                                                  DITT2560
DECKTYPE DC    CL3' '                                                   DITT2561
DECKNAME DC    CL8' '                                                   DITT2562
INL      DC    CL3' '                                                   DITT2563
OTL      DC    CL3' '                                                   DITT2564
CC       DC    X'00'                                                    DITT2565
PRTIO    DS    CL132                                                    DITT2566
CONIO    DS    CL132                                                    DITT2567
HEXIO    EQU   CONIO+12                                                 DITT2568
DEVTYP   DS    CL1                                                      DITT2569
HOLD     DC    1D'0'                                                    DITT2570
HOLDSEQ  DC    1D'0'                                                    DITT2571
SAVREGS  DC    7D'0'                                                    DITT2572
SAV      DC    3F'0'                                                    DITT2573
OFF      DC    X'00'                                                    DITT2574
DBLSW    DC    X'00'                                                    DITT2575
LOWLIM   DC    X'0000000000000000'                                      DITT2576
UPPLIM   DC    X'000000000000'                                          DITT2577
LLIMSAVE DC    X'0000000000000000'                                      DITT2578
ALTSW    DC    X'00'                                                    DITT2579
SW2314   DC    X'00'                                                    DITT2580
CT       DC    X'00'                                                    DITT2581
EOF      DC    X'00'                                                    DITT2582
TYP      DC    X'00'                                                    DITT2583
PL       DC    CL1' '                                                   DITT2584
ESW      DC    X'00'                                                    DITT2585
DTPIO    DS    CL1000                                                   DITT2586
         ORG   HH2                                                      DITT2587
         SPACE 2                                                        DITT2588
*********************************************************************** DITT2589
**********         DETERMINE PARTITION AND RELOCATE.        *********** DITT2590
**********     THIS SECTION IS OVERLAYED BY I/O AREA        *********** DITT2591
*********************************************************************** DITT2592
         SPACE 1                                                        DITT2593
RELOCAT  MVC   SET+1(99),SET                                            DITT2594
         LA    1,DTPIO                                                  DITT2595
         SR    2,1                 CALCULATE AMOUNT OF CORE AVAILABLE.  DITT2596
         C     2,DSL               IS AMT LESS THAN CCW MAXIMUM ?       DITT2597
         BNL   DSLOK               NO, BRANCH.                          DITT2598
         ST    2,DSL               YES, STORE NEW LNTH .                DITT2599
DSLOK    LA    1,CONIO                                                  DITT2600
         LA    2,RCNCCW                                                 DITT2601
         ST    2,RCNCCB+8                                               DITT2602
         BAL   8,OPCD              RELOCATE                             DITT2603
         LA    2,WCNCCW                                                 DITT2604
         ST    2,WCNCCB+8          CCB'S,                               DITT2605
         BAL   8,OPCD                                                   DITT2606
         LA    2,CDCCW             I/O AREAS,                           DITT2607
         ST    2,CDCCB+8                                                DITT2608
         BAL   8,OPCD              ETC.                                 DITT2609
         LA    1,PRTIO                                                  DITT2610
         LA    2,PNCCW                                                  DITT2611
         ST    2,PNCCB+8                                                DITT2612
         BAL   8,OPCD                                                   DITT2613
         LA    2,PRTCCW                                                 DITT2614
         ST    2,PRCCB+8                                                DITT2615
         BAL   8,OPCD                                                   DITT2616
         LA    1,DTPIO                                                  DITT2617
         LA    2,INTPCCW                                                DITT2618
         ST    2,INTPCCB+8                                              DITT2619
         BAL   8,OPCD                                                   DITT2620
         LA    2,OTTPCCW                                                DITT2621
         ST    2,OTTPCCB+8                                              DITT2622
         BAL   8,OPCD                                                   DITT2623
         LA    2,MSICCW                                                 DITT2624
         BAL   8,OPCD                                                   DITT2625
         LA    2,MSOCCW                                                 DITT2626
         BAL   8,OPCD                                                   DITT2627
         LA    2,RWCCW                                                  DITT2628
         BAL   8,OPCD                                                   DITT2629
         LA    2,HACCW                                                  DITT2630
         BAL   8,OPCD                                                   DITT2631
         LA    1,DTPIO+5                                                DITT2632
         LA    2,ROCCW                                                  DITT2633
         BAL   8,OPCD                                                   DITT2634
         LA    1,LOWLIM+2                                               DITT2635
         LA    2,SHCCW                                                  DITT2636
         BAL   8,OPCD                                                   DITT2637
         LA    1,LOWLIM                                                 DITT2638
         LA    2,SKCCW                                                  DITT2639
         ST    2,DSKCCB+8                                               DITT2640
         BAL   8,OPCD                                                   DITT2641
         LA    2,SKHA                                                   DITT2642
         BAL   8,OPCD                                                   DITT2643
         LA    1,SHCCW                                                  DITT2644
         LA    2,TIC                                                    DITT2645
         BAL   8,OPCD                                                   DITT2646
         LA    1,CC                                                     DITT2647
         LA    2,SPLCCW                                                 DITT2648
         BAL   8,OPCD                                                   DITT2649
         COMRG                     GET COMM. REGION.                    DITT2650
         MVC   DOSLCT,78(1)        STORE DOS LINE COUNT                 DITT2651
         TM    53(1),X'20'         DOS GENERATED WITH DASDFP=YES ?      DITT2652
         BZ    OKDISK              NO, BRANCH.                          DITT2653
         MVI   DASDFP+1,X'00'      YES, NOP BRANCH IN DASDFP ROUTINE.   DITT2654
OKDISK   SR    7,7                                                      DITT2655
         SR    8,8                                                      DITT2656
         SR    9,9                                                      DITT2657
         TM    53(1),X'40'         DOS GENERATED WITH MPS = YES ?       DITT2658
         BZ    BGRD                NO, BACKGROUND SUPPORT ONLY.         DITT2659
         LH    10,72(1)            YES, GET ADDRESS OF DOS FICL.        DITT2660
         LH    11,74(1)                 GET ADDRESS OF DOS NICL.        DITT2661
         ST    6,BUMP                                                   DITT2662
         LH    2,90(1)             GET ADDRESS OF PIB.                  DITT2663
         CLC   BUMP+1(3),57(2)     ARE WE IN F1 PARTITION ?             DITT2664
         BL    CHCKF2              NO, GO CHECK FOR F2.                 DITT2665
         MVI   FICL+1,X'03'        YES, SET FICL BUMP FOR F1.           DITT2666
         MVI   FILEMASK+2,X'22'    SET FILE PROTECT MASK FOR F1.        DITT2667
         MVI   DEVOWNER,X'05'      F1 PUB OWNERSHIP MASK                DITT2668
         MVI   PUBFLAG,X'02'       F1 PUB OWNERSHIP FLAG                DITT2669
         IC    7,2(10)             GET DISPL. TO F2 PROG. LUBS IN R7.   DITT2670
         IC    8,2(11)             GET # OF F2 PROG. LUBS IN R8.        DITT2671
         IC    9,3(10)             GET DISPL. TO F1 PROG. UNITS IN R9.  DITT2672
         B     CHKBJF              GO CHECK FOR BATCHED JOB FOREGRD.    DITT2673
CHCKF2   CLC   BUMP+1(3),41(2)     ARE WE IN F2 PARTITION ?             DITT2674
         BL    BGRD                NO, GO CHECK FOR BG.                 DITT2675
         MVI   FICL+1,X'02'        YES, SET FICL BUMP FOR F2.           DITT2676
         MVI   FILEMASK+2,X'21'    SET FILE PROTECT MASK FOR F2.        DITT2677
         MVI   DEVOWNER,X'06'      F2 PUB OWNERSHIP MASK                DITT2678
         MVI   PUBFLAG,X'01'       F2 PUB OWNERSHIP FLAG                DITT2679
         IC    7,1(10)             GET DISPL. TO BG PROG. LUBS IN R7.   DITT2680
         IC    8,1(11)             GET # OF BG LUBS IN R8.              DITT2681
         IC    9,2(10)             GET DISPL. TO F2 PROG. UNITS IN R9.  DITT2682
CHKBJF   TM    53(1),X'04'         DOS GENERATED WITH MPS = BJF ?       DITT2683
         BO    BGRD+4              YES, GO CHECK FOR CONTROL CARDS.     DITT2684
         LR    12,7                DISPL. TO LOWER PARTITION PROG. LUBS DITT2685
         AR    12,8                    ADD # OF PROG. LUBS.             DITT2686
         CR    12,9                COMPARE TO BEG. OF THIS PARTITION LB DITT2687
         BNE   RELOEND             NOT EQUAL, MUST BE VERSION III.      DITT2688
         MVI   CDCCB+6,X'01'       SET READER = SYS001,                 DITT2689
         MVI   PNCCB+6,X'01'           PUNCH = SYS002,                  DITT2690
         MVI   PRCCB+6,X'01'           PRINTER = SYS003.                DITT2691
         B     RELOEND                                                  DITT2692
BGRD     MVI   FICL+1,X'01'        SET FICL BUMP FOR BG.                DITT2693
         CLI   23(1),X'80'         WAS UPSI 1 CARD SUBMITTED ?          DITT2694
         BE    *+10                YES, CONTROL CARD OPERATION.         DITT2695
RELOEND  MVC   CNTRLSW,OFF         NO, TURN OFF CONTROL CARD SW.        DITT2696
         MVI   BLANKS,C' '             CONSOLE COMMUNICATION ONLY.      DITT2697
         MVC   BLANKS+1(79),BLANKS                                      DITT2698
         MVC   HH3,HH1                                                  DITT2699
         MVI   OFF,X'00'                                                DITT2700
         AR    7,8                 SYSUNT NOW CONTAINS THE LUB DISPL.   DITT2701
         STH   7,SYSUNT            TO THIS PARTITION'S SYSTEM LUBS.     DITT2702
         LH    3,64(1)             GET PUB TABLE ADDRESS IN R3.         DITT2703
         LH    2,76(1)             GET LUB TABLE ADDRESS IN R2.         DITT2704
         AH    2,SYSUNT            BUMP LUB TO THIS PART. SYS. LUBS     DITT2705
         CLI   4(2),X'FD'          IS SYSPCH ASSIGNED ?                 DITT2706
         BNL   CHKLST              NO, GO CHECK SYSLST.                 DITT2707
         SR    4,4                 YES,                                 DITT2708
         IC    4,4(2)              GET LUB FOR SYSPCH.                  DITT2709
         LA    9,8(0)              MULTIPLY THE PUB POINTER BY 8,       DITT2710
         MR    8,4                     FOR CORRECT SYSPCH PUB.          DITT2711
         AR    9,3                                                      DITT2712
         CLI   4(9),X'21'          IS SYSPCH = 2540  ?                  DITT2713
         BE    CHKLST              YES, NO CCW CHANGE.                  DITT2714
         CLI   4(9),X'20'          IS SYSPCH = 2520 B2 OR B3 ?          DITT2715
         BE    CHKLST              YES, NO CCW CHANGE.                  DITT2716
         CLI   4(9),X'31'          IS SYSPCH = 2520 B1 ?                DITT2717
         BE    CHKLST              YES, NO CCW CHANGE.                  DITT2718
         CLI   4(9),X'50'          IS SYSPCH = TAPE ?                   DITT2719
         BNE   CHK42N1             NO, GO CHECK 1442 N1.                DITT2720
         MVI   PNCCW,X'01'         YES, SET OP CODE = WRITE TAPE.       DITT2721
         MVI   CCEND+1,X'F0'       SET TAPE MK. SW FOR EOJ.             DITT2722
         MVI   SPOLPCH,X'F0'       SET PUNCH SPOOL SW ON.               DITT2723
         B     CHKLST              GO CHECK SYSLST.                     DITT2724
CHK42N1  CLI   4(9),X'30'          IS SYSPCH = 1442 N1 ?                DITT2725
         BNE   CHK42N2             NO, GO CHECK FOR 1442 N2.            DITT2726
         MVI   PNCCW,X'C1'         YES, SET CCW FOR PCH AND EJECT S2.   DITT2727
         B     CHKLST              GO CHECK SYSLST.                     DITT2728
CHK42N2  CLI   4(9),X'22'          IS SYSPCH = 1442 N2 ?                DITT2729
         BNE   CHKLST              NO, GO CHECK SYSLST.                 DITT2730
         MVI   PNCCW,X'81'         YES, SET CCW FOR PCH AND EJECT S1.   DITT2731
CHKLST   CLI   6(2),X'FD'          IS SYSLST ASSGNED ?                  DITT2732
         BNL   OKINTRP             NO, BYPASS LOOKUP.                   DITT2733
         SR    4,4                 YES,                                 DITT2734
         IC    4,6(2)              GET PUB POINTER AND                  DITT2735
         LA    9,8(0)              MULTIPLY BY 8                        DITT2736
         MR    8,4                 TO GET TO SYSLST PUB.                DITT2737
         AR    9,3                                                      DITT2738
         CLI   4(9),X'50'          IS SYSLST ASSGNED TO TAPE ?          DITT2739
         BNE   OKINTRP             NO, ASSUME 1403.                     DITT2740
         LA    7,SPLCCW            YES, MODIFY PRINTER CCB TO CHAIN     DITT2741
         ST    7,PRCCB+8           TO SPOOL CCW.                        DITT2742
         MVC   PRTCC+2(2),TPCC+2                                        DITT2743
         MVI   SPOLPRT,X'F0'       SET PRINTER SPOOL SW ON.             DITT2744
OKINTRP  MVC   CCBSAV,CDCCB+6      SAVE SYSIPT POINTER.                 DITT2745
         MVI   INTRP+1,X'00'       ENABLE INTERRUPT.                    DITT2746
         B     CONSOLE             RETURN,  INITIALIZATION COMPLETE.    DITT2747
OPCD     IC    3,0(2)                                                   DITT2748
         ST    1,0(2)                                                   DITT2749
         STC   3,0(2)                                                   DITT2750
         BR    8                                                        DITT2751
         END   DITTO                                                    DITT2752
/*       END DITTO SOURCE                                               DITT2753
// EXEC LNKEDT                                                          DITT2754
/&       END DITTO ASSEMBLE AND CATALOG                                 DITT2755
// JOB SYSGEN8B   $$BDITFP  DITTO FILE PROTECT TRANSIENT                $$FP0001
// OPTION CATAL,LOG                                                     $$FP0002
   PHASE $$BDITFP,+0                                                    $$FP0003
// EXEC ASSEMBLY                                                        $$FP0004
         SPACE 2                                                        $$FP0005
*********************************************************************** $$FP0006
*                                                                     * $$FP0007
*        THIS  B-TRANSIENT IS FETCHED BY THE PROBLEM PROGRAM 'DITTO', * $$FP0008
*        TO LOAD DISK FILE EXTENTS WHEN OPERATING IN A FILE PROTECTED * $$FP0009
*        ENVIRONMENT. THESE EXTENTS WILL BE DEQUED BY JOB CONTROL     * $$FP0010
*        AT EOJ TIME.                                                 * $$FP0011
*        INFORMATION PASSED TO THE TRANSIENT:                         * $$FP0012
*             REG  0  POINTS TO AN 8 BYTE PASS FIELD:                 * $$FP0013
*                  FIELD BYTE 0-3 = ADDRESS OF THE DESIRED LUB        * $$FP0014
*                  FIELD BYTE 4-7 = FILE MASK  (LIMITS AND            * $$FP0015
*                   PARTITION OWNERSHIP CODE).                        * $$FP0016
*        INFORMATION RETURNED TO DITTO:                               * $$FP0017
*             HEX FF RETURN TO BYTE 0 OF THE FILE MASK IF NO          * $$FP0018
*                   JIBS ARE AVAILABLE.                               * $$FP0019
*        PROCEDURE:                                                   * $$FP0020
*             1. CHECK LUB FOR AN ATTACHED JIB.                       * $$FP0021
*             2. IF NONE, OBTAIN AND ATTACH JIB AND LOAD XTENTS IN JIB* $$FP0022
*             3. IF JIB IS ATTACHED, FOLLOW JIB CHAIN UNTIL XTENT TYPE* $$FP0023
*                LOCATED OR END OF CHAIN REACHED.                     * $$FP0024
*             4. IF XTENT TYPE, INCREASE LIMITS TO MAXIMUM.           * $$FP0025
*             5. IF END OF CHAIN, ATTACH JIB AND LOAD XTENTS.         * $$FP0026
*                                                                     * $$FP0027
*              C. A. ALEXANDER     IBM     DETROIT, MICHIGAN          * $$FP0028
*********************************************************************** $$FP0029
         SPACE 2                                                        $$FP0030
         USING *,15                                                     $$FP0031
$$BDITFP DC    CL8'$$BDITFP'                                            $$FP0032
         STM   1,14,SAVE           SAVE REGISTERS                       $$FP0033
         SVC   22                  SEIZE THE SYSTEM                     $$FP0034
         LH    1,22                GET COMM. REGION                     $$FP0035
         LH    2,66(1)             ADDRESS OF 1ST AVAILABLE JIB         $$FP0036
         LR    10,0                                                     $$FP0037
         L     3,0(10)             ADDRESS OF LUB.                      $$FP0038
         BCT   3,*+4               DECREMENT LUB ADDRESS TO MAKE        $$FP0039
         BCT   3,*+4               LUB RESEMBLE A JIB.                  $$FP0040
         B     LUBJIB                                                   $$FP0041
JIBLOP   TM    2(3),X'20'          IS THIS AN XTENT JIB ?               $$FP0042
         BO    SHORTMOV            YES, BRANCH.                         $$FP0043
LUBJIB   CLI   3(3),X'FF'          END OF JIB CHAIN ?                   $$FP0044
         BE    GETJIB              YES, BRANCH AND ATTACH A JIB.        $$FP0045
         SR    7,7                 NO,                                  $$FP0046
         IC    7,3(3)              GET JIB POINTER.                     $$FP0047
         SLL   7,2                 MULTIPLY BY 4.                       $$FP0048
         LH    3,68(1)             GET BEGIN OF JIB TABLE.              $$FP0049
         AR    3,7                 BUMP TO NEXT JIB IN CHAIN.           $$FP0050
         B     JIBLOP                                                   $$FP0051
SHORTMOV MVC   0(3,3),4(10)        UPDATE FILE LIMITS IN JIB.           $$FP0052
         B     RETURN                                                   $$FP0053
GETJIB   SR    7,7                                                      $$FP0054
         CLI   0(2),X'FF'          ANY JIBS AVAILABLE ?                 $$FP0055
         BE    NOMORE              NO, BRANCH TO ERROR MSG.             $$FP0056
         MVC   3(1,3),0(2)         ATTACH NEW JIB TO EXISTING JIB.      $$FP0057
         LH    3,68(1)             GET ADDRESS OF JIB TABLE.            $$FP0058
         IC    7,0(2)              GET 1ST AVAIL JIB.                   $$FP0059
         SLL   7,2                 MULTIPLY BY 4.                       $$FP0060
         AR    3,7                 ADD TO JIB TABLE                     $$FP0061
         MVC   0(1,2),3(3)         RESET 1ST AVAILABLE JIB              $$FP0062
         MVC   0(4,3),4(10)        SET FILE LIMITS IN JIB.              $$FP0063
         B     RETURN                                                   $$FP0064
NOMORE   MVI   4(10),X'FF'         SET SW IN DITTO FOR NO MORE JIBS.    $$FP0065
RETURN   LM    1,14,SAVE                                                $$FP0066
         SVC   22                  RELEASE THE SYSTEM.                  $$FP0067
         SVC   11                  RETURN TO DITTO.                     $$FP0068
SAVE     DS    14F                                                      $$FP0069
         END                                                            $$FP0070
/*       END $$BDITFP SOURCE                                            $$FP0071
// EXEC LNKEDT                                                          $$FP0072
/&       END $$BDITFP ASSEMBLE AND CATALOG                              $$FP0073
// JOB SYSGEN8C   $$BDITAN DITTO TEMPORARY ASSGN TRANSIENT              $$AN0001
// OPTION CATAL,LOG                                                     $$AN0002
   PHASE $$BDITAN,+0                                                    $$AN0003
// EXEC ASSEMBLY                                                        $$AN0004
         SPACE 2                                                        $$AN0005
*********************************************************************** $$AN0006
*                                                                     * $$AN0007
*        THIS  B-TRANSIENT IS FETCHED BY THE PROBLEM PROGRAM 'DITTO', * $$AN0008
*        TO MAKE TEMPORARY LOGICAL UNIT ASSIGNMENTS.  DITTO WILL      * $$AN0009
*        VERIFY BEFORE FETCHING THIS TRANSIENT THAT:                  * $$AN0010
*             A.  THE DEVICE IS NOT DOWN.                             * $$AN0011
*             B.  THE DEVICE (IF NOT DISK) IS NOT OWNED               * $$AN0012
*                 BY ANOTHER PARTITION.                               * $$AN0013
*             C.  AN UNASSGNED LUB IS AVAILABLE.                      * $$AN0014
*        INFORMATION PASSED TO THE TRANSIENT:                         * $$AN0015
*             REG  0  POINTS TO AN 10 BYTE PASS FIELD:                * $$AN0016
*                  FIELD BYTE 0-1 = ADDRESS OF UNASSGNED LUB          * $$AN0017
*                  FIELD BYTE 2-3 = ADDRESS OF PUB                    * $$AN0018
*                  FIELD BYTE 4   = LUB POINTER VALUE                 * $$AN0019
*                  FIELD BYTE 5   = PUB OWNERSHIP FLAGS               * $$AN0020
*        INFORMATION RETURNED TO DITTO:                               * $$AN0021
*             HEX FF RETURN TO BYTE 4 IF NO                           * $$AN0022
*                   JIBS ARE AVAILABLE.                               * $$AN0023
*                                                                     * $$AN0024
*              C. A. ALEXANDER     IBM     DETROIT, MICHIGAN          * $$AN0025
*********************************************************************** $$AN0026
         SPACE 2                                                        $$AN0027
         USING *,15                                                     $$AN0028
$$BDITAN DC    CL8'$$BDITAN'                                            $$AN0029
         STM   1,14,SAVE           SAVE REGISTERS                       $$AN0030
         SVC   22                  SEIZE THE SYSTEM                     $$AN0031
         LH    1,22                GET COMM. REGION                     $$AN0032
         LH    2,66(1)             ADDRESS OF 1ST AVAILABLE JIB         $$AN0033
         LR    10,0                PASS FIELD POINTER                   $$AN0034
         LH    4,0(10)             REG 4 = ADDRESS OF NEWLUB            $$AN0035
         LH    5,2(10)             REG 5 = ADDRESS OF PUB               $$AN0036
         CLI   0(2),X'FF'          ANY JIBS ?                           $$AN0037
         BNE   ASSGN               YES, BRANCH.                         $$AN0038
         MVI   4(10),X'FF'         NO, RETURN NO JIB FLAG.              $$AN0039
         B     RETURN                                                   $$AN0040
ASSGN    MVC   0(1,4),4(10)        MOVE LUB POINTER VALUE TO NEWLUB     $$AN0041
         MVC   1(1,4),0(2)         MOVE JIB POINTER TO NEWLUB+1         $$AN0042
         SR    7,7                                                      $$AN0043
         IC    7,0(2)              GET 1ST AVAILABLE JIB POINTER        $$AN0044
         SLL   7,2                 MULTIPLY BY 4                        $$AN0045
         LH    8,68(1)             GET ADDRESS OF JIB TABLE.            $$AN0046
         AR    8,7                 BUMP TO 1ST AVAILABLE JIB            $$AN0047
         MVC   0(1,2),3(8)         UPDATE JIB POINTER                   $$AN0048
         MVI   0(8),X'FF'          BUILD JIB WITH STANDARD UNASSGN FOR  $$AN0049
         MVC   1(3,8),0(8)         LUB AND SET END OF JIB CHAIN.        $$AN0050
         MVI   2(8),X'80'          SET STD ASSGN FLAG                   $$AN0051
         OC    7(1,5),5(10)        OR IN PUB OWNERSHIP                  $$AN0052
RETURN   LM    1,14,SAVE                                                $$AN0053
         SVC   22                  RELEASE THE SYSTEM                   $$AN0054
         SVC   11                  RETURN TO DITTO                      $$AN0055
SAVE     DS    14F                                                      $$AN0056
         END                                                            $$AN0057
/*       END $$BDITAN SOURCE                                            $$AN0058
// EXEC LNKEDT                                                          $$AN0059
/&       END $$BDITAN ASSEMBLE AND CATALOG                              $$AN0060
