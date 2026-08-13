        TABS      20,28,44,45
        WITH    WI=107,DE=51
* BASIC COMPILED BY METAPLUS VERSION 1.4c
        ORG     0
STRVAR   RMB      1
VECTOR   RMB      1
ARRAY     RMB      1
FUNCTION RMB      1
STRINGFUNCTION   RMB      1
SCALAR     RMB      1
FREFLABEL RMB      1
ISLABEL   RMB      1
POTENTIALLABEL     RMB      1
COMMONSCALAR        RMB      1
INFORLOOP       RMB      1
STEPLIMALLOCATED   RMB      1
TYPESUBROUTINE   RMB      1
PARAMETER RMB      1
FREFSUBROUTINE   RMB      1
STRARRAY  RMB      1
        ORG     $104    METAMACHINE ENTRY POINTS
STARTTOKEN       RMB      3
GETTOKENCHAR      RMB      3
BACKUP     RMB      3
EATTOKEN RMB      3
FINDSTAR RMB      3
FINDSTARN        RMB      3
PUTCHAR   RMB      3
ENTERMETA        RMB      3
        ORG     $C80
        JMP     DEBLANKTOKEN
        JMP     ERRORTOKEN
        JMP     KEYWORDTOKEN
        #:21
        #.KEYWORDLIST
        #:31
.TA      EQU     *-6
.TC      SET     3
        RPT     29
        #0
.SA      EQU     *-$C0*2
.SC      SET     $C0
        RPT     576
        #0
:31     EQU     *
        +:11-*-1
        FCC       'Software Dynamics BASIC Compiler Version 1.4i (C) 1982'
        +$D
:11     EQU     *
BASIC      EQU     .SC
.SC      SET     .SC+1
        BASIC
        $2C
        0
.PK     SET     0
DIM     EQU     *
:22     +0
        +0
        +0
        #.PK
        +$80+:12-*-1
        FCC       "DIM"
:12     EQU     *
.PK     SET     :22
COMMON   EQU     *
:23     +0
        +0
        +0
        #.PK
        +$80+:13-*-1
        FCC       "COMMON"
:13     EQU     *
.PK     SET     :23
REM     EQU     *
:24     +0
        +0
        +0
        #.PK
        +$80+:14-*-1
        FCC       "REM"
:14     EQU     *
.PK     SET     :24
PROGRAM  EQU     *
:25     +0
        +0
        +0
        #.PK
        +$80+:15-*-1
        FCC       "PROGRAM"
:15     EQU     *
.PK     SET     :25
DATA      EQU     *
:26     +0
        +0
        +0
        #.PK
        +$80+:16-*-1
        FCC       "DATA"
:16     EQU     *
.PK     SET     :26
CONCATENATION       EQU     *
:27     +0
        +0
        +0
        #.PK
        +$80+:17-*-1
        FCC       "CONCATENATION"
:17     EQU     *
.PK     SET     :27
INCLUDE  EQU     *
:28     +0
        +0
        +0
        #.PK
        +$80+:18-*-1
        FCC       "INCLUDE"
:18     EQU     *
.PK     SET     :28
ON      EQU     *
:29     +0
        +0
        +0
        #.PK
        +$80+:19-*-1
        FCC       "ON"
:19     EQU     *
.PK     SET     :29
ERROR   EQU     *
:2A      +0
        +0
        +0
        #.PK
        +$80+:1A-*-1
        FCC       "ERROR"
:1A      EQU     *
.PK     SET     :2A
GOTO    EQU     *
:2B      +0
        +0
        +0
        #.PK
        +$80+:1B-*-1
        FCC       "GOTO"
:1B      EQU     *
.PK     SET     :2B
ELN     EQU     *
:2C      +0
        +0
        +0
        #.PK
        +$80+:1C-*-1
        FCC       "ELN"
:1C      EQU     *
.PK     SET     :2C
GOSUB    EQU     *
:2D     +0
        +0
        +0
        #.PK
        +$80+:1D-*-1
        FCC       "GOSUB"
:1D     EQU     *
.PK     SET     :2D
POP     EQU     *
:2E     +0
        +0
        +0
        #.PK
        +$80+:1E-*-1
        FCC       "POP"
:1E     EQU     *
.PK     SET     :2E
RETURN  EQU     *
:2F     +0
        +0
        +0
        #.PK
        +$80+:1F-*-1
        FCC       "RETURN"
:1F     EQU     *
.PK     SET     :2F
FOR     EQU     *
:210    +0
        +0
        +0
        #.PK
        +$80+:110-*-1
        FCC       "FOR"
:110    EQU     *
.PK     SET     :210
TO      EQU     *
:211    +0
        +0
        +0
        #.PK
        +$80+:111-*-1
        FCC       "TO"
:111    EQU     *
.PK     SET     :211
STEP    EQU     *
:212    +0
        +0
        +0
        #.PK
        +$80+:112-*-1
        FCC       "STEP"
:112    EQU     *
.PK     SET     :212
CYCLE     EQU     *
:213    +0
        +0
        +0
        #.PK
        +$80+:113-*-1
        FCC       "CYCLE"
:113    EQU     *
.PK     SET     :213
NEXT    EQU     *
:214    +0
        +0
        +0
        #.PK
        +$80+:114-*-1
        FCC       "NEXT"
:114    EQU     *
.PK     SET     :214
LET     EQU     *
:215    +0
        +0
        +0
        #.PK
        +$80+:115-*-1
        FCC       "LET"
:115    EQU     *
.PK     SET     :215
PRINT   EQU     *
:216    +0
        +0
        +0
        #.PK
        +$80+:116-*-1
        FCC       "PRINT"
:116    EQU     *
.PK     SET     :216
USING   EQU     *
:217    +0
        +0
        +0
        #.PK
        +$80+:117-*-1
        FCC       "USING"
:117    EQU     *
.PK     SET     :217
FORMAT   EQU     *
:218    +0
        +0
        +0
        #.PK
        +$80+:118-*-1
        FCC       "FORMAT"
:118    EQU     *
.PK     SET     :218
TAB       EQU     *
:219    +0
        +0
        +0
        #.PK
        +$80+:119-*-1
        FCC       "TAB"
:119    EQU     *
.PK     SET     :219
INPUT   EQU     *
:21A     +0
        +0
        +0
        #.PK
        +$80+:11A-*-1
        FCC       "INPUT"
:11A     EQU     *
.PK     SET     :21A
READ     EQU     *
:21B     +0
        +0
        +0
        #.PK
        +$80+:11B-*-1
        FCC       "READ"
:11B     EQU     *
.PK     SET     :21B
WRITE   EQU     *
:21C     +0
        +0
        +0
        #.PK
        +$80+:11C-*-1
        FCC       "WRITE"
:11C     EQU     *
.PK     SET     :21C
STOP    EQU     *
:21D    +0
        +0
        +0
        #.PK
        +$80+:11D-*-1
        FCC       "STOP"
:11D    EQU     *
.PK     SET     :21D
EXIT    EQU     *
:21E    +0
        +0
        +0
        #.PK
        +$80+:11E-*-1
        FCC       "EXIT"
:11E    EQU     *
.PK     SET     :21E
WHILE   EQU     *
:21F    +0
        +0
        +0
        #.PK
        +$80+:11F-*-1
        FCC       "WHILE"
:11F    EQU     *
.PK     SET     :21F
UNTIL   EQU     *
:220    +0
        +0
        +0
        #.PK
        +$80+:120-*-1
        FCC       "UNTIL"
:120    EQU     *
.PK     SET     :220
DO      EQU     *
:221    +0
        +0
        +0
        #.PK
        +$80+:121-*-1
        FCC       "DO"
:121    EQU     *
.PK     SET     :221
END     EQU     *
:222    +0
        +0
        +0
        #.PK
        +$80+:122-*-1
        FCC       "END"
:122    EQU     *
.PK     SET     :222
REPEAT   EQU     *
:223    +0
        +0
        +0
        #.PK
        +$80+:123-*-1
        FCC       "REPEAT"
:123    EQU     *
.PK     SET     :223
UNLESS  EQU     *
:224    +0
        +0
        +0
        #.PK
        +$80+:124-*-1
        FCC       "UNLESS"
:124    EQU     *
.PK     SET     :224
WHEN    EQU     *
:225    +0
        +0
        +0
        #.PK
        +$80+:125-*-1
        FCC       "WHEN"
:125    EQU     *
.PK     SET     :225
IF      EQU     *
:226    +0
        +0
        +0
        #.PK
        +$80+:126-*-1
        FCC       "IF"
:126    EQU     *
.PK     SET     :226
THEN    EQU     *
:227    +0
        +0
        +0
        #.PK
        +$80+:127-*-1
        FCC       "THEN"
:127    EQU     *
.PK     SET     :227
ELSE    EQU     *
:228    +0
        +0
        +0
        #.PK
        +$80+:128-*-1
        FCC       "ELSE"
:128    EQU     *
.PK     SET     :228
ELSEIF  EQU     *
:229    +0
        +0
        +0
        #.PK
        +$80+:129-*-1
        FCC       "ELSEIF"
:129    EQU     *
.PK     SET     :229
FI      EQU     *
:22A     +0
        +0
        +0
        #.PK
        +$80+:12A-*-1
        FCC       "FI"
:12A     EQU     *
.PK     SET     :22A
POSITION        EQU     *
:22B     +0
        +0
        +0
        #.PK
        +$80+:12B-*-1
        FCC       "POSITION"
:12B     EQU     *
.PK     SET     :22B
RESTORE EQU     *
:22C     +0
        +0
        +0
        #.PK
        +$80+:12C-*-1
        FCC       "RESTORE"
:12C     EQU     *
.PK     SET     :22C
OPEN    EQU     *
:22D    +0
        +0
        +0
        #.PK
        +$80+:12D-*-1
        FCC       "OPEN"
:12D    EQU     *
.PK     SET     :22D
CREATE    EQU     *
:22E    +0
        +0
        +0
        #.PK
        +$80+:12E-*-1
        FCC       "CREATE"
:12E    EQU     *
.PK     SET     :22E
CLOSE    EQU     *
:22F    +0
        +0
        +0
        #.PK
        +$80+:12F-*-1
        FCC       "CLOSE"
:12F    EQU     *
.PK     SET     :22F
DELETE  EQU     *
:230    +0
        +0
        +0
        #.PK
        +$80+:130-*-1
        FCC       "DELETE"
:130    EQU     *
.PK     SET     :230
RENAME   EQU     *
:231    +0
        +0
        +0
        #.PK
        +$80+:131-*-1
        FCC       "RENAME"
:131    EQU     *
.PK     SET     :231
CHAIN     EQU     *
:232    +0
        +0
        +0
        #.PK
        +$80+:132-*-1
        FCC       "CHAIN"
:132    EQU     *
.PK     SET     :232
CALL      EQU     *
:233    +0
        +0
        +0
        #.PK
        +$80+:133-*-1
        FCC       "CALL"
:133    EQU     *
.PK     SET     :233
POKE    EQU     *
:234    +0
        +0
        +0
        #.PK
        +$80+:134-*-1
        FCC       "POKE"
:134    EQU     *
.PK     SET     :234
DEBUG    EQU     *
:235    +0
        +0
        +0
        #.PK
        +$80+:135-*-1
        FCC       "DEBUG"
:135    EQU     *
.PK     SET     :235
SYSCALL   EQU     *
:236    +0
        +0
        +0
        #.PK
        +$80+:136-*-1
        FCC       "SYSCALL"
:136    EQU     *
.PK     SET     :236
SUBROUTINE       EQU     *
:237    +0
        +0
        +0
        #.PK
        +$80+:137-*-1
        FCC       "SUBROUTINE"
:137    EQU     *
.PK     SET     :237
DEF     EQU     *
:238    +0
        +0
        +0
        #.PK
        +$80+:138-*-1
        FCC       "DEF"
:138    EQU     *
.PK     SET     :238
EXTERNAL EQU     *
:239    +0
        +0
        +0
        #.PK
        +$80+:139-*-1
        FCC       "EXTERNAL"
:139    EQU     *
.PK     SET     :239
LEN     EQU     *
:23A     +0
        +0
        +0
        #.PK
        +$80+:13A-*-1
        FCC       "LEN"
:13A     EQU     *
.PK     SET     :23A
MAXLEN   EQU     *
:23B     +0
        +0
        +0
        #.PK
        +$80+:13B-*-1
        FCC       "MAXLEN"
:13B     EQU     *
.PK     SET     :23B
LEFT$   EQU     *
:23C     +0
        +0
        +0
        #.PK
        +$80+:13C-*-1
        FCC       "LEFT$"
:13C     EQU     *
.PK     SET     :23C
MID$    EQU     *
:23D    +0
        +0
        +0
        #.PK
        +$80+:13D-*-1
        FCC       "MID$"
:13D    EQU     *
.PK     SET     :23D
RIGHT$  EQU     *
:23E    +0
        +0
        +0
        #.PK
        +$80+:13E-*-1
        FCC       "RIGHT$"
:13E    EQU     *
.PK     SET     :23E
ASC       EQU     *
:23F    +0
        +0
        +0
        #.PK
        +$80+:13F-*-1
        FCC       "ASC"
:13F    EQU     *
.PK     SET     :23F
CHR$     EQU     *
:240    +0
        +0
        +0
        #.PK
        +$80+:140-*-1
        FCC       "CHR$"
:140    EQU     *
.PK     SET     :240
COM      EQU     *
:241    +0
        +0
        +0
        #.PK
        +$80+:141-*-1
        FCC       "COM"
:141    EQU     *
.PK     SET     :241
ATN      EQU     *
:242    +0
        +0
        +0
        #.PK
        +$80+:142-*-1
        FCC       "ATN"
:142    EQU     *
.PK     SET     :242
SIN     EQU     *
:243    +0
        +0
        +0
        #.PK
        +$80+:143-*-1
        FCC       "SIN"
:143    EQU     *
.PK     SET     :243
COS      EQU     *
:244    +0
        +0
        +0
        #.PK
        +$80+:144-*-1
        FCC       "COS"
:144    EQU     *
.PK     SET     :244
TAN      EQU     *
:245    +0
        +0
        +0
        #.PK
        +$80+:145-*-1
        FCC       "TAN"
:145    EQU     *
.PK     SET     :245
LOG     EQU     *
:246    +0
        +0
        +0
        #.PK
        +$80+:146-*-1
        FCC       "LOG"
:146    EQU     *
.PK     SET     :246
EXP     EQU     *
:247    +0
        +0
        +0
        #.PK
        +$80+:147-*-1
        FCC       "EXP"
:147    EQU     *
.PK     SET     :247
SQR     EQU     *
:248    +0
        +0
        +0
        #.PK
        +$80+:148-*-1
        FCC       "SQR"
:148    EQU     *
.PK     SET     :248
INT     EQU     *
:249    +0
        +0
        +0
        #.PK
        +$80+:149-*-1
        FCC       "INT"
:149    EQU     *
.PK     SET     :249
ABS       EQU     *
:24A     +0
        +0
        +0
        #.PK
        +$80+:14A-*-1
        FCC       "ABS"
:14A     EQU     *
.PK     SET     :24A
SGN     EQU     *
:24B     +0
        +0
        +0
        #.PK
        +$80+:14B-*-1
        FCC       "SGN"
:14B     EQU     *
.PK     SET     :24B
COL      EQU     *
:24C     +0
        +0
        +0
        #.PK
        +$80+:14C-*-1
        FCC       "COL"
:14C     EQU     *
.PK     SET     :24C
VAL      EQU     *
:24D    +0
        +0
        +0
        #.PK
        +$80+:14D-*-1
        FCC       "VAL"
:14D    EQU     *
.PK     SET     :24D
PEEK    EQU     *
:24E    +0
        +0
        +0
        #.PK
        +$80+:14E-*-1
        FCC       "PEEK"
:14E    EQU     *
.PK     SET     :24E
FIND    EQU     *
:24F    +0
        +0
        +0
        #.PK
        +$80+:14F-*-1
        FCC       "FIND"
:14F    EQU     *
.PK     SET     :24F
RND     EQU     *
:250    +0
        +0
        +0
        #.PK
        +$80+:150-*-1
        FCC       "RND"
:150    EQU     *
.PK     SET     :250
ERR     EQU     *
:251    +0
        +0
        +0
        #.PK
        +$80+:151-*-1
        FCC       "ERR"
:151    EQU     *
.PK     SET     :251
PI      EQU     *
:252    +0
        +0
        +0
        #.PK
        +$80+:152-*-1
        FCC       "PI"
:152    EQU     *
.PK     SET     :252
AND      EQU     *
:253    +0
        +0
        +0
        #.PK
        +$80+:153-*-1
        FCC       "AND"
:153    EQU     *
.PK     SET     :253
OR      EQU     *
:254    +0
        +0
        +0
        #.PK
        +$80+:154-*-1
        FCC       "OR"
:154    EQU     *
.PK     SET     :254
NOT     EQU     *
:255    +0
        +0
        +0
        #.PK
        +$80+:155-*-1
        FCC       "NOT"
:155    EQU     *
.PK     SET     :255
EOF     EQU     *
:256    +0
        +0
        +0
        #.PK
        +$80+:156-*-1
        FCC       "EOF"
:156    EQU     *
.PK     SET     :256
ROWS    EQU     *
:257    +0
        +0
        +0
        #.PK
        +$80+:157-*-1
        FCC       "ROWS"
:157    EQU     *
.PK     SET     :257
COLUMNS  EQU     *
:258    +0
        +0
        +0
        #.PK
        +$80+:158-*-1
        FCC       "COLUMNS"
:158    EQU     *
.PK     SET     :258
TRUE    EQU     *
:259    +0
        +0
        +0
        #.PK
        +$80+:159-*-1
        FCC       "TRUE"
:159    EQU     *
.PK     SET     :259
FALSE    EQU     *
:25A     +0
        +0
        +0
        #.PK
        +$80+:15A-*-1
        FCC       "FALSE"
:15A     EQU     *
.PK     SET     :25A
XOR     EQU     *
:25B     +0
        +0
        +0
        #.PK
        +$80+:15B-*-1
        FCC       "XOR"
:15B     EQU     *
.PK     SET     :25B
CAT       EQU     *
:25C     +0
        +0
        +0
        #.PK
        +$80+:15C-*-1
        FCC       "CAT"
:15C     EQU     *
.PK     SET     :25C
DATE$    EQU     *
:25D    +0
        +0
        +0
        #.PK
        +$80+:15D-*-1
        FCC       "DATE$"
:15D    EQU     *
.PK     SET     :25D
TIME$   EQU     *
:25E    +0
        +0
        +0
        #.PK
        +$80+:15E-*-1
        FCC       "TIME$"
:15E    EQU     *
.PK     SET     :25E
COPYRIGHT$       EQU     *
:25F    +0
        +0
        +0
        #.PK
        +$80+:15F-*-1
        FCC       "COPYRIGHT$"
:15F    EQU     *
.PK     SET     :25F
NUM$    EQU     *
:260    +0
        +0
        +0
        #.PK
        +$80+:160-*-1
        FCC       "NUM$"
:160    EQU     *
.PK     SET     :260
NUMF$   EQU     *
:261    +0
        +0
        +0
        #.PK
        +$80+:161-*-1
        FCC       "NUMF$"
:161    EQU     *
.PK     SET     :261
HEX$    EQU     *
:262    +0
        +0
        +0
        #.PK
        +$80+:162-*-1
        FCC       "HEX$"
:162    EQU     *
.PK     SET     :262
UPPERCASE$        EQU     *
:263    +0
        +0
        +0
        #.PK
        +$80+:163-*-1
        FCC       "UPPERCASE$"
:163    EQU     *
.PK     SET     :263
LOWERCASE$        EQU     *
:264    +0
        +0
        +0
        #.PK
        +$80+:164-*-1
        FCC       "LOWERCASE$"
:164    EQU     *
.PK     SET     :264
.KEYWORDLIST    EQU     .PK
.VC      SET     0
COMPILEPHASE      EQU     .VC
.VC      SET     .VC+1
.VV     SET     0
COMPILINGPGMORIGIN       EQU     .VV
.VV     SET     .VV+1
COMPILINGCATSIZE   EQU     .VV
.VV     SET     .VV+1
COMPILINGDATAORIGIN        EQU     .VV
.VV     SET     .VV+1
COMPILINGCOMMON   EQU     .VV
.VV     SET     .VV+1
COMPILINGDIMS    EQU     .VV
.VV     SET     .VV+1
COMPILINGSTMTS   EQU     .VV
.VV     SET     .VV+1
COMPILEDONE      EQU     .VV
.VV     SET     .VV+1
RESULT  EQU     .VC
.VC      SET     .VC+1
.VV     SET     0
NUMORBOOLTYPE    EQU     .VV
.VV     SET     .VV+1
NUMERICTYPE      EQU     .VV
.VV     SET     .VV+1
BOOLTYPE EQU     .VV
.VV     SET     .VV+1
NUMBOOLORSTRTYPE EQU     .VV
.VV     SET     .VV+1
NUMORSTRTYPE    EQU     .VV
.VV     SET     .VV+1
STRINGTYPE      EQU     .VV
.VV     SET     .VV+1
ADDRESSANYTYPE    EQU     .VV
.VV     SET     .VV+1
ADDRESSSCALARTYPE   EQU     .VV
.VV     SET     .VV+1
ADDRESSARRAYTYPE   EQU     .VV
.VV     SET     .VV+1
ERRORRULE       EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     ERRORRULE*2+.TA
        #.
        ORG     .
*       ERROR HANDLER
*       SCAN UNTIL <CR> OR '\ IS SEEN
*
        IF      M6800!M6801
SUCCESS   EQU     $0D
FAILURE  EQU     $0C
        ELSE    (M6809)
SUCCESS   EQU     $1A01    SET CARRY
FAILURE  EQU     $1CFE    RESET CARRY
        FIN

ERRORTOKEN
        JSR     GETTOKENCHAR      GO GET A CHARACTER
        PSHA
        JSR     EATTOKEN YUM!
        PULA
        CMPA      #'\     CHECK FOR END OF STATEMENT
        BEQ      ERRORSKIPPED    B/ THAT'S ENOUGH...
        CMPA      #$0D    END OF LINE MARK ?
        BNE      ERRORTOKEN      B/ NO, GO EAT SOME MORE
ERRORSKIPPED    JSR     GETTOKENCHAR      NOW EAT UP ALL EMPTY LINES
        CMPA      #$D     ANOTHER END OF LINE ?
        BNE      ERRORSKIPPED1   B/ NO, MIGHT BE TIME TO LEAVE
ERRORSKIPPED2   JSR     EATTOKEN YES, SWALLOW IT WHOLE...
        BRA       ERRORSKIPPED    AND LOOP
ERRORSKIPPED1   CMPA      #$C      ALLOW FORM FEED HERE, TOO...
        BEQ      ERRORSKIPPED2   B/ GO EAT
        JSR     BACKUP     OOPS, WENT TOO FAR...
ERRORDONE       FAILURE          (HE SEZ, WITH A SMIRK ON HIS FACE...)
        RTS
DEBLANK   EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     DEBLANK*2+.TA
        #.
        ORG     .
*       DEBLANK THE INPUT
*
DEBLANKL  JSR     EATTOKEN MAKE THE BLANK DISAPPEAR
DEBLANKTOKEN      JSR     GETTOKENCHAR      GET NEXT INPUT BYTE
        CMPA      #'      A BLANK ?
        BEQ      DEBLANKL  B/ YES, "GULP!"
        CMPA      #$9     TAB ?
        BEQ      DEBLANKL  B/ YES, EAT THESE, TOO!
        CMPA      #'.     LINE CONTINUATION MARK?
        BNE      DEBLANK1  B/ NOPE, END OF BLANKS ENCOUNTERED
        JSR     GETTOKENCHAR      LOOKS LIKE START, SCAN A LITTLE FURTHER
        CMPA      #'.     ...?
        BNE      DEBLANK2  B/ NOT CONTINUATION
        JSR     GETTOKENCHAR      ...?
        CMPA      #'.     ...?
        BNE      DEBLANK3  B/ NOT CONTINUATION
        JSR     GETTOKENCHAR      ...?
        CMPA      #$D     "...<CR>..."?
        BNE      DEBLANK4
        JSR     GETTOKENCHAR      ...?
        CMPA      #'&     COMPLETE "..."<0D>"&" SEQUENCE ?
        BEQ      DEBLANKL  B/ YES, SWALLOW IT!
DEBLANK4  JSR     BACKUP
DEBLANK3  JSR     BACKUP
DEBLANK2  JSR     BACKUP
DEBLANK1  JSR     BACKUP
        FAILURE          ANOTHER WHITE LIE...
        RTS
KEYWORD EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     KEYWORD*2+.TA
        #.
        ORG     .
*       TEST FOR KEYWORD = SID OR NID
*       'A'-'Z' $( 'A'-'Z' / '0'-'9' ) ( '$'  / .EMPTY );
*
KEYWORDTOKEN    ; TELL METAMACHINE WHERE KEYWORD RULE IS
        JSR     GETTOKENCHAR      GET ME AN INPUT CHARACTER
        BSR      ISLETTER        CHECK FIRST CHARACTER
        BCC        NIDFAIL  B/ ISN'T AN ID
KEYLOOP JSR     GETTOKENCHAR      APPARANTLY AN ID, FINISH SCAN
        BSR      ISLETTER
        BCS       KEYLOOP B/ YES, KEEP GOING
        BSR      ISDIGIT
        BCS       KEYLOOP B/ YES, KEEP GOING
        CMPA      #'$     DOES IT APPEAR TO BE A STRING ID?
        BEQ      KEYOK   B/ YEP, MUST BE A VALID NUMERIC ID
        JSR     BACKUP     DON'T COUNT THE LAST CHARACTER
KEYOK   SUCCESS           ALL IS WELL
        RTS
NID     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     NID*2+.TA
        #.
        ORG     .
*       TEST FOR NUMERIC IDENTIFIER
*       'A'-'Z' $( 'A'-'Z' / '0'-'9' ) #'$' ;
*
        JSR     GETTOKENCHAR      GET ME AN INPUT CHARACTER
        BSR      ISLETTER        CHECK FIRST CHARACTER
        BCC        NIDFAIL  B/ ISN'T AN ID
NIDLOOP JSR     GETTOKENCHAR      APPARANTLY AN ID, FINISH SCAN
        BSR      ISLETTER
        BCS       NIDLOOP B/ YES, KEEP GOING
        BSR      ISDIGIT
        BCS       NIDLOOP B/ YES, KEEP GOING
        CMPA      #'$     DOES IT APPEAR TO BE A STRING ID?
        BNE      NIDOK   B/ NOPE, MUST BE A VALID NUMERIC ID
NIDFAIL  FAILURE          DON'T RECOGNIZE STRING IDS IN NUMERIC CONTEXT
        RTS
NIDOK   JSR     BACKUP     DON'T COUNT THE LAST CHARACTER
        SUCCESS           ALL IS WELL
        RTS
*
*       CHARACTER CHECK ROUTINES
*
ISLETTER        CMPA      #'A      IS IT A LETTER ?
        BCS       ISNTLETTER
        CMPA      #'Z+1
        BCS       ISLETTERRTS
        CMPA      #'A+$20  'A
        BCS       ISNTLETTER
        CMPA      #'Z+$21 'Z+1
ISLETTERRTS     RTS
ISNTLETTER      CLC
        RTS
SID     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     SID*2+.TA
        #.
        ORG     .
*       STRING ID TEST
*       'A'-'Z' $( 'A'-'Z' / '0'-'9' ) '$' ;
        JSR     GETTOKENCHAR      GET FIRST CHARACTER
        BSR      ISLETTER
        BCC        SIDFAIL  B/ ISN'T STRING ID
SIDLOOP JSR     GETTOKENCHAR      APPEARS TO BE STRING ID, CONTINUE SCAN
        BSR      ISLETTER
        BCS       SIDLOOP
        BSR      ISDIGIT
        BCS       SIDLOOP
        CMPA      #'$     TRAILING '$' PRESENT ?
        BNE      SIDFAIL  B/ NOT A STRING ID
        SUCCESS           IS A STRING ID!
        RTS
SIDFAIL  FAILURE  
        RTS
*
*       ISHEX -- CHECK (A) TO SEE IF A HEX DIGIT
*       RETURNS CARRY SET IF YES
*
ISHEX   CMPA      #'A      IS IT A HEX DIGIT ?
        BCS       ISDIGIT B/ NOT THE LETTER TYPE
        CMPA      #'F+1
        BCS       ISDIGITRTS
        CMPA      #'A+$20  'A
        BCS       ISDIGITFAIL
        CMPA      #'F+$21 'F+1
        RTS
*
*       ISDIGIT -- CHECK (A) TO SEE IF A DIGIT
*       RETURNS CARRY SET IF YES
ISDIGIT CMPA      #'0
        BCS       ISDIGITFAIL
        CMPA      #'9+1
ISDIGITRTS      RTS
ISDIGITFAIL      CLC
        RTS
ONEDIGITNUM     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     ONEDIGITNUM*2+.TA
        #.
        ORG     .
*       TEST FOR SINGLE DIGIT NUMBER
*       '0'-'9' #'0'-'9' #'.' #'E' #'E' ;
        BSR      COLLECTINTEGER    GET THE VALUE OF AN INTEGER
        TSTA             VALUE MUST BE 0-9 ONLY!
        BNE      NUMBERFAIL        B/ MORE THAN 255!
        CMPB      #9+1
        BCC        NUMBERFAIL        B/ MORE THAN 9!
VERIFYNOTFLOATING        JSR     GETTOKENCHAR      GET FOLLOWING CHARACTER
        CMPA      #'.     DECIMAL POINT ?
        BEQ      NUMBERFAIL        B/ YES, MUST BE FLOATING POINT
        CMPA      #'E     EXPONENT PREFIX ?
        BEQ      NUMBERFAIL        B/ YES, MUST BE FLOATING POINT
        CMPA      #'E+$20 'E...?
        BEQ      NUMBERFAIL        B/ YES, MUST BE FLOATING POINT
        JSR     BACKUP     REJECT LAST CHARACTER
        SUCCESS   
        RTS
NUMBERFAIL        FAILURE  
        RTS
TENTO255        EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     TENTO255*2+.TA
        #.
        ORG     .
*       TEST FOR VALUE IN RANGE OF 10 TO 255
*       THE BNF FOR THIS IS A MESS, SO I'LL SKIP IT...
        BSR      COLLECTINTEGER
        TSTA             CHECK FOR < 256...
        BNE      NUMBERFAIL        B/ TOO BIG, REJECT IT
        SUBB      #10     MAKE SURE VALUE IS >= 10...
        SBCA       #0
        BCC        VERIFYNOTFLOATING        B/ SEEMS TO BE PROPER SIZE
        FAILURE          NO LUCK, QUIT NOW!
        RTS
NUM     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     NUM*2+.TA
        #.
        ORG     .
*       TEST FOR VALUE IN RANGE 0-65535
*       AGAIN, THE BNF IS A MESS, SO YOU FIGURE IT OUT!
*
        BSR      COLLECTINTEGER
        BRA       VERIFYNOTFLOATING
*
*       COLLECTINTEGER -- COLLECTS INTEGER VALUE NOT EXCEEDING 65535
*       TAKES FAILURE IF NOT AN INTEGER OR > 65535
*
COLLECTINTEGER    LDX     #0      INITIAL VALUE
        STX     INTEGER
        JSR     GETTOKENCHAR
        BSR      ISDIGIT
        BCC        COLLECTINTEGERFAIL B/ NOT AN INTEGER
COLLECTINTEGERLOOP        EQU     *
        SUBA      #'0     CONVERT INTEGER DIGIT TO BINARY
        PSHA
        LDAA      INTEGER MULTIPLY OLD VALUE BY 10
        LDAB      INTEGER+1
        ASLB
        ROLA             *2
        BCS       COLLECTINTEGERFAIL1        B/ RESULT > 65535
        ASLB
        ROLA             *4
        BCS       COLLECTINTEGERFAIL1        B/ RESULT > 65535
        ADDB      INTEGER+1
        ADCA       INTEGER *5
        BCS       COLLECTINTEGERFAIL1        B/ RESULT > 65535
        ASLB
        ROLA             *10
        BCS       COLLECTINTEGERFAIL1        B/ RESULT > 65535
        TSX
        ADDB      0,X     ADD IN THE NEW DIGIT
        ADCA       #0
        BCS       COLLECTINTEGERFAIL1        B/ RESULT > 65535
        INS             POP THE DIGIT OFF THE STACK
        STAA      INTEGER
        STAB      INTEGER+1
        JSR     GETTOKENCHAR      GRAB THE NEXT INPUT STREAM CHARACTER
        BSR      ISDIGIT MEANT FOR US ?
        BCS       COLLECTINTEGERLOOP        B/ YEP, GO PROCESS
        JSR     BACKUP     NO, SO REJECT IT
        LDAA      INTEGER GET THE VALUE
        LDAB      INTEGER+1
        RTS

COLLECTINTEGERFAIL1        INS             POP DIGIT FROM STACK
COLLECTINTEGERFAIL INS             POP RETURN ADDRESS FROM STACK
        INS
        FAILURE  
        RTS
HEXNUM  EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     HEXNUM*2+.TA
        #.
        ORG     .
*       HEXADECIMAL NUMBER TEST
*
        JSR     GETTOKENCHAR              GET 1ST CHARACTER
        JSR     ISHEX
        BCC        HEXNUMFAIL       B/ NOT HEX
        JSR     GETTOKENCHAR
        JSR     ISHEX
        BCC        HEXNUMDONE
        JSR     GETTOKENCHAR
        JSR     ISHEX
        BCC        HEXNUMDONE
        JSR     GETTOKENCHAR
        JSR     ISHEX
        BCC        HEXNUMDONE
        SUCCESS           GOT 4 DIGITS, THAT'S ENOUGH...
        RTS

HEXNUMDONE      JSR     BACKUP     REJECT NON-HEX CHARACTER
        SUCCESS   
        RTS
HEXNUMFAIL       FAILURE  
        RTS
STRINGGUTS      EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     STRINGGUTS*2+.TA
        #.
        ORG     .
*       STRING GUTS
*       $( #'"' #:D #"'" :0-:7F );
STRINGGUTSL     JSR     GETTOKENCHAR
        CMPA      #''     SINGLE QUOTE ?
        BEQ      STRINGGUTSOK
        CMPA      #'"     DOUBLE QUOTE ?
        BEQ      STRINGGUTSOK
        CMPA      #$D
        BNE      STRINGGUTSL
STRINGGUTSOK    JSR     BACKUP
        SUCCESS
        RTS
STRING  EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     STRING*2+.TA
        #.
        ORG     .
*       STRING TEST
*       "'" $( #"'" :0-:7F ) "'" / '"' $( #'"' :0-:7F ) '"' ;
        JSR     GETTOKENCHAR
        CMPA      #''     SINGLE QUOTE ?
        BEQ      STRING1 B/ YES, LOOKS LIKE STRING
        CMPA      #'"     DOUBLE QUOTE ?
        BNE      STRINGFAIL       B/ NO
STRING1
        STAA      STRINGQUOTE
STRINGLOOP
        JSR     GETSTRINGBODYCHAR
        BNE      STRINGLOOP
STRINGEXIT      ; CHECK FOR STRING TERMINATED PROPERLY
        CMPA      STRINGQUOTE
        BNE      STRINGFAIL
        SUCCESS
        RTS
STRINGFAIL       FAILURE  
        RTS

GETSTRINGBODYCHAR
        JSR     GETTOKENCHAR      GET A CHARACTER FROM THE STRING BODY
        CMPA      #$D     MALFORMED STRING ?
        BEQ      GETSTRINGBODYCHARXIT       B/ YES
        CMPA      STRINGQUOTE     END OF STRING ?
GETSTRINGBODYCHARXIT       RTS
STRINGQUOTE     FCB       0
CHAR      EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     CHAR*2+.TA
        #.
        ORG     .
*       SINGLE CHARACTER STRING TEST
*
*       "'" #:D :0-:7F "'" / '"' #:D :0-:7F '"' ;
        JSR     GETTOKENCHAR
        CMPA      #''     SINGLE QUOTE ?
        BEQ      CHARSQ
        CMPA      #'"     DOUBLE QUOTE ?
        BEQ      CHARDQ
        FAILURE  
        RTS

CHARSQ    JSR     GETTOKENCHAR
        CMPA      #$D     CARRIAGE RETURN ?
        BEQ      CHARFAIL
        JSR     GETTOKENCHAR
        CMPA      #''
        BNE      CHARFAIL
        SUCCESS   
        RTS

CHARDQ    JSR     GETTOKENCHAR
        CMPA      #$D
        BEQ      CHARFAIL
        JSR     GETTOKENCHAR
        CMPA      #'"
        BNE      CHARFAIL
        SUCCESS   
        RTS
CHARFAIL   FAILURE  
        RTS
ANYCHAR    EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     ANYCHAR*2+.TA
        #.
        ORG     .
*       RECOGNIZES ANY NON-<CR> CHARACTER
        JSR     GETTOKENCHAR
        CMPA      #$D
        BEQ      CHARFAIL
        SUCCESS   
        RTS
LABELID   EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     LABELID*2+.TA
        #.
        ORG     .
*       TEST FOR PRESENCE OF LABEL
*
        JSR     GETTOKENCHAR
        JSR     ISLETTER
        BCC        LABELIDFAIL
LABELIDLOOP       JSR     GETTOKENCHAR
        JSR     ISLETTER
        BCS       LABELIDLOOP
        JSR     ISDIGIT
        BCS       LABELIDLOOP
        CMPA      #':
        BNE      LABELIDFAIL
        JSR     BACKUP     REJECT THE ':' SO WE ONLY GET ALPHANUM PART OF LABEL!
        SUCCESS   
        RTS

LABELIDFAIL        FAILURE  
        RTS
THENELSELABEL     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     THENELSELABEL*2+.TA
        #.
        ORG     .
*       TEST FOR PRESENCE OF LABEL IN "THEN..." OR "ELSE..." CONTEXT
*       I.E., LOOKS LIKE: NID $BLANK -'(' -'[' -'=' -'$' ;
        JSR     GETTOKENCHAR
        JSR     ISLETTER
        BCC        LABELIDFAIL
THENELSELOOP    JSR     GETTOKENCHAR
        JSR     ISLETTER
        BCS       THENELSELOOP
        JSR     ISDIGIT
        BCS       THENELSELOOP
        JSR     BACKUP
        BSR      SKIPBLANKS
        CMPA      #'=
        BEQ      LABELIDFAIL
        CMPA      #'(
        BEQ      LABELIDFAIL
        CMPA      #'[
        BEQ      LABELIDFAIL
        CMPA      #'$
        BEQ      LABELIDFAIL
        JSR     REJECTLOOKAHEAD
        SUCCESS   
        RTS
*
*       SKIPBLANKS -- SCAN OVER SPACE, TAB, '...' :D '&'
*       COUNT BYTES FOR BACKUP PURPOSES
*
SKIPBLANKS        CLR      LOOKAHEADCOUNT     = # BYTES SCANNED OVER
SKIPBLANKSL       JSR     GETLOOKAHEADCHARACTER
        CMPA      #' 
        BEQ      SKIPBLANKSL
        CMPA      #$9     TAB ?
        BEQ      SKIPBLANKSL
        CMPA      #'.     LINE CONTINUATION ?
        BNE      SKIPBLANKSRTS
        JSR     GETLOOKAHEADCHARACTER
        CMPA      #'.
        BNE      SKIPBLANKS2
        JSR     GETLOOKAHEADCHARACTER
        CMPA      #'.
        BNE      SKIPBLANKS3
        JSR     GETLOOKAHEADCHARACTER
        CMPA      #$D
        BNE      SKIPBLANKS4
        JSR     GETLOOKAHEADCHARACTER
        CMPA      #'&
        BEQ      SKIPBLANKSL
        JSR     BACKUP
        DEC      LOOKAHEADCOUNT
SKIPBLANKS4       JSR     BACKUP
        DEC      LOOKAHEADCOUNT
SKIPBLANKS3       JSR     BACKUP
        DEC      LOOKAHEADCOUNT
SKIPBLANKS2       JSR     BACKUP
        DEC      LOOKAHEADCOUNT
        LDAA      #'.
SKIPBLANKSRTS     RTS
FNUM    EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     FNUM*2+.TA
        #.
        ORG     .
*       FLOATING POINT NUMBER TEST
*       ( '-' / .EMPTY )
*       ( '0'-'9' $( '0'-'9' ) ( '.' / .EMPTY ) / '.' '0'-'9' )
*       ( 'E' ( '+' / '-' / .EMPTY ) '0'-'9' $( '0'-'9' ) / .EMPTY ) ;
*
        JSR     GETTOKENCHAR      GET 1ST BYTE OF FNUM
        CMPA      #'-     NEGATIVE SIGN ?
        BNE      FNUM1   B/ NOPE
        JSR     GETTOKENCHAR      ACCEPT "-", GET NEXT BYTE
FNUM1   BSR      FDIGITS1        LOOK FOR DIGIT STRING
        BCC        FNUMF   B/ NO LEAD DIGITS, MUST BE FRACTION SYNTAX
        CMPA      #'.     DECIMAL POINT?
        BNE      FNUM2   B/ NO DECIMAL POINT
        BSR      FDIGITS COLLECT FRACTIONAL PART
FNUM2   CMPA      #'E
        BEQ      FNUME   B/ LOOKS LIKE ONE COMING UP
        CMPA      #'E+$20 'E
        BNE      FNUMSUCCESS       NO EXPONET, GET OUT QUICK!
FNUME   JSR     GETTOKENCHAR      SEE IF DIGITS FOLLOW EXPONENT "E"
        CMPA      #'+     CHECK FOR SIGN
        BEQ      FNUMES  B/ SIGN IS INCLUDED
        CMPA      #'-     CHECK FOR - SIGN
        BEQ      FNUMES  B/ SIGN IS INCLUDED
        JSR     FDIGITS1        LOOK FOR DIGIT STRING
FNUMSUCCESS       JSR     BACKUP     'E' NOT PART OF NUMBER
        SUCCESS   
        RTS

FNUMES  JSR     FDIGITS EXPONENT HAS SIGN, LOOK FOR DIGITS
        BCS       FNUMSUCCESS       B/ GOOD EXPONENT FOUND
        JSR     BACKUP     "E+<X>" IS APPARANTLY NOT AN EXPONENT
        JSR     BACKUP
        JSR     BACKUP
        SUCCESS           BUT THE REST OF THE NUMBER LOOKS OK
        RTS

FNUMF   CMPA      #'.     MIGHT BE FRACTION ONLY F.P. NUMBER
        BNE      FNUMFAIL B/ NO SUCH LUCK.
        JSR     FDIGITS MUST HAVE DIGITS IN FRACTION PART
        BCS       FNUM2   B/ GOT DIGITS, LOOKS OK
FNUMFAIL FAILURE          NOT AN F.P. NUMBER
        RTS

FDIGITS JSR     GETTOKENCHAR      GET FIRST CHARACTER OF DIGIT STRING
FDIGITS1        JSR     ISDIGIT CHECK FOR DIGIT
        BCC        FDIGITFAIL       B/ NOT A DIGIT STRING
FDIGITSL        JSR     GETTOKENCHAR      GET NEXT CHARACTER OF DIGIT STRING
        JSR     ISDIGIT IS THIS A DIGIT TOO?
        BCS       FDIGITSL        B/ YES, GO COLLECT SOME MORE
        SEC              SAY "FOUND DIGIT STRING"
        RTS

FDIGITFAIL       CLC       SAY "DIDN'T FIND DIGIT STRING"
        RTS
LOOKAHEADELSE     EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     LOOKAHEADELSE*2+.TA
        #.
        ORG     .
*       LOOK AHEAD FOR ELSE CLAUSE
*       = :0D $BLANKS 'E' 'L' 'S' 'E' ;
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #$D
        BNE      LOOKAHEADELSEFAIL
        JSR     SKIPBLANKS
        CMPA      #'E
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'L
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'S
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'E
        BNE      LOOKAHEADELSEFAIL
LOOKAHEADELSETAILCHECK
        BSR      GETLOOKAHEADCHARACTER
        JSR     ISLETTER
        BCS       LOOKAHEADELSEFAIL
        JSR     ISDIGIT
        BCS       LOOKAHEADELSEFAIL
        CMPA      #'$
        BEQ      LOOKAHEADELSEFAIL
        BSR      REJECTLOOKAHEAD
        SUCCESS   
        RTS
*
*       REJECTLOOKAHEAD -- BACKUP OVER UNWANTED STUFF!
*
REJECTLOOKAHEAD    EQU     *       REJECT ALL BUT THE <CR>
        JSR     BACKUP
        DEC      LOOKAHEADCOUNT
        BNE      REJECTLOOKAHEAD
        RTS

LOOKAHEADELSEFAIL  FAILURE  
        RTS

GETLOOKAHEADCHARACTER INC      LOOKAHEADCOUNT
        JSR     GETTOKENCHAR
        CMPA      #'A+32   'A
        BCS       GETLOOKAHEADRTS   B/ DON'T HAVE TO UPPERCASE THE CHARACTER
        CMPA      #'Z+32+1        'Z+1
        BCC        GETLOOKAHEADRTS
        SUBA      #32     FORCE LOWERCASE LETTER TO UPPERCASE
GETLOOKAHEADRTS
        RTS

LOOKAHEADCOUNT     FCB       0       = # CHARACTERS SCANNED
LOOKAHEADELSEIF   EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     LOOKAHEADELSEIF*2+.TA
        #.
        ORG     .
*       LOOK AHEAD FOR ELSEIF CLAUSE
*       = :0D $BLANKS 'E' 'L' 'S' 'E' 'I' 'F';
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #$D
        BNE      LOOKAHEADELSEFAIL
        JSR     SKIPBLANKS
        CMPA      #'E
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'L
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'S
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'E
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'I
        BNE      LOOKAHEADELSEFAIL
        BSR      GETLOOKAHEADCHARACTER
        CMPA      #'F
        BEQ      LOOKAHEADELSETAILCHECK
        BRA       LOOKAHEADELSEFAIL
ZEROS   EQU     .TC
.TC      SET     .TC+1
.       SET     *
        ORG     ZEROS*2+.TA
        #.
        ORG     .
*       EAT UP LEADING ZEROS ON LINE NUMBERS
*       = '0' $'0'

        JSR     GETTOKENCHAR
        CMPA      #'0
        BNE      ZEROSFAIL
ZEROSL
        JSR     GETTOKENCHAR
        CMPA      #'0
        BEQ      ZEROSL
        JSR     BACKUP     REJECT NON-ZERO CHARACTER
        SUCCESS   
        RTS
ZEROSFAIL        FAILURE  
        RTS
LB       EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     LB*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '[+$80
        $2A
        $21
        FCC       ''
        '(+$80
        $29
RB       EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     RB*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        ']+$80
        $2A
        $21
        FCC       ''
        ')+$80
        $29
.       SET     *
        ORG     BASIC*2+.SA
        #.
        ORG     .
OPTENDOFLINE    EQU     .SC
.SC      SET     .SC+1
        OPTENDOFLINE
        ((:165-*)&$3FFF)+$8000
        $30
        FCC       '*      Compiled by 4/13/82 Software Dynamics BASIC Version 1.4'
        'i+$80
        $2F
        $30
        FCC       '.CATBUFSIZE       SET     $10'
        '0+$80
        $2F
        $30
        FCC       '.      SET     '
        '0+$80
        $2F
        $30
        FCC       '.NSV   SET     '
        '0+$80
        $2F
        $30
        FCC       '::     SET     '
        '0+$80
        $2F
ORIGIN  EQU     .SC
.SC      SET     .SC+1
        ORIGIN
        $30
        FCC       '$2E0'
        '0+$80
        $2F
        $3A,COMPILEPHASE,COMPILINGPGMORIGIN
        $35
:166    EQU     *
        $3B,COMPILEPHASE,COMPILINGPGMORIGIN
        ((:167-*)&$3FFF)+$8000
REMARKSTMT       EQU     .SC
.SC      SET     .SC+1
        REMARKSTMT
        ((:168-*)&$3FFF)+$4000
INCLUDESTMT      EQU     .SC
.SC      SET     .SC+1
        INCLUDESTMT
        ((:168-*)&$3FFF)+$4000
PGMORIGINSTMT   EQU     .SC
.SC      SET     .SC+1
        PGMORIGINSTMT
        ((:168-*)&$3FFF)+$4000
        $34
        $3A,COMPILEPHASE,COMPILINGCATSIZE
:168    EQU     *
        $2C
:167    EQU     *
        ((:166-*)&$3FFF)+$4000
        $34
        $35
:169    EQU     *
        $3B,COMPILEPHASE,COMPILINGCATSIZE
        ((:16A-*)&$3FFF)+$8000
        REMARKSTMT
        ((:16B-*)&$3FFF)+$4000
        INCLUDESTMT
        ((:16B-*)&$3FFF)+$4000
CATSIZESTMT       EQU     .SC
.SC      SET     .SC+1
        CATSIZESTMT
        ((:16C-*)&$3FFF)+$8000
PROGHEADER       EQU     .SC
.SC      SET     .SC+1
        PROGHEADER
        $2C
:16C     EQU     *
        ((:16B-*)&$3FFF)+$4000
FUNCTIONCOMPILATION        EQU     .SC
.SC      SET     .SC+1
        FUNCTIONCOMPILATION
        ((:16B-*)&$3FFF)+$4000
SUBROUTINECOMPILATION      EQU     .SC
.SC      SET     .SC+1
        SUBROUTINECOMPILATION
        ((:16B-*)&$3FFF)+$4000
        $34
        PROGHEADER
        $2C
:16B     EQU     *
        $2C
:16A     EQU     *
        ((:169-*)&$3FFF)+$4000
        $34
        $35
:16D    EQU     *
        $3B,COMPILEPHASE,COMPILINGDATAORIGIN
        ((:16E-*)&$3FFF)+$8000
        REMARKSTMT
        ((:16F-*)&$3FFF)+$4000
        INCLUDESTMT
        ((:16F-*)&$3FFF)+$4000
DATAORIGINSTMT    EQU     .SC
.SC      SET     .SC+1
        DATAORIGINSTMT
        ((:16F-*)&$3FFF)+$4000
        $34
        $3A,COMPILEPHASE,COMPILINGCOMMON
:16F    EQU     *
        $2C
:16E    EQU     *
        ((:16D-*)&$3FFF)+$4000
        $34
        $35
:170    EQU     *
        $3B,COMPILEPHASE,COMPILINGCOMMON
        ((:171-*)&$3FFF)+$8000
BASICCOMMON EQU     .SC
.SC      SET     .SC+1
        BASICCOMMON
        $2C
:171    EQU     *
        ((:170-*)&$3FFF)+$4000
        $34
        $35
:172    EQU     *
        $3B,COMPILEPHASE,COMPILINGDIMS
        ((:173-*)&$3FFF)+$8000
BASICDIMS  EQU     .SC
.SC      SET     .SC+1
        BASICDIMS
        $2C
:173    EQU     *
        ((:172-*)&$3FFF)+$4000
        $34
        $35
:174    EQU     *
        $3B,COMPILEPHASE,COMPILINGSTMTS
        ((:175-*)&$3FFF)+$8000
BASICSTMTS EQU     .SC
.SC      SET     .SC+1
        BASICSTMTS
        $2C
:175    EQU     *
        ((:174-*)&$3FFF)+$4000
        $34
OPEXIT  EQU     .SC
.SC      SET     .SC+1
        OPEXIT
        $2F
        $30
        FCC       '.PROGEND       EQU     '
        '*+$80
        $2F
        $30
        FCC       '.FREFLBL        EQU     '
        '.+$80
        $2F
        $30
        FCC       '.TNSS  EQU     :'
        ':+$80
        $2F
        $30
        FCC       '.TNSV  EQU     .NS'
        'V+$80
        $2F
        $30
        FCC       '       IF      .TNSV>>64+25'
        '6+$80
        $2F
        $30
        FCC       '       ?Maximum of 320 Scalar variables has been exceeded'
        '?+$80
        $2F
        $30
        FCC       '       FI'
        'N+$80
        $2F
        $30
        FCC       '       IFUND   '
        ':+$80
        $2F
        $30
        FCC       ':      EQU     '
        '*+$80
        $2F
RES     EQU     .SC
.SC      SET     .SC+1
        RES
        $30
        FCC       '.TNSS+.TNSV*'
        '6+$80
        $2F
        $30
        FCC       '       FI'
        'N+$80
        $2F
        $30
        FCC       '       IF      .PROGEND>>:&.PROGEND<<:+.TNSS+.TNSV*'
        '6+$80
        $2F
        $30
        FCC       '       ? Data space overlaps end of code spac'
        'e+$80
        $2F
        $30
        FCC       '       FI'
        'N+$80
        $2F
        $30
        FCC       '       IF      .PROGSTART>>:&.PROGSTART<<:+.TNSS+.TNSV*'
        '6+$80
        $2F
        $30
        FCC       '       ? Data space overlaps beginning of code spac'
        'e+$80
        $2F
        $30
        FCC       '       FI'
        'N+$80
        $2F
        $30
        FCC       '       END     .PROGSTAR'
        'T+$80
        $2F
:165    EQU     *
        $29
.       SET     *
        ORG     PROGHEADER*2+.SA
        #.
        ORG     .
        $30
        FCC       '.PROGSTAR'
        'T+$80
EQUHERE EQU     .SC
.SC      SET     .SC+1
        EQUHERE
        $2F
        $30
        FCC       '       JSR     $10'
        '0+$80
        $2F
BYTE     EQU     .SC
.SC      SET     .SC+1
        BYTE
        $30
        FCC       '$1'
        '4+$80
        $2F
ADDR     EQU     .SC
.SC      SET     .SC+1
        ADDR
        $30
        FCC       '.FREFLB'
        'L+$80
        $2F
INTEGER EQU     .SC
.SC      SET     .SC+1
        INTEGER
        $30
        FCC       '.CATBUFSIZ'
        'E+$80
        $2F
DADDR    EQU     .SC
.SC      SET     .SC+1
        DADDR
        $30
        FCC       '.TNS'
        'S+$80
        $2F
        DADDR
        $30
        FCC       '.TNSS+.TNSV*'
        '6+$80
        $2F
        $3A,COMPILEPHASE,COMPILINGDATAORIGIN
        $29
.       SET     *
        ORG     PGMORIGINSTMT*2+.SA
        #.
        ORG     .
        $3E,#PROGRAM
        ((:176-*)&$3FFF)+$8000
        $21
        FCC       'ORIGI'
        'N+$80
        ((:177-*)&$3FFF)+$4000
        $21
        FCC       'origi'
        'n+$80
:177    EQU     *
        $2C
        $21
        FCC       ''
        ':+$80
        $2C
        +HEXNUM
        $2C
ENDOFLINEOPTCOMMENT      EQU     .SC
.SC      SET     .SC+1
        ENDOFLINEOPTCOMMENT
        $2C
        ORIGIN
        $30
        FCC       ''
        '$+$80
        $32,1
        $2F
        $3A,COMPILEPHASE,COMPILINGCATSIZE
:176    EQU     *
        $29
.       SET     *
        ORG     CATSIZESTMT*2+.SA
        #.
        ORG     .
        $3E,#CONCATENATION
        ((:178-*)&$3FFF)+$8000
        $21
        FCC       'BUFFE'
        'R+$80
        ((:179-*)&$3FFF)+$4000
        $21
        FCC       'buffe'
        'r+$80
:179    EQU     *
        $2C
        $21
        FCC       'SIZ'
        'E+$80
        ((:17A-*)&$3FFF)+$4000
        $21
        FCC       'siz'
        'e+$80
:17A     EQU     *
        $2C
        $21
        FCC       ''
        '=+$80
        $2C
        +NUM
        $2C
        ENDOFLINEOPTCOMMENT
        $2C
        $30
        FCC       '.CATBUFSIZE       SET'
        '       +$80
        $32,1
        $2F
:178    EQU     *
        $29
.       SET     *
        ORG     SUBROUTINECOMPILATION*2+.SA
        #.
        ORG     .
        $3E,#SUBROUTINE
        ((:17B-*)&$3FFF)+$8000
        +NID
        ((:17C-*)&$3FFF)+$4000
        +SID
:17C     EQU     *
        $2C
        $23,TYPESUBROUTINE
PARAMETERDEFINITIONS      EQU     .SC
.SC      SET     .SC+1
        PARAMETERDEFINITIONS
        $2C
EXTERNALSUBFUNHEADER       EQU     .SC
.SC      SET     .SC+1
        EXTERNALSUBFUNHEADER
        $38
        +:17D-*-1
        +1
:17D    EQU     *
        $2C
        ENDOFLINEOPTCOMMENT
        $2C
:17B     EQU     *
        $29
.       SET     *
        ORG     FUNCTIONCOMPILATION*2+.SA
        #.
        ORG     .
        $3E,#DEF
        ((:17E-*)&$3FFF)+$8000
        +NID
        ((:17F-*)&$3FFF)+$8000
        $23,FUNCTION
        PARAMETERDEFINITIONS
        $2C
        EXTERNALSUBFUNHEADER
        $38
        +:180-*-1
        +1
:180    EQU     *
        $2C
        $21
        FCC       ''
        '=+$80
        ((:181-*)&$3FFF)+$8000
NUMEXP  EQU     .SC
.SC      SET     .SC+1
        NUMEXP
        $2C
OPFUNRET        EQU     .SC
.SC      SET     .SC+1
        OPFUNRET
        $2F
        $3A,COMPILEPHASE,COMPILEDONE
:181    EQU     *
        ((:182-*)&$3FFF)+$4000
        $34
:182    EQU     *
        $2C
:17F    EQU     *
        ((:183-*)&$3FFF)+$4000
        +SID
        ((:184-*)&$3FFF)+$8000
        $23,FUNCTION
        PARAMETERDEFINITIONS
        $2C
        EXTERNALSUBFUNHEADER
        $38
        +:185-*-1
        +1
:185    EQU     *
        $2C
        $21
        FCC       ''
        '=+$80
        ((:186-*)&$3FFF)+$8000
STREXP  EQU     .SC
.SC      SET     .SC+1
        STREXP
        $2C
        OPFUNRET
        $2F
        $3A,COMPILEPHASE,COMPILEDONE
:186    EQU     *
        ((:187-*)&$3FFF)+$4000
        $34
:187    EQU     *
        $2C
:184    EQU     *
:183    EQU     *
        $2C
        ENDOFLINEOPTCOMMENT
        $2C
:17E    EQU     *
        $29
.       SET     *
        ORG     EXTERNALSUBFUNHEADER*2+.SA
        #.
        ORG     .
        $30
        FCC       '.PROGSTAR'
        'T+$80
        EQUHERE
        $2F
        $30
        FCC       '       JSR     $10'
        '6+$80
        $2F
        BYTE
        $30
        FCC       '$1'
        '4+$80
        $2F
        ADDR
        $30
        FCC       '.FREFLB'
        'L+$80
        $2F
        DADDR
        $30
        FCC       '::-'
        '6+$80
        $2F
        DADDR
        $30
        FCC       '.TNS'
        'S+$80
        $2F
        DADDR
        $30
        FCC       '.TNSS+6*.TNS'
        'V+$80
        $2F
        BYTE
        $30
        FCC       ':NARG'
        'S+$80
        $2F
        $3A,COMPILEPHASE,COMPILINGDATAORIGIN
        $29
.       SET     *
        ORG     DATAORIGINSTMT*2+.SA
        #.
        ORG     .
        $3E,#DATA
        ((:188-*)&$3FFF)+$8000
        $21
        FCC       'ORIGI'
        'N+$80
        $2C
        $21
        FCC       ''
        ':+$80
        $2C
        +HEXNUM
        $2C
        ENDOFLINEOPTCOMMENT
        $2C
        $30
        FCC       ':      EQU     '
        '$+$80
        $32,1
        $2F
        $3A,COMPILEPHASE,COMPILINGCOMMON
:188    EQU     *
        $29
.       SET     *
        ORG     BASICCOMMON*2+.SA
        #.
        ORG     .
COMMONSTMT       EQU     .SC
.SC      SET     .SC+1
        COMMONSTMT
        $2A
        REMARKSTMT
        $2A
        INCLUDESTMT
        $2A
DIMLABEL  EQU     .SC
.SC      SET     .SC+1
        DIMLABEL
        ((:189-*)&$3FFF)+$8000
        COMMONSTMT
        ((:18A-*)&$3FFF)+$4000
        REMARKSTMT
        ((:18A-*)&$3FFF)+$4000
DIMSTMT EQU     .SC
.SC      SET     .SC+1
        DIMSTMT
        ((:18A-*)&$3FFF)+$4000
ENDOFLINE       EQU     .SC
.SC      SET     .SC+1
        ENDOFLINE
        ((:18A-*)&$3FFF)+$4000
LABELLEDFIRSTEXECUTABLELINE  EQU     .SC
.SC      SET     .SC+1
        LABELLEDFIRSTEXECUTABLELINE
        $38
        +:18B-*-1
        +1
:18B     EQU     *
:18A     EQU     *
        $2C
:189    EQU     *
        $2A
        $34
        $3A,COMPILEPHASE,COMPILINGDIMS
        $29
.       SET     *
        ORG     BASICDIMS*2+.SA
        #.
        ORG     .
        DIMSTMT
        $2A
        REMARKSTMT
        $2A
        INCLUDESTMT
        $2A
        DIMLABEL
        ((:18C-*)&$3FFF)+$8000
        DIMSTMT
        ((:18D-*)&$3FFF)+$4000
        REMARKSTMT
        ((:18D-*)&$3FFF)+$4000
        ENDOFLINE
        ((:18D-*)&$3FFF)+$4000
        LABELLEDFIRSTEXECUTABLELINE
        $38
        +:18E-*-1
        +1
:18E    EQU     *
:18D    EQU     *
        $2C
:18C     EQU     *
        $2A
        $34
        $3A,COMPILEPHASE,COMPILINGSTMTS
CODEBODY  EQU     .SC
.SC      SET     .SC+1
        CODEBODY
        EQUHERE
        $2F
        $29
.       SET     *
        ORG     BASICSTMTS*2+.SA
        #.
        ORG     .
LINELABEL EQU     .SC
.SC      SET     .SC+1
        LINELABEL
        ((:18F-*)&$3FFF)+$8000
LABELLEDBLOCK       EQU     .SC
.SC      SET     .SC+1
        LABELLEDBLOCK
        $38
        +:190-*-1
        +1
:190    EQU     *
        ((:191-*)&$3FFF)+$8000
RESTOFCLAUSE      EQU     .SC
.SC      SET     .SC+1
        RESTOFCLAUSE
        $2C
        ENDOFLINE
        $2C
:191    EQU     *
        ((:192-*)&$3FFF)+$4000
LINEBODY EQU     .SC
.SC      SET     .SC+1
        LINEBODY
        ((:192-*)&$3FFF)+$4000
FORMATLINE       EQU     .SC
.SC      SET     .SC+1
        FORMATLINE
        ((:192-*)&$3FFF)+$4000
        ENDOFLINE
:192    EQU     *
        $2C
:18F    EQU     *
        $2A
        LINEBODY
        $2A
        INCLUDESTMT
        $29
.       SET     *
        ORG     REMARKSTMT*2+.SA
        #.
        ORG     .
REMHEAD  EQU     .SC
.SC      SET     .SC+1
        REMHEAD
        ((:193-*)&$3FFF)+$8000
REMARK   EQU     .SC
.SC      SET     .SC+1
        REMARK
        $2C
        ENDOFLINE
        $2C
:193    EQU     *
        $29
.       SET     *
        ORG     REMHEAD*2+.SA
        #.
        ORG     .
        $21
        FCC       'RE'
        'M+$80
        $2A
        $21
        FCC       'Re'
        'm+$80
        $2A
        $21
        FCC       're'
        'm+$80
        $2A
        $21
        FCC       ''
        '!+$80
        $29
.       SET     *
        ORG     ENDOFLINE*2+.SA
        #.
        ORG     .
        $21
        +$80+$D
        ((:194-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
:194    EQU     *
        $29
.       SET     *
        ORG     OPTENDOFLINE*2+.SA
        #.
        ORG     .
:195    EQU     *
        $21
        +$80+$D
        ((:196-*)&$3FFF)+$4000
        $21
        +$80+$C
:196    EQU     *
        ((:195-*)&$3FFF)+$4000
        $34
        $29
.       SET     *
        ORG     ENDOFLINEOPTCOMMENT*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '\+$80
        ((:197-*)&$3FFF)+$8000
        REMARKSTMT
        $2C
:197    EQU     *
        $2A
        ENDOFLINE
        $29
.       SET     *
        ORG     LINEBODY*2+.SA
        #.
        ORG     .
CLAUSE    EQU     .SC
.SC      SET     .SC+1
        CLAUSE
        ((:198-*)&$3FFF)+$8000
        ENDOFLINE
        $2C
:198    EQU     *
        $2A
FUNCTIONDEFN     EQU     .SC
.SC      SET     .SC+1
        FUNCTIONDEFN
        ((:199-*)&$3FFF)+$8000
        ENDOFLINEOPTCOMMENT
        $2C
:199    EQU     *
        $2A
SUBROUTINEDEFN   EQU     .SC
.SC      SET     .SC+1
        SUBROUTINEDEFN
        ((:19A-*)&$3FFF)+$8000
        ENDOFLINEOPTCOMMENT
        $2C
:19A     EQU     *
        $2A
ENDSTMT EQU     .SC
.SC      SET     .SC+1
        ENDSTMT
        ((:19B-*)&$3FFF)+$8000
        $21
        +$80+$D
        $2C
:19B     EQU     *
        $29
EXECUTABLELINE     EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     EXECUTABLELINE*2+.SA
        #.
        ORG     .
        CLAUSE
        ((:19C-*)&$3FFF)+$8000
        ENDOFLINE
        $2C
:19C     EQU     *
        $29
.       SET     *
        ORG     LABELLEDFIRSTEXECUTABLELINE*2+.SA
        #.
        ORG     .
        $3A,COMPILEPHASE,COMPILINGSTMTS
        CODEBODY
        EQUHERE
        $2F
        LABELLEDBLOCK
        $38
        +:19D-*-1
        +1
:19D    EQU     *
        ((:19E-*)&$3FFF)+$8000
        RESTOFCLAUSE
        $2C
        ENDOFLINE
        $2C
:19E    EQU     *
        ((:19F-*)&$3FFF)+$4000
        LINEBODY
        ((:19F-*)&$3FFF)+$4000
        FORMATLINE
:19F    EQU     *
        $2C
        $29
.       SET     *
        ORG     LABELLEDBLOCK*2+.SA
        #.
        ORG     .
        $3E,#IF
        ((:1A0-*)&$3FFF)+$8000
IFSTMT  EQU     .SC
.SC      SET     .SC+1
        IFSTMT
        $2C
:1A0     EQU     *
        ((:1A1-*)&$3FFF)+$4000
        $3E,#FOR
        ((:1A2-*)&$3FFF)+$8000
FORSTMT EQU     .SC
.SC      SET     .SC+1
        FORSTMT
        $2C
:1A2     EQU     *
        ((:1A1-*)&$3FFF)+$4000
        $3E,#REPEAT
        ((:1A3-*)&$3FFF)+$8000
REPEATSTMT       EQU     .SC
.SC      SET     .SC+1
        REPEATSTMT
        $2C
:1A3     EQU     *
        ((:1A1-*)&$3FFF)+$4000
        $3E,#WHILE
        ((:1A4-*)&$3FFF)+$8000
WHILESTMT       EQU     .SC
.SC      SET     .SC+1
        WHILESTMT
        $2C
:1A4     EQU     *
        ((:1A1-*)&$3FFF)+$4000
        $3E,#UNTIL
        ((:1A5-*)&$3FFF)+$8000
UNTILSTMT       EQU     .SC
.SC      SET     .SC+1
        UNTILSTMT
        $2C
:1A5     EQU     *
        ((:1A1-*)&$3FFF)+$4000
        $3E,#DO
        ((:1A6-*)&$3FFF)+$8000
DOSTMT  EQU     .SC
.SC      SET     .SC+1
        DOSTMT
        $2C
:1A6     EQU     *
:1A1     EQU     *
        ((:1A7-*)&$3FFF)+$8000
        $30
        FCC       'E'
        ':+$80
        $32,1
        EQUHERE
        $2F
:1A7     EQU     *
        $29
.       SET     *
        ORG     CLAUSE*2+.SA
        #.
        ORG     .
EXECUTABLESTMT     EQU     .SC
.SC      SET     .SC+1
        EXECUTABLESTMT
        ((:1A8-*)&$3FFF)+$8000
        RESTOFCLAUSE
        $2C
:1A8     EQU     *
        $29
.       SET     *
        ORG     RESTOFCLAUSE*2+.SA
        #.
        ORG     .
:1A9     EQU     *
        $21
        FCC       ''
        '\+$80
        ((:1AA-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
        EXECUTABLESTMT
        $2C
:1AA      EQU     *
        ((:1A9-*)&$3FFF)+$4000
        $34
        $29
.       SET     *
        ORG     DIMLABEL*2+.SA
        #.
        ORG     .
LINENUM EQU     .SC
.SC      SET     .SC+1
        LINENUM
        ((:1AB-*)&$3FFF)+$8000
        $27,ISLABEL
        ((:1AC-*)&$3FFF)+$8000
DOUBLEDEFLINE    EQU     .SC
.SC      SET     .SC+1
        DOUBLEDEFLINE
        $2C
:1AC      EQU     *
        ((:1AD-*)&$3FFF)+$4000
        $34
        $23,ISLABEL
:1AD     EQU     *
        $2C
OPJMP   EQU     .SC
.SC      SET     .SC+1
        OPJMP
        $2F
        ADDR
GENLBL   EQU     .SC
.SC      SET     .SC+1
        GENLBL
        $33,1
        $2F
LABEL     EQU     .SC
.SC      SET     .SC+1
        LABEL
        $32,1
OPLINE  EQU     .SC
.SC      SET     .SC+1
        OPLINE
        $2F
        INTEGER
        $32,1
        $2F
        OPJMP
        $2F
        ADDR
        CODEBODY
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:1AB      EQU     *
        ((:1AE-*)&$3FFF)+$4000
        +LABELID
        ((:1AF-*)&$3FFF)+$8000
        $21
        FCC       ''
        ':+$80
        $2C
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
LABELGEN  EQU     .SC
.SC      SET     .SC+1
        LABELGEN
        $38
        +:1B0-*-1
        +1
:1B0     EQU     *
        $2C
        OPJMP
        $2F
        ADDR
        CODEBODY
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:1AF     EQU     *
:1AE     EQU     *
        ((:1B1-*)&$3FFF)+$8000
        $37,1
:1B1     EQU     *
        $29
.       SET     *
        ORG     LABELGEN*2+.SA
        #.
        ORG     .
        $27,ISLABEL
        ((:1B2-*)&$3FFF)+$8000
        DOUBLEDEFLINE
        $2C
:1B2     EQU     *
        $2A
        $27,POTENTIALLABEL
        ((:1B3-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        $25,COMMONSCALAR
        $2C
        $23,ISLABEL
        $31
EQUDATA   EQU     .SC
.SC      SET     .SC+1
        EQUDATA
        $2F
RESDATA   EQU     .SC
.SC      SET     .SC+1
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
        LABEL
        $31
OPLFREFL        EQU     .SC
.SC      SET     .SC+1
        OPLFREFL
        $2F
        DADDR
        $31
        $2F
        ADDR
        $30
        FCC       ''
        '.+$80
        $2F
        $30
        FCC       '.      SET     *-'
        '5+$80
        $2F
:1B3     EQU     *
        $2A
        $27,FREFLABEL
        ((:1B4-*)&$3FFF)+$8000
        $25,FREFLABEL
        $2C
        $23,ISLABEL
        LABEL
        $31
OPSETLABEL        EQU     .SC
.SC      SET     .SC+1
        OPSETLABEL
        $2F
:1B4     EQU     *
        $2A
DEFINED EQU     .SC
.SC      SET     .SC+1
        DEFINED
        ((:1B5-*)&$3FFF)+$8000
LABELUSEDASVAR      EQU     .SC
.SC      SET     .SC+1
        LABELUSEDASVAR
        $2C
:1B5     EQU     *
        $2A
        $34
        $23,ISLABEL
        LABEL
        $31
        OPSETLABEL
        $2F
        $29
.       SET     *
        ORG     LINELABEL*2+.SA
        #.
        ORG     .
        LINENUM
        ((:1B6-*)&$3FFF)+$8000
        $27,ISLABEL
        ((:1B7-*)&$3FFF)+$8000
        DOUBLEDEFLINE
        $2C
:1B7     EQU     *
        ((:1B8-*)&$3FFF)+$4000
        $34
        $23,ISLABEL
:1B8     EQU     *
        $2C
        LABEL
        $32,1
        OPLINE
        $2F
        INTEGER
        $32,1
        $2F
:1B6     EQU     *
        ((:1B9-*)&$3FFF)+$4000
        +LABELID
        ((:1BA-*)&$3FFF)+$8000
        $21
        FCC       ''
        ':+$80
        $2C
        LABELGEN
        $38
        +:1BB-*-1
        +1
:1BB      EQU     *
        $2C
:1BA      EQU     *
:1B9     EQU     *
        ((:1BC-*)&$3FFF)+$8000
        $37,1
:1BC      EQU     *
        $29
.       SET     *
        ORG     LINENUM*2+.SA
        #.
        ORG     .
        +ZEROS
        ((:1BD-*)&$3FFF)+$8000
        +NUM
        $2C
        $37,2
:1BD     EQU     *
        $2A
        +NUM
        ((:1BE-*)&$3FFF)+$8000
        $37,1
:1BE     EQU     *
        $29
.       SET     *
        ORG     DEFINED*2+.SA
        #.
        ORG     .
        $27,SCALAR
        $2A
        $27,COMMONSCALAR
        $2A
        $27,VECTOR
        $2A
        $27,ARRAY
        $2A
        $27,FUNCTION
        $2A
        $27,ISLABEL
        $2A
        $27,FREFLABEL
        $2A
        $27,TYPESUBROUTINE
        $2A
        $27,FREFSUBROUTINE
        $29
DEFINEDSTRINGQ  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     DEFINEDSTRINGQ*2+.SA
        #.
        ORG     .
        $27,STRVAR
        $2A
        $27,STRARRAY
        $2A
        $27,STRINGFUNCTION
        $2A
        $27,TYPESUBROUTINE
        $2A
        $27,FREFSUBROUTINE
        $29
.       SET     *
        ORG     LABELUSEDASVAR*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Already defined as a variabl'
        'e+$80
        $29
.       SET     *
        ORG     DOUBLEDEFLINE*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Doubly-defined line numbe'
        'r+$80
        $29
SINGLESUBREQ     EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     SINGLESUBREQ*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Single subscript require'
        'd+$80
        $29
DOUBLESUBREQ      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     DOUBLESUBREQ*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Double subscript require'
        'd+$80
        $29
NOFUNCTSHERE     EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOFUNCTSHERE*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Function not allowed her'
        'e+$80
        $29
DOUBLEDEFVAR      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     DOUBLEDEFVAR*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Use of name incompatible with previous us'
        'e+$80
        $29
DOUBLEDEFSTR     EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     DOUBLEDEFSTR*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Doubly defined string variabl'
        'e+$80
        $29
NOSUBVARS EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOSUBVARS*2+.SA
        #.
        ORG     .
        $2D
        FCC       'No subscripted variables allowed her'
        'e+$80
        $29
CANTSTORELABEL      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     CANTSTORELABEL*2+.SA
        #.
        ORG     .
        $2D
        FCC       'Assignment to a label is not allowe'
        'd+$80
        $29
UDFSID  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     UDFSID*2+.SA
        #.
        ORG     .
        $23,STRVAR
        $2D
        FCC       'Undefined string variabl'
        'e+$80
        $29
NOMATCHINGFOR     EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOMATCHINGFOR*2+.SA
        #.
        ORG     .
        $2D
        FCC       "No enclosing FOR with same variabl"
        'e+$80
        $29
VARUSEDASLABEL      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     VARUSEDASLABEL*2+.SA
        #.
        ORG     .
        $2D
        FCC       "Can't use variable name for labe"
        'l+$80
        $29
NOPARAMETERSHERE  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOPARAMETERSHERE*2+.SA
        #.
        ORG     .
        $2D
        FCC       "Parameter variable not allowed her"
        'e+$80
        $29
.       SET     *
        ORG     INCLUDESTMT*2+.SA
        #.
        ORG     .
        $3E,#INCLUDE
        ((:1BF-*)&$3FFF)+$8000
        $21
        FCC       ''
        '"+$80
        ((:1C0-*)&$3FFF)+$4000
        $21
        FCC       ""
        ''+$80
:1C0     EQU     *
        $2C
        +STRINGGUTS
        $2C
        $21
        FCC       ''
        '"+$80
        ((:1C1-*)&$3FFF)+$4000
        $21
        FCC       ""
        ''+$80
:1C1     EQU     *
        $2C
        $36
        ENDOFLINEOPTCOMMENT
        $2C
:1BF     EQU     *
        $29
.       SET     *
        ORG     EXECUTABLESTMT*2+.SA
        #.
        ORG     .
        REMHEAD
        ((:1C2-*)&$3FFF)+$8000
        REMARK
        $2C
:1C2     EQU     *
        $2A
ASSIGNSTMT       EQU     .SC
.SC      SET     .SC+1
        ASSIGNSTMT
        $2A
KEYWORDSTMT     EQU     .SC
.SC      SET     .SC+1
        KEYWORDSTMT
        $29
.       SET     *
        ORG     KEYWORDSTMT*2+.SA
        #.
        ORG     .
        $3E,#LET
        ((:1C3-*)&$3FFF)+$8000
        ASSIGNSTMT
        $2C
:1C3     EQU     *
        $2A
        $3E,#IF
        ((:1C4-*)&$3FFF)+$8000
        IFSTMT
        $2C
:1C4     EQU     *
        $2A
        $3E,#GOTO
        ((:1C5-*)&$3FFF)+$8000
GOTOSTMT        EQU     .SC
.SC      SET     .SC+1
        GOTOSTMT
        $2C
:1C5     EQU     *
        $2A
        $3E,#PRINT
        ((:1C6-*)&$3FFF)+$8000
PRINTSTMT       EQU     .SC
.SC      SET     .SC+1
        PRINTSTMT
        $2C
:1C6     EQU     *
        $2A
        $3E,#FOR
        ((:1C7-*)&$3FFF)+$8000
        FORSTMT
        $2C
:1C7     EQU     *
        $2A
        $3E,#CYCLE
        ((:1C8-*)&$3FFF)+$8000
CYCLESTMT EQU     .SC
.SC      SET     .SC+1
        CYCLESTMT
        $2C
:1C8     EQU     *
        $2A
        $3E,#GOSUB
        ((:1C9-*)&$3FFF)+$8000
GOSUBSTMT        EQU     .SC
.SC      SET     .SC+1
        GOSUBSTMT
        $2C
:1C9     EQU     *
        $2A
        $3E,#RETURN
        ((:1CA-*)&$3FFF)+$8000
RETURNSTMT      EQU     .SC
.SC      SET     .SC+1
        RETURNSTMT
        $2C
:1CA      EQU     *
        $2A
        $3E,#STOP
        ((:1CB-*)&$3FFF)+$8000
STOPSTMT        EQU     .SC
.SC      SET     .SC+1
        STOPSTMT
        $2C
:1CB      EQU     *
        $2A
        $3E,#WHILE
        ((:1CC-*)&$3FFF)+$8000
        WHILESTMT
        $2C
:1CC      EQU     *
        $2A
        $3E,#UNTIL
        ((:1CD-*)&$3FFF)+$8000
        UNTILSTMT
        $2C
:1CD     EQU     *
        $2A
        $3E,#REPEAT
        ((:1CE-*)&$3FFF)+$8000
        REPEATSTMT
        $2C
:1CE     EQU     *
        $2A
        $3E,#DO
        ((:1CF-*)&$3FFF)+$8000
        DOSTMT
        $2C
:1CF     EQU     *
        $2A
        $3E,#INPUT
        ((:1D0-*)&$3FFF)+$8000
INPUTSTMT       EQU     .SC
.SC      SET     .SC+1
        INPUTSTMT
        $2C
:1D0    EQU     *
        $2A
        $3E,#READ
        ((:1D1-*)&$3FFF)+$8000
READSTMT EQU     .SC
.SC      SET     .SC+1
        READSTMT
        $2C
:1D1    EQU     *
        $2A
        $3E,#WRITE
        ((:1D2-*)&$3FFF)+$8000
WRITESTMT       EQU     .SC
.SC      SET     .SC+1
        WRITESTMT
        $2C
:1D2    EQU     *
        $2A
        $3E,#POSITION
        ((:1D3-*)&$3FFF)+$4000
        $3E,#RESTORE
:1D3    EQU     *
        ((:1D4-*)&$3FFF)+$8000
POSITIONSTMT    EQU     .SC
.SC      SET     .SC+1
        POSITIONSTMT
        $2C
:1D4    EQU     *
        $2A
        $3E,#OPEN
        ((:1D5-*)&$3FFF)+$8000
OPENSTMT        EQU     .SC
.SC      SET     .SC+1
        OPENSTMT
        $2C
:1D5    EQU     *
        $2A
        $3E,#CREATE
        ((:1D6-*)&$3FFF)+$8000
CREATESTMT        EQU     .SC
.SC      SET     .SC+1
        CREATESTMT
        $2C
:1D6    EQU     *
        $2A
        $3E,#CLOSE
        ((:1D7-*)&$3FFF)+$8000
CLOSESTMT        EQU     .SC
.SC      SET     .SC+1
        CLOSESTMT
        $2C
:1D7    EQU     *
        $2A
        $3E,#DELETE
        ((:1D8-*)&$3FFF)+$8000
DELETESTMT      EQU     .SC
.SC      SET     .SC+1
        DELETESTMT
        $2C
:1D8    EQU     *
        $2A
        $3E,#RENAME
        ((:1D9-*)&$3FFF)+$8000
RENAMESTMT       EQU     .SC
.SC      SET     .SC+1
        RENAMESTMT
        $2C
:1D9    EQU     *
        $2A
        $3E,#ON
        ((:1DA-*)&$3FFF)+$8000
ONSTMT  EQU     .SC
.SC      SET     .SC+1
        ONSTMT
        $2C
:1DA     EQU     *
        $2A
        $3E,#CHAIN
        ((:1DB-*)&$3FFF)+$8000
CHAINSTMT EQU     .SC
.SC      SET     .SC+1
        CHAINSTMT
        $2C
:1DB     EQU     *
        $2A
        $3E,#CALL
        ((:1DC-*)&$3FFF)+$8000
CALLSTMT  EQU     .SC
.SC      SET     .SC+1
        CALLSTMT
        $2C
:1DC     EQU     *
        $2A
        $3E,#POKE
        ((:1DD-*)&$3FFF)+$8000
POKESTMT        EQU     .SC
.SC      SET     .SC+1
        POKESTMT
        $2C
:1DD    EQU     *
        $2A
        $3E,#DEBUG
        ((:1DE-*)&$3FFF)+$8000
DEBUGSTMT        EQU     .SC
.SC      SET     .SC+1
        DEBUGSTMT
        $2C
:1DE    EQU     *
        $2A
        $3E,#ERROR
        ((:1DF-*)&$3FFF)+$8000
ERRSTMT EQU     .SC
.SC      SET     .SC+1
        ERRSTMT
        $2C
:1DF    EQU     *
        $2A
        $3E,#SYSCALL
        ((:1E0-*)&$3FFF)+$8000
SYSCALLSTMT       EQU     .SC
.SC      SET     .SC+1
        SYSCALLSTMT
        $2C
:1E0    EQU     *
        $2A
        $3E,#EXIT
        ((:1E1-*)&$3FFF)+$8000
EXITSTMT        EQU     .SC
.SC      SET     .SC+1
        EXITSTMT
        $2C
:1E1    EQU     *
        $29
.       SET     *
        ORG     ENDSTMT*2+.SA
        #.
        ORG     .
        $3E,#END
        ((:1E2-*)&$3FFF)+$8000
        $3A,COMPILEPHASE,COMPILEDONE
:1E2    EQU     *
        $29
.       SET     *
        ORG     COMMONSTMT*2+.SA
        #.
        ORG     .
        $3E,#COMMON
        ((:1E3-*)&$3FFF)+$8000
DIMBODY  EQU     .SC
.SC      SET     .SC+1
        DIMBODY
        $2C
:1E3    EQU     *
        $29
NOTLEGALIFCOMMON  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOTLEGALIFCOMMON*2+.SA
        #.
        ORG     .
        $3B,COMPILEPHASE,COMPILINGCOMMON
        ((:1E4-*)&$3FFF)+$8000
        $2D
        FCC       "Can't initialize a COMMON variabl"
        'e+$80
:1E4    EQU     *
        $2A
        $34
        $29
.       SET     *
        ORG     DIMSTMT*2+.SA
        #.
        ORG     .
        $3E,#DIM
        ((:1E5-*)&$3FFF)+$8000
        $3A,COMPILEPHASE,COMPILINGDIMS
        DIMBODY
        $2C
:1E5    EQU     *
        $29
.       SET     *
        ORG     DIMBODY*2+.SA
        #.
        ORG     .
DIMOBJ   EQU     .SC
.SC      SET     .SC+1
        DIMOBJ
        ((:1E6-*)&$3FFF)+$8000
:1E7    EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1E8-*)&$3FFF)+$8000
        DIMOBJ
        $2C
:1E8    EQU     *
        ((:1E7-*)&$3FFF)+$4000
        $34
        ENDOFLINEOPTCOMMENT
        $2C
:1E6    EQU     *
        $29
.       SET     *
        ORG     DIMOBJ*2+.SA
        #.
        ORG     .
        +SID
        ((:1E9-*)&$3FFF)+$8000
        DEFINEDSTRINGQ
        ((:1EA-*)&$3FFF)+$8000
        DOUBLEDEFSTR
        $2C
:1EA     EQU     *
        ((:1EB-*)&$3FFF)+$4000
        $34
:1EB     EQU     *
        $2C
        $31
        EQUDATA
        $2F
        LB
        ((:1EC-*)&$3FFF)+$8000
        +NUM
        $2C
        RB
        $2C
        LB
        ((:1ED-*)&$3FFF)+$8000
        +NUM
        $2C
        RB
        $2C
        $24,1,STRARRAY
OPSAINIT EQU     .SC
.SC      SET     .SC+1
        OPSAINIT
        $2F
        DADDR
        $30
        FCC       ":"
        ':+$80
        $2F
        INTEGER
        $32,2
        $2F
        INTEGER
        $32,3
        $2F
        RESDATA
        $32,2
        $30
        FCC       "*"
        '(+$80
        $32,3
        $30
        FCC       "+4)+"
        '2+$80
        $2F
:1ED    EQU     *
        ((:1EE-*)&$3FFF)+$4000
        $21
        FCC       ''
        '/+$80
        ((:1EF-*)&$3FFF)+$8000
        NOTLEGALIFCOMMON
        $2C
INIT    EQU     .SC
.SC      SET     .SC+1
        INIT
        GENLBL
        $33,2
        $30
        FCC       '-*-2   ?Initializing string exceeds DIMensio'
        'n+$80
        $2F
        INTEGER
        $32,2
        $2F
        INTEGER
        GENLBL
        $33,2
        $30
        FCC       '-*-'
        '2+$80
        $2F
BYTELST  EQU     .SC
.SC      SET     .SC+1
        BYTELST
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
        RESDATA
        $30
        FCC       '4'
        '++$80
        $32,2
        $2F
        $21
        FCC       ''
        '/+$80
        $2C
:1EF    EQU     *
        ((:1F0-*)&$3FFF)+$4000
        $3B,COMPILEPHASE,COMPILINGCOMMON
        ((:1F1-*)&$3FFF)+$8000
        INIT
        $30
        FCC       ''
        '2+$80
        $2F
        INTEGER
        $31
        $2F
        RESDATA
        $30
        FCC       '4'
        '++$80
        $31
        $2F
:1F1    EQU     *
        ((:1F0-*)&$3FFF)+$4000
        $34
        INIT
        $30
        FCC       ''
        '4+$80
        $2F
        INTEGER
        $31
        $2F
        INTEGER
        $30
        FCC       ''
        '0+$80
        $2F
        RESDATA
        $30
        FCC       '4'
        '++$80
        $31
        $2F
:1F0    EQU     *
        ((:1F2-*)&$3FFF)+$8000
        $24,1,STRVAR
:1F2    EQU     *
:1EE    EQU     *
        $2C
:1EC     EQU     *
        ((:1F3-*)&$3FFF)+$4000
        $21
        FCC       ''
        '/+$80
        ((:1F4-*)&$3FFF)+$8000
        NOTLEGALIFCOMMON
        $2C
        INIT
        GENLBL
        $33,2
        $30
        FCC       '-*-'
        '2+$80
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
        INTEGER
        GENLBL
        $33,2
        $30
        FCC       '-*-'
        '4+$80
        $2F
        INTEGER
        GENLBL
        $33,2
        $30
        FCC       '-*-'
        '2+$80
        $2F
        BYTELST
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
        RESDATA
        $30
        FCC       '*'
        '-+$80
        GENLBL
        $33,1
        $2F
        $21
        FCC       ''
        '/+$80
        $2C
        $24,1,STRVAR
:1F4    EQU     *
:1F3    EQU     *
        $2C
:1E9    EQU     *
        $2A
        +NID
        ((:1F5-*)&$3FFF)+$8000
        $27,ISLABEL
        ((:1F6-*)&$3FFF)+$4000
        $27,FREFLABEL
:1F6    EQU     *
        ((:1F7-*)&$3FFF)+$8000
        VARUSEDASLABEL
        $2C
:1F7    EQU     *
        ((:1F8-*)&$3FFF)+$4000
        DEFINED
        ((:1F9-*)&$3FFF)+$8000
        DOUBLEDEFVAR
        $2C
:1F9    EQU     *
        ((:1F8-*)&$3FFF)+$4000
        $34
:1F8    EQU     *
        $2C
        LB
        ((:1FA-*)&$3FFF)+$8000
        $31
        EQUDATA
        $2F
        +NUM
        $2C
        RB
        ((:1FB-*)&$3FFF)+$8000
        $24,1,VECTOR
        INIT
        $30
        FCC       ''
        '2+$80
        $2F
        INTEGER
        $32,2
        $2F
        RESDATA
        $30
        FCC       '8+6'
        '*+$80
        $32,2
        $2F
:1FB     EQU     *
        ((:1FC-*)&$3FFF)+$4000
        $21
        FCC       ''
        ',+$80
        ((:1FD-*)&$3FFF)+$8000
        +NUM
        $2C
        RB
        $2C
        $24,1,ARRAY
        INIT
        $30
        FCC       ''
        '4+$80
        $2F
        INTEGER
        $32,2
        $2F
        INTEGER
        $32,3
        $2F
        RESDATA
        $30
        FCC       '4+6*'
        '(+$80
        $32,2
        $30
        FCC       '+1)*'
        '(+$80
        $32,3
        $30
        FCC       '+1'
        ')+$80
        $2F
:1FD    EQU     *
:1FC     EQU     *
        $2C
:1FA     EQU     *
        ((:1FE-*)&$3FFF)+$4000
        $3B,COMPILEPHASE,COMPILINGCOMMON
        ((:1FF-*)&$3FFF)+$8000
        $31
        EQUDATA
        $2F
        $21
        FCC       ''
        '/+$80
        ((:1100-*)&$3FFF)+$8000
        NOTLEGALIFCOMMON
        $2C
CONSTANT  EQU     .SC
.SC      SET     .SC+1
        CONSTANT
        $2C
        $21
        FCC       ''
        '/+$80
        $2C
OPSTORE EQU     .SC
.SC      SET     .SC+1
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1100   EQU     *
        ((:1101-*)&$3FFF)+$4000
        $34
:1101   EQU     *
        $2C
        $24,1,COMMONSCALAR
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
:1FF    EQU     *
        ((:1FE-*)&$3FFF)+$4000
        $21
        FCC       ''
        '/+$80
        ((:1102-*)&$3FFF)+$8000
        +NUM
        ((:1103-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:1104-*)&$3FFF)+$8000
        $24,1,VECTOR
INITVECTORHEAD    EQU     .SC
.SC      SET     .SC+1
        INITVECTORHEAD
        $38
        +:1105-*-1+$80
        +1
:1105   EQU     *
        $2F
SIXBYTEINTEGER   EQU     .SC
.SC      SET     .SC+1
        SIXBYTEINTEGER
        $38
        +:1106-*-1
        +2
:1106   EQU     *
        $2F
VECTORINITLIST   EQU     .SC
.SC      SET     .SC+1
        VECTORINITLIST
        $2C
INITVECTORTAIL    EQU     .SC
.SC      SET     .SC+1
        INITVECTORTAIL
        $38
        +:1107-*-1+$80
:1107   EQU     *
        $2F
:1104   EQU     *
        ((:1108-*)&$3FFF)+$4000
        $34
        $24,1,SCALAR
INITSCALARHEAD      EQU     .SC
.SC      SET     .SC+1
        INITSCALARHEAD
        $2F
        SIXBYTEINTEGER
        $38
        +:1109-*-1
        +2
:1109   EQU     *
        $2F
        $32,1
ALLOCATESCALAR        EQU     .SC
.SC      SET     .SC+1
        ALLOCATESCALAR
        $2F
:1108   EQU     *
        $2C
:1103   EQU     *
        ((:110A-*)&$3FFF)+$4000
        +FNUM
        ((:110B-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:110C-*)&$3FFF)+$8000
        $24,1,VECTOR
        INITVECTORHEAD
        $38
        +:110D-*-1+$80
        +1
:110D   EQU     *
        $2F
SIXBYTEFNUM      EQU     .SC
.SC      SET     .SC+1
        SIXBYTEFNUM
        $38
        +:110E-*-1
        +2
:110E   EQU     *
        $2F
        VECTORINITLIST
        $2C
        INITVECTORTAIL
        $38
        +:110F-*-1+$80
:110F   EQU     *
        $2F
:110C    EQU     *
        ((:1110-*)&$3FFF)+$4000
        $34
        $24,1,SCALAR
        INITSCALARHEAD
        $2F
        SIXBYTEFNUM
        $38
        +:1111-*-1
        +2
:1111   EQU     *
        $2F
        $32,1
        ALLOCATESCALAR
        $2F
:1110   EQU     *
        $2C
:110B    EQU     *
        ((:110A-*)&$3FFF)+$4000
        $21
        FCC       ''
        ':+$80
        ((:1112-*)&$3FFF)+$8000
        +HEXNUM
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1113-*)&$3FFF)+$8000
        $24,1,VECTOR
        INITVECTORHEAD
        $38
        +:1114-*-1+$80
        +1
:1114   EQU     *
        $2F
SIXBYTEHEXINTEGER        EQU     .SC
.SC      SET     .SC+1
        SIXBYTEHEXINTEGER
        $38
        +:1115-*-1
        +2
:1115   EQU     *
        $2F
        VECTORINITLIST
        $2C
        INITVECTORTAIL
        $38
        +:1116-*-1+$80
:1116   EQU     *
        $2F
:1113   EQU     *
        ((:1117-*)&$3FFF)+$4000
        $34
        $24,1,SCALAR
        INITSCALARHEAD
        $2F
        SIXBYTEHEXINTEGER
        $38
        +:1118-*-1
        +2
:1118   EQU     *
        $2F
        $32,1
        ALLOCATESCALAR
        $2F
:1117   EQU     *
        $2C
:1112   EQU     *
:110A    EQU     *
        $2C
        $21
        FCC       ''
        '/+$80
        $2C
:1102   EQU     *
        ((:1FE-*)&$3FFF)+$4000
        $34
        $24,1,SCALAR
        $32,1
        ALLOCATESCALAR
        $2F
:1FE    EQU     *
        $2C
:1F5    EQU     *
        $29
.       SET     *
        ORG     CONSTANT*2+.SA
        #.
        ORG     .
        +ONEDIGITNUM
        ((:1119-*)&$3FFF)+$8000
OPLSMI  EQU     .SC
.SC      SET     .SC+1
        OPLSMI
        $31
        $2F
:1119   EQU     *
        $2A
        +TENTO255
        ((:111A-*)&$3FFF)+$8000
OPLBINT  EQU     .SC
.SC      SET     .SC+1
        OPLBINT
        $2F
        BYTE
        $31
        $2F
:111A    EQU     *
        $2A
        +NUM
        ((:111B-*)&$3FFF)+$8000
OPLINT  EQU     .SC
.SC      SET     .SC+1
        OPLINT
        $2F
        INTEGER
        $31
        $2F
:111B    EQU     *
        $2A
        +FNUM
        ((:111C-*)&$3FFF)+$8000
OPLFI   EQU     .SC
.SC      SET     .SC+1
        OPLFI
        $2F
        SIXBYTEFNUM
        $38
        +:111D-*-1
        +1
:111D   EQU     *
        $2F
:111C    EQU     *
        $2A
        $21
        FCC       ''
        ':+$80
        ((:111E-*)&$3FFF)+$8000
        +HEXNUM
        $2C
        OPLINT
        $2F
        INTEGER
        $30
        FCC       ''
        '$+$80
        $31
        $2F
:111E   EQU     *
        $29
.       SET     *
        ORG     BYTELST*2+.SA
        #.
        ORG     .
BYTEOBJ   EQU     .SC
.SC      SET     .SC+1
        BYTEOBJ
        ((:111F-*)&$3FFF)+$8000
:1120   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1121-*)&$3FFF)+$8000
        BYTEOBJ
        $2C
:1121   EQU     *
        ((:1120-*)&$3FFF)+$4000
        $34
:111F   EQU     *
        $29
.       SET     *
        ORG     BYTEOBJ*2+.SA
        #.
        ORG     .
        +STRING
        ((:1122-*)&$3FFF)+$8000
TEXT    EQU     .SC
.SC      SET     .SC+1
        TEXT
        $31
        $2F
:1122   EQU     *
        $2A
        +NUM
        ((:1123-*)&$3FFF)+$8000
        BYTE
        $31
        $2F
:1123   EQU     *
        $2A
        $21
        FCC       ''
        ':+$80
        ((:1124-*)&$3FFF)+$8000
        +HEXNUM
        $2C
        BYTE
        $30
        FCC       ''
        '$+$80
        $31
        $2F
:1124   EQU     *
        $29
.       SET     *
        ORG     VECTORINITLIST*2+.SA
        #.
        ORG     .
VECTORINITVALUE   EQU     .SC
.SC      SET     .SC+1
        VECTORINITVALUE
        ((:1125-*)&$3FFF)+$8000
:1126   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1127-*)&$3FFF)+$8000
        VECTORINITVALUE
        $2C
:1127   EQU     *
        ((:1126-*)&$3FFF)+$4000
        $34
:1125   EQU     *
        $29
.       SET     *
        ORG     VECTORINITVALUE*2+.SA
        #.
        ORG     .
        +NUM
        ((:1128-*)&$3FFF)+$8000
        SIXBYTEINTEGER
        $38
        +:1129-*-1
        +1
:1129   EQU     *
        $2F
:1128   EQU     *
        $2A
        +FNUM
        ((:112A-*)&$3FFF)+$8000
        SIXBYTEFNUM
        $38
        +:112B-*-1
        +1
:112B    EQU     *
        $2F
:112A    EQU     *
        $2A
        $21
        FCC       ''
        ':+$80
        ((:112C-*)&$3FFF)+$8000
        +HEXNUM
        $2C
        SIXBYTEHEXINTEGER
        $38
        +:112D-*-1
        +1
:112D   EQU     *
        $2F
:112C    EQU     *
        $29
.       SET     *
        ORG     FORMATLINE*2+.SA
        #.
        ORG     .
        $3E,#FORMAT
        ((:112E-*)&$3FFF)+$8000
        +STRING
        $2C
OPJUC    EQU     .SC
.SC      SET     .SC+1
        OPJUC
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '-*-'
        '1+$80
        $2F
        TEXT
        $31
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
        ENDOFLINEOPTCOMMENT
        $2C
:112E   EQU     *
        $29
.       SET     *
        ORG     ASSIGNSTMT*2+.SA
        #.
        ORG     .
        +NID
        ((:112F-*)&$3FFF)+$8000
        $27,SCALAR
        ((:1130-*)&$3FFF)+$8000
NOSUBSCRIPTS      EQU     .SC
.SC      SET     .SC+1
        NOSUBSCRIPTS
        $2C
EQUALNUMEXP      EQU     .SC
.SC      SET     .SC+1
        EQUALNUMEXP
        $2C
OPSSCALAR  EQU     .SC
.SC      SET     .SC+1
        OPSSCALAR
        $38
        +:1131-*-1
        +1
:1131   EQU     *
        $2C
:1130   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:1133-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $31
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
        EQUALNUMEXP
        $2C
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1133   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,COMMONSCALAR
        ((:1134-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $27,PARAMETER
        ((:1135-*)&$3FFF)+$8000
OPLOAD   EQU     .SC
.SC      SET     .SC+1
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
        EQUALNUMEXP
        $2C
OPST    EQU     .SC
.SC      SET     .SC+1
        OPST
        $2F
:1135   EQU     *
        ((:1136-*)&$3FFF)+$4000
        EQUALNUMEXP
        ((:1137-*)&$3FFF)+$8000
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1137   EQU     *
:1136   EQU     *
        $2C
:1134   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,VECTOR
        ((:1138-*)&$3FFF)+$8000
ONESUBSCRIPT      EQU     .SC
.SC      SET     .SC+1
        ONESUBSCRIPT
        $2C
        $28,1,PARAMETER
        ((:1139-*)&$3FFF)+$8000
OPVPA    EQU     .SC
.SC      SET     .SC+1
        OPVPA
        $2F
        DADDR
        $32,1
        $2F
        EQUALNUMEXP
        $2C
        OPST
        $2F
:1139   EQU     *
        ((:113A-*)&$3FFF)+$4000
        EQUALNUMEXP
        ((:113B-*)&$3FFF)+$8000
OPVSS   EQU     .SC
.SC      SET     .SC+1
        OPVSS
        $2F
        DADDR
        $32,1
        $2F
:113B    EQU     *
:113A    EQU     *
        $2C
:1138   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,ARRAY
        ((:113C-*)&$3FFF)+$8000
TWOSUBSCRIPTS     EQU     .SC
.SC      SET     .SC+1
        TWOSUBSCRIPTS
        $2C
        $28,1,PARAMETER
        ((:113D-*)&$3FFF)+$8000
OPAPA     EQU     .SC
.SC      SET     .SC+1
        OPAPA
        $2F
        DADDR
        $32,1
        $2F
        EQUALNUMEXP
        $2C
        OPST
        $2F
:113D   EQU     *
        ((:113E-*)&$3FFF)+$4000
        EQUALNUMEXP
        ((:113F-*)&$3FFF)+$8000
OPASS    EQU     .SC
.SC      SET     .SC+1
        OPASS
        $2F
        DADDR
        $32,1
        $2F
:113F   EQU     *
:113E   EQU     *
        $2C
:113C    EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:1140-*)&$3FFF)+$4000
        $27,FREFLABEL
:1140   EQU     *
        ((:1141-*)&$3FFF)+$8000
        CANTSTORELABEL
        $2C
:1141   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:1142-*)&$3FFF)+$8000
ARGUMENTLIST     EQU     .SC
.SC      SET     .SC+1
        ARGUMENTLIST
        $38
        +:1143-*-1+$80
:1143   EQU     *
        $2C
        $21
        FCC       ''
        '=+$80
        $2C
NUMERICARGUMENT   EQU     .SC
.SC      SET     .SC+1
        NUMERICARGUMENT
        $2C
OPCALL    EQU     .SC
.SC      SET     .SC+1
        OPCALL
        $2F
        ADDR
        $30
        FCC       'SE'
        'T+$80
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '+'
        '1+$80
        $2F
:1142   EQU     *
        ((:1132-*)&$3FFF)+$4000
        $27,TYPESUBROUTINE
        ((:1144-*)&$3FFF)+$4000
        $27,FREFSUBROUTINE
:1144   EQU     *
        ((:1145-*)&$3FFF)+$8000
        ARGUMENTLIST
        $38
        +:1146-*-1+$80
:1146   EQU     *
        $2C
        OPCALL
        $2F
        ADDR
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $2F
:1145   EQU     *
        ((:1132-*)&$3FFF)+$4000
        NOSUBSCRIPTS
        ((:1147-*)&$3FFF)+$8000
        $23,SCALAR
        $31
        ALLOCATESCALAR
        $2F
        EQUALNUMEXP
        $2C
        OPSSCALAR
        $38
        +:1148-*-1
        +1
:1148   EQU     *
        $2C
:1147   EQU     *
:1132   EQU     *
        $2C
:112F   EQU     *
        $2A
OBVIOUSSTRINGTARGET       EQU     .SC
.SC      SET     .SC+1
        OBVIOUSSTRINGTARGET
        ((:1149-*)&$3FFF)+$8000
        $21
        FCC       ''
        '=+$80
        $2C
        STREXP
        $2C
OPSTS   EQU     .SC
.SC      SET     .SC+1
        OPSTS
        $2F
:1149   EQU     *
        $2A
        +SID
        ((:114A-*)&$3FFF)+$8000
STRVARREF        EQU     .SC
.SC      SET     .SC+1
        STRVARREF
        $38
        +:114B-*-1
        +1
:114B    EQU     *
        ((:114C-*)&$3FFF)+$8000
        LB
        ((:114D-*)&$3FFF)+$8000
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:114E-*)&$3FFF)+$8000
        NUMEXP
        $2C
        RB
        $2C
OPSS2   EQU     .SC
.SC      SET     .SC+1
        OPSS2
        $2F
        $21
        FCC       ''
        '=+$80
        $2C
        STREXP
        $2C
        OPSTS
        $2F
:114E   EQU     *
        ((:114F-*)&$3FFF)+$4000
        RB
        ((:1150-*)&$3FFF)+$8000
OPSS1   EQU     .SC
.SC      SET     .SC+1
        OPSS1
        $2F
        EQUALNUMEXP
        $2C
OPSTB    EQU     .SC
.SC      SET     .SC+1
        OPSTB
        $2F
:1150   EQU     *
:114F   EQU     *
        $2C
:114D   EQU     *
        ((:1151-*)&$3FFF)+$4000
        $21
        FCC       ''
        '=+$80
        ((:1152-*)&$3FFF)+$8000
        STREXP
        $2C
        OPSTS
        $2F
:1152   EQU     *
:1151   EQU     *
        $2C
:114C    EQU     *
        ((:1153-*)&$3FFF)+$4000
        $27,STRINGFUNCTION
        ((:1154-*)&$3FFF)+$8000
        ARGUMENTLIST
        $38
        +:1155-*-1+$80
:1155   EQU     *
        $2C
        $21
        FCC       ''
        '=+$80
        $2C
STRINGARGUMENT   EQU     .SC
.SC      SET     .SC+1
        STRINGARGUMENT
        $2C
        OPCALL
        $2F
        ADDR
        $30
        FCC       'SE'
        'T+$80
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '+'
        '1+$80
        $2F
:1154   EQU     *
        ((:1153-*)&$3FFF)+$4000
        UDFSID
:1153   EQU     *
        $2C
:114A    EQU     *
        $2A
        $3E,#LEN
        ((:1156-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
DEFSID  EQU     .SC
.SC      SET     .SC+1
        DEFSID
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        EQUALNUMEXP
        $2C
OPSSL   EQU     .SC
.SC      SET     .SC+1
        OPSSL
        $2F
:1156   EQU     *
        $2A
        $3E,#RND
        ((:1157-*)&$3FFF)+$8000
        EQUALNUMEXP
        $2C
OPSETSEED       EQU     .SC
.SC      SET     .SC+1
        OPSETSEED
        $2F
:1157   EQU     *
        $29
.       SET     *
        ORG     EQUALNUMEXP*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '=+$80
        ((:1158-*)&$3FFF)+$8000
        NUMEXP
        $2C
:1158   EQU     *
        $29
.       SET     *
        ORG     OBVIOUSSTRINGTARGET*2+.SA
        #.
        ORG     .
        $3E,#LEFT$
        ((:1159-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        DEFSID
        $2C
        OPLSMI
        $30
        FCC       '+'
        '1+$80
        $2F
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
:1159   EQU     *
        ((:115A-*)&$3FFF)+$4000
        $3E,#MID$
        ((:115B-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        DEFSID
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
:115B    EQU     *
:115A    EQU     *
        ((:115C-*)&$3FFF)+$8000
        OPSS2
        $2F
:115C    EQU     *
        $2A
        $3E,#RIGHT$
        ((:115D-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        DEFSID
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
OPRIGHT EQU     .SC
.SC      SET     .SC+1
        OPRIGHT
        $2F
:115D   EQU     *
        $29
.       SET     *
        ORG     STRVARREF*2+.SA
        #.
        ORG     .
        $27,STRVAR
        ((:115E-*)&$3FFF)+$8000
        $27,PARAMETER
        ((:115F-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
:115F   EQU     *
        ((:1160-*)&$3FFF)+$4000
        $34
OPLSD   EQU     .SC
.SC      SET     .SC+1
        OPLSD
        $2F
        DADDR
        $32,1
        $2F
:1160   EQU     *
        $2C
:115E   EQU     *
        $2A
        $27,STRARRAY
        ((:1161-*)&$3FFF)+$8000
        $27,PARAMETER
        ((:1162-*)&$3FFF)+$8000
        OPLOAD
        $2F
:1162   EQU     *
        ((:1163-*)&$3FFF)+$4000
        $34
OPLADDR  EQU     .SC
.SC      SET     .SC+1
        OPLADDR
        $2F
:1163   EQU     *
        $2C
        DADDR
        $32,1
        $2F
        LB
        ((:1164-*)&$3FFF)+$8000
        NUMEXP
        $2C
        RB
        $2C
OPSSA    EQU     .SC
.SC      SET     .SC+1
        OPSSA
        $2F
:1164   EQU     *
        ((:1165-*)&$3FFF)+$4000
        $2D
        FCC       "String array requires subscript her"
        'e+$80
:1165   EQU     *
        $2C
:1161   EQU     *
        $29
.       SET     *
        ORG     GOTOSTMT*2+.SA
        #.
        ORG     .
        $3E,#ELN
        ((:1166-*)&$3FFF)+$8000
OPGOELN EQU     .SC
.SC      SET     .SC+1
        OPGOELN
        $2F
:1166   EQU     *
        $2A
        $34
        OPJMP
        $2F
TARGET   EQU     .SC
.SC      SET     .SC+1
        TARGET
        $2C
        $29
.       SET     *
        ORG     TARGET*2+.SA
        #.
        ORG     .
        LINENUM
        ((:1167-*)&$3FFF)+$8000
        ADDR
        LABEL
        $31
        $2F
:1167   EQU     *
        $2A
        +NID
        ((:1168-*)&$3FFF)+$8000
TARGETSUB EQU     .SC
.SC      SET     .SC+1
        TARGETSUB
        $2C
:1168   EQU     *
        $29
.       SET     *
        ORG     TARGETSUB*2+.SA
        #.
        ORG     .
        $27,ISLABEL
        ((:1169-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:116A-*)&$3FFF)+$8000
        $25,COMMONSCALAR
        $2C
:116A    EQU     *
        ((:1169-*)&$3FFF)+$4000
        $27,FREFLABEL
        ((:1169-*)&$3FFF)+$4000
        DEFINED
        ((:116B-*)&$3FFF)+$8000
        VARUSEDASLABEL
        $2C
:116B    EQU     *
        ((:1169-*)&$3FFF)+$4000
        $34
        $23,FREFLABEL
:1169   EQU     *
        ((:116C-*)&$3FFF)+$8000
        ADDR
        LABEL
        $31
        $2F
:116C    EQU     *
        $29
.       SET     *
        ORG     FORSTMT*2+.SA
        #.
        ORG     .
DEFSCALAR  EQU     .SC
.SC      SET     .SC+1
        DEFSCALAR
        ((:116D-*)&$3FFF)+$8000
        $27,INFORLOOP
        ((:116E-*)&$3FFF)+$8000
        $25,INFORLOOP
        $2C
        $2D
        FCC       "Loop variable controls an enclosing FO"
        'R+$80
:116E   EQU     *
        ((:116F-*)&$3FFF)+$4000
        $27,PARAMETER
        ((:1170-*)&$3FFF)+$8000
        $2D
        FCC       "Can't use a parameter as FOR loop inde"
        'x+$80
:1170   EQU     *
        ((:116F-*)&$3FFF)+$4000
        $34
        $23,INFORLOOP
:116F   EQU     *
        $2C
        $27,STEPLIMALLOCATED
        ((:1171-*)&$3FFF)+$4000
        $34
        $23,STEPLIMALLOCATED
        $30
        FCC       'S'
        ':+$80
        $32,1
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       '1'
        '2+$80
        $2F
:1171   EQU     *
        $2C
        EQUALNUMEXP
        $2C
        $3E,#TO
        $2C
        NUMEXP
        $2C
        $3E,#STEP
        ((:1172-*)&$3FFF)+$8000
        NUMEXP
        $2C
:1172   EQU     *
        ((:1173-*)&$3FFF)+$4000
        $34
        OPLSMI
        $30
        FCC       ''
        '1+$80
        $2F
:1173   EQU     *
        $2C
        $30
        FCC       'F'
        ':+$80
        $32,1
SETHERE EQU     .SC
.SC      SET     .SC+1
        SETHERE
        $2F
OPFOR   EQU     .SC
.SC      SET     .SC+1
        OPFOR
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $28,1,SCALAR
        ((:1174-*)&$3FFF)+$8000
SADDR    EQU     .SC
.SC      SET     .SC+1
        SADDR
        $32,1
        $2F
:1174   EQU     *
        ((:1175-*)&$3FFF)+$4000
        $28,1,COMMONSCALAR
        ((:1176-*)&$3FFF)+$8000
        DADDR
        $32,1
        $2F
:1176   EQU     *
:1175   EQU     *
        $2C
        DADDR
        $30
        FCC       'S'
        ':+$80
        $32,1
        $2F
        $3E,#UNTIL
        ((:1177-*)&$3FFF)+$8000
CONDITION        EQU     .SC
.SC      SET     .SC+1
        CONDITION
        $2C
OPBF     EQU     .SC
.SC      SET     .SC+1
        OPBF
        $2F
:1177   EQU     *
        ((:1178-*)&$3FFF)+$4000
        $3E,#WHILE
        ((:1179-*)&$3FFF)+$8000
        CONDITION
        $2C
OPBT     EQU     .SC
.SC      SET     .SC+1
        OPBT
        $2F
:1179   EQU     *
:1178   EQU     *
        ((:117A-*)&$3FFF)+$8000
        ADDR
        $30
        FCC       "*+"
        '5+$80
        $2F
OPEXITLOOP      EQU     .SC
.SC      SET     .SC+1
        OPEXITLOOP
        $2F
        ADDR
        $30
        FCC       'F'
        ':+$80
        $32,1
        $2F
:117A    EQU     *
        ((:117B-*)&$3FFF)+$4000
        $34
:117B    EQU     *
        $2C
FORCLAUSE EQU     .SC
.SC      SET     .SC+1
        FORCLAUSE
        ((:117C-*)&$3FFF)+$4000
        $3E,#DO
        ((:117D-*)&$3FFF)+$8000
        ENDOFLINE
        ((:117E-*)&$3FFF)+$8000
FORBLOCK  EQU     .SC
.SC      SET     .SC+1
        FORBLOCK
        $2C
:117E   EQU     *
        ((:117F-*)&$3FFF)+$4000
        CLAUSE
        ((:1180-*)&$3FFF)+$8000
        $3E,#END
        ((:1181-*)&$3FFF)+$4000
        $34
:1181   EQU     *
        $2C
:1180   EQU     *
:117F   EQU     *
        $2C
:117D   EQU     *
:117C    EQU     *
        $2C
        $26,1,INFORLOOP
OPNEXT  EQU     .SC
.SC      SET     .SC+1
        OPNEXT
        $2F
        ADDR
        $30
        FCC       'F'
        ':+$80
        $32,1
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:116D   EQU     *
        $29
.       SET     *
        ORG     FORBLOCK*2+.SA
        #.
        ORG     .
        LINELABEL
        ((:1182-*)&$3FFF)+$8000
        LABELLEDBLOCK
        $38
        +:1183-*-1
        +1
:1183   EQU     *
        ((:1184-*)&$3FFF)+$8000
        FORCLAUSE
        $2C
:1184   EQU     *
        ((:1185-*)&$3FFF)+$4000
        EXECUTABLESTMT
        ((:1186-*)&$3FFF)+$8000
        FORCLAUSE
        $2C
:1186   EQU     *
        ((:1185-*)&$3FFF)+$4000
        FORMATLINE
        ((:1187-*)&$3FFF)+$8000
        FORBLOCK
        $2C
:1187   EQU     *
        ((:1185-*)&$3FFF)+$4000
        ENDOFLINE
        ((:1188-*)&$3FFF)+$8000
        FORBLOCK
        $2C
:1188   EQU     *
        ((:1185-*)&$3FFF)+$4000
MUSTBENEXT       EQU     .SC
.SC      SET     .SC+1
        MUSTBENEXT
:1185   EQU     *
        $2C
:1182   EQU     *
        $2A
        EXECUTABLESTMT
        ((:1189-*)&$3FFF)+$8000
        FORCLAUSE
        $2C
:1189   EQU     *
        $2A
        INCLUDESTMT
        ((:118A-*)&$3FFF)+$8000
        FORBLOCK
        $2C
:118A    EQU     *
        $2A
        MUSTBENEXT
        $29
.       SET     *
        ORG     FORCLAUSE*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '\+$80
        ((:118B-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
        EXECUTABLESTMT
        ((:118C-*)&$3FFF)+$8000
        FORCLAUSE
        $2C
:118C    EQU     *
        ((:118D-*)&$3FFF)+$4000
        MUSTBENEXT
:118D   EQU     *
        $2C
:118B    EQU     *
        $2A
        ENDOFLINE
        ((:118E-*)&$3FFF)+$8000
        FORBLOCK
        $2C
:118E   EQU     *
        $29
.       SET     *
        ORG     MUSTBENEXT*2+.SA
        #.
        ORG     .
        $3E,#NEXT
        ((:118F-*)&$3FFF)+$8000
        +NID
        $2C
        $27,INFORLOOP
        ((:1190-*)&$3FFF)+$4000
        NOMATCHINGFOR
:1190   EQU     *
        $2C
:118F   EQU     *
        $2A
        $3E,#END
        $2A
        $2D
        FCC       'Missing NEX'
        'T+$80
        $29
.       SET     *
        ORG     CYCLESTMT*2+.SA
        #.
        ORG     .
        +NID
        ((:1191-*)&$3FFF)+$8000
        $27,INFORLOOP
        ((:1192-*)&$3FFF)+$4000
        NOMATCHINGFOR
:1192   EQU     *
        $2C
        OPNEXT
        $2F
        ADDR
        $30
        FCC       'F'
        ':+$80
        $32,1
        $2F
:1191   EQU     *
        $29
.       SET     *
        ORG     GOSUBSTMT*2+.SA
        #.
        ORG     .
        $3E,#POP
        ((:1193-*)&$3FFF)+$8000
        NUMEXP
        $2C
OPGPOP  EQU     .SC
.SC      SET     .SC+1
        OPGPOP
        $2F
:1193   EQU     *
        $2A
        $34
OPGSB    EQU     .SC
.SC      SET     .SC+1
        OPGSB
        $2F
        TARGET
        $2C
        $29
.       SET     *
        ORG     RETURNSTMT*2+.SA
        #.
        ORG     .
        $3E,#SUBROUTINE
        ((:1194-*)&$3FFF)+$8000
OPSUBRET EQU     .SC
.SC      SET     .SC+1
        OPSUBRET
        $2F
:1194   EQU     *
        $2A
NUMORSTREXP     EQU     .SC
.SC      SET     .SC+1
        NUMORSTREXP
        ((:1195-*)&$3FFF)+$8000
        OPFUNRET
        $2F
:1195   EQU     *
        $2A
        $34
OPRET   EQU     .SC
.SC      SET     .SC+1
        OPRET
        $2F
        $29
.       SET     *
        ORG     STOPSTMT*2+.SA
        #.
        ORG     .
OPSTP   EQU     .SC
.SC      SET     .SC+1
        OPSTP
        $2F
        $29
.       SET     *
        ORG     EXITSTMT*2+.SA
        #.
        ORG     .
        +NID
        ((:1196-*)&$3FFF)+$8000
        $27,INFORLOOP
        ((:1197-*)&$3FFF)+$8000
        OPEXITLOOP
        $2F
        ADDR
        $30
        FCC       'F'
        ':+$80
        $32,1
        $2F
:1197   EQU     *
        ((:1198-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:1199-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        $30
        FCC       'E'
        ':+$80
        $31
        $2F
:1199   EQU     *
        ((:1198-*)&$3FFF)+$4000
        NOMATCHINGFOR
:1198   EQU     *
        $2C
:1196   EQU     *
        $2A
        $3E,#SUBROUTINE
        ((:119A-*)&$3FFF)+$8000
        OPSUBRET
        $2F
:119A    EQU     *
        $2A
        +NUM
        ((:119B-*)&$3FFF)+$8000
        $27,ISLABEL
        ((:119C-*)&$3FFF)+$4000
        $2D
        FCC       "Must be loop labe"
        'l+$80
:119C    EQU     *
        $2C
        ADDR
        $30
        FCC       'E'
        ':+$80
        $31
        $2F
:119B    EQU     *
        $2A
        $34
        OPEXIT
        $2F
        $29
.       SET     *
        ORG     ONSTMT*2+.SA
        #.
        ORG     .
        $3E,#ERROR
        ((:119D-*)&$3FFF)+$8000
        $3E,#GOTO
        ((:119E-*)&$3FFF)+$8000
OPTRP   EQU     .SC
.SC      SET     .SC+1
        OPTRP
        $2F
        $21
        FCC       ''
        '0+$80
        ((:119F-*)&$3FFF)+$8000
        ADDR
        $30
        FCC       ''
        '0+$80
        $2F
:119F   EQU     *
        ((:11A0-*)&$3FFF)+$4000
        TARGET
:11A0    EQU     *
        $2C
:119E   EQU     *
        ((:11A1-*)&$3FFF)+$4000
        $3E,#DO
        ((:11A2-*)&$3FFF)+$8000
        OPTRP
        $2F
        ADDR
        $30
        FCC       '*+'
        '5+$80
        $2F
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        DOSTMT
        $2C
OPERRST EQU     .SC
.SC      SET     .SC+1
        OPERRST
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:11A2    EQU     *
:11A1    EQU     *
        $2C
:119D   EQU     *
        $2A
        NUMEXP
        ((:11A3-*)&$3FFF)+$8000
        $3E,#GOTO
        ((:11A4-*)&$3FFF)+$8000
OPON    EQU     .SC
.SC      SET     .SC+1
        OPON
        $2F
:11A4    EQU     *
        ((:11A5-*)&$3FFF)+$4000
        $3E,#GOSUB
        ((:11A6-*)&$3FFF)+$8000
OPONG   EQU     .SC
.SC      SET     .SC+1
        OPONG
        $2F
:11A6    EQU     *
:11A5    EQU     *
        $2C
        ADDR
        GENLBL
        $33,1
        $2F
        TARGET
        $2C
:11A7    EQU     *
        $21
        FCC       ''
        ',+$80
        ((:11A8-*)&$3FFF)+$8000
        TARGET
        $2C
:11A8    EQU     *
        ((:11A7-*)&$3FFF)+$4000
        $34
        GENLBL
        $33,1
        EQUHERE
        $2F
:11A3    EQU     *
        $29
.       SET     *
        ORG     REMARK*2+.SA
        #.
        ORG     .
:11A9    EQU     *
REMARKCHAR EQU     .SC
.SC      SET     .SC+1
        REMARKCHAR
        ((:11A9-*)&$3FFF)+$4000
        $34
        $29
.       SET     *
        ORG     REMARKCHAR*2+.SA
        #.
        ORG     .
        +ANYCHAR
        $29
.       SET     *
        ORG     CHAINSTMT*2+.SA
        #.
        ORG     .
        STREXP
        ((:11AA-*)&$3FFF)+$8000
OPCHAIN   EQU     .SC
.SC      SET     .SC+1
        OPCHAIN
        $2F
:11AA     EQU     *
        $29
.       SET     *
        ORG     CALLSTMT*2+.SA
        #.
        ORG     .
        $3E,#SYSCALL
        ((:11AB-*)&$3FFF)+$8000
        SYSCALLSTMT
        $2C
:11AB     EQU     *
        $2A
        +NID
        ((:11AC-*)&$3FFF)+$4000
        +SID
:11AC     EQU     *
        ((:11AD-*)&$3FFF)+$8000
        $27,TYPESUBROUTINE
        ((:11AE-*)&$3FFF)+$4000
        $27,FREFSUBROUTINE
        ((:11AE-*)&$3FFF)+$4000
        DEFINED
        ((:11AF-*)&$3FFF)+$4000
        DEFINEDSTRINGQ
:11AF    EQU     *
        ((:11B0-*)&$3FFF)+$8000
        DOUBLEDEFVAR
        $2C
:11B0    EQU     *
        ((:11AE-*)&$3FFF)+$4000
        $34
        $23,FREFSUBROUTINE
:11AE    EQU     *
        $2C
        ARGUMENTLIST
        $38
        +:11B1-*-1+$80
:11B1    EQU     *
        $2C
        OPCALL
        $2F
        ADDR
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $2F
:11AD    EQU     *
        $29
.       SET     *
        ORG     ARGUMENTLIST*2+.SA
        #.
        ORG     .
        GENLBL
        $33,1
        $30
        FCC       '       SET     '
        '0+$80
        $2F
        $21
        FCC       ''
        '(+$80
        ((:11B2-*)&$3FFF)+$8000
ARGUMENT EQU     .SC
.SC      SET     .SC+1
        ARGUMENT
        $2C
        GENLBL
        $33,1
        $30
        FCC       '       SET     1'
        '++$80
        GENLBL
        $33,1
        $2F
:11B3    EQU     *
        $21
        FCC       ''
        ',+$80
        ((:11B4-*)&$3FFF)+$8000
        ARGUMENT
        $2C
        GENLBL
        $33,1
        $30
        FCC       '       SET     1'
        '++$80
        GENLBL
        $33,1
        $2F
:11B4    EQU     *
        ((:11B3-*)&$3FFF)+$4000
        $34
        $21
        FCC       ''
        ')+$80
        $2C
:11B2    EQU     *
        ((:11B5-*)&$3FFF)+$4000
        $34
:11B5    EQU     *
        $2C
        $29
VARIABLEONLY       EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     VARIABLEONLY*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '++$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '-+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '!+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $3E,#XOR
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       '*'
        '*+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '*+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '/+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '&+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '^+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '<+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       '<'
        '=+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '=+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       '>'
        '=+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       ''
        '>+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $21
        FCC       '<'
        '>+$80
        $22
        ((:11B6-*)&$3FFF)+$8000
        $3E,#CAT
        $22
        ((:11B6-*)&$3FFF)+$8000
        $34
:11B6    EQU     *
        $29
PUSHPARAMTEMPADDR  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     PUSHPARAMTEMPADDR*2+.SA
        #.
        ORG     .
        GENLBL
        $33,1
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
        OPSTORE
        $2F
        DADDR
        GENLBL
        $33,1
        $2F
        OPLADDR
        $2F
        DADDR
        GENLBL
        $33,1
        $29
.       SET     *
        ORG     POKESTMT*2+.SA
        #.
        ORG     .
        NUMEXP
        ((:11B7-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
OPOKE   EQU     .SC
.SC      SET     .SC+1
        OPOKE
        $2F
:11B7    EQU     *
        $29
.       SET     *
        ORG     DEBUGSTMT*2+.SA
        #.
        ORG     .
OPDBG    EQU     .SC
.SC      SET     .SC+1
        OPDBG
        $2F
        $29
.       SET     *
        ORG     ERRSTMT*2+.SA
        #.
        ORG     .
        NUMEXP
        ((:11B8-*)&$3FFF)+$8000
OPERRCAUSE        EQU     .SC
.SC      SET     .SC+1
        OPERRCAUSE
        $2F
:11B8    EQU     *
        $2A
        $34
        OPERRST
        $2F
        $29
.       SET     *
        ORG     FUNCTIONDEFN*2+.SA
        #.
        ORG     .
        $3E,#DEF
        ((:11B9-*)&$3FFF)+$8000
        +NID
        ((:11BA-*)&$3FFF)+$8000
        DEFINED
        ((:11BB-*)&$3FFF)+$8000
        DOUBLEDEFVAR
        $2C
:11BB     EQU     *
        ((:11BC-*)&$3FFF)+$4000
        $34
        $23,FUNCTION
:11BC     EQU     *
        $2C
        PARAMETERDEFINITIONS
        $2C
        $21
        FCC       ''
        '=+$80
        ((:11BD-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $32,1
LOCALSUBFUNHEADER   EQU     .SC
.SC      SET     .SC+1
        LOCALSUBFUNHEADER
        $2F
        NUMEXP
        $2C
        OPFUNRET
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:11BD    EQU     *
        ((:11BE-*)&$3FFF)+$4000
ROUTINEBODY      EQU     .SC
.SC      SET     .SC+1
        ROUTINEBODY
        $38
        +:11BF-*-1
        +1
:11BF    EQU     *
:11BE    EQU     *
        $2C
:11BA     EQU     *
        ((:11C0-*)&$3FFF)+$4000
        +SID
        ((:11C1-*)&$3FFF)+$8000
        DEFINEDSTRINGQ
        ((:11C2-*)&$3FFF)+$8000
        DOUBLEDEFSTR
        $2C
:11C2    EQU     *
        ((:11C3-*)&$3FFF)+$4000
        $34
        $23,STRINGFUNCTION
:11C3    EQU     *
        $2C
        PARAMETERDEFINITIONS
        $2C
        $21
        FCC       ''
        '=+$80
        ((:11C4-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $32,1
        LOCALSUBFUNHEADER
        $2F
        STREXP
        $2C
        OPFUNRET
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:11C4    EQU     *
        ((:11C5-*)&$3FFF)+$4000
        ROUTINEBODY
        $38
        +:11C6-*-1
        +1
:11C6    EQU     *
:11C5    EQU     *
        $2C
:11C1    EQU     *
:11C0    EQU     *
        $2C
:11B9    EQU     *
        $29
.       SET     *
        ORG     SUBROUTINEDEFN*2+.SA
        #.
        ORG     .
        $3E,#SUBROUTINE
        ((:11C7-*)&$3FFF)+$8000
        +NID
        ((:11C8-*)&$3FFF)+$4000
        +SID
:11C8    EQU     *
        $2C
        $27,FREFSUBROUTINE
        ((:11C9-*)&$3FFF)+$8000
        $25,FREFSUBROUTINE
        $2C
:11C9    EQU     *
        ((:11CA-*)&$3FFF)+$4000
        DEFINED
        ((:11CB-*)&$3FFF)+$4000
        DEFINEDSTRINGQ
:11CB     EQU     *
        ((:11CC-*)&$3FFF)+$8000
        DOUBLEDEFVAR
        $2C
:11CC     EQU     *
        ((:11CA-*)&$3FFF)+$4000
        $34
:11CA     EQU     *
        $2C
        $23,TYPESUBROUTINE
        PARAMETERDEFINITIONS
        $2C
        ROUTINEBODY
        $38
        +:11CD-*-1
        +1
:11CD    EQU     *
        $2C
:11C7    EQU     *
        $29
.       SET     *
        ORG     ROUTINEBODY*2+.SA
        #.
        ORG     .
        $3E,#EXTERNAL
        ((:11CE-*)&$3FFF)+$8000
        RESDATA
        $30
        FCC       '-:NARGS*'
        '6+$80
        $2F
:11CE    EQU     *
        $2A
        ENDOFLINEOPTCOMMENT
        ((:11CF-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $32,1
        LOCALSUBFUNHEADER
        $2F
:11D0   EQU     *
        REMARKSTMT
        ((:11D1-*)&$3FFF)+$4000
        INCLUDESTMT
        ((:11D1-*)&$3FFF)+$4000
        $3E,#DIM
        ((:11D2-*)&$3FFF)+$8000
        DIMBODY
        $2C
:11D2   EQU     *
:11D1   EQU     *
        ((:11D0-*)&$3FFF)+$4000
        $34
ROUTINEBLOCK      EQU     .SC
.SC      SET     .SC+1
        ROUTINEBLOCK
        $2C
        OPSTP
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:11CF    EQU     *
        $29
.       SET     *
        ORG     ROUTINEBLOCK*2+.SA
        #.
        ORG     .
        LINELABEL
        ((:11D3-*)&$3FFF)+$8000
        LABELLEDBLOCK
        $38
        +:11D4-*-1
        +1
:11D4   EQU     *
        ((:11D5-*)&$3FFF)+$8000
        RESTOFCLAUSE
        $2C
        ENDOFLINE
        $2C
        ROUTINEBLOCK
        $2C
:11D5   EQU     *
        ((:11D6-*)&$3FFF)+$4000
        EXECUTABLELINE
        ((:11D7-*)&$3FFF)+$8000
        ROUTINEBLOCK
        $2C
:11D7   EQU     *
        ((:11D6-*)&$3FFF)+$4000
        FORMATLINE
        ((:11D8-*)&$3FFF)+$8000
        ROUTINEBLOCK
        $2C
:11D8   EQU     *
        ((:11D6-*)&$3FFF)+$4000
        ENDOFLINE
        ((:11D9-*)&$3FFF)+$8000
        ROUTINEBLOCK
        $2C
:11D9   EQU     *
        ((:11D6-*)&$3FFF)+$4000
        FUNCTIONDEFN
        ((:11DA-*)&$3FFF)+$8000
        ENDOFLINEOPTCOMMENT
        $2C
        ROUTINEBLOCK
        $2C
:11DA    EQU     *
        ((:11D6-*)&$3FFF)+$4000
        SUBROUTINEDEFN
        ((:11DB-*)&$3FFF)+$8000
        ENDOFLINEOPTCOMMENT
        $2C
        ROUTINEBLOCK
        $2C
:11DB    EQU     *
        ((:11D6-*)&$3FFF)+$4000
MUSTBEEND        EQU     .SC
.SC      SET     .SC+1
        MUSTBEEND
:11D6   EQU     *
        $2C
:11D3   EQU     *
        $2A
        EXECUTABLELINE
        ((:11DC-*)&$3FFF)+$8000
        ROUTINEBLOCK
        $2C
:11DC    EQU     *
        $2A
        FUNCTIONDEFN
        ((:11DD-*)&$3FFF)+$8000
        ENDOFLINE
        $2C
        ROUTINEBLOCK
        $2C
:11DD   EQU     *
        $2A
        SUBROUTINEDEFN
        ((:11DE-*)&$3FFF)+$8000
        ENDOFLINE
        $2C
        ROUTINEBLOCK
        $2C
:11DE   EQU     *
        $2A
        INCLUDESTMT
        ((:11DF-*)&$3FFF)+$8000
        ROUTINEBLOCK
        $2C
:11DF   EQU     *
        $2A
        MUSTBEEND
        $29
.       SET     *
        ORG     PARAMETERDEFINITIONS*2+.SA
        #.
        ORG     .
        $34
        $30
        FCC       ':NARGS  SET     '
        '0+$80
        $2F
        $21
        FCC       ''
        '(+$80
        ((:11E0-*)&$3FFF)+$8000
NEWPARAMETER      EQU     .SC
.SC      SET     .SC+1
        NEWPARAMETER
        $2C
        $30
        FCC       ':NARGS  SET     :NARGS+'
        '1+$80
        $2F
:11E1   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:11E2-*)&$3FFF)+$8000
        NEWPARAMETER
        $2C
        $30
        FCC       ':NARGS  SET     :NARGS+'
        '1+$80
        $2F
:11E2   EQU     *
        ((:11E1-*)&$3FFF)+$4000
        $34
        $21
        FCC       ''
        ')+$80
        $2C
:11E0   EQU     *
        ((:11E3-*)&$3FFF)+$4000
        $34
:11E3   EQU     *
        $2C
        $29
.       SET     *
        ORG     LOCALSUBFUNHEADER*2+.SA
        #.
        ORG     .
        $30
        FCC       '       JSR     $10'
        '6+$80
        $2F
        BYTE
        $30
        FCC       '$1'
        '4+$80
        $2F
        ADDR
        $30
        FCC       ''
        '0+$80
        $2F
        DADDR
        $30
        FCC       '::-'
        '6+$80
        $2F
        DADDR
        $30
        FCC       '.TNS'
        'S+$80
        $2F
        DADDR
        $30
        FCC       '.TNSS+6*.TNS'
        'V+$80
        $2F
        BYTE
        $30
        FCC       ':NARG'
        'S+$80
        $29
.       SET     *
        ORG     NEWPARAMETER*2+.SA
        #.
        ORG     .
        +NID
        ((:11E4-*)&$3FFF)+$8000
        DEFINED
        ((:11E5-*)&$3FFF)+$8000
        DOUBLEDEFVAR
        $2C
:11E5   EQU     *
        ((:11E6-*)&$3FFF)+$4000
        LB
        ((:11E7-*)&$3FFF)+$8000
        $21
        FCC       ''
        '*+$80
        $2C
        $21
        FCC       ''
        ',+$80
        ((:11E8-*)&$3FFF)+$8000
        $21
        FCC       ''
        '*+$80
        $2C
        RB
        $2C
        $23,ARRAY
:11E8   EQU     *
        ((:11E9-*)&$3FFF)+$4000
        RB
        ((:11EA-*)&$3FFF)+$8000
        $23,VECTOR
:11EA    EQU     *
:11E9   EQU     *
        $2C
:11E7   EQU     *
        ((:11E6-*)&$3FFF)+$4000
        $34
        $23,COMMONSCALAR
:11E6   EQU     *
        $2C
:11E4   EQU     *
        ((:11EB-*)&$3FFF)+$4000
        +SID
        ((:11EC-*)&$3FFF)+$8000
        DEFINEDSTRINGQ
        ((:11ED-*)&$3FFF)+$8000
        DOUBLEDEFSTR
        $2C
:11ED   EQU     *
        ((:11EE-*)&$3FFF)+$4000
        LB
        ((:11EF-*)&$3FFF)+$8000
        $21
        FCC       ""
        '*+$80
        $2C
        RB
        $2C
        $24,1,STRARRAY
:11EF   EQU     *
        ((:11EE-*)&$3FFF)+$4000
        $34
        $23,STRVAR
:11EE   EQU     *
        $2C
:11EC    EQU     *
:11EB    EQU     *
        ((:11F0-*)&$3FFF)+$8000
        $23,PARAMETER
        $31
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
:11F0   EQU     *
        $29
.       SET     *
        ORG     OPENSTMT*2+.SA
        #.
        ORG     .
CHNL     EQU     .SC
.SC      SET     .SC+1
        CHNL
        ((:11F1-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
        STREXP
        $2C
OPOPN   EQU     .SC
.SC      SET     .SC+1
        OPOPN
        $2F
:11F1   EQU     *
        $29
.       SET     *
        ORG     CREATESTMT*2+.SA
        #.
        ORG     .
        CHNL
        ((:11F2-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
        STREXP
        $2C
OPCREAT   EQU     .SC
.SC      SET     .SC+1
        OPCREAT
        $2F
:11F2   EQU     *
        $29
.       SET     *
        ORG     CLOSESTMT*2+.SA
        #.
        ORG     .
        CHNL
        ((:11F3-*)&$3FFF)+$8000
OPCLS    EQU     .SC
.SC      SET     .SC+1
        OPCLS
        $2F
:11F4   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:11F5-*)&$3FFF)+$8000
        CHNL
        $2C
        OPCLS
        $2F
:11F5   EQU     *
        ((:11F4-*)&$3FFF)+$4000
        $34
:11F3   EQU     *
        $29
.       SET     *
        ORG     DELETESTMT*2+.SA
        #.
        ORG     .
        STREXP
        ((:11F6-*)&$3FFF)+$8000
OPDEL   EQU     .SC
.SC      SET     .SC+1
        OPDEL
        $2F
:11F6   EQU     *
        $29
.       SET     *
        ORG     RENAMESTMT*2+.SA
        #.
        ORG     .
        STREXP
        ((:11F7-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
        STREXP
        $2C
OPREN   EQU     .SC
.SC      SET     .SC+1
        OPREN
        $2F
:11F7   EQU     *
        $29
.       SET     *
        ORG     POSITIONSTMT*2+.SA
        #.
        ORG     .
        CHNL
        ((:11F8-*)&$3FFF)+$8000
SETPOSITION     EQU     .SC
.SC      SET     .SC+1
        SETPOSITION
        ((:11F9-*)&$3FFF)+$4000
        $21
        FCC       ''
        ',+$80
        ((:11FA-*)&$3FFF)+$8000
        NUMEXP
        $2C
OPRESTR EQU     .SC
.SC      SET     .SC+1
        OPRESTR
        $2F
:11FA    EQU     *
:11F9   EQU     *
        $2C
:11F8   EQU     *
        $29
.       SET     *
        ORG     SYSCALLSTMT*2+.SA
        #.
        ORG     .
        CHNL
        ((:11FB-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
:11FB    EQU     *
        ((:11FC-*)&$3FFF)+$4000
        $34
OPZCHN   EQU     .SC
.SC      SET     .SC+1
        OPZCHN
        $2F
:11FC    EQU     *
        ((:11FD-*)&$3FFF)+$8000
SYSCALL1  EQU     .SC
.SC      SET     .SC+1
        SYSCALL1
        $2C
:11FD   EQU     *
        $29
.       SET     *
        ORG     SYSCALL1*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '(+$80
        ((:11FE-*)&$3FFF)+$8000
SYSCALL2  EQU     .SC
.SC      SET     .SC+1
        SYSCALL2
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
:11FE   EQU     *
        $2A
        SYSCALL2
        $29
.       SET     *
        ORG     SYSCALL2*2+.SA
        #.
        ORG     .
        STREXP
        ((:11FF-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:1200-*)&$3FFF)+$8000
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1201-*)&$3FFF)+$8000
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1202-*)&$3FFF)+$8000
        NUMEXP
        $2C
OPSYSCALL4ARG      EQU     .SC
.SC      SET     .SC+1
        OPSYSCALL4ARG
        $2F
:1202   EQU     *
        ((:1203-*)&$3FFF)+$4000
        $34
OPSYSCALL3ARG      EQU     .SC
.SC      SET     .SC+1
        OPSYSCALL3ARG
        $2F
:1203   EQU     *
        $2C
:1201   EQU     *
        ((:1204-*)&$3FFF)+$4000
        $34
OPSYSCALL2ARG      EQU     .SC
.SC      SET     .SC+1
        OPSYSCALL2ARG
        $2F
:1204   EQU     *
        $2C
:1200   EQU     *
        ((:1205-*)&$3FFF)+$4000
        $34
OPSYSCALL1ARG      EQU     .SC
.SC      SET     .SC+1
        OPSYSCALL1ARG
        $2F
:1205   EQU     *
        $2C
:11FF   EQU     *
        $29
.       SET     *
        ORG     CHNL*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '#+$80
        ((:1206-*)&$3FFF)+$8000
        NUMEXP
        $2C
OPCHNL   EQU     .SC
.SC      SET     .SC+1
        OPCHNL
        $2F
:1206   EQU     *
        $29
CHNLWOPTPOS      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     CHNLWOPTPOS*2+.SA
        #.
        ORG     .
        CHNL
        ((:1207-*)&$3FFF)+$8000
        SETPOSITION
        ((:1208-*)&$3FFF)+$4000
        $34
:1208   EQU     *
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
:1207   EQU     *
        $29
.       SET     *
        ORG     INPUTSTMT*2+.SA
        #.
        ORG     .
        $21
        FCC       '"'
        '"+$80
        ((:1209-*)&$3FFF)+$4000
        $21
        FCC       "'"
        ''+$80
:1209   EQU     *
        ((:120A-*)&$3FFF)+$8000
        OPZCHN
        $2F
INPUTSETPOS     EQU     .SC
.SC      SET     .SC+1
        INPUTSETPOS
        $2C
:120A    EQU     *
        ((:120B-*)&$3FFF)+$4000
STRCON   EQU     .SC
.SC      SET     .SC+1
        STRCON
        ((:120C-*)&$3FFF)+$8000
        OPZCHN
        $2F
        INPUTSETPOS
        $2C
OPPS    EQU     .SC
.SC      SET     .SC+1
        OPPS
        $2F
:120C    EQU     *
        ((:120B-*)&$3FFF)+$4000
        CHNLWOPTPOS
        ((:120B-*)&$3FFF)+$4000
        OPZCHN
        $2F
        INPUTSETPOS
        $2C
OPRMPT  EQU     .SC
.SC      SET     .SC+1
        OPRMPT
        $2F
:120B    EQU     *
        ((:120D-*)&$3FFF)+$8000
OPINL   EQU     .SC
.SC      SET     .SC+1
        OPINL
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
INLIST  EQU     .SC
.SC      SET     .SC+1
        INLIST
        $2C
        GENLBL
        $33,1
        EQUHERE
        $2F
:120D   EQU     *
        $29
.       SET     *
        ORG     INPUTSETPOS*2+.SA
        #.
        ORG     .
        SETPOSITION
        ((:120E-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        $2C
:120E   EQU     *
        $2A
        $34
        $29
.       SET     *
        ORG     INLIST*2+.SA
        #.
        ORG     .
INSVAR   EQU     .SC
.SC      SET     .SC+1
        INSVAR
        $2A
INNVAR   EQU     .SC
.SC      SET     .SC+1
        INNVAR
        ((:120F-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:1210-*)&$3FFF)+$8000
        INLIST
        $2C
:1210   EQU     *
        ((:1211-*)&$3FFF)+$4000
        $34
:1211   EQU     *
        $2C
:120F   EQU     *
        $29
.       SET     *
        ORG     INSVAR*2+.SA
        #.
        ORG     .
        OBVIOUSSTRINGTARGET
        ((:1212-*)&$3FFF)+$8000
OPINS   EQU     .SC
.SC      SET     .SC+1
        OPINS
        $2F
        OPSTS
        $2F
:1212   EQU     *
        $2A
        +SID
        ((:1213-*)&$3FFF)+$8000
        STRVARREF
        $38
        +:1214-*-1
        +1
:1214   EQU     *
        ((:1215-*)&$3FFF)+$8000
        LB
        ((:1216-*)&$3FFF)+$8000
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1217-*)&$3FFF)+$8000
        NUMEXP
        $2C
        RB
        $2C
        OPSS2
        $2F
        OPINS
        $2F
        OPSTS
        $2F
:1217   EQU     *
        ((:1218-*)&$3FFF)+$4000
        RB
        ((:1219-*)&$3FFF)+$8000
        OPSS1
        $2F
OPINP   EQU     .SC
.SC      SET     .SC+1
        OPINP
        $2F
        OPSTB
        $2F
:1219   EQU     *
:1218   EQU     *
        $2C
:1216   EQU     *
        ((:121A-*)&$3FFF)+$4000
        $34
        OPINS
        $2F
        OPSTS
        $2F
:121A    EQU     *
        $2C
:1215   EQU     *
        ((:121B-*)&$3FFF)+$4000
        $27,STRINGFUNCTION
        ((:121C-*)&$3FFF)+$8000
        $2D
        FCC       "Not Implemente"
        'd+$80
:121C    EQU     *
        ((:121B-*)&$3FFF)+$4000
        UDFSID
:121B    EQU     *
        $2C
:1213   EQU     *
        $29
.       SET     *
        ORG     INNVAR*2+.SA
        #.
        ORG     .
        +NID
        ((:121D-*)&$3FFF)+$8000
        $27,SCALAR
        ((:121E-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
        OPINP
        $2F
        OPSSCALAR
        $38
        +:121F-*-1
        +1
:121F   EQU     *
        $2C
:121E   EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:1221-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $31
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
        OPINP
        $2F
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1221   EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,COMMONSCALAR
        ((:1222-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $27,PARAMETER
        ((:1223-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
        OPINP
        $2F
        OPST
        $2F
:1223   EQU     *
        ((:1224-*)&$3FFF)+$4000
        $34
        OPINP
        $2F
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1224   EQU     *
        $2C
:1222   EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,VECTOR
        ((:1225-*)&$3FFF)+$8000
        ONESUBSCRIPT
        $2C
        $28,1,PARAMETER
        ((:1226-*)&$3FFF)+$8000
        OPVPA
        $2F
        DADDR
        $32,1
        $2F
        OPINP
        $2F
        OPST
        $2F
:1226   EQU     *
        ((:1227-*)&$3FFF)+$4000
        $34
        OPINP
        $2F
        OPVSS
        $2F
        DADDR
        $32,1
        $2F
:1227   EQU     *
        $2C
:1225   EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,ARRAY
        ((:1228-*)&$3FFF)+$8000
        TWOSUBSCRIPTS
        $2C
        $28,1,PARAMETER
        ((:1229-*)&$3FFF)+$8000
        OPAPA
        $2F
        DADDR
        $32,1
        $2F
        OPINP
        $2F
        OPST
        $2F
:1229   EQU     *
        ((:122A-*)&$3FFF)+$4000
        $34
        OPINP
        $2F
        OPASS
        $2F
        DADDR
        $32,1
        $2F
:122A    EQU     *
        $2C
:1228   EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:122B-*)&$3FFF)+$4000
        $27,FREFLABEL
:122B    EQU     *
        ((:122C-*)&$3FFF)+$8000
        CANTSTORELABEL
        $2C
:122C    EQU     *
        ((:1220-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:122D-*)&$3FFF)+$8000
        ARGUMENTLIST
        $38
        +:122E-*-1+$80
:122E   EQU     *
        $2C
        OPINP
        $2F
        PUSHPARAMTEMPADDR
        $2F
        OPCALL
        $2F
        ADDR
        $30
        FCC       'SE'
        'T+$80
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '+'
        '1+$80
        $2F
:122D   EQU     *
        ((:1220-*)&$3FFF)+$4000
        NOSUBSCRIPTS
        ((:122F-*)&$3FFF)+$8000
        $23,SCALAR
        $31
        ALLOCATESCALAR
        $2F
        OPINP
        $2F
        OPSSCALAR
        $38
        +:1230-*-1
        +1
:1230   EQU     *
        $2C
:122F   EQU     *
:1220   EQU     *
        $2C
:121D   EQU     *
        $2A
        $3E,#LEN
        ((:1231-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        DEFSID
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPINP
        $2F
        OPSSL
        $2F
:1231   EQU     *
        $2A
        $3E,#RND
        ((:1232-*)&$3FFF)+$8000
        OPINP
        $2F
        OPSETSEED
        $2F
:1232   EQU     *
        $29
.       SET     *
        ORG     PRINTSTMT*2+.SA
        #.
        ORG     .
        CHNL
        ((:1233-*)&$3FFF)+$8000
        SETPOSITION
        ((:1234-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:1235-*)&$3FFF)+$8000
USINGPART        EQU     .SC
.SC      SET     .SC+1
        USINGPART
        ((:1236-*)&$3FFF)+$4000
PRINTLIST       EQU     .SC
.SC      SET     .SC+1
        PRINTLIST
:1236   EQU     *
        $2C
:1235   EQU     *
        ((:1237-*)&$3FFF)+$4000
        $21
        FCC       ''
        ';+$80
        ((:1237-*)&$3FFF)+$4000
        $34
OPPCR    EQU     .SC
.SC      SET     .SC+1
        OPPCR
        $2F
:1237   EQU     *
        $2C
:1234   EQU     *
        ((:1238-*)&$3FFF)+$4000
        $21
        FCC       ''
        ',+$80
        ((:1239-*)&$3FFF)+$8000
        USINGPART
        ((:123A-*)&$3FFF)+$4000
        PRINTLIST
:123A    EQU     *
        $2C
:1239   EQU     *
        ((:1238-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:1238   EQU     *
        $2C
:1233   EQU     *
        $2A
        $34
        OPZCHN
        $2F
        SETPOSITION
        ((:123B-*)&$3FFF)+$8000
        $21
        FCC       ''
        ',+$80
        ((:123C-*)&$3FFF)+$8000
        USINGPART
        ((:123D-*)&$3FFF)+$4000
        PRINTLIST
:123D   EQU     *
        $2C
:123C    EQU     *
        ((:123E-*)&$3FFF)+$4000
        $21
        FCC       ''
        ';+$80
        ((:123E-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:123E   EQU     *
        $2C
:123B    EQU     *
        ((:123F-*)&$3FFF)+$4000
        USINGPART
        ((:123F-*)&$3FFF)+$4000
        PRINTLIST
        ((:123F-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:123F   EQU     *
        $2C
        $29
.       SET     *
        ORG     PRINTLIST*2+.SA
        #.
        ORG     .
        $3E,#TAB
        ((:1240-*)&$3FFF)+$8000
TERMGIVINGNUMERIC        EQU     .SC
.SC      SET     .SC+1
        TERMGIVINGNUMERIC
        $2C
OPTAB     EQU     .SC
.SC      SET     .SC+1
        OPTAB
        $2F
:1240   EQU     *
        ((:1241-*)&$3FFF)+$4000
        SETPOSITION
:1241   EQU     *
        ((:1242-*)&$3FFF)+$8000
        $21
        FCC       ''
        ';+$80
        $2C
        PRINTLIST
        ((:1243-*)&$3FFF)+$4000
        $34
:1243   EQU     *
        $2C
:1242   EQU     *
        $2A
        NUMORSTREXP
        ((:1244-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:1245-*)&$3FFF)+$8000
PRINTV  EQU     .SC
.SC      SET     .SC+1
        PRINTV
        $2C
:1245   EQU     *
        ((:1246-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:1247-*)&$3FFF)+$8000
PRINTSTRING     EQU     .SC
.SC      SET     .SC+1
        PRINTSTRING
        $2C
:1247   EQU     *
:1246   EQU     *
        $2C
:1244   EQU     *
        $29
.       SET     *
        ORG     SETPOSITION*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '@+$80
        ((:1248-*)&$3FFF)+$8000
        NUMEXP
        $2C
        OPRESTR
        $2F
:1248   EQU     *
        $29
.       SET     *
        ORG     PRINTSTRING*2+.SA
        #.
        ORG     .
        OPPS
        $2F
        $21
        FCC       ''
        ',+$80
        ((:1249-*)&$3FFF)+$8000
OPTABCOL   EQU     .SC
.SC      SET     .SC+1
        OPTABCOL
        $2F
        PRINTLIST
        ((:124A-*)&$3FFF)+$4000
        $34
:124A    EQU     *
        $2C
:1249   EQU     *
        ((:124B-*)&$3FFF)+$4000
        $21
        FCC       ''
        ';+$80
        ((:124C-*)&$3FFF)+$8000
        PRINTLIST
        ((:124D-*)&$3FFF)+$4000
        $34
:124D   EQU     *
        $2C
:124C    EQU     *
        ((:124B-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:124B    EQU     *
        $2C
        $29
.       SET     *
        ORG     PRINTV*2+.SA
        #.
        ORG     .
OPPV    EQU     .SC
.SC      SET     .SC+1
        OPPV
        $2F
        $21
        FCC       ''
        ',+$80
        ((:124E-*)&$3FFF)+$8000
        OPTABCOL
        $2F
        PRINTLIST
        ((:124F-*)&$3FFF)+$4000
        $34
:124F   EQU     *
        $2C
:124E   EQU     *
        ((:1250-*)&$3FFF)+$4000
        $21
        FCC       ''
        ';+$80
        ((:1251-*)&$3FFF)+$8000
OPPSP   EQU     .SC
.SC      SET     .SC+1
        OPPSP
        $2F
        PRINTLIST
        ((:1252-*)&$3FFF)+$4000
        $34
:1252   EQU     *
        $2C
:1251   EQU     *
        ((:1250-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:1250   EQU     *
        $2C
        $29
.       SET     *
        ORG     USINGPART*2+.SA
        #.
        ORG     .
        $3E,#USING
        ((:1253-*)&$3FFF)+$8000
        STREXP
        ((:1254-*)&$3FFF)+$8000
OPUSE   EQU     .SC
.SC      SET     .SC+1
        OPUSE
        $2F
:1254   EQU     *
        ((:1255-*)&$3FFF)+$4000
        $34
OPUSL   EQU     .SC
.SC      SET     .SC+1
        OPUSL
        $2F
        TARGET
        $2C
:1255   EQU     *
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1256-*)&$3FFF)+$8000
USINGOBJECT       EQU     .SC
.SC      SET     .SC+1
        USINGOBJECT
        $2C
:1257   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1258-*)&$3FFF)+$8000
        USINGOBJECT
        $2C
:1258   EQU     *
        ((:1257-*)&$3FFF)+$4000
        $34
:1256   EQU     *
        ((:1259-*)&$3FFF)+$4000
        $34
:1259   EQU     *
        $2C
        $21
        FCC       ''
        ';+$80
        ((:125A-*)&$3FFF)+$8000
OPPRUS  EQU     .SC
.SC      SET     .SC+1
        OPPRUS
        $2F
:125A    EQU     *
        ((:125B-*)&$3FFF)+$4000
        $34
        OPPCR
        $2F
:125B    EQU     *
        $2C
:1253   EQU     *
        $29
.       SET     *
        ORG     USINGOBJECT*2+.SA
        #.
        ORG     .
        NUMORSTREXP
        ((:125C-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:125D-*)&$3FFF)+$8000
        OPPV
        $2F
:125D   EQU     *
        ((:125E-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:125F-*)&$3FFF)+$8000
        OPPS
        $2F
:125F   EQU     *
:125E   EQU     *
        $2C
:125C    EQU     *
        $29
.       SET     *
        ORG     READSTMT*2+.SA
        #.
        ORG     .
        CHNLWOPTPOS
        ((:1260-*)&$3FFF)+$8000
RDVAR    EQU     .SC
.SC      SET     .SC+1
        RDVAR
        $2C
:1261   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1262-*)&$3FFF)+$8000
        RDVAR
        $2C
:1262   EQU     *
        ((:1261-*)&$3FFF)+$4000
        $34
:1260   EQU     *
        $29
.       SET     *
        ORG     RDVAR*2+.SA
        #.
        ORG     .
        +NID
        ((:1263-*)&$3FFF)+$8000
        $27,SCALAR
        ((:1264-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
OPRV    EQU     .SC
.SC      SET     .SC+1
        OPRV
        $2F
        OPSSCALAR
        $38
        +:1265-*-1
        +1
:1265   EQU     *
        $2C
:1264   EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:1267-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $31
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
        OPRV
        $2F
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:1267   EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,COMMONSCALAR
        ((:1268-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        NOSUBSCRIPTS
        $2C
        $27,PARAMETER
        ((:1269-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
        OPRV
        $2F
        OPST
        $2F
:1269   EQU     *
        ((:126A-*)&$3FFF)+$4000
        $34
        OPRV
        $2F
        OPSTORE
        $2F
        DADDR
        $32,1
        $2F
:126A    EQU     *
        $2C
:1268   EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,VECTOR
        ((:126B-*)&$3FFF)+$8000
        ONESUBSCRIPT
        $2C
        $28,1,PARAMETER
        ((:126C-*)&$3FFF)+$8000
        OPVPA
        $2F
        DADDR
        $32,1
        $2F
        OPRV
        $2F
        OPST
        $2F
:126C    EQU     *
        ((:126D-*)&$3FFF)+$4000
        $34
        OPRV
        $2F
        OPVSS
        $2F
        DADDR
        $32,1
        $2F
:126D   EQU     *
        $2C
:126B    EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,ARRAY
        ((:126E-*)&$3FFF)+$8000
        TWOSUBSCRIPTS
        $2C
        $28,1,PARAMETER
        ((:126F-*)&$3FFF)+$8000
        OPAPA
        $2F
        DADDR
        $32,1
        $2F
        OPRV
        $2F
        OPST
        $2F
:126F   EQU     *
        ((:1270-*)&$3FFF)+$4000
        $34
        OPRV
        $2F
        OPASS
        $2F
        DADDR
        $32,1
        $2F
:1270   EQU     *
        $2C
:126E   EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:1271-*)&$3FFF)+$4000
        $27,FREFLABEL
:1271   EQU     *
        ((:1272-*)&$3FFF)+$8000
        CANTSTORELABEL
        $2C
:1272   EQU     *
        ((:1266-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:1273-*)&$3FFF)+$8000
        ARGUMENTLIST
        $38
        +:1274-*-1+$80
:1274   EQU     *
        $2C
        OPRV
        $2F
        PUSHPARAMTEMPADDR
        $2F
        OPCALL
        $2F
        ADDR
        $30
        FCC       'SE'
        'T+$80
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '+'
        '1+$80
        $2F
:1273   EQU     *
        ((:1266-*)&$3FFF)+$4000
        NOSUBSCRIPTS
        ((:1275-*)&$3FFF)+$8000
        $23,SCALAR
        $31
        ALLOCATESCALAR
        $2F
        OPRV
        $2F
        OPSSCALAR
        $38
        +:1276-*-1
        +1
:1276   EQU     *
        $2C
:1275   EQU     *
:1266   EQU     *
        $2C
:1263   EQU     *
        $2A
        +SID
        ((:1277-*)&$3FFF)+$8000
        STRVARREF
        $38
        +:1278-*-1
        +1
:1278   EQU     *
        ((:1279-*)&$3FFF)+$8000
        LB
        ((:127A-*)&$3FFF)+$8000
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:127B-*)&$3FFF)+$8000
        NUMEXP
        $2C
        RB
        $2C
        OPSS2
        $2F
OPRS    EQU     .SC
.SC      SET     .SC+1
        OPRS
        $2F
:127B    EQU     *
        ((:127C-*)&$3FFF)+$4000
        RB
        ((:127D-*)&$3FFF)+$8000
        OPSS1
        $2F
        OPRV
        $2F
        OPSTB
        $2F
:127D   EQU     *
:127C    EQU     *
        $2C
:127A    EQU     *
        ((:127E-*)&$3FFF)+$4000
        $34
        OPRS
        $2F
:127E   EQU     *
        $2C
:1279   EQU     *
        ((:127F-*)&$3FFF)+$4000
        $27,STRINGFUNCTION
        ((:1280-*)&$3FFF)+$8000
        ARGUMENTLIST
        $38
        +:1281-*-1+$80
:1281   EQU     *
        $2C
        $2D
        FCC       "HOW MUCH STRING SHOULD I READ?"
        '?+$80
        OPCALL
        $2F
        ADDR
        $30
        FCC       'SE'
        'T+$80
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '+'
        '1+$80
        $2F
:1280   EQU     *
        ((:127F-*)&$3FFF)+$4000
        UDFSID
:127F   EQU     *
        $2C
:1277   EQU     *
        $2A
        OBVIOUSSTRINGTARGET
        ((:1282-*)&$3FFF)+$8000
        OPRS
        $2F
:1282   EQU     *
        $2A
        $3E,#LEN
        ((:1283-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        DEFSID
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPRV
        $2F
        OPSSL
        $2F
:1283   EQU     *
        $2A
        $3E,#RND
        ((:1284-*)&$3FFF)+$8000
        OPRV
        $2F
        OPSETSEED
        $2F
:1284   EQU     *
        $29
.       SET     *
        ORG     WRITESTMT*2+.SA
        #.
        ORG     .
        CHNLWOPTPOS
        ((:1285-*)&$3FFF)+$8000
WRVAR    EQU     .SC
.SC      SET     .SC+1
        WRVAR
        $2C
:1286   EQU     *
        $21
        FCC       ''
        ',+$80
        ((:1287-*)&$3FFF)+$8000
        WRVAR
        $2C
:1287   EQU     *
        ((:1286-*)&$3FFF)+$4000
        $34
:1285   EQU     *
        $29
.       SET     *
        ORG     WRVAR*2+.SA
        #.
        ORG     .
        NUMORSTREXP
        ((:1288-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:1289-*)&$3FFF)+$8000
OPWV    EQU     .SC
.SC      SET     .SC+1
        OPWV
        $2F
:1289   EQU     *
        ((:128A-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:128B-*)&$3FFF)+$8000
OPWS    EQU     .SC
.SC      SET     .SC+1
        OPWS
        $2F
:128B    EQU     *
:128A    EQU     *
        $2C
:1288   EQU     *
        $29
.       SET     *
        ORG     WHILESTMT*2+.SA
        #.
        ORG     .
        GENLBL
        $33,1
        EQUHERE
        $2F
        CONDITION
        $2C
        $3E,#DO
        $2C
        OPBF
        $2F
        ADDR
        GENLBL
        $33,2
        $2F
        CLAUSE
        ((:128C-*)&$3FFF)+$8000
OPTEND  EQU     .SC
.SC      SET     .SC+1
        OPTEND
        $2C
:128C    EQU     *
        ((:128D-*)&$3FFF)+$4000
        ENDOFLINE
        ((:128E-*)&$3FFF)+$8000
DOBLOCK   EQU     .SC
.SC      SET     .SC+1
        DOBLOCK
        $2C
:128E   EQU     *
:128D   EQU     *
        $2C
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        GENLBL
        $33,2
        EQUHERE
        $2F
        $29
.       SET     *
        ORG     UNTILSTMT*2+.SA
        #.
        ORG     .
        GENLBL
        $33,1
        EQUHERE
        $2F
        CONDITION
        $2C
        $3E,#DO
        $2C
        OPBT
        $2F
        ADDR
        GENLBL
        $33,2
        $2F
        CLAUSE
        ((:128F-*)&$3FFF)+$8000
        OPTEND
        $2C
:128F   EQU     *
        ((:1290-*)&$3FFF)+$4000
        ENDOFLINE
        ((:1291-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:1291   EQU     *
:1290   EQU     *
        $2C
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        GENLBL
        $33,2
        EQUHERE
        $2F
        $29
.       SET     *
        ORG     DOSTMT*2+.SA
        #.
        ORG     .
        CLAUSE
        ((:1292-*)&$3FFF)+$8000
        OPTEND
        $2C
:1292   EQU     *
        $2A
        ENDOFLINE
        ((:1293-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:1293   EQU     *
        $29
.       SET     *
        ORG     DOBLOCK*2+.SA
        #.
        ORG     .
        LINELABEL
        ((:1294-*)&$3FFF)+$8000
        LABELLEDBLOCK
        $38
        +:1295-*-1
        +1
:1295   EQU     *
        ((:1296-*)&$3FFF)+$8000
        RESTOFCLAUSE
        $2C
        ENDOFLINE
        $2C
        DOBLOCK
        $2C
:1296   EQU     *
        ((:1297-*)&$3FFF)+$4000
        EXECUTABLELINE
        ((:1298-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:1298   EQU     *
        ((:1297-*)&$3FFF)+$4000
        FORMATLINE
        ((:1299-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:1299   EQU     *
        ((:1297-*)&$3FFF)+$4000
        ENDOFLINE
        ((:129A-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:129A    EQU     *
        ((:1297-*)&$3FFF)+$4000
        MUSTBEEND
:1297   EQU     *
        $2C
:1294   EQU     *
        $2A
        EXECUTABLELINE
        ((:129B-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:129B    EQU     *
        $2A
        INCLUDESTMT
        ((:129C-*)&$3FFF)+$8000
        DOBLOCK
        $2C
:129C    EQU     *
        $2A
        MUSTBEEND
        $29
.       SET     *
        ORG     MUSTBEEND*2+.SA
        #.
        ORG     .
        $3E,#END
        $2A
        $2D
        FCC       "Missing Block EN"
        'D+$80
        $29
.       SET     *
        ORG     REPEATSTMT*2+.SA
        #.
        ORG     .
        GENLBL
        $33,1
        EQUHERE
        $2F
        CLAUSE
        ((:129D-*)&$3FFF)+$4000
        ENDOFLINE
        ((:129E-*)&$3FFF)+$8000
REPEATBLOCK        EQU     .SC
.SC      SET     .SC+1
        REPEATBLOCK
        $2C
:129E   EQU     *
:129D   EQU     *
        $2C
        $3E,#UNLESS
        ((:129F-*)&$3FFF)+$8000
        CONDITION
        $2C
        OPBF
        $2F
        OPTEND
        $2C
:129F   EQU     *
        ((:12A0-*)&$3FFF)+$4000
        $3E,#WHEN
        ((:12A1-*)&$3FFF)+$8000
        CONDITION
        $2C
        OPBT
        $2F
        OPTEND
        $2C
:12A1    EQU     *
        ((:12A0-*)&$3FFF)+$4000
        OPTEND
        ((:12A2-*)&$3FFF)+$8000
        OPJMP
        $2F
:12A2    EQU     *
:12A0    EQU     *
        $2C
        ADDR
        GENLBL
        $33,1
        $2F
        $29
.       SET     *
        ORG     REPEATBLOCK*2+.SA
        #.
        ORG     .
        LINELABEL
        ((:12A3-*)&$3FFF)+$8000
        LABELLEDBLOCK
        $38
        +:12A4-*-1
        +1
:12A4    EQU     *
        ((:12A5-*)&$3FFF)+$8000
        RESTOFCLAUSE
        $2C
        ENDOFLINE
        $2C
        REPEATBLOCK
        $2C
:12A5    EQU     *
        ((:12A6-*)&$3FFF)+$4000
        EXECUTABLELINE
        ((:12A7-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12A7    EQU     *
        ((:12A6-*)&$3FFF)+$4000
        FORMATLINE
        ((:12A8-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12A8    EQU     *
        ((:12A6-*)&$3FFF)+$4000
        ENDOFLINE
        ((:12A9-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12A9    EQU     *
        ((:12A6-*)&$3FFF)+$4000
        $34
:12A6    EQU     *
        $2C
:12A3    EQU     *
        $2A
        EXECUTABLELINE
        ((:12AA-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12AA     EQU     *
        $2A
        INCLUDESTMT
        ((:12AB-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12AB     EQU     *
        $2A
        $34
        $29
.       SET     *
        ORG     OPTEND*2+.SA
        #.
        ORG     .
        $3E,#END
        $2A
        $34
        $29
.       SET     *
        ORG     IFSTMT*2+.SA
        #.
        ORG     .
        $3E,#ERROR
        ((:12AC-*)&$3FFF)+$8000
        $3E,#WHEN
        $2C
        OPTRP
        $2F
        ADDR
        GENLBL
        $33,3
        $2F
        CLAUSE
        ((:12AD-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
:12AD    EQU     *
        ((:12AE-*)&$3FFF)+$4000
        ENDOFLINE
        ((:12AF-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
:12AF    EQU     *
:12AE    EQU     *
        $2C
        OPTRP
        $2F
        ADDR
        $30
        FCC       ''
        '0+$80
        $2F
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $3E,#THEN
        $2C
        GENLBL
        $33,3
        OPTRP
        $2F
        ADDR
        $30
        FCC       ''
        '0+$80
        $2F
        LINENUM
        ((:12B0-*)&$3FFF)+$4000
THENELSELABELCHECK  EQU     .SC
.SC      SET     .SC+1
        THENELSELABELCHECK
:12B0    EQU     *
        ((:12B1-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        LABEL
        $31
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
UNCONDELSE       EQU     .SC
.SC      SET     .SC+1
        UNCONDELSE
        $2C
:12B1    EQU     *
        ((:12B2-*)&$3FFF)+$4000
        $3E,#ELN
        ((:12B3-*)&$3FFF)+$8000
        OPGOELN
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
        UNCONDELSE
        $2C
:12B3    EQU     *
        ((:12B2-*)&$3FFF)+$4000
        ENDOFLINE
        ((:12B4-*)&$3FFF)+$8000
IFBLOCK   EQU     .SC
.SC      SET     .SC+1
        IFBLOCK
        $38
        +:12B5-*-1+$80
:12B5    EQU     *
        $2C
:12B4    EQU     *
        ((:12B2-*)&$3FFF)+$4000
IFCLAUSE  EQU     .SC
.SC      SET     .SC+1
        IFCLAUSE
        $38
        +:12B6-*-1+$80
:12B6    EQU     *
:12B2    EQU     *
        $2C
:12AC     EQU     *
        $2A
        CONDITION
        ((:12B7-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
        $3E,#THEN
        $2C
        LINENUM
        ((:12B8-*)&$3FFF)+$4000
        THENELSELABELCHECK
:12B8    EQU     *
        ((:12B9-*)&$3FFF)+$8000
        OPBT
        $2F
        ADDR
        LABEL
        $31
        $2F
        UNCONDELSE
        $2C
:12B9    EQU     *
        ((:12BA-*)&$3FFF)+$4000
        $3E,#ELN
        ((:12BB-*)&$3FFF)+$8000
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        OPGOELN
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
        UNCONDELSE
        $2C
:12BB     EQU     *
        ((:12BA-*)&$3FFF)+$4000
        ENDOFLINE
        ((:12BC-*)&$3FFF)+$8000
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        IFBLOCK
        $38
        +:12BD-*-1+$80
:12BD    EQU     *
        $2C
:12BC     EQU     *
        ((:12BA-*)&$3FFF)+$4000
        $34
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        IFCLAUSE
        $38
        +:12BE-*-1+$80
:12BE    EQU     *
        $2C
:12BA     EQU     *
        $2C
:12B7    EQU     *
        $29
.       SET     *
        ORG     THENELSELABELCHECK*2+.SA
        #.
        ORG     .
        +THENELSELABEL
        ((:12BF-*)&$3FFF)+$8000
        $27,ISLABEL
        ((:12C0-*)&$3FFF)+$4000
        $27,FREFLABEL
        ((:12C0-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:12C1-*)&$3FFF)+$8000
        $25,COMMONSCALAR
        $2C
:12C1    EQU     *
        ((:12C0-*)&$3FFF)+$4000
        $27,SCALAR
        ((:12C2-*)&$3FFF)+$4000
        $27,COMMONSCALAR
        ((:12C2-*)&$3FFF)+$4000
        $27,VECTOR
        ((:12C2-*)&$3FFF)+$4000
        $27,ARRAY
        ((:12C2-*)&$3FFF)+$4000
        $27,STRARRAY
:12C2    EQU     *
        ((:12C3-*)&$3FFF)+$8000
        VARUSEDASLABEL
        $2C
:12C3    EQU     *
        ((:12C0-*)&$3FFF)+$4000
        $27,TYPESUBROUTINE
        ((:12C4-*)&$3FFF)+$4000
        $27,FREFSUBROUTINE
        ((:12C4-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:12C4-*)&$3FFF)+$4000
        $27,STRINGFUNCTION
:12C4    EQU     *
        ((:12C5-*)&$3FFF)+$8000
        $2D
        FCC       "Can't use function or subroutine name as labe"
        'l+$80
:12C5    EQU     *
        ((:12C0-*)&$3FFF)+$4000
        $34
        $23,FREFLABEL
:12C0    EQU     *
        $2C
:12BF    EQU     *
        $29
.       SET     *
        ORG     IFBLOCK*2+.SA
        #.
        ORG     .
        REPEATBLOCK
        ((:12C6-*)&$3FFF)+$8000
        $3E,#ELSE
        ((:12C7-*)&$3FFF)+$8000
ELSEJMP EQU     .SC
.SC      SET     .SC+1
        ELSEJMP
        $38
        +:12C8-*-1+$80
:12C8    EQU     *
        $2F
ELSEBODY EQU     .SC
.SC      SET     .SC+1
        ELSEBODY
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
:12C7    EQU     *
        ((:12C9-*)&$3FFF)+$4000
        $3E,#ELSEIF
        ((:12CA-*)&$3FFF)+$8000
        ELSEJMP
        $38
        +:12CB-*-1+$80
:12CB     EQU     *
        $2F
        IFSTMT
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
:12CA     EQU     *
        ((:12C9-*)&$3FFF)+$4000
MUSTBEFI EQU     .SC
.SC      SET     .SC+1
        MUSTBEFI
        ((:12CC-*)&$3FFF)+$8000
        GENLBL
        $33,1
        EQUHERE
        $2F
:12CC     EQU     *
:12C9    EQU     *
        $2C
:12C6    EQU     *
        $29
.       SET     *
        ORG     IFCLAUSE*2+.SA
        #.
        ORG     .
        CLAUSE
        ((:12CD-*)&$3FFF)+$8000
ELSEKEYWORD     EQU     .SC
.SC      SET     .SC+1
        ELSEKEYWORD
        ((:12CE-*)&$3FFF)+$8000
        ELSEJMP
        $38
        +:12CF-*-1+$80
:12CF    EQU     *
        $2F
        ELSEBODY
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
:12CE    EQU     *
        ((:12D0-*)&$3FFF)+$4000
ELSEIFKEYWORD   EQU     .SC
.SC      SET     .SC+1
        ELSEIFKEYWORD
        ((:12D1-*)&$3FFF)+$8000
        ELSEJMP
        $38
        +:12D2-*-1+$80
:12D2   EQU     *
        $2F
        IFSTMT
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
:12D1   EQU     *
        ((:12D0-*)&$3FFF)+$4000
OPTIONALFI       EQU     .SC
.SC      SET     .SC+1
        OPTIONALFI
        ((:12D3-*)&$3FFF)+$8000
        GENLBL
        $33,1
        EQUHERE
        $2F
:12D3   EQU     *
:12D0   EQU     *
        $2C
:12CD    EQU     *
        $29
.       SET     *
        ORG     ELSEJMP*2+.SA
        #.
        ORG     .
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,2
        $2F
        GENLBL
        $33,1
        EQUHERE
        $29
.       SET     *
        ORG     UNCONDELSE*2+.SA
        #.
        ORG     .
        ELSEKEYWORD
        ((:12D4-*)&$3FFF)+$8000
        ELSEBODY
        $2C
:12D4   EQU     *
        $2A
        ELSEIFKEYWORD
        ((:12D5-*)&$3FFF)+$8000
        IFSTMT
        $2C
:12D5   EQU     *
        $2A
        OPTIONALFI
        $29
.       SET     *
        ORG     ELSEKEYWORD*2+.SA
        #.
        ORG     .
        $3E,#ELSE
        $2A
        +LOOKAHEADELSE
        ((:12D6-*)&$3FFF)+$8000
        $3E,#ELSE
        $2C
:12D6   EQU     *
        $29
.       SET     *
        ORG     ELSEIFKEYWORD*2+.SA
        #.
        ORG     .
        $3E,#ELSEIF
        $2A
        +LOOKAHEADELSEIF
        ((:12D7-*)&$3FFF)+$8000
        $3E,#ELSEIF
        $2C
:12D7   EQU     *
        $29
.       SET     *
        ORG     ELSEBODY*2+.SA
        #.
        ORG     .
        LINENUM
        ((:12D8-*)&$3FFF)+$4000
        THENELSELABELCHECK
:12D8   EQU     *
        ((:12D9-*)&$3FFF)+$8000
        OPJMP
        $2F
        ADDR
        LABEL
        $31
        $2F
        OPTIONALFI
        $2C
:12D9   EQU     *
        $2A
        $3E,#ELN
        ((:12DA-*)&$3FFF)+$8000
        OPGOELN
        $2F
        OPTIONALFI
        $2C
:12DA    EQU     *
        $2A
        ENDOFLINE
        ((:12DB-*)&$3FFF)+$8000
        REPEATBLOCK
        $2C
        MUSTBEFI
        $2C
:12DB    EQU     *
        $2A
        CLAUSE
        ((:12DC-*)&$3FFF)+$8000
        OPTIONALFI
        $2C
:12DC    EQU     *
        $29
.       SET     *
        ORG     OPTIONALFI*2+.SA
        #.
        ORG     .
        $3E,#FI
        $2A
        $34
        $29
.       SET     *
        ORG     MUSTBEFI*2+.SA
        #.
        ORG     .
        $3E,#FI
        $2A
        $2D
        FCC       "Missing F"
        'I+$80
        $29
.       SET     *
        ORG     CONDITION*2+.SA
        #.
        ORG     .
        $3A,RESULT,BOOLTYPE
EXPRESSION      EQU     .SC
.SC      SET     .SC+1
        EXPRESSION
        $2C
        $29
.       SET     *
        ORG     NUMEXP*2+.SA
        #.
        ORG     .
NOTENDOFSTMT    EQU     .SC
.SC      SET     .SC+1
        NOTENDOFSTMT
        ((:12DD-*)&$3FFF)+$8000
        $3A,RESULT,NUMERICTYPE
        EXPRESSION
        $2C
:12DD   EQU     *
        $29
.       SET     *
        ORG     NOTENDOFSTMT*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '\+$80
        $22
        ((:12DE-*)&$3FFF)+$8000
        $21
        +$80+$D
        $22
        ((:12DE-*)&$3FFF)+$8000
        $3E,#ELSE
        $22
        ((:12DE-*)&$3FFF)+$8000
        $3E,#ELSEIF
        $22
        ((:12DE-*)&$3FFF)+$8000
        $3E,#END
        $22
        ((:12DE-*)&$3FFF)+$8000
        $3E,#FI
        $22
        ((:12DE-*)&$3FFF)+$8000
        $34
:12DE   EQU     *
        $29
.       SET     *
        ORG     STREXP*2+.SA
        #.
        ORG     .
NOTENDOFSTMTORTARGET     EQU     .SC
.SC      SET     .SC+1
        NOTENDOFSTMTORTARGET
        ((:12DF-*)&$3FFF)+$8000
        $3A,RESULT,STRINGTYPE
        EXPRESSION
        $2C
:12DF   EQU     *
        $29
.       SET     *
        ORG     NOTENDOFSTMTORTARGET*2+.SA
        #.
        ORG     .
        +NID
        $22
        ((:12E0-*)&$3FFF)+$8000
        +NUM
        $22
        ((:12E0-*)&$3FFF)+$8000
        NOTENDOFSTMT
        ((:12E0-*)&$3FFF)+$8000
:12E0   EQU     *
        $29
.       SET     *
        ORG     NUMORSTREXP*2+.SA
        #.
        ORG     .
        NOTENDOFSTMT
        ((:12E1-*)&$3FFF)+$8000
        $3A,RESULT,NUMORSTRTYPE
        EXPRESSION
        $2C
:12E1   EQU     *
        $29
.       SET     *
        ORG     ARGUMENT*2+.SA
        #.
        ORG     .
        $3A,RESULT,ADDRESSANYTYPE
        EXPRESSION
        $2C
        $29
.       SET     *
        ORG     NUMERICARGUMENT*2+.SA
        #.
        ORG     .
        $3A,RESULT,ADDRESSSCALARTYPE
        EXPRESSION
        $2C
        $29
.       SET     *
        ORG     STRINGARGUMENT*2+.SA
        #.
        ORG     .
        $3A,RESULT,STRINGTYPE
        EXPRESSION
        $2C
        $29
.       SET     *
        ORG     EXPRESSION*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMORBOOLTYPE
        ((:12E2-*)&$3FFF)+$8000
CONJUNCT  EQU     .SC
.SC      SET     .SC+1
        CONJUNCT
        $2C
OPTDISJUNCT      EQU     .SC
.SC      SET     .SC+1
        OPTDISJUNCT
        $2C
:12E2   EQU     *
        $2A
        $3B,RESULT,NUMERICTYPE
        ((:12E3-*)&$3FFF)+$8000
        $3A,RESULT,NUMORBOOLTYPE
        CONJUNCT
        $2C
RESTOFDISJUNCTRETURNNUMERIC       EQU     .SC
.SC      SET     .SC+1
        RESTOFDISJUNCTRETURNNUMERIC
        $2C
:12E3   EQU     *
        $2A
        $3B,RESULT,BOOLTYPE
        ((:12E4-*)&$3FFF)+$8000
        CONJUNCT
        $2C
        OPTDISJUNCT
        $2C
:12E4   EQU     *
        $2A
        $3B,RESULT,NUMBOOLORSTRTYPE
        ((:12E5-*)&$3FFF)+$8000
        CONJUNCT
        $2C
        OPTDISJUNCT
        $2C
:12E5   EQU     *
        $2A
        $3B,RESULT,NUMORSTRTYPE
        ((:12E6-*)&$3FFF)+$8000
        $3A,RESULT,NUMBOOLORSTRTYPE
        CONJUNCT
        $2C
        $3B,RESULT,STRINGTYPE
        ((:12E7-*)&$3FFF)+$4000
        RESTOFDISJUNCTRETURNNUMERIC
:12E7   EQU     *
        $2C
:12E6   EQU     *
        $2A
        $3B,RESULT,STRINGTYPE
        ((:12E8-*)&$3FFF)+$8000
        CONJUNCT
        $2C
:12E8   EQU     *
        $2A
        $3B,RESULT,ADDRESSANYTYPE
        ((:12E9-*)&$3FFF)+$8000
        CONJUNCT
        $2C
        $3B,RESULT,NUMERICTYPE
        ((:12EA-*)&$3FFF)+$4000
        $3B,RESULT,BOOLTYPE
:12EA    EQU     *
        ((:12EB-*)&$3FFF)+$8000
        RESTOFDISJUNCTRETURNNUMERIC
        $2C
        PUSHPARAMTEMPADDR
        $2F
        $3A,RESULT,ADDRESSSCALARTYPE
:12EB    EQU     *
        ((:12EC-*)&$3FFF)+$4000
        $34
:12EC    EQU     *
        $2C
:12E9   EQU     *
        $2A
        $3B,RESULT,ADDRESSSCALARTYPE
        ((:12ED-*)&$3FFF)+$8000
        CONJUNCT
        $2C
        $3B,RESULT,ADDRESSSCALARTYPE
        ((:12EE-*)&$3FFF)+$4000
        RESTOFDISJUNCTRETURNNUMERIC
        ((:12EF-*)&$3FFF)+$8000
        PUSHPARAMTEMPADDR
        $2F
        $3A,RESULT,ADDRESSSCALARTYPE
:12EF   EQU     *
:12EE   EQU     *
        $2C
:12ED   EQU     *
        $29
.       SET     *
        ORG     OPTDISJUNCT*2+.SA
        #.
        ORG     .
RESTOFDISJUNCT   EQU     .SC
.SC      SET     .SC+1
        RESTOFDISJUNCT
        $2A
        $34
        $29
.       SET     *
        ORG     RESTOFDISJUNCT*2+.SA
        #.
        ORG     .
        $3E,#OR
        ((:12F0-*)&$3FFF)+$8000
CONVERTNUMERICTOBOOL       EQU     .SC
.SC      SET     .SC+1
        CONVERTNUMERICTOBOOL
        $2C
        OPBT
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        CONJUNCT
        $2C
:12F1   EQU     *
        $3E,#OR
        ((:12F2-*)&$3FFF)+$8000
        OPBT
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        CONJUNCT
        $2C
:12F2   EQU     *
        ((:12F1-*)&$3FFF)+$4000
        $34
        GENLBL
        $33,1
        EQUHERE
        $2F
:12F0   EQU     *
        $29
.       SET     *
        ORG     CONVERTNUMERICTOBOOL*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMERICTYPE
        ((:12F3-*)&$3FFF)+$8000
        OPLSMI
        $30
        FCC       ''
        '0+$80
        $2F
OPNE    EQU     .SC
.SC      SET     .SC+1
        OPNE
        $2F
        $3A,RESULT,BOOLTYPE
:12F3   EQU     *
        ((:12F4-*)&$3FFF)+$4000
        $3B,RESULT,BOOLTYPE
:12F4   EQU     *
        $29
.       SET     *
        ORG     RESTOFDISJUNCTRETURNNUMERIC*2+.SA
        #.
        ORG     .
        RESTOFDISJUNCT
        ((:12F5-*)&$3FFF)+$8000
OPLBOOL  EQU     .SC
.SC      SET     .SC+1
        OPLBOOL
        $2F
        $3A,RESULT,NUMERICTYPE
:12F5   EQU     *
        $2A
        $3B,RESULT,NUMERICTYPE
        $2A
        $3B,RESULT,BOOLTYPE
        ((:12F6-*)&$3FFF)+$8000
        OPLBOOL
        $2F
        $3A,RESULT,NUMERICTYPE
:12F6   EQU     *
        $29
.       SET     *
        ORG     CONJUNCT*2+.SA
        #.
        ORG     .
RELATION EQU     .SC
.SC      SET     .SC+1
        RELATION
        ((:12F7-*)&$3FFF)+$8000
        $3E,#AND
        ((:12F8-*)&$3FFF)+$8000
        CONVERTNUMERICTOBOOL
        $2C
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        RELATION
        $2C
:12F9   EQU     *
        $3E,#AND
        ((:12FA-*)&$3FFF)+$8000
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        RELATION
        $2C
:12FA    EQU     *
        ((:12F9-*)&$3FFF)+$4000
        $34
        GENLBL
        $33,1
        EQUHERE
        $2F
:12F8   EQU     *
        ((:12FB-*)&$3FFF)+$4000
        $34
:12FB    EQU     *
        $2C
:12F7   EQU     *
        $29
.       SET     *
        ORG     RELATION*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMORBOOLTYPE
        ((:12FC-*)&$3FFF)+$8000
        $3A,RESULT,NUMBOOLORSTRTYPE
SUM     EQU     .SC
.SC      SET     .SC+1
        SUM
        $2C
        $3B,RESULT,BOOLTYPE
        ((:12FD-*)&$3FFF)+$4000
RESTOFRELATION   EQU     .SC
.SC      SET     .SC+1
        RESTOFRELATION
        ((:12FD-*)&$3FFF)+$4000
        $3B,RESULT,NUMERICTYPE
        ((:12FD-*)&$3FFF)+$4000
WRONGTYPE       EQU     .SC
.SC      SET     .SC+1
        WRONGTYPE
:12FD   EQU     *
        $2C
:12FC    EQU     *
        $2A
        $3B,RESULT,BOOLTYPE
        ((:12FE-*)&$3FFF)+$8000
        $3A,RESULT,NUMBOOLORSTRTYPE
        SUM
        $2C
        $3B,RESULT,BOOLTYPE
        ((:12FF-*)&$3FFF)+$4000
        RESTOFRELATION
        ((:12FF-*)&$3FFF)+$4000
        CONVERTNUMERICTOBOOL
        ((:12FF-*)&$3FFF)+$4000
        WRONGTYPE
:12FF   EQU     *
        $2C
:12FE   EQU     *
        $2A
        $3B,RESULT,NUMBOOLORSTRTYPE
        ((:1300-*)&$3FFF)+$8000
        SUM
        $2C
OPTRELATION      EQU     .SC
.SC      SET     .SC+1
        OPTRELATION
        $2C
:1300   EQU     *
        $2A
        $3B,RESULT,STRINGTYPE
        ((:1301-*)&$3FFF)+$8000
        SUM
        $2C
:1301   EQU     *
        $2A
        $3B,RESULT,ADDRESSANYTYPE
        ((:1302-*)&$3FFF)+$8000
        SUM
        $2C
        OPTRELATION
        $2C
:1302   EQU     *
        $2A
        $3B,RESULT,ADDRESSSCALARTYPE
        ((:1303-*)&$3FFF)+$8000
        SUM
        $2C
        OPTRELATION
        $2C
:1303   EQU     *
        $29
.       SET     *
        ORG     OPTRELATION*2+.SA
        #.
        ORG     .
        RESTOFRELATION
        $2A
        $34
        $29
.       SET     *
        ORG     RESTOFRELATION*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '=+$80
        ((:1304-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:1305-*)&$3FFF)+$8000
        SUM
        $2C
OPEQ    EQU     .SC
.SC      SET     .SC+1
        OPEQ
        $2F
:1305   EQU     *
        ((:1306-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:1307-*)&$3FFF)+$8000
        SUM
        $2C
OPSEQ   EQU     .SC
.SC      SET     .SC+1
        OPSEQ
        $2F
:1307   EQU     *
:1306   EQU     *
        $2C
:1304   EQU     *
        ((:1308-*)&$3FFF)+$4000
        $21
        FCC       '<'
        '>+$80
        ((:1309-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:130A-*)&$3FFF)+$8000
        SUM
        $2C
        OPNE
        $2F
:130A    EQU     *
        ((:130B-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:130C-*)&$3FFF)+$8000
        SUM
        $2C
OPSNE   EQU     .SC
.SC      SET     .SC+1
        OPSNE
        $2F
:130C    EQU     *
:130B    EQU     *
        $2C
:1309   EQU     *
        ((:1308-*)&$3FFF)+$4000
        $21
        FCC       '<'
        '=+$80
        ((:130D-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:130E-*)&$3FFF)+$8000
        SUM
        $2C
OPLE    EQU     .SC
.SC      SET     .SC+1
        OPLE
        $2F
:130E   EQU     *
        ((:130F-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:1310-*)&$3FFF)+$8000
        SUM
        $2C
OPSLE   EQU     .SC
.SC      SET     .SC+1
        OPSLE
        $2F
:1310   EQU     *
:130F   EQU     *
        $2C
:130D   EQU     *
        ((:1308-*)&$3FFF)+$4000
        $21
        FCC       ''
        '<+$80
        ((:1311-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:1312-*)&$3FFF)+$8000
        SUM
        $2C
OPLT    EQU     .SC
.SC      SET     .SC+1
        OPLT
        $2F
:1312   EQU     *
        ((:1313-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:1314-*)&$3FFF)+$8000
        SUM
        $2C
OPSLT   EQU     .SC
.SC      SET     .SC+1
        OPSLT
        $2F
:1314   EQU     *
:1313   EQU     *
        $2C
:1311   EQU     *
        ((:1308-*)&$3FFF)+$4000
        $21
        FCC       '>'
        '=+$80
        ((:1315-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:1316-*)&$3FFF)+$8000
        SUM
        $2C
OPGE    EQU     .SC
.SC      SET     .SC+1
        OPGE
        $2F
:1316   EQU     *
        ((:1317-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:1318-*)&$3FFF)+$8000
        SUM
        $2C
OPSGE   EQU     .SC
.SC      SET     .SC+1
        OPSGE
        $2F
:1318   EQU     *
:1317   EQU     *
        $2C
:1315   EQU     *
        ((:1308-*)&$3FFF)+$4000
        $21
        FCC       ''
        '>+$80
        ((:1319-*)&$3FFF)+$8000
        $3B,RESULT,NUMERICTYPE
        ((:131A-*)&$3FFF)+$8000
        SUM
        $2C
OPGT    EQU     .SC
.SC      SET     .SC+1
        OPGT
        $2F
:131A    EQU     *
        ((:131B-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:131C-*)&$3FFF)+$8000
        SUM
        $2C
OPSGT   EQU     .SC
.SC      SET     .SC+1
        OPSGT
        $2F
:131C    EQU     *
:131B    EQU     *
        $2C
:1319   EQU     *
:1308   EQU     *
        ((:131D-*)&$3FFF)+$8000
        $3A,RESULT,BOOLTYPE
:131D   EQU     *
        $29
CONVERTBOOLTONUMERIC       EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     CONVERTBOOLTONUMERIC*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMERICTYPE
        $2A
        $3B,RESULT,BOOLTYPE
        ((:131E-*)&$3FFF)+$8000
        OPLBOOL
        $2F
        $3A,RESULT,NUMERICTYPE
:131E   EQU     *
        $2A
        WRONGTYPE
        $29
.       SET     *
        ORG     SUM*2+.SA
        #.
        ORG     .
PRODUCT  EQU     .SC
.SC      SET     .SC+1
        PRODUCT
        ((:131F-*)&$3FFF)+$8000
SUMOP   EQU     .SC
.SC      SET     .SC+1
        SUMOP
        ((:1320-*)&$3FFF)+$8000
:1321   EQU     *
        SUMOP
        ((:1321-*)&$3FFF)+$4000
        $34
:1320   EQU     *
        ((:1322-*)&$3FFF)+$4000
        $3E,#CAT
        ((:1323-*)&$3FFF)+$8000
        $3B,RESULT,STRINGTYPE
        ((:1324-*)&$3FFF)+$4000
        WRONGTYPE
:1324   EQU     *
        $2C
OPCIN    EQU     .SC
.SC      SET     .SC+1
        OPCIN
        $2F
        PRODUCT
        $2C
OPCAT     EQU     .SC
.SC      SET     .SC+1
        OPCAT
        $2F
:1325   EQU     *
        $3E,#CAT
        ((:1326-*)&$3FFF)+$8000
        PRODUCT
        $2C
        OPCAT
        $2F
:1326   EQU     *
        ((:1325-*)&$3FFF)+$4000
        $34
OPCND    EQU     .SC
.SC      SET     .SC+1
        OPCND
        $2F
:1323   EQU     *
        ((:1322-*)&$3FFF)+$4000
        $34
:1322   EQU     *
        $2C
:131F   EQU     *
        $29
.       SET     *
        ORG     SUMOP*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '++$80
        ((:1327-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        PRODUCT
        $2C
OPADD    EQU     .SC
.SC      SET     .SC+1
        OPADD
        $2F
:1327   EQU     *
        $2A
        $21
        FCC       ''
        '-+$80
        ((:1328-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        PRODUCT
        $2C
OPSUB    EQU     .SC
.SC      SET     .SC+1
        OPSUB
        $2F
:1328   EQU     *
        $2A
        $21
        FCC       ''
        '!+$80
        ((:1329-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        PRODUCT
        $2C
OPIOR   EQU     .SC
.SC      SET     .SC+1
        OPIOR
        $2F
:1329   EQU     *
        $2A
        $3E,#XOR
        ((:132A-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        PRODUCT
        $2C
OPXOR   EQU     .SC
.SC      SET     .SC+1
        OPXOR
        $2F
:132A    EQU     *
        $29
.       SET     *
        ORG     PRODUCT*2+.SA
        #.
        ORG     .
POWER   EQU     .SC
.SC      SET     .SC+1
        POWER
        ((:132B-*)&$3FFF)+$8000
:132C    EQU     *
PRODUCTOP        EQU     .SC
.SC      SET     .SC+1
        PRODUCTOP
        ((:132C-*)&$3FFF)+$4000
        $34
:132B    EQU     *
        $29
.       SET     *
        ORG     PRODUCTOP*2+.SA
        #.
        ORG     .
        $21
        FCC       '*'
        '*+$80
        ((:132D-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        POWER
        $2C
OPSHF   EQU     .SC
.SC      SET     .SC+1
        OPSHF
        $2F
:132D   EQU     *
        $2A
        $21
        FCC       ''
        '*+$80
        ((:132E-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        POWER
        $2C
OPMUL   EQU     .SC
.SC      SET     .SC+1
        OPMUL
        $2F
:132E   EQU     *
        $2A
        $21
        FCC       ''
        '/+$80
        ((:132F-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        POWER
        $2C
OPDIV   EQU     .SC
.SC      SET     .SC+1
        OPDIV
        $2F
:132F   EQU     *
        $2A
        $21
        FCC       ''
        '&+$80
        ((:1330-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        POWER
        $2C
OPAND    EQU     .SC
.SC      SET     .SC+1
        OPAND
        $2F
:1330   EQU     *
        $29
.       SET     *
        ORG     POWER*2+.SA
        #.
        ORG     .
TERM    EQU     .SC
.SC      SET     .SC+1
        TERM
        ((:1331-*)&$3FFF)+$8000
        $21
        FCC       ''
        '^+$80
        ((:1332-*)&$3FFF)+$8000
        CONVERTBOOLTONUMERIC
        $2C
        POWER
        $2C
OPOWER  EQU     .SC
.SC      SET     .SC+1
        OPOWER
        $2F
:1332   EQU     *
        ((:1333-*)&$3FFF)+$4000
        $34
:1333   EQU     *
        $2C
:1331   EQU     *
        $29
.       SET     *
        ORG     TERM*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMORBOOLTYPE
        ((:1334-*)&$3FFF)+$8000
SYNTACTICTERM      EQU     .SC
.SC      SET     .SC+1
        SYNTACTICTERM
        $2C
        $3B,RESULT,NUMERICTYPE
        ((:1335-*)&$3FFF)+$4000
        $3B,RESULT,BOOLTYPE
        ((:1335-*)&$3FFF)+$4000
        WRONGTYPE
:1335   EQU     *
        $2C
:1334   EQU     *
        $2A
        $3B,RESULT,NUMERICTYPE
        ((:1336-*)&$3FFF)+$8000
        SYNTACTICTERM
        $2C
        CONVERTBOOLTONUMERIC
        $2C
:1336   EQU     *
        $2A
        $3B,RESULT,BOOLTYPE
        ((:1337-*)&$3FFF)+$8000
        SYNTACTICTERM
        $2C
        $3B,RESULT,BOOLTYPE
        ((:1338-*)&$3FFF)+$4000
        WRONGTYPE
:1338   EQU     *
        $2C
:1337   EQU     *
        $2A
        $3B,RESULT,NUMBOOLORSTRTYPE
        ((:1339-*)&$3FFF)+$8000
        SYNTACTICTERM
        $2C
        $3B,RESULT,NUMERICTYPE
        ((:133A-*)&$3FFF)+$4000
        $3B,RESULT,BOOLTYPE
        ((:133A-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:133A-*)&$3FFF)+$4000
        WRONGTYPE
:133A    EQU     *
        $2C
:1339   EQU     *
        $2A
        $3B,RESULT,NUMORSTRTYPE
        ((:133B-*)&$3FFF)+$8000
        $3A,RESULT,NUMBOOLORSTRTYPE
        SYNTACTICTERM
        $2C
        $3B,RESULT,STRINGTYPE
        ((:133C-*)&$3FFF)+$4000
        CONVERTBOOLTONUMERIC
:133C    EQU     *
        $2C
:133B    EQU     *
        $2A
        $3B,RESULT,STRINGTYPE
        ((:133D-*)&$3FFF)+$8000
        SYNTACTICTERM
        $2C
VERIFYSTRINGRESULT      EQU     .SC
.SC      SET     .SC+1
        VERIFYSTRINGRESULT
        $2C
:133D   EQU     *
        $2A
        $3B,RESULT,ADDRESSANYTYPE
        ((:133E-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:133E   EQU     *
        ((:133F-*)&$3FFF)+$8000
        SYNTACTICTERM
        $2C
:133F   EQU     *
        $29
VERIFYNUMERICRESULT      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     VERIFYNUMERICRESULT*2+.SA
        #.
        ORG     .
        $3B,RESULT,NUMERICTYPE
        $2A
        WRONGTYPE
        $29
.       SET     *
        ORG     VERIFYSTRINGRESULT*2+.SA
        #.
        ORG     .
        $3B,RESULT,STRINGTYPE
        $2A
        WRONGTYPE
        $29
.       SET     *
        ORG     WRONGTYPE*2+.SA
        #.
        ORG     .
        $2D
        FCC       "Wrong type of valu"
        'e+$80
        $29
.       SET     *
        ORG     TERMGIVINGNUMERIC*2+.SA
        #.
        ORG     .
        $3A,RESULT,NUMERICTYPE
        SYNTACTICTERM
        $2C
        CONVERTBOOLTONUMERIC
        $2C
        $29
TERMGIVINGSTRING        EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     TERMGIVINGSTRING*2+.SA
        #.
        ORG     .
        $3A,RESULT,STRINGTYPE
        SYNTACTICTERM
        $2C
        VERIFYSTRINGRESULT
        $2C
        $29
.       SET     *
        ORG     SYNTACTICTERM*2+.SA
        #.
        ORG     .
NUMVARIABLEORUSERFUNCTIONREF        EQU     .SC
.SC      SET     .SC+1
        NUMVARIABLEORUSERFUNCTIONREF
        $2A
STRVARIABLEORUSERFUNCTIONREF        EQU     .SC
.SC      SET     .SC+1
        STRVARIABLEORUSERFUNCTIONREF
        $2A
OBVIOUSLYNUMERICTERM      EQU     .SC
.SC      SET     .SC+1
        OBVIOUSLYNUMERICTERM
        $2A
OBVIOUSLYSTRINGTERM      EQU     .SC
.SC      SET     .SC+1
        OBVIOUSLYSTRINGTERM
        $2A
OBVIOUSLYBOOLEANTERM       EQU     .SC
.SC      SET     .SC+1
        OBVIOUSLYBOOLEANTERM
        $2A
NOTSOCLEARTERM    EQU     .SC
.SC      SET     .SC+1
        NOTSOCLEARTERM
        $29
.       SET     *
        ORG     NUMVARIABLEORUSERFUNCTIONREF*2+.SA
        #.
        ORG     .
        +NID
        ((:1340-*)&$3FFF)+$8000
        $27,SCALAR
        ((:1341-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
        $3B,RESULT,ADDRESSANYTYPE
        ((:1342-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:1342   EQU     *
        ((:1343-*)&$3FFF)+$8000
        VARIABLEONLY
        ((:1344-*)&$3FFF)+$8000
        OPLADDR
        $2F
        SADDR
        $32,1
        $2F
        $3A,RESULT,ADDRESSSCALARTYPE
:1344   EQU     *
        ((:1345-*)&$3FFF)+$4000
        $34
OPLSCALAR  EQU     .SC
.SC      SET     .SC+1
        OPLSCALAR
        $38
        +:1346-*-1
        +1
:1346   EQU     *
        $2C
        $3A,RESULT,NUMERICTYPE
:1345   EQU     *
        $2C
:1343   EQU     *
        ((:1347-*)&$3FFF)+$4000
        $34
        OPLSCALAR
        $38
        +:1348-*-1
        +1
:1348   EQU     *
        $2C
        $3A,RESULT,NUMERICTYPE
:1347   EQU     *
        $2C
:1341   EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,VECTOR
        ((:134A-*)&$3FFF)+$8000
        $3B,RESULT,ADDRESSANYTYPE
        ((:134B-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:134B    EQU     *
        ((:134C-*)&$3FFF)+$8000
        LB
        ((:134D-*)&$3FFF)+$8000
        NUMEXP
        $2C
ONESUB1  EQU     .SC
.SC      SET     .SC+1
        ONESUB1
        $2C
        $28,1,PARAMETER
        ((:134E-*)&$3FFF)+$8000
        OPVPA
        $2F
        DADDR
        $32,1
        $2F
        VARIABLEONLY
        ((:134F-*)&$3FFF)+$8000
        $3A,RESULT,ADDRESSSCALARTYPE
:134F   EQU     *
        ((:1350-*)&$3FFF)+$4000
        $34
OPLV    EQU     .SC
.SC      SET     .SC+1
        OPLV
        $2F
        $3A,RESULT,NUMERICTYPE
:1350   EQU     *
        $2C
:134E   EQU     *
        ((:1351-*)&$3FFF)+$4000
        VARIABLEONLY
        ((:1352-*)&$3FFF)+$8000
OPVSA    EQU     .SC
.SC      SET     .SC+1
        OPVSA
        $2F
        DADDR
        $32,1
        $2F
        $3A,RESULT,ADDRESSSCALARTYPE
:1352   EQU     *
        ((:1351-*)&$3FFF)+$4000
        $34
OPVSV   EQU     .SC
.SC      SET     .SC+1
        OPVSV
        $2F
        DADDR
        $32,1
        $2F
        $3A,RESULT,NUMERICTYPE
:1351   EQU     *
        $2C
:134D   EQU     *
        ((:1353-*)&$3FFF)+$4000
        VARIABLEONLY
        ((:1354-*)&$3FFF)+$8000
        $28,1,PARAMETER
        ((:1355-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
:1355   EQU     *
        ((:1356-*)&$3FFF)+$4000
        $34
        OPLADDR
        $2F
        DADDR
        $32,1
        $2F
:1356   EQU     *
        $2C
        $3A,RESULT,ADDRESSARRAYTYPE
:1354   EQU     *
:1353   EQU     *
        $2C
:134C    EQU     *
        ((:1357-*)&$3FFF)+$4000
        ONESUBSCRIPT
        ((:1358-*)&$3FFF)+$8000
        $28,1,PARAMETER
        ((:1359-*)&$3FFF)+$8000
        OPVPA
        $2F
        DADDR
        $32,1
        $2F
        OPLV
        $2F
:1359   EQU     *
        ((:135A-*)&$3FFF)+$4000
        $34
        OPVSV
        $2F
        DADDR
        $32,1
        $2F
:135A    EQU     *
        $2C
        $3A,RESULT,NUMERICTYPE
:1358   EQU     *
:1357   EQU     *
        $2C
:134A    EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,ARRAY
        ((:135B-*)&$3FFF)+$8000
        $3B,RESULT,ADDRESSANYTYPE
        ((:135C-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:135C    EQU     *
        ((:135D-*)&$3FFF)+$8000
        LB
        ((:135E-*)&$3FFF)+$8000
        NUMEXP
        $2C
TWOSUB1  EQU     .SC
.SC      SET     .SC+1
        TWOSUB1
        $2C
        $28,1,PARAMETER
        ((:135F-*)&$3FFF)+$8000
        OPAPA
        $2F
        DADDR
        $32,1
        $2F
        VARIABLEONLY
        ((:1360-*)&$3FFF)+$8000
        $3A,RESULT,ADDRESSSCALARTYPE
:1360   EQU     *
        ((:1361-*)&$3FFF)+$4000
        $34
        OPLV
        $2F
        $3A,RESULT,NUMERICTYPE
:1361   EQU     *
        $2C
:135F   EQU     *
        ((:1362-*)&$3FFF)+$4000
        VARIABLEONLY
        ((:1363-*)&$3FFF)+$8000
OPASA     EQU     .SC
.SC      SET     .SC+1
        OPASA
        $2F
        DADDR
        $32,1
        $2F
        $3A,RESULT,ADDRESSSCALARTYPE
:1363   EQU     *
        ((:1362-*)&$3FFF)+$4000
        $34
OPASV    EQU     .SC
.SC      SET     .SC+1
        OPASV
        $2F
        DADDR
        $32,1
        $2F
        $3A,RESULT,NUMERICTYPE
:1362   EQU     *
        $2C
:135E   EQU     *
        ((:1364-*)&$3FFF)+$4000
        VARIABLEONLY
        ((:1365-*)&$3FFF)+$8000
        $28,1,PARAMETER
        ((:1366-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
:1366   EQU     *
        ((:1367-*)&$3FFF)+$4000
        $34
        OPLADDR
        $2F
        DADDR
        $32,1
        $2F
:1367   EQU     *
        $2C
        $3A,RESULT,ADDRESSARRAYTYPE
:1365   EQU     *
:1364   EQU     *
        $2C
:135D   EQU     *
        ((:1368-*)&$3FFF)+$4000
        TWOSUBSCRIPTS
        ((:1369-*)&$3FFF)+$8000
        $28,1,PARAMETER
        ((:136A-*)&$3FFF)+$8000
        OPAPA
        $2F
        DADDR
        $32,1
        $2F
        OPLV
        $2F
:136A    EQU     *
        ((:136B-*)&$3FFF)+$4000
        $34
        OPASV
        $2F
        DADDR
        $32,1
        $2F
:136B    EQU     *
        $2C
        $3A,RESULT,NUMERICTYPE
:1369   EQU     *
:1368   EQU     *
        $2C
:135B    EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:136C-*)&$3FFF)+$8000
FUNCTIONCALL       EQU     .SC
.SC      SET     .SC+1
        FUNCTIONCALL
        $38
        +:136D-*-1
        +1
:136D   EQU     *
        $2C
        $3A,RESULT,NUMERICTYPE
:136C    EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:136E-*)&$3FFF)+$4000
        $27,FREFLABEL
:136E   EQU     *
        ((:136F-*)&$3FFF)+$8000
        OPLINT
        $2F
        ADDR
        LABEL
        $31
        $2F
        $3A,RESULT,NUMERICTYPE
:136F   EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,PARAMETER
        ((:1370-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
        $3B,RESULT,ADDRESSANYTYPE
        ((:1371-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:1371   EQU     *
        ((:1372-*)&$3FFF)+$8000
        VARIABLEONLY
        ((:1373-*)&$3FFF)+$8000
        $3A,RESULT,ADDRESSSCALARTYPE
:1373   EQU     *
        ((:1374-*)&$3FFF)+$4000
        $34
        OPLV
        $2F
        $3A,RESULT,NUMERICTYPE
:1374   EQU     *
        $2C
:1372   EQU     *
        ((:1375-*)&$3FFF)+$4000
        $34
        OPLV
        $2F
        $3A,RESULT,NUMERICTYPE
:1375   EQU     *
        $2C
:1370   EQU     *
        ((:1349-*)&$3FFF)+$4000
        $27,COMMONSCALAR
        ((:1376-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
:1376   EQU     *
        ((:1377-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
:1377   EQU     *
        ((:1378-*)&$3FFF)+$4000
        NOSUBSCRIPTS
        ((:1379-*)&$3FFF)+$8000
        $23,COMMONSCALAR
        $23,POTENTIALLABEL
:1379   EQU     *
:1378   EQU     *
        ((:137A-*)&$3FFF)+$8000
        $3B,RESULT,ADDRESSANYTYPE
        ((:137B-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:137B    EQU     *
        ((:137C-*)&$3FFF)+$8000
        VARIABLEONLY
        ((:137D-*)&$3FFF)+$8000
        OPLADDR
        $2F
        DADDR
        $32,1
        $2F
        $27,POTENTIALLABEL
        ((:137E-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        $32,1
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
:137E   EQU     *
        ((:137F-*)&$3FFF)+$4000
        $34
:137F   EQU     *
        $2C
        $3A,RESULT,ADDRESSSCALARTYPE
:137D   EQU     *
        ((:1380-*)&$3FFF)+$4000
        $34
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
        $3A,RESULT,NUMERICTYPE
:1380   EQU     *
        $2C
:137C    EQU     *
        ((:1381-*)&$3FFF)+$4000
        $34
        OPLOAD
        $2F
        DADDR
        $31
        $2F
        $3A,RESULT,NUMERICTYPE
:1381   EQU     *
        $2C
:137A    EQU     *
:1349   EQU     *
        $2C
:1340   EQU     *
        $29
.       SET     *
        ORG     STRVARIABLEORUSERFUNCTIONREF*2+.SA
        #.
        ORG     .
        +SID
        ((:1382-*)&$3FFF)+$8000
        $27,STRVAR
        ((:1383-*)&$3FFF)+$8000
        $27,PARAMETER
        ((:1384-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
:1384   EQU     *
        ((:1385-*)&$3FFF)+$4000
        $34
        OPLSD
        $2F
        DADDR
        $32,1
        $2F
:1385   EQU     *
        $2C
SUBSTRING        EQU     .SC
.SC      SET     .SC+1
        SUBSTRING
        $2C
:1383   EQU     *
        ((:1386-*)&$3FFF)+$4000
        $27,STRARRAY
        ((:1387-*)&$3FFF)+$8000
        $27,PARAMETER
        ((:1388-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $32,1
        $2F
:1388   EQU     *
        ((:1389-*)&$3FFF)+$4000
        $34
        OPLADDR
        $2F
        DADDR
        $32,1
        $2F
:1389   EQU     *
        $2C
        LB
        ((:138A-*)&$3FFF)+$8000
        NUMEXP
        $2C
        ONESUB1
        $2C
        OPSSA
        $2F
        SUBSTRING
        $2C
:138A    EQU     *
        ((:138B-*)&$3FFF)+$4000
        $34
        $3A,RESULT,ADDRESSARRAYTYPE
:138B    EQU     *
        $2C
:1387   EQU     *
        ((:1386-*)&$3FFF)+$4000
        $27,STRINGFUNCTION
        ((:138C-*)&$3FFF)+$8000
        FUNCTIONCALL
        $38
        +:138D-*-1
        +1
:138D   EQU     *
        $2C
        SUBSTRING
        $2C
:138C    EQU     *
        ((:1386-*)&$3FFF)+$4000
        UDFSID
:1386   EQU     *
        $2C
        $37,1
:1382   EQU     *
        $29
.       SET     *
        ORG     FUNCTIONCALL*2+.SA
        #.
        ORG     .
        ARGUMENTLIST
        $38
        +:138E-*-1+$80
:138E   EQU     *
        ((:138F-*)&$3FFF)+$8000
OPFNCALL  EQU     .SC
.SC      SET     .SC+1
        OPFNCALL
        $2F
        ADDR
        $32,1
        $2F
        BYTE
        GENLBL
        $33,1
        $2F
:138F   EQU     *
        $29
.       SET     *
        ORG     NOTSOCLEARTERM*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '(+$80
        ((:1390-*)&$3FFF)+$8000
        $3B,RESULT,ADDRESSANYTYPE
        ((:1391-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:1391   EQU     *
        ((:1392-*)&$3FFF)+$8000
        $3A,RESULT,NUMERICTYPE
:1392   EQU     *
        ((:1393-*)&$3FFF)+$4000
        $34
:1393   EQU     *
        $2C
        EXPRESSION
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1394-*)&$3FFF)+$8000
        OPLINT
        $2F
        INTEGER
        $30
        FCC       '25'
        '6+$80
        $2F
        OPMUL
        $2F
        EXPRESSION
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPADD
        $2F
        VARIABLEONLY
        $2C
:1394   EQU     *
        ((:1395-*)&$3FFF)+$4000
        $21
        FCC       ''
        ')+$80
:1395   EQU     *
        $2C
:1390   EQU     *
        $2A
BUILTINSTRFUN    EQU     .SC
.SC      SET     .SC+1
        BUILTINSTRFUN
        $2A
        $3E,#IF
        ((:1396-*)&$3FFF)+$8000
        $3B,RESULT,NUMORBOOLTYPE
        ((:1397-*)&$3FFF)+$4000
        $3B,RESULT,NUMERICTYPE
        ((:1397-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSSCALARTYPE
:1397   EQU     *
        ((:1398-*)&$3FFF)+$8000
CONDITIONBF       EQU     .SC
.SC      SET     .SC+1
        CONDITIONBF
        $38
        +:1399-*-1+$80
:1399   EQU     *
        $2C
        NUMEXP
        $2C
:1398   EQU     *
        ((:139A-*)&$3FFF)+$4000
        $3B,RESULT,NUMBOOLORSTRTYPE
        ((:139B-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSANYTYPE
:139B    EQU     *
        ((:139C-*)&$3FFF)+$8000
        CONDITIONBF
        $38
        +:139D-*-1+$80
:139D   EQU     *
        $2C
        NUMORSTREXP
        $2C
:139C    EQU     *
        ((:139A-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:139E-*)&$3FFF)+$8000
        CONDITIONBF
        $38
        +:139F-*-1+$80
:139F   EQU     *
        $2C
        STREXP
        $2C
:139E   EQU     *
        ((:139A-*)&$3FFF)+$4000
        WRONGTYPE
:139A    EQU     *
        $2C
        OPTENDOFLINE
        $2C
        OPJMP
        $2F
        ADDR
        GENLBL
        $33,2
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
        $3E,#ELSE
        $2C
        EXPRESSION
        $2C
        $3E,#FI
        $2C
        GENLBL
        $33,2
        EQUHERE
        $2F
:1396   EQU     *
        $29
.       SET     *
        ORG     CONDITIONBF*2+.SA
        #.
        ORG     .
        CONDITION
        ((:13A0-*)&$3FFF)+$8000
        OPTENDOFLINE
        $2C
        OPBF
        $2F
        ADDR
        GENLBL
        $33,1
        $2F
        $3E,#THEN
        $2C
:13A0    EQU     *
        $29
NOTARITHOP       EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     NOTARITHOP*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        '++$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '-+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '!+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $3E,#XOR
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       '*'
        '*+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '*+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '/+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '&+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $21
        FCC       ''
        '^+$80
        $22
        ((:13A1-*)&$3FFF)+$8000
        $34
:13A1    EQU     *
        $29
.       SET     *
        ORG     OBVIOUSLYBOOLEANTERM*2+.SA
        #.
        ORG     .
        $3E,#NOT
        ((:13A2-*)&$3FFF)+$8000
        $3A,RESULT,BOOLTYPE
        RELATION
        $2C
OPINV   EQU     .SC
.SC      SET     .SC+1
        OPINV
        $2F
:13A2    EQU     *
        ((:13A3-*)&$3FFF)+$4000
        $3E,#EOF
        ((:13A4-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPEOF   EQU     .SC
.SC      SET     .SC+1
        OPEOF
        $2F
:13A4    EQU     *
:13A3    EQU     *
        ((:13A5-*)&$3FFF)+$8000
        $3A,RESULT,BOOLTYPE
:13A5    EQU     *
        $29
ARRAYREF  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     ARRAYREF*2+.SA
        #.
        ORG     .
ARRAYREF1 EQU     .SC
.SC      SET     .SC+1
        ARRAYREF1
        ((:13A6-*)&$3FFF)+$4000
        $21
        FCC       ''
        '(+$80
        ((:13A7-*)&$3FFF)+$8000
        ARRAYREF1
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
:13A7    EQU     *
:13A6    EQU     *
        ((:13A8-*)&$3FFF)+$8000
        $37,1
:13A8    EQU     *
        $29
.       SET     *
        ORG     ARRAYREF1*2+.SA
        #.
        ORG     .
        +NID
        ((:13A9-*)&$3FFF)+$8000
        $27,VECTOR
        ((:13AA-*)&$3FFF)+$4000
        $27,ARRAY
        ((:13AA-*)&$3FFF)+$4000
        WRONGTYPE
:13AA     EQU     *
        $2C
        $27,PARAMETER
        ((:13AB-*)&$3FFF)+$8000
        OPLOAD
        $2F
        DADDR
        $31
        $2F
:13AB     EQU     *
        ((:13AC-*)&$3FFF)+$4000
        $34
        OPLADDR
        $2F
        DADDR
        $31
        $2F
:13AC     EQU     *
        $2C
        $3A,RESULT,ADDRESSARRAYTYPE
:13A9    EQU     *
        ((:13AD-*)&$3FFF)+$4000
        STRVARIABLEORUSERFUNCTIONREF
        ((:13AE-*)&$3FFF)+$8000
        $3B,RESULT,STRINGTYPE
        ((:13AF-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSARRAYTYPE
        ((:13AF-*)&$3FFF)+$4000
        WRONGTYPE
:13AF    EQU     *
        $2C
:13AE    EQU     *
:13AD    EQU     *
        ((:13B0-*)&$3FFF)+$8000
        $37,1
:13B0    EQU     *
        $29
.       SET     *
        ORG     OBVIOUSLYNUMERICTERM*2+.SA
        #.
        ORG     .
        +ONEDIGITNUM
        ((:13B1-*)&$3FFF)+$8000
        OPLSMI
        $31
        $2F
:13B1    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        +TENTO255
        ((:13B3-*)&$3FFF)+$8000
        OPLBINT
        $2F
        BYTE
        $31
        $2F
:13B3    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        +NUM
        ((:13B4-*)&$3FFF)+$8000
        OPLINT
        $2F
        INTEGER
        $31
        $2F
:13B4    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        +FNUM
        ((:13B5-*)&$3FFF)+$8000
        OPLFI
        $2F
        SIXBYTEFNUM
        $38
        +:13B6-*-1
        +1
:13B6    EQU     *
        $2F
:13B5    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $21
        FCC       ''
        ':+$80
        ((:13B7-*)&$3FFF)+$8000
        +HEXNUM
        $2C
        OPLINT
        $2F
        INTEGER
        $30
        FCC       ''
        '$+$80
        $31
        $2F
:13B7    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $21
        FCC       ''
        '-+$80
        ((:13B8-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPNEG   EQU     .SC
.SC      SET     .SC+1
        OPNEG
        $2F
:13B8    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#LEN
        ((:13B9-*)&$3FFF)+$8000
        ARRAYREF
        $2C
        $28,1,VECTOR
        ((:13BA-*)&$3FFF)+$8000
OPLENVECTOR      EQU     .SC
.SC      SET     .SC+1
        OPLENVECTOR
        $2F
:13BA     EQU     *
        ((:13BB-*)&$3FFF)+$4000
        $3B,RESULT,STRINGTYPE
        ((:13BC-*)&$3FFF)+$8000
OPLEN   EQU     .SC
.SC      SET     .SC+1
        OPLEN
        $2F
:13BC     EQU     *
        ((:13BB-*)&$3FFF)+$4000
        $3B,RESULT,ADDRESSARRAYTYPE
        ((:13BD-*)&$3FFF)+$8000
        $28,1,STRARRAY
        ((:13BE-*)&$3FFF)+$4000
        WRONGTYPE
:13BE    EQU     *
        $2C
        OPLENVECTOR
        $2F
:13BD    EQU     *
        ((:13BB-*)&$3FFF)+$4000
        WRONGTYPE
:13BB     EQU     *
        $2C
:13B9    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#MAXLEN
        ((:13BF-*)&$3FFF)+$8000
        ARRAYREF
        $2C
        $3B,RESULT,STRINGTYPE
        ((:13C0-*)&$3FFF)+$4000
        $28,1,STRARRAY
        ((:13C1-*)&$3FFF)+$8000
        $3B,RESULT,ADDRESSARRAYTYPE
        ((:13C2-*)&$3FFF)+$4000
        WRONGTYPE
:13C2    EQU     *
        $2C
        OPLSMI
        $30
        FCC       ''
        '1+$80
        $2F
        OPSSA
        $2F
:13C1    EQU     *
        ((:13C0-*)&$3FFF)+$4000
        WRONGTYPE
:13C0    EQU     *
        $2C
OPMAXLEN EQU     .SC
.SC      SET     .SC+1
        OPMAXLEN
        $2F
:13BF    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ROWS
        ((:13C3-*)&$3FFF)+$8000
        ARRAYREF
        $2C
        $28,1,ARRAY
        ((:13C4-*)&$3FFF)+$8000
OPROWSARRAY       EQU     .SC
.SC      SET     .SC+1
        OPROWSARRAY
        $2F
:13C4    EQU     *
        ((:13C5-*)&$3FFF)+$4000
        WRONGTYPE
:13C5    EQU     *
        $2C
:13C3    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#COLUMNS
        ((:13C6-*)&$3FFF)+$8000
        ARRAYREF
        $2C
        $28,1,ARRAY
        ((:13C7-*)&$3FFF)+$4000
        WRONGTYPE
:13C7    EQU     *
        $2C
OPCOLSARRAY        EQU     .SC
.SC      SET     .SC+1
        OPCOLSARRAY
        $2F
:13C6    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#COM
        ((:13C8-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPCOM    EQU     .SC
.SC      SET     .SC+1
        OPCOM
        $2F
:13C8    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ATN
        ((:13C9-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPATN    EQU     .SC
.SC      SET     .SC+1
        OPATN
        $2F
:13C9    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#SIN
        ((:13CA-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPSIN   EQU     .SC
.SC      SET     .SC+1
        OPSIN
        $2F
:13CA     EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#COS
        ((:13CB-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPCOS    EQU     .SC
.SC      SET     .SC+1
        OPCOS
        $2F
:13CB     EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#TAN
        ((:13CC-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPTAN    EQU     .SC
.SC      SET     .SC+1
        OPTAN
        $2F
:13CC     EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#LOG
        ((:13CD-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPLOG   EQU     .SC
.SC      SET     .SC+1
        OPLOG
        $2F
:13CD    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#EXP
        ((:13CE-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPEXP   EQU     .SC
.SC      SET     .SC+1
        OPEXP
        $2F
:13CE    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#SQR
        ((:13CF-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPSQR   EQU     .SC
.SC      SET     .SC+1
        OPSQR
        $2F
:13CF    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#INT
        ((:13D0-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPINT   EQU     .SC
.SC      SET     .SC+1
        OPINT
        $2F
:13D0   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ABS
        ((:13D1-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPABS     EQU     .SC
.SC      SET     .SC+1
        OPABS
        $2F
:13D1   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#SGN
        ((:13D2-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPSGN   EQU     .SC
.SC      SET     .SC+1
        OPSGN
        $2F
:13D2   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#COL
        ((:13D3-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPCOL    EQU     .SC
.SC      SET     .SC+1
        OPCOL
        $2F
:13D3   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#VAL
        ((:13D4-*)&$3FFF)+$8000
        TERMGIVINGSTRING
        $2C
OPVAL    EQU     .SC
.SC      SET     .SC+1
        OPVAL
        $2F
:13D4   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#PEEK
        ((:13D5-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPEEK   EQU     .SC
.SC      SET     .SC+1
        OPEEK
        $2F
:13D5   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ASC
        ((:13D6-*)&$3FFF)+$8000
        TERMGIVINGSTRING
        $2C
OPLDB    EQU     .SC
.SC      SET     .SC+1
        OPLDB
        $2F
:13D6   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#FIND
        ((:13D7-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
OPFIND  EQU     .SC
.SC      SET     .SC+1
        OPFIND
        $2F
:13D7   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#RND
        ((:13D8-*)&$3FFF)+$8000
OPRND   EQU     .SC
.SC      SET     .SC+1
        OPRND
        $2F
:13D8   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ERR
        ((:13D9-*)&$3FFF)+$8000
OPERR   EQU     .SC
.SC      SET     .SC+1
        OPERR
        $2F
:13D9   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#ELN
        ((:13DA-*)&$3FFF)+$8000
OPELN   EQU     .SC
.SC      SET     .SC+1
        OPELN
        $2F
:13DA    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#PI
        ((:13DB-*)&$3FFF)+$8000
OPPI    EQU     .SC
.SC      SET     .SC+1
        OPPI
        $2F
:13DB    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#TRUE
        ((:13DC-*)&$3FFF)+$8000
        OPLSMI
        $30
        FCC       ''
        '1+$80
        $2F
:13DC    EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $3E,#FALSE
        ((:13DD-*)&$3FFF)+$8000
        OPLSMI
        $30
        FCC       ''
        '0+$80
        $2F
:13DD   EQU     *
        ((:13B2-*)&$3FFF)+$4000
        $21
        FCC       ''
        '++$80
        ((:13DE-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
:13DE   EQU     *
:13B2    EQU     *
        ((:13DF-*)&$3FFF)+$8000
        $3A,RESULT,NUMERICTYPE
:13DF   EQU     *
        $29
.       SET     *
        ORG     DEFSCALAR*2+.SA
        #.
        ORG     .
        +NID
        ((:13E0-*)&$3FFF)+$8000
        $37,1
        $27,SCALAR
        ((:13E1-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
:13E1   EQU     *
        ((:13E2-*)&$3FFF)+$4000
        $27,PARAMETER
        ((:13E3-*)&$3FFF)+$8000
        NOPARAMETERSHERE
        $2C
:13E3   EQU     *
        ((:13E2-*)&$3FFF)+$4000
        $27,VECTOR
        ((:13E4-*)&$3FFF)+$4000
        $27,ARRAY
:13E4   EQU     *
        ((:13E5-*)&$3FFF)+$8000
        NOSUBVARS
        $2C
:13E5   EQU     *
        ((:13E2-*)&$3FFF)+$4000
        $27,ISLABEL
        ((:13E6-*)&$3FFF)+$4000
        $27,FREFLABEL
:13E6   EQU     *
        ((:13E7-*)&$3FFF)+$8000
        CANTSTORELABEL
        $2C
:13E7   EQU     *
        ((:13E2-*)&$3FFF)+$4000
        $27,POTENTIALLABEL
        ((:13E8-*)&$3FFF)+$8000
        $25,POTENTIALLABEL
        $2C
        $31
        EQUDATA
        $2F
        RESDATA
        $30
        FCC       ''
        '6+$80
        $2F
:13E8   EQU     *
        ((:13E9-*)&$3FFF)+$4000
        $27,COMMONSCALAR
:13E9   EQU     *
        ((:13EA-*)&$3FFF)+$8000
        NOSUBSCRIPTS
        $2C
:13EA    EQU     *
        ((:13E2-*)&$3FFF)+$4000
        $27,FUNCTION
        ((:13EB-*)&$3FFF)+$8000
        NOFUNCTSHERE
        $2C
:13EB    EQU     *
        ((:13E2-*)&$3FFF)+$4000
        NOSUBSCRIPTS
        ((:13EC-*)&$3FFF)+$8000
        $23,SCALAR
        $31
        ALLOCATESCALAR
        $2F
:13EC    EQU     *
:13E2   EQU     *
        $2C
:13E0   EQU     *
        $29
.       SET     *
        ORG     DEFSID*2+.SA
        #.
        ORG     .
        +SID
        ((:13ED-*)&$3FFF)+$8000
        STRVARREF
        $38
        +:13EE-*-1
        +1
:13EE   EQU     *
        ((:13EF-*)&$3FFF)+$4000
        UDFSID
:13EF   EQU     *
        $2C
:13ED   EQU     *
        $29
.       SET     *
        ORG     NOSUBSCRIPTS*2+.SA
        #.
        ORG     .
        LB
        ((:13F0-*)&$3FFF)+$8000
        $2D
        FCC       "Variable not DIM'ed as vector or arra"
        'y+$80
:13F0   EQU     *
        $2A
        $34
        $29
.       SET     *
        ORG     ONESUBSCRIPT*2+.SA
        #.
        ORG     .
        LB
        ((:13F1-*)&$3FFF)+$8000
        NUMEXP
        $2C
        ONESUB1
        $2C
:13F1   EQU     *
        $2A
        SINGLESUBREQ
        $29
.       SET     *
        ORG     ONESUB1*2+.SA
        #.
        ORG     .
        RB
        $2A
        SINGLESUBREQ
        $29
.       SET     *
        ORG     TWOSUBSCRIPTS*2+.SA
        #.
        ORG     .
        LB
        ((:13F2-*)&$3FFF)+$8000
        NUMEXP
        $2C
        TWOSUB1
        $2C
:13F2   EQU     *
        $2A
        DOUBLESUBREQ
        $29
.       SET     *
        ORG     TWOSUB1*2+.SA
        #.
        ORG     .
        $21
        FCC       ''
        ',+$80
        ((:13F3-*)&$3FFF)+$8000
        NUMEXP
        $2C
TWOSUB2  EQU     .SC
.SC      SET     .SC+1
        TWOSUB2
        $2C
:13F3   EQU     *
        $2A
        DOUBLESUBREQ
        $29
.       SET     *
        ORG     TWOSUB2*2+.SA
        #.
        ORG     .
        RB
        $2A
        DOUBLESUBREQ
        $29
.       SET     *
        ORG     BUILTINSTRFUN*2+.SA
        #.
        ORG     .
        $3E,#DATE$
        ((:13F4-*)&$3FFF)+$8000
OPDAT    EQU     .SC
.SC      SET     .SC+1
        OPDAT
        $2F
:13F4   EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#TIME$
        ((:13F6-*)&$3FFF)+$8000
OPTIM   EQU     .SC
.SC      SET     .SC+1
        OPTIM
        $2F
:13F6   EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#COPYRIGHT$
        ((:13F7-*)&$3FFF)+$8000
OPCOPYRIGHT      EQU     .SC
.SC      SET     .SC+1
        OPCOPYRIGHT
        $2F
:13F7   EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#NUM$
        ((:13F8-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPNUM   EQU     .SC
.SC      SET     .SC+1
        OPNUM
        $2F
:13F8   EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#NUMF$
        ((:13F9-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
OPNUMF  EQU     .SC
.SC      SET     .SC+1
        OPNUMF
        $2F
:13F9   EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#HEX$
        ((:13FA-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPHEX   EQU     .SC
.SC      SET     .SC+1
        OPHEX
        $2F
:13FA    EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#UPPERCASE$
        ((:13FB-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
OPUPPERC EQU     .SC
.SC      SET     .SC+1
        OPUPPERC
        $2F
:13FB    EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#LOWERCASE$
        ((:13FC-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
OPLOWERC EQU     .SC
.SC      SET     .SC+1
        OPLOWERC
        $2F
:13FC    EQU     *
        ((:13F5-*)&$3FFF)+$4000
        $3E,#CHR$
        ((:13FD-*)&$3FFF)+$8000
        TERMGIVINGNUMERIC
        $2C
OPCHR    EQU     .SC
.SC      SET     .SC+1
        OPCHR
        $2F
:13FD   EQU     *
:13F5   EQU     *
        ((:13FE-*)&$3FFF)+$8000
        SUBSTRING
        $2C
:13FE   EQU     *
        $29
.       SET     *
        ORG     OBVIOUSLYSTRINGTERM*2+.SA
        #.
        ORG     .
        +STRING
        ((:13FF-*)&$3FFF)+$8000
OPLSC    EQU     .SC
.SC      SET     .SC+1
        OPLSC
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '-*-'
        '1+$80
        $2F
        TEXT
        $31
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:13FF   EQU     *
        ((:1400-*)&$3FFF)+$4000
        $3E,#MID$
        ((:1401-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPSS2
        $2F
:1401   EQU     *
        ((:1400-*)&$3FFF)+$4000
        $3E,#LEFT$
        ((:1402-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        OPLSMI
        $30
        FCC       '+'
        '1+$80
        $2F
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPSS2
        $2F
:1402   EQU     *
        ((:1400-*)&$3FFF)+$4000
        $3E,#RIGHT$
        ((:1403-*)&$3FFF)+$8000
        $21
        FCC       ''
        '(+$80
        $2C
        STREXP
        $2C
        $21
        FCC       ''
        ',+$80
        $2C
        NUMEXP
        $2C
        $21
        FCC       ''
        ')+$80
        $2C
        OPRIGHT
        $2F
:1403   EQU     *
:1400   EQU     *
        ((:1404-*)&$3FFF)+$8000
        $3A,RESULT,STRINGTYPE
:1404   EQU     *
        $29
.       SET     *
        ORG     STRCON*2+.SA
        #.
        ORG     .
        +STRING
        ((:1405-*)&$3FFF)+$8000
        OPLSC
        $2F
        BYTE
        GENLBL
        $33,1
        $30
        FCC       '-*-'
        '1+$80
        $2F
        TEXT
        $31
        $2F
        GENLBL
        $33,1
        EQUHERE
        $2F
:1405   EQU     *
        $29
.       SET     *
        ORG     SUBSTRING*2+.SA
        #.
        ORG     .
        LB
        ((:1406-*)&$3FFF)+$8000
        NUMEXP
        $2C
        $21
        FCC       ''
        ',+$80
        ((:1407-*)&$3FFF)+$8000
        NUMEXP
        $2C
        RB
        $2C
        OPSS2
        $2F
        $3A,RESULT,STRINGTYPE
:1407   EQU     *
        ((:1408-*)&$3FFF)+$4000
        RB
        ((:1409-*)&$3FFF)+$8000
        OPSS1
        $2F
        OPLDB
        $2F
        $3A,RESULT,NUMERICTYPE
:1409   EQU     *
:1408   EQU     *
        $2C
:1406   EQU     *
        $2A
        $34
        $3A,RESULT,STRINGTYPE
        $29
.       SET     *
        ORG     OPLSMI*2+.SA
        #.
        ORG     .
        $30
        FCC       '       0'
        '++$80
        $29
.       SET     *
        ORG     OPLOAD*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '0+$80
        $29
.       SET     *
        ORG     OPLV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '1+$80
        $29
.       SET     *
        ORG     OPLADDR*2+.SA
        #.
        ORG     .
        OPLINT
        $29
.       SET     *
        ORG     OPLINT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '3+$80
        $29
.       SET     *
        ORG     OPLFI*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '5+$80
        $29
.       SET     *
        ORG     OPLDB*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '6+$80
        $29
OPDUP   EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     OPDUP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '7+$80
        $29
.       SET     *
        ORG     OPLSD*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '8+$80
        $29
.       SET     *
        ORG     OPLSC*2+.SA
        #.
        ORG     .
        $30
        FCC       '       01'
        '9+$80
        $29
.       SET     *
        ORG     OPLEN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '0+$80
        $29
.       SET     *
        ORG     OPSS1*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '1+$80
        $29
.       SET     *
        ORG     OPSS2*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '2+$80
        $29
.       SET     *
        ORG     OPVSA*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '3+$80
        $29
.       SET     *
        ORG     OPVSV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '4+$80
        $29
.       SET     *
        ORG     OPVSS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '5+$80
        $29
.       SET     *
        ORG     OPVPA*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '6+$80
        $29
.       SET     *
        ORG     OPASA*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '7+$80
        $29
.       SET     *
        ORG     OPASV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '8+$80
        $29
.       SET     *
        ORG     OPASS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       02'
        '9+$80
        $29
.       SET     *
        ORG     OPAPA*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '0+$80
        $29
.       SET     *
        ORG     OPST*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '1+$80
        $29
.       SET     *
        ORG     OPSTS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '2+$80
        $29
.       SET     *
        ORG     OPSTB*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '3+$80
        $29
.       SET     *
        ORG     OPADD*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '4+$80
        $29
.       SET     *
        ORG     OPSUB*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '5+$80
        $29
.       SET     *
        ORG     OPNEG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '6+$80
        $29
.       SET     *
        ORG     OPMUL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '7+$80
        $29
.       SET     *
        ORG     OPDIV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '8+$80
        $29
.       SET     *
        ORG     OPAND*2+.SA
        #.
        ORG     .
        $30
        FCC       '       03'
        '9+$80
        $29
.       SET     *
        ORG     OPIOR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '0+$80
        $29
.       SET     *
        ORG     OPSHF*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '1+$80
        $29
.       SET     *
        ORG     OPCOM*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '2+$80
        $29
.       SET     *
        ORG     OPEQ*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '3+$80
        $29
.       SET     *
        ORG     OPNE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '4+$80
        $29
.       SET     *
        ORG     OPLT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '5+$80
        $29
.       SET     *
        ORG     OPLE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '6+$80
        $29
.       SET     *
        ORG     OPGE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '7+$80
        $29
.       SET     *
        ORG     OPGT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '8+$80
        $29
.       SET     *
        ORG     OPBF*2+.SA
        #.
        ORG     .
        $30
        FCC       '       04'
        '9+$80
        $29
.       SET     *
        ORG     OPBT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '0+$80
        $29
.       SET     *
        ORG     OPINV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '1+$80
        $29
.       SET     *
        ORG     OPJMP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '2+$80
        $29
.       SET     *
        ORG     OPGSB*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '3+$80
        $29
.       SET     *
        ORG     OPRET*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '4+$80
        $29
.       SET     *
        ORG     OPSTP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '5+$80
        $29
.       SET     *
        ORG     OPLINE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '6+$80
        $29
.       SET     *
        ORG     OPCALL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '7+$80
        $29
.       SET     *
        ORG     OPPV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '8+$80
        $29
.       SET     *
        ORG     OPPCR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       05'
        '9+$80
        $29
.       SET     *
        ORG     OPPS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '0+$80
        $29
.       SET     *
        ORG     OPPSP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '1+$80
        $29
.       SET     *
        ORG     OPINS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '2+$80
        $29
.       SET     *
        ORG     OPCHNL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '3+$80
        $29
.       SET     *
        ORG     OPCIN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '4+$80
        $29
.       SET     *
        ORG     OPCAT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '5+$80
        $29
.       SET     *
        ORG     OPCND*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '6+$80
        $29
.       SET     *
        ORG     OPRIGHT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '7+$80
        $29
.       SET     *
        ORG     OPRESTR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '8+$80
        $29
.       SET     *
        ORG     OPWV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       06'
        '9+$80
        $29
.       SET     *
        ORG     OPWS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '0+$80
        $29
.       SET     *
        ORG     OPRV*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '1+$80
        $29
.       SET     *
        ORG     OPRS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '2+$80
        $29
.       SET     *
        ORG     OPFOR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '3+$80
        $29
.       SET     *
        ORG     OPNEXT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '4+$80
        $29
.       SET     *
        ORG     OPTABCOL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '5+$80
        $29
.       SET     *
        ORG     OPINP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '6+$80
        $29
.       SET     *
        ORG     OPEOF*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '7+$80
        $29
.       SET     *
        ORG     OPINL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '8+$80
        $29
.       SET     *
        ORG     OPTAB*2+.SA
        #.
        ORG     .
        $30
        FCC       '       07'
        '9+$80
        $29
.       SET     *
        ORG     OPSEQ*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '0+$80
        $29
.       SET     *
        ORG     OPSNE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '1+$80
        $29
.       SET     *
        ORG     OPSLT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '2+$80
        $29
.       SET     *
        ORG     OPSLE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '3+$80
        $29
.       SET     *
        ORG     OPSGT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '4+$80
        $29
.       SET     *
        ORG     OPSGE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '5+$80
        $29
.       SET     *
        ORG     OPON*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '6+$80
        $29
.       SET     *
        ORG     OPXOR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '7+$80
        $29
.       SET     *
        ORG     OPOKE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '8+$80
        $29
.       SET     *
        ORG     OPEEK*2+.SA
        #.
        ORG     .
        $30
        FCC       '       08'
        '9+$80
        $29
.       SET     *
        ORG     OPUSE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '0+$80
        $29
.       SET     *
        ORG     OPRMPT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '1+$80
        $29
.       SET     *
        ORG     OPINT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '2+$80
        $29
.       SET     *
        ORG     OPERR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '3+$80
        $29
.       SET     *
        ORG     OPOPN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '4+$80
        $29
.       SET     *
        ORG     OPCLS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '5+$80
        $29
.       SET     *
        ORG     OPCREAT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '6+$80
        $29
.       SET     *
        ORG     OPCHAIN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '7+$80
        $29
.       SET     *
        ORG     OPLFREFL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '8+$80
        $29
.       SET     *
        ORG     OPELN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       09'
        '9+$80
        $29
.       SET     *
        ORG     OPPI*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '0+$80
        $29
.       SET     *
        ORG     OPFIND*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '1+$80
        $29
.       SET     *
        ORG     OPUSL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '2+$80
        $29
.       SET     *
        ORG     OPJUC*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '3+$80
        $29
OPINIT  EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     OPINIT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '4+$80
        $29
.       SET     *
        ORG     OPZCHN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '5+$80
        $29
.       SET     *
        ORG     OPONG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '6+$80
        $29
.       SET     *
        ORG     OPDEL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '7+$80
        $29
.       SET     *
        ORG     OPREN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '8+$80
        $29
.       SET     *
        ORG     OPSSL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       10'
        '9+$80
        $29
.       SET     *
        ORG     OPSTORE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '0+$80
        $29
.       SET     *
        ORG     OPSGN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '1+$80
        $29
.       SET     *
        ORG     OPLBOOL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '3+$80
        $29
.       SET     *
        ORG     OPPRUS*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '4+$80
        $29
.       SET     *
        ORG     OPSETLABEL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '5+$80
        $29
.       SET     *
        ORG     OPMAXLEN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '6+$80
        $29
.       SET     *
        ORG     OPSYSCALL1ARG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '7+$80
        $29
.       SET     *
        ORG     OPSYSCALL2ARG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '8+$80
        $29
.       SET     *
        ORG     OPSYSCALL3ARG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       11'
        '9+$80
        $29
.       SET     *
        ORG     OPSYSCALL4ARG*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '0+$80
        $29
.       SET     *
        ORG     OPCOPYRIGHT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '1+$80
        $29
OPEXTENDED      EQU     .SC
.SC      SET     .SC+1
.       SET     *
        ORG     OPEXTENDED*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '2+$80
        $29
.       SET     *
        ORG     OPSUBRET*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '3+$80
        $29
.       SET     *
        ORG     OPFNCALL*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '4+$80
        $29
.       SET     *
        ORG     OPLBINT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '5+$80
        $29
.       SET     *
        ORG     OPEXITLOOP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '6+$80
        $29
.       SET     *
        ORG     OPTRP*2+.SA
        #.
        ORG     .
        $30
        FCC       '       12'
        '7+$80
        $29
.       SET     *
        ORG     OPATN*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '0+$80
        $29
.       SET     *
        ORG     OPSIN*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '1+$80
        $29
.       SET     *
        ORG     OPCOS*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '2+$80
        $29
.       SET     *
        ORG     OPLOG*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '3+$80
        $29
.       SET     *
        ORG     OPEXP*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '4+$80
        $29
.       SET     *
        ORG     OPSQR*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '5+$80
        $29
.       SET     *
        ORG     OPRND*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '6+$80
        $29
.       SET     *
        ORG     OPABS*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '7+$80
        $29
.       SET     *
        ORG     OPDBG*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '8+$80
        $29
.       SET     *
        ORG     OPOWER*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       00'
        '9+$80
        $29
.       SET     *
        ORG     OPERRST*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '0+$80
        $29
.       SET     *
        ORG     OPSETSEED*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '1+$80
        $29
.       SET     *
        ORG     OPCOL*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '2+$80
        $29
.       SET     *
        ORG     OPGPOP*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '3+$80
        $29
.       SET     *
        ORG     OPDAT*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '4+$80
        $29
.       SET     *
        ORG     OPTIM*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '5+$80
        $29
.       SET     *
        ORG     OPNUM*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '6+$80
        $29
.       SET     *
        ORG     OPNUMF*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '7+$80
        $29
.       SET     *
        ORG     OPVAL*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '8+$80
        $29
.       SET     *
        ORG     OPTAN*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       01'
        '9+$80
        $29
.       SET     *
        ORG     OPHEX*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '1+$80
        $29
.       SET     *
        ORG     OPGOELN*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '2+$80
        $29
.       SET     *
        ORG     OPUPPERC*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '3+$80
        $29
.       SET     *
        ORG     OPLOWERC*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '4+$80
        $29
.       SET     *
        ORG     OPEXIT*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '5+$80
        $29
.       SET     *
        ORG     OPLENVECTOR*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '6+$80
        $29
.       SET     *
        ORG     OPROWSARRAY*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '7+$80
        $29
.       SET     *
        ORG     OPCOLSARRAY*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '8+$80
        $29
.       SET     *
        ORG     OPFUNRET*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       02'
        '9+$80
        $29
.       SET     *
        ORG     OPCHR*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       03'
        '0+$80
        $29
.       SET     *
        ORG     OPERRCAUSE*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       03'
        '1+$80
        $29
.       SET     *
        ORG     OPSSA*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       03'
        '2+$80
        $29
.       SET     *
        ORG     OPSAINIT*2+.SA
        #.
        ORG     .
        OPEXTENDED
        $2F
        $30
        FCC       '       03'
        '3+$80
        $29
.       SET     *
        ORG     OPLSCALAR*2+.SA
        #.
        ORG     .
        $2E
        $32,1
        $30
        FCC       '+'
        '(+$80
        $32,1
        $30
        FCC       '<64)*$80+'
        '(+$80
        $32,1
        $30
        FCC       '>/64)*352'
        '0+$80
        $2F
        $29
.       SET     *
        ORG     OPSSCALAR*2+.SA
        #.
        ORG     .
        $2E
        $32,1
        $30
        FCC       '+'
        '(+$80
        $32,1
        $30
        FCC       '<64)*$C0+'
        '(+$80
        $32,1
        $30
        FCC       '>/64)*300'
        '8+$80
        $2F
        $29
.       SET     *
        ORG     ALLOCATESCALAR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       EQU     .NS'
        'V+$80
        $2F
        $30
        FCC       '.NSV   SET     .NSV+'
        '1+$80
        $29
.       SET     *
        ORG     SADDR*2+.SA
        #.
        ORG     .
        ADDR
        $30
        FCC       ':+.TNSS+6'
        '*+$80
        $29
.       SET     *
        ORG     LABEL*2+.SA
        #.
        ORG     .
        $30
        FCC       ''
        ':+$80
        $29
.       SET     *
        ORG     GENLBL*2+.SA
        #.
        ORG     .
        $30
        FCC       ':'
        ':+$80
        $29
.       SET     *
        ORG     EQUHERE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       EQU     '
        '*+$80
        $29
.       SET     *
        ORG     SETHERE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       SET     '
        '*+$80
        $29
.       SET     *
        ORG     EQUDATA*2+.SA
        #.
        ORG     .
        $30
        FCC       '       EQU     :'
        ':+$80
        $29
.       SET     *
        ORG     INIT*2+.SA
        #.
        ORG     .
        OPINIT
        $2F
        DADDR
        $30
        FCC       ':'
        ':+$80
        $2F
        INTEGER
        $29
.       SET     *
        ORG     RESDATA*2+.SA
        #.
        ORG     .
        $30
        FCC       '::     SET     ::'
        '++$80
        $29
.       SET     *
        ORG     DADDR*2+.SA
        #.
        ORG     .
        ADDR
        $30
        FCC       ':'
        '++$80
        $29
.       SET     *
        ORG     SIXBYTEINTEGER*2+.SA
        #.
        ORG     .
        INTEGER
        $30
        FCC       ''
        '0+$80
        $2F
        INTEGER
        $30
        FCC       ''
        '0+$80
        $2F
        INTEGER
        $32,1
        $29
.       SET     *
        ORG     SIXBYTEFNUM*2+.SA
        #.
        ORG     .
        $30
        FCC       '       FFC'
        '       +$80
        $32,1
        $29
.       SET     *
        ORG     SIXBYTEHEXINTEGER*2+.SA
        #.
        ORG     .
        INTEGER
        $30
        FCC       ''
        '0+$80
        $2F
        INTEGER
        $30
        FCC       ''
        '0+$80
        $2F
        INTEGER
        $30
        FCC       ''
        '$+$80
        $32,1
        $29
.       SET     *
        ORG     INITSCALARHEAD*2+.SA
        #.
        ORG     .
        OPINIT
        $2F
        SADDR
        $30
        FCC       '.NS'
        'V+$80
        $2F
        INTEGER
        $30
        FCC       ''
        '6+$80
        $29
.       SET     *
        ORG     INITVECTORHEAD*2+.SA
        #.
        ORG     .
        $32,1
        EQUDATA
        $2F
        GENLBL
        $33,1
        EQUHERE
        $30
        FCC       '+'
        '7+$80
        $2F
        INIT
        $30
        FCC       ''
        '(+$80
        GENLBL
        $33,2
        $30
        FCC       ')*6+'
        '2+$80
        $2F
        INTEGER
        GENLBL
        $33,2
        $30
        FCC       '-'
        '1+$80
        $29
.       SET     *
        ORG     INITVECTORTAIL*2+.SA
        #.
        ORG     .
        GENLBL
        $33,2
        $30
        FCC       '       EQU     (*'
        '-+$80
        GENLBL
        $33,1
        $30
        FCC       ')/'
        '6+$80
        $2F
        RESDATA
        GENLBL
        $33,2
        $30
        FCC       '*6+'
        '2+$80
        $29
.       SET     *
        ORG     ORIGIN*2+.SA
        #.
        ORG     .
        $30
        FCC       '       ORG'
        '       +$80
        $29
.       SET     *
        ORG     BYTE*2+.SA
        #.
        ORG     .
        $30
        FCC       '       '
        '++$80
        $29
.       SET     *
        ORG     RES*2+.SA
        #.
        ORG     .
        $30
        FCC       '       RMB'
        '       +$80
        $29
.       SET     *
        ORG     TEXT*2+.SA
        #.
        ORG     .
        $30
        FCC       '       FCC'
        '       +$80
        $29
.       SET     *
        ORG     ADDR*2+.SA
        #.
        ORG     .
        $30
        FCC       '       '
        '#+$80
        $29
.       SET     *
        ORG     INTEGER*2+.SA
        #.
        ORG     .
        $30
        FCC       '       '
        '#+$80
        $29
.       SET     *
        ORG     CODEBODY*2+.SA
        #.
        ORG     .
        $30
        FCC       '.COD'
        'E+$80
        $29
:21     EQU     *
        END
