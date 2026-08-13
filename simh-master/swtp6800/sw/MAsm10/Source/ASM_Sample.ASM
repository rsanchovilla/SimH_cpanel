        NAM     HOLA
        OPT     O,NOM,NOP 
*
* PRINT HOLA IN CONSOLE
*
* SWTBUG ROM ENTRY POINTS
*
MON     EQU     $E0D0     Monitor warm entry point
CO      EQU     $E075     CO    console char out
*
* EXBUG  ROM ENTRY POINTS
*
EXBUG   EQU    $F564     Monitor warm entry point
CONOUT  EQU    $F018     CONSOLE OUTPUT WITH NULLS LOGIC
*
* MINIBUG  ROM ENTRY POINTS
*
MIBUG   EQU    $FED6     Monitor warm entry point
OUTCH   EQU    $FEAE     CONSOLE OUTPUT WITH NULLS LOGIC
*
*
CR      EQU     $0D       CARRIAGE RETURN
LF      EQU     $0A       LINE FEED
EOF     EQU     $1A       END OF TEXT FILE
*
        ORG     $0100     Print Hola for SWTPC 6800
*
START   LDX     #MSG
        JSR     PRTSTR
        JMP     MON
*
MSG     FCB     CR,LF
        FCC     /Hola/
        FCB     CR,LF,0
*
PRTSTR  LDAA    0,X       Print StringZ at addr in reg X
        BEQ     PRTEND
        JSR     CO
        INX 
        BRA     PRTSTR
PRTEND  RTS     
*
        ORG     $0200     Print Hola for Motorola Exorciser
*
        LDX     #MSG
        JSR     PRTST2
        JMP     EXBUG
*
PRTST2  LDAA    0,X       Print StringZ at addr in reg X
        BEQ     PRTEN2
        JSR     CONOUT
        INX 
        BRA     PRTST2
PRTEN2  RTS     
*
        ORG     $0300     Print Hola for Motorola MEK 6800
*
        LDX     #MSG
        JSR     PRTST3
        JMP     MIBUG
*
PRTST3  LDAA    0,X       Print StringZ at addr in reg X
        BEQ     PRTEN3
        JSR     OUTCH
        INX 
        BRA     PRTST3
PRTEN3  RTS     
*
        END
*
