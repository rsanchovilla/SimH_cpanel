        NAM     HOLA
        OPT     O,NOM,NOP 
*
* PRINT HOLA IN CONSOLE
*
EDOS    EQU     $E800     Boot EDOS
CO      EQU     $F018     CO    console char out
*
CR      EQU     $0D       CARRIAGE RETURN
LF      EQU     $0A       LINE FEED
EOF     EQU     $1A       END OF TEXT FILE
*
        ORG     $0100
*
START   LDX     #MSG
        JSR     PRTSTR
        JMP     EDOS
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
        END
*
