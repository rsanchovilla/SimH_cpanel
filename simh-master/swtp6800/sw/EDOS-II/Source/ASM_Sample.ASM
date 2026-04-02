        NAM     HOLA
        OPT     O,NOM,NOP 
*
* PRINT HOLA IN CONSOLE
*
FDOS    EQU     $F000     Boot FDOS
UPDT    EQU     $F003     FDOS call updt
*
CI      EQU     $F006     CI    console char in 
CO      EQU     $F009     CO    console char out
RDI     EQU     $F00C     RDI   paper tape reader in
LO      EQU     $F00F     LO    list output
PO      EQU     $F012     PO    punch output
EXIT    EQU     $F015     MONITOR
*
CR      EQU     $0D       CARRIAGE RETURN
LF      EQU     $0A       LINE FEED
EOF     EQU     $1A       END OF TEXT FILE
*
        ORG     $0100
*
START   LDX     #MSG
        JSR     PRTSTR
        JMP     FDOS
*
MSG     FCB     CR,LF
        FCC     /Hola/
        FCB     CR,LF,0
*
PRTSTR  LDAA    $0000,X   Print StringZ at addr in reg X
        BEQ     PRTEND
        JSR     CO
        INX 
        BRA     PRTSTR
PRTEND  RTS     
*
        ORG     $E0FE    This is the prog start addr
        FDB     START    used by RUNGO command
*
        END
*
