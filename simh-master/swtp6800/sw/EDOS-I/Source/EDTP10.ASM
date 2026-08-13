        NAM     EDTP EDIT 1.0 EDITOR PATCH
        OPT     O,NOM,NOP 
*-------------------------------------------------------------------------
*
* Sphere FDOS-I Editor 1.0 Patch
* Jun/2026 by Roberto Sancho 
* to run on Exorciser with EXBUG ROM, 
* EXORDisk-I controller and
* Motorola EDOS-I
*       
*
* EDOS PROM ROUTINES
*
XRI    EQU    $E809    EDOS ROM DISK BYTE READ ROUTI 
XWRT   EQU    $E80C    EDOS ROM DISK WRITE ROUTINE
UPDT   EQU    $E80F    UPDATE
*
* EXBUG ROM ROUTINES
*
CONIN  EQU    $F015    CONSOLE INPUT WITH ECHO
CONOUT EQU    $F018    CONSOLE OUTPUT WITH NULLS LOGIC
*
* Patch EDITOR I/O vectors
*
       ORG    $0023
       JMP    CONIN
       JMP    CONOUT
       JMP    XRI
       JMP    XWRT
*
       ORG    $07E3
       JMP    CONIN
*
       ORG    $05E1 
       JMP    UPDT
*
* Patch EDITOR welcome banner
*
       ORG    $078E
       FCB    34
       FCC    /RESIDENT EXORCISER EDITOR VER. 1.0/
*
* Patch editor to comment out explicit input echo
*
       ORG    $028B   
       TST    $0026    Instead of JSR CONOUT
*
* Patch legitimate original code bug:
*
* free space computations are
* made using signed integer compares. So if > 32k
* ($8000), it is intepreted as negative free space
*
* Solution: patch available mem detection routine 
* so it returns last byte of RAM always < $8000
*
       ORG    $01D5   
SETHI  LDAA   $88
       LDX    #$8000
SH1    DEX
       STAA   0,X
       CMPA   0,X
       BNE    SH1 
       STX    $44      HIMEM     Last addr of RAM
       RTS     
*
       END     
