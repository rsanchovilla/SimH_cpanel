        NAM     EXEC I/O PATCH
        OPT     O,NOM,NOP 
*
* Patches Sphere FDOS-I EXEC for EDOS-I with 
* correct addresses for iCOM disk controller PIA ports,
* Controller PROM routine addresses, and EXBUG ROM 
* routine adresses
* Roberto Sancho, May/25
*
* set iCOM controler addr
*
DKDID   EQU     $EC00     iCOM floppy disk controller PIA ports
DKDIC   EQU     $EC01
DKCOD   EQU     $EC02
DKCOC   EQU     $EC03 
DKDOD   EQU     $EC06 
DKDOC   EQU     $EC07 
*
        ORG     $01AD
        LDAB    DKDID
        ORG     $01B8 
        LDAB    DKDID
        ORG     $082B 
        LDAA    DKDID
        ORG     $0841 
        LDAA    DKDID
        ORG     $085B 
        LDAA    DKDID
        ORG     $089B 
        LDAA    DKDID
*
        ORG     $01B3
        LDAA    DKDIC
*
        ORG     $01B0 
        STAA    DKCOD
        ORG     $06A2 
        STAA    DKCOD
        ORG     $0858 
        STAB    DKCOD
        ORG     $0866 
        STAB    DKCOD
        CLR     DKCOD     
        STAB    $0884 
        FDB     DKCOD
*
        ORG     $0855 
        STAA    DKCOC
        ORG     $0863 
        STAA    DKCOC
*
        ORG     $069D 
        STAA    DKDOD
        ORG     $0881 
        STAA    DKDOD
*
*
* FDOS PROM ROUTINES
*
INTIO  EQU    $E806    EDOS ROM DISK I/F INITIALIZAT 
XRI    EQU    $E809    EDOS ROM DISK BYTE READ ROUTI 
XWRT   EQU    $E80C    EDOS ROM DISK WRITE ROUTINE
*
       ORG     $0101 
       JSR     INTIO
*
       ORG     $0264 
       JSR     XRI
       ORG     $02F4 
       JSR     XRI
       ORG     $033F 
       JSR     XRI
       ORG     $03A8 
       JSR     XRI
       ORG     $04D4 
       JSR     XRI
       ORG     $04F5 
       JSR     XRI
       ORG     $078D 
       JSR     XRI
*
       ORG     $0297 
       JSR     XWRT
       ORG     $02BB 
       JSR     XWRT
       ORG     $0461 
       JSR     XWRT
       ORG     $046C 
       JSR     XWRT
       ORG     $048A 
       JSR     XWRT
       ORG     $04A6 
       JSR     XWRT
       ORG     $0556 
       JSR     XWRT
       ORG     $05D8 
       JSR     XWRT
       ORG     $05DD 
       JSR     XWRT
       ORG     $067E 
       JSR     XWRT
       ORG     $0690 
       JSR     XWRT
       ORG     $0792 
       JSR     XWRT
*
PROG   EQU    $E815    EDOS ROM LOAD A PROGRAM & GO  
ASSEM  EQU    $E81B    LOAD ASSEMBLER
EDITR  EQU    $E824    EDOS ROM LOAD A PROGRAM, STAR 
*
       ORG     $02D2 
       JMP     PROG      
       ORG     $0740 
       JMP     ASSEM     
       ORG     $0428 
       JMP     EDITR
*
INCDA  EQU    $E9EF    SUBROUTINE TO INCR DA TRACK & SECTOR
XUS    EQU    $EA03    EDOS ROM TRANSMIT UNIT/SECTOR 
SEEK   EQU    $EA2B    EDOS ROM SEEK SELECTED UNIT T 
RFLAG  EQU    $EA38    EDOS ROM RESET ERROR FLAG
CHK    EQU    $EA4D    CHECK IF DISK INSERTED
CHK1   EQU    $EA57    DISPLAY ERROR DOE
*
       ORG     $0770 
       JSR     INCDA     
       ORG     $07C9 
       JSR     INCDA     
       ORG     $07F7 
       JSR     INCDA     
       ORG     $08A5 
       JSR     INCDA     
*
       ORG     $07AA 
       JSR     XUS       
       ORG     $07BC 
       JSR     XUS       
       ORG     $07DD 
       JSR     XUS       
       ORG     $07EA 
       JSR     XUS       
       ORG     $0893 
       JSR     XUS       
*
       ORG     $07B2 
       JSR     SEEK      
       ORG     $07E5 
       JSR     SEEK      
*
       ORG     $080C 
       JSR     RFLAG     
       ORG     $0832 
       JSR     RFLAG     
       ORG     $0871 
       JSR     RFLAG     
*
       ORG     $07AD 
       JSR     CHK       
       ORG     $07E0 
       JSR     CHK   
       ORG     $0816 
       JMP     CHK1  
*
*
* EXBUG ROM ROUTINES
*
EXBUG   EQU     $F564     Monitor warm entry point
CONIN   EQU     $F015     CONSOLE INPUT WITH ECHO
CONOUT  EQU     $F018     CONSOLE OUTPUT WITH NULLS LOGIC
OCHAR   EQU     $F9CF     CONSOLE OUTPUT WITHOUT NULLS
LSTOUT  EQU     $EAB0     OUTPUT BYTE TO LIST DEVICE
RDRIN   EQU     $EB02     PAPER TAPE READER IN 
INITR   EQU     $EB04     INIT HIGH SPEED RDR
*
        ORG     $0166 
        JMP     EXBUG     
        ORG     $08AF
        JMP     CONOUT
        JMP     CONIN
        JMP     OCHAR
        JMP     LSTOUT
        JMP     RDRIN
*
        ORG     $0365 
        JSR     OCHAR
        ORG     $036A 
        JSR     OCHAR
        ORG     $036F 
        JSR     OCHAR
        ORG     $0374 
        JSR     OCHAR
        ORG     $0388 
        JSR     OCHAR
        ORG     $038D 
        JSR     OCHAR
        ORG     $0392 
        JSR     OCHAR
        ORG     $0612 
        JSR     OCHAR
        ORG     $0617 
        JSR     OCHAR
        ORG     $0626 
        JSR     OCHAR
        ORG     $0661 
        JSR     OCHAR
*
        ORG     $0684
        JSR     INITR
*
SPEED   EQU     $FF02    EXBUG SPEED FLAG 
EXPUN   EQU     $FF62    EXBUG PUNCH FLAG
*
        ORG     $060B 
        TST     SPEED     EXBUG SPEED FLAG 
        ORG     $0360 
        ROL     EXPUN     
        ORG     $0395 
        CLR     EXPUN     
*
SIOCSR  EQU     $FCF4		ACIA0 CONTROL/STATUS REGISTER
SIODR   EQU     $FCF5		ACIA0 DATA REGISTER
SBITC   EQU     $FCFD 		STOP BIT CONTROL FOR ACIA
*
        ORG     $0621 
        STAA    SIOCSR    
        ORG     $0631 
        LDAA    SIOCSR    
        ORG     $065C 
        STAA    SIOCSR    
        ORG     $0642 
        LDAA    SIODR     
        ORG     $0664 
        LDAA    SIODR     
        ORG     $0667 
        LDAA    SIODR     
        ORG     $061C 
        LDAA    SBITC     
        ORG     $0657 
        LDAA    SBITC     
*
* Patch DC command to avoid sending High Speed control char
* sequences ($10 $30 $FF at beginning of line, $10 $39 at end
* of line)
*
        ORG     $0365 
        TST     $1111   COMMENT OUT CALL TO OCHAR 
        ORG     $036A   TO AVOID SENDING UNWANTED 
        TST     $1111   CONTROL CHAR 
        ORG     $036F 
        TST     $1111   
        ORG     $038D 
        TST     $1111   
        ORG     $0392 
        TST     $1111   
*
AECHO   EQU     $FF53    =0 -> ECHO input chars, <>0 -> Do not ECHO
*
        ORG     $01A2     THIS PATCH SETS EXBUG ECHO 
        CLR     AECHO     AND REMOVES EXPLICIT ECHO OF INPUT CHARS
        JSR     CONIN     console char in            
        ANDA    #$7F
        RTS
*
*
*
        END









