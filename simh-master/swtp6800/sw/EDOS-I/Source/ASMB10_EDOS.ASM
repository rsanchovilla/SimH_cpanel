        NAM     ASM EDOS
        OPT     O,NOM,NOP 
*
* Assembler 1.0 Patch
* May/2026 by Roberto Sancho 
* to run on Exorciser with EXBUG ROM, 
* EXORDisk-I controller and
* Motorola EDOS-I
* 
*
* EDOS PROM ROUTINES
*
XRI    EQU    $E809    EDOS ROM DISK BYTE READ ROUTI 
XWRT   EQU    $E80C    EDOS ROM DISK WRITE ROUTINE
RESTR  EQU    $E82D    RESTORE IFILE PNTRS
UPDT   EQU    $E80F    UPDATE
*
* EXBUG ROM ROUTINES
*
EXBUG  EQU    $F564     Monitor warm entry point
CONIN  EQU    $F015     CONSOLE INPUT WITH ECHO
CONOUT EQU    $F018     CONSOLE OUTPUT WITH NULLS LOGIC
*
* EXBUG VARS
*
AECHO  EQU    $FF53     =0 -> ECHO input chars, <>0 -> Do not ECHO
*
*
* Patch ASMB I/O vectors
*
       ORG    $022D
       JMP    PRTSTR    Send string to printer 
*
       ORG    $0250
       JMP    CONIN     Console In
*
       ORG    $025A
       JMP    CONOUT    Console Out
*
       ORG    $0268
       JMP    CHKHEX    check hex char
*
* From here up $3FF can be used for additional routines       
*
       ORG    $0300
*
CHKHEX PSHA             
       JSR    CHEX
       BCC    L0273
       LDAA   #$18
       TAP     
       PULA    
       RTS     
L0273  INS     
       RTS     
CHEX   SUB A  #$30 
       BCS    CHEX2 
       ADD A  #$E9 
       BCS    CHEX2 
       ADD A  #6 
       BPL    CHEX1 
       ADD A  #7 
       BCS    CHEX2 
CHEX1  ADD A  #10 
       CLC             
CHEX2  RTS             
*       
START0 LDX    #MASMB           PRINT ASMB BANNER 
       JSR    PRTSTR           "M6800 ASSEMBLER VERSION 1.0",13,10
       JMP    $0AA8            GO TO ASMB TRUE START
MASMB  FCC    /M6800 ASSEMBLER VERSION 1.0/
       FCB    13,10,4
*       
PRTSTR STX    XSAV      Send string to printer 
       LDAA   0,X       (rerouted to console)
       CMPA   #4
       BNE    PRTST2
       RTS     
PRTST2 JSR    CONOUT    Can be replaced by LPTOUT
       LDX    XSAV
       INX
       BRA    PRTSTR
XSAV   RMB    2       
*
CMDLIN LDAB   $00              GENERATE COMMAND CHAR FROM
       BEQ    CLSOBJ           VALUE IN $00 (EDOS ASMB COMMAND PARAMS)
       LDAA   #'1
       LSRB    
       BCS    CMDLI3
       DECA    
       ABA     
       CLRB    
CMDLI3 ASLB    
       STAB   $00
       JMP    CONOUT    PRINT ON CONSOLE THE PASS NUMBER 
CLSOBJ JMP    UPDT      UPDATE & CLOSE OUTPUT FILE, RETURN TO EDOS
*
PATCH1 CPX     #$01BD
       BEQ     L0208
       JMP     $15FD
L0208  JMP     $1691
*
       NOP              Should be at addr <= $03FF
*
* Patch ASMB main code
*
       ORG    $0400     patch ASMB entry point
       JMP    START0
*       
       ORG    $068D     patch to write obj file to disk
       TST    $0040
       BNE    WRTEX  
       TST    $0046
       BEQ    WRTEX  
WRTDSK LDAA   0,X   
       CMPA   #$04
       BEQ    WRTEX  
       PSHB    
       STX    XSAV
       JSR    XWRT      DISK WRITE 
       LDX    XSAV
       PULB    
       INX     
       BRA    WRTDSK
WRTEX  RTS    
*
       ORG    $06DA     patch to read source file from disk
       BRA    RDDSK2
RDDSK  PSHB    
       STX    XSAV
       JSR    XRI       DISK READ
       LDX    XSAV
       PULB    
       RTS     
       NOP 
       NOP
       NOP 
RDDSK2 JSR    RDDSK
*
       ORG    $06C3 
       ROL    AECHO     =0 -> ECHO input chars, <>0 -> Do not ECHO
*
       ORG    $0AA8 
       LDX    #$1FFF    START OF SYMBOL TABLE
*
       ORG    $0AB3     
       JSR    RESTR     RESTORE IFILE PNTRS (i.el. rewind source file)
*       
       ORG    $0ABC     patch to convert EDOS ASMB options in command char
       JSR    CMDLIN
*       
       ORG    $0ADB     patch to process correctly EDOS ASMB options
       NOP             
       NOP
*       
       ORG    $0AF5 
       JMP    EXBUG     JUMP TO MONITOR
*
       ORG    $168C
       JMP    PATCH1
*
*
*
       END


