        NAM     MINIBUGIO
        OPT     O,NOM,NOP 
*
* Motorola Assembler 1.0 I/O Patch
* Jun/2026 by Roberto Sancho 
* to run on MEK 6800 with MINIBUG ROM, 
* 
*
* MINIBUG ROM ROUTINES
*
EXBUG  EQU    $FED6     Monitor warm entry point
CONIN  EQU    $FE00     CONSOLE INPUT WITH ECHO
CONOUT EQU    $FEAE     CONSOLE OUTPUT WITH NULLS LOGIC
*
* VARS
*
AECHO  EQU    $A00C     =0 -> ECHO input chars, <>0 -> Do not ECHO
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
START0 LDX    #MASMB           PRINT CR LF ON 1ST START
       JSR    PRTSTR           
       JMP    $0AA8            GO TO ASMB TRUE START
MASMB  FCB    13,10,4
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
CMDLIN CLR    AECHO      Enable Input chars echo. (=0 -> ECHO input chars, <>0 -> Do not ECHO)
       JSR    CONIN      READ COMMAND CHAR FROM CONSOLE (1,2,S) WITH ECHO
       PSHA
       LDAA   #13        PRINT <CR><LF>
       JSR    CONOUT
       LDAA   #10
       JSR    CONOUT
       PULA
       RTS
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
       ORG    $06C3 
       ROL    AECHO     =0 -> ECHO input chars, <>0 -> Do not ECHO
*
       ORG    $0697     Patch to use for input Paper Tape reader 
       JMP    $06A1     (with control chars $11 PTR ON, $13 PTR OFF)
       ORG    $06DA     instead of High Speed Cassette Paper Tape input
       JMP    $06E6     
       ORG    $06B7
       RTS     
       ORG    $070E
       NOP              allows sending PTR OFF char
*
       ORG    $072D
       FCC    "1.0"     fix the version number
*
       ORG    $0AA8 
       LDX    #$1FFF    START OF SYMBOL TABLE
*       
       ORG    $0ABC     patch to read command line char from console
       JSR    CMDLIN
*
       ORG    $0ADB     enable options '3 - START PASS TWO (NO LISTING)' 
       NOP              and            '4 - START PASS TWO (NO OBJ GENERATED)' 
       NOP
*
       ORG    $0AF5 
       JMP    EXBUG     JUMP TO MONITOR
*
       ORG    $0F3F
       FCB    $27       REPLACE BNE WITH BEQ. THIS FIXES THE "OPT O" BUG
*                       WITHOUT THIS FIX, "OPT O" AND "OPT NOO" ARE REVERSED
*
       ORG    $168C
       JMP    PATCH1
*
*
*
       END


