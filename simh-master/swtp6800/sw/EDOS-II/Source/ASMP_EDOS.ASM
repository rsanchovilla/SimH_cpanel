        NAM     ASMPATCH
        OPT     O,NOM,NOP 
*THIS PROGRAM PATCHES THE M6800 ASSEMBLER
*TO REQUEST THE USER TO SELECT IF OUTPUT IS 
*TO BE SENT THE PRINTER DEVICE USING THE 
* PROM DRIVER
*
*  From Bitsaver's manual
*  M6800_Co-Resident_Assembler_Reference_Manual_1976.pdf
*  Modified to work with EDOS-II under SimH
*  by Roberto Sancho, Mar/26
*
*
PASS   EQU    0
XSTACK EQU    $FF8A 
AECHO  EQU    $FF53     ECHO ON/OFF FLAG =0 -> ECHO, =1 -> NO ECHO
XDATA  EQU    $11E      asmb console string out
XCIE   EQU    $11B      asmb console in char
XHEAD  EQU    $133      asmb string out (w/leading <CR>)
XLINE  EQU    $136      asmb string out
LDATA  EQU    $EAD5     printer string out (w/leading <CR>)
LDATA1 EQU    $EADD     printer string out
ASMB   EQU    $100      assembler cold start
ENDSTB EQU    $301      default end-of-symbol table address
*
       ORG    $20       assembler entry point from FDOS-II
*
       LDS    #XSTACK
       LDAA   PASS
       CMPA   #$9       OBJECT ONLY?
       BEQ    ASM       YES
TOP    LDX    #MSG
       JSR    XDATA
       LDX    #0
DELAY  DEX
       BNE    DELAY
       CLR    AECHO     set echo on for user typed chars in console
       JSR    XCIE
       CMPA   #'Y
       BEQ    PRNTR
       CMPA   #'N
       BNE    TOP
ASM    JMP    ASMB1
PRNTR  LDX    #LDATA
       STX    XHEAD+1
       LDX    #LDATA1
       STX    XLINE+1
       BRA    ASM
MSG    FCC    /PRINTER?/
       FCB    4
*
       ORG    $1F00
* 
ASMB1  LDX    #$7FFF    ASMB can hadle RAM <32K because uses signed arithmetic for free space computations
       STX    ENDSTB    set end-of-symbol table address 

       JMP    ASMB
*
* Astral 2000 assembler Patched in Mar/2026 by 
* Roberto Sancho to run on SimH emulator using
* Motorola Exorciser EXBUG, and 
* iCOM FD360 floppy disk emulation
*
* EXBUG ROM entry points
*
EXBUG  EQU    $F000    ; ENTER EXBUG
XINCH  EQU    $F012    ; CHAR IN
XOUTCH EQU    $F018    ; CHAR OUT
XPCRLF EQU    $F021    ; PRINT C/R L/F 
XPDATA EQU    $F024    ; PRINT C/R L/F THEN STRING
XPDAT1 EQU    $F027    ; PRINT STRING 
*
FDOS   EQU    $E800    ; LOAD FDOS
EXEC   EQU    $E803    ; START EXEC
RESET  EQU    $E806    ; INITIALIZE DISK CONTROLLER I/O
XRI    EQU    $E809    ; RI - DISK READ VECTOR
XWRT   EQU    $E80C    ; WRT - DISK WRITE VECTOR
UPDT   EQU    $E80F    ; UPDT - UPDATE DISK DIRECTORY ON CLOSING OUTPUT FILE
RESTR  EQU    $E82D    ; RESTR - RESTORE FILE POINTERS
*
* Set call to PRINT C/R L/F DATA ON CONSOLE
       ORG    $011F
       FDB    XPDATA 
*
* Set call to PRINT DATA POINTED AT BY X- 
       ORG    $0122
       FDB    XPDAT1
*
* Set call to PRINT C/R L/F ON CONSOLE
       ORG    $0125
       FDB    XPCRLF 
*
* Set call to CHAR IN
       ORG    $0158
       FDB    XINCH   
*
* Set call to CHAR OUT
       ORG    $0178
       FDB    XOUTCH 
*
* Set call to MONITOR
       ORG    $0131
       FDB    EXBUG
*
* Set stack
       ORG    $0947
       FDB    XSTACK
*
* Set call to WRT - DISK WRITE VECTOR
       ORG    $01D4
       FDB    XWRT   
*
* Set call to RI - DISK READ VECTOR
       ORG    $0283
       FDB    XRI    
*
* Set call to RESTR - RESTORE FILE POINTERS
       ORG    $0950
       FDB    RESTR  
*
* Set call to UPDT - UPDATE DISK DIRECTORY ON CLOSING OUTPUT FILE
       ORG    $0957
       FDB    UPDT   
*
       END
