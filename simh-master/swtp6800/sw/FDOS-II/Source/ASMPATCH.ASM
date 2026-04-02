        NAM     ASMPATCH
        OPT     O,NOM,NOP 
*THIS PROGRAM PATCHES THE M6800 ASSEMBLER
*TO REQUEST THE USER TO SELECT IF OUTPUT IS 
*TO BE SENT THE PRINTER DEVICE USING THE 
* PROM DRIVER
*
*  From Bitsaver's manual
*  M6800_Co-Resident_Assembler_Reference_Manual_1976.pdf
*  Modified to work with FDOS-II under SimH
*  by Roberto Sancho, Mar/26
*
PASS    EQU     0
XSTACK  EQU     $E047
AECHO   EQU     $E00C     ECHO ON/OFF FLAG =0 -> ECHO, =1 -> NO ECHO
ATOPM   EQU     $E014     ADDRESS OF TOP MEMORY. 
XDATA   EQU     $11E      asmb console string out
XCIE    EQU     $11B      asmb console in char
XHEAD   EQU     $133      asmb string out (w/leading <CR>)
XLINE   EQU     $136      asmb string out
LDATA   EQU     $EEEF     printer string out (w/leading <CR>)
LDATA1  EQU     $EEF1     printer string out
ASMB    EQU     $100      assembler cold start
ENDSTBL EQU     $301      default end-of-symbol table address
*
        ORG     $20       assembler entry point from FDOS-II
*
        LDX     ATOPM     get top of ram
        STX     ENDSTBL   set end-of-symbol table address 
        LDS     #XSTACK
        LDAA    PASS
        CMPA    #$9       OBJECT ONLY?
        BEQ     ASM       YES
TOP     LDX     #MSG
        JSR     XDATA
        LDX     #0
DELAY   DEX
        BNE     DELAY
        CLR     AECHO     set echo on for user typed chars in console
        JSR     XCIE
        CMPA    #'Y
        BEQ     PRNTR
        CMPA    #'N
        BNE     TOP
ASM     JMP     ASMB
PRNTR   LDX     #LDATA
        STX     XHEAD+1
        LDX     #LDATA1
        STX     XLINE+1
        BRA     ASM
MSG     FCC     /PRINTER?/
        FCB     4
        END
