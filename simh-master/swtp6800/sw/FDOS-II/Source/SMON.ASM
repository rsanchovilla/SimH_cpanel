* NAM SMON
* VERSION 1.00
*
  OPT O,NOP
****************************************************
* REPLACEMENT FOR MIKBUG ROM
* FOR SWTPC 6800 COMPUTER SYSTEM
* COPYRIGHT 1977
* SOUTHWEST TECHNICAL PROD. CORP.
* AUGUST, 1977
****************************************************
* Modified by Roberto Sancho Mar/2026 
* To be used with FDOS-II/6800 OEM RESIDENT (the FDOSRES.S19 file, as
* iCOM Drive build-in PROM)
*
* ASTRAL 2000 FDOS-II original memory map:
*      0000-8000 Regular RAM
*      E000      ASTMON ROM Monitor RAM
*      E400      Keyboard/console/papertape/cassette/printer PIAs
*      E800      ASTMON ROM Monitor 
*      F000      iCOM FD360 Floppy PROM (the FDOS-ROM resident part)
*      F800      iCOM FD360 Floppy PIAs
*      FD00      video display card PIAs
*      FF00      same as rom at EF00
*
* Memory map for SimH SWTPC emulator to run ASTRAL 2000 FDOS-II:
*
*      0000-8000 Regular RAM
*      8000      Swtpc I/O PIAs
*      E000-E0FF Monitor RAM
*      E400-E4FF Monitor RAM
*      E800-EFFF Monitor ROM (based on SWTPBUG) 
*      F000      iCOM FD360 Floppy PROM (the FDOS-ROM resident part)
*      F800      iCOM FD360 Floppy PIAs
*      FFF8-FFFF Monitor ROM 
*
* to be used in SIMH Swtpc with 64k active
*
* changes to SWTBUG
*     removed 'D' command (boot disk)
*     removed 'C' command (clear CT-1204 screen)
*     changed 'Z' command. Now restarts monitor
*
* Sets Variables expected by FDOS-II
*
* issue "J F000" at monitor '$' prompt to boot FDOS
*
CTLPOR EQU $8004        ; CONSOLE CONTROL PORT ADDRESS
PARADR EQU $801C        ; PARALLEL PORT ADDRESS
HIMEM  EQU $7FFF        ; LAST BYTE OF USABLE MEM (SHOULD BE < 32K TO AVOID FDOS-II EDIT/ASMB MALFUNCTION)
*
       ORG $E000        ; START OF MONITOR RAM
* 
IRQ    RMB 2            ; IRQ POINTER
BEGA   RMB 2            ; BEGINNING ADDR PNCH
ENDA   RMB 2            ; ENDING ADDR PNCH
NMI    RMB 2            ; NMI INTERRUPT VECTOR
SP     RMB 1            ; S HIGH
       RMB 1            ; S LOW
PORADD RMB 2            ; PORT ADDRESS
PORECH RMB 1            ; ECHO ON/OFF FLAG =0 -> ECHO, =1 -> NO ECHO
XHI    RMB 1            ; XREG HIGH
XLOW   RMB 1            ; XREG LOW
CKSM   RMB 1            ; CHECKSUM
XTEMP  RMB 2            ; X-REG TEMP STGE
SWIJMP RMB 2            ; SWI JUMP VECTOR
*
ATOPM  RMB 2            ; ADDRESS OF TOP MEMORY. SHOULD BE AT $E014 (USED BY FDOS-II EDITOR)
       RMB 6
CHAR   RMB 1            ; INPUT CHAR. SHOULD BE AT $E01C
CHARUC RMB 1            ; INPUT CHAR UPPER CASE AND NO PARITY AT $E01D
*
BKPT   RMB 2            ; BREAKPOINT ADDRESS
BKLST  RMB 1            ; BREAKPOINT DATA
       RMB 38  
STACK  RMB 2            ; SWTBUG STACK
SPBAS  RMB 2            ; STACK SAVE WHEN CALLED FROM BASIC. SHOULD BE AT $E049
TW     RMB 2            ; TEMPORARY STORAGE
TEMP   RMB 1            ; TEMPORARY STORAGE
BYTECT RMB 1            ; BYTECT AND MCONT TEMP.
*
RDRON  RMB 1            ; =1 -> INPUT FROM PAPER TAPE READER
PCHON  RMB 1            ; =1 -> OUTPUT TO PAPER TAPE PUNCH
LPTINI RMB 1            ; =1 -> Line printer initialized
XSAVE2 RMB 2            ; save X
XSAVE3 RMB 2            ; save X
*
       ORG $E0FE
*
ENTRY  RMB 2            ; USEG BY FDOS-II TO GET THE START ADDR ON PROGS LOAD
*                       ; WITH WHEN
*
   ORG $E400
*
PIASNS  RMB 1           ; if BIT7=1 -> no pending char to be read. =0 -> INCH ready to be read
*                       ; set by CO routine. Read by FDOS-II BASIC
PIAECO  RMB 1           ; used by CI routine. =$37 -> no echo, =$3F -> echo
*                       ; set by FDOS-II EXEC, BASIC and EDIT
*
   ORG $E800
*
*                       ; JUMP TABLE FOR CONSOLE
*                       ; PRINTER, PAPER TAPE PUNCH, PAPER TAPE READ
CI     JMP INCH1        ; CONSOLE IN
CO     JMP OUTCH1       ; CONSOLE OUT
RDRIN  JMP RDRIN1       ; PAPER TAPE READER IN
PO     JMP PCHOU1       ; PAPER TAPE PUNCH OUT
LO     JMP OUTCH        ; REDIRECT PRINTER TO CONSOLE     
MNTR   JMP MNTR1 
*
START0 CLR RDRON        ; =0 -> NO INPUT FROM PAPER TAPE READER
       CLR PCHON        ; =0 -> NO OUTPUT TO PAPER TAPE PUNCH
       CLR LPTINI       ; =0 -> LINE PRINTER NOT INITIALIZED
       JMP START
*
RDRIN1 TST RDRON        ; IS TAPE READER ACTIVE?
 BNE   RDRIN0           ; JUMP IF SO
 PSHB                   ; SAVE B,X
 STX   XSAVE2
 JSR   RDON             ; TURN TAPE READER ON
 PULB
 LDX   XSAVE2           ; RESTORE B,X
 LDAA  #$01
 STAA  RDRON            ; =1 -> PAPER TAPE READER ACTIVE
 STAA  PORECH           ; NO ECHO
RDRIN0 JSR   INCH
 CLC                    ; CLEAR CARRY TO AS NO END ON PAPER TAPE INPUT 
 RTS
*
INCH1  TST RDRON        ; IS TAPE READER ACTIVE?
 BEQ   INCH0            ; JUMP IF NOT
 PSHB                   ; SAVE B,X
 STX   XSAVE2
 JSR   RDOFF            ; TURN TAPE READER OFF
 PULB
 LDX   XSAVE2           ; RESTORE B,X
 CLR   RDRON            ; =0 -> PAPER TAPE READER NOT ACTIVE
 CLR   PORECH           ; RESTORE ECHO ON
INCH0  LDAA PORECH      ; INPUT CHAR FROM CONSOLE (BLOCKING)
 PSHA                   ; SAVE ECHO STATE
 LDAA  PIAECO           ; =$37 -> no echo, =$3F -> echo
 ANDA  #$08
 EORA  #$08             ; now echo -> 0, no echo -> <>0
 STAA  PORECH           ; SET PORECH ECHO accorging to PIAECHO
 JSR   INCH
 STAA  CHAR             ; SAVE ORIGINAL CHAR
 PULA
 STAA  PORECH           ; RESTORE ECHO STATE
*                       ; CONVERT A TO UPPERCASE 
 LDAA  CHAR
 ANDA  #$7F
 CMPA  #$61
 BLT   CVUC1
 CMPA  #$7B
 BGE   CVUC1
 SUBA  #$20
CVUC1  STAA CHARUC      ; SAVE & RETURN UPPER CASE CHAR
 RTS
*
PCHOU1 TST PCHON        ; IS TAPE PUNCH ACTIVE?
 BNE   PCHOU0           ; JUMP IF SO
 PSHA
 PSHB        
 STX   XSAVE2           ; SAVE A,B,X
 JSR   PNCHON           ; TURN TAPE PUNCH ON
 LDAA  #$01
 STAA  PCHON            ; =1 -> PAPER TAPE PUNCH ACTIVE
 LDX   XSAVE2           ; RESTORE A,B,X
 PULB
 PULA
PCHOU0 JMP OUTCH            
*
OUTCH1 PSHA
       PSHB        
       STX XSAVE2       ; SAVE A,B,X
       TST PCHON        ; IS TAPE PUNCH ACTIVE?
       BEQ OUCH0        ; JUMP IF NOT
       LDAB #20         ;
OUTCHZ LDAA #$1A        ; PUNCH SEVERAL ^Z AS EOF
       JSR OUTCH 
       DECB
       BNE OUTCHZ
       JSR CRLF         ; AND THE A <CR><LF>
       JSR PNCHOF       ; TURN TAPE PUNCH OFF
       CLR PCHON        ; =0 -> PAPER TAPE PUNCH NOT ACTIVE       
       LDX XSAVE2
*                       ; SET BIT7=1 on PIASNS if NO char pending to be read with INCH
OUCH0  JSR  SAVGET      ; SAVE IXR, GET PORT# AND TYPE
       BNE  OUCH1       ; INPUT FROM PIA IF NOT
       LDAA 0,X         ; bit 0=0 -> no pending char to be read. 
       ANDA #$01
       EORA #$01
       ASRA     
       RORA           
       BRA  OUCH2
OUCH1  LDAA 0,X         ; LOOK FOR START BIT
       ANDA #$80
OUCH2  STAA PIASNS      ; update PIASNS: if BIT7=1 -> no pending char to be read. 
       PULB
       PULA
       LDX  XTEMP
       JMP  OUTCH  
*
MNTR1  CLR PORECH       ; TURN ECHO ON
       LDAA #$80
       STAA PIASNS      ; update PIASNS: if BIT7=1 -> no pending char to be read. 
       JMP START        ; START MONITOR 
*
GOTO   LDS  SPBAS       ; RELOAD SP SET BY BASIC (THE CALLING PROG) 
       RTI              ; RETURN TO IT
*
*I/O INTERRUPT SEQUENCE
IRQV LDX IRQ
 JMP 0,X

*JUMP TO USER PROGRAM
JUMP BSR BADDR
 JMP 0,X

*ASCII LOADING ROUTINE
LOAD JSR RDON           ; READER ON, DIS ECHO, GET P#
LOAD3 BSR INCH
 CMPA #'S
 BNE LOAD3              ; 1ST CHAR NOT S
 BSR INCH               ; READ CHAR
 CMPA #'9 
 BEQ LOAD21
 CMPA #'1
 BNE LOAD3              ; 2ND CHAR NOT 1
 CLR CKSM               ; ZERO CHECKSUM
 BSR BYTE               ; READ BYTE
 SUBA #2
 STAA BYTECT            ; BYTE COUNT
*BUILD ADDRESS
 BSR BADDR
*STORE DATA
LOAD11 BSR BYTE
 DEC BYTECT
 BEQ LOAD15             ; ZERO BYTE COUNT
 STAA 0,X               ; STORE DATA
 CMPA 0,X               ; DATA STORED?
 BNE LOAD19
 INX
 BRA LOAD11
LOAD15 INC CKSM
 BEQ LOAD3
LOAD19 LDAA #'?
 BSR OUTCH
LOAD21 JSR RDOFF
 JMP CONTRL

*BUILD ADDRESS
BADDR BSR BYTE          ; READ 2 FRAMES
 STAA XHI
 BSR BYTE
 STAA XLOW
 LDX XHI                ; LOAD IXR WITH NUMBER
 RTS

*INPUT BYTE (TWO FRAMES)
BYTE BSR INHEX          ; GET HEX CHAR
BYTE1 ASLA
 ASLA
 ASLA
 ASLA
 TAB
 BSR INHEX
 ABA
 TAB
 ADDB CKSM
 STAB CKSM
 RTS

OUTHL LSRA              ; OUT HEX LEFT BCD DIGIT
 LSRA
 LSRA
 LSRA
OUTHR ANDA #$F          ; OUT HEX RIGHT BCD DIGIT
 ADDA #$30
 CMPA #$39
 BLS OUTCH
 ADDA #$7

*OUTPUT ONE CHAR
OUTCH JMP OUTEEE
INCH JMP INEEE

*PRINT DATA POINTED TO BY X REG
PDATA2 BSR OUTCH
 INX
PDATA1 LDAA 0,X
 CMPA #4
 BNE PDATA2
 RTS                    ; STOP ON HEX 04

C1 JMP SWTCTL

*MEMORY EXAMINE AND CHANGE
CHANGE BSR BADDR
CHA51 LDX #MCL
 BSR PDATA1             ; C/R L/F
 LDX #XHI
 BSR OUT4HS             ; PRINT ADDRESS
 LDX XHI
 BSR OUT2HS             ; PRINT OLD DATA
 BSR OUTS               ; OUTPUT SPACE
ANOTH BSR INCH          ; INPUT CHAR
 CMPA #$20
 BEQ ANOTH
 CMPA #$D
 BEQ C1
 CMPA #'^               ; UP ARROW?
 BRA AL3                ; BRANCH FOR ADJUSTMENT
 NOP

*INPUT HEX CHARACTER
INHEX BSR INCH
INHEX1 SUBA #$30
 BMI C3
 CMPA #$9
 BLE IN1HG
 CMPA #$11
 BMI C3                 ; NOT HEX
 CMPA #$16
 BGT C3                 ; NOT HEX
 SUBA #7
IN1HG RTS

OUT2H LDAA 0,X          ; OUTPUT 2 HEX CHAR
OUT2HA BSR OUTHL        ; OUT LEFT HEX CHAR
 LDAA 0,X
 INX
 BRA OUTHR              ; OUTPUT RIGHT HEX CHAR

OUT4HS BSR OUT2H        ; OUTPUT 4 HEX CHAR + SPACE
OUT2HS BSR OUT2H        ; OUTPUT 2 HEX CHAR + SPACE

OUTS LDAA #$20          ; SPACE
 BRA OUTCH              ; (BSR & TRS)

*ENTER POWER ON SEQUENCE
START LDS #STACK
 BRA AL1                ; BRANCH FOR ADDRESS COMPATIBIL

********************************************
*PART OF MEMORY EXAMINE AND CHANGE
AL3 BNE SK1
 DEX
 DEX
 STX XHI
 BRA CHA51
SK1 STX XHI
 BRA AL4

EOE3 BRA CONTRL         ; BRANCH FOR MIKBUG EQUIV. CONT

AL4 CMPA #$30
 BCS CHA51
 CMPA #$46
 BHI CHA51
 BSR INHEX1
 JSR BYTE1
 DEX
 STAA 0,X               ; CHANGE MEMORY
 CMPA 0,X
 BEQ CHA51              ; DID CHANGE
 JMP LOAD19             ; DIDN'T CHANGE
C3 LDS SP
 BRA SWTCTL
**************************************************

*CONTINUE POWER UP SEQUENCE
AL1 STS SP              ; INIT TARGET STACK PTR.
 LDAA #$FF
 JSR SWISET
*CONFIGURE FOR PIA AND SEE IF OK
 LDX #CTLPOR
 JSR PIAINI             ; INIT PIA
 LDAA 0,X
 CMPA 2,X
 BRA AL2

 BRA PRINT              ; BRA FOR BILOAD

AL2 BNE CONTRL

*INITIALIZE AS ACIA
 LDAA #3                ; ACIA MASTER RESET
 STAA 0,X
 LDAA #$11
 STAA 0,X
 BRA CONTRL

*ENTER FROM SOFTWARE INTERRUPT
SF0 NOP
SFE1 STS SP             ; SAVE TARGETS STACK POINTER
*DECREMENT P COUNTER
 TSX
 TST 6,X
 BNE *+4
 DEC 5,X
 DEC 6,X
*PRINT CONTENTS OF STACK.
PRINT LDX #MCL
 JSR PDATA1
 LDX SP
 INX
 BSR OUT2HS             ; COND CODES
 BSR OUT2HS             ; ACC B
 BSR OUT2HS             ; ACC A
 BSR OUT4HS             ; IXR
 BSR OUT4HS             ; PGM COUNTER
 LDX #SP
 JSR OUT4HS             ; STACK POINTER
SWTCTL LDX SWIJMP
 CPX #SF0
 BEQ CONTR1

CONTRL LDS #STACK       ; SET CONTRL STACK POINTER
 LDX #CTLPOR            ; RESET TO CONTROL PORT
 STX PORADD
 CLR PORECH             ; TURN ECHO ON
 BSR SAVGET             ; GET PORT # AND TYPE
 BEQ POF1
 JSR PIAECH             ; SET PIA ECHO ON IF MP-C INTER
POF1 JSR PNCHOF         ; TURN PUNCH OFF
 JSR RDOFF              ; TURN READER OFF
CONTR1 LDX #MCLOFF
 JSR PDATA1             ; PRINT DATA STRING
 BSR INEEE              ; READ COMMAND CHARACTER

*COMMAND LOOKUP ROUTINE
LOOK LDX #TABLE
OVER CMPA 0,X
 BNE SK3
 JSR OUTS               ; SKIP SPACE
 LDX 1,X
 JMP 0,X
SK3 INX
 INX
 INX
 CPX #TABEND+3
 BNE OVER
SWTL1 BRA SWTCTL

*SOFTWARE INTERRUPT ENTRY POINT
SFE LDX SWIJMP          ; JUMP TO VECTORED SOFTWARE INT
 JMP 0,X

S9 FCB 'S,'9,4          ; END OF TAPE

***************************************************
MTAPE1 FCB $D,$A,$15,0,0,0,'S,'1,4    ; PUNCH FORMAT

MCLOFF FCB $13 ; READER OFF
MCL FCB $D,$A,$15,0,0,0,'$,4

***************************************************


*NMI SEQUENCE
NMIV LDX NMI            ; GET NMI VECTOR
 JMP 0,X

INEEE BRA INEEE1

*BYTE SEARCH ROUTINE
SEARCH JSR BADDR        ; GET TOP ADDRESS
 STX ENDA
 JSR BADDR              ; GET BOTTOM ADDRESS
 JSR BYTE               ; GET BYTE TO SEARCH FOR
 TAB
OVE LDAA 0,X
 STX XHI
 CBA
 BEQ PNT
 BRA INCR1
PNT LDX #MCL
 JSR PDATA1
 LDX #XHI
 BRA SKP0
***************************************************
OUTEEE BRA OUTEE1

*SAVE IXR AND LOAD IXR WITH CORRECT
*PORT NUMBER AND TEST FOR TYPE
SAVGET STX XTEMP ; STORE INDEX REGISTER
GETPT1 LDX PORADD
ISACIA PSHB
 LDAB 1,X
 CMPB 3,X
 PULB
 RTS
***************************************************

*CONTINUATION OF SEARCH ROUTINE
SKP0 JSR OUT4HS
 LDX XHI
INCR1 CPX ENDA
 BEQ SWTL1
 INX
 BRA OVE

INEEE1 BSR INCH8        ; INPUT 8 BIT CHARACTER
 ANDA #%01111111        ; GET RID OF PARITY BIT
 RTS

BILD INS                ; FIX UP STACK WHEN USING
 INS                    ; BINARY LOADER ON SWTPC TAPES
 INS

*INPUT ONE CHAR INTO ACC A
INCH8 PSHB              ; SAVE ACC B
 BSR SAVGET             ; SAVE IXR, GET PORT# AND TYPE
 BNE IN1                ; INPUT FROM PIA IF NOT
 LDAA #$15              ; RECONFIG FOR 8 BIT, 1 SB
 STAA 0,X
ACIAIN LDAA 0,X
 ASRA
 BCC ACIAIN             ; NOT READY
 LDAA 1,X               ; LOAD CHAR
 LDAB PORECH
 BEQ ACIOUT             ; ECHO
 BRA RES                ; DON'T ECHO

*OUTPUT ONE CHARACTER
OUTEE1 PSHB             ; SAVE ACC B
 BSR SAVGET
 BNE IOUT

ACIOUT LDAB #$11
 STAB 0,X
ACIOU1 LDAB 0,X
 ASRB
 ASRB
 BCC ACIOU1             ; ACIA NOT READY
 STAA 1,X               ; OUTPUT CHARACTER
RES PULB                ; RESTORE ACC B
 LDX XTEMP
 RTS

*PIA INPUT ROUTINE
IN1 LDAA 0,X            ; LOOK FOR START BIT
 BMI IN1
 BSR DDL                ; DELAY HALF BIT TIME
 LDAB #4                ; SET DEL FOR FULL BIT TIME
 STAB 2,X
 ASLB                   ; SET UP CNTR WITH 8
IN3 BSR DEL             ; WAIT ONE CHAR TIME
 SEC
 ROL 0,X
 RORA
 DECB
 BNE IN3
 BSR DEL                ; WAIT FOR STOP BIT
 LDAB PORECH            ; IS ECHO DESIRED?
 BEQ IOUT2              ; ECHO
 BRA RES                ; RESTORE IXR,ACCB
*PIA OUTPUT ROUTINE
IOUT BSR DDL1           ; DELAY ONE HALF BIT TIME
 LDAB #$A               ; SET UP COUNTER
 DEC 0,X                ; SET START BIT
 BSR DE                 ; START TIMER
OUT1 BSR DEL            ; DELAY ONE BIT TIME
 STAA 0,X               ; PUT OUT ONE DATA BIT
 SEC
 RORA                   ; SHIFT IN NEXT BIT
 DECB                   ; DECREMENT COUNTER
 BNE OUT1               ; TEST FOR 0
IOUT2 LDAB 2,X          ; TEST FOR STOP BITS
 ASLB                   ; SHIFT BIT TO SIGN
 BPL RES                ; BRA FOR 1 STOP BIT
 BSR DEL                ; DELAY FOR STOP BITS
 BRA RES
DEL TST 2,X             ; IS TIME UP
 BPL DEL
DE INC 2,X              ; RESET TIMER
 DEC 2,X
 RTS

DDL CLR 2,X             ; HALF BIT DELAY
DDL1 BSR DE
 BRA DEL


*OPTIONAL PORT ROUTINE
OPTL BSR INEEE1
 TAB
 CLR PORADD+1           ; SET I/O ADDRESS FOR $8000
 LDX PORADD
 BSR PIAINI             ; INITIALIZE PIA
 BSR PIAECH             ; SET ECHO
 LDX #TABLE1            ; P, L OR E
 TBA
 JMP OVER               ; LOOK AT TABLE FOR E, L OR P

PIAECH LDAA #$34        ; SET DDR
 STAA 3,X
 STAA 2,X
NOOPT RTS

*PIA INITIALIZATION ROUTINE
PIAINI INC 0,X          ; SET DDR
 LDAA #$7
 STAA 1,X
 INC 0,X
 STAA 2,X
 RTS

*GENERAL PURPOSE DELAY LOOP
DELAY LDX #$FFFF
DELAY1 DEX
 CPX #$8014             ; STOP AT 8014
DUM BNE DELAY1
 RTS

*BREAKPOINT ENTERING ROUTINE
BREAK LDX #SF0
 CPX SWIJMP             ; BREAKPOINTS ALREADY IN USE?
 BEQ INUSE
 INX
BREAK0 BSR STO1
 JSR BADDR
 STX BKPT
 LDAA 0,X
 STAA BKLST
 LDAA #$3F
 STAA 0,X
 LDX #SF0
 BSR STO1
 JMP CONTR1
INUSE LDX BKPT
 LDAA BKLST
 STAA 0,X
 LDX #SFE1
 BRA BREAK0

SWISET STAA STACK+1     ; FIX POWER UP INTERRUPT
 LDX #HIMEM             ; INIT ADDRESS OF TOP MEMORY
 STX ATOPM              
 LDX SWIJMP
 CPX #SF0
 BEQ STORTN
STO LDX #SFE1
STO1 STX SWIJMP
STORTN RTS

PUNCH1 BSR PUNCH
 BRA POFC4

*FORMAT END OF TAPE WITH PGM. CTR. AND S9
PNCHS9 LDX #$A049
 STX ENDA
 DEX
 BSR PUNCH2
 LDX #S9
PDAT JSR PDATA1
POFC4 BSR PNCHOF
 BSR DELAY
C4 JMP CONTRL

RDON CLR PORECH 
 INC PORECH             ; DISABLE ECHO FOR ACIA
 LDAA #$11              ; RON CHAR.
 LDAB #$20              ; STROBE CHAR
 BSR STROBE
 JSR ISACIA             ; CHECK TO SEE IF PIA
 BEQ RTNN
 LDAA #$3C              ; DISABLE PIA ECHO IF PIA
 STAA 3,X
RTNN RTS

RDOFF LDAA #$13         ; TURN READER OFF
 LDAB #$10
 BRA STROBE

PNCHON LDAA #$12
 LDAB #4
 BRA STROBE

PNCHOF LDAA #$14
 LDAB #$8

*PIA STROBING ROUTINE FOR PUNCH/READ ON/OFF
STROBE JSR OUTCH
 JSR GETPT1
 BEQ RTN1
 LDAA #2
 ORAB #1
 BSR STR2
 BSR STR1
 LDAA #2
 LDAB #1
 STAB 0,X
 BSR STR2
STR1 LDAA #6
STR2 STAA 1,X
 STAB 0,X
RTN1 RTS

*PUNCH FROM BEGINNING ADDRESS (BEGA) THRU
*ENDING ADDRESS (ENDA)
PUNCH LDX BEGA
PUNCH2 STX TW
 BSR PNCHON
PUN11 LDAA ENDA+1
 SUBA TW+1
 LDAB ENDA
 SBCB TW
 BNE PUN22
 CMPA #16
 BCS PUN23
PUN22 LDAA #15
PUN23 ADDA #4
 STAA BYTECT
 SUBA #3
 STAA TEMP
*PUNCH C/R L/F NULLS S1
 LDX #MTAPE1
 JSR PDATA1
 CLRB
*PUNCH FRAME COUNT
 LDX #BYTECT
 BSR PUNT2              ; PUNCH 2 HEX CHARACTERS
*PUNCH ADDRESS
 LDX #TW
 BSR PUNT2
 BSR PUNT2
*PUNCH DATA
 LDX TW
PUN32 BSR PUNT2         ; PUNCH ONE BYTE
 DEC TEMP
 BNE PUN32
 STX TW
 COMB
 PSHB
 TSX
 BSR PUNT2              ; PUNCH CHECKSUM
 PULB                   ; RESTORE STACK
 LDX TW
 DEX
 CPX ENDA
 BNE PUN11
RTN5 RTS

*PUNCH 2 HEX CHAR, UPDATE CHECKSUM
PUNT2 ADDB 0,X
 JMP OUT2H              ; OUTPUT 2 HEX CHAR AND RTS

*COMMAND TABLE
TABLE FCB 'G            ; GOTO
 FDB GOTO
 FCB 'Z                 ; GOTO PROM -> RESTART MONITOR
 FDB START
 FCB 'M                 ; MEMORY EXAM AND CHANGE
 FDB CHANGE
 FCB 'F                 ; BYTE SEARCH
 FDB SEARCH
 FCB 'R                 ; REGISTER DUMP
 FDB PRINT
 FCB 'J                 ; JUMP
 FDB JUMP
 FCB 'B                 ; BREAKPOINT
 FDB BREAK
 FCB 'O                 ; OPTIONAL PORT
 FDB OPTL
TABLE1 FCB 'P           ; ASCII PUNCH
 FDB PUNCH1
 FCB 'L                 ; ASCII LOAD
 FDB LOAD
TABEND FCB 'E           ; END OF TAPE
 FDB PNCHS9


OUTLPT PSHB
 STX XSAVE3             ; SAVE B,X
 LDX #PARADR            ; CONFIGURE PIA
 TST LPTINI             ; TEST IF PORT INITIALIZED
 BNE OULPT1             ; JUMP IF SO
 LDAB #$FF
 STAB 0,X
 LDAB #$3F
 STAB 1,X
 INC LPTINI     
OULPT1 STAA 0,X         ; SAVE CHAR TO SEND VIA MP-LA PORT
 LDAB #$37
 STAB 1,X               ; SEND CHAR TO PRINTER 
 LDAB #$3F        
 STAB 1,X
OULPT2 TST 1,X
 BPL OULPT2             ; WAIT UNTIL READY
 JSR OUTCH              ; ECHO PRINTED CHAR TO CONSOLE
 PULB
 LDX XSAVE3             ; RESTORE B,X
 RTS

*SEND TO LINE PRINTER DATA POINTED TO BY X REG
LDATA2 BSR OUTLPT
 INX
LDATA1 LDAA 0,X
 CMPA #4
 BNE LDATA2
 RTS                    ; STOP ON HEX 04

 ORG $ED5C              ; ROUTINE THAT SHOULD BE AT $ED5C, ... (USED BY FDOS-II FDSKO)

* GET 16 BIT VALUE IN X, A REG = TERMINATING CHAR
*
GV16   CLR BKPT
       CLR BKPT+1
G2     JSR INHEX2
       BNE G1           ; NOT HEX
       BSR SHFT
       BRA G2
G1     LDX BKPT
       TBA              ; TERMINATING CHAR
       RTS

SHFT   BSR G20
       BSR G20
       BSR G20
       BSR G20
       ADDA BKPT+1
       STAA BKPT+1
       RTS
*
G20    ASL BKPT
       ASL BKPT+1
       BCC G21
       INC BKPT
G21    RTS

INHEX2 JSR INCH
       TAB              ; CONVERT ASCII TO HEX
       SUBA #$30
       BMI G31          ; NOT HEX
       CMPA #$09
       BLE G32
       CMPA #$11
       BMI G31          ; NOT HEX
       CMPA #$16
       BGT G31          ; NOT HEX
       SUBA #7
G32    LDAB #0
G31    CMPB #0
       RTS


 ORG $EEEA              ; ROUTINES THAT SHOULD BE AT $EEEA, $EEEC, ... (USED BY FDOS-II ASMB)

   BSR CRLF             ; PRINT C/R L/F ON CONSOLE
   JMP PDATA1           ; PRINT DATA POINTED AT BY X-REG ON CONSOLE
   BSR LCRLF            ; PRINT C/R L/F ON LINE PRINTER
   JMP LDATA1           ; PRINT DATA POINTED AT BY X-REG ON LINE PRINTER

 ORG $EEF9              ; ROUTINE THAT SHOULD BE AT $EEF9 (USED BY FDOS-II ASMB)

CRLF STX XTEMP1
   LDX #MCRLF
   JSR PDATA1
   LDX XTEMP1
   RTS
XTEMP1 RMB 2
MCRLF  FCB $D,$A,0,0,0,0,4

LCRLF STX XTEMP1
   LDX #MCRLF
   JSR LDATA1
   LDX XTEMP1
   RTS

 ORG $FFF8

 FDB IRQV               ; IRQ VECTOR
 FDB SFE                ; SOFTWARE INTERRUPT
 FDB NMIV               ; NMI VECTOR
 FDB START0             ; RESTART VECTOR

 END

