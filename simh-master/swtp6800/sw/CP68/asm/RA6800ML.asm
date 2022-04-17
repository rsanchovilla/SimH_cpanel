        NAM ASM RESIDENT MACRO ASSEMBLER
*              (RELOCATING AND LINKING)
*              VERSION 1.0
*
* C.COPYRIGHT 1977 JACK E. HEMENWAY
* BOSTON MASS. ALL RIGHTS RESERVED
*
* MODIFIED BY ROBERTO SANCHO TO
* BE USED UNDER CP/68 OPERATING SYSTEM
* AND SIMH SWTPC MF68 DISK SIMULATOR
* (C) APR 2022
*
       ORG $2000
*
*     KEEP CP68 STACK POINTER
*     DO NO ISSUE: LDS #$A042
*
       JMP PASS1
*
* MNTAB MNEMONIC TABLE *
*
MNTAB  FCC 'ABA'
       FDB ADDR9
       FCB $1B
       FCC 'ADC'
       FDB ADDR1
       FCB $09
       FCC 'ADD'
       FDB ADDR1
       FCB $0B
       FCC 'AND'
       FDB ADDR1
       FCB $04
       FCC 'ASL'
       FDB ADDR3
       FCB $08
       FCC 'ASR'
       FDB ADDR3
       FCB $07
       FCC 'BCC'
       FDB ADDR8
       FCB $24
       FCC 'BCS'
       FDB ADDR8
       FCB $25
       FCC 'BEQ'
       FDB ADDR8
       FCB $27
       FCC 'BGE'
       FDB ADDR8
       FCB $2C
       FCC 'BGT'
       FDB ADDR8
       FCB $2E
       FCC 'BHI'
       FDB ADDR8
       FCB $22
       FCC 'BIT'
       FDB ADDR1
       FCB $05
       FCC 'BLE'
       FDB ADDR8
       FCB $2F
       FCC 'BLS'
       FDB ADDR8
       FCB $23
       FCC 'BLT'
       FDB ADDR8
       FCB $2D
       FCC 'BMI'
       FDB ADDR8
       FCB $2B
       FCC 'BNE'
       FDB ADDR8
       FCB $26
       FCC 'BPL'
       FDB ADDR8
       FCB $2A
       FCC 'BRA'
       FDB ADDR8
       FCB $20
       FCC 'BSR'
       FDB ADDR8
       FCB $8D
       FCC 'BVS'
       FDB ADDR8
       FCB $28
       FCC 'BVS'
       FDB ADDR8
       FCB $29
       FCC 'CBA'
       FDB ADDR9
       FCB $11
       FCC 'CLC'
       FDB ADDR9
       FCB $0C
       FCC 'CLI'
       FDB ADDR9
       FCB $0E
       FCC 'CLR'
       FDB ADDR3
       FCB $0F
       FCC 'CLV'
       FDB ADDR9
       FCB $0A
       FCC 'CMN'
       FDB POCMN
       FCB $FF
       FCC 'CMP'
       FDB ADDR1
       FCB $01
       FCC 'COM'
       FDB ADDR3
       FCB $03
       FCC 'CPX'
       FDB ADDR5
       FCB $8C
       FCC 'DAA'
       FDB ADDR9
       FCB $19
       FCC 'DEC'
       FDB ADDR3
       FCB $0A
       FCC 'DES'
       FDB ADDR9
       FCB $34
       FCC 'DEX'
       FDB ADDR9
       FCB $09
       FCC 'END'
       FDB POEND
       FCB $FF
       FCC 'ENT'
       FDB POENT
       FCB $FF
       FCC 'EOR'
       FDB ADDR1
       FCB $08
       FCC 'EQU'
       FDB POEQU
       FCB $FF
       FCC 'EXT'
       FDB POEXT
       FCB $FF
       FCC 'FCB'
       FDB POFCB
       FCB $FF
       FCC 'FCC'
       FDB POFCC
       FCB $FF
       FCC 'FDB'
       FDB POFDB
       FCB $FF
       FCC 'IF '
       FDB POIF
       FCB $FF
       FCC 'INC'
       FDB ADDR3
       FCB $0C
       FCC 'INS'
       FDB ADDR9
       FCB $31
       FCC 'INX'
       FDB ADDR9
       FCB $08
       FCC 'JMP'
       FDB ADDR7
       FCB $6E
       FCC 'JSR'
       FDB ADDR7
       FCB $AD
       FCC 'LDA'
       FDB ADDR1
       FCB $06
       FCC 'LDS'
       FDB ADDR5
       FCB $8E
       FCC 'LDX'
       FDB ADDR5
       FCB $CE
       FCC 'LSR'
       FDB ADDR3
       FCB $04
       FCC 'MAC'
       FDB POMAC
       FCB $FF
       FCC 'NAM'
       FDB PONAM
       FCB $FF
       FCC 'NEG'
       FDB ADDR3
       FCB $00
       FCC 'NIF'
       FDB PONIF
       FCB $FF
       FCC 'NOP'
       FDB ADDR9
       FCB $02
       FCC 'ORA'
       FDB ADDR1
       FCB $0A
       FCC 'PAG'
       FDB POPAG
       FCB $FF
       FCC 'PSH'
       FDB ADDR4
       FCB $36
       FCC 'PUL'
       FDB ADDR4
       FCB $32
       FCC 'RMB'
       FDB PORMB
       FCB $FF
       FCC 'ROL'
       FDB ADDR3
       FCB $09
       FCC 'ROR'
       FDB ADDR3
       FCB $06
       FCC 'RTI'
       FDB ADDR9
       FCB $3B
       FCC 'RTS'
       FDB ADDR9
       FCB $39
       FCC 'SBA'
       FDB ADDR9
       FCB $10
       FCC 'SBC'
       FDB ADDR1
       FCB $02
       FCC 'SEC'
       FDB ADDR9
       FCB $0D
       FCC 'SEI'
       FDB ADDR9
       FCB $09
       FCC 'SEV'
       FDB ADDR9
       FCB $0B
       FCC 'STA'
       FDB ADDR2
       FCB $07
       FCC 'STS'
       FDB ADDR6
       FCB $8F
       FCC 'STX'
       FDB ADDR6
       FCB $CF
       FCC 'SUB'
       FDB ADDR1
       FCB $00
       FCC 'SWI'
       FDB ADDR9
       FCB $3F
       FCC 'TAB'
       FDB ADDR9
       FCB $16
       FCC 'TAP'
       FDB ADDR9
       FCB $06
       FCC 'TBA'
       FDB ADDR9
       FCB $17
       FCC 'TPA'
       FDB ADDR9
       FCB $07
       FCC 'TST'
       FDB ADDR3
       FCB $0D
       FCC 'TSX'
       FDB ADDR9
       FCB $30
       FCC 'TXS'
       FDB ADDR9
       FCB $35
       FCC 'WAI'
       FDB ADDR9
       FCB $3E
* CHARACTER TABLE
*
CHRTAB FCB $00 SPACE  
       FCB $00 !
       FCB $00 "
       FCB $04 #
       FCB $04 $
       FCB $04 %
       FCB $00 &
       FCB $04 '
       FCB $00 (
       FCB $00 )
       FCB $24 *
       FCB $24 +
       FCB $04 ,
       FCB $24 -
       FCB $80 .
       FCB $24 /
       FCB $42 0
       FCB $42 1
       FCB $42 2
       FCB $42 3
       FCB $42 4
       FCB $42 5
       FCB $42 6
       FCB $42 7
       FCB $42 8
       FCB $42 9
       FCB $00 :
       FCB $00 ;
       FCB $00 <
       FCB $00 =
       FCB $00 >
       FCB $00 ?
       FCB $80 @
       FCB $83 A
       FCB $83 B
       FCB $82 C
       FCB $82 D
       FCB $82 E
       FCB $82 F
       FCB $80 G
       FCB $80 H
       FCB $80 I
       FCB $80 J
       FCB $80 K
       FCB $80 L
       FCB $80 M
       FCB $80 N
       FCB $80 O
       FCB $80 P
       FCB $80 Q
       FCB $80 R
       FCB $80 S
       FCB $80 T
       FCB $80 U
       FCB $80 V
       FCB $80 W
       FCB $81 X
       FCB $80 Y
       FCB $80 Z
       FCB $00 [
       FCB $00 BACK SLASH
       FCB $00 ]
       FCB $00 CAROT
       FCB $00 UNDERLINE
*
* MAIN PROGRAM LOOP
*
*
TABLES JMP ETABL    ENT TABLES  
UPDATE JMP EUPDT    ENT UPDATE
MONTOR JMP EMON     ENT MONTOR
GETB   JMP EGETB    ENT GETB
OUTB   JMP EOUTB    ENT OUTB
WREOF  JMP EWREOF   ENT WREOF
INITIO JMP EINIIO   ENT INITIO
RESTR  JMP EREST    ENT RESTR
*
       ENT PDATA1
       ENT INEEE
       ENT CRLF
*             
MACTBL RMB 2 MACRO TABLE
MACEND RMB 2 MACRO TABLE END
MACSTK RMB 2 MACRO STACK
SYMTAB RMB 2 SYMBOL TABLE
NSYM   RMB 2 NUMBER OF SYMBOLS
SYMEND RMB 2 SYMTAB END
*
*
OPTNS  RMB 1  OPTIONS
LNUM   RMB 2  LINE NUMBER
TSTPH  RMB 2  PHASING ERR CHECK LOC
LC     RMB 2  LOCATION COUNTER
PASS   RMB 1
LBFLG  RMB 1  LABEL FLAG
RELFLG RMB 1  RELOCATION FLAG
CMNFLG RMB 1  COMMON FLAG
EXTFLG RMB 1  EXTERNAL FLAG
ENTFLG RMB 1  ENTRY FLAG
DESCRA RMB 2  DESCRIPTOR ADDRESS
DESCRC RMB 1  DESCRIPTOR COUNT
CUCHAR RMB 2  CURRENT CHARACTER ADDRESS
CULINE RMB 2  CURRENT LINE ADDRESS
SYMPTR RMB 2  SYMBOL TABLE POINTER
LCN    RMB 1  # OF IN INSTRUCTION
LSAVE  RMB 2  LC SAVE ADDRESS
LCOUNT RMB 1  # OF LINE ON A PAGE
ECOUNT RMB 2  ERROR COUNT
MSTKPT RMB 2  MACRO STACK POINTER
STKSAV RMB 2  MACSTK POINTER SAVE
MXSAV1 RMB 2  MACRO TEMP SAVE
MXSAV2 RMB 2  MACRO TEMP SAVE
MACLIN RMB 72 MACRO EXPANSION LINE AREA
MACPAR RMB 50 MACRO PARAMETER AREA
MACFLG RMB 1  MACRO FLAG
MACPTR RMB 2  POINTER TO MACRO TABLE
MACSAV RMB 2  MACRO X REG SAVE AREA
MCLPTR RMB 2  MACLIN POINTER
CMNLC  RMB 2  COMMON BLOCK LC
INLINE RMB 80 INPUT LINE
       RMB 16 
IFSTK  EQU *  IF STACK
IFSTKP RMB 2  IF STACK POINTER
IFFLG  RMB 1  IF FLAG
*
OPTMSG FCC 'ENTER OPTIONS: '      
       FCB 4
*
*
INEEE  JMP EINCH   INPUT A CHAR FROM TTY
OUTEEE JMP EOUTCH  OUTPUT A CHAR TO TTY
*
*
*
*
* PASS 1 IS ENTRY POINT TO ASSEMBLER
*
*
PASS1  CLR PASS         PASS := 1
       JSR INITIO       INIT I/O
*       
OPIN   LDX #OPTMSG      
       JSR PDATA1       
*                      
       CLR OPTNS        
       COM OPTNS        NL,NO,NM,NS
*                      
OPIN1  JSR INEEE        GET OPTION
       CMPA #$0D       CR ?
       BEQ OPIN3        YES
*                      
       CMPA #'L'        LIST?
       BNE *+6          NO
*                      
       LDAA #$70       YES 
       BRA OPIN2        
*                      
       CMPA #'O'        OBJECT?
       BNE *+6          NO
*                      
       LDAA #$B0       YES 
       BRA OPIN2        
*                      
       CMPA #'S'        SYMBOL TABLE?
       BNE *+6          NO
*                      
       LDAA #$D0       YES 
       BRA OPIN2        
*                      
       CMPA #'M'        MACRO EXPANSION LISTING?
       BNE OPIN1        NO
*                      
       LDAA #$E0       YES 
*                      
OPIN2  ANDA OPTNS      TURN OFF "NOT" BIT
       STAA OPTNS      
       BRA OPIN1        GET ANOTHER OPTION
*                      
OPIN3  JSR CRLF         
*                      
* CONFIGURE TABLES     
*                      
       LDX TABLES+1     
       LDX 0,X          GET START OF TABLES
       STX MACTBL       INIT MACTBL
*                      
       STX PSTNG1       
       LDX #$0800       
       STX PSTNG2       
       LDX #PSTNG1      
       JSR ADD16        
       LDX PSTNG1       
       STX MACEND       INIT MACEND 
*                      
       LDX #$0100       
       STX PSTNG2       
       LDX #PSTNG1      
       JSR ADD16        
       LDX PSTNG1       
       STX MACSTK       INIT MACSTK
*                      
       INX              
       STX SYMTAB       INIT SYMTAB
*                      
       STX PSTNG2       
       LDX #$7FFF       32K SYSTEM
       STX PSTNG1       
       LDX #PSTNG1      
       JSR SUB16        
       LDAA PSTNG1     
       LDAB PSTNG1+1   
       LDX #0009        
       STX PSTNG2       
       LDX #PSTNG2      
       JSR DIV16        
       STAA NSYM       
       STAB NSYM+1     INIT NSYM (NUMBER OF SYMBOLS)
*                      
       LDX #$0009       
       STX PSTNG1       
       LDX #PSTNG1      
       JSR MPY16        
       STAA PSTNG1     
       STAB PSTNG1+1   
       LDX SYMTAB       
       STX PSTNG2       
       LDX #PSTNG1      
       JSR ADD16        
       LDX PSTNG1       
       STX SYMEND       
*
       LDAA #$20       BLANKS TO SYMTAB (SYMBOL TABLE)
       LDX SYMTAB       POINT TO SYMTAB
       STAA 0,X        BLANK LOCATION
       INX              BUMP POINTER
       CPX SYMEND       ALL DONE?
       BNE *-6          NO
*                      
       LDX #$0000       
       STX TSTPH        CLEAR TSTPH
       STX ECOUNT       CLEAR ECOUNT (ERRORS COUNT)
       LDX #$0000       
       STX CMNLC        INIT COMMON LC
*                      
PASS2  JSR ADRINT       CLEAR FLAGS
       CLR LCOUNT       LCOUNT := 0
       LDX MACTBL       INIT MACPTR (MACRO TABLE POINTER)
       STX MACPTR       
       CLR MACFLG       MODE := NOT-MACRO
       LDX MACSTK       INIT MACRO STACK POINTER
       STX MSTKPT       
       LDAA #$FF       
       STAA IFFLG      INIT TO ASSEMBLE
       LDX #IFSTK       
       STX IFSTKP       INIT IFSTK POINTER
       LDX #$0000       
       STX LC           INIT LC
       STX LNUM         INIT LINE NUMBERS
*             
* MAIN IS THE DRIVER SECTION OE TOP LEVEL
*      OF THE ASSEMBLER
*             
MAIN1  JSR RDLINE       GET A LINE OF SOURCE
       CLR LBFLG        SET FLAG TO NO LABEL
       LDX LNUM    
       INX         
       STX LNUM         BUMP LNUM
       LDX CULINE       POINT TO LINE
       LDAA 0,X        GET COL 1
       CMPA #$2A       COMMENT? (* CHAR)
       BNE MAIN3        NO
*
* HANDLE COMMENT LINE
*
MAIN1A JSR ADRINT       CLEAR PRINT FLAGS
       JSR PRINTL       PRINT THE LINE
       BRA MAIN1        GET ANOTHER LINE
*                  
MAIN3  TST IFFLG        ASSEMBLING?
       BNE MAIN3C       YES
*                  
       CMPA #$20       COL 1 BLANK?
       BEQ MAIN3A       YES
*                  
       JSR NXTOK        SCAN OVER LABEL
MAIN3A JSR NXTOK        GET MNEMONIC
       LDAA DESCRC     GET COUNT
       CMPA #3         IS <=3?
       BHI MAIN1A       NO PRINT THE LINE
*                  
       JSR MNLKP        SEARCH mbtab (MNEMONICs TABLE)
       CPX #POIF        IF ?
       BEQ MAIN3B       YES
*                  
       CPX #PONIF       NIF?
       BEQ MAIN3B       YES
*                  
       BRA MAIN1A       NEITHER
*                  
MAIN3B JMP 0,X          GOTO IF OR NIF PROCESSING ROUTINE
*                  
MAIN3C CMPA #$20        COL 1 BLANK?
       BEQ MAIN5        YES (THERE IS NO LABEL)
*                  
       JSR NXTOK        GET LABEL
       CMPB #$01        OK?
       BEQ MAIN4        YES
*                  
       LDX #$0205       ERROR
       JSR PRINTE   
       JSR PRINTL       PRINT LINE
       BRA MAIN1    
*                  
MAIN4  INC LBFLG        SET LABEL FLAG
       TST PASS         PASS?
       BNE MAIN5        PASS2
*                  
       JSR STOSYM       STORE LABEL IN SYMTAB
*                  
MAIN5  JSR NXTOK        GET MNEMONIC
       CMPB #$01        OK?
       BEQ MAIN7        YES
*                  
MAIN6  LDX #$0202       ERROR
       JSR PRINTE       
       JSR PRINTL       PRINT LINE
       BRA MAIN1    
*                  
MAIN7  JSR MNLKP        SEARCH MBTAB (MNEMONICS TABLE)
       CMPA #$00       IN THE MNTAB?
       BEQ MAIN9        YES
*                  
       JSR LKPSYM       MACRO NAME?
       CMPB #$FF       IN SYMTAB?
       BEQ MAIN8        NO, ERROR
*                  
       BITB #$20       MACRO NAME?
       BEQ MAIN8        NO, ERROR
*                  
*                  
       TST MACFLG       MACRO MODE?
       BEQ MAIN7A       NO
*
* PUSH PRESENT MACRO ONTO MACSTACK
*
       JSR MACPSH
       TST MACFLG       ERRORS?
       BEQ MAIN13       YES
*                  
MAIN7A STX MACPTR       SAVE MACRO LOC IN MACTBL
       JSR PRINTL   
       INC MACFLG       MODE := MACRO
*                  
       JSR NXTOK        PARAM?
       CMPB #$0D    
       BNE MAIN12       YES, SAVE THEM
*                  
       STAB MACPAR     NO, CR TO MACPAR
       BRA MAIN13   
*                  
MAIN9  JMP 0,X          GO TO ROUTINE
*                  
MAIN8  LDX #$0207       ERROR
       JSR PRINTE       PRINT it
MAIN10 JSR PRINTL       PRINT LINE NUMBER
MAIN13 JMP MAIN1        PROCESS NEXT LINE
*
* MOVE PARAMS ON MACRO CALL TO MACPAR
*
MAIN12 LDX #MACPAR      
       STX MACSAV       INIT POINTER
*                    
MAIN11 LDX DESCRA       POINT TO PARAMS
       LDAA 0,X        GET A CHAR
       INX            
       STX DESCRA       SAVE POINTER
*                    
       LDX MACSAV       GET POINTER
       STAA 0,X        MOVE CHAR
       INX            
       STX MACSAV       SAVE POINTER
*                    
       CMPA #$0D        EOL?
       BNE MAIN11       NO
*                    
       BRA MAIN13       YES
*                    
* GET A LINE OF SOURCE FROM INBUF *
* RETURNS ADDRESS OF LINE IN CULINE *
* CUCHAR:=ADDRESS OF FIRST CHARACTER*
*                    
RDLINE TST MACFLG       MACRO MODE?
       BEQ RDLINA       NO
*                    
       JSR RDMAC        EXPAND MACRO
*                    
       TST MACFLG       MACRO FULLY EXPANDED?
       BEQ RDLINA       YES
*                    
       RTS            
*                    
RDLINA LDX #INLINE    
       STX CUCHAR     
       STX CULINE     
*                    
RDL1   JSR GETB       
       BCC RDL1A      
*                    
       LDS #$A042       FLUSH STACK, EOF
       JMP POEND0     
*                    
RDL1A  CMPA #$0A       LF?
       BEQ RDL1         YES
*                    
       CMPA #$00       NULLS?
       BEQ RDL1         YES
*                    
       CPX #INLINE+79   LINE TO LONG?
       BEQ RDL2         YES
*                    
       STAA 0,X        STORE CHARACTER
       INX            
       BRA RDL3       
*                    
RDL2   LDAB #$0D       TRUNCATE LINE
       STAB 0,X       
*                    
RDL3   CMPA #$0D       CR?
       BNE RDL1         NO
*                    
       RTS            
*                    
* RDMAC: EXPAND MACRO CALLS
*                    
RDMAC  LDX MACPTR     
       LDAA 0,X        GET CHAR
       CMPA #$17       ETB?
       BNE RDMAC1       NO
*                    
       DEC MACFLG       DEC MODE COUNT
       BEQ RDMAC0       NO MORE MACROS
*                    
* PULL UP LAST MACRO STACKED
*                    
       JSR MACPUL     
       BRA RDMAC      
*                    
RDMAC0 RTS            
*                    
RDMAC1 LDX #MACLIN      POINT TO MACRO EXPANDAREA
       STX CULINE       INIT
       STX CUCHAR       INIT
       STX MCLPTR       INIT
*                    
RDMAC2 LDX MACPTR       POINT TO MACRO DEFINITION
       LDAA 0,X       
       INX            
*                    
       STX MACPTR     
       CMPA #'&'       MACRO PARM?
       BEQ RDMAC3      YES
*                    
       LDX MCLPTR       POINT TO MACLIN
       STAA 0,X        MOVE CHAR TO MACLIN
       INX            
       STX MCLPTR       SAVE POINTER
       CPX #MACLIN+71   OVERFLOW?
       BEQ RDMERR       YES
*                    
       CMPA #$0D       EOL?
       BNE RDMAC2       NO
       RTS              ALL DONE
*                    
* SUBSTITUTE POSITIONAL PARAMS
*                    
RDMAC3 LDAB 0,X        GET POSITIONAL # OF PARAM
       SUBA #$2F       CONVERT TO BINARY
       INX              SKIP OVER POS#
       STX MACPTR     
       LDX #MACPAR      POINT TO PARMS FROM CALL
*                    
* SCAN OVER PARAMS   
*                    
RDMAC6 STX MACSAV       SAVE
*                    
RDMAC4 LDAA 0,X       GET A CHARACTER
       INX            
       CMPA #','        END OF PARAM?
       BEQ RDMAC7       YES
*                    
       CMPA #$0D       EOL?
       BNE RDMAC4       NO
*                    
RDMAC7 DEC B            FOUND PARAM?
       BNE RDMAC6       NO
*                    
       LDX MACSAV       POINT TO PARAMETER
       LDAA 0,X        FOUND PARAM, GET CHAR
       INX            
       STX MACSAV       SAVE POINTER
*                    
RDMAC5 LDX MCLPTR       POINT TO MACLIN
       STAA 0,X        MOVE CHAR
       INX            
       STX MCLPTR       SAVE POINTER
       CPX #MACLIN+71   OVERFLOW?
       BEQ RDMERR       YES
*                    
*                    
       LDX MACSAV       POINT TO MACPAR
       LDAA 0,X        GET NEXT CHAR
       INX            
       STX MACSAV       SAVE 
       CMPA #','        END OF PARAM?
       BEQ RDMAC2       YES
*                    
       CMPA #$0D       EOL?
       BNE RDMAC5       NO
*                    
       BRA RDMAC2     
*                    
RDMERR LDAA #$0D       END LINE
       STAA 0,X       
       LDX #$0203       ERROR MESSAGE
       JSR PRINTE     
       BRA RDMAC2     
*                    
* PUSH A MACRO ONTO THE MACRO STACK
*                    
MACPSH STX MXSAV1       SAVE X-REG
       STS STKSAV       SAVE STACK POINTER
       LDS MSTKPT       LOAD MACRO STACK POINTER
*                    
* PUSH MCLPTR,MACSAV,MACPTR ONTO STACK
*                    
       LDX #MCLPTR+1  
       LDAB #6        
*                    
MPSH1  LDAA 0,X         GET A BYTE
       DEX            
       STX MXSAV2       SAVE POINTER
       TSX              X:=STKPTR+1
       DEX            
       CPX MACEND       END OF STACK?
       BEQ MPSH5        YES, ERROR
*                    
       LDX MXSAV2       RESTORE POINTER
       PSH A            PUSH A BYTE ONTO STACK
       DEC B            ALL DONE?
       BNE MPSH1        NO
*                    
* PUSH MACPAR IN REVERSE ORDER
*                    
       LDX #MACPAR    
*                    
MPSH2  LDAA 0,X         FIND EOL
       CMPA #$0D       EOL?
       BEQ MPSH3        YES
*                    
       INX            
       BRA MPSH2      
*                    
MPSH3  LDAA 0,X        GET A BYTE
       STX MXSAV2       SAVE POINTER
       TSX              X:=STKPTR+1
       DEX            
       CPX MACEND       END OF STACK?
       BEQ MPSH5        YES, ERROR
*                    
       LDX MXSAV2       RESTORE POINTER
       PSH A            PUT ON STACK
       DEX              POINT TO NEXT CHAR
       CPX #MACPAR-1    ALL DONE?
       BNE MPSH3        NO
*                    
       STS MSTKPT       SAVE STACK POINTER
       LDS STKSAV       RESTORE STACK
       LDX MXSAV1       RESTORE X-REGISTER
       RTS            
*                    
* MACRO NESTING OVERFLOW ERROR
*                    
MPSH5  LDS MACSTK       FLUSH STACK
       STS MSTKPT     
       LDS STKSAV     
       LDX #$0251       ERROR NUMBER
       JSR PRINTE     
       LDX MXSAV1       RESTORE X-REG
       CLR MACFLG       GET OUT OF MACRO MODE
       RTS            
*                    
* PULL A MACRO FROM STACK
*                    
MACPUL STX MXSAV1       SAVE X-REG
       STS STKSAV       SAVE STACK POINTER
       LDS MSTKPT       LOAD MACRO STACK POINTER
*                    
* PULL MACPAR OFF OF THW FROM STACK
*                    
       LDX #MACPAR    
*                    
MPUL1  PULA             PULL A CHAR
       STAA 0,X        SAVE IN MACPAR
       INX            
       CMPA #$0D       EOL?
       BNE MPUL1        NO
*                    
* PULL MACPTR,MACSAV,MCLPTR
*                    
       LDX #MACPTR    
       LDAB #6        
*                    
MPUL2  PUL A            PULL A CHARACTER
       STAA 0,X        SAVE
       INX            
       DEC B          
       BNE MPUL2        NOT DONE
*                    
       STS MSTKPT       SAVE MACRO STACK POINTER
       LDS STKSAV       RESTORE STACK POINTER
       LDX MXSAV1       RESTORE X-REG
       RTS            
*                    
* COMPARE TWO STRINGS *
*                    
* ON ENTRY (X) = A PRAM LIST OF 5 BYTES *
*    ADDR(STRING1)   
*    ADDR(STRING2)   
*    COUNT OF # BYTES TO BE COMPARED
*                    
* ON RETURN IF CC Z BIT IS SET THERE IS A MATCH
*    EXAMPLE :       
*          LDX #STRING1
*          JSR COMPAR
*          BEQ ------   MATCH
*                    
*    STRING1 RMB 2   
*    STRING2 RMB 2   
*    COUNT   RMB 1   
*                    
COMPAR PSH A          
       PSH B          
       LDAB 4,X        GET COUNT
       STX XSAV         SAVE PARAM POINTER
CMP1   LDX XSAV         GET PARAM POINTER
       LDX 0,X          GET ADDR(STRING1)
       LDAA 0,X        GET CHARACTER
       LDX XSAV         GET PTR
       INC 1,X          PTR SET TO NEXT 
       BNE CMP2         CHAR IN
       INC 0,X          STRING1
CMP2   LDX XSAV         GET PARAM POINTER
       LDX 2,X          GET ADDR(STRING2)
       CMPA 0,X        COMPARE 
       BNE CDONE        NOT EQUAL
       LDX XSAV         GET PARAM POINTER
       INC 3,X          PTR SET TO NEXT
       BNE CMP3         CHAR IN
       INC 2,X          STRING2
CMP3   DEC B            DECREMENT COUNTER
       BNE CMP1         TRY AGAIN
CDONE  PUL B            DONE
       PUL A          
       RTS            
*                    
XSAV   RMB 2            PARAM PTR SAVE AREA
*                    
* NEXT TOKEN ROUTINE 
* SCANS A LINE OF SOURCE CODE AND RETURNS
* THE NEXT TOKEN, CLASS &RC IN REGS A,B
* THE ADDRESS OF THE TOKEN IS RETURNED IN
* DESCRA AND THE NUM OF BYTES IN THE TOKEN IS
* THE RC AND CLASS ARE:
*                    
*   TYPE    RC(B)    CLASS(A)
*                    
* NAME       01        02        SUBSTRINGS
* HEX        03        02
* DECIMAL    09        02
*                    
* #          23        04        DELIMITERS
* ,          2C        04
* '          27        04
*                    
* *          2A        24        ARITHMETIC
* /          2F        24
* +          2B        24
* -          2D        24
*                    
* A          41        01        A,B,X REGS
* B          42        01
* X          58        01
*                    
* CR         0D        0D        EOL
*                    
* ERROR      00        00        ERRORS
*                    
*                    
NXTOK  CLR DESCRC     
       INC DESCRC       DESCRC := 1
NXT0   LDX CUCHAR       POINT TO CURRENT CHAR
       STX DESCRA       INIT DESCRA
       LDAA 0,X        GET CHAR
       INX            
       STX CUCHAR       POINT TO NEXT CHAR
       CMPA #$20       LESS THAN 20 HEX? (SPACE)
       BEQ NXT0         BLANK,SKIP OVER
       BHI NXT1         IS > 20
*                    
       CMPA #$0D       CR?
       BNE NXTER        NO,UNRECOGNIZABLE CHAR
       TAB              YES, SET RC
       RTS            
*                    
NXT1   CMPA #$5F       IS >5F?
       BLS NXT3         NO
       BRA NXTER        YES,UNRECOGNIZABLE CHAR
*                    
NXT3   JSR GCHRTB       GET BYTE FROM CHARTAB
       BITA #$01       A,B,X REG?
       BEQ NXT4         NO
*                    
       LDX CUCHAR       YES,CHECK NEXT CHAR
       LDAB 0,X       
       CMPB #$20       BLANK?
       BEQ NXT3A        YES 
       CMPB #$0D       EOL?
       BNE NXT4A        NO GOTO NSCAN
*                    
NXT3A  LDX DESCRA       GET RC
       LDAB 0,X       
       RTS            
*                    
NXT4   BITA #$80       NAME?
       BEQ NXT5         NO
NXT4A  JSR NSCAN        YES SCAN NAME STRING
       RTS            
*                    
NXT5   BITA #$40       DECIMAL?
       BEQ NXT6         NO
       JSR DSCAN        YES,SCAN DECIMAL STRING
       RTS            
*                    
NXT6   BITA #$20       ARITHMETIC?
       BNE NXT3A        YES GET RC AND RETURN
*                    
       BITA #$04       DELIMITERS?
       BEQ NXTER        NO,UNRCOGNIZABLE CHAR
       LDX DESCRA       GET CHAR
       LDAB 0,X       
       CMPB #$24       IS ? (HEX)
       BNE NXT3A        NO,GET RC AND RTN
*                    
       JSR HSCAN        SCAN HEX STRING
       RTS            
*                    
NXTER  CLR A            TROUBLE,SET RC,CLASS=00
       CLR B          
       RTS            
*                    
* DSCAN - SCAN DECIMAL STRING AND STOP AT
* FIRST NON-DECMIAL CHARACTER.
*                    
DSCAN  LDX CUCHAR       POINT TO NEXT CHAR
       LDAA 0,X        GET CHAR
       INC DESCRC       INCR DESCRC COUNT
       INX            
       STX CUCHAR       POINT TO NEXT CHAR
       JSR GCHRTB       GET BYTE IN CHARTAB
       BITA #$40       DECIMAL?
       BNE DSCAN        YES CONTINUE SCAN
       LDAB #$09      
       BRA ENDSCN       RETURN
*                    
* NSCAN SCAN NAME STRING AND STOP AT 
* FIRST NON-ALPHANUMERIC CHAR
*                    
NSCAN  LDX CUCHAR       POINT TO NEXT CHAR
       LDAA 0,X        GET CHAR
       INC DESCRC       INCR DESCRC COUNT
       INX            
       STX CUCHAR       POINT TO NEXT CHAR
       JSR GCHRTB       GET BYTE FROM CHARTAB
       BITA #$80       ALPHA?
       BNE NSCAN        YES CONTINUE SCAN
       BITA #$40       NUMERIC?
       BNE NSCAN        YES CONTINUE SCAN
       LDAB #$07       NAME TO LONG?
       CMPB DESCRC    
       BCC NSCANA       NO
       STAB DESCRC     YES,TRUNCATE
NSCANA LDAB #$01       LOAD RC
       BRA ENDSCN       RETURN
*                    
* HSCAN - SCAN A STRING OF HEX DATA AND STOP
* AT FIRST NON-HEXADECIMAL CHARACTER.
*                    
HSCAN  CLR DESCRC       DESCRC:=0
       LDX CUCHAR       POINT TO NEXT CHAR
       STX DESCRA       INIT DESCRA
HSCAN1 LDX CUCHAR       POINT TO NEXT CHAR
       LDAA 0,X        GET CHAR
       INC DESCRC       INCR DESCRC COUNT
       INX            
       STX CUCHAR       POINT TO NEXT CHAR
       JSR GCHRTB       GET BYTE FROM CHRTAB
       BITA #$02       HEX?
       BNE HSCAN1       YES CONTINUE SCAN
       LDAB #$03      
*                    
ENDSCN DEC DESCRC       DESCRC := CORRECT COUNT
       LDX CUCHAR     
       DEX            
       STX CUCHAR       CUCHAR:= CORRECT VALUE 
       LDAA #$02       LOAD CLASS RC
       RTS              ALL DONE
*                    
* GET A BYTE FROM CHARACTER TABLE INDEXED BY 
* VALUE IN A REGISTER.
*                    
GCHRTB CMPA #$20       VALID CHAR?
       BCS GCHRTR       NO <20
       CMPA #$5F       VALID CHAR?
       BHI GCHRTR       NO >5F
*                    
       CLR CHPTR        INIT PARAM
       STAA CHPTR+1    SAVE CHAR
       LDX #CHPTR       POINT TO PARAM
       JSR ADD16        ADD IN BASE OF CHARTAB
       LDX CHPTR        GET BYTE IN CHARTAB
       LDAA 0,X       
       RTS            
*                    
GCHRTR CLR A          
       RTS            
*                               
CHPTR  RMB 2            PARAM LIST
       FDB CHRTAB-$20 
*                    
* TABLE MANIPULATION ROUTINES FOR TABLES
* SYMTAB AND MNTAB   
*                    
* STORAGE LOCATIONS USED BY ROUTINES:
*                    
PSTNG1 RMB 2            ADDRESS OF MENMONIC
PSTNG2 RMB 2            ADDRESS IN THE TABLE
PCOUNT RMB 1            LENGTH OF MNEMONIC
TBADD  RMB 2            TABLE POINTER
HSMBL  RMB 6            SYMBOL TEMP LOC
HKEYA  RMB 2            HASHED CODE
HKEYB  RMB 2            TEMP LOC FOR HASHED CODE
HSAV1  RMB 2            TEMP LOC FOR PTR
HSAV2  RMB 2            TEMP LOC FOR PTR
*                    
*                    
* STORE A SYMBOL IN SYMTAB (SYMBOL TABLE)
* ON ENTRY DESCRA CONTAINS THE ADDRESS OF
* THE SYMBOL, AND DESCRC CONTAINS THE LENGTH
* A STANDARD HASH CODE ALGORITHM IS USED
*                    
STOSYM JSR HASH         GET HASHED KEY
       STX SYMPTR       SAVE 
*                    
* SEE IF LOC(HASH KEY) IS EMPTY
*                    
SYMA   LDAA 0,X         GET FIRST CHARACTER
       CMPA #$20       BLANK?
       BNE SYMB         NO
*                    
* STORE SYMBOL IN SYMTAB
*                    
       STX HSAV2        SAVE TABLE POINTER
       LDX #HSMBL       POINT TO HSMBL
       STX HSAV1        SAVE 
       LDAB #6         LOAD SYMBOL LENGTH
*                    
* DO TRANSFER        
*                    
SYM1   LDX HSAV1        POINT TO HSYMBL
       LDAA 0,X        GET CHAR
       INX            
       STX HSAV1        POINT TO NEXT CHAR
       LDX HSAV2        POINT TO TABLE SPACE
       STAA 0,X        STORE CHAR IN SYMTAB
       INX            
       STX HSAV2        POINT TO NEXT POS
       DEC B            ALL DONE?
       BNE SYM1         NO
*                    
* STORE STORE LC, AND SET INFO BYTE
*                    
       LDAA LC         GET LC
       STAA 0,X        STORE
       LDAA LC+1       GET LS BYTE OF LC
       STAA 1,X        STORE 
       LDAA #$40       INFO BYTE:=RELOC,DEFINED
       STAA 2,X       
       RTS              RETURN
*                    
* COMPARE HSMBL WITH ENTRY IN SYMTAB
*                    
SYMB   JSR SYMCMP       COMPARE
       BNE SYMC         NO MATCH
*                    
* ERROR, SYMBOL ALREADY IN TABLE
*                    
       LDX SYMPTR       GET ADDRESS OF ENTRY
       LDAA #$80      
       ORAA 8,X        SET REDEFINED BIT
       STAA 8,X       
       LDX #$0206       LOAD ERROR NUMBER
SYMB1  JSR PRINTE       PRINT IT
       RTS              RETURN
*                    
* FIND ANOTHER SLOT IN SYMTAB FOR SYMBOL 
*                    
SYMC   JSR SYMMOD       GET ADDR(NEXT SLOT)
       CPX HKEYA        CHECKED ALREADY?
       BEQ *+4          YES,TABLE IS FULL
*                    
       BRA SYMA         TRY AGAIN
*                    
       LDX #$0221       LOAD ERROR NUMBER
       BRA SYMB1        PRINT IT & RETURN
*                    
* LOOK UP SYMBOL IN SYMTAB
* ON ENTRY DESCRA = ADDRESS OF SYMBOL
*          DESCRC = SYMBOL LENGTH
* ON RETURN B = VALUE OF INFO BYTE
*             = FF SYMBOL NOT FOUND
*           X = VALUE OF SYMBOL
*                    
*                    
LKPSYM JSR HASH         GET KEY
       STX SYMPTR       SAVE 
*                    
LKPSM1 LDAA 0,X        GET CHARACTER
       CMPA #$20       BLANK?
       BNE LKPSM3       NO
*                    
* ENTRY NOT IN SYMTAB
*                    
LKPSM2 LDAB #$FF       LOAD RC
       RTS              RETURN
*                    
* COMPARE SYMBOL WITH ENTRY IN SYMTAB
*                    
LKPSM3 JSR SYMCMP       COMPARE 
       BNE LKPSM4       NO MATCH 
*                    
* FOUND, EXTRACT INFO, AND VALUE
*                    
       LDX SYMPTR       POINT TO ENTRY
       LDAB 8,X        GET INFO BYTE
       LDX 6,X          GET VALUE
       RTS            
*                    
* PROBE AGAIN FOR SYMBOL IN SYMTAB
*                    
LKPSM4 JSR SYMMOD       GET NEXT KEY
       CPX HKEYA        ALREADY CHECKED?
       BNE LKPSM1       NO,TRY AGAIN
       LDAB #$FF       SET RC
       RTS            
*                    
* ROUTINE TO COMPARE SYMBOL WITH ENTRY
*                    
SYMCMP STX PSTNG1       SAVE PTR TO ENTRY
       LDAA #6        
       STAA PCOUNT     PCOUNT := LENGTH(SYMBOL)
       LDX #HSMBL     
       STX PSTNG2       POINT TO HSMBL
       LDX #PSTNG1      POINT TO PARAMS
       JSR COMPAR       COMPARE
       RTS            
*                    
* FIND NEXT SLOT IN SYMTAB 
* SYMPTR:=SYMPTR+9 (MODULO NSYM)
*                    
SYMMOD LDX SYMPTR       GET ADDR(CURRENT SLOT)
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
*                    
* BEYOND SYMTAB?     
*                    
       CPX SYMEND     
       BNE *+5          NO
       LDX SYMTAB       POINT TO FIRST ENTRY
       STX SYMPTR       SAVE PTR TO ENTRY
       RTS            
*                    
* DELETE LAST SYMBOL ENTERED
*                    
DELSYM LDX SYMPTR     
       LDAA #$20       LOAD BLANK
       LDAB #9         LOAD ENTRY LENGTH
*                    
DEL1   STAA 0,X        BLANK BYTE
       INX              POINT TO NEXT BYTE
       DEC B            ALL DONE ?
       BNE DEL1         NO
*                    
       RTS              YES, RETURN
*                    
* HASH SYMBOL TO PRODUCE A KEY
*                    
HASH   LDX #$2020       BLANK HSMBL
       STX HSMBL      
       STX HSMBL+2    
       STX HSMBL+4    
*                    
* MOVE SYMBOL TO HSMBL
*                    
       LDX #HSMBL       POINT TO HSMBL
       STX HSAV2        SAVE 
       LDX DESCRA       POINT TO SYMBOL
       STX HSAV1        SAVE 
       LDAB DESCRC     GET LENGTH(SYMBOL)
*                    
HASH1  LDX HSAV1        POINT TO SYMBOL
       LDAA 0,X        GET CHAR
       INX            
       STX HSAV1        POINT TO NEXT CHAR
       LDX HSAV2        POINT TO HSYMBL
       STAA 0,X        STORE CHAR
       INX            
       STX HSAV2        POINT TO NEXT CHAR
       DEC B            ALL DONE
       BNE HASH1        NO
*                    
* FOLD HSMBL TO CREATE A KEYA
*                    
       LDX HSMBL        HKEYA:=HSMBL(2)
       STX HKEYA      
       LDX HSMBL+2    
       STX HKEYB      
       LDX #HKEYA     
       JSR ADD16              +HSMBL+2(2)
       LDX HSMBL+4    
       STX HKEYB      
       LDX #HKEYA     
       JSR ADD16              +HSMBL+4(2)
*                    
* HKEYA:=REMAINDER OF HKEYA/NSYM       
*                    
       LDAA HKEYA      LOAD VALUES     
       LDAB HKEYA+1   
       LDX NSYM       
       STX HKEYB      
       LDX #HKEYB       POINT TO NSYM
       JSR DIV16      
       STX HKEYA        SAVE REMAINDER
*                    
* HKEYA:=HKEYA*9     
*                    
       CLR A          
       LDAB #9        
       LDX #HKEYA     
       JSR MPY16      
       STAA HKEYA     
       STAB HKEYA+1   
*                    
* ADD IN BASE OF SYMTAB
*                    
       LDX SYMTAB     
       STX HKEYB      
       LDX #HKEYA     
       JSR ADD16      
       LDX HKEYA      
       RTS            
*                    
* LOOK UP MNEMONIC IN MNTAB
* ON ENTRY DESCRA = ADDR OF MNEMONIC
*          DESCRC = LENGTH OF MNEMONIC
* ON RETURN          
*  REG A = 00 FOUND  
*  REG A = FF NOT IN TABLE 
*  REG X = ADDR OF ROUTINE TO PROCESS 
*          THE OPCODE/PSEUDOP
*  REG B = MACHINE CODE FOR OPCODES
*        = FF FOR PSEUDOPS
*                    
* THE ALGORITHM USED IS A BINARY SEARCH
*                    
* TEMPORARY LOCATIONS:
LP     RMB 1            ONE BELOW LOWEST ENTRY
MP     RMB 1            ONE HIGHER THAN HIGHEST ENTRY
IP     RMB 1            CALCULATED PROBE VALUE
ENSIZ  FDB 6            LENGTH OF ENTRY IN MNTAB
*                    
MNLKP  LDAA DESCRC    
       STAA PCOUNT     INIT PCOUNT
       LDAA #$57      $57=CHRTAB-MNTAB/6+1 (# OF ENTRIES+1)
       STAA MP         INIT MP
       CLR A          
       STAA LP         INIT LP
*                    
MNLKPA LDAA LP        
       INC A            A:=LP+1
       CMPA MP         MP:=LP+1 ?
       BNE MNLKPB       NO
*                    
       LDAA #$FF       YES, ENTRY NOT IN TABLE
       RTS            
*                    
* IP:=(LP+MP)/2 TRUNCATED
*                    
MNLKPB LDAB LP        
       ADDB MP         B:=LP+MP 
       ROR B            B:=B/2
       STAB IP         SAVE IP
*                    
* GET 16 BITADDRESS OF ENTRY
*                    
       CLR A          
       LDX #ENSIZ       GET ENTRY LENGTH
       DEC B            B:=IP-1
       JSR MPY16        GET (IP-1)*6
       STAA PSTNG1     SAVE
       STAB PSTNG1+1  
       LDX #MNTAB     
       STX PSTNG2       PSTNG2:=BASE OF MNTAB
       LDX #PSTNG1      POINT TO PARAMS
       JSR ADD16        PSTNG1:=(IP-1)*6+MNTAB
       LDX PSTNG1     
       STX TBADD        SAVE
*                    
* COMPARE MNEMONIC WITH ENTRY IN MNTAB
*                    
       LDX DESCRA       GET MNEMONIC ADDRESS
       STX PSTNG2       INIT PARAM FOR COMPARE
       LDX #PSTNG1      POINT TO PARAMS
       JSR COMPAR       COMPARE
       BCS MNLI         ENTRY<MNEMONIC
       BNE MNMI         ENTRY>MNEMONIC
*                    
       CLR A            ENTRY FOUND
       LDX TBADD        POINT TO ENTRY
       LDAB 5,X        GET MACHINE CODE
       LDX 3,X          GET BRANCH TO ROUTINE ADDR
       RTS            
*                    
* ENTRY<MNEMONIC LP:=IP
*                    
MNLI   LDAA IP        
       STAA LP        
       BRA MNLKPA       TRY AGAIN
*                    
* ENTRY>MNEMONIC MP:=IP
*                    
MNMI   LDAA IP        
       STAA MP        
       BRA MNLKPA       TRY AGAIN
*                    
* EVALUATE NUMBERS, SYMBOLS AND EXPRESSIONS
*                    
VALUE  RMB 2          
TMPVAL RMB 2          
CLFLG  RMB 1            CLASS OF PREVIOUS TOKEN
CLASS  RMB 1            CLASS OF CURRENT TOKEN
OPERN  RMB 1            ARITHMETIC OPERATOR 
*                    
NSEVL  CLR VALUE      
       CLR VALUE+1      VALUE := 0
       CLR CLFLG        CLFLG := 0
       STAA CLASS      SAVE CLASS OF TOKEN
       CMPB #$2A       IS * ?
       BNE NSVLC1       NO
*                    
       LDX LC           YES
       STX VALUE        VALUE := LC
       LDAA #2        
       STAA CLFLG      CLFLG:=2
       COM RELFLG       RELFLG:=RELOC
*                    
NSVLA  LDX CUCHAR     
       LDAA 0,X        GET NEXT CHAR
       CMPA #$20       BLANK?
       BEQ NSVLB        YES
       CMPA #$0D       EOL?
       BEQ NSVLB        YES
       CMPA #$2C       COMMA?
       BNE NSVLC        NO
*                    
NSVLB  LDX VALUE      
       STX ADR1         ADR1,2:=VALUE
       CLR B            RC := 00
       RTS              ALL DONE
*                    
NSVLC  JSR NXTOK        GET NEXT TOKEN
       STAA CLASS      SAVE TOKEN CLASS
NSVLC1 CMPA CLFLG      CLASS = CLFLG?
       BNE NSVLF        NO
*                    
NSVLD  LDX #$0204       ERROR 
NSVLE  CLR B          
       COM B            RC := FF
       RTS              RETURN
*                    
NSVLF  CMPA #$02       IS STRING?
       BEQ NSVLH        YES
*                    
       CMPA #$24       ARITHMETIC OPERATOR?
       BEQ NSVLG        YES
       BRA NSVLD        ERROR
*                    
NSVLG  TST CLFLG        CLFLG=0?
       BEQ NSVLD        YES,ERROR
*                    
       STAB OPERN      SAVE OPERATOR
       STAA CLFLG      CLFLG:=CLASS
       JMP NSVLA        SCAN AGAIN
*                    
NSVLH  CMPB #$03        HEX STRING?
       BNE NSVLJ        NO
*                    
       LDAB DESCRC     YES
       CMPB #4         IS > 4?
       BLE NSVLH1       NO 
*                    
       LDX #$0210       YES,ERROR
       BRA NSVLE      
*                    
NSVLH1 JSR CVHB         CONVERT
       BRA NSVLM      
*                    
NSVLJ  CMPB #$09        DECIMAL?
       BNE NSVLK        NO
*                    
       LDAB DESCRC    
       CMPB #5         LENGTH > 5?
       BLE NSVLJ1       NO 
*                    
       LDX #$0210       YES,ERROR 
       BRA NSVLE      
*                    
NSVLJ1 JSR CVDB         CONVERT
       BRA NSVLM      
*                    
NSVLK  CMPB #$01       SYMBOL?
       BEQ NSVLL        YES
*                    
       JMP NSVLD        NO,ERROR
*                    
NSVLL  JSR LKPSYM       LOOKUP SYMBOL 
       BITB #$80       REDEFINED?
       BNE NSVLLA       YES
*                    
       BITB #$40       RELOC?
       BEQ *+7          NO
*                    
       COM RELFLG       YES RELFLG:=RELOC
       BRA NSVLM      
*                    
       BITB #$10       COMMON?
       BEQ *+5          NO
*                    
       COM CMNFLG       YES
       BRA NSVLM      
*                    
NSVLLA LDX #$0211       NO,ERROR 
       JMP NSVLE      
*                    
NSVLM  STX TMPVAL       SAVE,CONVERTED VALUE
       TST CLFLG        CLFLG=0?
       BNE NSVLP        NO
*                    
       LDX TMPVAL       YES
       STX VALUE        VALUE:=TMPVAL
*                    
NSVLN  LDAA CLASS     
       STAA CLFLG      CLFLG:=CLASS
       JMP NSVLA        SCAN AGAIN
*                    
NSVLP  LDAA OPERN      GET LAST OPERATOR
       CMPA #$2B       IS + ?
       BNE NSVLP1       NO
*                    
       LDX #VALUE     
       JSR ADD16        VALUE:=VALUE+TMPVAL
       BRA NSVLN      
*                    
NSVLP1 CMPA #$2D       IS - ?
       BNE NSVLP2       NO
*                    
       LDX #VALUE       YES
       JSR SUB16        VALUE:=VALUE-TMPVAL
       BRA NSVLN      
*                    
NSVLP2 CMPA #$2A       IS * ?
       BNE NSVLP3       NO
*                    
       LDAA VALUE     
       LDAB VALUE+1   
       LDX #TMPVAL    
       JSR MPY16        VALUE:=VALUE*TMPVAL 
       STAA VALUE     
       STAB VALUE+1   
       JMP NSVLN       
*                    
NSVLP3 CMPA #$2F       IS / ?
       BEQ NSVLP4       YES
*                    
       JMP NSVLD        NO, ERROR
*                    
NSVLP4 LDAA VALUE     
       LDAB VALUE+1   
       LDX #TMPVAL    
       JSR DIV16        VALUE:=VALUE/TMPVAL
       STAA VALUE     
       STAB VALUE+1   
       JMP NSVLN      
*                    
* CVHB CONVERT HEX TO BINARY
*                    
* ON ENTRY DESCRA = ADDRESS OF STRING
*          DESCRC = # OF BYTES IN STRING
* ON RETURN [X]=VALUE
*                    
HVAL   RMB 2            TEMP STORAGE
*                    
CVHB   LDX DESCRA       GET ADDRESS OF STRING
       CLR HVAL       
       CLR HVAL+1     
       LDAB DESCRC     GET COUNT
       DEX              DECR PTR TO STRING
CVHB1  INX              POINT TO RIGHT MOST
       DEC B            BYTE OF THE 
       BNE CVHB1        STRING
*                    
       LDAB DESCRC     GET COUNT 
       JSR CVHBS        CONVERT
       STAA HVAL+1     SAVE
       DEC B            DECR COUNT
       BEQ CVHBD        (1 HEX DIGIT) 
       DEX              POINT TO NEXT LEFT BYTE
       JSR CVHBS        CONVERT
       ASL A            SHIFT TO LEFT NIBBLE
       ASL A          
       ASL A          
       ASL A          
       ORAA HVAL+1     CONVERT TO BYTE
       STAA HVAL+1     SAVE
       DEC B            DECREMENT COUNT
       BEQ CVHBD        (2 HEX DIGITS)
       DEX              POINT TO NEXT LEFT BYTE
       JSR CVHBS        CONVERT
       STAA HVAL       SAVE
       DEC B            DECREMENT COUNT
       BEQ CVHBD        (3 HEX DIGITS)
*                    
       DEX              POINT TO NEXT LEFT BYTE
       JSR CVHBS        CONVERT
       ASL A            SHIFT TO LEFT NIBBLE
       ASL A          
       ASL A          
       ASL A          
       ORAA HVAL       CONVERT TO BYTE
       STAA HVAL       SAVE 
CVHBD  LDX HVAL         GET FINAL VALUE
       RTS              RETURN
*                    
* ROUTINE TO CONVERT ASCII TO BINARY
*                    
CVHBS  LDAA 0,X        GET BYTE
       SUBA #$30       CONVERT
       CMPA #$09       0 - 9 ?
       BLE *+4          YES
       SUBA #$07       NO, 10 - 15
       RTS            
*                    
* CVDB: CONVERT DECIMAL TO BINARY
* ON ENTRY DESCRA = ADDRESS OF DECIMAL STRING
*          DESCRC = # BYTES IN DECIMAL STRING
* ON RETURN [X] = VALUE IN BINARY
*                    
DVAL   RMB 2            TEMP STORAGE FOR BINARY
DCOUNT RMB 1            DIGIT COUNT 
TENVL  RMB 2            POWER OF TEN
DXSAV  RMB 2            TEMPORARY STORAGE FOR X
*                    
CVDB   CLR DVAL         DVAL:=0
       CLR DVAL+1     
       CLR TENVL      
       CLR TENVL+1    
       INC TENVL+1      TENVL:=1
       LDX DESCRA       POINT TO STRING
       DEX            
       LDAB DESCRC    
       STAB DCOUNT     INIT DCOUNT
*                    
CVDB1  INX              POINT TO
       DEC B            LEAST SIGNIFICANT
       BNE CVDB1        DIGIT
*                    
CVDB2  STX DXSAV        SAVE POINTER
       LDAB 0,X        GET DIGIT
       ANDB #$0F       CONVERT TO BCD
       CLR A            CLEAR ACCUMULATOR
       LDX #TENVL       POINT TO POWER OF TEN
       JSR MPY16        (A,B):=TENVL*DIGIT
       ADDB DVAL+1     DVAL:=DVAL+TENVL*DIGIT
       ADCA DVAL     
       STAA DVAL      
       STAB DVAL+1    
       CLR A          
       LDAB #$0A       B:=10 
       LDX #TENVL       POINT TO POWER OF TEN
       JSR MPY16        TENVL:=TENVL*10
       STAA TENVL     
       STAB TENVL+1   
       LDX DXSAV        RESTORE POINTER TO STRING
       DEX              POINT NEXT LEFT DIGIT
       DEC DCOUNT       DONE?
       BNE CVDB2        NO 
       LDX DVAL         GET FINAL VALUE
       RTS              RETURN
*                    
* MPY16 16 BIT MULTIPLY ROUTINE
* (A,B):=(A,B)*(2 BYTES POINTED AT BY X REG)
* USES 7 BYTES ON THE STACK
*                    
MPY16  PSH B            PUT VALUES ON THE STACK
       PSH A          
       LDAA 1,X       
       PSH A          
       LDAA 0,X       
       PSH A          
       LDAA #16       
       PSH A          
       TSX              POINT TO DATA
       LDAA 3,X       
*                    
MPY163 ASL B          
       ROL A            FORM ANSWER
       ASL 2,X          SHIFT MULTIPLICAND
       ROL 1,X        
       BCC MPY167     
       ADDB 4,X        ADD MULTIPLIER
       ADCA 3,X      
MPY167 DEC 0,X        
       BNE MPY163       COUNT NOT ZERO
       INS            
       INS            
       INS            
       INS            
       INS            
       RTS              ALL DONE
*                    
* DIV16 16 BIT DIVIDE (UNSIGNED)
* (A,B):=(A,B)/ (X),(X+1)
* [X]=REMAINDER      
*                    
DIV16  PSH B            DIVIDEND TO STACK
       PSH A          
       LDAA 0,X       
       LDAB 1,X       
       PSH B            DIVISOR TO STACK
       PSH A          
       DES              LEAVE R(X)M FOR COUNT
       TSX              (X) PNTR TO STACKED DATA
       LDAA #1        
       TST 1,X        
       BMI DIV153     
DIV151 INC A          
       ASL 2,X        
       ROL 1,X        
       BMI DIV153     
       CMPA #17       
       BNE DIV151     
DIV153 STAA 0,X        SAVE COUNT
       LDAA 3,X       
       LDAB 4,X       
       CLR 3,X        
       CLR 4,X        
DIV163 SUBB 2,X      
       SBCA 1,X      
       BCC DIV165       DIVISOR STILL OK
       ADDB 2,X        DIVISOR TOO LARGE
       ADCA 1,X        RESTORE
       CLC            
       BRA DIV167     
DIV165 SEC            
DIV167 ROL 4,X        
       ROL 3,X        
       LSR 1,X          ADJUST DIVISOR
       ROR 2,X        
       DEC 0,X        
       BNE DIV163     
*                    
       STAA 0,X        SAVE REMAINDER IN X
       STAB 1,X       
       LDX 0,X        
       INS              CLEAN UP STACK
       INS            
       INS            
       PUL A          
       PUL B          
       RTS            
*                    
* ADD16 16 BITADDITION
*[X] POINTS:         
*           LOC(2),TEMP(2)
* LOC(2):=LOC(2)+TEMP(2)
*                    
ADD16  PSH A          
       PSH B          
       LDAA 1,X       
       LDAB 0,X       
       ADDA 3,X      
       ADCB 2,X      
       STAA 1,X       
       STAB 0,X       
       PUL B          
       PUL A          
       RTS            
*                    
* SUB16 16 BIT SUBTRACTION
* [X] POINTS:        
*            LOC(2),TEMP(2)
* LOC(2):=LOC(2)-TEMP(2)
*                    
SUB16  PSH A          
       PSH B          
       LDAA 1,X       
       LDAB 0,X       
       SUBA 3,X      
       SBCB 2,X      
       STAA 1,X       
       STAB 0,X       
       PUL B          
       PUL A          
       RTS            
*                    
* PRINTL PRINT A LINE ON THE TTY 
*                    
PRINTL LDAA OPTNS      GET OPTIONS
       BITA #$80       LIST?
       BNE PLEND        NO
       TST PASS         PASS2
       BEQ PLEND        PASS1
       TST MACFLG       MACRO FLAG SET?
       BEQ PRINT1       NO
*                    
       BITA #$10       PRINT MACROS?
       BNE PLEND        NO
*                    
PRINT1 JSR LINCK        CHECK LINE #
       JSR OUTL         PRINT A LINE
PLEND  RTS              ALL DONE
*                    
* LINE CHECK FOR TOP OF PAGE, ETC.
*                    
LINCK  PSH B          
       LDAB LCOUNT    
       CMPB #$00       END OF PAGE?
       BNE LINCKA       NO
       JSR SPACER       YES SPACE TO TOP OF PAGE
LINCKA INC LCOUNT       BUMP LCOUNT 
       LDAB LCOUNT    
       CMPB #$3C       LCOUNT = 60?
       BNE LINCKB       NO
       CLR LCOUNT       YES,SET FOR TOP OF PAGE
LINCKB PUL B          
       RTS            
*                    
* SPACE TO TOP OF PAGE AND PRINT PAGE MARK
*                    
SPACER LDX #HEADR       POINT TO DATA
       JSR PDATA1       PRINT ON TTY
       RTS            
*                    
HEADR  FDB $0D0C        CRLF
       FDB $0D0A      
       FDB $0D0A      
       FCC '.............'
       FDB $0D0A      
       FDB $0D0A      
       FDB $0D0A      
       FCB $04          EOT  zzz
*                    
* PRINT A FORMATED LINE OF LISTING ON THE TTY
*                    
MCOUNT RMB 1            # BYTES OF MACHINE CODE
POP    RMB 1            PSEUDOPIO:=N0;1,2 BYTES
OPCD   RMB 1            OPCODE IN HEX 
ADR1   RMB 1            INSTRUCTION ADDRESS
ADR2   RMB 1            
LINEN  RMB 5            LINENUM IN ASCII
       FDB $2004        EOT    
*                    
*                    
OUTL   LDX #LINEN       LOAD PARAMS
       LDAA LNUM       LOAD LINNUM (BINARY)
       LDAB LNUM+1    
       JSR CVBTD        CONVERT TO DECIMAL (ASCII)
       LDX #LINEN+1     POINT TO DECIMAL LINE#
       JSR PDATA1       PRINT LINE NUM
*                    
       TST MACFLG       MACRO LINE?
       BEQ OUTLB1       NO
*                    
       LDAA #'+'      
       JSR OUTCHR     
*                    
       LDX #BLANK6    
OUTLB1 JSR PDATA1       PRINT 2 BLANKS
*                    
       TST MCOUNT       PRINT LC ?
       BNE OUTLA        YES
       TST POP          PRINT LC?
       BNE OUTLA        YES
*                    
       LDX #BLANK3      NO, PRINT BLANKS (5)
       JSR PDATA1     
       BRA OUTL2      
*                    
OUTLA  LDX #LC          POINT TO LC
       JSR OUT4HS       PRINT IN HEX, SPACE
       LDAB POP        PSEUDOP?
       BEQ OUTL2        NO
       CMPB #$01       1 BYTE?
       BEQ OUTL1        YES
       LDX #ADR1        POINT TO ADR1, ADR2
       JSR OUT4HS       PRINT 2 BYTES 4 HEX, SPACE
       LDX #BLANK5      POINT TO SPACES
       JSR PDATA1       PRINT BLANKS
       BRA OUTL6      
*                    
OUTL1  LDX #ADR2        POINT TO ADR2
       JSR OUT2HS       PRINT 1 BYTE 2 HEX, SPACE
       LDX #BLANK3      POINT TO BLANKS
       JSR PDATA1       PRINT BLANKS 
       BRA OUTL6      
OUTL2  LDAB MCOUNT     PRINT NOTHING?
       BNE OUTL3        NO 
       LDX #BLANK       PRINT JUST 8 BLANKS
       JSR PDATA1     
       BRA OUTL6      
*                    
OUTL3  LDX #OPCD      
       JSR OUT2HS       PRINT OPCODE(HEX), SPACE
       CMPB #$01       ONLY OP CODE?
       BNE OUTL4        NO
       LDX #BLANK3      PRINT BLANKS
       JSR PDATA1     
       BRA OUTL6      
*                    
OUTL4  CMPB #$02       1 BYTE ADDRESS?
       BNE OUTL5        NO,2 BYTES
       LDX #ADR2        POINT TO OPERAND
       JSR OUT2HS       PRINT 1 BYTE ADDRESS,SPACE
       LDX #BLANK6      PRINT BLANKS
       JSR PDATA1     
       BRA OUTL6      
*                    
OUTL5  LDX #ADR1        POINT TO ADR1,ADR2
       JSR OUT4HS     
*                    
OUTL6  TST CMNFLG       COMMON?
       BEQ *+6          NO
*                    
       LDAA #'C'      
       BRA OUTL6B     
*                    
       TST EXTFLG       EXTERNAL?
       BEQ *+6          NO
*                    
       LDAA #'X'      
       BRA OUTL6B     
*                    
       TST ENTFLG       ENTRY?
       BEQ *+6          NO
*                    
       LDAA #'N'      
       BRA OUTL6B     
*                    
       TST RELFLG       RELOCATABLE?
       BEQ *+6          NO
*                    
       LDAA #'R'      
       BRA OUTL6B     
*                    
OUTL6A LDAA #$20       LOAD SPACE
OUTL6B JSR OUTCHR       PRINT (A)
       LDAA #$20       LOAD SPACE
       JSR OUTCHR       PRINT SPACE
OUTL7A LDX CULINE       POINT TO LINE
OUTL7  LDAA 0,X        GET CHAR
       PSH A            SAVE A
       JSR OUTCHR       PRINT CHAR
       INX              BUMP POINTER
       PUL A            RESTORE A
       CMPA #$0D       CR?
       BNE OUTL7        NO
       LDAA #$0A       YES
       JSR OUTCHR       PRINT LF
       CLR POP        
       CLR MCOUNT     
       CLR RELFLG     
       RTS            
*                    
BLANK  FCB $20          BLANKS
       FCB $20          
       FCB $20          
BLANK3 FCB $20          
       FCB $20          
BLANK5 FCB $20          
BLANK6 FCB $20          
       FCB $20          
       FCB $04          EOT
*                    
* CONVERT BINARY 16 BITS TO 5 DECIMAL DIGITS
* ON ENTRY (A,B) = 16 BITBINARY VALUE
* [X] = ADDRESS OF 5 BYTE STRING FOR DECIMAL
* (ASCII) CONVERTED VALUE
*                    
CVBTD  STX SAVEX        SAVE DATA PTR
       LDX #KIOK        LOAD PTR TO CONSTANTS
CVDEC1 CLR SAVEA        INIT DEC CHAR
CVDEC2 SUBB 1,X      
       SBCA 0,X      
       BCS CVDEC5       OVERFLOW
       INC SAVEA        BUMP CHAR BEING BUILT
       BRA CVDEC2     
*                    
CVDEC5 ADDB 1,X        RESTORE PARTIAL RESULT
       ADCA 0,X      
       PSH A          
       STX SAVEX1     
       LDX SAVEX        LOAD STORE CHAR PTR
       LDAA SAVEA     
       ADDA #$30       MAKE ASCII CHAR
       STAA 0,X       
       PUL A          
       INX            
       STX SAVEX      
       LDX SAVEX1       LOAD PTR TO CONSTANTS
       INX            
       INX            
       CPX #KIOK+10   
       BNE CVDEC1     
       RTS            
*                    
* CONSTANTS          
*                    
KIOK   FDB 10000      
       FDB 1000       
       FDB 100        
       FDB 10         
       FDB 1          
*                    
* TEMPORARY STORAGE  
*                    
SAVEA  RMB 1          
SAVEX  RMB 2          
SAVEX1 RMB 2          
*                    
* PRINT ERROR MESSAGE ROUTINE
* ON ENTRY [X] = ERROR NUM IN BCD
*                    
ERNUM  RMB 2            ERROR NUM IN BCD
ERMSA  FCC '**** ERROR# '
ERMSB  RMB 3            ERROR NUM IN ASCII
       FCB $20          BLANK
ERMSC  RMB 5            ERROR NUM IN ASCII
       FCB $20          BLANK
       FCC ':'       
       FCB $04          EOT
*                    
PRINTE PSH A          
       PSH B          
       STX ERNUM        SAVE ERROR NUM
       LDAA ERNUM      GET ERROR NUM
       ADDA #$30       CONVERT TO ASCII
       STAA ERMSB      SAVE 
       LDAA ERNUM+1    GET ERROR NUM
       LSR A            SHIFT TO RIGHT NIBBLE
       LSR A          
       LSR A          
       LSR A          
       ADDA #$30       CONVERT TO ASCII
       STAA ERMSB+1    SAVE
       LDAA ERNUM+1    GET ERROR NUM
       ANDA #$0F       MASK OUT LEFT NIBBLE
       ADDA #$30       CONVERT TO ASCII
       STAA ERMSB+2    SAVE
       LDX #ERMSC       POINT TO LNUM AREA
       LDAA LNUM      
       LDAB LNUM+1    
       JSR CVBTD        CONVERT LNUM TO DECIMAL 
       LDX #ERMSA       PRINT MESSAGE
       JSR PDATA1     
       JSR OUTL7A       PRINT LAST PART OF LINE
       PUL B          
       PUL A          
       LDX ECOUNT       BUMP ECOUNT
       INX            
       STX ECOUNT     
PEXIT  LDX ERNUM      
       RTS            
*                    
*        ADDRESS TYPE 1
*                    
* [ADCADDAND BIT CMP EOR LDA ORA SBC SUB]
*                    
* IMMEDIATE (2 BYTES):
*   CCC A #NUMBER      CCC B #NUMBER
*   CCC A #SYMBOL      CCC B #SYMBOL
*   CCC A #EXPRESSION  CCC B #EXPRESSION
*   CCC A #'C'         CCC B #'C
*                    
* DIRECT (2 BYTES) OR EXTENDED (3 BYTES):
*   CCC A NUMBER       CCC B NUMBER
*   CCC A SYMBOL       CCC B SYMBOL
*   CCC A EXPRESSION   CCC B EXPRESSION
*                    
* INDEXED (2 BYTE):  
*   CCC A NUMBER,X     CCC B NUMBER,X
*   CCC A SYMBOL,X     CCC B SYMBOL,X
*   CCC A EXPRESSION,X CCC B EXPRESSION,X
*                    
ADDR1  JSR ADRINT       INIT ADDR FIELD VALUES
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR1B       NO
*                    
ADDR1A LDX #$0204       ERROR
       JSR PRINTE       PRINT 
       BRA ADDR1E       RETURN
*                    
ADDR1B JSR ABRCK        CHECK FOR REG A OR B
       LDAB ABR        NEITHER?
       BEQ ADDR1A       YES ERROR
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$23       IMMEDIATE MODE?
       BNE ADDR1C       NO
       COM IMMED        SET IMMEDIATE FLAG
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$27       "'" ?
       BNE ADDR1C       NO
*                    
       LDX CUCHAR       GET NEXT CHARACTER
       LDAA 0,X       
       STAA ADR2      
       BRA ADDR1K     
*                    
ADDR1C JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       LDAB IMMED      IMMEDIATE MODE?
       BEQ ADDR1D       NO
ADDR1K LDAB #$80       IMMEDIATE FORM A
       STAB ORBYA      NIBBLE
       LDAB #$C0       OF
       STAB ORBYB      MACHINE CODE
       BRA ADDR1H     
*                    
ADDR1D JSR NXTOK        GET NEXT TOKEN
       JSR INXCK        INDEXED?
       BNE ADDR1G       YES
*                    
       TST CMNFLG       COMMON?
       BNE ADDR1L       YES
*                    
       TST RELFLG       RELOC?
       BNE ADDR1L       YES
*                    
       LDAB ADR1       DIRECT?
       BEQ ADDR1F       YES
*                    
ADDR1L LDAB #$B0       EXTENDED,FORM A
       STAB ORBYA      NIBBLE
       LDAB #$F0       OF
       STAB ORBYB      MACHINE CODE
*                    
ADDR1E JSR LCNAB3       FORM MACHINE CODE
       BRA ADDR1J     
*                    
ADDR1F LDAB #$90       DIRECT,FORM A
       STAB ORBYA      NIBBLE
       LDAB #$D0       OF
       STAB ORBYB      MACHINE CODE
       BRA ADDR1H     
*                    
ADDR1G LDAB #$A0       INDEXED,FORM A
       STAB ORBYA      NIBBLE 
       LDAB #$E0       OF
       STAB ORBYB      MACHINE CODE
*                    
ADDR1H JSR LCNAB2       FORM MACHINE CODE
ADDR1J JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 2
*                    
* [STA]              
*                    
* DIRECT (2 BYTES) OR EXTENDED (3 BYTES)
*   CCC A NUMBER        CCC B NUMBER
*   CCC A SYMBOL        CCC B SYMBOL
*   CCC A EXPRESSION    CCC B EXPRESSION
*                    
* INDEXED (2 BYTES)  
*   CCC A NUMBER,X      CCC B NUMBER,X
*   CCC A SYMBOL,X      CCC B SYMBOL,X
*   CCC A EXPRESSION,X  CCC B EXPRESSION,X
*                    
ADDR2  JSR ADRINT       INIT ADR FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR2B       NO
*                    
ADDR2A LDX #$0204       ERROR
       JSR PRINTE       PRINT 
       BRA ADDR2E       RETURN
*                    
ADDR2B JSR ABRCK        CHECK FOR REGISTER A OR B
       LDAB ABR        NEITHER?
       BEQ ADDR2A       YES ERROR
       JSR NXTOK        GET NEXT TOKEN
       JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       JSR NXTOK        GET NEXT TOKEN
       JSR INXCK        INDEXED?
       BNE ADDR2G       YES
*                    
       TST CMNFLG       COMMON?
       BNE ADDR2K       YES
*                    
       TST RELFLG       RELOC?
       BNE ADDR2K       YES
*                    
       LDAB ADR1       DIRECT?
       BEQ ADDR2F       YES
*                    
ADDR2K LDAB #$B0       EXTENDED,FORM A
       STAB ORBYA      NIBBLE 
       LDAB #$F0       OF
       STAB ORBYB      MACHINE CODE
*                    
ADDR2E JSR LCNAB3       FORM MACHINE CODE
       BRA ADDR2J     
*                    
ADDR2F LDAB #$90       DIRECT,FORM A
       STAB ORBYA      NIBBLE
       LDAB #$D0       OF
       STAB ORBYB      MACHINE CODE
       BRA ADDR2H     
*                    
ADDR2G LDAB #$A0       INDEXED,FORM A
       STAB ORBYA      NIBBLE
       LDAB #$E0       OF
       STAB ORBYB      MACHINE CODE
*                    
ADDR2H JSR LCNAB2      FORM MACHINE CODE
ADDR2J JSR LCLCN       LC:=LC+LCN
       JMP MAIN1       RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 3
*                    
* [ASL ASR CLR COM DEC INC LSR NEG ROL ROR TST]
*                    
* ACCUMULATOR (1 BYTE)
*   CCC A            
*   CCC B            
*                    
* EXTENDED (3 BYTES) 
*   CCC NUMBER       
*   CCC SYMBOL       
*   CCC EXPRESSION   
*                    
* INDEXED (2 BYTES)  
*   CCC NUMBER,X     
*   CCC SYMBOL,X     
*   CCC EXPRESSION,X 
*                    
ADDR3  JSR ADRINT       INIT ADR FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR3B       NO
*                    
       LDX #$0204       ERROR
       JSR PRINTE       PRINT
       BRA ADDR3D       RETURN
*                    
ADDR3B JSR ABRCK        CHECK FOR REGISTER A OR B
       TST ABR          NEITHER?
       BEQ ADDR3C       YES
       LDAB #$40       ACCUMULATOR,FORM A
       STAB ORBYA      NIBBLE
       LDAB #$50       OF
       STAB ORBYB      MACHINE CODE
       JSR LCNAB1       FORM MACHINE CODE
       BRA ADDR3F     
*                    
ADDR3C JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       JSR NXTOK        GET NEXT TOKEN
       JSR INXCK        INDEXED?
       BNE ADDR3E       YES  
*                    
       LDAB #$70       EXTENDED,FORM A 
       STAB ORBYA      NIBBLE OF MACHINE CODE
*                    
ADDR3D JSR LCN3         FORM MACHINE CODE
       BRA ADDR3F     
*                    
ADDR3E LDAB #$60       INDEXED,FORM A
       STAB ORBYA      NIBBLE OF MACHINE CODE
       JSR LCN2         FORM MACHINE CODE
*                    
ADDR3F JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 4
*                    
* [PSH PUL]          
*                    
* ACCUMULATOR (1 BYTE)
*   PSH A            
*   PSH B            
*   PUL A            
*   PUL B            
*                    
ADDR4  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       JSR ABRCK        CHECK FOR A, B REG
       TST ABR          NEITHER ?
       BNE ADDR4A       NO
*                    
       LDX #$0204       ERROR 
       JSR PRINTE     
*                    
ADDR4A INC ORBYB        ORBYB:=01
       JSR LCNAB1       FORM MC
       JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 5
*                    
* [CPX LDS LDX]      
*                    
* IMMEDIATE (3 BYETS)
*   CCC #NUMBER      
*   CCC #SYMBOL      
*   CCC #EXPRESSION  
*   CCC #'CC         
*                    
* DIRECT (2 BYTES) OR EXTENDED (3 BYTES)
*   CCC NUMBER       
*   CCC SYMBOL       
*   CCC EXPRESSION   
*                    
* INDEXED (2 BYTES)  
*   CCC NUMBER,X     
*   CCC SYMBOL,X     
*   CCC EXPESSION,X  
*                    
ADDR5  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR5B       NO
*                    
ADDR5A LDX #$0240       ERROR
       JSR PRINTE       PRINT 
       BRA ADDR5E       RETURN
*                    
ADDR5B CMPB #$23       IMMEDIATE?
       BNE ADDR5C       NO
       COM IMMED        SET IMMEDIATE FLAG
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$27       "'" ?
       BNE ADDR5C       NO
*                    
       LDX CUCHAR       YES,GET NEXT 2 CHARACTERS
       LDAA 0,X       
       STAA ADR1      
       LDAA 1,X       
       STAA ADR2      
       BRA ADDR5E     
*                    
ADDR5C JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       LDAB IMMED      IMMEDIATE?
       BEQ ADDR5D       NO
       BRA ADDR5E       YES
*                    
ADDR5D JSR NXTOK        GET NEXT TOKEN
       JSR INXCK        INDEXED?
       BNE ADDR5G       YES
*                    
       TST CMNFLG       COMMON?
       BNE ADDR5K       YES
*                    
       TST RELFLG       RELOC?
       BNE ADDR5K       YES
*                    
       LDAB ADR1       DIRECT?
       BEQ ADDR5F       YES
*                    
ADDR5K LDAB #$30       EXTENDED,FORM A
       STAB ORBYA      NIBBLE OF MACHINE CODE
ADDR5E JSR LCN3         FORM MACHINE CODE
       BRA ADDR5J     
*                    
ADDR5F LDAB #$10       DIRECT,FORM A
       STAB ORBYA      NIBBLE OF MACHINE CODE
       BRA ADDR5H     
*                    
ADDR5G LDAB #$20       INDEXED,FORM A
       STAB ORBYA      NIBBLE OF MC
*                    
ADDR5H JSR LCN2         FORM MC
ADDR5J JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 6
*                    
* [STX STS]          
*                    
* DIRECT (2 BYTES) OR EXTENDED (3 BYTES)
*   CCC NUMBER       
*   CCC SYMBOL       
*   CCC EXPRESSION   
*                    
* INDEXED (2 BYTES)  
*   CCC NUMBER,X     
*   CCC SYMBOL,X     
*   CCC EXPRESSION,X 
*                    
ADDR6  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR5C       NO
       BRA ADDR5A       YES,ERROR
*                    
*        ADDRESS TYPE 7
*                    
* [JMP JSR]          
*                    
* INDEXED (2 BYTES)  
*   CCC NUMBER,X     
*   CCC SYMBOL,X     
*   CCC EXPRESSION,X 
*                    
ADDR7  JSR ADRINT       INIT ADDRESS FIELF FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR7A       NO
*                    
       LDX #$0204       ERROR 
       JSR PRINTE       PRINT
       BRA ADDR7B     
*                    
ADDR7A JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       JSR NXTOK        GET NEXT TOKEN
       JSR INXCK        INDEXED?
       BNE ADDR7C       YES
*                    
ADDR7B LDAB #$10       EXTENDED,FORM A NIBBLE
       STAB ORBYA      OF MC
       JSR LCN3         FORM MACHINE CODE
       BRA ADDR7D     
*                    
ADDR7C JSR LCN2         FORM MACHINE CODE
ADDR7D JSR LCLCN        LC:=LC+LCN 
       JMP MAIN1        RETURN TO MAIN LOOP 
*                    
*        ADDRESS TYPE 8
*                    
* [BCC BCS BEQ BGE BGT BHI BLE BLS
*  BLT BMI BNE BPL BRA BSR BVC BVS]
*                    
* RELATIVE (2 BYTES) 
*   CCC NUMBER       
*   CCC SYMBOL       
*   CCC EXPRESSION   
*                    
ADDR8  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE ADDR8A       NO
*                    
       LDX #$0204       ERROR
       JSR PRINTE       PRINT
       BRA ADDR8D     
*                    
ADDR8A TST PASS         PASS ?
       BEQ ADDR8D       PASS1
*                    
       JSR NSEVL        PASS 2 EVAL OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
       LDX LC           LSAVE:=LC+2
       INX            
       INX            
       STX LSAVE      
       LDAA ADR2       CALCULATE OFFSET 
       LDAB ADR1      
       SUBA LSAVE+1  
       SBCB LSAVE    
*                    
       CMPB #$FF       CHECK FOR OUT OF RANGE 
       BNE ADDR8E     
       TST A            NEGATIVE? (FF - 80)
       BMI ADDR8C       OK
*                    
ADDR8E CMPB #$00      
       BNE ADDR8F       OUT OF RANGE 
       TST A            POSITIVE? (00 - 7F)
       BPL ADDR8C       OK
*                    
ADDR8F LDX #$0208       ERROR
       JSR PRINTE       PRINT
*                    
ADDR8C STAA ADR2       SAVE OFFSET
ADDR8D JSR LCN2         FORM MC
       JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
*        ADDRESS TYPE 9
*                    
* [ABA CBA CLC CLI CLV DES DEX INS
*  INX NOP RTI RTS SBA SEC SEI SEV
*  SWI TAB TAP TBA TPA TSX TXS WAI]
*                    
* INHERENT (1 BYTE)  
*   CCC              
*                    
ADDR9  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       TST PASS         PASS ? 
       BEQ ADDR9A       PASS 1
*                    
       JSR OUTBIN       OUTPUT MC
*                    
ADDR9A INC MCOUNT       MCOUNT:=1
       INC LCN          LCN:=1
       JSR PRINTL     
       JSR LCLCN        LC:=LC+LCN  
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
* ROUTINES USED TO INIT AND CHECK ADDRESS FIELD
* FLAGS,MC FORMS AND LISTING FLAGS.
*                    
ABR    RMB 1            REG A OR B FLAG
IMMED  RMB 1            IMMEDIATE MODE FLAG
INDEX  RMB 1            INDEX MODE FLAG
ORBYA  RMB 1            FORM FOR A NIBBLE OF MC
ORBYB  RMB 1            FORM FOR A NIBBLE OF MC
*                    
ADRINT CLR LCN        
       CLR RELFLG     
       CLR CMNFLG     
       CLR EXTFLG     
       CLR ENTFLG     
       CLR POP        
       STAB OPCD       SAVE OPCODE
       CLR MCOUNT     
       CLR ABR        
       CLR IMMED      
       CLR INDEX      
       CLR ADR1       
       CLR ADR2       
       CLR ORBYA      
       CLR ORBYB      
       RTS            
*                    
* CHECK FOR PRESENCE OF A OR B REG
*                    
ABRCK  CMPB #'A'        IS "A"?
       BEQ ABRCKA       YES
       CMPB #'B'        IS "B"?
       BEQ ABRCKA       YES
       RTS              NEITHER, RETURN
*                    
ABRCKA STAB ABR        SAVE REG
       RTS            
*                    
* CHECK FOR INDEXED MODE
*                    
INXCK  CMPB #$2C        COMMA?
       BNE INXCKR       NO
       JSR NXTOK        GET NEXT TOKEN
       CMPB #'X'        IS "X"?
       BNE INXCKR       NO 
       COM INDEX        INDEX:=FF
       RTS            
*                    
INXCKR CLR INDEX      
       RTS            
*                    
* CHECK FOR PASS 2 ERRORS
*                    
P2ERR  CMPB #$FF       ERROR (FROM NSEVL)?
       BNE P2ERRB       NO
*                    
       TST PASS         YES,PASS?
       BEQ P2ERRA       PASS1
       JSR PRINTE       PASS 2,PRINT ERROR
P2ERRA CLR ADR1       
       COM ADR1         ADR1:=FF (TO KILL DIRECT)
P2ERRB RTS            
*                    
* ROUTINES TO FINISH UP ADDRESS TYPE PROCESSING
* THESE ROUTINES DO THE FOLLOWING:
*   PASS 1:          
*     A. LCN := # OF BYTES IN THE INSTRUCTION
*   PASS 2:          
*     A. FORM COMPLETE OPCODE
*     B. OUTPUT MACHINE CODE GENERATED
*     C. PRINT A LINE OF LISTIN
*     D. LCN := # OF BYTES IN THE INSTRUCTION
*                    
* LCNAB1 - 1 BYTE ACCUMULATOR INSTRUCTIONS
*                    
LCNAB1 TST PASS         PASS?  
       BEQ LNAB1S       PASS 1
*                    
       LDAB OPCD       PASS 2,LOAD PARTIAL OPCODE
*                    
* EXTENDED (3 BYTES):
*   CCC  NUMBER      
*   CCC  SYMBOL      
*   CCC  EXPRESSION  
*                    
       LDAA ABR        A OR B?
       BEQ LNAB1O       NEITHER
*                    
       CMPA #$42       IS "B"?
       BEQ LNAB1B       YES
       ORAB ORBYA      A FORM COMPLETE OPCODE
       BRA LNAB1C     
*                    
LNAB1B ORAB ORBYB      B FORM COMPLETE OPCODE
LNAB1C STAB OPCD       SAVE
LNAB1O JSR OUTBIN       OUTPUT OPCODE
       INC MCOUNT       MCOUNT:=1
       JSR PRINTL       PRINT A LINE OF LISTING
LNAB1S INC LCN          LCN=1
       RTS              RETURN
*                    
* LCNAB2 2 BYTE REGISTER (A,B); INDEXED,
* DIRECT, AND IMMEDIATE TYPE INSTRUCTIONS
*                    
LCNAB2 TST PASS         PASS ?
       BEQ LCN2B        PASS 1
*                    
       LDAB OPCD       PASS 2,GET PARTIAL OPCODE
       LDAA ABR        A OR B ?
       BEQ LCN2A        NEITHER
*                    
       CMPA #'B'        B ?
       BEQ LNB2         YES
       ORAB ORBYA      A, FORM COMPLETE OPCODE
       BRA LNAB2S     
*                    
LNB2   ORAB ORBYB      B, FORM COMPLETE OPCODE
LNAB2S STAB OPCD       SAVE 
       BRA LCN2A        FINISH UP
*                    
* LCN2 2 BYTE INDEXED, DIRECT, AND IMMEDIATE TYPE 
* INSTRUCTIONS       
*                    
LCN2   TST PASS         PASS ?
       BEQ LCN2B        PASS 1
*                    
       CLR RELFLG     
       CLR CMNFLG     
       LDAB OPCD       PASS 2,GET PARTIAL OPCODE
       ORAB ORBYA      FORM COMPLETE OPCODE
       STAB OPCD       SAVE
*                    
LCN2A  JSR OUTBIN       OUPUT OPCODE
       LDAB ADR2       GET ADDRESS PART OF MC
       JSR OUTBIN       OUTPUT IT
       INC MCOUNT       MCOUNT:=2
       INC MCOUNT     
       JSR PRINTL       PRINT A LINE OF LISTING
*                    
LCN2B  INC LCN          LCN:=2
       INC LCN        
       RTS              RETURN
*                    
* LCNAB3 3 BYTE REGISTER (A,B) EXTENDED TYPE 
* INSTRUCTIONS       
*                    
LCNAB3 TST PASS         PASS ?
       BEQ LCN3B        PASS 1
*                    
       LDAB OPCD       PASS 2 GET PARTIAL OPCODE
       LDAA ABR        A OR B ?
       BEQ LCN3A        NEITHER
       CMPA #'B'        B ?
       BEQ LNB3         YES
       ORAB ORBYA      A, FORM COMPLETE OPCODE  
       BRA LNAB3S     
*                    
LNB3   ORAB ORBYB      B, FORM COMPLETE OPCODE
LNAB3S STAB OPCD       SAVE
       BRA LCN3A        FINISH UP
*                    
* LCN3 3 BYTE EXTENDED AND IMMEDIATE TYPE
* INSTRUCTIONS       
*                    
LCN3   TST PASS         PASS ?
       BEQ LCN3B        PASS 1
*                    
       LDAB OPCD       GET PARTIAL OPCODE
       ORAB ORBYA      FORM COMPLETE OPCODE
       STAB OPCD       SAVE
*                    
LCN3A  JSR OUTBIN       OUTPUT OPCODE
       LDAB ADR1       OUTPUT THE REST OF THE MC
       JSR OUTBIN     
       LDAB ADR2      
       JSR OUTBIN     
*                    
       TST CMNFLG       COMMON?
       BEQ *+6          NO
*                    
       LDAB #'M'       "COMMON"
       BRA LCN3C      
*                    
       TST RELFLG       RELOC?
       BEQ *+7          NO
*                    
       LDAB #'R'       LOAD "R"
LCN3C  JSR OUTBNR     
*                    
       INC MCOUNT       MCOUNT:=3  
       INC MCOUNT     
       INC MCOUNT     
       JSR PRINTL       PRINT A LINE OF LISTING
*                    
LCN3B  INC LCN          LCN:=3
       INC LCN        
       INC LCN        
       RTS              RETURN
*                    
* LCLCN  LC := LC + LCN
*                    
LCLCN  LDAA LC+1      
       LDAB LC        
       ADDA LCN        ADD LCN
       ADCB #$00     
       STAA LC+1       SAVE LC
       STAB LC        
       RTS              RETURN
*                    
* POCMN: ALLOCATE COMMON STORAGE AREAS
*                    
POCMN  JSR ADRINT     
       JSR LBLCK      
       JSR NXTOK        GET SYMBOL NAME
       CMPB #1         OK?
       BEQ POCMN2       YES
*                    
POCMN0 LDX #$0216       ERROR
POCMN1 JSR PRINTE     
       BRA POCMN4     
*                    
POCMN2 TST PASS         PASS ?
       BNE POCMN3       PASS 2
*                    
       JSR STOSYM       ENTER NAME IN SYMTAB
       LDX SYMPTR     
       STX CMNXS        SAVE ENTRY ADDRESS
*                    
       JSR NXTOK        GET DELIMITER
       CMPB #$2C       IS COMMA?
       BNE POCMN0       NO
*                    
       JSR NXTOK        POINT TO OPERAND
       JSR NSEVL        GET VALUE
       CMPB #$FF       OK?
       BEQ POCMN1       NO
*                    
       LDX CMNXS        POINT TO ENTRY
       LDAA #$BF      
       ANDA 8,X       TURN OFF REL BIT
       ORAA #$10      TURN ON COMMON BIT
       STAA 8,X       
*                    
       LDAA CMNLC     GET COMMON LC
       STAA 6,X       STORE IN ENTRY
       LDAA CMNLC+1   
       STAA 7,X       
*                    
* CMNLC := CMNLC + [ADR1,ADR2]
*                    
       LDAA ADR2      
       LDAB ADR1      
       ADDA CMNLC+1  
       ADCB CMNLC    
       STAA CMNLC+1   
       STAB CMNLC     
*                    
POCMN3 JSR LKPSYM       LOOK UP SYMBOL
       LDX SYMPTR       POINT TO ENTRY
       LDX 6,X          GET COMMON ADDRESS
       STX ADR1         SET UP FOR PRINTL
       COM POP        
       COM CMNFLG     
       INC MCOUNT     
       INC MCOUNT     
*                    
POCMN4 JSR PRINTL     
       JMP MAIN1      
*                    
CMNXS  RMB 2          
*                    
* POEND: PROCESS END PSEUDO OP
*                    
POEND  JSR ADRINT       INIT FLAGS
       JSR LBLCK        CHECK FOR A LABEL
POEND0 TST PASS         PASS ?
       BNE POEND2       PASS2
*                    
       LDX LC           PASS1      
       STX TSTPH        TSTPH:=LC
       COM PASS         PASS:=PASS 2
*                    
       JSR RESTR        REWIND INPUT FILE
*                    
       JMP PASS2        EXECUTE PASS2
*                    
POEND2 LDX TSTPH        PHASING ERRORS?
       CPX LC         
       BEQ ENDP2        NO
*                    
       LDX #$0220     
       JSR PRINTE       PRINT ERROR 
*                    
ENDP2  LDAA OPTNS     
       BITA #$80       LISTING?
       BNE ENDP3        NO     
*                    
       JSR PRINTL     
       JSR CRLF       
*                    
ENDP3  LDAA OPTNS     
       BITA #$20       LIST SYMTAB?
       BEQ SORT1        YES
*                    
       JMP ENDP6        NO
*                    
SORT1  LDX #ZZZ         INIT SORT
       STX CBLOCK     
       CLR SORTF        CLEAR SORT FLAG
       LDX SYMTAB       POINT TO TABLE
       BRA SORT3      
*                    
SORT2  INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
       INX            
*                    
SORT3  CPX SYMEND       AT TABLE END?
       BNE SORT2A       NO
*                    
       TST SORTF        FOUND AN ENTRY?
       BEQ *+5        
*                    
       JMP SORT5        PRINT ENTRY
       JMP ENDP6        ALL DONE
*                    
SORT2A LDAB 0,X       
       CMPB #$20       BLANK?
       BEQ SORT2        YES, GET NEXT ENTRY
*                    
       LDAB 8,X       
       CMPB #$FF       USED ENTRY?
       BEQ SORT2        YES
*                    
* COMPARE ENTRY AT CBLOCK WITH NEW ENTRY
*                    
       STX CXS2         SET UP FOR COMPARISON
       STX PSTNG2     
       LDX CBLOCK     
       STX PSTNG1     
       LDAB #6        
       STAB PCOUNT    
       LDX #PSTNG1    
       JSR COMPAR     
       BHI SORT4        NEED SWITCH
*                    
       LDX CXS2       
       BRA SORT2      
*                    
SORT4  LDX CXS2         NEW CBLOCK POINTERS
       STX CBLOCK     
       LDAB #$FF      
       STAB SORTF      SET SORT FLAG
       BRA SORT2      
*                    
SORT5  JSR LINCK      
       LDAB #6        
       LDX CBLOCK     
*                    
ENDP4  LDAA 0,X        GET CHAR
       JSR OUTCHR       PRINT 
       INX              POINT TO NEXT CHAR
       DEC B            DECR COUNT
       BNE ENDP4        NOT DONE
*                    
       LDAA #$20       PRINT SPACE
       JSR OUTCHR     
       JSR OUT4HS       PRINT 4 HEX LOCATION
       STX ENDXS      
       LDAB 0,X       
       BITB #$40       RELOC?
       BEQ *+7          NO
*                    
       LDAA #'R'       LOAD "R" 
       JSR OUTCHR       PRINT IT
*                    
       BITB #$20       MACRO NAME?
       BEQ *+7          NO      
*                    
       LDAA #'M'       LOAD "M"
       JSR OUTCHR       PRINT IT
*                    
       BITB #$10       COMMON?
       BEQ *+7          NO
*                    
       LDAA #'C'      
       JSR OUTCHR     
*                    
       BITB #$08       EXTERNAL?
       BEQ *+7          NO
*                    
       LDAA #'X'      
       JSR OUTCHR     
*                    
       BITB #$04       ENTRY?
       BEQ *+7        
*                    
       LDAA #'N'      
       JSR OUTCHR     
*                    
       LDAB 0,X        REDEFIEND?
       BPL ENDP5        NO
*                    
       LDX #REDEF       PRINT ERROR MESSAGE
       JSR PDATA1     
*                    
ENDP5  LDX ENDXS      
       LDAB #$FF       GET DONE
       STAB 0,X       
       JSR CRLF      
       JMP SORT1      
*                    
ENDP6  JSR CRLF      
       JSR CRLF      
       LDX #ENDMB       PRINT # OF ERRORS MSG
       LDAA ECOUNT    
       LDAB ECOUNT+1  
       JSR CVBTD        CONVERT TO ASCII
       LDX #ENDMA     
       JSR PDATA1       PRINT IT
*                    
       JSR CRLF      
       JSR CRLF      
*                    
       LDX #CMSG        COMMON AREA MESSAGE
       JSR PDATA1     
*                    
       LDX #CMNLC       POINT TO COMMON LENGTH
       JSR OUT4HS     
       JSR CRLF      
*                    
       LDAA OPTNS     
       BITA #$40       OBJECT?
       BNE ENDP7        NO
*                    
ENDP6A JSR WREOF        WRITE EOF
       JMP UPDATE       CLOSE FILE AND EXIT TO MONTOR
*                    
ENDP7  JMP MONTOR     
*                    
REDEF  FCC ' REDEFINED'
       FCB $04           EOT
*                    
ENDMA  FCC 'THERE WERE: '
ENDMB  RMB 5          
       FCC ' ERRORS'  
       FCB 4          
*                    
CMSG   FCC 'COMMON LENGTH= '
       FCB 4          
*                    
CRLF   LDX #MCRLF     
       JSR PDATA1     
       RTS            
*                    
MCRLF  FDB $0D0A        CR,LF       
       FCB $04          EOT
*                    
ZZZ    FCC '[[[[[['   
       FDB 0000       
       FCB 0          
CBLOCK RMB 2          
CXS2   RMB 2          
SORTF  RMB 1          
ENDXS  RMB 2          
*                    
*                    
* POENT: PROCESS "ENTRY" PSEUDO OP
*        DEFINES AN ENTRY POINT FOR 
*        REFERENCE BY OTHER MODULES.
*                    
POENT  JSR ADRINT       INIT
       JSR LBLCK        CHECK FOR A LABEL
*                    
       JSR NXTOK        GET ENTRY NAME
       CMPB #1         OK?
       BEQ POENT1       YES
*                    
       LDX #$0216       NO, ERROR
       JSR PRINTE     
       BRA POENT3     
*                    
POENT1 TST PASS         PASS? 
       BEQ POENT4       PASS 1
*                    
       JSR LKPSYM       GET ENTRY ADDRESS
       CMPB #$FF       IN SYMTAB
       BNE POENT2       YES
*                    
       LDX #$0211       NO, ERROR
       JSR PRINTE     
       BRA POENT3     
*                    
POENT2 STX ADR1         SAVE ENTRY ADDRESS
       LDX SYMPTR       POINT TO ENTRY
       LDAA 8,X        GET INFO BYTE
       ORAA #$04       TURN ON ENTRY BIT
       STAA 8,X       
*                    
       JSR PBLOCK       OUTPUT LABEL
       LDAB ADR1       OUTPUT ENTRY ADDRESS     
       JSR OUTBIN     
       LDAB ADR2      
       JSR OUTBIN     
       LDAB #'R'       "RELOCATABLE"
       JSR OUTBNR     
       LDAB #'N'       ENTRY  "ENT"
       JSR OUTBNR     
*                    
POENT3 COM POP        
       COM ENTFLG     
       INC MCOUNT     
       INC MCOUNT     
       JSR PRINTL     
POENT4 JMP MAIN1        ALL DONE
*                    
* PBLOCK: ROUTINE TO WRITE LOADED ENTRY
* SYMBOL ON TAPE     
*                    
PBLOCK LDX SYMPTR       POINT TO ENTRY SYMBOL
       LDAA #6         LENGTH
*                    
PBLK2  LDAB 0,X        GET A CHAR
       PSH A          
       STX PBXS       
       JSR OUTBIN     
       PUL A          
       LDX PBXS       
       INX            
       DEC A            ALL DONE?
       BNE PBLK2        NO
*                    
       RTS            
*                    
PBXS   RMB 2          
*                    
* POEQU: PROCESS EQU PSEUDOP
*                    
POEQU  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       LDX SYMPTR     
       STX EQUXS        SAVE SYMPTR
       TST LBFLG        LABEL?
       BNE EQUB         YES
*                    
       LDX #$0213       NO,ERROR
EQUA   JSR PRINTE       PRINT ERROR
       BRA EQUE       
*                    
EQUB   JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE EQUC         NO
*                    
       LDX #$0216       ERROR
       BRA EQUA       
*                    
EQUC   JSR NSEVL        EVALUATE OPERAND
       CMPB #$FF       ERRORS?
       BEQ EQUA         YES
*                    
       TST PASS         PASS?
       BNE EQUD         PASS2
*                    
       LDX EQUXS      
       LDAA 8,X        GET INFO BYTE
       TST RELFLG       RELOCATE?
       BNE EQUF         YES
       ANDA #$BF       NO, - TURN OFF IN INFO BYTE
*                    
       TST CMNFLG       COMMON?
       BEQ EQUF         NO
*                    
       ORAA #$10       YES TURN ON IN INFO BYTE
*                    
EQUF   STAA 8,X       
       LDAA ADR2       STORE VALUE
       STAA 7,X       
       LDAA ADR1      
       STAA 6,X       
*                    
EQUD   COM POP          SET PSEUDOP FLAG
       INC MCOUNT       MCOUNT:=2
       INC MCOUNT     
*                    
EQUE   JSR PRINTL       PRINT A LINE OF LISTING
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
EQUXS  RMB 2            SAVE AREA
*                    
* POEXT: PROCESS "EXTERNAL" PSEUDOP
*        MAKE EXTERNALLY-DEFINED SUBROUTINE
*        AVAILABLE TO MODULE
*                    
POEXT  JSR ADRINT       INIT
       JSR LBLCK        CHECK FOR A LABEL
*                    
       JSR NXTOK        GET EXTERNAL ENTRY NAME
       CMPB #1         OK?
       BEQ POEXT1       YES
*                    
       LDX #$0216       NO, ERROR
       JSR PRINTE     
       JSR PRINTL     
       BRA POEXT4     
*                    
POEXT1 INC LCN        
       INC LCN        
       INC LCN        
*                    
       TST PASS         PASS?
       BNE POEXT2       PASS2
*                    
       JSR STOSYM       PUT NAME IN SYMBOL TABLE
       LDX SYMPTR     
       LDAA 8,X       
       ORAA #$08       SET EXT BIT
       STAA 8,X       
       BRA POEXT3     
*                    
POEXT2 LDAB #$7E       "JMP"
       STAB OPCD      
       JSR OUTBIN     
       JSR LKPSYM       SET UP ENTRY NAME
       JSR PBLOCK       OUTPUT NAME
       LDAB #'X'       "EXT"
       JSR OUTBNR     
       CLR ADR1       
       CLR ADR2       
       INC MCOUNT     
       INC MCOUNT     
       INC MCOUNT     
       COM EXTFLG     
       JSR PRINTL     
*                    
POEXT3 JSR LCLCN      
POEXT4 JMP MAIN1        ALL DONE
*                    
* POFCB: PROCESS FCB PSEUDOP
*                    
POFCB  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE FCBB         NO
*                    
FCBA   LDX #$0216       ERROR
       JSR PRINTE       PRINT
       BRA FCBC         FINISH UP
*                    
FCBB   JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS ERRORS
       INC LCN          LCN:=1
       TST PASS         PASS? 
       BEQ FCBD         PASS1
*                    
       LDAB ADR2       PASS2,OUTPUT MC
       JSR OUTBIN     
       INC MCOUNT       MCOUNT:=1
       INC POP          POP:=1
*                    
FCBC   JSR PRINTL       PRINT A LINE OF LISTING
FCBD   JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
* POFCC: PROCESS FCC PSEUDO OP
*                    
POFCC  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$27       QUOTE?
       BEQ FCCB         YES
*                    
FCCA   LDX #$0204       ERROR
       JSR PRINTE       PRINT
       BRA FCCG         FINISH UP 
*                    
FCCB   LDX CUCHAR       GET CURRENT CHAR
       LDAB 0,X       
       CMPB #$0D       EOL? 
       BEQ FCCA         YES
       STAB ADR2       NO, SAVE CHAR
*                    
FCCC   TST PASS         PASS ?
       BEQ FCCD         PASS1
       JSR OUTBIN       OUTPUT MC 
FCCD   LDX CUCHAR       CUCHAR:=CUCHAR+1
       INX            
       STX CUCHAR     
       INC LCN          LCN:=LCN+1
       LDAB 0,X        GET CHAR 
       CMPB #$27       QUOTE?
       BEQ FCCE         YES
*                    
       CMPB #$0D       EOL?
       BNE FCCC         NO 
       BRA FCCF         YES 
*                    
FCCE   LDAB 1,X        GET NEXT CHAR
       CMPB #$27       TWO QUOTES?
       BNE FCCF         NO
       INX            
       STX CUCHAR       CUCHAR:=CUCHAR+1
       BRA FCCC       
*                    
FCCF   INC POP          POP:=1
       INC MCOUNT       MCOUNT:=1
*                    
FCCG   JSR PRINTL       PRINT LINE OF LISTING
       JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
* PROCESS FDB PSEUDO OP
*                    
POFDB  JSR ADRINT       INIT ADDRESS FIELD FLAGS
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL?
       BNE FDBB         NO
*                    
FDBA   LDX #$0216       ERROR
       JSR PRINTE       PRINT 
       BRA FDBC         FINISH UP
*                    
FDBB   JSR NSEVL        EVALUATE OPERAND
       JSR P2ERR        PRINT PASS 2 ERRORS
*                    
       INC LCN          LCN:=2
       INC LCN        
       TST PASS         PASS?
       BEQ FDBD         PASS1
*                    
       LDAB ADR1       OUTPUT MC
       JSR OUTBIN     
       LDAB ADR2      
       JSR OUTBIN     
       TST RELFLG       RELOC?
       BEQ FDCF         NO
*                    
       LDAB #'R'       YES
       BRA FDCG       
*                    
FDCF   TST CMNFLG       COMMON?
       BEQ FDCE         NO
*                    
       LDAB #'M'       YES
*                    
FDCG   JSR OUTBNR       OUTPUT "R" OR "M"
FDCE   INC MCOUNT       MCOUNT:=2
       INC MCOUNT     
       COM POP          POP:=FF
*                    
FDBC   JSR PRINTL       PRINT A LINE OF LISTING
FDBD   JSR LCLCN        LC:=LC+LCN
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
* PROCESS THE IF PSEUDO OP
*                    
POIF   JSR ADRINT     
       JSR LBLCK      
       JSR NXTOK      
       CMPB #$0D       EOL?
       BNE POIFB        NO
*                    
POIFA  LDX #$0216     
       JSR PRINTE     
       BRA POIFE      
*                    
POIFB  JSR NSEVL        EVALUATE OPERAND
       CMPB #$FF       ERRORS? 
       BEQ POIFA        YES
*                    
       JSR PSHIF        STACK PRESENT IFFLG
       TST IFFLG        ASSEMBLING?
       BEQ POIFE        NO
*                    
       TST ADR1         IS =0?
       BNE POIFC        NO
*                    
       TST ADR2         IS =0?
       BNE POIFC        NO
*                    
       CLR IFFLG        TURN OFF ASSEMBLING
       BRA POIFE      
*                    
POIFC  LDAA #$FF       TURN ON ASSEMBLING     
       STAA IFFLG     
*                    
POIFE  JSR PRINTL     
       JMP MAIN1      
*                    
* PROCESS THE MAC PSEUDOP
*                    
CMFLG  RMB 1            COMMENT FLAG 0=NO,FF=YES
MACERR RMB 1            MAC ERROR 0=NO,FF=YES    
*                    
POMAC  JSR ADRINT       INIT FLAGS
       JSR PRINTL     
       CLR CMFLG      
       CLR MACERR     
       TST PASS         PASS?
       BNE POMAC2       PASS2
*                    
       TST LBFLG        LABELED?
       BNE POMAC1       YES, OK
*                    
       COM MACERR       SET ERROR FLAG
       LDX #$0226       ERROR
       JSR PRINTE     
       BRA POMAC      
*                    
POMAC1 LDX SYMPTR       PT TO LABEL
       LDAA #$20      
       STAA 8,X        SET MACRO FLAG IN SYMTAB
       LDAA MACPTR    
       STAA 6,X        SAVE MACRO LOC
       LDAA MACPTR+1  
       STAA 7,X       
*                    
       JSR NXTOK        CHECK FOR "C"
       LDX DESCRA     
       LDAA 0,X       
       CMPA #'C'        "C"? 
       BNE *+5          NO
*                    
       COM CMFLG        YES,SAVE COMMENTS
*                    
POMAC2 JSR RDLINA       GET NEXT LINE
       LDX LNUM       
       INX            
       STX LNUM       
       JSR PRINTL     
       LDX CULINE       PT TO LINE
       LDAA 0,X        GET FIRST CHAR
       CMPA #'*'        COMMENT?
       BNE POMAC5       NO
*                    
       TST CMFLG        SAVE COMMENTS?
       BEQ POMAC2       NO
*                    
       JSR MACMOV       YES, SAVE
*                    
       BRA POMAC2     
*                    
POMAC5 CLR LBFLG        CLEAR LABEL FLAG
       LDX CULINE       POINT TO LINE
       LDAA 0,X        GET CHAR
       CMPA #$20       BLANK?
       BEQ POMAC6       YES
*                    
       JSR NXTOK        GET LABEL
       COM LBFLG        SET LABEL FLAG
*                    
POMAC6 JSR NXTOK        GET MNEMONIC
       LDAA #4         SET FOR COMPARE
       STAA PCOUNT    
       LDX DESCRA     
       STX PSTNG1     
       LDX #MEND      
       STX PSTNG2     
       LDX #PSTNG1      POINT TO PARAMS
       JSR COMPAR       COMPARE
       BNE POMAC8       MEND NOT FOUND
*                    
       TST LBFLG        LABELED?
       BEQ POMAC7       YES
*                    
       LDX #$0227       ERROR
       JSR PRINTE     
       BRA POMAC7     
*                    
POMAC8 JSR MACMOV       PUT INTO MACTBL
       JMP POMAC2     
*                    
POMAC7 TST PASS         PASS?
       BNE POMACA       PASS2
*                    
       LDAA #$17       ETB TO END OF MACRO
       LDX MACPTR     
       STAA 0,X       
       INX            
       STX MACPTR     
*                    
POMACA JMP MAIN1        ALL DONE
*                    
* MOVE MACRO TO MACRO TABLE
*                    
MACMOV TST PASS         PASS?
       BNE MACMVE       PASS2
*                    
       TST MACERR       ERRORS?
       BNE MACMVE       YES
*                    
       LDX CULINE     
       STX CUCHAR     
*                    
MACLOP LDX CUCHAR       GET CHAR FROM INBUF
       LDAA 0,X       
       INX            
       STX CUCHAR     
       LDX MACPTR       POINT TO MACTBL
       CPX MACEND       FULL?
       BNE MACMV1       NO
*                    
       LDAA #$0D       CR TO MACTBL
       STAA 0,X       
       INX            
       STX MACPTR     
*                    
       LDX #$0228       ERROR 
       JSR PRINTE     
       BRA MACMVE     
*                    
MACMV1 STAA 0,X        STORE CHAR IN MACTBL
       INX            
       STX MACPTR     
       CMPA #$0D       EOL?
       BNE MACLOP       NO
*                    
MACMVE RTS              ALL DONE
*                    
MEND   FCC 'MEND'     
*                    
* PONAM: PROCESS NAM PSEUDOP
*                    
PONAM  JSR ADRINT     
       JSR LBLCK      
*                    
       JSR NXTOK        GET PROGRAM NAME
       CMPB #1         OK?
       BEQ PONAM1       YES
*                    
       LDX #$0216       ERROR
       JSR PRINTE     
       JMP POENT3       
*                    
PONAM1 TST PASS         PASS?
       BNE PONAM2       PASS 2
*                    
       JSR STOSYM       SAVE NAME IN SYMTAB
       JMP POENT1     
*                    
PONAM2 LDAB CMNLC      OUTPUT COMMON BLOCK SIZE
       JSR OUTBIN       
       LDAB CMNLC+1   
       JSR OUTBIN     
*                    
       LDAB #'P'       "PROGRAM"
       JSR OUTBNR     
*                    
       JMP POENT1       PROCESS AS ENTRY NAME
*                    
* PONIF: PROCESS NIF PSEUDOP
*                    
PONIF  JSR ADRINT     
       JSR LBLCK      
       JSR PULIF        GET LAST IFFLG
       JSR PRINTL     
       JMP MAIN1      
*                    
* PSHIF: PUSH THE CURRENT IFFLG ONTO THE IFSTACK
*                    
PSHIF  STS STKSAV       SAVE STACK POINTER
       LDX IFSTKP       LOAD IF STACK POINTER
       CPX #IFSTK-8      FULL?
       BEQ PSPLER       YES
*                    
       LDS IFSTKP       LOAD STACK POINTER
       LDAA IFFLG     
       PSH A            STACK IFFLG
       BRA PSPLCM     
*                    
* PULIF: PULL LAST IFFLG OFF OF THE IFSTACK
*                    
PULIF  STS STKSAV       SAVE STACK POINTER
       LDX IFSTKP       LOAD IF STACK POINTER
       CPX #IFSTK       UNDERFLOW? 
       BEQ PSPLER       YES
*                    
       LDS IFSTKP       LOAD STACK POINTER
       PUL A            POP LAST IFFLG
       STAA IFFLG     
       BRA PSPLCM     
*                    
PSPLER LDX #$0254     
       JSR PRINTE     
       RTS            
*                    
PSPLCM STS IFSTKP     
       LDS STKSAV     
       RTS            
*                    
* POPAG: PROCESS THE PAGE PSEUDOP
*                    
POPAG  JSR ADRINT       INIT FLAGS
       JSR LBLCK        CHECK FOR LABEL
       TST PASS         PASS ?
       BEQ PAGEND       PASS 1
*                    
       LDAB LCOUNT     LCOUNT=0?
       BEQ PAGEND       YES
*                    
       LDAA #$3C       B:=60
       SUBB LCOUNT     B:=60-LCOUNT
*                    
PAGEA  JSR CRLF       
       DEC B            TO
       BNE PAGEA        TOP OF PAGE       
       CLR LCOUNT       LCOUNT:=0
PAGEND JMP MAIN1        RETURN TO MAIN LOOP
*                    
* PORMB: PROCESS RMB PSEUDOP
*                    
PORMB  JSR ADRINT       INIT ADDRESS FIELD FLAGS 
       JSR NXTOK        GET NEXT TOKEN
       CMPB #$0D       EOL? 
       BNE RMBB         NO
*                    
       LDX #$0216       ERROR
RMBA   JSR PRINTE       PRINT
       BRA RMBC         FINISH UP
*                    
RMBB   JSR NSEVL        EVALUATE OPERAND
       CMPB #$FF       ERRORS?
       BEQ RMBA         YES
*                    
       TST PASS         PASS?
       BEQ RMBD         PASS1
*                    
       JSR RMBOUT       OUTPUT MC
       INC MCOUNT       MCOUNT:=2
       INC MCOUNT     
       COM POP          SET PSEUDOOP FLAG
*                    
RMBC   JSR PRINTL       PRINT A LINE OF LISTING
RMBD   LDAA LC+1       LC:=LC+ADR1,ADR2
       LDAB LC        
       ADDA ADR2     
       ADCB ADR1     
       STAA LC+1      
       STAB LC        
       JMP MAIN1        RETURN TO MAIN LOOP
*                    
RMBOUT CLR B            LOAD 00
       LDX ADR1         LSAVE:=#BYTES FROM RMB
       STX LSAVE      
*                    
RMBOTA JSR OUTBIN       OUTPUT MC
       LDX LSAVE      
       DEX            
       BEQ RMBOTB       DONE
*                    
       STX LSAVE      
       CLR B          
       BRA RMBOTA       DO AGAIN
*                    
RMBOTB RTS              RETURN
*                    
* LBLCK: CHECK FOR AN ILLEGAL LABEL ON A
*        PSEUDOOP. IF THERE IS ONE DELETE IT.
*        AND PRINT AN ERROR MESSAGE.
*                    
LBLCK  TST LBFLG        LABEL?
       BEQ LBLCK2       NO
*                    
       TST PASS         PASS? 
       BNE LBLCK1       PASS2
*                    
       JSR DELSYM       PASS1 DELETE LAST SYMBOL
*                    
LBLCK1 LDX #$0223       ERROR
       JSR PRINTE       PRINT 
*                    
LBLCK2 RTS              RETURN
*                    
* OUTBIN: OUTPUT A BYTE AS TWO HEX ASCII CHARS.
* OUTBNR: OUTPUT "R", "N", OR "X".
*                    
OUTBIN LDAA OPTNS     
       BITA #$40       OUTPUT?
       BNE OUTRET       NO
*                    
       TBA            
       BSR OUTHL        CONVERT LEFT NIBBLE
       JSR OUTB       
       TBA            
       BSR OUTHR        CONVERT RIGHT NIBBLE 
       JSR OUTB       
OUTRET RTS            
*                    
OUTBNR LDAA OPTNS     
       BITA #$40       OUTPUT?
       BNE OUTRET       NO 
*                    
       TBA            
       JSR OUTB       
       RTS            
*                    
OUTHL  LSR A          
       LSR A          
       LSR A          
       LSR A          
OUTHR  ANDA #$0F      
       ADDA #$30     
       CMPA #$39      
       BLS *+4        
       ADDA #$07     
       RTS            
*                    
* ASSORTED I/O ROUTINES
*                    
PDATA2 JSR OUTCHR     
       INX            
PDATA1 LDAA 0,X       
       CMPA #$04      
       BNE PDATA2     
       RTS            
*                    
OUT2H  LDAA 0,X       
OUT2HA BSR OUTHLL     
       LDAA 0,X       
       INX            
       BRA OUTHRR     
*                    
OUT4HS BSR OUT2H      
OUT2HS BSR OUT2H      
OUTS   LDAA #$20      
       JMP OUTCHR     
*                    
OUTHLL LSR A          
       LSR A          
       LSR A          
       LSR A          
*                    
OUTHRR ANDA #$0F      
       ADDA #$30     
       CMPA #$39      
       BLS OUTCHR     
*                    
       ADDA #$07     
*                    
OUTCHR PSH A          
       JSR OUTEEE     
       PUL A          
       CMPA #$0A       LF?
       BNE OUTCHE       NO
*                    
       PSH A          
       PSH B          
       LDAB #8        
*                    
OUTCHL LDA #$00       
       JSR OUTEEE     
       DEC B          
       BNE OUTCHL     
*                    
       PUL B          
       PUL A              
OUTCHE RTS            
* 
* 
* DRIVER FOR CP/68 OPERATING SYSTEM
* WITH MF68 FLOPPY DISK
* IN SIMH SWTPC SIMULATOR
* 
* 
* 
FCBSTA EQU 5  ERROR STATUS FLAG
FCBDBA EQU 7  DATA BUFFER ADDRESS
FCBDRV EQU 9  DRIVE NUMBER
FCBTRK EQU 10 TRACK NUMBER
FCBSCT EQU 11 SECTOR NUMBER
FCBTlK EQU 12 TRACK LINK POINTER
FCBSLK EQU 13 SECTOR LINK POINTER

*
* COMMAND-LINE INTERPRETER BASE-PAGE LOCATIONS
*
DSCRA  EQU $20   ADDRESS OF TOKEN FOR CP68
DSCRC  EQU $22   NUMBER OF CHARS IN CURRENT TOKEN FOR CP68  
CUCHR  EQU $23   ADDR OF NEXT CHAR TO BE PROCESSED IN CP68 COMMAND LIEN

EINIIO CLR OFOFL    CLEAR OUTPUT FILE OPEN FLAG
       SWI NXTOK GET NEXT TOKEN TO SKIP .CMD PART OF TRANSIENT COMMAND IN COMMAND LINE
        FCB $2F
       LDX #ERRIFN   SET ERROR MSG
       STX OPERRM
       LDX #INFCB   X=INPUT FCB
       JSR GETFN    GET FNAME FROM COMMAND LINE, STORE IT IN FCB
       JSR OPENI    OPEN INPUT FILE
       LDX CUCHR
       DEX
EINI0  INX
       LDAA 0,X     GET NEXT CHAR IN COMMAND LINE
       CMPA #$0D    IS EOL?
       BEQ EINI1    BR IF COMMAND LINE TERMINATED
       CMPA #$20     IS BLANK?
       BEQ EINI0    BR TO SKIP CHAR IF IT IS 
       CMPA #','    IS COMMA?
       BEQ EINI0    BR TO SKIP CHAR IF IT IS 
       STX CUCHR    SET BEGGINING OF SECOND FNAME IN CMD LINE
       LDX #ERROFN   SET ERROR MSG
       STX OPERRM
       LDX #OUTFCB  X=OUTPUT FCB
       JSR GETFN    GET FNAME FROM COMMAND LINE, STORE IT IN FCB
       INC OFOFL    SIGNAL OUTPUT FILENAME SET
EINI1  RTS

GETFN  SWI FMTFCB PROCESS COMMAND LINE (POINTED BY CUCHR) TO GET FILENAME AND STORE IT IN FCB POINTED BY X-REG
         FCB $2C
       LDAA FCBSTA,X
       BEQ GETFN1    BR IF STATUS OK
       LDX OPERRM    GET ERR MSG
       JMP DSKERR    PRINT IT AND RETERN TO O/S 
GETFN1 RTS 

EGETB  JMP FGETB    GETB ROUTINE: READ A BYTE FROM INPUT FILE

EOUTB  PSHA         SAVE BYTE TO WRITE TO FILE 
       LDAA OFOFL   IS OUTPUT FILE PROVIDED IN CMD LINE?
       BEQ EOUTB5   BR IF NOT (ERROR, MISSING OUTPUT FILE)
       CMPA #2       
       BEQ EOUTB4   BR IF OUTPUT FILE ALREADY OPEN
       PSHB
       SWI PSHX
        FCB $05       
       JSR OPENO    OPEN OUTPUT FILE
       INC OFOFL    SIGNAL FILE OPEN IN FLAG
       SWI PULX
        FCB $06
       PULB        
EOUTB4 PULA         RETRIEVE BYTE TO WRITE TO FILE
       JMP FOUTB    OUTB ROUTINE: WRITE A BYTE TO OUTPUT FILE
EOUTB5 LDX #ERRMOF  MSG 'MISSIGN OUTPUT FILENAME' 
       JMP DSKERR   PRINT ERROR    
              
EWREOF RTS          WRITE EOF ROUTINE

EREST  JMP REWIND  RESTR ROUTINE: REOPEN INPUT FILE FOR PASS2

EUPDT  JSR CLOSEI   UPDATE ROUTINE: OUTPUT/INPUT FILE CLOSE, THEN RETURN TO O/S
       LDAA OFOFL   CHECK IF OUTPUT FILE OPEN
       CMPA #2 
       BNE EMON     BR IF NOT
       JSR CLOSEO   CLOSE OUTPUT FILE
EMON   SWI WARMS      CP68 WARM START
        FCB $1F       RETURN TO OPERATING SYSTEM

OFOFL  RMB 1        OUTPUT FILE OPEN FLAG (1=FILE OPEN)

INFCB  FCB 0,0      INPUT FILE FCB
       FCC 'DSK' 
       FCB 0        FCBSTA=5
       FCB 0        FCBDTT=6  00 FOR FILE READ, FF FOR FILE WRITE
       FDB INBUF
       FCB 0        FCBDRV=9
       FCB 0        FCBTRK=10
       FCB 0        FCBSCT=11
       FCB 0,0,0,0  FCBFWD=12, FCBBAK=14
       RMB 8        FCBNAM=16
       FCC '.'  
       RMB 3        FCBEXT=25
       FCB $04      END OF FILENAME
       FCB 0        FCBTYP=29  FILETYPE
       FCB 0        FCBACS=30  FILE ACCESS CODE
       FCB 0,0      FCBFTS=31  FIRST TRACK/SECT
       FCB 0,0      FCBLTS=33  LAST TRACK/SECT
       FCB 0,0      FCBNMS=35  NUMBER OF SECTORS
       FCB 0,0      FCBNFB=37  NEXT FCB IN ACTIVE CHAIN
       FCB 0,0      FCBIND=39  INDEX IN DATA BUFFER
       FCB 0        FCBSCF=41  SPACE COMPRESSION FLAG (COMPRESSION OFF)
      
INBUF  RMB 128      INPUT BUFFER        

OUTFCB FCB 0,0      OUTPUT FILE
       FCC 'DSK' 
       FCB 0
       FCB $FF      OUTPUT FILE
       FDB OUTBUF
       FCB 0        FCBDRV=9
       FCB 0        FCBTRK=10
       FCB 0        FCBSCT=11
       FCB 0,0,0,0  FCBFWD=12, FCBBAK=14
       RMB 8        FCBNAM=16
       FCC '.'  
       RMB 3        FCBEXT=25
       FCB $04      END OF FILENAME
       FCB 0        FCBTYP=29  FILETYPE
       FCB 0        FCBACS=30  FILE ACCESS CODE
       FCB 0,0      FCBFTS=31  FIRST TRACK/SECT
       FCB 0,0      FCBLTS=33  LAST TRACK/SECT
       FCB 0,0      FCBNMS=35  NUMBER OF SECTORS
       FCB 0,0      FCBNFB=37  NEXT FCB IN ACTIVE CHAIN
       FCB 0,0      FCBIND=39  INDEX IN DATA BUFFER
       FCB 0        FCBSCF=41  SPACE COMPRESSION FLAG (COMPRESSION OFF)
OUTBUF RMB 128      OUTPUT BUFFER        

OPENI  LDX #INFCB   
       CLR FCBSTA,X CLEAR STATUS
       SWI OPEN
        FCB $14
       TST FCBSTA,X ERROR?
       BEQ OPEN2
       LDX #ERROIN  SET THE ERROR MSG TO PRINT IF OPEN FAILS
       JMP DSKERR
OPEN2  RTS 
OPERRM RMB 2         * THE ERROR MSG TP PRINT IF OPEN FAILS
       
OPENO  LDX #OUTFCB
       CLR FCBSTA,X CLEAR STATUS
       SWI OPEN
        FCB $14
       LDAA FCBSTA,X ERROR?
       BEQ OPEN4
       CMPA #03     OUTPUT FILE ALREADY EXISTS?
       BEQ OPEN3   BR IF SO
       LDX #ERROOU
       JMP DSKERR
OPEN3  LDX #ERROFE  
       JMP DSKERR
OPEN4  RTS 
       
DSKERR JSR PDATA1    PRINT ERROR MESSAGE
       JMP EMON      AND RETURN TO OS
ERROIN FCC 'INPUT FILE OPEN ERROR'
       FCB $0D,$0A,$04
ERROOU FCC 'OUTPUT FILE OPEN ERROR'
       FCB $0D,$0A,$04
ERRCLS FCC 'CLOSE FILE ERROR'
       FCB $0D,$0A,$04
ERRGET FCC 'INPUT FILE READ ERROR'
       FCB $0D,$0A,$04
ERROUT FCC 'OUTPUT FILE WRITE ERROR'
       FCB $0D,$0A,$04
ERRIFN FCC 'INVALID INPUT FILE NAME'
       FCB $0D,$0A,$04
ERROFN FCC 'INVALID OUTPUT FILE NAME'
       FCB $0D,$0A,$04
ERRMOF FCC 'MISSING OUTPUT FILE NAME'
       FCB $0D,$0A,$04
ERROFE FCC 'OUTPUT FILE ALREADY EXISTS'
       FCB $0D,$0A,$04      

CLOSEI LDX #INFCB
       BRA CLOSE2
CLOSEO LDX #OUTFCB
CLOSE2 CLR FCBSTA,X 
       SWI CLOSE
        FCB $15
       TST FCBSTA,X ERROR?
       BEQ CLOSE3
       LDX #ERRCLS
       JMP DSKERR
CLOSE3 RTS 

FGETB  PSHB  GET BYTE FROM INPUT FILE, CARRY SET IF EOF
       SWI PSHX
        FCB $05
       LDX #INFCB
       SWI READ
        FCB $18
       LDAB FCBSTA,X  GET STATUS                       
       BEQ FGETB1
       CMPB #8   STATUS 8 IS END OF FILE     
       BEQ FGETB2      
       LDX #ERRGET OTHERWISE READ ERROR
       JMP DSKERR
FGETB1 SWI PULX
        FCB $06
       PULB
       CLC
       RTS       
FGETB2 SWI PULX
        FCB $06
       PULB
       SEC
       RTS       

FOUTB  PSHB   WRITE BYTE INTO OUTPUT FILE
       SWI PSHX
        FCB $05
       LDX #OUTFCB
       SWI WRITE
        FCB $19
       TST FCBSTA,X  GET STATUS                       
       BEQ FOUTB1
       LDX #ERROUT WRITE ERROR
       JMP DSKERR
FOUTB1 SWI PULX
        FCB $06
       PULB
       RTS       
        
REWIND LDX #INFCB REWIND INPUT FILE
       SWI REWD         
        FCB $16
       TST FCBSTA,X  GET STATUS                       
       BEQ REW1
       LDX #ERROIN RE-OPEN ERROR
       JMP DSKERR
REW1   RTS        
        

EINCH  JMP $E1AC   INPUT A CHAR FROM TTY: USE SWTBUF EINCH
EOUTCH JMP $E1D1   OUTPUT A CHAR TO TTY: USE SWTBUF OUTEEE

* START OF ASSEMBLER TABLES
ETABL  FDB *+2

       END
       