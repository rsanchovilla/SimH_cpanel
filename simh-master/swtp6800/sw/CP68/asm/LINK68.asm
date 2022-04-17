       NAM LINK  LINKING LOADER
*      
*       C.COPYRIGHT 1977 BY
*       ROBERT D.GRAPPEL LEXINGTON MASS.
*       AND KACK E. HEMENWAY BOSTON MASS.
*       ALL RIGHTS RESERVED
*
*       MODIFIED BY ROBERTO SANCHO TO 
*       SUPPORT CP/68 OPERATING SYSTEM
*       (C) APR 2022        
*
*       DO NOT CHANGE CP/68 STACK 
*       SO DO NOT ISSUE: LDS #$A042

         ORG $B000      

START   JMP    LOAD
               
TABLES         JMP ETABL    ENT TABLES  
MONTOR         JMP EMON     ENT MONTOR
GETB           JMP EGETB    ENT GETB
OUTB           JMP EOUTB    ENT OUTB  
WREOF          JMP EWREOF   ENT WREOF
INITIO         JMP EINIIO   ENT INITIO
UPDATE         JMP EUPDT    ENT UPDATE
*
*
INEEE   JMP    EINCH     INPUT A CHAR
OUTEEE  JMP    EOUTCH    OUTPUT A CHAR TO TTY
BASE    RMB    2         BASE ADDRESS
NFLAG   RMB    1         NIBBLE FLAG 0=LFT, FF=RIGTH
BYTE    RMB    1         TEMPORARY LOCATION
LC      RMB    2         LOCATION COUNTER
DESCRA  RMB    2         DESCRIPTOR ADDRESS
DESCRC  RMB    1         DESCRIPTOR COUNT
NXTSYM  RMB    2         NEXT ENTRY IN SYMTAB
SYMTAB  RMB    2         SYMBOL TABLE 
SYMEND  RMB    2         END OF ADDR
SYMPTR  RMB    2         SYMTAB (SYMBOL TABLE) POINTER
STRNG1  RMB    2         PARAM LIST
STRNG2  RMB    2         FOR 
COUNT   RMB    1         COMPARE
XSAV    RMB    2         TEMP 
HIVAL   RMB    2         HIGHEST COMMON COUNT
CBAS    RMB    2         START OF COMMON
CBASSV  RMB    2         CBAS SAVE TEMP 
HICBAS  RMB    2         END OF COMMON
BASESV  RMB    2         FIRST LOCATION
LAST    RMB    2         LAST LOCATION
UPLIM   RMB    2         UPPER MEMORY LIMIT
*                        
*                        
* LOAD IS THE ENTRY POINT TO THE LOADER
*                        
LOAD    JSR    INITIO    INIT THE I/O
        LDX   TABLES+1  POINT TO START OF SYMTAB
        LDAA  #$A3       ADD SPACE FOR 75 SYMBOLS
        LDAB  #$02       
        ADDA   1,X        ADD TO START OF SYMTAB
        ADCB   0,X        
        STAA   SYMEND+1  INIT
        STAB   SYMEND    
        LDX   0,X        GET START OF SYMTAB
        STX    SYMTAB    
        STX    NXTSYM    
*                        
        CLR    NFLAG     NFLAG := LEFT
        JSR    CRLF      
*                        
        LDX   #MSGL      GET BASE ADDRESS
        JSR    PDATA1    
        JSR    BADDR     GET VALUE IN HEX
        JSR    CRLF      
        STX    BASE      INIT
        STX    BASESV    INIT
*                        
        LDX   #MSGA      GET UPPER MEMORY LIMIT
        JSR    PDATA1    
        JSR    BADDR     GET VALUE IN HEX
        STX    UPLIM     
        JSR    CRLF      
*                        
        LDX   #MSGB      GET START OF COMMON
        JSR    PDATA1    
        JSR    BADDR     INIT
        JSR    CRLF      
        STX    CBAS      INIT
        STX    CBASSV    INIT
        STX    HICBAS    INIT
        LDX    BASE      GET START OF MEMORY
*                        
*                        
LOAD2   JSR    GETB      GET A BYTE
        BCS    LOADE     EOF 
*                        
        CMPA   #$00      NULL?
        BEQ    LOAD2     YES
*                        
        CMPA   #'P'      PROGRAM MODULE?
        BNE    *+5       
*                        
        JMP    LOADP     YES
*                        
        CMPA   #'N'      ENTRY ?
        BNE    *+5       NO
*                        
        JMP    LOADN     YES
*                        
        CMPA   #'X'      EXTERNAL ?
        BNE    *+5       NO
*                        
        JMP    LOADX     YES
*                        
        CMPA   #'M'      COMMON?
        BNE    *+5       NO
*                        
        JMP    LOADM     YES 
*                        
        CMPA   #'R'      RELOCATABLE?
        BNE    *+5       NO
*                        
        JMP    LOADR     YES
*                        
* CONVERT TWO ASCII BYTES TO ONE HEX BYTE
*                        
        SUBA   #$30      
        CMPA   #$09      
        BLE    *+4       
        SUBA   #$07      
*                        
        TST    NFLAG     WHICH NIBBLE ?
        BNE    RNIBL     RIGHT
*                        
        ASL A            LEFT
        ASL A            
        ASL A            
        ASL A            
        STAA   BYTE      SAVE LEFT NIBBLE
        COM    NFLAG     SET FOR RIGHT NIBBLE        
        BRA    LOAD2     
*                        
RNIBL   LDAB   BYTE      GET LEFT NIBBLE
        ABA              
        STAA   0,X       
        STX    LAST      LOADED ADDRESS
        INX              
        JSR    MEMCHK    CHECK MEMORY LIMIT
        COM    NFLAG     SET FOR LEFT NIBBLE
        BRA    LOAD2     
*                        
* EOF FINISH LOAD        
*                        
LOADE   JSR    CRLF      
        JSR    CRLF      
        LDX    #MSGE     "LOAD LIMITS"
        JSR    PDATA1    
        JSR    CRLF      
        LDX    #BASESV   PRINT STARTING ADDRESS
        JSR    OUT4HS    
        LDX    #LAST     
        JSR    OUT4HS    PRINT END ADDRESS
        JSR    CRLF      
        JSR    CRLF      
*                        
        JSR    PRTSYM    PRINT LOAD MAP
        JSR    CRLF      "COMMON"
        LDX    #MSGC     
        JSR    PDATA1    
        JSR    CRLF      
        LDX    #CBAS     PRINT START OF COMMON
        JSR    OUT4HS    
        LDX    HICBAS    
        CPX    CBAS      ANY COMMON ?
        BEQ    LOADE1    NO
*                        
        DEX              HICBAS:=HICBAS-1
        STX    HICBAS    
*                        
LOADE1  LDX    #HICBAS   PRINT END OF COMMON
        JSR    OUT4HS    
        JSR    CRLF      
        JSR    CRLF      
*                        
*                        
        LDX    #SAVFIL   
        JSR    PDATA1    PRINT PROMPT
        JSR    INEEE     
        CMPA   #'Y'      SAVE LOADED FILE?
        BNE    *+8       NO
*                        
        JSR    CRLF      
        JMP    PUNCH     
*                        
        JSR    CRLF      
        JMP    UPDATE    ALL DONE
*                        
*                        
* RELOCATE ADDRESS       
*                        
LOADR   DEX              POINT TO ADDRESS
        DEX              
        LDAA   1,X       GET ADDRESS
        LDAB   0,X       
        ADDA   BASE+1    ADD IN RELOCATION
        ADCB   BASE      
        STAA   1,X       STORE
        STAB   0,X       
        INX              POINT TO NEXT ADDRESS
        JSR    MEMCHK    CHECK MEMORY LIMIT
        INX              
        JSR    MEMCHK    CHECK MEMORY LIMIT
        JMP    LOAD2     
*                        
* PROGRAM MODULE         
*                        
LOADP   DEX              
        DEX              BACKUP OVER COMMON LENGTH
        STX    BASE      SAVE AS NEW BASE
        LDAA   1,X       
        LDAB   0,X       
        ADDA   CBAS+1    
        ADCB   CBAS      
        STAA   CBASSV+1  SAVE IN CBASSV
        STAB   CBASSV    
*                        
* SEE IF NEW COMMON IS LONGER
* THAN LAST COMMON       
*                        
        LDAA   HICBAS+1  
        LDAB   HICBAS    
        SUBA   CBASSV+1  
        SBCB   CBASSV    
*                        
        BCC    LOADP1    NO
*                        
        LDX    CBASSV    YES
        STX    HICBAS    
*                        
LOADP1  LDX    BASE      LOAD NEW BASE
        JMP    LOAD2     
*                        
* COMMON RELOCATION      
*                        
LOADM   DEX              POINT TO ADDRESS
        DEX              
        LDAA   1,X       GET ADDRESS
        LDAB   0,X       
        ADDA   CBAS+1    ADD IN BASE OF COMMON
        ADCB   CBAS      
        STAA   1,X       STORE 
        STAB   0,X       
        INX              POINT TO NEXT ADDRESS
        JSR    MEMCHK    CHECK MEMORY LIMIT
        INX              
        JSR    MEMCHK    CHECK MEMORY LIMIT
*                        
        JMP    LOAD2     
*                        
* HANDLE ENTRY SYMBOL    
*                        
LOADN   LDAB   #6        6 CHAR/SYMBOL
        STAB   DESCRC    
        DEX              
        DEX              
        LDAA   0,X       LC:=ENTRY VALUE
        STAA   LC        
        LDAA   1,X       
        STAA   LC+1      
        DEX              BACK UP TO START OF SYMBOL
        DEX              
        DEX              
        DEX              
        DEX              
        DEX              
        STX    LAST      INIT LAST
        STX    DESCRA    POINT TO SYMBOL
        JSR    LKPSYM    
        CMPB   #$FF      SYMBOL IN TABLE?
        BNE    LOADN4    YES
*                        
        JSR    STOSYM    STORE SYMBOL
        LDX    SYMPTR    
        INC    8,X       SET DEFINED BIT
*                        
LOADN3  LDX    LAST      RESTORE POINTER
        JMP    LOAD2     
*                        
LOADN4  BITB   #$01      ALREADY DEFINED?
        BEQ    LOADN6    NO
*                        
        ORAB   #$80      SET REDEFINED BIT
        LDX    SYMPTR    
        STAB   8,X       
        BRA    LOADN3    
*                        
LOADN6  STX    DESCRA    
        LDAA   LC        ADDR OF SYMBOL
        LDAB   LC+1      
        LDX    SYMPTR    
        INC    8,X       DET DEFINED BIT
        STAA   6,X       
        STAB   7,X       
*                        
* FOLLOW LINKAGES        
*                        
        LDX    DESCRA    
LOADN5  LDX    0,X       
        STX    SYMPTR    
        LDX    DESCRA    
        STAA   0,X       
        STAB   1,X       
        LDX    SYMPTR    
        CPX    #$FFFF    AT END LINK?
        BEQ    LOADN3    YES
*                        
        STX    DESCRA    NO
        BRA    LOADN5    
*                        
* HANDLE EXTERNAL SYMBOL 
*                        
LOADX   LDAB   #6        6 CHARS/SYMBOL
        STAB   DESCRC    
        DEX              BACK UP TO START OF SYMBOL
        DEX              
        DEX              
        DEX              
        DEX              
        DEX              
        STX    DESCRA    POINT TO SYMBOL
        STX    LC        SAVE ADDRESS
        INX              
        INX              
        STX    LAST      SAVE ADDRESS
        JSR    LKPSYM    
        CMPB   #$FF      IN TABLE?
        BNE    LOADX4    YES
*                        
        JSR    STOSYM    NO,SAVE SYMBOL
        LDX    LC        
LOADX2  LDAA   #$FF      SET END LINK
        STAA   0,X       
        STAA   1,X       
*                        
LOADX3  LDX    LAST      LOAD NEW LAST
        JMP    LOAD2     
*                        
LOADX4  LDX    SYMPTR    POINT TO SYMBOL ENTRY
        LDAB   8,X       GET INFO BYTE
        BITB   #$01      DEFINED?
        BEQ    LOADX5    NO
*                        
        LDAA   6,X       GET ENTRY ADDRESS
        LDAB   7,X       
        LDX    LC        
        STAA   0,X       
        STAB   1,X       SAVE ADDRESS
        BRA    LOADX3    
*                        
LOADX5  LDX    6,X       GET FIRST LINK
*                        
LOADX6  STX    DESCRA    FOLLOW LINKAGE
        LDX    0,X       
        CPX    #$FFFF    END LINK?
        BNE    LOADX6    
*                        
        LDAA   LC        
        LDAB   LC+1      
        LDX    DESCRA    
        STAA   0,X       NEW LINK ADDRESS
        STAB   1,X       
        LDX    LC        
        BRA    LOADX2    SET NEW END LINK
*                        
* SYMBOL TABLE ROUTINES  
*                        
* STORE SYMBOL IN SYMTAB 
*                        
STOSYM  LDX    NXTSYM    
        STX    SYMPTR    SAVE ENTRY ADDR
        CPX    SYMEND    FULL?
        BNE    STOSY1    NO
*                        
        LDX    #SYMFUL   ERROR 
        JSR    PDATA1    
        JMP    MONTOR    RETURN TO EXEC
*                        
* MOVE SYMBOL TO SYMTAB  
*                        
STOSY1  LDX    DESCRA    GET ADDRESS OF SYMBOL
        LDAA   0,X       
        INX              
        STX    DESCRA    
        LDX    NXTSYM    
        STAA   0,X       
        INX              
        STX    NXTSYM    
        DEC    DESCRC    
        BNE    STOSY1    
*                        
        LDAA   LC        GET LC (LOCATION COUNTER)
        STAA   0,X       
        LDAA   LC+1      
        STAA   1,X       
        LDAA   #$00      SET INFO BIT
        STAA   2,X       
        INX              
        INX              
        INX              
        STX    NXTSYM    
        RTS              
*                        
* LOOK UP SYMBOL IN TABLE
*                        
LKPSYM  LDX    SYMTAB    
        CPX    NXTSYM    EMPTY TABLE?
        BEQ    LKPSY3    YES
*                        
LKPSY1  STX    SYMPTR    
        STX    STRNG1    
        LDX    DESCRA    
        STX    STRNG2    
        LDAA   DESCRC    
        STAA   COUNT     
        LDX    #STRNG1   
        JSR    COMPAR    
        BEQ    LKPSY2    MATCH
*                        
        LDX    SYMPTR    
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        CPX    NXTSYM    END OF ENTRIES?
        BNE    LKPSY1    NO
*                        
* NOT IN SYMTAB          
*                        
LKPSY3  LDAB   #$FF      
        RTS              
*                        
* FOUND SYMBOL           
*                        
LKPSY2  LDX    SYMPTR    
        LDAB   8,X       
        LDX    6,X       GET VALUE
        RTS              
*                        
* COMPARE TWO STRINGS    
*                        
* ON ENTRY [X] = A PARAM LIST OF 5 BYTES:
*    ADDR (STRING 1)     
*    ADDR (STRING 2)     
*    COUNT OF BYTES TO BE COMPARED
*                        
* ON RETURN IF CC Z BIT IS SET THERE IS A MATCH
*                        
COMPAR  PSH A            
        PSH B            
        LDAB   4,X       GET COUNT
        STX    XSAV      SAVE PARAM POINTER
CMP1    LDX    XSAV      GET PARAM POINTER
        LDX    0,X       GET ADDR(STRING1)
        LDAA   0,X       GET CHAR
        LDX    XSAV      
        INC    1,X       PTR SET TO NEXT 
        BNE    CMP2      CHAR IN
        INC    0,X       STRING1
CMP2    LDX    XSAV      GET PARAM POINTER
        LDX    2,X       GET ADDR(STRING2)
        CMPA   0,X       COMPARE 
        BNE    CDONE     NOT EQUAL
        LDX    XSAV      GET PARAM POINTER
        INC    3,X       PTR SET TO NEXT
        BNE    CMP3      CHAR IN
        INC    2,X       STRING2
CMP3    DEC B            DEC COUNTER
        BNE    CMP1      TRY AGAIN
CDONE   PUL B            DONE
        PUL A            
        RTS              
*                        
* PRINT LOAD MAP         
*                        
PRTSYM  LDX    #MAPMSG   "LOAD MAP"
        JSR    PDATA1    
        JSR    CRLF      
        LDX    SYMTAB    
        CPX    NXTSYM    ANY SYMBOLS?
        BEQ    PRTSM3    NO 
*                        
* FIND LOWEST VALUE ENTRY TO PRINT
*                        
SORT    LDX    #$FFFF    
        STX    HIVAL     
        LDX    SYMTAB    
*                        
SORT1   STX    SYMPTR    
        LDAA   #$FF      
        CMPA   8,X       ALREADY PRINTED?
        BEQ    SORT2     YES
*                        
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        STX    STRNG1    POINT TO ENTRY
        LDX    #HIVAL    
        STX    STRNG2    POINT TO HIVAL
        LDAA   #2        
        STAA   COUNT     SET COUNT
        LDX    #STRNG1   POINT TO PARAMETERES
        JSR    COMPAR    
        BCS    SORT3     ENTRY < HIVAL
*                        
SORT2   LDX    SYMPTR    
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        INX              
        CPX    NXTSYM    END OF TABLE?
        BEQ    SORT4     YES
*                        
        BRA    SORT1     NO
*                        
SORT3   LDX    SYMPTR    HIVAL := ENTRY
        STX    DESCRA    SAVE LOWEST ENTRY ADDR
        LDX    6,X       GET VALUE
        STX    HIVAL     
        BRA    SORT2     
*                        
SORT4   LDX    #$FFFF    
        CPX    HIVAL     PRINTED ENTIRE MAP?
        BNE    PRTSM0    NO
*                        
PRTSM3  RTS              YES, ALL DONE
*                        
PRTSM0  LDX    DESCRA    GET ENTRY TO BE PRINTED
PRTSM1  LDAB   #6        PRINT 6 CHAR SYMBOL
PRTSM2  LDAA   0,X       GET CHAR
        INX              
        JSR    OUTEEE    
        DEC B            DONE?
        BNE    PRTSM2    NO
*                        
        JSR    OUTS      PRINT SPACE
*                        
        JSR    OUT4HS    PRINT HEX VALUE
        LDAB   0,X       GET INFO BYTE
        BITB   #$01      UNRESOLVED?
        BNE    PRTSM4    NO
*                        
        LDX    #UNRES    YES
        JSR    PDATA1    
*                        
PRTSM4  BITB   #$80      REDEFINED?
        BEQ    PRTSM5    NO
*                        
        LDX    #REDEF    YES
        JSR    PDATA1    
*                        
*                        
PRTSM5  JSR    CRLF      
        LDX    DESCRA    FLAG AS PRINTED
        LDAA   #$FF      
        STAA   8,X       
        JMP    SORT      GET ANOTHER ENTRY
*                        
* CHECK TO SEE IF MEMORY OVERRUN
*                        
MEMCHK  CPX    UPLIM     OVERRUN?
        BEQ    MEMCKE    YES
*                        
        RTS              NO
*                        
MEMCKE  JSR    CRLF      
        LDX    #MSGD     ERROR MESSAGE
        JSR    PDATA1    
        JSR    CRLF      
        INS              FIX STACK
        INS              
        JMP    LOADE     PRINT PARTIAL LOAD MAP
*                        
* I/O ROUTINES           
*                        
BXSAV   RMB 2            
*                        
* BUILD 16 BIT ADDRESS   
*                        
BADDR   BSR    INBYTE    
        STAA   BXSAV     
        BSR    INBYTE    
        STAA   BXSAV+1   
        LDX    BXSAV     
        RTS              
*                        
* INPUT A BYTE           
*                        
INBYTE  BSR    INHEX     
        ASL A            
        ASL A            
        ASL A            
        ASL A            
        TAB              
        BSR    INHEX     
        ABA              
        RTS              
*                        
* INPUT HEX CHARACTER    
*                        
INHEX   JSR    INEEE     
        SUBA   #$30      
        BMI    NOTHEX    
*                        
        CMPA   #$09      
        BLE    INHEXR    
*                        
        CMPA   #$11      
        BMI    NOTHEX    
*                        
        CMPA   #$16      
        BGT    NOTHEX    
*                        
        SUBA   #$07      
*                        
INHEXR  RTS              
*                        
* NOT  A HEX CHARACTER   
*                        
NOTHEX  LDAA   #'?'      
        JSR    OUTEEE    
        BRA    INHEX     
*                        
* PRINT DATA STRING      
*                        
PDATA2  JSR    OUTEEE    
        INX              
PDATA1  LDAA   0,X       
        CMPA   #4        
        BNE    PDATA2    
*                        
        RTS              
*                        
* OUTPUT TWO HEX CHARACTERS
*                        
OUT2H   LDAA   0,X       
OUT2HA  BSR    OUTHL     
        LDAA   0,X       
        INX              
        BRA    OUTHR     
*                        
OUT4HS  BSR    OUT2H     
OUT2HS  BSR    OUT2H     
OUTS    LDAA   #$20      
        JMP    OUTEEE    
*                        
OUTHL   LSR A            
        LSR A            
        LSR A            
        LSR A            
*                        
OUTHR   ANDA   #$0F      
        ADDA   #$30      
        CMPA   #$39      
        BLS    OUTCH     
*                        
        ADDA   #$07      
*                        
OUTCH   JMP    OUTEEE    
*                        
CRLF    LDAA   #$0D      
        JSR    OUTEEE    
        LDAA   #$0A      
        JSR    OUTEEE    
*                        
        RTS              
*
* MESSAGES
*
SYMFUL  FCC    'SYMBOL TABLE OVERFLOW'
        FCB    4
MSGL    FCC    'ENTER BASE ADDRESS: '
        FCB    4    
MSGA    FCC    'ENTER UPPER MEMORY LIMIT: '
        FCB    4    
MSGB    FCC    'ENTER START OF COMMON: '
        FCB    4    
MSGC    FCC    'COMMON:'
        FCB    4
MSGD    FCC    '****** MEMORY OVERRUN ******'
        FCB    4
MSGE    FCC    'LOAD LIMITS: '
        FCB    4
MAPMSG  FCC    'LOAD MAP'
        FCB    4
UNRES   FCC    'UNRESOLVED '
        FCB    4
REDEF   FCC    'REDEFINED'
        FCB    4
SAVFIL  FCC    'SAVE LOADED FILE? Y OR N: '
        FCB    4
*
* PUNCH: OUTPUT LOAD MODULE IN mikbug FORMAT
* (BASESV - LAST)
*
MCONT   RMB 1
TEMP    RMB 1
*
PUNCH   EQU *

PUN11   LDAA  LAST+1
        SUBA  BASESV+1
        LDAB  LAST
        SBCB  BASESV
        BNE PUN22
*
        CMPA  #16
        BCS PUN23
*
PUN22   LDAA  #15 
*
PUN23   ADDA  #4
        STAA  MCONT
        SUBA  #3
        STAA  TEMP
*
        LDX #MTAPE1
        JSR DDATA1
        CLR B
*
* OUTPUT FRAME COUNT
*
        LDX #MCONT
        BSR PUNT2
*
* OUTPUT ADDRESS
*
        LDX #BASESV
        JSR PUNT2
        JSR PUNT2
*
* OUTPUT DATA
*                        
        LDX BASESV
*
PUN32   JSR PUNT2
        DEC TEMP
        BNE PUN32                                           
*
        STX BASESV
        COM B
        PSH B
        TSX
        JSR PUNT2
        PUL B
        LDX BASESV
        DEX
        CPX LAST
        BNE PUN11
*
        LDX #EOF
        JSR DDATA1
*
        JSR WREOF                
*
        JMP UPDATE
*
PUNT2   ADDB  0,X
OUT2HD  LDAA  0,X
        BSR OUTHLL
        LDAA  0,X
        INX
        BRA OUTHRR
*       
OUTHLL  LSR A
        LSR A 
        LSR A 
        LSR A 
*
OUTHRR  ANDA  #$0F
        ADDA  #$30
        CMPA  #$39        
        BLS OTHRR
*
        ADDA  #7
*
OTHRR   JSR OUTB                
        RTS
*         
DDATA2  JSR OUTB
        INX
DDATA1  LDAA  0,X
        CMPA  #4
        BNE DDATA2
*
        RTS
*
EOF     FDB $0D0A
        FCC 'S9'
        FDB $0D0A
        FCB 4
*
MTAPE1  FDB $0D0A
        FCC 'S1'
        FCB 4
*

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
       FCB 3        FCBTYP=29  FILETYPE -> OUTPUT TO TEXT FILE (IS A S19 FILE)
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
       LDAA FCBSTA,X ERROR?
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

* START OF LINKER TABLES
ETABL  FDB *+2

        END                
