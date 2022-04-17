* INITIALIZE A DISK FOR CP/68 OPERATING SYSTEM
* BASED ON ICOM 8 INCH FLOPPY DISK PROGRAM BY J. HEMENWAY 1979
* 
* MODIFIED BY ROBERTO SANCHO TO SUPPORT MF68 DISKS ON SWTPC SIMH EMULATION
* APR/2022 (FROM LISTING IN PDF P166)
*
* DISK GEOMETRY: 35 TRACKS X 18 SECTORS X 128 BYTES
* TRACK 0 HAS SECTORS NUMBERED 0,2..18 (NO SECTOR ONE, FIRST SECTOR IS ZERO).
* TRACK 1..34 HAS SECTORS NUMBERED 1,2..18
* THIS IS THE SAME LAYOUT AS FLEX 1.0 ON MF68 
*
* TRACK 0: SECTOR 0,2,3: HOLDS BOOT PROGRAM
* TRACK 0: SECTOR 4..18: DIRECTORY
* TRACK 1..34: DATA SECTORS FOR FILES
*
* DISK ATTRIBUTES
*
SECSIZ EQU 128    128 BYTES PER SECTOR
TRKSIZ EQU 18     18 SECTORS PER TRACK
DSKSIZ EQU 34     34 DATA TRACKS ON DISK (LESS TRACK 0) 
*
* FCB DEFINITIONS:
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
DESCRA EQU $20   ADDRESS OF TOKEN
VALUE  EQU $27   VALUE OF NUMERIC TOKEN
*

       ORG $2000

FCBSPC FCB 0,0
       FCC 'DSK'
       FCB 0
       FCB $FF
       FCB 0,0,0,0,0,  0,0,0,0,0    SIZE: 35 BYTES
       FCB 0,0,0,0,0,  0,0,0,0,0  
       FCB 0,0,0,0,0,  0,0,0,0,0  
       FCB 0,0,0,0,0
       
BUFFER FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0      SIZE 128 BYTES = SECTOR BUFFER
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0

       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0
       FCB 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0

PROMPT FCC 'INIT. DISK IN DRIVE '
DRVNO  FCB 0
       FCC ' ? '
       FCB $04

* INIT ENTRY POINT 

INITR  LDA A VALUE+1      GET DRIVE TO INIT   .*tron0 
       AND A #$03         MAX 4 DRIVES
       LDX   #FCBSPC      POINT T0 FCB
       STA A FCBDRV,X     STORE DRIVE NUM
       ADD A #$30         MAKE DRV NUM ASCII
       STA A DRVNO        PUT IN PROMPT LINE
       LDX   #PROMPT      
       SWI PRTMSG         OUTPUT PROMPT 
        FCB 49
       SWI GTCMD          GET USER RESPONSE
        FCB 48
       LDX   DESCRA       
       LDA A 0,X          GET FIRST CAHR OF RESPONSE
       CMP A #'Y'         WAS IT 'YES'?
       BEQ   INITR2       BR IF YES
       RTS                QUIT
                          
INITR2 LDX   #FCBSPC                           
       CLR   FCBTRK,X     TRAK=0
       CLR   FCBSCT,X     SECTOR=0
       SWI TXAB  
        FCB 2        
       LDX   #BOOT        POINT TO BOOT CODE
       SWI XABX          
        FCB 4
       STA A FCBDBA,X
       STA B 8,X
       BSR   WRTBL        WRITE FIRST SECTOR -> FIRST 128 BYTES OF BOOT PROG
       TST   FCBSTA,X     DISK ERROR?
       BEQ   INIT2B       BR IF NO ERROR
       BRA   INITQ0       FATAL ERROR, QUIT
INIT2B INC   FCBSCT,X     
       INC   FCBSCT,X     SECTOR=2
       SWI TXAB
        FCB 2        
       LDX   #BOOT+$80
       SWI XABX
        FCB 4
       STA A FCBDBA,X
       STA B 8,X
       BSR   WRTBL        WRITE SECOND SECTOR -> SECOND 128 BYTES OF BOOT PROG
       TST   FCBSTA,X     DISK ERROR?
       BEQ   INIT2C       BR IF NO ERROR
       BRA   INITQ0       FATAL ERROR, QUIT
INIT2C INC   FCBSCT,X     SECTOR=3
       SWI TXAB
        FCB 2        
       LDX   #BOOT+$100   WRITE THIRD BLOCK -> THIRD 128 BYTES OF BOOT PROG
       SWI XABX           BOOT PROG CAN HAVE UP TO 128x3-6 BYTES
        FCB 4
       STA A FCBDBA,X
       STA B 8,X
       LDA A #$01         SET 01/01 AS TWO LAST BYTES OF SECTOR 3
       STA A BOOT+$17E           
       STA A BOOT+$17F           
       BSR   WRTBL        WRITE SECTOR 3 - LAST 128 BYTES OF BOOT SECTOR      
       TST   FCBSTA,X     ERROR?      
       BEQ   INIT2D       BR IF NOT      
INITQ0 BRA   INITQ        ERROR     
INIT2D SWI   TXAB         RESTORE BACK EMPTY BUFFER      
        FCB 2        
       LDX   #BUFFER      AS SECTOR DATA ORIGIN              
       SWI XABX                 
        FCB 4
       STA A FCBDBA,X                 
       STA B 8,X                 
       BRA   INITR3       GO TO WRITE SECTOR #4 - FIRST DIR SECTOR     
           
WRTBL  BRA   WRTBLK       JMP TO OUT OF RANGE "BSR WRTBLK"
           
INITR3 INC   FCBSCT,X SECTOR=4
       CLR   BUFFER+SECSIZ-2
       CLR   BUFFER+SECSIZ-1
           
INITR4 BSR   WRTBLK       INIT DIR TO ZERO: WRT SECTOR
       TST   FCBSTA,X     ERROR? 
       BEQ   INIT4B       BR IF OK
       BRA   INITQ        ERROR
INIT4B LDA A FCBSCT,X     GET CURRENT SECT
       INC A              NEXT SECT
       CMP A #TRKSIZ      END OF TRACK?
       BEQ   INITR5       BR IF YES
       STA A FCBSCT,X
       BRA   INITR4       CONT WRITING
           
INITR5 LDA A #$01         INIT REST OF DISK 
       STA A FCBSCT,X     SECTOR=1
       STA A FCBTRK,X     TRACK=1
       TAB  
INITR6 INC B              MAKE SECTOR LINKAGE
       CMP B #TRKSIZ+1    END OF TRACK?
       BNE   INITR7       NO
       LDA B #$01
       INC A              .*echo trk:a
       CMP A #DSKSIZ+1    END OF DISK?
       BNE   INITR7       NO
       CLR A              LAST SECTOR POINTS TO 0,0
       CLR B
INITR7 STA A BUFFER       TRACK LINK
       PSH B              SAVE LSEC
       BSR   GETSC        GET PSEC 
       STA B BUFFER+1     SECTOR LINK
       PUL B              RESTORE LSEC
       BSR   WRTBLK       WRITE SECTOR
       TST A              DONE?
       BNE   INITR8       NO
       TST B              DONE?
       BNE   INITR8       NO
       RTS                YES! DONE
INITR8 STA A FCBTRK,X     
       PSH B              SAVE LSEC
       BSR   GETSC        GET PSEC
       STA B FCBSCT,X     
       PUL B              GET LSEC
       BRA   INITR6       KEEP WRITING

INITQ  LDX   #QMSG        MSG INITIALIZATION FAILED
       SWI PRTMSG
        FCB 49
       RTS 
QMSG   FCC 'INITIALIZATION FAILED' 
       FCB $0D              

GETSC  SWI PSHX           CONVERT LSEC (LOGICAL SECT) TO PSEC (PHYSICAL SECT)
        FCB 5
       LDX   #TBL         X=PHYSICAL TO LOGICAL TABLE
       SWI ADDBX
        FCB 10
       DEX                SECTOR STARTS AT 1
       LDA B 0,X          GET PSEC
       SWI PULX           RESTORE X-REG
        FCB 6 
       RTS  
           
WRTBLK PSH A              WRITE A SECTOR WITH ERROR CHECKING
       CLR A
       CLR   FCBSTA,X     CLEAR ERROR FLAG
       SWI IOHDR             
        FCB 19
       STA A FCBSTA,X       
       TST A              ERROR?
       BNE   WRTERR       YES
       PUL A
       RTS  
           
WRTERR TAB             
       BSR   OUTHL        CONVERT LEFT DIGIT
       STA A ERTYPE
       TBA  
       BSR   OUTHR        CONVERT RIGHT DIGIT
       STA A ERTYPE+1
       SWI PSHX           SAVE X
        FCB 5
       LDA A FCBSCT,X
       BSR   OUTHL        MAKE SECT NUM HEX
       STA A SECT
       LDA A FCBSCT,X
       BSR   OUTHR
       STA A SECT+1
       LDA A FCBTRK,X
       BSR   OUTHL
       STA A TRACK
       LDA A FCBTRK,X
       BSR   OUTHR
       STA A TRACK+1
       LDX   #DERROR
       SWI PRTMSG
        FCB 49
       SWI WARMSTART
        FCB 31
       RTS  
       
DERROR FCC 'DISK ERROR:'
ERTYPE FCB 0,0
       FCC ' AT SECTOR '
SECT   FCB 0,0
       FCC ' TRACK '
TRACK  FCB 0,0                
       FCB $0D              
           
OUTHL  LSR A           CONVERT BINARY TO HEX-ASCII
       LSR A
       LSR A
       LSR A
OUTHR  AND A #$0F      GET NIBBLE
       ADD A #$30      MAKE ASCII
       CMP A #$39      IS >9?
       BLS   OUTH1     NO
       ADD A #$07      YES
OUTH1  RTS  

*      PHYSICAL TO LOGICAL TABLE

       FCB 0
       
TBL    FCB $01, $06, $0b, $10
       FCB $03, $08, $0d, $12
       FCB $05, $0a, $0f, $02
       FCB $07, $0c, $11, $04
       FCB $09, $0e

* BOOT CODE GOES HERE (MAX 3x128-6 BYTES)
* LOADED AT ADDRESS $2400 BY SWTBUG 'D' COMMAND

BOOT   NOP   
       END
 

