;* 
;* Loader for Software Dynamics SDOS 1.1 Operating System
;* I/O Routines for MF68 Floppy Disk
;*
;* (C) Roberto Sancho, Jun/2022
;*

; this loader is implemented to work with D command from SWTBUG ROM
; D command loads sector zero into addr $2400, and branch to it
; Sector zero of disk is the boot sector of SDOS. It has 16 bytes of prog
; then 16 bytes for disk info, then 32 bytes for disk name (ascii)
; the rest of sector is prog. 


       org  $2400

       ldx  #base+5		; 8 bytes to setup and branch
       stx  $00		
       jmp  st0
       
       fcc  "1234----"		;  8 bytes	This space is reserved. will be overwritten
       fcc  "1234----5678----"	; 16 bytes	with boot info. It is important that this
       fcc  "1234----5678----"	; 16 bytes      occupies exactly 8+8+16*3=64 bytes. No one more
       fcc  "1234----5678----"	; 16 bytes      no one less


       ; the boot prog continues here
	
st0    ldx  $00			; this prog places the loaded (starting at LoaderStart)
       cpx  #base+5+LoaderLen	; into its execution address 
       bne  st1			; and the branch to it
       jmp  LoaderStart
st1    ldaa 0,x       
       inx
       stx $00
       ldx  base+1     ; dest
       staa 0,x
       inx
       stx  base+1     
       bra st0

base   equ  *

       org  $5000
buf    equ  $5800	

LoaderStart  equ *

			; at last, we are at loader on the correct execution address. Go on ...
			; this is the entry point when called from SWTBUG "D" command
			; in this case, all the program is loaded at $2400 and onwards
			; track zero on disk has sectors numbered a 0,1, 3,... (weird, isn't it?)

boot   JSR INTDK	; INIT THE DISK SYSTEM disk vars
       ldaa $2411	; get num of sectors per cluster
       inca             ; after the incr, a=first sector of Cluster 1
       staa sect	; the first sector to read (is the first sector of sdos.sys)      
       ldx #buf+128
       stx ddata
       clr goflag
       
rdty   tst  goflag
       bne  go
       jsr  rdbyte	; get load record type
       cmpa #0		; .*echo type:a
       beq  rdty0	; br if type 0 (fill record)
       cmpa #1
       bne rd0b
rdty1  jsr  rdword	; type 1 record: skip 4 bytes
       jsr  rdword	
       bra rdty
rdty0  jsr  rdword	; type 0 record (fill record). get length
       stx  plen	; .*echo ..skip:x
rd0a   ldx  plen
       beq  rdty	; br if record done, to read next one
       dex
       stx  plen
       jsr  rdbyte	; read byte and discard it
       bra  rd0a       
rd0b   cmpa #2
       beq  rdty2	; br if type 2 (load record)
       cmpa #3
       beq  rdty3	; br if type 3 (last load record)
go     jmp  $2000	; start sdos.sys execution .*echo Start
rdty3  inc  goflag       
rdty2  jsr  rdword	; type 2 start addr
       stx  pdata	; .*echo ..addr:x
       jsr  rdword	; type 2 length
       stx  plen	; .*echo ..len:x
rd2    ldx  plen	; copy type 2 record data
       beq  rdty	; br if record done, to read next one
       dex
       stx  plen
       jsr  rdbyte
       ldx  pdata
       staa  0,x	; .*echo ......addr:x
       inx		; .*echo .........data:a
       stx  pdata
       bra  rd2

rdword jsr  rdbyte   	
       staa $00	
       jsr  rdbyte
       staa $01
       ldx $00
       rts

rdbyte  ldx ddata	
	cpx #buf+128
        bne rdbyte1
	bsr rdsk	; read disk on disk buf empty
	ldx #buf	; point again to start of buf
	stx ddata
rdbyte1	ldaa 0,x
	inx
	stx ddata
        rts		

rdsk    ldx #buf	
        stx ddata
        jsr rdsec	; **troff
	ldab sect	; **tron
	cmpb #18	; if just read sector 18 (last on on tracks)
        bne rdsk1	; then incr track number, reset sector num to 1
	tst side
	bne rdsk0a
	inc side
	bra rdsk0b
rdsk0a  clr side
        inc trk
rdsk0b  clr sect
rdsk1   inc sect
        rts	

PDATA   fdb 0        
PLEN    fdb 0         
GOFLAG  fcb 0

; DISK DRIVERS FOR SOUTHWEST TECHNICAL PRODUCTS MF-68

; INPUT PARAMS

DRV    fcb 0         ; DRIVE TO OPERATE ON
TRK    fcb 0         ; TRACK TO READ/WRITE
SECT   fcb 0         ; SECTOR TO READ/WRITE
SIDE   fcb 0         ; SIDE TO READ/WRITE
DDATA  fdb 0         ; POINTER TO DATA BUFFER (MUST HAVE 128 BYTES)

; COMMANDS FOR WD1771 CONTROLLER CHIP
;
FDRSC  EQU $0B       ; RESTORE
FDSKI  EQU $1B       ; SEEK
FDRDC  EQU $8C       ; READ A SECTOR
FDWRC  EQU $AC       ; WRITE A SECTOR

; ADDR OF WD1771 CONTROLLER CHIP REGISTERS 

DRVREG EQU $8014
CMDREG EQU $8018
TRKREG EQU $8019
SECREG EQU $801A
DATREG EQU $801B

; DRIVER VARS

CURDRV FCB $FF       ; PRESENTLY SELECTED DRIVE #
CTRK0  fcb 0,0,0,0   ; TRACK STATE TABLE
RCNT   fcb 0         ; RETRY COUNT
SIDEBT fcb 0         ; SIDE SELECTION BIT

; INIT THE DISK SYSTEM

INTDK  CLR CURDRV    ; ASSUME DRIVE 0 BOOTED
       LDAA #$FF
       STAA CTRK0       
       STAA CTRK0+1
       STAA CTRK0+2       
       STAA CTRK0+3
DUMMY  RTS       

; READ A SECTOR
;
RDSEC  JSR SELDRV 
RDSEC1 JSR READ       ; GETA SECTOR
       BEQ QUIT10     ; OK
       DEC RCNT       ; RETRY AGAIN?
       BNE RDSEC1     ; YES
       BRA QERR

; WRITE A SECTOR
;
WRSEC  JSR SELDRV 
WTSEC1 JSR WRITE
       BEQ QUIT10     ; OK
       DEC RCNT       ; TRY AGAIN?
       BNE WTSEC1     ; YES
QERR   TSTA           ; A HAS THE STATUS BITS WITH ERROR 
QUIT10 RTS

; READ A SECTOR (A=TRK, B=SECTOR, X=CTRKx)
;
READ   JSR SEEK
       LDAA #FDRDC    ; READ
       ADDA SIDEBT
       STAA CMDREG     
       JSR DEL56U     ; DELAY
       LDX DDATA      ; GET BUFFER ADDRESS
       LDAB #128      ; 128 BYTES/SECTOR     
READ1  LDAA CMDREG     
       BITA #$02      ; DATA REG FULL?
       BNE READ2      ; YES
       BITA #1        ; BUSY?
       BNE READ1      ; YES
       BRA READ3      ; ERROR
READ2  LDAA DATREG    ; GET A BYTE
       STAA 0,X       ; STORE IN BUFFER
       INX             
       DECB            
       BNE READ1      ; DO AGAIN
       BSR WBUSY      ; WAIT TILL DONE
READ3  ANDA #$1C      ; MASK OF STATUS BITS
       RTS             
                       
WBUSY  LDAA CMDREG     
       BITA #1        ; BUSY?
       BNE WBUSY      ; YES
       RTS

; WRITE A SECTOR (A=TRK, B=SECTOR, X=CTRKx)

WRITE  JSR SEEK
       LDAA #FDWRC    ; WRITE COMMAND
       ADDA SIDEBT
       STAA CMDREG     
       JSR DEL56U      
       LDX DDATA      ; GET BUFFER ADDRESS
       LDAB #128      ; 128 BYTES/SECTOR
WRITE1 LDAA CMDREG     
       BITA #2        ; REG EMPTY?
       BNE WRITE2     ; YES
       BITA #1        ; BUSY?
       BNE WRITE1     ; YES
       BRA QERR       ; ERROR
WRITE2 LDAA 0,X       ; GET A BYTE
       STAA DATREG     
       INX             
       DECB            
       BNE WRITE1     ; DO AGAIN
       JSR WBUSY      ; WAIT FOR BUSY
WRITE3 ANDA #$7C      ; MASK OFF STATUS BITS
       RTS

; SEEK A=TRACK, B=SECTOR, X=CTRKx

SEEK   CMPA TRKREG    ; ON TRACK?
       BEQ SEEK2      ; YES
       STAA DATREG    ; NO, STORE TRACK#
       JSR DEL56U      
       STAA 0,X       ; SAVE CURRENT TRACK OF DRIVE
       LDAA #FDSKI    ; SEEK COMMAND
       STAA CMDREG     
       JSR DEL56U      
       JSR WBUSY      ; WAIT FOR BUSY
SEEK2  STAB SECREG    ; SET SECTOR
       JSR DEL56U
       LDAA SIDE
       BEQ SEEK3
       LDAA #$08      ; SELECT SIDE 1
SEEK3  STAA SIDEBT       
       RTS

; DEL56U- DELAY 56us INCLUDING THE CALL TO DELAY56U

DEL56U JSR DELAY2
DELAY2 JSR DELAY3
DELAY3 RTS

; FIND TRACK- RETURN X->TRACK TABLE FOR CURDRV

FNDTRK LDX #CTRK0     ; POINT TO TABLE
       LDAA CURDRV
       BEQ FNDTK3
FNDTK2 INX
       DECA
       BNE FNDTK2
FNDTK3 RTS

; RESTORE SEEK TRACK0

RESTOR LDAA #FDRSC    ; RESTORE COMMAND
       STAA CMDREG
       JSR DEL56U
       JSR WBUSY
       RTS

; SELECT THE DRIVE
; RETURN A=TRACK, B=SECTOR, X=CTRKx

SELDRV BSR FNDTRK     ; SAVE TRACK FOR CURRENT DRIVE
       LDAA TRKREG     
       STAA 0,X        
       LDAA DRV       ; GET DRIVE 
       STAA CURDRV     
       STAA DRVREG    ; SELECT THE DRIVE
       BSR FNDTRK     ; LOAD TRACK FOR NEW DRIVE
       LDAA 0,X       ; A=CURRENT DRIVE TRACK
       STAA TRKREG     
       CMPA #$FF      ; INITIALIZED?
       BNE SELD1      ; YES
       BSR RESTOR     ; SEEK TRACK0
SELD1  LDAA #5        ; SET RETRY COUNT
       STAA RCNT       
       LDAA TRK       ; GET TRACK#        
       LDAB SECT      ; GET SECTOR#
       RTS            

LoaderLen equ	*-LoaderStart+1
	
       org $6000	; this is the entry point when LOADER is bring to ram as S19 file using SWTBUG "L" command
			; in this case, the program is loaded at its assembled address
			; track zero on disk has sectors numbered a 1,2,3,... (the expected SDOS numbering)
       
boot2  JSR INTDK	
       ldaa #1
       staa sect
       clr  side
       clr  trk
       jsr  rdsk
       ldaa buf+$11	; this is the number of sectors pre cluster
       staa $2411
       jmp  boot

       ; to start the LOADER with SWTBUG "G" command
       ORG $A048
       FDB boot2       

       END



