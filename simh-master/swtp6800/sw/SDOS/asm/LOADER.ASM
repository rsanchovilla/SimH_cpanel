;* 
;* Loader for Software Dynamics SDOS 1.1 Operating System
;* I/O Routines for MF68 Floppy Disk
;*
;* (C) Roberto Sancho, Jun/2022
;*

;* Added fixes from MAY/2023 by Mike Douglas to work on real hardware
;* The MF68 does not support dosuble side operations, so it has been made to work
;* on Peripheral Technologies FD-2 Floppy controller (controller chip is WD2797)
;* which supports double side and is compatible with MF68
;*

; this loader is implemented to work with D command from SWTBUG ROM
; D command loads sector zero into addr $2400, and branch to it
; Sector zero of disk is the boot sector of SDOS. It has 16 bytes of prog
; then 16 bytes for disk info, then 32 bytes for disk name (ascii)
; the rest of sector is prog. 

; Comments by Mike Douglas on physical disk format:
; THE SWTBUG D COMMAND READS CONSECUTIVE SECTORS STARTING AT SECTOR 0
; INTO RAM AT $2400 UNTIL AN ERROR OCCURS. THIS LOADER IS 5 SECTORS
; LONG AND IS ON SECTORS 0-4 ON THE DISK. SECTOR 5 DOES NOT EXIST
; AND SECTORS RESUME WITH SECTOR 6. THE SWTBUG D COMMAND STOPS
; READING WHEN IT IS NOT ABLE TO READ SECTOR 5. ONLY TRACK 0
; SIDE 0 IS FORMATTED THIS WAY (0,1,2,3,4,6,7...). ALL OTHER TRACKS
; HAVE SECTORS 1-18.


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

boot   ldaa $2411	; get num of sectors per cluster
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
; (FD-2 CONTROLLER)

; INPUT PARAMS

DRV    fcb 0         ; DRIVE TO OPERATE ON
TRK    fcb 0         ; TRACK TO READ/WRITE
SECT   fcb 0         ; SECTOR TO READ/WRITE
SIDE   fcb 0         ; SIDE TO READ/WRITE
DDATA  fdb 0         ; POINTER TO DATA BUFFER (MUST HAVE 128 BYTES)

; COMMANDS FOR WD1771 CONTROLLER CHIP
;
FDRSC  EQU $08       ; RESTORE
FDSKI  EQU $18       ; SEEK
FDRDC  EQU $88       ; READ A SECTOR
FDWRC  EQU $A8       ; WRITE A SECTOR

; ADDR OF WD1771 CONTROLLER CHIP REGISTERS 

DRVREG EQU $8014
CMDREG EQU $8018
TRKREG EQU $8019
SECREG EQU $801A
DATREG EQU $801B

; DRIVER VARS

RCNT   FCB  0         ; RETRY COUNT

; READ A SECTOR
;
RDSEC  JSR SELDRV     ; SELECT DRIVE AND SIDE
RDSEC1 JSR READ       ; READ A SECTOR
       BEQ RDEXIT     ; OK
       DEC RCNT       ; RETRY AGAIN?
       BNE RDSEC1     ; YES
       TSTA           ; A=ERROR STATUS
RDEXIT RTS

; READ A SECTOR (A=TRK, B=SECTOR)
;
READ   JSR SEEK
       LDAA #FDRDC    ; READ COMMAND
       STAA CMDREG     
       JSR DEL56U     ; DELAY
       LDX DDATA      ; GET BUFFER ADDRESS

READ1  LDAA CMDREG     
       BITA #$02      ; DATA REG FULL?
       BNE READ2      ; YES
       BITA #1        ; BUSY?
       BNE READ1      ; LOOP IF BUSY, ELSE ERROR

       ANDA #$1C      ; MASK OFF STATUS BITS
       RTS             

READ2  LDAA DATREG    ; GET A BYTE
       STAA 0,X       ; STORE IN BUFFER
       INX             
       BNE READ1      ; LOOP TO READ NEXT BYTE
                       
; SEEK A=TRACK, B=SECTOR

SEEK   STAB SECREG    ; SET SECTOR
       CMPA TRKREG    ; ON TRACK?
       BEQ CMDEX      ; YES

       STAA DATREG    ; NO, STORE TRACK#
       LDAA #FDSKI    ; SEEK COMMAND
DOCMD  STAA CMDREG
       JSR DEL56U

WBUSY  LDAA CMDREG
       BITA #1
       BNE WBUSY
CMDEX  RTS

; DEL56U- DELAY 56us INCLUDING THE CALL TO DELAY56U

DEL56U JSR DELAY2
DELAY2 JSR DELAY3
DELAY3 RTS

; RESTORE SEEK TRACK0

RESTOR BSR SELDRV     ; SELECT THE DRIVE
       LDAA #FDRSC    ; RESTORE COMMAND
       BRA DOCMD      ; EXEC DISK CONTROLLER CMD

; SELECT THE DRIVE
; RETURN A=TRACK, B=SECTOR

SELDRV LDAA DRV       ; GET DRIVE 
       TST  SIDE
       BEQ  DOSEL
       ORAA #$40
DOSEL  STAA DRVREG    ; SELECT THE DRIVR

       LDAA #5        ; SET RETRY COUNT
       STAA RCNT       
       LDAA TRK       ; GET TRACK#        
       LDAB SECT      ; GET SECTOR#
       RTS            

LoaderLen equ	*-LoaderStart+1
	
       org $6000	; this is the entry point when LOADER is bring to ram as S19 file using SWTBUG "L" command
			; in this case, the program is loaded at its assembled address
			; track zero on disk has sectors numbered a 1,2,3,... (the expected SDOS numbering)
       
boot2  ldaa #1
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



