
* 
* Loader for Smoke System Broadcasting DOS-68 Operating System
* I/O Routines for MF68 Floppy Disk
* 
* This program patches SSD DOS-68 to use MF68 instead of 
* own SSB floppy disk
*
* C(C) Roberto Sancho, Apr/2022
* Uses BC00-BD00 for loader. Once DOS is started this mem is available for any program
*      BE00-BEFF for MF60 I/O Routines.  This mem should not be overwritten/modified
*

SECBUF  EQU $7F80       BUFFER FOR READ DISK SECTOR 
MON     EQU $E0E3       SWTBUG Monitor start 

        ORG $BC00

START   JSR   INTDK  
        LDA A #$FF      Init PBUF     
        STA A PBUF   
        LDX   #$E0E3    Init jump to SWTBUG monitor  
        STX   TA        as default TA (so if loaded prog has no TA, return to monitor)
        LDA A #08
        STA A BFDDRG    Select drive 0 
        LDX   #$8051    Track and sector for DOS68 file (main OS program)
        STX   SECBUF    Set FWD Link Trk and Sect in sector buffer
        LDX   #SECBUF
        STX   BFDSBU    Set addr where disc sector is to be read (=the sector buffer)
RDNEXT  BSR   RDBYTE    read a byte from disk file in B
        CMP B #$42      is data record?
        BEQ   RDDATR    yes -> branch
        CMP B #$47      is transfer address record?
        BNE   RDNEXT    no -> skip this byte and read next from disk 
        BSR   RDBYTE    yes -> get transfer addr (TA) hi
        STA B TA
        BSR   RDBYTE    get transfer addr lo
        STA B TA+1      save TA address 
        BRA   RDNEXT

* READ A DATA RECORD INTO MEM

RDDATR  CLR A
        BSR   RDBYTE    B=Data record last byte used (=len-1)
        INC B           B=Data Record len
        PSH B
        BSR   RDBYTE    B=Data load addr hi
        STA B PSTORE
        BSR   RDBYTE    B=Data load addr lo
        STA B PSTORE+1
RDDATB  BSR   RDBYTE    get record data byte
        LDX   PSTORE    load address for byte 
        STA B 0,X       save data byte at load addr
        INX 
        STX   PSTORE
        PUL B           B=Bytes remaining to load
        DEC B 
        PSH B
        BNE   RDDATB    continue until all bytes are load
        PUL B
        BSR   RDBYTE    get chksum byte  
        INC A
        BEQ   RDNEXT
        JMP   MON       CHKSUM Error -> return to monitor

* READ BYTE FROM DISK FILE, FOLLOWING FWD TRK AND SEC LINKS
* RETRUN B=BYTE READ, A=CHKSUM 

RDBYTE  LDX   PBUF      pointer to next byte to get from disk sector buffer
        BMI   RDTKSEC   128 bytes already read -> load next sector following FWD LINK
        LDA B 0,X       read byte from secotr buffer
        INX 
        STX   PBUF
        ABA             A=A+B=checksum
        RTS 

RDTKSEC LDX   SECBUF    get FWD Trk and Sect to load
        BEQ   JMPTA     If no FWD link, jumo to Transfer Address
        STX   BFDTRG    TrSect to load
        LDX   #SECBUF+4 init sector buffer data address
        STX   PBUF
        PSH A
        JSR   BFDRDS    read sector into 7f80-7fff (SECBUF)
        PUL A
        BEQ   RDBYTE
        JMP   MON       return to monitor if read error

JMPTA   JSR  PATCHD     Patch the loaded system before jumping to it
        LDX  TA         Jump to transfer address to start 
        JMP  0,X        loaded program 

* PATCH DOS68 TO USE MF68 I/O ROUTINES (AT ADDRESS BEXX) INSTEAD OF 
* CALLING SSB FLOPPY ROM (AT ADDRESS 80XX)

PATCHD  CLR A

* PATCH CALL INIT ROUTINE AT NEW ADDRESS
* DDE4  7E 8026         JMP  $8026    SSB INITIALIZE PIA ROUTINE
*                   TO  JMP  $BE26    MF68 INIT
        LDA A #$BE
        STA A $DDE5
        
* PATCH CALL ROUTINES AT NEW ADDRESS
* DD47  BD 8038         JSR  $8038    SSB RESTORE
* DDA0  BD 8038         JSR  $8038    SSB RESTORE
* DD30  CE 802C         LDX  #$802C   SSB WRITE SECTOR ROUTINE
* DD35  CE 8029         LDX  #$8029   SSB READ SECTOR ROUTINE 
* DD88  8C 8029         CPX  #$8029   SSB READ SECTOR ROUTINE 
*                    TO JSR  $BEXX    MF68 
        LDA A #$BE
        STA A $DD48
        STA A $DDA1
        STA A $DD31
        STA A $DD36
        STA A $DD89

* Patch the whole routine
* DD51:	BD 806C         JSR  $806C     SSB Write track register
* DDB7:	BD 8072         JSR  $8072     SSB Read track register
*                   ->  JMP  $BE38    

        LDX   #$BE38
        STX   $DD52
        STX   $DDB8
        
        RTS
        
PBUF    RMB 2           POINTER TO NEXT BYTE TO GET FROM READ DISK SECTOR BUFFER
TA      RMB 2           TRANSFER ADDRESS (=COLD START ADDR OF OPERATING SYSTEM)
PSTORE  RMB 2           POINTER WHERE THE BYTE READ FROM DISK IS TO BE STORED IN MEM
      
        
*
* SSB Smoke Signal Broadcasting BFD-68 disk drive ROM replacement.
* Allows to use MF68 TSC Disk Drive
*

BFDDRG EQU $A07B  Drive Register  (8-> select drive 0, $10 drive 1, $20 drive 2)
BFDTRG EQU $A07C  Track Register  ($80..$A2)
BFDSRG EQU $A07D  Sector Register ($40..$53)
BFDSBU EQU $A07E  Sector Buffer Pointer

*
* The following is the original entry for SSB ROM
* as listed in 68'Micro Journal, Volumne 1 issue 1, feb 1979, p13-20
* 
* BFDCLD EQU $8020  SSB Cold Start
* BFDWRM EQU $8023  SSB Warm Start
* BFDINT EQU $8026  SSB Initialize PIA routine
* BFDRDS EQU $8029  SSB Read Sector routine 
* BFDWRS EQU $802C  SSB Write Sector routine
* BFDRDT EQU $802F  SSB Read Track routine
* BFDWRT EQU $8032  SSB Write Track routine
* BFDSEK EQU $8035  SSB Seek routine
* BFDRST EQU $8038  SSB Restore routine  
*
*            $8060      Send command
*            $8063      Operation complete
*            $8066      Clean up & return
*            $8069      Write sector register
*            $806C      Write track register
*            $806F      Write track number
*            $8072      Read track register
*            $8075      Step in
*            $8078      Step out
*            $807B      Step
*

*
* THE MEM AREA BE00-BFFF HOLDS THE SSB BFD-68 ROM REPLACEMENT
* SSB ROM ENTRY POINTS STARTS AT $8020
* REPLACEMENT ENTRY POINTS STARTS AT $BE20

       ORG $BE00

CTRK0  RMB 4  TRACK STATE TABLE
RCNT   RMB 1  RETRY COUNT

       ORG $BE20

BFDCLD JMP MON    SSB Cold Start
BFDWRM JMP MON    SSB Warm Start
BFDINT JMP INTDK  SSB Initialize 
BFDRDS JMP RDSEC  SSB Read Sector routine  
BFDWRS JMP WRSEC  SSB Write Sector routine
BFDRDT JMP MON    SSB Read Track routine
BFDWRT JMP MON    SSB Write Track routine
BFDSEK JMP MON    SSB Seek routine
BFDRST JMP DUMMY  SSB Restore routine (Dummy)

*
* DISK DRIVERS FOR SOUTHWEST TECHNICAL PRODUCTOS
*
* COMMANDS
*
FDRSC  EQU $0B    RESTORE
FDSKI  EQU $1B    SEEK
FDRDC  EQU $8C    READ A SECTOR
FDWRC  EQU $AC    WRITE A SECTOR
*
DRVREG EQU $8014
CMDREG EQU $8018
TRKREG EQU $8019
SECREG EQU $801A
DATREG EQU $801B
*
* FCB DEFINITIONS:
*
FCBSTA EQU 5  STATUS 
FCBDBA EQU 7  DATA BUFFER ADDRESS
FCBDRV EQU 9  DRIVE#
FCBTRK EQU 10 TRACK# 
FCBSCT EQU 11 SECTOR# 
*
* INIT THE DISK SYSTEM
*
INTDK  LDA A #$FF
       STA A CTRK0       
       STA A CTRK0+1
       STA A CTRK0+2       
       STA A CTRK0+3
DUMMY  RTS       

*
* READ A SECTOR
*
* drive in $A07B: $08=drive 0, $10=drive 1, $20=drive 2
* track in $A07C ($80..$A2)
* sector in $A07D ($40..$51)
* Sector Buffer in $A07E/F
* return A=0 if ok, <>0 if error
*
RDSEC  JSR SELDRV 
RDSEC1 JSR READ       GET A SECTOR
       BEQ QUIT10     OK
       DEC RCNT       RETRY AGAIN?
       BNE RDSEC1     YES
       BRA QERR
*
* WRITE A SECTOR
*
* drive in $A07B: $08=drive 0, $10=drive 1, $20=drive 2
* track in $A07C ($80..$A2)
* sector in $A07D ($40..$51)
* Sector Buffer in $A07E/F
* return A=0 if ok, <>0 if error
*
WRSEC  JSR SELDRV 
WTSEC1 JSR WRITE
       BEQ QUIT10     OK
       DEC RCNT       TRY AGAIN?
       BNE WTSEC1     YES
QERR   TST A          A HAS THE STATUS BITS WIITH ERROR 
QUIT10 RTS
*
* READ A SECTOR (A=TRK, B=SECTOR, X=CTRKx)
*
READ   JSR SEEK
       LDA A #FDRDC   READ
       STA A CMDREG
       JSR DEL30U     DELAY
       LDX BFDSBU     GET BUFFER ADDRESS
       LDA B #128     128 BYTES/SECTOR     
READ1  LDA A CMDREG
       BIT A #$02     DATA REG FULL?
       BNE READ2      YES
       BIT A #1       BUSY?
       BNE READ1      YES
       BRA READ3      ERROR
READ2  LDA A DATREG   GET A BYTE
       STA A 0,X      STORE IN BUFFER
       INX
       DEC B
       BNE READ1      DO AGAIN
       BSR WBUSY      WAIT TILL DONE
READ3  AND A #$1C     MASK OF STATUS BITS
       RTS
*
WBUSY  LDA A CMDREG
       BIT A #1       BUSY?
       BNE WBUSY      YES
       RTS
*
* WRITE A SECTOR (A=TRK, B=SECTOR, X=CTRKx)
*
WRITE  JSR SEEK
       LDA A #FDWRC   WRITE COMMAND
       STA A CMDREG
       JSR DEL30U
       LDX BFDSBU     GET BUFFER ADDRESS
       LDA B #128     128 BYTES/SECTOR
WRITE1 LDA A CMDREG
       BIT A #2       REG EMPTY?
       BNE WRITE2     YES
       BIT A #1       BUSY?
       BNE WRITE1     YES
       BRA QERR       ERROR
WRITE2 LDA A 0,X      GET A BYTE
       STA A DATREG
       INX
       DEC B
       BNE WRITE1     DO AGAIN
       JSR WBUSY      WAIT FOR BUSY
WRITE3 AND A #$5C     MASK OFF STATUS BITS
       RTS
*
* SEEK A=TRACK, B=SECTOR, X=CTRKx
*
SEEK   CMP A TRKREG   ON TRACK?
       BEQ SEEK2      YES
       STA A DATREG   NO, STORE TRACK#
       JSR DEL30U
       STA A 0,X      SAVE CURRENT TRACK OF DRIVE
       LDA A #FDSKI   SEEK COMMAND
       STA A CMDREG
       JSR DEL30U
       JSR WBUSY      WAIT FOR BUSY
SEEK2  STA B SECREG   SET SECTOR
       JSR DEL30U
       RTS
*
* DELAY 30 USECS
*
DEL30U INX
       DEX
       INX
       DEX
       INX
       DEX
       INX
       DEX
       RTS
*
* DRIVE SELECT (A=DRIVE#, return X=CTRKx)
*
DRIVE  STA A DRVREG   INIT REGISTER
       JSR DEL30U
       LDX #CTRK0     POINT TO TABLE
       TAB
       BEQ DSEL3
DSEL2  INX
       DEC B
       BNE DSEL2
DSEL3  RTS
*
* RESTORE SEEK TRACK0 (X=CTRKx)
*
RESTOR LDA A #FDRSC   RESTORE COMMAND
       STA A CMDREG
       JSR DEL30U
       JSR WBUSY
       CLR 0,X        CTRKx:=00
       RTS
*
* SELECT DRIVE, HANDLE SSB STYLE PARAMS
* SELECT THE DRIVE
* RETURN A=TRACK, B=SECTOR, X=CTRKx
*
SELDRV LDA A BFDDRG   SSB SELECTED DRIVE 
       ASR A
       ASR A
       ASR A
       ASR A
       AND A #$03
       BSR DRIVE      SELECT DRIVE, SET X=CTRKx
       LDA B 0,X      A=CURRENT DRIVE TRACK
       CMP B #$FF     INITIALIZED?
       BNE SELD1      YES
       JSR RESTOR     SEEK TRACK0
SELD1  LDA A #5       SET RETRY COUNT
       STA A RCNT
       LDA A BFDTRG   GET SSB TRACK#        
       SUB A #$80     MF68 FLEX1 DISK FORMAT HAS TRACKS RANGING FROM 0..34
       LDA B BFDSRG   GET SECTOR 
       SUB B #$3F     MF68 FLEX1 DISK FORMAT HAS SECTORS RANGING FROM 1..18
       RTS
*
       END



