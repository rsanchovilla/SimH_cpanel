*        SDOSCMDS11K.ASM -- ASSEMBLY LANGUAGE COMPONENT OF SDOSCMDS.BAS
*
*        7/9/82  Added assembly language subroutine to scan over empty
*                directory entries
*        5/15/82 FIXED "DOESN'T GET RIGHT FILE SIZE WHEN CONCATENATING 2 FILES"
*                      (BUG IN COPYF)
*                FIXED DEBUG JUST DIES...
*        3/23/82 CONVERTED TO 680C
*                FIXED "DOESN'T COPY LAST BYTE ON DISK-TO-DISK COPY IF ZERO"
*        7/21/80 MOD TO RESET CARRY ON SUBROUTINE RETURNS
*        2/28/84 FIXED APPENDING BUG WHERE A BYTE PER FILE -1 WAS BEING
*                LOST, DUE TO SPARSE POSITION/WRITE AT END
*        3/2/84 REVISED CRC CODE FOR SPEED


         IFUND LISTASM
         ELSE
         LIST LISTASM
         FIN
*

         IFUND M6800
         IFUND M6801
         IFUND M6809
M6800    EQU   1
M6801    EQU   0
M6809    EQU   0
         FIN
         FIN
         FIN

TEMPA    EQU   0                       SCRATCHPAD LOCATIONS
TEMPB    EQU   1
TEMPX    EQU   0

SYSCALL  EQU   $FB

SYSCALL:LOAD       EQU   $5
SYSCALL:READB      EQU   $B
SYSCALL:WRITEB     EQU   $D
SYSCALL:CONTROL    EQU   $E
SYSCALL:ERROREXIT  EQU   $12
SYSCALL:DEBUG      EQU   $18
CC:POSITION        EQU   0             CAUSES FILE POSITIONING
CC:DUMPBUFFERS     EQU   1             USED TO DUMP BUFFER POOL TO DISK

SCBLK:RPLEN     EQU     8
SCBLK:RDBUF     EQU     $A
SCBLK:RDLEN     EQU     $C
DIR:HCSIC          EQU   $12
DIR:NCLUSTERS      EQU   $13
ERR:BADARGUMENT    EQU   20            BAD ARGUMENT TO SIN/COS/ATN/TAN ((OR CRC!!!))
ERR:WRONGARGCOUNT  EQU   27
ERR:NOTENUFSPACE   EQU   105
ERR:EOFHIT         EQU   $3E9
ERR:ILLDEVICEOP    EQU   $40A

STRINGHEAD:SIZE    EQU   4             SIZE OF STRING HEAD

IOERROR            EQU   $103          I/O ERROR ENTRY POINT INTO RTP

         IFUND STACKSLOP
STACKSLOP          EQU   256           SPACE NEEDED FOR STACK
         FIN

SYSCALL0 JSR   SYSCALL
         BCS   SYSCALL1                B/ OOPS...
         RTS   ALL OK, BYE!

SYSCALL1 TXD                           COPY ERROR CODE TO (D)
         JMP   IOERROR
         PAGE
* THIS IS THE CODE THAT IS RELOCATED TO PAGE ZERO TO IMPLEMENT "DEBUG" COMMAND
* NOTE! IF THE PROGRAM TO BE DEBUGGED STEPS ON THIS CODE,
* THEN THE "DEBUG" COMMAND WILL SIMPLY BLOW UP WHEN INVOKED
:DEBUGTARGET   EQU $B0                 = WHERE TO RELOCATE THIS CODE TO

:DEBUG   EQU   *                       BEGIN BLOCK OF CODE TO BE RELOCATED
:ERR     STX   :ERCODE                 ERROR WHILE LOADING...
         LDX   #:EREXIT                SO DO AN ERROR EXIT TO RECOVER
:DOLOAD  EQU   *+:DEBUGTARGET-:DEBUG
         BSR   SYSCALL-:DEBUGTARGET+:DEBUG DO A SYSTEM CALL (1ST TIME DOES "LOAD")
         BCS   :ERR                    B/ GOT ERROR WHILE LOADING
         LDD   :STARTADDRESS           USE START ADDRESS AS "DEBUG" PC
         PSHD
         LDX   #:CDEBUG                NOW DO A "DEBUG" SYSTEM CALL...
         BRA   SYSCALL-:DEBUGTARGET+:DEBUG TO WAKE DEBUGGER (NOTE RETURN ADDRESS!)

:LOAD    EQU   *+:DEBUGTARGET-:DEBUG
         FCB   SYSCALL:LOAD            SYSCALL BLOCK TO LOAD A FILE
         FCB   $E
         FDB   0                       PARAM BYTES IGNORED
:FNAME   FDB   0                       POINTER TO NAME OF FILE TO BE LOADED
:LOADL   FDB   0                       = SIZE OF FILE NAME GIVEN
         FDB   0                       EXPECTED REPLY LENGTH = 4
         FDB   :LOADREPLY              REPLY BUFFER
         FDB   4

:LOADREPLY EQU *+:DEBUGTARGET-:DEBUG
         FDB   0                       HOLDS REPLY FROM "LOAD" SYSCALL
:STARTADDRESS EQU *+:DEBUGTARGET-:DEBUG
         FDB   0

:CDEBUG  EQU   *+:DEBUGTARGET-:DEBUG
         FCB   SYSCALL:DEBUG           SYSCALL BLOCK TO INVOKE LOCAL DEBUGGER
         FCB   2

:EREXIT  EQU   *+:DEBUGTARGET-:DEBUG
         FCB   SYSCALL:ERROREXIT       SYSCALL BLOCK TO CAUSE ERROR EXIT
         FCB   4
:ERCODE  EQU   *+:DEBUGTARGET-:DEBUG
         FDB   0                       ERROR CODE TO BE DISPLAYED
:DEBUGSIZE EQU *-:DEBUG                = SIZE OF CODE TO RELOCATED

         IF :DEBUGTARGET+:DEBUGSIZE>>$F0
         ??MUST LOWER :DEBUGTARGET??
         FIN
         PAGE
*        LOAD AND DEBUG
*        LOADS A PROGRAM INTO CORE AND CALLS IDB
*        IMPLEMENTS "DEBUG" COMMAND
:SRC     EQU   0                       SOURCE POINTER FOR BLOCK MOVE
:DEST    EQU   2                       DESTINATION POINTER FOR BLOCK MOVE

LOADD    JSR   GETSTRING               GET STRING SIZE TO (D), ADDRESS TO (X)
         STD   :LOADL                  SAVE ACTUAL STRING SIZE
         STX   :FNAME
         LDX   #:DEBUGTARGET           = WHERE TO MOVE "LOAD&DEBUG" CODE TO
         STX   :DEST
         LDAA  #:DEBUGSIZE             = # OF BYTES TO MOVE TO PAGE ZERO
         LDX   #:DEBUG
:LOOP    LDB   ,X+                     COPY A BYTE TO PAGE ZERO
         STX   :SRC                    SOURCE
         LDX   :DEST
         STB   ,X+
         STX   :DEST
         LDX   :SRC
         DECA
         BNE   :LOOP
         LDX   SYSCALL+1               START THE GUY OUT WITH A FRESH STACK
         TXS
         LDX   #:LOAD                  GET ADDRESS OF 1ST SYSTEM CALL TO INVOKE
         JMP   :DOLOAD                 JUMP TO RELOCATED ROUTINE

GETSTRING ; GET ADDRESS OF STRING BODY INTO (X), LENGTH INTO (D)
;        (X) ON ENTRY POINTS TO STRING DESCRIPTOR (SEE BASIC MANUAL)
         LDD  4,X                      GET LENGTH OF STRING
         LDX   2,X                     GET ADDRESS OF STRING
         CMPD  #$FFFF                  = "THE ENTIRE STRING" ?
         BNE   GETSTRING1              B/ NO, (D) CONTAINS COUNT
         LDD   2,X                     YES, USE CURLEN OF STRING AS COUNT
GETSTRING1
         LEAX  STRINGHEAD:SIZE,X       MAKE POINTER TO 1ST BYTE OF STRING
         RTS
         PAGE  COUNT BYTE -- CONVERTS COMPLEMENTED BASE ONE BYTE TO BASE TWO
*        COUNT BITS IN A BYTE
*
CNTBYTE  LDX   2,X                     PICK UP POINTER TO BYTE
         LDAA  STRINGHEAD:SIZE,X       PICK UP BYTE
         COMA                          CONVERT TO POSITIVE LOGIC
         CLRB                          ZERO SUM OF BITS
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         RORA                          ADD NEXT BIT TO SUM
         ADCB  #0
         STAB  STRINGHEAD:SIZE,X
         RTS
         PAGE  Fast Directory Search
*        FindBusyDirectoryEntry -- Finds next busy Directory Entry
*        Causes error trap 1001 (EndOfFile) if no more
*        Otherwise, reads 32 byte directory entry and verifies validity
*        "DIRECTORY.SYS" must be open on channel 2
*
FindBusyDirectoryEntry
         jsr   GetString               Get address and length of string
         stx   Read+scblk:rdbuf        set target of read binary syscall
         std   Read+scblk:rdlen        how much to read
FindBusyDirectoryEntryLoop
         ldx   #read                   read next directory entry
         jsr   syscall0                and error back to caller if EOF
         ldx   Read+scblk:rdbuf        is directory entry valid ?
         lda   dir:hcsic,x             (DIR:HCSIC <>0?)
         beq   FindBusyDirectoryEntryLoop b/ no, read to obtain another
         ldx   dir:nclusters,x         is allocated cluster count zero ?
         beq   FindBusyDirectoryEntryLoop b/ yes, ignore this directory entry
         rts                           assert: carry is reset
         PAGE  COMPUTE CRC FOR STRING OF BYTES FROM BASIC PROGRAM
*        BUILDS 512 BYTE CRC TABLE IN FREE MEMORY.
*        IF MEMORY NOT AVAILABLE, RETURN NOTNUFSPACE ERROR (105)

*        INVOCATION SYNTAX:
*        CALL CRCINIT

CRCINIT EQU     *
         TSX
         TXD
         SUBD   #ENDPGM+STACKSLOP+512  Are 512 available for CRC table ?
         LBCS   COPYCANT               b/ no, go complain
gencrctable ; generate CRC table for efficient operation
; This routine initializes the CRCTABLE for use by GENCRC.
; It pre-computes the crc divisor for each of the 256 possible cases
; (1 per possible byte pattern).
; The crc polynomial used is crc-ccit = x^16+x^12+x^5+x^0
         ldx   #crctable               where table is
         clra                          byte to perform CRC upon
gencrctableloop ; compute 8 bits of CRC on (A)
         psha                          save counter
         clrb                          extend (A) to 16 bits
         bsr   gencrctable4bits
         bsr   gencrctable4bits
         sta   ,x+                     upper 8 bits of pre-computed CRC
         stab  255,x                   lower 8 bits stored 256 bytes further down in table
         pula                          restore counter
         inca                          generate next byte code to do CRC on
         bne   gencrctableloop
         clc
         rts

gencrctable4bits bsr gencrctable2bits
gencrctable2bits bsr gencrctable1bit
gencrctable1bit ; generate 1 CRC table bit
         asld                          shift MSB into carry
         bcc   gencrctablerts          b/ MSB is zero, divisor won't go in
         eora  #$10                    CRC-CCIT polynomial (:11021)...
         eorb  #$21                    will go in, subtract it mod 2
gencrctablerts rts
         page
*        CRC -- PERFORM CRC OVER A STRING
*        INVOCATION SYNTAX:
*
*        CALL CRC(BYTESTOCRC$,CRCREMAINDER$)
*
*        BYTESTOCRC$ IS A STRING EXPRESSION (100 BYTES)
*        CRCREMAINDER$ MUST HAVE A CURLEN OF 2
*

CRC      CMPA  #2                      ARE THERE TWO ARGUMENTS?
         BNE   CRCWRONGARGCOUNT        B/ NO
         STX   ARGLIST                 SAVE POINTER TO ARGUMENTS
         JSR   GETSTRING               GET CRCREMAINDER$ ADDRESS
         CMPD  #2                      LENGTH MUST BE 2...
         BNE   CRCBADARGUMENT          B/ WRONG LENGTH
         STX   CRCREMAINDERPTR         SAVE WHERE TO PUT RESULT
         LDD   0,X                     FETCH PREVIOUS VALUE OF CRC REMAINDER
         STAA  CRCUPPER                SET UP TO CRC
         STAA  CRCUPPERDUPLICATE       I know this looks funny, but is needed
         STAB  CRCLOWER
         LDX   ARGLIST                 FETCH POINTER TO STRING TO BE CRC'D
         LEAX  6,X                     (SKIP OVER CRCREMAINDER$ ARGUMENT)
         JSR   GETSTRING
         STX   NEXTBYTE                = ADDRESS OF STRING TO CRC
         STD   BYTECOUNT               AND LENGTH OF STRING TO CRC

CRCLOOP  LDX   NEXTBYTE                PERFORM CRC ON NEXT BYTE
         LDA   ,X+                     FETCH BYTE TO PERFORM CRC ON
         STX   NEXTBYTE                SAVE POINTER
gencrcstep ; adjust CRCUPPER/CRCLOWER according to byte in (A); destroys (A)
;  *** WARNING: THIS IS NON-REENTRANT ROUTINE!
;  This code works for 6800/6801/6809; you'll find it hard to make it faster!
;  The basic idea here is to do an XOR based upon an entire byte
;  at a time, rather than one bit at a time.  CRCTABLE contains pre-computed
;  8 bit CRCs to help us do this process quickly.
         ldab  #0                      (2~)
crclower equ *-1                       storing in instruction speeds this up
         ldx   #crctable               (3~) set up to index CRCTABLE
         eorb  255,x                   (5~) = "eorb crctable+crcupper"
crcupper equ *-1                       byte adjusted by STAB below
         if    m6800!m6801
         ldx   #crctable+256           (3~) set up index upper half CRCTABLE
         eora  255,x                   (5~) = "eora crctable+crcupper+256"
crcupperduplicate equ *-1              byte adjusted by STAB below
         else  (m6809)
         eora  256+255,x               (4~) = "eora crctable+crcupper+256"
crcupperduplicate equ *-1              byte adjusted by STAB below
         fin
         stab  crcupper                (4~) store the updated crc
         stab  crcupperduplicate       (4~)
         staa  crclower                (4~)

         LDX   BYTECOUNT               DOWN COUNT REMAINING BYTES
         DEX
         STX   BYTECOUNT
         BNE   CRCLOOP                 B/ MORE BYTES TO PROCESS
         LDAA  CRCUPPER                GET RESULT
         LDAB  CRCLOWER
         LDX   CRCREMAINDERPTR         = WHERE TO PUT IT
         STD   ,X
         CLC
         RTS

CRCWRONGARGCOUNT LDX #ERR:WRONGARGCOUNT TELL THE CALLER
         SEC
         RTS

CRCBADARGUMENT LDX #ERR:BADARGUMENT    TELL THE CALLER
         SEC
         RTS

CRCREMAINDERPTR FDB 0                  = WHERE TO PUT FINAL CRC
NEXTBYTE FDB   0                       = ADDRESS OF NEXT BYTE TO ADD TO CRC
BYTECOUNT FDB  0                       = # BYTES LEFT TO CRC
ARGLIST  FDB   0                       = ADDRESS OF ARGUMENT LIST
         PAGE  COPY FILE SUBROUTINE
*        COPYFILE -- AN ASSEMBLY SUBROUTINE TO COPY FILES QUICKLY
*        COPIES FILE CONTENTS OF CHANNEL 2 TO CHANNEL 1 AS FAST AS POSSIBLE
*
COPYINIT LDX   2,X                     INITIALIZE COPYFILE SUBROUTINE
         LDA   4,X
         STA   SPARSEFLAG              0 --> "PRESERVE SPARSENESS OF FILE"
         LDX   #0                      ZERO NUMBER OF BYTES WRITTEN TO TARGET
         STX   FILEPOSITION
         STX   FILEPOSITION+2
         LDA   #1
         STA   FILEISEMPTY             ASSUME THAT FILE IS EMPTY
         LDX   #ENDPGM+1               SET BUFFER ADDRESS, DESTROYED BY
         STX   COPYBUFFER              FINDBUSYDIRECTORYENTRY
         CLC
         RTS

COPYCANT
         LDX   #ERR:NOTENUFSPACE       ASSUME NOT ENOUGH SPACE TO DO COPY
         SEC
         RTS

*        NOTE THAT COPYF MAY BE CALLED SEVERAL TIMES AFTER A CALL TO COPYINIT
*        THIS IS BECAUSE OF .COPY fid1,fid2,... TO fid
*        THE LOGIC IN HERE WILL NOT CONCATENATE SPARSE FILES IN AN OPTIMAL WAY

COPYF    LDX   2,X                     WORKHORSE PART OF COPYFILE
         LDA   4,X                     GET MARGIN FROM BASIC PROGRAM
         STA   MARGIN
         COMA                          MAKE MASK
         STA   COPYBUFFERSIZE          SAVE IT TEMPORARILY
***** NOTE: A "BIG" COPY SHOULD USE THE ENTIRE ADDRESS SPACE, NOT
***** JUST WHAT IS AVAILABLE ABOVE THE DEFAULTPROGRAM!
         TSX                           COMPUTE USABLE BUFFER SPACE
         TXD
         SUBD  #ENDPGM+STACKSLOP
         BCS   COPYCANT
         ANDB  COPYBUFFERSIZE          MASK OFF PARTIAL "MARGIN"
         STD   COPYBUFFERSIZE          SAVE SIZE OF COPY BUFFER
         BEQD  COPYCANT                B/ NO ROOM, CAN'T DO THE COPY!
         ADDD  COPYBUFFER              DETERMINE END OF COPY BUFFER
         STD   COPYBUFFEREND
         CLRA
         LDB   MARGIN
         ADDD  #1
         STD   WRITEBUFFERSIZE         = SIZE OF WRITE BUFFER
*        NOTE: SHOULD ONLY DUMP BUFFERS IF COPYING TO DISK DEVICE OR DISK FILE!
COPYLOOP LDX   #DUMPBUFFERS            REAM THE DISK BUFFER POOL
         JSR   SYSCALL                 TO PREVENT HEADS FROM BOUNCING AROUND
         BCC   COPYLOOP1               B/ NO ERROR
         CPX   #ERR:ILLDEVICEOP        UNDEFINED OPERATION ?
         LBNE  SYSCALL1                B/ NO, ABORT THE COPY!
COPYLOOP1 ; SLURP AS MUCH AS POSSIBLE FROM THE SOURCE FILE
         LDX   #READ                   READ AS MUCH AS WE CAN

         JSR   SYSCALL
         BCS   COPYREADERROR           B/ ERROR WHILE READING IN COPY
         CLR   FILEISEMPTY             REMEMBER THAT FILE IS NOT EMPTY
         LDX   COPYBUFFER              = WHERE TO START WRITING FROM
COPYLOOP2 JSR  COPYWRITEBLOCK
         CPX   COPYBUFFEREND
         BNE   COPYLOOP2               B/ MUST WRITE MORE OF COPY BUFFER
         BRA   COPYLOOP                B/ COPY BUFFER COMPLETELY DUMPED

COPYREADERROR CPX #ERR:EOFHIT          ARE WE DONE ?
         LBNE  SYSCALL1                B/ NO, SOMETHING AWFUL HAPPENED!
         LDX   REPLYLENGTH             DID WE GET ANY DATA ?
         BEQ   COPYREADERROR1          B/ NO
         CLR   FILEISEMPTY             YES, REMEMBER FILE IS NOT EMPTY
COPYREADERROR1
         LDAB  MARGIN                  COMPUTE ADDRESS OF LAST FULL MARGIN
         COMB
         LDAA  #$FF                    = UPPER HALF OF MARGIN MASK
         ANDA  REPLYLENGTH
         ANDB  REPLYLENGTH+1
         ADDD  COPYBUFFER
         STD   COPYBUFFEREND
         LDX   COPYBUFFER
COPYFINAL CPX  COPYBUFFEREND           WRITE LAST BUFFERFUL OF DATA TO FILE
         BEQ   COPYFRACTION            B/ LAST "MARGIN"FUL HAS BEEN WRITTEN
         JSR   COPYWRITEBLOCK          WRITE ANOTHER MARGIN'S WORTH
         BRA   COPYFINAL               OUTPUT THE REMAINING FULL MARGINS

COPYFRACTION LDAB REPLYLENGTH+1        OUTPUT REMAINING FRACTIONAL MARGIN
         ANDB  MARGIN                  (B) CONTAINS NUMBER OF BYTES
         BEQ   COPYLASTBYTE            B/ ZERO, WRITE THE LAST BYTE OUT ANYWAY
         STB   WRITEBUFFERSIZE+1       FILE SIZE IS SET RIGHT
         CLR   WRITEBUFFERSIZE         ASSERT: RESETS THE CARRY BIT!
         JSR   COPYUPDATEPOSB          UPDATE FILE POSITION BY ADDING (B)
         STX   WBUF
COPYLASTWRITE ; ISSUE THE FINAL WRITE, ABORT IF ERROR
         LDX   #WRITE                  ISSUE THE FINAL WRITE
         JMP   SYSCALL0

COPYLASTBYTE ; WRITE LAST BYTE OF SOURCE FILE TO DESTINATION...
*        TO HANDLE CASE OF FILE BEING SPARSE AT THE END
         TST   FILEISEMPTY             IF FILE IS EMPTY, DON'T DO THIS...
         BNE   DORTS                   B/ NO DATA TO WRITE
         DEX                           = ADDRESS OF LAST BYTE OF SOURCE FILE
         STX   WBUF
         LDD   #1                      = # BYTES TO WRITE
         STD   WRITEBUFFERSIZE
         LDD   FILEPOSITION+2          BACKUP FILE POSITION TO LAST FILE BYTE
         SUBD  #1
         STD   FILEPOSITION+2
         LDD   FILEPOSITION
         SBCB  #0
         SBCA  #0
         STD   FILEPOSITION
         BSR   COPYPOSITION            POSITION TO LAST BYTE
         LDX   #WRITE
         JSR   SYSCALL0                ISSUE FINAL WRITE
         CLC
         LDAB  #1                      UPDATEPOSITION BY 1 + 0 CARRY
         JMP   COPYUPDATEPOSB          FIXES POSITION FOR APPENDING FILES
         PAGE
COPYWRITEBLOCK EQU *                   (X) = LOCATION TO WRITE NEXT
         STX   WBUF                    = WHERE TO WRITE FROM NEXT
         LDA   ,X+                     INSPECT 1ST BYTE OF MARGIN
         LDAB  MARGIN
COPYWRITEBLOCKLOOP ; SCAN MARGIN FOR NON-ZERO BYTE, OPTIMIZED FOR SPEED
         IF    M6800!M6801
         ORAA  0,X                     INSPECT NEXT BYTE OF MARGIN
         BNE   COPYNOTSPARSE           THIS CHUNK OF MARGIN IS NOT SPARSE
         INX                           ADVANCE MARGIN SCAN POINTER
         ELSE  (M6809)
         ORA   ,X+                     INSPECT NEXT BYTE OF MARGIN
         BNE   COPYNOTSPARSE           THIS CHUNK OF MARGIN IS NOT SPARSE
         FIN   M6800!M6801
         DECB                          DOWN COUNT SIZE OF CHUNK TO SCAN
         BNE   COPYWRITEBLOCKLOOP      B/ HAVEN'T SCANNED ALL THE BYTES YET
         ORA   SPARSEFLAG              CHUNK IS COMPLETELY ZERO...
         BNE   COPYNOTSPARSE           B/ BUT IT MUST BE WRITTEN ANYWAY!
         BSR   COPYUPDATEPOS           COMPUTE SIZE OF FILE IF WE WROTE ZEROS
         BSR   COPYPOSITION            POSITION PAST THE CHUNK OF ZEROS
         BRA   COPYUPDATEWBUF          GO UPDATE THE BUFFER WRITE POINTER

COPYNOTSPARSE LDX #WRITE               MUST WRITE CHUNK TO FILE
         JSR   SYSCALL0                ISSUE THE WRITE, GIVE UP IF ERROR
         BSR   COPYUPDATEPOS           KEEP TRACK OF NUMBER OF BYTES WRITTEN
COPYUPDATEWBUF ; ADVANCE WRITE BUFFER POINTER
         LDD   WBUF                    ADVANCE WBUF TO NEXT MARGIN
         SEC                           = WBUF + MARGIN + 1
         ADCB  MARGIN
         ADCA  #0
         STD   WBUF
         LDX   WBUF
DORTS    RTS

COPYPOSITION ; POSITION THE TARGET FILE TO "FILEPOSITION", IGNORE EOF ERROR
         LDX   #POSITION               POSITION PAST SPARSE PART OF FILE
         JSR   SYSCALL
         BCC   DORTS                   B/ NO ERROR
         CPX   #ERR:EOFHIT             ERROR WHILE POSITIONING PAST ZERO SECTOR
         BEQ   DORTS                   B/ JUST EOF, IGNORE
         JMP   SYSCALL1                OTHER ERROR, ABORT COPY AND SIGNAL USER

COPYUPDATEPOS ; UPDATE POSITION IN WHICH TO WRITE NEXT CHUNK OF DATA
         LDB   MARGIN                  = LAST POSITION + MARGIN SIZE
         SEC                           (NOTE: FILE POSITION IS 32 BITS)
COPYUPDATEPOSB ; UPDATE POSITION BY ADDING (B)+CARRY
         ADCB  FILEPOSITION+3
         STAB  FILEPOSITION+3
         BCC   COPYUPDATERTS
         INC   FILEPOSITION+2
         BNE   COPYUPDATERTS
         LDX   FILEPOSITION
         INX
         STX   FILEPOSITION
COPYUPDATERTS
         CLC                           FIXES ERROR RETURN
         RTS
         PAGE
READ     FCB   SYSCALL:READB           SYSCALL TO READ FROM THE SOURCE FILE
         FCB   14                      SYSCALL BLOCK LENGTH
         FCB   2                       CHANNEL
         FCB   0                       2ND PARAMS BYTE (IGNORED)
         FDB   0                       WRITE BUFFER (NULL)
         FDB   0                       NULL LENGTH
REPLYLENGTH FDB 0                      ACTUAL READ LENGTH
COPYBUFFER FDB ENDPGM+1                COPY BUFFER ADDRESS
COPYBUFFERSIZE FDB 0                   IS A MULTIPLE OF A SECTOR SIZE

WRITE    FCB   SYSCALL:WRITEB          SYSTEM CALL TO WRITE TO TARGET FILE
         FCB   8                       SYSCALL BLOCK LENGTH
         FCB   1                       = CHANNEL NUMBER
         FCB   0                       2ND PARAMS BYTE (IGNORED)
WBUF     FDB   0                       ADDRESS OF DATA TO BE WRITTEN
WRITEBUFFERSIZE FDB 0                  NUMBER OF BYTES TO BE WRITTEN

POSITION FCB   SYSCALL:CONTROL         SYSTEM CALL TO POSITION TARGET FILE
         FCB   8                       SYSCALL BLOCK LENGTH
         FCB   1                       ON CHANNEL 1
         FCB   CC:POSITION             CONTROL FUNCTION
         FDB   FILEPOSITION            WRITE BUFFER
         FDB   4                       # BYTES IN POSITIONING INFORMATION

DUMPBUFFERS ; SYSCALL BLOCK TO DUMP BUFFERS OF TARGET FILE
         FCB   SYSCALL:CONTROL
         FCB   4
         FCB   1,CC:DUMPBUFFERS

FILEPOSITION FDB 0,0                   4 BYTES OF CURRENT FILE POSITION
COPYBUFFEREND FDB 0                    1+LAST BYTE OF LAST MARGIN
FILEISEMPTY FCB 1                      REMEMBERS THAT FILE HAS ZERO DATA BYTES
* THE "MARGIN" IS A (MULTIPLE OF A) POWER OF 2...
* WITH THE CONSTRAINT THAT IT MUST BE <= 256
* BECAUSE OF THE TACKY WAY THIS PROGRAM WAS BUILT, THE MARGIN VALUE ACTUALLY
* STORED IS THE DESIRED MARGIN,-1 BECAUSE IT WAS HISTORICALLY NECESSARY TO
* KEEP IT IN 1 BYTE, AND NOBODY HAS REMOVED THIS RESTRICTION YET.
MARGIN   FCB   0                       MARGIN VALUE-1
SPARSEFLAG FCB 0                       =0 IF COPY IS TO PRESERVE SPARSENESS

ENDPGM   EQU   *
CRCTABLE EQU   ENDPGM

         END   .PROGSTART
