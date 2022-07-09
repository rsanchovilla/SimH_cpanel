	TITLE *** SDOS -- SOFTWARE DYNAMICS 6800/6809 OPERATING SYSTEM (C) 1978 ***
	PAGE	SDOS REVISION HISTORY
SC:GETLINEFLAGSHINT	EQU	$34	*** FIGURE OUT WHERE TO PUT THIS!!
	NAME	SDOS1.1
********************************************************************************
******** Copyright (C) 1978, 1979, 1980, 1981, 1982 by Software Dynamics ********
********************************************************************************
*	This file, and all derivatives of it, in either source or object format,
*	are a copyright and Trade Secret of Software Dynamics.
*	Possession of its contents, modification or distribution of all
*	or any part of this information without explicit written permission
*	from Software Dynamics is strictly prohibited.
***************************************************************************
*
*	SDOS REVISION HISTORY:
*
*	9/2/78	SDOS1.0f	Original SDOS for 6800 only
*
*	11/21/81	SDOS1.1c	6800/6809 SDOS identical to SDOS1.0f
*			All bugs fixed by 1.0f patch file are corrected
*			Can use SDOS1.1 with I/O packages built for SDOS1.0f
*			MIKBUG no longer accepted
*
*	1/2/82		SDOS1.1d	Trivial fixes to 1.1c
*	2/1/82		SDOS1.1e	More fixes
*	3/16/82		SDOS1.1f	6809 Test flight version. Works fine.
*
*	8/16/82		SDOS1.1g	Several modifications.
*					1) Changed so serialnumber can be
*					   in fixed memory location, to allow
*					   SDOS to run on mass-produced computers
*					   that have boot ROMs that cannot be
*					   changed to match requirements of
*					   encryption scheme.
*					2) Enhanced performance of Block move
*					   for 6800 slightly.
*					3) Improved disk I/O to enhance
*					   performance on floppies.
*					4) Fixed "LOG doesn't print
*					   characters typed on CONSOLE:"
*					5) Verifies that SERIALNUMBER.SYS
*					   is an encrypted file
	page
*	Known Bugs:
*		After a Write at EOF, SC:GETEOF doesn't always claim
*		that EOF condition has occurred.
*
	page
*   SDOS1.1 improvements to SDOS1.0f:
*   1)  Fixed sparse file problem.
*   2)  Fixed Rename to same name to re-hash to proper directory slot.
*   3)  Replaced block move by very fast block move routine; added entry point
*       for fast block move to entry point table.
*   4)  Installed Illegal LSN check in SDOS so it need not be replicated
*       in drivers.
*   5)  Changed Mount logic to use BOOT:DIRLSN pointer instead of NLCN/2.
*       SDOS now verifies that DIRECTORY.SYS is present in directory entry
*   6)  Dumpbuffers modified to move contents of modified FCBs back
*       to disk.
*   7)  EXIT changed so that if error when fetching DEFAULTPROGRAM,
*       then the disk is dismounted and SDOS tries again.
*   8)  Installed Serial Numbering (SERIALNUMBER.SYS) in SDOSINIT.
*       SERIALNUMBER.SYS chains to INITIALIZE.SYS if present,
*       and sets up INITIALIZE.DO as DO file.
*       Added Decrypting loader for encrypted files.
*   9)  Changed Disk File and Device drivers to zero column count on Ascii:FF,
*       decrement column count on Ascii:BS, and to advance column count to
*       next multiple of 8 on Ascii:HT.
*   10) Modified Dismount to delete ErrorMsgs.sys if it is marked as deleted.
*       Also modified SDOS to allow ErrorMsgs.sys to be opened on the disk
*       selected by DEFAULTDISK.
*   11) Changed so Dumpbuffers no longer checks for open files; now only
*       Dismount checks for open files.
*   12) Changed Dumpbuffers so it do it does NOT mark sectors as invalid.
*   13) Modified SetMap logic to mark Last Bad LSN as invalid, and mark
*       all sectors in the pool as invalid.
*   14) Changed PARSENAME so it no longer skips leading blanks on disk file name
*   15) Changed checksum over SDOS so it cannot be trivially defeated by
*       exchanging 2 bytes, and so that it is statically computed at assembly time.
*   16) Modified CHAIN to make a Zero start addresss illegal. Also revised
*       so CHAIN will return errors to caller if no bytes are loaded, and to
*       use the stack at the top of the address space once some bytes are loaded.
*   17) Added patch space to SDOS.
*   18) Added check in Disk File/Device driver so that ^C^C...
*       while "READA"ing nulls will terminate relatively quickly.
*   19) Fixed DisplayError to close Log channel if it gets an error, and
*       then try to display the error again before giving up completely.
*   20) Changed file name parser to recognize disk device names of the form
*       xxxx:<blank or control char> so that D0:<CR> is recognized properly.
*   21) Changed Disk File Close to automaticallly deallocate extra space
*       allocated to a file when the FCB is freed.
*   22) All updates to a file (Create, Rename, Writea, WriteB, CC:SetFileSize)
*       cause the file creation date to be modified, and to reset a new
*       bit in the directory flags that sez: "This file has been backed up."
*   23) Added SYSCALL:LUNNAME to convert a logical unit number into the text string
*       that gives the name of that device.
*   24) Added SC:GETFILEDATE, SC:GETFILEPROT, CC:SETFILEDATE, CC:SETFILESIZE,
*       CC:SETFILEPROT to allow utility programs to manipulate files more easily.
*   25) Need to add disassembler confusers around the code:
*       1) assemble in location dependent code
*       2) Mix data constants into the code
*       3) insert dummy instructions into code whose body overlaps
*          a real instruction to throw off disassembly synchronization.
*   26) Allow CC:DUMPBUFFERS to be invoked on a file; dumps all sectors
*       intended for the device
*   27) SYSCALL:CREATE syscall block extension optionally specifies file size
*   28) ERR:NOSUCHPROGRAM issued if LOAD/CHAIN can't find file
*   29) Date not set --> can't create or update files!
*   30) Modified CC:Position for disk device and files to zero the column count
*   31) Added SYSCALL:DELAY, (dummy) SYSCALL:INTERLOCK system calls
*   32) Implemented Write Protect bit (can't rename, delete, write or chop)
*   33) Added implied positioning on Read/Write system calls
*   34) Added CC:POSITIONTOEND file system call
*   35) Made SDOS:ERROR logic fully re-entrant by placing errorcode in TEMPX
*   36) Made SC:GETERRORSTATS a built-in function of SDOS disk driver.
*       Includes Operation Count, Error Statuses, and LastErroringLSN
*
*	END REVISION HISTORY

*
*	Things to do:
*
*	for SDOS 1.2
*		after user-selectable timeout, start dumping dirty pages
*		optimize interface to disk read/write routines
*		add write protect checking
*		gettype should return value of DCB:DVTYP byte
*		should not allow ':' as last character of file name
*		reply buffer outside of user space or in scratchpad s/b illegal
*
	WITH	WI=107,DE=51
*
	IFUND	MEMSIZE
MEMSIZE	EQU	56	We can't beleive you would want it smaller...
	FIN
K	EQU	1024

	IF	M6800!M6801
SDOS:ESTSIZE	EQU	$3200
	ELSE	(M6809)
SDOS:ESTSIZE	EQU	$2E00
	FIN

	IFUND	CODE
CODE	EQU	MEMSIZE*K-SDOS:ESTSIZE
	FIN

LISTDEFS	EQU	1	FORCE LISTING OF EQUATES FILE
SYSTEMDEFS	EQU	1	WE WANT ALL THE DEFINITIONS!
	INCLUDE	SDOS11DEFS.ASM	GO PROCESS THE DEFINITIONS FILE

	TABS	10,18,30	TAB SETTING FOR LISTING SDOS PROPER
	page	*** SDOS Boot time Initialization Code ***
*	"SDOSINIT" -- SDOS INITIALIZING CODE
*

	ORG	SYSCALL$
	JMP	SDOSINIT	SO BOOT'S EXIT SYSCALL RESETS SDOS
*	THIS PLACE IS CHANGED TO A "JMP SYSCALLENTRY" BY INIT CODE
	page
	Org	$2000	BOOT.SYS owns $0-$1FFF when loading SDOS.SYS

SDOSINIT	EQU	*
*
*	SET UP SDOS STACK POINTER
*	ASSERT: DP REGISTER CONTAINS ZERO HERE!
*
	LDX	CODE+SDOS:CONFIGURATION	GET THE STACK POINTER
	LDX	CNFG:DRIVERBASE,X
	TXS		SO (S) POINTS TO 1ST BYTE BELOW DRIVER PACKAGE
	STS	SDOSSTACK	AND SAVE FOR RE-USE BY EXIT
	LDAA	#$7E	("JMP" OPCODE) SET UP I/O PACKAGE BASE...
	STAA	,X	TO POINT TO SDOS ENTRY POINT
	LDD	#SYSCALLENTRY
	STD	1,X
	CLRA		MAKE SURE SDOSINIT CODE IS OK!
	LDX	#SDOSINIT	BASE OF AREA TO CHECKSUM
SDOSINITCKSUML	; LOOP TO CHECKSUM BYTES
	ASLA		MAKE CHECKSUM DEPEND ON ORDER OF BYTES
	IF	M6800!M6801
	ADCA	0,X
	INX
	ELSE
	ADCA	,X+
	FIN
	CPX	#SDOSINITEND
	BNE	SDOSINITCKSUML
	LDX	#ERR:SELFTESTCKSUM	ASSUME WE'RE SCREWED UP!
	ADDA	#$FF
	BCS	*	CHECKSUM <> 0, I'M SICK!
	JSR	SDOSINITS	CALL THE INITIALIZING SUBROUTINE
	BCS	*	B/ CROAKED IN CRITICAL INITIALIZATION CODE
	PAGE
*	Now test the ROM for proper construction.
*	If SERIALNUMBERLOCATION is zero,
*	Then: Follow Restart Vector to JMP around Serial number,
*	Verify presence of ROM serial number,
*	Restart-zeros-memory when RunningEncrypted code,
*	and NMI-is-nop code when RunningEncrypted.
*	Verify that all this stuff is in ROM so it cannot be turned off.
*
*	If SERIALNUMBERLOCATION is not zero, simply check
*	that serial number (as specified by SERIALNUMBERLOCATION) is in ROM.
*	Don't check SELECTBANK or NMI routine structure or operation;
*	don't fool with the RUNNINGENCRYPTED byte.  This allows SDOS
*	to operate in mass-produced, commercial micros such as
*	the Radio Shack Color Computer, which have Mask ROMs that cannot
*	be changed by SD (Sigh!)
*
*
	ldx	SerialNumberLocation	get pointer to S/N in mask-rom system
	beq	VerifyRestartCode0	b/ Standard ROM structure
	lda	#SerialNumber:Size	= size of serial number
	jsr	WriteRandom	Verify serial number is in ROM
	jmp	FetchSerialNumberSys	All done with ROM checking

VerifyRestartCode0 ; Check for standard ROM structure
	ldx	$fffe	follow restart vector to JMP around serial number
	stx	CopyofRestartVector
	ldx	$fffc	Get NMI vector pointer
	stx	CopyofNMIVector
	ldx	#$fffc	make sure NMI and RESTART vectors are ROM
	ldaa	#4	= size of region to write on
	jsr	WriteRandom	write all over region
	ldx	$fffe	Get "trashed-if-RAM" Restart Vector
	cpx	CopyofRestartVector Did it get trashed?
	bne	IncorrectROM	b/ yes, so-called "ROM" is not.
	ldaa	#8+3	verify ROM serial number is in ROM.
	jsr	Writerandom	over region (X) thru (X+A)
*
*	Verify presence of Reset code to zero memory if Encryted flag is set
*	Including checks for RunningEncrypted flag is in RAM,
*	that SelectBank subroutine actually works,
*	and that the entire thing is in ROM
*	Also verify that ROM causes NMI when encrypted to be ignored
*
	ldx	1,x	pointer to "Zero Memory if encrypted" code
*
*
	ldaa	#DontZeroMemory-Restart = length of Zero Memory sequence
	jsr	writerandom	write randomly over region x thru x+a
	if	m6800!m6801
	ldaa	0,x	Must be LDS immediate opcode
	cmpa	#$8E
	else	(m6809)
	ldd	0,x	Must be LDS immediate opcode
	cmpd	Restart
	fin
	bne	IncorrectROM	b/ Nope.
	ldaa	LdaEncrypted-Restart,x This must be "ldaa" extended opcode
	ldab	LdaEncrypted+3-Restart,x and this must be "Beq" opcode
	cmpd	#$B627
	bne	IncorrectROM	b/ its not...
	ldaa	LdaEncrypted+5-Restart,x	Must be "ldaa" immediate opcode
	ldab	EraseBank-Restart,x and this must be "Jsr" extended opcode
	cmpd	#$86BD	check...
	bne	IncorrectROM	Wrong code --> won't run program
	ldd	LdaEncrypted+1-Restart,x = address of RunningEncrypted
	std	RunningEncryptedp
	ldab	#DontZeroMemory-EraseMem = # bytes to push before compare...
VerifyRestartCode1 ; push bytes from Restart-zeros-memory into stack...
	dex	back up to previous byte
	ldaa	DontzeroMemory-Restart,x push a byte...
	psha
	decb
	bne	VerifyRestartCode1 b/ didn't push enough bytes yet
	ldx	#EraseMem-1	= code to compare against
	ldab	#DontZeroMemory-EraseMem = # bytes to compare
VerifyRestartCodeloop
	inx		advance pointer to next byte to match
	pula		pop byte to compare
	cmpa	0,x
	bne	IncorrectROM	b/ contents of Restart-zeros-mem is wrong
	decb
	bne	VerifyRestartCodeloop b/ more bytes to compare
*
*	Reset code has correct structure.
*	Verify that RunningEncrypted byte is in RAM, and is thus not always 0.
*
	ldx	Runningencryptedp	Verify runningencrypted is in RAM
	clr	0,x	will it store a zero?
	ldaa	0,x
	bne	IncorrectROM	b/ RunningEncrypted is not in RAM
	ldaa	#$39	Verify that RunningEncrypted byte is RAM
	staa	0,x	(Store an "RTS" instruction there!)
	cmpa	0,x	(SELECTBANK will blow up if zeros RunningEncrypted flag
	bne	IncorrectROM	b/ RunningEncrypted is not in RAM
	page
*
*	Now verify that SELECTBANK works properly.
*	Note: a theif could build a phony SelectBank that checks the
*	return address to see if it points to EraseMem (or the stack pointer
*	to see if it contains the Restart value - 2), and if so,
*	simply jumps to Dontzeromemory (see ROM descripton of Encrypted files)
*	That defeats our purpose. I hope nobody figures it out.
*	We can actually verify that for this machine, the Selectbank
*	routine is correct by examining the serial number, which tells us
*	the distributor, and therefore the machine organization;
*	this in turn tells us exactly what the SelectBank code should be.
*	But we don't do that yet, because a hole in I/O package is easier to
*	take advantage of, so fixing this wouldn't really help much.
*	We could also check (??) that select bank has no instructions
*	in it that reference the stack register. That is, instructions
*	that compare the stack pointer to anything, or copy the stack
*	pointer (i.e., push it, store it, etc.) (to prevent copying it and
*	THEN comparing), and indexed references to it
*	(to prevent the return address from being examined)]. This is
*	lots tougher! But DOABLE!
*
	ldd	#VerifyNMIisnop	where to come back to...
	pshd		Call SelectBank to make sure it works!
	ldd	RunningEncryptedp	With Return address = RunningEncrypted-1
	pshd		Push "Return address" to byte on stack
	clra		Verify that Select Bank subroutine works..
	ldx	$fffe	Get address of RunningEncrypted routine
	ldx	1,x	= address of Reset-zeros-memory code
	ldx	EraseBank-Restart+1,x	= pointer to SelectBank routine
	jmp	0,x	invoke it to select bank 0
	page
IncorrectROM ; Contents of Boot ROM is incorrect
	ldd	#SayInvalidROM-$1275 pass control to routine to print it...
	addd	#$1275	in a non-obvious way
	pshd		people with disassemblers will never get it
	rts
*
*	Verify that NMI routine acts as NOP when RunningEncrypted
*
VerifyNMIisnop
	ldx	$fffc	= address of NMI code
	cpx	CopyofNMIVector	Ensure NMI vector is in ROM
	bne	IncorrectROM	b/ its not.
	ldaa	#5	size of NMI code that we wish to test
	jsr	WriteRandom	all over the NMI code
	ldaa	0,x	this is sposd to be LDAA opcode
	ldab	3,x	and this is sposd to be "BNE" opcode
	cmpd	#$B626	check them both...
	bne	IncorrectROM	b/ NMI routine is RAM or wrong
	ldd	1,x	sposd to be address of RunningEncrypted
	cmpd	RunningEncryptedp	heck it out...
	bne	IncorrectROM	b/ pointers are different!
	ldab	4,x	= displacement to RTI in BNE instruction
	if	m6800!m6801
	tba
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	asra		shift right, propogating the sign bit
	addd	$fffc	= address of RTI,-5
	tdx
	else	(m6809)
	leax	b,x	= address of RTI,-5
	fin
	clr	5,x	make sure RTI is in ROM
	ldaa	5,x	should be an RTI opcode
	cmpa	#$3B	an RTI instruction?
	lbne	IncorrectROM	b/ NMI routine is incorrect!
*
*	End ROM checking
*	Assert: RunningEncrypted is now set!
	page
FetchSerialNumberSys ; Pass control to SERIALNUMBER.SYS
	LDX	#OPENSERIALNO.SYS	SEE IF SERIALNUMBER.SYS IS ENCRYPTED
	JSR	DOSYSCALL
	LDX	#READSERIALNUMBERFILETYPE	GO GET 1ST LOAD RECORD OF FILE
	JSR	DOSYSCALL
	LDX	#CLOSESERIALNO.SYS	LEAVE THE SLATE CLEAN, CLOSE THE FILE
	JSR	DOSYSCALL	(WE MUST DO THIS BECAUSE FIRSTCHAIN DOESN'T CALL EXITS)
	LDX	#ERR:NOTALOADFILE	ASSUME THE WORST...
	LDA	DECRYPTBUFFER	FETCH LOAD RECORD TYPE
	CMPA	#LOAD:ENCRYPTED	AN ENCRYPTED LOAD FILE ?
	BNE	INITIALIZEERR	B/ NOT ENCRYPTED, GO TELL USER AND DIE...
	LDX	#CHAINTOSERIALNO.SYS	GO GET SERIALNUMBER.SYS
	STX	CODE+SDOS:IOBLOCKPTR	SET UP SYSCALL BLOCK POINTER LIKE SYSCALLENTRY
	INC	SDOSRECURSIVECALL	LIKE SYSCALL ENTRY
	JSR	FIRSTCHAIN	AVOID SYSCALL ENTRY SO "SAFEGAURD" NOT INVOKED FOR THIS CALL
	BCC	*	CAN'T COME BACK WITH CARRY CLEAR!
	CPX	#ERR:SERIALNOWRONG	WRONG MACHINE?
	BEQ	ERRWRONGSERIALNUMBER	B/ YES, TELL THE GUY.
*
*	If no SERIALNUMBER.SYS, or phony SERIALNUMBER.SYS,
*	then the Secret Long-term operation enabling signal...
*	will not be passed to SDOS, and so SDOS will die relatively quickly!
*
INITIALIZEERR	; PROBLEM OCCURRED AFTER INITIALIZING SDOS
	STX	ERRORCODE	PROBLEM WASN'T WHAT I EXPECTED
	LDX	#SETINITERROR	SO TELL THE OPERATOR...
	JSR	SYSCALLENTRY
	BCS	*	B/ THIS ISN'T OUR DAY FOR THINGS TO GO RIGHT
	LDX	#DISPLAYINITERROR
	JSR	SYSCALLENTRY
	BCS	*
	BRA	*	IT CAN'T GET HERE, BUT I'M PARANOID ANYWAY...
	page
*
*	Copy of code that is required to be in ROM Restart Routine
*
Restart Lds	#$1234	Reset the stack pointer
LdaEncrypted
	Ldaa	$1234	"RunningEncrypted"
	Beq	DontzeroMemory	B/ don't have to erase the world
	Ldaa	#$12	Number of banks, -1
EraseBank
	Jsr	$1234	"JSR SelectBank" (1234 is dummy)
EraseMem	; This is copy of code that is required to be in ROM Restart routine
	Ldx	#$BFFF	(this code zeros memory)
EraseLoop
	Clr	0,x
	Dex
	Bne	EraseLoop
	Deca		(decrements Bank number)
	bpl	Erasebank
DontzeroMemory ; end of code required to be in ROM Restart routine
	page
*
*	WriteRandom -- Write Random numbers over the Region (X) to (X)+(A)
*	Exits with (X) = value on entry
WriteRandom
	stx	tempx	preserve (X)
WriteRandomL
	ldab	seed	generate new random number
	beq	WriteRandom1	b/ zero seed, handle as special case
	bpl	WriteRandom2
WriteRandom1 ;	THIS ROUTINE NEEDS WORK!!!
	eorb	#$81
WriteRandom2
	aslb
	adcb	#0
	stab	seed
	stb	,x+	store random trash on the byte
	deca		down count # bytes to trash
	bne	WriteRandomL
	ldx	tempx	restore x to original value
	rts
	page
DoSyscall ; Execute syscall (X) and print error if failed
	jsr	syscallentry
	bcs	initializeerr
	rts

ERRWRONGSERIALNUMBER ; print out serial no of CPU, SerialNumber.sys and die.
	jsr	getserialnumberaddress	get pointer to ROM serial number
	ldd	#CPUSerialNumber	Where to put converted serial number
	jsr	cvttohexserialno convert (X) to hexadecimal serial # string
	ldx	#openserialno.sys
	bsr	dosyscall
	ldx	#ReadSerialNumberFromFile	into DecryptBuffer
	bsr	dosyscall
	ldx	#decryptbuffer
	ldd	#SDOSSerialNumber where to put Ascii hex version of Serial #
	jsr	cvttohexserialno
	ldx	#PrintCantRun
	bsr	dosyscall
	page
WaitforMessagePrinted
	ldaa	#4*(CantRunMsgEnd-CantRunmsg)//10 max time to print message
delayloop ; wait for 8 * 65536 * .5 sec = .262 second
	dex
	bne	delayloop
	deca	down count # quarter-seconds to delay...
	bne	delayloop
	ldx	zapmemoryloop	copy zap memory routine to $100
	stx	$100
	ldx	zapmemoryloop+2
	stx	$102
	ldx	zapmemoryloop+4
	stx	$104
	ldx	zapmemoryloop+6
	stx	$106
	ldx	#$c000	erase memory (Paranoia strikes!)
	ldaa	#$3E	wait for interrupt opcode
	jmp	$100	go destory memory

zapmemoryloop ; destroy memory contents
	sta	,-x	this loop will stop...
	sta	,-x	when ZapLast is overwritten
	bra	zapmemoryloop
	page
PrintCantRun ; Syscall to print "Can't run on this CPU"
	fcb	Syscall:Writea,Writea:Sclen
	fcb	0,ignored
	fdb	CantRunMsg,CantRunMsgEnd-CantRunMsg

Cantrunmsg
	fcc	"Cannot run SDOS for CPU serial number "
SDOSSerialNumber
	fcc	"xxxxxxxxxxxxxxxx"
	fcb	ascii:cr
	fcc	"on this processor (CPU serial number "
CPUSerialNumber
	fcc	"xxxxxxxxxxxxxxxx)."
	fcb	ascii:cr
Cantrunmsgend

SayInvalidROM ; Tell would-be theif that ROM isn't right!
*	Transfers to this location must be via RTS with registers trashed...
*	So that a breakpoint here cannot be used to trace backwards...
*	to the code jumping here
	ldx	#PrintInvalidROM	Let the theif know
	jsr	dosyscall
	jmp	WaitforMessagePrinted and then erase memory

PrintInvalidROM ; Syscall to display "Invalid ROM"
	fcb	Syscall:Writea,Writea:Sclen
	fcb	0,ignored
	fdb	InvalidROMMsg,InvalidROMMsgEnd-InvalidROMMSG

InvalidROMMsg
	fcc	"Boot ROM not constructed properly for Serial Numbered SDOS Systems"
	fcb	ascii:cr
InvalidROMMsgEnd
	page
*
*	CvttoHexSerialNo -- Convert (X) for 8 bytes to Ascii serial number
*	(D) points to target string buffer
*
CvttoHexSerialNo
	std	topointer	save pointer to target
	ldab	#SerialNumber:Size	# bytes to convert
CvttoHexSerialNoloop ; convert a byte
	pshb
	lda	,x+
	bsr	HextoD	Ascii of both nibbles to (D)
	stx	frompointer
	ldx	topointer	target buffer pointer
	std	,x++
	stx	topointer
	ldx	frompointer
	pulb
	decb
	bne	CvttoHexSerialNoLoop b/ more byte to convert
	rts

Hextod ; Convert (A) to Ascii Hex in (a,b)
	tab	duplicate byte so we can split into nibbles
	lsra	move upper nibble to lower part of (a)
	lsra
	lsra
	lsra
	andb	#$F	extract lower nibble
	adda	#'0	convert (a) to hex digit
	cmpa	#'9	valid digit?
	bls	Hextod1	b/ yep.
	adda	#'A-10-'0	no, convert to A-F hex.
Hextod1
	addb	#'0	convert (b) to hex ascii
	cmpb	#'9	valid digit?
	bls	Hextod2	b/ yep.
	addb	#'A-10-'0	no, convert to A-F hex
Hextod2
	rts

SDOSINITCKSUM	FCB	CHANGED	CHECKSUM OVER SDOSINIT CODE
*
*	SDOSINITCKSUM has a value such that the "sum" of all bytes...
*	between SDOSINIT and SDOSINITEND is zero.
*	Note that SDOSINITCKSUM is part of that sum.
SDOSINITERRED	EQU	*
	JMP	ERRORINX

SDOSINITS	; INITIALIZING SUBROUTINE
*
*	WITH INTERRUPTS DISABLED, GO RESET ALL THE DEVICE DRIVERS
*
	LDX	CODE+SDOS:CONFIGURATION	SCAN THROUGH THE DEVICE LIST
	LDX	CNFG:DEVICEDCBS,X	AND RESET EACH DEVICE FOUND IN THE LIST
	STX	DCBPOINTER	SAVE POINTER TO DCB LIST
	LDX	DCB:DRIVER,X	SAVE POINTER TO CONSOLE DRIVER FOR USE BY READA
	STX	CONSOLEDRIVER
	LDX	DCBPOINTER	FIND OUT LENGTH OF CONSOLE DEVICE NAME
	LDX	DCB:NAME,X
	STX	CONSOLENAMEPTR	SAVE ADDRESS OF CONSOLE NAME
SDOSINIT0	; LOOP TO COMPUTE LENGTH OF CONSOLE DEVICE NAME
	INX		BUMP POINTER TO CONSOLE DEVICE NAME PAST NAME CHARACTER
	INC	CONSOLENAMELEN+1	BUMP # CHARACTERS IN CONSOLE NAME LENGTH
	LDAA	,X	LOOK AT BYTE OF CONSOLE NAME
	BNE	SDOSINIT0	B/ NOT END OF CONSOLE DEVICE NAME
	BRA	SDOSINIT1A	GO RESET THE DEVICES IN THE LIST

SDOSINIT1	; FIND END OF NEXT DEVICE NAME
	STX	DCBPOINTER	SAVE POINTER TO REST OF DEVICE NAME LIST
SDOSINIT1A	EQU	*
	LDX	DCBPOINTER	RESET A DEVICE
	LDX	DCB:DRIVER,X	WITH INTERRUPTS OFF....
	STX	DRIVERVPOINTER	AND INTERRUPT SYSTEM NOT READY!
	JSR	[DRIVER:RESET,X]
	BCS	SDOSINITERRED	B/ DRIVER HAD AN ERROR!
	LDX	DCBPOINTER	GET ADDRESS OF REST OF DEVICE NAME LIST
	LDX	DCB:NEXTDCB,X	END OF LIST ?
	BNE	SDOSINIT1	B/ NO, GO RESET THE NEXT DEVICE
	JSR	[DRIVER:RESET+DISKFILEDRIVER]	RESET THE DISK FILE DRIVER
	JSR	[DRIVER:RESET+DISKDEVICEDRIVER]	RESET THE DISK DEVICE DRIVER
*** NOTE: THE DISK DRIVERS ARE RESET LAST SO THAT THE INIT ROUTINES...
*** FOR ALL THE OTHER DEVICES CAN BE ASSEMBLED AND EXECUTED IN THE
*** DISK BUFFER AREA (SETTING UP THE DISK DRIVERS DESTROYS THE CONTENTS OF THE BUFFER AREA)
	PAGE
*
*	SET UP INTERRUPT SYSTEM
*
	LDX	CODE+SDOS:CONFIGURATION	SET UP THE INTERRUPT STACK POINTER...
	LDX	CNFG:INTERRUPTSTACK,X	SO SCHEDULER CAN GRAB ITS VALUE QUICKLY
	STX	INTERRUPTSTACK
	LDX	CODE+SDOS:CONFIGURATION	MAKE A "JMP IOINTPOLL"...
	LDX	CNFG:IOINTPOLL,X	SO WE CAN GET TO DEVICE POLL ROUTINE QUICKLY
	STX	JMPIOINT+1	THE "JMP" PART IS ASSEMBLED IN, ALREADY
	LDX	CODE+SDOS:CONFIGURATION	SET UP "INTDISABLE"
	LDAA	CNFG:INTDISABLE,X	GRAB THE 3 BYTES FROM THE CONFIGURATION TABLE
	LDX	CNFG:INTDISABLE+1,X
	STAA	INTDISABLE	AND STORE THEM IN A MORE CONVENIENT PLACE
	STX	INTDISABLE+1
	LDX	CODE+SDOS:CONFIGURATION	SET UP "INTENABLE"
	LDAA	CNFG:INTENABLE,X
	LDX	CNFG:INTENABLE+1,X
	STAA	INTENABLE
	STX	INTENABLE+1
	LDX	CODE+SDOS:CONFIGURATION	SET UP "INTRTI"
	LDAA	CNFG:INTRTI,X
	LDX	CNFG:INTRTI+1,X
	STAA	INTRTI
	STX	INTRTI+1
	LDX	CODE+SDOS:CONFIGURATION	SET UP NUMBER OF TIME OUT BLOCKS
	LDD	CNFG:TIMEOUTLIST,X	GRAB POINTER TO LIST OF TIMEOUT BLOCKS
	STD	DELAYBLOCK+TIMEOUT:LINK	ATTACH TO DELAYBLOCK TIMEOUT BLOCK
	LDD	#DELAYBLOCK	MAKE CONFIGURATION TABLE POINT TO DELAY BLOCK
	STD	CNFG:TIMEOUTLIST,X	ASSERT: # TIMEOUT BLOCKS NOW >= 1!
	LDX	CNFG:TIMEOUTLIST,X	COUNT NUMBER OF TIMEOUT BLOCKS

SDOSINIT1L	; COUNT # TIMEOUT BLOCKS
	INC	NTIMEOUTBLKS	COUNT A TIMEOUT BLOCK
	LDX	TIMEOUT:LINK,X	FIND ADDRESS OF NEXT TIMEOUT BLOCK
	BNE	SDOSINIT1L	B/ ANOTHER TIMEOUT BLOCK FOUND!
SDOSINIT1B	EQU	*
	PAGE
*
*	SET UP TASKS
*
*	ADD TIMEOUT TASK AND USER TASK TO END OF TASK QUEUE
*
	LDD	CODE+SDOS:CONFIGURATION	COMPUTE POINTER TO "CNFG:TASKQUEUE"
	ADDD	#CNFG:TASKQUEUE
	TDX
SDOSINITTCB1	; FIND END OF TASK QUEUE SO WE CAN TACK ON SDOS TASKS
	STX	TEMPX	IN CASE THIS TCB IS THE LAST IN THE QUEUE
	LDX	TCB:LNK,X	FOLLOW LINK TO NEXT TCB
	BNE	SDOSINITTCB1	B/ NO, CHARGE!
	LDX	TEMPX	THIS GUY IS LAST IN QUEUE
	LDD	#TIMEOUTTASKTCB	MAKE HIM POINT TO SDOS'S TWO TASKS
	STD	TCB:LNK,X
	LDX	CODE+SDOS:CONFIGURATION	INITIALIZE THE TASK CONTROL BLOCKS
	LDX	CNFG:TASKQUEUE,X	GET POINTER TO FIRST TCB IN LIST
SDOSINITTCBL	; MARK TASK AS "EXECUTING"
	LDD	#EXECUTING
	STD	TCB:COND,X
	LDX	TCB:LNK,X	FIND NEXT TASK
	BNE	SDOSINITTCBL	B/ MORE TASKS TO MARK
	LDX	CODE+SDOS:CONFIGURATION	LET I/O PACKAGE SET UP ITS END OF INTERRUPT SYSTEM
	JSR	[CNFG:INTSETUP,X]	THIS MUST BE LAST SO I/O PACK CAN TACK...
*	LOW PRIORITY TIMEOUT TASK ON TASK QUEUE
	LDX	#FORCESCHEDULE	TRIGGER INTERRRUPT
	JSR	CODE+SDOS:STARTIO	TO CAUSE ALL TASKS TO START RUNNING
*	INTERRUPTS ARE ON WHEN WE RETURN!
*
*	RESET THE I/O CONTROL BLOCKS
*
	JSR	INITIOCBS	SO THEY ALL APPEAR TO BE CLOSED
*
*	MAKE THE LOG AND THE SYS CHANNEL FREE
*
	LDX	#0
	STX	LOGIOCB+IOCB:DRIVER
	STX	SYSIOCB+IOCB:DRIVER
	PAGE
*
*	DOUBLE-CHECK CHECKSUM COMPUTED BY SDOSINIT
*	IF WRONG, ASSUME WE'VE BEEN TAMPERED WITH, AND GET EVEN!
*
	LDX	#SDOSINIT-26	NO POINT IN MAKING THIS OBVIOUS...
	CLRB		ZERO A CHECKSUM REGISTER
SDOSINITDBLCHECKL
	INX		DO THIS OUT OF ORDER TO MAKE CODE HARD TO RECOGNIZE
	ASLB
	ADCB	#0	DON'T DO "ADCB n,X" CAUSE THAT'S EASY TO FIND...
	ADDB	25,X	AND THIS HAS THE SAME EFFECT ANYWAY!
	CPX	#SDOSINITEND-25-1	DONE CHECKSUMMING?
	BNE	SDOSINITDBLCHECKL	B/ NO
	STS	TEMPX	NOW ADD (B) TO (S)...
	ADDB	TEMPX+1	(S) WON'T CHANGE IF CHECKSUM IS CORRECT (ZERO!)
	STB	TEMPX+1	OTHERWISE, IT WILL CHANGE BY SOME AMOUNT...
	LDS	TEMPX	THAT WILL CAUSE "RTS" BELOW TO BLOW UP!
	JSR	OPENCONSOLE	OPEN UP CHANNEL 0 TO THE CONSOLE
	LDX	#EXITCOPYRIGHT	THEN SAY "HELLO!"
	JSR	SYSCALLENTRY
*
*	PRINT DISK IDENTIFICATION
*
	LDX	DEFAULTDISKDCB	SET UP TO READ LSN 0 ON DRIVE 0
	STX	SYSSECTORDB+SECTORDB:DISKINFO
	LDX	#SYSSECTORDB	MAKE A ZEROED LSN
	CLR	SECTORDB:LSN,X
	CLR	SECTORDB:LSN+1,X
	CLR	SECTORDB:LSN+2,X
	JSR	READSECTOR	GO READ SECTOR ZERO
	LDX	RDSI:SECTORBASE,X	GET BASE OF SECTOR BUFFER
	LDAA	#BOOT:DISKID	FIND BEGINNING OF DISK ID MESSAGE
SDOSINIT2	EQU	*
	INX
	DECA
	BNE	SDOSINIT2	= BASE OF SECTOR + BOOT:DISKID
	STX	SDOSINITID+WRITEA:BUFFERP	SAVE POINTER TO STRING
	JSR	WAITRDSI	...
	LDX	RDSI:SECTORBASE,X	GET POINTER TO SECTOR ZERO BUFFER
	LDAA	BOOT:CREATIONDATE+1,X	DISPLAY CREATION DATE
	JSR	BCDTOASCII	CONVERT BCD DIGITS TO ASCII IN (A,B)
	STD	INITDATE	STORE DAY INTO INITIALIZED DATE STRING
	LDAA	BOOT:CREATIONDATE+2,X	CONVERT YEAR
	JSR	BCDTOASCII
	STD	INITDATE+6
	LDAA	BOOT:CREATIONDATE,X	GET BCD EQUIVALENT OF CREATION DATE
	JSR	BCDTOASCII
	STD	INITDATE+3
	LDX	#DISPLAYINITDATE
	JSR	SYSCALLENTRY	DISPLAY DATE THE DISK WAS INITIALIZED
	LDX	#SDOSINITID	AND GO PRINT THE DISK ID
	JSR	SYSCALLENTRY
	JSR	EXITCR	AND PRINT A TERMINATING "CR"
	OKRTS		ALL DONE!
	PAGE
*
*	BCDTOASCII -- CONVERT BCD DIGIT PAIR IN (A)...
*		INTO ITS CONSTITUENT ASCII DIGITS IN (A,B)
*
BCDTOASCII	EQU	*
	TAB
	LSRA
	LSRA
	LSRA
	LSRA
	ANDB	#$F
	ADDD	#'0*256+'0
	RTS
*
*	DSKFRESET -- DISK FILE DRIVER RESET
*
DSKFRESET	EQU	*
	JSR	INITDISKINFO	SET UP ALL THE TABLES
	JSR	INITFCBS
	JSR	INITRDSI
	OKRTS
	PAGE
*
*	INITRDSI -- SETS UP RESIDENT DISK SECTOR INFO TABLE
*		ALLOCATES FROM CNFG:FREESPACE IN FOLLOWING WAY
*		1)  FIND FIRST WORD IN FREESPACE WHICH IS ON A SECTOR BOUNDARY
*		2)  COUNT # RDSIS ALLOCATABLE BELOW THAT
*		3)  COUNT # SECTOR BUFFERS ALLOCATABLE FROM FREESPACE REMAINING
*		4)  COUNT # RDSIS ALLOCATABLE FROM REMAINDER
*		5)  IF # RDSIS<# SECTOR BUFFS, USE A SECTOR BUFF FOR MORE RDSIS
*		6)  LOOP ON 5 UNTIL # RDSIS>=#SECTOR BUFFERS
*		7)  ALLOCATE RDSIS
*		8)  ALLOCATE SECTORS AS A LUMP
*		9)  ALLOCATE REST OF RDSIS
*		10) DISTRIBUTE SECTOR BUFFERS TO RDSIS
*		11) DONE
*
INITRDSI	EQU	*
* 1)  FIND FIRST WORD IN FREESPACE THAT CAN BE A SECTOR BOUNDARY
	LDX	CODE+SDOS:CONFIGURATION
	LDD	CNFG:DSKBUFFERPOOL,X	FIND 1ST POWER OF 2 BOUNDARY
*	WHICH IS A MULTIPLE OF CNFG:SECSIZE
	ADDD	MAXSECTORSIZE
	SUBD	#1	ROUND UP TO NEXT BOUNDARY
	PSHD		SAVE POINTER INTO 1ST SECTOR BUFFER
	LDD	MAXSECTORSIZE	MAKE A BOUNDARY MASK
	SUBD	#1
	COMD
	ANDD	0,S	MASK TO OBTAIN FIRST SECTOR BOUNDARY
	LEAS	2,S
	STD	FIRSTBUF
* 2) COUNT # RDSI'S ALLOCATABLE
	LDX	CODE+SDOS:CONFIGURATION
	SUBD	CNFG:DSKBUFFERPOOL,X	COMPUTE # WORDS BETWEEN
	CMPD	CNFG:DSKPOOLSIZE,X	FIRST WORD OF FS AND FIRSTBUF
	BHI	ERRNOTENOUGHPOOL	B/ >, ILLEGAL SPACE ALLOCATION
INITRDSI1	EQU	*
	STD	FIRSTSEGSIZE	SAVE 1ST SEGMENT SIZE
	CLR	NRDSIS	NRDSIS := 0
INITRDSI2	EQU	*
	INC	NRDSIS	ROOM FOR ONE MORE RDSI
	BEQ	INITRDSI2A	B/ QUIT IF 255 RDSI'S AVAILABLE
	SUBD	#RDSI:SIZE
	BCC	INITRDSI2	B/ MORE SPACE AVAILABLE
INITRDSI2A	EQU	*
	DEC	NRDSIS	= ACTUAL # RDSI'S AVAILABLE SO FAR
* 3) COUNT # POTENTIAL SECTOR BUFFERS
	LDX	CODE+SDOS:CONFIGURATION
	LDD	CNFG:DSKPOOLSIZE,X	ALL OF FREE SPACE - SIZE OF 1ST SEGMENT
	SUBD	FIRSTSEGSIZE
	CLR	NBUFFERS	ASSUME 0 SECTOR BUFFERS AVAILABLE
INITRDSI3	EQU	*
	INC	NBUFFERS	COUNT A BUFFER
	BEQ	INITRDSI3A	B/ QUIT IF 255 BUFFERS AVAILABLE
	SUBD	MAXSECTORSIZE
	BCC	INITRDSI3	B/ MORE ROOM STILL
INITRDSI3A	EQU	*
	DEC	NBUFFERS	= ACTUAL NUMBER OF BUFFERS AVAILABLE
	BEQ	ERRNOTENOUGHPOOL	B/ NO BUFFERS AVAILABLE!
* 4) COUNT # RDSI'S AVAILABLE FROM REMAINDER
INITRDSI4L	EQU	*
	ADDD	MAXSECTORSIZE
INITRDSI4	EQU	*
	INC	NRDSIS	BUMP # RDSI'S AVAILABLE
	BEQ	INITRDSI4A	B/ AT LEAST 255 AVAILABLE
	SUBD	#RDSI:SIZE
	BCC	INITRDSI4	B/ MORE SPACE!
	ADDD	#RDSI:SIZE	(A,B):= NUMBER OF BYTES LEFT OVER
INITRDSI4A	EQU	*
	DEC	NRDSIS	= ACTUAL # RDSI'S AVAILABLE
* 5) CHECK TO MAKE SURE # RDSI'S >= # SECTOR BUFFERS
	PSHA		SAVE (A)
	LDAA	NRDSIS
	CMPA	NBUFFERS
	PULA		RESTORE (A)
	BCC	INITRDSI7	B/ NRDSIS >= NBUFFERS
INITRDSI6	; NRDSIS < NBUFFERS
	DEC	NBUFFERS	THROW AWAY A BUFFER
	BNE	INITRDSI4L	USE THROWN AWAY BUFFER AS SPACE FOR RDSI'S
ERRNOTENOUGHPOOL	EQU	*
	JSR	ERRET
	FDB	ERR:NOTENOUGHPOOL

INITRDSI7	; NRDSIS >= NBUFFERS, USE NBUFFERS
	LDAA	NBUFFERS
	STAA	NRDSIS
	CMPA	#1	ENOUGH RDSIS TO MAKE SDOS RUN?
	BLS	ERRNOTENOUGHPOOL	B/ NOPE, MUST BE AT LEAST 2!
	LDX	#LASTSECTORREADQ	NOW ALLOCATE RDSI'S FROM FIRST SEGMENT
	STX	LASTSECTORREADQ+RDSI:FLINK	MAKE RDSI QUEUE EMPTY
	STX	LASTSECTORREADQ+RDSI:BLINK
	LDX	CODE+SDOS:CONFIGURATION
	LDD	CNFG:DSKBUFFERPOOL,X	GET POINTER TO NEXT POSSIBLE RDSI
	BSR	ALLOCRDSIS	GO ALLOCATE SOME RDSI'S
* 8) SKIP OVER SECTOR BUFFERS AS A LUMP
	LDAA	NRDSIS	= # BUFFERS TO SKIP (>=1)
	STAA	COUNT
	LDD	FIRSTBUF
INITRDSI8	; SKIP A SECTOR BUFFER
	ADDD	MAXSECTORSIZE
	DEC	COUNT	DOWN COUNT # BUFFERS TO SKIP
	BNE	INITRDSI8	B/ MORE BUFFERS TO SKIP
	LDX	#$FFFF	SET SEGMENT SIZE = "BIGGEST"
	STX	FIRSTSEGSIZE	(A,B) = POINTER TO REST OF RDSI'S
	BSR	ALLOCRDSIS	AND ALLOCATE REST OF RDSI'S
* 9) NOW DISTRIBUTE SECTOR BUFFERS TO RDSI'S
	LDX	LASTSECTORREADQ+RDSI:FLINK	1ST RDSI
	LDD	FIRSTBUF	STORE SECTOR BUFFER ADDRESS
INITRDSI9	EQU	*
	STD	RDSI:SECTORBASE,X
	CLR	RDSI:DISKINFO,X	SET DRIVE = "SECTOR NOT IN BUFFER"
	CLR	RDSI:DISKINFO+1,X	SO A LDX DETECTS THAT IT IS ZERO!
	STAA	RDSI:LSN+2,X	MAKE LSN <> 0 SO IT IS NOT THE MAGIC "0" SECTOR!!
	CLR	RDSI:MODIFIED,X	TAG "SECTOR NOT MODIFIED"
	CLR	RDSI:STATE,X	MARK RDSI AS "IDLE"
* NOTE: RDSI:TRACK is GARBAGE if RDSI:DSKINFO=0
	ADDD	MAXSECTORSIZE	FIND NEXT SECTOR BUFFER
	LDX	RDSI:FLINK,X	FIND NEXT RDSI
	CPX	#LASTSECTORREADQ	ALL RDSI'S INITZD?
	BNE	INITRDSI9	B/ NO
	OKRTS		YES, LET'S SPLIT!!
	PAGE
*
*	ALLOCRDSIS -- AND ADD TO LASTSECTORREADQ
*		(A,B) POINTS TO NEXT POSSIBLE RDSI
*		FIRSTSEGSIZE CONTAINS CONTIGUOUS SPACE REMAINING
*		ALLOCATES NO MORE THAN "NRDSIS" RDSI'S
*
ALLOCRDSIS	EQU	*
	STD	RDSIPOINTER	SAVE NEXT POTENTIAL RDSI ADDRESS
	LDD	FIRSTSEGSIZE	GET SIZE OF REMAINING SEGMENT
	SUBD	#RDSI:SIZE	ENOUGH SPACE LEFT TO ALLOCATE?
	BCS	ALLOCRDSISRTS	B/ NOT ENOUGH ROOM LEFT
	STD	FIRSTSEGSIZE
	LDAA	NBUFFERS	GET # RDSI'S TO ALLOCATE
	BEQ	ALLOCRDSISRTS	B/ DON'T ALLOCATE ANY MORE
	DEC	NBUFFERS	= ONE PER BUFFER
	LDX	RDSIPOINTER	GET POINTER TO RDSI TO ALLOCATE
	JSR	ENQUELASTSECTORREAD	ADD TO LASTSECTORREADQ
	LDD	#RDSI:SIZE	FIND NEXT RDSI
	ADDD	RDSIPOINTER
	BRA	ALLOCRDSIS	GO ALLOCATE ANOTHER

ALLOCRDSISRTS	EQU	*
	RTS
	PAGE
*
*	INITFCBS -- COMPUTES NUMBER OF FCBS AND STORES IN NFCBS
*		THEN SCANS ALL FCBS ZEROING FCB:REFCOUNT
*
INITFCBS	EQU	*
	LDX	CODE+SDOS:CONFIGURATION	COMPUTE # OF FCBS...
	LDAA	CNFG:NIOCHANNELS,X	= 1 PER I/O CHANNEL
	ADDA	NDRIVES	+2 PER DISK DRIVE
	ADDA	NDRIVES
	ADDA	#NMAGICFCBS	+ ODDBALL ASSORTMENT
	STAA	NFCBS	FOR USE BY FINDFREEFCBS, SEARCHFCBS
	STAA	COUNT	SO WE KNOW HOW MANY TO ZAP
	LDD	[CNFG:IOCBPOINTERS,X] GET ADDRESS OF 1ST IOCB
INITFCBL	EQU	*
	SUBD	#FCB:SIZE	FIND PREVIOUS FCB = THIS FCB - SIZE OF FCB
	TDX		COPY ADDRESS TO (X)
	CLR	FCB:REFCOUNT,X	ZAP THE REFERENCE COUNT
	DEC	COUNT
	BNE	INITFCBL	B/ MORE TO INIT
	LDX	#0	RESET POINTER TO ERROR MESSAGE FCB
	STX	ERRFCBPOINTER
	RTS
	PAGE
*
*	INITDISKINFO -- INITIALIZES ALL DISKINFO TABLES TO "DISMOUNTED"
*	COMPUTE MAXSECTORSIZE = MAX OF ALL SECTOR SIZES
*	COUNT # DRIVES
*
INITDISKINFO	EQU	*
	LDX	#0	ZERO MAX SECTOR SIZE FOUND SO FAR
	STX	MAXSECTORSIZE
	CLR	NDRIVES	ASSUME NO DISK DRIVES IN THIS SYSTEM (A LITTE FUNNY)
	LDX	CODE+SDOS:CONFIGURATION	FIND ADDRESS OF 1ST DISKINFO TABLE
	LDX	CNFG:DISKDCBS,X
	STX	DEFAULTDISKDCB	SO WE KNOW WHICH DISK TO DEFAULT TO
INITDISKINFOL	EQU	*
	STX	DCBPOINTER	SAVE THIS A MOMENT
	INC	NDRIVES	COUNT # DISKINFO TABLES (NOT TO EXCEED 255)
	CLR	DSKINFO:DIRFCB,X	MARK "NO DIRFCB SET UP" (DISMOUNTED)
	CLR	DSKINFO:DIRFCB+1,X
	CLR	DSKINFO:MAPFCB,X	DITCH MAP FCB...
	CLR	DSKINFO:MAPFCB+1,X
	CLR	DSKINFO:SECTORDB,X	MARK THIS DRIVE AS "NOT DOING ANY I/O"
	CLR	DSKINFO:SECTORDB+1,X
	LDAA	#-1	SET BAD LSN TO "NO BAD LSN"
	STAA	DSKINFO:BADLSN,X
	STAA	DSKINFO:BADLSN+1,X
	STAA	DSKINFO:BADLSN+2,X
	STAA	DSKINFO:ERRLSN,X	RESET LAST ERRORING LSN, TOO!
	STAA	DSKINFO:ERRLSN+1,X
	STAA	DSKINFO:ERRLSN+2,X
	LDD	DSKINFO:NBPS,X	COMPARE SECTOR SIZE FOR THIS DISK...
	CMPD	MAXSECTORSIZE	TO BIGGEST SECTOR SIZE FOUND SO FAR
	BCS	INITDISKINFO2	B/ SMALLER
	STD	MAXSECTORSIZE	KEEP BIGGEST SECTOR SIZE FOUND SO FAR
INITDISKINFO2	EQU	*
	CLR	DSKINFO:LOG2NBPS,X	NOW COMPUTE LOG(2) OF NBPS
	DEC	DSKINFO:LOG2NBPS,X	(INIT TO -1)
INITDISKINFO2L	EQU	*
	INC	DSKINFO:LOG2NBPS,X	LOG2 = LOG2 + 1
	LSRD		DIVIDE SECTOR SIZE BY POWER OF TWO
	BCC	INITDISKINFO2L	B/ STILL >= 1, KEEP COUNTING!
	TSTD		ALL SIGNIFICANCE SHOULD BE SHIFTED OUT
	BNED	ERRSECTORSIZE2	B/ SECTOR SIZE NOT A POWER OF 2!
	LDD	DSKINFO:NBPS,X	SET DSKINFO:NBPSM1 := DSKINFO:NBPS-1
	SUBD	#1	FOR EVERYONE ELSE'S CONVENIENCE
	STD	DSKINFO:NBPSM1,X
*
*	NOW COMPUTE NLSN = (NSPT*NTPC*NCYL)
*
	LDD	DSKINFO:NCYL,X	SET NLSN:=NCYL
	CLR	DSKINFO:NLSN,X
	STD	DSKINFO:NLSN+1,X
	LDD	DSKINFO:NTPC,X	THEN SET NLSN:=NLSN*NTPC
	BSR	MULNLSNBYAB
	LDD	DSKINFO:NSPT,X	AND THEN SET NLSN:=NLSN*NSPT
	BSR	MULNLSNBYAB
*
*	NOW RESET THE DISK DEVICE
*
	LDX	DCB:DRIVER,X	CALL THE DISK DRIVER RESET ROUTINE
	LDD	DRIVER:DISKRESET,X
	JSR	SECTORDRIVER	WHICH SETS (X) = DCBPOINTER
	BCS	INITDISKERRED	B/ OOPS...
	LDX	DCBPOINTER	NOW FIND NEXT DISK INFO TABLE
	LDX	DCB:NEXTDCB,X
	BNE	INITDISKINFOL
	OKRTS

INITDISKERRED	EQU	*
	JMP	ERRORINX
	PAGE
ERRSECTORSIZE2	EQU	*
	JSR	ERRET
	FDB	ERR:SECTORSIZE2	SECTOR SIZE IS NOT A POWER OF 2!
*
*	MULNLSNBYAB -- MULTIPLY DSKINFO:NLSN BY (A,B)
*	(X) POINTS TO DCB FOR DISK; (X) IS PRESERVED
*	PLACES PRODUCT IN DSKINFO:NLSN
*	OKRET IF PRODUCT < 2^24
*	ERRET IF PRODUCT >= 2^24
*
********* SHOULD THIS BE DONE USING "MUL" INSTRUCTION???
*
MULNLSNBYAB	EQU	*
	STD	TEMP.MPCND	SAVE MULTIPLICAND
	LDAA	#25	# MULTIPLIER BITS (THE 25TH BIT IS GAURANTEED = 0)
	STAA	COUNT	SET UP LOOP COUNT
	CLRA		ZERO THE PARTIAL PRODUCT
	CLRB		AND THE CARRY OUT OF THE PARTIAL PRODUCT
MULNLSNBYABL	; PROCESS NEXT MULTIPLIER BIT
	RORD		PRESERVE CARRY OUT OF PARTIAL PRODUCT
*			RIGHT SHIFT PARTIAL PRODUCT...
	ROR	DSKINFO:NLSN,X	INTO MULTIPLIER
	ROR	DSKINFO:NLSN+1,X
	ROR	DSKINFO:NLSN+2,X	AND SHIFT OUT MULTIPLIER BIT
	BCC	MULNLSNBYAB1	B/ DON'T ADD (SET CARRY OUT OF PARTIAL PRODUCT TO 0)
	ADDD	TEMP.MPCND	ADD MULTIPLICAND TO PARTIAL PRODUCT
MULNLSNBYAB1	EQU	*
	DEC	COUNT	DOWN COUNT # MULTIPLIER BITS
	BNE	MULNLSNBYABL	B/ MORE MULTIPLIER BITS TO PROCESS
	BCS	ERRNLSNGE224	B/ PRODUCT >= 2^24
	TSTD		UPPER 2 BYTES OF 5 BYTE PRODUCT <> 0 ?
	BNED	ERRNLSNGE224	B/ YES, PRODUCT TOO BIG
	OKRTS

ERRNLSNGE224	EQU	*
	JSR	ERRET
	FDB	ERR:NLSNGE224
	PAGE
*
*	INITIOCBS -- SETS UP IOCBS FOR USE BY DISKFILE DRIVERS,
*		DISKDEVICEDRIVERS
*
INITIOCBS	EQU	*
	LDX	CODE+SDOS:CONFIGURATION	SCAN ALL THE IOCBS
	LDAA	CNFG:NIOCHANNELS,X
	STAA	INITCOUNTER
	LDX	CNFG:IOCBPOINTERS,X
INITIOCBL	; INIT NEXT IOCB
	STX	INITX
	LDX	,X	GET IOCB ADDRESS
	BSR	INITIOCB	DUH
	LDX	INITX	FIND NEXT IOCB POINTER
	LEAX	2,X
	DEC	INITCOUNTER
	BNE	INITIOCBL	B/ MORE IOCBS
	LDX	#SYSIOCB	INIT THE SYSTEM IOCB
	BSR	INITIOCB
	LDX	#LOGIOCB	INIT THE LOG FILE IOCB
	BSR	INITIOCB
	OKRTS		AND EXIT
*
*	INITIOCB -- RESET IOCB(X)
*
INITIOCB	EQU	*
	CLR	IOCB:DRIVER,X
	CLR	IOCB:DRIVER+1,X
	RTS
	page
*
*	TimeoutTask Initialization
*
ExitScanPtr	Equ	2	Pointer used to Scan Exit code for checking
ExitPartialChksum	Equ	4	Partially computed checksum

TimeouttaskSetup	; Timeout Task initializing code
	Clr	ExitPartialChksum	zero initial value of checksum
	Ldx	#Exitchksumbase-15	don't make base of checksum obvious
	Stx	Exitscanptr
	Jmp	Timeouttaskbored

SDOSINITEND	; END OF SDOSINIT CHECKSUMMED CODE
	page
*	The following routine is used by SD personnel after SDOS is assembled or patched...
*	to compute the various checksum values required to make SDOS operate.
*	Control is transferred here (instead of to SDOSINIT);
*	the checksum bytes are modified until the correct checksums are obtained
*	(this process presumes that the memory of the computer works!)
*	The values in the checksum bytes must then be patched into SDOS with BMP.
*	The code is a little obscure because we don't want it accidentally found
*
	Fcb	$BF	Red herring for would-be disassemblers
ComputeChecksums; Compute checksums over SDOS
*	Note: Exit Checksum must be computed before MainChecksum!
ComputeExitChecksum ; loop back here until SDOSEXITCHKSUM gives zero
	ldx	#EXITCODECHKSUM-51	don't make it obvious where checksum byte is
	inc	51,x	old value didn't work, try this one!
	ldx	#EXITCHKSUMBASE-41	don't make it obvious where checksum byte is
	clrb		zero the checksum accumulator
ComputeExitChecksuml	; loop here to checksum a byte
	aslb		make checksum depend on order of the data
	adcb	41,x
	inx
	cpx	#EXITCHKSUMEND-41	done?
	bne	ComputeExitChecksuml	b/ no
	negb		did we get proper checksum?
	bne	ComputeExitChecksum
ComputeMainChecksum ; loop back here until main SDOS checksum gives zero
	ldx	#SDOSCODECHKSUM-49	don't make it obvious where checksum byte is
	inc	49,x	old value didn't work, try this one!
	ldx	#SDOSCHKSUMBASE-7	where to start checksumming over SDOS
	clrb		zero the checksum accumulator
ComputeMainChecksuml ; loop here to checksum a byte
	aslb		make checksum depend on order of the data
	adcb	7,x
	inx
	cpx	#SDOS:END-7
	bne	ComputeMainChecksuml	b/ more bytes to checksum
	addb	#0	did checksum come up zero?
	bne	ComputeMainChecksum	b/ no, try a new value!
	page
ComputeInitChecksum ; loop back here until SDOSINIT checksum gives zero
	ldx	#SDOSINITCKSUM-22	don't make it obvious where checksum byte is
	inc	22,x	old value didn't work, try this one!
	ldx	#SDOSINIT-75	don't make it obvious where checksum byte is
	clrb		zero the checksum accumulator
ComputeInitChecksuml	; loop here to checksum a byte
	aslb		make checksum depend on order of the data
	inx		do this one first to be confusing!
	adcb	74,x
	cpx	#SDOSINITEND-74-1	done?
	bne	ComputeInitChecksuml	b/ no
	negb		did we get proper checksum?
	bne	ComputeInitChecksum
	rti		if you let it get here, you don't know what you're doing...
	PAGE
*
*	STORAGE USED ONLY BY INITIALIZATION ROUTINES
*
FIRSTBUF	RMB	2	POINTER TO 1ST SECTOR BUFFER
FIRSTSEGSIZE	RMB	2	SIZE OF 1ST REGION FROM WHICH TO ALLOCATE RDSI'S
NBUFFERS	RMB	1	# OF SECTOR BUFFERS AVAILABLE
INITCOUNTER	RMB	1
INITX	RMB	2
*
EXITCOPYRIGHT	; USED TO DISPLAY SDOS BANNER, COPYRIGHT MESSAGE
	FCB	SYSCALL:WRITEA	FUNCTION
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL
	RMB	1	ANY GARBAGE
	FDB	SDOSMSG	BUFFER ADDRESS
	FDB	COPYRIGHTEND-SDOSMSG	REQUESTED COUNT
*
SDOSINITID	; USED TO DISPLAY DISK IDENTIFICATION STRING
	FCB	SYSCALL:WRITEA	FUNCTION
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL
	RMB	1	ANY OLD GARBAGE FOR 2ND PARAMETER BYTE
	RMB	2	POINTER TO ID STRING IN BOOT SECTOR
	FDB	BOOT:SIZE-BOOT:DISKID	LENGTH OF ID STRING
*
DISPLAYINITDATE	; SYSCALL USED TO DISPLAY DISK INITZ DATE
	FCB	SYSCALL:WRITEA
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL NUMBER
	RMB	1	ANY GARBAGE BYTE
	FDB	INITDATE	STRING TO DISPLAY
	FDB	INITDATELEN	SIZE OF STRING TO DISPLAY

INITDATE	FCC	"00/00/00 "
INITDATELEN	EQU	*-INITDATE
	PAGE
CHAINTOSERIALNO.SYS ; SYSTEM CALL TO CHAIN TO SERIALNUMBER.SYS
	FCB	SYSCALL:CHAIN,CHAIN:SCLEN
	FCB	IGNORED,IGNORED
	FDB	SERIALNUMBER.SYS,SERIALNUMBER.SYSL
	FDB	CHANGED
	FDB	SCRATCH,4

SERIALNUMBER.SYS	FCC	"SERIALNUMBER.SYS"
SERIALNUMBER.SYSL	EQU	*-SERIALNUMBER.SYS

OPENSERIALNO.SYS	; SYSTEM CALL TO OPEN SERIALNUMBER.SYS
	FCB	SYSCALL:OPEN,OPEN:SCLEN
	FCB	1,IGNORED
	FDB	SERIALNUMBER.SYS,SERIALNUMBER.SYSL
	FDB	CHANGED
	FDB	SCRATCH,4

SCRATCH	RMB	4

CLOSESERIALNO.SYS	; SYSTEM CALL TO CLOSE SERIALNUMBER.SYS
	FCB	SYSCALL:CLOSE,CLOSE:SCLEN
	FCB	1

SETINITERROR	; SYSCALL TO TELL SDOS WHAT ERROR CODE WE WANT DISPLAYED
	FCB	SYSCALL:SETERROR,SETERROR:SCLEN
ERRORCODE	FDB	CHANGED

DisplayInitError	; Syscall to display error code last set
	Fcb	Syscall:Disperror,Disperror:Sclen

ReadSerialNumberFromFile	; Syscall to read Serial number from encrypted file
	fcb	Syscall:Readb,RWposition:sclen
	fcb	1,ignored
	fdb	ignored,ignored
	fdb	changed	expected value of 8
	fdb	DecryptBuffer,SerialNumber:Size	where to read serial number
	fdb	$0000,$0008	file position to start reading at

ReadSerialNumberFileType ; Syscall to read type of 1st load record of file
	fcb	Syscall:Readb,Readb:Sclen
	fcb	1,ignored
	fdb	ignored,ignored
	fdb	changed	expected value of 1
	fdb	DecryptBuffer,1	where to read load record type byte to

seed	rmb	1	self-initializes with garbage

RunningEncryptedp	rmb	2	Pointer to Running Encrypted byte

CopyofNMIVector ; save copy of NMI vector for NMI-vector-not-in-RAM test
	fdb	changed
CopyofRestartVector ; copy of Restart vector for Restart-vector-not-in-RAM test
	fdb	changed

	IF	*>/CODE
	?SDOS INITIALIZATION CODE OVERLAPS SDOS?
	FIN
	PAGE	--- SDOS FRONT END ---
*	SDOS ENTRY POINTS, AND DATA SHARED WITH I/O PACKAGE
*		(SEE SDOS:XXX DISPLACEMENTS)
*
	ORG	CODE	BASE OF SDOS
	FCB	SDOSVERSION	FOR REFERENCE PURPOSES
LASTERROR	FDB	0	LAST ERROR ENCOUNTERED BY SDOS
	RMB	2	POINTER TO CONFIGURATION LIST
	RMB	2	TO IDENTIFY WHICH USER WE SOLD IT TO
*
*	THIS AREA CONTAINS THE VARIABLES THAT ARE PASSED
*	TO THE DRIVERS
*
	RMB	2	POINTER TO THE I/O CALL BLOCK
IOCBPOINTER	RMB	2	POINTER TO I/O CONTROL BLOCK SPECIFIED BY SYSCALL CHANNEL NUMBER
	RMB	3	CLOCK: 100THS OF A SECOND (0..100*60*60*24)
	RMB	3	CLOCK: DD/MM/YY (BCD)
	FCB	-1	STACKSWITCHED: -1 --> RUNNING UNDER TASK'S STACK
*			>= 0 --> RUNNING UNDER INTERRUPT STACK
	JMP	IOINTERRUPT	IN SIMPLE INTERRRUPT SCHEME, I/O INTERRUPT GOES HERE FIRST
	JMP	IORTI	RTI WITHOUT EVENT
	JMP	FORCESCHEDULE	RTI WITH EVENT
	JMP	CLOCKTICKED	ENTRY POINT FOR "CLOCKTICKED" SUBROUTINE
	FDB	USERTASKTCB	POINTER TO CURRENTLY ACTIVE TASK'S CONTROL BLOCK
	FCB	1	0 --> KILLABLE; <>0 --> UNKILLABLE (KILLPROOFFLAG)
*	INITIALLY KILLPROOF TO PREVENT "SDOSINIT" FROM BEING STOPPED
	JMP	KILLUSERPROGRAM	ENTRY POINT FOR "KILL USER PROGRAM" SUBROUTINE
	JMP	STARTIO$	(TASK) SIMULATE INTERRUPT TO (X)
	JMP	WAIT$	(TASK) WAIT FOR CONDITION (A,B)
	JMP	WAITEVENT$	(TASK) WAIT FOR EVENT (X)
	JMP	ERRET	ENTRY POINT FOR ERROR SIGNALING ROUTINE
	JMP	ERRORSAVE	ENTRY POINT TO SAVE ERROR CODE IN (X)
	JMP	ERRORED	ENTRY POINT TO SIGNAL "I CAN'T HANDLE THAT ERROR"
	JMP	CHECKRDLEN	ENTRY POINT FOR "CHECK REPLY DATA LENGTH"
	JMP	CHECKWRLEN	ENTRY POINT FOR "CHECK WRITE DATA LENGTH"
	JMP	CHECKSCLEN	ENTRY POINT FOR "CHECK SYSCALL BLOCK LENGTH"
	JMP	TABLEBRANCH	ENTRY POINT FOR "BRANCH THRU TABLE" ROUTINE
	JMP	BLOCKMOVE	ENTRY POINT FOR "FAST BLOCK MOVE"
	JMP	GETRUNNINGENCRYPTED	ENTRY POINT TO GET ADDRESS OF RUNNING ENCRYPTED FLAG
	RMB	SDOS:ENTRYSIZE-(*-CODE)
	PAGE
*	TEMPS FOR SDOS FRONT END
*
STARTADDRESS	RMB	2	EXECUTION START ADDRESS
LOADCOUNT	RMB	2	COUNTER FOR LOADER

DEFAULTDISKDCB	RMB	2	POINTER TO DISK INFO TABLE FOR DEFAULT DISK
NAMESCANPTR	RMB	2	USED TO SCAN USER-SUPPLIED DEVICE NAME
NAMESIZE	RMB	2	SIZE OF USER-SUPPLIED DEVICE NAME
CONSOLEDRIVER	RMB	2	POINTER TO CONSOLE DEVICE DRIVER (SET UP BY SDOSINIT)
DRIVERVPOINTER	RMB	2	POINTER TO DEVICE DRIVER ENTRY VECTOR
ERRORADDRESS	RMB	2	HOLDS LAST ERROR ADDRESS (CONVENIENCE FOR SYSTEMS PROGRAMMERS)
*
*	CLOCKTICK AND TIMEOUT ROUTINE DATA STORAGE
*
DAYSPERMONTHX	FDB	DAYSPERMONTH&$FF00	USED TO ACCESS DAYS PER MONTH GIVEN MONTH
NTIMEOUTBLKS	FCB	0	SET BY SDOSINIT TO # TIMEOUT BLOCKS
CLOCKTICKEDEVENT	FCB	0	# CLOCK TICKS FOR TIMEOUT TASK TO PROCESS
TIMEOUTLEPTR	RMB	2	TIMEOUT LIST ENTRY POINTER

DELAYBLOCK	; TIMEOUT BLOCK USED BY SYSCALL:DELAY ROUTINE
	FDB	CHANGED	to point to rest of timeout blocks (TIMEOUT:LINK)
	FDB	$FFFF	Timeout:fuse, initialized to 18.2 minutes
	FDB	SDOSNOTREGISTERED	WHERE TO GO IF SECRET DELAY CALL NOT ISSUED!

DELAYCOMPLETE	FCB	0	EVENT MARKING COMPLETION OF DELAY SYSCALL TIMEOUT
	PAGE
*
*	TIMEOUT AND USER TASK STUFF
*
	IF	M6800!M6801
	RMB	MINSTACK-CONTEXTBLOCK:SIZE	STACK SPACE FOR TIMEOUT TASK
TIMEOUTTASKSTKINIT	EQU	*-1	 INITIAL VALUE FOR TIMEOUT TASK'S STACK
	FCB	0,IGNORED,IGNORED,IGNORED,IGNORED	ZERO CC SO INTERRUPTS ARE ENABLED
	FDB	TIMEOUTTASKSETUP	INITIAL PC OF TIMEOUT TASK
	ELSE	(M6809)
	RMB	MINSTACK-CONTEXTBLOCK:SIZE	STACK SPACE FOR TIMEOUT TASK
TIMEOUTTASKSTKINIT	EQU	*	INITIAL VALUE FOR TIMEOUT TASK'S STACK
	FCB	$80,IGNORED,IGNORED,0	CC WITH INTS ENABLED, GARBAGE (D), (DP) = 0
	FDB	IGNORED	GARBAGE (X)
	FDB	IGNORED	GARBAGE (Y)
	FDB	IGNORED	GARBAGE (U)
	FDB	TIMEOUTTASKSETUP	INITIAL PC OF TIMEOUT TASK
	FIN

TIMEOUTTASKTCB	EQU	*
	FDB	USERTASKTCB	TCB:LNK POINTS TO USER TASK
	FDB	TIMEOUTTASKSTKINIT	TCB:STACK INITIAL VALUE
	RMB	TCB:SIZE-(*-TIMEOUTTASKTCB)

USERTASKTCB	; USER TASK CONTROL BLOCK
	FDB	0	END OF TCB CHAIN
	RMB	TCB:SIZE-(*-USERTASKTCB)
	PAGE	WORKING STORAGE FOR DISK FILE DRIVERS
*
*	VARIABLE STORAGE FOR DISK FILE DRIVER
*
*	SCRATCH PAD VARIABLES (USE TO SHORTEN/SPEED UP INSTRUCTIONS IN SDOS)
*
TOPOINTER	EQU	0	COPY TARGET
FROMPOINTER	EQU	2	COPY SOURCE
COUNT	EQU	4	1 BYTE COUNTER, GP

TEMP.MPCND	EQU	TEMPX	MULTIPLICAND
TEMP.DIVISOR	EQU	TEMPX	DIVISOR
TEMP.QUOTIENT	EQU	0	QUOTIENT (2 BYTES)
TEMP.DIVIDEND	EQU	2	DIVIDEND (2 BYTES)
TEMP.MPR	EQU	5	MULTIPLIER (1 BYTE)
*
*	HIGH RAM SCRATCH AREA
*
WRITEFLAG	RMB	1	0--> READ; 1--> WRITE REQUEST
BUFFERPOINTER	RMB	2	POINTER INTO USER'S BUFFER
TRANSFERCOUNT	RMB	2	# BYTES AVAILABLE TO BE TRANSFERRED FROM SECTOR
USEDCOUNT	RMB	2	# BYTES ACTUALLY TRANSFERRED
COLUMNCOUNT	RMB	1	COLUMN COUNTER
USERIOCBPOINTER	RMB	2	HOLDS REAL IOCBPOINTER DURING A SEARCHDIR
RDSIPOINTER	RMB	2	POINTER TO RDSI OF LAST SECTOR READ
FCBPOINTER	RMB	2	POINTER TO FCB FOR CURRENT IOCB
DIRENTRY	RMB	2	POINTER TO DIRECTORY ENTRY FOUND BY SEARCHDIR
DIRENTRYDISP	RMB	2	DISPLACEMENT FROM SECTORBASE OF DIRENTRY
*
SEARCHNAMEP	RMB	2	POINTER TO 16 BYTE FILENAME TO SEARCH FOR
SEARCHSAVEP	RMB	2	SAVED VALUE OF SEARCHNAMEP
SEARCHSTARTLSN	RMB	LSN:SIZE	STARTING LSN OF DIRECTORY SEARCH
FREEDIRLSN	RMB	LSN:SIZE	LSN OF FREE DIRECTORY SLOT
FREEDIRENTRYDISP	RMB	2	DISPLACEMENT FROM SECTORBASE OF FREE ENTRY
DIRSECTOREND	RMB	2	ADDRESS OF BYTE PAST END OF DIRECTORY SECTOR
*
FILENAMEBUF	RMB	DIR:NAMESIZE	BUFFER TO HOLD PARSED FILENAME
FILENAMEFILL	RMB	2	POINTER USED TO FILL FILENAMEBUF
FILENAMELENGTH	RMB	1	LENGTH OF FILENAME
FILESIZERQSTD	RMB	4	REQUESTED FILE SIZE IN BYTES
FILESIZE2	FCB	0,0,0,CHANGED	TEMP TO HOLD 32 BIT DIGIT
PARSECOUNT	RMB	2	UNUSED LENGTH OF NAME TO BE PARSED
FILEPROT	RMB	1	REQUESTED FILE PROTECTION BITS
*
ALLOCATIONCOUNT	RMB	LCN:SIZE	NUMBER OF CLUSTERS TO ALLOCATE TO A FILE
ALLOCATIONPLCN	RMB	LCN:SIZE	PREVIOUS LCN ALLOCATED TO FILE
DIVIDEPOINTLCN	RMB	LCN:SIZE	DIVIDES $DISKMAP FOR FWD/BKWD SEARCH
SEARCHLCN	RMB	LCN:SIZE	LCN OF CURRENT MAP BIT
AVAILABLELCN	RMB	LCN:SIZE	LCN OF FREE CLUSTER
AVAILABLECOUNT	RMB	LCN:SIZE	NUMBER OF FREE CLUSTERS STARTING AT AVAILABLELCN
DESIREDLCN	RMB	LCN:SIZE	VALUE OF LCN WHICH WE'D LIKE TO ALLOCATE
LCNGROUPLENGTH	RMB	LCN:SIZE	LENGTH OF GROUP OF FREE CLUSTERS WE'RE LOOKING AT
DISKMAPSECTORSTART	RMB	2	POINTER TO 1ST BYTE OF $DISKMAP SECTOR
DISKMAPSECTOREND	RMB	2	POINTER TO BYTE PAST END OF $DISKMAP SECTOR
DISKMAPBYTEPOINTER	RMB	2	POINTER TO $DISKMAP BYTE CONTAINING LCN BIT
DISKMAPMASK	RMB	1	MASK REPRESENTING BIT IN BYTE CONTTAINING LCN
*
LASTSECTORREADQ	; QUEUE OF RDSI'S IN LRU ORDER
	RMB	RDSI:SIZE	ONLY RDSI:FLINK,RDSI:BLINK ARE VALID
*
READFLAG	RMB	1	WHICH ENTRY: INVENTSECTOR OR READSECTOR
READSECTORDBP	RMB	2	POINTER TO SECTORDB BLOCK FOR READSECTOR
REMOVESECTORCNT	RMB	1	IOCB COUNTER FOR REMOVERDSI
REMOVEIOCBPTR	RMB	2	POINTER FOR REMOVERDSI
*
NRDSIS	RMB	1	# RDSI'S ACTUALLY SET UP (255 MAX)
NFCBS	RMB	1	NUMBER OF FCBS TO SEARCH (SEARCHFCBS)
NDRIVES	RMB	1	NUMBER OF DISKINFO TABLES
*
ERRFCBPOINTER	FDB	0	POINTER TO ERROR MESSAGE FCB
*	INIT AS ZERO IN CASE OF ERROR DURING INIT PROCESS
MAXSECTORSIZE	RMB	2	SIZE OF LARGEST SECTOR
SYSFCBPOINTER	RMB	2	POINTER TO SPECIAL SYSTEM FCB (USED BY DELETE)
SYSSECTORDB	RMB	SECTORDB:SIZE	SPECIAL SYSTEM SECTORDB
*
LOCATERDCNX	RMB	2	TEMP FOR LOCATERDCN
DCBPTRSAVE	RMB	2	SAVED COPY OF DCBPOINTER (USED BY DISKxxx)
*
LOADSYSCALLPTR	RMB	2	POINTER TO LOAD SYSCALL BLOCK
	PAGE	SDOS FRONT END VARIABLE STORAGE
SYSCALLX	FDB	OPCODES&$FF00	USED BY "SYSCALL$" TO JUMP INDEXED ON OPCODE
	ORG	(*+1)/2*2	ENSURES EVEN ADDRESS FOR SDOSSTACK
SDOSSTACK	RMB	2	POINTER TO BASE OF STACK USED BY SDOS
	RMB	2	THIS HOLE MUST FOLLOW SDOSSTACK
*			BECAUSE IT'S USED BY THE COPYRIGHT CHECK
USERSLASTERROR	RMB	2
EXITDAMAGEPTR	FCB	((EXITDAMAGE-22)/2)&$FF
	FCB	((EXITDAMAGE-22)/2)/256
DECBUF	RMB	3	CONVERSION BUFFER FOR DISPLAYERROR
ERRORNUMBER	RMB	2	THIS MUST FOLLOW DECBUF
STRINGBUFFER	RMB	6	ASCII BUFFER FOR DISPLAYERROR
LOGIOCB	RMB	IOCB:SIZE
SYSIOCB	RMB	IOCB:SIZE
*
*	TASKING SYSTEM VARIABLES
*
DONTSTOPME	FCB	0	0 --> OK TO SCHEDULE
*			1 --> IN TASK CRITICAL REGION; DON'T RESCHEDULE!
SURPRISE	RMB	1	<> 0 --> SCHEDULING DECISION IS OK
*
SCHEDX	RMB	2	TEMPORARY X STORAGE FOR SCHEDULING ROUTINES

*	THE FOLLOWING ARE SET UP BY SDOSINIT FROM THE CONFIGURATION TABLE
*
INTDISABLE	JMP	INTDISABLE	DISABLE ALL INTERRUPTS SUBROUTINE (PRESERVES (X))
INTENABLE	JMP	INTENABLE	ENABLE ALL INTERRUPTS SUBROUTINE (PRESERVES (X))
INTRTI	JMP	INTRTI	SET PRIORITY INTERRUPT MASK AND DO RTI (CODE)
INTERRUPTSTACK	RMB	2	COPY OF CNFG:INTERRUPTSTACK
JMPIOINT	JMP	JMPIOINT	JUMPS TO I/O DEVICE POLL CHAIN
*
*	STUFF NEEDED TO KILL USER PROGRAMS
*
KILLREQUESTEDF	FCB	1	0 --> KILL WAS REQUESTED WHILE UNKILLABLE
KILLEDF	FCB	1	0 --> ALREADY KILLED; 1--> NOT KILLED
SDOSRECURSIVECALL	FCB	0	0 --> SDOS NOT CALLED
*			> 0 --> SDOS CALLED, USERRETAPTR IS VALID
USERRETAPTR	RMB	2	USER PROGRAM'S STACK REGISTER CONTENTS ON ENTRY TO SDOS
*			POINTS TO HIS RETURN ADDRESS (USED BY "KILLUSERPROGRAM")
USERSYSCALLPTR	RMB	2	POINTS TO USER'S LAST SYSCALL BLOCK
	PAGE
*
*	INTERNAL SYSCALL CONTROL BLOCKS
*
CONSOLEOPEN	FCB	SYSCALL:OPEN	USED TO OPEN CONSOLE WHEN EOF HIT ON "DO" FILE
	FCB	OPEN:SCLEN
	FCB	0	CHANNEL
	FCB	IGNORED	2ND PARAMETER BYTE
CONSOLENAMEPTR	FDB	CHANGED	POINTER TO CONSOLE NAME (SET BY SDOSINIT)
CONSOLENAMELEN	FDB	CHANGED	LENGTH OF CONSOLE NAME (SET BY SDOSINIT)
	FDB	CHANGED	EXPECTED VALUE OF 2
	FDB	BUFFER	WHERE TO PUT # BYTES OF FILENAME USED
	FDB	2	SIZE OF EXPECTED RESPONSE
*
ACKCONTROLC	FCB	SYSCALL:STATUS	SYSCALL USED TO ACK ^C^C
	FCB	STATUS:SCLEN
	FCB	0,SC:GETLINEFLAGSHINT
	FDB	IGNORED,IGNORED
	FDB	CHANGED
	FDB	BUFFER,1	READ LINE FLAGS TO GET ^C ERROR
*
LOGOUT	FCB	SYSCALL:WRITEA	USED TO COPY CONSOLE OUTPUT TO LOG FILE
	FCB	WRITEA:SCLEN
	FCB	LOGCHANNEL	CHANNEL
	FCB	IGNORED	2ND PARAM
	FDB	CHANGED	BUFFER ADDRESS
	FDB	CHANGED	REQUESTED COUNT
	FDB	CHANGED	REPLY COUNT -- ALWAYS ZEROED
	FDB	IGNORED,IGNORED	REPLY BUFFER AND LENGTH
	FDB	CHANGED,CHANGED	= DESIRED FILE POSITION
*
DISPOUT	FCB	SYSCALL:WRITEA	USED TO WRITE ERROR NUMBER ON CONSOLE
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL
	FCB	IGNORED
	FDB	STRINGBUFFER	BUFFER ADDRESS
	FDB	CHANGED	REQUESTED COUNT
*
DISPREADB3	FCB	SYSCALL:READB	USED TO READ POINTER FROM $ERRORMSGS FILE
	FCB	READB:SCLEN
	FCB	SYSCHANNEL	CHANNEL
	FCB	IGNORED
	FDB	IGNORED,IGNORED	WRBUF,WRLEN
	FDB	CHANGED	REPLY LENGTH: EXPECTED VALUE OF 3
	FDB	DISPPOS1+1	BUFFER ADDRESS
	FDB	3	REQUESTED COUNT
DISPPOS1	; THESE 4 BYTES GET MODIFIED BY DISPREADB3, DISPUSERERROR
	FCB	0,CHANGED,CHANGED,CHANGED
*
DISPGETCHAR	FCB	SYSCALL:READA	USED TO READ ERROR MESSAGE FROM $ERRORMESSAGES
	FCB	READA:SCLEN
	FCB	SYSCHANNEL	CHANNEL
	FCB	0	LINE MODE FLAG: NOT LINE MODE
	FDB	IGNORED,IGNORED	WRBUF,WRLEN
	FDB	CHANGED	RPLEN: EXPECTED VALUE OF 1
	FDB	BUFFER	BUFFER ADDRESS
	FDB	1	REQUESTED COUNT
*
BUFFER	FDB	CHANGED,CHANGED	4 BYTE SCRATCH AREA
*
EXITCHAIN	; USED TO CHAIN TO "DEFAULTPROGRAM"
	FCB	SYSCALL:CHAIN
	FCB	CHAIN:SCLEN
	FCB	IGNORED,IGNORED	PARAMS
	FDB	DEFAULTPROG,DEFAULTPROGL	WRBUF,WRLEN
	FDB	CHANGED	RESULT SIZE: EXPECTED VALUE OF 4
	FDB	LOADFILEPOS,4	READ-BACK BUFFER DEFINITION
*
EXITSCLOSE	; USED BY EXITS TO CLOSE THE I/O CHANNELS
	FCB	SYSCALL:CLOSE
	FCB	CLOSE:SCLEN
	FCB	CHANGED	CHANNEL NUMBER TO BE CLOSED
*
IOBLOCK	; ADJUSTED I/O REQUEST GETS COPIED HERE
	RPT	SCBLK:DATA
	FCB	CHANGED
	PAGE
*	SDOS SAFEGUARD CODE
*	THIS CODE IS USED TO ENSURE THAT SD'S VERSION OF SERIALNUMBER.SYS...
*	WAS TRULY EXECUTED PROPERLY.  OUR VERSION PASSES A SECRET SIGNAL
*	TO "CHAIN" IN THE SYSCALL:PARAMS FEILD; THIS CODE IS PASSED TO
*	THIS ROUTINE IN THE (X) REGISTER.  ANY OTHER CODE PASSED IN (X)
*	WILL CAUSE SDOS TO EVENTUALLY BLOW UP (I.E., LONG TERM UNRELIABLE).
*	THIS CODE IS INTENDED TO BE EXTREMELY OBTUSE, TO PREVENT WOULD-BE
*	PIRATES FROM UNDERSTANDING (A DISASSEMBLY OF) IT, AND THEREFORE
*	BREAKING OUR PROTECTION.  WE HOPE IT WORKS!
*
	NOP		FORCE DISASSEMBLER IN SYNC WITH THE WRONG STUFF
	NOP
	NOP
	FCB	$F7	STAB EXTENDED OPCODE, TO KEEP DISASSEMBLER IN FALSE SYNC
	IF	M6800!M6801
SAFEGAURD	; ENTRY POINT TO SDOS SAFEGAURDING CODE
	FCB	$FF	STX EXTENDED ONTO ADDRESS PART OF INSTRUCTION...
	FDB	*	THEREBY SETTING UP SECRET MESSAGE AS EXECUTABLE CODE
SAFEGAURD1
	ROLA		RED HERRING INSTRUCTION
	LDX	#$3B3B	"RTI, RTI" OPCODES
	LDA	#$34	"DES" OPCODE
	STX	SAFEGAURD1	PLANT RTI, RTI AFTER SDOS ENABLING KEY
	STA	SAFEGAURD	PLANT "DES" AT SAFEGAURD ENTRY POINT
	RTS		CHAINING TO INITIALIZE.SYS OR DEFAULTPGM...
*	WILL ZERO USER SPACE BECAUSE OF INCOMPATIBLE DECRYPTION KEYS!
	PAGE
*	AFTER EXECUTION WITH "INS, RTS" SDOS ENABLING KEY IN (X),
*	THE SAFEGAURD CODE CONVERTS ITSELF INTO THE FOLLOWING:
*
*	SAFEGAURD
*		DES
*		INS
*		RTS
*		RTI
*		RTI
*		RTI
*		RTI
*
*	IF THE PROPER ENABLING KEY IS NOT GIVEN, THE "DES" ... "RTI" SEQUENCE
*	WILL BLOW UP ON SUCCESSIVE CALLS TO SAFEGAURD!
	ELSE	(M6809)
SAFEGAURD	; ENTRY POINT TO SDOS SAFEGAURDING CODE
	LDD	#$7F32	= BYTES OF "LEAS -1,S" REVERSED (WHEEE!)
	STX	*	STORE USER'S PURPORTED "(LEAS) 1,S/RTS"
	LDX	#$3B3B	"RTI","RTI" OPCODES
	STX	SAFEGAURD+5	PLANT "RTI","RTI"
	STD	SAFEGAURD+1	PLANT "    ...-1,S/LEAS ..."
	STB	SAFEGAURD	PLANT "LEAS..."
	RTS
	FIN
	page
LOADGETPOS	FCB	SYSCALL:STATUS	USED BY LOADER TO PERFORM A "SKIP N BYTES" LOADER COMMAND
	FCB	STATUS:SCLEN
	FCB	SYSCHANNEL
	FCB	SC:GETPOS
	FDB	IGNORED,IGNORED	WRBUF,WRLEN
	FDB	CHANGED	RPLEN: EXPECTED VALUE OF 4
	FDB	LOADFILEPOS	PLACE TO PUT LOAD FILE POSITION
	FDB	4	SIZE OF LOAD FILE POSITION BUFFER
*
LOADFILEPOS	; CURRENT POSITION OF LOAD FILE
	FCB	CHANGED,CHANGED,CHANGED,CHANGED
*
FILLDECRYPTBUFFER ; SYSCALL BLOCK TO READ 8 BYTES INTO DECRYPTBUFFER
	 FCB	 SYSCALL:READB,READB:SCLEN
	 FCB	 SYSCHANNEL,IGNORED
	 FDB	 IGNORED,IGNORED
	 FDB	 CHANGED	 EXPECTED VALUE IS 8
	 FDB	 DECRYPTBUFFER,8

LOADMULTIPLEOF8	 ; SYSCALL BLOCK TO LOAD MULTIPLE OF 8 BYTES
	 FCB	 SYSCALL:READB,READB:SCLEN
	 FCB	 SYSCHANNEL,IGNORED
	 FDB	 IGNORED,IGNORED
	 FDB	 CHANGED	 EXPECTED VALUE = OPTIMIZEDLOADCOUNT
LOADADDRESS ; THIS IS WHERE LOADER WILL PLACE NEXT LOADED BYTE
	 FDB	 CHANGED	 REPLY BUFFER
OPTIMIZEDLOADCOUNT ; THIS IS NUMBER OF BYTES TO LOAD
	 FDB	 CHANGED	 ALWAYS A MULTIPLE OF 8

GET1BYTE	; SYSCALL BLOCK USED TO READ 1 BYTE FROM SYSCHANNEL INTO BUFFER
	FCB	SYSCALL:READB,READB:SCLEN
	FCB	SYSCHANNEL,IGNORED
	FDB	IGNORED,IGNORED
	FDB	CHANGED	RPLEN: EXPECTED VALUE OF 1
	FDB	BUFFER,1	WHERE TO PLACE REPLY

DECRYPTBUFPTR FDB DECRYPTBUFFER POINTER TO SCAN DECRYPTBUFFER

DECRYPTBUFFER     ; THIS IS WHERE CHUNKS OF 8 BYTES GET READ FOR DECRYPTION
	fcb	0,1,2,3,4,5,6,7	Loader decryption area
DECRYPTBUFFEREND  ; END OF DECRYPT BUFFER

NKEYS	FCB	CHANGED	TYPE 5 RECORD: KEY COUNT

RUNNINGENCRYPTEDFLAG	FCB	1	HOLDS RUNNINGENCRYPTED FLAG IF MASK ROM
;			initially TRUE so VTDRIVER will not recognize ^D during boot process

OKTODECRYPTFLAG	FCB	CHANGED	1 --> OK TO DECRYPT THIS MODULE

OLDKEYENCRYPTED	; ENCRYPTED VERSION OF LAST DECRYPTION KEY USED
	FCB	0,0,0,0,0,0,0,0	SPACE TO HOLD KEY (8 BYTES)

NEWKEYENCRYPTED	; ENCRYPTED VERSION OF KEY USED TO LOAD THIS FILE
	FCB	0,0,0,0,0,0,0,0	SPACE TO HOLD 8 BYTE KEY
	page
*        Encryption Subroutine
*        Encrypts 8 data bytes pointed to by (X)...
*            according to 8 byte key stored in locations KEY
*        Result is stored as 8 byte string at (X)
*
*        Encryption algorithm is logically as follows:
*        (Some minor modifications make it fast enough to run on a 6800)
*            Result:=ValuetobeEncrypted
*            For bit=64 to 1 -- one iteration per bit
*                If MSB(Result) -- Inspect MSB of Result to decide what to do
*                Then
*                    -- Encrypt step: XOR with KEY
*                    Result:=(Result XOR KEY)*2+[MSB(Result) XOR MSB(KEY)]
*                Else
*                    -- Randomize step: XOR with Randomizing constant
*                    Result:=(Result XOR RNDIZER)*2+[MSB(Result) XOR MSB(KEY)]
*                Fi
*            Endloop -- Encrypted result is in Result
*
*       Decryption algorithm is logically as follows:
*            Result:=ValuetobeDecrypted
*            For bit=64 to 1 -- one iteration per bit
*                If LSB(Result) XOR MSB(KEY) -- Decrypt LSB to decide how undo
*                Then -- Decrypt step
*                     Result:=[INT(Result/2)+LSB(Result)*2^63] XOR KEY
*                Else -- "UnRandomize" step
*                     Result:=INT([Result XOR (Randomizer*2)]/2)
*                Fi
*             Endloop -- Decrypted result is in "Result"
	page
*
*        Randomizer constant is as follows (random 64 bit string):
*        To prevent loss of this string due to accidental damage to this text,
*        it is repeated 5 times here:
*        %0101010110100110000010101001110011100011010101111010110000111001
*        %0101010110100110000010101001110011100011010101111010110000111001
*        %0101010110100110000010101001110011100011010101111010110000111001
*        %0101010110100110000010101001110011100011010101111010110000111001
*        %0101010110100110000010101001110011100011010101111010110000111001
*
*        The Hex equivalent of the randomizer string is:
*
*        $55 A6 0A 9C E3 57 AC 39
*
*
*        KEY is a buffer of 8 bytes; it should be in page zero for speed!
*        We have chosen to put it inline in EORA #.. instructions instead
*        in the Decrypt routine.  Encrypt simply references the decrypt bytes.
*
*	NOTE: IT MIGHT BE POSSIBLE TO SPEED UP THIS CODE FOR 6801/6809!
	page
decrypt1	; do "decrypt step"
	staa	0,x
	ldaa	1,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key1	equ	*+1
	eora	#$16	$16 is red herring
	staa	1,x
	ldaa	2,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key2	equ	*+1
	eora	#$73	$73 is red herring
	staa	2,x
	ldaa	3,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key3	equ	*+1
	eora	#$F6	$F6 is red herring
	staa	3,x
	ldaa	4,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key4	equ	*+1
	eora	#$09	$09 is red herring
	staa	4,x
	ldaa	5,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key5	equ	*+1
	eora	#$56	$56 is red herring
	staa	5,x
	ldaa	6,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key6	equ	*+1
	eora	#$ED	$ED is red herring
	staa	6,x
	ldaa	7,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key7	equ	*+1
	eora	#$41	$41 is red herring
	staa	7,x
	decb		down count # iterations
	bne	decryptloop	b/ more to do
	jmp	decryptrts	all done, go clean up and exit!
	page
*	Decrypt -- Undoes Encrypt
*	Decryption key is stored inline in locations KEY0, KEY1, ... KEY7
*	Block of 8 bytes at (X) is decrypted in place
*
Decrypt ; Do the encryption in reverse, literally
*	ldab	#64	64 iterations of reverse encryption required
	ldab	key7	do 8 + 2 lsb of key iterations
	andb	#3
	addb	#8
	jsr	eorall8bytes	unscramble least significant byte
	staa	7,x	do first decryption iteration
decryptloop ; right shift current value and EOR with key
	rora		Shift encrypted "encrypt" bit into carry...
	ldaa	0,x	EOR with this byte of key
	rora		shift right, saving carry from left
Key0	equ	*+1
	eora	#$92	92 is red herring
SDOSCHKSUMBASE	; ALL CODE FROM HERE TO END OF SDOS IS CHECKSUMMED!!
	bmi	decrypt1	b/ encrypt bit was 1, go do "decrypt step"
decrypt0 ; "encrypt" bit was 0, right shift current value and EOR with randomizer
	eora	key0	undo "eora key" done by decryptloop
	anda	#$7F	force MSB to be zero
	eora	#$55	EOR with randomizer byte
	staa	0,x	(Note: MSB(Randomizer) must be zero)
	ldaa	1,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$A6
	staa	1,x
	ldaa	2,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$0A
	staa	2,x
	ldaa	3,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$9C
	staa	3,x
	ldaa	4,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$E3
	staa	4,x
	ldaa	5,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$57
	staa	5,x
	ldaa	6,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$AC
	staa	6,x
	ldaa	7,x	EOR with this byte of key
	rora		shift right, saving carry from left
	eora	#$39
	staa	7,x
	decb		down count # iterations
	bne	decryptloop	b/ more iterations to try
decryptrts
	jsr	eorall8bytes	now unscramble Most significant byte
	staa	0,x
	rts		decryption complete, result is in Result
	page
Eorall8bytes ; Compute XOR of all 8 bytes
	ldaa	4,x	Why are the index displacements all mixed up?
	eora	7,x	Because it puzzles the hell out of whoever...
	eora	5,x	attempts to dis-assemble it!
	eora	0,x
	eora	2,x
	eora	6,x
	eora	1,x
	eora	3,x	so Most sig byte of result depends on all 64 bits
	rts

	if	0	Encrypt is not needed by this version of SDOS
EncryptLSB ; Set MSB to XOR of all 8 bytes
*	think about this, someday...
*	Dennis Painter sez: put key just below (S) and run with
*	Interrupts disabled.	Then it cannot be damaged unless an
*	NMI occurs, which will erase it.
Encrypt	; And now it begins
*	ldab	#64	= # iterations to execute (too slow!)
	ldab	key+7	do 8 + 2 lsb of key iterations
	andb	#3
	addb	#8
	jsr	Eorall8bytes	scramble MSB
	staa	0,x	examine sign bit to decide what to do
	bpl	encryptrandomize b/ MSB is zero, go do randomize step
encryptstep ; MSB is one, perform encryption step
*	EOR current value with key, shift left
	ldaa	7,x	EOR this byte with key byte
	eora	key+7
	asla		shift left, forcing LSB=0
	staa	7,x
	ldaa	6,x	EOR this byte with key byte
	eora	key+6
	rola		shift left, saving carry from byte to right
	staa	6,x
	ldaa	5,x	EOR this byte with key byte
	eora	key+5
	rola		shift left, saving carry from byte to right
	staa	5,x
	ldaa	4,x	EOR this byte with key byte
	eora	key+4
	rola		shift left, saving carry from byte to right
	staa	4,x
	ldaa	3,x	EOR this byte with key byte
	eora	key+3
	rola		shift left, saving carry from byte to right
	staa	3,x
	ldaa	2,x	EOR this byte with key byte
	eora	key+2
	rola		shift left, saving carry from byte to right
	staa	2,x
	ldaa	1,x	EOR this byte with key byte
	eora	key+1
	rola		shift left, saving carry from byte to right
	staa	1,x
	ldaa	0,x	EOR this byte with key byte
	eora	key+0
	rola		shift left, saving carry from byte to right
	bcc	encryptstep0	b/ encrypted MSB is now zero
	inc	7,x	remember that encrypted MSB is a one
encryptstep0 ; encrypted MSB has been recorded in LSB of result
	decb		down count # iterations to perform
	beq	encryptrts	b/ done encrypting
	staa	0,x
	bmi	encryptstep	b/ new MSB is one, go do encrypt step
encryptrandomize ; MSB is zero, perform randomize step
	ldaa	7,x	EOR this byte with randomizer byte
	eora	#$39
	asla		shift left, forcing LSB to be zeroed
	staa	7,x
	ldaa	6,x	EOR this byte with randomizer byte
	eora	#$AC
	rola		shift left, saving carry from byte to right
	staa	6,x
	ldaa	5,x	EOR this byte with randomizer byte
	eora	#$57
	rola		shift left, saving carry from byte to right
	staa	5,x
	ldaa	4,x	EOR this byte with randomizer byte
	eora	#$E3
	rola		shift left, saving carry from byte to right
	staa	4,x
	ldaa	3,x	EOR this byte with randomizer byte
	eora	#$9C
	rola		shift left, saving carry from byte to right
	staa	3,x
	ldaa	2,x	EOR this byte with randomizer byte
	eora	#$0A
	rola		shift left, saving carry from byte to right
	staa	2,x
	ldaa	1,x	EOR this byte with randomizer byte
	eora	#$A6
	rola		shift left, saving carry from byte to right
	staa	1,x
	ldaa	key	EOR MSB of result with MSB of key
	anda	#$80	(Note: this requires that MSB(randomizer) be zero)
	eora	0,x	EOR MSB with MSB of key
	eora	#$55	and EOR rest with randomizer
	rola		shift left, saving carry from byte to right
	bcc	encryptrand0	b/ encrypted MSB=0
	inc	7,x	record that encrypted MSB is 1
encryptrand0 ; encrypted MSB is now saved in LSB of result
	decb		down count # iterations to perform
	beq	encryptrts	b/ done encrypting
	staa	0,x
	bpl	encryptrandomize b/ next bit is zero
	jmp	encryptstep

encryptrts ; encryption is complete
	staa	0,x	store MSB of final result
	jsr	eorall8bytes	scramble lsbyte
	staa	7,x
	rts
	fin
	PAGE	SDOS FRONT END CODE
SDOSMSG	FCB	ASCII:FF,ASCII:CR
	FCC	'SDOS Version '
	FCB	'0+((SDOSVERSION/$10)&$F)
	FCB	'.
	FCB	'0+((SDOSVERSION/$1)&$F)
	FCB	SDOSSUBREVISION
	FCC	', '
COPYRIGHTCHECKSUM	EQU	$36
COPYRIGHT	FCC	'Copyright (C) 1978 Software Dynamics'
	FCB	ASCII:CR
COPYRIGHTEND	EQU	*
	PAGE
*
*	SYSCALL MINIMUM SIZE, N-WAY OPCODE BRANCH TABLE
*
OPCODES	OPEN:SCLEN,#OPEN	OPEN FILE
	CREATE:SCLEN,#CREATE	CREATE A NEW FILE
	CLOSE:SCLEN,#CLOSE	CLOSE A FILE
	RENAME:SCLEN,#RENAME	RENAME A FILE
	DELETE:SCLEN,#DELETE	DELETE A FILE
	LOAD:SCLEN,#LOAD	LOAD A FILE INTO MEMORY
	CHAIN:SCLEN,#CHAIN	CHAIN TO A FILE
	CREATELOG:SCLEN,#CREATELOG	CREATE A FILE FOR OUTPUT ON LOG CHANNEL
	CLOSELOG:SCLEN,#CLOSELOG	CLOSE LOG OUTPUT FILE
	DISKDEFAULT:SCLEN,#DISKDEFAULT	SET DISK DEFAULT DEVICE
	READA:SCLEN,#READA	READ ASCII BYTES FROM A FILE
	READB:SCLEN,#READB	READ BINARY BYTES FROM A FILE
	WRITEA:SCLEN,#WRITEA	WRITE ASCII BYTES TO A FILE
	WRITEB:SCLEN,#WRITEB	WRITE BINARY BYTES TO A FILE
	CONTROL:SCLEN,#CONTROL	PERFORM A CONTROL OPERATION ON AN I/O CHANNEL
	STATUS:SCLEN,#STATUS	READ STATUS FROM AN I/O CHANNEL
	WAITDONE:SCLEN,#WAITDONE	WAIT FOR I/O CHANNEL OPERATION DONE
	EXIT:SCLEN,#EXIT	GIVE CONTROL BACK TO THE OPERATING SYSTEM
	ERROREXIT:SCLEN,#ERROREXIT	EXIT TO SYSTEM WITH ERROR CODE
	SETERROR:SCLEN,#SETERROR	REPORT AN ERROR TO THE SYSTEM
	GETERROR:SCLEN,#GETERROR	RETURN THE LAST ERROR CODE
	DISPERROR:SCLEN,#DISPLAYERROR	DISPLAY USER'S LAST ERROR
	KILLPROOF:SCLEN,#KILLPROOF	MAKE USER PROGRAM KILL-PROOF
	KILLENABLE:SCLEN,#KILLENABLE	MAKE USER PROGRAM KILLABLE
	DEBUG:SCLEN,#DEBUG	CALL SYSTEM DEBUGGER
	ATTNCHECK:SCLEN,#ATTNCHECK	OPERATOR ATTENTION CHECK
	ISCONSOLE:SCLEN,#ISCONSOLE	IS CHANNEL ZERO OPEN TO CONSOLE PREDICATE
	INTERLOCK:SCLEN,#INTERLOCK	OPERATE ON INTERLOCK OBJECT
	DELAY:SCLEN,#DELAY	DELAY FOR SPECIFIED NUMBER OF CLOCK TICKS
	READLUNNAME:SCLEN,#READLUNNAME	READ NAME OF LOGICAL UNIT
	GETSERIALNUMBER:SCLEN,#GETSERIALNUMBER	GET SERIAL NUMBER OF COMPUTER
OPCODEMAX	EQU	(*-OPCODES)/3-1
	PAGE
*	THE FOLLOWING POINTER CAN BE USED TO LOCATE THE SDOS:XXX TABLE
*	IT IS INTENDED TO BE USED IN DIAGNOSING SYSTEM FAILURES;
*	($FC,$FD) POINT TO I/O PAK, WHICH HAS 3 BYTE JMP TO SYSCALLENTRY
*	THE SDOS:XXX TABLE CONTAINS THE LAST ERROR DETECTED BY SDOS
*
	FDB	CODE	POINTER TO SDOS:XXX TABLE
*
SYSCALLENTRY	EQU	*
	LDAA	SDOSRECURSIVECALL	CHECK # TIMES SDOS HAS BEEN RECURSIVELY CALLED
	BNE	SYSCALL1	B/ NOT FIRST TIME SDOS WAS CALLED
	STS	USERRETAPTR	SAVE STACK SO WE CAN KILL USER PROGRAM IF NEEDED
	STX	USERSYSCALLPTR	SAVE POINTER TO USER'S SYSCALL
SYSCALL1	EQU	*
	STX	CODE+SDOS:IOBLOCKPTR
	LDAB	SCBLK:OPCODE,X	GET THE OPCODE
	CMPB	#OPCODEMAX	IS THE OPCODE LEGAL?
	BHI	ERRILLSYSCALL	B/ NO
	ASLB		MULTIPLY OPCODE VALUE BY 3
	ADDB	SCBLK:OPCODE,X	N*2+N = N*3
	IF	OPCODEMAX*3>>255
	+ERROR	OPCODE INDEXING WON'T WORK
	FIN
	STAB	SYSCALLX+1	LOCATE OPCODE TABLE SLOT CORRESPONDING TO OPCODE
	LDAA	SCBLK:WLEN,X	COPY SYSCALL BLOCK LENGTH TO (A)
	ANDA	#%01111111	MASK OFF WAIT FLAG
	CMPA	#SCBLK:RPLEN+1	DOES SYSCALL BLOCK INCLUDE SPACE FOR REPLY LENGTH ?
	BLS	SYSCALL2	B/ NO, LEAVE SCBLK:RPLEN ALONE
	CLR	SCBLK:RPLEN,X	YES, ZERO IT (CONVENIENCE FOR SYSCALL PROCESSORS)
	CLR	SCBLK:RPLEN+1,X
*	CHECK TO SEE IF SCBLK:RDBUF POINTS INTO SCRATCHPAD ?
SYSCALL2	EQU	*
*	CMPA	SCBLK:WLEN,X	COPY WAIT FLAG TO CARRY BIT
	LDX	SYSCALLX	GRAB POINTER TO OPCODE TABLE SLOT
	CMPA	OPCODES&$FF,X	IS SIZE OF SYSCALL BLOCK >= NECESSARY MINIMUM ?
	BCS	ERRSYSCALLTOOSHORT	B/ NO, YOU DIE!
	INC	SDOSRECURSIVECALL	NOW THAT USERRETAPTR IS VALID,...
*			LET KILLUSER PROGRAM KNOW ABOUT IT
	JSR	[(OPCODES&$FF)+1,X]	GO DO THE SYSCALL FUNCTION
	BCS	*+2	MAKE SURE WE GET CONTROL EVEN IF ERROR OCCURS
	DEC	SDOSRECURSIVECALL	DOWN COUNT # RECURSIVE CALLS TO SDOS
	BCS	ERRORED	B/ SOME ERROR DID HAPPEN, ABORT!
	RTS		RETURN TO CALLER
*
ERRILLSYSCALL	BSR	ERRET
	FDB	ERR:ILLEGALSYSCALL

ERRSYSCALLTOOSHORT	EQU	*
	BSR	ERRET
	FDB	ERR:SYSCALLTOOSHORT
	PAGE
*	SDOS ERROR HANDLING
*
*	ERRET -- CALLED VIA A JSR WITH IN-LINE 2-BYTE ERROR CODE
*		ERROR CODE IS LOADED INTO (X)
*		CARRY IS SET TO INDICATE ERROR CONDITION
*		AND A RTS IS PERFORMED
*		IF CALLING ROUTINE HAS NO BCS/BCC AFTER JSR,
*		ERRET AUTOMATICALLY POPS THE STACK ONE SUBROUTINE LEVEL
*		AND LOOKS AGAIN FOR A BCS
*		***NOTE: THIS MEANS TOP LEVEL (USER!) MUST HAVE BCC/BCS!***
*		This routine places error code in TEMPX while unwinding stack
*
ERRET	PULX		GET RETURN ADDRESS TO (X)
	STX	ERRORADDRESS	THIS HELPS WHEN WE'RE DEBUGGING SDOS
	LDX	,X	GET ERROR CODE INTO (X)
ERRORINX	; ENTRY POINT IF ERROR CODE IS ALREADY IN (X)
	STX	LASTERROR	SAVE THE ERROR CODE IN CASE OF CRASH
	STX	TEMPX	SAVE ERROR CODE IN RE-ENTRANT WAY
	BRA	ERRORED	SKIP INTO BCC/BCS STACK POP LOOP

ERRETL	; POP RETURN ADDRESS FROM THE STACK
	LEAS	2,S
ERRORED	; NOW FAKE "RTS" UNTIL BCC/BCS ENCOUNTERED
	LDX	0,S	GRAB RETURN ADDRESS
	LDAA	,X	DOES RETURN ADDRESS POINT TO BCS/BCC ?
	IF	M6800!M6801
*	SIMULATED LONG BRANCHES START WITH BCC/BCS!
	ELSE	(M6809)
	CMPA	#$10	EXTENDED OPCODE?
	BNE	ERRORED1	B/ NO
	LDAA	1,X	YES, GET 2ND BYTE OF OPCODE
ERRORED1	; (A) CONTAINS OPCODE TO INSPECT
	FIN
	ANDA	#\%1	(MASK OFF "INVERT BRANCH CONDITION" BIT)
	CMPA	#$24	(BCS OPCODE FOR 6800/6801 AND 6809)
	BNE	ERRETL	NO, SIMULATE "BCS TO A RTS"
	LDX	TEMPX	GET THE ERROR CODE AGAIN
	ERRORRTS	SET THE CARRY AND EXIT

ERRORSAVE	; SAVE THE ERROR CODE IN (X)
	STX	LASTERROR	SAVE CODE IN CASE OF CRASH
	STX	TEMPX	SAVE ERROR CODE IN RE-ENTRANT FASHION
	ERRORRTS	AND EXIT
	PAGE	SYSCALL:ISCONSOLE HANDLER
*
*	ISCONSOLE -- PERFORM "SYSCALL:ISCONSOLE"
*
ISCONSOLE	EQU	*
	LDX	CODE+SDOS:CONFIGURATION	FIND IOCB POINTER FOR CHANNEL 0
	LDX	[CNFG:IOCBPOINTERS,X]	(GET POINTER TO LIST OF IOCB POINTERS)
	LDX	IOCB:DRIVER,X	GET POINTER TO DRIVER FOR DEVICE
	BEQ	ERRCLOSED	B/ CHANNEL IS NOT OPEN
	CPX	CONSOLEDRIVER	OPEN TO CONSOLE DRIVER ?
	BEQ	ISCONSOLEOKRTS	B/ YES, TAKE "OK" EXIT
	BSR	ERRET	NO
	FDB	ERR:NOTOPENTOCONSOLE

ERRCLOSED	EQU	*
	BSR	ERRET
	FDB	ERR:CLOSED

ERRCHBUSY	EQU	*
	BSR	ERRET
	FDB	ERR:CHBUSY

ERRCHTOOBIG	EQU	*
	BSR	ERRET
	FDB	ERR:CHTOOBIG
	PAGE	CHANNEL NUMBER CHECKING/IOCB DETERMINATION
*	CHECKCHANNELOPEN -- CHECK THAT CHANNEL SPECIFIED BY SYSCALL IS OPEN
*	RETURNS POINTER TO IOCB FOR CHANNEL IN IOCBPOINTER
*	ALSO SETS UP DRIVERVPOINTER, DCBPOINTER FROM IOCB IF CHANNEL IS OPEN
*
CHECKCHANNELOPEN	EQU	*
	BSR	GETIOCBPOINTER	FIRST, LOCATE THE IOCB FOR THE DESIRED CHANNEL
	LDX	IOCB:DRIVER,X	GRAB POINTER TO DEVICE DRIVER FOR CHANNEL
	BEQ	ERRCLOSED	B/ CHANNEL IS NOT OPEN!
	STX	DRIVERVPOINTER	FOR LATER USE
	LDX	IOCBPOINTER	SET UP DCB POINTER FROM IOCB, ALSO
	LDX	IOCB:DCB,X
	STX	DCBPOINTER
	LDX	DRIVERVPOINTER	FOR CONVENIENCE OF CALLERS
ISCONSOLEOKRTS	EQU	*
	OKRTS
*
*	CHECKCHANNELCLOSED -- CHECK THAT CHANNEL SPECIFIED BY SYSCALL IS CLOSED
*	SETS IOCBPOINTER TO POINTER TO I/O CONTROL BLOCK FOR SPECIFIED CHANNEL
*
CHECKCHANNELCLOSED	EQU	*
	BSR	GETIOCBPOINTER	FIRST, LOCATE THE PROPER IOCB
CHECKCHANNELCL1	; ENTRY POINT FOR CREATELOG
	LDX	IOCB:DRIVER,X	GRAB POINTER TO DEVICE DRIVER
	BNE	ERRCHBUSY	B/ CHANNEL IS ALREADY BUSY!
	OKRTS
	PAGE
*
*	GETIOCBPOINTER -- SETS IOCBPOINTER USING SCBLK:PARAMS AS CHANNEL NUMBER
*	ALSO RETURNS IOCBPOINTER IN (X)
*
GETIOCBPOINTER	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	GRAB POINTER TO SYSCALL BLOCK
	LDAB	SCBLK:PARAMS,X	THIS IS WHERE ***ALL*** CHANNEL NUMBERS MUST BE!
	LDX	CODE+SDOS:CONFIGURATION	VALID CHANNEL NUMBER ?
	CMPB	CNFG:NIOCHANNELS,X	...?
	BCC	GETIOCB1	B/ NO!
	CLRA		MAKE CHANNEL NUMBER INTO 16 BIT QUANTITY
	ASLD		DOUBLE TO MAKE INDEX INTO CHANNEL POINTER TABLE
	ADDD	CNFG:IOCBPOINTERS,X	COMPUTE POINTER TO IOCB POINTER
	TDX		GRAB POINTER TO IOCB
	LDX	0,X
GETIOCBX	EQU	*
	STX	IOCBPOINTER
	OKRTS

GETIOCB1	; INVALID CHANNEL NUMBER FOUND
	LDAA	SDOSRECURSIVECALL	USER SYSCALL?
	DECA		(NESTING = 1?)
	BEQ	ERRCHTOOBIG	B/ YES, REALLY IS INVALID CHANNEL NUMBER
	LDX	#LOGIOCB	ASSUME LOG IOCB WAS MEANT
	CMPB	#LOGCHANNEL	ASSUMPTION CORRECT ?
	BEQ	GETIOCBX	B/ YES
	LDX	#SYSIOCB	NO, ALL OTHERS GET TURNED INTO SYSIOCB CHANNELS
	BRA	GETIOCBX
	PAGE
*
*	ISDIG -- IS (A) A DIGIT ?
*	RETURNS CARRY RESET IF YES
*	RETURNS CARRY SET IF NO
*	(A) LEFT UNDISTURBED
*
ISDIG	EQU	*
	CMPA	#'9	DIGIT ?
	BHI	ISDNO	B/ NOPE
	CMPA	#'0	...?
	RTS		WITH CARRY SET PROPERLY

ISDNO	EQU	*
	SEC		MEANING "NOPE..."
	RTS
*
*	ISALPHANUM -- IS (A) AN ALPHA OR NUMERIC CHARACTER ?
*	RETURNS CARRY RESET IF TRUE; LOWER CASE LETTER FOLDED INTO UPPER CASE
*	OTHERWISE, RETURNS CARRY SET
*	(A) IS UNDISTURBED
*
ISALPHANUM	EQU	*
	BSR	ISDIG	TRY FOR A DIGIT, FIRST...
	BCC	ISALPHANUMRTS	B/ ITS A DIGIT, GET OUT NOW!
	CMPA	#'Z+32	LOWER CASE ALPHA ?
	BHI	ISDNO	B/ NO, AND ITS NOT UPPER CASE, EITHER
	CMPA	#'A+32	...?
	BCS	ISALPHANUM1	B/ NO, MIGHT BE UPPER CASE
	ANDA	#%01011111	IS LOWER CASE, CONVERT TO UPPER CASE
ISALPHANUMRTS	; CHEAP EXIT
	RTS		WITH THE CARRY RESET

ISALPHANUM1	; MIGHT BE UPPER CASE
	CMPA	#'Z	...?
	BHI	ISDNO	B/ NOPE
	CMPA	#'A	...?
	RTS
	PAGE
*
*	FINDDRIVER -- CONVERT DEVICE NAME TO DRIVER ENTRY POINTER VECTOR ADDRESS
*		ALSO FILLS IN DCBPOINTER
*	SYSCALL POINTED TO BY FNAMESYSCALLPTR SELECTS FILENAME AND SIZE
*	RETURNS (X) POINTING TO DRIVER
*	SETS RDBUF TO LENGTH OF PREFIX OF USER-SUPPLIED FILENAME THAT MATCHES DEVICE NAME
*
FINDDRIVER	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	CHECK FOR DEVICE NAME GIVEN
	LDX	SCBLK:WRLEN,X	GET LENGTH OF FILENAME
	STX	TEMPX	SAVE AS LOOP COUNTER
	LDX	CODE+SDOS:IOBLOCKPTR	GET ADDRESS OF FILE NAME
	LDX	SCBLK:WRBUF,X
	STX	FROMPOINTER	SAVE STRING SCAN POINTER
FINDDRIVER1L	; CHECK NEXT CHARACTER TO SEE IF VALID DEVICE NAME
	LDX	TEMPX	GET REMAINING STRING COUNT
	BEQ	FINDDEFAULT	B/ NO BYTES LEFT, VALID DEVICE NAME NOT SEEN
	DEX		DOWN COUNT # BYTES REMAINING
	STX	TEMPX
	LDX	FROMPOINTER	GET BYTE FROM STRING
	LDA	,X+
	STX	FROMPOINTER
	BSR	ISALPHANUM	ALPHANUMERIC CHARACTER ?
	BCC	FINDDRIVER1L	B/ YES, CONTINUE SCAN FOR ":"
FINDDRIVER2	; END OF ALPHANUMERIC DEVICE NAME PREFIX FOUND
	CMPA	#':	MUST HAVE ":" OR A DEVICE NAME IS NOT SPECIFIED
	BNE	FINDDEFAULT	NO ":", MUST BE FILE NAME ON DEFAULT DISK
	PAGE
*	FOUND SOMETHING THAT LOOKS LIKE A DEVICE NAME
*
	LDX	CODE+SDOS:CONFIGURATION	FIRST, LOOK FOR A DISK DEVICE NAME
	LDX	CNFG:DISKDCBS,X
FINDDISK	EQU	*
	STX	DCBPOINTER	POINTER TO DISK INFO ENTRY
	LDX	DCB:NAME,X	GET POINTER TO DEVICE NAME STRING
	BSR	CMPDVNAME	COMPARE AGAINST FILENAME STRING
	BCC	FOUNDDISK	B/ THAT'S IT!
	LDX	DCBPOINTER	GET ADDRESS OF NEXT DISK INFO ENTRY
	LDX	DCB:NEXTDCB,X
	BNE	FINDDISK	B/ NOT END OF LIST
	LDX	DEFAULTDISKDCB	ASSUME DEFAULT DISK CHOSEN
	STX	DCBPOINTER	REMEMBER IT...
	LDX	#DEFAULTDISKNAME	CHECK TO MAKE SURE...
	BSR	CMPDVNAME	CHECK DEFAULT DISK NAME AGAINST GIVEN DEVICE NAME
	BCC	FOUNDDISK	B/ IT MATCHES, FAKE IT FROM HERE!
*
*	SEARCH THE NON-DISK LIST
*
	LDX	CODE+SDOS:CONFIGURATION
	LDX	CNFG:DEVICEDCBS,X	GET POINTER TO LIST OF NON-DISK DEVICES
FINDDEVICEL	; SEE IF THIS IS IT
	STX	DCBPOINTER	SAVE POINTER TO NON-DISK DCB
	LDX	DCB:DRIVER,X	GET DEVICE DRIVER ADDRESS IN CASE OF MATCH
	STX	DRIVERVPOINTER	AND SAVE
	LDX	DCBPOINTER	NOW COMPARE DEVICE NAME STRINGS
	LDX	DCB:NAME,X	GET ADDRESS OF NON-DISK DEVICE NAME
	BSR	CMPDVNAME	GO SEE IF DEVICE NAME IS IN LIST
	BCC	FOUNDDEVICE	B/ WE FOUND IT!
	LDX	DCBPOINTER	NO MATCH, FIND NEXT ENTRY IN DCB LIST
	LDX	DCB:NEXTDCB,X	END OF DCB LIST HIT ?
	BNE	FINDDEVICEL	B/ NO
	JSR	ERRET	NOT A VALID DEVICE NAME
	FDB	ERR:NOSUCHDEVICE
	PAGE
*
*	NO DEVICE NAME, ASSUME A FILE ON DEFAULT DISK DRIVE
*
FINDDEFAULT	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	SET NAMESIZE BACK TO USER-SUPPLIED SIZE
	LDX	SCBLK:WRLEN,X
	STX	NAMESIZE	(INDICATES 0 BYTES MATCHED A DEVICE NAME)
	LDX	DEFAULTDISKDCB	GET POINTER TO DEFAULT DISK INFO TABLE
	STX	DCBPOINTER
FOUNDDISK1	EQU	*
	LDX	#DISKFILEDRIVER	SET UP POINTER TO SDOS DISK FILE DRIVER
FOUNDDISK2	EQU	*
	STX	DRIVERVPOINTER	AND SAVE WHICH DISK DRIVER TO USE!
FOUNDDEVICE	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	COMPUTE LENGTH OF DEVICE PREFIX
	LDD	SCBLK:WRLEN,X	= USER SUPPLIED LENGTH - # OF BYTES REMAINING AFTER MATCH
	SUBD	NAMESIZE
	LDX	SCBLK:RDBUF,X	PUT SIZE OF SCANNED DEVICE NAME IN REPLY BUFFER
	STD	0,X
	RTS
*
FOUNDDISK	EQU	*
	LDX	NAMESIZE	NULL FILENAME?
	BEQ	FOUNDDISK3	B/ YES, HE SPECIFIED ONLY A DISK
	LDX	NAMESCANPTR	LOOK AT 1ST BYTE FOLLOWING DISK NAME
	LDAA	0,X
	CMPA	#ASCII:SPACE	A NON-BLANK PRINTING CHARACTER?
	BHI	FOUNDDISK1	B/ YES, FILENAME HAS BEEN SPECIFIED
FOUNDDISK3	; ONLY NAME OF DISK DEVICE SPECIFIED
	LDX	#DISKDEVICEDRIVER
	BRA	FOUNDDISK2

DEFAULTDISKNAME	FCC	"DISK:"
	FCB	0
	PAGE
*	CMPDVNAM -- COMPARE FILENAME TO DEVICE NAME
*	(X) POINTS TO DEVICE NAME ENDED BY 0
*	FILENAME TO MATCH AGAINST IS SELECTED BY SYSCALL
*	EXIT WITH CARRY RESET IF FOUND
*	NAMESCANPTR IS ADVANCED PAST DEVICE NAME IF FOUND
*	CARRY SET IF NOT FOUND
*	SETS NAMESIZE TO # BYTES OF FILENAME GIVEN MINUS SIZE OF DEVICE NAME PREFIX
*
CMPDVNAME ; COMPARE FILENAME TO DEVICE NAME
	STX	TEMPX	SAVE POINTER TO NAME IN DCB
	LDX	CODE+SDOS:IOBLOCKPTR	GET SIZE OF USER-SPECIFIED FILE NAME
	LDX	SCBLK:WRLEN,X
	STX	NAMESIZE
	BEQ	CMPDVNAMENF	B/ ZERO LENGTH --> NOT FOUND!
	LDX	CODE+SDOS:IOBLOCKPTR	GET POINTER TO USER-SPECIFIED FILE NAME
	LDX	SCBLK:WRBUF,X
	STX	NAMESCANPTR
CMPDVNAML	; COMPARE NEXT BYTE OF USER-SPECIFIED NAME WITH DEVICE NAME
	LDX	NAMESCANPTR	GET BYTE FROM USER-SPECIFIED NAME
	LDA	,X+
	STX	NAMESCANPTR
	CMPA	#$60	FOLD TO UPPER CASE IF LOWER CASE
	BLS	CMPDVNAML1	B/ NOT LOWER CASE
	CMPA	#$7A
	BHI	CMPDVNAML1	B/ NOT LOWER CASE
	ANDA	#%01011111	FOLD IT!
CMPDVNAML1 ; FOLDED CHARACTER OF USER SPECIFIED NAME IN (A)
	LDX	TEMPX	COMPARE TO NEXT BYTE OF DCB NAME
	CMPA	,X	CHARACTERS MATCH?
	BNE	CMPDVNAMENM	B/ NO, END OF DCB NAME ?
	INX		BUMP POINTER
	STX	TEMPX	UPDATE DCB NAME SCAN POINTER
	LDX	NAMESIZE	EXHAUSTED USER-SPECIFIED NAME?
	DEX		(REMEMBER THAT WE HAVE LOOKED AT ANOTHER BYTE)
	STX	NAMESIZE
	BNE	CMPDVNAML	B/ NO, COMPARE NEXT BYTE
CMPDVNAMEZ	; USER-SPECIFIED NAME IS EXHAUSTED
	LDA	[TEMPX]	AT END OF DCB-SPECIFIED NAME ?
	BNE	CMPDVNAMENF	B/ NO, USER NAME DOESN'T MATCH DCB NAME
	LDX	NAMESCANPTR	YES, GET POINTER TO UNSCANNED PART OF USER NAME
	OKRTS		AND SIGNAL 'MATCH!'

CMPDVNAMENF	; USER FILE NAME DOESN'T START WITH THIS DEVICE NAME
	ERRORRTS		SAY "NO MATCH"

CMPDVNAMENM	; NAME DOESN'T MATCH
	LDX	NAMESCANPTR	BACK UP THE SCAN POINTER
	DEX
	STX	NAMESCANPTR
	BRA	CMPDVNAMEZ
	PAGE	SYSCALL:INTERLOCK/SYSCALL:DELAY HANDLERS
*
*	INTERLOCK -- PERFORM SYSCALL:INTERLOCK
*	(EXECUTE A NO-OP SINCE SINGLE USER SYSTEMS CANNOT INTERLOCK!)
*
INTERLOCK
	JSR	CHECKWRLEN	MAKE SURE WRITE BUFFER IS SUPPLIED
	FDB	16	AND THAT IT IS THE PROPER LENGTH
	LDA	INTERLOCK:FUNCTION+1,X	GET INTERLOCK OPCODE
	CMPA	#IC:CREATE	A CREATE CALL?
	BNE	INTERLOCKOKRTS	B/ NO, JUST EXIT
	JSR	CHECKRDLEN	YES, MAKE SURE A REPLY BUFFER IS GIVEN
	FDB	16
INTERLOCKOKRTS
	OKRTS
*
*	DELAY -- PERFORM SYSCALL:DELAY
*
DELAY
	LDX	CODE+SDOS:IOBLOCKPTR	FETCH DELAY PERIOD
	LDX	DELAY:PERIOD,X
	BNE	DELAY1	B/ DON'T DEFUSE 12 HOUR DELAY
	LDX	#DELAYDONE	SECRET DELAY SIGNAL PASSED, DEFUSE 12 HOUR TIMEOUT BOMB
	STX	DELAYBLOCK+TIMEOUT:ROUTINE
	OKRTS

DELAY1	CLR	DELAYCOMPLETE	RESET DELAY COMPLETED EVENT
	STX	DELAYBLOCK+TIMEOUT:FUSE	SET FUSE TO DESIRED DELAY
	LDX	#DELAYCOMPLETE	NOW WAIT FOR DELAY COMPLETE EVENT
	JSR	WAITEVENT$
	OKRTS
*
*	SDOS NOT REGISTERED TIME BOMB
*
SDOSNOTREGISTERED
	LDX	#$FFFF	SET UP 18.2 MINUTE TIMEOUT AGAIN
	STX	DELAYBLOCK+TIMEOUT:FUSE
	LDA	DELAYCOMPLETE	HAVE 12 HOURS PASSED YET ?
	CMPA	#12*60//18	(= THIS MANY 18.5 MINUTE TIMEOUTS ?)
	BLS	DELAYDONE	B/ ...TICK... NOT YET
	LDX	#EXITCHAIN-9	*** BANG! **** MODIFY EXIT TO DISPLAY ERROR NEXT TIME
	LDD	#ERR:SDOSNOTREGISTERED+1206	WHAT TO DISPLAY ON NEXT EXIT
	SUBD	#1206	FIX UP NON-OBVIOUS CONSTANT TO CORRECT VALUE
	STD	ERROREXIT:CODE+9,X	MAKE "CHAIN DEFPGM" INTO "ERROREXIT"
	SUBD	#ERR:SDOSNOTREGISTERED-(SYSCALL:ERROREXIT*256+ERROREXIT:SCLEN)
	STD	SCBLK:OPCODE+9,X
DELAYDONE ; DELAY IS DONE, SIGNAL COMPLETION OF DELAY EVENT
	INC	DELAYCOMPLETE	MARK THE EVENT
	JMP	IORTI
	PAGE	MAJOR CHANNEL OPERATIONS
*	OPEN -- PERFORM "SYSCALL:OPEN"
*
OPEN	EQU	*
*
*	HERE'S WHERE WE DO THE COPYRIGHT CHECK
*
	LDX	#COPYRIGHTEND-1-98	COMPUTE CHECKSUM BACKWARDS
	LDAA	#(-COPYRIGHTCHECKSUM)&$FF
OPEN1	ADDA	98,X
	DEX
	CPX	#COPYRIGHT-98-1
	BNE	OPEN1
*
*	IF A USER CHANGED THE COPYRIGHT STRING CHECKSUM...
*	WE WILL COMPUTE AN ADDRESS CORRESPONDING TO SDOSSTACK
*	IF THE CHECKSUM IS OK, WE WILL COMPUTE AN ADDRESS
*	CORRESPONDING TO SDOSSTACK+2
*
*	INTO THIS MAGIC COMPUTED ADDRESS, WE WILL
*	PLACE THE MANUFACTURED ADDRESS OF EXITDAMAGE (A CRITICAL PLACE)
*	THE RESULT IS TO HAVE EXIT AND CHAIN LOAD UP A BAD STACK POINTER
*	WHICH WILL EVENTUALLY DESTROY EXITDAMAGE
*	WHICH WILL EVENTUALLY BRING DOWN THE SYSTEM (WE HOPE)
*	ALL OF THIS IS TO TRY TO PREVENT THEFT
*
	SUBA	#1	IF YOU DON'T UNDERSTAND THIS, IMAGINE A GUY DISASSEMBLING IT!
	LDAA	#0	USE THE Z-BIT TO FORM AN ADDRESS
	ADCA	#0	= 1 IF CHECKSUM OK, = 0 OTHERWISE
	CLRB
	ADDA	#((SDOSSTACK-34)/2)&$FF	THIS IS PURPOSELY DONE WIERD
	ADCB	#((SDOSSTACK-34)/2)/256	TO DEFEAT WOULD BE DISASSEMBLERS AND THEIVES
	ASLA
	ROLB
	PSHA
	PSHB
	LDD	EXITDAMAGEPTR
	ASLA		THIS CODE DOES FUNNY THINGS...
	ROLB		TO CONFUSE WOULD-BE DISASSEMBLERS AND THEIVES
	ADDA	#22
	ADCB	#0
	LDX	,S++
	STAA	35,X
	STAB	34,X
*
*	END OF COPYRIGHT CHECK
*
	JSR	FNAMEPROCESS	PROCESS THE FILE NAME TO GET THE DRIVER ADDRESS
	JSR	[DRIVER:OPEN,X]	GO OPEN THE FILE
	BCS	OPENERRED	B/ DRIVER GOT AN ERROR ON OPEN
OPEN2	EQU	*
	LDD	DRIVERVPOINTER	GET PTR TO DEVICE DRIVER VECTOR
	LDX	IOCBPOINTER
	STD	IOCB:DRIVER,X	SAVE IN OPEN CHANNEL TABLE
*
*	PLANTDCBINIOCB -- SET DCB POINTER INTO IOCB
*
PLANTDCBINIOCB	EQU	*
	LDX	IOCBPOINTER	GET IOCB ADDRESS
	LDD	DCBPOINTER	SAVE DCB ADDRESS IN IOCB, TOO!
	STD	IOCB:DCB,X
	CLC
OPENERRED
	RTS
	PAGE
*
*	CREATELOG -- PERFORM "SYSCALL:CREATELOG"
*
CREATELOG	EQU	*
	LDX	#LOGIOCB	SET UP IOCBPOINTER...
	STX	IOCBPOINTER	AS THOUGH "CHECKCHANNELCLOSED" HAD BEEN CALLED
	JSR	CHECKCHANNELCL1	MAKE SURE THAT LOG CHANNEL IS CLOSED
	BSR	FNAMEPROCESS1	TO FIND DRIVER ADDRESS
	BRA	CREATE1	AND GO CREATE THE FILE
*
*	CREATE -- PERFORM "SYSCALL:CREATE"
*
CREATE	EQU	*
	BSR	FNAMEPROCESS	PROCESS CHANNEL NUMBER AND FILE NAME
CREATE1	EQU	*
	JSR	[DRIVER:CREATE,X]	GO CREATE THE FILE
	BCC	OPEN2	OPEN THE CHANNEL
	RTS		OOPS, EXIT WITH ERROR
*
*	DELETE -- PERFORM "SYSCALL:DELETE"
*
DELETE	EQU	*
	JSR	FNAMEPROCESS1	GO PARSE FILENAME TO DETERMINE DRIVER ADDRESS
	JMP	[DRIVER:DELETE,X]	NOW DELETE THE FILE
*
*	READB -- PERFORM "SYSCALL:READB"
*
READB	JSR	CHECKCHANNELOPEN
	JMP	[DRIVER:READB,X]	INVOKE THE DRIVER TO DO THE WORK
*
*	WRITEB -- PERFORM "SYSCALL:WRITEB"
*
WRITEB	JSR	CHECKCHANNELOPEN
	CPX	CONSOLEDRIVER	DON'T WRITE IF CHANNEL 0 AND NOT CONSOLE!
	BEQ	WRITEB1	B/ OK TO WRITE
	LDX	CODE+SDOS:IOBLOCKPTR	ISN'T CONSOLE, IS THIS CHANNEL ZERO ?
	LDA	WRITEB:CHANNEL,X	...?
	BEQ	WRITEBOKRTS	B/ YES, DON'T DO THE WRITE!
	LDX	DRIVERVPOINTER	NOPE, MUST DO THE WRITE
WRITEB1
	JMP	[DRIVER:WRITEB,X]
	PAGE
*	FNAMEPROCESS -- CHECK TO MAKE SURE CHANNEL IS CLOSED
*	LOCATE DRIVER AND STRIP OFF DEVICE NAME
*	BUILDS COPY OF SYSCALL IN IOBLOCK
*	EXCEPT THAT FILENAME POINTER HAS BEEN STEPPED PAST DEVICE NAME PREFIX
*	ASSUMES THAT A FILENAME SYSCALL WITH SCLEN=SCLEN:DATA WAS PASSED BY USER
*
FNAMEPROCESS	; ENTRY POINT FOR OPEN/CREATE
	JSR	CHECKCHANNELCLOSED	AND LOCATE THE IOCB FOR THE SPECIFIED CHANNEL
FNAMEPROCESS1	; ENTRY POINT FOR OTHER FILENAME SYSCALLS
	JSR	CHECKRDLEN	MAKE SURE READ-BACK BUFFER HAS AT LEAST 2 BYTES
	FDB	2	TO PLACE SIZE OF FILENAME INTO
	JSR	FINDDRIVER	SPECIFIED BY FILENAME STRING
	LDX	DRIVERVPOINTER	FOR CONVENIENCE OF EVERYBODY ELSE
WRITEBOKRTS
	OKRTS		AND EXIT
	PAGE
*	DISKDEFAULT -- PERFORM "SYSCALL:DISKDEFAULT"
*
DISKDEFAULT	EQU	*
	BSR	FNAMEPROCESS1	GO PROCESS FILE NAME TO GET DEVICE NAME
	CPX	#DISKDEVICEDRIVER	THIS IS ONLY LEGAL DRIVER THAT CAN RESULT
	BNE	ERRMUSTBEDISK	B/ SPECIFIED OTHER THAN DISK DEVICE
	LDX	DCBPOINTER	USE THIS DISK INFO TABLE FOR DEFAULT FROM NOW ON
	STX	DEFAULTDISKDCB	!
	OKRTS		HOW EASY!
CLOSERTS	EQU	*-1

ERRMUSTBEDISK	EQU	*
	JSR	ERRET
	FDB	ERR:MUSTBEDISK
*
*	CLOSE -- PERFORM "SYSCALL:CLOSE"
*
CLOSE	JSR	CHECKCHANNELOPEN
	JSR	[DRIVER:CLOSE,X]
	BCC	CLOSE1	B/ NO ERROR
	JSR	ERRORSAVE	GO SET THE ERROR
*	THE FOLLOWING CODE DOES NOT FIDDLE THE CARRY BIT
*	DON'T FOOL AROUND WITH IT, OR ELSE...
CLOSE1	LDX	IOCBPOINTER
	LDAA	#0
	STAA	IOCB:DRIVER,X
	STAA	IOCB:DRIVER+1,X
	BCC	CLOSERTS	(OKRTS)
	JMP	ERRORED
*
*	CLOSELOG -- PERFORM "SYSCALL:CLOSELOG"
*
CLOSELOG	LDX	#LOGCLOSE
	JMP	SYSCALLENTRY
*
LOGCLOSE	FCB	SYSCALL:CLOSE	FUNCTION
	FCB	CLOSE:SCLEN
	FCB	LOGCHANNEL	CHANNEL
	PAGE
*
*	RENAME -- PERFORM "SYSCALL:RENAME"
*
RENAME	EQU	*
	JSR	CHECKCHANNELOPEN	OR YOU'RE DEAD!
	JSR	FNAMEPROCESS1	TO FIND OUT THE DEVICE DRIVER
	LDX	IOCBPOINTER	RENAME TO SAME DEVICE ?
	LDX	IOCB:DRIVER,X	...?
	CPX	DRIVERVPOINTER	...?
	BNE	ERRRENAMEDEVICE	B/ TO DIFFERENT DEVICE, I CAN'T ALLOW THAT...
	CPX	#DISKFILEDRIVER	IS THIS THE DISK FILE DRIVER ?
	BNE	RENAME1	B/ NO
	LDX	IOCBPOINTER	RENAME A FILE ON ONE DISK TO ANOTHER DISK ?
	LDX	IOCB:DCB,X	...?
	CPX	DCBPOINTER	...?
	BNE	ERRRENAMEDEVICE	B/ YES, THAT'S ILLEGAL TOO!
RENAME1	EQU	*
	LDX	DRIVERVPOINTER	GO DO THE RENAME
	JMP	[DRIVER:RENAME,X]

ERRRENAMEDEVICE	EQU	*
	JSR	ERRET
	FDB	ERR:RENAMEDEVICE
	PAGE
READAEOF	; RE-CAUSE EOF ERROR
	JSR	ERRET
	FDB	ERR:EOFHIT
*
*	READA -- PERFORM "SYSCALL:READA"
*
READA	JSR	CHECKCHANNELOPEN
	JSR	[DRIVER:READA,X]	ISSUE THE READ
	LBCC	READALOG	B/ ALL OK, SEE IF WE LOG IT
	PSHX		SAVE THE ERROR CODE
	JSR	READALOG	LOG WHAT WAS COLLECTED SO FAR
	BCS	*+2	WE CAN ONLY RECOVER FROM 1 ERROR, LOSE THE 2ND
	PULX		GET ORIGINAL ERROR CAUSE
	CPX	#ERR:EOFHIT	DID WE HIT EOF?
	LBNE	ERRORINX	B/ NO, SOME OTHER ERROR
	LDX	USERSYSCALLPTR	EOF ON CHANNEL 0 ?
	LDAA	READA:CHANNEL,X	(GRAB THE CHANNEL NUMBER)
	BNE	READAEOF	B/ NO, EOF ON A NON-ZERO CHANNEL!
	JSR	ISCONSOLE	IS CHANNEL ZERO OPEN TO THE CONSOLE?
	BCC	READAEOF	B/ YES, PASS EOF ERROR BACK TO CALLER
	LDX	#CLOSECHANNEL0	CLOSE CHANNEL 0
	JSR	SYSCALLENTRY
	LDX	LOGIOCB+IOCB:DRIVER	IS LOG CHANNEL OPEN ?
	BEQ	READA2	B/ NO, LEAVE IT ALONE!
	CPX	CONSOLEDRIVER	YES, OPEN TO THE CONSOLE ?
	BNE	READA2	B/ NO, LEAVE IT ALONE!
	JSR	CLOSELOG	YES, CLOSE THE LOG FILE!
READA2	EQU	*
	JSR	OPENCONSOLE	RE-OPEN CHANNEL 0 TO THE CONSOLE
*
*	BUILD NEW "READA" REQUEST IN "IOBLOCK" TO...
*	READ REMAINDER OF INPUT REQUEST
*
	LDX	#SYSCALL:READA*256+READA:SCLEN	SET UP SYSCALL OPCODE AND LENGTH
	STX	IOBLOCK+SCBLK:OPCODE
	LDX	USERSYSCALLPTR	COMPUTE NEW BUFFER POINTER
	LDD	READA:ACTUALCOUNT,X
	ADDD	READA:BUFFERP,X
	STD	IOBLOCK+READA:BUFFERP
	LDD	READA:MAXCOUNT,X	COMPUTE NEW MAX COUNT
	SUBD	READA:ACTUALCOUNT,X
	STD	IOBLOCK+READA:MAXCOUNT
	LDAA	READA:LMFLAG,X
	LDX	#IOBLOCK
	STAA	READA:LMFLAG,X
	CLR	READA:CHANNEL,X
	CLR	READA:ACTUALCOUNT,X	ZERO THE ACTUAL # BYTES MOVED
	CLR	READA:ACTUALCOUNT+1,X
	STX	CODE+SDOS:IOBLOCKPTR
	LDX	DRIVERVPOINTER	STILL VALID BECAUSE OF OPENCONSOLE
	JSR	[DRIVER:READA,X]	ISSUE READ TO CONSOLE
	LBCS	ERRORINX	ERROR IN FINISHING THE READ!
READA1	LDX	USERSYSCALLPTR	DOCTOR UP THE ORIGINAL I/O BLOCK
	LDD	READA:ACTUALCOUNT,X	(ADJUSTS THE # BYTES XFERRED BY 2ND AND LATER "READAS"...
	ADDD	IOBLOCK+READA:ACTUALCOUNT	DONE BECAUSE OF "EOFHIT"S ON CHANNEL ZERO)
	STD	READA:ACTUALCOUNT,X
*
*	LOG THE CHANNEL ZERO ACTIVITY
*
READALOG	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	SET UP WRITE BUFFER DATA...
	LDD	READA:ACTUALCOUNT,X	FOR LOG SYSCALL BLOCK...
	LDX	READA:BUFFERP,X	IN CASE WE ARE LOGGING
WRITEALOG	; ENTRY POINT FOR WRITEA TO HANDLE LOGGING
	STD	LOGOUT+WRITEA:COUNT	SAVE LENGTH OF STUFF TO WRITE TO LOG
	STX	LOGOUT+WRITEA:BUFFERP	SAVE LOCATION OF DATA TO LOG
	BSR	DOWELOG	OUTPUT DUPLICATE OF READ/WRITE DATA TO LOG DEVICE ?
	BCC	READARTS	B/ NOT LOGGING
	LDX	CODE+SDOS:IOBLOCKPTR	SET UP TO WRITE TO LOG DEVICE
	LDA	SCBLK:WLEN,X	COPY LENGTH OF SYSCALL BLOCK
	ANDA	#$7F	MASK OFF "NO WAIT" BIT
	CMPA	#RWPOSITION:SCLEN	LONGER THAN LONGEST REASONABLE ?
	BLS	READALOG0	B/ NOPE
	LDA	#RWPOSITION:SCLEN	YES, SHORTEN TO SIZE SET ASIDE FOR LOG BLOCK
READALOG0
	STA	LOGOUT+SCBLK:WLEN	SET SIZE OF LOG REQUEST
	LDD	RW:POSITION,X	COPY POSITION FROM ORIGINAL BLOCK...
	STD	LOGOUT+RW:POSITION	IN CASE IT WAS SUPPLIED
	LDX	RW:POSITION+2,X
	STX	LOGOUT+RW:POSITION+2
	LDX	#LOGOUT	SEND COPY TO LOG CHANNEL
	JMP	SYSCALLENTRY

CLOSECHANNEL0	FCB	SYSCALL:CLOSE
	FCB	CLOSE:SCLEN
	FCB	0	CHANNEL
	PAGE
*	CHECK IF WE ARE LOGGING
*	C SET IF YES, CLEAR IF NO
*
DOWELOG	LDX	CODE+SDOS:IOBLOCKPTR
	IF	READA:CHANNEL#WRITEA:CHANNEL
	?DOWELOG WON'T WORK?
	FIN
	LDAA	READA:CHANNEL,X	IS IT CHANNEL 0?
	BNE	DONTLOG	B/ NO, DON'T LOG
	LDX	LOGIOCB+IOCB:DRIVER	IS THE LOG FILE OPEN?
	BEQ	DONTLOG	B/ NO, DON'T LOG
	SEC		"WE ARE LOGGING"
	RTS
DONTLOG	CLC		"WE ARE NOT LOGGING"
READARTS	RTS
	PAGE
*
*	WRITEA -- PERFORM "SYSCALL:WRITEA"
*
WRITEA	JSR	CHECKCHANNELOPEN
	CPX	CONSOLEDRIVER	DON'T WRITE IF IS CHANNEL 0 AND IS NOT CONSOLE!
	BEQ	WRITEA0	B/ IS CONSOLE, IS ALWAYS OK TO WRITE
	LDX	CODE+SDOS:IOBLOCKPTR	NOT CONSOLE, IS THIS CHANNEL 0 ?
	LDA	WRITEA:CHANNEL,X	...?
	BEQ	WRITEA1	B/ YES, DON'T DO WRITE, WOULD DAMAGE "DO" FILE!
	LDX	DRIVERVPOINTER	MUST DO WRITE, GET POINTER TO DRIVER VECTOR
WRITEA0
	JSR	[DRIVER:WRITEA,X]	GO DO THE WRITE
	BCS	READARTS	B/ ERROR, GIVE UP NOW!
WRITEA1	LDX	CODE+SDOS:IOBLOCKPTR	SET UP TO HANDLE LOGGING
	LDD	WRITEA:COUNT,X	GET NUMBER OF BYTES TO WRITE
	LDX	WRITEA:BUFFERP,X	AND WHERE TO WRITE THEM FROM
	BRA	WRITEALOG	GO SEE IF WE SHOULD LOG THIS DATA
	PAGE	MISCELLANEOUS SYSCALLS
*
*	CONTROL -- PERFORM "SYSCALL:CONTROL"
*
CONTROL	EQU	*
	BSR	STATUSCONTROL	DO ALL THE COMMON WORK
	JMP	[DRIVER:CONTROL,X]	CALL THE DRIVER
*
*	STATUSCONTROL -- COMMON SUBROUTINE FOR "CONTROL", "STATUS" SYSCALLS
*	IF OPERATION IS ON CHANNEL 0, IT IS AIMED AT LOG CHANNEL IF LOG IS OPEN
*	SETS UP (A) TO CONTAIN THE ACTUAL STATUS (CONTROL) REQUEST CODE
*	RETURNS (X) = DRIVERVPOINTER
*
STATUSCONTROL	EQU	*
	JSR	CHECKCHANNELOPEN	OR WE CAN'T DO ANYTHING AT ALL!
	BSR	DOWELOG	LOGGING ?
	BCC	STATUSCONTROL1	B/ NOT LOGGING
	STX	DRIVERVPOINTER	LOGGING, USE LOGIOCB'S DRIVER...
	LDX	#LOGIOCB	SET EVERYTHING ELSE UP...
	STX	IOCBPOINTER
	LDX	IOCB:DCB,X	AS THOUGH "OPEN" ON THE LOG CHANNEL...
	STX	DCBPOINTER	HAD BEEN PERFORMED
STATUSCONTROL1	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	GET STATUS REQUEST CODE TO (A)
	LDAA	STATUS:CODE,X
	LDX	DRIVERVPOINTER	GET DRIVER ADDRESS TO (X), AS PROMISED
	OKRTS
*
*	STATUS -- PERFORM "SYSCALL:STATUS"
*
STATUS	EQU	*
	BSR	STATUSCONTROL	DO COMMON WORK
	JMP	[DRIVER:STATUS,X]	CALL THE DEVICE DRIVER
	PAGE
*	DEBUG -- PERFORM "SYSCALL:DEBUG"
*	ASSERT: SDOSRECURSIVECALL=1 (I.E., SDOS ITSELF DOES NOT INVOKE THIS!)
*
DEBUG	LDX	CODE+SDOS:CONFIGURATION
	LDX	CNFG:DEBUGGER,X	IS THERE A DEBUGGER?
	BEQ	DEBUG1	B/ NOPE
	LEAS	2,S	POP RETURN ADDRESS PUSHED BY "SYSCALL2"
	DEC	SDOSRECURSIVECALL	FLAG "NOT IN SDOS"
	JMP	,X	NOW ENTER THE DEBUGGER (RETURN VIA RTS)

DEBUG1	EQU	*
	LDX	#ERR:NODEBUGGER	YOU GUESSED IT!
	JMP	EXIT1	GO STICK USER WITH THIS ERROR
*
*	ATTNCHECK -- PERFORM "SYSCALL:ATTNCHECK"
*
ATTNCHECK	LDX	CODE+SDOS:CONFIGURATION	DOES OKRTS IF OPERATOR INTERVENTION NO REQUIRED
	JMP	[CNFG:ATTNCHECK,X]
*
*	SETERROR -- PERFORM "SYSCALL:SETERROR"
*
SETERROR	LDX	CODE+SDOS:IOBLOCKPTR
	LDX	SETERROR:CODE,X
	STX	USERSLASTERROR
WAITDONE	; FOR NOW, SINCE IT'S NOT REALLY IMPLEMENTED
	OKRTS
*
*	GETERROR -- PERFORM "SYSCALL:GETERROR"
*
GETERROR	EQU	*
	JSR	CHECKRDLEN	MAKE SURE READ BUFFER IS LARGE ENOUGH
	FDB	2	2 BYTES OF ERROR CODE TO READ BACK
	LDD	USERSLASTERROR	COPY ERROR CODE TO READ BUFFER
	STD	[SCBLK:RDBUF,X]
	OKRTS
	PAGE	ERROR DISPLAY
*	DISPLAYERROR -- DISPLAY "USERSLASTERROR" ON THE CONSOLE...
*	AS EITHER AN ERROR STRING FROM THE $ERRORMESSAGES FILE
*	OR AS "ERROR ddddd" IF WE CAN'T GET TO $ERRORMESSAGES SOMEHOW
*	IF WE CANNOT PRINT THE ERROR MESSAGE, CROAK AND DIE!
*	(EXAMINE "LASTERROR" IN SDOS:XXXX TO FIND OUT WHY)
*
DISPLAYERROR	LDX	USERSLASTERROR	DO IT NOW, IN CASE OF ERROR WHILE
	STX	ERRORNUMBER	PROCESSING THIS ERROR
DISPLAYERRORRETRY ; TRY TO DISPLAY ERROR AGAIN
	JSR	OPENCONSOLE	MAKE SURE WE CAN OUTPUT THE ERROR MESSAGE
	BCC	DISPLAYERRORA	B/ NO ERROR
	CPX	#ERR:CHBUSY	IS CHANNEL ALREADY OPEN ?
	BNE	*	LOOP IF NOT, CAN'T RECOVER!
DISPLAYERRORA	EQU	*
	JSR	DSKFOPENERRORFILE	TRY TO OPEN THE "ERRORMSGS.SYS" FILE
	BCS	DISPLAYERROR4A	B/ CAN'T OPEN, GO DISPLAY ERROR IN NUMERIC FORMAT
*	NOW THE ERRORMSGS.SYS FILE IS OPEN!
*
*	POSITION TO THE ERROR MESSAGE POINTER
*	= 3 * ERROR NUMBER
*
	LDX	#DISPPOS1
	CLR	1,X	(0,X IS ASSEMBLED AS A ZERO!)
	LDD	ERRORNUMBER
	ASLD		*2
	ROL	1,X
	ADDD	ERRORNUMBER	*3
	BCC	DISPLAYERROR0
	INC	1,X
DISPLAYERROR0	STD	2,X
	LDX	#DISPPOS	NOW POSITION ERRORMSGS.SYS FILE...
	JSR	SYSCALLENTRY	TO THE ERROR MESSAGE STRING POINTER
	BCS	DISPLAYERROR4	B/ OOPS, SOME PROBLEM OCCURRED
	LDX	#DISPREADB3	READ IN THE STRING ADDRESS
	JSR	SYSCALLENTRY
	BCS	DISPLAYERROR4
	LDX	DISPPOS1+2	IS THE ERROR MSG IN THE FILE?
	BNE	DISPLAYERROR1A	B/ YUP
	LDAA	DISPPOS1+1	...?
	BEQ	DISPLAYERROR4	B/ NO, PRINT THE ERROR CODE THE HARD WAY
DISPLAYERROR1A	EQU	*
	LDX	#DISPPOS	POSITION TO THE STRING
	JSR	SYSCALLENTRY
	BCS	DISPLAYERROR4	B/ ERROR
*
*	COPY THE STRING TO CHANNEL 0
*
DISPLAYERROR1	LDX	#DISPGETCHAR
	JSR	SYSCALLENTRY
	BCS	DISPLAYERROR4
	LDAA	BUFFER
	CMPA	#ASCII:CR
	BEQ	DISPLAYERRORDONE	B/ I'M DONE!
	LDX	#DISPPUTCHAR	NOT DONE, OUTPUT THIS CHARACTER TO CONSOLE
	JSR	SYSCALLENTRY
	BCC	DISPLAYERROR1	B/ NO ERROR, GO OUTPUT NEXT CHARACTER
DISPERRKILLLOG	; PERHAPS ERROR IS CAUSED BY LOG DEVICE
	BSR	DISPLAYERRORDONE	CLOSE ERRORMSGS.SYS FILE TO KEEP THINGS NEAT
	JSR	CLOSELOG	GET RID OF POTENTIAL SOURCE OF PROBLEM
	BCC	DISPERRKILLLOG1	B/ MANAGED TO CLOSE LOG FILE!
	CPX	#ERR:CLOSED	FAIL BECAUSE ALREADY CLOSED ?
	BEQ	*	B/ YES, SITUATION IS HOPELESS...
DISPERRKILLLOG1	; MANAGED TO CLOSE THE LOG FILE (PERHAPS WITH ERROR)
	LDX	#CLOSECHANNEL0	TURN OFF THE "DO" FILE
	JSR	SYSCALLENTRY
	BCS	*	B/ BUT IT WAS OPEN A MINUTE AGO!!!???
	BRA	DISPLAYERRORRETRY
*
*	CLOSE THE ERROR FILE (I'M DONE & ALL IS WELL)
*
DISPLAYERRORDONE	LDX	#DISPCLOSE
	JSR	SYSCALLENTRY
	BCS	*	IF ITS NOT PERFECT, I WON'T LET YOU PLAY WITH MY BALL!
	RTS
	PAGE
*	AN ERROR WAS DETECTED WHILE TRYING TO PRINT AN ERROR STRING
*	IGNORE THE ERROR, AND ATTEMPT TO PRINT "ERROR <ERRORNUMBER>" INSTEAD
*	(IN DECIMAL FORMAT)
*
DISPLAYERROR4	BSR	DISPLAYERRORDONE	CLOSE THE ERROR FILE
DISPLAYERROR4A	EQU	*
	LDX	#DISPERROR
	JSR	SYSCALLENTRY
	BCS	DISPERRKILLLOG	IF I CAN'T PRINT THE ERROR, WE DON'T GO NOWHERE!
	LDAB	#16	(16 BITS TO CONVERT)
	LDX	#DECBUF	CONVERT 16 BITS BINARY TO 3 DIGITS BCD
	CLR	0,X
	CLR	1,X
	CLR	2,X
DISPLAYERROR5	EQU	*
	ASL	ERRORNUMBER-DECBUF+1,X
	ROL	ERRORNUMBER-DECBUF,X
	LDAA	2,X
	ADCA	2,X
	DAA
	STAA	2,X
	LDAA	1,X
	ADCA	1,X
	DAA
	STAA	1,X
	LDAA	0,X
	ADCA	0,X
	DAA
	STAA	0,X
	DECB
	BNE	DISPLAYERROR5
	LDAB	#3	(3 BYTES OF BCD)
DISPLAYERROR6	LDAA	2,X
	ANDA	#$F
	PSHA
	LDAA	2,X
	LSRA
	LSRA
	LSRA
	LSRA
	ANDA	#$F
	PSHA
	DEX
	DECB
	BNE	DISPLAYERROR6
	LDX	#STRINGBUFFER
	LDAA	#'0
	STAA	0,X
	LDAB	#6	(6 BYTES ON THE STACK)
DISPLAYERROR7
	PULA
	DECB
	BEQ	DISPLAYERROR9	B/ NO MORE (THEY WERE ALL ZERO)
	TSTA
	BEQ	DISPLAYERROR7	B/ ZERO SUPPRESS
DISPLAYERROR9	INCB
	BRA	DISPLAYERROR8A

DISPLAYERROR8
	PULA
DISPLAYERROR8A
	ADDA	#'0
	STA	,X+
	STAA	0,X
	DECB
	BNE	DISPLAYERROR8
	DEX
	STX	TEMPX	COMPUTE LENGTH OF THE STRING (ALWAYS < 256)
	LDAB	TEMPX+1
	SUBB	#(STRINGBUFFER&$FF)-1
	LDX	#DISPOUT
	STAB	WRITEA:COUNT+1,X
	JSR	SYSCALLENTRY
	LBCS	DISPERRKILLLOG	ITS GOTTA BE PERFECT, OR I QUIT!
	RTS
*
DISPERRORMSG	FCC	'Error '
DISPERRORMSGL	EQU	*-DISPERRORMSG
*
DISPCLOSE	FCB	SYSCALL:CLOSE	FUNCTION
	FCB	CLOSE:SCLEN
	FCB	SYSCHANNEL	OPEN
*
DISPPOS	; USED TO POSITION $ERRORMESSAGES FILE
	FCB	SYSCALL:CONTROL
	FCB	CONTROL:SCLEN+4	ROOM FOR POINTER TO WRBUF
	FCB	SYSCHANNEL	CHANNEL
	FCB	CC:POSITION	CONTROL SUB-CODE
	FDB	DISPPOS1	POINTER TO 4 BYTE FILE POSITION DESIRED
	FDB	4	SIZE OF FILE POSITION DATA
*
DISPPUTCHAR	; USED TO WRITE ERROR MESSAGE BYTES TO CONSOLE
	FCB	SYSCALL:WRITEA
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL
	FCB	IGNORED
	FDB	BUFFER	THIS IS WHERE CHARACTER IS
	FDB	1	ALL OF 1 BYTE!
*
DISPERROR	; USED TO DISPLAY "Error " ON CONSOLE
	FCB	SYSCALL:WRITEA
	FCB	WRITEA:SCLEN
	FCB	0	CHANNEL
	FCB	IGNORED
	FDB	DISPERRORMSG	BUFFER ADDRESS
	FDB	DISPERRORMSGL	# BYTES TO PRINT
	PAGE	USER PROGRAM EXIT LOGIC
*	KILLUSERPROGRAM -- ENTERED TO ABORT THE CURRENT USER PROGRAM
*	CAN BE CALLED FROM WITHIN SDOS
*	OR FROM WITHIN AN I/O PACKAGE SUBROUTINE OF SDOS
*	OR FROM AN INTERRUPT ROUTINE
*	RETURNS CARRY SET IF CAN'T KILL USER INSTANTLY, SO INPUT ROUTINES DON'T ABORT!
*
KILLUSERPROGRAM	EQU	*
	LDAA	CODE+SDOS:KILLPROOF	IS USER PROGRAM KILL PROOF ?
	BNE	KILLUSERRTS	B/ YES, DON'T KILL IT!
	CLR	KILLEDF	REMEMBER THAT WE KILLED HIM (USED BY "CHAIN")
	JSR	KILLPROOF	ONCE KILLED, DON'T LET IT BE KILLED AGAIN!
	LDD	#KILLPROGRAM	(A,B):= DESIRED PC OF USER TASK
	TST	SDOSRECURSIVECALL	USER TASK EXECUTING INSIDE SDOS SOMEWHERE ?
	BNE	KILLUSER2	B/ YES, FIX UP HIS RETURN ADDRESS AND WE'RE DONE!
	LDX	USERTASKTCB+TCB:STACK	GET STACK POINTER FOR USER TASK
	STD	REG:PC,X	MODIFY CONTEXT BLOCK'S PC...
*	SO THAT THE "RTI" WILL TAKE USER TASK TO "KILLPROGRAM"
	OKRTS

KILLUSER2	; MODIFY USER'S RETURN ADDRESS SO HE GOES TO "KILLPROGRAM"
	IF	M6800!M6801
	LDX	USERRETAPTR	GET USER'S (S) ON ENTRY TO SDOS
	STD	1,X	THIS IS WHERE PC IS IN STACK
	ELSE	(M6809)
	STD	[USERRETAPTR]
	FIN
	OKRTS
*
KILLUSERRTS	; USER IS KILL PROOF
	CLR	KILLREQUESTEDF	CAN'T KILL HIM NOW, SET TIME BOMB
	ERRORRTS
	PAGE
*	KILLPROOF -- PERFORM "SYSCALL:KILLPROOF"
*	KILLENABLE -- PERFORM "SYSCALL:KILLENABLE"
*
KILLPROOF	EQU	*
	LDAA	#1	MAKE USER PROGRAM UNKILLABLE (FOR THE NONCE)
	BRA	KILLPROOF1

KILLENABLE	; MAKE USER PROGRAM KILLABLE
	CLRA		0 --> KILLABLE
KILLPROOF1	STAA	CODE+SDOS:KILLPROOF
	ORAA	KILLREQUESTEDF	DID SOMEBODY ASK FOR THIS GUY TO BE KILLED ?
KILLEDTEST	; CHECK TO SEE IF USER HAS BEEN KILLED
	BEQ	KILLPROGRAM	B/ NOT KILLPROOF, AND KILL REQUESTED IN PAST
	OKRTS		ALL IS DANDY, GET OUT!
*
*	KILLPROGRAM -- THIS IS WHERE USER TASK IS FORCED TO GO
*	HERE WE MAKE USER TASK DO A FORCED EXIT
*
KILLPROGRAM	EQU	*
	BCS	*+2	EAT USER ERROR IF KILLED ****IS THIS WHAT WE WANT???***
	INC	SDOSRECURSIVECALL	SO SYSCALLS WITH FUNNY CHANNEL NUMBERS ARE LEGAL
	LDX	#CLOSECHANNEL0	KILL ANY "DO" FILE THAT MIGHT BE OPEN
	JSR	SYSCALLENTRY
	BCS	EXIT1	B/ MORE IMPORTANT ERROR OCCURRED
	JSR	CLOSELOG	IF ITS OPEN
	BCC	KILLPROGRAM1	B/ IT WAS OPEN, AND WE CLOSED IT SUCCESSFULLY
	CPX	#ERR:CLOSED	THIS IS OK, TOO!
	BNE	EXIT1	B/ WRONG REASON, GO SHOW TO USER
KILLPROGRAM1	EQU	*
	JSR	OPENCONSOLE	RE-OPEN THE CONSOLE ON CHANNEL 0
	BCS	EXIT1	B/ MORE IMPORTANT ERROR OCCURRED
	LDX	#ACKCONTROLC	ACKNOWLEDGE RECEIPT OF ^C^C
	JSR	SYSCALLENTRY
	BCS	EXIT1	B/ RECEIPT ACKNOWLEDGED WITH ERROR 1041
	LDX	#ERR:PROGRAMKILLED	NOW CAUSE APPROPRIATE ERROR TO OCCUR
	BRA	EXIT1
	PAGE
*	EXIT -- EXIT FROM USER PROGRAM BACK TO COMMAND INTERPRETER
*	CLOSES ALL OPEN CHANNELS EXCEPT 0 & FLUSHES THE BUFFERS
*	THEN EXITS TO OPERATING SYSTEM
*
ERROREXIT	LDX	CODE+SDOS:IOBLOCKPTR
	LDX	ERROREXIT:CODE,X
	BRA	EXIT1
*
EXITERRED	; EXIT GOT AN ERROR, DISPLAY IT!
	PSHX		SAVE THIS ERROR CODE
	JSR	DISPLAYERRORCR	DISPLAY THE PREVIOUS ERROR
	PULD		GET EXIT'S ERROR CODE BACK
	STD	USERSLASTERROR	SAVE THE ERROR CODE
	JSR	DISPLAYERRORCR	SHOW EXIT'S ERROR
	PAGE
EXITCHKSUMBASE	; CODE FROM HERE TO EXITCHKSUMEND IS CHECKSUMMED
EXIT	LDX	#0	PICK UP ERROR CODE FOR "NORMAL EXIT"
EXIT1	JSR	KILLPROOF	SO WE DON'T GET KILLED BY SOME ZEALOT
	LDAA	#1	GET RID OF ANY OLD KILL REQUESTS
	STAA	KILLREQUESTEDF
	STAA	KILLEDF	FORGET THAT USER MIGHT HAVE BEEN KILLED
	LDS	SDOSSTACK	PICK UP A REASONABLE STACK POINTER
	STX	USERSLASTERROR	SAVE LAST ERROR CODE
	JSR	INTENABLE	IN CASE SOMEBODY MANAGED TO TURN THEM OFF
	JSR	EXITS	GO CLOSE UP THE FILES
	BCS	EXITERRED	B/ ERROR, GO TRY TO DEAL WITH IT
	BSR	DISPLAYERRORCR
	LDX	#SDOSCHKSUMBASE
EXITCODECHKSUM	EQU	*+1	CONTAINS VALUE THAT MAKES CHECKSUM OVER EXIT ALL ZERO
	LDAB	#CHANGED	CHANGED BYTE IS CHECKSUM BYTE
	CLRA		COMPUTE CHECKSUM ON SDOS CODE
EXITCKSUML	; DO CHECKSUM THAT IS HARD TO BEAT BY TRANSPOSITION OF BYTES
	ASLA
	IF	M6800!M6801
	ADCA	0,X
	INX
	ELSE	(M6809)
	ADCA	,X+
	FIN
	CPX	#SDOS:END
	BNE	EXITCKSUML
	TSTA		IS CHECKSUM OK (=0?)
	BEQ	EXIT2	B/ YES, GO CHAIN TO DEFAULTPROGRAM
	LDX	#ERR:SDOSCKSUM	DISPLAY THE ERROR!
	STX	USERSLASTERROR
	BSR	DISPLAYERRORCR
	BRA	*	WE'RE DEAD...
	page
;	SerialNumberLocation is included in the critically secure "EXIT" region...
;	because of the importance of knowing that its contents are not forged.

SerialNumberLocation	fdb	0	Points to SerialNumber if Mass-produced ROM
		; Points to different place for each Mass-Produced computer type
		; Contains Zero if Standard SDOS boot ROM structure.

EXITCHKSUMEND	; ALL CODE BETWEEN EXITCHKSUMBASE AND HERE IS CHECKSUMMED
*
*	SDOSCODECHKSUM contains a value that makes the checksum over SDOS give 0
*
SDOSCODECHKSUM	FCB	CHANGED
	PAGE
EXIT2	CLR	SDOSRECURSIVECALL	SWITCH TO USER MODE ("NOT IN SDOS")
	JSR	KILLENABLE	ALLOW USER PROGRAM TO GET KILLED
	LDX	#EXITCHAIN	ALL IS OK, GO GET THE DEFAULT PROGRAM
	JSR	SYSCALLENTRY
	BCC	*	WE GOT HERE WITHOUT AN ERROR???
	INC	SDOSRECURSIVECALL	SWITCH BACK TO "IN SDOS" MODE
	CPX	#ERR:NOSUCHPROGRAM	THIS KIND OF ERROR ?
	BNE	EXIT3	B/ NO, TELL THE USER
	LDX	#ERR:NODEFAULTPROGRAM	SCREAM "NO DEFAULTPROGRAM"!
EXIT3	STX	USERSLASTERROR
	BSR	DISPLAYERRORCR
*
*	Since there is no DEFAULTPROGRAM on this disk, dismount the disk.
*
	LDX	DEFAULTDISKDCB	THIS IS THE DISK TO DISMOUNT.
	STX	DCBPOINTER
	JSR	DSKDDISMOUNT
	BRA	EXIT	AND START OVER AGAIN

DEFAULTPROG	FCC	'DEFAULTPROGRAM'
DEFAULTPROGL	EQU	*-DEFAULTPROG
*
EXITCR	LDX	#EXITCR1
	JMP	SYSCALLENTRY
*
EXITCR1	; SYSCALL USED TO PRINT A <CR> ON CHANNEL 0
	FCB	SYSCALL:WRITEA
	FCB	WRITEA:SCLEN
	FCB	0,IGNORED	CHANNEL
	FDB	COPYRIGHTEND-1	YOU'LL SEE A :0D THERE...
	FDB	1	THIS IS ALL WE WANT TO WRITE!
	PAGE
*
*	DISPLAYERRORCR -- DISPLAY USER'S LAST ERROR, FOLLOWED BY <CRLF>
*
DISPLAYERRORCR	EQU	*
	LDX	USERSLASTERROR	GET ERROR NUMBER
	BEQ	DISPLAYERRORCR1	B/ NO ERROR!
	JSR	DISPLAYERROR	GO PRINT THE ERROR STRING OR NUMBER
	BSR	EXITCR	GO DISPLAY <CRLF>
	BCS	*	LOOP HERE IF WE CAN'T GET IT OUT!
DISPLAYERRORCR1	EQU	*
	OKRTS
*
*	OPEN UP CHANNEL 0 TO THE FIRST DEVICE IN THE
*	DEVICE NAME LIST
*	***** RULE *****
*	THE FIRST DEVICE IN THE DEVICE LIST IS THE CONSOLE
*
OPENCONSOLE	LDX	#CONSOLEOPEN
	JMP	SYSCALLENTRY
	PAGE
	IF	0	this code is cute, but is wrong evolutionary path...
*
*	SCANDCBSFORLUN -- SCAN DOWN DCB CHAIN, DECREMENT LOGICAL UNIT # IN D...
*	UNTIL ZERO OR DCB CHAIN IS EXHAUSTED
*	RETURNS (X)=0 --> NOT FOUND; <>0 --> FOUND
*
SCANDCBSFORLUN1	; CHECK: IS LUN ZERO?
	TSTD
	BEQD	SCANDCBSFORLUNF	B/ YES, WE FOUND DEVICE NAME!
	SUBD	#1	DECREMENT LUN
	LDX	DCB:NEXTDCB,X	FIND NEXT DCB
SCANDCBSFORLUN	; SCAN DCB CHAIN FOR LUN (D)
	BNE	SCANDCBSFORLUN1	B/ MORE DCB CHAIN TO SEARCH
	RTS		WITH "Z" SET --> NOT FOUND!

SCANDCBSFORLUNF	; FOUND DCB THAT MATCHES LOGICAL UNIT NUMBER!
	LDX	DCB:NAME,X	GET POINTER TO NAME
	RTS		WITH "Z" BIT RESET

ERRNOSUCHLUN
	JSR	ERRET
	FDB	ERR:NOSUCHLUN
*
*	READLUNNAME -- PERFORM "SYSCALL:READLUNNAME"
*	CONVERTS LOGICAL UNIT NUMBER TO DEVICE NAME STRING
*
READLUNNAME
	LDX	CODE+SDOS:IOBLOCKPTR	GET LOGICAL UNIT NUMBER
	LDD	READLUNNAME:LUN,X
	LDX	CODE+SDOS:CONFIGURATION	SEE IF LUN IS IN DISK DCBS
	LDX	CNFG:DISKDCBS,X
	BSR	SCANDCBSFORLUN
	BNE	READLUNNAME1	B/ YES!
	LDX	CODE+SDOS:CONFIGURATION	SEE IF LUN IS IN DEVICE DCBS
	LDX	CNFG:DEVICEDCBS,X
	BSR	SCANDCBSFORLUN
	BEQ	ERRNOSUCHLUN	B/ LUN IS NOT LEGAL IN THIS CONFIGURATION
READLUNNAME1	; (X) = ADDRESS OF DEVICE NAME FOR SPECIFIED LUN
	STX	FROMPOINTER	SAVE DEVICE NAME ADDRESS
	LDAB	#-1	GET LENGTH OF DEVICE NAME
READLUNNAME2	; SCAN DEVICE NAME TO GET LENGTH
	INCB		BUMP LENGTH
	IF	M6800!M6801
	LDAA	,X	GET BYTE OF DEVICE NAME
	BEQ	READLUNNAME3	B/ END OF NAME
	INX
	BRA	READLUNNAME2
READLUNNAME3
	ELSE	(M6809)
	LDA	,X+
	BNE	READLUNNAME2
	FIN
	CLRA
	STD	TEMPX	= LENGTH OF DEVICE NAME
	LDX	CODE+SDOS:IOBLOCKPTR	GET USER BUFFER ADDRESS
	LDD	READLUNNAME:MAXCOUNT,X	= MAX # BYTES TO TRANSFER
	SUBD	TEMPX	ROOM FOR REPLY?
	BCS	ERRSYSTOOSHORTJ
	LDD	TEMPX	SET REPLY LENGTH
	STD	READLUNNAME:ACTUALCOUNT,X
	IF	M6800!M6801
	LDX	READLUNNAME:BUFFERP,X	= WHERE TO PUT NAME
	STX	TOPOINTER
	ELSE	(M6809)
	LDY	READLUNNAME:BUFFERP,X	= WHERE TO PUT NAME
	FIN
	LDX	FROMPOINTER
	JSR	BLOCKMOVE
	OKRTS
	ELSE
READLUNNAME	EQU	ERRILLSYSCALL	to cauterize the wound...
	FIN
	PAGE
*	CHECKRDLEN -- CHECK SCBLK:RDLEN...
*	TO MAKE SURE ITS VALUE >= 2 BYTES AT RETURN ADDRESS
*	ALSO CHECKS THAT SYSCALL BLOCK IS LONG ENOUGH TO INCLUDE SCBLK:RDLEN
*	SETS SCBLK:RPLEN TO 2 BYTE INLINE VALUE IF RDLEN IS OK
*	(X) = CODE+SDOS:IOBLOCKPTR ON EXIT
*	SKIPS AROUND 2 BYTE INLINE VALUE IF OK
*	ERROR EXIT IF NOT OK
*	**** CHECKWRLEN, CHECKRDLEN SHOULD BE RECODED SO THAT THE FOLLOWING WORKS:
*	JSR	CHECKRDLEN
*	FDB	XXXX
*	BCS	error...
*
CHECKRDLEN	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	CHECK THAT READ DATA BUFFER IS INCLUDED...
	LDAA	SCBLK:WLEN,X	IN THE SYSCALL BLOCK
	ANDA	#$7F	BYEBYE WAIT BIT
	CMPA	#SCBLK:RDLEN+2	IS SYSCALL BLOCK LONG ENOUGH ?
	BCS	ERRSYSTOOSHORTJ	B/ NOPE, STICK THE USER WITH AN ERROR
	LDD	[0,S]	IS 0 > MINLENGTH-RDLEN-1 ?
	SEC		(-1 PART)
	LDX	CODE+SDOS:IOBLOCKPTR	SUBTRACT RDLEN
	SBCB	SCBLK:RDLEN+1,X
	SBCA	SCBLK:RDLEN,X
	BCC	ERRRDBUFTOOSMALL	B/ NO, NOT ENOUGH READ BUFFER
	ADCB	SCBLK:RDLEN+1,X	SET REPLY LENGTH = DESIRED # BYTES
	ADCA	SCBLK:RDLEN,X	(A,B):=DESIRED # BYTES
	STD	SCBLK:RPLEN,X	RPLEN:=(A,B)
CHECKRDLEN1	EQU	*
	LDA	1,S	ALL IS OK, TAKE SKIP EXIT
	ADDA	#2	BUMP RETURN PAST INLINE ARGUMENT
	IF	M6800!M6801
	STAA	1,X
	BCC	CHECKRDLEN2
	INC	0,X
	ELSE	(M6809)
	STAA	1,S
	BCC	CHECKRDLEN2
	INC	0,S
	FIN
CHECKRDLEN2	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR
	OKRTS
	PAGE
ERRSYSTOOSHORTJ	EQU	*
	JMP	ERRSYSCALLTOOSHORT

ERRRDBUFTOOSMALL	EQU	*
	JSR	ERRET
	FDB	ERR:RDBUFTOOSMALL
*
*	CHECKWRLEN -- CHECK SCBLK:WRLEN...
*	TO MAKE SURE ITS VALUE >= 2 BYTES AT RETURN ADDRESS
*	ERROR EXIT IF NOT
*	RETURNS WITH (X) CONTAINING CODE+SDOS:IOBLOCKPTR OTHERWISE
*	SKIPS AROUND 2 BYTE INLINE VALUE
*
CHECKWRLEN	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	CHECK THAT WRBUF INFO IS INCLUDED...
	LDAA	SCBLK:WLEN,X	IN THE SYSCALL
	ANDA	#$7F	BYEBYE WAIT BIT
	CMPA	#SCBLK:WRLEN+2	SYSCALL BLOCK LONG ENOUGH ?
	BCS	ERRSYSTOOSHORTJ	B/ NO, GO STICK IT TO THE USER
	LDD	[0,S]	IS 0 > MINLENGTH - WRLEN -1 ?
	SEC		(-1 PART)
	LDX	CODE+SDOS:IOBLOCKPTR	NOW SUBTRACT WRLEN
	SBCB	SCBLK:WRLEN+1,X
	SBCA	SCBLK:WRLEN,X
	BCS	CHECKRDLEN1	B/ ITS OK
ERRWRBUFTOOSMALL	EQU	*
	JSR	ERRET
	FDB	ERR:WRBUFTOOSMALL
*
*	CHECKSCLEN -- CHECK SCBLK:WLEN...
*	TO MAKE SURE IT IS LARGER THAN (B), ERROR EXIT IF NOT
*
CHECKSCLEN
	LDX	CODE+SDOS:IOBLOCKPTR	GET WLEN OF SYSCALL BLOCK
	LDA	SCBLK:WLEN,X
	ANDA	#$7F	AND MASK OFF THE NO WAIT BIT
	CBA		IS SYSCALL BLOCK LONG ENOUGH?
	BCS	ERRSYSTOOSHORTJ	B/ NOPE, STAB USER IN BACK TO GET EVEN!
	RTS		ALL IS WONDERFUL...
	PAGE
*	EXITS -- DO CLEAN UP APPROPRIATE FOR PROGRAM COMPLETION
*	CLOSES ALL I/O CHANNELS EXCEPT 0
*	DUMPS ALL DISK BUFFERS TO DISK
*
EXITS	LDX	CODE+SDOS:CONFIGURATION
	LDAA	CNFG:NIOCHANNELS,X
	STAA	EXITSCLOSE+CLOSE:CHANNEL	SET UP CHANNEL-CLOSING SYSCALL
EXITS2	LDX	#EXITSCLOSE	CLOSE A CHANNEL
	DEC	CLOSE:CHANNEL,X
	BEQ	EXITS3	B/ ALL USER I/O CHANNELS EXCEPT ZERO CLOSED
	JSR	SYSCALLENTRY	GO CLOSE THIS CHANNEL
	BCC	EXITS2
	CPX	#ERR:CLOSED	WAS THE CHANNEL CLOSED?
	BEQ	EXITS2	B/ YEP, GO ON
EXITSERRED	; NOPE, HE'S DEAD (CARRY IS STILL SET)
	JMP	ERRORED
*
*	NOW GO THROUGH AND DUMP THE DISK BUFFERS ON ALL THE DRIVES
*
EXITS3	LDX	CODE+SDOS:CONFIGURATION
	LDX	CNFG:DISKDCBS,X
EXITS4	STX	DCBPOINTER
	JSR	DSKDUMPBUFFERS
	BCC	EXITS6	B/ NO PROBLEMS
	CPX	#ERR:FILEISOPEN	WAS IT THE FILE OPEN PROBLEM?
	BNE	EXITSERRED	B/ NOPE, YOU'RE DEAD
EXITS6	LDX	DCBPOINTER
	LDX	DCB:NEXTDCB,X
	BNE	EXITS4
	IF	*&1	FORCE EXITDAMAGE TO BE EVEN ADDRESS
	NOP
	FIN
EXITDAMAGE	; THIS PLACE IS MASHED BY COPYRIGHT CHECK FAILURE
	OKRTS
	PAGE	CHAIN/LOAD SYSCALLS
*	CHAIN -- PERFORM "SYSCALL:CHAIN"
*	ALWAYS RETURNS CONTROL TO CALLER AFTER ERROR UNTIL TYPE 1 RECORD VALIDATED
*	ONCE TYPE 1 RECORD SEEN, ERROREXIT IS FORCED ON AN ERROR.
*	CHAIN SWITCHES STACK TO TOP OF USER SPACE ONCE TYPE 1 RECORD SEEN.
*
*	CHAIN Transition/action table:
*	Unencrypted --> Unencrypted:	Do nothing
*	Unencrypted --> Encrypted:	Zero the user space, save key, mark 'running encrypted'
*	Encrypted --> Encrypted:	Do nothing
*	Encrypted --> Encrypted':	Zero the user space, save decryption key
*	Encrypted --> Unencrypted:	Zero the user space, save key, mark 'runencrypted'

*	LOAD Transition/action table:
*	Unencrypted --> Unencrypted:	Do nothing
*	Unencrypted --> Encrypted:	Signal error
*	Encrypted --> Encrypted:	Do nothing
*	Encrypted --> Encrypted':	Signal error
*	Encrypted --> Unencrypted:	Signal error
*
*	These routines use OldKeyEncrypted and NewKeyEncrypted to decide transitions
*	Definition: If OldKeyEncrypted=0, then we were running Unencrypted
*		If NewKeyEncrypted=0, then we are transiting to Unencrypted
*
CHAIN	EQU	*
	LDX	CODE+SDOS:IOBLOCKPTR	REMEMBER WHERE THE CHAIN SYSCALL IS
	STX	USERSYSCALLPTR	(THIS IS NEEDED BECAUSE EXIT DOES A CHAIN)
	BSR	EXITS	GO CLOSE ALL THE CHANNELS
	LDX	USERSYSCALLPTR	GRAB ADDRESS OF CHAIN SYSCALL AGAIN
	STX	CODE+SDOS:IOBLOCKPTR	MAKE IT THE OFFICIAL "BEING PROCESSED" SYSCALL AGAIN
	LDX	SCBLK:PARAMS,X	GET LONG-TERM OPERATION ENABLING KEY
	JSR	SAFEGAURD	SET UP A DISASTER IF WRONG KEY IS PASSED
FIRSTCHAIN	; ENTRY POINT USED TO CHAIN TO "SERIALNUMBER.SYS"
	BSR	LOADINIT	OPEN THE FILE AND VERIFY IT IS AN OBJECT FILE
	LDX	STARTADDRESS	CHECK THE START ADDRESS FOR ILLEGAL VALUE
	BEQ	ERRZEROSTARTADDRESS	B/ NO START ADDRESS!
	LDS	SDOSSTACK	FILE IS OK, SWITCH TO STANDARD STACK FOR CHAINING
*** ^C^C IS PERFECTLY SAFE HERE, BECAUSE SDOSRECURSIVE CALL IS SET!
	BSR	COMPAREDECRYPTIONKEYS	ARE OLD AND NEW ENCRYPTION KEYS IDENTICAL ?
	BCC	CHAINOK	B/ DECRYPTION KEYS MATCH, DO NOTHING
	JSR	ZAPUSERSPACE	MAKE TRASH OUT OF INFORMATION IN USER SPACE
	if	m6800!m6801
	ldx	#OldKeyEncrypted	save this "key" as last "key" used
	stx	topointer
	else	(m6809)
	ldy	#OldKeyEncrypted
	fin
	LDX	#NEWKEYENCRYPTED	= FROM ADDRESS
	ldd	#8	= # bytes to save
	jsr	blockmove	Note: it is NOT secure to call code+sdos:blockmove here!
CHAINOK	; ITS OK TO DO THE CHAIN, HOP TO IT!
	JSR	LOADREST	GO LOAD THE REST OF THE FILE
	BCC	CHAIN1	B/ NO LOADER ERROR
	JMP	EXIT1	I GIVE UP

CHAIN1	CLR	SDOSRECURSIVECALL	FLAG "NOT IN SDOS"
	LDAA	KILLEDF	SEE IF USER GOT KILLED WHILE LOADING!
	JSR	KILLEDTEST	...?
	LDX	STARTADDRESS	ALL IS DONE...
	JMP	,X	PASS CONTROL TO USER PROGRAM!

ERRZEROSTARTADDRESS	; START ADDRESS IS ZERO ON A CHAIN
	LDX	#ERR:ZEROSTARTADDRESS	GET ERROR CODE
	JMP	LOADER2	GO CLOSE THE FILE, TRASH THE KEY, AND EXIT!
	PAGE
*	COMPAREDECRYPTIONKEYS -- COMPARE OLD AND NEW DECRYPTION KEYS
*	SIGNAL ERROR IF NO MATCH
*	(DO SO IN A SUBTLE WAY, SO IT IS NOT EASY TO FIND WHO GENERATES THE ERROR CODE)
*
COMPAREDECRYPTIONKEYS
	IF	M6800!M6801
	LDX	#OLDKEYENCRYPTED	= COMPARE "TO" ADDRESS
	STX	TOPOINTER
	ELSE	(M6809)
	LDY	#OLDKEYENCRYPTED
	FIN
	LDX	#NEWKEYENCRYPTED	= COMPARE "FROM" ADDRESS
	LDB	#8	= # BYTES TO COMPARE (KEY SIZE)
	JSR	BLOCKCOMPARE
	BEQ	COMPAREDECRYPTIONMATCH	B/ ALL IS OK
	LDD	#ERR:DECRYPTIONKEYSDONTMATCH-1234	GENERATE ERROR CODE...
	ADDD	#1234	IN SUBTLE WAY
	TDX
	ERRORRTS		AND TAKE ERROR EXIT

COMPAREDECRYPTIONMATCH	; OLD AND NEW KEYS MATCH!
	OKRTS
	PAGE
*
*	LOADINIT -- OPEN LOAD FILE, COMPUTE DECRYPTION KEY,
*	AND VERIFY OBJECT FORMAT CONTENT
*	IF AN ERROR OCCURS, CLOSE THE LOAD FILE
*
LOADINIT
	LDX	CODE+SDOS:IOBLOCKPTR	REMEMBER ADDRESS OF SYSTEM CALL
	STX	LOADSYSCALLPTR	SO WE CAN SAVE START ADDRESS IN REPLY BUFFER, LATER
	LDAA	#$7E	SET UP SYSCALL JUMP IN PAGE ZERO...
	STAA	SYSCALL$	TO JUMP TO I/O PACKAGE BASE
	LDX	CODE+SDOS:CONFIGURATION	(SO USER KNOWS WHERE BASE OF I/O PACK IS!)
	LDX	CNFG:DRIVERBASE,X
	STX	SYSCALL$+1
	JSR	CHECKRDLEN	NEED 4 BYTE READ-BACK AREA
	FDB	2+2	2 FOR FILENAME LENGTH, 2 FOR START ADDRESS
	LDX	#SYSIOCB	FAKE A CHECKCHANNEL CALL ON SYS CHANNEL
	STX	IOCBPOINTER
	JSR	FNAMEPROCESS1	GO FIGGER OUT DA DRIVER ADDRESS
	JSR	[DRIVER:OPEN,X]	AND OPEN IT!
	BCS	LOADINITCANTOPEN	B/ HAD A PROBLEM!
	JSR	OPEN2	GO ADJUST FILE NAME, AND MARK SYSIOCB AS "OPEN"
	CLR	OKTODECRYPTFLAG	FLAG "DON'T DECRYPT!"
	LDX	#DECRYPTBUFFER	SET UP SO GET BYTE GETS 1ST BYTE OF FILE
	STX	DECRYPTBUFPTR
	LDX	#FILLDECRYPTBUFFER	READ 1ST 8 BYTES OF FILE
	JSR	SYSCALLENTRY
	BCS	LOADER2	B/ PROBABLY EOF, TOO BAD!
	JSR	GETBYTE	GET ME A BYTE
	CMPA	#LOAD:ENCRYPTED	IS THIS AN ENCRYPTED FILE?
	LBEQ	LOADENCRYPTED	B/ YES, GO COMPUTE DECRYPTION KEY AND PROCESS 1ST LOAD RECORD
	LDX	#NEWKEYENCRYPTED	SET NEWKEYENCRYPTED = 0 --> UNENCRYPTED FILE
	LDB	#8	= # BYTES TO ZERO
ZERONEWKEYLOOP
	CLR	,X+
	DECB
	BNE	ZERONEWKEYLOOP
LOADSTART	; VERIFY THAT THIS IS A PROPERLY CONSTRUCTED START RECORD
	CMPA	#LOAD:TYPE1	DOES IT APPEAR TO BE SDOS BINARY FORMAT ?
	BEQ	LOADTYPE1	B/ YES
CANTLOAD	; THIS IS NOT AN OBJECT FILE
	BSR	LOADCLOSE	CLOSE THE FILE
	JSR	ERRET
	FDB	ERR:NOTALOADFILE
*
LOADTYPE1	; FETCH START ADDRESS AND VERIFY COMPLEMENT IN TYPE1 RECORD
	JSR	GETWORD	GET THE EXECUTION ADDRESS
	STD	STARTADDRESS
	LDX	LOADSYSCALLPTR	COPY START ADDRESS TO SYSCALL REPLY BUFFER
	LDX	SCBLK:RDBUF,X
	STD	2,X
	JSR	GETWORD
	ADDD	STARTADDRESS
	CMPD	#-1
	BNE	CANTLOAD	B/ NOT TYPE 1 RECORD!
LOADINITOKRTS
	OKRTS

ERRSERIALNOWRONG
	LDX	#ERR:SERIALNOWRONG
LOADER2	; ERRORED WHILE TRYING TO LOAD
	STX	STARTADDRESS	SAVE THE ERROR CODE
	BSR	LOADCLOSE	CLOSE UP THE LOAD FILE
	LDX	STARTADDRESS	GET ERROR CODE BACK
	BRA	LOADINITCANT1	GO STICK USER WITH ERROR

LOADINITCANTOPEN	; ERROR WHEN OPENING LOAD FILE
	CPX	#ERR:FILENOTFOUND	IS THIS THE CAUSE?
	BNE	LOADINITCANT1	B/ NO
	LDX	#ERR:NOSUCHPROGRAM	YES, CHANGE ERROR CODES
LOADINITCANT1
	JMP	ERRORINX	TRIGGER THE ERROR AGAIN
	PAGE
*	LOAD -- PERFORM "SYSCALL:LOAD"
*
LOAD	EQU	*
	JSR	LOADINIT	LOAD THE FILE INTO MEMORY
	JSR	COMPAREDECRYPTIONKEYS	ARE OLD AND NEW DECRYPTION KEYS IDENTICAL ?
	BCS	LOADER2	B/ KEYS DON'T MATCH, CLOSE FILES AND SIGNAL ERROR!
*
LOADREST	; LOAD OBJECT RECORDS FROM FILE
	JSR	LOADCONTENTS	LOAD THE CONTENTS OF THIS FILE
	BCS	LOADER2	B/ ERRORED IN LOADING FILE CONTENTS
LOADCLOSE	; CLOSE THE LOAD FILE AND QUIT
	LDX	#OLDKEYENCRYPTED	DECIDE IF WE SHOULD RUN ENCRYPTED OR NOT
	JSR	SETKEY	(TRASH THE DECRYPTION KEY, THIS PATH ALWAYS TAKEN!)
	LDAA	,X	IF OLDKEYENCRYPTED<>0, THEN RUN ENCRYPTED!
	ORAA	1,X
	ORAA	2,X
	ORAA	3,X
	ORAA	4,X
	ORAA	5,X
	ORAA	6,X
	ORAA	7,X
	JSR	GETRUNNINGENCRYPTED	SET RUNNINGENCRYPTED FLAG
	STA	,X	SET RUNNINGENCRYPTED TO PROPER STATE
	LDX	#LOADCLOSE1	NOW CLOSE THE LOAD FILE
	JMP	SYSCALLENTRY

LOADCLOSE1	EQU	*
	FCB	SYSCALL:CLOSE
	FCB	CLOSE:SCLEN
	FCB	SYSCHANNEL
	PAGE
*
*	SYSCALL:GETSERIALNUMBER
*	RETURNS SERIALNUMBER OF PROCESSOR IN REPLY BUFFER
*
GETSERIALNUMBER
	JSR	CHECKRDLEN	MAKE SURE REPLY BUFFER IS LARGE ENOUGH
	FDB	SERIALNUMBER:SIZE
	IF	M6800!M6801
	LDX	SCBLK:RDBUF,X	WHERE TO PUT SERIAL NUMBER
	STX	TEMPX	= COPY-TO ADDRESS
	ELSE	(M6809)
	LDY	SCBLK:RDBUF,X	WHERE TO PUT SERIAL NUMBER
	FIN
	BSR	GETSERIALNUMBERADDRESS	GET ADDRESS AND SIZE OF SERIAL NUMBER
	JSR	BLOCKMOVE	*** NOT SAFE TO USE SDOS:BLOCKMOVE ENTRY POINT ***
	OKRTS
*
*	GETSERIALNUMBERADDRESS -- RETURNS LOCATION AND SIZE OF SERIALNUMBER
*
GETSERIALNUMBERADDRESS
	LDD	#SERIALNUMBER:SIZE	GET SIZE OF SERIAL NUMBER TO (D)
	LDX	SERIALNUMBERLOCATION	GET LOCATION OF S/N FOR MASK ROM
	BNE	GETSERIALNUMBERADDRESSRTS	B/ MASK ROM
	LDX	$FFFE	STANDARD SDOS BOOT ROM, FIND JMP PRECEDING S/N
	LEAX	3,X	ADVANCE POINTER SO IT POINTS AT S/N IN ROM
GETSERIALNUMBERADDRESSRTS
	RTS
	PAGE	SDOS BINARY FORMAT LOADER
GETRUNNINGENCRYPTED ; GET ADDRESS OF RUNNING ENCRYPTED FLAG TO (X)
	LDX	SERIALNUMBERLOCATION	MASK ROM ?
	BEQ	GETRUNNINGENCRYPTED1	B/ NO, GO LOCATE VIA RESTART VECTOR
	LDX	#RUNNINGENCRYPTEDFLAG	YES, USE DEFAULT RUNNING ENCRYPTED FLAG
	RTS

GETRUNNINGENCRYPTED1 ; STANDARD SDOS ROM STRUCTURE
	LDX	$FFFE	GET ADDRESS OF JMP PRECEDING SERIAL NUMBER
	LDX	1,X	GET POINTER TO RESTART CODE
	LDX	LDAENCRYPTED-RESTART+1,X	= ADDRESS OF RUNNINGENCRYPTED FLAG
	RTS

LOADENCRYPTED ; LOAD AN ENCRYPTED FILE, PROCESS TYPE 5 RECORD
	BSR	GETRUNNINGENCRYPTED	GET ADDRESS OF RUNNING ENCRYPTED
	INC	,X	BUMP RUNNINGENCRYPTED; = 2 --> WAS ALREADY RUNNING ENCRYPTED
	LDX	SERIALNUMBERLOCATION	SHOULD WE CHECK SELECT BANK ROUTINE ?
	BNE	LOADENCRYPTED1	B/ NO, THIS MACHINE HAS MASK-ROM --> NO SELECT BANK!
	LDX	$FFFE	LOCATE SELECT BANK VIA RESTART VECTOR
	LDX	1,X	CALL SELECTBANK TO VERIFY THAT IT REALLY WORKS!
	CLRA		SELECT BANK 0
	JSR	[ERASEBANK-RESTART+1,X]
LOADENCRYPTED1
	LDB	DECRYPTBUFFER+1	GET KEY COUNT FROM TYPE 5 RECORD
	STB	NKEYS	= # OF KEYS ENCODED
	LDX	#SDOSSECRETKEY
	JSR	SETKEY	TO START DECRYPT CASCADE THAT GENERATES KEY
	LDX	#DECRYPTBUFFER	START CASCADE OF DECRYPTING 05 RECORD
	JSR	DECRYPT	THIS RANDOMIZES AS MUCH AS ENCRYPT
	JSR	SETKEY	SET KEY TO CONTENTS OF BUFFER
	CLR	OKTODECRYPTFLAG	FLAG 'NO SERIAL NUMBER MATCHES'
LOADCASCADELOOP ; PROCESS SERIAL NUMBERS BY CASCADING THEM
	LDX	#FILLDECRYPTBUFFER READ IN THE NEXT SERIAL NUMBER
	JSR	SYSCALLENTRY
	IF	M6800!M6801
	LDX	#DECRYPTBUFFER	= COMPARE "TO" ADDRESS
	STX	TOPOINTER
	ELSE	(M6809)
	LDY	#DECRYPTBUFFER
	FIN
	BSR	GETSERIALNUMBERADDRESS	GET POINTER TO SERIAL NUMBER IN ROM
	; ASSERT: GETSERIALNUMBERADDRESS SETS (D) TO 8
	JSR	BLOCKCOMPARE	COMPARE DECRYPTBUFFER AGAINST ROM SERIAL NUMBER
	TPA		HAS 4'S BIT SET IF MATCHED
	ANDA	#4	= 0 IF NO MATCH, <>0 IF MATCH
*	SET OKTODECRYPTFLAG IF THERE IS A MATCH
*	DON'T USE CONDITIONAL BRANCH AS THEY ARE EASY TO FIND
*
	EORA	OKTODECRYPTFLAG	NOW SET FLAG IF OK TO DECRYPT
	STA	OKTODECRYPTFLAG
*** IF DECRYPT KEY WERE FUNCTION OF ROM, THEN WE WOULD NOT HAVE TO CHECK SERIAL
*** NUMBER IN ROM AT ALL; SIMPLY TRY TO LOAD IT. ILLEGAL LOAD RECORD REPORTS AS
*** WRONG SERIAL NUMBER!
	LDX	#DECRYPTBUFFER	START CASCADE OF DECRYPTING 05 RECORD
	JSR	DECRYPT	THIS RANDOMIZES AS MUCH AS ENCRYPT
	JSR	SETKEY	SET KEY TO CONTENTS OF BUFFER
	DEC	NKEYS	PROCESSED A KEY...
	BNE	LOADCASCADELOOP	B/ MORE KEYS TO PROCESS
	TST	OKTODECRYPTFLAG	DID WE FIND A MATCHING SERIAL NUMBER?
	LBEQ	ERRSERIALNOWRONG
*
*	AT THIS POINT, THE DECRYPTION KEY FOR THE OBJECT RECORDS IS COMPLETE
*	NOW WE START THE ACTUAL LOADING PROCESS
*	WE STILL CHECK FOR OBJECT FILE FORMAT IN CASE THE DECRYPTION KEY IS BAD
	PAGE
*	NOW GENERATE AN ENCRYPTED VERSION OF THE DECRYPTION KEY...
*	SO THAT CHAIN/LOAD CAN COMPARE TO OLD VERSION OF "DECRYPTION" KEY
*
	jsr	decrypt	"Encrypt" the key using itself
	IF	M6800!M6801
	LDX	#NEWKEYENCRYPTED	= "TO" ADDRESS
	STX	TOPOINTER
	ELSE	(M6809)
	LDY	#NEWKEYENCRYPTED
	FIN
	LDX	#DECRYPTBUFFER	= FROM ADDRESS
	LDD	#8	= SIZE OF KEY
	JSR	BLOCKMOVE	SAVE NEWKEY, ENCRYPTED FOR LATER COMPARISION
*	LDX	#DECRYPTBUFFEREND	SET UP TO GET PRESUMED TYPE1 RECORD CODE
	STX	DECRYPTBUFPTR
	JSR	GETBYTE GET ME A BYTE
	jmp	loadstart	go process type1 load record

	FCB	$CE	TO THROW OFF WOULD-BE DISASSEMBLERS
SDOSSECRETKEY FCB $4C,$B0,$B7,$4E,$9B,$65,$72,$C9
	PAGE
LOADSETPOS	; SYSCALL TO POSITION LOAD FILE PAST SKIP RECORD
	FCB	SYSCALL:CONTROL	USED BY LOADER TO EXECUTE "SKIP N BYTES"
	FCB	CONTROL:SCLEN+4
	FCB	SYSCHANNEL
	FCB	CC:POSITION
	FDB	LOADFILEPOS	POINTER TO POSITION SELECT BUFFER
	FDB	4	SIZE OF POSITION SELECT BUFFER

LOADTYPE0	; IGNORE "LOADCOUNT" BYTES (ASSERT: CANNOT OCCUR IN ENCRYPTED FILES!!!)
	JSR	GETWORD
LOADTYPE0L	; USE UP BYTES IN DECRYPT BUFFER
	STD	LOADCOUNT	SAVE # BYTES TO SKIP
	LDX	DECRYPTBUFFER	USE UP BYTES IN DECRYPTBUFFER, FIRST
	CPX	#DECRYPTBUFFEREND	DECRYPT BUFFER EXHAUSTED?
	BEQ	LOADTYPE0.2	B/ YES
	JSR	GETBYTE	NO, USE UP A BYTE
	LDD	LOADCOUNT	DECREMENT SKIP LOADCOUNT
	SUBD	#1
	BNED	LOADTYPE0L	B/ MORE BYTES TO SKIP
	BRA	LOADNEXT	EXHAUSTED SKIP LOADCOUNT!

LOADTYPE0.2 ; SKIP OVER BIG CHUNK OF BYTES IN FILE
	LDX	#LOADGETPOS	GET FILE POSITION
	JSR	SYSCALLENTRY
	LDD	BUFFER	ADD LOADCOUNT BYTES TO CURRENT POSITION
	ADDD	LOADFILEPOS+2
	STD	LOADFILEPOS+2
	BCC	LOADTYPE0.1
	LDX	LOADFILEPOS	PROPOGATE CARRY TO UPPER 16 BITS
	INX
	STX	LOADFILEPOS
LOADTYPE0.1	EQU	*
	LDX	#LOADSETPOS	AND POSITION THERE
	JSR	SYSCALLENTRY
	BRA	LOADNEXT
	page
*	LOAD BYTES AND THEN LOOK FOR NEXT RECORD
*
LOADTYPE2
	BSR	LOAD2AND3
*
*	GET NEXT RECORD, TYPE 0, 2, & 3 ALLOWED HERE
*
LOADCONTENTS	; LOAD CONTENTS OF OBJECT FILE
LOADNEXT	JSR	GETBYTE
	CMPA	#LOAD:TYPE2	DATA RECORD ?
	BEQ	LOADTYPE2	B/ YES
	CMPA	#LOAD:TYPE3	LAST DATA RECORD ?
	BEQ	LOADTYPE3	B/ YES
	CMPA	#LOAD:TYPE0	SKIP RECORD ?
	BEQ	LOADTYPE0	B/ YES
ERRBADLOADRECORD	EQU	*
	JSR	ERRET
	FDB	ERR:BADLOADRECORD

ERRNOTENOUGHROOM	EQU	*
	JSR	ERRET
	FDB	ERR:NOTENOUGHROOM
	PAGE
*	LOAD BYTES AND THEN QUIT
*
LOADTYPE3 ; LAST LOAD RECORD, LOAD THE BYTES AND QUIT
*
*
*	LOAD BYTES FROM DISK FILE INTO USER SPACE
*
LOAD2AND3 ; LOAD CONTENTS OF TYPE 2 OR TYPE 3 RECORD
	JSR	GETWORD GO GET THE LOAD ADDRESS
	STD	LOADADDRESS
	JSR	GETWORD
	STD	LOADCOUNT
	ADDD	LOADADDRESS	= ADDRESS OF LAST BYTE LOADED, +1
*
*	LOADBOUNDSCHECK -- MAKE SURE (A,B) <= DRIVER BASE
*	WE SHOULD ALSO CHECK TO MAKE SURE WE ARE LOADING ON TOP OF THE STACK, TOO!
*
	LDX	CODE+SDOS:CONFIGURATION
	SEC		ACTUALLY TEST FOR (A,B)-1 < DRIVERBASE
	SBCB	CNFG:DRIVERBASE+1,X	...?
	SBCA	CNFG:DRIVERBASE,X	...?
	BCC	ERRNOTENOUGHROOM	B/ HE'S DEAD!
LOAD2AND3.1 ; CHECK FOR DONE LOADING BYTES FROM THIS RECORD
	LDX	LOADCOUNT	ARE WE DONE?
	BEQ	LOAD2AND3RTS	B/ YES
LOAD2AND3LOOP ; LOAD SOME BYTES FROM THIS RECORD
	LDX	DECRYPTBUFPTR	TRY TO OPTIMIZE THE LOAD PROCESS
	CPX	#DECRYPTBUFFEREND IS THERE STILL STUFF IN THE DECRYPT BUFFER?
	BNE	LOAD2AND3BYTE	B/ STUFF STILL IN THE BUFFER
	LDD	LOADCOUNT	AT LEAST 8 BYTES TO LOAD?
	ANDB	#(\7)&$FF	(D) = MULTIPLE OF 8 BYTES TO LOAD
	STD	OPTIMIZEDLOADCOUNT
	BEQD	LOAD2AND3BYTE	B/ NOT ENOUGH BYTES TO LOAD
*
*	LOAD A MULTIPLE OF 8 BYTES INTO MEMORY QUICKLY
*
	LDD	LOADCOUNT	COMPUTE REMAINING LOADCOUNT AFTER OPTIMIZED LOAD
	CLRA
	ANDB	#7	= REMAINING LOADCOUNT AFTER MULTIPLE OF 8 LOADED
	STD	LOADCOUNT
	LDX	#LOADMULTIPLEOF8
	JSR	SYSCALLENTRY	EOF WILL OCCUR IF LOAD FILE IS NOT MULTIPLE OF 8 BYTES IN SIZE
	LDX	LOADADDRESS	WHERE TO START DECRYPTING
	LDD	LOADADDRESS	COMPUTE WHERE TO END DECRYPTING
	ADDD	OPTIMIZEDLOADCOUNT
	STD	LOADADDRESS	= 1ST PLACE NOT YET LOADED
	TST	OKTODECRYPTFLAG	ARE WE DECRYPTING?
	BEQ	LOAD2AND3.1	B/ NO, GO LOAD MORE BYTES
LOAD2AND3MASSDECRYPT ; DECRYPT JUST LOADED BLOCK OF BYTES
	JSR	DECRYPT	GO DECRYPT A BLOCK OF 8 BYTES
	LEAX	8,X	FIND NEXT BLOCK OF 8 BYTES
	CPX	LOADADDRESS	ALL LOADED BYTES DECRYPTED YET?
	BNE	LOAD2AND3MASSDECRYPT B/ NO, GO DECRYPT SOME MORE
	BRA	LOAD2AND3.1	YES, GO SEE IF MORE BYTES TO LOAD
*
*	LOAD A BYTE THE HARD WAY
*
LOAD2AND3BYTE ; LOAD ONE BYTE THE UNOPTIMIZED WAY
	BSR	GETBYTE
	LDX	LOADADDRESS
	STA	,X+
	STX	LOADADDRESS
	LDX	LOADCOUNT
	DEX
	STX	LOADCOUNT
	BNE	LOAD2AND3LOOP
LOAD2AND3RTS	OKRTS	YOU GUESS...
	PAGE
*
*
GETBYTE ; GET NEXT DECRYPTED BYTE FROM THE FILE INTO (A)
	LDX	DECRYPTBUFPTR	IS THE BUFFER EMPTY?
	CPX	#DECRYPTBUFFEREND
	BNE	GETBYTE1	B/ NOPE
	TST	OKTODECRYPTFLAG	BUFFER IS EMPTY, ARE WE DECRYPTING?
	BNE	GETBYTED	B/ YES, GO FETCH ANOTHER BUFFERFUL TO DECRYPT
	LDX	#GET1BYTE	NO, FETCH NEXT LOAD RECORD HEADER BYTE
	JSR	SYSCALLENTRY
	LDA	BUFFER	GET THE BYTE
	OKRTS

GETBYTED	; GET NEXT 8 BYTES FROM FILE AND DECRYPT
	LDX	#FILLDECRYPTBUFFER
	JSR	SYSCALLENTRY
	LDX	#DECRYPTBUFFER	START BUFFER POINTER AT BEGIN BUFFER
	JSR	DECRYPT	DECRYPT THE 8 BYTES JUST READ
GETBYTE1 ; FETCH DECRYPTED BYTE FROM BUFFER
	LDA	,X+	GET THE CHAR
	STX	DECRYPTBUFPTR
	OKRTS		I'M DONE
*
GETWORD ; GET 2 BYTES FROM THE DISK BUFFER INTO (D)
	BSR	GETBYTE
	PSHA
	BSR	GETBYTE
	TAB
	PULA
	RTS
	page
SETKEY ; SET "KEY" TO 8 BYTES POINTED TO BY (X)
	LDD	4,X	COPY KEY BYTES TO INLINE CODE
	STB	KEY5	DO SO IN AS APPARENTLY DISORGANIZED WAY AS POSSIBLE
	STA	KEY4
	LDD	0,X
	STA	KEY0
	STB	KEY1
	LDD	6,X
	STB	KEY7
	STA	KEY6
	LDD	2,X
	STA	KEY2
	STB	KEY3
	RTS

ZAPUSERSPACE	; OLD DECRYPTION KEY <> NEW, ERASE USER SPACE!
;	Note: If this Trashed the user space, it would be much harder to
;	determine program bounds of decrypted program
	IF	M6800!M6801
	TSX
ZAPUSERSPACEL ; ZAP ANOTHER USER SPACE BYTE
	CLR	,-X
	CPX	#$100	STOP ZEROING AT $100
	BNE	ZAPUSERSPACEL
	RTS
	ELSE	(M6809)
	LDX	#$100	USE BLOCKMOVE LIKE 360 MVC INSTRUCTION TO ZERO MEMORY
	STX	,X	ZERO $100 THRU 101, FIRST
	DEC	,X	DON'T YOU LOVE OBSCURE CODE?
	LDD	SDOSSTACK	= TOP OF SPACE TO ZERO
	SUBA	#2	(D) = # BYTES TO CLEAR, NOT COUNT PZ AND 256 BYTES OF STACK
	LEAY	2,X	WHERE TO MOVE STUFF TO...
	JMP	BLOCKMOVE	NOT SECURE TO CALL CODE+SDOS:BLOCKMOVE
	FIN
