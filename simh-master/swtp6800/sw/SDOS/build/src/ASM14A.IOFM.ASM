	TITLE	- - - I / O    S E C T I O N - - -
	PAGE	- - - I / O   F U N C T I O N S - - -
*
*
*   I / O   S E C T I O N   O F   A S M - 1 2 5
*   -------------------------------------------
*
*
*        All sorts of crud is located hereabouts...
*
*        (This is meant to be a general description of how
*         I/O is structured in ASM-125. . . . I'll tell you
*         more when I figure out what I'm doing.)
*
*
*	MODIFIED FOR SD I/O PACKAGE -- 7/22/1977 - STEFAN DEMETRESCU
*
*
*    H E R E   T H E R E   B E   M O N S T E R S
*
*
*
         PAGE     - - - L I S T - - -
*
*
*  L I S T
*
*
LST:     FCB      ':,TAB,0
*
LIST     EQU      *
         LDAA     PASS              DO NOTHING, PASS 1
         BEQ      LISTR
         LDAA     LIST:T
         LDAB     LIST:P
         ANDA     #LF:ERR           ERROR ON THIS LINE?
	STAA	DOCOPY	IF ERROR THEN COPY LO TO CONS
         BNE      LIST1             B/ YES: ALWAYS LIST.
* NO ERROR:
         BITB     #LL:LIST          LISTING ON?
         BEQ      LISTR             B/ NO: RETURN.
* NO ERROR, LISTING ON:  CHECK FOR SUPPRESSION OF THIS TYPE
* OF LINE.
*
LIST05	LDAA	LIST:T	RELOAD THE FLAGS
         LDX      #LSTSUP           GET @ OF LIST
LIST10   BITA     0,X
         BEQ      LIST11            IF T-BIT IS ON...
         BITB     1,X               ...AND P-BIT IS OFF...
         BEQ      LISTR             ...THEN DONT LIST ME.
*
LIST11   LEAX	2,X
         TST      0,X
         BNE      LIST10            KEEP TRYING.
*
* MUST PRINT:
*     STILL, IF LF:PTD IS ON, DON'T LIST THE LINE.
* I.E. DON'T LIST THE SI LINE BUT LIST OTHER SH--
*
*
LIST1    LDX      LBBEG
         JSR      PTTL	PUMP OUT # ON LEFT
*	JSR	PUTB3	  AND RESET ALL THE CRAP ON THE LEFT.  MAINLY FOR RPTS.
	LDAA	LIST:T	CK/ FLAGS FOR SOURCE LINE:
	BITA	#LF:PTD	  IS IT ALREADY PRINTED?
	BNE	LIST2	  B/ YES:  DON'T PRINT LINE AGAIN.


**** PRINT TEXT OF LINE ****

	LDAA	#TAB	MOVE OVER TO THE <LINE#> FIELD.
	JSR	LSTC	  (WHICH IS CONVENIENTLY PLACED AT THE FIRST TAB STOP)
         LDAB     LINEN+1	PRINT LINE #
         LDAA     LINEN
         JSR      PTNLB             PRINT W/ LEADING BLANKS
         LDX      #LST:
         JSR      PTTL              PRINT LINE, NEXT TAB STOP
         LDX      INBUF
         JSR      PTTL              PRINT INPUT LINE.
         BRA      LIST21
*
LIST2    EQU      *	SI LINE ALREADY PRINTED
         LDAA     COLNO
         BEQ      LIST21            OPTIMIZE CRLF.
         JSR      CRLF	NO XTRA CR/LF IF LINE IS NULL
*
LIST21   LDAA     LIST:T
         ORAA     #LF:PTD	MARK AS PRINTED
         STAA     LIST:T
         BITA     #LF:ERR
         BNE      LISTE	IF ERR - GO PRT IT
*
LISTR	CLR	DOCOPY	CLEAR IN CASE IT WAS SET
	JMP	PUTB3	  AND RESET THE LISTING BUFFER.
*
LSTSUP   EQU      *
         FCB      LF:PTD,LL:GEN
	IF	CONDASM=1!REPEAT=1
	FCB	LF:CA,LL:PCA
         FCB      LF:SKP,LL:SKP
	FIN
         FCB      LF:CC,LL:PCC
         FCB      0
         PAGE     - - - L I S T   E R R O R S - - -
*
*
* ONLY PRINTS AN ERROR ONCE SINCE ERR:F IS DESTROYED IN THIS ROUTINE
*
LISTE    EQU      *
         DO       BIGMSG=0
         LDX      #STARS
         JSR      PTTL
         FIN
         LDX      #EMSG
LIST30	EQU	*	COME HERE TO LIST ANOTHER ERROR...
	LSR	ERR:F+F$ERRMAX-1
LIST30$	RPT	F$ERRMAX-1
	ROR	ERR:F+F$ERRMAX-2-(*-LIST30$)/(LIST30$-LIST30)
         BCC      LIST31
* PRINT MESSAGE
         DO       BIGMSG=1
         STX      LTPTR2
         LDX      #STARS
         JSR      PTTL
         LDX      LTPTR2
         FIN
         JSR      PTTL
         INX
         DO       BIGMSG=1
         STX      LTPTR2
         JSR      CRLF
         LDX      LTPTR2
         FIN
         BRA      LIST30
*
LIST31   EQU      *                 SKIP TO NEXT MESSAGE.
         LDAA     0,X
         INX
         TSTA
         BGT      LIST31
         BEQ      LIST30
	IF	ERRLINES=1
	BSR	SVERL
	FIN
	IF	BIGMSG=0
	JSR	CRLF
         FIN
	BRA	LISTR	GO CLEAR DOCOPY FLAG
	PAGE	- - - S V E R L - - -
	IF	ERRLINES=1
*
*	SVERL:
*	SAVE ERROR LINE(S)
*	CONSTRUCTS ERROR-LINE LIST FOR PASS 2
*	ERROR LIST BLOCKS HAVE THE FOLLOWING FORM:
*	(X)  -->  0,1    NEXT; 0--> LAST
*	  2,3    FPGE OF ERROR
*	  4,5    LINE OF ERROR
*
SVERL	EQU	*
	LDAA	PASS	IS THIS PASS ONE ?
	BEQ	SVERLR	B/ PASS ONE, DON'T KEEP ERRORS!
	LDAA	OPTF	IS THIS STUFF GOING TO GO ANYWHERE?
	BITA	#OPT:EL	WELL?
	BEQ	SVERLR	IF NOT, RETURN
	LDX	.CURINCLB	GET ADDRESS OF ERROR CHAIN
	rpt	INCLB:ERRCHN	Add appropriate value;
	INX
	STX	TPTR	SAVE CHAIN SCAN POINTER
SVERL0	LDX	TPTR
	LDX	ERB:NEXT,X	FOLLOW LINK TO NEXT ERROR BLK
	BEQ	SVERL1	B/ FOUND END
	STX	TPTR	SAVE NEW ERROR BLOCK ADDRESS
	LDX	ERB:FPGE,X	IS THIS RIGHT PLACE FOR ERROR ?
	CPX	FPGE	( = PROPER FPGE ?)
	BNE	SVERL0	NO, KEEP LOOKING
	LDX	TPTR	YES, GET BLOCK ADDRESS AGAIN
	LDX	ERB:LINEN,X	RIGHT LINE NUMBER ?
	CPX	LINEN
	BNE	SVERL0	B/ NO, KEEP LOOKING
SVERLR	RTS		RETURN IF ALREADY AN ENTRY
*
SVERL1	LDAB	#ERB:SIZE:	GET 6 BYTES
	JSR	GTBLK
	BEQ	SVERLR	B/ NONE, QUIT SAVING ERROR BLOCKS
	STX	TPTR1	ELSE SAVE POINTER TO NEW BLOCK
*
*	CONSTRUCT NEW ERROR BLOCK
*
	CLR	ERB:NEXT,X	MARK AS LAST BLOCK...
	CLR	ERB:NEXT+1,X
	LDAA	FPGE	COPY IN FILE PAGE NUMBER
	LDAB	FPGE+1
	STAA	ERB:FPGE,X
	STAB	ERB:FPGE+1,X
	LDAA	LINEN	COPY IN LINE NUMBER
	LDAB	LINEN+1
	STAA	ERB:LINEN,X
	STAB	ERB:LINEN+1,X
*
*	CHAIN INTO LAST OF LIST
*
	LDX	TPTR	GET POINTER TO LAST BLOCK
	LDAA	TPTR1	GET ITS NEW NEXT POINTER
	LDAB	TPTR1+1
	STAA	ERB:NEXT,X	MAKE NEXT(LAST):=A(NEW BLOCK)
	STAB	ERB:NEXT+1,X
	RTS
	FIN
         PAGE     - - - E R R O R   M E S S A G E S - - -
*
EMSG     EQU      *
         DO       BIGMSG=1
	FCC	'Illegal Label.'
         FCB      0
	FCC	'Syntax Error.'
         FCB      0
	FCC	'Illegal Argument.'
         FCB      0
	FCC	'Undefined Symbol.'
         FCB      0
	FCC	'Double Definition.'
         FCB      0
	FCC	'Use of Doubly-defined Symbol.'
         FCB      0
	FCC	'Nesting Error.'
         FCB      0
	FCC	'End of Source File Encountered.'
         FCB      0
         FCC      'Overflow.'
         FCB      0
         FCC      'BRA/BSR Out of Range.'
         FCB      0
	FCC	'Illegal Value.'
         FCB      0
	FCC	'Illegal Digit.'
         FCB      0
         FCC      'Out of Memory.'
         FCB      0
	FCC	'Illegal String.'
         FCB      0
	FCC	'Phase Error.'
         FCB      0
	FCC	'Register Field Missing.'
	FCB	0
	FCC	'Input line too long.'
	FCB	0
	FCC	'INCLUDE file not found.'
	fcb	0
	fcc	'Impossible forced reference (<< or <).'
	fcb	0
	fcc	'Start address = 0 or does not match other end(s).'
	fcb	0
         ELSE
*
*  SHORT ERROR MESSAGES
*
         FCB      'L,0              ILLEGAL LABEL
         FCB      '?,0              SYNTAX ERROR
         FCB      'A,0              ILLEGAL AF
         FCB      'U,0              UNDEFINED SYMBOL
         FCB      'D,0              DOUBLE DEF
         FCB      '2,0              USE OF DOUBLE-DEF
         FCB      'N,0              NESTING ERROR
	FCB	'E,0	END OF SOURCE HIT.
         FCB      'O,0              OVERFLOW
         FCB      'B,0              OUT-OF-RANGE BRANCH/BSR
         FCB      'V,0              ILLEGAL VALUE
         FCB      'I,0              ILLEGAL DIGIT
         FCB      'M,0              OUT OF MEMORY
         FCB      'S,0              ILLEGAL STRING
	FCB	'P,0	PHASE ERROR.
	FCB	'R,0	Register Field Missing.
	FCB	'T,0	Input line too long.
	FCB	'Q,0	Impossible forced reference
	FCB	'F,0	File not found (INCLUDE)
	FCB	'^,0	Start address = 0 or does not match other end(s)
         FIN
         FCB      -1&$FF
*
STARS    FCC      ' *** '
         FCB      0
         PAGE     - - - L S T C - - -
*
*  L S T C
*
*        OUTPUT A CHARACTER TO LO, FORMATTED.
*
* EXPAND TABS - CALL TOLO TO PUT CHARS OUT
*
*
LSTCP    PSHB
         BSR      LSTC
         PULB
         RTS

LSTC     EQU      *
         TSTA
         BMI      TOLO             ANY CH W/ SIGN ON IS UNFORMATTED.
         CMPA     #TAB              ALL TABS GET EXPANDED.
         BEQ      LSTC2
*
	IF	1	;VT driver should do this
LSTC3    ANDA     #$7F
         CMPA     #$D
         BEQ      LSTC5
	CMPA	#$C
	BEQ	LSTC5
         CMPA     #$1F
         BGT      LSTC5
         ADDA     #$40              CHANGE CTRL CH TO ^, CHAR.
         PSHA
         LDAA     #'^
         BSR      TOLO
         PULA
	FIN
LSTC5    BRA	TOLO
*
*
*
*  T O L O
*
* PUT CHARACTER IN A TO LO - IF LLTP=0 (I.E. NO LINES LEFT
* ON PAGE) THEN SET LLTP=DEPTH - CALL PUTHDR.
*
* IF <CR> ADD <LF>
*
*
CRLF     LDAA     #$D
TOLO     EQU      *
         LDAB     PASS
         BEQ      TOLR
         ANDA     #$7F
         CMPA     #$D
         BEQ      TOL1
         LDAB     WIDTH
         CMPB     COLNO
         BLS      TOLR              TRUNCATE BUFFER
*
TOL3     LDAB     LLTP
         BNE      TOL31             DO TITLE?
         PSHA
         LDAB     DEPTH
         STAB     LLTP
         JSR      PUTHDR
         PULA
*
TOL31    CMPA     #$20              CH LESS THAN $1F DONT SPACE.
         BLT      TOL4
         INC      COLNO
TOL4,PUTLO EQU	*
	LDAB	OPTF	PRINTING TO LO?
	BITB	#OPT:LO
	BEQ	NOPUT	  B/ NOT PUTTING, CHECK FOR DIAGNOSTIC OUTPUT.
	PSHA		SAVE FOR DO
	LDAB	#LO	TO LO
	JSR	WASCII$	PRT IT
	PULA
	LDAB	OPT1F
	BITB	#OPT1:DO	Diagnostic Output?
	BEQ	TOLR	  B/No output.
NOPUT	LDAB	DOCOPY	C/ IS THIS AN ERROR LINE?
	BEQ	TOLR	  B/ NO..... CAN'T COPY.
	JMP	PUTC	ELSE, PUT TO CONS&RETURN
*
TOLR     RTS
*
TOL1     EQU      *
         CLR      COLNO             <CR> SEEN
         BSR      TOL3              OUTPUT THE CR,
	LDAA	DEPTH	Page size = 0??
	BEQ	TOL2	B/YES
         DEC      LLTP
TOL2	RTS
*
*
LSTC2    LDAB     WIDTH
	CMPB	COLNO	EXPANDING TABS,
         BLS      TOLR              B/ PAST END OF PAGE
         LDAA     #BLANK
         BSR      TOLO
         LDAA     COLNO             WHATS THE COLUMN?
         BEQ      TOLR              B/ STILL ZERO ==> QUIT.
         LDX      TABTBL            SCAN FOR TABS
         CLRB
*
LSTC2L   CMPA     0,X               IS THIS IT?
         BEQ      TOLR              B/ YES: ALL DONE.
         BHI      LSTC21            B/ TOO SMALL.
         INCB                       REMEMBER THAT THERE'S ONE BIGGER
LSTC21   EQU      *
         INX
         TST      0,X
         BNE      LSTC2L            MORE TO SCAN.
         TSTB                       ANY USE TRYING AGAIN?
         BEQ      TOLR              B/ NO:  ONE BLANK DID IT.
         BRA      LSTC2
*
NUMSTRT  EQU      15                START COLUMN FOR NNNN:
LSTSTRT  EQU      NUMSTRT+6
TABTB:	EQU	*
	FCB	NUMSTRT,LSTSTRT

	PGEN	DEBUG	LIST IT ALL IF DEBUGGING.
	RPT	8
	FCB	LSTSTRT+8*(*-TABTB:-1)
	PGEN	0

	FCB	0
	PAGE	- - - S E T D A T I M - - -
*
* SET THE DAY AND TIME IN THE DATIME BUFFER SO THAT IT COMES OUT
*	ALL PRETTY ON THE LISTING (WEE)!
*
SETDATIM	LDX	#DATIME	LOAD BUFFER ADDRESS
	STX	TPTR	SAVE IN TEMP PTR
	JSR	DATE$	GET THE DATE
	BSR	MOVEIN	MOVE THE STRING IN
	LDAA	#BLANK	LOAD THE SPECE
	STAA	0,X	SA		PUT BETWEEN DATE $ TIME
	INX
	STX	TPTR	SAVE
	JSR	TIME$	GET THE TIME
	BRA	MOVEIN

MOVEL	LDX	TPTR1
MOVEIN	LDAB	0,X	LOAD THE NEXT CHAR TO SAVE
	INX		INC PTR
	STX	TPTR1	SAVE
	LDX	TPTR	LOAD OTHER PTR
	STAB	0,X	SAVE CHAR
	INX		PT TO NEXT 
	STX	TPTR
	DECA		ANY MORE CHARS?
	BNE	MOVEL	IF SO, GO DO NEXT
	CLR	1,X	CLEAR IN CASE THIS IS THE END OF THE BUFFER
	RTS
*
	PAGE	- - - P H L - - -
*
* PRINT HIGH AND LOW NIBBLES OF A IN HEX TO LO DEVICE
*
PHL	JSR	ATX2	TURN INTO A PUMPKIN
	PSHB		(APPLY PRESSURE TO DEHYDRATE)
	BSR	PHL.LSTC	SEND TO THE MARKET
	PULA		REHYDRATE
	BRA	PHL.LSTC

PUTTAB	LDAA	#TAB	  (SALESMAN ENTERS AND PICKS UP TAB)
PHL.LSTC	JMP	LSTC	EXPAND LINE OF PRODUCTS TO LADIES' WARE.
*
	PAGE	- - - P U T H D R - - -
ASMBF	FCB	$C	Top-of-form
ASMBF1	FCB	$D,$D,$D,$D	margin at top of page
	IFUND	MC6809
	FCC	'ASM/6800 '
	ELSE
	FCC	'ASM/6809 '
	FIN
	FCB	(RELEASE/$10&$F)+'0
	FCB	'.
	FCB	(RELEASE&$F)+'0
ASMPATCH	FCB	ASSEMBLY
	FCB	'0	PATCH NUMBER
		; ASM IS WHICH.....
	FCC	/: /
	FCB	0
*
PNUM	FCC	/; Page /
	FCB	0
FSMI	FCC	/; Form /
	FCB	0
*
HDRNAME	EQU	$15	FIRST COLUMN OF NAME FIELD,
HDRTTL	EQU	40	FIRST COLUMN OF TITLE FIELD.

HDRTABS	FCB	HDRNAME,HDRTTL,0	TAB TABLE FOR HEADER.
		; NECESSARY SINCE DUMP SUPPLIES ITS OWN.
*
*
*
PUTHDR   EQU	*


*** MAKE SURE WE DON'T BLOW UP DUE TO SHORT PAGE LENGTH...
	LDAA	LLTP	THIS COUNTER HAS BEEN SET TO FORM DEPTH.
	CMPA	#MINDEP	IS IT REASONABLE?
	BHI	PUTHDR1	B/ YUP.  OUTPUT HEADER.
	LDAA	#$FF	OTHERWISE SET LINES LEFT TO 255,
	STAA	LLTP
	CLR	DEPTH	AND SET PAGE SIZE TO ZERO

*** GET READY TO OUPUT HEADER.
PUTHDR1	EQU	*
	LDAA	DOCOPY	NEVER COPY HEADER
	PSHA	SO SAVE FLAG
	CLR	DOCOPY
*
	LDAA	TABTBL	SAVE OLD TAB POINTER,
	PSHA
	LDAA	TABTBL+1
	PSHA
	LDX	#HDRTABS	SO'S WE CAN USE OUR OWN.
	STX	TABTBL	(DON'T TALK TO ME ABOUT LAZY)


*** AT LAST... START DOING WORK FOR HEADER ***

	LDX	PAGEN	WHICH PAGE IS THIS?
	CPX	#1	PAGE # 1?
	BNE	PUTHDR2	B/ NO, USE ORDINARY PUT.
	LDX	#ASMBF1	PAGE # 1:  DON'T SKIP TO BOTTOM OF PAGE.
	BRA	PUTHDR5

PUTHDR2	LDX	#ASMBF
PUTHDR5	LDAA	LLTP	(BUT ACCOUNT FOR THOSE FOUR LINES...)
	SUBA	#4	(AS IF BY MAGIC)
	STAA	LLTP
	BRA	PUTHDR3

PUTHDR3	BSR	PTTL
	LDAA	LINEPC
	BSR	PHL
	LDAA	LINEPC+1
	BSR	PHL
	BSR	PUTTAB	OUTPUT A TAB BETWEEN PC AND NAME.
	LDX	#NAMEBF
	BSR	PTTL
	DO	TITLES=1
	BSR	PUTTAB	OUTPUT A TAB TO GET TO TITLE FIELD.
	LDX	#TTLBUF
	BSR	PTTL
	FIN
	JSR	CRLF
	LDX	#DATIME
	BSR	PTTL
	LDX	#PNUM
	BSR	PTTL	PRT ALL THE FUNNY LETTERS
	LDAB	PAGEN+1
	LDAA	PAGEN
	BSR	PTNL
	LDX	#PAGEN
	JSR	INCBCD		BUMP PAGE #
	LDX	#FSMI
	BSR	PTTL
	LDAB	FPGE+1
	LDAA	FPGE
	BSR	PTNL
	DO	TITLES=1
	JSR	PUTTAB	OUTPUT A TAB TO GET TO SUBTITLE FIELD.
	LDX	#SBTTBF
	BSR	PTTL
	FIN
	JSR	CRLF
	LDX	.CURINCLB	OUTPUT SOURCE FILE NAME
	LDAA	INCLB:LEN,X	LENGTH OF NAME
	PSHA
	JSR	INCLB@NAME	GET ADDRESS OF NAME
	PULA
PUTHDR4	PSHA	SAVE COUNTER
	LDAA	0,X
	INX
	STX	LTPTR	COPY
	LDAB	LTPTR+1	NAME
	PSHB
	LDAB	LTPTR	ONE
	PSHB
	JSR	LSTC	CHARACTER
	PULX		AT A TIME
	PULA
	DECA
	BNE	PUTHDR4
	JSR	CRLF
	PULA	GET OLD TABTBL+1,
	STAA	TABTBL+1	SHOVE IT BACK.
	PULA	GET OLD TABTBL,
	STAA	TABTBL	SHOVE IT BACK.
	PULA	GET THE OLD DOCOPY FLAG
	STAA	DOCOPY	AND PUT IT BACK
	RTS
*
         PAGE     - - - P T T L - - -
*
*
*  P T T L
*
*        Put string (X) to listing.
*
*        NOTE THE IMMENSE HASSLE DUE TO THE POSSIBILITY THAT WE
*        MIGHT GET REENTERED (BY CAUSING A PAGE THROW).
*
*		ENDED BY NUL.
*
PTTL     EQU      *
PTTL0    LDAA     0,X
         BNE      PTTL1             B/ NOT END.
         RTS
*
PTTL1    INX                        POINT TO NEXT
         STX      LTPTR             SAVE POINTER.
         LDAB     LTPTR+1           PUSH IT.  WE MIGHT GET
         PSHB
         LDAB     LTPTR
         PSHB                       ...REENTERED.
         JSR      LSTC              OUTPUT (A)
	PULX
         BRA      PTTL
         PAGE     - - - P T N L - - -
*
*
*   P T N L
*
*        Output number in A,B to LO suppressing leading zeros.
*        A,B must not be zero.
*
*
PTNL     EQU      *
         PSHB
         PSHA
         CLRB
         BRA      PTNLE
*
PTNLB    EQU      *
         PSHB
         PSHA
         LDAB     #BLANK            BLANK FILL
*
PTNLE    TSX
         LDAA     1,X
         STAB     1,X
*
PTNL1    JSR      ATX2
         TSX
         PSHA
         LDAA     0,X
         STAB     0,X
         JSR      ATX2
         PSHB
	PAGE
*
*  STACK NOW LOOKS LIKE THIS:
*
*   SP   ==> NIBBLE 2 OF NUMBER IN ASCII
*            NIBBLE 3 OF NUMBER IN ASCII
*            NIBBLE 4 OF NUMBER IN ASCII
*   X+1  ==> FILL CH.
*
         LDAB     #4                OUTPUT 4 CH,
         STX      LTPTR
*
PTNL2    EQU      *
         CMPA     #'0               IS IT A LEADING ZERO?
         BNE      PTNL3               B/ NO.  START SIGNIFICANCE
	CMPB	#1	IS THIS THE LAST DIGIT?
	BEQ	PTNL3	  B/ YES.  UNCONDITIONALLY START SIGNIF.
         LDAA     1,X               GET FILL,
         BEQ      PTNL21              B/ DON'T PUT A NULL.
         JSR      LSTCP
         LDX      LTPTR             GET POINTER BACK.
*
PTNL21   EQU      *
         DECB
         PULA
         BNE      PTNL2
         RTS
*
PTNL3    EQU      *
         JSR      LSTCP
         PULA
         DECB
         BNE      PTNL3
COLRTN   RTS
         PAGE     - - - P U T B - - -
*
*
*   P U T B
*
*        Output a byte to the BO tape, if this is pass 2 and
*        if BO was requested.  Always increment PC.  Also,
*        arrange for the listing of the byte.  Call LIST if we
*        just ran out of space in this listing line.
*
*
PUTB     EQU      *
         LDAB     PCPUT             HAS PC BEEN LISTED YET?
         BNE      PUTB1             B/ YES:  DONT WORRY ABOUT IT.
         PSHA                       ELSE SAVE OUR BYTE,
         BSR      PUTPC             OUTPUT THE PC,
         PULA                       AND GET OUR BYTE BACK.
*
PUTB1    EQU      *
         JSR      PNCHB             OUTPUT BYTE TO BO
         LDX      PC                BUMP PC
         INX
         STX      PC
         LDX      LBFPT             GET POINTER TO NEXT BYTE IN LIST BUF
PUTB01   BSR      ATX2              MAKE (A) INTO HEX IN A,B
         STAA     0,X               HOSE INTO BYTE 1
         STAB     1,X               HOSE INTO BYTE 2
         CLR      2,X               END BUFFER MARK.
         INX
         CPX      EOLBF             END OF BUFFER?
         BEQ      PUTB2             B/ YES (I HAVE SOME SLOP)
         INX
         CPX      EOLBF             I REPEAT...
         BEQ      PUTB2             IF CPX GAVE REAL CC, T'WERE EASIER..
         STX      LBFPT             SAVE POINTER
         RTS                        AND SPLIT.
*
PUTB2    EQU      *
         JSR      LIST              OUTPUT THIS LINE.
PUTB3    CLR      PCPUT             PC HASN'T BEEN PUT YET,
         LDX      LBBEG             INITIALIZE POINTER,
PUTB4    STX      LBFPT
         CLR      0,X               AND MARK END OF BUFFER
         RTS
         PAGE     - - - C L R L S T - - -
*
*
*   C L R L S T
*
*        Clears listing flags, marks PC as not output.
*
CLRLST   EQU      *
         CLR      LIST:T            CLEAR FLAGS FOR THIS LINE,
         BRA      PUTB3             AND INITIALIZE ME.
         PAGE     - - - B I N   T O   H E X   R O U T I N E S - - -
*
*
*   A T X
*
*        Converts low-order nibble of ACCA to hex character in A.
*
*   A T X 2
*
*        Converts byte in ACCA to two hex characters:  high in
*        ACCA, low in ACCB.
*
*
ATX2     EQU      *
         PSHA                       SAVE BYTE,
         BSR      ATX               CONVERT LOW NIBBLE.
         TAB                        SAVE IN ACCB,
         PULA                       AND CONVERT HIGH NIBBLE.
         LSRA                       EXTRACT IT
         LSRA
         LSRA
         LSRA
*                                   AND FALL INTO ATX.
*
ATX      EQU      *
         ANDA     #$F               MASK OFF NIBBLE,
         ADDA     #'0               MAKE HEX DIGIT,
         CMPA     #'9               OVERFLOW?
         BLE      *+4               B/ NO: RETURN
         ADDA     #'A-'9-1          ELSE ADJUST
         RTS                        AND SPLIT.
         PAGE     - - - P U T P C - - -
*
*
*   P U T P C
*
*        Puts PC into listing buffer followed by a blank.  Sets
*        PCPUT so we remember that we put it.  Note that PC is
*        always listed at the extreme left of the listing buffer.
*
*
PUTPC    EQU      *
	LDX	PC	SAVE FOR HEADER OUTPUT
	STX	LINEPC
         LDX      LBFPT             GET LISTING POINTER,
         LDAA     PC                THOSE OF YOU WHO LIKE PRISTINE
         BSR      PUTB01            CODE, PLEASE IGNORE THESE
         LDAA     PC+1              FOUR LINES OF CODE.  YOU
         BSR      PUTB01            REALLY DIDN'T WANT TO KNOW...
         LDAA     #BLANK            PUT A BLANK AFTER IT
         STAA     0,X               THUSLY,
         STAA     PCPUT             REMEMBER THAT PC IS PUT,
         INX
         BRA      PUTB4             AND APPEND A NULL.
         PAGE     - - - M R K S K P - - -
*
*
*   M R K S K P
*
*        Shovels *S* into listing buffer, sets LF:SKP and
*        returns.
*
*
MRKSKP   EQU      *
         LDAA     LIST:T            MARK AS SKIPPED
         ORAA     #LF:SKP
         STAA     LIST:T
         BSR      STUFFB
         FCC      / *S*/
         FCB      0
         RTS


         PAGE     - - - P U T V - - -
*
*
*   P U T V
*
*        Outputs VAL as a 2-byte number in listing field.  If
*        PCPUT is clear, then 2 blanks are stuffed into the
*        the listing buffer.
*
*        In any event, VAL is then stuffed into the next available
*        bytes.
*
*
PUTV     EQU      *
         LDX      LBFPT             GET A POINTER,
         LDAA     PCPUT             HAS PC BEEN OUTPUT?
         BNE      PUTV1             B/ YES.  NOTHING TO DO.
         BSR      STUFFB            STUFF SOME BYTES
         FCC      /  /               2 BLANKS
         FCB      0                 AND A NULL
PUTV1    LDX      LBFPT             GET POINTER.
         LDAA     VAL
         JSR      PUTB01            OUTPUT IT.
         LDAA     VAL+1
         JMP      PUTB01            ALSO LOW BYTE.
         PAGE     - - - S T U F F B - - -
*
*
*   S T U F F B
*
*        Stuffs the string following the call into the listing
*        buffer.  No idiot checks.  Must be terminated by a null.
*
STUFFB   EQU      *
	PULX		Get return address
STFFBL   EQU      *
         LDAA     0,X
         STX      TPTR1
         LDX      LBFPT
         STAA     0,X
         BEQ      STFFBR
         INX
         STX      LBFPT
         LDX      TPTR1
         INX
         BRA      STFFBL
STFFBR   LDX      TPTR1             GET RETURN @
         JMP      1,X               AND LEAVE.
         PAGE     - - - P N C H B - - -
*53*
*
* NAME: PNCHB
*        PUNCH (A) IN OBJECT TAPE.
*
* INPUT:
*        ACCA     BYTE TO BE PUNCHED
*        BBPTR    NEXT FREE BYTE IN BINARY BUFFER
*        *BBPTR   CURRENT CHECKSUM.
*
* OUTPUT:
*        BBPTR    NEXT FREE BYTE IN BINARY BUFFER
*        *BBPTR   CURRENT CHECKSUM
*
PNCHB    LDX      BBPTR             GET POINTER TO NEXT BYTE,
         CPX      #BBEND            END OF BUFFER?
         BNE      PNCH1             B/ NO:  JUST ADD BYTE.
         BSR      PNCHIT            ELSE PUNCH THE BUFFER
PNCH1    TAB                        GET COPY OF BYTE,
         ADDB     0,X               MAKE NEW CHECKSUM;
         STAA     0,X               SAVE THIS BYTE,
         INX
         STAB     0,X
         INC      BBCNT             INCREMENT # OF BYTES IN THIS RECORD.
         STX      BBPTR             AND SAVE POINTER
PNCHR    RTS                        GO AWAY.
         PAGE	- - - P N C H I T - - -
*53*
*
* NAME:  PNCHIT
*        PUNCHES ACCUMULATED BINARY RECORD, RESETS THE POINTERS.
*	SAVES A.
*
PNCHIT   EQU      *
         LDAB     PASS              ANYTHING TO DO, THIS PASS?
         BEQ      PNCSET
         LDAB     OPTF              BINARY WANTED?
         BITB     #OPT:BO
         BEQ      PNCSET            B/ NO:  RETURN.
         PSHA
         LDAB     BBCNT             ANYTHING TO PUNCH?
	CMPB	#2
         BLS      PNCSPC            B/ NO:  JUST RESET THE BUFFER.

	BSR	OutputSDOSrecord
	BRA	PNCSPC

*
* SET UP BUFFER FOR NEXT TIME THRU.
PNCSET   PSHA                       EXTERNAL ENTRY:  SAVE ACCA
PNCSPC   LDX      #BBST             INITIALIZE POINTER,
         CLR      0,X                  **      CHECKSUM,
         CLR      BBCNT                **      COUNT,
         STX      BBPTR                **      POINTER.
         LDAA     PC                   **      PC (HIGH)
         BSR      PNCH1
         LDAA     PC+1                 **      PC (LOW)
         BSR      PNCH1
         PULA                       RESTORE ACCA,
         RTS                        AND EXIT.
         PAGE     - - - P N C E O F - - -
   
*
*
*   P N C E O F
*
*        Punches last buffer, S9, 30 nulls, and turns off
*        punch.
*
PNCEOF   EQU      *
         LDAB     OPTF              GET OPTION FLAGS:
         BITB     #OPT:BO           BO REQUESTED?
         BEQ      PNCER             B/ NO:  RETURN.
         BSR      PNCHIT

	TST	CURREC+R$VALID	Have we output a record to the file?
	BNE	PNCEOF1	  B/ Yes; no problem.
	JSR	PutRecordHdr	Otherwise, make room for the record hdr,
PNCEOF1	equ	*	  ...then output a zero-length LOADGO.
	LDAA	#1	Set argument: this is the last record;
	JSR	FinishCurrec	  go write its header.
	LDAB	#BO	Position the binary file to...
	JSR	REWANY	  ...its beginning.

**** Construct a Start record on the stack ****
	LDAA	SADDR	Load up the start address (high)
	LDAB	SADDR+1	  (low)
	PSHB		Save it (twice) on the stack.
	PSHA			...
	PSHB			...
	PSHA			...
	LDAA	#RTYPE$START	Load type byte,
	PSHA		  put it at head of start record.
	TSX		Load pointer to record into X,
	COM	3,X	  make last 2 bytes complement of...
	COM	4,X	  ...the real start address.

**** Send the start record to an unsuspecting file ****
	LDAA	#5	Load the record size,
	LDAB	#BO	  load the channel number.
	JSR	WRITEREC$	Go go go.
*			Pop the stack to eliminate the record;
	LEAS	5,S	  five of these are needed.

PNCER    RTS
	PAGE	- - -  O u t p u t   S D O S   R e c o r d  - - -
*****************************************************************************
*
* Name:  OutputSDOSrecord
*
* Function:
*	Outputs the record that has been accumulated in BBST..BBEND into
*	the current SDOS-style load file.  Attempts to optimize the
*	alignment of the load records according to SDOS conventions.
*
* Changed:		Preserved:
*	A B C X			***NONE***
*
* Input:
*	BBST [0..1]	Base address of this load record.
*
*	BBCNT		B/ Number of bytes in binary buffer,
*			   including the base address.
*
*	BBST [2..BBCNT-1]	Data bytes to be output.  If BBCNT=2,
*			   this is a null record and is ignored.
*
*	Currec+R$VALID	B/ Flag, indicating (if non-zero) that
*			   a record segment is open in the output
*			   file.
*
*	Currec+R$FILEBASE	Q/ File position pointer.  If R$VALID is
*			   true, then this is the base file address
*			   of the first data byte in the current
*			   record.  If R$VALID is false, this is
*			   the file address of the first available
*			   byte in the file.
*
*	Currec+R$BASE	W/ [Assuming R$VALID is true:]  Target
*			   load address of the first data byte in
*			   the currently open record of the output
*			   file.
*
*	Currec+R$LEN	W/ [Assuming R$VALID is true:] Length of
*			   the currently open record of the output
*			   file.  If R$VALID is false, this MUST
*			   be zero.
*
*****************************************************************************

OutputSDOSrecord	EQU *
	LDX	BBST	Copy target load address for this record
	STX	NEWREC+R$BASE	  to a safe/handy place.
	LDAA	BBCNT	Load the length of the record,
	SUBA	#2	  remove crud counting PC at head of record,
	BEQ	OutSDOS%	  B/ Split if there's nothing to output.
;			     (Redundant check).
	STAA	NEWREC+R$LEN+1	Save the count in a safe/handy place.
	CLR	NEWREC+R$LEN	Zap the high order byte.  It makes Terry
			; happier if we do it here.

**** Check alignment conditions, and call AlignSDOSrecord if appropriate ****
	BSR	CheckSDOSalign	Sets CY if we should align.
	BCC	OutSDOS$1	  B/ No alignment, si vous plait.
	JSR	AlignSDOSrecord	Else, do the alignment.

**** Leave room for record header, if necessary ****
OutSDOS$1	EQU *
	LDAA	CURREC+R$VALID	Are we currently building a record?
	BNE	OutSDOS$2	  B/ yeah.... no need for header.
	JSR	PutRecordHdr	Otherwise, put the record header.

**** Output the record segment ****
OutSDOS$2	EQU *
	JSR	GetNewrecBase	Prepare to make a file position....
	JSR	FilePosition	  X will return as pointer to 4-byte buffer.
	LDAB	#BO	  which has been filled w/ appropriate file
	JSR	POSITION$	  address.
	LDX	#BBST+2	Load base address of record,
	LDAA	NEWREC+R$LEN+1	  load length,
	LDAB	#BO	  set channel,
	JSR	WRITEREC$	  and send a number of bytes.
	JSR	GetNewrecEnd	Now, compute new length of CURREC according
	LDX	#CURREC+R$BASE	  to:  CURREC.LEN max ...
	JSR	DP$SUB	         ... (NEWREC.BASE+NEWREC.LEN)-CURREC.BASE
	LDX	#CURREC+R$LEN
	JSR	DP$MAX
	STAA	0,X	Save result in CURREC.R$LEN
	STAB	1,X
OutSDOS%	RTS		  and split.
	PAGE	- - - C h e c k   S D O S  A l i g n - - -
*****************************************************************************
*
* Name:  CheckSDOSalign ()
*
* Function:
*	Examines the new record segment that is to be output, along with the
*	OutSDOSrecord data base, and decides whether we should attempt to
*	align this record by ending the old record and starting a new one.
*	If so, the CY flag is set on exit.
*
* Results:
*	CY		If set, CheckSDOSalign thinks we should
*			  attempt to align this record with the
*			  sector size on the disk.
*
* Algorithm:
*	The following checks are made:
*
*	*  We call CkNewrecord, which will close the current record if
*	   it makes sense/is necessary to do so.
*
*	*  If the new record is not of maximum length, we do not align.
*		--ELSE--
*	*  If the current load record segment is empty, we align.
*		--ELSE--
*	*  If the top target address of this record is less than that of
*	   the top of the new record, and writing the new record will cause
*	   a sector crossing, we align.
*
*****************************************************************************

CheckSDOSalign	EQU *
	BSR	CkNewRecord	Close current record, if necessary.
	LDAB	NEWREC+R$LEN+1	If length <> maximum permitted, this...
	CMPB	#BBEND-(BBST+2)	  record is a 'patch' record & should not
	BNE	CkSDOSalign:NO	  be aligned.  B/ Don't align.

**** Check for no current record ****
	LDAB	CURREC+R$VALID	Is current record valid?
	BEQ	CkSDOSalign:YES	  B/ No:  then align this record.

**** Check for record overlapping current record ****
	JSR	GetNewrecEnd	Get address of end of this record segment.
	PSHB		Save again during computation.
	PSHA
	JSR	GetCurrecEnd	Compute end address of Current Record.
	TSX		Point to (Newrec.BASE+Newrec.LEN).
	JSR	DP$CMP	  Check other bound for containment.
	LEAS	2,S	  (Pop used-up value)
	BCC	CkSDOSAlign:NO	  B/ Record is contained: don't align.

**** Newrec overlaps Currec, but we may still want to align if this record ****
**** will cross a sector boundary in the file.                             ****
	SUBB	#1	Compute address of last byte in CURREC.
	SBCA	#0	  ....
	JSR	SectorNum	Extract sector field from CURREC's end address.
	PSHB		Save on TOS.
	PSHA
	JSR	GetNewrecEnd	Compute sector number for last byte written.
	SUBB	#1
	SBCA	#0
	JSR	SectorNum
	TSX		Now, compare them.
	JSR	DP$CMP
	LEAS	2,S	  (pop stack)
	BNE	CkSDOSalign:YES	B/ Sector crossing --> align!

**** Dont Align..... ****
CkSDOSalign:NO	EQU *
	CLC
	RTS

**** Do Align ...... ****
CkSDOSalign:YES	EQU *
	SEC
	IF	1	Don't ever align for now.
	CLC
	FIN
	RTS
	page	- - - C h e c k   N e w   R e c o r d - - -
*****************************************************************************
*
* Name:  CkNewRecord ()
*
* Function:
*	Compares the new record segment against the current record segment.
*	If the new record segment cannot/should not be included in the
*	current record, we call FinishCurrec to close the current record
*	segment.
*
* Changes:		Preserves:
*	****ALL****		****NONE****
*
*****************************************************************************

CkNewRecord	equ *
	LDAA	CURREC+R$VALID	Is current record segment open?
	BEQ	CkNewRec%	  B/ No:  nothing to do.
	JSR	GetNewrecBase	Is target load address of new record
	LDX	#CURREC+R$BASE	  below target load address of
	JSR	DP$CMP	  current record segment?
	BCS	CkNewrec:POOF	  B/ Yes:  finish current record.
	JSR	GetCurrecEnd	Is target load address of new record
	ADDB	#1	  beyond a "reasonable" distance from
	ADCA	#0	  the end target address of the currently
	BCS	CkNewrec%	  open record? B/ We are too close to $FFFF.
	LDX	#NEWREC+R$BASE	  If so, finish current record.
	JSR	DP$CMP	  .....
	BCC	CkNewrec%	  B/ It's close enough, use current record.

**** Finish current record & split ****
CkNewrec:POOF	equ *
	CLRA		Flag:  not last record.
	JSR	FinishCurrec

**** Split ****
CkNewrec%	equ *
	RTS
	PAGE	- - - P u t   R e c o r d   H d r - - -
*****************************************************************************
*
* Name:  PutRecordHdr ()
*
* Function:
*	When we are beginning a new load record, PutRecordHdr does all
*	the work required to set up the software tables.  It assumes that
*	CURREC.FILEBASE points to the first free byte of the output file.
*	CURREC.FILEBASE is advanced 5 bytes, to reserve space for a load
*	record header; CURREC.LEN is set to zero, to indicate the lack of
*	data output; CURREC.VALID is set non-zero ("true") to indicate
*	that the rest of the nonsense is valid; and CURREC.BASE is set to
*	NEWREC.BASE, which is assumed to be the base target load address
*	for this record.
*
* Changed:		Preserved:
*	***ALL***		***NONE***
*
*****************************************************************************

PutRecordHdr	EQU *
	CLRA		Load AB w/ 5
	LDAB	#5
	STAB	CURREC+R$VALID	Mark Currec as valid.
	JSR	UpdateFilebase	Add 5 to Filebase.
	LDX	NEWREC+R$BASE	Set new base of record,
	STX	CURREC+R$BASE
	LDX	#0	Set length to zero.
	STX	CURREC+R$LEN
	RTS
	PAGE	- - - A l i g n   S D O S   R e c o r d - - -
*****************************************************************************
*
* Name:  AlignSDOSrecord ()
*
* Function:
*	Assuming that we really do want to perform an alignment operation,
*	we do the work here.  Of course, if the record just happens already
*	to be aligned, we don't have to do much.
*
* Arguments:
*	***NONE***
*
* Changes:
*	A, B, C, X
*
* Input:
*	CURREC+R$VALID	Byte flag, indicating whether we are
*			  currently building a record.
*
*	   ...+R$BASE	Target load address of current record.
*
*	   ...+R$FILEPOINTER	File address of start of data bytes in
*			  current record.
*
*	NEWREC+R$BASE	Target load address of new record segment
*			  (which we are attempting to align).
*
*	   ...+R$LEN	Length (number of data bytes) in new
*			  record segment.
*
*	SectorMask	Word, mask for extracting sector field
*			  of a file address.  Comes from disk
*			  DCB via SYSCALL.
*
*	NotSectorMask	Word, mask for extracting byte-within-sector
*			  field of a file address.  Source same as
*			  SectorMask's.
*
*****************************************************************************

AlignSDOSrecord	equ *
	LDAA	CURREC+R$VALID	Is a record being built?
	BEQ	AlignSDOS$1	  B/ No-- must align.

**** Check whether current record is already aligned ****
	JSR	GetCurrecBase	Get target load address of Currec.
	JSR	SectorRem	  and compute offset in current sector.
	PSHB		Save on tos.
	PSHA			....
	LDAA	CURREC+R$FILEBASE+2 Get file base address of Currec;
	LDAB	CURREC+R$FILEBASE+3	(double precision);
	JSR	SectorRem	  compute file sector offset.
	TSX		Compare the offsets;
	JSR	DP$CMP	  if they are equal, we're aligned.
	LEAS	2,S	  (pop stack)
	BEQ	AlignSDOS%	  B/ already aligned!

**** Need to do work to align. ****
AlignSDOS$1	EQU *
	CLRA		Set argument = FALSE,
	JSR	FinishCurrec	  and flush the record.
	JSR	GetCurrecBase	Get dummy value for FilePosition.
	ADDB	#8	Add in overhead for skip+rec header.
	ADCA	#0
	JSR	FilePosition	Compute file pointer for end of record.
	LDAA	2,X	  Load lsw into AB,
	LDAB	3,X
	PSHB		  and save on TOS.
	PSHA
	JSR	GetNewrecBase	AB := Newrec.BASE
	TSX
	JSR	DP$SUB	Compute the difference,
	JSR	SectorRem	  and take it modulo some power of 2.
			; (since it's a power of 2, we dont need
			;  to do this til now).
	STAA	0,X	Save result on TOS for a second,
	STAB	1,X
	LDAA	NotSectorMask	Check whether we can chuck the skip record.
	LDAB	NotSectorMask+1
	SUBB	#2	If so, this operation will yeild a zero.
	JSR	DP$CMP	  (Note, no carry possible from above SUB).
	PULA		Restore offset to AB,
	PULB
	BEQ	AlignSDOS%	  B/ No skip record needed.  offset=5.

**** Output a skip record, skipping (AB) bytes. ****
	PSHB		Construct a record on TOS,
	PSHA
	LDAA	#RTYPE$SKIP
	PSHA		  do this now, cause it's clearer.
	JSR	GetCurrecBase	Get CURREC.BASE
	BSR	FilePosition
	LDAB	#BO	Position to that byte, pleeze.
	JSR	POSITION$
	TSX		Get address of record,
	LDAA	#3	Get length of skip record,
	LDAB	#BO
	JSR	WRITEREC$

**** Now, update Filebase to point to end of skipped region ****
	LDAB	#3	Add three bytes to pointer for skip hdr,
	CLRA		  so load AB with 3.
	TSX		Get pointer to our copy of skip record,
	INX		  point to count of bytes to skip,
	JSR	DP$ADD	  and add it to 3 to get total to add to...
	BSR	UpdateFilebase	  the file pointer.
	LEAS	3,S	Prune stack of 3 bytes
AlignSDOS%	equ *
	RTS
	PAGE	- - - U p d a t e   F i l e b a s e - - -
*****************************************************************************
*
* Name:  UpdateFilebase (Offset: WORD)
*
* Function:
*	Adds offset (passed in AB) to CURREC+R$FILEBASE.
*
* Argument:
*	AB	"Offset"	Unsigned integer number to be added to
*			  file base pointer.
*
* Changed:
*	A, B, C.
*
* Preserved:
*	X
*
* Data:
*	CURREC+R$FILEBASE	Has (AB) added to it, in place.
*
*****************************************************************************

UpdateFilebase	EQU *
	ADDB	CURREC+R$FILEBASE+3
	STAB	CURREC+R$FILEBASE+3
	ADCA	CURREC+R$FILEBASE+2
	STAA	CURREC+R$FILEBASE+2
	BCC	UpdateFilebase%
	INC	CURREC+R$FILEBASE+1
	BNE	UpdateFilebase%
	INC	CURREC+R$FILEBASE
UpdateFilebase%	EQU *
	RTS
	PAGE	- - - F i n i s h   C u r r e c - - -
*****************************************************************************
*
* Name:  FinishCurrec(LAST)
*
* Function:
*	Finishes build process for current SDOS load record by writing its
*	header into the file.
*
* Arguments:
*	ACCA	"LAST"	Flag, designating whether this should be
*			  a LOAD record (if 0) or a LOADGO record
*			  (if non-zero).
*
* Changed:
*	A, B, C, X
*
* Input:
*	CURREC+R$VALID	Flag, indicating (if non-zero) that we have
*			  a record that we need to terminate; or
*			  (if zero) that we should/need do nothing
*			  in this routine.
*
*	CURREC+R$FILEBASE	File-pointer, (4 bytes), gives the address
*			  of the first data byte in the current
*			  record.  We assume that 5 bytes of space
*			  have been left in front of this for a
*			  record header.
*
*	CURREC+R$BASE	Target load address for first byte in this
*			  record.
*
*	CURREC+R$LEN	Number of bytes in this record.
*
	page
*
* Output:
*	CURREC+R$VALID	Set to zero, indicating that we have no
*			  current record.
*
*	CURREC+R$FILEBASE	Updated to point to the first free byte in
*			  the file.
*
*****************************************************************************

FinishCurrec	EQU *
	LDAB	CURREC+R$VALID
	BEQ	FinishCurrec%

**** Subtract 5 from Filebase to get to where header should go ****
	PSHA		(Save "Last Record Flag")
	LDX	#CURREC+R$FILEBASE
	CLRA		Load AB w/ 5...
	LDAB	#5	  ....,
	BSR	SubFilebase	  which we then subtract from CURREC.FILEBASE.

**** Position file to where header should go ****
	LDAB	#BO	Position the binary channel, silly!
	JSR	POSITION$

**** Write appropriate record type mark, followed by the rest of the header ****
	PULA		Restore "Last Record Flag" to ACCA.
	LDX	#CURREC+R$LEN+1
FinishCurrec1	equ *
	LDAB	0,X
	DEX
	PSHB
	CPX	#CURREC+R$BASE-1	Done yet?
	BNE	FinishCurrec1

	LDAB	#RTYPE$LOAD	Assume not last record, but just in case:
	TSTA		Check "last-record" flag
	BEQ	FinishCurrec2	  B/ Not last record.
	LDAB	#RTYPE$LOADGO	Oops!  Last record.  Set type & proceed.

FinishCurrec2	equ *
	PSHB		Save type byte,
	TSX		  get pointer to built record,
	LDAA	#5	  set count,
	LDAB	#BO	  set channel,
	JSR	WRITEREC$	  write a buncha bytes.

**** Make Currec point to next spare byte in file ****
	CLR	CURREC+R$VALID	Mark record as closed.
	CLRA		Restore Filebase,
	LDAB	#5
	BSR	UpdateFilebase	  by adding 5.

**** Clean up stack & exit ****
	LEAS	3,S	Prune 3 bytes from stack
	PULA		Now, read the length off the stack,
	PULB
	BSR	UpdateFilebase	  and add it to the filepointer.

FinishCurrec%	equ *
	RTS
	page	- - - F i l e   P o s i t i o n - - -
*****************************************************************************
*
* Name:  FilePosition (TargetAddress (*in AB*) )
*
* Function:
*	Given a target load address in AB, Fileposition returns a pointer
*	to a four byte buffer that contains a filepointer.  This filepointer
*	can be used to do a position prior to a write for outputting a
*	record that starts at TargetAddress.  The filepointer is built in
*	a special buffer; CURREC.FILEBASE is not modified.
*
* Arguments:
*	AB	"TargetAddress"	Target load address.
*
* Results:
*	X	"FPbuffer"	Address of four byte file address.
*
* Changed:		Preserved:
*	A, B, C, X		***NONE***
*
*****************************************************************************

**** Compute file address (& return pointer to buffer) for target address ****
**** in AB.  Address assumes inclusion in current record                  ****
FilePosition	equ *
	LDX	#FPbuffer	Load pointer to file-position buffer
	ADDB	CURREC+R$FILEBASE+3	FPbuffer := CURREC.FILEBASE +
	STAB	3,X			    NEWREC.BASE -
	ADCA	CURREC+R$FILEBASE+2		    CURREC.BASE
	STAA	2,X
	LDAA	CURREC+R$FILEBASE+1
	ADCA	#0
	STAA	1,X
	LDAA	CURREC+R$FILEBASE
	ADCA	#0
	STAA	0,X
	LDAA	CURREC+R$BASE
	LDAB	CURREC+R$BASE+1

**** FALL INTO SubFilebase *************
	page

*****>>>>>> FilePosition is falling in from previous page <<<<<<*****



*****************************************************************************
*
* Name:  SubFilebase (Offset (*in AB*), Pointer (*in X*))
*
* Function:
*	This routine subtracts a specified 16-bit unsigned integer from
*	a 32-bit integer in memory, updating the 32-bit integer in place.
*
* Arguments:
*	X	"Pointer"	Address of byte 0 of the integer to be
*			  modified.
*
*	AB	"Offset"	16-bit, unsigned positive value to be
*			  subtracted from the integer at X.
*
* Changed:		Preserved:
*	A, B, C			X
*
*****************************************************************************

**** Subtract 16 bit quantity in AB from 32-bit value @ (0..3,X)
SubFilebase	equ *
	COMA
	COMB
	SEC
	ADCB	3,X
	ADCA	2,X
	STAB	3,X
	STAA	2,X
	LDAA	#$FF
	ADCA	1,X
	STAA	1,X
	LDAA	#$FF
	ADCA	0,X
	STAA	0,X
	RTS
	page	- - - S e c t o r   N u m - - -
*****************************************************************************
*
* Name:  SectorNum (Address: WORD (*in AB*) )
*
* Function:
*	Given a value in AB that is a pointer into a disk file,
*	SectorREM returns in AB that portion of AB that selects the 
*	sector.  Note that this value is NOT right justified; it is merely
*	isolated in AB.  The other bits will just be set to zero.  The
*	following identity may clarify this:
*
*		(x) (x = SectorNum(x)+SectorREM(x))
*
*
* Changes:		Preserves:
*	A, B, C			X
*
*****************************************************************************

**** The bits in AB which correspond to byte-in-sector are CLEARED ****
SectorNum	equ *
	ANDA	SectorMask
	ANDB	SectorMask+1
	RTS

	page	- - - S e c t o r   R E M - - -
*****************************************************************************
*
* Name:  SectorREM (Address: WORD (*in AB*) )
*
* Function:
*	Given a value in AB that is a pointer into a disk file,
*	SectorREM returns in AB that portion of AB that selects the byte
*	within a sector.
*
* Changes:		Preserves:
*	A, B, C			X
*
*****************************************************************************

**** The bits in AB which correspond to sector-number are CLEARED ****
SectorREM	equ *
	ANDA	NotSectorMask
	ANDB	NotSectorMask+1
	RTS

	page	- - - D o u b l e   P r e c i s i o n   R o u t i n e s - - -
*****************************************************************************
*
* Name:  Double Precision Routines
*
* Function:
*	These routines provide OutputSDOSrecord and friends with some 16-bit
*	arithmetic operations.  One of the (many) nice features about this
*	package is that (where it makes sense) these routines return REAL
*	condition codes (as opposed to what you get out of a CPX, for example).
*
*	These routines all have a common calling sequence.  AB are treated as
*	a 16-bit accumulator, with A being the high-order byte and B being
*	the low-order byte.  X points to the first byte of the other operand
*	that is to be added, or whatever, to the accumulator.  If the operations
*	are done in the correct order, i.e. if B is operated upon before A,
*	and the routines close by branching to DP$CC, you will get the
*	appropriate condition code on exit.
*
*	The following routines are currently provided:
*
*	DP$ADD		Add (0,X) to AB.
*	DP$SUB		Subtract (0,X) from AB.
*	DP$CMP		Compare AB to (0,X).
*	DP$MAX		If AB << (0,X) then AB := (0,X).
*	DP$CC		If CCZ is set and B is not zero, CCZ is reset.
*
*****************************************************************************


**** AB := AB + (0,X) ****
DP$ADD	equ	*
	ADDB	1,X
	ADCA	0,X

DP$CC	equ	*	Fix condition codes for 16-bit operations
	BNE	DP$CC%	  The only bit that needs help is the Z bit.
	PSHA		  --and that only needs help if A=0.
	TPA
	TSTB
	BEQ	DP$CC1
	EORA	#(1##2)	Toggle the Z bit iff. low byte (B) was zero.
DP$CC1	TAP
	PULA
DP$CC%	RTS

**** AB := AB - (0,X) ****
DP$SUB	equ	*
	SUBB	1,X
	SBCA	0,X
	BRA	DP$CC

**** Compare AB to (0,X); return appropriate condition code ****
DP$CMP	equ	*
	PSHB
	PSHA
	BSR	DP$SUB
	PULA
	PULB
	RTS

**** AB := AB max (0,X) ****
DP$MAX	equ	*
	BSR	DP$CMP
	BCC	DP$MAX%
	LDAA	0,X
	LDAB	1,X
DP$MAX%	RTS
	page	- - - G e t   R e c   E n d - - -
*****************************************************************************
*
* Name:  GetNewrecEnd ()
*
* Function:
*	Computes target load address of first byte after segment currently
*	being processed.  Definition is:
*
*		LET GetNewrecEnd() = GetRecEnd(#NEWREC)
*
* Changes:		Preserves:
*	A B C X			****NONE****
*
*****************************************************************************

**** AB := Target address of last byte in NEWREC ****
GetNewrecEnd	equ *
	LDX	#NEWREC	Get address of structure base,
	BRA	GetRecEnd	  go compute address.

*****************************************************************************
*
* Name:  GetCurrecEnd ()
*
* Function:
*	Computes (in AB) the target load address of the first byte
*	following the current record.  This function is merely:
*
*		LET GetCurrecEnd() = GetRecEnd(#CURREC)
*
* Changes:		Preserves:
*	A B C X			****NONE****
*
*****************************************************************************

**** AB := Target address of last byte in CURREC ****
GetCurrecEnd	equ *
	LDX	#CURREC	Get address of structure base.

*****************************************************************************
*
* Name:  GetRecEnd (Pointer: ^{either NEWREC or CURREC})
*
* Function:
*	Returns in AB the target load address of the first byte after
*	the specified record/record segment.
*
* Changes:		Preserves:
*	A, B, C			X
*
*****************************************************************************

**** AB := Target address of last byte in record at (0,X) ****
GetRecEnd	equ *
	LDAA	R$BASE,X
	LDAB	R$BASE+1,X
	ADDB	R$LEN+1,X
	ADCA	R$LEN,X
	RTS
	page	- - - G e t   R e c o r d   B a s e - - -
*****************************************************************************
*
* Name:  GetNewrecBase ()
*
* Function:
*	Loads AB with the contents of NEWREC+R$BASE; i.e., with the target
*	load address of the record segment being processed.
*
* Changes:		Preserves:
*	A, B, C			X
*
*****************************************************************************

GetNewrecBase	equ *
	LDAA	NEWREC+R$BASE
	LDAB	NEWREC+R$BASE+1
	RTS

*****************************************************************************
*
* Name:  GetCurrecBase ()
*
* Function:
*	Loads AB with the contents of CURREC+R$BASE, i.e., with the target
*	load address of the record currently being built.
*
* Changes:		Preserves:
*	A, B, C			X
*
*****************************************************************************

**** AB := Currec.BASE ****
GetCurrecBase	equ *
	LDAA	CURREC+R$BASE
	LDAB	CURREC+R$BASE+1
	RTS
         PAGE     - - - G E T C S - - -
*
*
*  G E T C S
*
* GET A CHAR FROM SI DEVICE IN A. IF EOF IS HIT, EOFF#0.
*
*
GETCS	EQU	*
	LDAA	RJCH	A CHARACTER WAITING TO BE GIVEN TO THE TURKEY?
	BNE	GETNLF	  B/ YUP, GIVE IT HIM.

GETCS1	EQU	*	LOOP FOR ACTUAL I/O.
	LDAB	#SI
	JSR	EOF$	SEE IF EOF HIT
	TAB
	LDAA	#$D	DEFAULT IF EOF
	STAB	EOFF	SAVE FLAG
	BNE	GETNLF	B/EOF
	LDAB	#SI	READ FROM SI
	JSR	RASCII$	READ
	TSTA		A NULL?
	BEQ	GETCS1	WE IGNORE THOSE
	CMPA	#$A	A LF?
	BNE	GETNLF	IF NOT, OK
	LDAB	OPTF	SEE IF WE IGNORE THOSE
	BITB	#OPT:LF	WELL?
	BNE	GETCS1	IF ON, IGNORE

GETNLF	EQU	*
	CLR	RJCH	NO CHARACTER REJECTED.
	RTS
*
* REWSI - REWIND SI DEVICE TO BEGINNING
*
REWSI	LDAB	#SI	LOAD CHANNEL #

REWANY	equ	*	Rewind any channel (# in ACCB).
	LDX	#ZEROES	PT TO SOME ZEROES
	JMP	POSITION$	REWIND AND RETURN
*
ZEROES	FDB	0,0
         PAGE     - - - P U T C C - - -
*
*
*   P U T C C
*
*        Output ch in A to C device, with minimal editing.
*	ALL CH <$20 --> UP-ARROW FORM
*
PUTCC    EQU      *
         PSHA
         CMPA     #$D
         BEQ      PUTC3
         CMPA     #$20
         BGE      PUTC3
         PSHA
         LDAA     #'^
	BSR	PUTC
         PULA
         ADDA     #$40
*
PUTC3	BSR	PUTC
         PULA
         RTS
*
*
* PUTC : PUT A CHAR IN A TO CONS DEVICE - SAVE B.
*
PUTC	PSHB		SAVE B
	LDAB	#CONS	GET CORR CHANNEL #
WRTOUT	STX	IOXSAVE	SAVE X REG
	JSR	WASCII$	WRITE
	BRA	IORET
*
* GETC - GET A CHAR FROM CONS DEV. SAVE B.
*
GETC	PSHB		SAVE B
	STX	IOXSAVE	SAVE X REG
GETC2	LDAB	#CONS	GET CH #
	JSR	RASCII$	READ
	TSTA		IS IT A NUL?
	BEQ	GETC2	IF SO, IGNORE
IORET	PULB		GET B BACK
	LDX	IOXSAVE	GET X BACK
	RTS
         PAGE     - - - P U T T - - -
*
*
*  P U T T
*
*        Puts TEXTZ string following call to control device.
*
PUTT     EQU      *
	PULX
         LDAA     0,X               NULL STRING IS NOT OK.
PUTT1    BSR      PUTCC
         INX
         LDAA     0,X               GET NEXT
         BNE      PUTT1
         JMP      1,X
	PAGE	- - - G E T L - - -
*
* GET A LINE FROM THE CONS DEVICE INTO THE BUFFER SPECIFIED
* BY THE X REGISTER.
*
*	RETURNS B=#OF CHARS
*
GETL	CLRB		CLEAR THE COUNT
NCHR	BSR	GETC	GET THE CHAR
	STAA	0,X	SAVE
	BEQ	NCHR	DONT SAVE NULLS, STUPID!
	INX		PT TO NEXT
	INCB		INC CHAR COUNT
	CMPA	#$D	DONE YET?
	BNE	NCHR	IF NOT, GET MORE
	CLR	0,X	MARK EOB
	RTS
         PAGE     - - - G E T O P T - - -
*
*
*  G E T O P T
*
*	GET THE SI,LO,AND BO ASSIGNMENTS FROM USER.
* OPEN THE APROPRIATE FILES AND START ASSEMBLY WITH
* INPUT FROM THE CONS DEVICE UNTIL AN EMPTY LINE IS GIVEN
*
*
GETOPT   EQU      *

**** Output a greeting (?) ****
	LDX	ASMPATCH	Setup assembly
	STX	ASMPAT	 and patch
         JSR      PUTT
         FCB      $D
	IFUND	MC6809
	FCC	'Software Dynamics ASM/6800, Version '
	ELSE
	FCC	'Software Dynamics ASM/6809, Version '
	FIN
	FCB	(RELEASE/$10&$F)+'0,'.,(RELEASE&$F)+'0
	FCB	ASSEMBLY	Show assembly
ASMPAT	FCC	'  '
	FCC	' / '
	FCB	(VERS/$1000&$F)+'0+('A-'9-1)*((VERS/$1000&$F)>9)
	FCB	(VERS/$100&$F)+'0+('A-'9-1)*((VERS/$100&$F)>9)
	FCB	(VERS/$10&$F)+'0+('A-'9-1)*((VERS/$10&$F)>9)
	FCB	(VERS&$F)+'0+('A-'9-1)*((VERS&$F)>9)
         FCB      $D
         FCB      0
	STS	FLAMOUT	SAVE STACK FOR I/O ERROR POSSIBILITY

**** Get the source-file's name from the user ****
RSI	LDX	#RSI	GET LOC FOR I/O ERR
	STX	IOERRLOC
	JSR	PUTT	SAY SOMETHING
	FCC	/Source File=/
	FCB	0
	JSR	GETNAME	GO GET A NAME
	BEQ	RSI	IF NULL - ASK AGAIN
	LDAB	#SI	LOAD CH#
	PSHA		<<SAVE SIZE>>
	JSR	OPEN$	OPEN THE SI CHANNEL

**** MAKE AN INCLUDE BLOCK FOR THE ORIGINAL SOURCE FILE ****
	PULB		GET LENGTH OF NAME;
	LDX	INBUF	  GET BASE OF NAME;
	STX	TPTR
	JSR	MAKEINCLB	  MAKE ME AN INCLB.
	STX	.CURINCLB	SAVE POINTER,
	CLR	INCLVL	  RESET INCLUDE LEVEL TO 0.

**** Get the listing-file's name from the user ****
RLO	LDX	#RLO	ERR RET
	STX	IOERRLOC
	JSR	PUTT
	FCC	/Listing File=/
	FCB	0
	BSR	GETNAME	GET THE NAME
	BNE	WANTLO	GO HANDLE IN NAME NOT NULL
	LDAA	#\OPT:LO
	BSR	OPTAND	INDICATE NO BO
	BRA	RBO

**** Open the listing file for the user (if possible) ****
WANTLO	LDAB	#LO	CH #
	JSR	CREATE$	CREATE THE LO FILE
	LDAA	#OPT:LO
	BSR	OPTOR

**** Get the binary-file's name from the user ****
RBO	LDX	#RBO	GET ERR RETURN ADDR
	STX	IOERRLOC
	JSR	PUTT
	FCC	/Binary File=/
	FCB	0
	BSR	GETNAME	GET A NAME
	BNE	WANTBO	IF NOT, NULL HANDLE
	LDAA	#\OPT:BO
	BSR	OPTAND	INDICATE NO BO
	BRA	OPTEND	FINISH UP

**** Create the binary output file; then check that it's a file ****
WANTBO	LDAB	#BO	GET CHANNEL #
	JSR	CREATE$	CREATE FILE

	LDAB	#BO	This will be enforced by GTSIZE$.
	JSR	GTSIZE$	Give me NBPS in AB.
	SUBB	#1	Convert it to a mask;
	SBCA	#0		....
	STAA	NotSectorMask	  this mask will extract the byte-within-...
	STAB	NotSectorMask+1	  ...-sector field from a file pointer.
	COMA		Make mask to extract the sector-number
	COMB		  field from a file pointer;
	STAA	SectorMask	Put it in appropriately named slot.
	STAB	SectorMask+1		....

	LDAA	#OPT:BO
	BSR	OPTOR	PUT IN OPTION WORD

**** Done processing options; get out ****
OPTEND	LDX	#CRAPOUT$	ERR RET IF I/O ERROR DURING ASM
	STX	IOERRLOC
	RTS
*
*
* GETNAME - GO GET A NAME FROM CONS.
*	RETURN SIZE IN A AND B AND PTR TO FIRST BYTE IN X.
*
GETNAME	LDX	INBUF	TELL SYSTEM WHICH BUFER
	JSR	GETL	GET THE LINE
	LDX	INBUF	LOAD UP PTR TO BUF
	DECB		GET SIZE W/O CR
	TBA		SIZE IN A
	RTS
	PAGE	- - - O P T O R / O P T A N D - - -
*
* SET OR CLEAR THE INDICATED BIT IN THE OPTF BYTE
*
OPTOR	ORAA	OPTF	OR IN THE BIT
	BRA	OPTAND1		AND STORE BACK

OPTAND	ANDA	OPTF	AND OUT THE BIT
OPTAND1	STAA	OPTF	STORE BACK
	RTS


*
*  SET OR CLEAR INDICATED BIT IN OPT1F BYTE.
*

OPT1OR	ORAA	OPT1F	MERGE IN THE BIT
	BRA	OPT1AND1		  AND STORE BACK.

OPT1AND	ANDA	OPT1F	MASK OUT THE BIT
OPT1AND1	STAA	OPT1F	  AND SHOVE BACK.
	RTS		RETURN.
	PAGE	- - - E X I T - - -
*
*
* CLOSE ANY OPEN FILES AND EXIT
*
EXIT	LDAB	#SI	ALWAYS CLOSE SI
	JSR	CLOSE$
	LDAA	OPTF
	BITA	#OPT:BO	DO WE CLOSE BO?
	BEQ	NCLBO	IF NO, SKIP
	LDAB	#BO
	JSR	CLOSE$
NCLBO	LDAA	OPTF
	BITA	#OPT:LO	CLOSE LO?
	BEQ	NCLLO
	LDAB	#LO
	JSR	CLOSE$
NCLLO	LDAB	ERRC	ANY ERRORS?
	BEQ	AXEIT	B/NO
	JSR	ISCONSOLE$
	BCC	AXEIT	B/IS CONSOLE
	LDX	#100
	JSR	CRAPOUT$

AXEIT	JSR	EXIT$
	PAGE	- - - I / O   E R R O R   H A N D L I N G - - -
*
* PRINT IO ERROR # XX <CR> AND THEN GO TO ADDR IN IOERRLOC.
*
IOERROR	JSR	ERROR$	OUTPUT THE ERROR PLEEZE.
	JSR	PUTT
	FCB	$D,0
	LDX	IOERRLOC	GET RET ADDR
	LDS	FLAMOUT	REWIND STACK
	JMP	0,X	GO,GO,GO
*
	PAGE	- - - I N L I N E - - -
*
* INLINE - INPUT A LINE FROM THE APROPRIATE PLACE.
*
*	IF PASS1 - INITIALY GET LINES FROM CONS
* AND SAVE THEM FOR PASS 2 IN SYM TABLE AREA. AS SOON AS
* EMPTY LINE IS INPUT - READ FROM SI DEVICE
*
*	IF PASS 2 - INITIALLY GET STORED LINES FROM PASS 1.
* THEN, WHEN WE RUN OUT OF USER INPUT LINES, READ LINES
* FROM THE SI DEVICE.
*
*
*
*
*	FORMAT OF SAVED LINE BLOCKS:
*
*		------------------
*		*    CHAR        *
*		*    CHAR        *
*                  . . . . . . 
*		*     <CR>       *
*		*     <NUL>      *
*		*   PTR TO NEXT  *
*		* LINE BLOCK     *
*		------------------
*
*
INLINE	LDX	INBUF	PT TO START OF BUFFER
	STX	INPTR	SAVE AWAY
	LDAA	INCLVL	ARE WE AT BOTTOM LEVEL?
	BNE	STDLINE	  B/ NO: ALWAYS READ FROM FILE.
	LDAA	INCNT	DO WE READ FROM CONS?
	BNE	STDLINE	IF NOT, STANDARD LINE
	LDAA	PASS	WHICH PASS
	BNE	P2INPUT	IF PASS 2 DO WEIRD STUFF
	STX	TPTR1	SAVE SOURCE ADDR FOR MOVE
	LDAA	#'>
	JSR	PUTCC	PROMPT THE GUY SO THAT HE DOESN'T FALL ASLEEP
	JSR	GETL	GET A LINE
	DECB
	BEQ	P2INDONE	IF NULL LINE, WE ARE DONE
*
* GOT LINE - NOW GET A BLOCK FROM SYM TBL AND SAVE IT THERE
*
	ADDB	#4	ADD TO COUNT OR <CR><NUL>+2 PTR BYTES
	JSR	GTBLK	GET SOME SPACE
	BEQ	P2INDONE	IF NO SPACE, ACT LIKE NORMAL
	STX	TPTR	SAVE ADDR OF BLOCK
	LDX	INLLINK	LD ADDR OF LAST LINK
	LDAA	TPTR	LINK THIS BLOCK IN
	STAA	0,X
	LDAA	TPTR+1
	STAA	1,X
	BSR	MOVEB	TPTR1 --> TPTR
	INX		POINT PAST YE NULL,
	STX	INLLINK	SAVE PTR TO LAST LINK
	CLR	0,X	INDICATE END OF LIST
	CLR	1,X
OUTLINE	RTS		ALL DONE (PHEW)
*
* MUST GET LINES FROM SAVED LIST
*
P2INPUT	STX	TPTR	SAVE AS DESTINATON ADDRESS
	LDX	INLLINK	LD PTR TO LINK TO NEXT BYTE
	LDX	0,X	LD ADDR OF NEXT BLK
	BEQ	P2INDONE	IF=0 END OF FUNNY INPUT - READ FROM SI NOW
	STX	TPTR1	SAVE AS SOURCE ADDR
	BSR	MOVEB	FILL THE INPUT BUFFER
	LDX	TPTR1	LD PTR TO NEXT LINK
	STX	INLLINK	SAVE IN SAFE PLACE
	RTS
*
* HANDLE REMAINING LINES AS NORMAL (I.E. FROM SI).
*
P2INDONE	DEC	INCNT	MAKE INCNT # 0
	JSR	FORMFEED	  EJECT A PAGE.
STDLINE	EQU	*
	LDAA	EOFF	END OF FILE?
	BNE	OUTLINE	B/YES
STDLINE1	JSR	GETCS	GET A SOURCE CHAR,
	CMPA	#CTRLL	IS IT A CONTROL-L?
	BNE	READL3	  B/ NOPE: START SAVING LINE.
	JSR	FORMFEED	THROW FORM,
	BRA	STDLINE	  AND EAT ANY MORE ^L-S.
READL1   JSR      GETCS             ;GET A SOURCE CHARACTER
READL3   EQU      *
*2*
*
* GOT A CH IN A; PUT IT IN THE BUFFER.
         LDX      INPTR             ;POKE THIS CHARACTER AWAY,
         STAA     0,X               ;IN THE BUFFER
         CPX      INBUFE            HIT END OF LINE BUFFER?
         BEQ      READ99               B/ YES:  DON'T BUMP POINTER.
         INX                        ;AUTOINCREMENT
READ98	STX      INPTR             ;SAVE POINTER.
*
* GOT ALL THE LINE?
         CMPA     #$D               ;CARRIAGE RETURN?
         BNE      READL1            ;B/ NO, GO GET ANOTHER CH.
         CLR      0,X               ;CR SEEN: APPEND A NULL.
	RTS

****  ARRGH.... OUT OF INPUT BUFFER..... ******
READ99	JSR	ILTRUNC	SET ERROR FLAG,
	LDX	INPTR	  RESTORE INPUT POINTER,
	BRA	READ98	  AND CONTINUE UNTIL WE FIND A $D.
	PAGE	- - - M O V E B - - -
*
* MOVEB
*
* MOVE STRING PTD TO BY TPTR1 TO STRING PTD TO BY TPTR.
*	STRING ENDS WHEN NULL SEEN.
*
* MOVEB1
*
* MOVE STRING POINTED TO BY (X) TO STRING POINTED TO BY TPTR.
*
MOVEB0	INX		BUMP DESTINATION POINTER,
	STX	TPTR	  AND SAVE.
*
MOVEB	LDX	TPTR1	LOAD PTR
MOVEB1	LDAA	0,X	LD CHAR
	INX
	STX	TPTR1
	LDX	TPTR
	STAA	0,X	SAVE CHAR
	TSTA		A NULL?
	BNE	MOVEB0	KEEP ON MOVING
	RTS
	IF	DUMPOK=0
DUMPTBL	JSR	EXIT
	ELSE
	TITLE	- - - S Y M B O L    T A B L E    D U M P - - -
	PAGE	- - - D U M P - - -
*
*
*	PAGE ZERO CELLS FOR DUMP
*
*
*	RTNPTR		PTR: SORT ROUTINE
*	SCHAIN		PTR: HEAD OF SORTED SYMBOL LIST
*
DUMPTBL	EQU	*
	LDAA	OPTF	SEE IF WE HAVE TO DUMP TABLE
	COMA
	BITA	#OPT:LO!OPT:DMP	ARE BOTH ON?
	BNE	DUMPEX	IF NOT, DON'T BOTHER
	JSR	DUMPNAME	DUMP LIST SORTED BY NAME
	IF	DUMPV=1	ONLY IF WE ARE DUMPING BY VALUE
	JSR	DUMPVAL	DUMP LIST SORTED BY VALUE
	FIN
	JSR	CRLF	FINISH PRETTY PRINT
	LDX	VAL	ANY SYMBOLS DUMPED ?
	BNE	DUMPTBL1	YES, GO PRINT CORRECT MESSAGE
	LDX	#NOSYM	GET "NO SYMBOLS...." MESSAGE
	JSR	PTTL	AND PRINT IT
DUMPTBL0	EQU	*
	LDX	#WEREDEF	GET "...WERE DEFINED..."
DUMPTBL2	JSR	PTTL
	JSR	CRLF
DUMPEX	RTS
DUMPTBL1	EQU	*
	DEX		ONLY ONE SYMBOL ?
	BNE	DUMPTBL3
	LDX	#ONESYM	YES, GET PROPER MESSAGE
	BRA	DUMPTBL2
DUMPTBL3	EQU	*
	LDAA	VAL	ELSE GET VALUE TO (A,B)
	LDAB	VAL+1
	JSR	PTNL	PRINT OUT NUMBER OF SYMBOLS
	BRA	DUMPTBL0	GO PRINT REST OF MESSAGE
NOSYM	FCC	/No/
	FCB	0
ONESYM	FCC	/One Symbol./
	FCB	0
WEREDEF	FCC	/ Symbols./
	FCB	0
	PAGE
*
*
*	DUMP ROUTINES:
*
*	DUMPNAME -- PRODUCES LIST SORTED BY NAME
*
*	DUMPVAL -- PRODUCES LIST SORTED BY VALUE
*
*
SYMTABS	FCB	16,32,48,64,80,96,112,0
DUMPHDR	FCC	/Symbols Sorted by /
	IF	DUMPV=0
	FCC	/Name/
	FCB	0
	ELSE
	FCB	0
DNHDR	FCC	/Name/
	FCB	0
DVHDR	FCC	/Value/
	FCB	0
	FIN
DUMPTRLR	FCB	':,$D,0
*
* GO SORT AND THEN PRINT SORTED LIST
*
DUMPNAME	EQU	*
	IF	DUMPV=1
	LDX	#SORTNAME	GET ADDRESS OF SORT ROUTINE
	STX	RTNPTR	AND SAVE IT
	LDX	#DNHDR	THIS IS REALLY KLUDGEY
	BRA	DUMP1
DUMPVAL	EQU	*
	LDX	#SORTVAL	GET ADDRESS OF SORT ROUTINE
	STX	RTNPTR
	LDX	#DVHDR
DUMP1	STX	MLA	SAVE POINTER TO NAME/VALUE WORD
	FIN
	JSR	EJECT	NEW PAGE, PLEASE...
	LDX	#SBTTBF	LOAD SUBTTL BUFFER ADDRESS.
	STX	TPTR	  USE AS DESTINATION FOR MOVEB.
	LDX	#DUMPHDR	SPIT "SORTED BY MESSAGE"
	JSR	MOVEB1
	IF	DUMPV=1
	LDX	MLA	GET NAME/VALUE WORD POINTER
	JSR	MOVEB1	AND GET RID OF IT ON THE DUMMY, TOO...
	FIN
	LDX	#DUMPHDR	RE-ISSUE MESSAGE FOR READABILITY.
	JSR	PTTL	  (I.E., MAKE IT PART OF THE LISTING)
	IF	DUMPV=1
	LDX	MLA	GIVE 'EM THE TYPE....
	JSR	PTTL
	FIN
	LDX	#DUMPTRLR	NOW GIVE 'EM A :, $D.
	JSR	PTTL	  ......
	JSR	SORT	MAKE SOME SENSE OF THE HASH
	LDX	#SYMTABS	AND SUBSTITUTE OUR TABS
	STX	TABTBL
*
*	NOW LOOP THRU THE SORTED LIST AND PRINT
*
	LDAA	#$7F	INITZ COLUMN COUNT
	STAA	COLNO
DUMP2	LDX	SCHAIN	GET HEAD OF UNPRINTED CHAIN
	STX	SYMPT	SAVE IN TEMP
	BEQ	DUMPX	IF DONE, BR OUT
	LDX	0,X	GET LINK TO NEXT SYM
	STX	SCHAIN	SAVE FOR NEXT TIME
	JSR	DUMPRINT	PRINT SYM IN SYMPT
	JSR	REHASH	REHASH SYM IN SYMPT
	BRA	DUMP2	GO DO IT AGAIN
*
DUMPX	JMP	CRLF	OUTPUT EXTRACRLF TO END THIS LINE
*			AND THEN RETURN TO CALLER
	PAGE	- - - S O R T - - -
*
*	SORT:  BETCHA CAN'T GUESS WHAT THIS MNEMONIC MEANS...
*
*	(TERRY DIDN'T TELL ME OR I WOULD HAVE BLINDLY COPIED IT!)
*
* GO SORT INDIVIDUAL SUBLISTS THEN SORT THE WHOLE THING
*
SORT	EQU	*
	JSR	TBLSORT	SORT ALL THE SUBLISTS
*
*	NOW MERGE THE CHAINS
*
	LDX	#0	INITZ DEFINED SYMBOL COUNT TO ZERO
	STX	VAL
	STX	SCHAIN	SET SORTED LIST TO EMPTY
	LDX	#SCHAIN	SET END OF LIST POINTER TO A(HEAD)
	STX	SORTAIL
SORT0	LDX	#HSHTBL	NOW SCAN THE HASH TABLE
	STX	BUCKET	REMEMBER WHICH BUCKET WE ARE FOOLING WITH
* X=BEST BUCKET FOUND SO FAR
SORT1	STX	LAST	THIS IS LAST SYMBOL WE LOOKED AT
	LDX	S:NEXT,X	FIND NEXT SYMBOL IN BUCKET CHAIN
	STX	THIS
*
* LAST - PTR TO BEST BUCKET FOUND
* THIS - PTR TO FIRST SYM IN BEST BUCKET FOUND
*
SORT2	LDX	BUCKET	SCAN OTHER BUCKET HEADS...
	LEAX	2,X
	STX	BUCKET
	CPX	#HSHEND
	BEQ	SORT3	THIS IS BEST SYMBOL, ADD TO SORTCHAIN
	LDX	S:NEXT,X	COMPARE BUCKET HEAD WITH THIS SYMBOL
	STX	NEW
*
* NEW - PTR TO SYM NOW BEING COMPARED WITH SYM POINTED
* TO BY THIS
*
	IF	DUMPV=1
	LDX	RTNPTR	USE THE SPECIFIED COMPARISON SUBROUTINE
	JSR	0,X	SORT BY NAME OR BY VALUE
	ELSE
	JSR	SORTNAME
	FIN
	BVC	SORT2	B/ THIS <= NEW
	LDX	BUCKET	ELSE NEW < THIS, USE NEW INSTEAD
	BRA	SORT1	REMEMBER NEW AS BEST
*
* FOUND THE BEST SYMBOL - ADD TO CHAIN
*
SORT3	EQU	*	THIS --> NEXT TO CHAIN
	LDX	THIS	EXHAUST LISTS FINALLY ?
	BNE	SORT4	NO
	RTS		YES, GET OUT!

SORT4	LDD	S:NEXT,X	FIND NEXT(THIS)
	CLR	0,X	MAKE IT END OF SORT CHAIN
	CLR	1,X
	STD	[LAST]	DELETE SYMBOL FROM HEAD OF BUCKET
	LDX	SORTAIL	HANG THIS SYMBOL ON TAIL OF SORTCHAIN
	LDD	THIS
	STD	S:NEXT,X
	STD	SORTAIL	MAKE TAIL POINT TO THIS SYMBOL
	LDX	#VAL	GET POINTER TO BCD BUFFER
	JSR	INCBCD	GO BUMP (SORTED) SYMBOL COUNT
	BRA	SORT0
	PAGE	- - - T B L S O R T - - -
*	TBLSORT:
*	SORTS EACH OF THE SUBLISTS IN THE HASH TABLE, USING
*	THE COMPARISON ROUTINE PASSED IN RTNPTR.  ASSUMES NOTHING.
*	OUTPUTS IN VAL A 2-BYTE COUNT OF THE # OF DEFINED
*	SYMBOLS (IN DECIMAL)
*
* PICK UP ONE BUCKET AT A TIME AND GIVE IT TO SORTCHAIN
*
*
*
TBLSORT	EQU	*
	LDX	#HSHTBL	SCAN THE HASH TABLE
TBLSORTL	EQU	*
	STX	BUCKET	PT TO NEXT BUCKET PTR
*
*	SORT BUCKET CHAIN
*
	LDX	0,X	GET HEAD OF CHAIN
	STX	NEW	SAVE IN NEW
	LDX	BUCKET	MARK OUTPUT CHAIN AS NULL
	CLR	0,X	DELINK CHAIN
	CLR	1,X
	BSR	SORTCHAIN	THIS GUY DOES ALL THE HARD WORK!
	LDX	BUCKET	DONE WITH THIS BUCKET,...
	LEAX	2,X	DO THE NEXT BUCKET
	CPX	#HSHEND	ALL BUCKETS PROCESSED ?
	BNE	TBLSORTL	B/ NO, SORT ANOTHER
SORTRTS	RTS
*
*	SORTCHAIN:
*	SORTS A BUCKET CHAIN INTO ORDER
*
*
SORTCHAIN EQU	*
	LDX	NEW	WHILE NEW <> 0 DO:
	BEQ	SORTRTS	B/ ALL DONE
	LDX	BUCKET		LAST:= BUCKET;
SORTCHAIN0	EQU	*
	STX	LAST
	LDX	S:NEXT,X	THIS:=NEXT(LAST);
	STX	THIS
	BEQ	SORTCHAIN1	B/ GO INSERT
	IF	DUMPV=1
	LDX	RTNPTR		IF THIS=0 OR GT(THIS,NEW) THEN BREAK
	JSR	0,X		END;
	ELSE
	JSR	SORTNAME
	FIN
	BVS	SORTCHAIN1	B/ GO INSERT
	LDX	THIS
	BRA	SORTCHAIN0
SORTCHAIN1 EQU	*
	LDD	NEW	NEXT(LAST):=NEW
	LDX	LAST
	STD	S:NEXT,X
	LDX	NEW	NEW:=NEXT(NEW)
	LDX	S:NEXT,X
	STX	NEW
	LDX	LAST	NEXT(NEXT(LAST)):=THIS
	LDX	S:NEXT,X
	LDD	THIS
	STD	S:NEXT,X
	BRA	SORTCHAIN
	PAGE	- - - S P S T - - -
*
*
* SPTST - CHECK IF EITHER OF THE SYMBOLS GIVEN TO THE SORT ROUTINES
*	ARE NULL POINTERS. (I.E. NO SYMBOLS AT ALL)
*
* IF SO, THE V CC IS SET AS FOLLOWS:
*
*	NEW=0  --> V=0, regardless of state of THIS.
*	NEW<>0, THIS=0 --> V=1
*
SPTST	LDX	NEW	GET 1ST PTR
	BEQ	NNEW	INDICATE IT IS NULL
	LDX	THIS	LOAD 2ND PTR
	BNE	SPOK	IF NOT NULL EITHER, JUST RETURN(X=THIS)
	LEAS	2,S	ELSE, POP RETURN ADDR
SEV	SEV		SET V=1
SPOK	RTS		RETURN
*
NNEW	LEAS	2,S	POP RETURN ADDR
CLV	CLV		SET V=0
	RTS		RETURN
*
	PAGE	- - - S O R T   C O M P A R I S O N   R O U T I N E S - - -
*
*
*
*	1) ALWAYS RETURNS V SET IF "THIS" SHOULD FOLLOW "NEW"
*	2) ALWAYS DESTROY (A,B)
*
	IF	DUMPV=1
*	SORTVAL -- SORTS ON VALUE FIELD.  BY 16 BIT MAGNITUDE.
	FIN
*
*	SORTNAME -- SORTS ON NAME FIELD; ASCII COLLATING SEQUENCE.
*
*
	IF	DUMPV=1
SORTVAL	EQU	*
	BSR	SPTST	GO SEE IF ANY PTRS ARE NULL
	LDD	S:VAL,X	GET VALUE OF THIS NODE
	LDX	NEW	GET ADDRESS OF OTHER NODE
	CMPA	S:VAL,X
	BHI	SEV	B/ HI ==> THIS > NEXT, INSERT HERE
	BNE	CLV	B/ LOW ==> THIS < NEXT
	CMPB	S:VAL+1,X	B/ H(THIS)=H(NEXT), COMPARE L(THIS):L(NEXT)
	BHI	SEV	B/ HI ==> THIS >= NEXT, INSERT HERE
	BNE	CLV	IF EQUAL DROP THROUGH TO NAME SORT
	FIN
*
*
*
*
SORTNAME	EQU	*
	BSR	SPTST	SEE IF EITHER ARG IS NULL
	STX	TPTR	SAVE SYMBOL NAME SCAN POINTER
	LDAB	S:LEN,X	AND LENGTH OF FIRST SYMBOL
	LDX	NEW	NOW FOR THE OTHER SYMBOL
	STX	TPTR1	SAVE POINTER TO IT, TOO...
	CMPB	S:LEN,X	FIGURE OUT WHICH IS LONGER
	TPA		NEWT'S EYE AND HAIR OF BAT...
	PSHA		THAR BE FUNNY STUFF HERE, MATEY!
	BLS	SORTNAM1	IF YOU UNDERSTAND THIS GIZMO...
	LDAB	S:LEN,X	PLEASE TELL US WHAT IT DOES!
*
*		A RUMOR INDICATES GET MIN(LEN(THIS),LEN(NEW))
*	GOD HELP YOU IF ANY ZERO LENGTH SYMBOLS ARE MIXED UP IN THIS
*
* B=MIN(LEN(THIS),LEN(NEW))
*
*
SORTNAM1	EQU	*
	LDX	TPTR	GET POINTER TO THIS SYMBOL NAME
	LDAA	S:TEXT,X	GET NEXT CH OF SYMBOL NAME
	INX
	STX	TPTR	BUMP AND UPDATE SYMBOL SCAN POINTER
	LDX	TPTR1	NOW GET A BYTE FROM OTHER SYMBOL
	CMPA	S:TEXT,X	DECIDE WHICH IS LEXICOGRAPHICALLY SMALLER
	BEQ	SORTNAM2	THERE IS ALWAYS THIS POSSIBILITY
	INS		WHAT'S IN THE STACK ?
	BHI	SEV	ONE IS BIGGER!
	BRA	CLV	OR IT IS SMALLER...
SORTNAM2	EQU	*	RATS, BOTH CHARACTERS WERE THE SAME!
	INX		BUMP POINTER TO 2ND SYMBOL NAME
	STX	TPTR1
	DECB		DOWN COUNT THE LENGTHS
	BNE	SORTNAM1	B/MORE BYTES TO COMPARE
*
*	ALL COMPARED TEXT IS EQUAL, CHOOSE SHORTER SYM.
*
*
*
*
	PULA		TERRY CAN GET HERE!
	TAP		MAYBE AT 3AM IN THE MORNIGN ?
	BCC	SEV	DO SOMETHING IRRATIONAL
	CLV
	RTS		JUST AS BIG AS THE "BRA CLV"
	PAGE	- - - R E H A S H - - - -
*	REHASH
*	RETURNS A SYMBOL TO ITS HASH CHAIN.
*	SYMPT POINTS TO A SYMBOL.
*	USES SPECIAL ENTRY TO HASH.
*
REHASH	EQU	*
	LDX	SYMPT	GET SYMBOL ADDRESS
	JSR	HASH:PTR	X:=HASH BUCKET ADDRESS
REHASH0	EQU	*
	TST	S:NEXT,X	FIND END OF CHAIN
	BNE	REHASH1
	TST	S:NEXT+1,X
	BEQ	REHASH2
REHASH1	EQU	*
	LDX	S:NEXT,X	FOLLOW LINKS TO END OF CHAIN
	BRA	REHASH0
*
REHASH2	EQU	*
	LDD	SYMPT	TACK THIS SYMBOL ON TO END OF HASH CHAIN
	STD	S:NEXT,X
	LDX	SYMPT	MARK THIS SYMBOL AS END OF CHAIN
	CLR	S:NEXT,X
	CLR	S:NEXT+1,X
	RTS
	PAGE	- - - D U M P R I N T - - -
*
*	DUMPRINT
*	SYMPT IS ADDRESS OF SYMBOL; TPTR IS CLOBBERED
*
DUMPRINT	EQU	*
	LDAB	WIDTH	ELIMINATE OCCASIONAL
	SUBB	COLNO	  DANGLING SYMBOLS BY ONLY
	BITB	#$F0	  USING COLUMNS...
	BEQ	DMPTCR	  OF MINIMUM LENGTH.   B/ TOO SMALL.
	LDAB	COLNO	OUTPUT TAB IF SYMBOL...
	ADDB	#$F+1	WILL FIT IN REMAINDER...(+1 TO REMEMBER
			;  EXTRA IMPLIED SPACE)
	ANDB	#$F0	OF LINE, ELSE <CR>
	ADDB	#6	NOTE THAT EXTREMELY LONG...
	LDX	SYMPT	SYMBOLS THAT WONT FIT ON ONE LINE...
	ADDB	S:LEN,X	WILL BE CHOPPED
	LDAA	#TAB
	CMPB	WIDTH
	BLS	DMPT1
DMPTCR	LDAA	#$D	GET <CR>
DMPT1	BSR	DPUTLO	**POOF**
	LDX	SYMPT	OUTPUT ' ' IF SYMBOL
	LDAA	#BLANK	WAS USED, ELSE '*'
	LDAB	S:TYPE,X
	BITB	#T:USED
	BNE	DMPT2	LOVE THOSE RELATIVE BRANCHES!
	LDAA	#'*
DMPT2	BITB	#T:DDEF	IS THIS A DDEF?
	BEQ	DMPT3	IF NOT, GO ON
	LDAA	#'+	ELSE INDICATE THE DDEF CONDITION
DMPT3	BSR	DPUTLO	OUTPUT 'SYMBOL REFERENCED' CHARACTER
	LDX	SYMPT	OUTPUT THE SYMBOL NAME
	LDAB	S:LEN,X	GET ITS LENGTH
DUMPRINT1 EQU	*
	LDAA	S:TEXT,X	GET BYTE OF NAME
	INX		BUMP NAME SCAN POINTER
	STX	TPTR	TERRY IS CLEVER, CLEVER...
	BSR	DPUTLO	HOPE THIS PRESERVES (B)
	DECB		IF NOT, I WILL HAVE TO FIX IT!
	BLE	DUMPRINT2	USED UP, GO PRINT VALUE
	LDX	TPTR	GET POINTER TO SYMBOL NAME
	BRA	DUMPRINT1	TALK ABOUT GROUND OUT CODE...
DPUTLO	JMP	LSTCP	THIS STUFF LOOKS LIKE HAMBURGER
DUMPRINT2 EQU	*
	LDAA	#'/	OUTPUT '/'
	BSR	DPUTLO	WHAT DO YOU WANT FOR 2AM IN THE MORNING?
	LDX	SYMPT	IF SYMBOL IS UNDEFINED,
	LDAA	S:TYPE,X	OUTPUT VALUE AS "UNDF"
	BITA	#T:UNDF	AND EXIT
	BEQ	DUMPRINT3
	LDX	#UNDFTXT
	JMP	PTTL	AND RETURN
UNDFTXT	EQU	*
	FCC	"****"
	FCB	0
DUMPRINT3 EQU	*
	LDX	SYMPT	HOME STRETCH COMING UP!
	LDAA	S:VAL,X	IF SYMBOL HAS A VALUE,
	BSR	OUTX2	OUTPUT AS XXXX AND EXIT
	LDX	SYMPT	THIS TRASH SHOWS THE VALUE OF...
	LDAA	S:VAL+1,X	EVEN A STUPID SIL
OUTX2	JSR	ATX2	DON'T LOOK NOW, TERRY CHEATED!
	BSR	DPUTLO
	TBA	
	BRA	DPUTLO	BUT HE FINISHED FIRST!
	FIN
	PAGE	- - - SPECIAL SYMBOL DEFINITIONS - - -

SYM6800	FDB	0	S:NEXT
	IFUND	MC6809
	FDB	1	S:VAL
	ELSE
	FDB	0	S:VAL
	FIN
	FCB	T:SPEC,5	S:TYPE; S:LEN
	FCC	'M6800'


SYM6801	#0,#0,T:SPEC,5	S:NEXT; S:VAL; S:TYPE; S:LEN
	FCC	'M6801'


SYM6809	FDB	0	S:NEXT
	IFUND	MC6809
	FDB	0	S:VAL
	ELSE
	FDB	1	S:VAL
	FIN
	FCB	T:SPEC,5	S:TYPE; S:LEN
	FCC	'M6809'

         PAGE     - - - N O N - P A G E - Z E R O   D A T A - - -
*
*
*
* DEBUG SPACE
*
*
         DO       DEBUG=1
         ORG      *
PATCH    RMB      $80
         ELSE
PATCH    RMB      $20
         FIN

DATBEG   EQU      *

*
*  NON-PAGE ZERO STUFF
*

DATIME	RMB	20	SPACE FOR DAY AND TIME
NULLTMP	RMB	5	SAVE ROOM FOR DUMMY OUT OF MEM SYMBOL
*
TBTB	RMB	2	SAVE 2 SPOTS FOR INITIAL TABS
	RMB	TABMAX+1	SAVE ROOM FOR USER TABS + NUL

*
NAMESZ   EQU      30
NAMEBF   RMB      NAMESZ            30 BYTES FOR THE NAME
         DO       TITLES=1
TTLMAX   EQU      105               MAX TITLE SIZE
TTLBUF   RMB      TTLMAX            ONE FOR THE NULL
SBTTBF   RMB      TTLMAX
         FIN

*
BBST    RMB      BBSIZE+3
BBEND   EQU      *-1
*

**** SDOS Load File Data ****
FPbuffer		rmb 4	Q/ Temporary file-pointer buffer.
SectorMask	rmb 2	W/ Mask for extracting sector #.
NotSectorMask	rmb 2	W/ Mask for extracting byte-in-sector.
Newrec		rmb R$SHORT:	Record.descriptor/ Base/length of new seg.
Currec		rmb R$LONG:	Record.descriptor/ Base, Length, ...
			;	            Fileposition, Valid.

**** Hash Table ****
	IF	PZBASE#0
HSHTBL	RMB	(HASH:M+1)*2
HSHEND	EQU	*
	FIN

*
* STRING BUFFER
*
         DO       STRINGS=1
STBUF    RMB      $100              STRINGS UP TO 256 BYTES ?!
         FIN

* LISTING BUFFER
LBSIZ    EQU      NUMSTRT
LBBEG:   RMB      LBSIZ
EOLBF:   EQU      *-3

*
* INPUT BUFFER
*
IBUFI    RMB      IBUFS             INPUT BUFFER
INBUFE:  RMB      2                 A LITTLE EXTRA SLOP.
*
   
DATEND   EQU      *

ASMEND	EQU	*
		;(IT TELLS SYM TABLE WHERE TO STOP!!)
