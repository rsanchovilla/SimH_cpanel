	PAGE	- - -  M A I N   L O O P  - - -
*
*  MAIN LOOP OF ASSEMBLER
*
ASM      EQU      *                 START ADDRESS FOR ASSEMBLY
	LDS	STACK$	LOAD STACK PTR
	JSR	RESET$	RESET EVERYONE
         JSR      P1INIT            DO SETUP FOR PASS 1,
LOOP1    JSR      PINIT             DO SETUP FOR EVERY PASS,
*
LOOP     BSR      READ              READ RECORD & DO PRE-SCAN
         JSR      DOIT              EXECUTE THE COMMAND
         LDAA     EOFF              DID WE HIT AN END ?
         BEQ      LOOP              B/ NO:  KEEP GOING
*
ENDIT    JSR      END:              WHEN WE HIT AN END OF FILE,
	BRA	LOOP	  Treat as END.  Only returns if end of
;			  INCLUDE file.
*
*	SIMPLE, ISN'T IT?
*
	PAGE	- - -  R E A D   I N P U T   L I N E  - - -
*
*
*  R E A D
*
*        NO ARGUMENTS.
*        SETS @INBUF := NEXT NON-COMMENT INPUT LINE, TERMINATED
*          BY A NULL.  USES INPTR TO MAINTAIN POINTER INTO INBUF
*
*        THE FOLLOWING FUNCTIONS ARE HANDLED HERE (OR IN ROUTINES CALLED HERE):
*
*        (1)      RECOGNITION & ELIMINATION OF COMMENT LINES.
*        (2)      RECOGNITION OF COMMAND FIELD.  (CFPTR)
*        (3)      MACRO EXPANSION (ACTUAL TEXT SUBSTITUTION ONLY);
*                 CODE TO DO MACRO EXPANSION HERE IFF. MACRO = 1
*        (4)      ITERATIVE ASEMBLY (PACKING & UNPACKING FROM MEM)
*                 IFF. ITER = 1
*        (5)      NULLS ARE ELIMINATED (I.E. IGNORED)
*        (6)      ^L (FORM-FEED) CHARACTERS IN COLUMN 1 ARE
*                 REMOVED, AND A PAGE IS SIGNALED.
*	(7)	SOURCE LINE NUMBERS ARE ELIMINATED, AND THE LABEL
*		FIELD IS FOUND (LFPTR).
*	(8)	IF END OF FILE WAS ENCOUNTERED READING THIS LINE, **EOF
*		ENOUNTERED** IS REPORTED.
*
	PAGE

**** Handle end of file -- dummy up an END ****
RENDIT	LDAA	INCLVL	ARE WE IN AN INCLUDE FILE?
	BNE	INCLEOF	B/YES
	JSR	ENDER	BITCH ABOUT MISSING END
	LDX	INBUF
	STX	TPTR
	LDX	#RENDEND	Load address of an END image,
	JSR	MOVEB1	  move it to INBUF.
	BRA	READ1	Now, process line.

INCLEOF	PULA		PRUNE
	PULA		 STACK
	JMP	ENDIT

*
* LINE W/O CF:
READC1   TSTB                       ;LF SEEN?
         BEQ      READC             ;NO
READE	LDX	#FAKEEQU	WELL.. FAKE "LF EQU *"
	STX	CFPTR
	INX
	STX	EXPTR
	RTS
*
READC    LDAA     EOFF              HIT END OF FILE?
         BNE      RENDIT             B/ YES:  PERFORM AN "END"
	LDX	RPTF	ARE WE SKIPPING DUE TO RPT 0 ?
	BEQ	READC2	B/ YES ==> MARK AS SKIPPED.
	LDAA	SKPFLG	ARE WE SKIPPING DUE TO IF 0 ?
	BEQ	READC3	  B/ NO; NORMAL LISTING.
READC2	JSR	MRKSKP	SKIPPING A COMMENT (?!)
READC3	EQU	*
         JSR      LIST              LIST A COMMENT LINE...
READ     EQU      *                 ENTRY POINT FOR READ
         JSR      CLRLST            CLEAR LISTING FUNCTIONS,
         LDX      #LINEN            BUMP LINE #
         JSR      INCBCD            IN BCD.
	JSR	INLINE	GO GET A LINE FROM WHOMEVER IS GIVING THEM OUT
READ1	EQU	*	CONTINUE WITH LINE PROCESSING.
	PAGE
*
*   LINE HAS BEEN INPUT;  NOW FIND THE CF, FILTER OUT COMMENTS,
*   AND HANDLE CONTROL-L'S.
*
READ20	EQU	*
         LDX      INBUF             ;MOVE POINTER...
         STX      EXPTR             ;TO START OF BUFFER
*
*START TO SCAN LINE
*
READ2	BSR	RGNUM	READ LINE NUMBER (IF ANY)...SET UP LFPTR.
	JSR      GETXC             ;GET THE FIRST CHAR.(INC EXPTR)
         JSR      WHICH             ;WHAT IS IT?
         FCB      +BLANK
         FDB      READ3             B/ BLANK:  END OF LABEL.(NO LABEL)
         FCB      +TAB
         FDB      READ3             B/ TAB:  END OF LABEL.(NO LABEL)
         FCB      +'*
         FDB      READC             B/ * ==> COMMENT
         FCB      +';
         FDB      READC             B/ ; ==> COMMENT
         FCB      +$D
         FDB      READC             B/ CR ==> COMMENT
*
         DO       STRINGS=1
         FCB      +'"
         FDB      READQS            B/ " ==> QUOTED STRING
         FIN
*
         FCB      0
*
* NONE OF THE ABOVE:  EAT A LABEL FIELD.
READ2L   JSR      GETXC             ;GET A CHAR
         JSR      WHICH             ;WHAT IS IT.
   
         FCB      +BLANK            B/ BLANK:  END OF LABEL.
         FDB      READ3A
         FCB      +TAB
         FDB      READ3A            B/ TAB ==> END OF LABEL.
         FCB      +';
         FDB      READE             B/ '; ==> LF W/O CF ==> YECCH.
         FCB      +$D
         FDB      READE             B/  CR ==> LF W/O CF ==> YELL.
*
         DO       STRINGS=1
         FCB      +'"               B/ DBL QUOTE ==> ANOTHER STRING
         FDB      READQS
         FIN
*
         FCB      +0                END OF LIST
         BRA      READ2L


FAKEEQU	FCC	'EQU *'
	FCB	$0D
	PAGE
*
*  RGNUM --
*
*	SCANS AND DELETES LINE NUMBERS FROM BEGINNING OF LINE (IF LN-
* SCANNING WAS REQUESTED).
*
*	SETS LFPTR TO FIRST CH OF LABEL FIELD.
*

RGNUM	EQU	*
	LDAA	OPTF	ARE LINE NUMBERS PRESENT TO BE DELETED?
	BITA	#OPT:LN	  FIND OUT BY CHECKING THE OPTION FLAGS...
	BEQ	RGRTS	  B/ NO...  JUST SET LFPTR AND RETURN.

RGNUM1	EQU	*	OTHERWISE GNAW AWAY AT UNSIGHTLY
	JSR	GETXC	  LINE NUMBERS.   READ A CHARACTER,
	JSR	CHRANGE	  AND MAKE DECISION ACCORDING TO WHAT IT IS.
	+'0,'9,RGNUM1	DIGIT ==> EAT SOME MORE.
	+'.,'.,RGNUM1	DOT   ==> EAT SOME MORE.
	+BLANK,BLANK,RGRTS	SPACE ==> END OF NUMBER, EAT IT TOO.
	+TAB,TAB,RGRTS	TAB   ==> LIKE SPACE.
	0		<END OF LIST>
 
*  ALL OTHER CHARACTERS COME HERE... AND MEAN 'IMMEDIATE END OF NUMBER, START
*  OF LABEL FIELD.

	JSR	REJXC	SO GIVE THE CHARACTER BACK TO THE LABEL FIELD.

RGRTS	EQU	*	COME HERE TO SET LFPTR TO EXPTR AND SPLIT.
	LDX	EXPTR	WHAT DID I TELL YOU?
	STX	LFPTR	(A BRIEF MUSICAL INTERLUDE)
	RTS		LIKE THE MAN SAID....
*
* EAT A STRING:
*
         DO       STRINGS=1
READQS   EQU      *
         JSR      GETXC             ;GET A CHARACTER
         CMPA     #$D               ;END OF LINE?
         BEQ      READE             ;B/ YES ==> END OF THE LINE FOR THIS
         CMPA     #'"               ;END OF STRING?
         BNE      READQS            ;B/ NO ==> GET ANOTHER CH.
         BRA      READ2L            ;END ==> GO EAT REST OF LF.
         FIN
*
*  EAT BLANKS TO START OF CF
*
READ3A   LDAB     #$FF              ;REMEMBER THAT LF SEEN
         BRA	READ3L	;SKIP NEXT BYTE
READ3    CLRB                       ;REMEMBER:  NO LF SEEN
READ3L   JSR      GETXC             ;GET ANOTHER CHARACTER
         JSR      WHICH             ;WHAT IS IT.
         FCB      +BLANK
         FDB      READ3L            BLANK ==> KEEP GETTING
         FCB      +TAB
         FDB      READ3L            TAB >> DITTO
         FCB      +';
         FDB      READC1            ; ==> NO CF
         FCB      +$D
         FDB      READC1            CR ==> NO CF
         FCB      +0                END.
*
* FOUND CF.
         LDX      EXPTR             GET POINTER TO CF
         DEX                        ;POINT TO START,
         STX      CFPTR             ;AND SAVE IT.
READR    RTS                        ;SPLIT.

**** Image of end statement for happy assembler ****
RENDEND fcc	/	END	;<<Supplied By ASM>>/
	fcb	$D,0
	PAGE	- - -  F O R M F E E D  - - -

**** FORMFEED ****

*	DOES ALL WORK REQUIRED WHEN A FORM SEPARATION IS DETECTED
*	IN THE SOURCE FILE.  FAIRLY TRIVIAL.


FORMFEED	EQU	*
	JSR	EJECT	ADVANCE LISTING,
	LDX	#FPGE	BUMP FORM COUNTER.
	BSR	INCBCD
	LDX	#1	RESET LINE-#-WITHIN-FORM
	STX	LINEN

	RTS		AND GO HOME.
         PAGE     - - - I N C B C D - - -
*
*  INCBCD --
*        INCREMENTS A BCD NUMBER POINTED TO BY X
*
*        ACCA IS CHANGED
*
INCBCD   EQU      *
         LDAA     1,X               GET LOW BYTE
         ADDA     #1                INCREMENT IT
         DAA                        ADJUST IT,
         STAA     1,X               AND SAVE IT.
         LDAA     0,X               DO SAME FOR HIGH BYTE
         ADCA     #0
         DAA
         STAA     0,X
         RTS
         PAGE     - - - W H I C H - - -
*
*4*
*
*
* WHICH --
*        DOES A TABLE BRANCH ACCORDING TO THE CONTENTS OF A.
*
*        A IS COMPARED W/ THE FIRST BYTE OF A 3-BYTE ENTRY;
*        IF IT MATCHES, THEN THE NEXT TWO BYTES ARE TAKEN AS
*        THE EXIT ADDRESS.  OTHERWISE, THE NEXT ENTRY IS TRIED.
*        THIS GOES ON UNTIL AN ENTRY IS FOUND W/ A ZERO ENTRY.
*
*        NOTE THAT A SHOULD NOT BE ZERO.  IF NO MATCH IS FOUND
*        THEN WHICH RETURNS TO THE BYTE FOLLOWING THE ZERO BYTE.
*
*
WHICH    EQU      *
	PULX		;Get table address
*
WHCH1    CMPA     0,X               ;IS THIS THE ONE?
         BEQ      WHCHX             ;B/YES ==>  BYE.
	LEAX	3,X
         TST      0,X               ;END OF LIST?
         BNE      WHCH1             ;B/ NO==> TRY ANOTHER.
*
         JMP      1,X               ;TAKE FAIL EXIT.
*
* MATCH FOUND--
WHCHX    LDX      1,X               GET THE RETURN @
         JMP      0,X               AND GO, BABY, GO.
         PAGE     - - - C H R A N G E - - -
*
*
*  C H R A N G E
*
*        Check for character in A in a specified range; branch
*        to appropriate place if so.
*
*
CHRANGE  EQU      *
	PULX		;Table address
CHRLP    CMPA     0,X
         BLT      CHRNX
         CMPA     1,X
         BGT      CHRNX
         LDX      2,X
         JMP      0,X
*
CHRNX	LEAX	4,X
         TST      0,X
         BNE      CHRLP
         JMP      1,X               TAKE A FLYING LEAP
         PAGE     - - - E R R O R   S U B R O U T I N E S - - -
   
*5*
*
* ERROR ROUTINES:
*
*  ERROR ROUTINES WILL ALWAYS CONSIST OF A BSR TO ERROR,
*  FOLLOWED BY A TWO-BYTE FLAG VALUE.  THIS VALUE WILL BE
*  INCLUSIVE 'OR'ED INTO ERRFLG.
*
CRSCAN   BSR      ERROR             ;LF W/O CF.
         FDB      F:ILLF
*
ILLF     EQU      CRSCAN
*
SYNTAX   BSR      ERROR             ;SYNTAX ERROR
         FDB      F:SYNTX
*
ILLAF    BSR      ERROR
         FDB      F:ILLAF           ;ILLEGAL AF
*
UNDEF    BSR      ERROR
         FDB      F:UNDF            ;UNDEFINED SYMBOL
*
DDEF     BSR      ERROR
         FDB      F:DDEF            ;DOUBLY DEFINED SYMBOL
*
UDDEF    BSR      ERROR
         FDB      F:UDDF            ;USE OF DBL-DEF SYMBOL
*
NSTER    BSR      ERROR             *E* NESTING ERROR IF/DO/CASE
         FDB      F:NST
*
ENDER    BSR      ERROR             *E* EOF HIT BEFORE END STATEMENT.
         FDB      F:END
*
OVRFLW   BSR      ERROR             *E* VALUE OVERFLOWED FIELD
         FDB      F:OVF
*
BRANERR  BSR      ERROR             *E* OUT-OF-RANGE BRA/BSR
         FDB      F:BRA
*
ILLVAL   BSR      ERROR             *E* ILLEGAL VALUE
         FDB      F:ILV
*
BADDIG   BSR      ERROR             *E* BAD DIGIT
         FDB      F:BADD
*
SYMOV    BSR      ERROR             *E* SYMBOL TABLE OVERFLOW
         FDB      F:SYMOV
*
ILLSTR   BSR      ERROR             *E* ILLEGAL STRING
         FDB      F:ILST
*
PHASERR	BSR	ERROR	*E* PHASE ERROR.
	FDB	F:PHASE
*
ILLCF	BSR	ERROR	*E* REGISTER MISSING ON OPCODE LINE.
	FDB	F:ILLCF
*
ILTRUNC	BSR	ERROR	*E* INPUT LINE WAS TOO LONG.
	#F:ILTRUNC

FILNOTF	BSR	ERROR	*E* INCLUDE FILE NOT FOUND
	#F:FILNOTF

FRCREFERR
	BSR	ERROR	*E* Forced reference impossible
	#F:FRCREF

SAERR	BSR	ERROR	*E* START ADDRESS ERROR (NON-MATCH)
	#F:SAERR
	PAGE
*
*
* NAME:	ERROR
*
* FUNCTION:	MERGES ERROR CODE INTO FLAGS OF ERRORS REPORTED
*	FOR THIS LINE.
*
* CALL:
*	<TOS, TOS-1>	ADDRESS OF ERROR CODE.
*	<NOS, NOS-1>	RETURN ADDRESS.
*
* ERROR CODES HAVE THE FOLLOWING FORM:
*	FCB	GROUP,MASK
*
*	GROUP	INDICATES WHICH CELL IN ERR:F IS TO BE UPDATED;
*	MASK	IS A BIT MASK INDICATING WHICH BIT IS TO BE UPDATED.
*
* REGISTERS:
*	A AND B ARE PRESERVED;  X IS DESTROYED.
*
*
ERROR
	PULX		;Error code address
         PSHA                       ;GET SOME WORKING STORAGE
	PSHB
	LDAA	1,X	LOAD ERROR MASK,
	LDAB	0,X		ERROR GROUP.
	JSR	LDAXB	CONVERT ERROR GROUP TO ERROR FLAG CELL @.
	#ERR:F			(BASE OF ERROR FLAG TABLE)
	BITA	0,X	IS FLAG ALREADY SET?
	BNE	RTERR		B/ YEAH, DON'T BUMP COUNT.
	ORAA	0,X	ELSE, SET FLAG,
	STAA	0,X		RETURN RESULT TO TABLE.

*  BUMP GLOBAL ERROR COUNT.....
	LDAA	ERRC	GET THE COUNT -- IT'S BCD.
	ADDA	#1		ADD 1 TO IT,
	DAA			IN DECIMAL.
	BCS	ERROR$1		B/ OVERFLOW, DON'T SAVE.
	STAA	ERRC	ELSE SAVE.

ERROR$1 EQU	*
	LDAA	LIST:T	SET "ERROR LINE" FLAG FOR LISTER.
	ORAA	#LF:ERR
	STAA	LIST:T

*  COMMON EXIT PATH.
RTERR	PULB		RESTORE REGISTERS & EXIT.
	PULA
	CLC
	RTS
         PAGE     - - - I S I T - - -
*
*
*  I S I T
*
*        SCANS A LIST OF VALUES, RETURNS CC # 0 IF
*        NONE OF THEM ARE EQUAL  TO THE VALUE IN ACCA
*
*        A, X ARE DESTROYED.
*
*
ISIT     EQU      *
	PULX		Pointer to table
ISITL    CMPA     0,X               IS THE NEXT ONE THE ONE I WANT?
         BEQ      ISITR             B/ YES.  GET OUT
         INX                        POINT TO NEXT...
         TST      0,X               END OF LIST ?
         BNE      ISITL             B/ NO.  TRY ANOTHER
         INX                        (BUMP POINTER AND CLEAR CCZ)
         JMP      0,X               GO DO THE NEXT INSTRUCTION
ISITR    INX                        EAT UP THE REST OF THE LIST
         TST      0,X
         BNE      ISITR
         JMP      1,X               AND LEAVE.
         PAGE     - - - D O I T - - -
*
*
*
* DOIT --
*
*        CALLS DOITL, CONTROLS SKIPPING, ETC.
*
*        DOITL IS PERFORMED ACCORDING TO THE FOLLOWING CONDITIONS:
*        (A)      RPTF<0  ==> DOITL IS CALLED ONCE.
*        (B)      RPTF=0  ==> DOITL IS NOT CALLED. LINE IS SKIPPED.
*        (C)      RPTF>0  ==> DOITL IS CALLED RPTF TIMES.
*        (D)      SKPFLG#0 ==> DOITL IS NOT CALLED; WE ARE SKIPPING.
*
*        IN THIS LAST CASE (SKPFLG#0), WE CHECK WHETHER THE
*        COMMAND IS SOMETHING WHICH AFFECTS OUR SKIPPING.
*        THE COMMANDS ARE:
*        (1)      DO/IF/CASE        BUMP SKPLVL
*        (2)      ELSE/ELSEIF       IF SKPLVL=0, & IF SKIPFLB
*                                   > 0, THEN DEC SKPFLG.  IF
*                                   SKPFLG = 0, THEN TERMINATE
*                                   THE SKIP.  IF SKPLVL<0, CONTINUE
*                                   SKIPPING.
*        (3)      FIN               DEC SKPLVL; IF SKPLVL=0 THEN
*                                   SKPFLG:=0, WHICH STOPS THE SKIP.
*
*
	PAGE
DOIT     EQU      *
         DO       CONDASM=1         FOR CONDITIONAL ASSEMBLY...
         LDAA     SKPFLG            ;ARE WE SKIPPING MULTIPLE LINES?
         BNE      DOSKP             ;B/ YES ==> HANDLE SKIP.
         FIN
*
         DO       REPEAT=1
	LDX	RPTF	;IS THIS A NORMAL LINE?
	BEQ	DOSKR	B/ NO ==> RPT 0 WAS DONE ON ME.
	BMI	DONORM	B/ YES; PERFORM ONE TIME.
*  REPEAT N TIMES.
	BSR	CDOSKP	CK/ CAN THIS LINE BE RPT'D?
	BVC	DOSKR	  B/ NO.  WANDER AROUND AND YELL ... EVENTUALLY.
DOMANY   BSR      DONORM            ;EXECUTE THE LINE,
         LDX      RPTF              ;DECREMENT THE COUNTER.
         DEX
         STX      RPTF
         BGT      DOMANY            ;B/ DO IT AGAIN, SAM.
	LDX	#-1
         STX      RPTF              ;SET RPTF := -1
         RTS
         FIN
*
*  NORMAL PERFORM.
	IF	REPEAT=1
DORPTER	JSR	NSTER	==>SPOT THE TWIT<==
	FIN
DONORM   JMP      DOITL             ;GO DO THE GOOD THING.
*                                   (AND DON'T COME BACK)
	PAGE
*
* DO SKIP FOR RPT (DO1)
*
         IF       REPEAT=1
DOSKR    LDX	#-1	SET X TO $FFFF,
	STX	RPTF	  AND CLEAN UP RPTF.
	BSR	CDOSKP	CK/ O.K. TO SKIP THIS LINE?
	BVC	DORPTER	  B/ NO.  FORCE IT.  REPORT ERROR.
         FIN
         IF       CONDASM!REPEAT
DOSK1    JSR      MRKSKP            ;MARK AS SKIPPED LINE,
	JMP	LIST	;AND LIST THE LINE
*
DOSK2	LDAA	LIST:T	LOAD LIST FLAGS
	ORAA	#LF:CA	MARK THIS AS A CA LINE
	STAA	LIST:T	SHOVE BACK
	JMP	LIST	GO LIST CA LINE
         FIN
	PAGE
*
* DO SKIP FOR DO/IF/CASE.
*
         IF       CONDASM=1
DOSKP	BSR	CDOSKP	CK/ IS THE COMMAND SIGNIFICANT DURING C.A. SKIPS?
	BVS	DOSK1	  B/ NOPE.  MARK AS SKIPPED, GET ANOTHER LINE.
	JMP	0,X	YES:  GO TO HANDLER.
	FIN

	IF	CONDASM!REPEAT
*
*  CDOSKP --
*
*	CHECKS CF OF CURRENT LINE.  IF CF(1) IS SYMBOL ONLY, AND IF
*	SYMBOL SPECIFIES A C/A DIRECTIVE, THEN V IS CLEARED, AND
*	X IS SET TO ADDRESS OF HANDLER.
*
*	OTHERWISE V IS SET, AND X IS DONT-CARE.
*

CDOSKP	JSR	CHCFSYM	CK/ IS CF(1) A SYMBOL ONLY?
	BNE	DOSK1R	  B/ NO.  NOT C/A DIRECTIVE.
	JSR	CHSYMD	CK/ IS SYM A DIRECTIVE WE KNOW?
	BNE	DOSK1R	  B/ NO.  NOT C/A DIRECTIVE.
         LDX      SYMPT             GET POINTER TO DIRECTIVE
         LDAA     S:VAL,X           ELSE GET GOOD STUFF
         LDAB     S:VAL+1,X         AND SCAN FOR THE DIRECTIVE WE LIKE.
         LDX      #DOSKPT           GET @ OF TABLE,
DOSKPL   CMPA     0,X               HIGH BYTE MATCH?
         BNE      DOSKP1            B/ NO, TRY NEXT
         CMPB     1,X               LOW BYTE MATCH?
         BNE      DOSKP1            B/ NO, TRY NEXT
	CPX	#DOSKPT1	CK/ END DIRECTIVE GET US HERE?
	BEQ	DOSKPL1	  B/ YES, DON'T MARK AS C/A LINE.
	LDAA	#LF:CA	MARK THIS LINE...
	ORAA	LIST:T	  AS A CONDITIONAL ASSEMBLY LINE
	STAA	LIST:T
DOSKPL1	EQU	*
         LDX      2,X               GET BRANCH @,
	CLV		ENSURE V IS RESET.
	RTS		  ALL DONE.
*
DOSKP1   CPX      0,X               END OF LIST?
	BEQ	DOSK1R	  B/ YES: NOT ONE OF OURS, KIMOSAVE.
	LEAX	4,X
         BRA      DOSKPL


*
DOSK1R	SEV		YOU CAN'T ALWAYS GET WHAT YOU WANT.
	RTS

*
DOSKPT   EQU      *
         FDB      DO:,DOSKD
         FDB      IF:,DOSKD
	FDB	IFUND:,DOSKD
         FDB      CASE:,DOSKD
         FDB      ELSE:,DOSKE
         FDB      ELSIF:,DOSKE
         FDB      FIN:,DOSKF
	FDB	RPT:,DOSK1	NO-OP FOR RPT, JUST PUT IT IN TABLE.
DOSKPT1	FDB      END:,DOSKX
         FDB      *
         FIN
	PAGE
	IF	CONDASM=1
*
* TRYING TO SKIP DO/IF/CASE:
DOSKD    INC      SKPLVL            ;INCREMENT SKIP LEVEL.
         BNE      DOSK1             B/ NO OVERFLOW IN SKIP COUNTER,
         JSR      NSTER             *E* NESTING ERROR
         DEC      SKPLVL            DECREASE SKIP LEVEL,
DOSK1A   BRA      DOSK1             AND SPLIT.
*
* TRYING TO SKIP ELSE/ELSEIF
DOSKE    LDAA	SKPLVL            IF SKIP LEVEL>0, THEN...
         BNE      DOSK1A              B/ THIS ELSE IS MEANINGLESS
*  THIS ELSE MIGHT TERMINATE THE SKIP
         LDAA	SKPFLG            ARE WE SKIPPING TO AN ELSE?
	BLT	DOSK1A	  B/ NO, ONLY A FIN WILL STOP US.
         DEC      SKPFLG            IS THIS THE 'ELSE' WE WANT?
	BGT	DOSK1A	  B/ NO, GOT SOME MORE TO SKIP.
*  THIS ELSE WILL STOP US.  WE MAY HAVE TO DO SOME WORK IF
*  THIS IS AN ELSEIF, HOWEVER.
         LDX      SYMPT             WAS THIS AN ELSEIF?
         LDX      S:VAL,X
         CPX      #ELSIF:
         BNE      DOSK2A             B/ NO.  FLAGS ARE RESET, GO LIST.
*
* WORK FOR ELSIF:
	LDX	CFPTR	SET UP AFPTR'S VALUE.
	STX	AFPTR
         JSR      EVAFCT            WHATS THE VALUE?
         JSR      PUTV              LIST THE VALUE,
         LDX      VAL               START EXECUTING?
         BGT      DOSK2A            B/ YES.
         INC      SKPFLG            WE ARE SKIPPING TO ANOTHER ELSE.
DOSK2A   JMP      DOSK2             SO GO AWAY.
*  END HIT WHILE SKIPPING
DOSKX    JSR      ENDER             *E* END HIT WHILE SKIPPING.
         JMP      END:
	PAGE
*
*  WORK FOR FIN:
*
DOSKF    LDAA	SKPLVL            MOVE US BACK DOWN A NOTCH...
         BEQ      DOSKF1            B/ END OF SKIP...
         DEC      SKPLVL
         BRA      DOSK1A            NOT END: MARK AS SKIPPED.
*
DOSKF1   STAA     SKPFLG            STOP THE SKIP.
         DEC      DOLVL
         BRA      DOSK2A            AND MARK AS EXECUTED
         FIN





	IFUND	MC6809
	INCLUDE	ASM14A.DITL0.ASM	ASM FOR 6800
	ELSE
	INCLUDE	ASM14A.DITL9.ASM	ASM FOR 6809
	FIN
         PAGE     - - - D I R E C T I V E   T A B L E - - -
*
*
*  D I R E C T I V E   T A B L E
*
*        D0 IS THE HEAD OF A STANDARD SYMBOL CHAIN.  REMEMBER
*        THAT THE FIRST 2 BYTES POINT TO THE NEXT IN THE CHAIN,
*        THE NEXT 2 BYTES POINT TO THE HANDLING ROUTINE,
*        THE NEXT BYTE IS THE TYPE, AND THE NEXT IS THE #
*        OF CHARACTERS IN THE SYMBOL TEXT.
*
*
DIREND	EQU	0	Null link marks end of table

D0       FDB      D1                HEAD OF CHAIN.
*
D1       FDB      D2,EQU:,DT:SS!DT:AF+3
         FCC      /EQU/
         DO       NICE=1
D2       FDB      D3,SET:,DT:SS!DT:AF+3
         FCC      /SET/
         ELSE
D2       EQU      *
         FIN
D3       FDB      D4,RMB:,DT:AF!DT:LF!3
         FCC      /RMB/
D4       FDB      D5,FCB:,DT:AF!DT:LF!3
         FCC      /FCB/
D5       FDB      D5A,FDB:,DT:AF!DT:LF!DT:SS!3
         FCC      /FDB/
D5A	FDB	D6,FFC:,DT:AF!DT:SS!3
	FCC	/FFC/
D6       FDB      D7,ORG:,DT:AF!DT:SS!3
         FCC      /ORG/
	IF	TITLES=0
D7       FDB      D8,NOP,DT:!3
         FCC      /NAM/
D8       FDB      D9,NOP,DT:!4
         FCC      /NAME/
	ELSE
D7	FDB	D8,NAME:,DT:SS!DT:AF!3
	FCC	/NAM/
D8	FDB	D9,NAME:,DT:SS!DT:AF!4
	FCC	/NAME/
	FIN
D9	FDB	D10,OPT:,DT:AF!DT:SI!4
         FCC      /WITH/
D10      FDB      D11,END:,DT:!DT:AF!3
         FCC      /END/
*
         IF       TITLES=0
D11      FDB      D12,PAGE:,DT:CC!4
	FCC	/PAGE/
         ELSE
D11      FDB      D11A,PAGE:,DT:CC!DT:SS!DT:AF!4
	FCC	/PAGE/
D11A	FDB	D12,TITLE:,DT:CC!DT:SS!DT:AF!5
	FCC	/TITLE/
         FIN
         DO       NICE=1
D12      FDB      D13,SPACE:,DT:CC!DT:AF!5
         FCC      /SPACE/
         ELSE
D12      EQU      *
         FIN
D13      FDB      D14,SPACE:,DT:CC!DT:AF!3
         FCC      /SPC/
D14      FDB      D14B,FCC:,DT:AF!DT:LF!3
         FCC      /FCC/
D14B	EQU	*
	IF	NICE=1
	FDB	D14C,LIST:,DT:CC!DT:SI!DT:AF!4
	FCC	/LIST/
	FIN
D14C	EQU	*
	IF	NICE=1
	FDB	D14D,PCC:,DT:AF!DT:SI!3
	FCC	/PCC/
	FIN
D14D	EQU	*
	IF	NICE=1
	FDB	D14E,PSR:,DT:CC!DT:SI!DT:AF!3
	FCC	/PSR/
	FIN
D14E	EQU	*
	IF	NICE=1
	FDB	D14F,PGEN:,DT:CC!DT:SI!DT:AF!4
	FCC	/PGEN/
	FIN
D14F	EQU	*
	IF	NICE=1
	FDB	D14G,PCA:,DT:CC!DT:SI!DT:AF!3
	FCC	/PCA/
	FIN
D14G	EQU	*
	IF	REPEAT=1
	FDB	D14H,RPT:,DT:AF!DT:LF!DT:CA!3
	FCC	/RPT/
	FIN
D14H	EQU	*
         DO       CONDASM=1
D15      FDB      D16,DO:,DT:AF!DT:DO!DT:CA!2
         FCC      /DO/
D16      FDB      D17,ELSE:,DT:CA!4
         FCC      /ELSE/
D17      FDB      D18,FIN:,DT:CA!3
         FCC      /FIN/
D18      FDB      D20,IF:,DT:AF!DT:DO!DT:CA!2
         FCC      /IF/
D20      FDB      D21,ELSIF:,DT:CA!6
         FCC      /ELSEIF/
*
D21      FDB      D22,CASE:,DT:AF!DT:DO!DT:CA!4
         FCC      /CASE/
*
D22	FDB	D22B,IFUND:,DT:AF!DT:DO!DT:CA!5
	FCC	/IFUND/
D22B	FDB	D23,IFDEF:,DT:AF!DT:DO!DT:CA!5
	FCC	/IFDEF/
	ELSE
D15	EQU	*
	FIN
*
	IF	NICE=1	ONLY GET TABS IF NICE STUFF IS AROUND
D23	FDB	D24,TABS:,DT:AF!DT:SI!DT:CC!4
	FCC	/TABS/
	ELSE
D23	EQU	*
	FIN
*
D24	FDB	D25,END:,DT:!3
	FCC	/MON/
*
D25	FDB	D26,NOP,DT:!3
	FCC	/OPT/
*
D26	FDB	D27,INCLUDE:,DT:AF!DT:SS!7
	FCC	/INCLUDE/
*
	IFUND	MC6809
D27	EQU	DIREND
	ELSE
D27	FDB	DIREND,SETDPR:,DT:SS!DT:AF!6
	FCC	/SETDPR/
	FIN


*  TO BE ADDED LATER:
*        CAL      (SWI THAT LOOKS AT AF)
*        DATA
*        TEXT
*        TEXTC
*        TEXTZ
*
	PAGE	- - - FORM FLOATING CONSTANT - - -
	TITLE	- - - D I R E C T I V E   P R O C E S S I N G - - -
*
*
*   F F C :
*
*	HANDLES FFC COMMAND 
*
*
FFC:	CLRA		Set max
	LDAB	#$FF	 char to scan
	LDX	AFPTR	Point to AF
	JSR	FCONVI	Go form floating value
	RTS		Normal exit, return to directive handler
	NOP		Filler, RTS takes on 1 byte
	BRA	FCONVOVERFLOW	Overflow exit
	JMP	SYNTAX	Syntax error exit

FCONVOVERFLOW
	JMP	OVRFLW	Flag overflow error

	INCLUDE ASM14A.FCONV.ASM
	PAGE	- - - FORM CONSTANT BYTE - - -
*
*
*   F C B :
*
*        HANDLES FCB COMMAND.
*
*
FCB:     EQU      *
         BSR      EVNUL              EVALUATE FIRST FIELD
         LDAA     VAL               CHECK FOR IN RANGE.
         BEQ      FCB1              B/ ITS GOOD.
         COMA                       CHECK FOR $FF (LEADING SIGN)
         BNE      FCB2              B/ NO WAY ITS GOOD.
         LDAA     VAL+1             ELSE GET LOW BYTE,
         BLT      FCB3              B/ SIGN IS ON: NO OVERFLOW.
FCB2     JSR      OVRFLW            *E* BYTE OVERFLOWED FIELD.
FCB1     LDAA     VAL+1             GET LOW VALUE,
FCB3     JSR      PUTB              AND OUTPUT IT.
         JSR      GTCMA             GET NEXT COMMA.
         BVS      FCB:              B/ THERE IS ONE.
         RTS                        ELSE LEAVE: ALL DONE.
         PAGE     - - - F D B - - -
*
*
*   F D B :
*
*        HANDLES FDB COMMAND.
*
*
FDB:     EQU      *
	BSR	EVNUL	GET VALUE,
         LDAA     VAL               PUT H(VAL)
         JSR      PUTB
         LDAA     VAL+1             PUT L(VAL)
         JSR      PUTB
         JSR      GTCMA             A COMMA, PLEEZ.
         BVS      FDB:              B/ GOT ONE: DO ANOTHER FIELD.
NOP      RTS                        ELSE RETURN
	PAGE	- - - E V N U L - - -
*
*
* EVNUL - EVAL AN EXP AND IF IT IS NULL RETURN A ZERO - NOT AN ERR
*	RETURNS	C=0 IF EVALUBLE EXPRESSION PRESENT
*		C=1 IF NONE OR EXPRESSION CONTAINED A FORWARD REFERENCE
*
EVNUL	EQU	*
	JSR	GNOBJ	SEE IF A TERM COMES NEXT
	INC	RJOBJ	IN ANY CASE - REJECT THIS OBJECT
	CMPA	#O:TERM	WELL?
	BNE	EVIT	IF NOT, JUST MUNCH A EXP
	CLR	VAL	CLEAR VALUE
	CLR	VAL+1	DITTO
EVSEC	SEC
	RTS
*
EVIT	JSR	EVAL	GO TO IT
	LDAA	XTYPE	Get extpression type
	BITA	#XT:FREF	Forward reference?
	BNE	EVSEC	B/Yes
	CLC
	RTS
*
	PAGE	- - - L I S T I N G   C O N T R O L - - -
	IF	NICE=1
*
*
* LIST:
*	HANDLES LIST PSEUDO-OP;  IF AF #0 AND  LO WAS SPECIFIED, TURNS LISTING
*	ON; ELSE TURNS LISTING OFF.
*
* PCC:
*	HANDLES "PCC <VAL>"; TURNS LISTING OF CONTROL COMMANDS OFF OR ON; COMMANDS
*	AFFECTED ARE PAGE, TITLE, LIST, PCC, PSR, PCA, PGEN, SPACE.
*
* PSR:
*	HANDLES "PSR <VAL>"; TURNS LISTING OF SKIPPED RECORDS OFF OR
*	ON.
*
* PCA:
*	HANDLES "PCA <VAL>"; TURNS LISTING OF CONDITIONAL-ASSEMBLY COMMANDS OFF
* 	OR ON.  COMMANDS AFFECTED ARE DO, IF, ELSE, FIN, ELSEIF, RPT, CASE.
*
* PGEN:
*	HANDLES "PGEN <VAL>"; TURNS LISTING OF EXTRA GENERATED-CODE LISTING
*	LINES OFF OR ON.  IF "PGEN 0" IS SPECIFIED, THEN ONLY THE FIRST 4 BYTES
* 	OF A GENERATIVE DIRECTIVE WILL BE LISTED.  EG, FCC /ABCDEF/ WILL ONLY
* 	HAVE THE FIRST FOUR DATA BYTES LISTED.
*
LIST:	EQU	*
	LDAA	OPTF	IS LO INHIBITED?
	COMA
	BITA	#OPT:LO!OPT:LST	ARE BOTH ON?
	BNE	EVIT		B/ NO:  EVALUATE AND CHUCK.
	LDAB	#LL:LIST	GET LISTING BIT FOR LATER.
LIST:1	LDAA	PASS	IF THIS IS PASS 1, SPLIT.
	BEQ	EVIT	  B/ ALL DONE.
	PSHB		SAVE THE BIT TO BE AFFECTED,
	BSR	EVIT	  AND SLURP UP THE AF.
	PULB		GET OUR BIT BACK.
	LDAA	XTYPE	CHECK THE EXPRESSION FOR OK-NESS.
	BITA	#XT:UNDF	ONLY THING BAD IN PASS2 IS UNDEF.
	BNE	LIST:ILV	B/ REPORT ERROR IF UNDEFINED.
	TBA		COPY BIT INTO A,
	EORA	#-1	COMPLEMENT IT,
	ANDA	LIST:P	AND SCRUP THAT BIT IN LIST FLAGE.
	LDX	VAL	TEST CONTROL VALUE...
	BLE	LIST:2	  B/ TURN BIT OFF.
	ABA		  ELSE MERGE BIT IN.
LIST:2	STAA	LIST:P	SHOVE FLAGS BACK,
	BRA	ORG2	AND PUT THE VALUE IN DISPLAY.
LIST:ILV	JSR	ILLVAL	REPORT BAD VALUE,
	BRA	ORG2	AND PUT THE VALUE.
PCC:	EQU	*
	LDAB	#LL:PCC	GET OPTION
	BRA	LIST:1
PSR:	EQU	*
	LDAB	#LL:SKP	GET OPTION
	BRA	LIST:1
PCA:	EQU	*
	LDAB	#LL:PCA	GET OPTION
	BRA	LIST:1
PGEN:	EQU	*
	LDAB	#LL:GEN	GET OPTION
	BRA	LIST:1
	FIN
	PAGE	- - - R P T - - -
	IF	REPEAT=1
*
*
* RPT:
*	HANDLES RPT PSUEDO-OP.  IF RPTF IS NON-ZERO WHEN CALLED, THEN NSTERR IS 
* 	CALLED, AND THE RPT IS NOT PERFORMED.  RPTF IS SET TO +1 IN THIS CASE, FORCING
*	THE RPT TO BE ABORTED.  IF RPTF IS NEGATIVE, THEN AF IS EVALUATED, AND
*	IF IT IS OK, THEN RPTF IS SET TO THE VALUE.  IF <VAL> IS NEG, THEN RPTF IS SET 
*	TO ZERO.  
*
RPT:	EQU	*
	JSR	EVALCT	EVALUATE AS CONTROL VALUE,
	BNE	RPT:0	  B/ NO GOOD; SKIP NEXT.
	
	LDX	VAL	VAL IS GOOD; STUFF IT AWAY.
	BGE	RPT:1	  B/ IF NEG, THEN VAL=0.
RPT:0	LDX	#0	FORCE RPTF TO ZERO,
RPT:1	STX	RPTF	SAVE RPTF,
	BRA	ORG2	AND PUT VALUE.
	FIN
         PAGE     - - - O R G - - -
*
*
*   O R G :
*
*        HANDLES ORG COMMAND.  AF MAY NOT BE A FORWARD REF;
*        LF IS EQUATED TO NEW PC.
*
*
ORG:     EQU      *
         JSR      EVALCT            GET NEW PC
*        JSR      ISCTR             IS IT A CONTROL VALUE?
*                                   THE ABOVE MAKES SURE.
         BNE      ORG1              B/ NO GOOD.  DONT GO ANYWHERE
	JSR	ECKEOF	MAKE SURE THERE'S NO EXTRA SYNTAX
         LDX      VAL               ELSE SET NEW PC,
         STX      PC
ORG1     JSR      LFPC              (I KNOW, I COULD GO TO
*                                   LFEQU, SINCE VAL=PC, BUT
*                                   THAT WOULD UNDULY SCREW THE
*                                   SLOB WHO BLEW THE VALUE.
         JSR      PNCHIT            PUNCH THE BUFFER,
ORG2     JMP      PUTV              AND OUTPUT THE VALUE.
         PAGE     - - - E Q U - - -
*
*
*   E Q U :
*
*        HANDLES EQU COMMAND.  If value is a forward ref, well
*        so much the better.  By this, Motorola's weird forward
*        referencing is allowed on EQU's - I hope.
*
*
EQU:     EQU      *
         JSR      EVAL              EVALUATE THE AF,
	JSR	ECKEOF	ERROR IF EOF NOT NEXT
         JSR      LFEQU             THIS GETS DULL QUICK.
         BRA      ORG2              AND SPLIT.
         PAGE     - - - S E T - - -
         DO       NICE=1
*
*
*   S E T :
*
*        Handles SET command.  All of the above (EQU) holds,
*        except I throw up on forward references.  Be thee
*        warned...
*
*
SET:     EQU      *
         JSR      EVALCT            GET THE VALUE.
	JSR	ECKEOF	ERROR IF END OF FIELD NOT NEXT
         JSR      LFSET             IF NOT, IT'S ZERO NOW.
         BRA      ORG2              (SAVE A BYTE WHILE YOU CAN)
         FIN


	IFUND	MC6809
	ELSE
*
*
*   S E T D P R
*
*	Set the DPR register value for 6809
*
SETDPR:	JSR	EVALCT	AF value
	JSR	ECKEOF	Error if not at EOL
	CLR	VAL+1	Zap low order 8 bits of value
	LDAA	VAL
	STAA	DPRSET	Set DPR value
	BRA	ORG2
	FIN
         PAGE     - - - R M B - - -
*
*
*   R M B :
*
*        Handles RMB command.  I do very little idiot checking
*        here.  The value has to be a control value, but other
*        than that, I don't really much care.  If you really
*        want to RMB -3, alright:  but don't complain to me when
*        someone tells you your code is obscure (not to mention
*        opaque).
*
*
RMB:     EQU      *
         JSR      EVNUL	GET THE VALUE,
	JSR      ISCTR             MAKE IT NOT A FWD REF.
         JSR      PUTPC             LIST CURRENT PC
         LDAA     PC+1              AND ADD IN THE VALUE.
         ADDA     VAL+1
         STAA     PC+1
         LDAA     PC
         ADCA     VAL
         STAA     PC
         JSR      PNCHIT            PUNCH THE BINARY BUFFER,
	BRA	ORG2	  AND PUT VAL IN THE LISTING FIELD.
         PAGE     - - - F C C - - -
*
*
*   F C C :
*
*        Form constant characters.  This one is a mess.  There
*        are 2 possible forms of the FCC command.  First, there
*        is the standard FCC /string/ command.  That's easy;
*        if the delimiter is not a digit I just eat the string.
*           If the delimiter is a digit ('0-'9) then we go into
*        underdrive:  we scan until we find a non-digit; then
*        if it's not a comma we go off and take the first digit
*        as the delimiter.  Otherwise, we back up, call GNOBJ to
*        get us the number, eat the comma and use the next ch as
*        the delimiter.  Yecch.
*
*
FCC:     EQU      *
         LDX      #0                CLEAR # OF BYTES NEEDED
         STX      VAL
         JSR      GTCHT             GET FIRST CH OF AF, W/ TYPE
         CMPB     #C:NUM            IS IT A DIGIT
         BNE      FCC1              B/ NO:  MUST BE DELIMITER
FCC0     JSR      GTCHT             ELSE SLURP UP DIGITS
         CMPB     #C:NUM
         BEQ      FCC0              LIKE THAT.
         CMPA     #ASCII:COMMA	IS THE TERMINATOR A COMMA?
         BNE      FCCN              B/ NO:  BACK US UP & EAT WHOLE STRIN
* FOUND  FCC  <NUM>,/STRING/
         LDX      AFPTR             BACK US UP
         JSR      GNOBX             GET THE NUMBER
         LDAA     VAL               WHAT IS IT?
         BGE      FCC01             B/ ITS OK.
         JSR      ILLVAL            *E* WHAT DO YOU MEAN, NEGATIVE?
FCC01    JSR      GETXC             EAT THE COMMA
FCC02    JSR      GETXC             AND GET THE DELIMITER.
*
*  VAL NOW CONTAINS THE NUMBER OF BYTES (TOTAL) TO BE USED;
*  ACCA NOW HOLDS THE DELIMITER.
*
FCC1     TAB                        SAVE THE DELIMITER.
         CMPB     #$D               IS IT A (HORRORS) <EOL> ?
         BNE      FCC2              B/ YES: GO EAT THE STRING.
*  I SAW THE END OF THE STRING.  OUTPUT TRAILING BLANKS
FCCD     EQU      *
         LDX      VAL               GET # OF TRAILING BLANKS LEFT
	BLE	TABRTS	B/ ALL DONE: RETURN.
         DEX
         STX      VAL               SAVE VALUE
         LDAA     #BLANK
         JSR      PUTB              AND OUTPUT THE BLANK
         BRA      FCCD              ...TRY AGAIN.


*
* FCC2 -- OUTPUT THE STRING.
*
FCC2     JSR      GETXC             GET THE NEXT CHARACTER,
	ANDA	#$7F	  MASK IT TO SEVEN BITS...
         CBA                        IS IT THE DELIMITER?
         BEQ      FCCD              B/ YES:  ALL DONE.
         CMPA     #$D               A <EOL>?
         BNE      FCC3              B/ NO:  THIS IS A GOOD CH.
         JSR      ILLSTR            *E* ILLEGAL STRING
         BRA      FCCD              ALL DONE.
*
FCC3     PSHB                       SAVE THE DELIMITER
	LDAB	0,X	BUT FIRST -- MAKE SURE IT LISTS AS STRING CH.
	ORAB	#$80	  BY TURNING ON SIGN BIT,
	STAB	0,X	  AND POKING IT BACK.  (X) SET UP BY GETXC.
         JSR      PUTB              OUTPUT THE CHARACTER
	PULB		RETRIEVE DELIMITER CHARACTER.
	LDX	VAL	REMEMBER HOW MANY CHARACTERS WE HAVE OUTPUT ALREADY:
	DEX		  EACH CHARACTER WE OUTPUT NOW MEANS 
	STX	VAL	  ONE LESS BLANK TO OUTPUT LATER.
         BRA      FCC2              GO, GO, GO.
*
* BACK UP TO THE START & EAT AS REGULAR STRING
*
FCCN     LDX      AFPTR             HO HUM...
         STX      EXPTR
         BRA      FCC02             GO EAT THE STRING.
	PAGE	- - - T A B S - - -
	IF	NICE=1
*
*
* TABS COMMAND - RESETS TABS TO GIVEN COLUMNS
*	IF AN ERROR IS ENCOUNTERED, TABS ARE SET TO ORIGINAL DEFAULTS.
*
*  TABS MUST BE IN ASCENDING ORDER BETWEEN 2 AND 127 INCLUSIVE.
*
TABS:	LDX	#TBTB+1	GET USER-TABS POINTER.
	STX	TABTBL	SAVE PTR WHERE TO STORE NEXT GIVEN TAB-1
	CLR	1,X	MAKE A HOLE AT THE END....
	LDX	#NUMSTRT*$100+LSTSTRT	GET INITIAL TWO TABS
	STX	TBTB	STORE THEM
NTAB	JSR	EVAL	MAKE SURE IT IS AN OK VALUE
	LDAA	VAL+1	IS VALUE GIVEN OK?
	LDAB	VAL	WHAT ABOUT THE UPPER BYTE?
	BEQ	TABOK	IF OK - OK
TABOVF	JSR	ILLVAL	REPORT ERROR
TABERR	LDX	#TABTB:	LOAD DEFAULT TABLE
TABRET	STX	TABTBL	SAVE IT
TABRTS	RTS
*
*
TABOK	ADDA	#LSTSTRT-1	MAKE VALUE OK
	BCS	TABOVF	  B/ OOPS! TOO BIG.
	LDX	TABTBL	GET PTR TO NEXT TAB
	CMPA	0,X	SEE IF TAB IS GREATER THAN THE ONE BEFORE
	BLS	TABOVF	  B/ NOT, ERROR
	INX		PT TO NEXT TAB
	STAA	0,X	SAVE IT
	STX	TABTBL	SAVE PTR FOR NEXT TIME
	CPX	#TBTB+TABMAX+2
	BEQ	TABOVF	IF RAN OUT OF SPACE - ERROR
	CLR	1,X	SET END OF TAB TABLE
	JSR	GTCMA	GET THE COMMA
	BVS	NTAB	IF FOUND, GO DO NEXT TAB
	LDX	#TBTB	ELSE, LOAD UP RIGHT TABLE ADDR
	BRA	TABRET	AND GET OUT
	FIN
         PAGE     - - - P A G E - - -
   
*
*
*  P A G E :
*
*        Handles PAGE command.  Just puts us at bottom-of-form
*        and returns.
	IF	TITLES=1
*	IF ARGUMENT FIELD IS NON-ZERO, SETS SUBTITLE AS WELL.
	FIN
*
*
*  S P A C E :
*
*        Handles SPACE / SPC commands.  It skips <EXP> lines, or
*        to bottom-of-form:  whichever comes first.
*        If <EXP> is zero, then we skip 1 line.
*
*  E J E C T
*
*        Functionally equivalent to PAGE command, but for use
*        by the innards of the assembler.
	IF	TITLES=1
*
* T I T L E :
*
*	LIKE PAGE, BUT SETS THE TITLE FIELD OF THE PAGE HEADING.
*
	FIN
* N A M E :
*	EXACTLY LIKE TITLE BUT PUTS THE STRING IN THE NAME BUFFER.
*
*
* FORMAT OF COMMANDS WITH TITLES POSSIBLE:
*
*	NAME/PAGE/TITLE     (;)(TITLE DESIRED)
*
* IF A ";" IS PRESENT AS THE FIRST CHAR,
*   IT IS TURNED INTO A SPACE .
*
*
	IF 	TITLES=1
TITLE:	EQU	*
	LDX	#TTLBUF	GET ADDRESS OF BUFFER,
	BRA	PAGE0	HANDLE LIKE PAGE
NAME:	EQU	*
	LDX	#NAMEBF	GET NAME-BUFFER ADDRESS
	LDAA	#NAMESZ-1	AND SIZE;
	BRA	PAGE0A	B/ NOW HANDLE LIKE PAGE, TITLE -- BUT DON'T EJECT!!
	FIN
SPACE:   EQU      *
         JSR      EVALCT            EVALUATE THE AF.
*        JSR      ISCTR             MAKE SURE ITS LEGAL
         LDX      VAL
         BGT      SPC1              VAL > 0 ==> GO DO IT.
         LDAA     #1                ELSE SKIP 1
         BRA      SKIP
*
SPC1     LDAA     VAL+1             GET SKIP COUNT,
         LDAB     VAL               TEST H(VAL)
         BEQ      SKIP              B/ IT'S OK:  SKIP N
         LDAA     #$FF              ELSE MAKE BIG SKIP COUNT
*
SKIP     EQU      *                 ACCA = # OF LINES TO SKIP
         CMPA     LLTP              ANY LEFT ON THIS PAGE?
         BLS      SKIP1             B/ YES: GO LOOP THRU TO BOF
	BRA	EJECT
*
PAGE:    EQU      *
*
         DO       TITLES=1
	LDX	#SBTTBF	COPY INTO SUBTITLE BUFFER.
PAGE0	LDAA	#TTLMAX-1	GET COUNT,
	BSR	PAGE0A	YUP, IT'S OLD T.M. CHEAT TIME AGAIN.
	BRA	EJECT	DON'T YOU LOVE IT?

PAGE0A	STX	TPTR	SAVE BUFFER ADDRESS,
	STAA	VAL	AND LENGTH.
	LDX	AFPTR	COPY NON-BLANK PART INTO SBTTL:
         STX      EXPTR             SETUP POINTER,
         JSR      GETXC             GET A CH,
         CMPA     #$D               <EOL> ?
         BEQ      PAGE2             B/ YES:  DONT CHANGE SBTTL.
	CMPA	#$3B	; <SEMICOLON> ?   (ZZZZ changed <#';> to <$3B> to not confuse M680C)
	BNE	PAGE0B	  B/ NO; GO ON.
	LDAA	#BLANK	  ELSE CHANGE LEADING SEMICOLON TO
			; A BLANK; THIS ALLOWS ONE TO CLEAR HEADINGS BY
			; SAYING "PAGE ;<EOL>".  NOTE ALSO THAT ONE MAY
			; STILL START A TITLE W/ ";" BY SAYING 
			; " TITLE  ;;TEXT BEGINNING W/ SEMI;"  THE TITLE WIL
			; THEN BE ";TEXT BEGINNING W/ SEMI;".
PAGE0B	EQU	*
* MAKE FIRST TITLE/SUBTITLE/NAME RETROACTIVE TO START OF LISTING
*
	LDX	TPTR	GLOM ONTO BUFFER ADDR.
         LDAB     0,X	ANYTHING IN BUFFER YET?
         BEQ      PAGE1             B/ NO:  ALWAYS COPY IN.
         LDAB     PASS              SOMETHING THERE, & THIS IS PASS 1?
         BEQ      PAGE2             B/ YES:  DON'T COPY.
*
PAGE1    CLRB                       ZERO COUNT,
	LDX	TPTR	GET BUFFER ADDR AGAIN.
*
PAGEL    ORAA     #$80              MAKE SURE PRINTED LITERALLY.
         STAA     0,X               STUFF AWAY.
         INX
         INCB                       BUMP COUNT.
         STX      TPTR              AND SAVE POINTER.
         CMPB     VAL	AT END OF BUFFER?
         BEQ      PAGE11            B/ YES: ALL DONE.
         JSR      GETXC             GET A CHAR,
         LDX      TPTR              GET POINTER BACK,
         CMPA     #$D               <EOL> ?
         BNE      PAGEL             B/ NO: GO ON.
*
PAGE11   CLR      0,X               NULL AT END OF BUFFER.
*
PAGE2    EQU      *
	RTS		GO HOME.
         FIN
*
EJECT    EQU      *
	CLR	LLTP	Force no line left on page
	RTS

*
SKIP1    EQU      *
         LDAB     LIST:P
         BITB     #LL:LIST          IS LISTING ON?
         BEQ      SKIPR             B/ NO:  DONT MOVE THE CARRIAGE
*
SKIP2    PSHA
         JSR      CRLF              PUMP OUT A CRLF
         PULA
         DECA
         BGT      SKIP2             B/ GO DO ANOTHER.
SKIPR    RTS
	PAGE	- - - I N C L U D E : - - -
******************************************************************************
*
*  INCLUDE:
*
*	PROCESSES THE INCLUDE COMMAND.  SYNTAX IS:
*
*		INCLUDE   <FILENAME>
*
*	WHERE <FILENAME> MUST BE BOTH A VALID FILENAME *AND* A VALID ASSEMBLER
*	SYMBOL.  IF YOU HAVE AN INCLUDE FILE THAT WANTS TO BEGIN W/ A $,
*	ENTER:
*
*		INCLUDE   DISK:$WHATEVERITIS
*
******************************************************************************

INCLUDE: EQU	*
	LDX	RPTF	FIRST, VERIFY THAT WE'RE NOT BEING RPT'D.
	BLE	INCLUDE1	  B/ NO PROBLEM.
	DEX		IF > 0, RPTF MUST BE 1.
	BEQ	INCLUDE1	  B/ THAT'S WHAT IT IS.

**** ERROR:  ATTEMPT TO RPT AN INCLUDE ****
	LDX	#-1	FORCE RPTF TO (OFF).
	STX	RPTF	  ....
	BSR	INCLUDE:NSTER	  ALSO, REPORT NESTING ERROR.

**** VALIDATE SYNTAX OF COMMAND & GET FILENAME IN SYM:BLK ****
INCLUDE1 EQU	*
	JSR	GNOBJ	GET FIRST OBJECT OF AF.
	CMPA	#O:SYM	  IS IT A SYMBOL?
	BNE	INCLUDE2	  B/ NO: REPORT AN ERROR & ABORT.
	JSR	CKEOF	CHECK NEXT OBJECT; BETTER BE TERMINATOR.
	BEQ	INCLUDE3	  B/ SURE 'NUFF.  SYNTAX IS GOOD.
	PAGE

INCLUDE2 EQU	*
	JMP	SYNTAX	BAD COMMAND SYNTAX:  GET OUT OF HERE!

**** CHECK THAT NEITHER (1) THE NESTING LEVEL, NOR (2) THE INCLUDE COUNTER
**** WOULD BE OVERFLOWED BY THIS INCLUDE.  IF SO, QUIT NOW W/ "NESTING ERR".
INCLUDE3 EQU	*
	LDAA	INCLVL	INCLUDE FILE NESTING COUNTER KEEPS
	CMPA	#MAXINCLVL	  US FROM LOOPING FOREVER ON RECURSIVE INCLUDES.
	BCC	INCLUDE:NSTER	  B/ WOW! WE ARE REALLY UP TO OUR NECKS.
	LDAA	INCLNUM	ALSO, CHECK THE INCLUDE FILE NUMBER, AS
	CMPA	#MAXINCLNUM	  THIS NUMBER IS IMPORTANT FOR PASS SYNCH.
	BCS	INCLUDE4	  B/ SMALL ENOUGH; WE CAN DO IT.

INCLUDE:NSTER EQU *
	JMP	NSTER	SOME INCLUDE NESTING LIMIT EXCEEDED.

**** GET POINTER TO AN INCBL (INCLUDE-BLOCK) THAT WILL BE FOR THIS STMT ****
INCLUDE4 EQU	*
	LDX	#SYM:BLK+S:TEXT	LOAD ADDRESS OF BEGINNING OF NAME.
	LDAB	SYM:BLK+S:LEN	  LOAD LENGTH OF NAME.
	JSR	MAKEINCLB	  MAKE OR FIND THE APPROPRIATE BLOCK.
	STX	SYMPT	  SAVE POINTER IN SYMPT, TEMPORARILY;
	BEQ	INCLUDE%	  B/ IT'S NIL:  NO ROOM, CAN'T DO INCLUDE.

**** SAVE CURRENT INPUT FILE'S STATUS IN CURRENT INCLB ****
	JSR	INCLSAVE

**** MAKE NEW BLOCK POINT TO BLOCK HOLDING STATUS ****
	LDAA	.CURINCLB	...BY STORING A POINTER
	LDAB	.CURINCLB+1	  IN THE APPROPRIATE PLACE.
	LDX	SYMPT	LOAD POINTER;
	STAA	INCLB:PREVB,X	  SAVE HIGH BYTE;
	STAB	INCLB:PREVB+1,X	  LOW BYTE.

**** TRANSFER CONTROL TO THE NEW INCLB ****
	JSR	INCLCOPYOUT	COPY OUT THE STATUS & SET .CURINCLB
	PAGE
**** OPEN THE FILE SPECIFIED IN .CURINCLB ****
OPENCURINCLB EQU	*
	LDAB	#SI	START BY CLOSING THE SOURCE CHANNEL.
	JSR	CLOSE$
	LDX	#INCLNOTFOUND	SETUP FOR ERROR ON OPEN
	STX	IOERRLOC
	STS	FLAMOUT
	LDX	.CURINCLB	NOW; LOAD LENGTH OF NAME (USING PTR)
	LDAA	INCLB:LEN,X	  INTO ACCA.
	JSR	INCLB@NAME	LOAD ADDRESS OF TEXT FIELD INTO X.
	LDAB	#SI	LOAD CHANNEL NUMBER;
	JSR	OPEN$	  AND OPEN THE CHANNEL.
INCLERRESET
	LDX	#CRAPOUT$	Reset I/O package error vector
	STX	IOERRLOC
**** LIST THE LINE, NOW *****
	JSR	LIST
	RTS
	PAGE	- - - C O M P U T E   . C U R I N C L   A D D R S - - -
******************************************************************************
*
* NAME:  INCL@NAME()
*
* FUNCTION:
*	SETS X TO (.CURINCL)+INCLB:TEXT
*
*
* NAME:  AB.CURINCL(B: BYTE.OFFSET)
*
* FUNCTION:
*	SETS X TO (.CURINCL)+(B)
*
*
* NAME:  TABX(AB: WORD)	RETURNS X: WORD
*
* FUNCTION:
*	SETS X TO (AB).
*
******************************************************************************


**** LOAD (.CURINCLB)+#INCLB:TEXT INTO X ****
INCLB@NAME EQU	*
	LDAB	#INCLB:TEXT

**** LOAD (.CURINCLB)+(B) INTO X.  PRESERVES A ****
AB.CURINCLB EQU	*
	ADDB	.CURINCLB+1	COMPUTE LOW ORDER BYTE,
	PSHB		  SAVE,
	LDAB	#0	COMPUTE HIGH ORDER BYTE,
	ADCB	.CURINCLB	  LIKE THIS;
	PSHB		  SAVE.
	BRA	TABXX

	PAGE
**** TRANSFER AB TO X ****
TABX	EQU	*
	PSHB
	PSHA
TABXX	PULX
INCLUDE%	RTS
	PAGE	- - - M A K E I N C L B L O C K - - -
******************************************************************************
*
* NAME:  MAKEINCLB		TPTR points to NAME, B=NAME size
*
* FUNCTION:
*	RETURNS A POINTER TO AN INCLB THAT CONTAINS THIS INCLUDE FILE'S
*	NAME.  THIS INCLB IS ALSO PROPERLY INITIALIZED.  INCLVL & INCLNUM
*	ARE BOTH INCREMENTED.
*
******************************************************************************

MAKEINCLBABORT	EQU	*
	DEC	INCLNUM	BACKUP COUNTERS
	DEC	INCLVL
	LDX	#0	AND FLAG NO SPACE
	RTS

INCLRLNMS1 JMP	INCLRLNMS

MAKEINCLB EQU	*
	INC	INCLNUM	ONE MORE INCLUDE FILE FOR THE ROAD....
	INC	INCLVL	  ONE LEVEL DEEPER IN SHIT.
	JSR	FINDINCLB	TRY TO FIND A BLOCK BUILT LAST PASS....
	BNE	INCLRLNMS1	  B/ YUPPERS.  CLEAR OUT ITS LNUMS &
*			  PREPARE IT FOR REUSE.

**** NEED TO CREATE AN INCLUDE-BLOCK ****
	STX	TPTR	SAVE NAME POINTER.
	ADDB	#INCLB:SIZE:	  ADD IN OVERHEAD FOR HAPPY ASSEMBLERS.
	PSHB		  SAVE FOR HAPPY TERRY.
	JSR	GTBLK	GET (B) BYTES OF MEMORY.
	PULB		  RESTORE: IT'S HAPPY HOUR.
	BEQ	MAKEINCLBABORT	  B/ NO MEMORY -- RETURN NULL POINTER.

**** NOW HAVE MEMORY.  STPTR POINTS TO FIRST BYTE (AS DOES X) ****
	LDAA	#INCLB:SIZE:	INITIALIZE COUNTER.
MAKEINCLB1 EQU	*	LOOP POINT:
	CLR	0,X	  ZERO THE BLOCK.
	INX
	DECA
	BNE	MAKEINCLB1	  B/ NOT DONE YET.

**** NOW, COPY FILENAME TO BLOCK ****
	SUBB	#INCLB:SIZE:	TAKE OUT OVERHEAD BYTES;
	LDX	STPTR	GET POINTER TO INCLB
	STAB	INCLB:LEN,X	  AND SAVE COUNT.

MAKEINCLB2 EQU	*
	STX	TPTR1	SAVE DEST. POINTER.
	LDX	TPTR	LOAD SOURCE POINTER;
	LDAA	0,X	  LOAD NEXT CHARACTER.
	INX		  BUMP POINTER.
	STX	TPTR	  SAVE POINTER.
	LDX	TPTR1	LOAD DEST. POINTER;
	STAA	INCLB:TEXT,X	  SAVE NEXT CHARACTER.
	INX		  BUMP POINTER.
	DECB		DECREMENT COUNT;
	BNE	MAKEINCLB2	  B/ NOT DONE YET: LOOP.

**** SET UP A CUPPLE OTHER HANDY THINGS ****
	LDX	STPTR	GET POINTER TO BASE OF INCLB
	LDAA	INCLNUM	  AND POKE IN THE INCLUDE FILE NUMBER.
	STAA	INCLB:NUM,X		....

**** LINK INTO MASTER INCLBLK CHAIN ****
	LDX	INCLTAIL	GET POINTER TO LAST;
	BNE	MAKEINCLB3	  B/ CHAIN ISN'T EMPTY.
	LDX	STPTR	ELSE, INITIALIZE TAIL POINTER/HEAD POINTER.
	STX	INCLTAIL		....
	STX	INCLHEAD		....
MAKEINCLB% RTS

MAKEINCLB3 EQU	*	LINK INTO NON-NUL CHAIN.
	LDAA	STPTR	GET @/ NEW LAST BLOCK;
	LDAB	STPTR+1		....
	STAA	INCLB:NEXT,X	AND MAKE FLINK(TAIL):=@NEWBLOCK.
	STAB	INCLB:NEXT+1,X		....
	STAA	INCLTAIL	UPDATE TAIL POINTER;
	STAB	INCLTAIL+1		....
	LDX	INCLTAIL	LOAD NEW POINTER (ALWAYS <> 0).
	RTS
	PAGE	- - - F I N D I N C L B - - -
******************************************************************************
*
* NAME:  FINDINCLB
*
* FUNCTION:
*	GIVEN A FILENAME @[0,X], A LENGTH IN B AND THE CURRENT INCLUDE
*	FILE NUMBER FOR THIS PASS, ATTEMPTS TO FIND A CORRESPONDING
*	INCLB.  IF FAILURE, RETURNS ZERO IN X.
*
******************************************************************************

FINDINCLB EQU	*
	STX	TPTR	(SAVE ADDRESS)
	LDAA	INCLNUM	LOAD CURRENT INCLUDE-FILE NUMBER.
	LDX	INCLHEAD	GET HEAD OF INCLUDEBLOCK CHAIN.
	BEQ	FINDINCLB%	  B/ NO COULD FINDEM.

**** LOOP THROUGH CHAIN UNTIL WE FIND A FILE NUMBER THAT MATCHES ****
FINDINCLB1 EQU	*
	CMPA	INCLB:NUM,X	DOES THIS MATCH?
	BEQ	FINDINCLB2	  B/ YES!!
	LDX	INCLB:NEXT,X	NO: TRY NEXT.
	BNE	FINDINCLB1	  B/ THERE'S ANOTHER TO CHECK.
FINDINCLB% EQU	*
	LDX	TPTR	RESTORE POINTER;
	CLRA		  INDICATE FAILURE.
	RTS

**** FOUND A MATCH ****
FINDINCLB2 EQU	*
	LDAA	#1	SET NOT-ZERO,
	RTS		  AND RETURN OUR POINTER.
	PAGE	- - - I N C L S A V E - - -
******************************************************************************
*
* NAME:  INCLSAVE
*
* FUNCTION:
*	SAVE STATUS OF CURRENT SI FILE IN CURRENT INCLUDE BLOCK FOR
*	LATER RECOVERY.
*
******************************************************************************

INCLSAVE EQU	*
	LDAB	#INCLB:FPOS	CONSTRUCT POINTER TO FPOS BUFFER;
	JSR	AB.CURINCLB	  WILL COME BACK IN X.
	LDAB	#SI	READ POSITION OF SI CHANNEL.
	JSR	GETPOS$	  ....

**** COPY LINE NUMBER/FORM NUMBER ****
	LDX	.CURINCLB	GET POINTER TO INCLB;
	LDAA	FPGE	COPY FORM #.
	LDAB	FPGE+1		....
	STAA	INCLB:FPGE,X		....
	STAB	INCLB:FPGE+1,X		....
	LDAA	LINEN	COPY LINE #.
	LDAB	LINEN+1		....
	STAA	INCLB:LINEN,X		....
	STAB	INCLB:LINEN+1,X		....
	RTS
	PAGE	- - - P O P I N C L - - -
******************************************************************************
*
* NAME:  POPINCL()
*
* FUNCTION:
*	FINISHES PROCESSING CURRENT INCLUDE FILE, AND RESTORES INCLUDE
*	FILE FROM LOWER LEVEL.  YUP, IT REPOSITIONS AND EVERYTHING.
*
******************************************************************************

INCLNOTFOUND	EQU	*
	JSR	FILNOTF
	JSR	INCLERRESET	Reset I/O error vector

POPINCL	EQU	*
	DEC	INCLVL	BACK UP BY ONE;
	LDX	.CURINCLB	LOAD CURRENT INCLB;
	LDX	INCLB:PREVB,X	  FETCH POINTER TO WHOEVER CALLED IT;
	JSR	INCLCOPYOUT	  RESTORE THAT GUY AS CURRENT INCLUDE.

**** CLOSE PREVIOUS AND OPEN CURRENT ****
	JSR	OPENCURINCLB	THAT DOES THE OPEN.

**** RESET THE POSITION ****
	LDAB	#INCLB:FPOS	GET @/ SAVED POSITION;
	JSR	AB.CURINCLB	  (IN X);
	LDAB	#SI	SET CHANNEL NUMBER;
	JMP	POSITION$	  SET POSITION & SPLIT.
	PAGE	- - - I N C L R N M S - - -
******************************************************************************
*
* NAME:  INCLRNMS
*
* FUNCTION:
*	CLEARS THE FPGE/LINEN FIELDS OF THE INCLB WHOSE ADDRESS IS PASSED
*	IN X.
*
* CHANGES:		PRESERVES:
*	C			A B X
*
******************************************************************************

INCLRLNMS EQU	*
	CLR	INCLB:FPGE,X
	CLR	INCLB:FPGE+1,X
	INC	INCLB:FPGE+1,X	(SET FPGE TO 1)
	CLR	INCLB:LINEN,X
	CLR	INCLB:LINEN+1,X
	RTS
	PAGE	- - - I N C L W A T C H O U T - - -
******************************************************************************
*
* NAME:  INCLWATCHOUT()
*
* FUNCTION:
*	USED AS A GATE MONSTER ON ROUTINES THAT SHOULD NOT BE EXECUTED
*	UNLESS WE ARE DEALING WITH LEVEL 0 OF INCLUDES.
*
******************************************************************************

INCLWATCHOUT EQU	*
	TST	INCLVL	ARE WE AT LEVEL 0?
	BEQ	INCLWATCHOUT%	  B/ YES.  RETURN TO CALLER.
	LEAS	2,S	ELSE POP RETURN ADDRESS;
INCLWATCHOUT% EQU *		....
	RTS		  IN ANY EVENT, GO HOME.
	PAGE	- - - I N C L C O P Y O U T - - -
******************************************************************************
*
* NAME:  INCLCOPYOUT(X)
*
* FUNCTION:
*	X IS A POINTER TO AN INCLB THAT IS TO BE MADE THE CURRENT
*	INCLB.  WE STORE IT INTO .CURINCLB; THEN COPY OUT THE VARIOUS
*	LINE-# RELATED INFO INTO MAIN STORE.
*
******************************************************************************

INCLCOPYOUT EQU	*
	STX	.CURINCLB	SAVE POINTER.
	LDAA	INCLB:FPGE,X	COPY SOME CONTEXT:
	LDAB	INCLB:FPGE+1,X		....
	LDX	INCLB:LINEN,X		....
	STAA	FPGE		....
	STAB	FPGE+1		....
	STX	LINEN		....
	LDX	.CURINCLB	RESTORE POINTER;
	RTS		  SPLIT.
	PAGE	- - - D O ,   I F - - -
         DO       CONDASM=1
*
*
*  D O :
*
*        Handles DO psuedo-op:  right now is an IF, w/ error check.
*
DO:      JSR      EVALCT            GET CONTROL VALUE,
         LDAA     VAL
         BLT      IF:1
	BNE	DO:1	B/  FAR TOO BIG.
         LDAA     VAL+1
         CMPA     #1
         BLS      IF:1
DO:1	JSR      ILLVAL
         BRA      IF:1
*
IF:      JSR      EVALCT
IF:1     LDX      VAL
         BGT      IF:T
*
* AN IF THAT FAILED:
*    SET SKPFLG = 1, SKPLVL = 0
*
IFAIL	CLRA
         STAA     SKPLVL
         INCA
         STAA     SKPFLG            SKIP TO FIRST ELSE.
IF:X     JMP      PUTV              OUTPUT VALUE AND LEAVE.
*
* AN IF THAT SUCCEEDED:
*        INCREMENT DOLVL.
*
IF:T     EQU      IF:X
         FIN
	PAGE	- - - I F U N D : - - -
	DO	CONDASM=1	MUST HAVE OTHER STUFF TO GET THIS
*
*
* I F U N D :
*
*	IF VARIABLE IS UNDEFINED OR A FWD REF - THEN COND=TRUE
*
*
IFUND:	CLR	VAL	CLEAR VALUE (FALSE COND)
	CLR	VAL+1
	JSR	GNOBJ	GET THE SYMBOL
	CMPA	#O:SYM	IS IT?
	BNE	IFUERR	IF NOT, ERR
	JSR	CKEOF	IS FIELD SYMBOL ONLY?
	BNE	IFUERR	  B/ NOPE.  NO CAN DO.

*
* SYM TO BE TESTED IS IN SYM
*
	JSR	FSYM	WHERE ARE YOU?
	BNE	NDEFD	IF NOT THERE - SUCCESS - SET TRUE
	LDAA	S:TYPE,X	GET TYPE
	BITA	#T:UNDF!T:FREF	IS IT UNDEF OR FREF?
	BEQ	IFAIL	IT'S DEFINED (DARN) FAIL!!!
*
* SYM IS UNDEF - SET VAL=1 AND SUCCEED
*
NDEFD	INC	VAL+1	SAY "TRUE"
	BRA	IF:T	GOTO HANDLE TRUE
*
IFUERR	JSR	ILLAF	SCREAM
	BRA	IFAIL	FAIL THE TEST
	FIN
*
	PAGE	- - - I F U N D : - - -
	DO	CONDASM=1	MUST HAVE OTHER STUFF TO GET THIS
*
*
* I F D E F :
*
*	IF VARIABLE IS DEFINED OR NOT A FWD REF - THEN COND=TRUE
*
*
IFDEF:	LDX	#0	SET VALUE (TRUE COND)
	STX	VAL
	JSR	GNOBJ	GET THE SYMBOL
	CMPA	#O:SYM	IS IT?
	BNE	IFUERR	IF NOT, ERR
	JSR	CKEOF	IS FIELD SYMBOL ONLY?
	BNE	IFUERR	  B/ NOPE.  NO CAN DO.

*
* SYM TO BE TESTED IS IN SYM
*
	JSR	FSYM	WHERE ARE YOU?
	BNE	ISDEFD	IF NOT THERE - FAILURE - SET FALSE
	LDAA	S:TYPE,X	GET TYPE
	BITA	#T:UNDF!T:FREF	IS IT UNDEF OR FREF?
	BNE	IFAIL	IT'S NOT DEFINED (DARN) FAIL!!!
*
* SYM IS DEF - SET VAL=1 AND SUCCEED
*
ISDEFD	INC	VAL+1	SAY "TRUE"
	BRA	IF:T	GOTO HANDLE TRUE
	FIN
         PAGE     - - - E L S E : - - -
         DO       CONDASM=1
*
*
*   E L S E :
*
*        Handles direct execution of ELSE directive.  If DOLVL
*        is zero, then this is an error.  Otherwise, begin skipping
*        to FIN, w/ SKPLVL = 0.
*
*
*
*   E L S I F :
*
*        Handles direct execution of ELSEIF directive.  Just like
*        ELSE:, but needs different address
*
ELSIF:   NOP
ELSE:    EQU      *
         TST      DOLVL
         BEQ      ELSE:1
ELSE:0   CLRA
	STAA	VAL	(Clear high-order byte of listing Val)
         STAA     SKPLVL	Start skipping.
         DECA		  Set A to -1,
         STAA     SKPFLG	  sez:  skip to a FIN.
	LDAA	DOLVL	Display the nesting level on the listing:
	STAA	VAL+1	  poke it into the low order byte,
	BRA	IF:X	  and hop to someone who JMPs to PUTV.

*
ELSE:1   JMP      NSTER             *E* ELSE W/O DO.
         FIN
         PAGE     - - - F I N - - -
         DO       CONDASM=1
*
*
*  F I N :
*
*        Handles direct execution of FIN directive.  If DOLVL
*        is zero, then this is an error.  Otherwise, DOLVL is
*        decremented.
*
FIN:     EQU      *
         LDAA     DOLVL
         BEQ      ELSE:1            B/ NESTING ERROR.
         DECA
         STAA     DOLVL
         RTS
         FIN
         PAGE     - - - C A S E : - - -
         DO       CONDASM=1
*
*
*   C A S E :
*
*        Handles direct execution of CASE directive.  If
*        value of AF(1) < 0, then skip to FIN;
*                       = 0, then inc DOLVL, continue execution.
*                       > 0, then skip to n-th ELSE
*
CASE:    EQU      *
         JSR      EVALCT
         LDX      VAL
         BEQ      IF:T
         BSR      IF:X              LIST VALUE,
         LDX      VAL
         BLT      ELSE:0            ENTER SKIP MODE.
*  ELSE SKIP TO N-TH
         LDAA     VAL+1
         BPL      CASEOK
         JSR      ILLVAL            *E* ILLEGAL VALUE ON CASE:  TOO BIG
         BRA      ELSE:0
CASEOK   STAA     SKPFLG
         CLR      SKPLVL
         RTS
         FIN
	PAGE	- - - O P T  (HANDLES "WITH" COMMAND)  - - -
*
* OPT:
*	ACCEPTS OPTIONS AND SETS THE CORRECT BITS/ VALUES.
*
*
* LEGAL OPTIONS: (SEE OPTTABLE BELOW)
*
*
OPT:	JSR	GNOBJ	GET THE OPTION
	CMPA	#O:TERM	DONE?
	BNE	CONT	IF NOT, EAT THIS OPT
	CMPB	#ASCII:COMMA	A COMMA?
	BEQ	OPT:	IF SO, GET THE OPTION FOLLOWING
*
* A TERM SEEN - FINISH UP
*
	RTS		GO HOME
*
CONT	CMPA	#O:SYM	A SYMBOL?
	BEQ	OSEARCH	OF SO, SEE IF IT IS A VALID OPTION
OPTERR	JSR	ILLAF	YELL
	BRA	OPT:	TRY TO GET THE REST OF THE OPTIONS
*
OSEARCH	LDX	#OPTTABLE	GET ADDR OF OPTION TABLE
	JSR	FSYM1	GO FIND OPTION
	BNE	OPTERR	ERR IF NOT LEGAL OPTION
	LDAA	S:TYPE,X	GET THE VALUE TO PASS
	LDX	S:VAL,X	GET THE ADDR OF THE HANDLING ROUTINE
	JSR	0,X	GO DO WORK FOR OPTION
	BRA	OPT:	TRY FOR NEXT OPT
	PAGE
*
* NUMSET - SCAN FOR =<EXP> AND SET THE PAGE ZERO LOC
*	WHOSE ADDR IS IN A TO THE VALUE OF THE EXPRESSION.
*	ERROR IF VALUE HAS UPPER 8 BITS # 0.
*	  --OR--  IF VALUE IS << CONTENTS OF ACCB.
*
* WIDSET	-- SETS ACCB TO 30 (THE MINIMUM WIDTH) AND GOES TO NUMSET.
*
DEPSET	CLRB
	BSR	NUMSET	Get Value setting
	BEQ	DEPRTS	B/Zero OK (means no pages setting)
	LDAB	#MINDEP	Minimum depth
	CBA		Enough depth?
	BCS	NUMERR	B/No, we're not in over our heads
DEPRTS	RTS

WIDSET	EQU	*	COME HERE TO SET WIDTH....
	LDAB	#30	LOAD MINIMUM VALUE,

NUMSET	PSHB		SAVE LOW LIMIT.
	PSHA		SAVE ADDR TO POKE
*
* GET THE VALUE
*
	JSR	GNOBJ	GET THE NEXT OBJ
	DO	CONDASM=1	IF RELATIONAL OPS
	CMPA	#O:OP	AN OP?
	BNE	OPTVF	IF NOT, NO VALUE SPECD
	CMPB	#OP:EQ	EQUALS?
	BEQ	OPTV2	IF SO, GET THE <EXP> FOLLOWING
	ELSE
	CMPA	#O:UNKN	UNKNOWN?
	BNE	OPTVF	IF NOT, NO EQUALS - NO <EXP>
	CMPB	#'=	WELL?
	BEQ	OPTV2	IF SEEN = GO SCAN <EXP>
	FIN
OPTVF	INC	RJOBJ	I DIDN'T SEE THAT!
	LEAS	2,S	BALANCE YE STACK...
	RTS
*
OPTV2	JSR	EVAL	GET THE EXPRESSION
*
* GOT NUMBER IN VAL
*
	PULA		LOAD ADDRESS....
	STAA	TPTR+1		AND SAVE IN POINTER.
	PULB		LOAD LOW LIMIT.
	LDAA	VAL	IS THIS ZERO?
	BNE	NUMERR	IF NOT, ILLEGAL VALUE
	CLR	TPTR	(CLEAR HIGH-ORDER ADDRESS.)
	LDAA	VAL+1	GET LOW ORDER VALUE.
	CBA		IS IT BIG ENUFF?
	BCS	NUMERR		B/ NO WAY.
	LDX	TPTR	GET THE POINTER
	STAA	0,X	SAVE THE VALUE
	RTS
NUMERR	JMP	ILLVAL	ILLEGAL VALUE!
	PAGE
*******************************************************************
*
*    OPTION  TABLE
*
*******************************************************************

OPTTABLE	FDB	OPT1	LIST HEAD POINTER

OPT1	FDB	OPT2,OPTOR
	FCB	OPT:MCM,3
	FCC	/MCM/
OPT2	FDB	OPT3,OPTAND
	FCB	\OPT:MCM,4
	FCC	/NMCM/
OPT3	FDB	OPT4,OPTOR
	FCB	OPT:LN,2
	FCC	/LN/
OPT4	FDB	OPT5,OPTAND
	FCB	\OPT:LN,3
	FCC	/NLN/
OPT5	FDB	OPT6,OPTOR
	FCB	OPT:LF,2
	FCC	/LF/
OPT6	FDB	OPT7,OPTAND
	FCB	\OPT:LF,3
	FCC	/NLF/
OPT7	FDB	OPT8,OPTOR
	FCB	OPT:DMP,3
	FCC	/DMP/
OPT8	FDB	OPT9,OPTAND
	FCB	\OPT:DMP,4
	FCC	/NDMP/
OPT9	FDB	OPT10,OPTOR
	FCB	OPT:EL,2
	FCC	/EL/
OPT10	FDB	OPT11,OPTAND
	FCB	$FF&\OPT:EL,3
	FCC	/NEL/
OPT11	FDB	OPT12,WIDSET
	FCB	WIDTH,2
	FCC	/WI/
OPT12	FDB	OPT13,DEPSET
	FCB	DEPTH,2
	FCC	/DE/
OPT13	FDB	OPT14,OPTOR
	FCB	OPT:LST,3
	FCC	/LST/
OPT14	FDB	OPT15,OPTAND
	FCB	\OPT:LST,4
	FCC	/NLST/
OPT15	FDB	OPT16,OPT1OR
	FCB	OPT1:DO,2
	FCC	/DO/

OPT16	FDB	OPT17,OPT1AND
	FCB	\OPT1:DO,3
	FCC	/NDO/

OPT17	FDB	OPT18,OPT1OR
	FCB	OPT1:6801,5
	FCC	/M6801/
*
OPT18	FDB	OPT19,OPT1AND
	FCB	\OPT1:6801,6
	FCC	/NM6801/
*
OPT19	EQU	0
*
         PAGE     - - - E N D : - - -
*
*
*   E N D :
*
*        Handles good stuff for end-of-file
*
END:     EQU      *
         LDX      RPTF	Check Repeat count
         BLT      END:0	B/None
         JSR      NSTER	Nesting error
	LDX	#-1	Force RPTF to proper value
	STX	RPTF
*
END:0	LDAA	DOLVL	Check DO level
	BEQ	END:01	B/Ok
	JSR	NSTER	Nesting error
	CLR	DOLVL	Fix DOLVL
END:01
	BSR	SACHECK	Go check for a start address expression
	LDAA	INCLVL	Where are we in the world of includes?
	BEQ	END:00	  B/ At the bottom!  A real end.
	CLR	EOFF	We have processed the EOF.
	JMP	POPINCL	  And rewind to previous incarnation.

END:00   LDAA     PASS
         BEQ      END:1
	PAGE
*
*
*  END OF PASS 2
*
*
         JSR      LIST
         JSR      PNCEOF
	LDAA	LIST:P	TURN ON LISTING
	ORAA	#LL:LIST	SO THAT EJECT WILL WORK
	STAA	LIST:P
	JSR	DUMPTBL	DUMP THE SYMBOLS
	BSR	SUMM	DUMP THE ERROR LINES/COUNT
	LDAA	PAGEN+1
	BITA	#1	Odd number of output pages?
	BEQ	ENDALL	B/No
	LDAA	#$0C	FF
	JSR	LSTC	Final form
ENDALL	JSR	EXIT	GET OUT
*
END:1	LDS	STACK$	WIND BACK STACK
         JSR      P2INIT
         JSR      PNCSET            SET UP BINARY FOR OUTPUT
	JSR	ZAPCHN	TURN ALL SYMS INTO FORWARD REFS.
         JMP      LOOP1             AND GO DO PASS 2

	PAGE
SACHECK	LDAA	PASS	Pass 2 ?
	BEQ	SACHECKRTS	B/No
	LDAA	EOFF	End of input file ?
	BNE	SACHECK2	B/Yes
	JSR	EVNUL	Eat a start address (?)
	BCS	SACHECKRTS	B/No start address expression
	TST	SADDRFLG	;Any previous start address ?
	BEQ	SACHECK1	;B/No
	LDX	VAL	Zero start address ?
	BEQ	SABOMB	B/Yes, error
	CPX	SADDR	= Previous start address
	BNE	SABOMB	;B/No, this is an error
	BRA	SACHECKRTS	;Yes, ok

SACHECK1 LDX	VAL	;Val = 0?
	BEQ	SABOMB	;B/Yes, error
	STX	SADDR	;No, save start address
	COM	SADDRFLG	Flag SADDR set
	BRA	SACHECKRTS

SACHECK2 LDX	SADDR	;Force value = Start address
	STX	VAL

SACHECKRTS
	RTS

SABOMB	JSR	SAERR	Mark error
	RTS


SADDR	FDB	0	Start address
SADDRFLG	FCB	0	Set flag
	PAGE	- - - S U M M A R Y - - -
*
* SUMM
*	CALLED AT END OF PASS 2 TO OUTPUT MISCELLANEOUS
*	SUMMARY INFORMATION. PRINTS OUT # OF ERRORS, ERROR LINES
*
SUMM     EQU      *
	CLR	SBTTBF	Reset sub-title buffer
	INC	DOCOPY	COPY ALL THIS STUFF TO CONS
	IF	ERRLINES=1
	BSR	PRTLNS	PRINT THE ERROR LINES
	FIN
	JSR	CRLF	SPACE BETWEEN THIS AND THE NUMBER OF ERRORS
         JSR      CRLF
	LDX	#STARS	PUT OUT SOME STARS,
	JSR	PTTL	NOT TOO HARD.
         LDAB     ERRC
	BEQ	SUMM1	B/NO ERRORS
	CMPB	#1	ONE ERROR ONLY?
	BEQ	SUMMONE	  B/ THEN TELL HIM GRAMMATICALLY.
	CMPB	#$99	HIT MAX?
	BNE	NAT	NO - HOW DULL
	LDX	#ATLEAST	LOAD FUNNY MESSAGE
	JSR	PTTL
NAT	EQU	*
	CLRA
	LDAB	ERRC	LOAD ERR AGAIN IN CASE THAT WE LOST IT
         JSR      PTNL
         LDX      #CERR
SUMM0    JSR      PTTL
	LDAA	#7	GET A BELL
	JSR	PUTC	PRINT IT OUT
	CLR	DOCOPY		DON'T COPY TO CONS ANY MORE
	LDAA	#$0C	;=Form feed
	JMP	TOLO	;Output extra Form
*
SUMM1    LDX      #CERRN
         BRA      SUMM0
*
SUMMONE	LDX	#ONEERR	GRAB A MSG,
	BRA	SUMM0	  AND PUT IT OUT.
*
CERRN    FCC      /No/
CERR     FCC      / Assembly Errors./
         FCB      $D,0
*
ATLEAST	FCC	/At Least /
	FCB	0
*
ONEERR	FCC	/One Assembly Error./
	FCB	$D,0
	PAGE	- - - P R I N T   E R R O R   L I N E S - - -
	IF	ERRLINES=1
*
* GET ERRCHAIN AND PRINT ALL THE GARBAGE ON IT UNTIL WE RUN OUT
*	OF STUFF TO PRINT.
*
PRTLNS	LDAA	OPTF	WANTS ERRORS?
	BITA	#OPT:EL	WANTS ERR LINES?
	BEQ	PRTLNS%	FOOL!
	LDAB	ERRC	SEE IF ANY ERRORS TO LIST
	BEQ	PRTLNS%	IF NOT, DON'T BOTHER
	JSR	EJECT	POOF
	LDX	#ERRHDR	WARN HIM OF WHAT WE ARE GOING TO DO
	JSR	PTTL
	LDX	INCLHEAD	Get head of include-file blocks;
PRTLNS0	STX	SORTAIL	SAVE IN HANDY (UNUSED) PLACE.
	BEQ	PRTLNS%	  B/ LAST INCLUDE BLOCK PROCESSED.
	LDX	INCLB:ERRCHN,X	LOAD POINTER TO THIS FILE'S ERROR CHAIN.
	BEQ	PRTLNS1	  B/ NO ERRORS HERE:  TRY NEXT.
	STX	ERRCHAIN	OK -- SET UP ERRCHAIN,
	JSR	CRLF	  HOSE OUT A CR,
	LDX	SORTAIL	***PRINT OUT FILE NAME***
	LDAB	INCLB:LEN,X	  B:= LEN(NAME).
PRTLNS2	LDAA	INCLB:TEXT,X	  A:= CHARACTER.
	INX
	STX	TPTR	SAVE POINTER FOR A SEC.
	PSHB		 AND COUNTER TOO
	JSR	PUTLO
	PULB
	LDX	TPTR
	DECB		  ARE WE DONE?
	BNE	PRTLNS2	  B/ N.

**** OUTPUT THE ERROR CHAIN ****
	BSR	NERRLN

**** TRY NEXT INCLB ****
PRTLNS1	LDX	SORTAIL	GET @/ CURRENT BLOCK;
	LDX	INCLB:NEXT,X	  GET @/NEXT;
	BNE	PRTLNS0	  B/ DO ANOTHER.
PRTLNS% RTS

**** DUMP ERROR CHAIN ****
NERRLN	JSR	CRLF	SPACE ONCE
NERR	LDX	ERRCHAIN	GET THE NEXT ENTRY TO PRINT
	BEQ	ERFIN	IF NULL - WE DONE
	LDAB	COLNO	GET COLUMN # FOR SPACING,
	PSHB		  AND SAVE FOR LATER.
	LDAA	2,X	LD FPGE OF ERROR
	LDAB	3,X	DITTO
	JSR	PTNLB	SPIT IT OUT
	LDAA	#'-	PUT OUT A DASH
	JSR	LSTC
	LDX	ERRCHAIN	GET PTR BACK
	LDAA	4,X	LOAD LINE # OF ERROR
	LDAB	5,X
	LDX	0,X	GET LINK TO NEXT ERR ENTRY
	STX	ERRCHAIN
	JSR	PTNL	PRT LINE #
	LDAB	COLNO	GET COLUMN #,
	PULA		  GET STARTING COLUMN NUMBER,
	SBA		  AND SEE HOW MANY SPACES TO PRINT.
	ADDA	#10	ALLOWING 10 CHARACTERS PER COLUMN,
	TAB		  B NOW CONTAINS NUMBER OF SPACES TO PRINT BEFORE NEXT.
	CMPB	#10	MAKE IDIOT CHECK,
	BHI	NERR1	  B/ DUMBER THAN I THOUGHT.

NERR0	LDAA	#BLANK	HOSE OUT A BLANK
	JSR	LSTCP	  BUT DONT CHANGE B!
	DECB		  BECAUSE I WANT IT
	BGT	NERR0	SO I KNOW WHEN TO STOP.

NERR1	EQU	*
	LDAA	COLNO	CAN WE FIT ANOTHER?
	ADDA	#10
	CMPA	WIDTH	WELL?
	BLS	NERR	IF SO, DO IT
	BRA	NERRLN	GET NEW LINE OTHERWISE
*
*
ERRHDR	FCC	/Error Lines:/
	FCB	$D,0
*
	FIN
	PAGE	- - - Z A P C H N - - -
*
* ZAPCHN
*	CALLED AT END OF PASS 1 TO TURN ALL SYMBOLS INTO FORWARD
*	REFERENCES.  DOESN'T REALLY DO TOO MUCH.
*
ZAPCHN	EQU	*
	LDAB	#HASH:M+1	GET NUMBER OF BUCKETS,
	LDX	#HSHTBL-S:NEXT	GET BASE POINTER,
ZAPCHN0	STX	TPTR	  AND SAVE FOR LATER.
ZAPCHN1	LDX	S:NEXT,X	GET POINTER TO NEXT BLOCK,
	BEQ	ZAPCHN2	  B/ HIT END OF THIS HASH CHAIN.
	LDAA	S:TYPE,X	SET TYPE TO TYPE!T:FREF.
	BITA	#T:SPEC	Special symbol ?
	BNE	ZAPCHN1	B/Yes
	ORAA 	#T:FREF
	STAA	S:TYPE,X
	BRA	ZAPCHN1	DO THIS FOR ENTIRE CHAIN.
ZAPCHN2	LDX	TPTR	HIT END OF ONE CHAIN, MOVE
	LEAX	2,X	  ON TO THE NEXT.
	DECB		REDUCE LOOP COUNTER,
	BNE	ZAPCHN0	  B/ GO ON IF ANY ARE LEFT.
ERFIN,EDONE RTS		ELSE, ALL DONE.
	PAGE	- - - A L L   P A S S   I N I T - - -
*
*
* PINIT
*	DOES ALL INITIALIZATION REQUIRED AT THE START OF
*	AN ASSEMBLY PASS.  CLEARS PAGE ZERO BETWEEN PVARZ
*	AND PVARE, SETS LISTING FLAGS, INITIALIZES PAGE NUMBER AND
*	PAGE IN FILE.  FORCES RPTF TO -1.  
*
*
PINIT    LDX      #PVARZ
PIN1     CLR      0,X
         INX
         CPX      #PVARE
         BNE      PIN1
*
         LDX      #-1
         STX      RPTF
*
	LDAA	#LLDFLT	SET DEFAULT LISTING FLAGS.  SETS
			;  A DEFAULT OF:
			; PCC 0    (NOT LL:PCC)
			; PSR 1  (LL:SKIP)
			; PCA 1	( LL:PCA)
			; PGEN 0	(NOT LL:PGEN)
			; IF YOU WANT DIFFERENT DEFAULTS, 
			; CHANGE THE CONSTANT BEING LOADED.
         LDAB     OPTF
	COMB
	BITB	#OPT:LO!OPT:LST	ARE BOTH ON?
         BNE      PIN2
         ORAA     #LL:LIST
PIN2	STAA     LIST:P
         LDX      #1                INITIALIZE PAGE
         STX      PAGEN
	DEX		<<X := 0>>
         STX      FPGE              AND PAGE IN FILE.
*
	LDX	#TABTB:	INITIALIZE TABS TO DEFAULT
	STX	TABTBL	SAVE AWAY
*
	LDX	#INFLINK	SET PTR FOR CONS LINES
	STX	INLLINK
         RTS
	PAGE	- - - P A S S    1    I N I T I A L I Z A T I O N - - -
*
* P1INIT
*	DOES ALL INITIALIZATION FOR BEGINNING OF ASSEMBLY.  CLEARS ALL
*	OF PAGE ZERO TO ZERO, CLEARS ALL OF THE DATA AREA (DATBEG TO 
*	DATEND) TO ZERO. GETS AND OF MEMORY AND STORES IT IN STPTR.
*	OTHER INITIAL VALUES FOR PAGE
*	ZERO ARE COPIED IN FROM THE TABLE PZ1I: THROUGH PZ1E:.  THESE VALUES
*	ARE TRANSFERRED TO THE RANGE PZ1I THROUGH PZ1E.  THE BINARY BUFFER POINTER
*	IS THEN INITIALIZED SEPARATELY (FOR REASONS THAT ARE PURELY HISTORICAL); 
*	WE THEN CALL GETOPT TO READ OPTIONS FROM THE USER.
*
P1INIT	LDX	#PZBASE
P1I1	CLR	0,X	ZERO $0 --> $FF
	INX
	CPX	#PZEND
	BNE	P1I1
*
	LDX	#DATBEG	ZERO DATA AREAS
P1I2	CLR	0,X
	INX
	CPX	#DATEND
	BNE	P1I2
*
	LDX	IOPKGBASE	LOAD FIRST UNUSABLE LOCATION
*
	DEX
	STX	STPTR	TBL GROWS DOWN!!!
*
*  COPY IN INITIAL VALUES.
*
	LDX	#PZ1I
	STX	TPTR
	LDX	#PZ1I:
*
P1I5	LDAA	0,X
	INX
	STX	TPTR1
	LDX	TPTR
	STAA	0,X
	INX
	STX	TPTR
	LDX	TPTR1
	CPX	#PZ1E:
	BNE	P1I5
*
	JSR	MCSYMINT	Setup symbols M6800, M6801, M6809
	LDX	#BBST
	STX	BBPTR
	JSR	SETDATIM	GO SET THE DAY AND TIME IN THE BUFF
	DEC	INCLVL	ORIGINAL SOURCE LEVEL IS -1
	DEC	INCLNUM
	JSR	GETOPT
	LDAA	OPTF	DO WE HAVE
	BITA	#OPT:LO	 A LISTING OUTPUT FILE?
	BEQ	P1I7	B/NONE
	LDAB	#LO	LO CHANNEL
	JSR	GETWD$	GET WIDTH AND DEPTH
	TSTA		GOT A WIDTH?
	BEQ	P1I6	B/NO
	STAA	WIDTH	YES - UPDATE
P1I6	TSTB		GOT A DEPTH?
	BEQ	P1I7	B/NO
	STAB	DEPTH	YES - UPDATE
P1I7	EQU	*

**** INITIALIZE BINARY FILE POINTER ****
	LDAA	#5	Rest of filepointer is zero after
	STAA	CURREC+R$FILEBASE+3	initialization.
	RTS

**** Page zero initial values ****
PZ1I:	FDB	IBUFI	INBUF
	FDB	INBUFE:	INBUFE
	FCB	DWIDTH	WIDTH (WIDTH OF LISTING PAGE IN COLUMNS)
	FCB	DDEPTH	DEPTH (FORM DEPTH IN LINES)
	FDB	TABTB:	TABTBL (ADDRESS OF TAB TABLE)
	FDB	LBBEG:	LBBEG (ADDRESS OF LISTING BUFFER)
	FDB	EOLBF:	EOLBF (END OF LISTING BUFFER)
	FCB	OPTDFLT	OPTF (DEFAULT OPTIONS)
	FCB	OPT1DFLT	OPT1F (SECONDARY DEFAULT OPTIONS)
	FDB	ASMEND	STLOW (LOW ADDRESS OF FREE SPACE)
PZ1E:	 EQU	*

	IF	(PZ1E:-PZ1I:)#(PZ1E-PZ1I)
PZ1E:	EQU	PZ1E	MAKE ME AN ERROR, BWANA TERRY.
	FIN		(CODER, CHECK THYSELF)
	PAGE	- - - P A S S   2   I N I T I A L I Z A T I O N - - -
*
* P2INIT
*	INITIALIZATION ROUTINE FOR BEGINNING PASS 2.
*	CLEARS PC, SETS PASS TO 1 AND EXITS.
*
P2INIT   EQU      *
         CLRA
         STAA     PC
         STAA     PC+1
         INCA
         STAA     PASS
	LDAA	OPTF	; RESET ALL THE VARIOUS OPTIONS THAT
	ANDA	#\(OPT:LN!OPT:MCM!OPT:LF)	; DONT WANT TO
	STAA	OPTF	; BE CARRIED ACROSS PASSES.
	JMP	REWSI	REWIND THE SI DEVICE AND GET OUT
	PAGE	- - - S P E C I A L   S Y M B O L   S E T U P - - -
*
*
MCSYMINT
	LDX	#SYM6800
	BSR	SYMZIP	Setup M6800
	LDX	#SYM6809
	BSR	SYMZIP	Setup M6809
	LDX	#SYM6801
	BSR	SYMZIP	Setup M6801
	RTS

SYMZIP	STX	SYMPT	Save pointer
	JSR	REHASH	Hash symbol into symbol table
	RTS
