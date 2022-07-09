	TITLE	- - -   D E C O D E   L I N E   - - -
	PAGE	- - -   D O I T L   - - -
*
*  DOITL--
*
*        DECODES COMMAND AND FIGGERS OUT WHAT TO DO W/ IT.
*
*	(1)	IF CF(1) IS 3 OR 4 CH, SCAN OPTABLE FOR VALID OP;
*
*	(2)	IF NOT (1) THEN SCAN LONG OPCODE TABLE;
*
*        (3)      IF NOT (1) OR NOT (2) THEN IF CF(1) IS A SYMBOL ONLY,
*                 AND IF THAT SYMBOL IS IN THE PSUEDO-OP TABLE,
*                 (USER OR ASSEMBLER) THEN WE SCAN A DIRECTIVE.
*
*        (4)      IF NOT (3) THEN CF IS HANDLED AS A FUNNY
*                 DATA EXP.
*
*        AFTERWARDS, LIST IS CALLED.
*
*
DOITEX9	JSR	LFPC	Set Label Field sym to PC value
	JMP      DOITEX

DOITL    EQU      *
         LDX      CFPTR
         STX      AFPTR             PREPARE TO LOOK FOR AF.
         JSR      CHCFSYM           IS CF(1) A SYMBOL??
         BNE      DOITEX9           B/ NO==> GO TREAT AS FUNNY DATA STAT
	CMPB	#ASCII:COMMA	CK/ IS TERMINATOR A COMMA? (B SET BY CHCFSYM)
	BEQ	DOITEX9	  B/ YUP.  CAN'T BE OPCODE.
	PAGE	MC6809 Opcode Processing
OP6809	CLRA		Initialize:
	STAA	PCNEXTREF	*' used flag
	STAA	PCNEXT	*' correcting flag
	STAA	PCRFLAG	PCR indexing flag
	STAA	OPSIZE	Op code size (may include operand)
	STAA	POSTBYTE	Post byte value (if any)
	STAA	INDXFG	Indexing flag (validates POSTBYTE)
	STAA	BRACKET	"[" (Indirect) flag
	STAA	P2OP	Clear Page 2 Operator flag

	LDAA	SYM	Get symbol length
	CMPA	#4	Large symbol?
	BGT	DOITOPBIG	B/Yes
	BEQ	DOITOP	B/4 Characters

	LDAA	#$20	=Blank
	STAA	SYM+4	  into 4th character position
	PAGE
	IF	M6809=0
DOITOP	LDX	#OPTBL	Scan for 4 character op code
DOITOPSL	STX	OPPTR	Save pointer into OPTBL
	LDX	0,X	Get 1st two bytes of op
	BMI	DOITOPBIG	B/End of OPTBL
	CPX	SYM+1	Match??
	BNE	DOITOPS	B/No	
	LDX	OPPTR
	LDX	2,X	2nd two byte of op
	CPX	SYM+3	Match??
	BEQ	DOOPF	B/Yes
DOITOPS	LDX	OPPTR	Step pointer
	RPT	7
	INX
	BRA	DOITOPSL
	ELSE
DOITOP	LDY	#OPTBL	Scan for 4 character op code
DOITOPSL
	LDX	0,Y	Get 1st two bytes of op
	BMI	DOITOPBIG	B/End of OPTBL
	CPX	SYM+1	Match??
	BNE	DOITOPS	B/No	
	LDX	2,Y	2nd two bytes of op
	CPX	SYM+3	Match??
	BEQ	DOOPF	B/Yes
DOITOPS	LEAY	7,Y	Step pointer
	BRA	DOITOPSL
	FIN
	PAGE
DOITOPBIG LDX	#M6809LONG	Long opcode table 
	JSR	FSYM1	 and look it up
	BNE	DOITDR	B/Not found
	BRA	DOOPBG

DOOPF
	IF	M6809=1
	TFR	Y,X
	ELSE
	LDX	OPPTR	Load pointer
	FIN
	LEAX	2,X	Adjust to match symbol table offset
DOOPBG	JMP	DOOPC	Do opcode processing
	INCLUDE	ASM14A.DREX.ASM	*********************
	TITLE	 - - - O P C O D E    P R O C E S S I N G - - -
	PAGE	GET OP CHARACTER DETERMINATION
DOOPC	STX	OPPTR	Save for optype processing
	JSR	LFPC	Set label if any
	JSR	FINDAF
	LDX	OPPTR
	LDAA	OT:OPCD,X	Save
	STAA	OP	 Opcode
	LDAA	OT:OPER,X	Save
	STAA	GRP	 Operand flags
	CLR	EVAFBIAS	No bias
	LDAA	OT:TYPE,X	Get op type
	BPL	DOOPC1	B/Not 680C Op code
	INC	EVAFBIAS	Flag for AF evaluation +1
	CLR	OP	Assume setting for monadic op code correction
	BITB	#OTF:IMM	A dydatic op ?
	BEQ	DOOPC1	B/Yes
	LDAA	#$80	Set for dydatic op code correction
	STAA	OP
DOOPC1	BSR	OPSET	Do opcode setup and AF processing
	LDX	OPPTR	Pointer to opcode table entry
	LDAA	OT:TYPE,X	680C type opcode ?
	BMI	MC680C	B/YES
	CLRA		Add nothing to OP before output to BO
	BSR	OUTCODE	Output the code
	JMP	LIST


MC680C	LDAB	OT:OPCD,X	680C code #
	TBA		Copy opcode
	ANDB	#$7F	Mask for lower 7 bits
	JSR	TBJMP
	#*+2
	#OPB1A0		ADCD	0
	#OPB1A0		ANDD	1
	#OPB1A0		EORD	2
	#OPB1A0		ORD	3
	#OPB1A0		SBCD	4
	#OPINCD		INCD	5
	#OPDECD		DECD	6
	PAGE
	TITLE	Opcode and argument field setup and Processing
*
*	OPSET	Opcode and argument field setup and processing
*
*	This subroutine calls the proper opcode class routine to generate
* an address mode adjusted opcode and correct operand bytes.  If the
* opcode in OP has be set to $00 then the opcode will be biased for the
* for the monadic class of opcodes.  If OP was forced to $80 then the
* opcode would be biased for the didatic class of opcode address modes.
*

OPSET
	CLRA		Initialize:
	STAA	PCNEXTREF	*' used flag
	STAA	PCNEXT	*' correcting flag
	STAA	PCRFLAG	PCR indexing flag
	STAA	POSTBYTE	Post byte value (if any)
	STAA	INDXFG	Indexing flag (validates POSTBYTE)
	STAA	BRACKET	"[" (Indirect) flag
	STAA	P2OP	Clear Page 2 Operator flag
	STA	VAL	Clear expression accumulator
	STA	VAL+1
	LDAA	#1	Assume opcode size of 1
	STAA	OPSIZE
	LDX	OPPTR	Get
	LDAB	OT:TYPE,X	 type byte
	ANDB	#OTM:TYPE	Mask for type and
	ASLB		 MAKE WORD INDEX
	JSR	LDAXB	ADD TO
	OPTYPETBL	 TYPE TABLE
	PAGE
	LDX	0,X	Get address of type routine
	JSR	0,X	 and call it
	LDX	OPPTR	Point to op info again
	LDAB	OT:TYPE,X	Get op type word
	ANDB	#OTM:CLAS	 and mask for class
	LSRB		Shift to
	LSRB		 be a
	LSRB		  word index
	JSR	LDAXB	   and add
	OPCLASTABEL		    to class table
	LDX	0,X	Get routine address
	STX	OPCLASS	Save vector to op class routine
	LDAB	GRP	Load flags
	JSR	0,X	 and call it
	BCC	NOERRORS	B/NO SYNTAX ERRORS
	LDX	#0	Clear VAL
	STX	VAL
	JSR	SYNTAX

NOERRORS	LDAA	OPSIZE	Combine operand
	ADDA	PCNEXT	 length and opcode
	STAA	OPSIZE	  length.
	RTS



*
* OPCODE CLASS TABLE
*
OPCLASTABEL
	#REGTOREG
	#PUSHPULL
	#BRANCH
	#GENERAL
	PAGE
	TITLE	Output Opcode and Operand Bytes to BO
*
*	OUTCODE	Output opcode and operand bytes to BO
*
*	This routine outputs the opcode and operand bytes generated by
* OPSET.  This routine may only be used after a call to OPSET.
*
*	A-Reg	Value to bias OP by before output to BO
*

OUTCODE
	ADDA	OP	A-Reg + OP is value to output
	STAA	OP
	LDAA	P2OP	PREFIX??
	BEQ	OUTOP	B/NO
	JSR	PUTB	OUTPUT PREFIX
	DEC	OPSIZE
OUTOP	LDAA	OP	GET OPCODE
	JSR	PUTB	 AND OUTPUT IT
	DEC	OPSIZE
	LDAA	POSTBYTE	DO WE HAVE 
	TST	INDXFG	 A POST BYTE??
	BEQ	NOPOST	B/NO
	JSR	PUTB	OUTPUT POST BYTE
	DEC	OPSIZE
	PAGE
*
*  DO OUTPUT OF  2ND POST BYTE AND THEN VALUE
*
NOPOST	TST	OPSIZE	ANY OPERAND?
	BEQ	NOOPERAND	B/NO
	DEC	OPSIZE	2 BYTE OPERAND?
	BNE	OUTOPERAND2	B/Yes
	LDA	VAL	H.O. = 0?
	BEQ	OUTOPERAND	B/Yes
	LDB	VAL+1	L.O.
	ROLB		Sign to Carry
	ADCA	#0	Extended sign?
	BEQ	OUTOPERAND	B/Yes (Value = $FF80)
	JSR	OVRFLW	Overflow in byte operand
	BRA	OUTOPERAND	 and output byte anyway.

OUTOPERAND2
	LDAA	VAL	OUTPUT H.O. BYTE
	JSR	PUTB
OUTOPERAND
	LDAA	VAL+1	OUTPUT L.O. BYTE
	JSR	PUTB
NOOPERAND
	RTS
	PAGE
	TITLE	Special 680C Opcode Expansions
*
* M680C Opcodes which act on B-Reg with arg+1 and A-reg with arg+0
*
OPB1A0	TAB		680C code
	JSR	LDAXB	X := #B1A0TABLE + (B)
	#B1A0TABLE
	LDAA	0,X	Load opcode base
	STAA	COP	 and save it
	BSR	OUTCODE	Output "xxxB arg+1" code to BO
	LDAA	#$80	Reset op
	STAA	OP	 to diatic
	JSR	OPSET	Evaluate operand again
	LDAA	COP	Reload opcode base
	LDAB	OP
	CMPB	#$8F	Immediate addressing mode ?
	BHI	OPB1A0X	B/No
	LDAA	VAL	Shuffle for proper output
	STAA	VAL+1	 of H.O. byte
	CLR	VAL	Clear to avoid overflow
OPB1A0X	SUBA	#$40	Make opcode xxxA
	BSR	OUTCODE	Output "xxxA arg+0"
	JMP	LIST


B1A0TABLE
	FCB	$C9-$80	ADCB
	FCB	$C4-$80	ANDB
	FCB	$C8-$80	EORB
	FCB	$CA-$80	ORB
	FCB	$C2-$80	SBCB

OPCLASS	RMB	2	Operator class of current operand
	PAGE
*	INCD
*
OPINCD	LDAA	INCOPCODE
	JSR	OUTCODE	"INC  oper+1"
	LDAA	BNEOPCODE
	LDAB	INCOPCODE
	JSR	OPBRANCHOVER	"  BNE  XXX"
*			"  INC  oper+0"
*			"XXX:       "
	JMP	LIST

*	DECD
*
OPDECD	LDAA	TSTOPCODE
	JSR	OUTCODE	"TST  oper+1"
	LDAA	BNEOPCODE
	LDAB	DECOPCODE
	JSR	OPBRANCHOVER	"    BNE  XXX"
*			"    DEC  oper+0"
*			"XXX:           "
	INC	EVAFBIAS	Set flag to bias arg field eval by +1
	CLR	OP	Reset op
	JSR	OPSET	Evaluate op field again
	LDAA	DECOPCODE
	JSR	OUTCODE	"DEC  oper+1"
	JMP	LIST
	PAGE
*
* OPBRANCHOVER
*
* Entry:
*	A-Reg	Branch opcode
*	B-Reg	Branched over opcode
*
* Exit:
*	Output is effectively:
*		Bxx  Q
*		yyy  oper
*	       Q:
*
OPBRANCHOVER
	STAB	COP	Save op code
	JSR	PUTB	Put out branch op code
	LDX	PC	Bias PC
	INX		 by +1
	STX	PC	  for operand evaluation
	CLR	OP	Clear opcode
	JSR	OPSET	Do operand evaluation
	LDX	PC	Put PC
	DEX		 back the way
	STX	PC	  it was
	LDAA	OPSIZE	Get full instruction size
	JSR	PUTB	 and output it as 2nd byte of branch
	LDAA	COP	Branched over op code
	JSR	OUTCODE	"yyy  oper"
	RTS
	PAGE	REG,REG INSTRUCTIONS
*
*	Register, Register operand field
*
REGTOREG
	INC	OPSIZE
	JSR	RRSCAN	SCAN FOR A REGISTER
	BCS	TINTAX	B/REGISTER NOT FOUND
	STAA	VAL+1	SAVE REGISTER SETTING
	JSR	GTCMA	PAST THE COMMA
	BVC	TINTAX	B/NOT FOUND
	JSR	RRSCAN	SCAN FOR A REGISTER
	BCS	TINTAX	B/REGISTER NOT FOUND
	ORAB	VAL+1	COMBINE BOTH REGISTER SETTINGS
	STAB	VAL+1	 AND PUT IT BACK
	EORA	VAL+1	ARE THE
	BITA	#$80	 REGISTERS OF THE SAME TYPE?
	BNE	TINTAX	B/NO
JIPIT	RTS		DUMP BINARY

TINTAX	SEC
	RTS
         PAGE     - - - C H I M M - - -
*13*
*
*
* SUBROUTINE TO CHECK FOR IMMEDIATE INSTRUCTIONS:
* IF IT IS IMMEDIATE, THEN RETURNS Z=1, AND SETS VAL
* TO VALUE OF EXPRESSION.  OTHERWISE, RETURNS Z=0.
*
CHIMM    LDX      AFPTR             GET POINTER TO FIRST CH.
         BEQ      CHIRTN            B/ NO AF; NOT IMMEDIATE.
         LDAA     0,X               GET FIRST CH OF AF.
         CMPA     #'#               IS IT IMMEDIATE QUALIFIER?
         BNE      CHIRTS            B/ NO # AT START ==> NOT IMMEDIATE.
	LDX	OPPTR	This a
	LDAA	OT:TYPE,X	 Push/Pull
	CMPA	#OTF:PP/256	  instruction??
	BEQ	CHICHI	B/Yes
	JSR	OPFIXIM	SET FOR IMMEDIATE
	CLR	EVAFBIAS	Never any bias in any Immediate addresses

CHICHI	LDX	AFPTR
	LDAA	GRP	GET OPERAND FLAGS
	BITA	#OTF:2BYTE	LONG LITERAL??
	BEQ	CHILLIT	B/NO
	INC	OPSIZE	INCREASE SIZE
CHILLIT	INX                        POINT AT NEXT CH,
         JSR      EVAFX             GO EVALUATE THE EXPRESSION.
         CLRB                       SET APPROPRIATE CC.
CHIRTS   RTS                        AND EXIT, SUCCESS.
*
CHIRTN   INX                        SET CC # 0,
         RTS                        AND EXIT, FAIL.
         PAGE     - - - B R A N C H E S - - -
*
* RELATIVE BRANCHES:
BRANCH	INC	OPSIZE	ASSUME ONE BYTE OF OFFSET
	BITB	#OTF:2BYTE	LONG BRANCH?
	BEQ	BRAN0	B/NO
	INC	OPSIZE	BUMP OPSIZE
BRAN0	JSR      EVAF              EVALUATE THE ARG...
	LDAA     0,X               GET THE TERMINATOR
	CMPA     #ASCII:COMMA	IS IT A COMMA?
	BNE      BRAN1             B/ NO:  NOT TRYING TO INDEX.
	JSR      ILLAF             *E* TRIED TO INDEX A BRANCH.
BRAN1    LDAA     XTYPE             GET THE EXPRESSION TYPE
	BITA     #XT:FREF!XT:UNDF  IS IT EASY TO CHECK?
	BEQ      BRAN2             B/ YES.
	TST      PASS              IS THIS PASS 1?
	BEQ      OUT222             B/ YES
* CHECK FOR OUT OF RANGE BRANCH
BRAN2    LDAB     VAL+1             GET THE VALUE
	LDAA     VAL
	SUBB     PC+1
	SBCA     PC                (A,B) := VAL-(PC+OPSIZE)
	SUBB     OPSIZE
	STAB     VAL+1             SAVE NEW VAL,
	SBCA     #0
	STAA	VAL
	LDAA	GRP
	BITA	#OTF:2BYTE	LONG BRANCH ? (not Jersy again)
	BNE	OUT222	B/YES, NEVER OUT OF RANGE
	LDAA	VAL
	BEQ      BRANP             CHECK VAL FOR IN RANGE...
	BMI      BRANN             B/ VAL<0
BRANE    JSR      BRANERR           *E* OUT OF RANGE BRANCH
	CLRA		GENERATE A "BRA *"
	CLRB
	SUBB	OPSIZE
	SBCA	#0
	STAA	VAL
	STAB     VAL+1
OUT222	CLC
	RTS	    	AND OUTPUT THE CODE
	PAGE
*
* BRANCH IN NEGATIVE DIRECTION.
*
BRANN	COMA
	COMB                       PREPARE FOR RANGE CHECK.
*
* BRANCH IN POSITIVE DIRECTION.
* UNSIGNED.
*
BRANP	TSTA		<255?
	BNE	BRANTCHK	B/NO
	TSTB		<128?
	BGE      OUT222             B/ YES, B IS UNSIGNED ==> GOOD
BRANTCHK	LDAA	XTYPE	EVALUABLE??
	BITA	#XT:FREF!XT:UNDF!XT:UNEV
	BNE	BRANE	B/NO - FIX HIS WAGON
	INC	OPSIZE	BUMP TO 2 BYTE RELATIVE ADDRESS
	LDAA	GRP	GET OPERAND FLAGS
	ORAA	#OTF:2BYTE	FORCE 2 BYTE OPERAND FLAG
	STAA	GRP
	LDAA	OP	GET BRANCH OP
	BMI	BRBSR	B/BSR
	CMPA	#$20	"BRA" ??
	BEQ	BRBRA	B/YES
	INC	OPSIZE	BUMP FOR PREFIX
	LDAA	#$10	SET
	STAA	P2OP	 PREFIX
	JMP	BRAN0	  AND GO

BRBSR	LDAA	#$17	"LBSR" OP
	BRA	BRBRA1

BRBRA	LDAA	#$16	"LBRA" OP
BRBRA1	STAA	OP	FORCE TO LONG OP
	JMP	BRAN0	 AND TRY AGAIN
	PAGE	REGISTER SCANNING ROUTINES
*+
* REGIDX	SCAN FOR INDEX REGISTER
*
* RETURNS:	A reg = Index register setting
*		POSTBYTE = A reg ! POSTBYTE
*
*		Burps on error
*-
REGIDX	JSR	REGSCAN	GET A REGISTER
	LDAA	REG:IDX,X	LOAD REGISTER CODE
	BMI	BURP	ILLEGAL REGISTER
	ORAA	POSTBYTE
	STAA	POSTBYTE
	CLC
	RTS


* RRSCAN		Scan for valid R,R register
*
* RETURNS:  A = REGISTER CODE *$10
*           B = REGISTER CODE
*       CARRY = SET IF VALID REGISTER NOT FOUND
*
RRSCAN	BSR	REGSCAN	GET A REGISTER
	LDAA	REG:RR,X	LOAD RR CODE BYTE
	CMPA	#$FF	LEGAL?
	BEQ	BURP	B/NO
	TAB		COPY IN B
	RPT	4
	ASLA		SHIFT TO UPPER NIBBLE
	CLC		CLEAR CARRY
	RTS
	PAGE
*+
* REGSCAN	General Register Scan
*
* SCANS FOR SYMBOL, LOOKS UP SYMBOL IN TABLE OF VALID REGISTER MNEMONICS
* AND IF ALL IS WELL DOES A RETURN.  ANYTHING WRONG CAUSES THE ROUTINE
* TO BURP.
*
REGSCAN	JSR	GNOBJ	GRAB SOMETHING
	CMPA	#O:SYM	GOT A SYMBOL?
	BNE	BURP	B/NO
	LDX	#REGTBL	VALID REGISTER NAME TABLE
	JSR	FSYM1	LOOKUP REGISTER NAME
	BNE	BURP	B/NO FINDE
	RTS
	PAGE	RECOVERY ROUTINE FOR REGISTER SCANNERS
*+
* BURP	Branch Unconditonally to Recovery Proceedure
*
* This routine unwinds the stack until it finds a BSR or JSR which is
* followed by a BCS or BCC.  The carry is then set and control given
* to the BCC or BCS.
*-
BURP	TSX		GET STACK POINTER
BURP1	STX	TEMPX	SAVE POINTER INTO STACK
	LDX	0,X	GET ADDRESS FROM STACK
	LDAA	0,X	GET OPCODE AT RETURN ADDRESS
	CMPA	#$24	BCC?
	BEQ	BURPUP	B/YES
	CMPA	#$25	BCS?
	BEQ	BURPUP	B/YES
	LDX	TEMPX	RELOAD POINTER INTO STACK
	LEAX	2,X
	BRA	BURP1

BURPUP	LDX	TEMPX	GET THAT VALID NEW STACK POINTER
	TXS		ZAPPA THE STACKA
	SEC		SET DA CARRY
	RTS
	PAGE	PUSH and PULL Instruction Processing
*+
* PUSHPULL	PUSH and PULL Processing
*
*-
PUSHPULL	INC	OPSIZE	Count operand
	CLR	VAL+1
	JSR	CHIMM	Check for "immediate" form
	BEQ	PP2
PSHPUL	BSR	REGSCAN	GET A REGISTER
	BCS	BURP	B/NOT FOUND
	LDAA	REG:PP,X	GET PUSH/PULL BYTE
	CMPA	#$FF	IS IT LEGAL FOR PUSH/PULL
	BEQ	BURP	B/NO
	CMPA	#$40	U or SP??
	BNE	PP1	B/NO
	LDX	OPPTR
	LDAB	OT:TYPE-1,X	Get 'U or 'S
	EORB	OP	CORRECT
	BITB	#2	 STACK? (What a cludge!)
	BEQ	BURP	B/NO

PP1	ORAA	VAL+1	MIX'UM UP
	STAA	VAL+1	 TOGETHER
	JSR	GTCMA	PICK OFF A ',
	BVS	PSHPUL	B/FOUND

PP2	CLC		NOT FOUND END OF AF
	RTS
	PAGE	GENERAL TYPE
*+
* GENERAL TYPE	(OLD MONADIC AND DIDATICS)
*
*-
GENERAL
	TST	GRP	ANY OPERANDS?
	BNE	DOITTOIT	B/YES
	CLC
	RTS
	PAGE	6809 Dydatic (Memory Ref) Operand Processing
*+
*	Operand Processing for Memory Reference (Dydatic) Instructions
*
*	Modes:	Immediate
*		Extended
*		Direct
*		Indexed
*-

ABTBL	#DOITTO		Direct or Extended not specified
	#DOITTSS		"<<" Illegal
	#DOITTOSH	"<" Force Direct
	#DOITTOXF	">" Force Extended

DOITIDXJ	JMP	DOITIDX


DOITTOIT	INC	OPSIZE	Bump for argument (at least 1 byte)
	JSR	CHIMM	Check for Immediate
	BEQ	DOSHIP	B/Immediate
	JSR	SETWRAPUPTR	Init indexed wrapup scan pointer
	JSR	ABRKTEST	Check for "<","<<",">"
	STAB	ABRKT	 and set flag

	LDAA	0,X	Get 1st Character of AF
	CMPA	#$5B	Indirect ([) ??
	BNE	DOITRT	B/No

	INX		Step past "["
	STX	AFPTR
	LDAA	#PB:INDIR	Set Indirect bit
	STAA	POSTBYTE	 in Postbyte
	STAA	BRACKET	  and set "[" seen flag
	LDAA	ABRKT	Check for
	DECA		 "<<"?
	BNE	DOITRT	B/No
	JSR	SYNTAX
	INC	ABRKT	Force "<"

DOITRT	JSR	REGTST	"reg,"??
	BNE	DOITREGIDX	B/Yes, Register Offset Indexing

	LDX	AFPTR	Set Expression
	STX	EXPTR	 Pointer
	JSR	GTCMA	Comma Found??
	BVS	DOITIDXJ	B/Yes, Indexed for sure

	TST	BRACKET	"[" seen?
	BNE	DOITIDXJ	B/Yes

	JSR	EVAF	Get AF Value for Direct or Extended
	CMPB	#']	Was terminator "]" ?
	BNE	NOSBRK	B/No
	JSR	REJCH	Put it back (reject it)
NOSBRK
	LDAB	ABRKT	Get Bracket ("<") Flag if any
	JSR	TBJMP	Branch on flag value (0-3)
	#ABTBL		(Angle Bracket TaBLe)

DOSHIP	CLC
	RTS		SHIPIT
	PAGE
DOITTSS	JSR	FRCREFERR	Flag "<<" as error

DOITTO	LDAA	XTYPE	Expression Evaluable??
	BITA	#XT:FREF!XT:UNDF!XT:UNEV
	BNE	DOITTOXF	B/No, Force Extended

	LDAA	VAL	Yes, >8 bits??
	BNE	DOITTOX	B/Yes, May be extended

	TST	DPRSET	DPR Setting zero??
	BNE	DOITTOXF	NO, Force Extended

DOITTDR	JSR	OPFIXDR	Adjust op to Direct mode
	JMP	IWRAPUP1

DOITTOSH	LDAA	VAL	Forced Direct ("<")
	CMPA	DPRSET	Page setting match??
	BNE	DOITDER	B/No, DPR error

DOITTDPR	CLR	VAL	Yes, Do Direct
	BRA	DOITTDR


DOITDER	JSR	FRCREFERR	Flag bad DPR setting
	BRA	DOITTOXF


DOITTOX	LDAA	VAL	Can we 
	CMPA	DPRSET	 do a Direct mode??
	BEQ	DOITTDPR	B/Yes

DOITTOXF	INC	OPSIZE	Make Extended
	JSR	OPFIXEX	Make op Extended
	JMP	IWRAPUP1

	PAGE
*+
* Zero offset, Auto inc/dec and Register Offset processing
*-

DOITREGIDX
	STX	EXPTR	Update scan pointer
	BSR	OPFIXINDX	MONKEY WITH OPCODE FOR INDEXING
	INC	INDXFG	Mark indexing mode
	CMPB	#PB:ZIDX	Zero offset or auto inc/dec mode?
	BEQ	DOITAUTO	B/Yes
	ORAB	POSTBYTE	Put post byte together
	STAB	POSTBYTE
	JSR	REGIDX	Pick off index reg
	BCS	SINTAX	B/Reg not found
	JMP	IWRAPUP1	Return (register offset)

DOITAUTO	LDAA	EVAFBIAS	680C ?
	BNE	SINTAX	B/Yes, ",arg" addressing mode illegal in 680C
	LDX	EXPTR	Get scan pointer
	LDAA	0,X	 and a character
	CMPA	#'-	"-"??
	BNE	DOITAUTOI	B/No
	INX
	LDAB	#PB:AD1	Assume 1 byte pre-decrement
	CMPA	0,X	"--"??
	BNE	DOITAUTOD	B/No
	INX
	LDAB	#PB:AD2	2 byte pre-decrement
DOITAUTOD STX	EXPTR	Update scan pointer
	BSR	BADIND	Check of illegal indirect
	JSR	REGIDX	Scan off register
	BCS	SINTAX
	JMP	IWRAPUP1


OPFIXINDX
	LDAA	GRP	Is only indexed
	CMPA	#OTF:IND	 addressing mode allowed??
	BEQ	OPFIXINDXRET	B/Yes, LEA type instruction
	JSR	OPFIXIND	No, mess with opcode
OPFIXINDXRET
	RTS
	PAGE
DOITAUTOI
	JSR	REGIDX	Scan off index register
	BCS	SINTAX	B/Error
	LDX	EXPTR	Scanner pointer
	LDAB	#PB:ZIDX	Postbyte setup for Zero Index
	LDAA	0,X
	CMPA	#'+	R+??
	BNE	DOITZI	B/No
	LDAB	#PB:AI1	Assume R+
	INX
	CMPA	0,X	R++??
	BNE	DOITAI	B/No
	INX
	LDAB	#PB:AI2
DOITAI	STX	EXPTR
	BSR	BADIND	Check for illegal indirect
	JMP	IWRAPUP1

DOITZI	ORAB	POSTBYTE	Zero index
	STAB	POSTBYTE
	JMP	IWRAPUP1


BADIND	ORAB	POSTBYTE
	STAB	POSTBYTE
	ANDB	#PB:AMSK	An
	LSRB		 illegal indirect??
	BCS	BADIRTS	B/No
	BNE	SINTAX2	B/Yes
BADIRTS	RTS

SINTAX2	LEAS	2,X	Prune stack

SINTAX	SEC
	RTS
	PAGE
*+
*    Indexed or Extended Indirect
*-
DOITIDX	BSR	OPFIXINDX	FUNNY UP OPCODE FOR INDEXING
	INC	INDXFG	FLAG POST BYTE PRESENT
	LDAB	ABRKT	Any angle brackets??
	BEQ	DOIDXSET	B/No
	DECB		"<<"=0; "<"=1; ">"=2
	CMPB	PCNEXT	Are we forced to be longer??
	BLT	DOIDXSET	B/No
	STAB	PCNEXT	Yes, force

DOIDXSET	LDX	AFPTR	Set expression
	STX	EXPTR	 scan pointer
	JSR	GTCMA	Zip out to comma
	BVS	DOIDXREG	B/Found
	JSR	REJCH	Backup to possible "]"
	JSR	SETWRAPUPTR	Mark scan pointer
	LDAA	#2	Must be extended indirect!!
	STAA	PCNEXT	Force 2 bytes
	TST	BRACKET	Indirect? (better be)
	BEQ	ZINTAX
	TST	ABRKT	Angle brackets?? (better not.)
	BEQ	DOEVL	B/No (go do evil)
FORCERR	JSR	FRCREFERR	Flag angle bracket usage error
	BRA	ZEN	(Sound of one hand clapping)

ZINTAX	JSR	SYNTAX	Syntax error!
ZEN	LDAA	#2	Force 2 byte index or offset
	STAA	PCNEXT	 or extended addressing
	JMP	DO16B	  and try to get an expression value
	PAGE

*   !! We are indexing off a register: X, Y, U, S, PCR !!
DOIDXREG	JSR	REGSCAN	Pick off register (",reg")
	BCS	ZINTAX	B/No register, must have a register
	LDAA	REG:IDX,X	Get register code
	CMPA	#$FF	Illegal register??
	BEQ	ZINTAX	B/Yes, you bet your ass -- zap!
	JSR	SETWRAPUPTR	Init indexed wrapup scan pointer
	CMPA	#$FE	PCR???
	BEQ	DOPCR	B/Yes
	ORAA	POSTBYTE	Merge into
	STAA	POSTBYTE	 postbyte
	BRA	DOEVL

DOPCR	LDAB	#PB:8BIT!!PB:PCR8	Set PCR flag
	STAB	PCRFLAG
	LDAB	ABRKT
	DECB		"<<"???
	BEQ	FORCERR	B/Yes, that's a no-no
	TST	PCNEXT	Assumed 5-bit
	BNE	DOEVL	B/No

DOEVLN	INC	PCNEXT	Step to next size

DOEVL	JSR	EVAF	Evaluate AF
	JSR	PCRFIX	Do PCR fix if necessary
	LDAB	PCNEXT	Est. size of post post byte stuff
	JSR	TBJMP	Do table branch
	#*+2
	#DO5B
	#DO8B
	#DO16B
	PAGE
DO5B	LDAA	ABRKT	5-bit offset (we hope!)
	DECA		"<<" 5-Bit forced??
	BNE	DO5BCK	B/No
	TST	PASS	Pass 1??
	BEQ	DO5BGO	B/Yes
	TST	BRACKET	"<<["??
	BEQ	DO5B1	B/No
	JSR	FRCREFERR	Forced reference illegal
DO5B1	JSR	VALSIZE	Check value size
	BCS	DO5BGO	B/5-bit value
	JSR	BRANERR	Range error
	BRA	DO5BGO

DO5BCK	TST	BRACKET	Indirect?
	BNE	DO8BN	B/Yes - force 8 bit offset
	LDAA	XTYPE	Expression evaluable??
	BITA	#XT:FREF!XT:UNDF!XT:UNEV
	BNE	DO8B16	B/No - 16-bit forward reference
	JSR	VALSIZE	Check size of value
	BCC	DO8BN	B/Don't look like it fits

DO5BGO	LDAA	VAL+1	Mask value
	ANDA	#$1F	 to 5 bits
	ORAA	#PB:5BIT	  and put mode into post byte
	CMPA	#PB:5BIT	Zero offset ?
	BNE	DOMASH	B/No
	TST	ABRKT	Forced 5-bit ?
	BNE	DOMASH	B/Yes
	LDAA	#PB:ZIDX	Set zero index mode (reg indirect)
	BRA	DOMASH

DO8BN	INC	PCNEXT	Assume 8 bits
	TST	PCRFLAG	PC relative??
	BNE	DOEVL	B/Yes, re-evaluate
	TST	PCNEXTREF	Was "*'" used??
DOEVLJ	BNE	DOEVL	B/Yes, lets do it again

DO8B	LDAA	ABRKT	8-bit offset
	SUBA	#2	"<"??
	BNE	DO8BCK	B/No, not forced.
	TST	PASS	Pass 1??
	BEQ	DO8BGO	B/Yes
	JSR	VALSIZE	Check value size
	BVC	DO8BGO	B/8-bit value
	JSR	BRANERR	Range error
	BRA	DO8BGO
	page
DO8BCK	LDAA	XTYPE	Expression evaluable??
	BITA	#XT:FREF!XT:UNDF!XT:UNEV
	BNE	DO8B16	B/No
	BSR	VALSIZE	Check value size
	BVS	DO8B16	B/More than 8 bits

DO8BGO	LDAA	#PB:8BIT	Assume X, Y, U, S, indexing
	ORAA	PCRFLAG	Make PB:PCR8 if PCR
DOMASH	ORAA	POSTBYTE	Put in other junk
	STAA	POSTBYTE
	BRA	IWRAPUP

DO8B16	LDAA	#2	Force 2 byte value
	STAA	PCNEXT
	TST	PCNEXTREF	"*'" used??
	BNE	DOEVLJ	B/Yes
	TST	PCRFLAG	PC relative??
	BNE	DOEVLJ	B/Yes

DO16B	LDAA	#PB:16BIT	Assume X, Y, U, S, indexing
	ORAA	PCRFLAG	Make PB:PCR16 if PC relative
	ORAA	POSTBYTE	 and mix it in
	STAA	POSTBYTE
	LDX	EXPTR	Was
	DEX		 the
	LDAB	0,X	  expression
	CMPB	#ASCII:COMMA	   stopped by ","????
	BEQ	IWRAPUP	B/Yes
	LDAA	#PB:XIND	Must be extended indirect, set
	STAA	POSTBYTE	 to extended indirect.
	BRA	IWRAPUP2

IWRAPUP1
	JSR	SETWRAPUPTR	Set wrapup scan pointer
IWRAPUP	CLR	RJOBJ
	LDX	WRAPUPTR	Set scan pointer for
	STX	EXPTR	 wrapup scan

IWRAPUP2	JSR	GNOBJ	SCAN FOR "]"
	CMPA	#O:TERM	A terminator??
	BNE	DOISYN	B/No, Syntax error
	CLRA		Assume no "]"
	CMPB	#']	"]"?????
	BNE	DONOBRK	B/No "]"
	LDA	#PB:INDIR	Value of BRACKET if "[" seen
DONOBRK	CMPA	BRACKET	Balanced brackets ?
	BEQ	DOIRTS	B/No
DOISYN	JSR	SYNTAX	Unbalanced "[]"

DOIRTS	CLC
	RTS


*
*	Routine to save pointer for wrapup scan
*
SETWRAPUPTR
	LDX	EXPTR	Save operand scan
	STX	WRAPUPTR	 pointer for wrapup
	RTS

WRAPUPTR	RMB	2	Wrapup pointer
	PAGE
*+
* VALSIZE	Size Value as 5, 8, or 16 bit value
*
* Returns:	OV = 1 ==> 16 bits
*		C  = 0 ==> 8 bits
*		C  = 1 ==> 5 bits
*-
VALSIZE	LDAB	VAL+1	Take
	LDAA	VAL	 absolute
	BPL	VALSIZE1	  value
	COMB		   of
	COMA		    value

VALSIZE1	CLC
	BNE	VALS16	B/16 bits
	TSTB
	BMI	VALS16	B/16 bits (128-255)
	BITB	#$F0	5 bit offset??
	BNE	VALSIZE2	B/8 bit
	SEC
VALSIZE2	RTS

VALS16	SEV		SET OVERFLOW
	RTS
	PAGE
*+
* PCRFIX	Compute VAL as PC relative value if PCR mode
*
*	A and B registers zapped
*-
PCRFIX	TST	PCRFLAG	PC relative??	
	BEQ	PCRFIXR	B/No
	LDAA	VAL	Value
	LDAB	VAL+1
	SUBB	PC+1
	SBCA	PC	-PC
	SUBB	OPSIZE
	SBCA	#0	-OPSIZE
	SUBB	PCNEXT
	SBCA	#0	-PCNEXT
	STAA	VAL
	STAB	VAL+1
PCRFIXR	RTS
	PAGE
*+
* ABRKTEST	Angle Bracket Test
*
* This routine will step AFPTR past "<", "<<", ">" if they are there.
*
* Returns:	AFPTR updated (if Angle Brackets found)
*		X-Reg = AFPTR
*		A-Reg   Destroyed
*		B-Reg = 0 -> No Angle Brackets
*		        1 -> "<<"
*		        2 -> "<"
*		        3 -> ">"
*		ABRKT = B-Reg
*-
ABRKTEST	CLRB		Assume none
	LDX	AFPTR	Get 1st
	LDAA	0,X	 character of AF
	CMPA	#'<	Angle Bracket??
	BNE	ABRK1	B/No, Try other
	INX		Bump AFPTR
	LDAB	#2	Set to "<"
	CMPA	0,X	"<<"??
	BNE	ABRK3	B/No
	CLRB
	BRA	ABRK2	Yes

ABRK1	CMPA	#'>	Going my way??
	BNE	ABRK3	B/No, Not any at all
	LDAB	#2	Setup for 3
ABRK2	INX		Step AFPTR
	INCB

ABRK3	STAB	ABRKT	Save flag
	STX	AFPTR	Update AFTPR
	RTS
	PAGE	Set Addressing Mode (and see if legal)
*+
* OPFIX
*
* This routine adds a "correction" to the opcode to produce the correct
* addressing mode.  The instructions with a base opcode of < $80 have
* a different correction than those above.
*
*	NOTE: B-register is preserved through operation
*
*-
OPFIXIM	JSR	OPFIX	Immediate addressing mode
OPFIXDR	JSR	OPFIX	Direct addressing mode
OPFIXEX	JSR	OPFIX	Extended addressing mode
OPFIXIND	JSR	OPFIX	Indexed addressing mode


OPFIXTAB	FCB	0,0,OTF:IMM	Immediate Factors
	FCB	0,$10,OTF:DIR	Direct Factors
	FCB	$70,$30,OTF:EXT	Extended Factors
	FCB	$60,$20,OTF:IND	Indexed Factors

OPDISP	EQU	OPFIXTAB-OPFIXDR	Displacement to table entries

OPFIX	PULX		Get biased pinter into OPFIXTAB
	LDAA	GRP	Load operand flags
	BITA	OPDISP+2,X	Is addressing mode permitted??
	BEQ	OPBURP	B/no
	LDAA	OP	Dydatic Op??
	BPL	OPFIX2	B/No
	INX		Step to Dydatic entry
OPFIX2	ADDA	OPDISP,X	Add in correction factor
	STAA	OP	  and update OP
	RTS

OPBURP	JMP	BURP
	PAGE
*+
* REGTST		Register Test for Register Offset
*
* Check AF for "A,", "B,", "D," or ","
*
*
* Returns:	A-Reg   Destroyed
*		B-Reg = "A," -> PB:AOFF
*		        "B," -> PB:BOFF
*		        "D," -> PB:DOFF
*		        ","  -> PB:ZIDX
*		       else  -> 0
*		X-Reg = AFPTR
*		AFPTR   Steped passed any of the above listed objects.
*		Z-CC    Set to state of B-Reg
*-
REGTST	LDX	AFPTR	1st Character
	LDAA	0,X	 of AF
	LDAB	#PB:ZIDX	Assume ","
	CMPA	#ASCII:COMMA	","??
	BEQ	REGTSTY	B/Yes
	STAA	TEMPA	Save 1st char of AF
	CMPA	#$61	Lower case (>=a) ?
	BLT	REGTST1	B/No
	SUBA	#'a-'A	Fold into upper case
	STAA	0,X
REGTST1	LDX	0,X	1st 2 Characters of AF
	LDAB	#PB:BOFF	Assume "B," 
	CPX	#'B##8!',	Is it??
	BEQ	REGTSTX	B/Yes
	LDAB	#PB:AOFF	Assume "A,"
	CPX	#'A##8!',	Is it??
	BEQ	REGTSTX	B/Yes
	LDAB	#PB:DOFF	Assume "D,"
	CPX	#'D##8!',	Is it??
	BEQ	REGTSTX	B/Yes
	BSR	UNDO	Undo any folding of 1st char
	CLRB		Flag none
	RTS
	PAGE
REGTSTX	BSR	UNDO	Update
	INX		 AF Pointer
REGTSTY	INX		(No comment)
	STX	AFPTR
	TSTB		Set Z=0
	RTS

UNDO	LDX	AFPTR	Restore 1st
	LDAA	TEMPA	 character of
	STAA	0,X	  arg field
	RTS
	PAGE
*+
*	Register Symbol Table
*
* This table has the same format as the normal symbol table.
*
*	Link		W/ Link to next entry
REG:RR	EQU	S:VAL	B/ TFR or EXG register code
REG:PP	EQU	S:VAL+1	B/ PUSH/PULL register value
REG:IDX	EQU	S:TYPE	B/ Index mode register value
*	Symbol length	B/ Symbol length
*	Text String	S/ Symbol text
*-
REGTBL	#REGT00		Register table head

REGT00	#REGT01,+8,2,$FF,1,'A	A register
REGT01	#REGT02,0,6,$FF,1	D
	FCC	"D"
REGT02	#REGT03,9,4,$FF,1,'B	B register
REGT03	#REGT04,$A,1,$FF,2,'C,'C	Condition Codes
REGT04	#REGT05,$B,8,$FF,2,'D,'P	DPR (Data Page Register)
REGT05	#REGT06,4,$40,$60,1,'S	SP
REGT06	#REGT07,5,$80,$0C,2	PC
	FCC	"PC"
REGT07	#REGT08,$FF,$FF,$FE,3	PCR (PC Relative)
	FCC	"PCR"
REGT08	#REGT09,3,$40,$40,1,'U	User Stack
REGT09	#REGT10,1,$10,0,1,'X	IX
REGT10	#REGT11,2,$20,$20,1,'Y	IY
REGT11	EQU	0
	PAGE
*+
* OP TYPE PROCESSING
*-
OPTYPETBL
	#ONEBYTEOP
	#TWOBYTEOP10
	#TWOBYTEOP11
	#ONEBYTEOP	(DUMMY)


ONEBYTEOP
OPLENFIX LDAB	GRP	GET OPERAND FLAGS
	BEQ	OPLENFEXIT	B/NO OPERAND BYTE(S)

OPLENFEXIT
	RTS


TWOBYTEOP10
	LDAA	#$10	PREFIX BYTE
	BRA	P3OP

TWOBYTEOP11
	LDAA	#$11	PREFIX BYTE
P3OP	STAA	P2OP	SET FOR BIN OUTPUT
	BSR	OPLENFIX	ADJUST CODE LENGTH
	INC	OPSIZE	BUMP FOR PREFIX
	RTS


	PAGE	Op Code Type/Control Byte Definitions 6809
*
*		Opcode Type Byte	OT:TYPE
*
*             0 1 2 3 4 5 6 7
*             - ----- --- ---
*             !  !     !   !
*  RESERVED---(--(-----+   !
*             !  !         !
*       +-----+  !       TYPE
*     680C       !       ----
*     Flag       !    0 = 1 Byte Op Code
*                !    1 = 2 Byte Op Code with $10 Prefix
*                !    2 = 2 Byte Op Code with $11 Prefix
*                !   
*                !  
*              CLASS
*              -----
*	0 = Reg/Reg class opcode      OTF:RR
*	1 = Push/Pull class           OTF:PP
*	2 = Branch (relative) class   OTF:BRA
*	3 = General (old monadics, diadatics)  OTF:GEN
*
*
*		Operand Flag Byte	OT:OPER
*
*              0 1 2 3 4 5 6 7
*              ----- - - - - -
*                !   ! ! ! ! !
*                !   ! ! ! ! !
*                !   ! ! ! ! Extended Addressing  OTF:EXT
*                !   ! ! ! !
*                !   ! ! ! Indexed Addressing  OTF:IND
*                !   ! ! !
*       +--------+   ! ! Direct Addressing  OTF:DIR
*    Reserved        ! !
*                    ! Immediate Value Addressing  OTF:IMM
*	            !
*        0 = 1 Byte Literal/Immediate/Relative (branch)  OTF:2BYTE
*        1 = 2 Byte Literal/Immediate/Relative (branch)
	PAGE
*	Opcode Table Entry Layout for 6809
*
*	+--------+--------+--------+--------+
*	!  Four  !  Bytes !   of   !  Text  !
*	+--------+--------+--------+--------+
*
*	+--------+
*	!  Type  !	One byte of type   OT:TYPE
*	+--------+
*
*	+--------+
*	!  Flags !    	One byte of flags  OT:OPER    
*	+--------+
*
*	+--------+
*	! Opcode !  **	Opcode Value       OT:OPCD    
*	+--------+
*
**	There are 7 bytes per entry in the 6809 Opcode table
*
*   ** For 680C ops, the opcode byte is an index into a 680C branch table.
*
*
*

OC:1	EQU	0	One byte opcodes
OC:210	EQU	$100	Two byte opcode with $10 prefix
OC:211	EQU	$200	Two byte opcode with $11 prefix

OT:TYPE	EQU	2	Offset for type byte
OT:OPER	EQU	3	Offset for operand flags
OT:OPCD	EQU	4	Offset for opcode value
	PAGE
*
*	Flag Byte Definitions
*
OTF:EXT	EQU	1	Extended
OTF:IND	EQU	2	Indexed
OTF:DIR	EQU	4	Direct
OTF:IMM	EQU	8	Immediate
OTF:2BYTE	EQU	$10	2 Byte Literal/Immediate/Relative
OTF:RR	EQU	$0000	Register to Register
OTF:PP	EQU	$1000	Push/Pull
OTF:BRA	EQU	$2000	Branch
OTF:GEN	EQU	$3000	Old monadics & didatics
OTF:68C	EQU	$8000	680C code


*
*	Common flag combinations
*
OTF:NIL	EQU	OTF:GEN		No Argument Field
OTF:MON	EQU	OTF:EXT!OTF:IND!OTF:DIR!OTF:GEN	Memory reference monadic
OTF:DYD	EQU	OTF:MON!OTF:IMM	Dydatics
OTF:SBR	EQU	OTF:BRA	Short branchs
OTF:LBR	EQU	OTF:BRA!OTF:2BYTE!OC:210	Long branchs
OTF:LDYD1	EQU	OTF:DYD!OC:210	Long dydatic
OTF:LDYD2	EQU	OTF:DYD!OC:211	Long dydatic
OTF:DYDW	EQU	OTF:DYD!OTF:2BYTE	Dydatic with 2 byte Imm
OTF:LDYD1W	EQU	OTF:LDYD1!OTF:2BYTE
OTF:LDYD2W	EQU	OTF:LDYD2!OTF:2BYTE
OTF:LEA	EQU	OTF:GEN!OTF:IND	Load effective address setup
OTF:GENIMM	EQU	OTF:GEN!OTF:IMM

OTM:MSK	EQU	OTF:DYD	MASK FOR FLAGS
OTM:TYPE	EQU	$03	TYPE MASK FOR TYPE WORD
OTM:CLAS	EQU	$70	CLASS MASK FOR TYPE WORD

	PAGE
*+
*	INDEXING POST BYTE DEFINITIONS
*-

PB:AI2	EQU	$81	Auto post increment 2 bytes
PB:AI1	EQU	$80	Auto post increment 1 byte
PB:AD2	EQU	$83	Auto pre-decrement 2 bytes
PB:AD1	EQU	$82	Auto pre-decrement 1 byte
PB:ZIDX	EQU	$84	Zero index
PB:5BIT	EQU	$0	5 bit offset
PB:8BIT	EQU	$88	8 bit offset
PB:16BIT	EQU	$89	16 bit offset
PB:PCR8	EQU	$8C	8 bit PCR offset
PB:PCR16	EQU	$8D	16 bit PCR offset
PB:AOFF	EQU	$86	A-reg offset
PB:BOFF	EQU	$85	B-reg offset
PB:DOFF	EQU	$8B	D-reg offset
PB:XIND	EQU	$9F	Extended Indirect
PB:INDIR	EQU	$10	Indirect Bit
PB:AMSK	EQU	$11	Illegal indirect auto inc/dec mask
	PAGE	OPCODE TABLE
OPTBL	FCC	'ABX '
	#OTF:NIL,$3A
	FCC	'ADCA'
	#OTF:DYD,$89
	FCC	'ADCB'
	#OTF:DYD,$C9
	FCC	'ADCD'
	#OTF:DYD!OTF:68C,$00
	FCC	'ADDA'
	#OTF:DYD,$8B
	FCC	'ADDB'
	#OTF:DYD,$CB
	FCC	'ADDD'
	#OTF:DYDW,$C3
	FCC	'ANDA'
	#OTF:DYD,$84
	FCC	'ANDB'
	#OTF:DYD,$C4
	FCC	'ANDC'
	#OTF:IMM,$1C
	FCC	'ANDD'
	#OTF:DYD!OTF:68C,$01
	FCC	'ASL '
	#OTF:MON,$08
	FCC	'ASLA'
	#OTF:NIL,$48
	FCC	'ASLB'
	#OTF:NIL,$58
	FCC	'ASR '
	#OTF:MON,$07
	FCC	'ASRA'
	#OTF:NIL,$47
	FCC	'ASRB'
	#OTF:NIL,$57
	FCC	'BCC '
	#OTF:SBR,$24
	FCC	'BCS '
	#OTF:SBR,$25
	FCC	'BEQ '
	#OTF:SBR,$27
	FCC	'BEQD'
	#OTF:SBR,$27
	FCC	'BGE '
	#OTF:SBR,$2C
	FCC	'BGT '
	#OTF:SBR,$2E
	FCC	'BHI '
	#OTF:SBR,$22
	FCC	'BHS '
	#OTF:SBR,$24
	FCC	'BITA'
	#OTF:DYD,$85
	FCC	'BITB'
	#OTF:DYD,$C5
	FCC	'BLE '
	#OTF:SBR,$2F
	FCC	'BLO '
	#OTF:SBR,$25
	FCC	'BLS '
	#OTF:SBR,$23
	FCC	'BLT '
	#OTF:SBR,$2D
	FCC	'BMI '
	#OTF:SBR,$2B
BNEOPCODE EQU	*+6
	FCC	'BNE '
	#OTF:SBR,$26
	FCC	'BNED'
	#OTF:SBR,$26
	FCC	'BPL '
	#OTF:SBR,$2A
	FCC	'BRA '
	#OTF:SBR,$20
	FCC	'BRN '
	#OTF:SBR,$21
	FCC	'BSR '
	#OTF:SBR,$8D
	FCC	'BVC '
	#OTF:SBR,$28
	FCC	'BVS '
	#OTF:SBR,$29
	FCC	'CLR '
	#OTF:MON,$0F
	FCC	'CLRA'
	#OTF:NIL,$4F
	FCC	'CLRB'
	#OTF:NIL,$5F
	FCC	'CMPA'
	#OTF:DYD,$81
	FCC	'CMPB'
	#OTF:DYD,$C1
	FCC	'CMPD'
	#OTF:LDYD1W,$83
	FCC	'CMPS'
	#OTF:LDYD2W,$8C
	FCC	'CMPU'
	#OTF:LDYD2W,$83
	FCC	'CMPX'
	#OTF:DYDW,$8C
	FCC	'CMPY'
	#OTF:LDYD1W,$8C
	FCC	'COM '
	#OTF:MON,$03
	FCC	'COMA'
	#OTF:NIL,$43
	FCC	'COMB'
	#OTF:NIL,$53
	FCC	'CPX '
	#OTF:DYDW,$8C
	FCC	'CWAI'
	#OTF:GENIMM,$3C
	FCC	'DAA '
	#OTF:NIL,$19
DECOPCODE EQU	*+6
	FCC	'DEC '
	#OTF:MON,$0A
	FCC	'DECA'
	#OTF:NIL,$4A
	FCC	'DECB'
	#OTF:NIL,$5A
	FCC	'DECD'
	#OTF:MON!OTF:68C,$06
	FCC	'EORA'
	#OTF:DYD,$88
	FCC	'EORB'
	#OTF:DYD,$C8
	FCC	'EORD'
	#OTF:DYD!OTF:68C,$02
	FCC	'EXG '
	#OTF:RR,$1E
INCOPCODE EQU	*+6
	FCC	'INC '
	#OTF:MON,$0C
	FCC	'INCA'
	#OTF:NIL,$4C
	FCC	'INCB'
	#OTF:NIL,$5C
	FCC	'INCD'
	#OTF:MON!OTF:68C,$05
	FCC	'JMP '
	#OTF:MON,$0E
	FCC	'JSR '
	#OTF:MON,$8D
	FCC	'LBCC'
	#OTF:LBR,$24
	FCC	'LBCS'
	#OTF:LBR,$25
	FCC	'LBEQ'
	#OTF:LBR,$27
	FCC	'LBGE'
	#OTF:LBR,$2C
	FCC	'LBGT'
	#OTF:LBR,$2E
	FCC	'LBHI'
	#OTF:LBR,$22
	FCC	'LBHS'
	#OTF:LBR,$24
	FCC	'LBLE'
	#OTF:LBR,$2F
	FCC	'LBLO'
	#OTF:LBR,$25
	FCC	'LBLS'
	#OTF:LBR,$23
	FCC	'LBLT'
	#OTF:LBR,$2D
	FCC	'LBMI'
	#OTF:LBR,$2B
	FCC	'LBNE'
	#OTF:LBR,$26
	FCC	'LBPL'
	#OTF:LBR,$2A
	FCC	'LBRA'
	#OTF:SBR!OTF:2BYTE,$16
	FCC	'LBRN'
	#OTF:LBR,$21
	FCC	'LBSR'
	#OTF:SBR!OTF:2BYTE,$17
	FCC	'LBVC'
	#OTF:LBR,$28
	FCC	'LBVS'
	#OTF:LBR,$29
	FCC	'LDA '
	#OTF:DYD,$86
	FCC	'LDAA'
	#OTF:DYD,$86
	FCC	'LDAB'
	#OTF:DYD,$C6
	FCC	'LDB '
	#OTF:DYD,$C6
	FCC	'LDD '
	#OTF:DYDW,$CC
	FCC	'LDS '
	#OTF:LDYD1W,$CE
	FCC	'LDU '
	#OTF:DYDW,$CE
	FCC	'LDX '
	#OTF:DYDW,$8E
	FCC	'LDY '
	#OTF:LDYD1W,$8E
	FCC	'LEAS'
	#OTF:LEA,$32
	FCC	'LEAU'
	#OTF:LEA,$33
	FCC	'LEAX'
	#OTF:LEA,$30
	FCC	'LEAY'
	#OTF:LEA,$31
	FCC	'LSL '
	#OTF:MON,$08
	FCC	'LSLA'
	#OTF:NIL,$48
	FCC	'LSLB'
	#OTF:NIL,$58
	FCC	'LSR '
	#OTF:MON,$04
	FCC	'LSRA'
	#OTF:NIL,$44
	FCC	'LSRB'
	#OTF:NIL,$54
	FCC	'MUL '
	#OTF:NIL,$3D
	FCC	'NEG '
	#OTF:MON,$00
	FCC	'NEGA'
	#OTF:NIL,$40
	FCC	'NEGB'
	#OTF:NIL,$50
	FCC	'NOP '
	#OTF:NIL,$12
	FCC	'ORA '
	#OTF:DYD,$8A
	FCC	'ORAA'
	#OTF:DYD,$8A
	FCC	'ORAB'
	#OTF:DYD,$CA
	FCC	'ORB '
	#OTF:DYD,$CA
	FCC	'ORCC'
	#OTF:GENIMM,$1A
	FCC	'ORD '
	#OTF:DYD!OTF:68C,$03
	FCC	'PSHS'
	#OTF:PP,$34
	FCC	'PSHU'
	#OTF:PP,$36
	FCC	'PULS'
	#OTF:PP,$35
	FCC	'PULU'
	#OTF:PP,$37
	FCC	'ROL '
	#OTF:MON,$09
	FCC	'ROLA'
	#OTF:NIL,$49
	FCC	'ROLB'
	#OTF:NIL,$59
	FCC	'ROR '
	#OTF:MON,$06
	FCC	'RORA'
	#OTF:NIL,$46
	FCC	'RORB'
	#OTF:NIL,$56
	FCC	'RTI '
	#OTF:NIL,$3B
	FCC	'RTS '
	#OTF:NIL,$39
	FCC	'SBCA'
	#OTF:DYD,$82
	FCC	'SBCB'
	#OTF:DYD,$C2
	FCC	'SBCD'
	#OTF:DYD!OTF:68C,$04
	FCC	'SEX '
	#OTF:NIL,$1D
	FCC	'STA '
	#OTF:DYD!!OTF:IMM,$87
	FCC	'STAA'
	#OTF:DYD!!OTF:IMM,$87
	FCC	'STAB'
	#OTF:DYD!!OTF:IMM,$C7
	FCC	'STB '
	#OTF:DYD!!OTF:IMM,$C7
	FCC	'STD '
	#OTF:DYD!!OTF:IMM,$CD
	FCC	'STS '
	#OTF:LDYD1!!OTF:IMM,$CF
	FCC	'STU '
	#OTF:DYD!!OTF:IMM,$CF
	FCC	'STX '
	#OTF:DYD!!OTF:IMM,$8F
	FCC	'STY '
	#OTF:LDYD1!!OTF:IMM,$8F
	FCC	'SUBA'
	#OTF:DYD,$80
	FCC	'SUBB'
	#OTF:DYD,$C0
	FCC	'SUBD'
	#OTF:DYDW,$83
	FCC	'SWI '
	#OTF:NIL,$3F
	FCC	'SWI2'
	#OC:210!OTF:NIL,$3F
	FCC	'SWI3'
	#OC:211!OTF:NIL,$3F
	FCC	'SYNC'
	#OTF:NIL,$13
	FCC	'TFR '
	#OTF:RR,$1F
TSTOPCODE EQU	*+6
	FCC	'TST '
	#OTF:MON,$0D
	FCC	'TSTA'
	#OTF:NIL,$4D
	FCC	'TSTB'
	#OTF:NIL,$5D

	#-1		END OF OPCODE TABLE
	PAGE	LONG OPCODE TABLE

*
*	LONG OPCODE TABLE (>4 CHARACTERS)
*
*	Table format is the same as the symbol table
*
M6809LONG
	#M6809LONGLIST	LONG OPCODE LIST HEADER

M6809LONGLIST
	#M6809LONGEND
	#OTF:GENIMM,$1C,5
	FCC	"ANDCC"

M6809LONGEND EQU	0

	PAGE	MICRO/MACRO TABLE

*	The Micro/Macro table is used for macro definitions and for 6800
*	code equivalents.
*
*
*	The table entries are laid out the same as the symbol table.
*
MM:FLAG	EQU	S:VAL	0=Macro; <>0 = Length of Micro sequence
MM:PTR	EQU	S:VAL+1	Pointer to Micro sequence or Macro form
*
*           +----------------------+
*           |                      |
*           +         Link         +
*           |                      |
*           +----------------------+
*           |  Micro / Macro Flag  |  MM:FLAG
*           +----------------------+
*           |   Pointer to Micro   |
*           +      sequence or     +  MM:PTR
*           |      Macro form      |
*           +----------------------+
*           |  Length of Name text |
*           +----------------------+
*           |        Name          |
*           +        text          +
*           <          :           <
*           >                      >
*           +----------------------+

MICMAC	#.ABA		Micro/Macro table head

.ABA	#.ASLD,4,:ABA,3
	FCC	'ABA'
:ABA	$34,$04,$AB,$E0	PSHS B; ADDA ,S+

.ASLD	#.ASRD,2,:ASLD,4
	FCC	'ASLD'	ASLB; ROLA
:ASLD	$58,$49
	PAGE
.ASRD	#.CBA,2,:ASRD,4
	FCC	'ASRD'	ASRA; RORB
:ASRD	$47,$56

.CBA	#.CLC,4,:CBA,3
	FCC	'CBA'
:CBA	$34,$04,$A1,$E0	PSHS B; CMPA ,S+

.CLC	#.CLI,2,:CLC,3
	FCC	'CLC'
:CLC	$1C,$FE		ANDCC #$FE

.CLI	#.CLV,2,:CLI,3
	FCC	'CLI'
:CLI	$1C,$EF		ANDCC #$EF

.CLV	#.COMD,2,:CLV,3
	FCC	'CLV'
:CLV	$1C,$FD		ANDCC #$FD

.COMD	#.DES,2,:COMD,4
	FCC	'COMD'
:COMD	$53,$43		COMB; COMA

.DES	#.DEX,2,:DES,3
	FCC	'DES'
:DES	$32,$7F		LEAS -1,S

.DEX	#.ERRORRTS,2,:DEX,3
	FCC	'DEX'
:DEX	$30,$1F		LEAX -1,X

.ERRORRTS #.INS,3,:ERRORRTS,8
	FCC	'ERRORRTS'
:ERRORRTS $1A,$01,$39	ORCC #1; RTS

.INS	#.INX,2,:INS,3
	FCC	'INS'
:INS	$32,$61		LEAS 1,S
	PAGE
.INX	#.LSLD,2,:INX,3
	FCC	'INX'
:INX	$30,$01		LEAX 1,X

.LSLD	#.LSRD,2,:ASLD,4
	FCC	'LSLD'	(See ASLD)

.LSRD	#.NEGD,2,:LSRD,4
	FCC	'LSRD'
:LSRD	$44,$56		LSRA; RORB

.NEGD	#.OKRTS,4,:NEGD,4
	FCC	'NEGD'
:NEGD	$40,$50,#$8200	NEGA; NEGB; SBCA #0

.OKRTS	#.PSHA,3,:OKRTS,5
	FCC	'OKRTS'
:OKRTS	#$1CFE,$39	ANDCC #$FE; RTS

.PSHA	#.PSHB,2,:PSHA,4
	FCC	'PSHA'
:PSHA	$34,$02		PSHS A

.PSHB	#.PSHD,2,:PSHB,4
	FCC	'PSHB'
:PSHB	$34,$04		PSHS B

.PSHD	#.PSHX,2,:PSHD,4
	FCC	'PSHD'
:PSHD	$34,$06		PSHS D

.PSHX	#.PULA,2,:PSHX,4
	FCC	'PSHX'
:PSHX	$34,$10		PSHS X

.PULA	#.PULB,2,:PULA,4
	FCC	'PULA'
:PULA	$35,$02		PULS	A
	PAGE
.PULB	#.PULD,2,:PULB,4
	FCC	'PULB'
:PULB	$35,$04		PULS B

.PULD	#.PULX,2,:PULD,4
	FCC	'PULD'
:PULD	$35,$06		PULS D

.PULX	#.ROLD,2,:PULX,4
	FCC	'PULX'
:PULX	$35,$10		PULS X

.ROLD	#.RORD,2,:ROLD,4
	FCC	'ROLD'
:ROLD	$59,$49		ROLB; RORA

.RORD	#.SBA,2,:RORD,4
	FCC	'RORD'
:RORD	$46,$56		RORA; RORB

.SBA	#.SEC,4,:SBA,3
	FCC	'SBA'
:SBA	$34,$04,$A0,$E0	PSHS B; SUBA ,S+

.SEC	#.SEI,2,:SEC,3
	FCC	'SEC'
:SEC	$1A,$01		ORCC #$01

.SEI	#.SEV,2,:SEI,3
	FCC	'SEI'
:SEI	$1A,$10		ORCC #$10

.SEV	#.TAB,2,:SEV,3
	FCC	'SEV'
:SEV	$1A,$02		ORCC #$02

.TAB	#.TAP,3,:TAB,3
	FCC	'TAB'
:TAB	$1F,$89,$4D	TFR A,B; TSTA
	PAGE
.TAP	#.TBA,2,:TAP,3
	FCC	'TAP'
:TAP	$1F,$8A		TFR A,CC

.TBA	#.TDS,3,:TBA,3
	FCC	'TBA'
:TBA	$1F,$98,$4D	TFR B,A; TSTA

.TDS	#.TDX,2,:TDS,3
	FCC	'TDS'
:TDS	#$1F04		TFR D,S

.TDX	#.TPA,2,:TDX,3
	FCC	'TDX'
:TDX	$1F,$01		TFR D,X

.TPA	#.TSD,2,:TPA,3
	FCC	'TPA'
:TPA	$1F,$A8		TFR CC,A

.TSD	#.TSTD,2,:TSD,3
	FCC	'TSD'
:TSD	$1F,$40		TFR S,D

.TSTD	#.TSX,3,:TSTD,4
	FCC	'TSTD'
:TSTD	$83,$00,$00	SUBD #$0000

.TSX	#.TXD,2,:TSX,3
	FCC	'TSX'
:TSX	$1F,$41		TFR S,X

.TXD	#.TXS,2,:TXD,3
	FCC	'TXD'
:TXD	$1F,$10		TFR X,D

.TXS	#.WAI,2,:TXS,3
	FCC	'TXS'
:TXS	$1F,$14		TFR X,S
	PAGE
.WAI	#MICMACEND,2,:WAI,3
	FCC	'WAI'
:WAI	$3C,$FF		CWAI #$FF

MICMACEND	EQU	0

