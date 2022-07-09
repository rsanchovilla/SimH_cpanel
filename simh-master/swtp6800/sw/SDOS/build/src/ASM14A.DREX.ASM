	PAGE	- - -  D O I T D R  - - -
*
*
*   D O I T D R
*
*        PROCESSES DIRECTIVES...
*        ASSUMES THAT SYM CONTAINS THE SYMBOL IN CF(1) AND
*        THAT CF(1) IS A SYMBOL ONLY.
*
DOITDR   EQU      *
         LDX      #D0               GET HEAD OF ASM-125 CHAIN
         JSR      FSYM1             AND SEE IF ITS A SYSTEM DIRECTIVE
         BNE      DOITMM            B/ MUST BE A FUNNY DATA STMT OR MACRO
*
DIR1     LDX      SYMPT             GET POINTER TO DIRECTIVE'S BLOCK
         STX      DIRPT             SAVE FOR LATER.
         LDAA     S:TYPE,X          GET TYPE BITS,
         BITA     #D:LF             EQUATE LF TO PC?
         BEQ      DIR2              B/ NO...
         PSHA                       SAVE TYPE,
         JSR      LFPC              LF=PC
         PULA                       RESTORE TYPE.
*
DIR2     BITA     #D:AF             FIND AF ?
         BEQ      DIR31             B/ NO...
         PSHA                       SAVE TYPE,
         JSR      FINDAF            FIND ARG FIELD,
         PULA                       RESTORE TYPE.
*
DIR31    BITA     #D:DO
         BEQ      DIR3              NOT A DOLVL INCREMENTER.
         INC      DOLVL
         BNE      DIR3
         DEC      DOLVL
         PSHA
         JSR      NSTER
         PULA
*
DIR3     BITA     #D:CC             IS THIS A CONTROL CARD?
         BEQ      DIR32	B/ NO...
         LDAB     #LF:CC            ELSE FLAG AS SUCH
         ORAB     LIST:T
         STAB     LIST:T
*
DIR32	BITA	#D:CA	IS THIS A CONDITIONAL ASSEMBLY LINE?
	BEQ	DIR33	  B/ NO, GO ON.
	LDAB	#LF:CA	ELSE MARK THIS LINE FOR LISTER
	ORAB	LIST:T	  SO IT WILL CHECK WHETHER TO LIST IT.
	STAB	LIST:T
*
DIR33	BITA	#D:SI	Is this a directive we suppress during
	BEQ	DIR4	  an include?
	LDAB	INCLVL	Yup:  Are we doing an INCLUDE?
	BEQ	DIR4	  B/ No:  do the directive.
	JSR	MRKSKP	Yes:  Mark as skipped;
	BRA	DIR5	  and skip the processing.
*
DIR4     LDX      DIRPT             MAKE SURE WE HAVE PTR TO SYM,
         LDX      S:VAL,X           GET ADDRESS OF ROUTINE,
	PSHA		SAVE A
         JSR      0,X               CALL IT.
	PULA		GET BACK FLAGS
	BITA	#D:AF	DID HE WANT AN AF?
	BEQ	DIR5	IF NOT, DON'T BOTHER
	BITA	#D:SS	DO WE CHECK SYNTAX?
	BNE	DIR5	IF NOT, OK
	JSR	ECKEOF	CK FOR END OF FIELD
DIR5	JMP	LIST
	PAGE
*
*
*  D O I T M M
*
*	Does Micro expansions
*
*
*	
DOITMM
	IFUND	MC6809
*	JMP	DOITEX
	ELSE
	JSR	LFPC	Set LF to value of PC
	JSR	CHCFSYM	Rescan command field
	LDX	#MICMAC	Head of Micro/Macro table
	JSR	FSYM1	Scan table for symbol
	BNE	DOITEX	B/Not found
	STX	OPPTR
	LDAB	MM:FLAG,X	

DOITMICRO
	STAB	VAL	Save # of micro bytes
	LDX	MM:PTR,X	Load byte sequence pointer
DOITMICROL
	LDAA	0,X
	INX
	STX	OPPTR	Pointer into micro sequence
	JSR	PUTB	Output it
	LDX	OPPTR
	DEC	VAL	DONE?
	BNE	DOITMICROL	B/No
	JMP	LIST
	FIN
	PAGE	- - -  D O I T E X  - - -
*
*
*  D O I T E X
*
*        TAKES LINES OF FORM
*        <EXP>,<EXP>,<EXP>,<EXP>
*        AND EVALUATES THEM.  IF THE VALUE IS BETWEEN
*        0 AND 255, IT IS STUCK INTO 1 BYTE; OTHERWISE
*        IT IS HOSED INTO TWO.
*
*
DOITEX   EQU      *
*
         DO       NICE=1

         JSR	  LFPC	Set LF to value of PC 		; ZZZ Added by Roberto Sancho Jun 2022
         						; In order to allow assemble "LABEL NNN" lines
         						; as generaed by basic compiler

         LDX      CFPTR             GET POINTER TO CF(1)
         STX      EXPTR             INITIALIZE EXPTR,
         CLR      RJOBJ             NO REJECTED OBJECT.
*
DOEXPL	EQU	*	EXPRESSION SCANNING LOOP.
	JSR	GETXC	FIND ANY QUALIFIERS,
	JSR	WHICH	  AND FORK ON SAME.

	'+,DOEX1B	LEADING PLUS ==> ONE BYTE.
	'-,DOEX1B	LEADING MINUS ==> ONE BYTE.
	'#,DOEX2B	LEADING HASH ==> TWO BYTES.
	0		   <END OF LIST>

*** HANDLE IN DEFAULT FASHION.
	JSR	REJXC	IT'S A GOOD IDEA TO THROW BACK WHAT YOU CANT EAT.
	JSR	EVNUL	EVALUATE 1ST FIELD,
	LDAA	VAL	PICK UP HIGH BYTE, IN CASE WE DECIDE TO PUT IT.
	LDAB	XTYPE	IF EXPRESSION IS FWD-REF OR UNDEF,
	BITB	#XT:FREF!XT:UNDF	  THEN WE WILL ALWAYS ASSIGN 2 BYTES.
	BNE	DOEXP0	B/ PUT 2 BYTES.
	TSTA		OTHERWISE, WE DONT PUT H(VAL) UNLESS
			; IT'S NON-ZERO.
         BEQ      DOEXP1            B/ ONLY 1.
DOEXP0	JSR      PUTB              ELSE OUTPUT HIGH.
DOEXP1   LDAA     VAL+1             GET LOW BYTE
         JSR      PUTB              AND OUTPUT IT.
         JSR      GTCMA             FIND A COMMA
         BVS      DOEXPL            B/ FOUND ONE.
         JMP      LIST              ELSE ALL DONE: SPLIT.


*** GENERATE IN ONE BYTE ONLY.
DOEX1B	EQU	*
	JSR	REJXC	GIVE SIGN BACK TO EXPRESSION EVALUATOR,
	JSR	EVAL	  YUM...
	LDAA	VAL	CHECK FOR VALUE IN RANGE.
	BEQ	DOEXP1	  B/ 0 =< VAL =< $FF, FINE.
	CMPA	#$FF	(HIGH BYTE MUST BE ALL ONES)
	BNE	DOEX1B1	  B/ NOPE -- OVERFLOW.
	LDAB	VAL+1	(IF SIGN IS ON, SIGN MUST BE ON IN LOW BYTE.)
	BMI	DOEXP1	  B/ FAR OUT.  VALUE IS GOOD.

DOEX1B1	EQU	*
	JSR	OVRFLW	*E* REPORT OVERFLOW.
	BRA	DOEXP1	  BUT GENERATE IN ONE BYTE ANYWAY.


*** GENERATE TWO BYTES FOR "#<EXP>"
DOEX2B	EQU	*
	JSR	EVAL	EAT THE EXPRESSION (NOTE THAT NULL EXP IS BAD HERE).
	LDAA	VAL	GET BYTE TO BE OUTPUT,
	BRA	DOEXP0	AND OUTPUT TWO BYTES.
         ELSE                       YOU LOSE, BUCKWHEAT...
         JSR      SYNTAX            BARF.
         JMP      LIST
         FIN
