	TITLE	- - - E X P R E S S I O N S - - -
	PAGE	- - - E X P R E S S I O N   E V A L U A T I O N - - -
*    E X P R E S S I O N   E V A L U A T I O N
*
*
*  EVAL --
*
*        EATS EXP POINTED TO BY EXPTR.
*
*  EVAF --
*
*        EATS EXP POINTED TO BY AFPTR.  IF AFPTR IS ZERO, RETURNS
*        XTYPE:=0, VAL:=0.
*
*  THE FOLLOWING OPERATORS ARE RECOGNIZED:
*
*        DYADIC   MONADIC           WIERD
*        ------   -------           -----
*         +         +                 (
*         -         -                 )
*         *         \ (LOG. COMP)
*         /         & (TEST INV.)
*	## (SHIFT)
*         !
*         &
*         !!
*         //  (COVERED QUOTIENT)
*         \   (REMAINDER)
*         #   (.NE.)
*         <   (.LT.)
*         >   (.GT.)
*         <=  (.LE.)
*         =<  (.LE.)
*         >=  (.GE.)
*         =>  (.GE.)
*         =   (.EQ.)
*         <<  (.LO.)
*         >>  (.HI.)
*         \<  (.LS.)
*	 >/  (.HS.)
*
*   PRECEDENCE:
*   -----------
*
*   1.	UNARY  +,-,\,&
*   2.	##
*   3.	*,/,//,\
*   4.	+,-
*   5.	RELATIONALS
*   6.	&,!,!!
*
         PAGE
*20*
*
*  OBJECTS:
*  --------
*
*        1.  SYMBOLS (START W/ A-Z;  CONTAIN A-Z, 0-9, :, %, $, @
*        2.  $XXXXX  (HEX NUMBER)
*        3.  @OOOOO  (OCTAL NUMBER)
*        4.  %BBBBB  (BINARY NUMBER)
*        5.  DDDDDD  (DECIMAL NUMBER)
*        6.  "CC"    (STRING)
*        7.  'C      (ASCII CHARACTER)
*        8.  DXXXXH  (HEX NUMBER)
*        9.  00000O  (OCTAL NUMBER)
*       10.  00000Q  (OCTAL NUMBER)
*       11.  00000B  (BINARY NUMBER)
*
         PAGE     - - - E V A F  /  E V A L - - -
*47*
*
* EVAF
*	EVALUATE AF IF NO AF - SYNTAX ERROR
*
* AFPTR PTS TO THING TO EVAL
*
EVAF     LDX      AFPTR             GET POINTER,
EVAFX    STX      EXPTR             AND SHOVE IT.
         BEQ      EVAF9             B/ NOTHING TO DO...
         CLR      RJOBJ             NO REJECTED OBJECT
         BRA      EVAF1             B/ POINTER IS GOOD; EVAL EXP.
EVAF9	CLRA		ZERO A BUNCH'A CELLS.
	STAA	VAL	LIKE, VALUE,
	STAA	VAL+1	  SECOND BYTE,
	STAA	XTYPE	  EXPRESSION TYPE.
	JMP	ILLAF	SYN ERR IF NO AF
*
EVAF1	JSR	GNOBJ	PEEK AT FIRST TOKEN.
	INC	RJOBJ	  (I SHOULD'A PUT IN A PEEK-NEXT.)
	CMPA	#O:TERM	IS ANYTHING HERE TO EVALUATE?
	BEQ	EVAF9	  B/ NO.  ERROR, CLEAR VALUE AND SPLIT.

	BSR	EVAL	OK.  GO SEE WHAT HAPPENS.
	TST	EVAFBIAS	Bias evaluator output?
	BEQ	EVAF3	B/No
	LDX	VAL
	INX		Bias expression value by +1
	STX	VAL
	CLR	EVAFBIAS	Reset bias flag
EVAF3	LDX      EXPTR             GET EXPRESSION POINTER,
EVRTS    RTS                        AND GO HOME.
*
*
EVAL     BSR      EXP               EVALUATE THE EXPRESSION.
         JSR      GNOBJ             GET THE TRAILING OBJECT
         INC      RJOBJ             AND REJECT IT.
         CMPA     #O:TERM           IS IT A TERMINATOR?
         BEQ      EVRTS             B/ YES:  THIS IS OK.
         JMP      SYNTAX            ELSE FLAG ERROR.
         PAGE     - - - E V A F C T - - -
*
* EVAFCT --
*        FIND AF, EVALUATE IT, AND CROAK IF AF IS A FWD REF, ETC.
*
EVAFCT   JSR      FINDAF            FIND THE ARG FIELD.
         BSR      EVAF              EVALUATE,
	BRA	ISCTR
*
*
*  E V A L C T
*		EVALUATE AND CHECK TYPE
*
*  IF TYPE = FREF OR UNDEF THEN GIVE ILL VAL ERR AND
* RETURN WITH Z#0
*
*
*
EVALCT   EQU      *
         BSR      EVAL
*
*
*  I S C T R
*
*
ISCTR    LDAA     XTYPE             GET THE EXPR. TYPE,
         BITA     #XT:FREF!XT:UNDF	IS IT A FORWARD REF?
         BEQ      EVRTS             B/ NO ==> ITS OK.
         CLRA                       BAD EXP:
         STAA     VAL               VAL := 0
         STAA     VAL+1
	STAA	XTYPE	REMEMBER TO ZAP THE TYPE TO NICENESS.
         JSR      ILLVAL            *E* ILLEGAL VALUE.
         INCA                       SET CCZ
         RTS                        AND RETURN.
	PAGE	- - - E X P - - -
*20*
*
*   EXP:
*
*        THE EXPRESSION EVALUATOR IS A FINITE STATE MACHINE.
*
*        ST0:     PUSH OP:BOX; ==> ST1.
*
*        ST1:     READ:  O:VAL ==> ST2; O:OP ==> ST4; '( ==> ST5;
*                        O:TERM ==> ST8.
*
*        ST2:     PUSH VALUE;
*                 READ:  O:OP ==> ST3; O:VAL ==> ERROR; OTHERWISE ST7.
*
*        ST3:     IF PRECEDENCE (OP) > PRECEDENCE(STACKED-OP) THEN
*                    PUSH OP, ==> ST1
*                 ELSE EXECUTE STACKED-OP, ==> ST3
*
*        ST4:     IF OP = * THEN VAL:=PC, ==> ST2
*                 IF OP = +,-,&,\ THEN MAKE MONADIC OP, ==> ST3
*                 ELSE SYNTAX ERROR.
*
*        ST5:     RECURSIVELY CALL EXP. ==> ST6
*
*        ST6:     READ '), ==> ST2
*
*        ST7:     SET OP TO OP:EOX, ==>ST3
*
*        ST8:     IS STACKED-OP OP:BOX?  YES, VAL:=0, ==> ST7
*                 NO ==> SYNTAX ERROR.
*
         PAGE
*20*
*
*  EXP IS THE ENTRY WHICH SAVES THE STACK POINTER IN FLAMOUT, SO
*  WE CAN ABORT IN CASE OF ERROR.  THE LEFT PAREN HANDLER CALLS
*  EXP THROUGH EXP:R.
*
*  EXPSE IS THE ABORT EXIT.  IT LOADS THE STACK POINTER FROM
*  FLAMOUT AND LEAVES VAL AND XTYPE AS THEY STAND.  IT EXITS THRU
*  THE SYNTAX ERROR REPORTER.
*
*
EXP      STS      FLAMOUT           SAVE THE STACK POINTER.
         CLRA                       ZAP A FEW CELLS...
         STAA     XTYPE             EXPRESSION TYPE,
*
EXP:R    LDAA     #OP:BOX           PUSH BEGINNING OF EXP MARKER.
         PSHA
EXST1    BSR      GNOB:SJ           SORT-JUMP ON NEXT OBJECT
         FCB      +O:NUM            NUMBER?
         FDB      EXST2             NUMBER ==> STATE 2
*
         DO       STRINGS=1
         FCB      +O:STR
         FDB      EXST2S            STRING ==> ST2 (STRING)
         FIN
*
         FCB      +O:SYM
         FDB      EXST2SY           SYM ==> ST2(SYM)
         FCB      +O:OP
         FDB      EXST4             UNARY OP ==> ST4
         FCB      +O:LP
         FDB      EXST5             '( ==> ST5
         FCB      +O:TERM
         FDB      EXST8             TERMINATOR ==> ST8
         FCB      0
*
* SYNTAX ERROR
EXPSE    LDS      FLAMOUT           RESTORE STACK POINTER,
	CLRA		CLEAR THE VALUE IF ERROR
	STAA	VAL
	STAA	VAL+1
	INC	RJOBJ	SAY 'I DIDN'T SEE THE LAST OBJECT'
         JMP      SYNTAX            AND EXIT GIVING SYNTAX ERROR
*
* SAW A STRING:
         DO       STRINGS=1
EXST2S   EQU      *
         CLRB                       CLEAR OVERFLOW BYTE...
         LDAA     STBUF             GET LENGTH OF STRING:
         CMPA     #2                MAXIMUM FOR SIXTEEN BITS,
         BLT      EST2S1            B/ ONE CHARACTER.
         BEQ      EST2S2            B/ TWO CHARACTERS.
         JSR      OVRFLW            *E* STRING IN EXPRESSION TOO LONG
EST2S2   LDAA     STBUF+2           GET L(VAL) = STRING(2)
         LDAB     STBUF+1           MAKE H(VAL) := STRING(1)
         BRA      EXST21            GO SAVE VALUE.
* ONE OR NO CHARACTERS...
EST2S1   LDAA     STBUF+1           H(VAL)=0, L(VAL)= STRING(1)
         BRA      EXST21            GO SAVE VALUE
         FIN
	SPACE	3
*
* SAW A SYMBOL:
EXST2SY  JSR      SYMLK             LOOK IT UP.
	LDAA	TYPE	MERGE TYPE INTO XTYPE.
	ANDA	#XT:FREF!XT:UNDF	  BUT ONLY THESE BITS SHOULD
	ORAA	XTYPE	  BE CARRIED, AS OTHERWISE THINGS CAN GET
	STAA	XTYPE	  VERY CONFUSED, IN PARTICULAR EQU'S AND SET'S.
	SPACE	3
*
* SAW A VALUE:
EXST2    LDAA     VAL+1             GET VALUE,
         LDAB     VAL
EXST21   PSHA                       A=L(VAL), B=H(VAL)
         PSHB
         BSR      GNOB:SJ          READ AND FORK
         FCB      +O:OP
         FDB      EXST31            OP ==> TRY AND EXECUTE IT (JUST TRY)
         FCB      +O:NUM
         FDB      EXPSE
         FCB      +O:SYM
         FDB      EXPSE
*
         DO       STRINGS=1
         FCB      +O:STR
         FDB      EXPSE
         FIN
*
         FCB      $0

*  END OF EXPRESSION
EXST7    INC      RJOBJ             MARK AS REJECTED.
         LDAB     #OP:EOX           GET CORRECT OPERATOR (END OF EXP)
EXST31   STAB     CUROP             SAVE THE OPCODE IN A HANDY PLACE.
*
*  OPERATOR SEEN:
EXST3    TSX                        GET INDEX INTO STACK:
         LDAB     CUROP             GET NEW OPERATOR,
         ANDB     #$F0              THROW AWAY LSB FOR PRECEDENCE COMPARE
         CMPB     X:OP,X            DO THE COMPARE.
*21*
         BLS      ES3XO             B/ GO EXECUTE THE OPERATOR
*
*  THIS OPERATOR HAS HIGHER PRECEDENCE:  STACK IT.
         LDAA     OPTF              GET OPTION FLAGS:
         BITA     #OPT:MCM          MCM/MOTOROLA OPERATOR PRECEDENCE?
         BNE      ES3XO             B/ YES:  GO DO THAT OPERATOR.
ES3SO    LDAB     CUROP             GET THE OPERATOR
ES3SO1   PSHB                       PUT IT ON THE STACK,
         BRA      EXST1             AND GO READ AN OPERAND.

*
* STATE 5 --
*	LEFT PAREN HAS BEEN SEEN.  CALL THE EXPRESSION EVALUATOR
*	(THAT'S US, GUYS) TO EVALUATE THE ENCLOSED EXPRESSION.  THEN
*	IF THE CLOSING PAREN IS MISSING, BITCH.
*
EXST5    BSR      EXP:R             REENTER EXPRESSION
         JSR      GNOBJ
         CMPA     #O:RP             RIGHT PAREN?
         BEQ      EXST2             GO, BABY, GO.
         BRA      EXPSE             B/ ANYTHING ELSE ==> ERROR.
*
*
GNOB:SJ  JSR      GNOBJ
         JMP      WHICH
**
*
*
*  SAW A MONADIC OP
EXST4    CMPB     #OP:MUL           IS IT A '*'?
         BEQ      EXPC              RETURN PC
         TBA
         JSR      ISIT              IS IT A VALID MONADIC OP?
         FCB      OP:ADD,OP:SUB,OP:AND,OP:MOD,0
EXPSE1   BNE      EXPSE             B/ NO.  DIE.
         ORAB     #P:UN             GET PROPER PRECEDENCE,
         BRA      ES3SO1            AND GO STACK IT.
*
* SAW * IN CONTEXT WHICH MEANS 'PC'
*
EXPC
	IFUND	MC6809
	ELSE
	LDX	EXPTR	Backstep to
	DEX		 check for
	LDX	0,X	  "*'"
	FIN
	LDAA     PC+1
         LDAB     PC
	IFUND	MC6809
         BRA      EXST21
	ELSE
	CPX	#'*##8+''	"*'"??
	BNE	EXST21J	B/No
	ADDA	OPSIZE	Adjust for
	ADCB	#0	 opcode
	ADDA	PCNEXT	  and operand
	ADCB	#0	   length
	LDX	EXPTR	Step
	INX		 past
	STX	EXPTR	  the "'"
EXST21J	JMP	EXST21
	FIN
         PAGE
*
* HIT A TERMINATOR WHEN A VALUE WAS EXPECTED.  IN THIS CASE, THE
* STACK TOP IS AN OPERATOR.  UNLESS STACK-TOP = OP:BOX, DROP DEAD.
* OTHERWISE WIND DOWN GRACEFULLY.
*EXST8    TSX                        GET POINTER TO STACK TOP
*         LDAA     0,X               WHATS THE STACK TOP?
*         CMPA     #OP:BOX           IS IT COOL?
*         BNE      EXPSE1            B/ NO.  FLAMES AND SMOKE.
*         CLRA                       NULL EXP:  CLEAR THE VAL,
*         PSHA
*         PSHA
*         BRA      EXST7             AND WIND DOWN.
* NO NULL EXPS ALLOWED NOW
*
EXST8	JMP	EXPSE	ERROR HIM
*
         PAGE
*21*
*
*  EXECUTE AN OPERATOR:
*
*   AT THIS POINT THE STACK LOOKS LIKE THIS:
*
*
*        SP+1  (OBH)  ==>  RIGHT-OP
*        SP+2  (OBL)
*        SP+3  (X:OP) ==>  OPERATOR
*
*   IF X:OP IS DYADIC THEN THERE ARE TWO MORE BYTES:
*        SP+4  (OAH)  ==>  LEFT-OP
*        SP+5  (OAL)
*
*  AFTER THE EXECUTION, RIGHT-OP, OPERATOR AND LEFT-OP ARE POPPED
*  AND THE STACK LOOKS LIKE THIS:
*
*        SP+1  ==> RESULT HIGH
*        SP+2  ==> RESULT LOW
*
OBH      EQU      0
OBL      EQU      1
X:OP     EQU      2
OAH      EQU      3
OAL      EQU      4
*
ES3XO    LDAA     X:OP,X            GET OPERATOR OFF STACK...
         JSR      WHICH             AND FIND OUT WHAT IT IS...
*
* NB -- THE ORDER OF OPS IN THIS TABLE DETERMINES HOW QUICKLY
* AN OPERATOR IS RECOGNIZED...
*
         FCB      +OP:BOX           <LEFT-ANCHOR>
         FDB      XBOX
         FCB      +OP:ADD           A+B
         FDB      XADD
         FCB      +OP:SUB           A-B
         FDB      XSUB
         FCB      +OP:MUL           A*B
         FDB      XMUL
         FCB      +OP:DIV           A/B
         FDB      XDIV
	IF	SHIFT=1
         FCB      +OP:SHF           A##B
         FDB      XSHF
	FIN
         FCB      +OP:IOR           A!B
         FDB      XIOR
         FCB      +OP:AND           A&B
         FDB      XAND
         FCB      +OP:EOR           A!!B
         FDB      XEOR
*22*
         FCB      +OP:CQU           A//B
         FDB      XCQU
         FCB      +OP:MOD           A\B
         FDB      XMOD
         DO       CONDASM=1
         FCB      +OP:NE            A#B 
         FDB      XNE
         FCB      +OP:EQ            A=B
         FDB      XEQ
         FCB      +OP:LT            A<B
         FDB      XLT
         FCB      +OP:GE            A>=B  A=>B
         FDB      XGE
         FCB      +OP:GT            A>B
         FDB      XGT
         FCB      +OP:LE            A<=B  A=<B
         FDB      XLE
         FIN
         FCB      +OP:PLS           +A
         FDB      XPOS
         FCB      +OP:MNS           -A
         FDB      XNEG
         FCB      +OP:COM           \A
         FDB      XCOM
         FCB      +OP:NOT           &A
         FDB      XNOT
         DO       CONDASM=1
         FCB      +OP:HI            A>>B
         FDB      XHI
         FCB      +OP:LO            A<<B
         FDB      XLO
         FCB      +OP:LS            A\<B
         FDB      XLS
         FCB      +OP:HS            A>/B
         FDB      XHS
         FIN
         FCB      0
         JMP      EXPSE             B/ URP... SYNTAX ERROR.
         PAGE     - - - O P E R A T O R S - - -
*
* A+B
XADD     BSR      GTA
         ADDA     OBL,X             ADD B
         ADCB     OBH,X             HIGH BYTE,
*
OPFNSH   EQU      *                 A HAS NEW OA
         STAA     OAL,X             SAVE IT.
         STAB     OAH,X
OPFN1    EQU      *                 POP OFF OBH, OBL,  OPERATOR
	LEAS	2,S
OPFN2    EQU      *
         INS                        POP THE OPERATOR
         JMP      EXST3             AND GO SCAN ANOTHER OPERATOR
*
* A-B
XSUB     EQU      *
         BSR      GTA               A := OAL;  B:= OAH
         SUBA     OBL,X
         SBCB     OBH,X
         BRA      OPFNSH            AND GO POKE.
         PAGE
	IF	SHIFT=1
* A##B  SHIFT A (+B PLACES TO LEFT) (-B PLACES TO RIGHT)
XSHF     BSR      GTA
XSHF0    TST      OBH,X             LEFT OR RIGHT SHIFT?
         BGE      XSHF1             B/ B>0 ==> LEFT SHIFT.
* SHIFT RIGHT...
XSHFR    LSRB
         RORA
         INC      OBL,X
         BNE      XSHFR             B/ STILL SOME LEFT TO DO...
         INC      OBH,X             INC H(CTR)
         BLT      XSHFR             B/ STILL SOME LEFT...
         BRA      OPFNSH            ALL DONE; OA IS SET
* SHIFT LEFT.
XSHF1    BGT      XSHF2             B/ SOME TO DO STILL.
         TST      OBL,X             IS COUNTER ZERO?
         BEQ      OPFNSH            B/ YES ==> ALL DONE.
XSHF2    ASLA
         ROLB
         DEC      OBL,X
         BNE      XSHF0             B/GO DO ANOTHER (WEIRD, EH)
         DEC      OBH,X
	BPL	XSHF2	B/ CAN'T BE THRU WITH THE SHIFT.
	BRA	OPFNSH	ALL DONE; SAVE THE VALUE.
	FIN
*
* A!B   LOGICAL INCLUSIVE OR.
XIOR     EQU      *
         BSR      GTA
         ORAA     OBL,X
         ORAB     OBH,X
         BRA      OPFNSH
*
* A&B  LOGICAL AND
XAND     EQU      *
         BSR      GTA
         ANDA     OBL,X
         ANDB     OBH,X
         BRA      OPFNSH
*24*
*
* A!!B  LOGICAL EXCLUSIVE OR
XEOR     EQU      *
         BSR      GTA
         EORA     OBL,X
         EORB     OBH,X
         BRA      OPFNSH
****
**** GTA
****    A := OAL, B := OAH, X := SP
GTA      TSX
	LEAX	2,X
         LDAA     OAL,X
         LDAB     OAH,X
         RTS
         PAGE
         DO       CONDASM=1
*** RELATIONAL OPERATORS...
*
* A#B A\=B
XNE      BSR      CPRAB             CLEARS A,B; X := SP
*                                   COMPARES OA TO OB
         BEQ      OPFNSH            B/ RELATION FALSE; AB IS READY.
* TRUE RELATION:
XRT      INCA                       SET OAL TO 1, RETURN.
         BRA      OPFNSH
*         SPACE
* A=B
XEQ      BSR      CPRAB
         BEQ      XRT
         BRA      OPFNSH
* A>B
XGT      BSR      CPRAB
         BGT      XRT
         BRA      OPFNSH
* A<B
XLT      BSR      CPRAB
         BLT      XRT
         BRA      OPFNSH
* A>=B
XGE      BSR      CPRAB
         BGE      XRT
         BRA      OPFNSH
*
* A<=B
XLE      BSR      CPRAB
         BLE      XRT
OPFNSH1	BRA      OPFNSH
*
* A<<B
XLO      BSR      CPRAB
         BCS      XRT               A MAG. LESS THAN B
	BRA      OPFNSH1
*
* A>>B
XHI      BSR      CPRAB
         BHI      XRT               A MAG. GTR. THAN B
         BRA      OPFNSH1
*
* A\<B  (A MAG. LOWER OR SAME. B)
XLS      BSR      CPRAB
         BLS      XRT
         BRA      OPFNSH1
* A>/B  (A MAG. HIGHER OR SAME. B)
XHS      BSR      CPRAB
         BCC      XRT
         BRA      OPFNSH1
         ELSE
OPFNSH1  BRA      OPFNSH            GET US BACK
         FIN
         PAGE
         DO       CONDASM=1
*
* CPRAB:  COMPARES OA TO OB, RETURNS A,B CLEARED, AND RETURNS
*   X AS PROPER INDEX INTO STACK.
CPRAB    EQU      *
         TSX                        GET THE STACK POINTER,
         INX
         INX                        (GET RID OF RETURN ADDR.)
         LDAA     OAL,X
         SUBA     OBL,X
         TPA                        GET CC
         TAB                        SAVE CC
         TAP                        RESTORE CC
         LDAA     OAH,X
         SBCA     OBH,X
         TPA                        SAVE CC,
         BNE      CPRAB1            B/ IF HIGH BYTE NOT ZERO, THEN
*                                   CC ARE OK.
         ANDA     #$FB              ELSE SCRUB Z FROM HIGH CC
         ANDB     #$04              RETAIN Z FROM LOW CC
         ABA                        MERGE FOR NEW CC,
CPRAB1   CLRB                       GET A ZERO,
         TAP                        SET CC,
         PSHB                       GET A ZERO IN X W/O ....
         PULA                       ...ZAPPING THE CC.
         RTS
         FIN
         PAGE
*25*
*         SPACE
* -B
XNEG     EQU      *
         CLRA
         CLRB
XNEG1    TSX
         SUBA     OBL,X
         SBCB     OBH,X
OPFN3    STAA     OBL+1,X           SHUFFLE UP ONE BYTE.
         STAB     OBH+1,X           (TROMP ON OPERATOR)
         JMP      OPFN2
*
* +B
XPOS     TSX
         LDAA     OBL,X
         LDAB     OBH,X
         BRA      OPFN3
*
* \B  (ONES COMPLEMENT B)
XCOM     LDAA     #-1
         TAB
         BRA      XNEG1
*
* &B  (INVERT TEST RESULT)
XNOT     CLRB
         CLRA
         TSX
	LDX	OBH,X	GET VALUE AS IT STANDS,
	TSX		  (AT LEAST THIS DOESN'T ZAP CC LIKE CLR -- IDIOTS)
	BGT	OPFN3	B/ WAS TRUE, MAKE FALSE (0)
	INCA		ELSE WAS FALSE, MAKE TRUE (1).
         BRA      OPFN3
	PAGE
*
* HIT END OF EXPRESSION:  THE ONLY OPERATOR THAT CAN FORCE
* EXECUTION OF <LEFT ANCHOR> IS OP:EOX
XBOX1    JMP      ES3SO1            BACK TO STACK THIS OP.
*
XBOX     LDAB     CUROP             WHAT WAS THE OPERATOR...
         CMPB     #OP:EOX           ...THAT GOT ME HERE?
         BNE      XBOX1             IF NOT <EOX>, THEN STACK IT.
*
         PULA                       SAVE THE VALUE
         STAA     VAL
         PULB
         STAB     VAL+1
         INS                        POP THIS OPERATOR
         RTS                        AND GO AWAY.  ALL DONE FOR EXP.
*28*
*
* MULTIPLY / DIVIDE OPERATORS........
*
XMUL     BSR      GMDARG            COPY ARGS INTO PLACES
         BSR      MULTPLY           GO DO MULTIPLY,
XMUL1    LDAA     MLC+1             GET L(RESULT)
         LDAB     MLC               GET H(RESULT)
XMUL2    TSX                        GET YE OLDE STACK POINTER,
         BRA      OPFNSH1
*
* A/B
XDIV     BSR      DODIV             COPY ARGS, CALL DIVIDE.
         BRA      XMUL1             AND GO RETURN RESULT...
*
* A//B  COVERED QUOTIENT
XCQU     BSR      DODIV
         LDAA     MLC+1
         LDAB     MLC               GET QUOTIENT...
	LDX	MLD	CK/ REMAINDER.  IF #0, THEN QUO. MUST BE BUMPED.
	BEQ	XMUL2	  B/ REM. IS 0; RESULT IS GOOD.
	ADDA	#1	ELSE BUMP QUOTIENT,
	ADCB	#0	  PROPOGATE CARRY,
	BRA	XMUL2	  AND RETURN RESULT.
*
* A\B  A MOD B
XMOD     BSR      DODIV
         LDAA     MLD+1             RETURN REMAINDER.
         LDAB     MLD
         BRA      XMUL2             GO BYE-BYE
	PAGE	- - - D O D I V - - -
*29*
*
* DODIV --
*        DIVIDES OA BY OB;
*        QUOTIENT IN MLC
*        REMAINDER IN MLD
*
*        IF ENTERED BY GTMDARG, JUST COPIES ARGS AND RETURNS.
*
GMDARG   LDAA     #1                REMEMBER WHAT WE ARE...
	BRA	DODIV1
DODIV    CLRA
DODIV1	TSX
         LDX      OAH+2,X           (RETURN @ IS ON STACK)
         STX      MLA
         TSX
         LDX      OBH+2,X
         STX      MLB
         TSTA
         BEQ      DIVIDE            B/ GO DO DIVIDE.
         RTS                        ELSE EXIT.
	PAGE	- - -  D I V I D E  - - -
*
* DIVIDE --
*
*  MLA IS THE DIVIDEND;
*  MLB IS THE DIVISOR;
*  MLC IS THE QUOTIENT;
*  MLD IS THE REMAINDER.
*
*  IF DIVISOR IS ZERO, OVERFLOW IS SIGNALED.;  RESULT IS $FFFF
*
*
DIVIDE   EQU      *
	LDX	MLB	CK/ DIVISION BY ZERO?
	BNE	DIV1	  B/ NOPE.
	STX	MLC	...TRYING TO FOOL ME, EH?
	STX	MLD	  IN THAT CASE, CLEAR REMANDER & QUOTIENT,
	JMP	OVRFLW	  FLAG OVERFLOW AND RETURN.
*
DIV1     LDX      MLA               INITIALIZE THE REMAINDER;
         STX      MLC
         LDX      #0                ALSO THE QUOTIENT.
         STX      MLD               (THIS ALSO MAKES A HANDY CONSTANT)
         LDAB     #16               NUMBER OF DIVIDE STEPS...
*
DIVL     ASL      MLC+1,X           SHIFT  (REMAINDER,QUOTIENT) LEFT 1
         ROL      MLC,X
         ROL      MLD+1,X
         ROL      MLD,X
         LDAA     MLD+1             COMPARE REMAINDER TO DIVISOR...
         SUBA     MLB+1
         PSHA                       (SAVE THE RESULT.)
         LDAA     MLD
         SBCA     MLB
         BCS      DIV2              B/ NO DO SUBTRACT.
         STAA     MLD               SAVE HIGH QUOTIENT
         PULA                       GET LOW QUOTIENT
         STAA     MLD+1
         INC      MLC+1,X           NEXT QUOTIENT BIT IS 1
	BRA	DIV3

DIV2     INS
DIV3	DECB                       DECREMENT STEP COUNTER,
         BNE      DIVL              B/ DO ANOTHER.
         RTS                        ELSE ALL DONE.
	PAGE	- - -  M U L T I P L Y  - - -
*         SPACE
*
* MULTPLY --
*
* MLA IS MULTIPICAND
* MLB IS MULTIPLIER
* MLC IS RESULT
* MLD IS OVERFLOW WORD
*
MULTPLY  EQU      *
         LDAB     #16               GET # OF MULTIPLY STEPS
         STAB     MLC               AND PUT IN A SAFE PLACE.
         CLRA                       PRESET RESULT TO ZERO
         CLRB
         LDX      MLB               TRANSFER MULTIPLIER ....
         STX      MLD               ... TO OVERFLOW WORD.
         LDX      #0                GET HANDY CONSTANT.
*
MULLP    ASLB
         ROLA
         ROL      MLD+1,X
         ROL      MLD,X
         BCC      MUL1              B/ DON'T DO AN ADD STEP.
         ADDB     MLA+1
         ADCA     MLA
*
MUL1     DEC      MLC,X             THIS STEP DONE...
         BGT      MULLP             B/ SO DO ANOTHER.
         STAA     MLC               SAVE RESULT
         STAB     MLC+1
         LDX      MLD               CHECK FOR OVERFLOW.
         BEQ      MULV1             B/ NONE.
         SEV
MULV1    RTS
	TITLE	- - - G E T    N E X T    O B J E C T - - -
	PAGE	- - - G T C H T - - -
*30*
*
*  GET NEXT CH, AND ITS TYPE.
*        A := CH;  B := TYPE.
*
GTCHT    EQU      *
         JSR      GETXC             GET CHARACTER
         JSR      CHRANGE           CHECK FOR IN RANGE.
         FCB      'A,'Z
         FDB      GTCH1             ALPHABETIC
         FCB      'a,'z             lower case ==>UPPER CASE
         FDB      GTCH1L
         FCB      '0,'9
         FDB      GTCH2             DIGIT
         FCB      0
* WEIRD -- FIND IT IN TABLE.
         LDX      #CHTBL            GET AN INDEX
         JMP      FINDAX            AND FIND IN THE TABLE.
*
*
GTCH1L   ADDA     #'A-'a            MAKE IT UPPER CASE.
GTCH1    LDAB     #C:ALPH
         RTS
GTCH2    LDAB     #C:NUM            IT'S A DIGIT
         RTS
         PAGE     - - - TABLE OF CHARACTERS AND TYPES - - -
*
* CHTBL:
*        FIRST BYTE OF EACH IS AN ASCII CHARACTER;
*        SECOND BYTE IS ITS TYPE.  TABLE ENDS W/ A $80 FOR FINDAX;
*        THUS FORCES ALL OTHER CHARS TO TYPE OF C:UNK.
*
CHTBL    EQU      *
         FCB      ':,C:ALPH
         FCB      '$,C:ALPH
         FCB      '@,C:ALPH
         FCB      '%,C:ALPH
*
         DO       STRINGS=1
         FCB      '",C:QQ
         FIN
*
         FCB      '',C:Q
         FCB      '(,C:LP
         FCB      '),C:RP
         FCB      BLANK,C:BL
         FCB      TAB,C:BL
         FCB      $D,C:CR
         FCB      ';,C:CR
         FCB      ',,C:TERM
	FCB	'],C:TERM
         FCB      '.,C:ALPH
         FCB      $80,C:UNK
         PAGE     - - - T B J M P - - -
*32*
*
* TBJMP --
*        DOES A TABLE BRANCH.  (B) = WORD INDEX
*	A-Reg Preserved
*
TBJMP	PULX		Pull return address
         ASLB                       MAKE A WORD INDEX,
	DO	M6809=1
	LDX	,X	Table address
	JMP	[B,X]
	ELSE
         ADDB     1,X               GET VALUE OF LOW
         PSHB
         LDAB     0,X
         ADCB     #0
         PSHB
	PULX
         LDX      0,X               GET ACTUAL ADDRESS,
         JMP      0,X               AND GO TO IT.
	FIN
	PAGE	- - - C K E O F - - -
*
* CKEOF - CHECK THAT THE NEXT THING TO BE SCANNED IS
*	THE END OF A FIELD. (I.E. A TERMINATOR
*	BUT NOT A COMMA).
*
*	IF EOF SEEN -- Z=1
*	IF NOT      -- Z=0
*
CKEOF	BSR	GNOBJ	GET THE OBJ TO LOOK AT
	INC	RJOBJ	REJECT IT
	CMPA	#O:TERM	A TERM?
	BNE	RTEOF	IF NOT, Z=0 RETURN
	CMPB	#ASCII:COMMA	A COMMA?
	TPA		INVERT THE Z BIT
	BITA	#4
RTEOF	RTS
*
* ECKEOF - CK IF EOF IS NEXT. IF NOT, GIVE ILLEGAL AF ERR 
*	AND RETURN. ELSE JUST RETURN.
*
ECKEOF	BSR	CKEOF	CHECK IT
	BEQ	RTEOF	IF FOUND EOF - JUST RETURN
	JMP	ILLAF	ELSE, GO TELL HIM ABOUT ILLEGAL AF
*
         PAGE     - - - G T C M A - - -
*
*   G T C M A
*
*        Calls GNOBJ until a terminator is seen; then returns
*        V=1 if terminator was a comma.  Otherwise, V=0.
*
GTCMA    EQU      *
         BSR      GNOBJ             GET OBJECT
         CMPA     #O:TERM
         BNE      GTCMA
         CMPB     #ASCII:COMMA
	SEV
	BEQ	GTCRET
	INC	RJOBJ	REJECT THIS OBJECT IF A TERM IS SEEN
	CLV
GTCRET	RTS
	PAGE	- - - G N O B J - - -
*33*
*
*
*   G N O B J   /    G N O B X
*
*        GETS NEXT OBJECT FROM STRING POINTED TO BY
*        EXPTR.  OBJECT IS ONE OF THE FOLLOWING --
*
*        OBJ      VALUE RETURNED:
*        ---      ---------------
*
*        SYMBOL   O:SYM;  SYM:=TEXT OF SYMBOL
*        NUMBER   O:NUM;  VAL:=VALUE
*        STRING   O:STR;  STBUF:=STRING
*        (        O:LP;
*        )        O:RP;
*        DELIM.   (BLANK, TAB, ';, ',, $D) O:TERM; B:=CH
*        OPERATOR O:OP; B:=OPERATOR
*        OTHERS   O:UNK; B:=CH.
*
* NB:
*   ---->NO MATTER HOW HARD YOU TRY, ONCE YOU HAVE GOTTEN TO A $D
*        YOU CAN'T GO PAST IT.  THE SAME HOLDS TRUE FOR '; NOT IN A STRI
*   ---->IF RJOBJ IS NON-ZERO, THEN THE LAST OBJECT RETURNED IS
*        RETURNED AGAIN.
*   ---->A:=OBJECT; B:= SOME KIND OF TYPE DEPENDENT INFO.
*
*        IF ENTERED VIA GNOBX, EXPTR IS SET TO (X); REJECTED OBJECT
*        IS IGNORED.
*
*
         PAGE
*33*
GNOBJ    LDAA     RJOBJ             WAS LAST OBJECT REJECTED?
         BEQ      GNOB1             B/ NO.  GET A NEW ONE.
* OBJECT WAS REJECTED; RETURN IT AGAIN
         LDAA     RJA
         LDAB     RJB
GNOBC    CLR      RJOBJ
         RTS
*
* OH, WELL....  GO GET AN OBJECT.
*
GNOBX    STX      EXPTR
         BSR      GNOBC             SAVE A BYTE WHERE YOU CAN...
GNOB1    JSR      GTCHT
         JSR      TBJMP             AND DO A TABLE JUMP.
         FDB      GNTBL-2
*34*
*
GNTBL    FDB      GSYM              C:ALPH ==> SYMBOL
         FDB      GNUM              C:NUM ==> NUMBER
         FDB      GEOL              C:CR ; OR $D
*
         DO       STRINGS=1
         FDB      GSTR              C:QQ " ==> STRING
         ELSE
         FDB      GUNK              *** SHOULD NEVER HAPPEN...
         FIN
*
         FDB      GCH               C:Q ' ==> CHARACTER
         FDB      GETLP             C:LP ( ==> LEFT PAREN
         FDB      GETRP             C:RP ) ==> RIGHT PAREN
         FDB      GTRM              C:TERM ==> DELIMITER.
         FDB      GBLK              C:BL ==> BLANK.
         FDB      GUNK              C:UNK ==> MAYBE OPERATOR.
         PAGE     - - - G H E X - - -
*
*
*   G H E X
*
*        COLLECTS A HEXADECIMAL #.
*        IF ENTERED THRU GHEX, THEN ACCB SHOULD CONTAIN
*        THE RADIX OF THE NUMBER-1 ($F, $7 OR $1).  IT IS
*        ASSUMED THAT A PREQUALIFIER WAS SEEN.
*
*        IF ENTERED THRU GHXE, THEN A POST-QUALIFIER WAS
*        SEEN & GOT US HERE.  THE QUALIFIER DIGIT IS IN (A),
*        THE RADIX IS IN B.
*
*        IN ANY EVENT, EITHER THE POST-QUALIFIER OR A NULL IS
*        STORED IN RJA FOR WHEN WE FIND THE END OF THE #.
*
*
GHEX     EQU      *                 B CONTAINS RADIX.
         CLRA
GHXE     STAA     RJA               NO QUALIFIER EXPECTED.
         CLRA
         STAA     VAL               ACCA BETTER BE ZERO...
         STAA     VAL+1
         BSR      GTXD              GET A HEX DIGIT...
         BEQ      GHX0              B/ OK...
         JSR      SYNTAX            *E* QUALIFIER $%@ W/O NUMBER
         BRA      GOTNUM
* GOT 1ST DIGIT.
GHX0     EQU      *
         CBA                        IS DIGIT LEGAL?
         BGT      GHXI              B/ NO, TOO BIG.
         PSHB                       SAVE THE DIGIT.
GHXL     ASL      VAL+1             MULTIPLY BY 2^B
         ROL      VAL
         BCC      GHX1              B/ NO OVERFLOW...
         JSR      OVRFLW            *E* NUMBER TOO BIG.
GHX1     LSRB                       ANY SHIFTS LEFT?
         BNE      GHXL              B/ YUP: GO DO ANOTHER.
*
         PULB                       GET RADIX BACK.
         ADDA     VAL+1             GET NEW DIGIT,
         STAA     VAL+1             (NO CARRY POSSIBLE)
GHX2     BSR      GTXD              GET NTH HEX DIGIT
         BEQ      GHX0              B/ GOT ANOTHER: ACCUMULATE
*
* VAL CONTAINS VALUE; RETURN TYPE = NUMBER
GOTNUM   LDAA     #O:NUM            IT'S A NUM
*         SPACE
GOTOBJ   STAA     RJA
         STAB     RJB
         LDX      EXPTR
	RTS
*  WIERD DIGIT FOUND:
GHXI     EQU      *
         PSHA                       SAVE THIS DIGIT...
         BSR      GTXD              GET A HEX DIGIT...
         BEQ      GHXI1             B/ NOT LAST:  ILLEGAL.
* THE DIGIT MAY HAVE BEEN QUALIFIER.
         PULA                       GET THE DIGIT BACK...
         CMPA     RJA               WAS IT THE QUALIFIER WE WANTED?
         BEQ      GOTNUM            B/ YES... # IS OK.
         BRA      GHXI2             ELSE ERROR...
* ILLEGAL DIGIT...
GHXI1    INS                        POP STACK
         JSR      RJXCH             REJECT CHAR.
GHXI2    JSR      BADDIG            *E* BAD DIGIT
         BRA      GHX2              AND GO ON.
         PAGE     - - - G T X D - - -
*
*  GTXD--
*        GET A HEX DIGIT INTO A.  DOES NOT CLOBBER B.
*        RETURNS A AS HEXADECIMAL VALUE OR AS TERMINATOR.
*        IF Z IS SET ON RETURN, THEN A IS A VALID HEX DIGIT.
*
GTXD     EQU      *
         PSHB                       SAVE ACCB
         JSR      GTCHT             GET EXPRESSION CH.
         JSR      CHRANGE           WHAT IS IT?
         FCB      '0,'9             DIGIT.
         FDB      GTXD1
         FCB      'A,'Z             DIGIT.
         FDB      GTXD2
         FCB      0
         LDAA     #'Z               GET A BAD DIGIT,
         CMPB     #C:ALPH           WAS IT ALPHABETIC?
         BEQ      GTXD2             B/ YES.  GO ON.
*
         PULB                       NOT A VALID DIGIT:
         JMP      RJXCH             DOES A DEX; CC WILL HAVE Z CLEAR.
*
GTXD2    SUBA     #'A-'0-$A
GTXD1    SUBA     #'0
*
         CLRB                       SET CCZ
         PULB                       AND RESTORE ACCB
         RTS                        AND EXIT.
         PAGE
*
* GET A DECIMAL NUMBER --
*        CODE TO GET NNNNB, NNNNH, NNNNO WILL ALSO GO HERE.
*
GNUM     LDX      EXPTR             SAVE POINTER...
         STX      TPTR              IN CASE IT AINT DECIMAL
         SUBA     #'0               MAKE A REAL DIGIT,
         STAA     VAL+1             POKE IT AWAY,
         CLR      VAL               AND CLEAR HIGH BYTE.
GNUML    JSR      GTCHT             GET NEXT CH W/ TYPE,
         CMPB     #C:NUM            IS IT A DIGIT?
         BNE      GNUMYCH           B/ NO:  MAYBE ALL DONE.
         SUBA     #'0               MAKE A REAL DIGIT,
         PSHA                       AND SAVE IT.
         LDAA     VAL               GET H(VAL)
         LDAB     VAL+1             GET L(VAL)
         ASLB                       M
         ROLA                         U
         PSHB                           L
         PSHA                             T
         ASLB                               I
         ROLA                           B     P
         ASLB                       T     Y     L
         ROLA                         E           Y
         TSX                            N
*
* STACK NOW LOOKS LIKE THIS:
*
*        SP+1 ==> VAL*2
*        SP+2
*        SP+3 ==> NEW DIGIT
*
*        ACCA ==> VAL*8
*        ACCB
*
         ADDB     1,X               ADD IN LOW BYTE
         ADCA     #0                PROPOGATE CARRY
         ADDB     2,X
         ADCA     0,X               AND NEW DIGIT, W/ CARRY
         STAA     VAL
         STAB     VAL+1
	LEAS	3,S	Prune stack
         BRA      GNUML             AND GO GET ANOTHER
         PAGE
*
* NOT A DIGIT:  MAY BE THE END OF THE NUMBER...
*
GNUMYCH  JSR      RJXCH             REJECT THIS CHARACTER,
         CMPB     #C:ALPH           IS IT AN ALPHABETIC CH?
         BNE      GOTNUM            B/ NO: MUST BE TERMINATOR
*
* THIS IS WEIRD:  FIND A NON-ALPHABETIC.
*        USES RJA AS TEMP FOR LAST CH SEEN;
*        USES RJB AS TEMP FOR TYPE OF LAST CH SEEN.
*
GNUMY1   STAA     RJA               SAFE PLACE
         STAB     RJB
         JSR      GTCHT             WHAT'S NEXT?
         CMPB     #C:NUM            A DIGIT?
         BEQ      GNUMY1            B/ YES: KEEP SLURPING
         CMPB     #C:ALPH           A LETTER?
         BEQ      GNUMY1            B/ YES: KEEP SLURPING
         LDAA     RJA               ELSE GET LAST DIGIT OF #
         LDAB     RJB               WITH TYPE,
         CMPB     #C:ALPH           DID IT END W/ QUALIFIER?
         BEQ      GNUMY2            B/ MAYBE.
*
* ILLEGAL DIGIT DETECTED...
GNUMY11  JSR      REJCH             REJECT THE TERMINATOR
         JSR      BADDIG            *E* ILLEGAL DIGIT
         JMP      GOTNUM            AND RETURN THE NUMBER.
*
* MAYBE NNNNH, NNNNO, OR NNNNB
*
GNUMY2   EQU      *                 WHAT WAS TERMINATOR?
         LDX      #GNUMQ            @ OF QUALIFIER TABLE.
         JSR      FINDAX            GO FIND ACCA.
         BVS      GNUMY11           B/ NOT A LEGAL TERMINATOR.
* QUALIFIER WAS FOUND:
*        B NOW CONTAINS RADIX DIGIT.
*
         LDX      TPTR              GET @ OF START OF #,
         DEX                        POINT TO START,
         STX      EXPTR             AND SAVE.
         SUBA     #'A-$A            MAKE INTO QUALIFIER DIGIT,
         JMP      GHXE              AND GO EAT IT UP.
         PAGE
*
* QUALIFIER TABLES:
*        (1) POST-FIX QUALIFIER
*
GNUMQ    EQU      *
         FCB      +'H,$F
         FCB      +'O,$7
         FCB      +'Q,$7
         FCB      +'B,$1
         FCB      $80
*
*        (2) PRE-FIX QUALIFIERS
*
GSYMT    EQU      *
         FCB      '$,$F
         FCB      '@,$7
         FCB      '%,$1
         FCB      $80
         PAGE
*37*
*
* GSYM --
*
*        GETS A SYMBOL...
*        DOESN'T LOOK IT UP.
*
*        ANYTHING STARTING W/ $, % OR @ IS SHUNTED TO APPROPRIATE ROUTIN
*
GSYM0    JMP      GHEX              GET US THERE.
GSYM     EQU      *
         LDX      #GSYMT            CHECK FOR $,%,@
         JSR      FINDAX
         BVC      GSYM0             AHA!  EAT NUMBER.
* A GOOD SYMBOL...
         LDAB     #1                GET SIZE
         LDX      #SYM
GSYML    INX
         STAA     0,X               SAVE CH
         STAB     SYM               AND NEW COUNT.
         STX      TPTR              SAVE IN SAFE PLACE.
GSYML1   JSR      GTCHT             GET A CHARACTER,
         CMPB     #C:ALPH           IS IT ALPHABETIC?
         BEQ      GSYM1             B/ YES: SAVE IT.
         CMPB     #C:NUM            IS IT NUMERIC?
         BNE      GSYMD             B/ NO: ALL DONE.
GSYM1    LDAB     SYM               GET SIZE.
         CMPB     #SYMMAX           TOO BIG?
         BEQ      GSYML1            B/ YES, DONT SAVE IT
         INCB
         LDX      TPTR              ELSE GET POINTER,
         BRA      GSYML             AND SAVE.
*
GSYMD    LDAA     #O:SYM            IT'S A SYMBOL
         JSR      RJXCH             REJECT THE LAST CH,
GOTOBJ1  JMP      GOTOBJ            B/ GOT THE OBJECT.
         PAGE
*39*
*
* GCH --
*        RETURNS CHARACTER FOLLOWING SQUOTE; MAY BE
*        ANYTHING EXCEPT $D
*
GCH      LDX      EXPTR             GET POINTER TO NEXT,
         LDAA     0,X               GET NEXT
         CMPA     #$D               IS IT A CR?
         BEQ      GCHD              B/ YES:  BLECH.
         ORAA     #$80              MARK AS "IN STRING",
         STAA     0,X               SAVE IT,
         INX                        POINT TO NEXT CH.
         STX      EXPTR             AND SAVE.
         ANDA     #$7F              SCRUB $80 BIT.
GCH1     STAA     VAL+1             AND SAVE VAL.
         CLR      VAL
         JMP      GOTNUM            B/ GOT A NUMERIC VALUE.
GCHD     JSR      ILLSTR            *E* ' FOLLOWED BY <EOL>
         CLRA                       RETURN A NULL
         BRA      GCH1
         PAGE
*39*
*
* GET A STRING.
*        STBUF := STRING; BYTE 0= COUNT; "" ==> "
*
         DO       STRINGS=1
GSTR     EQU      *
         CLRB                       CLEAR COUNT,
         LDX      #STBUF            GET POINTER TO BUFFER
         STAB     1,X               AND PUT A NULL THERE.
GSTRL    STX      TPTR              SAVE POINTER,
GSTRL1   BSR      GETXC             AND GET A CHARACTER.
         CMPA     #$D               END OF LINE?
         BEQ      GSTRE             B/ YECCH.
         CMPA     #'"               DQUOTE?
         BEQ      GSTRD             B/ MAYBE "";
GSTR1    LDX      TPTR              GET POINTER
         INCB                       BUMP COUNT
         BNE      GSTR2             B/ STILL ROOM
         JSR      OVRFLW
         DECB                       BACK TO 255
         BRA      GSTRL1            AND EAT UNTIL DONE.
GSTR2    INX
         STAA     0,X               SAVE CHARACTER
         BRA      GSTRL             AND GET ANOTHER
*
GSTRD    BSR      GETXC             GET ANOTHER CHARACTER,
         CMPA     #'"               ANOTHER DQUOTE?
         BEQ      GSTR1             B/ YES: GO SAVE IT.
         STX      EXPTR             ELSE DELETE CH,
GSTR3    STAB     STBUF             SAVE COUNT
         LDAA     #O:STR            GET STRING TYPE
GOTOBJ2  BRA      GOTOBJ1
* ERROR IN STRING
GSTRE    STX      EXPTR             POINT TO OFFENSIVE CHARACTER,
         CLRB                       NULL STRING,
         JSR      ILLSTR            *E* '" W/O MATCHING '"
         BRA      GSTR3             AND LEAVE.
         ELSE
GOTOBJ2  BRA      GOTOBJ1
         FIN
         PAGE
*39*
*INC EXPTR AND LOAD UP NEXT CHAR
*
GETXC    EQU      *
         LDX      EXPTR             GET POINTER,
         LDAA     0,X               GET NEXT CH,
         INX
         STX      EXPTR             SAVE POINTER,
         DEX
         RTS                        AND LEAVE.
*
REJCH    EQU      *
REJXC    EQU      *
RJXCH    EQU      *
         LDX      EXPTR
         DEX
         STX      EXPTR
         RTS
         PAGE
*41*
*
* SAW LEFT OR RIGHT PAREN
*
GETLP    LDAA     #O:LP             OBJECT IS LEFT PAREN
	BRA	GOTOBJ2
GETRP    LDAA     #O:RP             OBJECT IS RIGHT PAREN.
         BRA      GOTOBJ2           AND LEAVE.
*
*
* SAW A BLANK OR A TAB
*
GBLK     BSR      GETXC             WHATS THE NEXT CHARACTER?
         CMPA     #TAB              IS IT A TAB?
         BEQ      GBLK              B/ YES, EAT IT UP.
         CMPA     #BLANK            SAME FOR BLANK.
         BEQ      GBLK
         STX      EXPTR             ELSE REJECT CHARACTER.
         LDAA     #BLANK            AND RETURN STANDARD SEPARATOR.
GTRM     TAB                        COPY TO CORRECT PLACE,
         LDAA     #O:TERM           GET TYPE,
         BRA      GOTOBJ2
         PAGE
GEOL     LDAA     #$D               RETURN END OF LINE CH,
         JSR      REJCH             REJECT THIS CHARACTER,
         BRA      GTRM              AND RETURN A TERMINATOR.
*41*
*
* GUNK --
*        GCHECKS WHETHER NEXT CH. IS AN OPERATOR;
*        IF SO RETURNS OP, TYPE; OTHERWISE RETURNS O:UNKN,
*        CH IN ACCB.
*
GUNK     CMPA     #$20              IS IT A CONTROL CH?
         BLT      GILGL             B/ YES: ILLEGAL.
         CMPA     #$5F              IS IT TOO BIG FOR TTY?
         BGT      GILGL             B/ YES: ILLEGAL.
         LDX      #GOPTB-2          MAYBE AN OPERATOR:
         BSR      FINDAX            GO CHECK.
         BVC      GOPTR             B/ GO EAT AN OPERATOR
GILGL    TAB
         LDAA     #O:UNKN
         BRA      GOTOBJ2
         PAGE
*41*
*
* GOPTR--
*        CHARACTER IN A IS THE START OF AN OPERATOR.
*        EXTENSION VALUE IS IN B
*        X CONTAINS THE @ OF THE BYTE PAIR: MATCH, VALUE.
*        NOW WE NEED TO CHECK WHETHER THE OPERATOR IS A
*        TWO-CHARACTER OP.
*
GOPTR    TSTB                       TEST EXTENSION:
         BGE      GOPTR2            B/ NEED TO GET ANOTHER CHARACTER.
         NEGB                       MAKE REAL VALUE,
GOPTR1   LDAA     #O:OP             GET TYPE
         BRA      GOTOBJ2           AND LEAVE.
*
GOPTR2   BSR      GETXC             GET NEXT CH,
         BSR      LDAXB             GET NEW POINTER IN X
         FDB      G2OTB             (SECOND TABLE)
         BSR      FINDAX            FIND IN TABLE
         TST      0,X
         BGE      GOPTR1            SECOND CH WAS OP.
         JSR	REJXC	ELSE REJECT SECOND CH.
	TSTB		WAS FIRST CH IN FACT AN OP ?
	BNE	GOPTR1	B/ YES:  HANDLE AS ONE.
	BRA	GILGL	B/ NO:  IT'S AN UNKNOWN.
	PAGE	- - - F I N D A X - - -
*41*
*
* FINDAX
*        LOOPS THROUGH A TABLE, ASSUMED TO CONSIST OF 2-BYTE
*        ENTRIES, COMPARING ACCA TO FIRST BYTE.  IF MATCH, THEN,
*        LEAVES X POINTING TO GOOD ENTRY.  ELSE RETURNS WITH
*        CCV SET.  IN EITHER CASE, B IS LOADED WITH SECOND
*        BYTE OF ENTRY WHICH SENT US HOME, BE IT THE $80 OR
*        A MATCHED BYTE.
*
*        NOTE THAT THE TABLE MAY CONSIST ONLY OF A $80 ENTRY.
*	ALSO, TOP BIT OF (A) MUST BE ZERO, OR ALL BETS ARE OFF!
*
FINDAXL  LEAX	2,X
FINDAX   EQU      *
         LDAB     1,X               GET SECOND BYTE.
         CMPA     0,X               IS THIS ONE WE WANT?
         BVS      FINDAXX           B/ NO: END HIT.
         BNE      FINDAXL           B/ NO: MORE TO COME.
FINDAXX  RTS                        ELSE RETURN.
	PAGE	- - - L D A X B - - -
*43*
*
* LDAXB --
*        LOADS X WITH SUM OF B REGISTER AND POINTER
*        FOLLOWING CALL.
*        ACCB IS CLOBBERED.
*
LDAXB    TSX                        GET RETURN ADDRESS
         LDX      0,X               ......
         ADDB     1,X               ADD LOW BYTE OF PTR TO B,
         PSHB                       TXFER TO STACK.
         LDAB     #0                EXTEND ZEROS FOR B.
         ADCB     0,X               ADD HIGH BYTE OF PTR,
         PSHB                       AND SAVE ON STACK.
         TSX                        NOW, RECOMPUTE RETURN @
         LDAB     3,X               GET RETURN(L)
         ADDB     #2
         STAB     3,X
	BCC	LDAXB$1
	INC	2,X
LDAXB$1	LDX      0,X
	LEAS	2,S	Prune stack
         RTS
	PAGE	- - - O P E R A T O R   T A B L E S - - -
*42,43*
*
*  OPERATOR TABLES.
*
GOPTB    FCB      '+,-OP:ADD&$FF
         FCB      '-,-OP:SUB&$FF
         FCB      '&,-OP:AND&$FF
         FCB      '*,-OP:MUL
	IF	(SHIFT=1)
	FCB	'#,G2NE
	ELSEIF	CONDASM=1
	FCB	'#,-OP:NE
	FIN
         FCB      '/,G2DIV
         FCB      '!,G2OR
         FCB      '\,G2MOD
         IF       CONDASM=1
         FCB      '<,G2LT
         FCB      '>,G2GT
         FCB      '=,G2EQ
         FIN
         FCB      $80               CAUSES OVERFLOW.
         PAGE
*
* EXTENSION TABLES FOR 2-CH OPS.
G2OTB    EQU      *
	IF	SHIFT=1
G2NE	EQU      *-G2OTB           DISPLACEMENT.
         FCB      '#,OP:SHF         SHIFT ##
         FCB      $80
	  IF	CONDASM=1
	  FCB	OP:NE
	  ELSE	
	  FCB	0
	  FIN
	FIN
*
G2DIV    EQU      *-G2OTB
         FCB      '/,OP:CQU         //
         FCB      $80,OP:DIV
*
G2OR     EQU      *-G2OTB
         FCB      '!,OP:EOR         !!
         FCB      $80,OP:IOR
*
G2MOD    EQU      *-G2OTB
         IF       CONDASM=1
         FCB      '<,OP:LS
         FIN
         FCB      $80,OP:MOD
*
         DO       CONDASM=1
G2LT     EQU      *-G2OTB
         FCB      '<,OP:LO          <<
         FCB      '=,OP:LE          <=
         FCB      $80,OP:LT         <
*
G2GT     EQU      *-G2OTB
         FCB      '>,OP:HI          >>
	FCB	'/,OP:HS	>/
         FCB      '=,OP:GE          >=
         FCB      $80,OP:GT         >
*
G2EQ     EQU      *-G2OTB
         FCB      '<,OP:LE          =<
         FCB      '>,OP:GE          =>
         FCB      $80,OP:EQ         =
         FIN
	TITLE	- - - S Y M B O L    M A N I P U L A T I O N S - - -
	PAGE	- - - C H C F S Y M - - -
*38*
*
* CHCFSYM --
*        CHECKS WHETHER CF(1) IS A SYMBOL;
*
* INPUT:
*        CFPTR    POINTS TO CF
*
* OUTPUT:
*        SYM      CONTAINS SYMBOL IF ONE WAS FOUND.
*
* SCRATCH:
*        EXPTR, RJA, RJB, ETC. FOR GNOBJ
*
* INTERFACE:
*        GNOBJ:   CALLED TO GET OBJECTS.
*
CHCFSYM  LDX      CFPTR             GET POINTER,
         JSR      GNOBX             GET FIRST OBJECT.
         CMPA     #O:SYM            IS IT A SYMBOL?
         BNE      CHCFRTS           B/ NO.  RETURN.
         JSR      GNOBJ             GET TERMINATOR
         CMPA     #O:TERM           IS IT A REAL TERMINATOR?
CHCFRTS  RTS                        AND RETURN.
         PAGE     - - - C H S Y M D - - -
*
*
*  C H S Y M D
*
*        Checks whether SYM is a directive.  If so, sets
*        SYMPT pointing to the directive table entry, sets CCz=1
*        and returns.
*
*        If symbol is not a directive, CCZ=0.
*
*        To find the directive:  first the user's directive
*        chain is searched; if the directive is in the user's
*        chain, & it is undefined, (i.e. T:FREF is set in the
*        type byte) then we say, "There is no such directive"
*        (at least, none defined by the user) and search the
*        assembler's chain.  If T:FREF is clear, then he has
*        redefined the directive, so we go to him.
*        Otherwise we search the assembler's chain; if found
*        o.k., if not, that's o.k. too.
*
*
CHSYMD   EQU      *
CHSYMD1  LDX      #D0               ELSE GET HEAD OF SYSTEM CHAIN
         JMP      FSYM1
*
         PAGE
*44*
*
* FINDAF --
*        SCANS FROM AFPTR TO END OF FIELD, THEN FINDS FIRST
*        THING AFTER TERMINATOR; SETS AFPTR TO POINT TO IT.
*
* ONLY ALLOWS ONE SYMBOL FOLLOWED BY A TERMINATOR. SYNTAX ERR IF NOT.
*
*
FINDAF   LDX      AFPTR
         JSR      GNOBX             GET FIRST OBJECT,
         BRA      FINDAF1           AND GO TEST IT
*
FINDAFE	JSR	SYNTAX	ERROR BUT KEEP SCANNING
FINDAFL  JSR      GNOBJ             WHAT'S NEXT.
FINDAF1	CMPA	#O:SYM	A SYM?
	BEQ	FINDAFL	IF FOUND SYM - NOW LOOK FOR TERM
	CMPA	#O:TERM	A TERM?
	BNE	FINDAFE	IF NOT, ERROR BUT KEEP SCANNING TO A TERM
         CMPB     #ASCII:COMMA	JUST END OF THIS SUBFIELD?
         BEQ      FINDAFE           B/ YES:  KEEP GOING.
         STX      AFPTR             ELSE: SAVE POINTER,
         RTS                        AND SPLIT.
         PAGE
*49*
*
*  HASH--
*        Computes hash pointer for a symbol in SYM.  The hashing
*        algorithm used is Out-of-table Chaining; the hash code
*        is generated by accumulating a weird sum.
*        HASH:M is a mask which determines the size of the hash
*        table; note that the first pointer returned is a pointer
*        to the appropriate hash bucket.
*        (The above means that S:NEXT must be the first entry in
*         a symbol).
*
******
*
* HASH:PTR WORKS LIKE HASH EXCEPT THAT THE SYMBOL IS NOT IN SYM
* BUT RATHER IT IS POINTED TO BY THE X REG.
*      X=START OF SYM BLOCK
*
HASH	EQU	*
	LDX	#SYM:BLK	LOAD POINTER TO DUMMY SYM BLOCK.

HASH:PTR	EQU	*
	LDAB	S:LEN,X	LOAD LENGTH FOR HASH
	CLRA		CLEAR ACCUMULATED SUM
HASHL    ROLA                       LEFT-SHIFT SUM,
         ADCA     S:TEXT,X	ADD THE CARRY IN W/ THE NEXT CH.
         INX                        POINT TO NEXT
         DECB                       ANY LEFT TO SUM?
         BNE      HASHL             B/ YES:  GO.
         ANDA     #HASH:M           GET RID OF TRASH BITS;
         ASLA                       MAKE IT A WORD INDEX
         ADDA     #HSHTBL&$FF       MAKE LOW BYTE OF ADDRESS,
         PSHA                       SAVE,
         LDAA     #HSHTBL/256       MAKE HIGH BYTE OF ADDRESS,
         ADCA     #0
         PSHA
	PULX		Get pointer to hash bucket
	RTS		 and leave
         PAGE
*50*
*
* GTBLK:
*
*        GET A BLOCK FROM THE SYMBOL TABLE.
*        ACCB HOLDS # OF BYTES WANTED;
*        ON EXIT, A BNE WILL SUCCEED IFF. THE BLOCK WAS OBTAINED.
*        OTHERWISE, ERROR SYMOV (SYMBOL TABLE OVERFLOW) HAS NOT
*        BEEN REPORTED AND X IS CLEARED.
*           IF THE BLOCK WAS OBTAINED, (X) IS THE ADDRESS; THE
*        ADDRESS IS ALSO SAVED IN STPTR.
*
GTBLK    EQU      *
         LDAA     STPTR+1           CALCULATE NEW POINTER.
         SBA                        NEED (B) BYTES.
         LDAB     STPTR             GET HIGH BYTE OF POINTER,
         SBCB     #0                WITH CARRY
         PSHA                       SAVE LOW BYTE,
         PSHB                       AND HIGH BYTE.
         SUBA     STLOW+1           COMPARE TO POINTER TO LAST AVAILABLE
         SBCB     STLOW             ...BYTE:
         BHI      GTBLK1            B/ THERE IS ROOM.
         LEAS	2,S	POP POINTER,
         LDX      #0                SIGNAL: NO BLOCK
         RTS                        AND SPLIT.
*
GTBLK1	PULX		Get pointer to block
         STX      STPTR             SAVE POINTER,
         RTS                        AND SPLIT.
         PAGE
*48*
*
* FSYM
*        FINDS SYMBOL WHICH IS IN SYM IN THE SYMBOL TABLE AS
*        A USER SYMBOL.  A HASHING ALGORITHM IS USED;  SEE
*        HASH (BELOW).  If entered through FINDSYM1, then X
*        is assumed to contain first link of the chain.  If the
*        symbol is found, then CCZ = 0, and SYMPT points to first
*        byte of entry.  Otherwise CCZ = 1, and SYMPT points to
*        the last entry in the chain.  Entries in the table are
*        assumed to be of the following form:
*
*        S:NEXT   ==> Pointer to next entry in chain, or zero if last.
*        S:NEXT+1
*        S:VAL    ==> Value of symbol.
*        S:VAL+1
*        S:TYPE   ==>  Type of symbol.
*        S:LEN    ==>  # of characters in text.
*        S:TEXT   ==>  Zero or more text characters.
*
FSYM     EQU      *
         BSR      HASH              GET HASH CODE FOR SYM
	BRA	FSYM1

FSYME    LDX      SYMPT             NEW ITERATION:  GET POINTER,
*
FSYM1    EQU      *                 COME HERE W/ YOUR OWN POINTER.
	DO	M6809=0
	LDAA	SYM	Length of target symbol
	ELSE
         LDD 	SYM               GET LENGTH OF TARGET SYMBOL
	FIN
FSYM2    STX      SYMPT             SAVE POINTER
         LDX      S:NEXT,X          GET POINTER TO NEXT.
         BEQ      FSYMX             B/ HIT END OF CHAIN.
*
	DO	M6809=0
         CMPA     S:LEN,X           CHECK LENGTH OF THIS SYM...
	ELSE
	CMPD	S:LEN,X	Check length and first byte
	FIN
         BNE      FSYM2             B/ CAN'T MATCH:  TRY NEXT.
*
* FOUND POSSIBLE MATCH:  DO COMPARISON.
*
         STX      SYMPT             SAVE POINTER TO ME...
         STX      TPTR              SAVE TEMP POINTER FOR COMPARISON.
	DO	M6809=0
         LDX      #SYM+1            GET POINTER TO SYM FOR COMPARE.
	BRA	FSYM4	AND ENTER LOOP.
	ELSE
	LEAY	S:TEXT,X	A(1st char of symbol)
	LDX	#SYM+1
	FIN

	DO	M6809=0
FSYM3    LDX      TPTR1             GET POINTER TO NEXT CH IN SYM.
FSYM4	LDAB     0,X               GET NEXT CH FROM TARGET SYM.
         INX                        POINT TO NEXT
         STX      TPTR1             AND SAVE.
         LDX      TPTR              GET POINTER TO NEXT IN TABLE,
         CMPB     S:TEXT,X          IS THIS ONE EQUAL, TOO?
         BNE      FSYME             B/ NO:  DO NEXT.
         INX
         STX      TPTR              POINT TO NEXT,
         DECA                       ALL DONE?
         BNE      FSYM3             B/ NO.
	ELSE
FSYM3	LDAB	,X+	Char from SYM
	CMPB	,Y+	 compare with char in table
	BNE	FSYME	B/ No match
         DECA                       ALL DONE?
         BNE      FSYM3             B/ NO.
	FIN
* FOUND SYM:  SYMPT IS CORRECT.
         LDX      SYMPT             RETURN A POINTER
         CLRA                       SET CCZ
         RTS                        SO LEAVE;  Z IS SET.
*
* HIT END OF CHAIN:  SYMPT POINTS TO PREVIOUS BLOCK.
*
FSYMX    LDX      SYMPT             GET POINTER, SETTING CCZ
         RTS                        AND EXIT.
	PAGE	- - - E N T S Y M - - -
*50.1*
*
*  ENTSYM:
*        ENTER A SYMBOL (FROM SYM) INTO TABLE.
*
* CALL:
*        JSR      ENTSYM
*
* INPUT:
*        SYM      CONTAINS SYMBOL
*        TYPE     CONTAINS TYPE FOR SYMBOL
*        VAL      CONTAINS VALUE FOR SYMBOL
*
*        STPTR    CONTAINS @ OF LOW END OF TABLE.
*
* OUTPUT:
*        SYMPT    CONTAINS ADDRESS OF SYMBOL BLOCK IN TABLE
*
* NOTE THAT VAL, TYPE AND SYM MUST BE ARRANGED TO LOOK LIKE A
* SYMBOL ENTRY.
*
ENTSYM   EQU      *
         LDAB     SYM               GET # OF BYTES IN SYMBOL,
         ADDB     #SENT:L           ADD ON OVERHEAD,
         PSHB                       AND SAVE FOR LATER.
         JSR      GTBLK             GO GET A BLOCK
         BNE      ENTSYM1           B/ NOT OUT OF ROOM.
	JSR	SYMOV		OUT OF ROOM: REPORT TO ERROR HANDLER
	LDX	NULLSYM+1	INITIALIZE THE DUMMY SYMBOL
	STX	NULLTMP+1
	LDX	NULLSYM+2+1
	STX	NULLTMP+2+1
	LDX	#NULLTMP-2	LOAD PTR TO DUMMY ENTRY
	CLR	NULLTMP
         INS                        POP SIZE,
         BRA      ENTSYM2           AND LEAVE.
*
NULLSYM	EQU	*
	FDB	0	VALUE
	FCB	T:UNDF	TYPE=UNDEF
	FCB	1,'?	SYMBOL NAME
*
* FOUND ROOM TO PUT THE SYMBOL IN:
*
ENTSYM1  EQU      *                 (REMEMBER: X HOLDS POINTER TO BLOCK)
         STX      TPTR              SAVE FOR LATER;
         PULB                       GET LENGTH BACK,
         LDX      #SYM:BLK          GET POINTER TO FIRST BYTE TO COPY,
ENTSYML  EQU      *
         STX      TPTR1             AND SAVE IT.
         LDAA     0,X               GET NEXT BYTE OF ENTRY,
         LDX      TPTR              GET POINTER INTO TABLE,
         STAA     0,X
         INX
         STX      TPTR              AND SAVE (TPTR)+
         LDX      TPTR1             GET NEW POINTER,
         INX                        POINT TO NEXT,
         DECB
         BNE      ENTSYML           B/ GO COPY ANOTHER.
         LDX      SYMPT             LINK THIS SYMBOL INTO CHAIN:
         LDAA     STPTR             GET NEW ADDRESS,
         STAA     S:NEXT,X          POKE IT IN.
         LDAA     STPTR+1
         STAA     S:NEXT+1,X        DITTO FOR LOW BYTE.
         LDX      STPTR             NOW, POINT TO NEW SYM,
         CLR      S:NEXT,X          AND MAKE IT THE LAST.
         CLR      S:NEXT+1,X
*
ENTSYM2  STX      SYMPT             SAVE POINTER TO THIS SYM,
         RTS                        AND EXIT.
	PAGE	- - - S Y M L K - - -
*51*
*
* NAME:  SYMLK
*        LOOK UP SYM IN SYMBOL TABLE, ENTER IT IF IT ISN'T THERE.
*
* INPUT:
*        SYM,     CONTAINS SYMBOL TO LOOK FOR.
*
* OUTPUT:
*        TYPE,    CONTAINS TYPE OF SYMBOL.
*        VAL,     CONTAINS VALUE OF SYMBOL.
*        SYMPT,   POINTS TO SYMBOL IN TABLE.
*        XTYPE,   MODIFIED TO ACCOUNT FOR SYMBOL'S TYPE.
*
* ERRORS:
*        IF SYMBOL IS UNDEFINED, THEN *UNDEF* IS FLAGGED.
*        IF SYMBOL IS A DOUBLE-DEF, THEN *DDEF-USE* IS FLAGGED.
*
SYMLK    JSR      FSYM              LOOK FOR THE SYMBOL IN THE TABLE.
         BEQ      SYMLK1            B/ FOUND IT:  CHECK TYPES.
*
* NEED TO ENTER SYM IN TABLE:
*
         LDAA     #T:UNDF           IT'S UNDEFINED,
         STAA     TYPE
         LDX      #0                W/ NO VALUE.
         STX      VAL
         BSR      ENTSYM            ENTER IT.  SYMPT WILL BE GOOD.
*
* (X) NOW POINTS TO SYMBOL ENTRY.
*
SYMLK1   EQU      *
         LDAA     S:TYPE,X
         STAA     TYPE              COPY THE TYPE TO APPROPRIATE PLACE.
         ORAA     #T:USED           IT HAS BEEN USED;
         STAA     S:TYPE,X          REMEMBER THAT.
         LDX      S:VAL,X           ALONG W/ THE VALUE.
         STX      VAL
         BITA     #T:UNDF           IS IT UNDEFINED?
         BEQ      SYMLK2A           B/ NO.
         JSR      UNDEF             ELSE FLAG UNDEFINED SYMBOL.
SYMLK2A  BITA     #T:DDEF           IS IT A DBL-DEF?
         BEQ      SYMLK3            B/ NO.
         JSR      UDDEF             *E* USE OF DBL-DEF.
SYMLK3	EQU	*
	RTS		>>>ALL DONE, GO HOME.<<<
	PAGE	- - - S Y M D E F - - -
*51*
*
* NAME:  SYMDEF
*        DEFINES A SYMBOL; CHECKS FOR DOUBLE DEFS, ETC.
*
* ENTRY:  SYMEQU
*        EQUATES SYM TO VAL, TYPE
*
* ENTRY:  SYMSET
*        SETS SYM TO VAL, TYPE:  CAN BE RESET.
*
* INPUT:
*        (A)      TYPE BIT: EITHER T:SET OR ZERO.
*        SYM      SYMBOL TO BE DEFINED.
*        VAL      VALUE TO BE GIVEN TO SYMBOL.
*        TYPE     TYPE TO BE GIVEN TO SYMBOL.
*
* ERRORS:
*        IF SYMBOL WAS PREVIOUSLY EQU'ED, OR IF SYMBOL WAS
*        SET AND NOW IS BEING EQU'ED, THEN
*        *DDEF* IS FLAGGED.
*
*
ENTIT	JMP	ENTSYM	HOP,HOP,HOP,HOP
*
SYMSET   LDAA     #T:SET            GET TYPE BITS
	BRA	SYMEQU1

SYMEQU   CLRA                       IT'S AN EQUATE.
SYMEQU1	ORAA     TYPE              SAVE TYPE BITS.
         STAA     TYPE
*
SYMDEF   EQU      *
         JSR      FSYM              IS SYMBOL DEFINED?
	BEQ	OLDSYM	IF SO, GO ON
	LDAA	PASS	CK/ SYMBOL DEF ATTEMPTED DURING PASS 2?
	BEQ	ENTIT	  B/ NO, THIS IS PASS 1.  ENTER IT.
	JSR	PHASERR	YES: REPORT **PHASE ERROR**,
	BRA	ENTIT	  BUT ENTER IT ANYWAY.
*
OLDSYM	EQU	*
*
* CHECK WHETHER IT IS LEGAL TO REDEFINE THE SYMBOL.
*
         LDAA     S:TYPE,X          GET THE TYPE BITS.
         BITA     #T:UNDF!T:FREF!T:SET  IS IT LEGAL TO REDEFINE?
         BEQ      SYMDEF2           B/ NO:  ERROR.
	BITA	#T:DDEF	CK/ WAS DDEF DETECTED IN PASS 2?
	BEQ	SYMDEF0	  B/ NO.  NO ERROR.
	BSR	SYMDDEF	**DOUBLE DEF** BUT SET VALUE HERE ANYWAY.
*
* MAYBE IT'S LEGAL TO REDEFINE THIS.
*
	LDX	SYMPT	IT IS A GOOD IDEA TO HAVE A REASONABLE PTR HERE;
	LDAA	S:TYPE,X	A GOOD TYPE IS HELPFUL TOO.
SYMDEF0	BITA	#T:SET	CK/ WAS SYMBOL PREVIOUSLY SET?
	BNE	SYMDEF3	  B/ YES: MAKE SURE THIS IS A SET OPERATION.
*
* FORWARD REF RESOLUTION:
*   (I DON'T LIKE THIS CODE VERY MUCH, BUT AH, WELL...)
*
	BITA	#T:UNDF	CK/ IS SYMBOL TRULY UNDEFINED?
	BNE	SYMDEF1	  B/ YES.  ALWAYS GIVE NEW VALUE.
	LDAB	TYPE	GET TYPE OF OPERATION (INPUT AS ARGUMENT).
	BITB	#T:SET	  CK/ ARE WE DOING A SET?
	BNE	SYMDEF2	  B/ YUP.  NOT KOSHER TO SET A PREV. EQU'D SYM.  
*
* IT'S AN FREF
*
	LDX	SYMPT	ELSE CHECK FOR PHASE ERROR
	LDX	S:VAL,X	  IS VALUE SUPPLIED SAME AS OLD
	CPX	VAL	  VALUE OF SYMBOL?
	BEQ	SYMDEF1	B/ YES:  NO PHASE ERROR HAS OCCURED.
	JSR	PHASERR	ELSE REPORT PHASE ERROR AND SET VAL ANYWAY.
SYMDEF1  LDX      SYMPT             MAKE SURE WE HAVE THE RIGHT POINTER
         LDAA     S:TYPE,X          GET SYMBOL TYPE,
         ANDA     #T:USED!T:DDEF     THESE ATTRIBUTES ARE PRESERVED
         ORAA     TYPE              ACROSS REDEFINITION.
         STAA     S:TYPE,X
         LDAA     VAL
         STAA     S:VAL,X
         LDAA     VAL+1
         STAA     S:VAL+1,X
         RTS
*
* THIS SYMBOL WAS OPERATED ON BY A SET BEFORE:  IS THIS A
* SET OPERATION?
*
SYMDEF3  LDAA     TYPE
         BITA     #T:SET
         BNE      SYMDEF1           B/ YES:  GO DO IT.
*
* ATTEMPT ILLEGALLY TO REDEFINE A SYMBOL.
*
SYMDEF2  LDAA     S:TYPE,X
	ANDA	#\T:FREF	FORWARD NO MORE! (OR SLAPSTICK?)
	ORAA     #T:DDEF           REMEMBER DOUBLE DEFINITION.
         STAA     S:TYPE,X          AND SAVE.
SYMDDEF	JMP	DDEF	*E* DOUBLE DEF!
	PAGE	- - L F E Q U - - -
*52*
*
* NAME:  LFEQU
*        EQUATES ALL OF LF TO VALUE, XTYPE
*
* ENTRY:  LFSET
*        SETS ALL OF LF TO VAL
*
* ENTRY:  LFPC
*        EQUATES ALL OF LF TO PC
*
LFPC     EQU      *
         LDX      PC
         STX      VAL               QUICK KLUDGE...
	CLRA		MAKE SURE TYPE IS SET CORRECTLY.
	STAA	XTYPE	  I.E., NON-FREF, ABS, EQU.
LFEQU    EQU      *
         CLRA                       THIS IS AN EQU..
	BRA	LFSET1

LFSET    EQU      *
         LDAA     #T:SET            THIS IS A SET...
LFSET1	ORAA	XTYPE	MERGE INTO EXP. TYPE.
LFEQ0    STAA     TYPE              SAVE THE TYPE,
         LDX      LFPTR             GET POINTER TO LF,
         JSR      GNOBX             GET FIRST OBJECT.
         INC      RJOBJ             REJECT IT.
LFEQL    JSR      GNOB:SJ           GET OBJ AND FORK.
         FCB      +O:TERM           A TERMINATOR?
         FDB      LFEQ1             B/ YES:  MAYBE DONE.
         FCB      +O:SYM            A SYMBOL?
         FDB      LFEQ2             B/ YES:  DEFINE IT.
         FCB      +0                END OF LIST.		
LFEQE    JMP      ILLF              *E* ILLEGAL LABEL FIELD.
*
LFEQ1    CMPB     #ASCII:COMMA	A COMMA?
         BEQ      LFEQL             B/ YES: GO ON EATING LF.
         RTS                        OTHERWISE: ALL DONE.
*
LFEQ2    JSR      SYMDEF            GO DEFINE THE SYMBOL,
         BRA      LFEQL             AND GO ON.
