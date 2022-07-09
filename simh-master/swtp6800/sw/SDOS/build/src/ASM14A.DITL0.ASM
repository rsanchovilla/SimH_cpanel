	TITLE	- - -   D E C O D E   L I N E   - - -
	PAGE	- - -   D O I T L   - - -
*9*
*
*  DOITL--
*
*        DECODES COMMAND AND FIGGERS OUT WHAT TO DO W/ IT.
*
*        (1)      IF CF(1) IS 3 OR 4 CH, AND IF FIRST 3 ARE A
*                 VALID OP, THEN WE SCAN AN OPCODE.
*
*        (2)      IF NOT (1) THEN IF CF(1) IS A SYMBOL ONLY,
*                 AND IF THAT SYMBOL IS IN THE PSUEDO-OP TABLE,
*                 (USER OR ASSEMBLER) THEN WE SCAN A DIRECTIVE.
*
*        (3)      IF (1) NOR (2) THEN CF IS HANDLED AS A FUNNY
*                 DATA EXP.
*
*        AFTERWARDS, LIST IS CALLED.
*
*
DOITDR9	JMP	DOITDR
DOITEX9  JMP      DOITEX
DOITL    EQU      *
         LDX      CFPTR
         STX      AFPTR             PREPARE TO LOOK FOR AF.
         JSR      CHCFSYM           IS CF(1) A SYMBOL??
         BNE      DOITEX9           B/ NO==> GO TREAT AS FUNNY DATA STAT
	CMPB	#',	CK/ IS TERMINATOR A COMMA? (B SET BY CHCFSYM)
	BEQ	DOITEX9	  B/ YUP.  CAN'T BE OPCODE.
         LDAA     #$FF              SET FLAG:  NO REGISTER SEEN.
         STAA     REG
         LDAB     SYM               GET LENGTH OF SYMBOL,
         LSRB                       IF SYM WAS ORIGINALLY ...
         TPA                        ...3 OR 4 CHARACTERS LONG, THEN...
         ADCB     #-2               ...RESULT IS ZERO.
         BNE      DOITDR9           B/ NOT AN OPCODE.

**** CHECK FOR M6801 OPCODES, IF REQUESTED ****
	LDAB	OPT1F	LOAD APPROPRIATE OPTION FLAG,
	BITB	#OPT1:6801	  NEED TO LOOK?
	BEQ	DOITL$1	  B/ NO, THIS IS "NM6801".
	PSHA		6801:  SAVE P REG,
	LDX	#M6801OPS	  LOAD @ OF OP TABLE,
	JSR	FSYM1	  AND SEE IF SYM IS ONE WE KNOW.
	PULA		  (SNEAKILY RESTORE P INTO ACCA)
	BNE	DOITL$1	B/ NO LO SE; TREAT AS 6800 OPCODE.

**** 6801 OPCODE RECOGNIZED; X POINTS TO SYMBOL BLOCK ****
	LDX	S:VAL,X	FETCH OPCODE, OPCODE GROUP,
	BRA	DOOPCX	  AND GO JOIN COMMON CODE.

**** NO 6801 OPCODE.  TRY FOR 6800 OP *****
DOITL$1	EQU	*
*
* POSSIBLY AN OPCODE.  IF 4 CH, THEN LAST MUST "A/B"
         TAP                        WHAT WAS THE RESULT
         BCS      DOITO3            B/ ONLY 3 CH
         LDAB     SYM+4             WHAT IS THE 4TH CHARACTER.
         SUBB     #'A               "A"
         LSRB
	BNE	DOITDR9	B/ NOT A/B ==> DIRECTIVE.
         ADCB     #0                IF A, THEN RB=0, ELSE 1
         STAB     REG               SAVE RESULT.
	PAGE	- - - OPCDODE DECODING - - -
*
* GOT A 3 CHAR CF(1) OR 3+A/B
*
* LOOK IN SYM TBL TO
* FIND OP CODE.
DOITO3   LDX      #OPTBL            GET ADDRESS OF TABLE.
DOITOL   LDAA     SYM+1             CHECK NEXT SYM
         CMPA     0,X
         BNE      DOITO4
         LDAA     SYM+2
         CMPA     1,X
         BNE      DOITO4
         LDAA     SYM+3
         CMPA     2,X
         BEQ      DOOPC             B/ FOUND A MATCH.
DOITO4   BLS      DOITDR            B/ NOT IN TABLE ==> DIRECTIVE
         INX
         INX
         INX
         INX
         INX
         BRA      DOITOL            TRY NEXT.
	PAGE
*10*
*
*
* FOUND OPCODE;  GET GROUP, VALUE
DOOPC    LDX      3,X
DOOPCX	STX      OP                OP & GRP ARE ADJACENT
*
* GOT GROUP, VALUE:  CHECK WHETHER TO SCAN FOR REGISTER.
         LDAA     GRP
         BGT      DOOCNR            B/ NO REGISTER ALLOWED
* REG ALLOWED
         LDAB     REG               REGISTER ALREADY SEEN?
         BGE      DOOCGO            B/ GOT REGISTER.
*
*GOT OP THAT MAY HAVE REG BUT NO REG YET
*   3 CHAR OPCODE:  SCAN FOR REGISTER
         JSR      FINDAF            FIND ARGUMENT FIELD
         JSR      GNOBJ             GET NEXT OBJECT
         CMPA     #O:SYM            IS IT A SYMBOL?
         BNE      DOOC2             B/ NO --> NO REGISTER.
	JSR	CKEOF	SEE IF END OF IELD NEXT
	BNE	DOOC2	B/ NO --> NO REGISTER
         LDX      #$100+'A          SYMBOL MUST BE "A" OR "B"
         CLRA                       CLEAR THE REGISTER FLAG
         CPX      SYM               IS THE SYMBOL "A"?
         BEQ      DOOCR             B/ YES:  RA CONTAINS REG.
         INX                        ELSE TRY "B"
         INCA                       AND SET FLAG FOR REG.B
         CPX      SYM               IS THE SYMBOL "B"?
         BNE      DOOC2             B/ NO:  NO REGISTER.
DOOCR    STAA     REG               SAVE REGISTER FLAG,
         LDAA     GRP               RESTORE GROUP CODE
	PAGE
*
* NOW GO DO APPROPRIATE GROUP
DOOCGO   PSHA                       SAVE REG A
         JSR      LFPC              SET LF TO HERE;
         PULA
         JSR      ISIT              WHAT GROUP...
         FCB      T:DRNI,T:DYDR,T:DYDX,T:BRA,T:JMP,T:DXNI,0
         BNE      DOOCG1            B/ DONT FIND AF
         JSR      FINDAF
         LDAA     GRP               GET GROUP CODE
DOOCG1   EQU      *
         TAB      GET VALUE IN SAFE PLACE...
         SUBB     #T:BASE                MAKE INDEX FROM 0 TO N
         JSR      TBJMP             DO TABLE JUMP
         FDB      BTBL
* COME HERE IF REGISTER IS NOT FOUND
DOOC2    LDX      CFPTR
         STX      AFPTR
         LDAA     GRP               GET GROUP CODE
         BGE      DOOCGO            B/ OK NOT TO HAVE REGISTER
	JSR	ILLCF	ELSE =>USER IS TWIT<=
	CLRA		  AND TREAT AS REGISTER A.
	BRA	DOOCGO	  (NOTE THAT AFPTR HAS ALREADY BEEN RESET.)
*
         PAGE
 
*11*
*
* COME HERE IF NO REGISTER ALLOWED:
DOOCNR   LDX      CFPTR
         STX      AFPTR
         LDAB     REG
         BLT      DOOCGO            B/ NO REGISTER THERE;
*	BRA	DOITDR	  ELSE FALL INTO DIRECTIVE PROCESSING.
	INCLUDE ASM14A.DREX.ASM	************************************
	TITLE	- - - O P C O D E   P R O C E S S I N G - - -
	PAGE	- - - O P C O D E   B R A N C H   T A B L E - - -
   
*12*
*
* BRANCH TABLE
*
BTBL	FDB	DYADRNI	GROUP-3	DYADIC REGISTER, NO IMMEDIATE.
	FDB      PSHPUL            GROUP-2	MONADIC IN A OR B
         FDB      DYADR             GROUP-1	DYADIC IN (A OR B) AND MEMORY.
         FDB      MONAD             GROUP 0	MONADIC IN A, B, OR MEMORY.
         FDB      NILAD             GROUP 1	NILADIC.
         FDB      DYADX             GROUP 2	DYADIC WITH ONE SOURCE IMPLICIT; 16-BIT IMM.
         FDB      BRANCH            GROUP 3	RELATIVE BRANCH.
         FDB      JMPJSR            GROUP 4	FUNNY FOR INSTS W/ INDEXED OR EXTENDED ONLY.
	FDB	DYADXNI	GROUP 5	DYADIC IN X, S; IMMEDIATE NOT ALLOWED.
*
         PAGE     - - - D Y A D I C   R E G I S T E R - - -
*
* DYADR -- DYADIC REGISTER: EG. ADD, SUB
DYADRSCN	LDAA     REG               GET REGISTER VALUE
         LSRA                       :00 1
         RORA                       :80 0
         RORA                       :40 0
         ORAA     OP                MAKE OPCODE
         LDAB     OP                SAVE OLD OP,
         STAA     OP                UPDATE
	RTS
*
* CHECK FOR IMMEDIATE.
DYADR	EQU	*
	BSR	DYADRSCN	SET UP REGISTER IN OPCODE.
         BSR      CHIMM             CHECK IMMEDIATE & EVAL IF SO
         BNE      DYADR4            B/ NOT IMMEDIATE.
         LDAA     VAL               IN RANGE?
         BEQ      DYADR1            B/ YES.
	TST	VAL+1	IS IT POSSIBLE THAT THE SIGN IS EXTENDED?
	BGE	DYOVF	IF NOT, ERR
         COMA                       IS HIGH ORDER STUFF SIGN
         BEQ      DYADR1            B/ YES.
DYOVF	JSR	OVRFLW	*E* OVERFLOW
DYADR1   EQU      *
OUT2     CLRA                       OUTPUT OP, VALL
OUT2O    ORAA     OP                OR OP W/ ACCA, OUTPUT OP,VAL
OUT2A    JSR      PUTB              OUTPUT A
	JSR	ECKEOF	ERROR IF END OF FIELD NOT NEXT
OUTVL    LDAA     VAL+1             OUTPUT LOW VAL
OUT1A    EQU      *
OUTL     JSR      PUTB              PUT THE BYTE
OUTX     JMP      LIST              AND FLAKE OUT.
*
*
* CHECK FOR INDEXED:
*
DYADRNI	BSR	DYADRSCN	SET UP REGISTER IN OPCODE.

DYADXNI	EQU	*	ENTER HERE FOR STX, ETC.
DYADR4   BSR      CHINDX            INDEXED INSTRUCTION?
         BSR      CHDIR             NO:  DIRECT?
*
*VAL=AF --> JUST OUTPUT IT
*
         JMP      DOEXT             NO==> EXTENDED
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
         INX                        POINT AT NEXT CH,
         JSR      EVAFX             GO EVALUATE THE EXPRESSION.
         CLRB                       SET APPROPRIATE CC.
CHIRTS   RTS                        AND EXIT, SUCCESS.
*
CHIRTN   INX                        SET CC # 0,
         RTS                        AND EXIT, FAIL.
         PAGE     - - - C H I N D X - - -
*
* SUBROUTINE TO CHECK FOR INDEXED INSTRUCTION.  EATS THE FOLLOWING
* CONSTRUCTS AS INDEXED:
*        X
*        ,X
*        <EXP>,X
*
* NOTE THAT X MUST BE ALL BY ITSELF IN ORDER TO BE RECOGNIZED;
*        +X, (X), X+3 ETC. WILL NOT BE RECOGNIZED AS INDEXED INSTRUCTION
*
CHINDX   LDX      AFPTR             GET POINTER TO AF,
         JSR      GNOBX             GET FIRST OBJECT.
         CMPA     #O:SYM            IS IT A SYMBOL?
         BNE      CHIND1            B/ NO:  SEE IF <EXP>,X
         BSR      ISITX             IS SYM = "X"?
         BNE      CHIND1            B/ NO:  NOT SIMPLEST FORM.
CHINDCL  CLRA                       CLEAR VALUE
         STAA     VAL
         STAA     VAL+1
*  INDEXED INSTRUCTION FOUND...
CHIN2    INS                        POP RETURN @
         INS
         LDAA     VAL               GET HIGH BYTE OF INDEX VAL.
         BEQ      CHIN0             B/ NO OVERFLOW...
         JSR      OVRFLW            *E* OVERFLOW ON INDEX INSTR.
CHIN0    LDAA     #$20              ELSE 'OR' IN INDEX BIT,
	ORAA	OP	OR INTO THE OP
	JSR	PUTB	PUT IT
	BRA	OUTVL	GO PUT OUT THE DISPLACEMENT
*
CHIND1   LDX      AFPTR             RESET TO START OF AF
         STX      EXPTR
         JSR      GTCMA             FIND THE COMMA...
         BVC      CHIRTS            B/ WRETCH... NO CAN BE INDEXED
         JSR      GNOBJ             GET WHATS AFTER THE COMMA...
         CMPA     #O:SYM            IS IT A SYMBOL?
         BNE      CHIRTS            B/ NO ==> NOT INDEXED.
         BSR      ISITX             IS SYMBOL X?
         BNE      CHIRTS            B/ NO ==> NOT INDEXED
	LDX	AFPTR	SEE IF THE FIRST AF CHAR WAS A COMMA (AS IN ,X)
	LDAA	0,X	LOAD 1ST CHAR
	CMPA	#',	IS IT?
	BEQ	CHINDCL	IF SO, SET NULL OFFSET
         JSR      EVAF              GET VAL FOR AF
         BRA      CHIN2             AND GO CHECK THAT ITS GOOD.
****
**** CHECK THAT SYM IS "X"
ISITX    LDX      #$100+'X
         CPX      SYM
	BNE	CHIRTS	RETURN NE IF NOT X SYMBOL
	JMP	CKEOF	SEE IF EOF NEXT
*
         PAGE     - - - C H D I R - - -
*
* MISC BRANCHES TO GET US WHERE WE'RE GOING
*
OUT2A1	BRA	OUT2A
DYADR41	BRA	DYADR4
*
*
*
* SUBROUTINE TO CHECK FOR DIRECT MODE.
CHDIR    EQU      *                 WHAT'S THE EXPRESSION TYPE?
         JSR      EVAF              EVALUATE AF
         LDAA     XTYPE             GET TYPE BACK.
         BITA     #XT:FREF!XT:UNDF!XT:UNEV  IS IT EVALUABLE?
         BNE      CHIRTS            B/ NO ==> NOT DIRECT.
         LDAA     VAL               GET HIGH BYTE OF VAL...
         BNE      CHIRTS            B/ NOT ZERO ==> CAN'T BE DIRECT.
         INS                        ELSE POP RETURN ADDRESS,
         INS
         LDAA     #$10              MERGE IN NEAT STUFF
         JMP      OUT2O             AND GO DEPOSIT.
*
	PAGE	- - -  M O N A D I C   G R O U P  - - -
*14*
*
*
* MONADIC GROUP:  COM, NEG, ETC.
MONAD    LDAA     REG               REGISTER SEEN?
         BLT      MONAD1            B/ NO ==> MUST GET AF
         BEQ      MONAD0
         LDAA     #$10              GET VAL FOR REG. B
	BRA	MONAD0
*
*  NILADIC:  INX, DES, ETC.
NILAD    CLRA                       IN ORDER TO GET OPCODE.
MONAD0   ORAA     OP                MERGE INTO OPCODE,
         JMP      OUT1A             AND OUTPUT 1 BYTE.
         PAGE     - - - M O N A D I C S ,   J U M P S ,   E T C - - -
*
* NO REGISTER SEEN:  REFERS TO MEMORY
MONAD1   JSR      FINDAF            GET POINTER TO AF
JMPJSR   JSR      CHINDX            CHECK FOR INDEXED;
*
*  DO WORK FOR EXTENDED MODE INSTRUCTIONS
DOEXTV   JSR      EVAF              GET VALUE FOR AF...
DOEXT    LDAA     #$30              GET ADDRESSING BITS
OUT3A    ORAA     OP                MAKE EXTENDED
         JSR      PUTB              OUTPUT THE BYTE.
         LDAA     VAL               OUTPUT VAL(H)
         BRA      OUT2A1
         PAGE     - - - D Y A D I C S   I N   X ,   S - - -
*
*  DYADICS FOR X, S, ETC:  CPX, LDS
DYADX	JSR      CHIMM             CHECK FOR IMMEDIATE.
         BNE      DYADR41           B/ NOT IMMEDIATE, TRY OTHER MODES.
         CLRA                       ELSE OUTPUT THE OP,
         BRA      OUT3A             ...VAL(H) AND VAL(L).
         PAGE     - - - P U S H ,   P U L L - - -
*15*
*
* PSHPUL:  PSHA/B PULA/B INSTRUCTIONS.
PSHPUL   LDAA     REG               GET VALUE TO "OR" W/ OPCODE
         BRA      MONAD0            AND GO OUTPUT IT.
         PAGE     - - - B R A N C H E S - - -
*
* RELATIVE BRANCHES:
BRANCH   JSR      EVAF              EVALUATE THE ARG...
         LDAA     0,X               GET THE TERMINATOR
         CMPA     #',               IS IT A COMMA?
         BNE      BRAN1             B/ NO:  NOT TRYING TO INDEX.
         JSR      ILLAF             *E* TRIED TO INDEX A BRANCH.
BRAN1    LDAA     XTYPE             GET THE EXPRESSION TYPE
         BITA     #XT:FREF!XT:UNDF  IS IT EASY TO CHECK?
         BEQ      BRAN2             B/ YES.
         LDAA     PASS              IS THIS PASS 1?
         BEQ      OUT222             B/ YES:  OUTPUT 2 BYTES AND GO ON.
* CHECK FOR OUT OF RANGE BRANCH
BRAN2    LDAB     VAL+1             GET THE VALUE
         LDAA     VAL
         SUBB     PC+1
         SBCA     PC                (A,B) := VAL-(PC+2)
         SUBB     #2
         STAB     VAL+1             SAVE NEW VAL,
         SBCA     #0
         BEQ      BRANP             CHECK VAL FOR IN RANGE...
         BLT      BRANN             B/ VAL<0
BRANE    JSR      BRANERR           *E* OUT OF RANGE BRANCH
         LDAA     #-2               GENERATE A "BRA *"
         STAA     VAL+1
OUT222	JMP	OUT2	AND OUTPUT THE CODE
*
* BRANCH IN NEGATIVE DIRECTION.  (A)  MUST = $FF
*
BRANN    COMA
         BNE      BRANE             B/ (A) WAS NOT SIGN.
         COMB                       PREPARE B FOR RANGE CHECK.
*
* BRANCH IN POSITIVE DIRECTION.  (A) KNOWN TO BE ZERO; (B) MUST BE
* UNSIGNED.
*
BRANP    TSTB                       IS B GOOD?
         BGE      OUT222             B/ B IS UNSIGNED ==> GOOD
         BRA      BRANE             ELSE GO ERROR.
	TITLE	- - - A S S E M B L E R   T A B L E S - - -
         PAGE     - - - O P C O D E   T A B L E - - -
*16
*
* OPTBL--
*
*        5 BYTES PER OPCODE:
*
*        3 BYTES OF TEXT;
*        1 BYTE OF VALUE
*        1 BYTE OF TYPE
*
*
*
* NOTE THAT TYPE CODES THAT ARE .LE. ZERO MEAN THAT A REGISTER CAN BE
* ASSOCIATED; IF TYPE .LT. ZERO, THERE MUST BE A REGISTER;
* IF TYPE .EQ. ZERO, THERE MAY BE A REGISTER;
* IF TYPE .GT. ZERO, THERE MAY NOT BE A REGISTER.
*
OPTBL    EQU      *
         FCC      /ABA/
         FCB      $1B,T:NIL
         FCC      /ADC/
         FCB      $89,T:DYDR
         FCC      /ADD/
         FCB      $8B,T:DYDR
         FCC      /AND/
         FCB      $84,T:DYDR
         FCC      /ASL/
         FCB      $48,T:MOND
         FCC      /ASR/
         FCB      $47,T:MOND
         FCC      /BCC/
         FCB      $24,T:BRA
         FCC      /BCS/
         FCB      $25,T:BRA
         FCC      /BEQ/
         FCB      $27,T:BRA
         FCC      /BGE/
         FCB      $2C,T:BRA
         FCC      /BGT/
         FCB      $2E,T:BRA
         FCC      /BHI/
         FCB      $22,T:BRA
	FCC	/BHS/
	FCB	$24,T:BRA
         FCC      /BIT/
         FCB      $85,T:DYDR
         FCC      /BLE/
         FCB      $2F,T:BRA
	FCC	/BLO/
	FCB	$25,T:BRA
         FCC      /BLS/
         FCB      $23,T:BRA
         FCC      /BLT/
         FCB      $2D,T:BRA
         FCC      /BMI/
         FCB      $2B,T:BRA
         FCC      /BNE/
         FCB      $26,T:BRA
*17*
         FCC      /BPL/
         FCB      $2A,T:BRA
         FCC      /BRA/
         FCB      $20,T:BRA
         FCC      /BSR/
         FCB      $8D,T:BRA
         FCC      /BVC/
         FCB      $28,T:BRA
         FCC      /BVS/
         FCB      $29,T:BRA
*
         FCC      /CBA/
         FCB      $11,T:NIL
         FCC      /CLC/
         FCB      $0C,T:NIL
         FCC      /CLI/
         FCB      $0E,T:NIL
         FCC      /CLR/
         FCB      $4F,T:MOND
         FCC      /CLV/
         FCB      $0A,T:NIL
         FCC      /CMP/
         FCB      $81,T:DYDR
         FCC      /COM/
         FCB      $43,T:MOND
         FCC      /CPX/
         FCB      $8C,T:DYDX
         FCC      /DAA/
         FCB      $19,T:NIL
         FCC      /DEC/
         FCB      $4A,T:MOND
         FCC      /DES/
         FCB      $34,T:NIL
         FCC      /DEX/
         FCB      $09,T:NIL
         FCC      /EOR/
         FCB      $88,T:DYDR
         FCC      /INC/
         FCB      $4C,T:MOND
         FCC      /INS/
         FCB      $31,T:NIL
         FCC      /INX/
         FCB      $08,T:NIL
         FCC      /JMP/
         FCB      $4E,T:JMP
         FCC      /JSR/
         FCB      $8D,T:JMP
*18*
         FCC      /LDA/
         FCB      $86,T:DYDR
         FCC      /LDS/
         FCB      $8E,T:DYDX
         FCC      /LDX/
         FCB      $CE,T:DYDX
         FCC      /LSR/
         FCB      $44,T:MOND
         FCC      /NEG/
         FCB      $40,T:MOND
         FCC      /NOP/
         FCB      $01,T:NIL
         FCC      /ORA/
         FCB      $8A,T:DYDR
         FCC      /PSH/
         FCB      $36,T:PSH
         FCC      /PUL/
         FCB      $32,T:PSH
         FCC      /ROL/
         FCB      $49,T:MOND
         FCC      /ROR/
         FCB      $46,T:MOND
         FCC      /RTI/
         FCB      $3B,T:NIL
         FCC      /RTS/
         FCB      $39,T:NIL
         FCC      /SBA/
         FCB      $10,T:NIL
         FCC      /SBC/
         FCB      $82,T:DYDR
         FCC      /SEC/
         FCB      $0D,T:NIL
         FCC      /SEI/
         FCB      $0F,T:NIL
         FCC      /SEV/
         FCB      $0B,T:NIL
*
         DO       NICE=1
         FCC      /SK1/
         FCB      $85,T:NIL
         FCC      /SK2/
         FCB      $8C,T:NIL
         FIN
*
         FCC      /STA/
         FCB      $87,T:DRNI	**NO IMMEDIATE ALLOWED**
         FCC      /STS/
         FCB      $8F,T:DXNI	**NO IMMEDIATE ALLOWED**
         FCC      /STX/
         FCB      $CF,T:DXNI	**NO IMMEDIATE ALLOWED**
         FCC      /SUB/
         FCB      $80,T:DYDR
         FCC      /SWI/
         FCB      $3F,T:NIL
         FCC      /TAB/
         FCB      $16,T:NIL
         FCC      /TAP/
         FCB      $06,T:NIL
         FCC      /TBA/
         FCB      $17,T:NIL
         FCC      /TPA/
         FCB      $07,T:NIL
         FCC      /TST/
         FCB      $4D,T:MOND
         FCC      /TSX/
         FCB      $30,T:NIL
         FCC      /TXS/
         FCB      $35,T:NIL
         FCC      /WAI/
         FCB      $3E,T:NIL
         FCB      -1&$FF
	PAGE	- - - 6 8 0 1   O P C O D E   T A B L E - - -
*
*  6 8 0 1   O P C O D E   T A B L E
*
*	THIS TABLE IS FORMATTED AS A STANDARD LINKED SYMBOL TABLE.
* S:VAL	OPCODE ROOT VALUE.
* S:VAL+1	OPCODE GROUP.
* S:TYPE	<CURRENTLY UNUSED; SET TO ZERO>
*
*	TO SEARCH FOR A SYMBOL IN THIS TABLE, DO:
*		LDX	#M6801OPS
*		JSR	FSYM1
*		BNE	<NOT-FOUND>
*		BEQ	<FOUND>	... X HOLDS @ OF SYMBOL.
*

M6801OPS	FDB	*+2	HEAD LINK.
*
	#M6801.2	<NEXT>
	$3A,T:NIL,,3
	FCC	/ABX/
*
M6801.2	#M6801.3
	$C3,T:DYDX,,4
	FCC	/ADDD/
*
M6801.3	#M6801.4
	$05,T:NIL,,4
	FCC	/ASLD/
*
M6801.4	#M6801.5
	$24,T:BRA,,3
	FCC	/BRN/
*
M6801.5	#M6801.6
	$8D,T:DXNI,,3
	FCC	/JSR/
*
M6801.6	#M6801.7
	$CC,T:DYDX,,3
	FCC	/LDD/
*
M6801.7	#M6801.8
	$48,T:MOND,,3
	FCC	/LSL/
*
M6801.8	#M6801.9
	$48,T:NIL,,4
	FCC	/LSLA/
*
M6801.9	#M6801.A
	$58,T:NIL,,4
	FCC	/LSLB/
*
M6801.A	#M6801.B
	$05,T:NIL,,4
	FCC	/LSLD/
*
M6801.B	#M6801.C
	$04,T:NIL,,4
	FCC	/LSRD/
*
M6801.C	#M6801.D
	$3D,T:NIL,,3
	FCC	/MUL/
*
M6801.D	#M6801.E
	$36,T:NIL,,4
	FCC	/PSHX/
*
M6801.E	#M6801.F
	$38,T:NIL,,4
	FCC	/PULX/
*
M6801.F	#M6801.G
	$CD,T:DXNI,,3
	FCC	/STD/
*
M6801.G	#M6801.I
	$83,T:DYDX,,4
	FCC	/SUBD/
*
M6801.I	EQU	0
*
