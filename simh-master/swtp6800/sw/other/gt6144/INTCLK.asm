	NAM INTCLK
; * CLOCK PROGRAM FOR MP-T INTERRUPT TIMER
; * DISPLAYS TIME IN 12 HOUR FORMAT 12:24.56
; *
; * MEMORY ADDRESS $A002 AND $A003 HOLD ADDRESS
; * OF THE MP-T PORT, DEFAULT IS #5 OR $8010
; *
; * ADDRESS $A000 AND $A001 MUST BE LOADED WITH
; * INTERUPT SREVICE ROUTINE WHICH IS $013D
; * 
	ORG $0100
	SEI
 	LDS #$A042
	LDX #$0020
 	JSR PDATA1
 	LDX #$0060
 	JSR PDATA1
 	LDX #$0061
 	LDAA #$0D
 	JSR OUTEEE
 LOOP1 	JSR INEEE
 	STAA 0,X
 	CPX #$0068
 	BEQ CONFIG
 	INX
 	BRA LOOP1
 CONFIG	LDX $A002
 	LDAA #$FF
 	STAA 2,X
 	LDAA #$3D
 	STAA 3,X
 	LDAA #$80
 	STAA 2,X
 	LDAA #$06
 	STAA 2,X
 	NOP
 	CLI
 LOOP2	NOP
 	BRA LOOP2
 INTSER	LDX $A002
 	LDA A 2,X
 	LDX #$0060
 	LDAA #$39
 	LDAB #$30
 	CMP A 8,X
 	BEQ SKIP2
 	INC 8,X
 	BRA DISPLY
 SKIP2	STAB 8,X
 	LDAA #$35
 	CMPA 7,X
 	BEQ SKIP3
 	INC 7,X
 	BRA DISPLY
 SKIP3	STAB 7,X
 	LDAA #$39
 	CMPA 5,X
	BEQ SKIP4
 	INC 5,X
 	BRA DISPLY
 SKIP4	STAB 5,X
 	LDAA #$35
 	CMPA 4,X
 	BEQ SKIP5
 	INC 4,X
 	BRA DISPLY
 SKIP5	STAB 4,X
 	LDAA #$32
 	LDAB #$39
 	CMPA 2,X
 	BEQ SKIP6
 	CMP B 2,X
 	BEQ SKIP8
 	INC 2,X
 	BRA DISPLY
 SKIP6	LDAA #$31
 	CMPA 1,X
 	BEQ SKIP7
 	INC 2,X
 	BRA DISPLY
 SKIP7	STAA 2,X
 	LDAB #$20
 	STAB 1,X
 	BRA DISPLY
SKIP8 	LDAA #$30
	LDAB #$31
	STAA 2,X
	STAB 1,X
DISPLY	LDX #$0060
	JSR PDATA1
 	RTI
 
	ORG $0020
 
 	FCB $10,$16,$0D,$00,$00,$00
	FCC /SWTPC 6800 COMPUTER SYSTEM TIME:/
 	FCB $0D,$0A,$00,$00,$00,$04
 
	ORG $0060
 
 	FCB $0D
 	FCC /HH:MM.SS /
 	FCB $04
 
	ORG $A048
	
 	FDB $0100
 
	ORG $A002
 
 	FDB $8010
 
	ORG $A000
 
 	FDB $013D
 
INEEE 	EQU $E1AC
PDATA1 	EQU $E07E
OUTEEE 	EQU $E1D1
 
	END