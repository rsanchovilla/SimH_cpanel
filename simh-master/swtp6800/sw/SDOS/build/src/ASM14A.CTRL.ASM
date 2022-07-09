	PAGE
	WITH	WI=106,DE=51
	PCC	1
	IFUND	M6800
M6800	EQU	1
M6801	EQU	0
M6809	EQU	0
	FIN
	IFUND	MC6809
	TITLE	- - -   M C S - 6 8 0 0   A S S E M B L E R   - - -
	NAME	ASM/6800 1.4
TYPE1RECORD	EQU	1	Binary type file (6800)
	ELSE
	TITLE	- - -   M C S - 6 8 0 9   A S S E M B L E R   - - -
	NAME	ASM/6809 1.4
TYPE1RECORD	EQU	2	Binary type file (6809)
	FIN
DEBUG	EQU	1

	IFUND	ASSEMBLY
ASSEMBLY	EQU	'A	Default is "A"
	FIN
	PAGE	- - - ASSEMBLY CONTROL - - -
	PCC	1
	TABS	10,19,37,42,50,58
	PCC	0
**** THIS FILE COPYRIGHT (C) 1979 TERRILL MOORE.
*	INFRINGERS BEWARE:
*	VIOLATORS OF COPYRIGHT ARE SUBJECT TO $150 FINE PER VIOLATION;
*	COURTS HAVE ESTABLISHED THAT ANY TRANSFER OF A COPYRIGHTED
*	COMPUTER FILE FROM ONE PLACE IN THE MACHINE TO ANOTHER IS
*	AN INFRINGEMENT.  ALL COPIES MADE
*	OF THIS FILE WILL BE CONSIDERED AN INFRINGEMENT UNLESS PRIOR
*	PERMISSION IS OBTAINED FROM TERRILL M. MOORE, 6504 1/2 W. OCEAN FRONT,
*	NEWPORT BEACH, CALIFORNIA  92661
*
*
*
*	BY AUTHORITY GRANTED TO SOFTWARE DYNAMICS BY T. MOORE:
*	SOFTWARE DYNAMICS HAS TAKEN THE LIBERTY TO REVISE AND IMPROVE
*	ASM-125 WITH SHARED PROFITS GOING TO BOTH T. MOORE AND SD.
*
*
	PAGE
* 9/03/79	Version 1.3:
*
*		--Changed assembler to output SDOS-style load files instead
*		  of MIKBUG (tm) format.  This can be reset by setting SDOSbinary
*		  to 0 and reassembling.
*
*		--Added logic to END command to find a start address & put it
*		  into the file.
*
*		--Incorporated 6801 patches from (unreleased) version 1.3C
*
*
* 9/5/79  tmm	--Added INCLUDE file logic.  This is rev C.
*
*
* 12/5/79 WAM -- INCLUDE logic and other bug cleanup.  Rev F.
*
* 3/6/80 - 10/04/81   WAM -- 6809 changes. Rev G.
*
* 10/04/81  WAM -- 680C Changes to rev G
*
* 08/16/83 - 00/00/83  WAM -- Version 1.4A:
*			* Changes to source includes for D1:
*			* Fix to END start address processing
*			* Minor fixes to INCLUDEs
*
*
*
	PAGE
*	The INCLUDE structure for the assembler is as follows:
*
*
*
*         +--> DEFS   +--> DITL0 --> DREX
*         |           |
*         +--> MAIN --+--> DITL9 --> DREX
*         |           |
*  CTRL --+           +--> FCONV
*         |
*         +--> EVAL
*         |
*         +--> IOFM
*
*  IODR --> SDOSUSERDEFS.ASM
*
*
*
*
* What the files are about:
*       ASMnnn.CTRL.ASM                Master control file for assembly
*       ASMnnn.DEFS.ASM                Assembler definitions
*       ASMnnn.MAIN.ASM                Main part of assembler
*       ASMnnn.DITL9.ASM               6809 Source line decoder
*       ASMnnn.DITL0.ASM               6800 Source line decoder
*       ASMnnn.DREX.ASM                Directives and data expression parser
*       ASMnnn.FCONV.ASM               Floating point number parser
*       ASMnnn.EVAL.ASM                Expression evaluator
*       ASMnnn.IOFM.ASM                I/O formatting and calls to IO package
*
*       ASMnnIODR.ASM                  IO package, separate assembly.
*
*
*
*
	INCLUDE	ASM14A.DEFS.ASM
	INCLUDE	ASM14A.MAIN.ASM
	INCLUDE	ASM14A.EVAL.ASM
	INCLUDE	ASM14A.IOFM.ASM

	END	START
