        NAM     BASPATCH
        OPT     O,NOM,NOP 
*THIS PROGRAM PATCHES THE ASTRAL 2000 BASIC
*TO DISABLE AUTO-PAPER READ ACTIVATION WHEN ':' BASIC
*PROMPT IS SHOWN, AND AUTO-PAPER PUNCH ACTIVATION THEN
*LIST COMMAND IS ISSUED. IT ALSO FIXES BASIC LOAD/SAVE
*AND TLOAD/TSAVE COMMANDS 
*
*
* remove ^R ($12) PTP ON at beginning of each listed line
* convert JSR PRTCH in TST PRTCH to not out the char 
        ORG     $0310 
        FCB     $7D
*
* remove ^R ($14) PTP OFF at beginning of each listed line
* convert JMP PRTCH in RTS to just return 
        ORG     $033A
        FCB     $39
*
* the following path is needed in order to read from PTR only 
* when TAPPEND or TLOAD commands are used
* remove ^Q ($11) PTR ON issued ':' prompt
* convert JSR PRTCH in TST PRTCH to not out the char 
        ORG     $0206
        FCB     $7D
*
* the following is needed to accept basic programs without requiring each
* line to start by char $02 just before line number digits
        ORG     $0B8E
        FCB     $03
        END
