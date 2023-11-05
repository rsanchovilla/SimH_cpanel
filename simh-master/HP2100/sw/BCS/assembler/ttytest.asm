ASMB,R,L,B
      NAM SAMPL
      ENT START

* Wastefully exercise a 12531 TTY interface
* Read characters one at a time,
* dump the value to the status lights,
* the print them
*
* B keeps a counter
*
* by Tim Riker <Tim@Rikers.org>
*
* set TTY to I/O number
TTY   EQU 11B

*     ORG 100B     Uncomment to generate ansolute code
START NOP
      CLB          Clear B
      CLA          Clear A
LOOP  CLC 0        Turn off all devices
      STB COUNT    Save counter
      LDB TTYI     Get tty input command
      OTB TTY      Send Command code
      LDB COUNT    Restore counter
      STC TTY,C    Set I/O control bit
WAITR SFS TTY      Skip if control bit is set
      JMP WAITR    Loop till ready
      LIA TTY      Read one char
      OTA 1        Write to S
      STB COUNT    Save counter
      LDB TTYO     Get tty output command
      OTB TTY      Send Command code
      LDB COUNT    Restore counter
      INB          Increment counter
      OTA TTY      Write character back
      STC TTY,C    Set I/O control bit
WAITW SFS TTY      Skip if control bit is set
      JMP WAITW    Wait till we write it
      JMP LOOP     Next char
TTYT  OCT 110000   TTY punch (tape)
TTYO  OCT 120000   TTY print (output)
TTYTO OCT 130000   TTY print and punch
TTYI  OCT 140000   TTY input
TTYIT OCT 150000   TTY input and punch
TTYIO OCT 160000   TTY input and print
TTYA  OCT 170000   TTY input, print and punch
COUNT OCT 0
      END 
