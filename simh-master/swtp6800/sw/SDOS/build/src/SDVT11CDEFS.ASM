                   page Virtual Terminal Driver definitions
*     SDVT11CDEFS.ASM -- definitions file for VT driver version 1.1C
*
*     Environment Definition
*
*       Context block layouts differ for 6800 and 6809

                   ifund m6800
m6800              equ   1
m6801              equ   0
m6809              equ   0
                   fin   m6800

::                 set   *
                   if    m6809
                   org   0

env:cc             rmb   1             condition code
env:a              rmb   1             a register
env:b              rmb   1             b register
env:dp             rmb   1             direct page register
env:x              rmb   2             x register
env:y              rmb   2             y register
env:u              rmb   2             u register (user stack pointer)
env:p              rmb   2             program counter
env:size           equ   *
                   else
                   org   1

env:cc             rmb   1             condition code
env:b              rmb   1             b register
env:a              rmb   1             a register
env:x              rmb   2             x register
env:p              rmb   2             program counter
env:size           equ   *             (which includes the trash byte on top)
                   fin

env:minstack       equ   (env:size-env:cc)*3   environment, IRQ, NMI

                   org   ::
                   page
*     SDOS Entry Points
*
*       Used as portals in the "black box" called the VT Driver

sdos:extensionsize equ   78      size of extension to SDOS:xxx entries

::                 set   *
                   org   -sdos:extensionsize

*                  Internal ring buffer management

sdos:vttlputbuf    rmb   3             task level (A) to output buffer
sdos:vtilputbuf    rmb   3             interrupt level (A) to input buffer

*                  Assorted entry points for the outside world

sdos:vtedittask    rmb   3             Newly hatched TCB points (P) here
sdos:vtnewpageff   rmb   3             DPB:NEWPAGE jmps here to output ASCII:FF
sdos:vtnewlinecrlf rmb   3             DPB:NEWLINE jmps here to output <CR><LF>
sdos:vtsimeeol     rmb   3             DPB:ERASETOEOL jmps here if device can't EEOL
sdos:vtmalnewpage  rmb   3             DPB:NEWPAGE jmps here to output user-defined clearscreen
sdos:vtmalnewline  rmb   3             DPB:NEWLINE jmps here to output user-defined <CR><LF>, idles
sdos:vtmaleeol     rmb   3             DPB:ERASETOEOL jmps here to output user-defined EEOL sequence
sdos:vtmalposition rmb   3             DPB:POSITION jmps here to output user-defined cursor control sequence
sdos:vtattncheck   rmb   3             attention check requests come here
sdos:vtinputto     rmb   3             time out on input
sdos:vtoutputto    rmb   3             time out on output
sdos:vtinputint    rmb   3             input interrupts come here
sdos:vtoutputint   rmb   3             output interrupts come here
sdos:vttlstore     rmb   3             called by special output routines
sdos:vtdispatch    rmb   driver:pfrestart+2 VT driver dispatch vector

*                  Information needed by the outside world

sdos:vtintdcb      rmb   2             dcb pointer for interrupt level

                   org   ::
                   page
*     Profile Control Block
*
*       One instance for each type of device
*       Copied to DCB by CC:SETPROFILE control call

::                 set   *
                   org   0

dpb:profilenumber  rmb   1             holds unique profile number
dpb:profilename    rmb   2             pointer to profile name
dpb:dvtyp          rmb   1             device type
dpb:next           rmb   2             next dpb or 0
dpb:defwidth       rmb   1             default display width
dpb:defdepth       rmb   1             default display depth
dpb:flags          rmb   1             flags:
dpbflags:mal       equ   %00000001        malleable
dpbflags:output    equ   %00000010        output only
dpbflags:wrap      equ   %00000100        wrap at end and begin of line
dpbflags:autonl    equ   %00001000        device has auto-nl feature
dpbflags:hcedit    equ   %00010000        hardcopy editing, only
dpb:outto          rmb   2             default timeout constant for terminal
dpb:xlatei         rmb   3             routine to perform input translation
dpb:setcoloring    rmb   3             call to set output color
dpb:setbackground  rmb   3             call to set background color
dpb:newpage        rmb   3             call to clear screen/go to top of form
dpb:newline        rmb   3             call to go to new line
dpb:erasetoeol     rmb   3             call to erase to end of line
dpb:position       rmb   3             call to position cursor on CRT
dpb:gpinit         rmb   8             for use of special function handlers

dpb:size           equ   *
                   org   ::
                   page
*     Device Control Block
*
*       One instance for each device
*
*       NOTE: Information is placed in this DCB in an order that minimizes
*             the space occupied by indexed '09 offsets, (or sometimes, that
*             minimizes the time used by a frequently executed routine).
*             This ordering is based on an empirical analysis of static
*             references to DCB: offsets, and to a dynamic analysis of
*             output interrupt and task wake-up condition routines.
*             Altering the order of these definitions may cause the driver
*             to grow significantly, or to impair its performance.
::                 set   *
                   org   dcb:size
*                  F l a g s
*                  These flags have the largest static usage of any variables
*                  in the VT driver, and are placed here to ensure 4 bit offset
*
dcb:editflags      rmb   1            flags used by edit task
dcbeditflags:esc   equ   %00000001       s-> activation due to esc
dcbeditflags:readb equ   %00000010       s-> input is binary; r-> input is ascii
dcbeditflags:wrap  equ   %00000100       s-> wrap at end and begin of line
dcbeditflags:page  equ   %00001000       s-> page mode
dcbeditflags:activate equ %00010000      s-> activation in line buffer
dcbeditflags:into  equ   %00100000       s-> activation due to timeout
dcbeditflags:hcedit equ  %01000000       s-> hardcopy editing only
dcbeditflags:killp equ   %10000000       s-> port is killproof (no ^C, ^D)
dcb:reminders      rmb   1            flags for posting reminders
dcbreminders:ctls  equ   %00000010       ^S posted
dcbreminders:ctlp  equ   %00000100       ^P posted
dcbreminders:ctlo  equ   %00001000       ^O posted
dcbreminders:rip   equ   %00010000       read in progress
dcbreminders:into  equ   %00100000       input timeout occurred
dcb:welflags       rmb   1            flags to support write edit line, fields
dcbwelflags:pref   equ   %00000001       s-> line buffer pre-filled
dcbwelflags:retype equ   %00000010       s-> don't retype line buffer
dcbwelflags:fldw   equ   %00000100       s-> field width is valid
dcbwelflags:flde   equ   %00001000       s-> field end is valid
dcbwelflags:echo   equ   %00100000       s-> no echo; r-> echo
                   page
*                  The following variables have 4 bit index offset values
dcb:ilsw           rmb   1            Interrupt level switches
dcbilsw:alphalock  equ   %00000001       force lowercase to upper (^A)
dcbilsw:ctlc       equ   %00000010       abort (^C) received
dcbilsw:outto      equ   %00000100       output timeout has occurred
dcbilsw:hcfreeze   equ   %10000000       freeze output at interrupt level
;                                        Note: must be MSB for efficient test
;                                        in interrupt output routine

dcb:colct          rmb   1            Editor task: logical column counter
;                                     advances even if physical line wraps
;                                     does NOT advance if echo is disabled,
;                                     or end-of-field reached
dcb:ctlccount      rmb   1            Editor task: number of ^C's to display
dcb:linebufptr     rmb   2            Editor task: -> next byte in line buffer
*                                     Note: dcb:linebufptr has offset $F
                   page
*                  Information used at the input interrupt level

dcb:iilspl         rmb   1            special keys of interest to tl
dcbiilspl:page     equ   %00000001       page mode (^P)
dcbiilspl:freeze   equ   %00000010       freeze output (^S)
dcbiilspl:discard  equ   %00000100       discard output (^O)
dcbiilspl:continue equ   %00001000       continue suspended output (^Q)
dcbiilspl:into     equ   %00100000       input timeout occurred
dcb:iillflgs       rmb   1            input interrupt level line flags
dcbiillflgs:ctlg   equ   %00001000       ^G: continue from BASIC breakpoint
dcbiillflgs:ctlv   equ   %00010000       ^V: single step BASIC program
dcbiillflgs:ctlb   equ   %00100000       ^B: set BASIC breakpoint
dcbiillflgs:ctlt   equ   %01000000       ^T: toggle BASIC program trace
dcbiillflgs:esc    equ   %10000000       ESC

*                  Information used at the output interrupt level

dcb:oilquiescent   rmb   1            0 if expecting interrupt

*                  Information provided by the caller
dcb:scb            rmb   scblk:end+4  syscall block + extension
dcb:callerscb      rmb   2            caller's syscall block address
                   page
*                  Information used to manage standard ring buffers
*                  These definitions are here due to both high static
*                  frequency of reference, and high dynamic references
*                  in output routines (typically for 9600 to 38.4kb CRTs)
*
dcb:ringinfetch    rmb   2            ring buffer; input fetch pointer
dcb:ringindata     rmb   2            ring buffer; available data until end
dcb:ringinstore    rmb   2            ring buffer; input store pointer
dcb:ringinroom     rmb   2            ring buffer; available space until end
dcb:ringinbase     rmb   2            ring buffer; base of input ring pointer
dcb:ringinlen      rmb   2            ring buffer; length of input ring buffer
dcb:ringoutfetch   rmb   2            ring buffer; output fetch pointer
dcb:ringoutdata    rmb   2            ring buffer; available data until end
dcb:ringoutstore   rmb   2            ring buffer; output store pointer
dcb:ringoutroom    rmb   2            ring buffer; available space until end
dcb:ringoutbase    rmb   2            ring buffer; base of output ring buffer
dcb:ringoutlen     rmb   2            ring buffer; length of output ring buffer
dcb:ringoutthreshold rmb 1            ring buffer; # free bytes to wake task

*                  The following items are used very heavily in task
*                  scheduling and/or character output routines

dcb:beepcount      rmb   1            number of beeps to output
dcb:lineflags      rmb   1            logical sum of dcb:iillflgs
dcb:tldata         rmb   2            count of input data avail for task level
dcb:ildata         rmb   2            count of output data avail for int. level
;                                     note: -1 --> none
dcb:tlroom         rmb   2            amount of room available for task level
                   page
*                  The following variables are listed according to their
*                  number of static references in the VT driver

dcb:linebufcount   rmb   1            number of bytes in line buffer
dcb:linebuf        rmb   2            base address of line buffer
dcb:displaywidth   rmb   1            display width, if malleable
dcb:displaydepth   rmb   1            display depth, if malleable
dcb:endcol         rmb   1            last display column of input
dcb:cursorlost     rmb   1            0 -> cursor not lost
dcb:row            rmb   1            virtual row
dcb:col            rmb   1            virtual column
dcb:readcol        rmb   1            virtual column at time of read
dcb:welpos         rmb   1            cursor position after retype
dcb:rowct          rmb   1            lines to output before page break
dcb:readaerr       rmb   2            cc:ckactivation stores errors here for reada
dcb:fieldend       rmb   1            last column of field
dcb:ctlckill       rmb   1            user has been killed
dcb:coloring       rmb   2            use to color output

****************** Beyond this point, the number of static/dynamic references
****************** to variables is low enough so order is not important
                   page
*                  Information used at the task level

dcb:background     rmb   1            use to color background
dcb:newstatus      rmb   1            <> 0 -> status changed since last exam
dcb:linebuflen     rmb   1            max length of line buffer
dcb:actcol         rmb   1            colct of activation character
dcb:actdisp        rmb   1            byte displacement of activation character
dcb:ilroom         rmb   2            input room available for interrupt level
dcb:except         rmb   1            exception flags
dcbexcept:sedit    equ   %00000001       s-> special activation for SEDIT

*                  More information provided by the caller

dcb:edittaskfn     rmb   2            points to code for edit task to do
dcb:processid      rmb   2            process id of owner (tcb address)
dcb:opencount      rmb   1            number of times open to this process
dcb:calleriocb     rmb   2            caller's iocb address
dcb:fieldwidth     rmb   1            current field width; 0 -> line mode
dcb:readperiod     rmb   2            period for timed read (or 0)
                   page
****************************************************************************
*                  The following variables must be in the same order
*                  as the Write buffer part of an AlterProfile syscall
dcb:posnsl         rmb   1            significant length of position sequence
dcb:posn           rmb   4               position sequence, if malleable
dcb:posnidles      rmb   1               idles to follow, if malleable
dcb:rowdisp        rmb   1            displacement of row in position sequence
dcb:coldisp        rmb   1            displacement of col in position sequence
dcb:clearsl        rmb   1            significant length of clear sequence
dcb:clear          rmb   4               clear sequence, if malleable
dcb:clearidles     rmb   1               idles to follow, if malleable
dcb:eeolsl         rmb   1            significant length of eeol sequence
dcb:eeol           rmb   4               eeol sequence, if malleable
dcb:eeolidles      rmb   1               idles to follow, if malleable
****************** End of Alterprofile block *******************************

dcb:idletrigger    rmb   1            character to trigger idles (CR or LF)
dcb:idlecount      rmb   1            number of idles to follow idletrigger
dcb:baudrate       rmb   2            holds device baud rate (bits/second)
dcb:lastlinebufcount rmb 1            holds size of last-entered line
                   page
*                  Timeout blocks

dcb:outputtoblk    rmb   timeout:size timeout block for output
dcb:inputtoblk     rmb   timeout:size timeout block for timed input

*                  Routines to service hardware device represented by DCB

dcb:reset          rmb   3            call to initialize device
dcb:tlclosedev     rmb   3            call to terminate device usage
dcb:ilputdev       rmb   4            jmp to output byte in (A), exit via RTI
dcb:ilgetdev       rmb   3            call to input byte
dcb:control        rmb   3            call for unrecognized control calls
dcb:status         rmb   3            call for unrecognized status calls
dcb:isdeviceready  rmb   3            call to see if device is ready
dcb:ilenboi        rmb   3            call to enable empty buffer output int
                                      ; returns Z reset if device ready now
dcb:ildisoi        rmb   3            call to disable empty buffer output int
dcb:iloto          rmb   3            call to handle output timeout on device
dcb:ilsetbaudrate  rmb   3            call to set baud rate to (D) or errorrts

dcb:tlputbuf       rmb   3            call to buffer output

dcb:profile        rmb   dpb:size     device profile block
dcb:gpuse          equ   dcb:profile+dpb:gpinit
dcb:xlatestate     equ   dcb:gpuse    this is commonly used

*                  Large data blocks, infrequently referenced

dcb:tabs           rmb   17           16 tab stops (max); 0 marks end of table
dcb:activation     rmb   16           activation set (128 bits)

dcb:vtsize         equ   *            dcb can be extended from here
                   org   ::
                   page
*                  Extensions to the Configuration Table

::                 set   *
                   org   cnfg:timeoutlist+2

cnfg:vtprofiles    rmb   2            head of profile chain
cnfg:vtdebug       rmb   2            interrupt level ep to debugger
cnfg:mtprims       rmb   2            -> MT primitives vector

cnfg:vtsize        equ   *
                   org   ::

*                  Control calls

                                   org   cc:devicespecificop+$20
cc:writeanowait                    rmb   1      write ascii, do not block
cc:settimeshare                    rmb   1      set the timeshare flag
cc:setexception                    rmb   1      set/clear exception flags
cc:writebnowait                    rmb   1      write binary, do not block
cc:stoptimeshare                   rmb   1      disable timesharing

*                  Status calls

                                   org   sc:devicespecificop+$20
sc:attentionck                     rmb   1      check for attention  (s/u)
sc:statusck                        rmb   1      check for change of status (s/u)
sc:gettimeshare                    rmb   1      check for SDOS/MT running (MT)
sc:allstatus                       rmb   1      check for change of status on any port (MT)
                   ifund   sc:getlineflagshint ; kluge around SDOS11DEFS
sc:getlineflagshint                rmb   1      returns lineflags w/o clearing
                   else
                                   rmb   1      returns lineflags w/o clearing
                   fin
sc:getfreecount                    rmb   1      returns dcb:tlroom
sc:getdatacount                    rmb   1      returns dcb:tldata

lineflags                          equ   sysdependent
                   page
                   org   0

alterprofile:cplen                 rmb   1      length of cursor posn sequence
alterprofile:cpseq                 rmb   4      cursor posn sequence
alterprofile:cpidles               rmb   1      number of idles to follow c.p.s.
alterprofile:rowdisp               rmb   1      disp. to row in c.p.s.
alterprofile:coldisp               rmb   1      disp. to column in c.p.s.
alterprofile:cllen                 rmb   1      length of clear sequence
alterprofile:clseq                 rmb   4      clear sequence
alterprofile:clidles               rmb   1      number of idles to follow c.s.
alterprofile:eeollen               rmb   1      length of eeol sequence
alterprofile:eeolseq               rmb   4      eeol sequence
alterprofile:eeolidles             rmb   1      number of idles to follow e.s.
alterprofile:size                  equ   *

*                  Coloring definitions

*                  Attribute byte
vtcoloring:intensitymask equ          %11000000 2 bits of intensity
vtcoloring:intensitynormal equ        %00000000 "normal intensity" code
vtcoloring:intensitylow  equ          %01000000 "low intensity" code
vtcoloring:intensityhigh equ          %10000000 "high intensity" code
                   equ                %11000000 "undefined intensity" code

vtcoloring:blink         equ          %00100000 1 --> "blink enabled"
vtcoloring:underscore    equ          %00010000 1 --> "underscore characters"
vtcoloring:reversevideo  equ          %00001000 1 --> "reverse video"

vtcoloring:colormask     equ          %00000111 contains inverted color info
vtcoloring:notred        equ          %00000100 0 --> "red" gun enabled
vtcoloring:notgreen      equ          %00000010 0 --> "green" gun enabled
vtcoloring:notblue       equ          %00000001 0 --> "blue" gun enabled

*                  Character set selection byte
vtcoloring:charactersetmask equ       %11100000 selects which character set
vtcoloring:charactersetnormal equ     %00000000 selects "standard" set

                   org   ::
                   page
*                        Errors specific to SDOS/MT

::                 set   *
                   org   1230

err:sdosmtalreadyrunning           rmb   1      SDOS/MT is already running
err:statushaschanged               rmb   1      port status has changed since last examination
err:sdosmtprimsmissing             rmb   1      SDOS/MT primitives not defined in config table

                   org   ::
