                   title Software Dynamics Virtual Terminal Driver
                   page  Things To Do
                   psr   0
                   pca   0
                   ifund m6800
m6800              equ   1
m6801              equ   0
m6809              equ   0
                   fin   m6800
                   if    m6809
                   with  wi=105,de=51
                   else
                   with  wi=105,de=51
                   fin   m6809

version            equ   $11     VT Driver version 1.1
revision           equ   'c      change this for each assembly!

; to assemble, refer to SDxxVT.do on the Components pack

; Things left to do:
;     1) Write EditLine does not work with echo disabled
;     2) dcb:colct=255 gives trouble if CC:NOECHO is set and an input request
;        occurs.  Perhaps dcb:colct should be incremented ?? (WHAT?)
;     3) If VT driver outputs a free <CR>, it should surpress the next
;        character if it is a <CR>.
;     4) If user task did I/O instead of waking editor task, significant
;        savings due to lack of scheduling (~40%! for single char output)
;        could be attained.
;        Clever interlocks would be needed.  Another way to help would be
;        if user task could signal to scheduler the specific editor task
;        to wake, 6-10% could be saved.
;     4a) Use fast task scheduler to minimize edit task wakeup overhead.
;     5) Should send XON/XOFF to devices that attempt to overfill the input
;        ring buffer.  Perhaps need a DPB entry point that default
;        sends XON/XOFF; custom code could do anything appropriate.
;        This logic might only be necessary for non-terminal input devices ?
;     6) allow overstrike editing
;     7) ^C, ^P don't display if CC:NOECHO is set and cursor is moved.
;        VT:LOGCHR supresses output due to WELFLAGS.  This should be fixed.
;     8) Echo ^A<bs><bs> when toggle to upper case, ^a<bs><bs> when lower
;     9) Use Z page register to ref DCB instead of index register on '09
;        Would make code considerably smaller, but probably not much faster.
;
;     Perhaps....
; add to DEFS file a conditional switch for each of the SDOS components:
;    file driver
;    MT
;    VT Driver
;    SDNET
                   page  Revision History
;    Version 1.0i 4/28/82
;         Revised SDOSIOVTDEFS.ASM so that DCB:xxx definitions take advantage
;         of 6809 offsets in a manner to optimize space and time.
;         No real changes to code function, but several speedups in trivial
;         ways, like recoding task wake-up conditions to minimize time.
;         Dcb:ringxxx variables that used to be one byte long are now
;         two bytes long so they can be used even for customized ring
;         buffer routines.
;         Added dcb:ringoutthreshold, which acts as hysteresis to prevent
;         output bound edit task from waking up every time the output buffer
;         has just one available slot.  Note that ...threshold is only 1 byte;
;         this is because it is compared in a wakeup routine where time is
;         precious.
;
;    Version 1.0j 5/3/82
;         Trivial fixes (branch out of range, etc.) to make 6800 version.
;         Changed ORG of VTDRIVER to be slightly higher because it has
;         shrunk somewhat.
          page
;    Version 1.1a 11/10/82
;         Revision number change due to change in VT driver spec.
;         a) VT:ILINPUT, VT:ILOUTPUT gain control BEFORE stacks switched.
;            (this speeds up output path by ~10% at 9600 baud on 1Mhz CPU)
;         b) a new entry in the SDOS/MT primitives, ABORTTIMESLICE, is called
;            by VT driver when new status (READ-/WRITE- completion) is ready.
;            ABORTTIMESLICE must be 1st entry in SDOS/MT primitive table.
;         c) Added CC:WRITEBNOWAIT, similar to CC:WRITEANOWAIT for use by /MT
;         d) Changed SDOS:VTCLEARIN so it must be called from interrupt level
;            with (X)=DCB pointer.
;         Other improvements (no change to specification):
;         Fixed so beeps if error status returned by input device
;         Activation character causes rescheduling, other input chars don't.
;         If line buffer has only one byte left and non-activation char is
;         entered, then BEEP.
;         Fixed so acknowledging ^C^C (error 1041) makes line buffer empty.
;         Fixed so ^P, ^Q work properly on HCEDIT devices; ^P complains if
;            received by output only device.
;         Fixed so DCB:LINEFLAGS not handled as side effect to task wake-up
;            routine; freebie result: wakeup routines (esp input wait) are
;            much faster, improving system responsiveness
;         Fixed "^P entered instantly after input terminated places keyboard
;            back in input state" (usually followed soon by crash).
;         Fixed "ascii:ff to device with depth=0 outputs ^E"
;         Fixed so ^P to hardcopy devices stops at BOTTOM of page,
;               instead of top.
          page
;    Version 1.1b 12/26/83
;         Fixed error propogation doesn't clear extra stuff off stacks
;           (caused VT driver to do funny things [like crash] on timeout)
;         Speed up block output by 10% at 9600, 20% at 19200 baud
;         Added CC:SETTIMEOUT to override default timeout from DPB
;         When closing output-only devices, issues <CR> is partial line,
;           issues <FF> if partial page.  This sends LPTs to new page.
;         Require VT100 DPB --> no code in VT driver reqd to support VT100.
;         CREATE to non-ready device gives error; OPEN does not. Allows
;           device to be opened and queried as to type and parameters.
;         Fixed "output while no echo bumps column count".
;         Defined VT:COLORING bits.
;         Added CC:STOPTIMESHARE to allow /MT to shut down someday.
;         Modified SYSCALL:WRITEB to zero the column count.
;         Added ^F editing character: go to left end of field.
;         Modified ^R to go to right end of field if not hardcopy device.
;         <ESC> in input line now moves cursor to end of field.
;         Malleable device simulates Form by multiple ASCII:LF if clear
;           screen sequence is 0 bytes long.
;         Fixed "^C^X at column 255 in input mode" crashes VT driver.
;         Allow ^O, ^P, ^S to operate correctly during long WRITEAs.
;         Allow SET to read back device descriptors: SC:GETOUTPUTTIMEOUT,
;           and SC:GETPROFILEALTERATION
;         Defined CC:SETINPUTTRANSLATION, SC:GETINPUTTRANSLATION. No code
;           required in VT driver to accomplish this.
;         Fixed "ASCII:BS in column zero backspaces! Should not."
;         Removed VTO:... table, saving ~80 bytes.
;         Now checks for output-only device on SYSCALL:READA, SYSCALL:READB,
;            CC:WRITEEDITLINE and CC:ACTIVATIONCK
;         Fixed WriteEditLine and fields so they operate correctly
;            when echo is enabled
;         Inserted "ok..." in front of several RTS's to ensure proper
;            proper error propogation
;         Device output timeout routine now calls DCB:RESET --> mod MAKEVTCNFG
;         Discovered bug in VT driver spec: DPB:SETCOLORING and
;            DPB:SETBACKGROUND must exit with carry reset --> mod IOVTDPBS.ASM
;         DPB:SETBACKGROUND now called after each FORM character issued
                    page
;    Version 1.1c 2/14/84
;         Implement ^W (re-edit last input line)
;         Speed up interrupt output routines --> add DCB:ILENBOI, DCB:ILDISOI
;         Changed meaning of RING:xxx to speed output path. see VT:TLPUTBUF
;         Revised ring buffering routines to handle 2 byte counts everywhere
;                 thus making user-supplied code for ring mgmt unnecessary
;         Deleted DCB:CLEARIN, DCB:CLEAROUT,
;                 DCB:TLGETBUF, DCB:ILPUTBUF, DCB:ILGETBUF slots
;                 (DCB:TLPUTBUF retained for memory-mapped devices)
;         Deleted SDOS:VTCLEARIN,SDOS:VTCLEAROUT,
;                 SDOS:VTTLGETBUF,SDOS:VTILGETBUF entry points, and
;                 replaced references to same with refs to VT:... instead
;                 (SDOS:VTTLPUTBUF retained for non-memory-mapped devices,
;                  SDOS:VTILPUTBUF retained for many-to-many input xlations)
;         Added DCB:ILOTO to handle timeout on output device
;         Remove DCB:TLBUFFER from DCB and replace by SDOS:VTTLSTORE entry pt
;         Added CC:SETBAUDRATE; new device routine now req'd to set baudrate
;               added DCB:BAUDRATE slot to remember value (affects MAKEVTCFG)
;         Added SC:GETBAUDRATE to retreive last-selected baud rate
;         Added SC:GETTABS, SC:GETIDLES, SC:GETWRAP,
;               SC:GETCOLORING,SC:GETBACKGROUND to return values set by CC:xxx
;         Added SC:GETPROFILENAME to return text string corresponding to
;               given profile number
;         Changed DPB:PROFILE to DPB:PROFILENUMBER
;         Added DPB:PROFILENAME to point to a profile text name
;         Fixed "if number lines printed > dcb:depth, simulated forms
;               get confused"
;         Changed SDVT11DEFS.ASM to match DCB mods.
;         Changed MAKEVTCNFG to match new DCB structure
;         Changed SET (SET11C) to allow full use of SC:... calls
;         Modified IOVTDPBS.ASM to use SDOS:VTTLSTORE
;         Deleted DCB:TCB and DCB:TASKSTACK because no one used them
;         Moved VT driver revision identification to front of VT:RESET
;         DCB:ILPUTDEV is now 4 bytes long to allow STAA> addr\RTI in DCB
;         Fixed conflict between "not implemented" and "timeouterror"...
;               returns from DPB:SPECIALFN by replacing DPB:SPECIALFN with
;               DPB:POSITION, DPB:NEWPAGE, DPB:ERASETOEOL and DPB:NEWLINE.
;         Deleted SDOS:VTMALLPT,SDOS:VTMALVT
;         Added SDOS:VTMALPOSITION, SDOS:VTMALNEWPAGE, SDOS:VTMALEEOL,
;           SDOS:MALNEWLINE, SDOS:VTNEWPAGEFF, SDOS:NEWLINECRLF,SDOS:VTSIMEEOL
                    page
; For consideration in Version 1.2a (SDVT12A.ASM)
;         Allow renaming of devices (feature of SDOS1.2)
;         integrate SDOS proper/Vt driver entry points into one table
;         Allow WriteEditLine to operate with no echo and SEDIT
;         Modify VT:RESTORECURSOR to just position cursor if we know
;                where it is; would be great help with SEDIT.
     page  Definitions
                   tabs  11,19
                   list  0
iopkdefs           equ   1
                   include sdos11defs.asm
                   list  1
                   include sdvt11cdefs.asm
                   page Configuration parameters
                   ifund sdos
                   if    m6809
sdos               equ   $b200
                   else
sdos               equ   $ae00
                   fin   m6809
                   fin   sdos

                   if    m6809
vt:driverestimatedsize equ $1A00
                   else
vt:driverestimatedsize equ $1980
                   fin   m6809

                   ifund driver
driver             equ   sdos-sdos:extensionsize-vt:driverestimatedsize
                   fin   driver

                   page VT Driver Entry Points
*     SDOS Entry Points
*
*       Used as portals in the "black box" called the VT Driver

          org   sdos-sdos:extensionsize

*         Internal ring buffer management

          jmp     vt:tlputbuf   task level (A) to output buffer, ref'd by DCB
          jmp     vt:ilputbuf   interrupt level (A) to input buffer
                                ; this entry ref'd only by input translation

*         Assorted entry points for the outside world

          jmp     vt:edittask   Newly hatched TCB points (P) here
          jmp     vt:newpageff  Output ASCII:FF for new page
          jmp     vt:newlinecrlf Output ASCII:CR, ASCII:LF for new line
          jmp     vt:simeeol    Simulate Erase-to-eol with spaces, backspaces
          jmp     vt:malnewpage Output user-definable new-page control sequence
          jmp     vt:malnewline Output user-defined CR,LF sequence plus idles
          jmp     vt:maleeol    Output user-definable erase-to-end-of-line
          jmp     vt:malposition Output user-definable cursor control sequence
          jmp     vt:attncheck  check for attention
          jmp     vt:inputto    entry point for time-out on input
          jmp     vt:outputto   entry point for time-out on output
          jmp     vt:ilinput    receiver of input interrupts
          jmp     vt:iloutput   receiver of output interrupts
          jmp     vt:tlstore    entry point for DPB special output routines
          page
;   this vector is referenced by each dcb, and is used by SDOS to make read,
;   write, control, and status requests of the VT driver

          fdb     vt:dopen         open a device
          fdb     vt:dclose        close a device
          fdb     vt:dreada        read ascii
          fdb     vt:dwritea       write ascii
          fdb     vt:dreadb        read binary
          fdb     vt:dwriteb       write binary
          fdb     vt:dcreate       create is almost the same as open
          fdb     vt:illdevop           rename is senseless
          fdb     vt:illdevop           delete is senseless
          fdb     vt:dcontrol      perform a control operation
          fdb     vt:dstatus       return device status
          fdb     vt:dreset        once-only at system startup
          fdb     vt:illdevop           power-fail restart--dream on!

*         Information needed by the outside world

;sdos:vtintdcb ; interrupt routines store DCB address here
          rmb     2             dcb pointer for interrupt level

          if      *>>sdos
          ?error  VT Driver dispatch table overlaps SDOS
          fin     *>>sdos
          page    Internal Data Area
          org     driver

*         Internal data area

vt:sdosmtflag
          fcb     1             SDOS/MT is running if 0

vt:sdosmtstatuschange ; points to location that tells SDOS/MT that status has changed
          fdb     0

vt:timeoutblocks
          fcb     0             number of timeout blocks in system

vt:allstatuscount
          fcb     0             count of DCBs with changed status
          page    VT User Level Procedure
vt:validateprocessid ; make sure no other process has this device open
          ldx     dcbpointer
          ldd     dcb:processid,x
          beqd    vt:setprocessid
          subd    sdos+sdos:currentask
          beqd    vt:processidmatches
          ldx     #err:busyforanotherprocess
vt:errorinx ; called with the error number in (x)
          jsr     sdos+sdos:errorsave
          jmp     sdos+sdos:errored

vt:setprocessid
          ldd     sdos+sdos:currentask
          std     dcb:processid,x
vt:processidmatches
          okrts
          page
vt:dcreate

;         validate process id (tcb address)
;         test if device is ready (to prevent opening un-ready LPT: devices)
;         otherwise identical to vt:open

          bsr     vt:validateprocessid
          jsr     dcb:isdeviceready,x
          bcc     vt:dopen3
          ldab    dcb:profile+dpb:flags,x
          bitb    #dpbflags:output
          bne     vt:dcreate1
          jsr     sdos+sdos:error
          #err:devicenotready

vt:dcreate1
          jsr     sdos+sdos:error
          #err:printernotready

vt:dopen

;   validate process id (tcb address)

;   clear iocb:eofflag
;   set tabs to 8,16,...,128
;   reset activation to default
;   reset field definitions
;   set coloring to white
;   set background to black

;   may be desirable to sync setting of tabs with edit task

          bsr     vt:validateprocessid
          page
vt:dopen3
          inc     dcb:opencount,x
;vt:openreset ; entry point for CC:RESET to reset device state identical to OPEN
          ldx     sdos+sdos:iocbpointer
          clr     iocb:eofflag,x
          ldx     dcbpointer
          ldaa    #16                           16 tab stops
          ldab    #8
vt:dopen2
          stb     dcb:tabs,x
          inx
          addb    #8
          deca
          bne     vt:dopen2
          clr     dcb:tabs,x                    null at end of tab table
          inx
          ldaa    #16                           = size of activation table
vt:dopen1
          clr     dcb:tabs,x
          inx
          deca
          bne     vt:dopen1
          ldx     dcbpointer
          clr     dcb:fieldwidth,x              "no field is active now"
          clra
          clrb
          std     dcb:coloring,x
          jsr     dcb:profile+dpb:setcoloring,x
          ldaa    #$07
          staa    dcb:background,x
          jsr     dcb:profile+dpb:setbackground,x
          clr     dcb:except,x
          jsr     vt:killenable
          jmp     vt:echo
          page
vt:dclose

;   decrement the open count;
;   when the count goes to zero, move to top-of-page for printers,
;   then issue a dumpbuffers request and clear the dcb:processid field,
;   allowing another process to use the device

          ldx     dcbpointer
          ldaa    dcb:opencount,x
          deca
          bne     vt:dclose1
          ldd     #vt:closecleanup
          jsr     vt:postedittask
          bcc     vt:dclose2
          jsr     sdos+sdos:errorsave
          ldx     dcbpointer
          bsr     vt:dclose2                     go do common cleanup
          jmp     sdos+sdos:errored

vt:dclose2
          clra
          clrb
          std     dcb:processid,x
vt:dclose1
          dec     dcb:opencount,x
          okrts
          page
vt:dreada
          ldx     dcbpointer              see if any bad news left around
          ldd     dcb:readaerr,x          complain about it
          beqd    vt:dreada.2             b/ definitely not
vt:dreada.1 ; re-entry point for vt:dgetlineflagshint
          clr     dcb:readaerr,x          (and then forget about it)
          clr     dcb:readaerr+1,x
          tdx
          jmp     vt:errorinx

vt:dreada.2
          jsr     sdos+sdos:checkrdlen
          #0
          ldx     sdos+sdos:ioblockptr
          lda     scblk:params+1,x        reada, non-line mode, with rdlen > 0
          bne     vt:dreada1              is not permitted (ill-defined)
          ldd     scblk:rdlen,x
          beqd    vt:dreada1
          jmp     vt:illdevop

vt:dreada1
          ldx     sdos+sdos:iocbpointer
          lda     iocb:eofflag,x
          bne     vt:dreadaateof
          ldd     #vt:reada
          bra     vt:postedittask

vt:dreadaateof
          jsr     sdos+sdos:error
          #err:eofhit
          page
vt:dreadb
          jsr     sdos+sdos:checkrdlen
          #0
          ldd     #vt:readb
          bra     vt:postedittask
          page
vt:dwriteb
*         jsr     sdos+sdos:checkwrlen ; already checked by SDOS
*         #0
          ldd     #vt:writeb
          bra     vt:postedittask

vt:dwritea
*         jsr     sdos+sdos:checkwrlen ; already checked by SDOS
*         #0
          ldd     #vt:writea
*         bra     vt:postedittask
          page
vt:postedittask

;   called with (D) = address of function that edit task should execute

;   Copy the pertinent parts of the scb to the dcb; as the last step, clear
;   dcb:doneflag, which unblocks the edit task.  The edit task will
;   increment this field as its last step before blocking.

;   block the user task until dcb:doneflag becomes non-zero

;   note that some of the control functions will error if the device
;   is still busy; others will return without blocking for function
;   completion

          pshd                                   save function address
          ldx     dcbpointer                     wait for done with prev op
          bsr     vt:blockusertask               assert: cannot error!
          puld
          bsr     vt:postedittask2               start this operation
          bsr     vt:blockusertask               wait for done with this op
          ldx     dcb:lasterror,x
          beq     vt:postedittask1
          jmp     vt:errorinx

vt:postedittask1
          ldx     dcbpointer
          okrts
          page
vt:postedittask2 ; enter with (D) pointing to function for edit task to do
;   assert: edit task is not doing any function now
;   prepare the dcb for the edit task and then notify the edit task of work

          pshd
          bsr     vt:copyscb                     assert: cannot error!
          puld
          std     dcb:edittaskfn,x               save edit task function address
          ldd     sdos+sdos:ioblockptr
          std     dcb:callerscb,x
          ldd     sdos+sdos:iocbpointer
          std     dcb:calleriocb,x
          clr     dcb:doneflag,x
          ldx     #vt:ilforcereschedule     cause SDOS to reschedule
          jsr     sdos+sdos:startio
          ldx     dcbpointer
          okrts
          page
vt:blockusertask

;   block the user task until dcb:doneflag becomes non-zero, indicating that
;   the device is quiescent

          lda     dcb:doneflag,x
          bne     vt:blockusertask1
          ldd     #vt:blockusertaskcond
          jsr     sdos+sdos:waitcond
          ldx     dcbpointer
vt:blockusertask1
          rts                               assert: this routine cannot error!

vt:blockusertaskcond

;   the user task is blocked until dcb:doneflag becomes non-zero

          ldaa    dcb:doneflag,x
          rts
          page
vt:copyscb

;   copies the user's syscall block into the dcb

          if      m6809
          leay    dcb:scb,x
          else
          txd
          addd    #dcb:scb
          std     tempx
          fin
          ldx     sdos+sdos:ioblockptr
          clra
          ldab    scblk:wlen,x
          cmpb    #scblk:end+4
          bls     vt:copyscb1
          ldab    #scblk:end+4
vt:copyscb1
          jsr     sdos+sdos:blockmove
          ldx     dcbpointer
          rts
          page
vt:dactivationck2 ; start a read, then check again for activation
;  this efficiently handles the case of single character input
;  when an input character is waiting (this happens a lot with SEDIT)
          ldd     #vt:activationck
          bsr     vt:postedittask2
vt:dactivationck

;   See if an activation character has been received.
;   If not, set the edit task going

          ldx     dcbpointer
          jsr     vt:testoutput           ensure not output-only device!
          ldaa    dcb:editflags,x
          bita    #dcbeditflags:activate!dcbeditflags:esc!dcbeditflags:into
          bne     vt:dactivationck4       b/ got something worth comment
          lda     dcb:readaerr,x          an error is also worth comment
          oraa    dcb:readaerr+1,x
          beq     vt:dactivationck1
vt:dactivationck4
          jsr     sdos+sdos:error
          #err:activationreceived

vt:dactivationck1
          lda     dcb:doneflag,x
          bne     vt:dactivationck2
          ldd     dcb:edittaskfn,x          see if he's busy with an
          subd    #vt:activationck           activationck request
          beqd    vt:dactivationck3         B/ yes
          jsr     sdos+sdos:error
          #err:ioinprogress

vt:dactivationck3
          okrts
          page
vt:dcontrol

;   perform control functions, when the device is available

;   certain control functions will return err:ioinprogress error, rather
;   than blocking the user level task

;   allow for external control

          ldx     sdos+sdos:ioblockptr
          ldab    scblk:params+1,x
          cmpb    #cc:writeanowait
          beq     vt:dwriteanowait            b/ take fast track for cc:writeanowait
          cmpb    #cc:activationck
          beq     vt:dactivationck
          cmpb    #cc:writebnowait            this entry can't be in table
          beq     vt:dwritebnowait
;         cmpb    #cc:reset                   reset request ?
;         lbeq    vt:openreset                b/ yes, go service
          ldaa    #vt:cclen                   search table to find control call
          ldx     #vt:cctable
vt:dcontrol1
          cmpb    0,x
          beq     vt:dcontrolmatch
          leax    4,x
          deca
          bne     vt:dcontrol1
          ldx     dcbpointer
          jmp     dcb:control,x               he'll do an okrts or die trying
          page
vt:dcontrolmatch
          ldd     2,x
          pshd
          ldab    1,x
          beq     vt:dcontrolshortscb         b/ block does not require wrbuf
          ldx     sdos+sdos:ioblockptr
          ldaa    scblk:wlen,x
          cmpa    #scblk:wrlen+2
          blo     vt:errscbtooshort
          clra
          sec
          sbcb    scblk:wrlen+1,x
          sbca    scblk:wrlen,x
          bcc     vt:errwrbuftoosmall
vt:dcontrolshortscb
          puld
          jmp     vt:postedittask
          page
vt:errscbtooshort
          leas    2,s
          jsr     sdos+sdos:error
          #err:syscalltooshort

vt:errwrbuftoosmall
          leas    2,s
          jsr     sdos+sdos:error
          #err:wrbuftoosmall
          page
vt:dwritebnowait

;         same as writeanowait, but for writeb

          jsr     sdos+sdos:checkrdlen
          #3
          ldd     #vt:writebnowait
          bra     vt:dwriteanowait1

vt:dwriteanowait

;   since this control call has some peculiar properties, it is treated
;   separately, here

;   CAVEAT: the address of RDBUF is remembered, even when the calling
;           program no longer occupies the address space!!!

          jsr     sdos+sdos:checkrdlen
          #3
          ldd     #vt:writeanowait               get description of desired fn
vt:dwriteanowait1
          ldx     scblk:rdbuf,x
          clr     0,x                            reset 'writea done' flag
          clr     1,x                            reset error code to 'none'
          clr     2,x
          ldx     dcbpointer
          tst     dcb:doneflag,x
          bne     vt:dwriteanowait2              b/ edit task not busy
          jsr     sdos+sdos:error
          #err:ioinprogress

vt:dwriteanowait2
          ldx     dcbpointer
          jmp     vt:postedittask2
          page
vt:cctable

;   this table is organized as
;      <CC:function>,<minimum WRLEN>,<edit task function address>

          +cc:position,+4,#vt:position
;         +cc:writeanowait,+0,#vt:writeanowait
          +cc:setactblock,+16,#vt:setactblock
          +cc:setfieldsize,+1,#vt:setfieldsize
          +cc:writeeditline,+0,#vt:writeeditline
          +cc:setreadtimeout,+2,#vt:setreadtimeout
;         +cc:activationck,+0,#vt:activationck
          +cc:echo,+0,#vt:echo
          +cc:noecho,+0,#vt:noecho
          +cc:wrap,+0,#vt:wrap
          +cc:nowrap,+0,#vt:nowrap
          +cc:idles,+1,#vt:idles
          +cc:tabs,+1,#vt:tabs
          +cc:clrinput,+0,#vt:clrinput
          +cc:clroutput,+0,#vt:clroutput
          +cc:setprofile,+1,#vt:setprofile
          +cc:alterprofile,+alterprofile:size,#vt:alterprofile
          +cc:setparams,+2,#vt:setparams
          +cc:settimeshare,+0,#vt:settimeshare
          +cc:dumpbuffers,+0,#vt:dumpbuffers
          +cc:coloring,+2,#vt:coloring
          +cc:background,+1,#vt:background
          +cc:killproof,+0,#vt:killproof
          +cc:killenable,+0,#vt:killenable
          +cc:setexception,+1,#vt:setexception
;         +cc:writebnowait,+0,#vt:writebnowait
;         +cc:reset,+0,#vt:reset
          +cc:setoutputtimeout,+2,#vt:setoutputtimeout
          +cc:stoptimeshare,+0,#vt:stoptimeshare
          +cc:setbaudrate,+2,#vt:setbaudrate
vt:cclen  equ     (*-vt:cctable)/4
          page
vt:dstatus

;   perform status functions and return immediately

;   allow for external status

;   it may be necessary to block the user task so that the status is
;   always valid and consistent

          ldx     sdos+sdos:ioblockptr
          ldaa    #vt:sclen
          ldab    scblk:params+1,x
          cmpb    #sc:getlineflagshint          this is an immediate response
          beq     vt:dgetlineflagshint          b/ go get the hint
          cmpb    #sc:allstatus                 so is this one
          beq     vt:dallstatus                 b/ go get the status
          ldx     #vt:sctable
vt:dstatus1
          cmpb    0,x
          beq     vt:dstatusmatch
          leax    4,x
          deca
          bne     vt:dstatus1
          ldx     dcbpointer
          jmp     dcb:status,x                  he'll do an okrts or die trying
          page
vt:dstatusmatch
          ldd     2,x
          pshd
          ldab    1,x
          ldx     sdos+sdos:ioblockptr
          ldaa    scblk:wlen,x
          cmpa    #scblk:rdlen+2
          lblo    vt:errscbtooshort
          clra
          std     scblk:rplen,x
          sec
          sbcb    scblk:rdlen+1,x
          sbca    scblk:rdlen,x
          bcs     vt:dstatusshortscb
          leas    2,s                           pop function address
          jsr     sdos+sdos:error
          #err:rdbuftoosmall

vt:dstatusshortscb
          puld
          jmp     vt:postedittask
          page
vt:dgetlineflagshint
          jsr     sdos+sdos:checkrdlen
          #1
          ldx     dcbpointer
          jsr     vt:statusck          ; acknowledge any status change
          bcs     *+2                  ; (bcs handles "status changed" error)
          ldx     dcbpointer           ; ('test-and-"set"' follows)
          lsr     dcb:ctlckill,x       ; see if it's croak time
          bcs     vt:dgetlineflagscroak ; b/ bye-bye
          ldd     dcb:readaerr,x       ; check for ^C^C in readaerr...
                                       ; to handle case where user running
                                       ; under SDOS/MT issued vt:activationck
                                       ; and then went compute-bound
          cmpd    #err:programkilled   ; this is just as deadly
          lbeq    vt:dreada.1          ; b/ and that's what he got...
          ldb     dcb:lineflags,x      ; not croak time: get the hint
vt:dgetlineflagsstore
          ldx     sdos+sdos:ioblockptr
          stb     [scblk:rdbuf,x]
          okrts

vt:dgetlineflagscroak
          jsr     vt:clearactivation            ensure that line buffer is empty
          jsr     sdos+sdos:error
          #err:programkilled

vt:dallstatus
          jsr     sdos+sdos:checkrdlen
          #1
          ldb     vt:allstatuscount             this says if anyone's twitched
          bra     vt:dgetlineflagsstore
          page
vt:sctable

;   this table is organized as
;      <SC:function>,<minimum RDLEN>,<edit task function address>

;         +sc:allstatus,+1,#vt:allstatus
;         +sc:getlineflagshint,+1,#vt:getlineflagshint
          +sc:getfreecount,+2,#vt:getfreecount
          +sc:getdatacount,+2,#vt:getdatacount
          +sc:statusck,+0,#vt:statusck
          +sc:getlineflags,+1,#vt:getlineflags
          +sc:attentionck,+1,#vt:attentionck
          +sc:getcol,+1,#vt:getcol
          +sc:geteof,+1,#vt:geteof
          +sc:getpos,+4,#vt:getpos
          +sc:getactcol,+2,#vt:getactcol
          +sc:gettype,+1,#vt:gettype
          +sc:getparams,+2,#vt:getparams
          +sc:getprofile,+1,#vt:getprofile
          +sc:gettimeshare,+0,#vt:gettimeshare
          +sc:getprofilealteration,+alterprofile:size,#vt:getprofilealteration
          +sc:getoutputtimeout,+2,#vt:getoutputtimeout
          +sc:getbaudrate,+2,#vt:getbaudrate
          +sc:gettabs,+16,#vt:gettabs
          +sc:getidles,+2,#vt:getidles
          +sc:getwrap,+1,#vt:getwrap
          +sc:getcoloring,+2,#vt:getcoloring
          +sc:getbackground,+1,#vt:getbackground
          +sc:getprofilename,+16,#vt:getprofilename
vt:sclen  equ     (*-vt:sctable)/4
          page
          ifund   revision
revision  equ     ascii:space
          fin     revision

;   This MUST be immediately before the code for VT:RESET

          fcb     version,revision
vt:dreset ; call the device reset routine
;   initialize various dcb:xxx entries
;   called once per DCB at boot time, and then never called again

          tst     vt:timeoutblocks     used by SC:SETOUTPUTTIMEOUT, SC:SETREADTIMEOUT
          bne     vt:dreset1           B/ already counted them
          ldx     sdos+sdos:configuration
          ldx     cnfg:timeoutlist,x
          beq     vt:dreset1
vt:dreset2
          inc     vt:timeoutblocks
          ldx     timeout:link,x
          bne     vt:dreset2
vt:dreset1
          ldx     dcbpointer
          ldaa    dcb:displaydepth,x
          staa    dcb:cursorlost,x     cursor lost for displaydepth lines
          ldd     #vt:tlputbuf         default value for dcb:tlbuffer
          tst     dcb:tlputbuf+1,x     JMP 0 in there now ?
          bne     vt:dreset3           b/ no, leave it alone
          std     dcb:tlputbuf+1,x     yes, replace with default
vt:dreset3
          jsr     dcb:reset,x          this routine lives in disk buffer pool
                                       ; or in a line or ring buffer
          ldd     dcb:baudrate,x       set initial baud rate
          jsr     dcb:ilsetbaudrate,x
;         bcs     *+2                  ignore error if can't set baud rate
          jsr     vt:clearin           empty the input ring
          jsr     vt:clearout          empty the output ring
          ldaa    dcb:profile+dpb:profilenumber,x holds initial DPB number
;         jsr     vt:setprofile.reset
;         okrts
          jmp     vt:setprofile.reset
          page    VT Edit Task Level Procedure
vt:edittask

;   performs input and output editing, asynchronous to the User Level
;   procedure; (dcbpointer) -> dcb

;   the task is blocked until a request has been issued (dcb:doneflag is
;   cleared); the last error is cleared; a pre-computed function address
;   found in dcb:dittaskfn, is used to select the appropriate function,
;   the returned error, if any, is saved in dcb:lasterror
;   dcb:doneflag is set to one to indicate completion, and
;   the task is again blocked until a new request has been made

          ldx     dcbpointer
          bsr     vt:blockedittask               don't come back until work to do
          bcs     vt:edittask                    FIREWALL: defends against
;  extremely faint chance that ^P (or the like) causes a
;  Device Timed Out class error. A proper cure would store the error code
;  into dcb:lasterror.
          clr     dcb:lasterror,x         zero the last error code
          clr     dcb:lasterror+1,x
          if      m6800!m6801
          ldd     dcb:edittaskfn,x        fetch desired function address
          bsr     vt:edittask3            call the desired function, X -> DCB
          else    (m6809)
          jsr     [dcb:edittaskfn,x]      call editor task desired function
                                          ; assert: X -> DCB
          fin
          bcc     vt:edittask1
          txd
          ldx     dcbpointer
          std     dcb:lasterror,x
vt:edittask1
          ldx     dcbpointer
          ldaa    dcb:scb+scblk:wlen,x
          anda    #$7f                           ignore wait bit
          cmpa    #scblk:rdlen+2
          blo     vt:edittask2
          ldd     dcb:scb+scblk:rplen,x
          ldx     dcb:callerscb,x
          std     scblk:rplen,x
          ldx     dcbpointer
vt:edittask2
          inc     dcb:doneflag,x
          bra     vt:edittask

          if      m6800!m6801
vt:edittask3
          ldx     dcbpointer              do this here for callee convenience
          pshd                            push function address
          rts                             go to function, return is on stack
          fin
          page
vt:blockedittask1 ; process keyboard service request
          bsr     vt:servicelineflags            handle line flags if changed
          bsr     vt:bck
          jsr     vt:postctlc
;         bra     vt:blockedittask

vt:blockedittask

;   block the edit task until dcb:doneflag becomes zero, a ^C was received,
;   or a special control function was requested

;   check reminders, here

          ldd     #vt:blockedittaskcond
          jsr     sdos+sdos:waitcond
          ldx     dcbpointer
          lda     dcb:doneflag,x                 done ?
          bne     vt:blockedittask1              yes, must be ^P, ESC, etc.
          okrts
          page
vt:blockedittaskcond

;   the edit task is blocked until dcb:doneflag becomes zero

          lda     dcb:doneflag,x
          beq     vt:blockedittaskcond2
          lda     dcb:iillflgs,x                   check line flag arrival
          ora     dcb:beepcount,x                  check "should complain"
          ora     dcb:iilspl,x                     check special fn  (^P, etc.)
          ora     dcb:ctlccount,x                  check "handle ^C"
          rts

vt:blockedittaskcond2
          inca                                     = ldaa    #1
          rts
          page
vt:servicelineflags

;   Checks the interrupt level line flags to see if any have changed; if
;   so, the change is noted in dcb:lineflags, and the interrupt level line
;   flags byte is cleared to acknowledge receipt.

;   dcb:iillflgs set to non-zero value only by interrupt service routine
;   reset to zero only by this routine (task level)

          ldaa    dcb:iillflgs,x
          beq     vt:servicelineflagsrts
          ldb     vt:sdosmtflag
          beq     vt:servicelineflags3
          tfr     a,b
          orab    lineflags
          stab    lineflags
vt:servicelineflags3
          oraa    dcb:lineflags,x
          staa    dcb:lineflags,x
          jsr     vt:setnewstatus         signal "new status" to SDOS/MT
vt:servicelineflags5
          clr     dcb:iillflgs,x          ack service of line flags
vt:servicelineflagsrts
          rts
          page
vt:bck

;   services asynchronous beep requests; also services ^C, ^O, ^P, ^S, and ^Q

;   this routine will be called recursively during processing of ^S, but only
;   once

;   note: this routine CAN return errors such as Device Timed Out!

          lda     dcb:beepcount,x
          beq     vt:bck3
vt:bck19
          ldaa    #ascii:bel
          jsr     vt:tlstore
          dec     dcb:beepcount,x
          bne     vt:bck19
          okrts

vt:bck3

;   now look at the special control keys;  if any of them are set, process
;   them as appropriate, and then clear the interrupt level flag, so that
;   another may be accumulated; after clearing the flag, return with carry
;   set to indicate a condition was serviced

          ldaa    dcb:iilspl,x
          bne     vt:bck4
          okrts
          page
vt:bck4

;   test for ^P, and service it if set: toggle the page mode flag, and
;   set the lines per page count to display depth.  if the page mode
;   flag is being toggled off, this count is ignored elsewhere.
;   complain if output only device

          bita    #dcbiilspl:page
          beq     vt:bck5
          ldb     dcb:profile+dpb:flags,x
          bitb    #dpbflags:output
          bne     vt:bckcomplain                 b/ output only device
          ldaa    dcb:displaydepth,x
          bne     vt:bck41
          inca
vt:bck41
          staa    dcb:rowct,x
          ldaa    #dcbeditflags:page
          eora    dcb:editflags,x
          staa    dcb:editflags,x
vt:bck1
          clr     dcb:iilspl,x
;         clc                                    accomplished by CLR
          rts

vt:bck5

;   test for ^Q, and service it if set: replenish the lines per page count
;   with display depth--this will cause ^S and ^P conditions to resume
;   output.  if ^O is set, the reminder is cleared

          bita    #dcbiilspl:continue
          beq     vt:bck6
          ldaa    dcb:displaydepth,x
          bne     vt:bck51
          inca
vt:bck51
          staa    dcb:rowct,x                    this unblocks ^P freeze
          jsr     vt:clearreminder
          bra     vt:bck1
          page
vt:bck6

;   test for ^S, and service it if set: beep if any reminders are already
;   current; otherwise, post a ^S reminder and wait until an acceptable
;   condition occurs, then clear the reminder and resume output

          bita    #dcbiilspl:freeze
          beq     vt:bck7
          ldaa    dcb:reminders,x
          anda    #dcbreminders:ctls+dcbreminders:ctlp+dcbreminders:ctlo
          beq     vt:bck61
vt:bck2
vt:bckcomplain ; bellyache about something, and take away the cause
          clr     dcb:iilspl,x
          inc     dcb:beepcount,x
          bra     vt:bck19                       go BEEP

vt:bck61
          clr     dcb:iilspl,x
          ldaa    #'^
          ldab    #'S
          jsr     vt:postreminder
          ldaa    #dcbreminders:ctls
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
          clr     dcb:rowct,x
          jsr     vt:waitfreeze
          jsr     vt:clearreminder
          jmp     vt:bck
          page
vt:bck7

;   test for ^O, and service it if set: beep if any reminders are already
;   current, or a read is in progress; otherwise, post a ^O reminder and exit

          bita    #dcbiilspl:discard
          beq     vt:bck9
          ldaa    dcb:reminders,x
          bita    #dcbreminders:ctls+dcbreminders:ctlp+dcbreminders:rip
          bne     vt:bck2
          bita    #dcbreminders:ctlo
          bne     vt:bck71
          ldaa    #'^
          ldab    #'O
          jsr     vt:postreminder
          ldaa    #dcbreminders:ctlo
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
          jmp     vt:bck1

vt:bck71
          jsr     vt:clearreminder
          jmp     vt:bck1

vt:bck9

;   test for an input timeout request from the interrupt level, and record
;   it if set

          bita    #dcbiilspl:into
          beq     vt:bck11
          ldaa    #dcbreminders:into
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
vt:bck11
          jmp     vt:bck1
          page
vt:reada ; read ascii

;         ldx     dcbpointer
          jsr     vt:testoutput        complain if this is output only device
          lda     dcb:scb+scblk:wlen,x
          cmpa    #scblk:end+4         see if positioning is requested
          blo     vt:reada7.1          b/ not requested
          jsr     vt:testhardcopy      can't position on hardcopy
          ldd     dcbpointer
          addd    #dcb:scb+scblk:end
          tdx
          jsr     vt:impliedposition
          ldx     dcbpointer
vt:reada7.1
          ldaa    #\dcbeditflags:readb
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          lda     dcb:scb+scblk:params+1,x if READA, non-linemode, then 0 count
          beq     vt:reada2            B/ non-linemode
          bsr     vt:flbfer
vt:reada1
          ldd     dcb:scb+scblk:rdlen,x
          beqd    vt:reada8
          jsr     vt:linebuffetch
          bcs     vt:reada5
          cmpa    #ascii:sub           (^Z)
          bne     vt:reada3

;   by placing ^Z in buffer, EOF isn't set until ^Z is actually read!
;   if ^Z is an activation character, then don't signal eofhit

          ldab    #%1##(ascii:sub&%111)
          bitb    dcb:activation+(ascii:sub##-3),x
          bne     vt:reada3
          ldx     dcb:calleriocb,x
          inc     iocb:eofflag,x
          jsr     sdos+sdos:error
          #err:eofhit
          page
vt:reada3
          jsr     vt:storerdbuf
          bra     vt:reada1

vt:reada8

;   RDBUF has been completely filled-- if there is more data to go in it,
;   then an activation character has obviously NOT been stored in the
;   RDBUF and err:activationnotinbuffer is returned, along with the contents
;   of RDBUF

          lda     dcb:linebufcount,x
          beq     vt:reada2
          jsr     sdos+sdos:error
          #err:activationnotinbuffer

vt:reada2
          okrts
          page
;   check for input terminated by ESC or by timeout, and return with the
;   appropriate error; ESC takes precedence over (and cancels) input timeout
;   condition

vt:reada5
          ldaa    #dcbeditflags:esc
          bita    dcb:editflags,x
          beq     vt:reada5.1
          lda     #\(dcbeditflags:esc!dcbeditflags:into)
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          jsr     sdos+sdos:error
          #err:attention

vt:reada5.1
          ldaa    #dcbeditflags:into
          bita    dcb:editflags,x
          beq     vt:reada2
          coma
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          jsr     sdos+sdos:error
          #err:timedinputexpired
          page
vt:flbfer ; if line buffer not filled, then fill it from the typeahead buffer

;   this calls vt:flb and serves to clean up all flags, regardless
;   of the nature of the exit from vt:flb

          bsr     vt:flb
          bcs     vt:flbfererr
vt:flbfercleanup
          lda     #\(dcbwelflags:retype!dcbwelflags:pref!dcbwelflags:fldw!dcbwelflags:flde)
          anda    dcb:welflags,x
          sta     dcb:welflags,x
          ldaa    #\dcbreminders:rip
          anda    dcb:reminders,x
          staa    dcb:reminders,x
          ldd     #0                   de-fuse input time-outs
          std     dcb:inputtoblk+timeout:fuse,x
          std     dcb:readperiod,x
          clr     dcb:iilspl,x         prevent timed-input splinter
          okrts

vt:flbfererr
          jsr     sdos+sdos:errorsave
          ldx     dcbpointer
          bsr     vt:flbfercleanup
          jmp     sdos+sdos:errored
        page
vt:flboverhead

;   set up readcol, endcol, and field width (if required)

        lda     dcb:colct,x
        sta     dcb:readcol,x
        sta     dcb:endcol,x

;   if a field width has been defined, compute the end of the field and
;   check that it does not exceed the width of the display

        lda     dcb:welflags,x
        bita    #dcbwelflags:fldw
        beq     vt:flboverhead.1a      b/ field width not set
        oraa    #dcbwelflags:flde      note that field end is valid
        sta     dcb:welflags,x
        lda     dcb:readcol,x
        adda    dcb:fieldwidth,x
        bcs     vt:errbadfield         b/ unreasonable limit for field
        sta     dcb:fieldend,x         note loc of char past end of field
        cmpa    dcb:displaywidth,x     check that it doesn't equal or exceed
        bhs     vt:errbadfield           display width
vt:flboverhead.1a
        lda     dcb:welflags,x         see if welpos is valid
        bita    #dcbwelflags:pref
        beq     vt:flboverhead.1b      b/ not valid
        lda     dcb:welpos,x
        adda    dcb:readcol,x          offset welpos by readcol
        bcc     vt:flboverhead.2       b/ no overflow
        ldaa    #255                   overflow, use this as max value
vt:flboverhead.2
        sta     dcb:welpos,x
vt:flboverhead.1b
        okrts

vt:errbadfield
        jsr     sdos+sdos:error
        #err:badfieldwidth
        page
vt:flb

;   if line buffer is pre-filled then
;      set readcol, endcol
;      if field width then set fieldend
;      echo the line
;      if activation then
;         set actcol, actdisp, line buffer pointer
;         return
;      else position the cursor
;   else
;      if activation then return
;      set readcol, endcol, line buffer count, line buffer pointer
;      if field width then set fieldend
;   fi
;   if attention then return with error
;   set read in progress, read ascii, read time-out

        lda     dcb:welflags,x         see if buffer has been pre-filled
        bita    #dcbwelflags:pref
        lbeq    vt:flbnopref           b/ it hasn't been pre-filled

;   the buffer has been pre-filled-- set up to echo it

        bsr     vt:flboverhead         take care of fields, readcol, endcol

;   if echo is not in effect for pre-fill display, inhibit actual output

        lda     dcb:welflags,x
        bita    #dcbwelflags:retype
        beq     vt:flb.1b              b/ echoing is in effect
        ldb     dcb:colct,x            save real colct for later
        pshb
        page
vt:flb.1b

;   echo the pre-filled buffer, making the welpos field exact:
;   if, at any time while echoing the buffer, the previous colct was less
;   than the welpos and the current colct is greater than or equal to
;   the welpos, set the colct to the current welpos

;   echo the buffer by reseting the line buffer pointer, and calling
;   vt:editff.1 for line buffer count characters; endcol is updated by
;   vt:editff

        ldd     dcb:linebuf,x
        std     dcb:linebufptr,x
        lda     dcb:linebufcount,x
        beq     vt:flb.1h              b/ nothing to echo
vt:flb.1d
        psha
        lda     dcb:colct,x            remember colct before move
        psha
        jsr     vt:editff.1            display the character
        bcs     vt:flberr3s            b/ error! go recover.
        pula                           now, see if we passed welpos
        ldb     dcb:colct,x
        cmpb    dcb:welpos,x
        blo     vt:flb.1c              b/ no--still before welpos
        cmpa    dcb:welpos,x           passed welpos, but just?
        bhs     vt:flb.1c              b/ no--a lot earlier
        stb     dcb:welpos,x           make welpos accurate
        ldd     dcb:linebufptr,x       remember buffer pointer, too
        std     tempx+4
vt:flb.1c
        pula                           now see how many more to go
        deca
        bne     vt:flb.1d              b/ more to go
vt:flb.1h
        lda     dcb:welpos,x           check for position to end of line
        cmpa    #255
        bne     vt:flb.1e              b/ nothing special
        lda     dcb:colct,x
        sta     dcb:welpos,x           now, even welpos at eol is accurate!
        page
vt:flb.1e
        lda     dcb:colct,x            mark activation column, displacement
        sta     dcb:actcol,x           in case activation
        ldb     dcb:linebufptr+1,x
        subb    dcb:linebuf+1,x
        stb     dcb:actdisp,x

;   if actual output was suppressed, colct, welflags, and welpos must be
;   restored to their proper values

        lda     dcb:welflags,x
        bita    #dcbwelflags:retype
        beq     vt:flb.1f              b/ echoing is in effect
        pulb
        stb     dcb:colct,x            restore colct to what it was
        lda     dcb:welflags,x         turn off retype flag
        anda    #\dcbwelflags:retype
        sta     dcb:welflags,x
vt:flb.1f

;   if activation has really occurred, then actcol and actdisp are already
;   correct and all that must be done is to reset the line buffer pointer
;   and return

        lda     dcb:editflags,x
        bita    #dcbeditflags:activate
        beq     vt:flb.1g              b/ activation has not occurred
        ldd     dcb:linebuf,x
        std     dcb:linebufptr,x
        okrts

vt:flberr3s ; error occurred, 3 bytes on top of stack need removal.
        ins                            remove a byte
vt:flberr2sj ; error occurred, 2 bytes on top of stack need removal.
        jmp     vt:err2s               go remove 2 bytes and propogate error
        page
vt:flb.1g

;   well, we didn't activate, so move the cursor to welpos

        ldd     tempx+4                take a shortcut to welpos
        std     dcb:linebufptr,x
        lda     dcb:row,x
        ldb     dcb:welpos,x
        stb     dcb:colct,x
        jsr     vt:cursorposn
        pshd
        jsr     dcb:profile+dpb:position,x
        bcs     vt:flberr2sj           b/ error occurred, go recover

;       assert: the positioning routine did it's thing

        leas    2,s
        bra     vt:flbnoact
        page
vt:flbnopref

;   the buffer wasn't pre-filled; see if there's activation

        lda     dcb:editflags,x
        bita    #dcbeditflags:activate!dcbeditflags:esc!dcbeditflags:into
                                       ; check for:
                                       ;    input data terminated
                                       ;    user struck ESC
                                       ;    input timed out
        beq     vt:flb.1k              b/ no activation

;   there is activation: whoever set activation has already set up all
;   the necessary information, so just return

        okrts

vt:flb.1k

;   there's no activation, so set up a nice, empty buffer

        clr     dcb:linebufcount,x
        ldd     dcb:linebuf,x
        std     dcb:linebufptr,x
        jsr     vt:flboverhead
        page
vt:flbnoact

;   all of the pre-buffer-filling shenanigans have been attended to--
;   get ready to start the editing

;   if attention then return with error
;   set read in progress, read ascii, read time-out

          jsr     vt:attncheck1
          lbcs    vt:flbattn
          jsr     vt:ctlocheck
          ldaa    #\dcbeditflags:readb
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          ldaa    #dcbreminders:rip
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
          ldd     dcb:readperiod,x
          std     dcb:inputtoblk+timeout:fuse,x
          page
vt:flbloop

;   Repeat
;      Fetch a character from the interrupt input buffer
;      If non-standard_activation Then Gosub handle_non-standard_activation
;      If RUBOUT Then Gosub edit_rub
;      If character >= space then Gosub buffer_and_echo
;      On character Gosub edit_*
;   Unless activation Or Error End

;   terminate the read if a field is currently defined and the cursor is at
;   or beyond the end of the field

          lda     dcb:welflags,x
          bita    #dcbwelflags:flde
          beq     vt:flbloop1          b/ don't activate if fields not current
          lda     dcb:colct,x
          cmpa    dcb:fieldend,x
          blo     vt:flbloop1
          jsr     vt:markactivation
vt:flbloop1
          jsr     vt:attncheck1
          bcc     vt:flb2
vt:flbattn

;   some kind of error was encountered: if it's anything but input timeout,
;   or "attention", ream the line buffer, and return with the error.
;   Input timeouts and attention return the collected buffer contents.

          cpx     #err:timedinputexpired
          beq     vt:flbattn1
          cpx     #err:attention
          beq     vt:flbattn2
          jsr     sdos+sdos:errorsave
          ldx     dcbpointer
          clr     dcb:linebufcount,x
          jsr     vt:clearactivation
          jmp     sdos+sdos:errored
          page
vt:flbattn1

;   it was an input timeout error, so keep everything that was entered
;   so far, set activation, and return with the error

          ldx     dcbpointer
          jsr     vt:markactivation
          ldaa    #dcbeditflags:into
          oraa    dcb:editflags,x
          staa    dcb:editflags,x
          bra     vt:flb2

vt:flbattn2

;   it was an attention, so keep everything that was entered so far,
;   set activation, and return with the error

          ldx     dcbpointer
          jsr     vt:markactivation
          jsr     vt:displayrest        move cursor to end of field
          ldaa    #dcbeditflags:esc
          oraa    dcb:editflags,x
          staa    dcb:editflags,x
;         bra     vt:flb2
          page
vt:flb2

;   before going to process the next character, see if activation
;   has occurred

          lda     #dcbeditflags:activate
          bita    dcb:editflags,x
          beq     vt:flb3              b/ no activation, this time
          ldaa    dcb:linebufcount,x   set up for ^W on next call to vt:flbfer
          staa    dcb:lastlinebufcount,x
          ldd     dcb:linebuf,x
          std     dcb:linebufptr,x
          okrts

vt:flb3

;   no activation, so get the next character and decide what to do with it

          jsr     vt:tlfetch
          bcs     vt:flbattn           and handle error
;                                      (this handles ^C^C, timed input and ATTN)
          bsr     vt:processinputcharacter
          bra     vt:flbloop
          page
vt:processinputcharacter

;   *** HERE IS WHERE EDITOR TASK STARTS INPUT CHARACTER PROCESSING
;   character in (A); the parity bit is zero

          jsr     vt:checkactivation   (note: only way to activate on Null!)
          lbcs    vt:editactne         b/ handle non-standard activation
;   Note: vt:logchr won't echo ASCII:RUBOUT even if marked as activation
          cmpa    #ascii:rubout
          lbeq    vt:editrub           b/ handle RUB
          cmpa    #ascii:space
          lbhs    vt:edityawn          b/ handle boring character

;   since we're here, the character must be an exciting control character:
;   use the table to vector to the correct handler

          if      m6800!m6801
          staa    tempx+2
          tfr     a,b
          aslb
          clra
          addd    #vt:linebufctlchars
          tdx
          ldd     0,x
          pshd
          ldx     dcbpointer
          ldaa    tempx+2              pass char in (A) for use by callee
          rts
          else    (m6809)
          ldu     #vt:linebufctlchars  branch on control character
          tfr     a,b                  preserve char in (A) for use by callee
          aslb
;         ldx     dcbpointer           for convenience of callee
          jmp     [b,u]
          fin
          page
vt:linebufctlchars

;   once it has been determined that the character input is a control character
;   which is not an activation character, this handy little vector is used
;   for quick dispatch

          fdb     vt:editrts   (^@)     ignore
          fdb     vt:editrts   (^A)     ignore--handled elsewhere
          fdb     vt:editrts   (^B)     ignore--handled elsewhere
          fdb     vt:editrts   (^C)     ignore--handled elsewhere
          fdb     vt:editrts   (^D)     ignore--handled elsewhere
          fdb     vt:editenq   (^E)     erase to end of line
          fdb     vt:editack   (^F)     move cursor to front (left) of field
          fdb     vt:editrts   (^G)     ignore
          fdb     vt:editbs    (^H)     backspace cursor
          fdb     vt:edityawn  (^I)     tabbing
          fdb     vt:editlf    (^J)     cursor down--fields only
          fdb     vt:editvt    (^K)     cursor up--fields only
          fdb     vt:editff    (^L)     forespace cursor
          fdb     vt:editacte  (^M)     activate on carriage return
          fdb     vt:edityawn  (^N)     pass through
          fdb     vt:editrts   (^O)     ignore--handled elsewhere
          fdb     vt:editrts   (^P)     ignore--handled elsewhere
          fdb     vt:editrts   (^Q)     ignore--handled elsewhere
          fdb     vt:editdc2   (^R)     retype line/move cursor to right end
          fdb     vt:editrts   (^S)     ignore--handled elsewhere
          fdb     vt:editrts   (^T)     ignore--handled elsewhere
          fdb     vt:editnak   (^U)     delete character under cursor
          fdb     vt:editrts   (^V)     ignore--handled elsewhere
          fdb     vt:editlast  (^W)     get last line
          fdb     vt:editcan   (^X)     cancel line
          fdb     vt:edityawn  (^Y)     pass through   
          fdb     vt:editacte  (^Z)     activate on end of file
          fdb     vt:editrts   (ESC)    ignore--handled elsewhere
          fdb     vt:edityawn  (^\)     pass through   
          fdb     vt:edityawn  (^])     pass through   
          fdb     vt:edityawn  (^^)     pass through   
          fdb     vt:edityawn  (^_)     pass through   
        page
vt:insertcharacter

;   insert the character in (a) into the line buffer according to
;   the line buffer pointer

;   calculate number of bytes to shuffle to the right in order to perform
;   the insertion

        clr     dcb:lastlinebufcount,x  remember that ^W is illegal now
        ldb     dcb:linebufcount,x
        addb    dcb:linebuf+1,x
        subb    dcb:linebufptr+1,x
        ldx     dcb:linebufptr,x
        stb     tempx
        beq     vt:insertcharacter.3   b/ special case: no bytes to shuffle
vt:insertcharacter.2

;   insert by shuffling to the right

        ldb     ,x
        sta     ,x+
        tfr     b,a
        dec     tempx
        bpl     vt:insertcharacter.2
        bra     vt:insertcharacter.4

vt:insertcharacter.3
        sta     ,x
vt:insertcharacter.4
        ldx     dcbpointer
        inc     dcb:linebufcount,x
        okrts                          JMP in vt:editactne forces "okrts"
        page
vt:checkinsertroom

;   if another character could cause colct to wrap, or the character
;   would not fit in the line buffer, and leave room for one activation char,
;   then beep and return to the caller's caller

        ldb     dcb:colct,x
        cmpb    #254
        bhs     vt:checkinsertroom.1   b/ colct could wrap
        ldb     dcb:linebuflen,x
        decb                           subtract off space for activation char
        cmpb    dcb:linebufcount,x
        bls     vt:checkinsertroom.1   b/ no room in line buffer
        okrts

vt:checkinsertroom.1
        leas    2,s
vt:editcomplain ; issue error on editing keystroke
        ldaa    #ascii:bel
        jmp     vt:logchr
        page
vt:editlast ; ^W typed, retreive last keyboard line
        jsr     vt:hcbeep              ^W is illegal on hardcopy device
        ldab    dcb:lastlinebufcount,x is previous line buffer still intact ?
        beq     vt:editcomplain        b/ no, so we can't retreive it
        clr     dcb:lastlinebufcount,x make sure further ^W are illegal
        stab    dcb:linebufcount,x     make line buffer re-appear
        decb                           find last byte of buffer (...-1)
        clra                           (extend to 16 bits)
        addd    dcb:linebufptr,x       = bufaddr+count-1
        tdx
        ldaa    ,x                     fetch possible activation character
        ldx     dcbpointer             restore DCB address
        cmpa    #ascii:rubout          specially treated character ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
        if      1
        ; drop any non-printing character as good compromise between
        ; what would be perfect here and how much code it would take
        ; (if you want to see sample code, look in source here...
        ; for code conditionally assembled out)
        cmpa    #ascii:space           a printing character ?
        blo     vt:editlast.2          b/ no, drop from keyboard line
        else
        cmpa    #ascii:cr              standard activator ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
        ldab    dcb:welflags,x         are we in a field ?
; should also check for sedit activation, etc.
        bitb    #dcbwelflags:flde      ...?
        bne     vt:editlast.3          b/ no
        cmpa    #ascii:bs              left arrow ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
        cmpa    #ascii:vt              right arrow ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
        cmpa    #ascii:lf              down arrow ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
        cmpa    #ascii:ff              right arrow ?
        beq     vt:editlast.2          b/ yes, drop from keyboard line
vt:editlast.3 ; last character is not a special activator
        fin
        jsr     vt:checkactivation     is the character an activator ?
        bcc     vt:editlast.1          b/ no, retain last character
vt:editlast.2 ; last character of line is activator character, drop it
        dec     dcb:linebufcount,x     yes, drop it from the line buffer
vt:editlast.1
        jmp     vt:displayrest         go display the recovered line
        ; this leaves cursor in last character of displayed field,
        ; so we don't activate on field overflow
        page
vt:markactivation

;   used to mark non-character activation

        lda     dcb:colct,x
        sta     dcb:actcol,x
        ldb     dcb:linebufptr+1,x
        subb    dcb:linebuf+1,x
        stb     dcb:actdisp,x
        ldaa    #dcbeditflags:activate
        oraa    dcb:editflags,x
        staa    dcb:editflags,x
*       bsr     vt:setnewstatus
*       rts

vt:setnewstatus

;   if status has changed, flag the fact and increment vt:allstatuscount
;   "status change" is defined as:
;       ^C^C occurred
;       Input for a port is complete
;       Line flags for a port have changed
;   called only by task level routines or vt:ilasietx

        tst     dcb:newstatus,x
        bne     vt:setnewstatus.1    b/ this is not new status
        inc     dcb:newstatus,x
        inc     vt:allstatuscount
        lda     vt:sdosmtflag        running under /MT ?
        bne     vt:setnewstatus.1    b/ no
        inc     [vt:sdosmtstatuschange] yes, tell SDOS/MT about status change!
        ldx     sdos+sdos:configuration
        ldx     cnfg:mtprims,x       now ABORTTIMESLICE
        leax    -3,x                 form pointer to ABORTTIMESLICE vector
        jsr     0,x
        ldx     dcbpointer
vt:setnewstatus.1
        rts
        page
vt:multispace

;   (b) is count of number of spaces to stuff in the output buffer

        tstb
        beq     vt:multispace.rts
vt:multispace.1
        pshb
        ldaa    #ascii:space
        jsr     vt:logchr
        bcs     vt:err1sj              b/ error, remove 1 byte from stack
        pulb
        decb
        bne     vt:multispace.1
vt:multispace.rts
        okrts

vt:multibs

;   (b) is count of number of backspaces to stuff in the output buffer

        tstb
        beq     vt:multibs.rts
vt:multibs.1
        pshb
        ldaa    #ascii:bs
        jsr     vt:logchr
        bcs     vt:err1sj              b/ error, remove 1 byte from stack
        pulb
        decb
        bne     vt:multibs.1
vt:multibs.rts
        okrts
        page
vt:edityawn

;   the character is boring--
;   just buffer it (as long as it won't cause the column counter to wrap)

        jsr     vt:checkinsertroom
        jsr     vt:insertcharacter
        jsr     vt:editff.1            advance cursor, causing echoing
        lda     dcb:colct,x
        psha
        jsr     vt:displayrest         update rest of line
        bcs     vt:err1sj              b/ error, remove 1 byte from stack
        pula
        jsr     vt:hctest
        bcs     vt:editrts             b/ hard copy doesn't put the cursor back
;       jmp     vt:restorecursor       put the cursor back

vt:restorecursor ; now move cursor back to its insert position in line
        ; this backspaces over tabs, line wraps and the like
; ??? do a real cursor position if cursor is not lost ???
; this would be very useful when running SEDIT!
        cmpa    dcb:colct,x
        beq     vt:restorecursor.1     b/ cursor repositioned
        psha
        jsr     vt:editbs.1
        bcs     vt:err1sj              b/ error, remove 1 byte from stack
        pula
        bra     vt:restorecursor

vt:restorecursor.1
;        okrts

vt:editrts ; the character gets ignored-- just return without doing anything
        okrts
        page
vt:editacte

;   insert the activation character in (a) at the end of the line buffer
;   and echo the character

        psha                           assert: room avail in line buffer
        jsr     vt:markactivation      remember activation column & disp.
        jsr     vt:displayrest
        bcs     vt:err1sj              b/ error, remove 1 byte from stack
        pula
        bsr     vt:editinsertactivation append character and echo
        jmp     vt:editff.1

vt:err1sj ; error, need to remove 1 byte from stack before propogating it
        jmp     vt:err1s

vt:editactne

;   insert the activation character in (a) at the end of the line buffer
;   but don't echo the character unless it is printable

        cmpa    #ascii:space
        bhs     vt:editacte            b/ printable character
        psha                           assert: room avail in line buffer
        jsr     vt:markactivation      remember activation column & disp.
;       bcs     ?                      assert: no error exit possible
        pula
;       bra     vt:editinsertactivation

vt:editinsertactivation ; insert activation character (A) at end of buffer
        psha
        clra                           append character to line buffer
        ldb     dcb:linebufcount,x
        addd    dcb:linebuf,x
        std     dcb:linebufptr,x
        pula
        jmp     vt:insertcharacter
        page
vt:editcan.1 ; handle ^X on hardcopy device
        clr     dcb:linebufcount,x
        ldd     dcb:linebuf,x
        std     dcb:linebufptr,x
        lda     dcb:readcol,x
        sta     dcb:endcol,x
        lda     #ascii:can
vt:hardcopycue
        jsr     vt:logchr
        lda     #ascii:cr
        jsr     vt:logchr
        ldb     dcb:readcol,x
        jmp     vt:multispace
        page
vt:editcan ; ^X was typed: erase the entire input line

        jsr     vt:hctest
        bcs     vt:editcan.1           b/ hard copy editing
        bsr     vt:backspacetobol      move cursor to beginning of line
;       jmp     vt:editenq             and do Erase to EOL

vt:editenq

;   ^E was typed: erase all input under, and to the right, of the cursor

        jsr     vt:hcbeep
        ldb     dcb:linebufptr+1,x
        subb    dcb:linebuf+1,x
        stb     dcb:linebufcount,x
        ldb     dcb:endcol,x
        subb    dcb:colct,x
        pshb
        lda     dcb:colct,x
        sta     dcb:endcol,x
        jsr     vt:multispace          can't output ^E because...
        bcs     vt:err1sj              b/ error, 1 byte on top of stack
        pulb                           we might be in a field!
        jmp     vt:multibs
        page
vt:editack ; ^F was typed, back up cursor to start of input field
        jsr     vt:hcbeep              ^F is illegal on hardcopy device
;       bsr     vt:backspacetobol
;       rts

vt:backspacetobol ; back up cursor until it is at start of input field

        lda     dcb:colct,x
        cmpa    dcb:readcol,x
        beq     vt:backspacetobol.okrts b/ at begin of line--erase to eol
        jsr     vt:editbs.1            back up cursor
        bra     vt:backspacetobol

        page
vt:editdc2

;   ^R was typed: move cursor to right end of field if not hardcopy.
;   Otherwise, display the control character at the end of the line, display
;   a <CR>, and then display the entire input on the new physical line

        jsr     vt:hctest
        bcs     vt:editdc2.1           b/ hard copy editing
;       bsr     vt:displayrest         which advances cursor to right end!
;       okrts

vt:displayrest

;   display from line buffer pointer to the end of the logical line or field

        ldb     dcb:linebufcount,x
        addb    dcb:linebuf+1,x        assert: linebuffer is <= 255 bytes in size
        subb    dcb:linebufptr+1,x
        beq     vt:displayrest.1       b/ nothing else to display
vt:displayrest.2
        lda     dcb:welflags,x         are we in a field ?
        bita    #dcbwelflags:flde      ...?
        beq     vt:displayrest.3       b/ no
        lda     dcb:colct,x            yes, are we at field end ?
        cmpa    dcb:fieldend,x         ...?
        bhs     vt:displayrest.1       b/ yes, can't display more!
vt:displayrest.3
        pshb
        jsr     vt:editff.1
        bcs     vt:err1s2j             b/ error, must remove 1 byte from stack
        pulb
        decb
        bne     vt:displayrest.2
vt:displayrest.1
vt:backspacetobol.okrts
        okrts
        page
vt:editdc2.1

;   display the control character and start a new line, and then behave as
;   if displaying in place

        bsr     vt:hardcopycue
        ldd     dcb:linebuf,x
        std     dcb:linebufptr,x
        bsr     vt:displayrest
        lda     dcb:colct,x
        sta     dcb:endcol,x
        okrts

vt:err1s2j
        jmp     vt:err1s
        page
vt:editrub

;   RUB was typed: simulate typing of ^H^U (backspace and delete character
;   under cursor)

        jsr     vt:hctest
        bcs     vt:editrub.1           b/ hard copy editing
        ldb     dcb:colct,x
        cmpb    dcb:readcol,x
        lbeq    vt:exceptact           b/ cursor at far left of typed input
        jsr     vt:editbs.1            back up the cursor
        bra     vt:editnak             delete character under cursor

vt:editrub.1
        tst     dcb:linebufcount,x
        lbeq    vt:beeprts             b/ nothing more to delete
        dec     dcb:linebufcount,x     forget about character
        ldd     dcb:linebufptr,x
        subd    #1
        std     dcb:linebufptr,x
        lda     [dcb:linebufptr,x]
        ldx     dcbpointer
        ldb     dcb:cursorlost,x       don't let cursor get "found" during
        cmpb    #1                     hard copy rubout
        bne     vt:editrub.2           b/ not in danger of being found
        inc     dcb:cursorlost,x
vt:editrub.2
        jsr     vt:logchr              echo rubbed out character
        lda     dcb:colct,x
        sta     dcb:endcol,x
        okrts
        page
vt:editnak

;   ^U was typed: erase the character under the cursor

        jsr     vt:hcbeep
        ldb     dcb:colct,x
        cmpb    dcb:endcol,x
        lbeq    vt:exceptact           b/ nothing under cursor
        ldb     dcb:linebufcount,x
        lbeq    vt:beeprts             b/ no char to delete (firewall for colct= 255)
        addb    dcb:linebuf+1,x        compute number of chars left on line
        subb    dcb:linebufptr+1,x
        decb
        beq     vt:editnak.2           b/ deleting last char is trivial case
        ldx     dcb:linebufptr,x
vt:editnak.1

;   delete character by shuffling buffer left, on top of character to be
;   deleted

        lda     1,x
        sta     ,x+
        decb
        bne     vt:editnak.1
        ldx     dcbpointer
vt:editnak.2
        dec     dcb:linebufcount,x
        page
;   re-display the rest of the line, blanking out the remains of the previous
;   line, if it was longer

        lda     dcb:colct,x            remember current cursor position
        psha
        jsr     vt:displayrest         move cursor to end of line
        ldb     dcb:endcol,x           perform blanking, if necessary
        subb    dcb:colct,x
        beq     vt:editnak.5           b/ no blanking necessary
        pshb
        jsr     vt:multispace
        bcs     vt:err2s               b/ error, must pop 2 bytes from stack
        pulb
        jsr     vt:multibs
        bcs     vt:err1s               b/ error, must pop 1 byte from stack
vt:editnak.5
        lda     dcb:colct,x            update end column
        sta     dcb:endcol,x
        pula
        jmp     vt:restorecursor       restore cursor position
        page
vt:err4s ; error, must pop 4 bytes from stack before propogating error code
        ins                            pop a byte
vt:err3s ; error, must pop 3 bytes from stack before propogating error code
        ins                            pop a byte
vt:err2s ; error, must pop 2 bytes from stack before propogating error code
        ins                            pop a byte
vt:err1s ; error, must pop 1 bytes from stack before propogating error code
        ins                            pop a byte
        jmp     sdos+sdos:errored      propogate the error
        page
vt:editbs

;   ^H was typed: back up the cursor by one character-- note that this could
;   be multiple positions due to control characters, bells, or tabs

        jsr     vt:hcbeep
        ldb     dcb:colct,x
        cmpb    dcb:readcol,x
        lbeq    vt:beepact             b/ cursor at far left of typed input
vt:editbs.1
        ldd     dcb:linebufptr,x
        subd    #1
        std     dcb:linebufptr,x
        lda     [dcb:linebufptr,x]
        ldx     dcbpointer
        ldb     #1                     at least one backspace
        cmpa    #ascii:space
        bhs     vt:editbs.8            b/ normal character is one position
        incb
        cmpa    #ascii:ht
        beq     vt:editbs.2            b/ tabs get special attention
        cmpa    #ascii:bel
        beq     vt:editbs.7            b/ bel takes no space
vt:editbs.8
        jmp     vt:multibs

vt:editbs.7
        okrts

vt:editbs.2
        bsr     vt:findcolct           find the colct corresponding to pointer
        negb
        addb    dcb:colct,x
        jmp     vt:multibs
        page
vt:findcolct

;   backspacing over tabs is a real pain: all characters, from the beginning
;   of the line buffer to the line buffer pointer are logically (but not
;   actually displayed), and then that column count is subtracted from
;   the actual column count to get the actual number of backspaces to
;   output

        lda     dcb:colct,x
        psha
        lda     dcb:readcol,x
        sta     tempx+3
        ldd     dcb:linebufptr,x
        std     tempx+4
        ldx     dcb:linebuf,x
vt:findcolct.1
        cpx     tempx+4
        beq     vt:findcolct.4         b/ colct corresponds to linebufptr
        lda     ,x+
        cmpa    #ascii:space
        bhs     vt:findcolct.2         b/ standard printing character
        cmpa    #ascii:ht
        beq     vt:findcolct.3         b/ tab
        cmpa    #ascii:bel
        beq     vt:findcolct.1         b/ bell--0 characters displayed
        inc     tempx+3                standard ctl char displays 2 characters
vt:findcolct.2
        inc     tempx+3
        bra     vt:findcolct.1
        page
vt:findcolct.3
        lda     tempx+3                swap some temp cells around so that
        stx     tempx+2                  the nexttabstop routine can be
        ldx     dcbpointer                 safely called--after doing so
        sta     dcb:colct,x                  put things back the way they
        jsr     vt:nexttabstop                 were and proceed to the next
        ldx     tempx+2                          character
        sta     tempx+3
        bra     vt:findcolct.1

vt:findcolct.4
        pulb
        ldx     dcbpointer
        stb     dcb:colct,x
        ldb     tempx+3
        rts
        page
vt:editff

;   ^L was typed: move the cursor forward by one character-- note that this
;   could be multiple positions due to control characters, bells, or tabs

        jsr     vt:hcbeep
        ldb     dcb:colct,x
        cmpb    dcb:endcol,x
        beq     vt:ffact               b/ cursor at far right of typed input
vt:editff.1
        lda     [dcb:linebufptr,x]
        psha
        ldx     dcbpointer
        ldd     dcb:linebufptr,x
        addd    #1
        std     dcb:linebufptr,x
        pula
        jsr     vt:logchr              advance cursor by echoing character
        lda     dcb:colct,x
        cmpa    dcb:endcol,x
        bls     vt:editff.2
        sta     dcb:endcol,x           advance the ending column
vt:editff.2
        okrts

vt:ffact ; see if activation on ^L in field mode
        ldb     dcb:welflags,x
        bitb    #dcbwelflags:flde
        beq     vt:exceptact           b/ not activation because end of field
;       jmp     vt:editactne           this really does work! look carefully.
        page
vt:editlf
vt:editvt
vt:beepact

;   ^J, ^K, ^L, or ^H typed: if a field is currently defined, then activate;
;   otherwise, beep and return

        ldb     dcb:welflags,x
        bitb    #dcbwelflags:flde
        beq     vt:beeprts             b/ a field is not currently defined
        jmp     vt:editactne           activate with no echo

vt:exceptact

;   RUB, ^L, or ^U typed: if the SEDIT exception flag is set, activate;
;   otherwise, beep and return

        ldb     dcb:except,x
        bitb    #dcbexcept:sedit
        beq     vt:beeprts             b/ SEDIT exception flag not set
        jmp     vt:editactne           go activate for SEDIT
        page
vt:hcbeep

;   test if hard copy-- if so, then beep and return to the caller's caller;
;   otherwise, return to the caller

        bsr     vt:hctest
        bcc     vt:hcbeeprts
        leas    2,s                    forget the caller
vt:beeprts
        lda     #ascii:bel
        jmp     vt:logchr              put out the beep and return to his caller

vt:hcbeeprts
        rts                            not hard copy: AOK

vt:hctest

;   test if hard copy or if cursor is lost and dcb:endcol >= dcb:displaywidth
;   return carry set if hard copy, etc.

        ldb     #dcbeditflags:hcedit
        bitb    dcb:editflags,x
        bne     vt:hctest.1
        ldb     dcb:cursorlost,x
        bne     vt:hctest.2
vt:hctest.3
        okrts

vt:hctest.2
        ldb     dcb:endcol,x
        cmpb    dcb:displaywidth,x
        blo     vt:hctest.3
vt:hctest.1
        errorrts
        page
vt:linebuffetch

;   fetch the next byte from the line buffer; if dcb:linebufcount = 0 then
;   return with carry set, indicating end of buffer and activation reached

          lda     dcb:linebufcount,x
          beq     vt:clearactivation
          lda     [dcb:linebufptr,x]
          ldx     dcbpointer
          incd    dcb:linebufptr,x
          dec     dcb:linebufcount,x
          bne     vt:linebuffetch1
          bsr     vt:clearactivation
vt:linebuffetch1
          okrts

vt:clearactivation
          ldab    #\dcbeditflags:activate
          andb    dcb:editflags,x
          stab    dcb:editflags,x
          errorrts
          page
vt:ctlocheck

;   check for a ^O reminder; if present, clear it and prompt with a ?

          ldaa    #dcbreminders:ctlo
          bita    dcb:reminders,x
          beq     vt:ctlocheck1
          jsr     vt:clearreminder
          ldaa    #ascii:cr
          jsr     vt:logchr
          ldaa    #'?
          jsr     vt:logchr
          ldaa    #ascii:space
          jsr     vt:logchr
vt:ctlocheck1
          okrts
          page
vt:checkactivation

;   see if (a) is either a user-defined activation character or the
;   default new line character; errorrts if so
;   this routine preserves (A) and (X)

          tfr     a,b               ; divide (a) by 8 (preserve a)
          lsrb
          lsrb
          lsrb
          if      m6809
          leax    b,x
          else
          addb    dcbpointer+1
          stab    tempx+1
          ldab    #0
          adcb    dcbpointer
          stab    tempx
          ldx     tempx
          fin
          ldb     dcb:activation,x     get byte containing bit desired
          bne     vt:checkactivation1
vt:checkactivation2
          ldx     dcbpointer
          okrts                     ; not even close!
          page
vt:checkactivation1
          psha                      save the character
          anda    #%00000111        mask to get bit number
          if      m6809
          ldx     #vt:actbits       see if proper bit is set
          bitb    a,x
          else
          staa    tempx+1
          ldaa    #vt:actbits/256
          staa    tempx
          ldx     tempx
          bitb    vt:actbits&$FF,x
          fin
          pula                      restore the character
          beq     vt:checkactivation2 ; close, but no cigar!
          ldx     dcbpointer
          errorrts                  ; give the man a cigar!!

vt:actbits
          fcb     1,2,4,8,16,32,64,128
          page
vt:storerdbuf

;   Store (a) in the the caller's RDBUF; adjust local RDBUF, RDLEN, RPLEN
;   appropriately.  RDLEN is not checked for being zero.

          sta     [dcb:scb+scblk:rdbuf,x]
          ldx     dcbpointer
          incd    dcb:scb+scblk:rdbuf,x
          incd    dcb:scb+scblk:rplen,x
          decd    dcb:scb+scblk:rdlen,x
          rts
          page
vt:tlfetch

;   return in (a) the next byte from the input buffer; if none are available,
;   block the edit task until at least one byte is available.

;   check for reminders, here

          jsr     vt:postctlc
          lsr     dcb:ctlckill,x                see if user to be croaked
          bcc     vt:tlfetch1                   b/ he's safe, this time
          bsr     vt:clearactivation            ensure that line buffer is empty
          jsr     sdos+sdos:error
          #err:programkilled

vt:tlfetch1
          ldaa    dcb:reminders,x
          bita    #dcbreminders:into
          beq     vt:tlfetch3
          anda    #\dcbreminders:into
          staa    dcb:reminders,x
          jsr     sdos+sdos:error
          #err:timedinputexpired

vt:tlfetch3
          jsr     vt:checkto                    check for device timed out
          jsr     vt:attncheck1                 see if ESC already entered
          jsr     vt:tlgetbuf                   code could be inserted inline
          beq     vt:tlwaitdata
          okrts
          page
vt:tlwaitdata ; buffer was empty, wait for input data to arrive
          ldd     #vt:tlwaitdata1
          jsr     sdos+sdos:waitcond
          ldx     dcbpointer
          jsr     vt:servicelineflags           in case they changed
          jsr     vt:bck
          bra     vt:tlfetch

vt:tlwaitdata1 ; task wake-up subroutine to watch for input wake-up condition
          lda     dcb:reminders,x               watch for "timed input"
          anda    #dcbreminders:into
          ora     dcb:tldata,x                  check for input available
          ora     dcb:tldata+1,x
          ora     dcb:iillflgs,x                interrupt level line flags
          ora     dcb:iilspl,x                  special fn request
          ora     dcb:ctlccount,x               handle ^C request
          ora     dcb:beepcount,x               complain request
          rts
          page
vt:readb ; readb reads from line buffer and then directly from type-ahead buffer

;         ldx     dcbpointer
          jsr     vt:testoutput                 illegal if output-only
          ldaa    #dcbreminders:rip
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
          ldaa    #dcbeditflags:readb
          oraa    dcb:editflags,x
          staa    dcb:editflags,x
          clr     dcb:colct,x          zero column counter on READB
          jsr     vt:ctlocheck
          jsr     vt:attncheck1        clear old attention request
          bcs     vt:readb1
vt:readb1
          ldd     dcb:scb+scblk:rdlen,x
          beqd    vt:readb2
          jsr     vt:linebuffetch
          bcc     vt:readb11
          jsr     vt:tlfetch
vt:readb11
          jsr     vt:storerdbuf
          bra     vt:readb1
vt:readb2
          okrts
          page
vt:writeactlc ; service single ^C while doing WRITEA
          jsr     vt:postctlc          ^C clears output buffer!
vt:writea2
          pula                         restore previous echo status
          sta     dcb:welflags,x
vt:writea2.a
          okrts

vt:writea4 ; service special control key
          ldaa    dcb:ctlccount,x      ^C ?
          bne     vt:writeactlc        b/ yes, clear output buffer!
          jsr     vt:bck               no, must be ^P, ^S, ^Q or ^O
          bcs     vt:writeaerred       b/ error, give up!
          lda     dcb:reminders,x      ^O request occur while doing output ?
          bita    #dcbreminders:ctlo   ...?
          beq     vt:writea.loop       b/ no, special control key serviced
          bra     vt:writea2           b/ yes, no more output necessary

vt:writeaerred ; probably a time-out error occurred
          jsr     sdos+sdos:errorsave  save error code
          ldx     dcbpointer
          pula                         restore previous echo status
          sta     dcb:welflags,x
          jmp     sdos+sdos:errored    and then propogate the error
          page
          ifund   optimizedwritea
optimizedwritea   equ   1 ; set to 0 to see essence of vt:writea
          fin

vt:writea ; write ascii text from syscall write buffer to terminal device

;         ldx     dcbpointer
          jsr     vt:checkto           see if a timeout is lying in wait...
          ldaa    dcb:reminders,x
          bita    #dcbreminders:ctlo
          bne     vt:writea2.a
          lda     #255                 no field while WRITEA!!
          sta     dcb:fieldend,x
          lda     dcb:scb+scblk:wlen,x
          cmpa    #scblk:end+4         see if positioning is requested
          blo     vt:writea3.a         b/ not requested
          jsr     vt:testhardcopy      can't position on hardcopy
          ldd     dcbpointer
          addd    #dcb:scb+scblk:end
          tdx
          jsr     vt:impliedposition
          ldx     dcbpointer
vt:writea3.a
          lda     dcb:welflags,x       ensure that echo is enabled
          psha                         (save old state for restoration later)
          anda    #\dcbwelflags:echo
          sta     dcb:welflags,x
          page
vt:writea.loop ; loop to output characters
          ldaa    dcb:ctlccount,x      check for ^C (which clears output buf!)
          oraa    dcb:iilspl,x         or special control keys
          bne     vt:writea4           go service special keystroke
          if      optimizedwritea
          ldb     dcb:scb+scblk:wrlen+1,x enough in WRBUF to bother with ?
          cmpb    #2                   this is min required for a win...
          bhs     vt:writea.optimize   b/ yes, try to optimize
          ldb     dcb:scb+scblk:wrlen,x no, perhaps count > 255 ?
          bne     vt:writea.optimize   b/ yes, go try to optimize
          fin
vt:writea.onechar ; "writea" one character from wrbuf
          jsr     vt:fetchwrbuf        fetch byte from buffer
          beq     vt:writea2           b/ buffer is empty, all done!
          anda    #ascii:mask          dump stupid parity bit
;         cmpa    #ascii:rubout        a rubout ? (vt:logchr dumps rubouts)
;         beq     vt:writea.loop       b/ yes, we must not output it
          jsr     vt:logchr            output the character (could be rubout!)
          bcc     vt:writea.loop       b/ all is wonderful, output the next
          bcs     vt:writeaerred       b/ oops, something went wrong!
          page
;   An optimization here is to block transfer chunks of text to
;   the output ring buffer, instead of moving them one at a time.
;   This is accomplished by computing
;   max(#printingcharsinWrBufbeforeControlChar,RingSpace,colct,fieldend)
;   and block-moving that many characters to output ring.
;   savings would be about 10% of CPU at 9600 baud
;   Min sequence testing is performed in order from probable smallest...
;   values to probable largest values
          if      optimizedwritea
vt:writea.optimize ; try to write a chunk of characters into buffer
          lda     [dcb:scb+scblk:wrbuf,x] a control character ?
          if      m6800!m6801
          ldx     dcbpointer           restore X
          fin
          cmpa    #ascii:space         ...?
          blo     vt:writea.onechar    b/ yes, do it the hard way
;         assert: we are not in INPUT mode, no fields are present,
;         and echo is enabled!
;         ldb     dcb:welflags,x       are we doing input or fields ?
;         bne     vt:writea.onechar    b/ yes, do it one at a time
          ldaa    dcb:displaywidth,x   compute number of columns...
          suba    dcb:col,x            required before line wrap needed
          blo     vt:writea.onechar    b/ line wrap already needed
;         if things go right, dcb:col can never get to dcb:displaywidth
          ldb     dcb:colct,x          compute number of columns left
          comb                         before we get to 255
          cba                          choose min (a,b), place in (b)
          bhs     vt:writea3.f1
          tfr     a,b
vt:writea3.f1
          ; since only task level fiddles with dcb:ringoutroom,
          ; there can be no timing splinters when inspecting both halves
          tfr     b,a                  is there more ring left than (b)?
          adda    dcb:ringoutroom+1,x  (b)-ring room left
          bcc     vt:writea3.f2        b/ ring room left > (b)
          lda     dcb:ringoutroom,x    ...?
          inca                         >= 256 bytes of ring left ?
          bne     vt:writea3.f2        b/ yes, more than (b)
          clrb                         use -ringoutroom as block xfer max
          subb    dcb:ringoutroom+1,x  use this as block transfer max
          beq     vt:writea.onechar    b/ memory-mapped device
          page
vt:writea3.f2
          ; since only task level fiddles with dcb:tlroom,
          ; there can be no timing splinters when inspecting both halves
          cmpb    dcb:tlroom+1,x       how much output space left ?
          bls     vt:writea3.f2a       b/ more than (b)
          lda     dcb:tlroom,x         ...?
          bne     vt:writea3.f2a       b/ more than (b)
          ldb     dcb:tlroom+1,x       use this as block transfer max
vt:writea3.f2a
          cmpb    dcb:scb+scblk:wrlen+1,x choose min(B,wrlen)
          bls     vt:writea3.f3        b/ (B) is least
          lda     dcb:scb+scblk:wrlen,x ...?
          bne     vt:writea3.f3        b/ (B) is least
          ldb     dcb:scb+scblk:wrlen+1,x use this as block transfer max
          ; note: (B) can be zero here!
vt:writea3.f3 ; now (B) holds maximum number of characters safe to move
          pshb                         save potential # of bytes to block move
          page
          if      m6800!m6801
          ; now scan until limit to locate possible control characters
          ldx     dcb:scb+scblk:wrbuf,x fetch source of bytes
          incb                         to offset decb that starts loop below
vt:writea3.scanforcontrolchars ; now do expensive scan for control characters
          decb                         = bytes left to scan
          beq     vt:writea3.f4        b/ scanned all bytes
          lda     ,x                   fetch source byte
          bmi     vt:writea3.f4        b/ sign bit set, stop here!
          inx                          advance pointer
          cmpa    #ascii:rubout        this is technically a control character
          beq     vt:writea3.f4        b/ rubout, end of printables
          cmpa    #ascii:space         a control character found ?
          bhs     vt:writea3.scanforcontrolchars b/ no, loop til limit or ctl
vt:writea3.f4
          ldx     dcbpointer           restore X in case we take BEQ
          pula                         = number we thought we could handle
          sba                          number up to a control character
          beq     vt:writea.onechar    b/ 1st character is control character
                                       ; or there is no buffer space left!
          psha                         save number of chars to process
          tfr     a,b                  do the block move
          clra                         form 16 bit count
          ldx     dcb:ringoutstore,x   where to
          stx     tempx
          ldx     dcbpointer
          ldx     dcb:scb+scblk:wrbuf,x where from
          jsr     sdos+sdos:blockmove  go move characters!
          ldd     tempx                fetch next output ring location
          stx     tempx                save updated source pointer
          ldx     dcbpointer           now update all the pointers, counters
          std     dcb:ringoutstore,x   update the output ring pointer
          ldd     tempx                fetch updated source pointer
          std     dcb:scb+scblk:wrbuf,x and update dcb properly
          else    (m6809)
          ; scan until limit or control character, copying to output ring
          ldu     dcb:ringoutstore,x   where to store printing characters
          ldy     dcb:scb+scblk:wrbuf,x fetch source of bytes
          incb                         to offset decb that starts loop below
vt:writea3.scanforcontrolchars ; now do expensive scan for control characters
          decb                         = bytes left to scan
          beq     vt:writea3.f4        b/ scanned all bytes
          lda     ,y+                  fetch source byte
          bmi     vt:writea3.f4c       b/ sign bit set, stop here!
          cmpa    #ascii:space         a control character found ?
          blo     vt:writea3.f4c       b/ yes, stop copying at this character
          sta     ,u+                  store character into output ring
          cmpa    #ascii:rubout        this is technically a control character
          bne     vt:writea3.scanforcontrolchars b/ not rubout, go try another
          leau    -1,u                 undo overshoot on destination (rarely!)
vt:writea3.f4c ; (Y) has been advance past character to reject
          leay    -1,y                 back up so (Y) points to char to reject
vt:writea3.f4
;         ldx     dcbpointer
          negb                         determine actual count transferred
          addb    ,s+                  number up to a control character
          beq     vt:writea.onechar    b/ 1st character is control character
                                       ; or there is no buffer space left!
          pshb                         save number of chars moved to ring
          sty     dcb:scb+scblk:wrbuf,x now update all the pointers, counters
          stu     dcb:ringoutstore,x
          fin
          pula                         = number we moved
          psha                         STARTIO destroys TEMPB, so save on stk
          nega                         subtract transferred count...
          tfr     a,b                  by adding negative of count
          if      m6800!m6801
          sei                          prevent interrupt splinters
          else    (m6809)
          orcc    #%01010000           prevent interrupt splinters
          fin
          adda    dcb:ildata+1,x       tell interrupt level that data is ready
          staa    dcb:ildata+1,x
          bcs     vt:writea3.f4a       carry cancels "add #$ff" to upper half
          dec     dcb:ildata,x
vt:writea3.f4a
          addb    dcb:tlroom+1,x       must adjust with ints disabled...
          stab    dcb:tlroom+1,x       because interrupt service bumps this
          bcs     vt:writea3.f4b       carry cancels "add #$ff" to upper half
          dec     dcb:tlroom,x         "add #$ff" to upper half, lo frequency
vt:writea3.f4b
          if      m6800!m6801
          cli
          else    (m6809)
          andcc   #\%01010000          allow interrupts again
          fin
          ldaa    dcb:oilquiescent,x   output routines idle ?
          beq     vt:writea3.f0
          ldd     dcbpointer           yes, start them up!
          ldx     #vt:iloutputwasquiescent
          jsr     sdos+sdos:startio
          ldx     dcbpointer           restore X for use by rest of code
vt:writea3.f0
          page
          pula                         = number we moved
          staa    tempb                save it, we will use several times
          ldab    dcb:scb+scblk:wrlen+1,x adjust remaining count
          subb    tempb                subtract number we moved from WRLEN
          stb     dcb:scb+scblk:wrlen+1,x
          bcc     vt:writea3.7         b/ no borrow
          dec     dcb:scb+scblk:wrlen,x propogate borrow
vt:writea3.7
          ldab    dcb:ringoutroom+1,x  decrease number of bytes to buffer top
          addb    tempb
          bcc     vt:writea3.f6        b/ haven't hit top of ring buffer yet
          inc     dcb:ringoutroom,x
          bne     vt:writea3.f6        b/ haven't hit top of ring buffer yet
          ; ran off end of ring, put pointers back to front of ring
          ldab    dcb:ringoutbase,x
          stab    dcb:ringoutstore,x
          ldab    dcb:ringoutbase+1,x
          stab    dcb:ringoutstore+1,x
          ldab    dcb:ringoutlen,x
          stab    dcb:ringoutroom,x
          ldab    dcb:ringoutlen+1,x
vt:writea3.f6
          stab    dcb:ringoutroom+1,x
          ldab    dcb:colct,x          update colct by number of bytes moved
          addb    tempb
          stab    dcb:colct,x
          adda    dcb:col,x            update physical column by # bytes moved
          staa    dcb:col,x
          jmp     vt:writea.loop       go try to move another big chunk
;  there are $31 instructions (single stepping) on a 6800 to output a char
;  one byte at a time, for about 250us/byte.  An estimate of 110 instructions
;  for the optimized sequence above, plus 50 cycles/byte
;  shows that breakeven point is 2 or 3 characters.  At 10 characters/chunk,
;  the optimized routine should save 2500-550-500=1500 us.
          fin     optimizedwritea      end proposed code
          page
vt:nexttabstop

;   return in (a) the logical column number of the next tab stop
;   the cursor is always spaced at least one position

          ldab    dcb:colct,x
          incb
vt:nexttabstop1
          ldaa    dcb:tabs,x
          beq     vt:nexttabstop2
          inx
          cba
          blo     vt:nexttabstop1
          ldx     dcbpointer
          rts

vt:nexttabstop2
          ldx     dcbpointer
          ldaa    dcb:colct,x
          inca
          rts
          page
vt:logchr

;   display an ASCII character: adjust DCB:COLCT and display the character
;   if within field bounds; control characters are displayed as the letter
;   equivalent of the code, preceeded by a ^ (e.g., control-c is displayed
;   as ^C); ^E, ^G, ^H, ^I, ^J, ^L, and ^M receive special attention

        cmpa    #ascii:space
        blo     vt:logchr.1            b/ control character
        cmpa    #ascii:rubout          only unprintable code above $20 ?
        beq     vt:logchr.1b           b/ yes, so don't output it

;   we have a displayable character which is not RUB (assert!)

        ldb     dcb:welflags,x
        bitb    #dcbwelflags:flde!dcbwelflags:retype!dcbwelflags:echo
        beq     vt:logchr.bumpcolct    b/ fields not current, echo enabled
        bitb    #dcbwelflags:retype!dcbwelflags:echo
        bne     vt:logchr.1b           b/ no echoing of some sort
        ldb     dcb:colct,x
        cmpb    dcb:fieldend,x
        blo     vt:logchr.bumpcolct    b/ within field
vt:logchr.1b
        okrts

vt:logchr.bumpcolct ; output a normal character
        inc     dcb:colct,x            bump logical column count
        lbne    vt:cursorbump          no overflow, take hi-freq, fast path!
        dec     dcb:colct,x            colct must never wrap to zero!!!
        jmp     vt:cursorbump          go adjust cursor position
        page
vt:logchr.1

;   we have a control character; first see if it is one which needs special
;   attention.  Do these tests in order of frequency of occurence of character.
;   A frequency test is sufficient, as only ^M is common enough to require
;   fast service.
;   Note that we did not have to check echo conditions.  This is because
;   if any control character handler routine outputs a printable character,
;   it will do so via VT:LOGCHR, which will check the echo conditions!

        cmpa    #ascii:cr              ^M
        beq     vt:logcr
        cmpa    #ascii:ht              ^I
        beq     vt:loght
        cmpa    #ascii:bs              ^H
        beq     vt:logbs
        cmpa    #ascii:ff              ^L
        lbeq    vt:logff
        cmpa    #ascii:bel             ^G
        beq     vt:logbel
        cmpa    #ascii:enq             ^E
        lbeq    vt:logenq
        tsta                           ^@
        beq     vt:lognul
        cmpa    #ascii:lf              ^J
        beq     vt:loglf
vt:logctl

;   we have a boring control character: prefix the letter equivalent with a ^
;   and put out the pair of characters

        psha
        lda     #'^
        bsr     vt:logchr
        lbcs    vt:err1s               b/ error, must pop byte from stack
        pula
        oraa    #'@
        bra     vt:logchr
        page
vt:logbs

;   ^H was encountered: back up the column count, unless it's zero, and output
;   the backspace

        ldb     dcb:colct,x
        beq     vt:logbs.1a            b/ at col zero, supress backspace!
        dec     dcb:colct,x
vt:logbs.1
        ldb     dcb:welflags,x
        bitb    #dcbwelflags:flde!dcbwelflags:retype!dcbwelflags:echo
        lbeq    vt:cursorback
        bitb    #dcbwelflags:retype!dcbwelflags:echo
        bne     vt:logbs.1a            b/ no echoing of some sort
        ldb     dcb:colct,x
        incb
        cmpb    dcb:fieldend,x
        lbls    vt:cursorback          b/ still within field
vt:logbs.1a
        okrts

vt:logbel

;   ^G was encountered: output the bell

        jsr     vt:tlstore
;       okrts

vt:lognul
vt:loglf

;   ^@ or ^J was encountered: 'eat' the null or line feed

        okrts
        page
vt:loght

;   ^I was encountered: space to the next tab stop

        jsr     vt:nexttabstop
        suba    dcb:colct,x
        tfr     a,b
        jsr     vt:multispace
        okrts
        page
vt:logcr

;   ^M was encountered: output <CR>

        ldaa    #dcbeditflags:page
        bita    dcb:editflags,x
        beq     vt:logcr.1
        dec     dcb:rowct,x
        bne     vt:logcr.1             b/ not time to do a page break
        jsr     vt:pagebreakifcrt
vt:logcr.1
        clr     dcb:colct,x
        jsr     dcb:profile+dpb:newline,x
        ldaa    dcb:rowct,x
        bne     vt:logcr.3
        jsr     vt:pagebreakifhardcopy
vt:logcr.3
;       jmp     vt:cursornewline
          page
vt:cursornewline ; cursor is now at the beginning of the next line
          clr     dcb:col,x
          inc     dcb:row,x
          ldaa    dcb:row,x
          cmpa    dcb:displaydepth,x
          bne     vt:cursornewline.1
          dec     dcb:row,x            adjust row number to bottom of screen
          ldaa    dcb:profile+dpb:dvtyp,x get type of device
          ldab    dcb:cursorlost,x     do we really know where cursor is ?
          beq     vt:cursornewline.2   b/ yes
          cmpa    #dvtyp.printer       is device a printer ?
          bne     vt:cursornewline.1   b/ yes, we are still lost!
          dec     dcb:cursorlost,x     no, leave row # at bottom
vt:cursornewline.1
          okrts

vt:cursornewline.2 ; since we know where cursor is...
          cmpa    #dvtyp.printer       is device a printer ?
          bne     vt:cursornewline.1   b/ CRT, cursor must be on bottom row
          clr     dcb:row,x            yes, set row number to top of page
          okrts

vt:newlinecrlf ; entry point from DPB:NEWLINE
          ldaa    #ascii:cr            default is <cr><lf>
          bsr     vt:tlstorej1
          ldaa    #ascii:lf
vt:tlstorej1
          jmp     vt:tlstore
          page
vt:logff

;   ^L was encountered: literally output "^L<CR>" if the display depth is zero;
;   otherwise, clear the entire display, and leave the cursor at position (0,0)

        clr     dcb:colct,x
        lda     dcb:displaydepth,x
        beq     vt:logff.1             b/ continuous form paper
        bsr     vt:pagebreakifcrt
        jsr     dcb:profile+dpb:newpage,x
        ldaa    dcb:background,x       set up background after ClearScreen
        jsr     dcb:profile+dpb:setbackground,x
        bsr     vt:pagebreakifhardcopy
        jmp     vt:cursorclear

vt:logff.1

;   output "^L<CR>", since continuous form paper is being used

        jsr     vt:cursorclear
        lda     #ascii:ff
        bsr     vt:logctlj1
        bsr     vt:pagebreakifhardcopy  CRT with depth=0 gets no sympathy
        bra     vt:logcr

vt:newpageff ; entry point for DPB:NEWPAGE for nice devices
        lda     #ascii:ff              default is <ff>
        jmp     vt:tlstore

vt:logctlj1
        jmp     vt:logctl
        page
vt:logenq

;   ^E was encountered: output ^E if hardcopy edit is current; otherwise,
;   erase the display from the cursor to the end of the display line, and
;   leave the cursor where it was found

        ldb     #dcbeditflags:hcedit
        bitb    dcb:editflags,x
        bne     vt:logctlj1            b/ hardcopy edit-- put out ^E
        jmp     dcb:profile+dpb:erasetoeol,x

vt:simeeol ; entry point for DPB:ERASETOEOL for devices that can't do EEOL
;       Simulates Erase-To-End-Of-Line by outputting blanks to right margin,
;       and then backspacing to current column.
        lda     dcb:displaywidth,x
        suba    dcb:col,x              (use physical, not logical column)
        psha                           save for use as a backspace count
        beq     vt:logenq.3            b/ forward spacing is complete
vt:logenq.2
        psha
        lda     #ascii:space
        jsr     vt:tlstore
        lbcs    vt:err2s               b/ error, must pop 2 bytes from stack
        pula
        deca
        bne     vt:logenq.2
vt:logenq.3
        pula
        tsta
        beq     vt:logenq.1            b/ backspacing is complete
        deca
        psha
        lda     #ascii:bs
        jsr     vt:tlstore
        bcc     vt:logenq.3
vt:err1s3j
        jmp     vt:err1s               b/ error, must pop 1 byte from stack
        page
vt:pagebreakifhardcopy ; do page break if this is hardcopy-class device
          lda     dcb:editflags,x
          bita    #dcbeditflags:page
          beq     vt:pagebreak.no         b/ not doing paging
          bita    #dcbeditflags:hcedit    hardcopy device ?
          bne     vt:pagebreak            b/ yes, do page break
vt:pagebreak.no
vt:logenq.1
          okrts

vt:pagebreakifcrt ; do page break if this is CRT-class device
          lda     dcb:editflags,x
          bita    #dcbeditflags:page
          beq     vt:pagebreak.no         b/ not doing paging
          bita    #dcbeditflags:hcedit    hardcopy device ?
          bne     vt:pagebreak.no         b/ yes, don't do page break here
;         bra     vt:pagebreak

vt:pagebreak

;   post a page break (^P) and wait for it to terminate; page breaks do not
;   occur during read binary mode

          bita    #dcbeditflags:readb
          beq     vt:pagebreak1
          ldaa    dcb:displaydepth,x
          staa    dcb:rowct,x
          okrts

vt:pagebreak1
          ldaa    #'^
          ldab    #'P
          bsr     vt:postreminder
          ldaa    #dcbreminders:ctlp
          oraa    dcb:reminders,x
          staa    dcb:reminders,x
          clr     dcb:rowct,x
          bsr     vt:waitfreeze
          jmp     vt:clearreminder
          page
vt:postreminder

;   (a,b) contain the two byte reminder to be displayed in the lower right
;   corner of the display

;   reminders are not posted for output-only devices

          pshb
          ldab    #dpbflags:output
          bitb    dcb:profile+dpb:flags,x
          bne     vt:postreminder5
          ldab    #dcbeditflags:hcedit
          bitb    dcb:editflags,x
          beq     vt:postreminder3
vt:postreminder5
          pulb
vt:postreminder4
          okrts

vt:postreminder2
          ldb     dcb:colct,x                    prevent colct from wrapping
          cmpb    #254                           when ^S typed at col 254
          pulb
          bhs     vt:postreminder4               b/ would wrap, don't print ^S
          bsr     vt:logab
          ldaa    #ascii:bs              backspace twice
          tfr     a,b
;         bra     vt:logab

vt:logab ; log (A) and then log (B)
          pshb
          jsr     vt:logchr
          bcs     vt:err1s3j              error, must pop 1 byte from stack
          pula
          jmp     vt:logchr
          page
vt:postreminder3
          ldb     dcb:cursorlost,x
          bne     vt:postreminder2
          ldb     dcb:displaydepth,x
          beq     vt:postreminder2
          psha
          ldaa    dcb:row,x
          ldab    dcb:col,x
          pshd
          ldaa    dcb:displaydepth,x
          deca
          ldab    dcb:displaywidth,x
          subb    #3
          jsr     vt:position1
          bcs     vt:err4sj            b/ error, must pop 4 bytes from stack
          ldd     2,s
          ldx     dcbpointer
          bsr     vt:logab
          bcs     vt:err4sj            b/ error, must pop 4 bytes from stack
          puld
          leas    2,s
          jmp     vt:position1

vt:err4sj ; must pop 4 bytes from stack before propogating error
          jmp     vt:err4s
          page
vt:waitfreeze

;   wait for ^S or ^P conditions to be satisfied, then resume output
;   called only after posting ^S or ^P

          ldd     #vt:waitfreeze1
          jsr     sdos+sdos:waitcond
          ldx     dcbpointer
          jsr     vt:servicelineflags
          jsr     vt:bck
          bsr     vt:postctlc
          ldb     dcb:reminders,x                 freeze still in effect ?
          andb    #dcbreminders:ctls!dcbreminders:ctlp
          beq     vt:waitfreezerts                b/ nope
          ldb     dcb:rowct,x                     yes, unfreeze ?
          beq     vt:waitfreeze                   b/ nope
vt:waitfreezerts
          okrts

vt:waitfreeze1
          lda     dcb:iillflgs,x
          ora     dcb:iilspl,x
          ora     dcb:beepcount,x
          ora     dcb:ctlccount,x
          ora     dcb:rowct,x
          rts
          page
vt:postctlc

;   for each ^C requested, clear the typeahead buffer (done by vt:ilasetx),
;   clear reminders, turn off page mode,
;   clear the lineflags in the dcb and the userspace (if SDOS/MT
;   is not running), and write out the ^C.

          lda     dcb:ctlccount,x
          beq     vt:postctlc3.b                b/ nothing to post
          ldaa    dcb:displaydepth,x
          bne     vt:postctlc1
          inca
vt:postctlc1
          staa    dcb:rowct,x
          jsr     vt:clearreminder
          ldaa    #\dcbeditflags:page
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          clr     dcb:lineflags,x
          lda     vt:sdosmtflag
          beq     vt:postctlc3                  b/ SD/MT running
          clr     lineflags
vt:postctlc3
          ldaa    dcb:colct,x          prevent colct wrap (otherwise ^C at
          cmpa    #254                 col 254 leaves colct at zero,...
          bhs     vt:postctlc3.a       with disasterous results when ^X input)
          ldd     #'^##8+'C            "^C" text
          jsr     vt:logab
          jsr     vt:hctest            backspace over ^C unless hard copy
          bcs     vt:postctlc3.a
          ldb     #2
          jsr     vt:multibs
vt:postctlc3.a
          dec     dcb:ctlccount,x
vt:postctlc3.b
          okrts
          page
vt:clearreminder

;   clear any reminders, but only if one is displayed.  if a reminder
;   is cleared, it's flag is reset

          ldaa    #dcbreminders:ctls+dcbreminders:ctlp+dcbreminders:ctlo
          bita    dcb:reminders,x
          beq     vt:clearreminder1
          ldaa    #ascii:space
          tfr     a,b
          jsr     vt:postreminder
          ldaa    dcb:reminders,x
          anda    #\(dcbreminders:ctls+dcbreminders:ctlp+dcbreminders:ctlo)
          staa    dcb:reminders,x
vt:clearreminder1
          okrts

vt:cursorclear

;   cursor is now at (0,0) and is not lost

;   if field mode, set (row,col) to (readrow,readcol)

          clra
          clrb
;         bsr     vt:cursorposn
;         okrts

vt:cursorposn

;   cursor is now at (a,b) and is not lost

          staa    dcb:row,x
          stab    dcb:col,x
          clr     dcb:cursorlost,x
          okrts
          page
vt:cursorback

;   cursor is now one position backward; if last position was begin of line,
;   and not lost, wrap to end of previous line, unless at (0,0); if lost,
;   do not wrap

;   note that some strange things may happen if cursor is lost and
;   display will hardware wrap backwards

;   don't backspace hardcopy

          ldb     dcb:displaydepth,x
          beq     vt:cursorback5       b/ hard copy
          ldb     dcb:col,x
          beq     vt:cursorback1
          dec     dcb:col,x
          jsr     vt:tlstore
          okrts

vt:cursorback1
          ldab    dcb:editflags,x
          bitb    #dcbeditflags:wrap
          beq     vt:cursorback5
          ldb     dcb:cursorlost,x
          beq     vt:cursorback3
vt:cursorback5
          okrts
          page
vt:cursorback3
          ldb     dcb:row,x
          beq     vt:cursorback5
          dec     dcb:row,x
          ldab    dcb:displaywidth,x
          decb
          stab    dcb:col,x
          ldaa    dcb:row,x
          pshd
          jsr     dcb:profile+dpb:position,x
          lbcs    vt:err2s             b/ error, clean off the stack
          leas    2,s
          rts                          assert: C=0

vt:nopositioning ; remember that the cursor is lost and display '@@'
          ldaa    dcb:displaydepth,x
          staa    dcb:cursorlost,x
          ldd     #'@##8+'@
          jmp     vt:logab
          page
vt:cursorbump

;   cursor is now one position forward; if last position was end of line,
;   and not lost, wrap to begin of next line; if lost, do not wrap

;   note that some strange things may happen if cursor is lost and
;   display will hardware wrap forward

;   if device has auto newline feature, then the display width must be one less
;   than the actual display width, which is indicated by this bit in the dpb

;   note also that display width should reflect number of positions that
;   can be written into without causing cursor wrapping

;   if page mode, truncate--do not wrap

          inc     dcb:col,x                     assert: dcb:col < 255 !
          ldab    dcb:col,x
          cmpb    dcb:displaywidth,x
          blo     vt:tlstore                    hi freq path, make it fast!
          ldab    dcb:editflags,x
          bitb    #dcbeditflags:page
          bne     vt:cursorbump3
          bitb    #dcbeditflags:wrap
          bne     vt:cursorbump2
vt:cursorbump3
          dec     dcb:col,x
          okrts

vt:cursorbump2
          bsr     vt:tlstore
          lda     dcb:colct,x
          psha
          ldaa    #ascii:cr
          jsr     vt:logchr
          lbcs    vt:err1s             b/ error, must pop 1 byte from stack
          pula
          sta     dcb:colct,x
          okrts
          page
vt:fetchwrbuf

;   Return in (a) the next byte from the caller's WRBUF; Z bit is reset
;   if data is available, set if unavailable (WRLEN exhausted).  Local
;   WRBUF and WRLEN are adjusted as appropriate.

          ldb     dcb:scb+scblk:wrlen+1,x
          bne     vt:fetchwrbuf1                b/ some bytes left to fetch
          lda     dcb:scb+scblk:wrlen,x
          beq     vt:fetchwrbufempty            b/ wrbuf is empty, Z bit set
          dec     dcb:scb+scblk:wrlen,x
vt:fetchwrbuf1
          dec     dcb:scb+scblk:wrlen+1,x
          lda     [dcb:scb+scblk:wrbuf,x]
          if      m6800!m6801
          ldx     dcbpointer
          fin
          incd    dcb:scb+scblk:wrbuf,x        leaving Z bit reset
vt:fetchwrbufempty
          rts
          page
vt:tlwaitroom
          psha
          ldd     #vt:tlwaitroom1
          jsr     sdos+sdos:waitcond   assert: cannot take error exit
          ldx     dcbpointer
          jsr     vt:servicelineflags
          pula
          jsr     vt:checkto           check for device timed out
*         bra     vt:tlstore

vt:tlstore

;   store (a) in the output buffer; block the edit task on room available, if
;   there is no room; if the output interrupt level is quiescent, trigger
;   the output interrupt level.
;   returns with carry reset (see optimized path through vt:cursorbump)

          jsr     dcb:tlputbuf,x
          beq     vt:tlwaitroom        b/ no room, must wait!
          ldaa    dcb:oilquiescent,x
          beq     vt:tlstore1          b/ interrupt expected, we're done!

;   note: it is NOT reasonable to do direct I/O when dcb:oilquiescent because
;   then one would have to check dcb:oilquiescent both before and after the
;   call to tlputbuf, thus raising the overhead of inserting a character
;   into the buffer, which is the common case, and must be as fast as possible!
;   Also, we are not sure device is actually ready to take a character
;   (go see how vt:iloutputwasquiescent handles dcb:ilenboi says "not ready")

          ldd     dcbpointer           pass dcb address to startio routine
          ldx     #vt:iloutputwasquiescent
          jsr     sdos+sdos:startio
          ldx     dcbpointer
vt:tlstore1
          okrts
          page
vt:tlwaitroom1

;   "hang out" until either an output timeout has occurred, or space is
;   now available in the output buffer

          lda     dcb:tlroom+1,x       see if output threshold space exceeded
          suba    dcb:ringoutthreshold,x (delta must be >0 to wake task)
          bhi     vt:tlwaitroomrts     b/ yes, its worth waking the edit task
          ldaa    #dcbilsw:outto
          anda    dcb:ilsw,x           watch for timed out on output
          oraa    dcb:tlroom,x         watch for lotsa buffer space
          oraa    dcb:iillflgs,x       watch for line flags
vt:tlwaitroomrts
          rts
          page
vt:writeb ; almost the same as writea

;         ldx     dcbpointer
          jsr     vt:checkto           make sure no timeout is lying in wait
          clr     dcb:colct,x          zero the column count on SYSCALL:WRITEB
          ldaa    dcb:displaydepth,x
          staa    dcb:cursorlost,x
          ldaa    #dcbreminders:ctlo
          bita    dcb:reminders,x
          bne     vt:writeb2
vt:writeb3
          lda     dcb:ctlccount,x
          beq     vt:writeb4
          jsr     vt:postctlc
vt:writeb2
          okrts

vt:writeb4
          bsr     vt:fetchwrbuf
          beq     vt:writeb2
          bsr     vt:tlstore
          bra     vt:writeb3
          page
vt:position ; position cursor

;   if page mode, cause page break after positioning cursor

;   illegal to position device marked hard copy

;         ldx     dcbpointer
          jsr     vt:testhardcopy
          ldx     dcb:callerscb,x
          ldx     scblk:wrbuf,x
vt:impliedposition

;   Come here to service implied positioning of reada and writea.
;   Vt:testhardcopy has already been called in reada and writea code.

          ldd     0,x
          bned    vt:illdevop
          ldd     2,x
          ldx     dcbpointer
          cmpa    dcb:displaydepth,x
          bhs     vt:illdevop
          cmpb    dcb:displaywidth,x
          bhs     vt:illdevop
          bsr     vt:position1
          ldaa    #dcbeditflags:page
          bita    dcb:editflags,x
          beq     vt:position2
          jsr     vt:pagebreak
vt:position2
          okrts
          page
vt:position1
          pshd
          ldd     dcb:coloring,x
          ldaa    dcb:background,x
          jsr     dcb:profile+dpb:setcoloring,x
          bcs     vt:err2sj3           b/ error, must pop 2 bytes from stack
          jsr     dcb:profile+dpb:position,x
          bcs     vt:err2sj3           b/ error, must pop 2 bytes from stack
          ldd     dcb:coloring,x
          jsr     dcb:profile+dpb:setcoloring,x
          bcs     vt:err2sj3           b/ error, must pop 2 bytes from stack
          puld
          stb     dcb:colct,x
;         jsr     vt:cursorposn
;         okrts
          jmp     vt:cursorposn

vt:err2sj3 ; error, must pop 2 bytes from stack before propogating error
          jmp     vt:err2s

vt:testhardcopy ; complain if hardcopy device
          ldb     dcb:editflags,x
          bitb    #dcbeditflags:hcedit
          bne     vt:illdevop
          okrts

vt:testoutput ; complaind if output-only device
;   reada/readb not permitted to devices designated as output only
          ldaa    dcb:profile+dpb:flags,x
          bita    #dpbflags:output
          bne     vt:illdevop
          okrts

vt:illdevop
          jsr     sdos+sdos:error
          #err:illdeviceop
          page
vt:writebnowait ; variation on writeb
;   RDBUF flagged when WRLEN exhausted

;         ldx     dcbpointer
          jsr     vt:writeb
          bcc     vt:writeanowait1
          bra     vt:writeanowait2

vt:writeanowait ; variation on writea
;   RDBUF flagged when WRLEN exhausted

;         ldx     dcbpointer
          jsr     vt:writea
          bcc     vt:writeanowait1
vt:writeanowait2 ; save error code in (X) in reply buffer
          txd
          ldx     dcbpointer
          ldx     dcb:scb+scblk:rdbuf,x
          std     1,x
vt:writeanowait1
          ldx     dcbpointer
          inc     dcb:doneflag,x
          inc     [dcb:scb+scblk:rdbuf,x]
          leas    2,s                  don't do RTS to get back...
          jmp     vt:edittask          because SCBLK no longer exists!
          page
vt:setactblock ; set a non-standard activation set
;  which is a 128 bit table expressed as 16 bytes

;         ldx     dcbpointer
          if      m6809
          leay    dcb:activation,x
          else
          ldd     #dcb:activation
          addd    dcbpointer
          std     tempx
          fin
          ldx     dcb:scb+scblk:wrbuf,x
          ldd     #16
          jmp     vt:blockmove
          page
vt:setfieldsize ; set the input field size for a subsequent reada or cc:activationck

;         ldx     dcbpointer
          jsr     vt:testhardcopy
          ldx     dcb:callerscb,x
          lda     [scblk:wrbuf,x]
          beq     vt:errbadfieldj
          ldx     dcbpointer
          cmpa    dcb:displaywidth,x   complain right away if field rediculous
          bhs     vt:errbadfieldj
          staa    dcb:fieldwidth,x
          lda     #dcbwelflags:fldw    note that field width is valid
          oraa    dcb:welflags,x
          sta     dcb:welflags,x
          okrts

vt:errbadfieldj
          jmp     vt:errbadfield
          page
vt:illdevopj2
          jmp     vt:illdevop

vt:writeeditline

;   Data from WRBUF is copied to the end of the input line buffer until
;   WRLEN is exhausted or the line buffer is full or an activation is
;   encountered.  When a subsequent read or cc:activationck is issued,
;   the buffer is echoed and editing by the typist may proceed, unless
;   an activation character was encountered in WRBUF, in which case
;   the input operation is satisfied.

;   An optional extension to the syscall block indicates where the cursor
;   is to be left when the line is echoed at input time.  The extension
;   is illegal if an activation character is in WRBUF.

;         ldx     dcbpointer
          jsr     vt:testhardcopy      complain if hardcopy only...
          jsr     vt:testoutput        or if output-only device
          ldaa    #dcbeditflags:activate
          bita    dcb:editflags,x
          bne     vt:illdevopj2              B/ buffer already has activation
          lda     dcb:linebufcount,x
          bne     vt:writeeditline5          B/ data already in buffer
          ldd     dcb:linebuf,x
          std     dcb:linebufptr,x
vt:writeeditline5
          ldab    dcb:linebuflen,x
          subb    dcb:linebufcount,x
          clra
          subd    dcb:scb+scblk:wrlen,x
          bcs     vt:illdevopj2              B/ WRBUF too big
          page
          clra
          ldab    dcb:linebufcount,x
          stab    tempx+2
          addd    dcb:linebuf,x
          std     tempx+4
vt:writeeditline2
          jsr     vt:fetchwrbuf
          beq     vt:writeeditline3
          ldx     tempx+4
          sta     ,x+
          stx     tempx+4
          ldx     dcbpointer
          inc     tempx+2
          jsr     vt:checkactivation
          bcc     vt:writeeditline2          b/ activation not found yet
          page
vt:writeeditline6 ; come here with activation
          tst     dcb:scb+scblk:wrlen+1,x    ASSERT: WRLEN <= 255
                                             ;because we checked above to make
                                             ;sure it was smaller than linebuf
          bne     vt:illdevopj2              B/ data follows activation
          ldaa    dcb:scb+scblk:wlen,x       see if there's an extension
          cmpa    #scblk:end+1
          bhs     vt:illdevopj2              B/ can't posn after activation
          ldaa    #dcbeditflags:activate
          oraa    dcb:editflags,x
          staa    dcb:editflags,x
          jsr     vt:setnewstatus
          ldd     dcb:linebuf,x
          std     tempx+4
vt:writeeditline3 ; jump to here with no activation
          ldb     #255                 default cursor position
          ldaa    dcb:scb+scblk:wlen,x
          cmpa    #scblk:end+1
          blo     vt:writeeditline31
          ldb     dcb:scb+scblk:end,x  desired cursor position
vt:writeeditline31
          stb     dcb:welpos,x
          lda     dcb:welflags,x       note echoing, if current
          bita    #dcbwelflags:echo
          beq     vt:writeeditline32   b/ echoing
          oraa    #dcbwelflags:retype  note: no echoing
vt:writeeditline32
          oraa    #dcbwelflags:pref    note that buffer pre-filled
          sta     dcb:welflags,x
          ldd     tempx+4              this is new line buffer pointer
          std     dcb:linebufptr,x
          ldaa    tempx+2              this is new line buffer count
          staa    dcb:linebufcount,x
          okrts
          page
vt:setreadtimeout ; set the time period for a subsequent reada or cc:activationck

;         ldx     dcbpointer
          ldx     dcb:callerscb,x
          ldx     scblk:wrbuf,x
          clra
          ldab    vt:timeoutblocks
          addd    0,x
          ldx     dcbpointer
          std     dcb:readperiod,x
          okrts
          page
vt:activationck ; variation on reada
;   an err:illdeviceop error will be returned by vt:flb...
;   if a field was poorly defined

;   The following are all causes for input read termination:
;       activation received
;       ESC received (special activation)
;       edge of field overrun
;       input timeout
;       ^C^C
;       output timeout (shudder...)

;         ldx     dcbpointer
          jsr     vt:flbfer                     start input operation
          bcc     vt:activationck1
          txd
          ldx     dcbpointer                    save the error for reada
          std     dcb:readaerr,x
vt:activationck1
          okrts
          page
vt:echo ; enables echoing of typed input

;         ldx     dcbpointer
          ldaa    #\dcbwelflags:echo
          anda    dcb:welflags,x
          staa    dcb:welflags,x
          okrts

vt:noecho ; disables echoing of typed input

;         ldx     dcbpointer
          ldaa    #dcbwelflags:echo
          oraa    dcb:welflags,x
          staa    dcb:welflags,x
          okrts
          page
vt:wrap ; enables wrapping of typed input

;         ldx     dcbpointer
          ldaa    #dcbeditflags:wrap
          oraa    dcb:editflags,x
          staa    dcb:editflags,x
          okrts

vt:nowrap ; disables wrapping of typed input

;         ldx     dcbpointer
          ldaa    #\dcbeditflags:wrap
          anda    dcb:editflags,x
          staa    dcb:editflags,x
          okrts
          page
vt:idles

;   set the number of idles to follow a <CR><LF> sequence; a second data
;   byte of <CR> will cause <LF><CR> to preceed the idles specified

;         ldx     dcbpointer
          ldd     dcb:scb+scblk:wrlen,x
          tsta
          bne     vt:illdevopj1
          cmpb    #2
          blo     vt:idles3
          ldd     [dcb:scb+scblk:wrbuf,x]
          ldx     dcbpointer
          cmpb    #ascii:cr
          beq     vt:idles2
          cmpb    #ascii:lf
          bne     vt:illdevopj1
vt:idles2
          staa    dcb:idlecount,x
          stab    dcb:idletrigger,x
          okrts

vt:idles3
          lda     [dcb:scb+scblk:wrbuf,x]
          ldx     dcbpointer
          ldab    #ascii:lf
          bra     vt:idles2

vt:illdevopj1
          jmp     vt:illdevop
          page
vt:setexception ; Set exceptional processing flags
;   for the time being, there is only one flag for SEDIT.
;   Any other flags set will cause err:illdevop to be issued.

;         ldx     dcbpointer
          lda     [dcb:scb+scblk:wrbuf,x]
          ldx     dcbpointer
          tfr     a,b
          andb    #\dcbexcept:sedit
          bne     vt:illdevopj1        b/ he set other flags--too bad!!
          sta     dcb:except,x
          okrts

vt:setbaudrate ; set baud rate to contents of WRBUF

;         ldx     dcbpointer
          ldd     [dcb:scb+scblk:wrbuf,x]        fetch desired baud rate
          if      m6800!m6801
          ldx     dcbpointer
          sei
          else    (m6809)
          orcc    #%01010000                     lock out IRQ and FIRQ
          fin
          jsr     dcb:ilsetbaudrate,x            tell the hardware
          if      m6800!m6801
          cli
          else    (m6809)
          andcc   #\%01010000                    allow IRQ and FIRQ again
          fin
          bcs     vt:illdevopj1                  b/ can't change baud rate!
          ldd     [dcb:scb+scblk:wrbuf,x]        fetch desired baud rate
          if      m6800!m6801
          ldx     dcbpointer
          fin
          std     dcb:baudrate,x                 store for getbaudrate status
          okrts
          page
vt:tabs ; set tab column list, limit to 16 tab stops

;         ldx     dcbpointer
          tst     dcb:scb+scblk:wrlen,x
          bne     vt:illdevopj1
          ldab    dcb:scb+scblk:wrlen+1,x
          beq     vt:tabs0                b/ zero tab stops (is legal!)
          cmpb    #16                     16 tab stops, max
          bhi     vt:illdevopj1
          clra                            set "previous tab stop at col 0"
          ldx     dcb:scb+scblk:wrbuf,x
vt:tabs2 ; inspect tab stop list for monotonic increasing order
          cmpa    0,x
          bhs     vt:illdevopj1
          lda     ,x+
          decb
          bne     vt:tabs2
vt:tabs0 ; now copy tab stops to DCB, must handle zero tabs correctly
          if      m6800!m6801
          ldd     #dcb:tabs
          addd    dcbpointer
          std     tempx
          ldx     dcbpointer
          ldd     dcb:scb+scblk:wrlen,x
          ldx     dcb:scb+scblk:wrbuf,x
          jsr     sdos+sdos:blockmove
          clr     [tempx]              ! ASSERT ! tempx -> next sink byte
          else    (m6809)
          ldx     dcbpointer
          leay    dcb:tabs,x              destination
          ldd     dcb:scb+scblk:wrlen,x   count
          ldx     dcb:scb+scblk:wrbuf,x   source
          jsr     sdos+sdos:blockmove
          clr     ,y                      put zero marker at end of tab stops
          fin
          rts                   assert: C is reset because of CLR above
          page
vt:clrinput

;   clear the input (type-ahead) buffer

          ldd     dcbpointer           NOTE: this in not a LDX!
          ldx     #vt:clrinput1
          jsr     sdos+sdos:startio
          ldx     dcbpointer
          okrts

vt:clrinput1
          if      m6800!m6801
          pshd
          pulx
          else    (m6809)
          tdx
          fin
          jsr     vt:clearin
          jmp     sdos+sdos:rti

vt:clroutput

;   clear the output buffer

          ldx     sdos+sdos:configuration
          jsr     cnfg:intdisable,x
          ldx     dcbpointer
          jsr     vt:clearout
          ldx     sdos+sdos:configuration
          jsr     cnfg:intenable,x
          ldx     dcbpointer
          okrts
          page
vt:findprofile ; find profile block specified by profile number in (A)
; exit with (X) pointing to profile block, or issue error
;   signal 'Profile Not Found' if not found
          ldx     sdos+sdos:configuration  set up to search DPBs
          ldx     cnfg:vtprofiles,x
vt:findprofileloop
          cmpa    dpb:profilenumber,x  found desired DPB yet ?
          beq     vt:foundprofile      b/ yes
          ldx     dpb:next,x           no, inspect next profile block
          bne     vt:findprofileloop
          jsr     sdos+sdos:error
          #err:profilenotfound

vt:foundprofile ; (X) points to desired profile
          okrts
          page
vt:setprofile ; look up profile in list and install in dcb;
;   signal 'Profile Not Found' if none found
;         ldx     dcbpointer
          lda     [dcb:scb+scblk:wrbuf,x]  get DPB number
vt:setprofile.reset ; come here with (A)=DPB number from VT:RESET
          bsr     vt:findprofile
          if      m6809
          ldy     dcbpointer
          leay    dcb:profile,y
          else
          ldd     dcbpointer
          addd    #dcb:profile
          std     tempx
          fin
          ldd     #dpb:size
          jsr     sdos+sdos:blockmove
          ldx     dcbpointer
          ldaa    dcb:profile+dpb:defwidth,x
          ldab    dcb:profile+dpb:flags,x
          bitb    #dpbflags:autonl
          beq     vt:setprofile4
          deca
vt:setprofile4
          staa    dcb:displaywidth,x
          ldaa    dcb:profile+dpb:defdepth,x
          staa    dcb:displaydepth,x
          ldaa    dcb:editflags,x
          anda    #\(dcbeditflags:wrap+dcbeditflags:hcedit)
          bitb    #dpbflags:wrap
          beq     vt:setprofile3
          oraa    #dcbeditflags:wrap
vt:setprofile3
          bitb    #dpbflags:hcedit
          beq     vt:setprofile5
          oraa    #dcbeditflags:hcedit
vt:setprofile5
          staa    dcb:editflags,x
          ldb     #alterprofile:size   now erase the profile alteration
vt:setprofile6
          clr     dcb:posnsl,x
          inx
          decb
          bne    vt:setprofile6
          ldx    dcbpointer
          ldd    #0*256+ascii:lf       set default idle count and trigger
          jmp    vt:idles2
          page
vt:alterprofile ; alter the currently installed profile, if it is malleable
;  error "Profile Not Malleable", if not

          bsr     vt:checkmalleable    see if this operation is legal
          ldx     dcb:scb+scblk:wrbuf,x
          ldb     #4
          lda     alterprofile:cplen,x vt:malposition checks for < 3
          bsr     vt:rangecheck
          lda     alterprofile:cllen,x
          bsr     vt:rangecheck
          lda     alterprofile:eeollen,x
          bsr     vt:rangecheck
          ldb     #3
          lda     alterprofile:rowdisp,x
          bsr     vt:rangecheck
          lda     alterprofile:coldisp,x
          bsr     vt:rangecheck
          if      m6809
          ldu     dcbpointer
          leay    dcb:posnsl,u
          else    (m6800!m6801)
          ldd     #dcb:posnsl
          addd    dcbpointer
          std     tempx
          fin
;         assert: (X) = scblk:wrbuf
vt:blockmovealteration
          ldd     #alterprofile:size   get how much to move
vt:blockmove ; do an SDOS:BLOCKMOVE, and then do OKRTS
          jsr     sdos+sdos:blockmove
          ; can't do "jmp ...blockmove" because it doesn't zero carry on exit
          okrts
          page
vt:rangecheck

;   check  0 <= (a) <= (b); err:illdeviceop if not

*         cmpa    #0
*         lblo    vt:illdevop
          cba
          bls     vt:rangecheck.okrts
          jmp     vt:illdevop

vt:checkmalleable ; check to see if profile is malleable, error if not
;         ldx     dcbpointer
          ldaa    dcb:profile+dpb:flags,x
          bita    #dpbflags:mal
          beq     vt:checkmalleable1
vt:rangecheck.okrts
          okrts

vt:checkmalleable1
          jsr     sdos+sdos:error
          #err:profilenotmalleable

vt:getprofilealteration ; return Alterprofile data
          bsr     vt:checkmalleable
          if      m6800!m6801
          ldx     dcb:scb+scblk:rdbuf,x where to place results
          stx     tempx                tell block move
          ldd     dcbpointer           where to move result from
          addd    #dcb:posnsl
          pshd
          pulx
          else    (m6809)
          ldy     dcb:scb+scblk:rdbuf,x move data to user's reply buffer
          leax    dcb:posnsl,x         from alterprofile block in dcb
          fin
          bra     vt:blockmovealteration
          page
vt:setoutputtimeout ; set output time-out

;         ldx     dcbpointer
          clra                         fetch number of timeout blocks to (D)
          ldab    vt:timeoutblocks
          addd    [dcb:scb+scblk:wrbuf,x] combine with new timeout value
          if      m6800!m6801
          ldx     dcbpointer
          fin
          std     dcb:profile+dpb:outto,x and save in dcb
          okrts

vt:setparams ; set width and depth of display
;   note that zero depth => hardcopy

;         ldx     dcbpointer
          ldd     [dcb:scb+scblk:wrbuf,x] fetch width to (A), depth to (B)
          ldx     dcbpointer
          staa    dcb:displaywidth,x
          stab    dcb:displaydepth,x
          okrts
          page
vt:stoptimeshare ; shut of timesharing logic
          clr     vt:sdosmtflag        restore flag to original state
          inc     vt:sdosmtflag
          okrts

vt:gettimeshare ; error if SDOS/MT is already running
          lda     vt:sdosmtflag
          beq     vt:errmtalreadyrunning
          okrts

vt:errmtalreadyrunning
          jsr     sdos+sdos:error
          #err:sdosmtalreadyrunning

vt:errmtprimsmissing
          jsr     sdos+sdos:error
          #err:sdosmtprimsmissing

vt:errrdbuftoosmall
          jsr     sdos+sdos:error
          #err:rdbuftoosmall
          page
vt:settimeshare ; turn on timesharing, return /MT prims info to caller

          ldx     sdos+sdos:configuration
          ldx     cnfg:mtprims,x
          beq     vt:errmtprimsmissing

;   the primitives vector exists, see if it will fit in RDBUF

          stx     tempx+2
          ldd     0,x
          std     tempx
          ldx     dcbpointer
          ldd     dcb:scb+scblk:rdlen,x
          subd    tempx
          bcs     vt:errrdbuftoosmall

;   if the SDOS/MT flag is set, error; otherwise, set the flag and
;   return the SDOS/MT primitives vector in RDBUF

          lsr     vt:sdosmtflag
          bcc     vt:errmtalreadyrunning
          ldd     tempx
          if      m6809
          ldy     dcb:scb+scblk:rdbuf,x
          leax    -1,y                 CHEAT TO CONSTRUCT SDOS/MT POINTER
          else
          ldx     dcb:scb+scblk:rdbuf,x
          stx     tempx
          dex                             CHEAT TO CONSTRUCT SDOS/MT POINTER
          fin
          stx     vt:sdosmtstatuschange   remember where status change byte is!
          ldx     dcbpointer
          std     dcb:scb+scblk:rplen,x
          ldx     tempx+2
          leax    2,x                           bump past table size
          jmp     vt:blockmove
          page
vt:closecleanup ; last close on a device
;  moves to top-of-form if a printer, and then does a DUMPBUFFERS
;  This routine is invoked by automatically by CLOSE
;  It is usually a nop

;         ldx     dcbpointer
          ldaa    dcb:profile+dpb:flags,x is this an output only device ?
          bita    #dpbflags:output
          beq     vt:dumpbuffers2      b/ no, just dump the buffers
          lda     dcb:colct,x          at column zero ?
          beq     vt:dumpbuffers1      b/ yes, a complete line was output
          lda     #ascii:cr            no, finish the partial line
          jsr     vt:logchr
vt:dumpbuffers1
          lda     dcb:row,x            at top of form ?
          beq     vt:dumpbuffers2      b/ yes
          lda     #ascii:ff            no, finish partial page
          jsr     vt:logchr
vt:dumpbuffers2
;         jmp     vt:dumpbuffers       now dump the device buffers

vt:dumpbuffers ; requests the device to dump its buffers
;         ldx     dcbpointer
          jsr     dcb:tlclosedev,x     do device-specific work to dump buffers
          jmp     vt:checkto           check for timed-out dumping buffers
          page
vt:killproof ; disable ^C, ^D for this port only

;         ldx     dcbpointer
          lda     #dcbeditflags:killp
          oraa    dcb:editflags,x
          sta     dcb:editflags,x
          okrts

vt:killenable ; enable ^C, ^D for this port only
;  syscall:killdisable overrides

;         ldx     dcbpointer
          lda     #(\dcbeditflags:killp)&$ff
          anda    dcb:editflags,x
          sta     dcb:editflags,x
          okrts
          page
vt:coloring ; set output coloring

;         ldx     dcbpointer
          ldd     [dcb:scb+scblk:wrbuf,x]
          ldx     dcbpointer
          std     dcb:coloring,x
          jsr     dcb:profile+dpb:setcoloring,x
          okrts

vt:background ; set background coloring

;         ldx     dcbpointer
          lda     [dcb:scb+scblk:wrbuf,x]
          if      m6800!m6801
          ldx     dcbpointer
          fin
          staa    dcb:background,x
          jsr     dcb:profile+dpb:setbackground,x
          okrts
          page
vt:statusck ; errors if lineflags have changed, activation has occurred,
;   or if ^C^C has been signalled since the last sc:statusck

;         ldx     dcbpointer
          lsr     dcb:newstatus,x               assert: can only be 0 or 1
          bcc     vt:statusck.okrts
          dec     vt:allstatuscount             decrement the dcb count

;   if there is no remaining status of interest, forget that any has been seen

          lda     #dcbeditflags:activate!dcbeditflags:esc!dcbeditflags:into
          anda    dcb:editflags,x
          oraa    dcb:ctlckill,x
          oraa    dcb:lineflags,x
          beq     vt:statusck.okrts
          jsr     sdos+sdos:error
          #err:statushaschanged

vt:statusck.okrts
          okrts
          page
vt:getprofile ; return the profile number for DBP currently selected

;         ldx     dcbpointer
          ldb     dcb:profile+dpb:profilenumber,x get pointer to DPB
          bra     vt:return1byte

vt:getcol ; return current logical column count
;   if anything is in the line buffer, return the virtual input column
;   number; otherwise, return the current virtual column from dcb:colct

;   note that the virtual input column number is modulo the display width

;         ldx     dcbpointer
          if      1
          ldb     dcb:editflags,x
          bitb    #dcbeditflags:activate
          else
;   this test used to be as follows:
          ldb     dcb:linebufcount,x
;   and was changed because the above seemed more consistent with
;   the way input availibility is checked in the rest of the vtdriver
          fin
          beq     vt:getcol1
          ldb     dcb:linebufptr,x
          subb    dcb:linebuf,x assert: linebuf < 255 chars
          addb    dcb:readcol,x
          bra     vt:return1byte

vt:getcol1
          ldab    dcb:colct,x
vt:return1byte
          stb     [dcb:scb+scblk:rdbuf,x]
          okrts
          page
vt:geteof ; return the eof flag from the iocb

;         ldx     dcbpointer
          ldx     dcb:calleriocb,x
          ldb     iocb:eofflag,x
          ldx     dcbpointer
          bra     vt:return1byte

vt:gettype ; return the device type from dpb:dvtyp

;         ldx     dcbpointer
          ldb     dcb:profile+dpb:dvtyp,x
          bra     vt:return1byte

vt:getbackground ; return current background coloring

;         ldx     dcbpointer
          ldab    dcb:background,x
          bra     vt:return1byte

vt:getwrap ; return non-zero byte if in Wrap mode

;         ldx     dcbpointer
          ldab    dcb:editflags,x
          andb    #dcbeditflags:wrap
          bra     vt:return1byte

vt:getlineflags ; used to get a true copy of the lineflags...
;  hinted at by location $f0

          ldx     sdos+sdos:configuration
          jsr     cnfg:intdisable,x
          ldx     dcbpointer
          ldaa    dcb:lineflags,x
          clr     dcb:lineflags,x
          sta     [dcb:scb+scblk:rdbuf,x]
          ldx     sdos+sdos:configuration
          jsr     cnfg:intenable,x
          okrts
          page
vt:getparams ; return the current depth and width

;         ldx     dcbpointer
          ldaa    dcb:displaywidth,x
          ldab    dcb:displaydepth,x
          bra     vt:return2bytes

vt:getoutputtimeout ; return the timeout value for output

;         ldx     dcbpointer
          ldd     dcb:profile+dpb:outto,x
          bra     vt:return2bytes

vt:getbaudrate ; return last selected baud rate

;         ldx     dcbpointer
          ldd     dcb:baudrate,x
          bra     vt:return2bytes

vt:getactcol ; return the column number, and line buffer displacement,
;   at which the activation character was struck

;         ldx     dcbpointer
          lda     dcb:actcol,x
          ldb     dcb:actdisp,x
vt:return2bytes
          std     [dcb:scb+scblk:rdbuf,x]
          okrts
          page
vt:getfreecount ; return amount of room available in output ring

;         ldx     dcbpointer
          ldd     dcb:tlroom,x
          bra     vt:return2bytes

vt:getdatacount ; return amount of data ready in input ring

;         ldx     dcbpointer
          ldd     dcb:tldata,x
          bra     vt:return2bytes

vt:getidles ; return idle trigger plus idle count

;         ldx     dcbpointer
          ldaa    dcb:idlecount,x
          ldab    dcb:idletrigger,x
          bra     vt:return2bytes

vt:getcoloring ; return coloring information

;         ldx     dcbpointer
          ldd     dcb:coloring,x
          bra     vt:return2bytes
          page
vt:getpos ; return the current virtual position from dcb:row and dcb:col

;         ldx     dcbpointer
          ldaa    dcb:row,x
          ldab    dcb:col,x
          ldx     dcb:scb+scblk:rdbuf,x
          clr     0,x
          clr     1,x
          std     2,x
          okrts

vt:gettabs ; return current tab settings, requires 16 byte minimum RDBUF

;         ldx     dcbpointer
          if      m6800!m6801
          ldd     #dcb:tabs
          addd    dcbpointer
          std     tempx+2                 source buffer address
          fin
          ldab    #-1                     assume no tab stops, -1 offsets incb
vt:gettabs1 ; determine valid tab stop count
          incb
          inx                             inspect next tab stop
          lda     dcb:tabs-1,x            end of tab stop list ?
          bne     vt:gettabs1             b/ no, (B) holds # valid tab stops
          clra                            now (D) holds valid tab stop count
          ldx     dcbpointer
          std     dcb:scb+scblk:rplen,x   set reply length
          if      m6800!m6801
          ldx     dcb:scb+scblk:rdbuf,x   destination buffer
          stx     tempx
          ldx     tempx+2                 source buffer address
          else    (m6809)
          ldy     dcb:scb+scblk:rdbuf,x   destination
          leax    dcb:tabs,x              source
          fin
          jmp     vt:blockmove
          page
vt:getprofilename ; return text of profile name matching given DPB number

;         ldx     dcbpointer
          lda     [dcb:scb+scblk:wrbuf,x] fetch DPB number
          if      m6800!m6801
          ldx     dcbpointer            indirect addressing destroys (X)
          ldx     dcb:scb+scblk:rdbuf,x remember destination buffer
          stx     tempx
          else    (m6809)
          ldy     dcb:scb+scblk:rdbuf,x destination
          fin
          jsr     vt:findprofile       find desired DPB
          ldx     dpb:profilename,x    get pointer to profile name
          ldab    ,x                   = length of name
          inx                          pointer to body of name
          clra                         now (D) holds 16 bit reply count
          pshd                         save reply count
          jsr     vt:blockmove         move name to destination, reset C
          puld                         restore reply length
          ldx     dcbpointer           and place into syscall block
          std     dcb:scb+scblk:rplen,x
          rts                          assert: C=0
          page
vt:attentionck ; used by SDOS/MT to determine if attention has been requested
;  for the terminal in question

;         ldx     dcbpointer
          bra     vt:attncheck1

vt:attncheck ; called by SDOS to see if the operator requested attention,
;   which is determined by checking the lineflags of the CONSOLE: dcb

          ldx     sdos+sdos:configuration
          ldx     cnfg:devicedcbs,x
vt:attncheck1
          lda     dcb:lineflags,x
*         bita    #dcb:illflgs:esc,x
          bpl     vt:attncheck2
          anda    #(\dcbiillflgs:esc)&$ff
          staa    dcb:lineflags,x
          lda     vt:sdosmtflag
          beq     vt:attncheck3
          ldaa    #(\dcbiillflgs:esc)&$ff
          anda    lineflags
          staa    lineflags
vt:attncheck3
          jsr     sdos+sdos:error
          #err:attention

vt:attncheck2
          okrts
          page
vt:malposition ; entry point for DPB:POSITION on malleable terminals
;   called to output the cursor positioning sequence in the dcb

;   there are four parts to consider:
;           1) the skeletal sequence
;           2) the displacement of the row and column addresses within
;              the sequence
;           3) the offset to be added to the row and column addresses
;              supplied by the caller
;           4) the number of idle characters to be appended to the sequence

;   an example skeletal sequence found in a dcb:

;           *-----*---*-----*-----*        where :1b is an ESC, the "="
;           | :1b | = | :20 | :20 |        identifies this as a cursor
;           *-----*---*-----*-----*        positioning sequence, and the
;                                          :20 & :20 are the row and column
;           offsets, respectively; this sequence, if issued to the target
;           terminal, as is, would position to (0,0)

;   as the displacements of the row and column addresses within the sequence
;   may vary from device to device, the dcb contains displacements for
;   the row and column addresses; the offset to be added to the supplied
;   row or column is found at that displacement within the skeletal
;   sequence

;   as device requirements for idles trailing cursor control sequences vary,
;   provision is made by using the fifth byte of the skeletal sequence as
;   a count of the trailing idles to emit

;           *-----*---*-----*-----*-----*     where :03 is the count of
;           | :1b | = | :20 | :20 | :03 |     trailing idles to be emitted
;           *-----*---*-----*-----*-----*
          page
          ldaa     dcb:posnsl,x
          cmpa     #3                         must be 3 character sequence
          lblo     vt:nopositioning           b/ no, output "@@"
          ldd      dcb:posn,x
          std      tempx
          ldd      dcb:posn+2,x
          std      tempx+2
          clra
          ldab     dcb:rowdisp,x
          std      tempx+4
          ldb      2,s
          ldx      tempx+4
          addb     0,x
          stab     0,x
          ldx      dcbpointer
          ldab     dcb:coldisp,x
          std      tempx+4
          ldb      3,s
          ldx      tempx+4
          addb     0,x
          stab     0,x
          ldx      dcbpointer
          ldaa     dcb:posnsl,x
          ldab     dcb:posnidles,x
;         bra      vt:malcopy
          page
vt:malcopy

;   copy up to 4 bytes from tempx (length in (a)) to the output buffer
;   append (b) idles

          ldx      #0
          stx      tempx+4
vt:malcopy1
          pshd
          ldx      tempx+4
          lda      ,x+
          stx      tempx+4
          ldx      dcbpointer
          jsr      vt:tlstore
          lbcs     vt:err2s            b/ error, must pop 2 bytes from stack
          puld
          deca
          bne      vt:malcopy1
          tstb
          beq      vt:malcopy3
vt:malcopy2
          pshb
          clra
          jsr      vt:tlstore
          lbcs     vt:err1s            b/ error, must pop 1 byte from stack
          pulb
          decb
          bne      vt:malcopy2
vt:malcopy3
          okrts
          page
vt:malnewline

;   called to output a newline sequence, as described in the dcb:
;
;   if dcb:idletrigger is <LF> then <CR><LF><idles> will be output
;                         <CR> then <LF><CR><idles> will be output
;
;   in either case, <idles> is described by dcb:idlecount


          ldd      #ascii:lf##8+ascii:cr
          cmpb     dcb:idletrigger,x
          beq      vt:malnewline1
          if       m6800!m6801
          pshb
          tfr      a,b
          pula
          else     (m6809)
          exg      a,b
          fin
vt:malnewline1
          std      tempx
          ldaa     #2
          ldab     dcb:idlecount,x
          bra      vt:malcopy
          page
vt:malnewpage ; entry point for DPB:NEWPAGE for malleable devices
;   called to output the clear sequence in the dcb

;   this is similar to vt:malposn, above, except that there are only
;   two parts to consider:
;           1) the sequence
;           2) the number of idle characters to be appended to the sequence

          ldaa     dcb:clearsl,x
          beq      vt:malsimform       b/ no sequence, so simulate!
          ldd      dcb:clear,x
          std      tempx
          ldd      dcb:clear+2,x
          std      tempx+2
          ldaa     dcb:clearsl,x
          ldab     dcb:clearidles,x
vt:malcopyj1
          bra      vt:malcopy

vt:malsimform ; simulate form by outputting enough LineFeeds
; if device is a CRT, we could clear the screen, but we could not position
; to top (as required).  Since we can't do it right, we don't even try.
; So we assume the device must be printer, without checking.
          ldaa     dcb:displaydepth,x     how deep into page are we ?
          suba     dcb:row,x
          bne      vt:malsimformloop      b/ not at bottom of page
          lda      dcb:displaydepth,x     at bottom of page, throw blank page
vt:malsimformloop ; output (A) ASCII:LFs to sim form, caller will zero dcb:row
          psha                            save remaining count
          ldaa     #ascii:lf              output a single linefeed
          jsr      vt:tlstore
          lbcs     vt:err1s               b/ error, go pop remaining count
          pula                            pop remaining count
          deca                            count 1 linefeed that we output
          bne      vt:malsimformloop      b/ more linefeeds to output
          okrts
          page
vt:maleeol ; entry point for DPB:ERASETOEOL for malleable devices
;   called to output the eeol sequence in the dcb

;   this is similar to vt:malposn, above, except that there are only
;   two parts to consider:
;           1) the sequence
;           2) the number of idle characters to be appended to the sequence

;   as the sequence is not modified, it is output directly from its normal
;   location in the dcb

          ldaa     dcb:eeolsl,x
          lbeq     vt:simeeol   b/ no EEOL sequence, so fake it!
          ldd      dcb:eeol,x
          std      tempx
          ldd      dcb:eeol+2,x
          std      tempx+2
          ldaa     dcb:eeolsl,x
          ldab     dcb:eeolidles,x
          bra      vt:malcopyj1
          page    VT Interrupt Level Procedure
vt:inputto

;   receives control when an input timed period has expired; (d) -> dcb;
;   interrupts are disabled; if flag byte is busy, a short timeout is set
;   and the signalling is then tried again

          std     sdos+sdos:vtintdcb
          ldx     sdos+sdos:vtintdcb
          ldaa    #dcbreminders:rip
          bita    dcb:reminders,x
          bne     vt:inputto1
          jmp     sdos+sdos:rti

vt:inputto1
          lda     dcb:iilspl,x
          beq     vt:inputto2
          clra
          ldab    vt:timeoutblocks
          std     dcb:inputtoblk+timeout:fuse,x
          jmp     sdos+sdos:rti

vt:inputto2
          ldaa    #dcbiilspl:into
          staa    dcb:iilspl,x
          jmp     sdos+sdos:reschedule
          page
vt:outputto

;   receives control when an output timed period has expired; (d) -> dcb;
;   interrupts are disabled; timeout is marked in the switch byte and
;   rescheduling is invoked
;   editor task clears this bit upon signalling 'Device Timed Out' error

          std     sdos+sdos:vtintdcb
          ldx     sdos+sdos:vtintdcb
          ldaa    dcb:ilsw,x
          oraa    #dcbilsw:outto
          staa    dcb:ilsw,x
          jsr     dcb:iloto,x          also disables output interrupts
; leave dcb:oilquiescent reset so no further output requests are made
; until a call to vt:checkto discovers timeout and acknowledges it.
; This forces output ring to fill up, and VT:TLSTORE will discover the
; timeout when it attempts to fall asleep waiting for space,
; or device will be closed and vt:dumpbuffers will discover timeout.
          jmp     sdos+sdos:reschedule

vt:checkto

;   checks for 'device timed out' bit in dcb:ilsw
;   preserves (A)
;   callable by task level routines only!

          ldx     dcbpointer
          ldb     dcb:ilsw,x
          bitb    #dcbilsw:outto
          bne     vt:checkto.1
          okrts

vt:checkto.1
          ldd     dcbpointer
          ldx     #vt:clearto
          jsr     sdos+sdos:startio
          jsr     sdos+sdos:error
          #err:devicetimedout
          page
vt:clearto

;   clears 'timed out' bit in switch byte
;   interrupts must be disabled

          if      m6800!m6801
          pshd
          pulx
          else    (m6809)
          tdx
          fin
          ldb     dcb:ilsw,x
          andb    #\dcbilsw:outto
          stb     dcb:ilsw,x
          ldaa    #1
          sta     dcb:oilquiescent,x   remember that no more ints are expected
          ; this allows next output to trigger an output interrupt request
          jsr     vt:clearout          reset the output ring, too!
; it is best to clear output ring here, rather than in VT:OUTPUTTO, because
; this way we erase all the output intended for timed-out device, even
; if some occurs slightly after the timeout event itself.
; Also, since this is called by task level code, the reset occurs
; synchronously, rather than asynchronously (if called by interrupt level)
; with respect to character output routines, so no timing splinters occur
          jmp     sdos+sdos:rti
          page
vt:ilinput

;   receives control via dcb:inputint
;   control arrives here with interrupts disabled, stacks NOT switched
;   checks input mode, routes to ascii or binary mode input routines

          stx     sdos+sdos:vtintdcb
          inc     sdos+sdos:stackswitched       switch to interrupt stack
          bne     vt:ilinput1
          ldx     sdos+sdos:currentask
          sts     tcb:stack,x
          ldx     sdos+sdos:configuration
          lds     cnfg:interruptstack,x
          ldx     sdos+sdos:vtintdcb
vt:ilinput1
          ldaa    dcb:editflags,x
          bita    #dcbeditflags:readb
          beq     vt:ilasi

vt:ilbinarymodeinput

;   processes input as binary mode data

          jsr     dcb:ilgetdev,x
          bcs     vt:ilasibeep2        b/ error, go complain!
          jsr     vt:ilputbuf
          bne     vt:ilbinarymodeinput2
vt:ilasibeep2
          inc     dcb:beepcount,x
vt:ilbinarymodeinput2
          jmp     sdos+sdos:rti
          page
vt:ilasi
;   processes input as ASCII mode data, checking for control characters
;   and stripping off the "parity" bit

          jsr     dcb:ilgetdev,x           note: must not destroy (X) !
          bcs     vt:ilasibeep2            go complain about error
          anda    #ascii:mask
          jsr     dcb:profile+dpb:xlatei,x translate input
          lbcs    sdos+sdos:rti            character is to be ignored
          cmpa    #ascii:etx               (^C)
          bne     vt:ilasinotetx
vt:ilasietx
          ldab    #%1##(ascii:etx&%111)
          bitb    dcb:activation+(ascii:etx##-3),x
          lbne    vt:ilasiactivation       B/ this is an activation character!!
          jsr     vt:clearin               reset input ring to empty
          ldaa    #dcbeditflags:killp      see if port is killproof
          anda    dcb:editflags,x
          ora     dcb:ctlckill,x           see if already ^C^C'ed
          ora     sdos+sdos:killproof      see if system is killproof
          lbne    vt:ilasi32               bitch if any of above
          inc     dcb:ctlccount,x          put out '^C'
          ldaa    #dcbilsw:ctlc
          bita    dcb:ilsw,x
          bne     vt:ilasietx2             B/ this is the 2nd ^C
          oraa    dcb:ilsw,x
          staa    dcb:ilsw,x               remember the first time through
          jmp     sdos+sdos:reschedule
          page
vt:ilasietx2
          coma                             forget the first time through...
          anda    dcb:ilsw,x
          staa    dcb:ilsw,x
          lda     vt:sdosmtflag            ...and kill the user on the 2nd one
          beq     vt:ilasi94
          ldx     sdos+sdos:configuration
          ldx     cnfg:devicedcbs,x        ! change this when cnfg:vtdcbs in !
          cpx     sdos+sdos:vtintdcb
          bne     vt:ilasi94               kill only CONSOLE:
          jsr     sdos+sdos:killuserprogram
          ldx     sdos+sdos:vtintdcb
vt:ilasi94
          jsr     vt:setnewstatus
          ldx     sdos+sdos:vtintdcb       can't believe (X) on return...
          inc     dcb:ctlckill,x           mark user as killed
;                                          this forces vt:flbattn to ream
;                                          line buffer if in input mode
          ldaa    #16                      clear his activation table
;                                           so that further incoming chars
;                                            are treated according to standard
;                                             activation set
vt:ilasi941
          clr     dcb:activation,x
          inx
          deca
          bne     vt:ilasi941
          jmp     sdos+sdos:reschedule
          page
vt:ilasinotetx ; a non-^C character has arrived
          ldab    #\dcbilsw:ctlc
          andb    dcb:ilsw,x               cancel abort sequence
          stab    dcb:ilsw,x
          psha                             is this an activation character ?
          tfr     a,b
          lsra                             get byte number
          lsra
          lsra
          if      m6809
          leax    a,x
          else    (m6800!m6801)
          adda    sdos+sdos:vtintdcb+1
          psha
          ldaa    #0
          adca    sdos+sdos:vtintdcb
          psha
          pulx
          fin
          lda     dcb:activation,x         get byte from activation table
          andb    #%00000111               mask to obtain bit #
          if      m6809
          ldx     #vt:actbits
          bita    b,x
          else
          psha
          clra
          addd    #vt:actbits
          pshd
          pulx
          pula
          bita    0,x
          fin
          pula                             restore character code
          bne     vt:ilasiactivation       b/ activation, go store it!
          cmpa    #ascii:space
          bhs     vt:ilasiprinting         b/ not control character
          tfr     a,b                      thereby preserving (A)
          aslb                             double to make word index
          if      m6809
          ldx     sdos+sdos:vtintdcb       make (X) point at dcb
          ldu     #vt:ilasitable
          jmp     [b,u]                    char in (A), (X) --> dcb
          else
          addb    #vt:ilasitable&$FF
          pshb
          ldab    #vt:ilasitable/256
          adcb    #0
          pshb
          pulx
          ldab    1,x
          pshb
          ldab    0,x
          pshb
          ldx     sdos+sdos:vtintdcb
          rts
          fin
          page
vt:ilasiprinting
          ldx     sdos+sdos:vtintdcb       make (X) point at dcb
          ldab    dcb:ilsw,x
          bitb    #dcbilsw:alphalock
          beq     vt:ilasinormal
          cmpa    #ascii:esc+$60           lowercase z+1
          bhs     vt:ilasinormal
          cmpa    #'@+$21                  lowercase a
          blo     vt:ilasinormal
          anda    #\$20                    force to uppercase alpha
vt:ilasinormal
          ldab    dcb:profile+dpb:flags,x
          bitb    #dpbflags:output
          bne     vt:ilasibeep             B/ beep if output only
          jsr     vt:ilputbuf
          bne     vt:ilasi2
vt:ilasibeep
          inc     dcb:beepcount,x
vt:ilasi2
          jmp     sdos+sdos:rti            nonactivation char, reschedule unneeded

vt:ilasiactivation ; activation character received
          ldx     sdos+sdos:vtintdcb       make (X) point at dcb
          ldab    dcb:profile+dpb:flags,x
          bitb    #dpbflags:output
          bne     vt:ilasibeep             B/ beep if output only
          jsr     vt:ilputbuf
          beq     vt:ilasibeep
          jmp     sdos+sdos:reschedule     since activation was received
          page
vt:ilasitable ; branch table used to branch on input control character
          #sdos+sdos:rti                    ^@ (ignore nulls if not activation)
          #vt:ilasisoh                      ^A
          #vt:ilasistx                      ^B
          #vt:ilasinormal                   ^C (impossible to branch thru here)
          #vt:ilasieot                      ^D
          #vt:ilasinormal                   ^E
          #vt:ilasinormal                   ^F
          #vt:ilasibel                      ^G
          #vt:ilasinormal                   ^H
          #vt:ilasinormal                   ^I
          #vt:ilasinormal                   ^J
          #vt:ilasinormal                   ^K
          #vt:ilasinormal                   ^L
          #vt:ilasiactivation               ^M
          #vt:ilasinormal                   ^N
          #vt:ilasisi                       ^O
          #vt:ilasidle                      ^P
          #vt:ilasidc1                      ^Q
          #vt:ilasinormal                   ^R
          #vt:ilasidc3                      ^S
          #vt:ilasidc4                      ^T
          #vt:ilasinormal                   ^U
          #vt:ilasisyn                      ^V
          #vt:ilasinormal                   ^W
          #vt:ilasinormal                   ^X
          #vt:ilasinormal                   ^Y
          #vt:ilasinormal                   ^Z
          #vt:ilasiesc                      :1B
          #vt:ilasinormal                   :1C
          #vt:ilasinormal                   :1D
          #vt:ilasinormal                   :1E
          #vt:ilasinormal                   :1F
          page
vt:ilasisoh ; (^A)
          ldaa    dcb:ilsw,x
          eora    #dcbilsw:alphalock
          staa    dcb:ilsw,x
          bra     vt:ilasi2                don't need to reschedule for ^A
          page
vt:ilasieot ; (^D)
          jsr     sdos+sdos:getrunningencrypted
          lda     ,x                       running encrypted ?
          bne     vt:ilasi32               B/ we are-- too bad
          lda     vt:sdosmtflag
          bne     vt:ilasi31
vt:ilasi32
          ldx     sdos+sdos:vtintdcb
          bra     vt:ilasibeep

vt:ilasi31
          ldx     sdos+sdos:vtintdcb
          lda     #dcbeditflags:killp
          anda    dcb:editflags,x
          ora     sdos+sdos:killproof
          bne     vt:ilasi32               b/ system or port is killproof
          ldx     sdos+sdos:configuration
          ldx     cnfg:vtdebug,x
          beq     vt:ilasi32               b/ no debugger
          ldx     sdos+sdos:vtintdcb       cursor is now lost
          ldaa    dcb:displaydepth,x
          staa    dcb:cursorlost,x
          ldaa    sdos+sdos:stackswitched  =0 if interrupted task level
          bne     vt:ilasi33
          ldx     sdos+sdos:currentask
          lds     tcb:stack,x
vt:ilasi33
          dec     sdos+sdos:stackswitched
          ldx     sdos+sdos:configuration
          jmp     [cnfg:vtdebug,x]
          page
vt:ilasibel ; (^G)
          ldaa    #dcbiillflgs:ctlg
vt:ilasi41
          ldb     dcb:iillflgs,x
          bne     vt:ilasi32
          staa    dcb:iillflgs,x
vt:ilforcereschedule
          jmp     sdos+sdos:reschedule

vt:ilasiesc ; (ESC)
          ldaa    #dcbiillflgs:esc
          bra     vt:ilasi41

vt:ilasistx ; (^B)
          ldaa    #dcbiillflgs:ctlb
          bra     vt:ilasi41

vt:ilasidc4 ; (^T)
          ldaa    #dcbiillflgs:ctlt
          bra     vt:ilasi41

vt:ilasisyn ; (^V)
          ldaa    #dcbiillflgs:ctlv
          bra     vt:ilasi41
          page
vt:ilasisi ; (^O)
          ldaa    #dcbiilspl:discard
          bra     vt:ilasi91

vt:ilasidle ; (^P)
          ldaa    #dcbiilspl:page
*         jmp     vt:ilasi91

vt:ilasi91
          ldb     dcb:iilspl,x
          lbne    vt:ilasibeep
          staa    dcb:iilspl,x
          jmp     sdos+sdos:reschedule

vt:ilasidc1 ; (^Q)
          lda     dcb:ilsw,x           hardcopy freeze set ?
;         bita    #dcbilsw:hcfreeze    ...?
          bpl     vt:ilasidc1.1        b/ no
          anda    #(\dcbilsw:hcfreeze)&$ff
          staa    dcb:ilsw,x           remove hardcopy freeze
          bra     vt:iloutputwasq0     go set up for output interrupt

vt:ilasidc1.1
          ldaa    #dcbiilspl:continue
          bra     vt:ilasi91

vt:ilasidc3 ; (^S)
          bsr     vt:testoutputhardcopy
          bcc     vt:ilasi11.1
          ldab    #dcbilsw:hcfreeze
          orab    dcb:ilsw,x
          stab    dcb:ilsw,x
vt:ilasidc1.done ; all done servicing ^Q or ^S
          jmp     sdos+sdos:rti

vt:ilasi11.1
          ldaa    #dcbiilspl:freeze
          bra     vt:ilasi91
          page
vt:testoutputhardcopy

;   tests if DCB is output-only or hardcopy edit; if so, carry is set upon
;   return; otherwise, carry is clear

          ldab    #dpbflags:output
          bitb    dcb:profile+dpb:flags,x
          bne     vt:testoutputhardcopy.1
          ldab    #dcbeditflags:hcedit
          bitb    dcb:editflags,x
          bne     vt:testoutputhardcopy.1
          okrts

vt:testoutputhardcopy.1
          errorrts
          page
vt:iloutputwasquiescent

;   the edit task put a byte in an empty buffer and asked sdos:startio
;   to nudge this routine in the ribs to prime output interrupts

          std     sdos+sdos:vtintdcb   save DCB address passed in (D)
vt:iloutputwasq0 ; re-enter here to restart output after ^Q
          dec     sdos+sdos:stackswitched unswitch stacks...
          bpl     vt:iloutputwasq1     to match entry condition of vt:iloutput
          ldx     sdos+sdos:currentask
          lds     tcb:stack,x
vt:iloutputwasq1
          ldx     sdos+sdos:vtintdcb   get address of DCB to start up
          clr     dcb:oilquiescent,x   assume "output int is coming"
          ; assert: the device output buffer IS empty now!
          jsr     dcb:ilenboi,x        enable interrupt on buffer empty
                                       ; not because we want the interrupt now
                                       ; but because we want an interrupt
                                       ; after we output next character!
          beq     vt:iloutput.setfuse  b/ not ready for output yet
                                       ; output int will tell us when it is

vt:iloutput ; come here from interrupt poll chain via SDOS:VTOUTPUTINT
;   entered with stacks NOT switched and interrupts disabled!
;   device is still enabled to cause an output interrupt
;   since fast output is very common, this code must be as fast as possible!
          ldaa    dcb:ilsw,x           (4+1~) see if output is frozen
*         bita    #dcbilsw:hcfreeze    (is MSB of byte)
          bmi     vt:iloutput.freeze   (3~) B/ freeze flag is set
          ldd     dcb:profile+dpb:outto,x  (5+1~) re-fuse for next output character
          std     dcb:outputtoblk+timeout:fuse,x (5+4~) assume there is one!
          ; now get the next character out of the output ring.
          inc     dcb:ildata+1,x       ; (6+1~) count of -1 --> buffer empty
          bne     vt:iloutput.more     ; (3~) b/ fast path, still data in buffer
          inc     dcb:ildata,x         ; (6+1~)/256 propogate "carry"
          bne     vt:iloutput.more     ; (3~) b/ still data in buffer
          ; note: we don't mind a little inefficiency if empty output ring
          dec     dcb:ildata+1,x       ; (6+1~) restore "buffer empty" signal
          dec     dcb:ildata,x         ; (6+1~)
          inc     dcb:oilquiescent,x   remember "no output int expected"
vt:iloutput.freeze ; don't output any more data to device
          clra                         kill output timeout
          clrb
          std     dcb:outputtoblk+timeout:fuse,x
          jsr     dcb:ildisoi,x        ensure no more ints come from device
          rti

vt:iloutput.setfuse ; we enabled device for output buffer empty interrupt
;   but it does not have an empty output buffer at this instant
          ldd     dcb:profile+dpb:outto,x set up output fuse...
          std     dcb:outputtoblk+timeout:fuse,x to make sure he responds
          rti                          and get out!
          page
vt:iloutput.more ; more data is available in ring buffer to send to device
          inc     dcb:tlroom+1,x       ; (6+1~) tell task more room available
; Note that clock ticks are required to guarantee that editor task will
; discover the free space!  We could do a SDOS:RESCHEDULE here, to force
; discovery, but that would just be expensive, so we count on the clock.
          bne     vt:iloutput.send     ; (3~) b/ no carry, fast path
          inc     dcb:tlroom,x         ; (6+1~)/256
vt:iloutput.send ; send (A) to device
          inc     dcb:ringoutdata+1,x  ; (6+1~) at end of ring ?
          bne     vt:iloutput.getbyte  ; (3~) b/ no, fast path
          inc     dcb:ringoutdata,x    ; (6+1~) ...?
          bne     vt:iloutput.getbyte  ; (3~) b/ no
          ; end of ring, must adjust pointers back to beginning
          ldd     dcb:ringoutlen,x     ; (4+1~) contains -SizeOfRing
          std     dcb:ringoutdata,x    ; (4+1~) proper value after fetch byte
          if      m6809
          ldu     dcb:ringoutfetch,x   ; (5+1~) fetch byte to send
          lda     ,u                   ; (4+0~)
          else    (m6800!m6801)
          stx     sdos+sdos:vtintdcb
          ldx     dcb:ringoutfetch,x
          ldaa    0,x
          ldx     sdos+sdos:vtintdcb
          fin
          ldb     dcb:ringoutbase,x    ; (4+1~) now set ring pointer back...
          stb     dcb:ringoutfetch,x   ; (4+1~) to front of ring
          ldb     dcb:ringoutbase+1,x  ; (4+1~)
          stb     dcb:ringoutfetch+1,x ; (4+1~)
          jmp     dcb:ilputdev,x       ; (3+4~) which outputs byte, does RTI

vt:iloutput.getbyte ; fetch data byte and advance pointer
          if      m6809
          ldu     dcb:ringoutfetch,x   ; (5+1~)
          lda     ,u+                  ; (4+2~)
          stu     dcb:ringoutfetch,x   ; (5+1~)
          else    (m6800!m6801)
          stx     sdos+sdos:vtintdcb
          ldx     dcb:ringoutfetch,x
          ldaa    0,x
          ldx     sdos+sdos:vtintdcb
          incd    dcb:ringoutfetch,x
          fin     m6809
          jmp     dcb:ilputdev,x       ; (3+4~) which outputs byte, does RTI
;         from vt:iloutput to here is 72~ on 6809 --> ~110 cycle int service
;         note: dcb:ilputdev is 4 bytes, and normally contains:
;         staa    >devaddress
;         rti
          page    VT Ring Buffer Management
;   The VT driver provides efficient ring management routines for
;   buffering input/output data.  Buffer routines to put and retreive
;   data for both input and output are provided; one of the routines
;   is coded inline in VT:ILOUTPUT (above) to obtain very high performance.
;   Such rings may handle up to 65536 bytes per ring;
;   each DCB contains enough information (in DCB:RINGxxx)
;   to specify everything the VT driver needs to know about a ring.
;   Some of the values required are slightly unusual in order
;   to allow the most efficient ring management routines possible.
;   A ring buffer may begin at any address.

;   The format of the provided ring buffer control block in the DCB is:
;
;       *-------------------*-------------------*-------------------*
;       | input fetch addr  |      # data       | -dist to end ring !
;       *-------------------*-------------------*-------------------*
;       | input store addr  |    - # room -1    | -dist to end ring |
;       *-------------------*-------------------*-------------------*
;       |   buffer base     |    - length       |
;       *-------------------*-------------------*
;
;       *-------------------*-------------------*-------------------*
;       | output fetch addr |    - # data -1    | -dist to end ring |
;       *-------------------*-------------------*-------------------*
;       | output store addr |      # room       | -dist to end ring |
;       *-------------------*-------------------*-------------------*
;       |   buffer base     |    - length       |
;       *-------------------*-------------------*

;   Where the fetch and store addresses are the addresses of the next byte
;   to be accessed, "dist to end ring" is the number of bytes to go before
;   the associated pointer must be "wrapped" to the beginning, # data
;   is amount of data in the ring, # room is ringsize-#data and the
;   buffer base and length are the origin and length of the buffer.  The
;   base and length fields are used to initialize the addr and # fields
;   when a buffer wraps around.
          page
;                       fetch              store
;           xx00        xx25               xx87         xxFF
;           |              |                  |            |
;
;           *-----------------------------}{---------------*
;           |//////////////      (data)       /////////////|
;           *-------------------------}{-------------------*
;
;             room: 9D bytes            data: 62 bytes
;
;
;                       store              fetch
;           xx00        xx25               xx87         xxFF
;           |              |                  |            |
;
;           *-----------------------------}{---------------*
;       *-->|    (data)    ///////////////////    (data)   |---*
;       |   *-------------------------}{-------------------*   |
;       *--  --  --  --  --  --  --  --  --  --  --  --  --  --*
;
;             room: 61 bytes            data: 9E bytes

;   The above illustrates a 256 byte ring buffer, beginning on a page
;   boundary.  Note that the buffer shown always has one byte in "limbo",
;   i.e, it is neither "room" nor "data". ??? is this correct ???
          page
vt:tlputbuf ; for 16 bit rings
;   puts (a) into the output ring buffer; DCBPOINTER,(x) -> dcb
;   It destroys all other registers.
;   This routine MUST be called from task level, operates with ints enabled.
;   Bytes placed into the output ring buffer are removed by VT:ILOUTPUT.
;   Note that this routine is in critical output path
;   for fast terminals, and needs to be as lean as possible!
;   (This requirement is mitigated somewhat by WRITEA.OPTIMIZED,
;   which inserts long streams into output buffer without waste of
;   individual buffer updates on each byte).

;   Exit with Z set if buffer is full, no room to store character.
;   Exit with Z reset (nonzero) if character was stored.
;   On the 6809, this routine requires 69 cycles for the fast path.
          if      m6800!m6801
          sei                          ; (2~) make it safe to update dbl bytes
          else    (m6809)
          orcc    #%01010000           ; (3~) mask out FIRQ and IRQ
          fin
          ldab    dcb:tlroom+1,x       ; (4+1~) any room left in ring ?
          bne     vt:tlputbuf1         ; (3~) b/ yes, so put byte into ring
          ldab    dcb:tlroom,x         ; (4+1~)... ?
          bne     vt:tlputbuf0         ; (3~) b/ yes
          if      m6800!m6801
          cli                          ; (2~) let the sun shine again
          else    (m6809)
          andcc   #\%01010000          ; (3~) allow FIRQ and IRQ again
          fin
          rts                          ; assert: Z CC bit set

vt:tlputbuf0 ; there is lotsa room in the ring
          dec     dcb:tlroom,x         ; (6+1~) decrement upper half
vt:tlputbuf1 ; there is room in the ring
          dec     dcb:tlroom+1,x       ; (6+1~) decrement remaining room
          ldab    dcb:ildata+1,x       ; (4+1~) signal that data is available
          bne     vt:tlputbuf2         ; (3~) because it will be in a moment
          dec     dcb:ildata,x         ; (6+1~) propogate borrow bit
vt:tlputbuf2
          dec     dcb:ildata+1,x       ; (6+1~) bump interrupt data available
          if      m6809
          ldu     dcb:ringoutstore,x   ; (5+1~) store the byte in the ring
          sta     ,u+                  ; (4+2~)
          andcc   #\%01010000          ; (3~) allow FIRQ and IRQ again
          else    m6800!m6801
          ldx     dcb:ringoutstore,x
          staa    0,x
          cli                          ; (3~) allow interrupts as soon as safe
          ldx     dcbpointer
          fin
          inc     dcb:ringoutroom+1,x  ; (6+1~) hit end of ring ?
          bne     vt:tlputbuf3         ; (3~) b/ no, fast path
          inc     dcb:ringoutroom,x    ; (6+1~) ...?
          bne     vt:tlputbuf3         ; (3~) b/ no
          ; end of ring hit, reset pointer [low probability (<1%) path]
          ldd     dcb:ringoutlen,x     ; (5+1~) holds -SizeOfRing
          std     dcb:ringoutroom,x    ; (5+1~)
          ldd     dcb:ringoutbase,x    ; (5+1~)
          std     dcb:ringoutstore,x   ; (5+1~) assert: this resets Z bit
          rts

vt:tlputbuf3 ; not at end of ring yet
          if      m6809
          stu     dcb:ringoutstore,x   ; (5+1~) store updated ring pointer
          else    (m6800!m6801)
          incd    dcb:ringoutstore,x   ; update ring pointer
          fin     m6809
          rts                          ; (5~) with Z bit reset
          page
vt:clearout ; called from the task level to clear the output ring buffer
;   interrupts MUST be disabled to prevent timing splinters
;   This routine is called very infrequently

          clra                         ; cheap version of ldd #0
          clrb
          subd    dcb:ringoutlen,x     since DCB:RINGOUTLEN contains -size
          std     dcb:tlroom,x         = how much space available in ring
          ldd     dcb:ringoutlen,x     = - SizeOfRing
          std     dcb:ringoutdata,x    dist to end ring for vt:iloutput to use
          std     dcb:ringoutroom,x    dist to end ring for vt:tlputbuf to use
          ldd     #-1                  "-1" --> no data for interrupt level
          std     dcb:ildata,x
          ldd     dcb:ringoutbase,x    set pointers to start of ring
          std     dcb:ringoutfetch,x
          std     dcb:ringoutstore,x
          rts
          page
vt:clearin ; called from interrupt level to clear the input ring buffer
;   (X) must point to desired DCB; (X) is preserved.
;   interrupts MUST be disabled to prevent timing splinters
;   This routine is called very infrequently

          clra                         ; cheap version of ldd #0
          clrb
          std     dcb:tldata,x
          ldd     dcb:ringinlen,x      = - SizeOfRing
          std     dcb:ilroom,x         will be -1 when ring is (almost) full
          std     dcb:ringinroom,x     dist to end ring for vt:ilputbuf to use
          std     dcb:ringindata,x     dist to end ring for vt:tlgetbuf to use
          ldd     dcb:ringinbase,x
          std     dcb:ringinfetch,x
          std     dcb:ringinstore,x
          rts
          page
vt:ilputbuf
;   puts (a) into the input ring buffer; SDOS:VTINDCB,(x) -> dcb.
;   It destroys all other registers.
;   MUST be called from the interrupt level, with interrupts disabled.
;   This routine need not be extremely high performance,
;   as input interrupts are relatively infrequent.
;   Exit with Z set if buffer is full, no room to store character.
;   Exit with Z reset (nonzero) if character was stored.
;   This routine is coded tighter than vt:tlgetbuf because
;   it runs interrupt disabled, whereas vt:tlgetbuf runs mostly enabled

          inc     dcb:ilroom+1,x       ; -1 --> buffer is full
          bne     vt:ilputbuf1         ; b/ ring space is available, fast path
          inc     dcb:ilroom,x         ; finish "decrementing" remaining room
          bne     vt:ilputbuf1         ; b/ ring space is available
          dec     dcb:ilroom,x         ; no space in ring for new character
          dec     dcb:ilroom+1,x       ; restore count to "full" code
          clrb                         ; set Z bit --> no room
          rts                          ; exit with Z set

vt:ilputbuf1 ; there is room in the ring
          inc     dcb:tldata+1,x       ; bump number of data bytes collected
          bne     vt:ilputbuf2         ; b/ no carry, fast path
          inc     dcb:tldata,x         ; propogate carry to upper half
vt:ilputbuf2 ; store the byte into the ring
          if      m6809
          ldu     dcb:ringinstore,x    ; fetch store pointer
          sta     ,u+                  ; store the byte, advance the pointer
          else    (m6800!m6801)
          ldx     dcb:ringinstore,x    ; fetch store pointer
          sta     0,x                  ; store the byte
          ldx     sdos+sdos:vtintdcb   ; restore DCB pointer
          fin
          inc     dcb:ringinroom+1,x   ; at end of ring ?
          bne     vt:ilputbuf3         ; b/ no (fast path)
          inc     dcb:ringinroom,x     ; propogate carry to upper half
          bne     vt:ilputbuf3         ; b/ not end of ring
          ldd     dcb:ringinlen,x      ; yes, set counter to -SizeOfRing
          std     dcb:ringinroom,x
          ldd     dcb:ringinbase,x     ; and set pointer back to front of ring
          std     dcb:ringinstore,x    ; this leaves Z reset
          rts

vt:ilputbuf3 ; not at end of ring, update store pointer
          if      m6809
          stu     dcb:ringinstore,x    ; update the store pointer (resets Z)
          else    (m6800!m6801)
          incd    dcb:ringinstore,x    ; bump store pointer, leave Z reset
          fin
          rts                          ; with Z reset
          page
vt:tlgetbuf
;   returns the next input data byte into (a); DCBPOINTER,(x) -> dcb;
;   MUST be called from the task level.
;   This routine destroys all registers except (A) and (X).
;   This routine need not have very high performance,
;   as input events are far more infrequent than output events.
;   Exit with Z set if buffer is empty
;   Exit with Z reset (nonzero) if character was retreived.

          if      m6800!m6801
          sei                          ; (2~) make it safe to update dbl bytes
          else    (m6809)
          orcc    #%01010000           ; (3~) mask out FIRQ and IRQ
          fin
          ldab    dcb:tldata+1,x       ; any data available ?
          bne     vt:tlgetbuf1         ; b/ yes, go get byte from ring
          ldaa    dcb:tldata,x         ; is upper byte also zero ?
          bne     vt:tlgetbuf0         ; b/ no, data is available
          if      m6800!m6801
          cli                          ; (2~) let the sun shine again
          else    (m6809)
          andcc   #\%01010000          ; (3~) allow FIRQ and IRQ again
          fin
          rts                          ; with Z set

vt:tlgetbuf0 ; lotsa data is available
          dec     dcb:tldata,x         ; propogate borrow from "DEC" below
vt:tlgetbuf1 ; some data is available from ring buffer
          dec     dcb:tldata+1,x       ; decrease remaining ring data
          ldaa    dcb:ilroom+1,x       ; will borrow happen if DEC ILROOM ?
          bne     vt:tlgetbuf2         ; b/ no borrow possible
          dec     dcb:ilroom,x         ; propogate borrow bit from "DEC" below
vt:tlgetbuf2
          dec     dcb:ilroom+1,x       ; increase room about to be available
          if      m6809
          ldu     dcb:ringinfetch,x    ; get the byte from the ring
          lda     ,u+                  ; and advance the pointer
          andcc   #\%01010000          ; (3~) allow FIRQ and IRQ again
          else    (m6800!m6801)
          ldx     dcb:ringinfetch,x    ; get the byte from the ring
          ldaa    0,x
          cli                          ; (2~) allow ints as soon as safe
          ldx     dcbpointer           ; make (X) point to DCB again
          fin
          inc     dcb:ringindata+1,x   ; at end of ring ?
          bne     vt:tlgetbuf3         ; b/ no
          inc     dcb:ringindata,x     ; propogate carry
          bne     vt:tlgetbuf3         ; b/ not at end of ring
          ldab    dcb:ringinlen,x      ; reset data bytes to end...
          stab    dcb:ringindata,x     ; to -SizeOfRing
          ldab    dcb:ringinlen+1,x
          stab    dcb:ringindata+1,x
          ldab    dcb:ringinbase+1,x   ; move pointer to front of ring
          stab    dcb:ringinfetch+1,x
          ldab    dcb:ringinbase,x
          stab    dcb:ringinfetch,x    ; leaves Z bit reset
          rts                          ; with Z bit reset

vt:tlgetbuf3 ; not at end of ring, update DCB:RINGINFETCH
          if      m6809
          stu     dcb:ringinfetch,x    ; leaving Z bit reset
          else    (m6800!m6801)
          incd    dcb:ringinfetch,x    ; leaving Z bit reset
          fin
          rts                          ; with Z bit reset
          page Patch space and oversize checks
          pca      1
vt:patch  ; vt driver patch space (heaven forbid it should get used...)
          rpt     50
          swi

vt:driverend equ *
vt:driversize equ vt:driverend-driver

          if      vt:driversize>>vt:driverestimatedsize
          ? Estimated size of VT Driver is too small ?
          fin     vt:driversize>>vt:driverestimatedsize

          if      vt:driversize+128<<vt:driverestimatedsize
          ? Estimated size of VT Driver is too big ?
          fin     vt:driversize+128<<vt:driverestimatedsize

          if      vt:driverend>>sdos+sdos:extensionsize
          ?error  VT Driver overlaps SDOS
          fin     vt:driverend>>sdos+sdos:extensionsize

          end     ; **** Virtual Terminal Driver ****
