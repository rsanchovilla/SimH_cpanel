//TX67893  JOB  (1),TX67893,
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=8192K
//*
//*  2021/04/08 @kl TX67893 zap for VS1 6.7
//*                 circumvention for abend in console
//*                 with check for communications task
//*                 with linkage editor EXPAND to create
//*                 IFG0196X patch area.
//*
//RECEIVE  EXEC SMP
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TX67893)     /* REWORK(20210408) */             .
++ VER (X067)
   FMID(EDM1101)                                           .
++ IF FMID(EPM1101) THEN REQ (UX15738)
 /*
   PROBLEM DESCRIPTION(S):
     TX67893 -
       Abend in VS1 6.7 display console initialization during NIP.

       When VS1 6.7 attempts to activate consoles at the end of
       NIP, activation of display consoles fails, probably due to
       a race condition in which fields that are expected to be
       initialized haven't been initialized yet.  DIDOCS display
       console activation issues an OPEN for a DCB pointing to a
       DD statement constructed to define the console device.
       OPEN module IFG0196X issues a QMGRIO macro to write back
       the possibly-modified JFCB for the device being opened.  In
       the course of the QMGRIO, one of the following generally
       occurs:

       (1)  The QMGRIO macro expands to instructions to obtain
            the address of the QMGRIO service routine by chaining
            through the CVT and JESCT, then doing a BALR to the
            address obtained.  The CVT pointer to the JESCT,
            CVTJESCT, is zero, causing what the macro believes to
            be the address of QMGRIO to be garbage, so the BALR
            results in an abend0C1; or

       (2)  CVTJESCT has been initialized correctly, so the QMGRIO
            service routine receives control as it should, but its
            validation of the QMGRIO parameter list passed by
            IFG0196X finds the parameter list is defective, with
            fields that should be initialized set to zero, so it
            issues an abend1B0.

       After the abend, initialization of the display console
       stops, but its UCM is marked active, so messages are queued
       to it.  Because those messages are never displayed, WTO
       buffers are eventually exhausted.

       IFG0196X is changed to bypass the QMGRIO if the current
       task is the communications task.  This is a circumvention,
       not a fix, but it allows console initialization to complete
       successfully.

       Note that this usermod assumes that the fix for UX15738
       has been installed on the current system.  UX15738 fixes
       a problem with linkage editor EXPAND processing in the case
       of a target load library that has a block size that is a
       multiple of 1024 (the EXPAND appears to work, but
       patterns of binary zeroes are written over the first few
       bytes of the module).  Another problem encountered during
       the development of the usermod was the fact that IBM linked
       IFG0196X as NE (not editable), so the module has no ESD and
       cannot be reprocessed by the linkage editor.  That problem
       was circumvented by reconstructing the module in SYS1.AOSD0
       prior to VS1 system generation using the DELINK0 FE aid.

       Also note that TX67893 checks to see if the current job
       is the communications task by checking the TIOT pointed
       to by the current task for job name "CONSOLE", step name
       "SYSTEM", procedure step name "VMS".   Don't use that
       job and step name combination for any ordinary VS1 job,
       or it will be treated as the communications task and the
       QMGRIO bypassed.

   COMPONENT:  5741-SC1D1-EDM1101

   APARS FIXED: TX67893

   SPECIAL CONDITIONS:
     DEPENDENCY:  IFG0196X in the target SYS1.SVCLIB must
      already have been relinked to remove the NE (not
      editable) attribute set by default in the standard
      IBM distribution.

   COMMENTS:
     LAST CHANGE:  2021/04/08

     REWORK HISTORY:
      2021/04/08: Created.

     CROSS REFERENCE-MODULE/MACRO NAMES TO APARS
      IFG0196X  TX67893

     CROSS REFERENCE-APARS TO MODULE/MACRO NAMES
      TX67893   IFG0196X

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

     MODULES
       IFG0196X

     LISTEND
 */.
++ ZAP      (IFG0196X) DISTLIB(AOSD0   ).
NAME IFG0196X
 EXPAND IFG0196X(74)
IDRDATA TX67893
VER 000000 0530             START    BALR  RBASE,0            Establish base reg
VER 0002DC 58F0,0010                 L     R15,16(0,0)        +Get CVT pointer
VER 0002E0 58F0,F128        REP1     L     R15,CVTJESCT(,R15) +Get JESCT pointer
VER 0002E4 58F0,F010                 L     R15,JESRESQM(,R15) +Get QMGRIO addres
VER 0002E8 05EF                      BALR  R14,R15            +To queue manager
VER 000336 58F0,0010                 L     R15,16(0,0)        +Get CVT pointer
VER 00033A 58F0,F128        REP2     L     R15,CVTJESCT(,R15) +Get JESCT pointer
VER 00033E 58F0,F010                 L     R15,JESRESQM(,R15) +Get QMGRIO addres
VER 000342 05EF                      BALR  R14,R15            +To queue manager
VER 000400 0000000000000000 PATCHLOC DC    (PATCHLEN)X'00'    PATCH AREA
VER 000408 0000000000000000
VER 000410 0000000000000000
VER 000418 0000000000000000
VER 000420 0000000000000000
VER 000428 0000000000000000
VER 000430 0000000000000000
VER 000438 0000000000000000
VER 000440 0000
REP 0002E0 45E0,33FE                 BAL   R14,PATCH1         To patch area
REP 0002E4 4700,0000                 NOP   0                  No op
REP 0002E8 0700                      NOPR  0                  No op
REP 00033A 45E0,33FE                 BAL   R14,PATCH1         To patch area
REP 00033E 4700,0000                 NOP   0                  No op
REP 000342 0700                      NOPR  0                  No op
REP 000400 58F0,F000        PATCH1   L     R15,CVTTCBP        Address TCB words
REP 000404 58F0,F004                 L     R15,4(,R15)        Get current TCB
REP 000408 58F0,F00C                 L     R15,TCBTIO         Get TIOT
REP 00040C D517,3428,F000            CLC   KJNAME,TIOCNJOB    Confirm comm task
REP 000412 4780,3422                 BE    PATCH1B            Yes, skip QMGRIO
REP 000416 58F0,0010        PATCH1A  L     R15,CVTPTR         Get CVT address
REP 00041A 58F0,F128                 L     R15,CVTJESCT(,R15) Get JESCT pointer
REP 00041E 58F0,F010                 L     R15,JESRESQM(,R15) Get QMGRIO addres
REP 000422 07FF                      BR    R15                To QMGRIO
REP 000424 41F0,0000        PATCH1B  LA    R15,0              Set RC=0
REP 000428 07FE                      BR    R14                Skip QMGRIO
REP 00042A C3D6D5E2D6D3C540 KJNAME   DC    C'CONSOLE SYSTEM  VMS     '
REP 000432 E2E8E2E3C5D44040
REP 00043A E5D4E24040404040
??
//SMPCNTL  DD  *
  RECEIVE S(TX67893)
        .
//*
//APPLYCK EXEC SMP
//SMPCNTL  DD  *
  APPLY S(TX67893)
        CHECK
        BYPASS(ID)
        .
//*
//APPLY   EXEC SMP
//SMPCNTL  DD  *
  APPLY S(TX67893)
        DIS(WRITE)
        .
//

//REJECT  EXEC SMP
//SMPCNTL  DD  *
  REJECT S(TX67893).
//

//RESTORE EXEC SMP
//SMPCNTL  DD  *
  RESTORE S(TX67893).
//
