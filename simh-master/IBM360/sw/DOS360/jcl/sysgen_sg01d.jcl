// JOB SYSGEN01   ASSEMBLE NEW SUPERVISOR
// PAUSE SCRATCH TAPE NEEDED ON X'280'
MTC REW,X'280'
MTC WTM,X'280',5
MTC REW,X'280'
// ASSGN SYSPCH,X'280'
// OPTION DECK
// EXEC ASSEMBLY
         SUPVR AP=YES,                  MULTITASKING                   X
               ASCII=YES,               ASCII SUPPORT                  X
               ERRLOG=NO,               NO ERRLOG ON 360               X
               EU=NO,                   14XX EMULATOR                  X
               MCRR=NO,                 MACHINE CHECK RECORDING        X
               MICR=NO,                 MAGNETIC INK CHARACTER READERS X
               MPS=BJF,                 MULTIPROGRAMMING OPTION        X
               SYSTEM=DISK,             DISK OPERATING SYSTEM (DOS)    X
               TP=BTAM                  TELEPROCESSING OPTIONS
         SPACE 2
         CONFG DEC=YES,                 DECIMAL FEATURE                X
               FP=YES,                  FLOATING POINT FEATURE         X
               MODEL=40,                CPU MODEL/CLASS                X
               PORT=NO,                 NO PORTABILITY TO 370          X
               SP=YES,                  STORAGE PROTECT FEATURE        X
               TIMER=YES                CPU HAS CLOCK
         SPACE 2
         STDJC CHARSET=60C,             PL/I CHARACTER SET             X
               DATE=MDY,                FORMAT OF DATE (MDY OR YMD)    X
               DECK=NO,                 OBJECT TO SYSPCH               X
               DUMP=NO,                 AUTO-DUMP DUE TO ABEND         X
               ERRS=YES,                COMPILER ERRORS ON SYSLST      X
               LINES=56,                LINES PER PAGE ON SYSLST       X
               LIST=YES,                SOURCE LISTINGS ON SYSLST      X
               LISTX=NO,                OBJECT LISTINGS ON SYSLST      X
               LOG=YES,                 CONTROL STATEMENTS ON SYSLST   X
               SPARM=YES,               SYSPARM=                       X
               SYM=NO,                  SYMBOL & OFFSET LIST ON SYSLST X
               XREF=YES                 CROSS REFERENCE ON SYSLST       
         SPACE 2
         FOPT  AB=YES,                  ABEND EXIT FUNCTIONS           X
               CBF=NO,                  CONSOLE BUFFERING              X
               CCHAIN=YES,              RETRY I/O AT FAILING CCW       X
               CE=800,                  CE AREA (800 FOR PDAIDS)       X
               DASDFP=(1,2,2321),       DASD FILE PROTECT              X
               EVA=NO,                  TAPE ERROR VOLUME ANALYSIS     X
               IDRA=YES,                INDEPENDENT DIR READ AREA      X
               IT=F2,                   INTERVAL TIMER OWNER           X
               JA=(12,12,12),           JOB ACCOUNTING MAX DEVICES     X
               JALIOCS=(72,0),          JOB ACCOUNTING                 X
               OC=YES,                  OPERATOR CONSOLE SUPPORT       X
               OLTEP=YES,               ONLINE TESTING FUNCTIONS       X
               PC=YES,                  STXIT PC SUPPORT               X
               PCIL=YES,                PRIVATE CORE IMAGE SUPPORT     X
               PTO=YES,                 PHYSICAL TRANSIENT OVERLAP     X
               RETAIN=NO,               2955 DEVICE SUPPORT            X
               SKSEP=NO,                DASD SEEK SEPARATION           X
               SYSFIL=(2314,1000,1000), SYSTEM FILES ON DISK           X
               TEB=8,                   TAPE ERROR BLOCKS              X
               TEBV=NO,                 TAPE ERROR BLOCKS BY VOLUME    X
               TRKHLD=8,                TRACK HOLD                     X
               WAITM=YES                WAIT MULTIPLE SUPPORT
         SPACE 2
         PIOCS BMPX=NO,                 BURST MODE DEVICES ON BYTE CH  X
               CHANSW=NO,               TAPE CHANNEL SWITCHING         X
               MRSLCH=NO,               MICR ON SELECTOR CHANNEL       X
               SELCH=YES,               SELECTOR CHANNEL SUPPORT       X
               TAPE=9                   SUPPORT 7 & 9 TRACK TAPE        
         SPACE 2
         ALLOC F1=44K,                   SIZE OF F1  (MAX IS 510K)     X
               F2=128K                   SIZE OF F2  (MAX IS 510K)
         SPACE 2
         IOTAB BGPGR=16,                MAX BG SYSXXX ASSGN'S          X
               F1PGR=16,                MAX F1 SYSXXX ASSGN'S          X
               F2PGR=16,                MAX F2 SYSXXX ASSGN'S          X
               IODEV=32,                NUMBER OF DEVICES SUPPORTED    X
               JIB=64,                  NUM OF JOB INFORMATION BLOCKS  X
               CHANQ=16                 NUM OF CHANNEL QUEUE ENTRIES    
         SPACE 2
* BG PARTITION UNIT RECORD DEVICES
         DVCGEN CHUN=X'00C',DVCTYP=2501
         DVCGEN CHUN=X'00D',DVCTYP=2540P
         DVCGEN CHUN=X'00E',DVCTYP=1403
         SPACE 2
* F1 PARTITION UNIT RECORD DEVICES
         DVCGEN CHUN=X'01C',DVCTYP=2501
         DVCGEN CHUN=X'01D',DVCTYP=2540P
         DVCGEN CHUN=X'01E',DVCTYP=1403
         SPACE 2
* SYSTEM CONSOLE
         DVCGEN CHUN=X'01F',DVCTYP=1050A
         SPACE 2
* F2 PARTITION UNIT RECORD
         DVCGEN CHUN=X'02C',DVCTYP=2501
         DVCGEN CHUN=X'02D',DVCTYP=2540P
         DVCGEN CHUN=X'02E',DVCTYP=1403
         SPACE 2
* DISTRIBUTION SYSTEM DEVICES; GENERATED JUST IN CASE
         DVCGEN CHUN=X'130',DVCTYP=2311
         DVCGEN CHUN=X'131',DVCTYP=2311
         DVCGEN CHUN=X'132',DVCTYP=2311
         DVCGEN CHUN=X'133',DVCTYP=2311
         SPACE 2
* PRODUCTION DISK DEVICES
         DVCGEN CHUN=X'190',DVCTYP=2314  DOSRES
         DVCGEN CHUN=X'191',DVCTYP=2314  SPOOL1 - POWER
         DVCGEN CHUN=X'192',DVCTYP=2314  WRK14A - BG WORK FILES
         DVCGEN CHUN=X'193',DVCTYP=2314  WRK14B - F2 WORK FILES
         DVCGEN CHUN=X'194',DVCTYP=2314  OPTIONAL - TBD
         DVCGEN CHUN=X'195',DVCTYP=2314  OPTIONAL - TBD
         DVCGEN CHUN=X'196',DVCTYP=2314  OPTIONAL - TBD
         DVCGEN CHUN=X'197',DVCTYP=2314  OPTIONAL - TBD
         SPACE 2
* PRODUCTION TAPE DEVICES
         DVCGEN CHUN=X'280',DVCTYP=2400T9
         DVCGEN CHUN=X'281',DVCTYP=2400T9
         DVCGEN CHUN=X'282',DVCTYP=2400T9
         DVCGEN CHUN=X'283',DVCTYP=2400T9
         DVCGEN CHUN=X'284',DVCTYP=2400T9
         DVCGEN CHUN=X'285',DVCTYP=2400T9
         DVCGEN CHUN=X'286',DVCTYP=2400T9
         DVCGEN CHUN=X'287',DVCTYP=2400T9
         SPACE 2
* DEFAULT LOGICAL DEVICE ASSIGNMENTS
         SPACE 
* GLOBAL
         ASSGN SYSLOG,X'01F'
         ASSGN SYSLNK,X'192'
         SPACE 
* BG
         ASSGN SYSRDR,X'00C',BG
         ASSGN SYSIPT,X'00C',BG
         ASSGN SYSPCH,X'00D',BG
         ASSGN SYSLST,X'00E',BG
         ASSGN SYS001,X'192',BG   COMPILER WORKFILE
         ASSGN SYS002,X'192',BG   COMPILER WORKFILE
         ASSGN SYS003,X'192',BG   COMPILER/SORT WORKFILE
         ASSGN SYS004,X'192',BG   REQ'D FOR FCOBOL, USED FOR SORT
         ASSGN SYS005,X'192',BG   SORT WORKFILE
         SPACE 
* F2 - VALID ONLY WHEN POWER RUNNING
         ASSGN SYSRDR,X'02C',F2
         ASSGN SYSIPT,X'02C',F2
         ASSGN SYSPCH,X'02D',F2
         ASSGN SYSLST,X'02E',F2
         ASSGN SYS001,X'193',F2   COMPILER WORKFILE
         ASSGN SYS002,X'193',F2   COMPILER WORKFILE
         ASSGN SYS003,X'193',F2   COMPILER/SORT WORKFILE
         ASSGN SYS004,X'193',F2   REQ'D FOR FCOBOL, USED FOR SORT
         ASSGN SYS005,X'193',F2   SORT WORKFILE
         SPACE 2
* END OF SUPERVISOR - 24K
         SPACE
         SEND  24576
 END
/*
// EXEC ASSEMBLY
 PUNCH '/*'   AVOIDS ATTN 280 ON LINK STEP
 END
/*
// RESET SYSPCH
MTC WTM,X'280',2
MTC REW,X'280'
/& 
