* $$ JOB JNM=SYSGEN,USER='SYSPROG',CLASS=0,DISP=D
* $$ LST LST=00E,FNO=0001,CLASS=A
// JOB     ASSEMBLY    - ASSEMBLE AND LNKEDT AN ALC PROGRAM 
*
* $$ NOTE              - GENERATE CUSTOMIZED SUPERVISOR - $$A$SUPA
*
// ASSGN SYSSLB,X'131'
// ASSGN SYSLNK,X'131'
// ASSGN SYS001,X'131'
// ASSGN SYS002,X'131'
// ASSGN SYS003,X'131'
// OPTION  CATAL
   ACTION  CLEAR
// EXEC    PGM=ASSEMBLY,SIZE=296K
         TITLE 'DOS/VS SUPERVISOR A,SYSTEM=(2314,MOD=145,NPARTS=5)'
***********************************************************************
*                                                                     *
*        SYSEND = X'20000' = 128K                                     *
*                                                                     *
***********************************************************************
         SPACE
* SUPERVISOR CONFIGURATION
         SUPVR                                                         X
               ID=A,               NAME SUFFIX - $$A$SUPA              X
               AP=YES,             MULTITASKING                        X
               ASCII=YES,          ASCII TAPE TRANSLATE                X
               ERRLOG=RDE,         RELIABILITY DATA EXTRACTOR          X
               EU=YES,             14XX EMULATOR                       X
               MICR=NO,            MAGNITIC INK CHARACTER READERS      X
               NPARTS=5,           NUMBER OF PARTITIONS                X
               POWER=YES,          POWER/VS SPOOLING                   X
               PAGEIN=24,          PAGING MACRO SUPPORT                X
               PHO=YES,            PAGE HANDLING OVERLAP               X
               TP=(BTAM,VTAM)      TELEPROCESSING OPTIONS
         SPACE
* HARDWARE CONFIGURATION 
         CONFG                                                         X
               FP=YES,             FLOATING POINT FEATURE              X
               MODEL=145           CPU MODEL/CLASS
         SPACE
* STANDARD JOB CONTROL DEFAULTS
         STDJC                                                         X
               ALIGN=YES,          HALF/FULL WORD ALLIGNMENT           X
               ACANCEL=NO,         AUTO-CANCEL DUE TO BAD ASSGN        X
               CHARSET=60C,        PL/I CHARACTER SET TRANSLATOR       X
               DATE=MDY,           FORMAT OF DATE (MDY OR YMD)         X
               DECK=NO,            OBJECT TO SYSPCH                    X
               EDECK=NO,           EDITED MACROS TO SYSPCH             X
               DUMP=YES,           AUTO-DUMP DUE TO ABEND              X
               ERRS=YES,           COMPILER ERRORS ON SYSLST           X
               LINES=60,           LINES PER PAGE ON SYSLST            X
               LIST=YES,           SOURCE LISTINGS ON SYSLST           X
               LOG=YES,            LIST CONTROL STATEMENTS ON SYSLST   X
               RLD=YES,            RELOCATION DICTIONARY ON SYSLST     X
               SPARM=YES,          &SYSPARM ASSEMBLER VARIABLE         X
               SYM=YES,            SYMBOL & OFFSET LIST ON SYSLST      X
               XREF=NO             CROSS-REFERENCE ON SYSLST
         SPACE
* OPTIONAL FEATURE SPECIFICATIONS 
         FOPT                                                          X
               AB=YES,             ABEND EXIT FUNCTIONS                X
               CBF=NO,             CONSOLE BUFFERING                   X
               DASDFP=(1,6),       DASD FILE PROTECT (CHANNELS 1-6)    X
               DOC=3277,           DISPLAY OPERATOR CONSOLE            X
               ECPREAL=YES,        VIRTUAL ADDRESSING MACRO SUPPORT    X
               ERRQ=25,            TELEPROCESSING ERROR QUEUES         X
               EVA=NO,             TAPE ERROR VOLUME ANALYSIS          X
               FASTTR=YES,         DASD FAST CCW TRANSLATE             X
               GETVIS=YES,         GETVIS STORAGE MANAGEMENT           X
               IDRA=YES,           FETCH/LOAD INDEPENDENT DIRECTORIES  X
               IT=YES,             INTERVAL TIMER SUPPORT              X
               JA=(64,64,64,64,16), J/A + SIO COUNTERS/PARTN           X
               JALIOCS=(1024,224), J/A USER LIOCS AREAS                X
               OC=YES,             OPERATOR CONSOLE SUPPORT            X
               OLTEP=YES,          ONLINE TESTING FUNCTIONS            X
               PC=YES,             STXIT PC SUPPORT                    X
               PCIL=YES,           PRIVATE CORE IMAGE SUPPORT          X
               PD=YES,             PROBLEM DETERMINATION AIDS          X
               PFIX=YES,           PFIX/PFREE SUPPORT                  X
               PSLD=12,            PRIVATE 2ND LEVEL DIRECTORIES       X
               RELLDR=YES,         RELOCATING LOADER SUPPORT           X
               RETAIN=NO,          2955 DEVICE SUPPORT                 X
               RPS=YES,            ROTATION POSITION SENSING           X
               SKSEP=YES,          DASD SEEK SEPARATION                X
               SLD=16,             SYSTEM 2ND LEVEL DIRECTORIES        X
               SYNCH=YES,          SYNCHRONOUS SVC SWAPPING            X
               SYSFIL=YES,         SYSTEM FILES ON DASD                X
               TEB=NO,             2495 TAPE ERROR STATISTICS          X
               TOD=YES,            TIME-OF-DAY CLOCK                   X
               TRKHLD=255,         DASD TRACK HOLD (PROTECTION)        X
               TTIME=F2,           TASK TIMER SUPPORT                  X
               USERID=,            IPL ID TEXT (16 BYTES)              X
               VSAM=YES,           VSAM SUPPORT                        X
               WAITM=YES,          WAIT MULTIPLE SUPPORT               X
               XECB=40,            CROSS-PARTITION EVENT CONTROL       X
               ZONE=NO             ZONE EAST/WEST OF GMT 
         SPACE
* PHYSICAL IOCS SPECIFICATIONS
         PIOCS                                                         X
               BLKMPX=YES,         BLOCK MULTIPLEXOR SUPPORT           X
               BMPX=YES,           BURST MODE DEVICES ON BYTE CHANNEL  X
               CHANSW=NO,          TAPE CHANNEL SWITCHING              X
               DISK=(3350),        SUPPORT 3350 DASD TYPES             X
               MRSLCH=NO,          MICR ON SELECTOR CHANNEL            X
               TAPE=7              SUPPORT 7 & 9 TRACK TAPES
         SPACE
* VIRTUAL STORAGE SPECIFICATIONS
         VSTAB                                                         X
               RSIZE=2048K,        REAL ADDRESS AREA (NOT MEMORY SIZE) X
               VSIZE=14336K,       VIRTUAL ADDRESS AREA (16 MB MAX)    X
               BUFSIZE=256,        CHANNEL PROGRAM TRANSLATION BUFFERS X
               SVA=(1024K,64K)     SHARED VIRTUAL STORAGE AREA
         SPACE
* DEFAULT VIRTUAL PARTITION ALLOCATIONS
         ALLOC                                                         X
               F1=512K,                                                X
               F2=4096K,                                               X
               F3=512K,                                                X
               F4=4096K
         SPACE
* DEFAULT REAL PARTITION ALLOCATIONS
         ALLOCR                                                        X
               F1R=48K,                                                X
               F2R=64K,                                                X
               F3R=64K,                                                X
               F4R=64K,                                                X
               BGR=64K
         SPACE
* I/O DEVICE CONTROL SPECIFICATIONS
         IOTAB                                                         X
               BGPGR=48,           MAX BG SYSXXX ASSGN'S               X
               CHANQ=255,          NUMBER OF CHANNEL QUEUES            X
               D2311=0,                                                X
               D2314=0,                                                X
               D3330=0,                                                X
               D3340=0,                                                X
               D3350=48,           MAX 3350 DASD DEVICES               X
               D3420=8,            MAX 3420 TAPE DRIVES                X
               D3800=2,            MAX 3800 LASER PRINTERS             X
               F1PGR=12,           MAX F1 SYSXXX ASSGN'S               X
               F2PGR=12,           MAX F2 SYSXXX ASSGN'S               X
               F3PGR=48,           MAX F3 SYSXXX ASSGN'S               X
               F4PGR=48,           MAX F4 SYSXXX ASSGN'S               X
               IODEV=254,          NUMBER OF DEVICES SUPPORTED         X
               JIB=255,            NUMBER OF JOB INFORMATION BLOCKS    X
               NRES=64             NUMBER OF RESOURCE USAGE RECORDS
         SPACE
* DEFAULT DEVICE DEFINITIONS
         DVCGEN CHUN=X'00C',DVCTYP=2540R
         DVCGEN CHUN=X'00D',DVCTYP=2540P
         DVCGEN CHUN=X'00E',DVCTYP=1403U
         DVCGEN CHUN=X'01E',DVCTYP=1403U
         DVCGEN CHUN=X'01F',DVCTYP=1050A
         DVCGEN CHUN=X'02E',DVCTYP=1403U
         DVCGEN CHUN=X'03E',DVCTYP=1403U
         SPACE
         DVCGEN CHUN=X'120',DVCTYP=2311
         DVCGEN CHUN=X'121',DVCTYP=2311
         DVCGEN CHUN=X'122',DVCTYP=2311
         DVCGEN CHUN=X'123',DVCTYP=2311
         DVCGEN CHUN=X'124',DVCTYP=2311
         DVCGEN CHUN=X'125',DVCTYP=2311
         DVCGEN CHUN=X'126',DVCTYP=2311
         DVCGEN CHUN=X'127',DVCTYP=2311
         SPACE
         DVCGEN CHUN=X'130',DVCTYP=2314
         DVCGEN CHUN=X'131',DVCTYP=2314
         DVCGEN CHUN=X'132',DVCTYP=2314
         DVCGEN CHUN=X'133',DVCTYP=2314
         DVCGEN CHUN=X'134',DVCTYP=2314
         DVCGEN CHUN=X'135',DVCTYP=2314
         DVCGEN CHUN=X'136',DVCTYP=2314
         DVCGEN CHUN=X'137',DVCTYP=2314
         SPACE
         DVCGEN CHUN=X'150',DVCTYP=3350
         DVCGEN CHUN=X'151',DVCTYP=3350
         DVCGEN CHUN=X'152',DVCTYP=3350
         DVCGEN CHUN=X'153',DVCTYP=3350
         DVCGEN CHUN=X'154',DVCTYP=3350
         DVCGEN CHUN=X'155',DVCTYP=3350
         DVCGEN CHUN=X'156',DVCTYP=3350
         DVCGEN CHUN=X'157',DVCTYP=3350
         SPACE
         DVCGEN CHUN=X'180',DVCTYP=2400T9
         DVCGEN CHUN=X'181',DVCTYP=2400T9
         DVCGEN CHUN=X'182',DVCTYP=2400T9
         DVCGEN CHUN=X'183',DVCTYP=2400T9
         DVCGEN CHUN=X'184',DVCTYP=2400T9
         DVCGEN CHUN=X'185',DVCTYP=2400T9
         DVCGEN CHUN=X'186',DVCTYP=2400T9
         DVCGEN CHUN=X'187',DVCTYP=2400T9
         SPACE
         DVCGEN CHUN=X'220',DVCTYP=2311
         DVCGEN CHUN=X'221',DVCTYP=2311
         DVCGEN CHUN=X'222',DVCTYP=2311
         DVCGEN CHUN=X'223',DVCTYP=2311
         DVCGEN CHUN=X'224',DVCTYP=2311
         DVCGEN CHUN=X'225',DVCTYP=2311
         DVCGEN CHUN=X'226',DVCTYP=2311
         DVCGEN CHUN=X'227',DVCTYP=2311
         SPACE
         DVCGEN CHUN=X'230',DVCTYP=2314
         DVCGEN CHUN=X'231',DVCTYP=2314
         DVCGEN CHUN=X'232',DVCTYP=2314
         DVCGEN CHUN=X'233',DVCTYP=2314
         DVCGEN CHUN=X'234',DVCTYP=2314
         DVCGEN CHUN=X'235',DVCTYP=2314
         DVCGEN CHUN=X'236',DVCTYP=2314
         DVCGEN CHUN=X'237',DVCTYP=2314
         SPACE
         DVCGEN CHUN=X'250',DVCTYP=3350
         DVCGEN CHUN=X'251',DVCTYP=3350
         DVCGEN CHUN=X'252',DVCTYP=3350
         DVCGEN CHUN=X'253',DVCTYP=3350
         DVCGEN CHUN=X'254',DVCTYP=3350
         DVCGEN CHUN=X'255',DVCTYP=3350
         DVCGEN CHUN=X'256',DVCTYP=3350
         DVCGEN CHUN=X'257',DVCTYP=3350
         SPACE
         DVCGEN CHUN=X'440',DVCTYP=3277
         DVCGEN CHUN=X'441',DVCTYP=3277
         DVCGEN CHUN=X'442',DVCTYP=3277
         DVCGEN CHUN=X'443',DVCTYP=3277
         DVCGEN CHUN=X'444',DVCTYP=3277
         DVCGEN CHUN=X'445',DVCTYP=3277
         DVCGEN CHUN=X'446',DVCTYP=3277
         DVCGEN CHUN=X'447',DVCTYP=3277
         SPACE
* DEFAULT LOGICAL DEVICE ASSIGNMENTS
         ASSGN SYSLOG,X'01F'
         ASSGN SYSREC,X'131'
         ASSGN SYSCAT,X'131'       VSAM MASTER CATALOG
         ASSGN SYSRDR,X'00C',BG
         ASSGN SYSIPT,X'00C',BG
         SPACE
* PAGE DATASET DEFINITION
         DPD                                                           X
               UNIT=X'132',        DPD DEVICE                          X
               VOLID=SYSDPD,       DPD VOLID                           X
               CYL=1               DPD LOCATION
         SPACE
         SEND
         END
/*
// EXEC    PGM=LNKEDT
/*
/&
* $$ EOJ
