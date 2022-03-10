//G07STG1  JOB  1,CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:      G07STG1                                            ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Run SYSGEN stage 1 for VS1 6.0.                    ***
//*    Run on:   Generating system                                  ***
//*    Update:   2020/09/03                                         ***
//*                                                                 ***
//*********************************************************************
//*
//*
//*  Consoles:
//*
//*   | CUA | Devt | Type             | Alternate   |
//*   |-----+------+------------------+-------------|
//*   |     |      |                  |             |
//*   | 01F | 3277 | Master           | 010         |
//*   | 010 | 3215 | Secons           | 01F         |
//*
//*
//*  Card readers/Punchers:
//*
//*  |         | Channel 0 | Channel 1 | Channel 2 | Channel 3 |
//*  | Devt    | Cua - Cua | Cua - Cua | Cua - Cua | Cua - Cua |
//*  |---------+-----------+-----------+-----------+-----------|
//*  | 2540R   | 00C       | 10C       |           |           |
//*  | 2540P   | 00D       | 10D       |           |           |
//*
//*
//*  Printers:
//*
//*  |         | Channel 0 | Channel 1 | Channel 2 | Channel 3 |
//*  | Devt    | Cua - Cua | Cua - Cua | Cua - Cua | Cua - Cua |
//*  |---------+-----------+-----------+-----------+-----------|
//*  | 1403    | 00E       | 10E       |           |           |
//*
//*
//*  Display terminals (including display consoles)
//*
//*  |         | Channel 0 | Channel 1 | Channel 2 | Channel 3 |
//*  | Devt    | Cua - Cua | Cua - Cua | Cua - Cua | Cua - Cua |
//*  |---------+-----------+-----------+-----------+-----------|
//*  | 3277-2  | 010       |           |           |           |
//*  | 3277-2  | 0C0 - 0C7 |           |           |           |
//*
//*
//*  Disk devices
//*
//*  |         | Channel 0 | Channel 1 | Channel 2 | Channel 3 |
//*  | Devt    | Cua - Cua | Cua - Cua | Cua - Cua | Cua - Cua |
//*  |---------+-----------+-----------+-----------+-----------|
//*  | 2314    |           | 130 - 13F | 230 - 23F | 330 - 33F |
//*  | 3350    |           | 140 - 14F | 240 - 24F | 340 - 34F |
//*  | 3330    |           | 150 - 157 | 250 - 257 | 350 - 357 |
//*  | 3330-11 |           | 158 - 15F | 258 - 25F | 358 - 35F |
//*  | 3340    |           | 1C0 - 1CF | 2C0 - 2CF | 3C0 - 3CF |
//*  |         |           |  Shared   |  Shared   |  Shared   |
//*  |         |           |  DASD     |  DASD     |  DASD     |
//*
//*
//*  Tape devices
//*
//*  |         | Channel 0 | Channel 1 |
//*  | Devt    | Cua - Cua | Cua - Cua |
//*  |---------+-----------+-----------|
//*  | 3420    |           | 180 - 18F |
//*
//*
//*  Telecommunications devices
//*
//*  |         | Channel 0 |
//*  | Devt    | Cua - Cua |
//*  |---------+-----------|
//*  | 2740    | 01D - 01E |
//*  | BSC1    | 020 - 027 |
//*  | BSC2    | 030 - 037 |
//*  | BSC3    | 040 - 047 |
//*  | 2741P   | 050       |
//*  | 2741C   | 052       |
//*  | TWX     | 058       |
//*  | WTTA    | 05C       |
//*  | 3705    | 060       |
//*  | 3705    | 068       |
//*  | 3791L   | 070       |
//*
//*
/*JOBPARM LINECT=0
//ASM      EXEC PGM=IFOX00,PARM=(DECK,NOOBJ)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.AGENLIB,VOL=SER=FDLB60,UNIT=SYSDA
//         DD  DISP=SHR,DSN=SYS1.AMODGEN,VOL=SER=FDLB60,UNIT=SYSDA
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2   DD  DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3   DD  DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT DD  SYSOUT=A
//*SYSPRINT DD  DSN=&&LISTING,DISP=(NEW,PASS),UNIT=SYSDA,
//*             SPACE=(TRK,(90,30),RLSE),
//*             DCB=(RECFM=FB,LRECL=121,BLKSIZE=1210)
//*SYSPUNCH DD  UNIT=00D
//SYSPUNCH DD  SYSOUT=B
//*SYSPUNCH DD  DUMMY
//*SYSPUNCH DD  DSN=&&OBJSET,UNIT=SYSDA,
//*             SPACE=(80,(200,50)),DISP=(MOD,PASS)
//SYSIN    DD  *
     TITLE 'VS1 Release 6.0 stage 1'
***********************************************************************
*                                                                     *
*        VS1 6.0 stage 1.                                             *
*                                                                     *
***********************************************************************
     PRINT ON,NOGEN,NODATA
     EJECT ,
***********************************************************************
*                                                                     *
*        Processor options.                                           *
*                                                                     *
*        "CTIMERS=INCLUDE" and "CS=YES" are required.                 *
*                                                                     *
***********************************************************************
     SPACE 1
     CENPROCS                                                          +
               MODEL=168R,                 Model (needed for 6.0)      +
               CTIMERS=INCLUDE,            Extended timer support      +
               INSTSET=UNIV,               Universal instruction set   +
               CS=YES                      Compare and swap
     EJECT ,
***********************************************************************
*                                                                     *
*        Control program options.                                     *
*                                                                     *
***********************************************************************
     SPACE 1
     CTRLPROG                                                          +
               ASCII=INCLUDE,              Include ASCII SVC           +
               AUTO=(WARM,LIST),           Auto IPL member             +
               FETCH=PCI,                  PCI fetch                   +
               MAXIO=100,                  Simultaneous I/O requests   +
               OPTIONS=(BLDL,              Fixed BLDL table            +
               NODDR,                      No DDR support              +
               NODDRSYS,                   No DDR support for sysres   +
               RER,                        Reduced error recovery      +
               TRSVCTBL),                  Resident SVC TTR table      +
               OVERLAY=ADVANCED,           Advanced overlay support    +
               RESIDNT=(ACSMETH,           Access methods are resident +
               ERP,                        Allow RERP response at IPL  +
               RENTCODE,                   Modules can be resident     +
               TRSVC),                     T3/4 SVCs can be resident   +
               SECURTY=FPROT,              Fetch protection            +
               SYSQUE=64,                  SQA size in K               +
               TRACE=100,                  System trace table size     +
               TZ=(W,0),                   Pretend to be UTC           +
               VIRTUAL=16384,              Virtual storage size        +
               VSAM=INCLUDE,               VSAM in pagable supervisor  +
               WAIT=300,                   Job wait time               +
               WARN=0                      No power warning feature
     EJECT ,
***********************************************************************
*                                                                     *
*        Partitions.                                                  *
*                                                                     *
*        The maximum number of problem program partitions are         *
*        generated, but are mostly left inactive until needed.        *
*                                                                     *
***********************************************************************
     SPACE 1
     PARTITNS                                                          +
               P00(C-*,S-128),                                         +
               P01(C-A,S-3072),                                        +
               P02(C-A,S-3072),                                        +
               P03(C-A,S-3072),                                        +
               P04(C-A,S-3072),                                        +
               P05(C-A,S-0000),                                        +
               P06(C-A,S-0000),                                        +
               P07(C-A,S-0000),                                        +
               P08(C-A,S-0000),                                        +
               P09(C-A,S-0000),                                        +
               P10(C-A,S-0000),                                        +
               P11(C-A,S-0000),                                        +
               P12(C-A,S-0000),                                        +
               P13(C-A,S-0000),                                        +
               P14(C-A,S-0000),                                        +
               P15(C-A,S-0000)
     EJECT ,
***********************************************************************
*                                                                     *
*        Scheduler options.                                           *
*                                                                     *
***********************************************************************
     SPACE 1
     SCHEDULR                                                          +
               ALTCONS=010,                Master console alternate    +
               BCLMT=1,                    Notices messages            +
               CONSOLE=01F,                Master console address      +
               ESV=NO,                     No volume error statistics  +
               HARDCPY=(SYSLOG,ALL,CMDS),  hardcopy                    +
               IOC=01F,                    Console for DSS             +
               JOBQLMT=5,                  Queue records per init      +
               JOBQLST=4,                  Job list entries per init   +
               JOBQTMT=15,                 Reserved job queue records  +
               OLDWTOR=2,                  Default routing code        +
               OPTIONS=(VM,                Include VM support          +
               EXIT,                       WTO exit                    +
               REMOTE,                     Include RES                 +
               MCS),                       Include MCS                 +
               REPLY=40,                   Number of reply elements    +
               ROUTCDE=(3,4,5,6,7,8,9,10,12,13,14,15,16),              +
               SMF=FULL,                   Include SMF                 +
               SYSWFMT=12,                 Queue records per log track +
               SYSWTMT=90,                 Queue records reserved      +
               WTLCLSS=A,                  Write-to-log output class   +
               WTOBFRS=200                 WTO buffers
     EJECT ,
***********************************************************************
*                                                                     *
*        Job entry subsystem defaults.                                *
*                                                                     *
***********************************************************************
     SPACE 1
     JES       ALCUNIT=28138,              Allocation units            +
               BUFSIZE=3952,               JES buffer size             +
               JOBLOG=YES,                 Create job log              +
               JOBQEXT=200,                Each job list extension     +
               JOBQINT=400,                Resident job list space     +
               JOBQNXT=10,                 Job list extensions         +
               JOBQVOL=FGEN60,             Job queue volume            +
               JOUTLIM=0,                  Default OUTLIM              +
               LPRPARM=PA,                 Default SF printer parm     +
               LPUPARM=CB,                 Default SF punch parm       +
               LRDPARM=00600300005011E00011A,  Default SF reader parms +
               NUMBUFS=50,                 Number of JES buffers       +
               PRLRECL=133,                SF LRECL for local printer  +
               RDR=(R=5,Y=50,B=9600,A=1,N=42), Reader defaults         +
               SPOLCAP=80,                 Spool warn percentage       +
               SPOLVOL=FGEN60,             Volume containing spool     +
               STEPWTP=25,                 WTPs per step               +
               SWDSLMT=15,                 SWA reserved for termination+
               WTLRCDS=3000,               WTL switch limit            +
               WTR=(W=8,U=3,Z=50,B=4890)   Writer defaults
     EJECT ,
***********************************************************************
*                                                                     *
*        Linkage editor and loader defaults.                          *
*                                                                     *
***********************************************************************
     SPACE 1
     EDITOR                                                            +
               SIZE=(512,100)
     LOADER                                                            +
               LIB=SYSLIB,                                             +
               LIN=SYSLIN,                                             +
               PARM=(PRINT,NOMAP,NOLET,CALL,NORES),                    +
               PRINT=SYSLOUT,                                          +
               SIZE=512
     EJECT ,
***********************************************************************
*                                                                     *
*        Data management.                                             *
*                                                                     *
*        All optional access methods are included.                    *
*                                                                     *
***********************************************************************
     SPACE 1
     DATAMGT                                                           +
               ACSMETH=(VTAM,TCAM,BTAM,ISAM)
         EJECT ,
***********************************************************************
*                                                                     *
*        SVC table.                                                   *
*                                                                     *
***********************************************************************
     SPACE 1
     SVCTABLE                                                          +
               SVC-255-D1-S0,    AVAILABLE                             +
               SVC-214-E4-S0     AVAILABLE
     EJECT ,
***********************************************************************
*                                                                     *
*        Secondary consoles.                                          *
*                                                                     *
***********************************************************************
     SPACE 1
     SECONSLE  CONSOLE=010,ALTCONS=01F,AREA=19,                        +
                    ROUTCDE=ALL,VALDCMD=(1,2,3)
     EJECT ,
***********************************************************************
*                                                                     *
*        Channels.                                                    *
*                                                                     *
***********************************************************************
     SPACE 1
     CHANNEL   ADDRESS=(0),TYPE=MULTIPLEXOR
*    CHANNEL   ADDRESS=(1,2,3),TYPE=BLKMPXR
     CHANNEL   ADDRESS=(1,2,3),TYPE=SELECTOR
     EJECT ,
***********************************************************************
*                                                                     *
*        Devices.                                                     *
*                                                                     *
***********************************************************************
     SPACE 1
*--------------------------------------------------------------------*
*        3215 printer/keyboard console devices.                      *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(01F,1),UNIT=3215
     SPACE 1
*--------------------------------------------------------------------*
*        2540 reader/punch devices.                                  *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(00C,1),UNIT=2540R,MODEL=1
     IODEVICE  ADDRESS=(00D,1),UNIT=2540P,MODEL=1
     IODEVICE  ADDRESS=(10C,1),UNIT=2540R,MODEL=1
     IODEVICE  ADDRESS=(10D,1),UNIT=2540P,MODEL=1
     SPACE 1
*--------------------------------------------------------------------*
*        1403 printer devices.                                       *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(00E,1),UNIT=1403,MODEL=N1
     IODEVICE  ADDRESS=(01E,1),UNIT=1403,MODEL=N1
     SPACE 1
*--------------------------------------------------------------------*
*        3277 secondary console devices.                             *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(010,1),UNIT=3277,MODEL=2,                      +
               FEATURE=(AUDALRM,EBKY3277,NUMLOCK,DOCHAR,KB78KEY)
     SPACE 1
*--------------------------------------------------------------------*
*        Non-console 3270 devices.                                   *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(0C0,8),UNIT=3277,MODEL=2,                      +
               FEATURE=(AUDALRM,EBKY3277,NUMLOCK,DOCHAR,KB78KEY)
     SPACE 1
*--------------------------------------------------------------------*
*        Telecommunications devices.                                 *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=01D,UNIT=2740,TCU=2701,ADAPTER=IBM1,            +
               FEATURE=(CHECKING)
     IODEVICE  ADDRESS=(020,8),UNIT=BSC1,ADAPTER=BSCA,TCU=2703
     IODEVICE  ADDRESS=(030,8),UNIT=BSC2,ADAPTER=BSCA,TCU=2703,        +
               FEATURE=(AUTOCALL,AUTOANSR)
     IODEVICE  ADDRESS=(040,8),UNIT=BSC3,ADAPTER=BSCA,TCU=2703,        +
               FEATURE=(AUTOPOLL)
     IODEVICE  ADDRESS=(050,1),UNIT=2741P,ADAPTER=IBM1,TCU=2703,       +
               FEATURE=(AUTOANSR)
     IODEVICE  ADDRESS=(052,1),UNIT=2741C,ADAPTER=IBM1,TCU=2703,       +
               FEATURE=(AUTOANSR)
     IODEVICE  ADDRESS=(058,1),UNIT=TWX,ADAPTER=TELE2,TCU=2703,        +
               FEATURE=(AUTOANSR)
     IODEVICE  ADDRESS=(05C,1),UNIT=WTTA,ADAPTER=TELEW,TCU=2703,       +
               FEATURE=(AUTOANSR)
     IODEVICE  ADDRESS=(060,1),UNIT=3705,ADAPTER=CA1
     IODEVICE  ADDRESS=(068,1),UNIT=3705,ADAPTER=CA2
     IODEVICE  ADDRESS=(070,1),UNIT=3791L
     SPACE 1
*--------------------------------------------------------------------*
*        2314 disk drives.                                           *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(130,8),UNIT=2314,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(138,8),UNIT=2314,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(230,8),UNIT=2314,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(238,8),UNIT=2314,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(330,8),UNIT=2314,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(338,8),UNIT=2314,FEATURE=(SHARED)
     SPACE 1
*--------------------------------------------------------------------*
*        3350 disk drives.                                           *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(140,8),UNIT=3350,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(148,8),UNIT=3350,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(240,8),UNIT=3350,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(248,8),UNIT=3350,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(340,8),UNIT=3350,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(348,8),UNIT=3350,FEATURE=(SHARED)
     SPACE 1
*---------------------------------------------------------------------*
*        3330 disk drives.  Note that the string of 8 at 450          *
*        is not shared, page data sets are assumed to reside          *
*        on those devices.                                            *
*---------------------------------------------------------------------*
     IODEVICE  ADDRESS=(150,8),UNIT=3330,MODEL=1,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(158,8),UNIT=3330,MODEL=11,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(250,8),UNIT=3330,MODEL=1,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(258,8),UNIT=3330,MODEL=11,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(350,8),UNIT=3330,MODEL=1,FEATURE=(SHARED)
     IODEVICE  ADDRESS=(358,8),UNIT=3330,MODEL=11,FEATURE=(SHARED)
     SPACE 1
*--------------------------------------------------------------------*
*        3340 disk drives.  Note that the string of 8 at 4C0         *
*        is not shared, page data sets are assumed to reside         *
*        on those devices.                                           *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(1C0,8),UNIT=3340,FEATURE=(SHARED,RPS)
     IODEVICE  ADDRESS=(1C8,8),UNIT=3340,FEATURE=(SHARED,RPS)
     IODEVICE  ADDRESS=(2C0,8),UNIT=3340,FEATURE=(SHARED,RPS)
     IODEVICE  ADDRESS=(2C8,8),UNIT=3340,FEATURE=(SHARED,RPS)
     IODEVICE  ADDRESS=(3C0,8),UNIT=3340,FEATURE=(SHARED,RPS)
     IODEVICE  ADDRESS=(3C8,8),UNIT=3340,FEATURE=(SHARED,RPS)
     SPACE 1
*--------------------------------------------------------------------*
*        3420 tape drives.                                           *
*--------------------------------------------------------------------*
     IODEVICE  ADDRESS=(180,8),UNIT=3420,MODEL=8,FEATURE=(OPT1600)
     IODEVICE  ADDRESS=(188,8),UNIT=3420,MODEL=8,FEATURE=(OPT1600)
     EJECT ,
***********************************************************************
*                                                                     *
*        Esoterics.                                                   *
*                                                                     *
***********************************************************************
     SPACE 1
*--------------------------------------------------------------------*
*        Define TAPE esoteric group.                                 *
*--------------------------------------------------------------------*
     UNITNAME  NAME=TAPE,                                              +
               UNIT=((180,8),(188,8))    3420                          
     SPACE 1
*--------------------------------------------------------------------*
*        Define SYSDA esoteric group.                                *
*--------------------------------------------------------------------*
     UNITNAME  NAME=SYSDA,                                             +
               UNIT=((140,16),           3350                          +
               (240,16),                 3350                          +
               (340,16),                 3350                          +
               (150,16),                 3330                          +
               (250,16),                 3330                          +
               (350,16),                 3330                          +
               (1C0,16),                 3340                          +
               (2C0,16),                 3340                          +
               (3C0,16))                 3340
     SPACE 1
*--------------------------------------------------------------------*
*        Define SYSALLDA esoteric group.                             *
*--------------------------------------------------------------------*
     UNITNAME  NAME=SYSALLDA,                                          +
               UNIT=((140,16),           3350                          +
               (240,16),                 3350                          +
               (340,16),                 3350                          +
               (150,16),                 3330                          +
               (250,16),                 3330                          +
               (350,16),                 3330                          +
               (1C0,16),                 3340                          +
               (2C0,16),                 3340                          +
               (3C0,16))                 3340
     SPACE 1
*--------------------------------------------------------------------*
*        Define SORT esoteric group.                                 *
*--------------------------------------------------------------------*
     UNITNAME  NAME=SORT,                                              +
               UNIT=((130,8),(138,8),    2314                          +
               (230,8),(238,8),          2314                          +
               (330,8),(338,8))          2314
     SPACE 1
*--------------------------------------------------------------------*
*        Define SORTWORK esoteric group.                             *
*--------------------------------------------------------------------*
     UNITNAME  NAME=SORTWORK,                                          +
               UNIT=((130,8),(138,8),    2314                          +
               (230,8),(238,8),          2314                          +
               (330,8),(338,8))          2314
     SPACE 1
*--------------------------------------------------------------------*
*        Define SORTWK esoteric group.                               *
*--------------------------------------------------------------------*
     UNITNAME  NAME=SORTWK,                                            +
               UNIT=((130,8),(138,8),    2314                          +
               (230,8),(238,8),          2314                          +
               (330,8),(338,8))          2314
     EJECT ,
***********************************************************************
*                                                                     *
*        Universal character sets.                                    *
*                                                                     *
***********************************************************************
     SPACE 1
     UCS       IMAGE=(ALL1403,ALL3203,ALL3211),DEFAULT=ALL
     EJECT ,
***********************************************************************
*                                                                     *
*        Page data set support.  VS1 requires that each               *
*        device type that might be used for a page data               *
*        set be defined here.  The specified volsers are              *
*        dummies; the actual operational page data set                *
*        specification will be in placed in IEASYS00                  *
*        in SYS1.PARMLIB using IEBUPDTE.                              *
*                                                                     *
***********************************************************************
     SPACE 1
     PAGE      SIZE=(BLK,8192),VOLNO=P3330X,DEV=3330
     PAGE      SIZE=(BLK,8192),VOLNO=P33301,DEV=3330-1
     PAGE      SIZE=(BLK,8192),VOLNO=P3340X,DEV=3340
     PAGE      SIZE=(BLK,8192),VOLNO=P3350X,DEV=3350
         EJECT ,
***********************************************************************
*                                                                     *
*        Generation.                                                  *
*                                                                     *
***********************************************************************
     SPACE 1
     GENERATE                                                          +
               GENTYPE=ALL,                                            +
               INDEX=SYS1,                                             +
               JCLASS=A,                                               +
               OBJPDS1=SYS1.OBJPDS1,                                   +
               OBJPDS2=SYS1.OBJPDS2,                                   +
               OBJPDS3=SYS1.OBJPDS3,                                   +
               OCLASS=A,                                               +
               RESVOL=(FGEN60,3350)
     END   ,
/*
//*
//*PUNCH  EXEC PGM=IEBGENER
//*SYSPRINT DD SYSOUT=A
//*SYSUT2   DD SYSOUT=A,DCB=(RECFM=FB,LRECL=121,BLKSIZE=1210)
//*SYSUT1   DD DSN=&&LISTING,DISP=(OLD,DELETE)
//*SYSIN    DD DUMMY
//
