//VTPUTIL  JOB 2,'DEFINES',MSGLEVEL=(1,1),MSGCLASS=A,CLASS=A
//S0      EXEC PGM=IDCAMS
//PERM73   DD  UNIT=SYSDA,VOL=SER=PERM73,DISP=SHR
//SYSPRINT DD SYSOUT=A
//SYSIN    DD  *
 DELETE (U000.TPLOG.FILE) -
      CLUSTER -
      PURGE -
      CATALOG (SYS1.FGEN67.CATALOG)
    SET LASTCC = 0
    SET MAXCC  = 0
 DEFINE CLUSTER ( -
        NAME (U000.TPLOG.FILE) -
        RECORDS (2000) -
        RECORDSIZE (316    316) -
        VOLUMES (PERM73 ) -
        SHAREOPTION (4) -
        REUSE -
        NUMBERED -
        FREESPACE (0 0) -
        TO (99366)) -
        DATA (NAME (U000.TPLOG.FILE.@D@) -
        CONTROLINTERVALSIZE (4096)) -
        CATALOG (SYS1.FGEN67.CATALOG)
 DELETE (U000.TEMP.STORE.FILE) -
      CLUSTER -
      PURGE -
      CATALOG (SYS1.FGEN67.CATALOG)
 DEFINE CLUSTER ( -
        NAME (U000.TEMP.STORE.FILE) -
        RECORDS (2000) -
        RECORDSIZE (1000 1000) -
        VOLUMES (PERM73 ) -
        SHAREOPTION (4) -
        REUSE -
        NUMBERED -
        FREESPACE (0 0) -
        TO (99366)) -
        DATA (NAME (U000.TEMP.STORE.FILE.@D@) -
        CONTROLINTERVALSIZE (4096)) -
        CATALOG (SYS1.FGEN67.CATALOG)
    SET LASTCC = 0
    SET MAXCC  = 0
 DELETE (U000.SECURITY.FILE) -
      CLUSTER -
      PURGE -
      CATALOG (SYS1.FGEN67.CATALOG)
    SET LASTCC = 0
    SET MAXCC  = 0
 DEFINE CLUSTER ( -
        NAME (U000.SECURITY.FILE) -
        RECORDS (100) -
        RECORDSIZE (600 600) -
        VOLUMES (PERM73) -
        REUSE -
        SHAREOPTION (4) -
        INDEXED -
        FREESPACE (15 7) -
        KEYS (06 1) -
        TO (99366)) -
        DATA (NAME (U000.SECURITY.FILE.@D@) -
        CONTROLINTERVALSIZE (4096)) -
        INDEX (NAME (U000.SECURITY.FILE.@I@)) -
        CATALOG (SYS1.FGEN67.CATALOG)
/*
//S1      EXEC PGM=VTPUTIL
//STEPLIB  DD  DSN=SYS3.LINKLIB,DISP=SHR
//FILE00   DD  DSN=U000.TPLOG.FILE,DISP=SHR
//TPTS00   DD  DSN=U000.TEMP.STORE.FILE,DISP=SHR
//SYSOUT   DD  SYSOUT=A
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  *
*
*      FORMAT SYSTEM FILES
*
       FORMAT LOG
       FORMAT TS
/*
//S1      EXEC PGM=VTPUTIL
//STEPLIB  DD  DSN=SYS3.LINKLIB,DISP=SHR
//TPTS00   DD  DSN=U000.TEMP.STORE.FILE,DISP=SHR
//FILE00   DD  DSN=U000.TPLOG.FILE,DISP=SHR
//SYSOUT   DD  SYSOUT=A
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  *
*
*      FORMAT SYSTEM FILES
*
       FORMAT LOG
*
*      DEFINE SYSTEM FILES
*
       DEFINE FILE(00) TYPE(RRDS) OPEN(YES)
       DEFINE FILE(01) TYPE(KSDS) OPEN(YES)
*
*--------------------------------------------------------------------*
*      DEFINE LINE USAGE                                             *
*--------------------------------------------------------------------*
*
       DEFINE LINE(10) TYPE(L3270) OPEN(YES) CLIENT(100)
       DEFINE LINE(11) TYPE(L3270) OPEN(YES) CLIENT(200)
*
*      DEFINE MASTER MESSAGES
*
       DEFINE MESSAGE(01) -
         ('This is a sample message to be displayed during sign-on')
       DEFINE MESSAGE(02) -
         ('and sign-off.  This message can have any text or information')
*
*      LIST UTILITY
*
       LIST LOG
       LIST LG
       LIST DS
/*
//S2     EXEC PGM=OSS99
//STEPLIB DD  DSN=SYS3.LINKLIB,DISP=SHR
//FILE01  DD  DSN=U000.SECURITY.FILE,DISP=SHR
//SYSOUT  DD  SYSOUT=A
//SYSPRT  DD  SYSOUT=A
//SYSIPT  DD *
A000000HERC01 CUL8TR 000100002
A000000HERC02 CUL8TR 000200002
A000000HERC03 PASS4U 000100100
A000000HERC04 PASS4U 000200100
A000000COZZIE COZZIE 000300002*
A000001This application request program provides for programmers
B000001interface to programmer tools to develope applications
C000001for this system.  You have access to the libraries,
D000001catalog, list and reader queues via request selection.
E000001All requests will return to the base screen, this also
F000001includes (Help).  Using the Base and Return process is
G000001used throughout the system.  Any additional comments or
H000001help can be added through the Help file maintenance
I000001program.
J0000010000
A000002These series of programs provide for user access to files
B000002records and reports, which enable the record keeping
C000002activies for application systems.  The security level for
D000002each operator is determined by the client, to aid in the
E000002management of system resources.  Each application has a
F000002users guide, that can be refered to for more specific
G000002help and instruction.
H000002
I000002
J0000020000
A000003This program will provide an interface to the Accounts
B000003Receivable System.  Functions are:
C000003
D000003       (C) Customer Inquire   (I) Invoice
E000003       (L) Customer History   (H) Client Data
F000003
G000003Other file maintenance functions (refer to User's Guide).
H000003
I000003
J0000030000
A000004This program will provide an interface to the Accounts
B000004Payable System.  Functions are:
C000004
D000004       (V) Vendor data        (I) Invoice
E000004       (C) Client data        (L) Vendor history
F000004
G000004Other file maintenance functions (refer to User's Guide).
H000004
I000004
J0000040000
A000005This program will provide an interface to the Payroll
B000005System.  Functions are:
C000005
D000005       (E) Employee data      (T) Time Entry
E000005       (C) Client data        (L) Employee history
F000005
G000005Other file maintenance functions (refer to User's Guide).
H000005
I000005
J0000050000
A000006This program will provide an interface to the Fixed Assets
B000006System.  Functions are:
C000006
D000006       (A) Asset data          (AS) Asset Summary
E000006       (C) Client data         (L)  Asset history
F000006
G000006Other file maintenance functions (refer to User's Guide).
H000006
I000006
J0000060000
A000007This program will provide an interface to the Inventory
B000007System.  Functions are:
C000007
D000007       (I) Inventory data      (IS) Inventory summary
E000007       (C) Client data         (L)  Inventory history
F000007
G000007Other file maintenance functions (refer to User's Guide).
H000007
I000007
J0000070000
A000008This program will provide an interface to the Security/Help
B000008Maintenance System.  Functions are:
C000008
D000008       (CN) New Security       (CC) Change Security
E000008       (C)  Security Inquiry   (CD) Delete Security
F000008       (CR) Reset Security     (CS) Client Summary
G000008       (HM) Help Maintenance
H000008
I000008
J0000080000
A000009This program will provide an interface to the General Ledger
B000009System.  Functions are:
C000009
D000009       (A) Account data        (AS) Account summary
E000009       (C) Client data         (L)  Account history
F000009
G000009Other file maintenance functions (refer to User's Guide).
H000009
I000009
J0000090000
A000010This program will provide an interface to the Stock Management
B000010System.  Functions are:
C000010
D000010       (I) Inquiry             (C)  Account change
E000010       (T) Transfer            (L)  Account history
F000010
G000010Other file maintenance functions (refer to User's Guide).
H000010
I000010
J0000100000
A000020This program will provide for programmer development using a
B000020full screen editor, commands are:
C000020
D000020       LIB  Display Directory of Programs
E000020       ED   Edit member into work space
F000020       SUB  Submit work space to Power reader
G000020       TAB  Set tab locations
H000020       COPY copy data into work space
I000020
J0000200000
A000100ABC COMPANY, INC    29751 US HWY 50 E
B000100JOE DOKES
C000100DENVER        CO80006
D000100HERC01 GARY  T
A000101ABC COMPANY, INC    29751 US HWY 50 E
B000101JIMMY JOHNSON
C000101MOBIL         ABO81006
D000101HERC02 GARY  P
A000200COZZIE'S SORTS B&G  29701 US HWY 50 E
B000200GARY COZZOLINO
C000200PUEBLO        CO81006
D000200COZZIE COZZ  T
A000201COZZIE'S SORTS B&G  29701 US HWY 50 E
B000201GARY COZZOLINO
C000201PUEBLO        CO81006
D000201COZZIE COZZ  T
A999999COZZIE'S SORTS B&G  29701 US HWY 50 E
B999999GARY COZZOLINO
C999999PUEBLO        CO81006
D999999PASS4U HERC04L
/*
//S3      EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=A,OUTLIM=0
//SYSUT1   DD DSN=SYS1.PROCLIB,DISP=SHR
//SYSUT2   DD DSN=SYS1.PROCLIB,DISP=SHR
//SYSIN    DD DATA,DLM=##
./ ADD NAME=VTP
//VTP     PROC
//TP      EXEC PGM=VTP$000,TIME=1440
//STEPLIB  DD  DSN=SYS3.LINKLIB,DISP=SHR
//STEPCAT  DD  DSN=SYS1.FGEN67.CATALOG,DISP=SHR
//TPFILE00 DD  DSN=U000.TPLOG.FILE,DISP=SHR
//TPFILE01 DD  DSN=U000.SECURITY.FILE,DISP=SHR
//SYSPRINT DD  DSN=&STATS,DISP=(,PASS),UNIT=SYSDA,
//             DCB=BLKSIZE=121,SPACE=(TRK,5)
//TPLIN10  DD  UNIT=0C0
//TPLIN11  DD  UNIT=0C1
//DD1      DD  UNIT=SYSDA,VOL=SER=FGEN67,DISP=OLD
//DD2      DD  UNIT=SYSDA,VOL=SER=PERM73,DISP=OLD
//DD3      DD  UNIT=SYSDA,VOL=SER=WORK73,DISP=OLD
//SYSUDUMP DD  SYSOUT=A
//SYSLIST  DD  SYSOUT=A
//SYSTATS  DD  SYSOUT=A
//SYSUDUMP DD  SYSOUT=A
//JCL      DD  DSN=SYS2.RDRFILE,DISP=SHR
./     ADD NAME=COB2
//        PROC MBR=DUMMY,SOUT=A,PRM=DMAP
//COB     EXEC PGM=IEQCBL00,PARM='SIZE=400000,CLIST,&PRM',
//             REGION=768K             
//SYSPRINT  DD SYSOUT=&SOUT,DCB=(RECFM=FBA,LRECL=121,BLKSIZE=968)
//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,2))
//SYSUT2    DD UNIT=SYSDA,SPACE=(CYL,(10,2))
//SYSUT3    DD UNIT=SYSDA,SPACE=(CYL,(10,2))
//SYSUT4    DD UNIT=SYSDA,SPACE=(CYL,(10,2))
//SYSPUNCH  DD SYSOUT=A,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)
//SYSLIN    DD DSN=&&LOADSET,DISP=(,PASS),
//             UNIT=SYSDA,SPACE=(CYL,(10,2)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSLIB    DD DSN=SYS2.COPYLIB,DISP=SHR
//LKED    EXEC PGM=IEWL,
//             PARM='MAP,LIST,LET',
//             COND=(8,LT,COB)
//SYSLIN    DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//          DD DDNAME=SYSIN
//SYSLMOD   DD DSN=SYS2.LINKLIB(&MBR),DISP=SHR
//SYSLIB    DD DSN=SYS1.COBLIB,DISP=SHR
//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,2))
//SYSPRINT  DD SYSOUT=&SOUT,
//             DCB=(RECFM=FBA,LRECL=121,BLKSIZE=605)
./ ADD NAME=PLIL
//         PROC MBR=NONAME,SOUT=A
//PLI      EXEC PGM=IEMAA
//STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//SYSLIN   DD  DSN=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(80,(250,100))
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,
//             SPACE=(1024,(200,50),,CONTIG,ROUND),DCB=BLKSIZE=1024
//SYSPUNCH DD  DUMMY,DCB=BLKSIZE=80
//LKED     EXEC PGM=IEWL,PARM='SIZE=(65K,16K)',
//         COND=(9,LT,PLI),REGION=128K
//SYSLIB   DD  DSN=SYS2.LINKLIB,DISP=SHR
//         DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSLMOD  DD  DSN=SYS2.LINKLIB(&MBR),DISP=SHR
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),
//             DCB=BLKSIZE=1024
//SYSPRINT DD  SYSOUT=&SOUT,DCB=(RECFM=FA,BLKSIZE=121)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSIN    DD  DUMMY
./ ADD NAME=RPGL
//        PROC MBR=NONAME,SOUT=A
//RPG     EXEC PGM=IESRPG,PARM=(NODECK,LOAD,LIST,NOSEQN)
//STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT
//SYSPUNCH DD  SYSOUT=B
//SYSUT3   DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(600,(100,20))
//SYSUT2   DD  DSNAME=&&SYSUT2,UNIT=SYSDA,SPACE=(600,(100,20))
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(600,(100,20))
//SYSGO    DD  DSNAME=&&LOADSET,UNIT=(SYSDA,SEP=SYSPUNCH),
//             DISP=(MOD,PASS,DELETE),SPACE=(80,(200,50))
//LKED    EXEC PGM=IEWL,PARM=(XREF,LIST,LET)
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSNAME=SYS2.LINKLIB(&MBR),DISP=SHR
//SYSUT1   DD  DSNAME=&&SYSUT1,SPACE=(1024,(50,20)),
//             UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD))
//SYSPRINT DD  SYSOUT=&SOUT
./ ENDUP
##
/*
