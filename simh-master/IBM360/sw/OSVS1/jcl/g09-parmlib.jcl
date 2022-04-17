//G09PARML JOB  1,'Update PARMLIB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:      G10PARML                                           ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Update SYS1.PARMLIB on new VS1 6.0 sysres volume.  ***
//*    Run on:   Generating system                                  ***
//*    Update:   2020/10/05                                         ***
//*                                                                 ***
//*********************************************************************
//*
//PARMLIB EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=A
//SYSUT2   DD  DISP=SHR,DSN=SYS1.PARMLIB,UNIT=SYSDA,VOL=SER=FGEN60
//SYSIN    DD  DATA
./ ADD NAME=IEASYS00
PAGE=(V=PAGE83,BLK=8192)
./ ADD NAME=PRESRES
FRES60,0,2,3350,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
FGEN60,0,1,3350,N  MOUNT=RESIDENT  USE=PUBLIC   SKIP IPL MOUNT MSG
FDLB60,0,2,3350,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
PERM81,0,0,3350,N  MOUNT=RESIDENT  USE=STORAGE  SKIP IPL MOUNT MSG
WORK81,0,1,3350,N  MOUNT=RESIDENT  USE=PUBLIC   SKIP IPL MOUNT MSG
PAGE00,0,2,3340,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
PAGE80,0,2,3340,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
PAGE81,0,2,3340,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
PAGE83,0,2,3330,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
RESV01,0,2,3350,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
DLIBA1,0,2,3350,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
DLIBA2,0,2,3350,N  MOUNT=RESIDENT  USE=PRIVATE  SKIP IPL MOUNT MSG
IVS161,1,2,3330,N  MOUNT=RESERVED  USE=PRIVATE  SKIP IPL MOUNT MSG
IVS162,1,2,3330,N  MOUNT=RESERVED  USE=PRIVATE  SKIP IPL MOUNT MSG
WORK61,0,1,3350,N  MOUNT=RESIDENT  USE=PUBLIC   SKIP IPL MOUNT MSG
./ ADD NAME=COLD
NIP01
SET02
SMF01
CMD01
DFN01
JES01
./ ADD NAME=WARM
NIP01
SET01
SMF01
CMD01
DFN01
JES01
./ ADD NAME=NIP01
DEVSTAT=ALL
./ ADD NAME=SET01
SWPRM=(U),JLPRM=(U)
./ ADD NAME=SET02
SWPRM=(U),JLPRM=(U),Q=(,F),SPOOL=(,F)
./ ADD NAME=DFN01
P0=(C=*,128K)
P1=(C=A,3072K)
P2=(C=A,3072K)
P3=(C=A,3072K)
P4=(C=A,3072K,LAST)
./ ADD NAME=SMF01
    OPT=2,                      SYSTEM,JOB AND STEP DATA COLLECTION
    EXT=YES,                    USER EXITS ARE TO BE TAKEN
    JWT=300,                    MAXIMUM WAIT TIME
    BUF=2000,                   A 2000 BYTE BUFFER IS DEFINED
    SID=VS1A,                   SYSTEM ID
    OPI=NO,                     NO OPERATOR INTERVENTION
    MAN=ALL                     USER AND SYSTEM RECORDS PERMITTED
./ ADD NAME=JES01
         BUFSIZE=3952,NUMBUFS=50,STEPWTP=15,
         SWDSLMT=15,SPOLCAP=80,WTLRCDS=3000,
         RDR=(R=5,Y=50,B=9600,A=1,N=42),
         WTR=(W=8,U=3,Z=50,B=4890),JOUTLIM=0,
         ALCUNIT=28138,JOBLOG=YES,PRLRECL=133,
         LRDPARM=(00600300005011E00011A),
         LPRPARM=(PA),
         LPUPARM=(CB),
         RRDPARM=(00600300005011E00011A),
         RPRPARM=(PA),
         RPUPARM=(CB),
         SPOLVOL=(FGEN60),
         JOBQVOL=(FGEN60)
./ ADD NAME=CMD01
MONITOR JOBNAMES,T                      ** FROM CMD01 AT IPL
MONITOR DSNAME                          ** FROM CMD01 AT IPL
./ ADD NAME=LNKLST00
 SYS1.LINKLIB,SYS2.LINKLIB
./ ADD NAME=IEAAPF00
 SYS2.LINKLIB FGEN60
./ ADD NAME=IEABLD00
 SYS1.LINKLIB
./ ENDUP
/*
//