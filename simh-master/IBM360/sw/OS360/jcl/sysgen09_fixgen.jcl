//MVTSETUP JOB CLASS=A,MSGLEVEL=(1,1)
//*
//*********************************************************************
//*                                                                 ***
//*    Job:      MVTSETUP                                           ***
//*    Product:  OS/360 MVT.                                        ***
//*    Purpose:  Final setup for the freshly generated MVT system.  ***
//*              Increase default region sizes for the reader and   ***
//*              writer procs. Change default options for the       ***
//*              reader. Install a PRESRES member to make system    ***
//*              volumes permanently resident. Catalog the target   ***
//*              system's datasets in the target catalog.           ***
//*    Step:     8.1                                                ***
//*    Update:   2003/02/08                                         ***
//*                                                                 ***
//*********************************************************************
//*
//FIXRDR   EXEC PGM=IEBUPDTE
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  DISP=SHR,DSN=SYS1.PROCLIB,UNIT=3330,VOL=SER=MVTRES
//SYSUT2   DD  DISP=SHR,DSN=SYS1.PROCLIB,UNIT=3330,VOL=SER=MVTRES
//SYSIN    DD  DATA
./ CHANGE NAME=RDR,LIST=ALL
//             REGION=64K,                     READER BASIC REGION     ,06000017
//       PARM='80103005001024912810SYSDA   E00001A'  DEFAULT OPTIONS   ,08000019
./ ENDUP
/*
//*
//FIXWTR   EXEC PGM=IEBUPDTE
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  DISP=SHR,DSN=SYS1.PROCLIB,UNIT=3330,VOL=SER=MVTRES
//SYSUT2   DD  DISP=SHR,DSN=SYS1.PROCLIB,UNIT=3330,VOL=SER=MVTRES
//SYSIN    DD  DATA
./ CHANGE NAME=WTR,LIST=ALL
//IEFPROC  EXEC  PGM=IEFSD080,PARM='PA',REGION=64K,ROLL=(NO,NO)         10000020
./ ENDUP
/*
//*
//PRESRES  EXEC PGM=IEBUPDTE
//SYSPRINT DD  SYSOUT=A
//SYSABEND DD  SYSOUT=A
//SYSUT1   DD  DISP=SHR,DSN=SYS1.PARMLIB,UNIT=3330,VOL=SER=MVTRES
//SYSUT2   DD  DISP=SHR,DSN=SYS1.PARMLIB,UNIT=3330,VOL=SER=MVTRES
//SYSIN    DD  DATA
./ ADD NAME=PRESRES
MVTRES,0,2,3330,N
WORK01,1,0,3330,N
WORK02,1,1,3330,N
./ ENDUP
/*
//*
//SCRTCHUT EXEC PGM=IEFBR14
//SYSUT1   DD  DISP=(OLD,DELETE),DSN=SYS1.SG1
//SYSUT2   DD  DISP=(OLD,DELETE),DSN=SYS1.SG2
//SYSUT3   DD  DISP=(OLD,DELETE),DSN=SYS1.SG3
//SYSUT4   DD  DISP=(OLD,DELETE),DSN=SYS1.SG4
//
