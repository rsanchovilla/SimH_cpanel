//G12CMCAT  JOB  1,'Create MasterCat',                                 +
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:        G12CMCAT                                         ***
//*    Product:    VS1 6.7.                                         ***
//*    Purpose:    Create new VS1 system's master catalog.          ***
//*    Run under:  New system.                                      ***
//*    Update:     2023/03/19                                       ***
//*                                                                 ***
//*********************************************************************
//*
/*JOBPARM LINECT=0
//*
//*-----------------------------------------------------------------***
//*    Specify volser of sysres volume.                             ***
//*-----------------------------------------------------------------***
//VOL     EXEC PGM=IEFBR14
//RESVOL    DD DISP=OLD,UNIT=3350,VOL=(PRIVATE,RETAIN,SER=FGEN67)
//*
//*-----------------------------------------------------------------***
//*    Delete any existing VSAM catalog on the sysres volume        ***
//*    as well as the filler where the new catalog will go.         ***
//*-----------------------------------------------------------------***
//CLEANUP EXEC PGM=IDCAMS
//MCATFILL  DD DSN=SYS1.MCAT.FILLER,
//             DISP=(MOD,DELETE),SPACE=(TRK,1),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL)
//SYSPRINT DD  SYSOUT=A
//RESVOL   DD  DISP=OLD,VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL)
//SYSIN    DD  *
 DELETE SYS1.FGEN67.CATALOG MASTERCATALOG    FORCE    FILE(RESVOL)
 SET LASTCC = 0
 SET MAXCC  = 0
 EXPORT SYS1.FGEN67.CATALOG DISCONNECT
 SET LASTCC = 0
 SET MAXCC  = 0
/*
//*
//*-----------------------------------------------------------------***
//*    Define new master VSAM catalog on the sysres volume.         ***
//*-----------------------------------------------------------------***
//DEFMCAT EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=A
//RESVOL   DD  DISP=OLD,VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL)
//SYSIN    DD  *
  DEFINE MCAT ( NAME  (SYS1.FGEN67.CATALOG)    -
                FILE  (RESVOL)                 -
                VOL   (FGEN67)                 -
                CYL   (10)                     -
                BUFSP (8192)                   -
                NRVBL                          -
              )
/*
//
