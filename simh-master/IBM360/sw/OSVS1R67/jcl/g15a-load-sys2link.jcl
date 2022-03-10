//G15SLOAD JOB 1,RELOAD-SYS2LINK,                                      +
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:        G15SLOAD                                         ***
//*    Product:    VS1 6.7.                                         ***
//*    Purpose:    Load SYS2.LINKLIB and subroutine libraries       ***
//*                from ZSUP2A support tape.                        ***
//*    Run under:  New system.                                      ***
//*    Update:     2021/04/23                                       ***
//*                                                                 ***
//*    Note:       SYS2.LINKLIB from ZSUP2A contains the            ***
//*                following compilers:                             ***
//*                                                                 ***
//*                  OS ANS COBOL V2      OS FORTRAN G              ***
//*                  OS COBOL F           OS FORTRAN H              ***
//*                  OS PL/I F                                      ***
//*                                                                 ***
//*                as well as some utility programs.  This          ***
//*                job also restores the subroutine libraries       ***
//*                for the compilers in SYS2.LINKLIB:               ***
//*                                                                 ***
//*                  SYS1.COBLIB                                    ***
//*                  SYS1.FORTLIB                                   ***
//*                  SYS1.PL1LIB                                    ***
//*                                                                 ***
//*********************************************************************
//*
//*-----------------------------------------------------------------***
//*    Specify volsers.                                             ***
//*-----------------------------------------------------------------***
//VOL     EXEC PGM=IEFBR14,REGION=20K
//LIBVOL   DD  DISP=OLD,UNIT=SYSALLDA,VOL=SER=FGEN67
//*
//*-----------------------------------------------------------------***
//*    Restore data sets from ZSUP2A tape.                          ***
//*-----------------------------------------------------------------***
//LOAD    EXEC PGM=IEBCOPY,REGION=256K
//ISYS2LNK DD DISP=OLD,DSN=TAPE.SYS2LINK,
//            LABEL=(1,SL,EXPDT=98000),
//            UNIT=TAPE,VOL=(PRIVATE,RETAIN,SER=ZSUP2A)
//IFORTLIB DD DISP=(NEW,KEEP),DSN=TAPE.FORTLIB,
//            LABEL=(2,SL,EXPDT=98000),
//            UNIT=AFF=ISYS2LNK,VOL=(PRIVATE,RETAIN,SER=ZSUP2A)
//IPL1LIB  DD DISP=(NEW,KEEP),DSN=TAPE.PL1LIB,
//            LABEL=(3,SL,EXPDT=98000),
//            UNIT=AFF=ISYS2LNK,VOL=(PRIVATE,RETAIN,SER=ZSUP2A)
//ICOBLIB  DD DISP=(NEW,KEEP),DSN=TAPE.COBLIB,
//            LABEL=(4,SL,EXPDT=98000),
//            UNIT=AFF=ISYS2LNK,VOL=(PRIVATE,RETAIN,SER=ZSUP2A)
//OSYS2LNK DD DSN=SYS2.LINKLIB,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.VOL.LIBVOL,
//            SPACE=(CYL,(40,,250))
//OFORTLIB DD DSN=SYS1.FORTLIB,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.VOL.LIBVOL,
//            SPACE=(CYL,(1,,40))
//OPL1LIB  DD DSN=SYS1.PL1LIB,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.VOL.LIBVOL,
//            SPACE=(CYL,(2,,100))
//OCOBLIB  DD DSN=SYS1.COBLIB,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.VOL.LIBVOL,
//            SPACE=(CYL,(1,,50))
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(TRK,(90,90))
//SYSUT4   DD  UNIT=SYSALLDA,SPACE=(TRK,(90,90))
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  *
 COPY INDD=ISYS2LNK,OUTDD=OSYS2LNK
 COPY INDD=IFORTLIB,OUTDD=OFORTLIB
 COPY INDD=IPL1LIB,OUTDD=OPL1LIB
 COPY INDD=ICOBLIB,OUTDD=OCOBLIB
//
