//G10PROCL JOB 1,'Update PROCLIB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:      G10PROCL                                           ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Update SYS1.PROCLIB on new VS1 6.0 sysres volume.  ***
//*    Run on:   Generating system                                  ***
//*    Update:   2020/09/27                                         ***
//*                                                                 ***
//*********************************************************************
//*
//PROCLIB EXEC PGM=IEBUPDTE,PARM=NEW,REGION=128K
//SYSPRINT  DD SYSOUT=A
//SYSUT2    DD DISP=SHR,DSN=SYS1.PROCLIB,UNIT=3350,VOL=SER=FGEN60
//SYSIN     DD DATA
./ ADD NAME=DIP
//DIP      PROC
//*********************************************************************
//*                                                                 ***
//*    Procedure:  DIP                                              ***
//*    Purpose:    Reinitialize SYS1.LOGREC.                        ***
//*    Update:     2020/01/20.                                      ***
//*                                                                 ***
//*********************************************************************
//*
//IEFPROC  EXEC PGM=IFCDIP00,REGION=192K
//SERERDS  DD   DDNAME=IEFRDER
//IEFRDER  DD   DISP=OLD,DSN=SYS1.LOGREC
./ ADD NAME=S
//*********************************************************************
//*                                                                 ***
//*    PROCEDURE:  S                                                ***
//*    PURPOSE:    FORCE DEALLOCATION.                              ***
//*    UPDATE:     2006/07/11                                       ***
//*                                                                 ***
//*********************************************************************
//*
//IEFPROC EXEC PGM=IEFBR14,REGION=20K,TIME=1439
//* THIS PROCEDURE IS ACTIVATED BY A 'START DEALLOC' COMMAND TO
//* EXECUTE IEFBR14 CAUSING ALLOCATION TO DEALLOCATE A
//* DEVICE SPECIFIED ON A PREVIOUS 'VARY OFFLINE' COMMAND.
./ ADD NAME=WTRZ
//*********************************************************************
//*                                                                 ***
//*    Procedure:  WTRZ                                             ***
//*    Purpose:    Write class Z output to dummy file.              ***
//*    Update:     2020/08/24                                       ***
//*                                                                 ***
//*********************************************************************
//*
//WTRZ    PROC
//IEFPROC EXEC PGM=IEFOSC01,                                           *
//       PARM='PZ'
//*            UCCCCCCCC,RRRRRRRR,N,TT,LL,PPPP,E,XX,M,S,F      @X50AD6A
//*            U         UNIT DEVICE TYPE
//*            CCCCCCCC  1-8 OUTPUT CLASSES - OVERRIDDEN BY START OR
//*                      MODIFY COMMANDS
//*            RRRRRRRR  OPTIONAL SEPARATION JOB ROUTINE NAME
//*            N         OPTIONAL NUMBER OF JOB SEPARATORS
//*            TT        OPTIONAL TRANSLATE PARAMETER
//*            LL        OPTIONAL LINE COUNT
//*            PPPP      OPTIONAL CHECKPOINT COUNT
//*            E         OPTIONAL NUMBER OF END-OF-JOB SEPARATORS
//*            XX        OPTIONAL PARTITION,WTR WILL HAVE      @X31ES6A
//*                      DISPATCHING PRIORITY ONE LOWER THAN   @X31ES6A
//*                      PARTITION SPECIFIED.DEFAULT IS SYSTEM @X31ES6A
//*                      TASK PRIORITY.                        @X31ES6A
//*            M         OPTIONAL MULTIPLE(M) OR SINGLE(S)     @X04AA6A
//*                      DATA SETS/3540 DISKETTE.S IS DEFAULT  @X04AA6A
//*            S         OPTIONAL SECURITY(S),ALLOWS 3540      @X04AA6A
//*                      ACCESS METHOD TO PROVIDE SECURE DATA  @X04AA6A
//*                      SETS.DEFAULT IS NON SECURE.           @X04AA6A
//*            F         OPTIONAL MARK FORMS FOR 3800 PRINTER  @X50AD6A
//IEFRDER   DD DUMMY,
//             DCB=(BLKSIZE=133,LRECL=133,BUFL=133,BUFNO=2,RECFM=FM)
./ ADD NAME=RDR
//*********************************************************************
//*                                                                 ***
//*    Procedure:  RDR                                              ***
//*    Purpose:    Execute JEPS reader task.                        ***
//*    Update:     2020/08/24                                       ***
//*                                                                 ***
//*********************************************************************
//*
//IEFPROC  EXEC  PGM=IEFVMA,                                           *
//  PARM='00600300005011E00011AXX'                             @X31ES6A
//*       BPPTTTTSSCCCRLAAAAEFH                                  X02932
//*       B    PROGRAMMER NAME AND ACCOUNT NUMBER NOT NEEDED
//*              DEFAULTS ADDRSPC TO VIRT
//*       PP   PRIORITY=06
//*       TTTTSS   JOB STEP INTERVAL=30 MINUTES, 00 SECONDS
//*       CCC  JOB STEP DEFAULT SIZE WHEN REQUEST IS REAL=50K
//*       R    DISPLAY AND EXECUTE COMMANDS=1
//*       L    BYPASS LABEL OPTION=1
//*       AAAA COMMAND AUTHORITY FOR MCS=E000-ALL COMMANDS          MCS
//*              MUST BE AUTHORIZED                                 MCS
//*       E    JCL MESSAGE LEVEL DEFAULT=1-ALL MESSAGES
//*       F    ALLOCATION/TERMINATION MESSAGE LEVEL DEFAULT=1-ALL
//*              MESSAGES
//*       H    DEFAULT MSGCLASS=A
//*       XX   PARTITION,RDR WILL HAVE DISPATCHING PRIORITY 1  @X31ES6A
//*            LOWER THAN PARTITION SPECIFIED.XX=PRIORITY OF   @X31ES6A
//*            SYSTEM TASK                                     @X31ES6A
//IEFRDER DD   UNIT=2540,                                              *
//             LABEL=(,NL),                                            *
//             VOLUME=SER=SYSIN,                                       *
//             DISP=OLD,                                               *
//             DCB=(RECFM=F,LRECL=80)
./ ADD NAME=INITSWA
//*********************************************************************
//*                                                                 ***
//*    Procedure:  INITSWA                                          ***
//*    Purpose:    Execute initiator using SWA instead of SWADS.    ***
//*    Update:     2020/09/27                                       ***
//*                                                                 ***
//*********************************************************************
//*
//IEFPROC   EXEC   PGM=IEFIIC,PARM='SWA=2048'
./ ADD NAME=TAPEMAP
//********************************************************************* 00000100
//*                                                                 *** 00000200
//*    Procedure:  TAPEMAP                                          *** 00000300
//*    Purpose:    Execute TAPEMAP program.                         *** 00000400
//*    Update:     2020/08/24                                       *** 00000501
//*                                                                 *** 00000600
//********************************************************************* 00000700
//*                                                                     00000800
//TAPEMAP PROC PROG=TAPEMAP,OUT=A                                       00000902
//IEFPROC EXEC PGM=&PROG,REGION=256K                                    00001001
//SYSPRINT DD SYSOUT=&OUT                                               00001100
//SYSPRNT2 DD SYSOUT=&OUT                                               00001200
//SYSIN    DD DUMMY                                                     00001300
//SYSUT1   DD DDNAME=IEFRDER                                            00001400
//IEFRDER  DD DISP=OLD,UNIT=(TAPE,,DEFER),LABEL=(1,BLP),                00001500
//            DSN=TAPE,VOL=SER=TAPE                                     00001600
./ ENDUP
/*
//

