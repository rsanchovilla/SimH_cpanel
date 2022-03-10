//G06JOBCD JOB  1,'Update JOBCARD',                                    +
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:      G06JOBCD                                           ***
//*    Product:  VS1 6.7.                                           ***
//*    Purpose:  Provide a new JOBCARD macro for stage 2 jobs       ***
//*              when using the VS1 6.0 starter system as           ***
//*              the generating system.                             ***
//*    Run on:   Generating system                                  ***
//*    Update:   2021/04/04                                         ***
//*                                                                 ***
//*    Note:     This version of the G06JOBCD job is                ***
//*              intended to be run instead of the                  ***
//*              g06-jobcard-nonstarter.jcl version when            ***
//*              the generating system is the VS1 6.0               ***
//*              starter system.  The starter system does           ***
//*              not have VSAM, so we can't add a JOBCAT            ***
//*              DD statement to stage 2 jobs.                      ***
//*                                                                 ***
//*********************************************************************
//*
//*-----------------------------------------------------------------***
//*    Rename existing JOBCARD member to JOBCARDO.  Note that       ***
//*    if SYS1.AGENLIB does not already contain a JOBCARDO          ***
//*    backup member, this step will get a return code of 8         ***
//*    from the SCRATCH, which is normal.                           ***
//*-----------------------------------------------------------------***
//RENAME  EXEC PGM=IEHPROGM
//SYSPRINT DD  SYSOUT=A
//FDLB67   DD  DISP=OLD,UNIT=3350,VOL=SER=FDLB67
//SYSIN    DD  *
 SCRATCH       DSNAME=SYS1.AGENLIB,VOL=3350=FDLB67,MEMBER=JOBCARDO
 RENAME        DSNAME=SYS1.AGENLIB,VOL=3350=FDLB67,MEMBER=JOBCARD,     C
               NEWNAME=JOBCARDO
/*
//*
//*-----------------------------------------------------------------***
//*    Add new JOBCARD member.                                      ***
//*-----------------------------------------------------------------***
//JOBCARD EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  DUMMY
//SYSUT2   DD  DISP=SHR,DSN=SYS1.AGENLIB(JOBCARD),
//             VOL=SER=FDLB67,UNIT=SYSDA
//SYSUT1   DD  DATA,DLM='??'
         MACRO                                                          01000000
         JOBCARD                                                        02000000
         COPY  SGGBLPAK                                                 03000000
         AIF   ('&SGCTRLC(40)' NE 'SYS1').A1                            04000000
 PUNCH '//G09GEN&SGCTRLA(3) JOB 1,''SYSTEM GENERATION'',CLASS=&SGCTRLC(+05000000
               42),MSGCLASS=&SGCTRLC(41),TYPRUN=HOLD'                   06000000
         AGO   .MEND                                                    07000000
.A1      ANOP                                                           08000000
 PUNCH '//G09GEN&SGCTRLA(3) JOB 1,''SYSTEM GENERATION'',CLASS=&SGCTRLC(+09000000
               42),MSGCLASS=&SGCTRLC(41),TYPRUN=HOLD'                   10000000
.MEND    ANOP                                                           11000000
         MEND                                                           13000000
??
//
