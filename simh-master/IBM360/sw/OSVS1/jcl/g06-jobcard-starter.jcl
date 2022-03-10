//G06JOBCD JOB  1,CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:      G06JOBCD                                           ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Provide a new JOBCARD macro for stage 2 jobs       ***
//*              when using the VS1 6.0 starter system as           ***
//*              the generating system.                             ***
//*    Run on:   Generating system                                  ***
//*    Update:   2020/10/04                                         ***
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
//*    Rename existing JOBCARD member to JOBCARDO.                  ***
//*-----------------------------------------------------------------***
//RENAME  EXEC PGM=IEHPROGM
//SYSPRINT DD  SYSOUT=A
//FDLB60   DD  DISP=OLD,UNIT=3350,VOL=SER=FDLB60
//SYSIN    DD  *
 SCRATCH       DSNAME=SYS1.AGENLIB,VOL=3350=FDLB60,MEMBER=JOBCARDO
 RENAME        DSNAME=SYS1.AGENLIB,VOL=3350=FDLB60,MEMBER=JOBCARD,     C
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
//             VOL=SER=FDLB60,UNIT=SYSDA
//SYSUT1   DD  DATA,DLM=AA
         MACRO
         JOBCARD
         COPY  SGGBLPAK
         AIF   ('&SGCTRLC(40)' NE 'SYS1').A1
 PUNCH '//G08GEN&SGCTRLA(3) JOB 1,''SYSTEM GENERATION'',CLASS=&SGCTRLC(+
               42),MSGCLASS=&SGCTRLC(41),TYPRUN=HOLD'
         AGO   .MEND
.A1      ANOP
 PUNCH '//&SGCTRLC(40)&SGCTRLA(3) JOB 1,''SYSTEM GENERATION'',MSGLEVEL=X
               1,CLASS=&SGCTRLC(42),MSGCLASS=&SGCTRLC(41)'
.MEND    ANOP
         MEND
AA
//
