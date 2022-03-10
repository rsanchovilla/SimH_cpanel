//G13SMPIN  JOB  1,'Initialize SMP',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:        G13SMPIN                                         ***
//*    Product:    VS1 6.7.                                         ***
//*    Purpose:    Initialize SMP environment for new VS1 system:   ***
//*                allocate SMP data sets, add ACDS entries for     ***
//*                user SVCs, copy the ACDS to the CDS, initialize  ***
//*                SMP PTS, add SMP procedure, and run JCLIN with   ***
//*                stage 2.                                         ***
//*    Run under:  New system.                                      ***
//*    Update:     2021/04/26                                       ***
//*                                                                 ***
//*********************************************************************
//*
//*-----------------------------------------------------------------***
//*    Specify volsers.                                             ***
//*-----------------------------------------------------------------***
//VOL     EXEC PGM=IEFBR14
//DLIBVOL   DD DISP=OLD,UNIT=3350,VOL=(PRIVATE,RETAIN,SER=FDLB67)
//RESVOL    DD DISP=OLD,UNIT=3350,VOL=(PRIVATE,RETAIN,SER=FGEN67)
//PTSVOL    DD DISP=OLD,UNIT=3350,VOL=(PRIVATE,RETAIN,SER=PERM73)
//*
//*-----------------------------------------------------------------***
//*    Allocate SMP data sets.                                      ***
//*-----------------------------------------------------------------***
//SMPCDS    DD DSN=SYS1.SMPCDS,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(25,,2000),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPCRQ    DD DSN=SYS1.SMPCRQ,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(5,,350),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPMTS    DD DSN=SYS1.SMPMTS,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(5,,50),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPSTS    DD DSN=SYS1.SMPSTS,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(5,,50),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPSCDS   DD DSN=SYS1.SMPSCDS,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(2,,75),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPPTS    DD DSN=SYS1.SMPPTS,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=4080),
//             SPACE=(CYL,(15,,550),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.PTSVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPLOG    DD DSN=SYS1.SMPLOG,
//             DCB=(DSORG=PS,RECFM=U,BLKSIZE=260),
//             SPACE=(CYL,(2),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.RESVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//SMPACRQ   DD DSN=SYS1.SMPACRQ,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),
//             SPACE=(CYL,(5,,350),,CONTIG),
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.DLIBVOL),
//             DISP=(NEW,CATLG,DELETE)
//*
//*-----------------------------------------------------------------***
//*    Initialize the PTS SYS entry.  Note that specifying          ***
//*    "DISP=OLD" here for SMPLOG zeroes the log file.              ***
//*-----------------------------------------------------------------***
//INITPTS EXEC PGM=HMASMP,PARM='DATE=U'
//SMPOUT    DD SYSOUT=A
//SMPLOG    DD DISP=OLD,DSN=SYS1.SMPLOG
//SMPPTS    DD DISP=OLD,DSN=SYS1.SMPPTS
//SMPCNTL   DD *
  UCLIN PTS .
  ADD SYS DSSPACE(50,100,200)
          SREL(X067)
          PEMAX(9999)
          DSPREFIX(VS1R67.TLIB)
          LKEDPARM(XREF,LIST,LET,NCAL,SIZE=(400K,100K))
          FMID(EAS1101,
               EBA1101,
               EBT1101,
               EDE1101,
               EDM1101,
               EDS1101,
               EER1100,
               EGA1101,
               EGS1101,
               EMO1101,
               EMS1101,
               EPM1101,
               ERJ1101,
               ESA1101,
               ESS1101,
               EST1101,
               ESU1101,
               ESX1101,
               ESY1400,
               ETC0107,
               EUT1101,
               EVT0107).
  ENDUCL .
  LIST PTS SYS.
/*
//*
//*-----------------------------------------------------------------***
//*    Add ACDS entries for user SVCs via UCLIN.                    ***
//*-----------------------------------------------------------------***
//USERSVC EXEC PGM=HMASMP,PARM='DATE=U'
//SMPOUT    DD SYSOUT=A
//SMPACDS   DD DISP=SHR,DSN=SYS1.SMPACDS
//SMPLOG    DD DISP=MOD,DSN=SYS1.SMPLOG
//SMPCNTL   DD *
UCLIN ACDS              .
REP       MOD             ( IGC255   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC251   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC217   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC216   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC214   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC213   )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( RESLIB   )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0024I )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0024H )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0024B )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0023D )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0022H )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0022G )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
REP       MOD             ( IGC0023A )
          LASTUPD         ( EBA1101 )
          LASTUPDTYPE     ( ADD )
          DISTLIB         ( SVCLIBA  )
          FMID            ( EBA1101 )
          RMID            ( EBA1101 )
                          .
ENDUCL.
/*
//*
//*-----------------------------------------------------------------***
//*    Copy SYS1.SMPACDS to SYS1.SMPCDS.                            ***
//*-----------------------------------------------------------------***
//COPYCDS EXEC PGM=IEBCOPY
//SYSPRINT  DD SYSOUT=A
//SYSUT3    DD UNIT=SYSDA,SPACE=(TRK,90)
//SYSUT4    DD UNIT=SYSDA,SPACE=(TRK,90)
//SMPACDS   DD DISP=SHR,DSN=SYS1.SMPACDS
//SMPCDS    DD DISP=SHR,DSN=SYS1.SMPCDS
//SYSIN     DD *
 COPY INDD=SMPACDS,OUTDD=SMPCDS
/*
//*
//*-----------------------------------------------------------------***
//*    Add an SMP proc to SYS1.PROCLIB.                             ***
//*-----------------------------------------------------------------***
//SMPPROC EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT  DD SYSOUT=A
//SYSUT2    DD DISP=SHR,DSN=SYS1.PROCLIB,VOL=SER=FGEN67,UNIT=SYSALLDA
//SYSIN     DD DATA,DLM='??'
./ ADD NAME=SMP
//SMP     PROC SOUT=A,
//             WUNIT=SYSALLDA,
//             TUNIT=SYSALLDA,
//             TVOL=PERM73
//*
//*********************************************************************
//*                                                                 ***
//*    Procedure:  SMP                                              ***
//*    Purpose:    Procedure to execute SMP4.                       ***
//*    Update:     2021/04/26                                       ***
//*                                                                 ***
//*********************************************************************
//*
//SMP     EXEC PGM=HMASMP,
//             TIME=1440,PARM='DATE=U'
//*
//*-----------------------------------------------------------------***
//*      SMP and utility SYSOUT data sets.                          ***
//*-----------------------------------------------------------------***
//SMPLIST  DD  SYSOUT=&SOUT
//SMPOUT   DD  SYSOUT=&SOUT
//SMPRPT   DD  SYSOUT=&SOUT
//SYSPRINT DD  SYSOUT=&SOUT
//*
//*-----------------------------------------------------------------***
//*      SMP permanent data sets.                                   ***
//*-----------------------------------------------------------------***
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ
//SMPCDS   DD  DISP=SHR,DSN=SYS1.SMPCDS
//SMPCRQ   DD  DISP=SHR,DSN=SYS1.SMPCRQ
//SMPLOG   DD  DISP=MOD,DSN=SYS1.SMPLOG
//SMPMTS   DD  DISP=SHR,DSN=SYS1.SMPMTS
//SMPPTS   DD  DISP=SHR,DSN=SYS1.SMPPTS
//SMPSCDS  DD  DISP=SHR,DSN=SYS1.SMPSCDS
//SMPSTS   DD  DISP=SHR,DSN=SYS1.SMPSTS
//*
//*-----------------------------------------------------------------***
//*      SMP work files.                                            ***
//*-----------------------------------------------------------------***
//SMPWRK1  DD  UNIT=&WUNIT,SPACE=(CYL,(3,5,83)),DISP=(,DELETE),
//             DCB=BLKSIZE=3120
//SMPWRK2  DD  UNIT=&WUNIT,SPACE=(CYL,(3,5,83)),DISP=(,DELETE),
//             DCB=BLKSIZE=3120
//SMPWRK3  DD  UNIT=&WUNIT,SPACE=(CYL,(3,5,83)),DISP=(,DELETE),
//             DCB=BLKSIZE=3120
//SMPWRK4  DD  UNIT=&WUNIT,SPACE=(CYL,(3,5,83)),DISP=(,DELETE),
//             DCB=BLKSIZE=3120
//SMPWRK5  DD  UNIT=&WUNIT,SPACE=(CYL,(3,5,83)),DISP=(,DELETE)
//*
//*-----------------------------------------------------------------***
//*      Work files for SMP-invoked utilities.                      ***
//*-----------------------------------------------------------------***
//SYSUT1   DD  UNIT=&WUNIT,SPACE=(3000,(200,400))
//SYSUT2   DD  UNIT=&WUNIT,SPACE=(3000,(200,400))
//SYSUT3   DD  UNIT=&WUNIT,SPACE=(3000,(200,400))
//*
//*-----------------------------------------------------------------***
//*      Volume for saving relfile data sets.                       ***
//*-----------------------------------------------------------------***
//SMPTLIB  DD  DISP=OLD,UNIT=&TUNIT,VOL=SER=&TVOL
//*
//*-----------------------------------------------------------------***
//*      Macro libraries for SMP-generated assemblies.              ***
//*-----------------------------------------------------------------***
//SYSLIB   DD  DISP=SHR,DSN=SYS1.SMPMTS
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//*
//*-----------------------------------------------------------------***
//*      VS1 target libraries.                                      ***
//*-----------------------------------------------------------------***
//IMAGELIB DD  DISP=SHR,DSN=SYS1.IMAGELIB
//LINKLIB  DD  DISP=SHR,DSN=SYS1.LINKLIB
//MACLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB
//PROCLIB  DD  DISP=SHR,DSN=SYS1.PROCLIB
//RMTMAC   DD  DISP=SHR,DSN=SYS1.RMTMAC
//SAMPLIB  DD  DISP=SHR,DSN=SYS1.SAMPLIB
//SVCLIB   DD  DISP=SHR,DSN=SYS1.SVCLIB
//TELCMLIB DD  DISP=SHR,DSN=SYS1.TELCMLIB
//VTAMLIB  DD  DISP=SHR,DSN=SYS1.VTAMLIB
//*
//*-----------------------------------------------------------------***
//*      VS1 distribution libraries.                                ***
//*-----------------------------------------------------------------***
//ACMDLIB  DD  DISP=SHR,DSN=SYS1.ACMDLIB
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//AISPMAC  DD  DISP=SHR,DSN=SYS1.AISPMAC
//AMACLIB  DD  DISP=SHR,DSN=SYS1.AMACLIB
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//APARMLIB DD  DISP=SHR,DSN=SYS1.APARMLIB
//APROCLIB DD  DISP=SHR,DSN=SYS1.APROCLIB
//ARMTMAC  DD  DISP=SHR,DSN=SYS1.ARMTMAC
//ASAMPLIB DD  DISP=SHR,DSN=SYS1.ASAMPLIB
//ATCAMMAC DD  DISP=SHR,DSN=SYS1.ATCAMMAC
//ATSOMAC  DD  DISP=SHR,DSN=SYS1.ATSOMAC
//AOSA0    DD  DISP=SHR,DSN=SYS1.AOSA0
//AOSBB    DD  DISP=SHR,DSN=SYS1.AOSBB
//AOSB0    DD  DISP=SHR,DSN=SYS1.AOSB0
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AOSCA    DD  DISP=SHR,DSN=SYS1.AOSCA
//AOSCD    DD  DISP=SHR,DSN=SYS1.AOSCD
//AOSCE    DD  DISP=SHR,DSN=SYS1.AOSCE
//AOSC2    DD  DISP=SHR,DSN=SYS1.AOSC2
//AOSC5    DD  DISP=SHR,DSN=SYS1.AOSC5
//AOSC6    DD  DISP=SHR,DSN=SYS1.AOSC6
//AOSD0    DD  DISP=SHR,DSN=SYS1.AOSD0
//AOSD7    DD  DISP=SHR,DSN=SYS1.AOSD7
//AOSD8    DD  DISP=SHR,DSN=SYS1.AOSD8
//AOSG0    DD  DISP=SHR,DSN=SYS1.AOSG0
//AOST4    DD  DISP=SHR,DSN=SYS1.AOST4
//AOSU0    DD  DISP=SHR,DSN=SYS1.AOSU0
//AOS0A    DD  DISP=SHR,DSN=SYS1.AOS0A
//AOS00    DD  DISP=SHR,DSN=SYS1.AOS00
//AOS03    DD  DISP=SHR,DSN=SYS1.AOS03
//AOS04    DD  DISP=SHR,DSN=SYS1.AOS04
//AOS05    DD  DISP=SHR,DSN=SYS1.AOS05
//AOS06    DD  DISP=SHR,DSN=SYS1.AOS06
//AOS07    DD  DISP=SHR,DSN=SYS1.AOS07
//AOS11    DD  DISP=SHR,DSN=SYS1.AOS11
//AOS12    DD  DISP=SHR,DSN=SYS1.AOS12
//AOS20    DD  DISP=SHR,DSN=SYS1.AOS20
//AOS21    DD  DISP=SHR,DSN=SYS1.AOS21
//AOS22    DD  DISP=SHR,DSN=SYS1.AOS22
//AOS23    DD  DISP=SHR,DSN=SYS1.AOS23
//AOS26    DD  DISP=SHR,DSN=SYS1.AOS26
//AOS29    DD  DISP=SHR,DSN=SYS1.AOS29
//RESLIB   DD  DISP=SHR,DSN=SYS1.AUSERSVC
//SVCLIBA  DD  DISP=SHR,DSN=SYS1.AUSERSVC
//AUSERSVC DD  DISP=SHR,DSN=SYS1.AUSERSVC
??
//*
//*-----------------------------------------------------------------***
//*    Run JCLIN with stage 2 to create CDS entries for             ***
//*    the new system.                                              ***
//*-----------------------------------------------------------------***
//JCLIN   EXEC PGM=HMASMP,PARM='DATE=U'
//SMPOUT    DD SYSOUT=A
//SMPLOG    DD DISP=MOD,DSN=SYS1.SMPLOG
//SMPCDS    DD DISP=SHR,DSN=SYS1.SMPCDS
//SMPPTS    DD DISP=OLD,DSN=SYS1.SMPPTS
//SMPCNTL   DD *
  JCLIN.
/*
//SMPJCLIN  DD DATA,DLM='??'
