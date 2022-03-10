//G04INTDS  JOB 1,'Init misc DASD',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:      G04INTDS                                           ***
//*    Product:  VS1 6.7.                                           ***
//*    Purpose:  Initialize miscellaneous DASD                      ***
//*              for new VS1 6.7 system:                            ***
//*                - FGEN67 sysres volume                           ***
//*                - PERM73 storage volume                          ***
//*                - WORK73 work volume                             ***
//*                - PAGE73 paging volume                           ***
//*    Run on:   Generating system                                  ***
//*    Update:   2021/03/13                                         ***
//*                                                                 ***
//*    Note:     All volumes must have been created with            ***
//*              Hercules "dasdinit" and be mounted and             ***
//*              online before this job is run, and the             ***
//*              DLIB volume must have been restored so             ***
//*              SYS1.ASAMPLIB is available for the IPL text.       ***
//*                                                                 ***
//*********************************************************************
//*
//INIT    EXEC PGM=IEHDASDR
//SYSPRINT  DD SYSOUT=A
//ASAMPLIB  DD UNIT=SYSDA,VOL=SER=FDLB67,
//             DISP=OLD,DSN=SYS1.ASAMPLIB(IEAIPL00)
//SYSIN     DD * 
  ANALYZE  TODD=148,VTOC=1,EXTENT=29,IPLDD=ASAMPLIB,                   +
               NEWVOLID=FGEN67,PURGE=YES
  ANALYZE  TODD=14A,VTOC=1,EXTENT=29,                                  +
               NEWVOLID=PERM73,PURGE=YES
  ANALYZE  TODD=14B,VTOC=1,EXTENT=29,                                  +
               NEWVOLID=WORK73,PURGE=YES
  ANALYZE  TODD=151,VTOC=1,EXTENT=18,                                  +
               NEWVOLID=PAGE73,PURGE=YES
/*
//
