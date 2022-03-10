//G04INTDS  JOB 1,'Init misc DASD',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:      G04INTDS                                           ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Initialize miscellaneous DASD                      ***
//*              for new VS1 6.0 system:                            ***
//*                - FGEN60 sysres volume                           ***
//*                - PAGE83 paging volume                           ***
//*    Run on:   Generating system                                  ***
//*    Update:   2020/09/03                                         ***
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
//ASAMPLIB  DD UNIT=SYSDA,VOL=SER=FDLB60,
//             DISP=OLD,DSN=SYS1.ASAMPLIB(IEAIPL00)
//SYSIN     DD * 
  ANALYZE  TODD=149,VTOC=1,EXTENT=29,IPLDD=ASAMPLIB,                   +
               NEWVOLID=FGEN60,PURGE=YES
  ANALYZE  TODD=151,VTOC=1,EXTENT=18,                                  +
               NEWVOLID=PAGE83,PURGE=YES
/*
//
