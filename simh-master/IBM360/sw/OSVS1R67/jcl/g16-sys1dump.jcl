//G16SDUMP JOB 1,'Catalog SYS1.DUMP',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:        G16SDUMP                                         ***
//*    Product:    VS1 6.7.                                         ***
//*    Purpose:    Catalog SYS1.DUMP on FGEN67.                     ***
//*    Run under:  New system.                                      ***
//*    Update:     2021/04/23                                       ***
//*                                                                 ***
//*    Note:       To complete allocation of SYS1.DUMP, the         ***
//*                system must be re-IPLed.  VS1 will create        ***
//*                a SYS1.DUMP of appropriate size during the       ***
//*                IPL process.                                     ***
//*                                                                 ***
//*********************************************************************
//*
//CATLG   EXEC PGM=IEHPROGM
//SYSPRINT  DD SYSOUT=A
//FGEN67    DD DISP=OLD,UNIT=SYSALLDA,VOL=SER=FGEN67
//SYSIN     DD *
 CATLG CVOL=3350=FGEN67,VOL=3350=FGEN67,DSNAME=SYS1.DUMP
/*
//
