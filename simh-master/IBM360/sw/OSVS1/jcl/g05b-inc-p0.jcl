//G05INCP0  JOB 1,'Incr P0/SWA size',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*********************************************************************
//*                                                                 ***
//*    Job:      G06INCR                                            ***
//*    Product:  VS1 6.0.                                           ***
//*    Purpose:  Increment P0 partition size to 3073K               ***
//*              Increment SWA to 1600                              ***
//*    Run on:   Generating system                                  ***
//*    Update:   2021/10                                            ***
//*                                                                 ***
//*********************************************************************
//*
//* 2020/12/12 @kl updates to VS1 6.0 starter system
//*                SYS1.PARMLIB/SYS1.PROCLIB
//*
//PARMLIB EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=A
//SYSUT2  DD  DISP=SHR,DSN=SYS1.PARMLIB,UNIT=3330,VOL=SER=DLIBA1
//SYSIN    DD  *
./ ADD NAME=NIP01
HARDCPY=,DEVSTAT=ALL,PAGE=(V=DLIBA1,BLK=4096)               
./ ADD NAME=DFN01
P0=(3072K,LAST),END                                       
./ ENDUP
/*
//
