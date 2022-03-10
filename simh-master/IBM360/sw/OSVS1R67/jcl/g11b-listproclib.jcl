//LISTPDIR JOB  CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PGM=IEHLIST                                             
//SYSPRINT DD  SYSOUT=A                                                 
//DD1      DD  UNIT=SYSDA,VOL=SER=FGEN67,DISP=OLD,SPACE=(TRK,0)
//SYSIN    DD  *
  LISTPDS  DSNAME=SYS1.PROCLIB
/*                                                                      
//PRINTPDS JOB LIST
//         EXEC PGM=IEBPTPCH
//SYSPRINT DD SYSOUT=A
//SYSUT1   DD DSNAME=SYS1.PROCLIB,DISP=SHR
//SYSUT2   DD SYSOUT=A
//SYSIN    DD *
        PRINT TYPORG=PO
/*    
//


