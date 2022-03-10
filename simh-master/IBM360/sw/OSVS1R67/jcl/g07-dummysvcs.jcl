//G07SVCS  JOB 1,'Link dummy SVCs',                                    +
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=3072K
//*********************************************************************
//*                                                                 ***
//*    Job:      G07SVCS                                            ***
//*    Product:  VS1 6.7.                                           ***
//*    Purpose:  Link dummy SVCs that will be installed             ***
//*              when stage 2 is run.                               ***
//*    Run on:   Generating system                                  ***
//*    Update:   2021/04/04                                         ***
//*                                                                 ***
//*********************************************************************
//*
//*-----------------------------------------------------------------***
//*    Specify volser of DLIB volume.                               ***
//*-----------------------------------------------------------------***
//VOL     EXEC PGM=IEFBR14
//DLIBVOL   DD DISP=OLD,UNIT=3350,VOL=(PRIVATE,RETAIN,SER=FDLB67)
//*
//*-----------------------------------------------------------------***
//*    Create SYS1.AUSERSVC library of dummy SVCs by linking        ***
//*    IEFBR14 under the name of each SVC.                          ***
//*-----------------------------------------------------------------***
//LKED    EXEC PGM=IEWL,PARM=(XREF,LIST,MAP,RENT,REUS,DC)
//SYSPRINT  DD SYSOUT=A
//LINKLIB   DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSUT1    DD SPACE=(TRK,(90,30)),UNIT=SYSDA
//SYSLMOD   DD DSN=SYS1.AUSERSVC,
//*            DISP=OLD,
//             DISP=(NEW,CATLG),
//             SPACE=(CYL,(1,,10)),
//             UNIT=SYSDA,
//             VOL=(PRIVATE,RETAIN,REF=*.VOL.DLIBVOL)
//SYSLIN    DD *
 CHANGE IEFBR14(IGC255)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC255
 CHANGE IEFBR14(IGC251)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC251
 CHANGE IEFBR14(IGC0024I)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0024I
 CHANGE IEFBR14(IGC0024H)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0024H
 CHANGE IEFBR14(IGC0024B)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0024B
 CHANGE IEFBR14(IGC0023D)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0023D
 CHANGE IEFBR14(IGC0022H)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0022H
 CHANGE IEFBR14(IGC0022G)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0022G
 CHANGE IEFBR14(IGC0023A)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC0023A
 CHANGE IEFBR14(IGC217)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC217
 CHANGE IEFBR14(IGC216)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC216
 CHANGE IEFBR14(IGC214)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC214
 CHANGE IEFBR14(IGC213)
 INCLUDE LINKLIB(IEFBR14)
 NAME IGC213
/*
//
