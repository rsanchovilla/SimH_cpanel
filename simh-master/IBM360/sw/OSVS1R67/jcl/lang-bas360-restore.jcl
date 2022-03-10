//IBAS360 JOB CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//********************************************************************  00020000
//*                                                                     00030000
//*   COPY THE BASIC360 DISTRIBUTION FROM TAPE TO DISK                  00040000
//*                                                                     00050000
//*   ALL DATASET CREATED BY THIS JOB WILL BE CREATED WITH THE          00060000
//*   HIGH LEVEL QUALIFIER OF HERC01 ON VOLUME PUB000.  YOU CAN         00070000
//*   DO A CHANGE ALL TO USE A DIFFERENT QUALIFIER OR VOLUME.           00080000
//*                                                                     00090000
//*   THE DISTRIBUTION TAPE CONTAINS THE FOLLOWING DATASETS:            00100000
//*   LABEL   DSN        DESCRIPTION                                    00110000
//*       1   DISTRO1    THIS JCL                                       00120000
//*       2   DISTRO2    IEBCOPY UNLOADED HERC01.BASIC360.LOADLIB       00130000
//*       3   DISTRO3    IEBCOPY UNLOADED HERC01.BASIC360.PLI           00140000
//*                                                                     00150000
//********************************************************************  00400000
//*                                                                     00410000
//*  THIS WILL RESTORE THE TAPE TO PDS USING IEBCOPY                    00420000
//*                                                                     00430000
//********************************************************************  00440000
//STEP02  EXEC PGM=IEBCOPY
//SYSPRINT  DD SYSOUT=A
//SYSUT3    DD UNIT=SYSDA,SPACE=(TRK,(90,90))
//SYSUT4    DD UNIT=SYSDA,SPACE=(TRK,(90,90))
//IN02      DD DSN=DISTRO2,DISP=(OLD,KEEP),
//             UNIT=TAPE,VOL=(PRIVATE,RETAIN,SER=BAS220),
//             LABEL=(2,SL)
//OUT02     DD DSN=SYS2.BASIC360.LOADLIB,DISP=(NEW,CATLG,KEEP),                   
//             DCB=(RECFM=U,BLKSIZE=19068),
//             SPACE=(TRK,(25,15,15)),
//             UNIT=SYSDA,VOL=SER=FGEN67         
//IN03      DD DSN=DISTRO3,DISP=(OLD,KEEP),                        
//             UNIT=TAPE,VOL=(PRIVATE,RETAIN,SER=BAS220),             
//             LABEL=(3,SL)                                    
//OUT03     DD DSN=SYS2.BASIC360.PLI,DISP=(NEW,CATLG,KEEP),               
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),
//             SPACE=(TRK,(30,30,30)),
//             UNIT=SYSDA,VOL=SER=FGEN67
//SYSIN     DD *                                                           
 COPY INDD=IN02,OUTDD=OUT02                                             
 COPY INDD=IN03,OUTDD=OUT03                                             
//*                                                                     
//
