//STAGE1   JOB  MSGLEVEL=1
//ASMBLR   EXEC PGM=ASMBLR,REGION=128K
//SYSLIB   DD  DSN=SYS1.GENLIB,DISP=SHR,VOL=SER=DLIB04,UNIT=2311
//OBJPDS   DD  DSN=SYS1.OBJPDS,VOLUME=(,RETAIN,SER=WORK01),            X
//             DISP=(,CATLG),UNIT=3330,SPACE=(TRK,(45,5,12))
//SYSUT1   DD  DSN=SYS1.SG1,UNIT=3330,VOL=(,RETAIN,SER=WORK02),        X
//             SPACE=(CYL,(150,5)),DISP=(,CATLG)                 
//SYSUT2   DD  DSN=SYS1.SG2,UNIT=3330,VOL=(,RETAIN,SER=WORK01),        X
//             SPACE=(CYL,(100,5)),DISP=(,CATLG)                 
//SYSUT3   DD  DSN=SYS1.SG3,UNIT=3330,VOL=(,RETAIN,SER=WORK02),        X
//             SPACE=(CYL,(160,5)),DISP=(,CATLG)                 
//SYSUT4   DD  DSN=SYS1.SG4,UNIT=3330,VOL=(,RETAIN,SER=WORK02),        X
//             SPACE=(CYL,(2,5)),DISP=(,CATLG)
//SYSPUNCH DD  UNIT=00D,DCB=BLKSIZE=80
//SYSPRINT DD  SYSOUT=A,SPACE=(CYL,(100,10))
//SYSIN    DD  *
         CENPROCS  MODEL=50,INSTSET=UNIV,FEATURE=PROTECT
*
* I/O CONFIGURATION
*
MPX      CHANNEL   ADDRESS=0,TYPE=MULTIPLEXOR
CON01F   IODEVICE  UNIT=1052,ADDRESS=01F,MODEL=7
CNT00    IOCONTRL  UNIT=2821,MODEL=4,ADDRESS=00
DEV00C   IODEVICE  UNIT=2540R,ADDRESS=00C,MODEL=1
DEV00D   IODEVICE  UNIT=2540P,ADDRESS=00D,MODEL=1
DEV00E   IODEVICE  UNIT=1403,MODEL=N1,ADDRESS=00E,FEATURE=UNVCHSET
CNT01    IOCONTRL  UNIT=2703,ADDRESS=03
LINE410  IODEVICE UNIT=1050,ADAPTER=IBM1,ADDRESS=(030,16),             X
               FEATURE=(AUTOANSR)
CHAN1    CHANNEL   ADDRESS=1,TYPE=SELECTOR
         IOCONTRL  UNIT=3811,ADDRESS=15
DISK150  IODEVICE  UNIT=3330,MODEL=1,ADDRESS=(150,8)
CONT18   IOCONTRL  UNIT=3803,MODEL=2,ADDRESS=18
TAPE180  IODEVICE  UNIT=3420,MODEL=3,ADDRESS=(180,4),FEATURE=(9-TRACK)
         IOCONTRL  UNIT=2841,ADDRESS=19
         IODEVICE  UNIT=2311,ADDRESS=(190,4)
CHAN2    CHANNEL   ADDRESS=2,TYPE=SELECTOR
         IOCONTRL  UNIT=3811,ADDRESS=25
DISK250  IODEVICE  UNIT=3330,MODEL=1,ADDRESS=(250,8)
         IOCONTRL  UNIT=2841,ADDRESS=29
         IODEVICE  UNIT=2311,ADDRESS=(290,4)
         UNITNAME  NAME=SYSDA,UNIT=((150,8),(190,4),(250,8),(290,4))
         UNITNAME  NAME=SYSSQ,UNIT=((150,8))
         UNITNAME  NAME=SYSCP,UNIT=00D
         UNITNAME  NAME=TAPE,UNIT=((180,4))
         CTRLPROG  TYPE=MVT,MAXIO=40,ADDTRAN=5,QSPACE=20
         SCHEDULR  TYPE=MVT,STARTR=A-00C,STARTW=A-00E,                 X
               CONSOLE=01F,                                            X
               REPLY=20,WTOBFRS=100,STARTI=AUTO,JOBQFMT=12,JOBQWTP=2,  X
               JOBQLMT=60,JOBQTMT=60,INITQBF=10,VLMOUNT=AVR,           X
               WTLBFRS=10,OPTIONS=(LOG,TSO),                           X
               ACCTRTN=NOTSUPPLIED,ESV=NO,EVA=(5,5)
         SUPRVSOR  TIMER=JOBSTEP,TRACE=100,ASCII=INCRES,SER=SER0,      X
               OPTIONS=(APR,COMM,ONLNTEST,PROTECT,CCH,DDR),ALTSYS=250, X
               WAIT=MULTIPLE
         DATAMGT   ACSMETH=(BDAM,ISAM,TCAM,QTAM)                  
         HELP                                                        
         IMAGELIB                                                    
         MACLIB    EXCLUDE=(QTAM,GPS,OCR,FDM)                            
         PARMLIB                                                     
         PROCLIB                                                     
         SORTLIB                                                     
         SORTMERG  RECTYPE=(FIXED,VAR,LONG),SORTDEV=(2311,2314),       X
               CNTLFLD=(SINGLE,MULTIPLE),SIZE=65536
         TELCMLIB                                                    
         UADS                                                        
         UCS       UNIT=1403,IMAGE=(AN,PN,TN),DEFAULT=(AN,PN,TN)          
*                                                                     
* COMPILERS AND LIBRARIES                                             
*                                                                     
         ALGOL
         ALGLIB
         ASSEMBLR  DESIGN=F
         CHECKER   TYPE=FORTRAN,DESIGN=H
         CHECKER   TYPE=PL1,SIZE=(16K,20K)
         CMDLIB
         COBOL     DESIGN=E
         COBOL     DESIGN=U
         COBLIB    DESIGN=E
         COBLIB    DESIGN=U
         FORTRAN   DESIGN=H
         FORTRAN   DESIGN=G
         FORTLIB   DESIGN=H
         PL1       DESIGN=F
         PL1LIB    LIBFCNS=COMPLEX
         RPG
*
* LINKAGE EDITOR AND LOADER
*
         EDITOR    DESIGN=F128,SIZE=128K
         LOADER
         SYSUTILS  SIZE=64K                                          
*
* GENERATE MACRO
*
         OUTPUT    CLASS=(A,A)
         GENTSO    UT1SDS=SYS1.SG1,UT2SDS=SYS1.SG2,                    X
               UT3SDS=SYS1.SG3,UT4SDS=SYS1.SG4,UTDISP=KEEP,            X
               OBJPDS=SYS1.OBJPDS,ASMPRT=ON,DIRDATA=PDS,LEPRT=MAP,     X
               LNKVOL=MVTRES,LNKNAME=3330,SYOUTDV=3330,RESNAME=151,    X
               RESTYPE=3330,RESVOL=MVTRES,GENTYPE=ALL,INDEX=SYS1
         END                                                     
/*                                                               
//
