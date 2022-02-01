// JOB POWER02    PWRGEN ASSEMBLE POWER
// DLBL IJSYSRL,'POWER.PRIVATE.RELO',99/365,SD
// EXTENT SYSRLB,SPOOL1
// DLBL IJSYSSL,'POWER.PRIVATE.SOURCE',99/365,SD
// EXTENT SYSSLB,SPOOL1
// ASSGN SYSRLB,X'191'
// ASSGN SYSSLB,X'191'
// OPTION CATAL
   ACTION F1
// EXEC ASSEMBLY
*
* PHASE NAME IS POWER
*
*              IAG=NO,             UNKNOWN POWER MACRO OPERAND
*
* DBLK=920,    7 RECS/TRK, 88% TRACK UTILIZATION
* TRACKGP=2    2 TRACKS/GROUP
* NTRKGRP=500  NTRKGRP * TRACKGP = D FILE TRACKS = 1000 (50 CYL)
* MAXJOBS=110  Q FILE RECS = MAXJOBS + 1 (FOR THE Q FILE MASTER REC) 
*                 110 + 1 = 111 RECORDS IN Q FILE
*
* Q FILE RECSIZE = MAX(MASTER RECORD SIZE, DATA RECORD SIZE)
*
* MASTER RECORD SIZE = 208 + (32 * NUMPARTS)
*                    = 208 + (32 * 2)
*                    = 272
*    (NUMPARTS IS NUMBER OF PARTITIONS POWER WILL SUPPORT)
*
* DATA RECORD SIZE   = 151 + 3 * INT((NTRKGRP+7) / 8)
*                    = 151 + 3 * INT((500+7) / 8)
*                    = 151 + 3 * 63
*                    = 340
*
* Q FILE RECORD SIZE 340, 16 RECORDS/2314 TRACK, 7 TRACK Q FILE
*
* SPOOL1: 7 TRACKS QFILE, 2 TRACKS JCL SYSIPT, 1,000 DATA
*
*
POWER    POWER F2=YES,             SUPPORT F2 PARTITION                X
               TAPE=NO,            NO SUPPORT FOR TAPE SPOOLING        X
               SVCCODE=200,        SVC 200 TO INVOKE POWER             X
               ACCOUNT=NO,         NO POWER JOB ACCOUNTING SUPPORT     X
               READER=BOTH,        SUPPORT 2501-CLASS AND 1442-CLASS   X
               AUTOSTR=NO,         NO AUTOSTART                        X
               MAXRW=3,            1 PER UNIT RECORD TASK              X
               MAXCCB=5,           MAXCCB PLUS CONCURRENT TAPE SPOOLS  X
               MAXBUFS=17,         3 UR * 2 + 5 * PARTNS + 1 CONS      X
               RDRCLOS=PAUSE,      EOF/EOJ MEANS WAIT FOR MORE         X
               DISK=2314,          FILES ON 2314 DISKS (DUH)           X
               NUMDDKS=1,          ONLY ONE IJDFILE EXTENT             X
               DBLK=920,           BLKSIZE ON IJDFILE, BEST FOR 2314   X
               TRACKGP=2,          2 TRACKS/QFILE BLOCK, 10 GRPS/CYL   X
               NTRKGP=500,         DEFAULT FOR NR. OF TRACK GROUPS     X
               PRIORTY=5,          DEFAULT PRIORITY VS. 9=HIGHEST      X
               SLI=YES,            SUPPORT SOURCE LIBRARY INCLUDE      X
               SUBLIB=P,           SUBLIBRARY FOR SLI                  X
               STDLINE=100000,     LINES BEFORE SYSLOG MESSAGE         X
               STDCARD=100000,     CARDS BEFORE SYSLOG MESSAGE         X
               ADDITR=50000,       LINES/CARDS BEFORE SUBSEQUENT MSG   X
               LINETAB=,           DEFAULT PRINTER FCB/PRINTER TAPE    X
               JOBSEP=NO,          NO JOB SEPARATOR PAGES              X
               MAXRJS=0,           NO RJE SUPPORT                      X
               RJEND=NO,           NO RJE SUPPORT                      X
               RETRY=6,            NO RJE SUPPORT                      X
               MAXJOBS=110         SUPPORT 110 CONCURRENT JOBS
         END   INIT
/*
// EXEC LNKEDT
/&
