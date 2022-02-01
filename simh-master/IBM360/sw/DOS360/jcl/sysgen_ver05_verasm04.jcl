* $$JOB VERASM04,,,F2
// JOB VERASM04  WRITE DISK FILE IN F2
// OPTION LINK,DUMP
// EXEC ASSEMBLY
*
* WRITE 2000 23-BYTE DISK RECORDS WITH RECORD NUMBER IN POS 12-15
*
         TITLE 'DOS ASSEMBLER TEST'                                     $4650101
         PRINT GEN                                                      $4650102
         START 0                                                        $4650103
TESTGEN  BALR  8,0                   LOAD THE BASE REGISTER.            $4650104
         USING *,8                                                      $4650105
         LA    13,SAVEAREA                                              $4650106
         OPEN  OUTFILE               OPEN THE OUTPUT FILE.              $4650107
         LA    3,2000                GET STARTING RECORD VALUE.         $4650108
*
NEXT     CVD   3,DWD                 CONVERT THE VALUE TO DECIMAL.      $4650109
         UNPK  11(4,2),DWD           UNPACK THE VALUE.                  $4650110
         OI    14(2),X'F0'           MASK THE SIGN BIT.                 $4650111
         PUT   OUTFILE               WRITE A RECORD ON DISK.            $4650112
         BCT   3,NEXT                DECREMENT R3, BR IF >0             $4650013
*
         CLOSE OUTFILE               CLOSE THE DISK FILE.               $4650114
         EOJ                         RETURN TO THE CONTROL PROGRAM.     $4650115
         SPACE 2
A1       DC    23C' '                8-BYTE COUNT AREA, 15-BYTE RECS    $4650119
A2       DC    23C' '                R2 POINTED AT CURRENT IOAREA+8     $4650120
DWD      DS    D                                                        $4650121
SAVEAREA DS    9D                                                       $4650122
         SPACE 2
OUTFILE  DTFSD TYPEFLE=OUTPUT,       OUTPUT FILE                       X
               RECFORM=FIXUNB,       FIXED UNBLOCKED RECORDS           X
               DEVICE=2314,          DISK, RESIDES ON 2314             X
               BLKSIZE=23,           UNBLOCKED 23-BYTE RECS (DISK MIN) X
               IOAREA1=A1,IOAREA2=A2, TWO I/O AREAS                    X
               IOREG=(2)             POINT R2 AT CURRENT I/O AREA
         SPACE 2
         PRINT NOGEN
         SDMODFO                                                        $4650118
         END   TESTGEN                                                  $4650123
/*
// EXEC LNKEDT
// ASSGN SYS010,X'193'
// DLBL OUTFILE,'F2.IVP.WORKFILE',0,SD
// EXTENT SYS010,WRK14B,1,0,1400,300
// EXEC 
// DLBL   UIN,'F2.IVP.WORKFILE',,SD
// EXTENT SYS004,WRK14B
// ASSGN SYS004,X'193'       INPUT FROM FILE CREATED ABOVE
// ASSGN SYS005,X'01E'       OUTPUT TO PRINTER
// UPSI 00000000             STANDARD LABELS ON INPUT
// EXEC DKPR                 DISK TO PRINT
// UDP TL,FF,A=(15,15),B=(120),OC,R1,S1,E=(2314)
// END
/*
// DLBL SORTOUT,'F2.IVP.WORKFILE.SORTED',0,SD
// EXTENT SYS001,WRK14B,1,0,2600,300
// DLBL SORTIN1,'F2.IVP.WORKFILE',,SD
// EXTENT SYS002,WRK14B
// DLBL SORTWK1,'DOS.F2.WORKFILE.001',0,SD
// EXTENT SYS003,WRK14B,1,1,1700,300
// DLBL SORTWK2,'DOS.F2.WORKFILE.002',0,SD
// EXTENT SYS004,WRK14B,1,1,2000,300
// DLBL SORTWK3,'DOS.F2.WORKFILE.003',0,SD
// EXTENT SYS005,WRK14B,1,1,2300,300
// ASSGN SYS001,X'193'
// ASSGN SYS002,X'193'
// ASSGN SYS003,X'193'
// ASSGN SYS004,X'193'
// ASSGN SYS005,X'193'
// EXEC SORT
 SORT FIELDS=(1,15,CH,A),WORK=3,FILES=1
 RECORD TYPE=F,LENGTH=15
 INPFIL BLKSIZE=15
 OUTFIL BLKSIZE=15
 OPTION PRINT=ALL
 END
/*
// DLBL   UIN,'F2.IVP.WORKFILE.SORTED',,SD
// EXTENT SYS004,WRK14B
// ASSGN SYS004,X'193'       INPUT FROM FILE CREATED ABOVE
// ASSGN SYS005,X'02E'       OUTPUT TO PRINTER
// UPSI 00000000             STANDARD LABELS ON INPUT
// EXEC DKPR                 DISK TO PRINT
// UDP TL,FF,A=(15,15),B=(120),OC,R1,S1,E=(2314)
// END
/*
/*
/&
* $$EOJ