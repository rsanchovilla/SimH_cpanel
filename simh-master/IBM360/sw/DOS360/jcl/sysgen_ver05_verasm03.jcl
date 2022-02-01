* $$ JOB VERASM03,,,F2
// JOB VERASM03  DOS TAPE ASSEMBLER TEST F2
// OPTION LINK
// EXEC ASSEMBLY
SMPL     TITLE 'DOS ASSEMBLER TEST'                                     $4650001
*
* WRITE 2000 15-BYTE TAPE RECORDS WITH RECORD NUMBER IN POS 12-15
*
         START 0                                                        $4650003
TESTGEN  BALR  8,0                      LOAD BASE REG                   $4650004
         USING *,8                                                      $4650005
         LA    13,SAVEAREA                                              $4650006
         OPEN  OUTFILE                  OPEN OUTPUT FILE                $4650007
         LA    3,2000                   GET STARTING RECORD VALUE       $4650008
*
NEXT     CVD   3,DWD                    CONVERT VALUE TO DEC.           $4650009
         UNPK  11(4,2),DWD              UNPACK IT                       $4650010
         OI    14(2),X'F0'              MASK SIGN BIT                   $4650011
         PUT   OUTFILE                  WRITE RECORD ON TAPE            $4650012
         BCT   3,NEXT                   DECREMENT R3, BR IF >0          $4650013
*
         CLOSE OUTFILE                  CLOSE TAPE FILE                 $4650014
         EOJ                            RETURN TO CONTROL PROGRAM       $4650015
         SPACE 2
A1       DC    40C' '                                                   $4650019
A2       DC    40C' '                                                   $4650020
DWD      DS    D                                                        $4650021
SAVEAREA DS    9D                                                       $4650022
         SPACE 2
OUTFILE  DTFMT TYPEFLE=OUTPUT,          OUTPUT FILE                    X
               DEVADDR=SYS002,          ASSGN'D TO SYS002              X
               FILABL=STD,              STANDARD LABEL TAPE VOLUME     X
               BLKSIZE=40,              UNBLOCKED 15-BYTE RECORDS      X
               IOAREA1=A1,IOAREA2=A2,   TWO I/O AREAS                  X
               IOREG=(2)                POINT R2 AT CURRENT I/O AREA
         SPACE 2
         PRINT NOGEN
         MTMOD RECFORM=FIXUNB                                           $4650018
         SPACE 2
         END   TESTGEN                                                  $4650023
/*
// LBLTYP TAPE
// EXEC LNKEDT
// ASSGN SYS002,X'280'
/* PAUSE SCRATCH ON X'280'
// MTC REW,SYS002
// TLBL OUTFILE,,0
// EXEC
// MTC REW,SYS002
// UPSI 1
// EXEC DITTO
$$DITTO  TP    INPUT=SYS002,NBLKS=0200
$$DITTO  REW   OUTPUT=SYS002
$$DITTO  EOJ
/*
// TLBL SORTOUT,'SORTED.OUTFILE',0
// TLBL SORTIN1,'OUTFILE'
// DLBL SORTWK1,'DOS.F2.WORKFILE.001',0,SD
// EXTENT SYS003,WRK14B,1,1,1700,300
// DLBL SORTWK2,'DOS.F2.WORKFILE.002',0,SD
// EXTENT SYS004,WRK14B,1,1,2000,300
// DLBL SORTWK3,'DOS.F2.WORKFILE.003',0,SD
// EXTENT SYS005,WRK14B,1,1,2300,300
// ASSGN SYS001,X'281'
// ASSGN SYS002,X'280'
// ASSGN SYS003,X'193'
// ASSGN SYS004,X'193'
// ASSGN SYS005,X'193'
// EXEC SORT
 SORT FIELDS=(1,15,CH,A),WORK=3,FILES=1
 RECORD TYPE=F,LENGTH=40
 INPFIL BLKSIZE=40  
 OUTFIL BLKSIZE=40  
 OPTION PRINT=ALL,LABEL=(S,S)
 END
/*
// MTC REW,SYS001
// UPSI 1
// EXEC DITTO
$$DITTO  TP    INPUT=SYS001,NBLKS=0200
$$DITTO  REW   OUTPUT=SYS001
$$DITTO  REW   OUTPUT=SYS002
$$DITTO  EOJ
/*
/&
* $$EOJ