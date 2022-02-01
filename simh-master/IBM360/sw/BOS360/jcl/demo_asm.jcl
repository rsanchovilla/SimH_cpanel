// JOB DEMOASM
// OPTION LIST,LINK
        ACTION MAP
// EXEC ASSEMBLY
SMPL     TITLE '16 K BOS ASSEMBLER TEST '                               $4060001
         PRINT NOGEN                                                    $4060002
         START 0                                                        $4060003
TESTGEN  BALR  8,0                      LOAD BASE REG                   $4060004
         USING *,8                                                      $4060005
         OPEN  OUTFILE                  OPEN OUTPUT FILE                $4060006
         LA    3,2000                   GET STARTING RECORD VALUE       $4060007
NEXT     CVD   3,DWD                    CONVERT VALUE TO DEC.           $4060008
         UNPK  11(4,2),DWD              UNPACK IT                       $4060009
         OI    14(2),X'F0'              MASK SIGN BIT                   $4060010
         PUT   OUTFILE                  WRITE RECORD ON TAPE            $4060011
         BCT   3,NEXT                   TEST FOR TASK COMPLETE          $4060012
         CLOSE OUTFILE                  CLOSE TAPE FILE                 $4060013
         EOJ                            RETURN TO CONTROL PROGRAM       $4060014
OUTFILE  DTFMT BLKSIZE=15,DEVADDR=SYS002,FILABL=NO,IOAREA1=A1,         X$4060015
               IOAREA2=A2,IOREG=(2),TYPEFLE=OUTPUT                      $4060016
         MTMOD RECFORM=FIXUNB                                           $4060017
A1       DC    15C' '                                                   $4060018
A2       DC    15C' '                                                   $4060019
DWD      DS    D                                                        $4060020
         END   TESTGEN                                                  $4060021
/*
// EXEC LNKEDT
// ASSGN SYS002,X'00E'
// EXEC
/&
