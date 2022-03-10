//HLOPL1  JOB (001),'PL1 HELLO WORLD',                                 
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=256K            
//STEP1  EXEC PGM=IEMAA,PARM='LOAD,NODECK',REGION=52K                  
//SYSPRINT DD SYSOUT=A                                                 
//SYSLIN   DD DSNAME=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=SYSSQ,SPACE=(80,(250,100))                                                     
//SYSUT3   DD DSNAME=&&SYSUT3,
//            UNIT=SYSDA,SPACE=(80,(250,250)),
//            DCB=BLKSIZE=80                                           
//SYSUT1   DD DSNAME=&&SYSUT1,
//            UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),
//            DCB=BLKSIZE=1024                     
//SYSIN DD *                                                       
     HELLO: PROCEDURE OPTIONS(MAIN);   
        PUT FILE(SYSPRINT) LIST('HOLA'); 
     END;                        
/*
//LKED   EXEC PGM=IEWL,PARM=(XREF,LET,LIST,MAP),REGION=96K                                               
//SYSPRINT DD SYSOUT=A 
//SYSLIB   DD DSNAME=SYS1.PL1LIB,DISP=SHR,UNIT=SYSDA
//SYSLMOD  DD DSNAME=&&GOSET(GO),DISP=(MOD,PASS),
//            UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)
//SYSUT1   DD DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),
//            DCB=BLKSIZE=1024
//SYSLIN   DD DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//         DD DDNAME=SYSIN
/*
//GO     EXEC PGM=*.LKED.SYSLMOD
//SYSPRINT DD SYSOUT=A                            
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR          
/*
//                                         