//ICOMPPR JOB 'ROBERTO SANCHO',CLASS=A,MSGCLASS=A
//STEP1   EXEC PGM=IEBUPDTE,REGION=1024K
//*
//* ***************************************************************** *
//* CREATE MVT COMPILER PROCEDURES IN SYS1.PROCLIB                    *
//* ***************************************************************** *
//*
//SYSUT1   DD  DSN=SYS1.PROCLIB,DISP=OLD
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=OLD
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=COBEC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//COB    EXEC  PGM=IEPCBL00,REGION=24K                                    
//SYSPRINT DD SYSOUT=A                                                    
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSUT1 DD    UNIT=SYSDA,SPACE=(CYL,(8,4))                               
//SYSUT2 DD    UNIT=(SYSDA,SEP=SYSUT1),SPACE=(CYL,(16,4))                 
//SYSUT3 DD    UNIT=(SYSDA,SEP=(SYSUT1,SYSUT2)),SPACE=(CYL,(16,2))        
//SYSIN DD DDNAME=SYSIN,DCB=BLKSIZE=80                                    
./ ADD NAME=COBELG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//LKED   EXEC  PGM=IEWL,PARM=(XREF,LIST,LET),REGION=96K                   
//SYSLIN   DD  DDNAME=SYSIN                                               
//SYSLMOD  DD  DSNAME=&GODATA(RUN),DISP=(NEW,PASS),UNIT=SYSDA,         X  
//             SPACE=(1024,(50,20,1))                                     
//SYSLIB DD    DSNAME=SYS1.COBLIB,DISP=SHR                                
//SYSUT1     DD UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(50,20))    
//SYSPRINT DD  SYSOUT=A                                                   
//GO       EXEC PGM=*.LKED.SYSLMOD,COND=(5,LT,LKED)                       
//SYSABEND DD  SYSOUT=A                                                   
//SYSOUT   DD  SYSOUT=A,DCB=(,BLKSIZE=120,LRECL=120)                      
./ ADD NAME=COBUC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//COB  EXEC  PGM=IKFCBL00,PARM='DECK,NOLOAD,SUPMAP',REGION=86K            
//SYSPRINT  DD SYSOUT=A                                                   
//SYSPUNCH DD SYSOUT=B                                                    
//SYSUT1 DD  UNIT=SYSDA,SPACE=(460,(700,100))                             
//SYSUT2 DD  UNIT=SYSDA,SPACE=(460,(700,100))                             
//SYSUT3 DD  UNIT=SYSDA,SPACE=(460,(700,100))                             
//SYSUT4 DD  UNIT=SYSDA,SPACE=(460,(700,100))                             
./ ADD NAME=COBUCG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//COB EXEC PGM=IKFCBL00,PARM='LOAD',REGION=86K                            
//SYSPRINT DD SYSOUT=A                                                    
//SYSUT1 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT2 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT3 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT4 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSLIN DD DSNAME=&LOADSET,DISP=(MOD,PASS),                              
//             UNIT=SYSDA,SPACE=(80,(500,100))                            
//GO EXEC PGM=LOADER,PARM='MAP,LET',COND=(5,LT,COB),REGION=100K           
//SYSLIN DD DSNAME=*.COB.SYSLIN,DISP=(OLD,DELETE)                         
//SYSLOUT DD SYSOUT=A                                                     
//SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                                   
./ ADD NAME=COBUCLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//COB  EXEC  PGM=IKFCBL00,REGION=86K,PARM='LOAD,SUPMAP'                   
//SYSPRINT  DD SYSOUT=A                                                   
//SYSUT1 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT2 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT3 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSUT4 DD UNIT=SYSDA,SPACE=(460,(700,100))                              
//SYSLIN DD DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,                   
//             SPACE=(80,(500,100))                                       
//LKED EXEC PGM=IEWL,PARM='LIST,XREF,LET',COND=(5,LT,COB),REGION=96K      
//SYSLIN  DD DSNAME=&LOADSET,DISP=(OLD,DELETE)                            
//  DD  DDNAME=SYSIN                                                      
//SYSLMOD DD DSNAME=&GODATA(RUN),DISP=(NEW,PASS),UNIT=SYSDA,              
//             SPACE=(1024,(50,20,1))                                     
//SYSLIB DD   DSNAME=SYS1.COBLIB,DISP=SHR                                 
//SYSUT1 DD UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(50,20))        
//SYSPRINT DD SYSOUT=A                                                    
//GO  EXEC PGM=*.LKED.SYSLMOD,COND=((5,LT,COB),(5,LT,LKED))               
./ ADD NAME=COBULG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//LKED  EXEC  PGM=IEWL,PARM='LIST,XREF,LET',REGION=96K                    
//SYSLIN DD DDNAME=SYSIN                                                  
//SYSLMOD DD DSNAME=&GODATA(RUN),DISP=(NEW,PASS),UNIT=SYSDA,              
//             SPACE=(1024,(50,20,1))                                     
//SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                                   
//SYSUT1 DD UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(50,20))        
//SYSPRINT  DD SYSOUT=A                                                   
//GO  EXEC PGM=*.LKED.SYSLMOD,COND=(5,LT,LKED)                            
./ ADD NAME=COBECLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//COB    EXEC  PGM=IEPCBL00,REGION=24K                                    
//SYSPRINT DD SYSOUT=A                                                    
//SYSUT1 DD    UNIT=SYSDA,SPACE=(CYL,(8,4))                               
//SYSUT2 DD    UNIT=(SYSDA,SEP=SYSUT1),SPACE=(CYL,(16,4))                 
//SYSUT3 DD    UNIT=(SYSDA,SEP=(SYSUT1,SYSUT2)),SPACE=(CYL,(16,2))        
//SYSPUNCH DD  DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,             X  
//             SPACE=(TRK,(50,10))                                        
//SYSIN DD DDNAME=SYSIN,DCB=BLKSIZE=80                                    
//LKED EXEC PGM=IEWL,PARM=(XREF,LIST,LET),COND=(9,LT,COB),REGION=96K      
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(OLD,DELETE)                          
//         DD  DDNAME=SYSIN                                               
//SYSLMOD  DD  DSNAME=&GODATA(RUN),DISP=(NEW,PASS),UNIT=SYSDA,         X  
//             SPACE=(1024,(50,20,1))                                     
//SYSLIB   DD  DSNAME=SYS1.COBLIB,DISP=SHR                                
//SYSUT1   DD  UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(50,20))     
//SYSPRINT DD  SYSOUT=A                                                   
//GO       EXEC PGM=*.LKED.SYSLMOD,COND=((9,LT,COB),(5,LT,LKED))          
//SYSOUT   DD  SYSOUT=A,DCB=(,BLKSIZE=120,LRECL=120)                      
//SYSABEND DD  SYSOUT=A                                                   
./ ADD NAME=FORTGC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT     EXEC PGM=IEYFORT,REGION=100K                                   
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,             X  
//             SPACE=(80,(200,100),RLSE),DCB=BLKSIZE=80                   
./ ADD NAME=FORTGCL,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT     EXEC PGM=IEYFORT,REGION=100K                                   
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,             X  
//             SPACE=(80,(200,100),RLSE),DCB=BLKSIZE=80                   
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(XREF,LET,LIST),COND=(4,LT,FORT)     
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),DISP=(NEW,PASS),UNIT=SYSDA,         X  
//             SPACE=(1024,(20,10,1),RLSE),DCB=BLKSIZE=1024               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(100,10),RLSE),DCB=BLKSIZE=1024, X  
//             DSNAME=&SYSUT1                                             
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(OLD,DELETE)                          
//         DD  DDNAME=SYSIN                                               
./ ADD NAME=FORTGCLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT     EXEC PGM=IEYFORT,REGION=100K                                   
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,             X  
//             SPACE=(80,(200,100),RLSE),DCB=BLKSIZE=80                   
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(XREF,LET,LIST),COND=(4,LT,FORT)     
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),DISP=(NEW,PASS),UNIT=SYSDA,         X  
//             SPACE=(1024,(20,10,1),RLSE),DCB=BLKSIZE=1024               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(100,10),RLSE),DCB=BLKSIZE=1024, X  
//             DSNAME=&SYSUT1                                             
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(OLD,DELETE)                          
//         DD  DDNAME=SYSIN                                               
//GO EXEC PGM=*.LKED.SYSLMOD,COND=((4,LT,FORT),(4,LT,LKED))               
//FT05F001 DD  DDNAME=SYSIN                                               
//FT06F001 DD  SYSOUT=A                                                   
//FT07F001 DD  SYSOUT=B                                                   
./ ADD NAME=FORTGLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(XREF,LET,LIST)                      
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),DISP=(NEW,PASS),UNIT=SYSDA,         X  
//             SPACE=(1024,(20,10,1),RLSE),DCB=BLKSIZE=1024               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(100,10),RLSE),DCB=BLKSIZE=1024, X  
//             DSNAME=&SYSUT1                                             
//SYSLIN   DD  DDNAME=SYSIN                                               
//GO  EXEC  PGM=*.LKED.SYSLMOD,COND=(4,LT,LKED)                           
//FT05F001 DD  DDNAME=SYSIN                                               
//FT06F001 DD  SYSOUT=A                                                   
//FT07F001 DD  SYSOUT=B  
./ ADD NAME=FORTGCLD,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT      EXEC    PGM=IEYFORT,REGION=100K                               
//SYSPRINT  DD      SYSOUT=A                                              
//SYSPUNCH  DD      SYSOUT=B                                              
//SYSLIN    DD      DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,        X  
//             SPACE=(80,(200,100),RLSE),DCB=BLKSIZE=80                   
//GO        EXEC    PGM=LOADER,PARM=(MAP,LET,PRINT),COND=(4,LT,FORT)      
//SYSLIB    DD      DSNAME=SYS1.FORTLIB,DISP=SHR                          
//SYSLOUT   DD      SYSOUT=A                                              
//SYSLIN    DD      DSNAME=*.FORT.SYSLIN,DISP=(OLD,DELETE)                
//FT05F001  DD      DDNAME=SYSIN                                          
//FT06F001  DD      SYSOUT=A                                              
//FT07F001  DD      SYSOUT=B                                              
./ ADD NAME=FORTHC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT   EXEC  PGM=IEKAA00,REGION=228K                                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,UNIT=SYSSQ,DISP=(MOD,PASS),             *  
//             SPACE=(400,(200,50),RLSE)                                  
./ ADD NAME=FORTHCL,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT   EXEC  PGM=IEKAA00,REGION=228K                                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,UNIT=SYSSQ,DISP=(MOD,PASS),             *  
//             SPACE=(400,(200,50),RLSE)                                  
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(MAP,LET,LIST),COND=(4,LT,FORT)      
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),UNIT=SYSDA,DISP=(,PASS),            *  
//             SPACE=(3072,(30,10,1),RLSE)                                
//SYSLIN   DD  DSNAME=&LOADSET,DISP=(OLD,DELETE)                          
//         DD  DDNAME=SYSIN                                               
//SYSUT1 DD DSNAME=&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),SEP=SYSLMOD   
./ ADD NAME=FORTHCLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT   EXEC  PGM=IEKAA00,REGION=228K                                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSLIN   DD  DSNAME=&LOADSET,UNIT=SYSSQ,DISP=(MOD,PASS),             *  
//             SPACE=(400,(200,50),RLSE)                                  
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(MAP,LET,LIST),COND=(4,LT,FORT)      
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),UNIT=SYSDA,DISP=(,PASS),            *  
//             SPACE=(3072,(30,10,1),RLSE)                                
//SYSUT1 DD DSNAME=&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),SEP=SYSLMOD   
//SYSLIN DD    DSNAME=&LOADSET,DISP=(OLD,DELETE)                          
//         DD  DDNAME=SYSIN                                               
//GO  EXEC PGM=*.LKED.SYSLMOD,COND=((4,LT,FORT),(4,LT,LKED))              
//FT05F001 DD  DDNAME=SYSIN                                               
//FT06F001 DD  SYSOUT=A                                                   
//FT07F001 DD SYSOUT=B                                                    
./ ADD NAME=FORTHLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//LKED EXEC PGM=IEWL,REGION=96K,PARM=(MAP,LET,LIST)                       
//SYSLIB   DD  DSNAME=SYS1.FORTLIB,DISP=SHR                               
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DDNAME=SYSIN                                               
//SYSLMOD  DD  DSNAME=&GOSET(MAIN),UNIT=SYSDA,DISP=(,PASS),            *  
//             SPACE=(3072,(30,10,1),RLSE)                                
//SYSUT1 DD DSNAME=&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),SEP=SYSLMOD   
//GO  EXEC  PGM=*.LKED.SYSLMOD,COND=(4,LT,LKED)                           
//FT05F001 DD  DDNAME=SYSIN                                               
//FT06F001 DD  SYSOUT=A                                                   
//FT07F001 DD  SYSOUT=B                                                   
./ ADD NAME=FORTHCLD,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//FORT     EXEC  PGM=IEKAA00,REGION=256K                                  
//SYSPRINT DD SYSOUT=A                                                    
//SYSPUNCH DD SYSOUT=B                                                    
//SYSLIN   DD DSNAME=&LOADSET,UNIT=SYSDA,DISP=(MOD,PASS),                 
//            SPACE=(400,(200,50),RLSE)                                   
//GO       EXEC PGM=LOADER,PARM=(MAP),COND=(4,LT,FORT)                    
//SYSLIB   DD DSNAME=SYS1.FORTLIB,DISP=SHR                                
//SYSLOUT  DD SYSOUT=A                                                    
//SYSLIN   DD DSNAME=*.FORT.SYSLIN,DISP=(OLD,PASS)                        
//FT05F001 DD DDNAME=SYSIN                                                
//FT06F001 DD SYSOUT=A                                                    
//FT07F001 DD SYSOUT=B                                                    

./ ADD NAME=PL1LFG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//GO     EXEC  PGM=LOADER,PARM='MAP,PRINT',REGION=96K                     
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR                                
//SYSLOUT  DD  SYSOUT=A                                                   
//SYSPRINT DD  SYSOUT=A                                                   
./ ADD NAME=PL1DFC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//PL1D   EXEC  PGM=IEMAA,PARM='DECK,NOLOAD',REGION=52K                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSPUNCH DD  SYSOUT=B                                                   
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *  
//             DCB=BLKSIZE=80                                             
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*  
//             SEP=(SYSUT3,SYSPUNCH),DCB=BLKSIZE=1024                     
./ ADD NAME=PL1LFC,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK',REGION=52K                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *  
//             SPACE=(80,(250,100))                                       
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *  
//             DCB=BLKSIZE=80                                             
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*  
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024                       
./ ADD NAME=PL1LFCG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK',REGION=52K                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *  
//             SPACE=(80,(250,100))                                       
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *  
//             DCB=BLKSIZE=80                                             
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*  
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024                       
//GO     EXEC  PGM=LOADER,PARM='MAP,PRINT',REGION=96K,                 *  
//             COND=(9,LT,PL1L)                                           
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR                                
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)                         
//SYSLOUT  DD  SYSOUT=A                                                   
//SYSPRINT DD  SYSOUT=A                                                   
./ ADD NAME=PL1LFCL,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK',REGION=52K                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *  
//             SPACE=(80,(250,100))                                       
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *  
//             DCB=BLKSIZE=80                                             
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*  
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024                       
//LKED   EXEC  PGM=IEWL,PARM='XREF,LIST',COND=(9,LT,PL1L),             *  
//             REGION=96K                                                 
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR                                
//SYSLMOD  DD  DSNAME=&&GOSET(GO),DISP=(MOD,PASS),                     *  
//             UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)                     
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),       *  
//             SEP=(SYSLMOD,SYSLIB),DCB=BLKSIZE=1024                      
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)                         
//         DD  DDNAME=SYSIN                                               
./ ADD NAME=PL1LFLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//LKED   EXEC  PGM=IEWL,PARM='XREF,LIST',REGION=96K                       
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR                                
//SYSLMOD  DD  DSNAME=&&GOSET(GO),DISP=(MOD,PASS),                     *  
//             UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)                     
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),       *  
//             SEP=(SYSLMOD,SYSLIB),DCB=BLKSIZE=1024                      
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DDNAME=SYSIN                                               
//GO     EXEC  PGM=*.LKED.SYSLMOD,COND=(9,LT,LKED)                        
//SYSPRINT DD  SYSOUT=A                                                   
./ ADD NAME=PL1LFCLG,LIST=ALL
./ NUMBER NEW1=01,INCR=05
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK',REGION=52K                    
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *  
//             SPACE=(80,(250,100))                                       
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *  
//             DCB=BLKSIZE=80                                             
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*  
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024                       
//LKED   EXEC  PGM=IEWL,PARM='XREF,LIST',COND=(9,LT,PL1L),             *  
//             REGION=96K                                                 
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR                                
//SYSLMOD  DD  DSNAME=&&GOSET(GO),DISP=(MOD,PASS),                     *  
//             UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)                     
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),       *  
//             SEP=(SYSLMOD,SYSLIB),DCB=BLKSIZE=1024                      
//SYSPRINT DD  SYSOUT=A                                                   
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)                         
//         DD  DDNAME=SYSIN                                               
//GO     EXEC  PGM=*.LKED.SYSLMOD,COND=((9,LT,LKED),(9,LT,PL1L))          
//SYSPRINT DD  SYSOUT=A                                                   
./ ENDUP
><
//
