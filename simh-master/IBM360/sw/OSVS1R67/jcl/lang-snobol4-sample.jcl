//SMSNBL4 JOB 1,1,MSGCLASS=A,REGION=512K
//DATA   EXEC  PGM=SNOBOL4                                              00000200
//STEPLIB  DD  DSN=SYS2.SNOBOL4.LINKLIB,DISP=SHR
//FT06F001 DD  SYSOUT=A                                                 00000400
//FT07F001 DD  SYSOUT=A                                                 00000500
//FT05F001 DD  *
*
*	THIS IS A SAMPLE SNOBOL PROGRAM
*
         OUTPUT = 'HELLO, SNOBOL4.'
END
//
