   SET
   DPD TYPE=F,UNIT=X'132',VOLID=SYSDPD,CYL=1
// EXEC    PROC=VSAMRPS  - LOAD SYSTEM SVA/GETVIS AREAS
// EXEC    PROC=LABELS14 - LOAD 2314 SYSTEM STANDARD LABELS 
   ASSGN   SYSREC,X'131',PERM
   ALLOC   F1=512K,F2=4096K,F3=512K,F4=4096K
   ALLOCR  F1R=48K,F2R=48K,F3R=64K,F4R=64K,BGR=64K
// JOB     STARTUP       - INITIALIZE SYSTEM RECORDING FACILITIES 
/*
/&