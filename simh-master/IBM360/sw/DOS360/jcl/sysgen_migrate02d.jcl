// JOB MIGR02   COPY/CONVERT 2311 RES11A(130) TO 2314 WRK14A(192)
/*   WORKFILES FOR ASSEMBLY AND LNKEDIT
// DLBL IJSYS01,'DOS.WORKFILE.001',0,SD
// EXTENT SYS001,WRK14A,1,1,600,300
// DLBL IJSYS02,'DOS.WORKFILE.002',0,SD
// EXTENT SYS002,WRK14A,1,1,900,300
// DLBL IJSYS03,'DOS.WORKFILE.003',0,SD
// EXTENT SYS003,WRK14A,1,1,1200,300
// DLBL IJSYSLN,'DOS.WORKFILE.LNK',0,SD
// EXTENT SYSLNK,WRK14A,1,1,1500,300
/* **** ONLY RES11B HAS SOURCE BOOKS, SO MUST ASSIGN AS PRIVATE LIB
// DLBL IJSYSSL,'DOS.SYSRES.FILE.VOLUME.2',,SD
// EXTENT SYSSLB,RES11B,1,1,330,1650
// ASSGN SYS001,X'192'
// ASSGN SYS002,X'192'
// ASSGN SYS003,X'192'
// ASSGN SYSLNK,X'192'
// ASSGN SYSSLB,X'131'   RES11B PRV SLB
// OPTION LINK,NODECK,XREF,DUMP
* ASSEMBLE AND LINK CVT11RES
// EXEC ASSEMBLY
         TITLE 'CONVERT 2311 SYSRES TO 2314'                            C1R00220
         PRINT GEN                                                      C1R00230
CVT11RES START X'1800'                                                  C1R00240
*********************************************************************** C1R00250
*                                                                       C1R00260
* CVT11RES: Create a 199-Cylinder SYSRES on a 2314 disk volume          C1R00270
*                                                                       C1R00280
* Copyright (c) 2015 by Stephen R. Orso, All rights reserved.           C1R00290
*                                                                       C1R00300
*  Redistribution and use in source and binary forms, with or without   C1R00310
*  modification, are permitted provided that the following conditions   C1R00320
*  are met:                                                             C1R00330
*    * Redistributions of source code must retain the above copyright   C1R00340
*      notice, this list of conditions and the following disclaimer.    C1R00350
*    * Redistributions in binary form must reproduce the above          C1R00360
*      copyright notice, this list of conditions and the following      C1R00370
*      disclaimer in the documentation and/or other materials provided  C1R00380
*      with the distribution.                                           C1R00390
*    * The name of the author may not be used to endorse or promote     C1R00400
*      products derived from this software without specific prior       C1R00410
*      written permission.                                              C1R00420
*    * Products derived from this software may not be called "CVT11RES" C1R00430
*      nor may "CVT11RES" appear in their names without specific prior  C1R00440
*      written permission from the author.                              C1R00450
*                                                                       C1R00460
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS" AND ANY    C1R00470
*  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    C1R00480
*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR   C1R00490
*  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE    C1R00500
*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR  C1R00510
*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT    C1R00520
*  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   C1R00530
*  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF           C1R00540
*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT            C1R00550
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE    C1R00560
*  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH     C1R00570
*  DAMAGE.                                                              C1R00580
*                                                                       C1R00590
*********************************************************************** C1R00600
         EJECT                                                          C1R00610
*********************************************************************** C1R00620
*                                                                       C1R00630
* Create a 199-Cylinder SYSRES on a 2314 disk volume                    C1R00640
*    * Matches CORGZ " NEWVOL CL=55(10),RL=77(10),SL=65(10)"            C1R00650
*    * Core image directory starts on cyl 0 track 10 for 10 tracks      C1R00660
*    * Relocatable and Source Statement directories are 10 tracks each  C1R00670
*                                                                       C1R00680
*  1. Copy / update as needed library control records to 2314           C1R00690
*     a. Cyl 0 Head 1 Record 1: Core image status                       C1R00700
*     b. Cyl 0 Head 1 Record 2: pointer to relocatable directory        C1R00710
*     c. Cyl 0 Head 1 Record 3: pointer to soure book directory         C1R00720
*     d. Cyl 0 Head 1 Record 4: not used (DOS/VS used this for proclib) C1R00730
*  3. Copy $$A$IPL2 to 2314 (Cyl 0 Head 1 Record 5)                     C1R00740
*  4. Copy Core image library contents from 2311 to 2314                C1R00750
*     a. Re-block each phase to reflect 2314 shorter CIL block size     C1R00760
*     b. Update directory entry for new placement and block count       C1R00770
*     c. Format the balance of the core image directory with empty      C1R00780
*        directory records.                                             C1R00790
*     d. Format the balance of the core image library with empty        C1R00800
*        library records.                                               C1R00810
*  5. Copy relocatable module library contents from 2311 to 2314        C1R00820
*     a. No reblocking; library blocksizes same on 2311 & 2314          C1R00830
*     b. Update directory entry for new placement and block count       C1R00840
*     c. Format the balance of the relocatable directory with empty     C1R00850
*        directory records.                                             C1R00860
*     d. No need to format the balance of the library.  Apparently      C1R00870
*        MAINT and CORGZ always use formatting writes when adding       C1R00880
*        items to the relocatable library                               C1R00890
*  6. Copy source statement library contents from 2311 to 2314          C1R00900
*     a. No reblocking; library blocksizes same on 2311 & 2314          C1R00910
*     b. Update directory entry for new placement and block count       C1R00920
*     c. Format the balance of the relocatable directory with empty     C1R00930
*        directory records.                                             C1R00940
*     d. No need to format the balance of the library.  Apparently      C1R00950
*        MAINT and CORGZ always use formatting writes when adding       C1R00960
*        items to the source statement library                          C1R00970
*  7. Format/load Cyl 0 Head 5 through Head 9.  These are special       C1R00980
*     core image directories for:                                       C1R00990
*     a.  $$ transients other than $$BO transients (head 5)             C1R01000
*     b.  $$BO open transients (head 6)                                 C1R01010
*     c.  $ phases (not including $$ nor $$BO transients) head 7        C1R01020
*     d.  FGP..... phases (mostly POWER II phases) (head 8)             C1R01030
*     e.  Currently executing phase (head 9)                            C1R01040
*  8. Copy IPL bootstrap records from C0H0R1-2 from 2311 to 2314        C1R01050
*                                                                       C1R01060
*  This program runs in an emulated environment, so no disk error       C1R01070
*     checking is needed, nor is it performed.                          C1R01080
*                                                                       C1R01090
*  And although the Hercules emulator supports the 370 instruction      C1R01100
*  set, this program limits itself to S/360 instructions.  Reasons:     C1R01110
*                                                                       C1R01120
*  1. DOS/360 didn't support the 370 instructions.  A program check     C1R01130
*     on a 370 instruction could very well confuse the error handler.   C1R01140
*  2. This was the toolset available to assembly-language programmers   C1R01150
*     on 360 hardware.  This program serves as an example of how        C1R01160
*     things were done 'back in the day.'                               C1R01170
*  3. Should there arise another emulator that enforces a S/360         C1R01180
*     instruction set, this program will be ready to go.                C1R01190
*                                                                       C1R01200
*  Even though ICM, STCM, and MVCL would be immensely useful here.      C1R01210
*                                                                       C1R01220
*********************************************************************** C1R01230
         TITLE 'CONVERT 2311 SYSRES TO 2314   USEFUL EQUATES'           C1R01240
R0       EQU   0                   WORK REGISTER                        C1R01250
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R01260
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R01270
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R01280
R4       EQU   4                   CURRENT CYLINDER NR                  C1R01290
R5       EQU   5                   TRACK COUNT REMAINING                C1R01300
R6       EQU   6                   CURRENT TRACK NR                     C1R01310
R7       EQU   7                   RECORD COUNT REMAINING               C1R01320
R8       EQU   8                   CURRENT RECORD NR                    C1R01330
R9       EQU   9                                                        C1R01340
R10      EQU   10                                                       C1R01350
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R01360
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R01370
R13      EQU   13                                                       C1R01380
R14      EQU   14                  WORK REGISTER                        C1R01390
R15      EQU   15                  WORK REGISTER                        C1R01400
*                                                                       C1R01410
ENDDIR   EQU   C'*'                END OF DIRECTORY FLAG                C1R01420
EMPTYDIR EQU   C' '                BLANK-EMPTY DIRECTORY ENTRY          C1R01430
*                                                                       C1R01440
*  CCW PROGRAM SYMBOLIC EQUATES                                         C1R01450
*                                                                       C1R01460
SEEK     EQU   X'07'               SEEK BBCCHH                          C1R01470
SRCHIDEQ EQU   X'31'               SEARCH ID CCHHR EQUAL                C1R01480
TIC      EQU   X'08'               TRANSFER IN CHANNEL                  C1R01490
WRITECKD EQU   X'1D'               WRITE COUNT-KEY-DATA                 C1R01500
WRITEDAT EQU   X'05'               WRITE DATA (NON-FORMATTING)          C1R01510
READCKD  EQU   X'1E'               READ COUNT-KEY-DATA                  C1R01520
READDATA EQU   X'06'               READ DATA ONLY                       C1R01530
*                                                                       C1R01540
CMDCHAIN EQU   X'40'               COMMAND-CHAIN CCW                    C1R01550
*                                                                       C1R01560
         TITLE 'CONVERT 2311 SYSRES TO 2314   TRACK GEOMETRY CONSTANTS' C1R01570
*********************************************************************** C1R01580
*                                                                       C1R01590
* DIRECTORY ENTRY SIZE FOR THE VARIOUS LIBRARIES.  THESE ARE THE SAME   C1R01600
* REGARDLESS OF THE DEVICE TYPE HOSTING THE LIBRARY.                    C1R01610
*                                                                       C1R01620
*********************************************************************** C1R01630
GXXCIDES EQU   20                  CORE IMAGE DIRECTORY ENTRY SIZE      C1R01640
GXXRLDES EQU   16                  RELOCATABLE DIRECTORY ENTRY SIZE     C1R01650
GXXSLDES EQU   16                  SOURCE DIRECTORY ENTRY SIZE          C1R01660
*                                                                       C1R01670
         SPACE 3                                                        C1R01680
*********************************************************************** C1R01690
*                                                                       C1R01700
* TRACK GEOMETRY FOR 2311 LIBRARIES.  PRIVATE LIBRARIES USE THE SAME    C1R01710
* GEOMETRY AS THE CORRESPONDING SYSRES LIBRARY.  NOTE THAT RLB AND SLB  C1R01720
* LIBRARIES USE THE SAME DIRECTORY AND LIBRARY BLOCK SIZE ON BOTH 2311  C1R01730
* AND 2314 DASD, ALTHOUGH THE BLOCK COUNTS PER TRACK ARE OBVIOUSLY      C1R01740
* DIFFERENT                                                             C1R01750
*                                                                       C1R01760
*********************************************************************** C1R01770
G11TKCYL EQU   10                  TRACKS/CYLINDER 2311                 C1R01780
G11LSTTK EQU   G11TKCYL-1          NR. OF LAST TRACK IN A CYLINDER      C1R01790
*                                                                       C1R01800
G11CIDBS EQU   360                 CORE IMAGE DIRECTORY BLOCK SIZE      C1R01810
G11CIDEB EQU   G11CIDBS/GXXCIDES   CORE IMAGE DIR ENTRIES/BLOCK         C1R01820
G11CIDBT EQU   8                   CORE IMAGE DIRECTORY BLOCKS/TRACK    C1R01830
G11CILBS EQU   1728                CORE IMAGE LIBRARY BLOCK SIZE        C1R01840
G11CILBT EQU   2                   CORE IMAGE LIBRARY BLOCKS/TRACK      C1R01850
*                                                                       C1R01860
G11RLDBS EQU   320                 RELOCATABLE DIRECTORY BLOCK SIZE     C1R01870
G11RLDEB EQU   G11RLDBS/GXXRLDES   RELOCATABLE DIRECTORY ENTRIES/BLK    C1R01880
G11RLDBT EQU   9                   RELOCATABLE DIRECTORY BLOCKS/TRACK   C1R01890
G11RLBBS EQU   322                 RELOCATABLE LIBRARY BLOCK SIZE       C1R01900
G11RLBBT EQU   9                   RELOCATABLE LIBRARY BLOCKS/TRACK     C1R01910
*                                                                       C1R01920
G11SLDBS EQU   160                 SOURCE DIRECTORY BLOCK SIZE          C1R01930
G11SLDEB EQU   G11SLDBS/GXXSLDES   SOURCE DIRECTORY ENTRIES/BLK         C1R01940
G11SLDBT EQU   16                  SOURCE DIRECTORY BLOCKS/TRACK        C1R01950
G11SLBBS EQU   160                 SOURCE LIBRARY BLOCK SIZE            C1R01960
G11SLBBT EQU   16                  SOURCE LIBRARY BLOCKS/TRACK          C1R01970
         SPACE 3                                                        C1R01980
*********************************************************************** C1R01990
*                                                                       C1R02000
* TRACK GEOMETRY FOR 2314 LIBRARIES.  PRIVATE LIBRARIES USE THE SAME    C1R02010
* GEOMETRY AS THE CORRESPONDING SYSRES LIBRARY.  NOTE THAT RLB AND SLB  C1R02020
* LIBRARIES USE THE SAME DIRECTORY AND LIBRARY BLOCK SIZE ON BOTH 2311  C1R02030
* AND 2314 DASD, ALTHOUGH THE BLOCK COUNTS PER TRACK ARE OBVIOUSLY      C1R02040
* DIFFERENT                                                             C1R02050
*                                                                       C1R02060
*********************************************************************** C1R02070
*                                                                       C1R02080
G14TKCYL EQU   20                  TRACKS/CYLINDER 2311                 C1R02090
G14LSTTK EQU   G14TKCYL-1          NR. OF LAST TRACK IN A CYLINDER      C1R02100
*                                                                       C1R02110
G14CIDBS EQU   360                 CORE IMAGE DIRECTORY BLOCK SIZE      C1R02120
G14CIDEB EQU   G14CIDBS/GXXCIDES   CORE IMAGE DIR ENTRIES/BLOCK         C1R02130
G14CIDBT EQU   15                  CORE IMAGE DIRECTORY BLOCKS/TRACK    C1R02140
G14CILBS EQU   1688                CORE IMAGE LIBRARY BLOCK SIZE        C1R02150
G14CILBT EQU   4                   CORE IMAGE LIBRARY BLOCKS/TRACK      C1R02160
*                                                                       C1R02170
G14RLDBS EQU   320                 RELOCATABLE DIRECTORY BLOCK SIZE     C1R02180
G14RLDEB EQU   G14RLDBS/GXXRLDES   RELOCATABLE DIRECTORY ENTRIES/BLK    C1R02190
G14RLDBT EQU   17                  RELOCATABLE DIRECTORY BLOCKS/TRACK   C1R02200
G14RLBBS EQU   322                 RELOCATABLE LIBRARY BLOCK SIZE       C1R02210
G14RLBBT EQU   16                  RELOCATABLE LIBRARY BLOCKS/TRACK     C1R02220
*                                                                       C1R02230
G14SLDBS EQU   160                 SOURCE DIRECTORY BLOCK SIZE          C1R02240
G14SLDEB EQU   G14SLDBS/GXXSLDES   SOURCE DIRECTORY ENTRIES/BLK         C1R02250
G14SLDBT EQU   27                  SOURCE DIRECTORY BLOCKS/TRACK        C1R02260
G14SLBBS EQU   160                 SOURCE LIBRARY BLOCK SIZE            C1R02270
G14SLBBT EQU   27                  SOURCE LIBRARY BLOCKS/TRACK          C1R02280
*                                                                       C1R02290
         TITLE 'CONVERT 2311 SYSRES TO 2314   NEW SYSRES ALLOCATIONS'   C1R02300
*********************************************************************** C1R02310
*                                                                       C1R02320
* FOR SYSRES, LIBRARIES ALWAYS START ON A CYLINDER BOUNDARY.            C1R02330
*                                                                       C1R02340
* THE CORE IMAGE LIBRARY ALWAYS STARTS ON CYLINDER ZERO, FOLLOWED BY    C1R02350
* THE RELOCATABLE LIBRARY (IF PRESENT) AND THEN THE SOURCE STATEMENT    C1R02360
* LIBRARY (IF PRESENT).  A CYLINDER IS RESERVED AT THE END OF SYSRES    C1R02370
* FOR STORAGE OF SYSTEM (GLOBAL) AND PARTITION-SPECIFIC JCL.            C1R02380
*                                                                       C1R02390
* THE FIRST 10 TRACKS OF THE CORE IMAGE LIBRARY ARE USED FOR SYSTEM     C1R02400
* PURPOSES, AND THE CORE IMAGE DIRECTORY STARTS TRACK ON 10.  LIBRARY   C1R02410
* CONTENT STARTS ON THE FIRST TRACK FOLLOWING THE CORE IMAGE DIRECTORY. C1R02420
*                                                                       C1R02430
* THE RELOCATABLE LIBRARY STARTS ON THE FIRST CYLINDER FOLLOWING THE    C1R02440
* CORE IMAGE LIBRARY. THE DIRECTORY STARTS ON THE FIRST TRACK OF THAT   C1R02450
* CYLINDER, AND LIBRARY CONTENT STARTS ON THE FIRST TRACK FOLLOWING     C1R02460
* THE DIRECTORY.                                                        C1R02470
*                                                                       C1R02480
* THE SOURCE STATEMENT LIBRARY HAS THE SAME LAYOUT AS THE RELOCATABLE   C1R02490
* LIBRARY.                                                              C1R02500
*                                                                       C1R02510
* PRIVATE LIBRARIES HAVE THE SAME LAYOUT AS THE CORRESPONDING SYSTEM    C1R02520
* LIBRARY STORED ON SYSRES, INCLUDING TEN TRACKS FOR SYSTEM PURPOSES    C1R02530
* AT THE BEGINNING OF THE CORE IMAGE LIBRARY.                           C1R02540
*                                                                       C1R02550
*********************************************************************** C1R02560
         SPACE 3                                                        C1R02570
*********************************************************************** C1R02580
*                                                                       C1R02590
* DEFINITIONS FOR THE NEW SYSRES TO BE BUILT ON 2314 DISK               C1R02600
*                                                                       C1R02610
*********************************************************************** C1R02620
         SPACE 1                                                        C1R02630
R14CILCL EQU   56                  CORE IMAGE LIB CYLS, INCLUDING CYL 0 C1R02640
R14CIDTK EQU   10                  CORE IMAGE DIR TRACKS, STARTS C0H10  C1R02650
*                                  ...10 IS MIN & MAX ELSE CODE BREAKS  C1R02660
R14CILTB EQU   R14CILCL*G14TKCYL*G14CILBT   CIL LIBRARY BLOCKS TOTAL    C1R02670
*                                                                       C1R02680
R14RLBST EQU   R14CILCL            STARTING CYLINDER OF RELO DIR/LIB    C1R02690
R14RLBCL EQU   77                  RES RELO LIBRARY CYLS                C1R02700
R14RLDTK EQU   10                  RES RELO DIRECTORY TRACKS            C1R02710
R14RLBTB EQU   (R14RLBCL*G14TKCYL-R14RLDTK)*G14RLBBT  RLB TOTAL BLOCKS  C1R02720
R14RLDAT EQU   R14RLBST*G14TKCYL   RLD ABSOLUTE STARTING TRACK          C1R02730
R14RLBAT EQU   R14RLBST*G14TKCYL+R14RLDTK  RLB ABSOLUTE STARTING TRACK  C1R02740
*                                                                       C1R02750
R14SLBST EQU   R14RLBST+R14RLBCL   STARTING CYLINDER OF SRC DIR/LIB     C1R02760
R14SLBCL EQU   65                  RES SOURCE LIBRARY CYLS              C1R02770
R14SLDTK EQU   10                  RES SOURCE DIR TRACKS                C1R02780
R14SLBTB EQU   (R14SLBCL*G14TKCYL-R14SLDTK)*G14SLBBT  SLB TOTAL BLOCKS  C1R02790
R14SLDAT EQU   R14SLBST*G14TKCYL   SLD ABSOLUTE STARTING TRACK          C1R02800
R14SLBAT EQU   R14SLBST*G14TKCYL+R14SLDTK  SLB ABSOLUTE STARTING TRACK  C1R02810
*                                                                       C1R02820
R14LBLCL EQU   R14CILCL+R14RLBCL+R14SLBCL  CYL NR OF LABEL CYLINDER     C1R02830
*                                                                       C1R02840
         TITLE 'CONVERT 2311 SYSRES TO 2314    DISK READ/WRITE CURSOR'  C1R02850
*********************************************************************** C1R02860
*                                                                       C1R02870
*  DISK READ/WRITE CURSOR LAYOUT                                        C1R02880
*                                                                       C1R02890
*  INCLUDES CURRENT SEEK ADDRESS, ADDRESS OF DTFPH FOR I/O OPERATIONS,  C1R02900
*  TRACK GEOMETRY, AND EXTENT INFORMATION FOR DIRECTORY OR LIBRARY      C1R02910
*  CURRENTLY POINTED TO BY THE CURSOR.                                  C1R02920
*                                                                       C1R02930
*  USED TO PASS INFORMATION TO THE FOLLOWING ROUTINES                   C1R02940
*                                                                       C1R02950
*  XIO -      EXECUTE THE READ OR WRITE OPERATION AT THE BBCCHHR        C1R02960
*             USING THE DTFPH POINTED TO BY THIS BLOCK                  C1R02970
*  NXTCHR -   INCREMENT THE BBCCHHR TO THE NEXT RECORD, UPDATING TRACK  C1R02980
*             AND CYLINDER AS NEEDED, AND INDICATING END OF EXTENT IF   C1R02990
*             REACHED                                                   C1R03000
*  XCHR2RBA - CONVERT CHR TO A RELATIVE BLOCK ADDRESS                   C1R03010
*  XRBA2CHR - CONVERT RELATIVE BLOCK ADDRESS TO CHR.                    C1R03020
*                                                                       C1R03030
*********************************************************************** C1R03040
         SPACE 3                                                        C1R03050
CRSRDS   DSECT                                                          C1R03060
CRSRAD   DS    0D                  START OF CRSR CURSOR LAYOUT          C1R03070
CRSRBCHR DS    0CL7                CURRENT/NEXT SEEK/SEARCH ADDRESS     C1R03080
CRSRSK   DS    0CL6                CURRENT/NEXT SEEK ADDRESS            C1R03090
CRSRBB   DS    H                   ..BIN                                C1R03100
CRSRSRCH DS    0CL5                CURRENT/NEXT SEARCH ADDRESS          C1R03110
CRSRCC   DS    H                   ..CYLINDER                           C1R03120
CRSRHH   DS    H                   ..HEAD                               C1R03130
CRSRR    DS    X                   ..RECORD                             C1R03140
*                                                                       C1R03150
* REMAINING FIELDS SET BY CALLER PRIOR TO FIRST USE OF ROUTINE          C1R03160
*                                                                       C1R03170
CRSRFL   DS    X                   FLAG FOR WRITE CKD OR JUST R/W DATA  C1R03180
CRSRFW   EQU   X'00'               ..FORMATTING WRITE OP, SEEK PREV REC C1R03190
CRSRDT   EQU   X'01'               ..DATA ONLY OP, SEARCH FOR THIS REC  C1R03200
*                                  ..NOTE, FLAG SETS VALUE FOR FIRST    C1R03210
*                                    RECORD SEARCHED ON A TRACK AND FOR C1R03220
*                                    WHICH RECORD IS 'LAST' RECORD      C1R03230
*                                                                       C1R03240
CRSRDTF  DS    A                   ADDRESS OF DTFPH FOR I/O             C1R03250
CRSRTPC  DS    F                   NR. TRACKS PER CYLINDER (FW FOR DIV) C1R03260
         ORG   CRSRTPC             BACK TO TPC                          C1R03270
         DS    H                   FILLER - UPPER HW OF TPC             C1R03280
CRSRTPCH DS    H                   LOW HALFWORD OF TRACKS PER CYLINDER  C1R03290
CSRSSOE  DS    CL6                 START OF EXTENT BBCCHH               C1R03300
CRSREOE  DS    CL6                 END OF EXTENT BBCCHH                 C1R03310
*                                  ..SET THIS TO X'FFFF00000000'        C1R03320
*                                  ..TO BYPASS END OF EXTENT TESTS      C1R03330
CRSRSTK  DS    H                   RELATIVE STARTING TRACK OF EXTENT    C1R03340
CRSRRPT  DS    H                   MAX RECORDS PER TRACK                C1R03350
*                                                                       C1R03360
*                                                                       C1R03370
         TITLE 'CONVERT 2311 SYSRES TO 2314   DIRECTORY STATUS LAYOUT'  C1R03380
*********************************************************************** C1R03390
*                                                                       C1R03400
* DIRECTORY/LIBRARY STATUS RECORD LAYOUT FOR DOS/360 LIBRARIES.  ALL    C1R03410
* DIRECTORIES HAVE THE SAME STATUS RECORD LAYOUT BUT ARE STORED IN      C1R03420
* DIFFERENT LOCATIONS.                                                  C1R03430
*                                                                       C1R03440
* CORE IMAGE STATUS RECORD IS CYL 0 HEAD 1 REC 1 OF SYSRES FOR A        C1R03450
*   SYSTEM CORE IMAGE LIBRARY, AND CYL 0 HEAD 0 REC 1 OF A PRIVATE      C1R03460
*   CORE IMAGE LIBRARY DISK EXTENT.  THE CORE IMAGE DIRECTORY ALWAYS    C1R03470
*   STARTS ON TRACK 10 OF THE SYSRES OR PRIVATE CORE EXTENT.            C1R03480
* SYSRES CYL 0 HEAD 0 REC 2 AND REC 3 POINT TO THE FIRST TRACK OF THE   C1R03490
*   SYSTEM RELOCATABLE AND SOURCE SOURCE STATEMENT LIBRARY DIRECTORIES  C1R03500
*   RESPECTIVELY.                                                       C1R03510
* THE RELOCATABLE AND SOURCE STATEMENT LIBRARY STATUS RECORDS ARE IN    C1R03520
*   THE FIRST 80 BYTES OF THE FIRST DIRECTORY RECORD FOR THE RESPECTIVE C1R03530
*   LIBRARY.  EACH DIRECTORY CONTROL RECORD IN AN EMPTY LIBRARY SHOWS   C1R03540
*   ENTRY FIVE AS THE NEXT AVAILABLE ENTRY (RELATIVE TO ZERO) TO        C1R03550
*   SIMPLIFY THE CODE NEEDED TO SKIP OVER THE 80-BYTE (FIVE ENTRY)      C1R03560
*   DIRECTORY CONTROL RECORD                                            C1R03570
*                                                                       C1R03580
*********************************************************************** C1R03590
         SPACE 3                                                        C1R03600
*********************************************************************** C1R03610
*                                                                       C1R03620
*  GENERIC LIBRARY DIRECTORY STATUS RECORD, DSECT USED FOR EXTRACTING   C1R03630
*  INFORMATION FROM 2311 LIBRARY STATUS RECORDS                         C1R03640
*                                                                       C1R03650
*********************************************************************** C1R03660
LBSTAT   DSECT                                                          C1R03670
         DS    0D                  LIBRARY STATUS RECORD                C1R03680
LBSTATUS DS    0CL80                                                    C1R03690
*                                                                       C1R03700
*  DIRECTORY STATUS: START, NEXT, & LAST BBCCHHR(E)                     C1R03710
*                                                                       C1R03720
LBSDRST  DS    0CL7                DIR STARTING BBCCHHRE                C1R03730
LBSDRSTB DS    AL2                 ..DIR STARTING BIN                   C1R03740
LBSDRSTC DS    AL2                 ..DIR STARTING CYL                   C1R03750
LBSDRSTH DS    AL2                 ..DIR STARTING HEAD                  C1R03760
LBSDRSTR DS    AL1                 ..DIR STARTING RECORD                C1R03770
*                                                                       C1R03780
LBSDIR   DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R03790
LBSDRNX  DS    0CL8                DIR NEXT BBCCHHRE                    C1R03800
LBSDRNXB DS    AL2                 ..DIR NEXT AVAIL BIN                 C1R03810
LBSDRNXC DS    AL2                 ..DIR NEXT AVAIL CYL                 C1R03820
LBSDRNXH DS    AL2                 ..DIR NEXT AVAIL HEAD                C1R03830
LBSDRNXR DS    AL1                 ..DIR NEXT AVAIL RECORD              C1R03840
LBSDRNXE DS    AL1                 ..DIR NEXT AVAIL ENTRY               C1R03850
*                                                                       C1R03860
LBSDRLS  DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R03870
LBSDRLSB DS    AL2                 ..DIR ENDING BIN                     C1R03880
LBSDRLSC DS    AL2                 ..DIR ENDING CYL                     C1R03890
LBSDRLSH DS    AL2                 ..DIR ENDING HEAD                    C1R03900
LBSDRLSR DS    AL1                 ..DIR ENDING RECORD                  C1R03910
LBSDRLSE DS    AL1                 ..DIR ENDING ENTRY (REL 0)           C1R03920
*                                                                       C1R03930
*  LIBRARY STATUS: START, NEXT, & LAST BBCCHHR                          C1R03940
*                                                                       C1R03950
LBSLBST  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R03960
LBSLBSTB DS    AL2                 ..LIB STARTING BIN                   C1R03970
LBSLBSTC DS    AL2                 ..LIB STARTING CYL                   C1R03980
LBSLBSTH DS    AL2                 ..LIB STARTING HEAD                  C1R03990
LBSLBSTR DS    AL1                 ..LIB STARTING RECORD                C1R04000
*                                                                       C1R04010
LBSLIB   DS    0CL7                                                     C1R04020
LBSLBNX  DS    0CL7                HWORDS ARE ALIGNED IN THIS ONE       C1R04030
LBSLBNXB DS    AL2                 ..LIB NEXT AVAIL BIN                 C1R04040
LBSLBNXC DS    AL2                 ..LIB NEXT AVAIL CYL                 C1R04050
LBSLBNXH DS    AL2                 ..LIB NEXT AVAIL HEAD                C1R04060
LBSLBNXR DS    AL1                 ..LIB NEXT AVAIL RECORD              C1R04070
*                                                                       C1R04080
*                                  LIB ALWAYS ENDS ON CYL BOUNDARY      C1R04090
LBSLBLS  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R04100
LBSLBLSB DS    AL2                 ..LIB LAST AVAIL BIN                 C1R04110
LBSLBLSC DS    AL2                 ..LIB LAST AVAIL CYL                 C1R04120
LBSLBLSH DS    AL2                 ..LIB LAST AVAIL HEAD                C1R04130
LBSLBLSR DS    AL1                 ..LIB LAST AVAIL RECORD              C1R04140
*                                                                       C1R04150
*  LIBRARY BLOCK COUNTS                                                 C1R04160
*                                                                       C1R04170
LBSDACT  DS    A                  COUNT OF ACTIVE DIRECTORY ENTRIES     C1R04180
LBSLLEN  DS    A                  TOTAL BLKS IN LIBRARY                 C1R04190
LBSLACT  DS    A                  COUNT OF ACTIVE BLOCKS IN LIBRARY     C1R04200
LBSLDEL  DS    A                  COUNT OF LIBRARY DELETED BLOCKS       C1R04210
LBSLLEFT DS    A                  BLOCKS AVAILABLE AFTER LAST USED      C1R04220
         DS    Y                  AUTO CONDENSE LIMIT, 0=NO AUTO        C1R04230
*                                                                       C1R04240
LBSLCYLS DS    Y                  CYLINDERS FOR DIR AND LIB             C1R04250
*                                                                       C1R04260
LBSDTRKS DS    Y                  DIRECTORY TRACKS                      C1R04270
         DS    XL9'0'             RESERVED                              C1R04280
LBSLBLCL DS    AL1                CIL ONLY: ADDRESS OF LABEL CYL        C1R04290
*                                 SET TO 0 FOR ALL OTHER LIBS           C1R04300
*                                                                       C1R04310
         TITLE 'CONVERT 2311 SYSRES TO 2314   DIR ENTRY LAYOUTS'        C1R04320
*********************************************************************** C1R04330
*                                                                       C1R04340
*  SEPARATE LAYOUTS ARE USED FOR 2311 AND 2314 DIRECTORY ENTRIES TO     C1R04350
*  MAKE CODING CLEAR.  2311 AND 2314 DIRECTORY ENTRY LAYOUTS ARE THE    C1R04360
*  SAME FOR A GIVEN LIBRARY TYPE.                                       C1R04370
*                                                                       C1R04380
*********************************************************************** C1R04390
*                                                                       C1R04400
* CORE IMAGE DIRECTORY ENTRY LAYOUT FOR 2311 OPERATIONS                 C1R04410
*                                                                       C1R04420
DIR1ENTY DSECT                                                          C1R04430
DIR1CELL DS    0CL20                                                    C1R04440
DIR1NAME DS    CL8     PHASE NAME                                       C1R04450
DIR1LADR DS    AL3     PHASE LOAD ADDRESS (0=SELF-RELOCATING)           C1R04460
DIR1NBLK DS    X       NUMBER OF DISK RECORDS USED                      C1R04470
DIR1EADR DS    AL3     PHASE ENTRY POINT ADDRESS                        C1R04480
DIR1DADR DS    XL3     CHR OF PHASE IN CL                               C1R04490
DIR1LELS DS    H       LENGTH USED IN LAST RECORD                       C1R04500
*                                                                       C1R04510
* RELOCATABLE MODULE DIRECTORY ENTRY LAYOUT FOR 2311 OPERATIONS         C1R04520
*                                                                       C1R04530
DIR1RENT DSECT                                                          C1R04540
DIR1RCEL DS    0CL16                                                    C1R04550
DIR1RNAM DS    CL8     RELOCATABLE MODULE NAME                          C1R04560
DIR1RNBK DS    H       NUMBER OF DISK RECORDS USED                      C1R04570
DIR1RADR DS    0XL4    CHHR OF RELO MODULE IN RL                        C1R04580
DIR1RC   DS    X       ..CYLINDER ADDRESS IN LIB                        C1R04590
DIR1RHH  DS    XL2     ..HEAD ADDRESS IN LIB                            C1R04600
DIR1RR   DS    X       ..STARTING REC ADDRESS IN LIB                    C1R04610
DIR1RVER DS    X       VERSION LEVEL OF MODULE                          C1R04620
DIR1RMOD DS    X       MOD LEVEL OF MODULE                              C1R04630
*                                                                       C1R04640
* SOURCE STATEMENT DIRECTORY ENTRY LAYOUT FOR 2311 OPS                  C1R04650
*                                                                       C1R04660
DIR1SENT DSECT                                                          C1R04670
DIR1SCEL DS    0CL16                                                    C1R04680
DIR1SNAM DS    0CL9    SOURCE BOOK SUBLIBRARY AND NAME                  C1R04690
DIR1SSUB DS    C       ..SUBLIBRARY                                     C1R04700
DIR1SBKN DS    CL8     ..SOURCE BOOK NAME                               C1R04710
DIR1SADR DS    0XL3    CHHR OF RELO MODULE IN RL                        C1R04720
DIR1SC   DS    X       ..CYLINDER ADDRESS IN LIB                        C1R04730
DIR1SH   DS    X       ..HEAD ADDRESS IN LIB                            C1R04740
DIR1SR   DS    X       ..STARTING REC ADDRESS IN LIB                    C1R04750
DIR1SNBK DS    H       NUMBER OF DISK RECORDS USED                      C1R04760
DIR1SVER DS    X       VERSION LEVEL OF MODULE                          C1R04770
DIR1SMOD DS    X       MOD LEVEL OF MODULE                              C1R04780
*                                                                       C1R04790
* CORE IMAGE DIRECTORY ENTRY LAYOUT FOR 2314 OPERATIONS                 C1R04800
*                                                                       C1R04810
DIR4ENTY DSECT                                                          C1R04820
DIR4CELL DS    0CL20                                                    C1R04830
DIR4NAME DS    CL8     PHASE NAME                                       C1R04840
DIR4LADR DS    AL3     PHASE LOAD ADDRESS (0=SELF-RELOCATING)           C1R04850
DIR4NBLK DS    X       NUMBER OF DISK RECORDS USED                      C1R04860
DIR4EADR DS    AL3     PHASE ENTRY POINT ADDRESS                        C1R04870
DIR4DADR DS    XL3     CHR CHR OF PHASE IN CL                           C1R04880
DIR4LELS DS    H       LENGTH USED IN LAST RECORD                       C1R04890
*                                                                       C1R04900
* RELOCATABLE MODULE DIRECTORY ENTRY LAYOUT FOR 2314 OPERATIONS         C1R04910
*                                                                       C1R04920
DIR4RENT DSECT                                                          C1R04930
DIR4RCEL DS    0CL16                                                    C1R04940
DIR4RNAM DS    CL8     RELOCATABLE MODULE NAME                          C1R04950
DIR4RNBK DS    H       NUMBER OF DISK RECORDS USED                      C1R04960
DIR4RADR DS    0XL4    CHHR OF RELO MODULE IN RL                        C1R04970
DIR4RC   DS    X       ..CYLINDER ADDRESS IN LIB                        C1R04980
DIR4RHH  DS    XL2     ..HEAD ADDRESS IN LIB                            C1R04990
DIR4RR   DS    X       ..STARTING REC ADDRESS IN LIB                    C1R05000
DIR4RVER DS    X       VERSION LEVEL OF MODULE                          C1R05010
DIR4RMOD DS    X       MOD LEVEL OF MODULE                              C1R05020
*                                                                       C1R05030
* SOURCE STATEMENT DIRECTORY ENTRY LAYOUT FOR 2311 OPS                  C1R05040
*                                                                       C1R05050
DIR4SENT DSECT                                                          C1R05060
DIR4SCEL DS    0CL16                                                    C1R05070
DIR4SNAM DS    0CL9    SOURCE BOOK SUBLIBRARY AND NAME                  C1R05080
DIR4SSUB DS    C       ..SUBLIBRARY                                     C1R05090
DIR4SBKN DS    CL8     ..SOURCE BOOK NAME                               C1R05100
DIR4SADR DS    0XL3    CHHR OF RELO MODULE IN RL                        C1R05110
DIR4SC   DS    X       ..CYLINDER ADDRESS IN LIB                        C1R05120
DIR4SH   DS    X       ..HEAD ADDRESS IN LIB                            C1R05130
DIR4SR   DS    X       ..STARTING REC ADDRESS IN LIB                    C1R05140
DIR4SNBK DS    H       NUMBER OF DISK RECORDS USED                      C1R05150
DIR4SVER DS    X       VERSION LEVEL OF MODULE                          C1R05160
DIR4SMOD DS    X       MOD LEVEL OF MODULE                              C1R05170
*                                                                       C1R05180
         TITLE 'CONVERT 2311 SYSRES TO 2314   FORMAT CONTROL RECORDS'   C1R05190
*********************************************************************** C1R05200
*                                                                       C1R05210
*  PREAMBLE.  ESTABLISH ADDRESSABILITY, OPEN FILES, READ THE LIBRARY    C1R05220
*  POINTERS AND CONTROL RECORDS FROM THE TWO 2311 SYSRES FILES THAT     C1R05230
*  WILL BE COPIED TO THE NEW 2314 SYSRES, AND WRITE INITIAL VERSIONS    C1R05240
*  OF THOSE RECORDS TO THE 2314 SYSRES.                                 C1R05250
*                                                                       C1R05260
*  NOTE THAT $$A$IPL2, THE INITIAL IPL PROGRAM, IS ALSO READ AND        C1R05270
*  COPIED AT THIS TIME.                                                 C1R05280
*                                                                       C1R05290
*********************************************************************** C1R05300
         SPACE 1                                                        C1R05310
CVT11RES CSECT ,                                                        C1R05320
START    BALR  R11,0               ESTABLISH ADDRESSABILITY             C1R05330
         BCTR  R11,0               BACK TO THE START ADDRESS            C1R05340
         BCTR  R11,0                                                    C1R05350
         USING START,R11           MAKE FIRST 4K ADDRESSABLE            C1R05360
*                                                                       C1R05370
         LA    R12,4095(,R11)      SET BASE REGISTER FOR 2ND 4K         C1R05380
         LA    R12,1(,R12)                                              C1R05390
         USING START+4096,R12      MAKE SECOND 4K ADDRESSABLE           C1R05400
*                                                                       C1R05410
* OPEN FILES.  NOT STRICTLY NEEDED EXCEPT                               C1R05420
*   1) TO GET A FORMAT-1 LABEL WRITTEN TO THE VTOC OF THE NEW SYSRES.   C1R05430
*   2) TO ENABLE DISK ACCESS WHEN THE GENERATED SUPERVISOR SUPPORTS     C1R05440
*      DASD FILE PROTECTION (SUPERVISOR GENERATION MACRO FOPT,          C1R05450
*      DASDFP= OPERAND).  THE DISTRIBUTION SUPERVISOR DOES NOT INCLUDE  C1R05460
*      DASD FILE PROTECTION.                                            C1R05470
*                                                                       C1R05480
         OPEN  RES2311,SLB2311,RES2314                                  C1R05490
*                                                                       C1R05500
*  CURSORS ASSEMBLED TO READ FROM RES2311 AND WRITE TO RES2314.  THE    C1R05510
*  2311 CURSORS ARE POINTED TO SLB2311 WHEN THE SOURCE STATEMENT LIB    C1R05520
*  IS PROCESSED                                                         C1R05530
*                                                                       C1R05540
* READ C0H1 RECORDS 1-5 COUNT, KEY, AND DATA.  WE'LL USE THE COUNT      C1R05550
* FIELDS AS IS WHEN WRITING UPDATED SYSRES CONTROL AND IPL RECORDS      C1R05560
* TO THE NEW 2314 SYSRES.                                               C1R05570
*                                                                       C1R05580
         MVC   D1BCHR,C1BCHRH1     SET SEEK/SEARCH TO C0H1R0 TO         C1R05590
*                                  ..READ CKD RECORDS 1-5               C1R05600
         LA    R1,D1BCHR           POINT R1 AT 2311 READ CURSOR         C1R05610
         LA    R15,CCW1HD1         POINT TO CCW STRING FOR C0H1R1-5     C1R05620
         BAL   R2,XIO              READ 2311 C0H1 RECORDS 1-5           C1R05630
         MVC   CID1SCHR,C0H1R1+8   SAVE LOC OF 2311 CORE IMAGE DIR      C1R05640
         MVC   RLD1SCHR,C0H1R2+8   SAVE LOC OF 2311 RELO DIR            C1R05650
*                                                                       C1R05660
* UPDATE THE SYSRES CONTROL RECORDS TO REFLECT THE 2314 SYSRES          C1R05670
* LIBRARY ALLOCATIONS                                                   C1R05680
*                                                                       C1R05690
         MVC   C0H1R2+8(L'RLSDRST),RLSDRST  MOVE 2314 RL START          C1R05700
         MVC   C0H1R3+8(L'SLSDRST),SLSDRST  MOVE 2314 SL START          C1R05710
*                                                                       C1R05720
         MVC   D4BCHR,C1BCHRH1     SET SEEK/SEARCH TO C0H1R1            C1R05730
         LA    R1,D4BCHR           POINT R1 AT 2314 READ CURSOR         C1R05740
         LA    R15,CCW4HD1         POINT TO CCW STRING FOR C0H1R1-5     C1R05750
         BAL   R2,XIO              WRITE 2314 C0H1 RECORDS 1-5          C1R05760
         SPACE 3                                                        C1R05770
*                                                                       C1R05780
* Cyl 0 tracks 2,3,4 are work tracks; nothing needs to be written       C1R05790
*                                                                       C1R05800
         TITLE 'CONVERT 2311 SYSRES TO 2314   FORMAT CYL 0 HEADS 1-9'   C1R05810
*********************************************************************** C1R05820
*                                                                       C1R05830
* Cyl 0 tracks 5-9 will be updated automatically by MAINT or CORGZ      C1R05840
* (and LNKEDT but we're not cataloging anything with LNKEDT)            C1R05850
* specifically $MAINEOJ but these 5 tracks need to be formatted with    C1R05860
* a directory record filled with blanks.  Only one record is needed.    C1R05870
*                                                                       C1R05880
*   h5  transient dir  ($$ transients except $$BO transients            C1R05890
*   h6  open dir       ($$BO transients)                                C1R05900
*   h7  $ phase dir    (phases starting with '$', e.g., $JOBACCT)       C1R05910
*   h8  fgp phase dir  (phases starting with 'FGP' for Foreground pgm)  C1R05920
*   h9  current phase  (phases created by option link for load and go,  C1R05930
*                      or cataloged phase currently being executed)     C1R05940
*                                                                       C1R05950
* Tracks 5-9 are rebuilt if/as needed by $MAINEOJ                       C1R05960
*                                                                       C1R05970
* NOTE: THE BLANK RECORDS MUST REALLY BE EBCDIC BLANKS.  BINARY ZEROS,  C1R05980
* AN END OF DIRECTORY MARKER ('*') OR OTHER VALUES WILL CONFUSE FETCH   C1R05990
* IN THE SUPERVISOR AND THE SYSRES WILL NOT IPL.                        C1R06000
*                                                                       C1R06010
* NOTE THAT TO READ OR WRITE COUNT, KEY, AND DATA (CKD), THE SEARCH     C1R06020
* MUST BE FOR THE RECORD PRIOR TO THE ONE TO BE WRITTEN.  TO WRITE      C1R06030
* C0H5R1, ONE MUST SEEK C0H5R0.  FOR READ OR WRITE DATA ONLY, THE       C1R06040
* SEARCH IS FOR THE RECORD TO BE READ OR WRITTEN (REALLY A RE-WRITE).   C1R06050
*                                                                       C1R06060
* AND THIS IS WHY OPERATIONS ON RECORD ZERO HAVE THEIR OWN CCW          C1R06070
* COMMANDS.                                                             C1R06080
*                                                                       C1R06090
* REGISTER USAGE:                                                       C1R06100
*    R5 = NR TRACKS LEFT TO WRITE (LOOP CONTROL)                        C1R06110
*    R6 = CURRENT HEAD NR.                                              C1R06120
*                                                                       C1R06130
*********************************************************************** C1R06140
         SPACE 1                                                        C1R06150
*                                                                       C1R06160
* WE COULD USE INITAREA, BUT SETTING UP THE CALLING SEQUENCE WOULD      C1R06170
* NEARLY AS MUCH SPACE AS JUST DOING IT.                                C1R06180
*                                                                       C1R06190
         MVI   DIR4,C' '           MOVE A BLANK                         C1R06200
         MVC   DIR4+1(256),DIR4    PROPAGATE THROUGH NEXT 256 BYTES     C1R06210
         MVC   DIR4+257(L'DIR4-257),DIR4  BLANK OUT ENTIRE DIR REC      C1R06220
*                                                                       C1R06230
*   SET UP CCW AND SEEK/SEARCH FIELDS TO WRITE AN EMPTY DIR RECORD      C1R06240
*                                                                       C1R06250
         XC    D4DBCHR,D4DBCHR     ZERO OUT BBCCHHR IN SEEK/SEARCH      C1R06260
*                                  SEARCH WILL BE FOR R0 TO WRITE R1    C1R06270
         LA    R5,5                WRITE 5 RECORDS, TRACKS 5-6-7-8-9    C1R06280
         LA    R6,5                START ON HEAD FIVE                   C1R06290
*                                                                       C1R06300
*  LOOP TO WRITE ONE BLANK RECORD ONE PER TRACK ON TRACKS 5-9           C1R06310
*                                                                       C1R06320
CYL0FMT  STH   R6,D4DHEAD          UPDATE HEAD NUMBER IN CURSOR         C1R06330
         LA    R15,CCW4CDFW        POINT AT FORMATTING WRITE CCW        C1R06340
         LA    R1,D4DBCHR          POINT AT DIRECTORY WRITE CURSOR      C1R06350
         BAL   R2,XIOFW            WRITE RECORD ONE ON TRACK            C1R06360
         LA    R6,1(,R6)           INCREMENT HEAD NR.                   C1R06370
         BCT   R5,CYL0FMT          STOP AFTER FIVE RECORDS              C1R06380
*                                                                       C1R06390
* FIVE BLANK RECORDS WRITTEN ON CYL ZERO TRACKS 5-9                     C1R06400
*                                                                       C1R06410
         TITLE 'CONVERT 2311 SYSRES TO 2314   COPY CORE IMAGE LIBRARY'  C1R06420
*********************************************************************** C1R06430
*                                                                       C1R06440
*   READ 2311 CORE IMAGE DIRECTORY (CID) AND SET UP TO CREATE 2314      C1R06450
*      DIRECTORY.                                                       C1R06460
*                                                                       C1R06470
*   CID STARTS AT C1H0R1 ON 2311 AND AT C0H10R1 ON 2314.                C1R06480
*                                                                       C1R06490
*   REGISTER USAGE:                                                     C1R06500
*      R3 - 2311 OR 2314 BLOCK COUNT OF PHASE BEING PROCESSED           C1R06510
*      R4 - POINTER TO CURRENT BLOCK BEING READ OR WRITTEN IN IO        C1R06520
*      R6 - COUNT OF 2314 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R06530
*      R7 - POINTER TO CURRENT 2314 DIRECTORY RECORD                    C1R06540
*      R8 - COUNT OF 2311 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R06550
*      R9 - POINTER TO CURRENT 2311 DIRECTORY RECORD                    C1R06560
*      R10 - POINTER TO LIBRARY CURSOR FOR PHASE READ LOOP              C1R06570
*                                                                       C1R06580
*********************************************************************** C1R06590
         SPACE 1                                                        C1R06600
*                                                                       C1R06610
*  SET UP CURSORS FOR WRITING THE 2314 CORE IMAGE DIRECTORY AND         C1R06620
*  LIBRARY.  THE DTF ADDRESS IS STILL SET FROM ABOVE LIBRARY STATUS     C1R06630
*  RECORD READS.  WE DO NOT NEED THE DIRECTORY OR LIBRARY ABSOLUTE      C1R06640
*  STARTING TRACK.                                                      C1R06650
*                                                                       C1R06660
         MVC   D4DBCHR,CISDRST     SET 2314 CI DIR STARTING LOCATION    C1R06670
         MVI   D4DREC,0            SEARCH R0 TO WRITE R1                C1R06680
         MVI   D4DFLAG,CRSRFW      INDICATE WRITE CKD                   C1R06690
         MVC   D4DSOE,CISDRST      SET DIRECTORY START OF EXTENT        C1R06700
         MVC   D4DEOE,CISDRLS      SET DIRECTORY END OF EXTENT          C1R06710
         MVC   D4DRPT,=AL2(G14CIDBT)  SET DIR BLOCKS/TRACK              C1R06720
*                                                                       C1R06730
         MVC   D4BCHR,CISLBST      SET 2314 CI LIB STARTING LOCATION    C1R06740
         MVI   D4REC,0             SEARCH R0 TO WRITE R1                C1R06750
         MVI   D4FLAG,CRSRFW       INDICATE FORMATTING WRITE            C1R06760
         MVC   D4SOE,CISLBST       MOVE START OF CIL EXTENT             C1R06770
         MVC   D4EOE,CISLBLS       MOVE END OF CIL EXTENT FOR NXTCHR    C1R06780
         MVC   D4RPT,=AL2(G14CILBT)  MOVE BLOCKS/TRACK FOR NXTCHR       C1R06790
*                                                                       C1R06800
*  SET UP CURSORS TO READ THE 2311 CORE IMAGE DIRECTORY AND LIBRARY.    C1R06810
*  D1DEOE ALREADY INITIALIZED TO SUPPRESS END OF EXTENT CHECKING.  AND  C1R06820
*  ABSOLUTE STARTING TRACK IS NOT NEEDED HERE EITHER.  LIBRARY SEEK     C1R06830
*  ADDRESS IS RESET AT THE START OF EACH PHASE BEING COPIED.            C1R06840
*                                                                       C1R06850
         MVC   D1DBCHR,CID1SCHR    SET 2311 CI DIR START                C1R06860
         MVI   D1DFLAG,CRSRDT      INDICATE READ/WRITE DATA ONLY        C1R06870
         MVC   D1DRPT,=AL2(G11CIDBT)  SET LIBRARY BLOCKS/TRACK          C1R06880
*                                                                       C1R06890
         MVI   D1FLAG,CRSRDT       INDICATE READ/WRITE DATA ONLY        C1R06900
         MVC   D1RPT,=AL2(G11CILBT)  SET LIBRARY BLOCKS/TRACK           C1R06910
*                                                                       C1R06920
*  SET UP POINTERS TO CURRENT 2314 DIRECTORY OUTPUT RECORD              C1R06930
*                                                                       C1R06940
         LA    R6,G14CIDEB         ENTRIES PER RECORD 2314 OUTPUT       C1R06950
         LA    R7,DIR4             POINT TO 2314 DIRECTORY AREA         C1R06960
         USING DIR4ENTY,R7         MAKE IT ADDRESSABLE                  C1R06970
*                                                                       C1R06980
*  READ NEXT (OR FIRST) CORE IMAGE DIRECTORY RECORD                     C1R06990
*                                                                       C1R07000
CILNEXTR DS    0H                  PROCESS NEXT CIL DIR RECORD          C1R07010
         LA    R15,CCW1CDRD        READ CID DIRECTORY RECORD            C1R07020
         LA    R1,D1DBCHR          POINT TO SEEK/SEARCH FOR CIL DIR     C1R07030
         BAL   R2,XIO              READ 2311 DIR. RECORD                C1R07040
         LA    R8,G11CIDEB         ENTRIES PER RECORD 2311 INPUT        C1R07050
         LA    R9,DIR1             POINT TO 2311 DIRECTORY AREA         C1R07060
         USING DIR1ENTY,R9         MAKE IT ADDRESSABLE                  C1R07070
*                                                                       C1R07080
* FOR EACH ACTIVE DIRECTORY ENTRY, READ IN THE ENTIRE PHASE AND         C1R07090
* RE-BLOCK IT FROM 1728-BYTE RECORDS TO 1688-BYTE RECORDS.              C1R07100
*                                                                       C1R07110
CILNEXTE DS    0H                  PROCESS NEXT CIL DIR ENTRY           C1R07120
         CLI   DIR1NAME,ENDDIR     IS IT END OF DIRECTORY               C1R07130
         BE    CILCDONE            ..YES, WE'RE DONE WITH COPY          C1R07140
         CLI   DIR1NAME,EMPTYDIR   IS THIS ENTRY EMPTY                  C1R07150
         BE    CILEDONE            ..YES, WE'RE DONE WITH ENTRY         C1R07160
*                                                                       C1R07170
*   SET UP LIBRARY READ CURSOR BBCCHHR FOR THE 2311 PHASE               C1R07180
*                                                                       C1R07190
         MVC   D1CYL+1(1),DIR1DADR SETUP PHASE READ CC                  C1R07200
         MVC   D1HEAD+1(1),DIR1DADR+1  ..HH                             C1R07210
         MVC   D1REC,DIR1DADR+2    ..AND R                              C1R07220
*                                                                       C1R07230
         LA    R4,IOPH             POINT R4 AT I/O AREA                 C1R07240
         XR    R3,R3               CLEAR R3                             C1R07250
         IC    R3,DIR1NBLK         GET PHASE BLOCK COUNT                C1R07260
         LA    R10,D1BCHR          POINT R10 AT PHASE READ CURSOR       C1R07270
*                                                                       C1R07280
*  READ ENTIRE PHASE INTO STORAGE STARTING AT IO.                       C1R07290
*                                                                       C1R07300
CILRPHLP DS    0H                  TOP OF READ PHASE LOOP               C1R07310
         ST    R4,CCW1CLRD         SETUP CCW DATA ADDRESS               C1R07320
         MVI   CCW1CLRD,READDATA   RESTORE CCW COMMAND CODE             C1R07330
*                                  (I THINK ICM/STCM WERE ADDED         C1R07340
*                                  TO S/370 JUST TO AID CCW             C1R07350
*                                  MANIPULATION.)                       C1R07360
*                                                                       C1R07370
         LA    R15,CCW1CLRD        POINT AT CCW STRING TO EXECUTE       C1R07380
         LR    R1,R10              POINT AT BBCCDDR FOR SEEK/SEARCH     C1R07390
         BAL   R2,XIO              READ 2311 LIB. REC.                  C1R07400
         LR    R1,R10              POINT AT LIBRARY CURSOR              C1R07410
         BAL   R2,NXTCHR           INCREMENT TO NEXT LIB RECORD         C1R07420
         LA    R4,G11CILBS(,R4)    POINT PAST BLOCK JUST READ           C1R07430
         BCT   R3,CILRPHLP         LOOP IF MORE RECORDS TO READ         C1R07440
*                                                                       C1R07450
* PHASE COMPLETELY READ IN FROM 2311 TO LOCATION STARTING AT IOPH.      C1R07460
*   R7 POINTS TO NEXT AVAILABLE 2314 DIRECTORY SLOT                     C1R07470
*   R9 POINTS TO PHASE ENTRY IN 2311 DIRECTORY                          C1R07480
*                                                                       C1R07490
* SETUP NEW 2314 DIRECTORY ENTRY                                        C1R07500
*                                                                       C1R07510
         MVC   DIR4CELL,DIR1CELL   COPY 2311 DIR ENTRY TO 2314 DIR      C1R07520
*                                                                       C1R07530
         XR    R3,R3               CLEAR R3 FOR IC ARITHMETIC           C1R07540
         IC    R3,DIR1NBLK         GET NUMBER OF LIBRARY BLOCKS         C1R07550
         BCTR  R3,0                LESS LAST (PARTIAL) BLOCK            C1R07560
         MH    R3,=AL2(G11CILBS)   CALC BYTES OF ALL BUT LAST BLOCK     C1R07570
         AH    R3,DIR1LELS         ADD SIZE OF LAST BLOCK               C1R07580
*                                  R3 NOW HAS PHASE LENGTH IN BYTES     C1R07590
         SR    R2,R2               CLEAR R2 FOR DOUBLEWORD DIVIDE       C1R07600
         D     R2,=A(G14CILBS)     DIVIDE BY 2314 LIB BLOCK SIZE        C1R07610
         LTR   R2,R2               WHOLE NUMBER OF 2314 BLOCKS          C1R07620
         BNZ   NOTEVEN             ..NO, SKIP ADJUSTMENTS               C1R07630
         LA    R2,G14CILBS         SET LENGTH OF LAST BLOCK             C1R07640
         BCTR  R3,0                DON'T COUNT LAST FULL BLOCK          C1R07650
NOTEVEN  DS    0H                                                       C1R07660
         LA    R3,1(,R3)            INCLUDE LAST BLOCK                  C1R07670
*                                                                       C1R07680
         STC   R3,DIR4NBLK         STORE BLOCK COUNT IN 2314 ENTRY      C1R07690
         STH   R2,DIR4LELS         STORE LEN LAST BLOCK IN 2314 ENT     C1R07700
         MVC   DIR4DADR(1),D4CYL+1 STORE 2314 NEXT LIB CYL IN ENTRY     C1R07710
         MVC   DIR4DADR+1(1),D4HEAD+1  STORE 2314 NEXT HEAD             C1R07720
         IC    R15,D4REC           GET SEEK RECORD FOR WRITE CKD        C1R07730
         LA    R15,1(,R15)         INCREMENT TO RECORD TO BE WRITTEN    C1R07740
         STC   R15,DIR4DADR+2      STORE 2314 NEXT RECORD               C1R07750
         SPACE 3                                                        C1R07760
*********************************************************************** C1R07770
*                                                                       C1R07780
*   WRITE PHASE TO 2314 LIBRARY.  ENTIRE PHASE HAS BEEN LOADED INTO     C1R07790
*      MEMORY STARTING AT IO.  THESE WRITE OPERATIONS ARE DATA-ONLY     C1R07800
*      AND DO NOT NEED A COUNT FIELD BECAUSE THE CIL WAS FORMATTED      C1R07810
*      EARLIER IN THIS PROGRAM.                                         C1R07820
*                                                                       C1R07830
*   AT COMPLETION,                                                      C1R07840
*      D4BCHR POINTS TO NEXT FREE CIL BLOCK                             C1R07850
*      CISLACT UPDATED WITH BLOCKS WRITTEN FOR THIS PHASE               C1R07860
*                                                                       C1R07870
*   REGISTER USAGE:                                                     C1R07880
*      R2 - WORK REGISTER, ALSO RETURN FROM IO ROUTINE                  C1R07890
*      R3 - COUNT OF LIBRARY BLOCKS LEFT TO WRITE, INCL. ANY PARTIAL    C1R07900
*      R4 - POINTER TO CURRENT 2314 LIBRARY BLOCK                       C1R07910
*      R5 - POINTER TO LIBRARY WRITE CURSOR                             C1R07920
*      R6-R9 - RESERVED, USED BY OUTER LOOP CODING                      C1R07930
*                                                                       C1R07940
*********************************************************************** C1R07950
         LA    R4,IOPHFW           POINT R4 AT COUNT AREA FOR PHASE     C1R07960
         LA    R5,D4BCHR           POINT R5 AT WRITE CURSOR             C1R07970
         MVC   CCW4CLFW,CCW4CLSV   RESTORE CLB WRITE CKD CCW            C1R07980
*                                                                       C1R07990
CILWPHLP LTR   R3,R3               LAST BLOCK OF PHASE WRITTEN          C1R08000
         BNP   CIL4PD              ..YES, UPDATE CIL DIRECTORY          C1R08010
         ST    R4,CCW4CLFW         SETUP CCW I/O ADDRESS                C1R08020
         MVI   CCW4CLFW,WRITECKD   RESTORE WRITE CKD COMMAND CODE       C1R08030
         BCT   R3,NOTSHORT         BR IF NOT LAST BLOCK OF PHASE        C1R08040
*                                                                       C1R08050
*  LAST BLOCK OF PHASE.  ZERO THE BALANCE OF THE LAST BLOCK             C1R08060
*                                                                       C1R08070
         LH    R15,DIR4LELS        GET LENGTH OF LAST BLOCK             C1R08080
         LA    R0,G14CILBS         GET BLOCK SIZE OF FULL BLOCK         C1R08090
         SR    R0,R15              CALC LENGTH TO BE CLEARED            C1R08100
         LA    R1,8(R4,R15)        POINT PAST COUNT AND USED AREA       C1R08110
         MVI   0(R1),0             ZERO FIRST BYTE                      C1R08120
         BAL   R2,INITAREA         CLEAR REST OF BLOCK                  C1R08130
*                                                                       C1R08140
NOTSHORT DS    0H                  NO PARTIAL BLOCK AT PHASE END        C1R08150
         LR    R1,R5               POINT AT LIBRARY CURSOR              C1R08160
         LA    R15,CCW4CLFW        POINT R1 AT CCW FOR WRITE CKD        C1R08170
         BAL   R2,XIOFW            WRITE LIBRARY RECORD                 C1R08180
         L     R2,CISLACT          GET CIL STATUS BLOCKS ACTIVE         C1R08190
         LA    R2,1(,R2)           INCREMENT FOR BLOCK JUST USED        C1R08200
         ST    R2,CISLACT          UPDATE CIL STATUS BLOCKS ACTIVE      C1R08210
         LA    R4,G14CILBS(,R4)    POINT TO NEXT BLOCK TO WRITE         C1R08220
         LR    R1,R5               POINT R1 AT LIBRARY CURSOR           C1R08230
         BAL   R2,NXTCHR           UPDATE DISK ADDR FOR NEXT BLOCK      C1R08240
         B     CILWPHLP            CONTINUE WRITING PHASE BLOCKS        C1R08250
*                                                                       C1R08260
* PHASE WRITTEN TO CIL.  UPDATE 2314 DIRECTORY RECORD AND 2314 CID      C1R08270
* STATUS RECORD NEXT BLOCK AVAILABLE.                                   C1R08280
*                                                                       C1R08290
CIL4PD   DS    0H                  2314 PHASE DONE BEING WRITTEN        C1R08300
         L     R15,CISDACT         GET CURRENT COUNT OF ENTRIES ACTIVE  C1R08310
         LA    R15,1(,R15)         INCLUDE PHASE JUST COPIED            C1R08320
         ST    R15,CISDACT                                              C1R08330
*                                                                       C1R08340
         LA    R7,L'DIR4CELL(,R7)  POINT TO NEXT 2314 DIR. ENTRY        C1R08350
         BCT   R6,CILEDONE         BR IF ROOM FOR MORE 2314 PHASES      C1R08360
*                                                                       C1R08370
* 2314 DIRECTORY RECORD FULL.  WRITE IT OUT AND SET UP THE NEXT RECORD  C1R08380
*                                                                       C1R08390
         LA    R1,D4DBCHR          POINT TO DIRECTORY CURSOR            C1R08400
         LA    R15,CCW4CDFW        POINT TO WRITE DIR DATA CCW          C1R08410
         BAL   R2,XIOFW            WRITE OUT DIRECTORY RECORD           C1R08420
         LA    R1,D4DBCHR          POINT TO DIRECTORY CURSOR            C1R08430
         BAL   R2,NXTCHR           UPDATE TO NEXT DIRECTORY RECORD      C1R08440
*                                                                       C1R08450
         LA    R7,DIR4             POINT TO START OF DIRECTORY RECORD   C1R08460
         LA    R6,G14CIDEB         NUMBER OF ENTRIES AVAILABLE IN REC   C1R08470
         MVI   DIR4,0              CLEAR NEXT DIRECTORY RECORD          C1R08480
         MVC   DIR4+1(256),DIR4                   ..                    C1R08490
         MVC   DIR4+257(G14CIDBS-256-1),DIR4+256  ..                    C1R08500
         MVI   DIR4CELL,ENDDIR     MARK END OF DIRECTORY                C1R08510
*                                                                       C1R08520
CILEDONE DS    0H                  PROCESS NEXT CID ENTRY               C1R08530
         LA    R9,L'DIR1CELL(,R9)   POINT TO NEXT 2311 ENTRY            C1R08540
         BCT   R8,CILNEXTE         CONTINUE IF ENTRIES LEFT             C1R08550
*                                                                       C1R08560
         LA    R1,D1DBCHR          POINT TO NXTCHR PARM LIST            C1R08570
         BAL   R2,NXTCHR           GET NEXT DIR SEEK/SEARCH ADDR        C1R08580
         LTR   R15,R15             END OF EXTENT                        C1R08590
         BZ    CILNEXTR            ..NO, MORE DIR RECS TO DO            C1R08600
*                                                                       C1R08610
*  LAST 2311 DIRECTORY ENTRY PROCESSED.  UPDATE 2314 CID STATUS RECORD, C1R08620
*  AND THEN WRITE THE LAST DIRECTORY RECORD.                            C1R08630
*                                                                       C1R08640
CILCDONE DS    0H                  END OF 2311 DIRECTORY REACHED        C1R08650
         L     R15,CISDACT         GET ACTIVE ENTRY COUNT (=PHASES)     C1R08660
         CVD   R15,WORKCVD         CONVERT TO PACKED FOR STATS MSG      C1R08670
         ZAP   OMPHASES,WORKCVD    SAVE FOR SUMMARY STATS MESSAGES      C1R08680
         L     R15,CISLLEN         GET CURRENT CIL BLOCKS               C1R08690
         S     R15,CISLACT         ..LESS BLOCKS USED                   C1R08700
         ST    R15,CISLLEFT        ..STORE BLOCKS AVAILABLE             C1R08710
         MVC   CISLBNX(L'D4BCHR),D4BCHR  STORE NEXT LIB BLK IN STATUS   C1R08720
         MVC   CISDRNX(L'D4DBCHR),D4DBCHR  STORE NEXT DIR IN STATUS     C1R08730
         LA    R15,=AL2(G14CIDEB)  GET MAX ENTRIES PER BLOCK            C1R08740
         SR    R15,R6              REDUCE BY THOSE REMAINING            C1R08750
         STC   R15,CISDRNXE        STORE NEXT AVAIL ENTRY NR            C1R08760
*                                  ANY ENTRIES REMAINING                C1R08770
         BNP   NODIRREC            ..NOPE, WE'RE ALL DONE               C1R08780
*                                  ..YES, CREATE EMPTY ENTRY AND WRITE  C1R08790
         MVI   DIR4CELL,ENDDIR     MARK END OF DIRECTORY                C1R08800
         LA    R1,D4DBCHR          POINT AT DIRECTORY CURSOR            C1R08810
         LA    R15,CCW4CDFW        POINT AT CID WRITE DATA CCW          C1R08820
         BAL   R2,XIOFW            WRITE OUT LAST DIR. REC.             C1R08830
         LA    R1,D4DBCHR          POINT AT 2314 DIR WRITE CURSOR       C1R08840
         BAL   R2,NXTCHR           UPDATE TO NEXT REC/TRK/CYL           C1R08850
         LTR   R15,R15             END OF EXTENT REACHED                C1R08860
         BNZ   CLDFMTDN            ..YES, CID FORMAT DONE               C1R08870
NODIRREC DS    0H                                                       C1R08880
*                                                                       C1R08890
         DROP  R9                  END ADDRESSING 2311 DIR ENTRY        C1R08900
         DROP  R7                  END ADDRESSING 2314 DIR ENTRY        C1R08910
*                                                                       C1R08920
*  FORMAT THE REMAINDER OF THE CORE IMAGE DIRECTORY WITH EMPTY RECORDS  C1R08930
*                                                                       C1R08940
         MVI   DIR4,ENDDIR         INDICATE END OF DIRECTORY            C1R08950
         MVI   DIR4+1,0            CLEAR REST OF DIRECTORY RECORD       C1R08960
         MVC   DIR4+2(256),DIR4+1                   ..                  C1R08970
         MVC   DIR4+257(G14CIDBS-256-2),DIR4+256    ..                  C1R08980
         LA    R3,D4DBCHR          POINT R3 AT DIR WRITE CURSOR         C1R08990
         LA    R4,CCW4CDFW         POINT R4 AT CID WRITE CKD            C1R09000
*                                                                       C1R09010
*  CORE IMAGE DIRECTORY FORMAT LOOP TOP                                 C1R09020
*                                                                       C1R09030
CLDFMTLP DS    0H              FORMAT NEXT RELO DIR BLOCK               C1R09040
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R09050
         LR    R15,R4          POINT TO WRITE CKD CCW                   C1R09060
         BAL   R2,XIOFW        2314 WRITE WITH COUNT FLD CREATE         C1R09070
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R09080
         BAL   R2,NXTCHR       UPDATE TO NEXT REC/TRK/CYL               C1R09090
         LTR   R15,R15         END OF EXTENT REACHED                    C1R09100
         BZ    CLDFMTLP        GO FORMAT NEXT RECORD                    C1R09110
*                                                                       C1R09120
CLDFMTDN DS    0H              CORE IMAGE DIR FORMAT COMPLETE           C1R09130
*                                                                       C1R09140
* FORMAT THE BALANCE OF THE CORE IMAGE LIBRARY.  D4BCHR POINTS TO THE   C1R09150
* NEXT BLOCK TO BE WRITTEN, AND IT'S SET UP FOR FORMATTING WRITES       C1R09160
*                                                                       C1R09170
* THE DATA AREA NEEDS A LOOP TO CLEAR IT BECAUSE OF ITS SIZE (ON A      C1R09180
* 370, WE COULD JUST DO MVCL).  WE CLEAR AN AREA FOR ONE CIL BLOCK      C1R09190
* ROUNDED UP TO NEXT 256 BYTES MORE THAN NEEDED, BUT KEEPS CODE CLEAN.  C1R09200
*                                                                       C1R09210
         LA    R0,G14CILBS         GET CIL BLOCK SIZE ON 2314           C1R09220
         LA    R1,IOPH             POINT AT AREA TO CLEAR               C1R09230
         MVI   0(R1),0             INITIALIZE TO X'00'                  C1R09240
         BAL   R2,INITAREA         CLEAR THE AREA                       C1R09250
*                                                                       C1R09260
         LA    R3,D4BCHR           POINT AT CIL WRITE CURSOR            C1R09270
         LA    R4,CCW4CLFW         POINT AT WRITE CKD CIL CCW           C1R09280
         MVC   CCW4CLFW,CCW4CLSV   RESTORE WRITE CKD CCW                C1R09290
*                                                                       C1R09300
*   TOP OF LOOP TO FORMAT REMAINING RECORDS OF CORE IMAGE LIBRARY       C1R09310
*                                                                       C1R09320
CIL4RCLP DS    0H                  WRITE ONE BLANK CIL RECORD           C1R09330
         LR    R1,R3               POINT AT SEEK/SEARCH ADDR            C1R09340
         LR    R15,R4              POINT AT WRITE CKD CIL CCW           C1R09350
         BAL   R2,XIOFW            WRITE BLANK DIRECTORY RECORD         C1R09360
         LR    R1,R3               POINT R1 AT NXTCHR PARM LIST         C1R09370
         BAL   R2,NXTCHR           INCREMENT SEEK/SEARCH BBCCHHR        C1R09380
         LTR   R15,R15             END OF CORE IMAGE LIB EXTENT         C1R09390
         BZ    CIL4RCLP            ..RC=0, NO, CONTINUE FORMATTING      C1R09400
*                                                                       C1R09410
* REWRITE THE UPDATED CORE IMAGE LIBRARY STATUS RECORD TO C0H0R1        C1R09420
*                                                                       C1R09430
         MVC   D4BCHR,C4CILSTS     BBCCHHR FOR CI STATUS RECORD         C1R09440
         LA    R1,D4BCHR           POINT AT 2314 CURSOR                 C1R09450
         LA    R15,CCW4CSWD        POINT AT CI STATUS WRITE DATA        C1R09460
         BAL   R2,XIO              REWRITE CI STATUS                    C1R09470
*                                                                       C1R09480
* FALL THROUGH TO COPY THE RELOCATABLE LIBRARY                          C1R09490
*                                                                       C1R09500
         TITLE 'CONVERT 2311 SYSRES TO 2314   COPY RELOCATABLE LIB'     C1R09510
*********************************************************************** C1R09520
*                                                                       C1R09530
* THE SOURCE 2311 SYSRES USED FOR THIS PROGRAM INCLUDES A CORE IMAGE    C1R09540
* LIBRARY (ALREADY COPIED) AND A RELOCATABLE LIBRARY (TO BE COPIED      C1R09550
* IN THS SECTION).                                                      C1R09560
*                                                                       C1R09570
* COPY THE RELOCATABLE LIBRARY.  THIS IS MUCH SIMPLER THAN COPYING      C1R09580
* THE CORE IMAGE LIBRARY BECAUSE THE DIRECTORY AND LIBRARY BLOCK SIZES  C1R09590
* ARE THE SAME ON 2311 AND 2314.                                        C1R09600
*                                                                       C1R09610
* BUT THE DIRECTORY IS A TEENY BIT MORE DIFFICULT BECAUSE THE FIRST 80  C1R09620
* BYTES OF THE FIRST DIRECTORY RECORD (EQUAL TO FIVE ENTRIES) IS USED   C1R09630
* FOR THE DIRECTORY CONTROL RECORD.                                     C1R09640
*                                                                       C1R09650
* BECAUSE BLOCK SIZES ARE THE SAME FOR DIR AND LIBRARY, WE'LL JUST      C1R09660
* COPY THE DIRECTORY AND THE LIBRARY BLOCK FOR BLOCK FROM THE 2311      C1R09670
* VOLUME TO THE 2314 VOLUME.  WHILE PROCESSING THE DIRECTORY, WE'LL     C1R09680
* RECALCULATE THE BBCCHHR FROM 2311 TO 2314 BY CONVERTING IT TO A       C1R09690
* RELATIVE BLOCK ADDRESS FROM THE START OF THE LIBRARY AND THEN         C1R09700
* CONVERTING THAT BACK TO THE CORRECT BBCCHHR TO 2314 TRACK GEOMETRY    C1R09710
*                                                                       C1R09720
* A SOURCE STATEMENT LIBRARY WILL ALSO BE COPIED, BUT THAT LIBRARY      C1R09730
* WILL COME FROM ANOTHER SYSRES.  THE SAME DIRECTORY TRANSLATION WILL   C1R09740
* BE DONE.                                                              C1R09750
*                                                                       C1R09760
*********************************************************************** C1R09770
         SPACE 3                                                        C1R09780
*********************************************************************** C1R09790
* READ 2311 RELOCATABLE DIRECTORY (RLD) AND SET UP TO CREATE 2314       C1R09800
*      DIRECTORY.                                                       C1R09810
*                                                                       C1R09820
*   RLD STARTS AT THE ADDRESS POINTED TO BY C0H1R2 ON 2311.  THE        C1R09830
*   ADDRESS WAS SAVED DURING DURING THE TRANSFER OF C0H1 FROM 2311      C1R09840
*   TO 2314.                                                            C1R09850
*                                                                       C1R09860
*   THE NEW RLD IS LOCATED AT THE 2314 C0H1R2, BUT WE CAN JUST USE      C1R09870
*   THE RELOCATABLE LIBRARY STATUS RECORD TO LOCATE THINGS.             C1R09880
*                                                                       C1R09890
*   AS DIRECTORY RECORDS ARE COPIED, THE ENTRIES ARE ALTERED TO         C1R09900
*   UPDATE THE CHR USING 2314 TRACK GEOMETRY.  THE 2311 CHR IS          C1R09910
*   CONVERTED TO A BLOCK ADDRESS RELATIVE TO THE START OF THE 2311      C1R09920
*   LIBRARY AND THEN CONVERTED BACK TO A 2314 CCHHRR.                   C1R09930
*                                                                       C1R09940
*   REGISTER USAGE:                                                     C1R09950
*      R6 - COUNT OF 2314 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R09960
*      R7 - POINTER TO CURRENT 2314 DIRECTORY RECORD                    C1R09970
*      R8 - COUNT OF 2311 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R09980
*      R9 - POINTER TO CURRENT 2311 DIRECTORY RECORD                    C1R09990
*                                                                       C1R10000
*********************************************************************** C1R10010
         SPACE 3                                                        C1R10020
*                                                                       C1R10030
*  SET UP THE 2314 DIRECTORY AND LIBRARY CURSORS FOR RELO WRITE         C1R10040
*  OPERATIONS.                                                          C1R10050
*    - SET UP SEEK/SEARCH FIELDS TO WRITE FIRST 2314 RL RECORD          C1R10060
*    - INDICATE FORMATTING WRITES                                       C1R10070
*    - SET UP DIRECTORY EXTENT INFORMATION                              C1R10080
*    - SET UP TRACK GEOMETRY                                            C1R10090
*                                                                       C1R10100
         MVC   D4DBCHR,RLSDRST     MOVE RLD STARTING BBCCHHR            C1R10110
         MVI   D4DREC,0            SET SEARCH FOR R0 TO WRITE R1        C1R10120
         MVI   D4DFLAG,CRSRFW      INDICATE FORMATTING WRITE            C1R10130
         MVC   D4DSOE,RLSDRST      MOVE RLD EXTENT START BBCCHHR        C1R10140
         MVC   D4DEOE,RLSDRLS      MOVE RLD END OF EXTENT FOR NXTCHR    C1R10150
         MVC   D4DRPT,=AL2(G14RLDBT)  MOVE BLOCKS/TRACK FOR NXTCHR      C1R10160
         MVC   D4DSTK,=AL2(R14RLDAT) RLD ABSOLUTE STARTING TRACK        C1R10170
*                                                                       C1R10180
         MVC   D4BCHR,RLSLBST      SET 2314 RL LIB STARTING LOCATION    C1R10190
         MVI   D4REC,0             SET SEARCH FOR R0 FOR WRITE CKD      C1R10200
         MVI   D4FLAG,CRSRFW       INDICATE FORMATTING WRITES           C1R10210
         MVC   D4SOE,RLSLBST       MOVE RLB EXTENT START BBCCHHR        C1R10220
         MVC   D4EOE,RLSLBLS       MOVE RLB END OF EXTENT FOR NXTCHR    C1R10230
         MVC   D4RPT,=AL2(G14RLBBT)  SET BLOCKS PER 2314 RLB TRACK      C1R10240
         MVC   D4STK,=AL2(R14RLBAT)  RLB ABSOLUTE STARTING TRACK        C1R10250
*                                                                       C1R10260
*  SET UP THE 2311 DIRECTORY CURSOR FOR RELO READ OPERATIONS.           C1R10270
*    - SET UP SEEK/SEARCH FIELDS TO WRITE FIRST 2314 RL DIR RECORD      C1R10280
*    - INDICATE DATA-ONLY OPERATIONS (READS, NO FORMATTING WRITES)      C1R10290
*    - SET UP DIRECTORY EXTENT INFORMATION                              C1R10300
*    - SET UP TRACK GEOMETRY FOR THE RELO DIRECTORY                     C1R10310
*  THE 2311 RELO LIBRARY CURSOR IS SET UP AFTER READING THE RELO        C1R10320
*  LIBRARY STATUS RECORD.                                               C1R10330
*                                                                       C1R10340
         MVC   D1DBCHR,RLD1SCHR   POINT AT FIRST BLOCK IN RLD           C1R10350
         MVI   D1DFLAG,CRSRDT     INDICATE NOT FORMATTING WRITES        C1R10360
         MVC   D1DSOE,RLD1SCHR    SET RLD EXTENT START BBCCHHR          C1R10370
         MVC   D1DEOE(2),=X'FFFF' RLD END OF EXTENT NOT USED            C1R10380
         MVC   D1DRPT,=AL2(G11RLDBT)  SET RLD BLOCKS PER 2311 TRACK     C1R10390
*                                                                       C1R10400
*  CALCULATE ABSOLUTE TRACK NUMBER OF 2311 RELO DIR START AND SAVE IN   C1R10410
*  THE CURSOR.  WE'LL USE IT TO TRANSLATE THE 2311 RELO DIRECTORY NEXT  C1R10420
*  AVAILABLE ENTRY INTO THE 2314 NEXT AVAILABLE ENTRY.                  C1R10430
*                                                                       C1R10440
         LH    R15,D1DSOECC        GET DIRECTORY CYLINDER ADDR          C1R10450
         MH    R15,D1DTPCH         CONVERT TO TRACK ADDRESS             C1R10460
         AH    R15,D1DSOEHH        ADD STARTING HEAD TO TRACKS          C1R10470
         STH   R15,D1DSTK          STORE 2311 RELO DIR STARTING TRACK   C1R10480
*                                                                       C1R10490
*  READ THE FIRST 2311 RELO DIRECTORY RECORD.  WE WILL EXTRACT          C1R10500
*  DIRECTORY AND LIBRARY INFORMATION FROM THE STATUS RECORD IN THE      C1R10510
*  FIRST 80 BYTES, AND THEN WE'LL SET UP TO PROCESS ENTRIES STARTING    C1R10520
*  WITH ENTRY 5, WHICH IS PAST THE 80-BYTE DIRECTORY CONTROL RECORD.    C1R10530
*                                                                       C1R10540
         LA    R15,CCW1RDRD        POINT TO 2311 DIR READ CCW           C1R10550
         LA    R1,D1DBCHR          POINT TO 2311 DIR CURSOR             C1R10560
         BAL   R2,XIO              READ 2311 FIRST DIR. RECORD          C1R10570
         LA    R9,DIR1             POINT R9 AT DIRECTORY/STATUS REC     C1R10580
         USING LBSTATUS,R9         MAKE STATUS RECORD ADDRESSABLE       C1R10590
         MVC   D1DSOE,LBSDRST      MOVE RLD EXTENT START BBCCHHR        C1R10600
         MVC   D1DEOE,LBSDRLS      MOVE RLD END OF EXTENT FOR NXTCHR    C1R10610
*                                                                       C1R10620
*  SET UP THE 2311 LIBRARY CURSOR FOR RELO READ OPERATIONS              C1R10630
*                                                                       C1R10640
         MVC   D1BCHR,LBSLBST      POINT AT FIRST BLOCK IN RLB          C1R10650
         MVI   D1FLAG,CRSRDT       INDICATE NOT FORMATTING WRITES       C1R10660
         MVC   D1SOE,LBSLBST       MOVE RLB EXTENT START BBCCHHR        C1R10670
         MVC   D1EOE,LBSLBLS       MOVE RLB END OF EXTENT FOR NXTCHR    C1R10680
         MVC   D1RPT,=AL2(G11RLBBT)  SET RLB BLOCKS PER 2311 TRACK      C1R10690
*                                                                       C1R10700
*  CALCULATE ABSOLUTE TRACK NUMBER OF 2311 RELO LIB START AND SAVE      C1R10710
*  IN THE CURSOR.  WE'LL USE IT TO TRANSLATE LIBRARY ADDRESSES          C1R10720
*  FOUND IN RELO DIR ENTRIES FROM 2311 GEOMETRY TO 2314.                C1R10730
*                                                                       C1R10740
         LH    R15,D1DSTK          GET RELO DIR ABSOLUTE START TRACK    C1R10750
         AH    R15,LBSDTRKS        ADD SIZE OF DIR IN TRACKS            C1R10760
         STH   R15,D1STK           STORE LIB ABSOLUTE STARTING TRACK    C1R10770
*                                                                       C1R10780
*  UPDATE 2314 ACTIVE AND AVAILABLE RELOCATABLE LIBRARY BLOCKS          C1R10790
*                                                                       C1R10800
         L     R14,LBSLACT         GET 2311 RELO BLOCKS ACTIVE          C1R10810
         ST    R14,RLSLACT         STORE IN 2314 STATUS RECORD          C1R10820
         L     R15,RLSLLEN         GET 2314 TOTAL RLB BLOCKS            C1R10830
         SR    R15,R14             ..LESS BLOCKS ACTIVE                 C1R10840
         ST    R15,RLSLLEFT        ..STORE BLOCKS AVAILABLE             C1R10850
*                                                                       C1R10860
*  TRANSLATE 2311 NEXT DIRECTORY ENTRY BBCCHHR INTO THE CORRESPONDING   C1R10870
*  2314 NEXT ENTRY BBCCHHR.  BECAUSE DIR BLOCK SIZES MATCH, THE ENTRY   C1R10880
*  NUMBER ('E' IN BBCCHHRE) DOES NOT CHANGE.                            C1R10890
*                                                                       C1R10900
         MVC   WORKBCHR,LBSDRNX    PROVIDE DIR BBCCHHR TO XLATBCHR      C1R10910
         LA    R0,D4DBCHR          OUPUT CURSOR IS 2314 DIR             C1R10920
         LA    R1,D1DBCHR          INUT CURSOR IS 2311 DIR              C1R10930
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R10940
         MVC   RLSDRNX(L'WORKBCHR),WORKBCHR MOVE 2314 DIR NEXT BCHR     C1R10950
         MVC   RLSDRNXE,LBSDRNXE   MOVE ENTRY NR (STAYS SAME)           C1R10960
*                                                                       C1R10970
*  CALC 2311 RELO LIBRARY NEXT AVAIL BBCCHHR AND RELATIVE TRACK         C1R10980
*                                                                       C1R10990
         MVC   WORKBCHR,LBSLBNX    SAVE LIB NEXT ENTRY BBCCHHR          C1R11000
         LA    R0,D4BCHR           OUPUT CURSOR IS 2314 LIB             C1R11010
         LA    R1,D1BCHR           INUT CURSOR IS 2311 LIB              C1R11020
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R11030
         MVC   RLSLBNX(L'WORKBCHR),WORKBCHR MOVE 2314 LIB NEXT BCHR     C1R11040
         DROP  R9                  NO NEED FOR STATUS RECORD            C1R11050
*                                                                       C1R11060
*  INITIALIZE THE FIRST DIRECTORY BLOCK BY PLACING THE LIBRARY STATUS   C1R11070
*  RECORD IN THE FIRST 80 BYTES (FIRST FIVE ENTRIES)                    C1R11080
*                                                                       C1R11090
         MVC   IO+8(L'RLSTATUS),RLSTATUS  MOVE INIT'D DIR CTL RECORD    C1R11100
         MVI   IO+8+L'RLSTATUS,ENDDIR  PUT EODIR IN 5TH DIR ENTRY       C1R11110
         LA    R6,G14RLDEB     ENTRIES PER RECORD 2314 OUTPUT           C1R11120
         SH    R6,=AL2(L'RLSTATUS/GXXRLDES)  SKIP DIR CTL RECORD SPACE  C1R11130
         LA    R7,IO+8+L'RLSTATUS POINT TO FIRST DIR ENTRY TO PROCESS   C1R11140
         USING DIR4RENT,R7     MAKE 2314 DIR ENTRY ADDRESSABLE          C1R11150
*                                                                       C1R11160
         LA    R8,G11RLDEB     ENTRIES PER RECORD 2311 INPUT            C1R11170
         SH    R8,=AL2(L'RLSTATUS/GXXRLDES)  SKIP DIR CTL RECORD SPACE  C1R11180
         LA    R9,DIR1+L'RLSTATUS  POINT TO FIRST 2311 DIR ENTRY        C1R11190
         USING DIR1RENT,R9     MAKE 2311 DIR ENTRY ADDRESSABLE          C1R11200
         B     RLDENTRY        GO PROCESS FIRST ENTRY                   C1R11210
*                                                                       C1R11220
RLD1NXTR LA    R15,CCW1RDRD    POINT TO 2311 DIR READ CCW               C1R11230
         LA    R1,D1DBCHR      POINT TO 2311 DIR CURSOR                 C1R11240
         BAL   R2,XIO          READ 2311 DIR. RECORD                    C1R11250
         LA    R8,G11RLDEB     ENTRIES PER RECORD 2311 INPUT            C1R11260
         LA    R9,DIR1         POINT TO 2311 DIRECTORY AREA             C1R11270
*                                                                       C1R11280
RLDENTRY DS    0H              PROCESS CURRENT 2311 RLD ENTRY           C1R11290
*                                                                       C1R11300
         CLI   DIR1RNAM,ENDDIR END OF DIRECTORY ENTRIES                 C1R11310
         BE    RLDDONE         ..YES, UPDATE DIR AND LIB STATUS REC     C1R11320
         CLI   DIR1RNAM,EMPTYDIR UNUSED ENTRY                           C1R11330
         BE    RLDNEXTE        ..YES, GO PROCESS NEXT ENTRY             C1R11340
*                                                                       C1R11350
* ACTIVE RELOCATABLE DIRECTORY ENTRY.  CREATE THE CORRESPONDING 2314    C1R11360
* DIRECTORY ENTRY, ADJUSTING THE CHR TO REFLECT 2314 TRACK GEOMETRY     C1R11370
*                                                                       C1R11380
         L     R14,RLSDACT         GET CURRENT ACTIVE ENTRY COUNT       C1R11390
         LA    R14,1(,R14)         ANOTHER ACTIVE ENTRY                 C1R11400
         ST    R14,RLSDACT         UPDATE RL STATUS RECORD              C1R11410
         MVC   DIR4RCEL,DIR1RCEL   COPY RELO DIRECTORY ENTRY            C1R11420
*                                                                       C1R11430
         XC    WORKBCHR(3),WORKBCHR   CLEAR BB AND HIGH-ORDER C         C1R11440
         MVC   WORKBCHR+3(4),DIR4RADR   MOVE 2311 CHHR OF RELO MOD      C1R11450
         LA    R0,D4BCHR           OUPUT CURSOR IS 2314 LIB             C1R11460
         LA    R1,D1BCHR           INUT CURSOR IS 2311 LIB              C1R11470
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R11480
         MVC   DIR4RADR,WORKBCHR+3  MOVE RESULT CHHR TO ENTRY           C1R11490
*                                                                       C1R11500
*  FINISHED WITH CURRENT ENTRY, POINT TO NEXT.  IF END OF 2314 DIR REC, C1R11510
*  WRITE CURRENT AND RESET POINTERS TO NEXT 2314 RLD ENTRY.             C1R11520
*                                                                       C1R11530
         LA    R7,GXXRLDES(,R7)  POINT TO NEXT ENTRY                    C1R11540
         BCT   R6,RLD14NFL     DECREMENT, BRANCH IF ENTRIES LEFT        C1R11550
*                                                                       C1R11560
*  2314 RECORD FULL.  WRITE IT AND SET UP FOR NEXT ENTRY.               C1R11570
*                                                                       C1R11580
         LA    R15,CCW4RDFW    POINT AT RLD WRITE CKD CCW               C1R11590
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R11600
         BAL   R2,XIOFW        WRITE CURRENT RLD DIRECTORY              C1R11610
         LA    R1,D4DBCHR      POINT TO DIR SEEK/SEARCH FIELD           C1R11620
         BAL   R2,NXTCHR       UPDATE TO NEXT BBCCHHR                   C1R11630
         LA    R7,IO+8         POINT TO START OF NEW DIRECTORY RECD     C1R11640
         LA    R6,G14RLDEB     ENTRIES PER RECORD 2314 OUTPUT           C1R11650
*                                                                       C1R11660
RLD14NFL DS    0H              2314 DIR BLOCK NOT FULL                  C1R11670
*                                                                       C1R11680
* 2311 DIRECTORY ENTRY PROCESSED.  PROCESS THE NEXT ONE.                C1R11690
*                                                                       C1R11700
RLDNEXTE DS    0H                                                       C1R11710
         LA    R9,GXXRLDES(,R9) POINT TO NEXT 2311 ENTRY                C1R11720
         BCT   R8,RLDENTRY     PROCESS THAT ENTRY                       C1R11730
*                                                                       C1R11740
* END OF CURRENT DIRECTORY RECORD REACHED.  GET THE NEXT ONE.           C1R11750
*                                                                       C1R11760
         LA    R1,D1DBCHR      POINT TO 2311 DIR CURSOR                 C1R11770
         BAL   R2,NXTCHR       UPDATE CURSOR TO NEXT DIR RECD           C1R11780
         LTR   R15,R15         END OF 2311 DIR EXTENT REACHED           C1R11790
         BZ    RLD1NXTR        ..NO, GO READ NEXT RECORD                C1R11800
*                                                                       C1R11810
* 2311 DIRECTORY COMPLETE.  MAKE NEXT 2314 ENTRY 'END OF DIRECTORY'     C1R11820
* AND WRITE THE FINAL PARTIAL OR FULL DIRECTORY RECORD                  C1R11830
*                                                                       C1R11840
RLDDONE  DS    0H                                                       C1R11850
         MVI   DIR4RNAM,ENDDIR INDICATE END OF DIRECTORY                C1R11860
         MVI   DIR4RNAM+1,0    START FILL OF REST OF BLOCK              C1R11870
         MVC   DIR4RNAM+2(256),DIR4RNAM+1  ..WITH ZEROS                 C1R11880
         MVC   DIR4RNAM+258(G14RLDBS-256-2),DIR4RNAM+1 ..               C1R11890
         LA    R3,D4DBCHR      POINT TO DIR WRITE CURSOR                C1R11900
         LA    R4,CCW4RDFW     POINT TO WRITE DIR CKD CCW               C1R11910
*                                                                       C1R11920
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R11930
         LR    R15,R4          POINT TO WRITE CKD CCW                   C1R11940
         BAL   R2,XIOFW        2314 WRITE WITH COUNT FLD CREATE         C1R11950
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R11960
         BAL   R2,NXTCHR       UPDATE TO NEXT REC/TRK/CYL               C1R11970
         MVI   IO+8,ENDDIR     INDICATE END OF DIRECTORY                C1R11980
         MVI   IO+9,0          ZERO 2ND POSITION OF EMPTY REC           C1R11990
         MVC   IO+10(256),IO+9  FILL REST OF RECORD...                  C1R12000
         MVC   IO+266(G14RLDBS-256-2),IO+9  ...WITH ZEROS               C1R12010
*                                                                       C1R12020
*  FORMAT THE REMAINDER OF THE RELOCATABLE DIRECTORY WITH EMPTY         C1R12030
*  RECORDS                                                              C1R12040
*                                                                       C1R12050
RLDFMTLP DS    0H              FORMAT NEXT RELO DIR BLOCK               C1R12060
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R12070
         LR    R15,R4          POINT TO WRITE CKD CCW                   C1R12080
         BAL   R2,XIOFW        2314 WRITE WITH COUNT FLD CREATE         C1R12090
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R12100
         BAL   R2,NXTCHR       UPDATE TO NEXT REC/TRK/CYL               C1R12110
         LTR   R15,R15         END OF EXTENT REACHED                    C1R12120
         BZ    RLDFMTLP        ..NO, CONTINUE FORMATTING                C1R12130
*                                                                       C1R12140
RLDFMTDN DS    0H              RELO DIR FORMAT COMPLETE                 C1R12150
*                                                                       C1R12160
*  UPDATE RELO STATUS RECORD IN THE FIRST DIRECTORY BLOCK               C1R12170
*                                                                       C1R12180
         MVC   D4DBCHR,RLSDRST SET 2314 RL DIR STARTING LOCATION        C1R12190
         LA    R15,CCW4RDRD    POINT TO DIR READ CCW                    C1R12200
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R12210
         BAL   R2,XIO          READ RLD DIRECTORY RECORD ONE            C1R12220
         MVC   DIR4(L'RLSTATUS),RLSTATUS  MOVE UPDATED RL STATUS        C1R12230
         LA    R15,CCW4RDWD    POINT TO DIR WRITE DATA CCW              C1R12240
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R12250
         BAL   R2,XIO          WRITE RLD DIRECTORY RECORD ONE           C1R12260
*                                                                       C1R12270
*  RELOCATABLE DIRECTORY PROCESSED, WITH LIBRARY ADDRESSES IN EACH      C1R12280
*  ENTRY UPDATED TO REFLECT 2314 TRACK GEOMETRY.  NOW ALL THAT          C1R12290
*  REMAINS IS A COPY OF THE LIBRARY.  WE'LL JUST COPY BLOCKS UNTIL      C1R12300
*  END OF ACTIVE PORTION OF THE 2311 LIBRARY                            C1R12310
*                                                                       C1R12320
*  COPY RELOCATABLE LIBRARY.  BECAUSE THE BLOCK SIZE DID NOT CHANGE     C1R12330
*  FROM 2311 TO 2314, AND BECAUSE WE UPDATED BBCCHHR FOR EACH DIRECTORY C1R12340
*  ENTRY AS WE COPIED THE DIRECTORY, ALL WE NEED DO IS COPY BLOCK FOR   C1R12350
*  BLOCK FROM 2311 TO 2314.                                             C1R12360
*                                                                       C1R12370
         L     R5,RLSLACT      SET 2311/14 RLB ACTIVE BLOCK COUNT       C1R12380
         LA    R3,D4BCHR       POINT R3 AT 2314 OUTPUT CURSOR           C1R12390
         LA    R4,D1BCHR       POINT R4 AT 2311 READ CURSOR             C1R12400
*                                                                       C1R12410
RLBCOPY  DS    0H              COPY ANOTHER RLB BLOCK                   C1R12420
         LA    R15,CCW1RLRD    POINT AT RELO LIB READ CCW               C1R12430
         LR    R1,R4           POINT AT 2311 RELO LIB SEEK/SEARCH       C1R12440
         BAL   R2,XIO          READ 2311 RLB BLOCK                      C1R12450
         LA    R15,CCW4RLFW    POINT AT RELO LIB WRITE CCW              C1R12460
         LR    R1,R3           POINT AT 2314 RELO LIB SEEK/SEARCH       C1R12470
         BAL   R2,XIOFW        WRITE BLOCK TO 2314                      C1R12480
         LR    R1,R4           POINT AT 2311 RELO LIB SEEK/SEARCH       C1R12490
         BAL   R2,NXTCHR       UPDATE 2311 NEXT READ ADDRESS            C1R12500
         LR    R1,R3           POINT AT 2314 RELO LIB SEEK/SEARCH       C1R12510
         BAL   R2,NXTCHR       UPDATE 2314 NEXT WRITE ADDRESS           C1R12520
         BCT   R5,RLBCOPY      LOOP UNTIL COPY COMPLETE                 C1R12530
*                                                                       C1R12540
*  UPDATE DIRECTORY CONTROL RECORD IN FIRST 80 BYTES OF FIRST           C1R12550
*  DIRECTORY RECORD                                                     C1R12560
*                                                                       C1R12570
         MVC   D4DBCHR,RLSDRST SET 2314 RL DIR STARTING LOCATION        C1R12580
         LA    R15,CCW4RDRD    POINT TO RL DIR READ CCW                 C1R12590
         LA    R1,D4DBCHR      POINT TO DIRECTORY CURSOR                C1R12600
         BAL   R2,XIO          READ RLD DIRECTORY RECORD ONE            C1R12610
*                                                                       C1R12620
         MVC   DIR4(L'RLSTATUS),RLSTATUS  UPDATE RL STATUS RECORD       C1R12630
         LA    R15,CCW4RDWD    POINT TO RL DIR WRITE CCW                C1R12640
         LA    R1,D4DBCHR      POINT TO DIRECTORY CURSOR                C1R12650
         BAL   R2,XIO          WRITE RL UPDATED STATUS RECORD           C1R12660
         DROP  R7,R9           RLD ENTRIES NO LONGER NEED ADRESSING     C1R12670
*                                                                       C1R12680
* EXTRACT METRICS FOR COMPLETION MESSAGES                               C1R12690
*                                                                       C1R12700
         L     R15,RLSDACT     GET RELO MODULE COUNT                    C1R12710
         CVD   R15,WORKCVD     CONVERT BINARY TO PACKED                 C1R12720
         ZAP   OMRELOS,WORKCVD MOVE FOR METRICS MESSAGE                 C1R12730
         L     R15,RLSLACT     GET RELO LIB BLOCK COUNT                 C1R12740
         CVD   R15,WORKCVD     CONVERT BINARY TO PACKED                 C1R12750
         ZAP   OMRELOB,WORKCVD MOVE FOR METRICS MESSAGE                 C1R12760
*                                                                       C1R12770
         TITLE 'CONVERT 2311 SYSRES TO 2314   COPY SOURCE STMT LIB'     C1R12780
*********************************************************************** C1R12790
*                                                                       C1R12800
* THE SOURCE 2311 SYSRES USED FOR THIS PROGRAM INCLUDES A CORE IMAGE    C1R12810
* LIBRARY AND A RELOCATABLE LIBRARY (ALREADY COPIED).  A SECOND SYSRES  C1R12820
* INCLUDES A SOURCE STATEMENT LIBRARY THAT MUST BE COPIED TO THE        C1R12830
* TARGET 2314 SYSRES.                                                   C1R12840
*                                                                       C1R12850
* COPY THE SOURCE STATEMENT LIBRARY.  THIS IS MUCH THE SAME AS THE      C1R12860
* RELOCATABLE LIBRARY, AND AS WITH THE RELOCATABLE LIBRARY, THE         C1R12870
* DIRECTORY AND LIBRARY BLOCK SIZES ARE THE SAME ON 2311 AND 2314.      C1R12880
*                                                                       C1R12890
* AND THE DIRECTORY IS A TEENY BIT MORE DIFFICULT BECAUSE THE FIRST 80  C1R12900
* BYTES OF THE FIRST DIRECTORY RECORD (EQUAL TO FIVE ENTRIES) IS USED   C1R12910
* FOR THE DIRECTORY CONTROL RECORD.                                     C1R12920
*                                                                       C1R12930
* BECAUSE BLOCK SIZES ARE THE SAME FOR DIR AND LIBRARY, WE'LL JUST      C1R12940
* COPY THE DIRECTORY AND THE LIBRARY BLOCK FOR BLOCK FROM THE 2311      C1R12950
* VOLUME TO THE 2314 VOLUME.  WHILE PROCESSING THE DIRECTORY, WE'LL     C1R12960
* RECALCULATE THE BBCCHHR FROM 2311 TO 2314 BY CONVERTING IT TO A       C1R12970
* RELATIVE BLOCK ADDRESS FROM THE START OF THE LIBRARY AND THEN         C1R12980
* CONVERTING THAT BACK TO THE CORRECT BBCCHHR TO 2314 TRACK GEOMETRY    C1R12990
*                                                                       C1R13000
* THIS SOURCE STATEMENT LIBRARY COMES FROM ANOTHER SYSRES VOLUME,       C1R13010
* NOT THE SAME ONE THAT THE RELO AND CORE IMAGE LIBRARIES CAME FROM,    C1R13020
* BUT IT IS PART OF THE SAME DISTRIBUTION TAPE.                         C1R13030
*                                                                       C1R13040
*********************************************************************** C1R13050
         SPACE 3                                                        C1R13060
*********************************************************************** C1R13070
* READ 2311 SOURCE STATEMENT DIRECTORY (SLD) AND SET UP TO CREATE 2314  C1R13080
*      DIRECTORY.                                                       C1R13090
*                                                                       C1R13100
*   SLD STARTS AT THE ADDRESS POINTED TO BY C0H1R3 ON SLB2311.  THE     C1R13110
*   ADDRESS WAS SAVED DURING INITIALIZATION OF THIS PROGRAM             C1R13120
*                                                                       C1R13130
*   THE NEW SLD IS POINTED TO BY 2314 C0H1R2, BUT WE CAN JUST USE       C1R13140
*   THE SOURCE STATEMENT LIBRARY STATUS RECORD TO LOCATE THINGS.        C1R13150
*                                                                       C1R13160
*   AS DIRECTORY ENTRIES ARE COPIED, THEY ARE ALTERED TO UPDATE THE     C1R13170
*   CHR USING 2314 TRACK GEOMETRY.  THE 2311 CHR IS CONVERTED TO A      C1R13180
*   BLOCK ADDRESS RELATIVE TO THE START OF THE 2311 LIBRARY AND THEN    C1R13190
*   CONVERTED BACK TO A 2314 CCHHRR.                                    C1R13200
*                                                                       C1R13210
*   REGISTER USAGE:                                                     C1R13220
*      R6 - COUNT OF 2314 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R13230
*      R7 - POINTER TO CURRENT 2314 DIRECTORY RECORD                    C1R13240
*      R8 - COUNT OF 2311 DIR ENTRIES LEFT IN CURRENT DIR RECORD        C1R13250
*      R9 - POINTER TO CURRENT 2311 DIRECTORY RECORD                    C1R13260
*                                                                       C1R13270
*********************************************************************** C1R13280
         SPACE 3                                                        C1R13290
*                                                                       C1R13300
*  SET UP THE TWO 2311 CURSORS TO POINT TO THE DTF FOR THE SYSRES       C1R13310
*  CONTAINING THE SOURCE STATEMENT LIBRARY                              C1R13320
*                                                                       C1R13330
         LA    R15,SLB2311         GET ADDR OF DTFPH FOR SLB2311 RES    C1R13340
         ST    R15,D1DDTF          STORE IN DIRECTORY CURSOR            C1R13350
         ST    R15,D1DTF           ALSO STORE IN LIBRARY CURSOR         C1R13360
*                                                                       C1R13370
*  SET UP THE 2314 DIRECTORY AND LIBRARY CURSORS FOR SOURCE STATEMENT   C1R13380
*  WRITE OPERATIONS.                                                    C1R13390
*    - SET UP SEEK/SEARCH FIELDS TO WRITE FIRST 2314 SL RECORD          C1R13400
*    - INDICATE FORMATTING WRITES                                       C1R13410
*    - SET UP DIRECTORY EXTENT INFORMATION                              C1R13420
*    - SET UP TRACK GEOMETRY                                            C1R13430
*                                                                       C1R13440
         MVC   D4DBCHR,SLSDRST     MOVE SLD STARTING BBCCHHR            C1R13450
         MVI   D4DREC,0            SET SEARCH FOR R0 TO WRITE R1        C1R13460
         MVI   D4DFLAG,CRSRFW      INDICATE FORMATTING WRITE            C1R13470
         MVC   D4DSOE,SLSDRST      MOVE SLD EXTENT START BBCCHHR        C1R13480
         MVC   D4DEOE,SLSDSLS      MOVE SLD END OF EXTENT FOR NXTCHR    C1R13490
         MVC   D4DRPT,=AL2(G14SLDBT)  MOVE BLOCKS/TRACK FOR NXTCHR      C1R13500
         MVC   D4DSTK,=AL2(R14SLDAT) SLD ABSOLUTE STARTING TRACK        C1R13510
*                                                                       C1R13520
         MVC   D4BCHR,SLSLBST      SET 2314 RL LIB STARTING LOCATION    C1R13530
         MVI   D4REC,0             SET SEARCH FOR R0 FOR WRITE CKD      C1R13540
         MVI   D4FLAG,CRSRFW       INDICATE FORMATTING WRITES           C1R13550
         MVC   D4SOE,SLSLBST       MOVE SLB EXTENT START BBCCHHR        C1R13560
         MVC   D4EOE,SLSLBLS       MOVE SLB END OF EXTENT FOR NXTCHR    C1R13570
         MVC   D4RPT,=AL2(G14SLBBT)  SET BLOCKS PER 2314 SLB TRACK      C1R13580
         MVC   D4STK,=AL2(R14SLBAT)  SLB ABSOLUTE STARTING TRACK        C1R13590
*                                                                       C1R13600
*  SET UP THE 2311 DIRECTORY CURSOR FOR RELO READ OPERATIONS.           C1R13610
*    - GET BBCCHHR OF SOURCE LIB FROM SYSRES CONTROL RECORD             C1R13620
*    - SET UP SEEK/SEARCH FIELDS TO READ FIRST 2314 SL DIR RECORD       C1R13630
*    - INDICATE DATA-ONLY OPERATIONS (READS, NO FORMATTING WRITES)      C1R13640
*    - SET UP DIRECTORY EXTENT INFORMATION                              C1R13650
*    - SET UP TRACK GEOMETRY FOR THE RELO DIRECTORY                     C1R13660
*  THE 2311 RELO LIBRARY CURSOR IS SET UP AFTER READING THE RELO        C1R13670
*  LIBRARY STATUS RECORD.                                               C1R13680
*                                                                       C1R13690
         MVC   D1BCHR,C1BCHRH1     SET SEEK/SEARCH TO C0H1R1            C1R13700
         LA    R1,D1BCHR           POINT R1 AT 2311 READ CURSOR         C1R13710
         LA    R15,CCW1HD1         POINT TO CCW STRING FOR C0H1R1-5     C1R13720
         BAL   R2,XIO              READ 2311 SLB C0H1 RECORDS 1-5       C1R13730
         MVC   D1DBCHR,C0H1R3+8    SAVE LOC OF SOURCE DIR IN CURSOR     C1R13740
         MVI   D1DFLAG,CRSRDT      INDICATE NOT FORMATTING WRITES       C1R13750
         MVC   D1DSOE,D1DBCHR      SET SLD EXTENT START BBCCHHR         C1R13760
         MVC   D1DEOE(2),=X'FFFF'  SLD END OF EXTENT NOT USED           C1R13770
         MVC   D1DRPT,=AL2(G11SLDBT)  SET SLD BLOCKS PER 2311 TRACK     C1R13780
*                                                                       C1R13790
*  CALCULATE ABSOLUTE TRACK NUMBER OF 2311 SOURCE STATEMENT DIRECTORY   C1R13800
*  START AND SAVE IN THE CURSOR.  WE'LL USE IT TO TRANSLATE THE 2311    C1R13810
*  SOURCE STATEMENT DIRECTORY NEXT AVAILABLE ENTRY INTO THE 2314 NEXT   C1R13820
*  AVAILABLE ENTRY.                                                     C1R13830
*                                                                       C1R13840
         LH    R15,D1DSOECC        GET DIRECTORY CYLINDER ADDR          C1R13850
         MH    R15,D1DTPCH         CONVERT TO TRACK ADDRESS             C1R13860
         AH    R15,D1DSOEHH        ADD STARTING HEAD TO TRACKS          C1R13870
         STH   R15,D1DSTK          STORE 2311 SOURCE DIR START TRACK    C1R13880
*                                                                       C1R13890
*  READ THE FIRST 2311 SOURCE DIRECTORY RECORD.  WE WILL EXTRACT        C1R13900
*  DIRECTORY AND LIBRARY INFORMATION FROM THE STATUS RECORD IN THE      C1R13910
*  FIRST 80 BYTES, AND THEN WE'LL SET UP TO PROCESS ENTRIES STARTING    C1R13920
*  WITH ENTRY 5, WHICH IS PAST THE 80-BYTE DIRECTORY CONTROL RECORD.    C1R13930
*                                                                       C1R13940
         LA    R15,CCW1SDRD        POINT TO 2311 DIR READ CCW           C1R13950
         LA    R1,D1DBCHR          POINT TO 2311 DIR CURSOR             C1R13960
         BAL   R2,XIO              READ SLB2311 FIRST DIR. RECORD       C1R13970
         LA    R9,DIR1             POINT R9 AT DIRECTORY/STATUS REC     C1R13980
         USING LBSTATUS,R9         MAKE STATUS RECORD ADDRESSABLE       C1R13990
         MVC   D1DSOE,LBSDRST      MOVE SLD EXTENT START BBCCHHR        C1R14000
         MVC   D1DEOE,LBSDRLS      MOVE SLD END OF EXTENT FOR NXTCHR    C1R14010
*                                                                       C1R14020
*  SET UP THE 2311 LIBRARY CURSOR FOR SOURCE STATEMENT READ OPERATIONS  C1R14030
*                                                                       C1R14040
         MVC   D1BCHR,LBSLBST      POINT AT FIRST BLOCK IN SLB          C1R14050
         MVI   D1FLAG,CRSRDT       INDICATE NOT FORMATTING WRITES       C1R14060
         MVC   D1SOE,LBSLBST       MOVE SLB EXTENT START BBCCHHR        C1R14070
         MVC   D1EOE,LBSLBLS       MOVE SLB END OF EXTENT FOR NXTCHR    C1R14080
         MVC   D1RPT,=AL2(G11SLBBT)  SET SLB BLOCKS PER 2311 TRACK      C1R14090
*                                                                       C1R14100
*  CALCULATE ABSOLUTE TRACK NUMBER OF 2311 SOURCE STATEMENT LIBRARY     C1R14110
*  START AND SAVE IN THE CURSOR.  WE'LL USE IT TO TRANSLATE LIBRARY     C1R14120
*  ADDRESSES FOUND IN SOURCE STATEMENT DIRECTORY ENTRIES FROM 2311      C1R14130
*  GEOMETRY TO 2314.                                                    C1R14140
*                                                                       C1R14150
         LH    R15,D1DSTK          GET SOURCE DIR ABS. START TRACK      C1R14160
         AH    R15,LBSDTRKS        ADD SIZE OF DIR IN TRACKS            C1R14170
         STH   R15,D1STK           STORE LIB ABSOLUTE STARTING TRACK    C1R14180
*                                                                       C1R14190
*  UPDATE 2314 ACTIVE AND AVAILABLE SOURCE STATEMENT LIBRARY BLOCKS     C1R14200
*                                                                       C1R14210
         L     R14,LBSLACT         GET 2311 SOURCE BLOCKS ACTIVE        C1R14220
         ST    R14,SLSLACT         STORE IN 2314 STATUS RECORD          C1R14230
         L     R15,SLSLLEN         GET 2314 TOTAL SLB BLOCKS            C1R14240
         SR    R15,R14             ..LESS BLOCKS ACTIVE                 C1R14250
         ST    R15,SLSLLEFT        ..STORE BLOCKS AVAILABLE             C1R14260
*                                                                       C1R14270
*  TRANSLATE 2311 NEXT DIRECTORY ENTRY BBCCHHR INTO THE CORRESPONDING   C1R14280
*  2314 NEXT ENTRY BBCCHHR.  BECAUSE DIR BLOCK SIZES MATCH, THE ENTRY   C1R14290
*  NUMBER DOES NOT CHANGE.                                              C1R14300
*                                                                       C1R14310
         MVC   WORKBCHR,LBSDRNX    PROVIDE DIR BBCCHHR TO XLATBCHR      C1R14320
         LA    R0,D4DBCHR          OUPUT CURSOR IS 2314 DIR             C1R14330
         LA    R1,D1DBCHR          INUT CURSOR IS 2311 DIR              C1R14340
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R14350
         MVC   SLSDRNX(L'WORKBCHR),WORKBCHR MOVE 2314 DIR NEXT BCHR     C1R14360
         MVC   SLSDRNXE,LBSDRNXE   MOVE ENTRY NR (STAYS SAME)           C1R14370
*                                                                       C1R14380
*  CALC 2311 SOURCE STATEMENT LIBRARY NEXT AVAIL BBCCHHR AND RELATIVE   C1R14390
*  TRACK                                                                C1R14400
*                                                                       C1R14410
         MVC   WORKBCHR,LBSLBNX    SAVE LIB NEXT ENTRY BBCCHHR          C1R14420
         LA    R0,D4BCHR           OUPUT CURSOR IS 2314 LIB             C1R14430
         LA    R1,D1BCHR           INUT CURSOR IS 2311 LIB              C1R14440
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R14450
         MVC   SLSLBNX(L'WORKBCHR),WORKBCHR MOVE 2314 LIB NEXT BCHR     C1R14460
         DROP  R9                  NO NEED FOR STATUS RECORD            C1R14470
*                                                                       C1R14480
*  INITIALIZE THE FIRST DIRECTORY BLOCK BY PLACING THE LIBRARY STATUS   C1R14490
*  RECORD IN THE FIRST 80 BYTES (FIRST FIVE ENTRIES)                    C1R14500
*                                                                       C1R14510
         MVC   IO+8(L'SLSTATUS),SLSTATUS  MOVE SL LIB STATUS TO BLOCK   C1R14520
         LA    R6,G14SLDEB     ENTRIES PER RECORD 2314 OUTPUT           C1R14530
         SH    R6,=AL2(L'SLSTATUS/GXXSLDES)  SKIP DIR CTL RECORD SPACE  C1R14540
         LA    R7,IO+8+L'SLSTATUS POINT TO FIRST DIR ENTRY TO PROCESS   C1R14550
         USING DIR4SENT,R7     MAKE 2314 DIR ENTRY ADDRESSABLE          C1R14560
*                                                                       C1R14570
* POINT TO THE FIRST DIRECTORY ENTRY IN THE FIRST 2311 DIRECTORY RECORD C1R14580
*                                                                       C1R14590
         LA    R8,G11SLDEB     ENTRIES PER RECORD 2311 INPUT            C1R14600
         SH    R8,=AL2(L'SLSTATUS/GXXSLDES)  SKIP DIR CTL RECORD SPACE  C1R14610
         LA    R9,DIR1+L'SLSTATUS  POINT TO FIRST 2311 DIR ENTRY        C1R14620
         USING DIR1SENT,R9     MAKE 2311 DIR ENTRY ADDRESSABLE          C1R14630
         B     SLDENTRY        GO PROCESS FIRST ENTRY                   C1R14640
*                                                                       C1R14650
SLD1NXTR LA    R15,CCW1SDRD    POINT TO 2311 DIR READ CCW               C1R14660
         LA    R1,D1DBCHR      POINT TO 2311 DIR CURSOR                 C1R14670
         BAL   R2,XIO          READ SLB2311 DIR. RECORD                 C1R14680
         LA    R8,G11SLDEB     ENTRIES PER RECORD 2311 INPUT            C1R14690
         LA    R9,DIR1         POINT TO 2311 DIRECTORY AREA             C1R14700
*                                                                       C1R14710
SLDENTRY DS    0H              PROCESS CURRENT 2311 SLD ENTRY           C1R14720
*                                                                       C1R14730
         CLI   DIR1SNAM,ENDDIR END OF DIRECTORY ENTRIES                 C1R14740
         BE    SLDDONE         ..YES, UPDATE DIR AND LIB STATUS REC     C1R14750
         CLI   DIR1SNAM,EMPTYDIR UNUSED ENTRY                           C1R14760
         BE    SLDNEXTE        ..YES, GO PROCESS NEXT ENTRY             C1R14770
*                                                                       C1R14780
* ACTIVE SOURCE STMT DIRECTORY ENTRY.  CREATE THE CORRESPONDING 2314    C1R14790
* DIRECTORY ENTRY, ADJUSTING THE CHR TO REFLECT 2314 TRACK GEOMETRY     C1R14800
*                                                                       C1R14810
         L     R14,SLSDACT     GET CURRENT ACTIVE ENTRY COUNT           C1R14820
         LA    R14,1(,R14)     ANOTHER ACTIVE ENTRY                     C1R14830
         ST    R14,SLSDACT     UPDATE SL STATUS RECORD                  C1R14840
         MVC   DIR4SCEL,DIR1SCEL  COPY SOURCE DIRECTORY ENTRY           C1R14850
*                                                                       C1R14860
         XC    WORKBCHR,WORKBCHR  CLEAR XLATBCHR WORKAREA               C1R14870
         MVC   WORKBCHR+3(1),DIR4SC  MOVE 2311 CYLINDER BYTE            C1R14880
         MVC   WORKBCHR+5(2),DIR4SH  MOVE 2311 HEAD AND REC BYTES       C1R14890
         LA    R0,D4BCHR           OUPUT CURSOR IS 2314 LIB             C1R14900
         LA    R1,D1BCHR           INUT CURSOR IS 2311 LIB              C1R14910
         BAL   R2,XLATBCHR         TRANSLATE 2311 BCHR TO 2314 BCHR     C1R14920
         MVC   DIR4SADR,=X'030506' SELECT LOW BYTE OF CC, HH, AND R     C1R14930
         TR    DIR4SADR,WORKBCHR   PICK RESULT CHR OUT OF BBCCHHR       C1R14940
*                                                                       C1R14950
*  FINISHED WITH CURRENT ENTRY, POINT TO NEXT.  IF END OF 2314 DIR REC, C1R14960
*  WRITE CURRENT AND RESET POINTERS TO NEXT 2314 SLD ENTRY.             C1R14970
*                                                                       C1R14980
         LA    R7,GXXSLDES(,R7)  POINT TO NEXT ENTRY                    C1R14990
         BCT   R6,SLD14NF     DECREMENT, BRANCH IF ENTRIES LEFT         C1R15000
*                                                                       C1R15010
*  2314 RECORD FULL.  WRITE IT AND SET UP FOR NEXT ENTRY.               C1R15020
*                                                                       C1R15030
         LA    R15,CCW4SDFW    POINT TO DIR WRITE CKD CCW               C1R15040
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R15050
         BAL   R2,XIOFW        WRITE CURRENT RLD DIRECTORY              C1R15060
         LA    R1,D4DBCHR      POINT TO DIR SEEK/SEARCH FIELD           C1R15070
         BAL   R2,NXTCHR       UPDATE TO NEXT BBCCHHR                   C1R15080
         LA    R7,IO+8         POINT TO START OF NEW DIRECTORY RECD     C1R15090
         LA    R6,G14SLDEB     ENTRIES PER RECORD 2314 OUTPUT           C1R15100
*                                                                       C1R15110
SLD14NF  DS    0H              2314 DIR ENTRIES ARE AVAILABLE           C1R15120
*                                                                       C1R15130
* 2311 DIRECTORY ENTRY PROCESSED.  PROCESS THE NEXT ONE.                C1R15140
*                                                                       C1R15150
SLDNEXTE DS    0H                                                       C1R15160
         LA    R9,GXXSLDES(,R9) POINT TO NEXT 2311 ENTRY                C1R15170
         BCT   R8,SLDENTRY     PROCESS THAT ENTRY                       C1R15180
*                                                                       C1R15190
* END OF CURRENT DIRECTORY RECORD REACHED.  GET THE NEXT ONE.           C1R15200
*                                                                       C1R15210
         LA    R1,D1DBCHR      POINT TO 2311 DIR CURSOR                 C1R15220
         BAL   R2,NXTCHR       UPDATE CURSOR TO NEXT DIR RECD           C1R15230
         LTR   R15,R15         END OF 2311 DIR EXTENT REACHED           C1R15240
         BZ    SLD1NXTR        ..NO, GO READ NEXT RECORD                C1R15250
*                                                                       C1R15260
* 2311 DIRECTORY COMPLETE.  MAKE NEXT 2314 ENTRY 'END OF DIRECTORY'     C1R15270
* AND WRITE THE FINAL PARTIAL OR FULL DIRECTORY RECORD.  ALSO SET UP    C1R15280
* TO FORMAT THE BALANCE OF THE SOURCE STATEMENT DIRECTORY               C1R15290
*                                                                       C1R15300
SLDDONE  DS    0H                                                       C1R15310
         MVI   DIR4SNAM,ENDDIR INDICATE END OF DIRECTORY                C1R15320
         MVI   DIR4SNAM+1,0    START FILL OF REST OF BLOCK              C1R15330
         MVC   DIR4SNAM+2(G14SLDBS-2),DIR4SNAM+1  ..WITH ZEROS          C1R15340
         LA    R3,D4DBCHR      POINT TO DIR WRITE CURSOR                C1R15350
         LA    R4,CCW4SDFW     POINT TO WRITE DIR CKD CCW               C1R15360
*                                                                       C1R15370
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R15380
         LR    R15,R4          POINT TO WRITE CKD CCW                   C1R15390
         BAL   R2,XIOFW        2314 WRITE WITH COUNT FLD CREATE         C1R15400
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R15410
         BAL   R2,NXTCHR       UPDATE TO NEXT REC/TRK/CYL               C1R15420
*                                                                       C1R15430
         MVI   IO+8,ENDDIR     INDICATE END OF DIRECTORY                C1R15440
         MVI   IO+9,0          ZERO 2ND POSITION OF EMPTY REC           C1R15450
         MVC   IO+10(G14SLDBS-2),IO+9  ZERO REST OF RECORD              C1R15460
*                                                                       C1R15470
*  FORMAT THE REMAINDER OF THE SOURCE STATEMENT DIRECTORY WITH          C1R15480
*  EMPTY RECORDS                                                        C1R15490
*                                                                       C1R15500
SLDFMTLP DS    0H              FORMAT NEXT SOURCE DIR BLOCK             C1R15510
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R15520
         LR    R15,R4          POINT TO WRITE CKD CCW                   C1R15530
         BAL   R2,XIOFW        2314 WRITE WITH COUNT FLD CREATE         C1R15540
         LR    R1,R3           POINT TO WRITE CURSOR                    C1R15550
         BAL   R2,NXTCHR       UPDATE TO NEXT REC/TRK/CYL               C1R15560
         LTR   R15,R15         END OF EXTENT REACHED                    C1R15570
         BZ    SLDFMTLP        ..NO, CONTINUE FORMATTING                C1R15580
*                                                                       C1R15590
SLDFMTDN DS    0H              SOURCE DIR FORMAT COMPLETE               C1R15600
*                                                                       C1R15610
*  READ AND UPDATE THE 2314 SLD DIRECTORY CONTROL RECORD IN THE         C1R15620
*  FIRST DIRECTORY BLOCK.                                               C1R15630
*                                                                       C1R15640
         MVC   D4DBCHR,SLSDRST SET 2314 RL DIR STARTING LOCATION        C1R15650
         LA    R15,CCW4SDRD    POINT TO DIR READ CCW                    C1R15660
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R15670
         BAL   R2,XIO          READ SLD DIRECTORY RECORD ONE            C1R15680
         MVC   DIR4(L'SLSTATUS),SLSTATUS  MOVE UPDATED RL STATUS        C1R15690
         LA    R15,CCW4SDWD    POINT TO DIR WRITE DATA CCW              C1R15700
         LA    R1,D4DBCHR      POINT TO DIRECTORY READ/WRITE CURSOR     C1R15710
         BAL   R2,XIO          WRITE SLD DIRECTORY RECORD ONE           C1R15720
*                                                                       C1R15730
*  SOURCE STATEMENT DIRECTORY PROCESSED, WITH LIBRARY ADDRESSES IN      C1R15740
*  EACH ENTRY UPDATED TO REFLECT 2314 TRACK GEOMETRY.  NOW ALL THAT     C1R15750
*  REMAINS IS A COPY OF THE LIBRARY.  WE'LL JUST COPY BLOCKS UNTIL      C1R15760
*  END OF ACTIVE PORTION OF THE 2311 LIBRARY                            C1R15770
*                                                                       C1R15780
*  COPY SOURCE STATEMENT LIBRARY.  BECAUSE THE BLOCK SIZE DID NOT       C1R15790
*  CHANGE FROM 2311 TO 2314, AND BECAUSE WE UPDATED BBCCHHR FOR         C1R15800
*  EACH DIRECTORY ENTRY AS WE COPIED THE DIRECTORY, ALL WE NEED DO      C1R15810
*  IS COPY BLOCK FOR BLOCK FROM 2311 TO 2314.                           C1R15820
*                                                                       C1R15830
         L     R5,SLSLACT      SET 2314 SLB BLOCK COUNT                 C1R15840
         LA    R3,D4BCHR       POINT R3 AT 2314 OUTPUT CURSOR           C1R15850
         LA    R4,D1BCHR       POINT R4 AT 2311 READ CURSOR             C1R15860
*                                                                       C1R15870
SLBCOPY  DS    0H              COPY ANOTHER SLB BLOCK                   C1R15880
         LA    R15,CCW1SLRD    POINT AT SOURCE LIB READ CCW             C1R15890
         LR    R1,R4           POINT AT 2311 SRC LIB SEEK/SEARCH        C1R15900
         BAL   R2,XIO          READ 2311 SLB BLOCK                      C1R15910
         LA    R15,CCW4SLFW    POINT AT SOURCE LIB WRITE CCW            C1R15920
         LR    R1,R3           POINT AT 2314 SRC LIB SEEK/SEARCH        C1R15930
         BAL   R2,XIOFW        WRITE BLOCK TO 2314                      C1R15940
         LR    R1,R4           POINT AT 2311 SRC LIB SEEK/SEARCH        C1R15950
         BAL   R2,NXTCHR       UPDATE 2311 NEXT READ ADDRESS            C1R15960
         LR    R1,R3           POINT AT 2314 SRC LIB SEEK/SEARCH        C1R15970
         BAL   R2,NXTCHR       UPDATE 2314 NEXT WRITE ADDRESS           C1R15980
         BCT   R5,SLBCOPY      LOOP UNTIL COPY COMPLETE                 C1R15990
*                                                                       C1R16000
*  UPDATE DIRECTORY CONTROL RECORD IN FIRST 80 BYTES OF FIRST           C1R16010
*  DIRECTORY RECORD                                                     C1R16020
*                                                                       C1R16030
         MVC   D4DBCHR,SLSDRST SET 2314 SL DIR STARTING LOCATION        C1R16040
         LA    R15,CCW4SDRD    POINT TO SL DIR READ CCW                 C1R16050
         LA    R1,D4DBCHR      POINT TO DIRECTORY CURSOR                C1R16060
         BAL   R2,XIO          READ SLD DIRECTORY RECORD ONE            C1R16070
*                                                                       C1R16080
         MVC   DIR4(L'SLSTATUS),SLSTATUS  UPDATE SL STATUS RECORD       C1R16090
         LA    R15,CCW4SDWD    POINT TO SL DIR WRITE CCW                C1R16100
         LA    R1,D4DBCHR      POINT TO DIRECTORY CURSOR                C1R16110
         BAL   R2,XIO          WRITE RL UPDATED STATUS RECORD           C1R16120
         DROP  R7,R9           RLD ENTRIES NO LONGER NEED ADRESSING     C1R16130
*                                                                       C1R16140
*  SAVE RECORD COUNTS FOR END OF JOB SUMMARY MESSAGES                   C1R16150
*                                                                       C1R16160
         L     R14,SLSDACT     GET CURRENT ACTIVE ENTRY COUNT           C1R16170
         CVD   R14,WORKCVD     CONVERT TO PACKED DECIMAL                C1R16180
         ZAP   OMSRCS,WORKCVD  SAVE SOURCE BOOK COUNT FOR MESSAGES      C1R16190
         L     R14,SLSLACT     GET CURRENT ACTIVE BLOCK COUNT           C1R16200
         CVD   R14,WORKCVD     CONVERT TO PACKED DECIMAL                C1R16210
         ZAP   OMSRCB,WORKCVD  SAVE SOURCE BLOCK COUNT FOR MESSAGES     C1R16220
*                                                                       C1R16230
         TITLE 'CONVERT 2311 SYSRES TO 2314   FORMAT LABEL CYLINDER'    C1R16240
*********************************************************************** C1R16250
*                                                                       C1R16260
* FORMAT AN EMPTY 2314 LABEL CYLINDER BY WRITING AN EOF RECORD ON       C1R16270
* EACH TRACK IN THE CYLINDER.  EOF RECORD HAS ZERO DATA LENGTH.         C1R16280
* ONLY ONE RECORD PER TRACK, RECORD 1.                                  C1R16290
*                                                                       C1R16300
* REGISTER USAGE:                                                       C1R16310
*    R3 - SAVED POINTER TO 2314 WRITE CURSOR                            C1R16320
*    R4 - SAVED POINTER TO EOF RECORD WRITE CCW                         C1R16330
*    R5 = NR TRACKS LEFT TO FORMAT                                      C1R16340
*    R6 = CURRENT HEAD NR.                                              C1R16350
*                                                                       C1R16360
*********************************************************************** C1R16370
         SPACE 1                                                        C1R16380
         XC    D4BCHR,D4BCHR       ZERO OUT SEEK/SEARCH                 C1R16390
         MVC   D4CYL+1(1),CISLBLCL SET STARTING CYLINDER NR.            C1R16400
         LA    R5,G14TKCYL         FORMAT ENTIRE 2314 CYLINDER          C1R16410
         XR    R6,R6               SET STARTING TRACK NR                C1R16420
         LA    R3,D4BCHR           POINT R3 AT SEEK/SEARCH ADDR         C1R16430
         LA    R4,CCW4LBFW         POINT R4 AT EOF RECORD CCW           C1R16440
*                                                                       C1R16450
* WRITE ONE BLANK RECORD ONE PER TRACK ON TRACKS 5-9                    C1R16460
* R5 = NR TRACKS LEFT TO WRITE, R6 = CURRENT HEAD NR.                   C1R16470
*                                                                       C1R16480
LBCFMTLP DS    0H                  LABEL CYLINDER FORMAT TOP OF LOOP    C1R16490
         STH   R6,D4HEAD           UPDATE HEAD NR.                      C1R16500
         LR    R1,R3               POINT R1 AT WRITE CURSOR             C1R16510
         LR    R15,R4              POINT AT EOF RECORD WRITE CCW        C1R16520
         BAL   R2,XIOFW            WRITE EOF RECORD ON TRACK            C1R16530
         LA    R6,1(,R6)           INCREMENT HEAD NR.                   C1R16540
         BCT   R5,LBCFMTLP         FORMAT ALL 20 TRACKS                 C1R16550
*                                                                       C1R16560
* LABEL CYLINDER FORMATTED.  FALL THROUGH TO TRANSFER THE IPL           C1R16570
* BOOTSTRAP PROGRAM (C0H0 R1 & R2).                                     C1R16580
*                                                                       C1R16590
         TITLE 'CONVERT 2311 SYSRES TO 2314    PROCESSING COMPLETED'    C1R16600
*********************************************************************** C1R16610
*                                                                       C1R16620
* SYSRES BUILT.  CLEAN UP AND QUIT:                                     C1R16630
*                                                                       C1R16640
*  1.  CLOSE ALL FILES                                                  C1R16650
*  2.  COPY THE IPL RECORDS FROM THE 2311 SYSRES TO THE 2314 SYSRES.    C1R16660
*      (NOTE: $$BSYSWR TRANSIENT IS USED TO ENABLE IPL RECORD TRANSFER) C1R16670
*  3.  PRINT PHASE, RELO MODULE, AND SOURCE BOOK PROCESSED COUNTS       C1R16680
*  4.  PRINT EOJ MESSAGE                                                C1R16690
*  5.  RETURN TO THE SHADOWS TO AWAIT ANOTHER SYSRES.                   C1R16700
*                                                                       C1R16710
*********************************************************************** C1R16720
         SPACE 3                                                        C1R16730
         CLOSE RES2311,RES2314,SLB2311   CLOSE FILES TO UPDATE VTOC     C1R16740
*                                                                       C1R16750
* GAIN ACCESS TO TRACK ZERO OF THE NEW SYSRES SO WE CAN WRITE C0H0R1&2  C1R16760
*                                                                       C1R16770
         XR    R0,R0           IGNORE FILE PROTECT                      C1R16780
         LA    R1,=C'$$BSYSWR'                                          C1R16790
         SVC   2                                                        C1R16800
*                                                                       C1R16810
* COPY IPL1 AND IPL2 FROM 2311 TO 2314 CYL 0 HEAD 0.  INITIALIZE DISK   C1R16820
* UTILITY INTD ALREADY CREATED DUMMY IPL RECORDS OF THE RIGHT SIZE,     C1R16830
* SO FORMATTING WRITES ARE NOT NEEDED.  IN  FACT, ONE CANNOT USE        C1R16840
* FORMATTING WRITES (WRITE COUNT-KEY-DATA) BECAUSE DOING SO WILL WIPE   C1R16850
* OUT THE VOL1 LABEL ON THE SYSRES.                                     C1R16860
*                                                                       C1R16870
* THE IPL BOOTSTRAP PROGRAM WORKS ON BOTH 2311 AND 2314 DISKS.  NO      C1R16880
* ADJUSTMENTS ARE NEEDED DURING THE TRANSFER.  AND APPARENTLY           C1R16890
* $$A$IPL2, COPIED AT THE BEGINNING OF THIS PROGRAM, UNDERSTANDS        C1R16900
* HOW TO SEARCH A 2311 AND A 2314 SYSRES FOR THE REST OF THE IPL        C1R16910
* PROGRAMS, THE SUPERVISOR $$A$SUP1, AND JOB CONTROL PHASES.  SO NO     C1R16920
* ALTERATIONS ARE NEEDED THERE EITHER.                                  C1R16930
*                                                                       C1R16940
         XC    D1BCHR,D1BCHR       SET TO READ B0C0H0                   C1R16950
         MVI   D1REC,1             AND RECORD 1 (R2 IS CHAINED)         C1R16960
         LA    R1,D1BCHR           POINT R1 AT 2311 READ CURSOR         C1R16970
         LA    R15,CCW1T0          POINT R15 AT READ R1 & R2 CCWS       C1R16980
         BAL   R2,XIO              READ C0,H0,R1&2                      C1R16990
*                                                                       C1R17000
* WHILE WE CAN READ RECORDS TWO AT A TIME, APPARENTLY WRITING THEM      C1R17010
* TWO AT A TIME DOES NOT WORK AS WELL.  SO TWO WRITES.                  C1R17020
*                                                                       C1R17030
         MVC   D4BCHR,D1BCHR       SET TO WRITE B0C0H0R1                C1R17040
         LA    R1,D4BCHR           POINT R1 AT 2311 READ CURSOR         C1R17050
         LA    R15,CCW4T0R1        POINT R15 AT WRITE R1 CCW            C1R17060
         BAL   R2,XIO              WRITE C0H0R1                         C1R17070
*                                                                       C1R17080
         MVI   D4REC,2             SET TO WRITE B0C0H0R2                C1R17090
         LA    R1,D4BCHR           POINT R1 AT 2311 READ CURSOR         C1R17100
         LA    R15,CCW4T0R2        POINT R15 AT WRITE R2 CCW            C1R17110
         BAL   R2,XIO              WRITE C0H0R2                         C1R17120
*                                                                       C1R17130
* WE'RE DONE.  WE HAVE (WE HOPE) AN IPL'ABLE 2314 SYSRES                C1R17140
*                                                                       C1R17150
         UNPK  MSGPHC1,OMPHASES    MAKE PHASE COUNT PRINTABLE           C1R17160
         OI    MSGPHC1+L'MSGPHC1-1,C'0'  FORCE UNSIGNED RESULT          C1R17170
         LA    R1,MSGPHC           POINT AT PHASE COUNT MESSAGE         C1R17180
         BAL   R2,CONWRITE         WRITE CONSOLE MESSAGE                C1R17190
*                                                                       C1R17200
         UNPK  MSGRLC1,OMRELOS     MAKE RELO MODULE COUNT PRINTABLE     C1R17210
         OI    MSGRLC1+L'MSGRLC1-1,C'0'  FORCE UNSIGNED RESULT          C1R17220
         UNPK  MSGRLC2,OMRELOB     MAKE RELO BLOCK COUNT PRINTABLE      C1R17230
         OI    MSGRLC2+L'MSGRLC2-1,C'0'  FORCE UNSIGNED RESULT          C1R17240
         LA    R1,MSGRLC           POINT AT RELO COUNTS MESSAGE         C1R17250
         BAL   R2,CONWRITE         WRITE CONSOLE MESSAGE                C1R17260
*                                                                       C1R17270
         UNPK  MSGSBC1,OMSRCS      MAKE SRC BOOK COUNT PRINTABLE        C1R17280
         OI    MSGSBC1+L'MSGSBC1-1,C'0'  FORCE UNSIGNED RESULT          C1R17290
         UNPK  MSGSBC2,OMSRCB      MAKE SRC BOOK COUNT PRINTABLE        C1R17300
         OI    MSGSBC2+L'MSGSBC2-1,C'0'  FORCE UNSIGNED RESULT          C1R17310
         LA    R1,MSGSBC           POINT AT SRC BOOK COUNT MESSAGE      C1R17320
         BAL   R2,CONWRITE         WRITE CONSOLE MESSAGE                C1R17330
*                                                                       C1R17340
         LA    R1,MSGEOJ           POINT AT NORMAL EOJ MESSAGE          C1R17350
         BAL   R2,CONWRITE         WRITE CONSOLE MESSAGE                C1R17360
         EOJ                                                            C1R17370
         TITLE 'CONVERT 2311 SYSRES TO 2314    INITAREA ROUTINE'        C1R17380
*********************************************************************** C1R17390
*                                                                       C1R17400
* INITIALIZE AN AREA OF STORAGE.  THE FIRST BYTE OF THE AREA HAS BEEN   C1R17410
* SET BY THE CALLER TO THE VALUE TO BE USED TO CLEAR THE REST OF THE    C1R17420
* AREA.                                                                 C1R17430
*                                                                       C1R17440
* AT ENTRY:                                                             C1R17450
*    R0 CONTAINS THE LENGTH OF THE AREA TO BE INITIALIZED, INCLUDING    C1R17460
*       THE FIRST BYTE OF THE AREA, WHICH HAS BEEN SET BY THE CALLER    C1R17470
*       TO THE VALUE TO BE PROPAGATED THROUGH THE REST OF THE AREA.     C1R17480
*    R1 POINTS AT THE FIRST BYTE OF THE AREA TO BE INITIALIZED, WHICH   C1R17490
*       HAS BEEN SET BY THE CALLER TO THE INITIALIZATION VALUE.         C1R17500
*    R2 CALLER'S RETURN ADDRESS                                         C1R17510
*    FIRST BYTE OF AREA HAS BEEN SET BY THE CALLER TO THE VALUE         C1R17520
*    TO WHICH THE AREA SHOULD BE INITIALIZED.                           C1R17530
*                                                                       C1R17540
* EXAMPLE - TO INITIALIZE THE DIR4 CORE IMAGE DIRECTORY RECORD TO       C1R17550
*           BLANKS                                                      C1R17560
*    R0 SET TO 360, THE LENGTH OF A CORE IMAGE DIRECTORY RECORD         C1R17570
*    R1 SET TO THE ADDRESS OF DIR4                                      C1R17580
*    DIR4 BYTE ONE SET TO C' '                                          C1R17590
*                                                                       C1R17600
* REGISTER USAGE:                                                       C1R17610
*    R0  - AREA LENGTH, NOT SAVED BY THIS ROUTINE                       C1R17620
*    R1  - AREA POINTER, ALSO NOT SAVED BY THIS ROUTINE                 C1R17630
*    R2  - CALLER'S RETURN ADDRESS                                      C1R17640
*    R14 - WORK REGISTER, NOT SAVED NOR RESTORED                        C1R17650
*                                                                       C1R17660
*********************************************************************** C1R17670
         SPACE 1                                                        C1R17680
INITAREA DS    0H                  INITIALIZE AN AREA OF STORAGE        C1R17690
         LR    R14,R1              COPY ADDRESS OF AREA TO INITIALIZE   C1R17700
         BCTR  R0,R2               DECREMENT (INITIAL BYTE ALREADY SET) C1R17710
*                                  ..AND RETURN IF INIT'ING 1-BYTE AREA C1R17720
         SRDL  R0,8                GET COUNT OF 256-BYTE SEGMENTS IN R0 C1R17730
*                                  LENGTH OF LAST SEG IN R1 HIGH BYTE   C1R17740
         LTR   R0,R0               ANY 256-BYTE SEGMENTS                C1R17750
         BNP   INITLAST            ..NO, JUST DO LAST SEGMENT           C1R17760
*                                                                       C1R17770
* CLEAR WHOLE SEGMENTS OF 256 BYTES EACH                                C1R17780
*                                                                       C1R17790
INITLOOP DS    0H                  CLEAR ANOTHER 256-BYTE SEGMENT       C1R17800
         MVC   1(256,R14),0(R14)   PROPAGATE INITIALIZATION VALUE       C1R17810
         LA    R14,256(,R14)       POINT PAST CURRENT SEGMENT           C1R17820
         BCT   R0,INITLOOP         LOOP IF MORE SEGMENTS TO DO          C1R17830
*                                                                       C1R17840
* CLEAR ANY LAST FRACTION OF A 256-BYTE AREA                            C1R17850
*                                                                       C1R17860
INITLAST DS    0H                  WHOLE SEGMENTS DONE, NOW INIT LAST   C1R17870
         SLDL  R0,8                MOVE LENGTH REMAINING TO R0          C1R17880
         BCTR  R0,0                DECREMENT FOR EX OF MVC              C1R17890
         LTR   R0,R0               IF NOW NEGATIVE, NO FRACTION         C1R17900
         BCR   4,R2                ..(BMR) SO RETURN TO CALLER          C1R17910
         EX    R0,INITMVC          CLEAR LAST PORTION                   C1R17920
         BR    R2                  RETURN TO THE CALLER                 C1R17930
*                                                                       C1R17940
* FOLLOWING IS TARGET OF EX (EXECUTE) INSTRUCTION TO INITIALIZE THE     C1R17950
* LAST PORTION OF THE AREA (< 256 BYTES)                                C1R17960
*                                                                       C1R17970
INITMVC   MVC   1(*-*,R14),R14      PROPAGATE INITIALIZATION VALUE      C1R17980
*                                                                       C1R17990
         TITLE 'CONVERT 2311 SYSRES TO 2314    XIO/XOIFW SUBROUTINE'    C1R18000
*********************************************************************** C1R18010
*                                                                       C1R18020
* EXECUTE 2314 CHANNEL PROGRAM AND WAIT FOR COMPLETION.  NO ERROR       C1R18030
* HANDLING AS THIS PROGRAM RUNS IN AN EMULATION ENVIRONMENT.            C1R18040
*                                                                       C1R18050
* ENTRY POINTS                                                          C1R18060
*    XIO - USED FOR READS AND DATA-ONLY (NON-FORMATTING WRITES)         C1R18070
*    XIOFW - USED FOR FORMATTING WRITES OF A SINGLE RECORD.  THIS       C1R18080
*       ROUTINE WILL CONSTRUCT A COUNT FIELD AT THE START OF THE I/O    C1R18090
*       AREA FROM CCW INFORMATION AND THE SEARCH FIELD POINTED TO       C1R18100
*       BY R1.  COUNT FIELD KEYLEN ALWAYS SET TO ZERO FOR THIS.         C1R18110
*                                                                       C1R18120
* AT ENTRY:                                                             C1R18130
*    R1 POINTS AT THE OPERATION'S CURSOR, WHICH INCLUDES THE            C1R18140
*         SEEK/SEARCH BBCCHHR ID AND THE ADDRESS OF THE DTFPH TO BE     C1R18150
*         USED                                                          C1R18160
*    R15 POINTS AT THE CALLER'S CCW(S).  THESE CCWS LOGICALLY FOLLOW    C1R18170
*        THE SEEK/SEARCH/TIC NEEDED TO LOCATE CYL/HEAD/REC FOR THE      C1R18180
*        OPERATION REQUESTED BY THE CALLER                              C1R18190
*    DATA AREA POINTED TO BY ANY/ALL CCWS SET AS NEEDED                 C1R18200
*                                                                       C1R18210
* NOTE:                                                                 C1R18220
*    FOR FORMATTING WRITE OPERATIONS WHEN WRITING MULTIPLE RECORDS      C1R18230
*    (COMMAND-CHAINING FORMATTING WRITE CCW'S), CALLER IS EXPECTED      C1R18240
*    TO PROVIDE COUNT FIELDS AND SHOULD USE THE XIO ENTRY POINT.        C1R18250
*                                                                       C1R18260
* REGISTER USAGE:                                                       C1R18270
*    R0  = WORK REGISTER, NOT SAVED NOR RESTORED (XIOFW ENTRY ONLY)     C1R18280
*    R1  = POINTER TO BBCCHHR FOR READ/WRITE/REWRITE OPERATION          C1R18290
*    R2  = CALLER RETURN ADDRESS                                        C1R18300
*    R14 = WORK REGISTER, NOT SAVED NOR RESTORED (XIOFW ENTRY ONLY)     C1R18310
*    R15 = POINTER TO CALLER'S CCW(S).  AFTER EXCP POINTS TO CCB/DTFPH  C1R18320
*                                                                       C1R18330
*********************************************************************** C1R18340
         SPACE 1                                                        C1R18350
         USING CRSRAD,R1                                                C1R18360
XIOFW    L     R14,0(,R15)         GET ADDRESS OF I/O AREA FROM CCW     C1R18370
         MVC   0(L'CRSRSRCH,R14),CRSRSRCH   MOVE CCHHR TO COUNT FIELD   C1R18380
         XR    R0,R0               ZERO REGISTER R0                     C1R18390
         IC    R0,4(,R14)          GET SEEK RECORD NR.                  C1R18400
         AH    R0,=H'1'            INCREMENT (LA WON'T WORK ON R0)      C1R18410
         STC   R0,4(,R14)          STORE RECORD NR TO BE WRITTEN        C1R18420
         MVI   5(R14),0            ZERO KEY LENGTH IN COUNT FIELD       C1R18430
         LH    R0,6(,R15)          GET CCW DATA LENGTH                  C1R18440
         SH    R0,=H'8'            REDUCE BY LENGTH OF COUNT FIELD      C1R18450
         STH   R0,6(,R14)          STORE COUNT FIELD DATA LENGTH        C1R18460
         SPACE 3                                                        C1R18470
*                                                                       C1R18480
*  FALL THROUGH TO EXECUTE THE CHANNEL PROGRAM                          C1R18490
*                                                                       C1R18500
*  COUNT FIELD BUILT (XIOFW ENTRY) OR NOT NEEDED OR PROVIDED BY CALLER  C1R18510
*  (XIO ENTRY)                                                          C1R18520
*                                                                       C1R18530
XIO      DS    0H                                                       C1R18540
         MVC   CCWSEEK,0(R1)       MOVE I/O OP BBCCHHR FOR SEEK/SEARCH  C1R18550
         ST    R15,CCWTUSER        STORE ADDRESS OF USER CCW STRING     C1R18560
         MVI   CCWTUSER,TIC       RESTORE TIC COMMAND CODE              C1R18570
         L     R1,CRSRDTF          GET ADDRESS OF DTF FOR I/O OPER      C1R18580
         DROP  R1                                                       C1R18590
         EXCP  (1)                 EXECUTE CHANNEL PROGRAM              C1R18600
         WAIT  (1)                 WAIT FOR COMPLETION                  C1R18610
         BR    R2                  RETURN TO CALLER                     C1R18620
         TITLE 'CONVERT 2311 SYSRES TO 2314    INCREMENT BBCCHHR'       C1R18630
*********************************************************************** C1R18640
*                                                                       C1R18650
* INCREMENT SEEK/SEARCH BBCCHHR TO NEXT RECORD, ADJUSTING FOR END OF    C1R18660
* TRACK AND END OF CYLINDER.  NUMBER OF RECORDS PER TRACK AND TRACKS    C1R18670
* PER CYLINDER ARE PROVIDED BY THE CALLER IN THE PARAMETER DATA         C1R18680
* STRUCTURE POINTED TO BY R1.                                           C1R18690
*                                                                       C1R18700
* NOTE THAT SEEK/SEARCH BBCCHHR FOR WRITE COUNT-KEY-DATA IS FOR THE     C1R18710
* RECORD *PRIOR* TO THE ONE BEING WRITTEN, WHILE FOR READ OR WRITE      C1R18720
* JUST DATA (NON-FORMATTING I/O), THE BBCCHHR IS FOR THE RECORD         C1R18730
* BEING READ OR WRITTEN.  THIS AFFECTS END-OF-TRACK LOGIC               C1R18740
*                                                                       C1R18750
* AT ENTRY:                                                             C1R18760
*    R1 POINTS TO DATA STRUCTURE NXTCHRAD.  SEE DSECT NXTCHRAD FOR      C1R18770
*    LAYOUT AND CONTENTS.  THE CURRENT BBCCHHR IS AT THE BEGINNING      C1R18780
*    OF THIS STRUCTURE AND IS UPDATED BY THIS ROUTINE.                  C1R18790
*                                                                       C1R18800
* AT EXIT:                                                              C1R18810
*    BBCCHHR POINTED TO BY R1 UPDATED TO NEXT RECORD, WITH TRACK        C1R18820
*       AND CYL ADJUSTED AS NEEDED.                                     C1R18830
*    RETURN CODE IN R15                                                 C1R18840
*         ZERO - END OF EXTENT HAS NOT BEEN REACHED                     C1R18850
*         4    - NEXT RECORD TO BE WRITTEN EXCEEDS THE EXTENT           C1R18860
*                                                                       C1R18870
* REGISTER USAGE:                                                       C1R18880
*    R0 = WORK REGISTER USED NOT SAVED OR RESTORED BY THIS ROUTINE      C1R18890
*    R1 = SET BY CALLER                                                 C1R18900
*    R2 = CALLER RETURN ADDRESS                                         C1R18910
*    R14 = WORK REGISTER USED NOT SAVED OR RESTORED BY THIS ROUTINE     C1R18920
*    R15 = RETURN CODE REGISTER.  ALSO USED AS WORK REGISTER.           C1R18930
*                                                                       C1R18940
*********************************************************************** C1R18950
         SPACE 1                                                        C1R18960
NXTCHR   DS    0H                                                       C1R18970
         USING CRSRDS,R1           MAKE CALLER'S PARMS ADDRESSABLE      C1R18980
         XR    R15,R15             ZERO RETURN CODE REGISTER            C1R18990
         IC    R14,6(R1)           GET CURRENT RECORD                   C1R19000
         LA    R14,1(,R14)         INCREMENT TO NEXT RECORD             C1R19010
         STC   R14,6(,R1)          STORE UPDATED RECORD                 C1R19020
         CLC   6(1,R1),CRSRRPT+1   AT LAST RECORD ON TRACK NOW          C1R19030
         BCR   4,R2                ..(BLR) NO, RETURN WITH NEW REC      C1R19040
         BH    NXTCHRTK            IF PAST LAST REC, THEN NEW TRACK     C1R19050
*                                                                       C1R19060
*  AT LAST RECORD ON TRACK.  IF A WRITE-CKD OPERATION, THEN THE TRACK   C1R19070
*  IS FULL BECAUSE THE SEARCH IS FOR THE RECORD PRIOR TO THE LAST REC.  C1R19080
*  IF THIS IS A DATA-ONLY OP (READ OF ANY FLAVOR OR WRITE-DATA) THEN    C1R19090
*  WE ARE NOT AT END OF TRACK.                                          C1R19100
*                                                                       C1R19110
         CLI   CRSRFL,CRSRFW       FORMATTING WRITE                     C1R19120
         BCR   7,R2                ..(BNER) NO, RETURN TO THE CALLER    C1R19130
*                                                                       C1R19140
*  NEW TRACK AND MAYBE CYLINDER.                                        C1R19150
*                                                                       C1R19160
NXTCHRTK DS    0H                                                       C1R19170
         MVC   CRSRR,CRSRFL        NEW TRACK, SET FIRST RECORD SEARCH   C1R19180
         LH    R14,CRSRHH          GET TRACK NUMBER                     C1R19190
         LA    R14,1(,R14)         INCREMENT                            C1R19200
         STH   R14,CRSRHH          UPDATE SEARCH FIELD                  C1R19210
         C     R14,CRSRTPC         DID WE PASS LAST TRACK               C1R19220
         BL    NXTCHREX            ..NO, RETURN TO CALLER               C1R19230
*                                                                       C1R19240
* NOTE ABOVE TEST: CURSOR CONTAINS TRACKS PER CYLINDER, EX. 2314 1-20.  C1R19250
* BUT HEAD NUMBERS ARE REL 0, EX 0-19.  SO WHEN WE INCREMENT FROM 19 TO C1R19260
* 20, EQUAL TO TRACKS PER CYL, WE ARE PAST THE END OF THE CYLINDER.     C1R19270
*                                                                       C1R19280
         XC    CRSRHH,CRSRHH       RESET TO HEAD 0 ON NEXT CYL          C1R19290
         LH    R14,CRSRCC          GET TRACK NUMBER                     C1R19300
         LA    R14,1(,R14)         INCREMENT                            C1R19310
         STH   R14,CRSRCC          UPDATE SEARCH FIELD                  C1R19320
*                                                                       C1R19330
NXTCHREX DS    0H                  RETURN UPDATED BBCCHHR TO CALLER     C1R19340
         CLC   CRSREOE,CRSRSK      DID WE PASS END OF EXTENT            C1R19350
         BCR   11,R2               ..(BNLR) NO, RETURN, RC SET          C1R19360
         LA    R15,4               INDICATE END OF EXTENT               C1R19370
         BR    R2                  RETURN TO CALLER                     C1R19380
*                                                                       C1R19390
         DROP  R1                  DROP ADDRESSING OF CALLERS PARMS     C1R19400
         TITLE 'CONVERT 2311 SYSRES TO 2314    XLATE BBCCHHR 11 TO 14'  C1R19410
*********************************************************************** C1R19420
*                                                                       C1R19430
* TRANSLATE A 2311 BBCCHHR TO THE EQUIVALENT LOCATION ON A 2314.        C1R19440
*                                                                       C1R19450
* THIS ROUTINE DOES A TWO-STEP TRANSLATION.  FIRST, THE 2311 BBCCHHR    C1R19460
* IS CONVERTED TO A BLOCK NUMBER RELATIVE TO THE START OF THE LIBRARY   C1R19470
* OR DIRECTORY THAT CONTAINS THE BBCCHHR.  SECOND, THE ROUTINE          C1R19480
* CONVERTS THE BLOCK NUMBER BACK TO AN ABSOLUTE BBCCHHR ON A 2314.      C1R19490
*                                                                       C1R19500
* THE INPUT CURSOR PROVIDES THE DEVICE TRACK GEOMETRY, RECORDS PER      C1R19510
* TRACK, AND DIRECTORY OR LIBRARY ABSOLUTE STARTING TRACK FOR THE       C1R19520
* INPUT BBCCHHR.  THE OUTPUT CURSOR PROVIDES THE SAME INFORMATION FOR   C1R19530
* THE TARGET 2314 LIBRARY OR DIRECTORY.  USE OF CURSOR DATA ELIMINATES  C1R19540
* ANY NEED FOR THIS ROUTINE TO KNOW WHAT KIND OF LIBRARY OR DIRECTORY   C1R19550
* IS BEING PROCESSED.                                                   C1R19560
*                                                                       C1R19570
* AT ENTRY:                                                             C1R19580
*    FIELD WORKBCHR CONTAINS THE INPUT BBCCHHR                          C1R19590
*    R0 POINTS TO THE OUTPUT CURSOR                                     C1R19600
*    R1 POINTS TO THE INPUT CURSOR                                      C1R19610
*    R2 CALLER'S RETURN ADDRESS                                         C1R19620
*                                                                       C1R19630
* AT EXIT:                                                              C1R19640
*    FIELD WORKBCHR CONTAINS THE OUTPUT BBCCHHR                         C1R19650
*    R0 POINTS TO THE OUTPUT CURSOR                                     C1R19660
*    R1 POINTS TO THE OUTPUT CURSOR (NOTE CHANGE IN R1 CONTENTS)        C1R19670
*    R2 CALLER'S RETURN ADDRESS                                         C1R19680
*                                                                       C1R19690
* REGISTER USAGE:                                                       C1R19700
*    R0 = POINTER TO OUTPUT CURSOR                                      C1R19710
*    R1 = POINTER TO INPUT CURSOR, THEN TO OUTPUT CURSOR                C1R19720
*    R2 = CALLER RETURN ADDRESS                                         C1R19730
*    R14 = WORK REGISTER USED NOT SAVED OR RESTORED BY THIS ROUTINE     C1R19740
*    R15 = WORK REGISTER USED NOT SAVED OR RESTORED BY THIS ROUTINE     C1R19750
*                                                                       C1R19760
*********************************************************************** C1R19770
         SPACE 3                                                        C1R19780
XLATBCHR DS    0H                  TRANSLATE A DISK ADDR 2311 TO 2314   C1R19790
         USING CRSRAD,R1           R1 POINTS TO INPUT CURSOR            C1R19800
         LH    R14,WORKCC          GET CYLINDER ADDR                    C1R19810
         MH    R14,CRSRTPCH        CONVERT TO TRACKS                    C1R19820
         AH    R14,WORKHH          ADD HEAD NUMBER TO TRACKS            C1R19830
         SH    R14,CRSRSTK         SUBTRACT EXTENT STARTING TRACK       C1R19840
         MH    R14,CRSRRPT         CONVERT REL TRACK TO RELATIVE BLOCK  C1R19850
         XR    R15,R15             ZERO R15 FOR IC OF BLOCK NR          C1R19860
         IC    R15,WORKREC         GET BLOCK NR ON TRACK                C1R19870
         AR    R14,R15             CONVERT TO RELATIVE BLOCK IN EXTENT  C1R19880
         BCTR  R14,0               CONVERT BLOCK NR TO REL ZERO         C1R19890
*                                                                       C1R19900
         LR    R1,R0               POINT R1 AT OUTPUT CURSOR            C1R19910
*                                                                       C1R19920
         SRDL  R14,32              PREPARE REL BLOCK ADDR FOR DIVISION  C1R19930
         LH    R0,CRSRRPT          GET BLOCKS/TRACK INTO R0             C1R19940
         DR    R14,R0              DIVIDE BY BLOCKS/TRACK               C1R19950
         LA    R14,1(,R14)         REMAINDER IS REC NR, MAKE REL 1      C1R19960
         STC   R14,WORKREC         STORE RECORD NUMBER                  C1R19970
         AH    R15,CRSRSTK         ADD EXTENT START TO REL TRK          C1R19980
         XR    R14,R14             PREPARE FOR NEXT DIV                 C1R19990
         D     R14,CRSRTPC         CONVERT ABSOLUTE TRACK TO CCHH       C1R20000
         STH   R14,WORKHH          STORE REMAINDER AS HEAD NR           C1R20010
         STH   R15,WORKCC          STORE QUOTIENT AS CYL NR             C1R20020
         BR    R2                  RETURN TO THE CALLER                 C1R20030
*                                                                       C1R20040
         DROP  R1                  DROP CURSOR ADDRESSING               C1R20050
*                                                                       C1R20060
         TITLE 'CONVERT 2311 SYSRES TO 2314    CONSOLE I/O && MESSAGES' C1R20070
*********************************************************************** C1R20080
*                                                                       C1R20090
* CONSOLE I/O CONTROL BLOCKS AND MESSAGES                               C1R20100
*                                                                       C1R20110
*********************************************************************** C1R20120
CONWRITE MVC   CONCCW+6(2),0(R1)   MOVE MESSAGE LENGTH INTO CCW         C1R20130
         LA    R1,2(,R1)            POINT PAST MESSAGE LENGTH           C1R20140
         ST    R1,CONCCW           STORE MESSAGE ADDRESS                C1R20150
         MVI   CONCCW,CONCCWWR     RESTORE COMMAND CODE TO CCW          C1R20160
         EXCP  CONSOLE             WRITE CONSOLE MESSAGE                C1R20170
         WAIT  (1)                 WAIT FOR MESSAGE COMPLETION          C1R20180
         BR    R2                                                       C1R20190
*                                                                       C1R20200
* COUNTERS TO ACCUMULATE OPERATIONAL METRICS                            C1R20210
*                                                                       C1R20220
         DS   0D                   FORCE ALIGNMENT                      C1R20230
WORKCVD  DC   PL8'0'               WORKAREA FOR CONVERT TO BINARY       C1R20240
OMPHASES DC   PL3'0'               COUNT OF PHASES PROCESSED            C1R20250
OMRELOS  DC   PL3'0'               COUNT OF RELO MODULES PROCESSED      C1R20260
OMRELOB  DC   PL3'0'               COUNT OF RELO BLOCKS PROCESSED       C1R20270
OMSRCS   DC   PL3'0'               COUNT OF SOURCE BOOKS PROCESSED      C1R20280
OMSRCB   DC   PL3'0'               COUNT OF SOURCE BLOCKS PROCESSED     C1R20290
*                                                                       C1R20300
         DS    0H                  PHASE COUNT MESSAGE TEXT             C1R20310
MSGPHC   DC    AL2(MSGPHCLN)                                            C1R20320
MSGPHCTX DC    C'CVT11RES '                                             C1R20330
MSGPHC1  DC    C'00000'                                                 C1R20340
         DC    C' PHASES'                                               C1R20350
MSGPHCLN EQU   *-MSGPHCTX                                               C1R20360
*                                                                       C1R20370
         DS    0H                  RELO MODULE COUNT MESSAGE TEXT       C1R20380
MSGRLC   DC    AL2(MSGRLCLN)                                            C1R20390
MSGRLCTX DC    C'CVT11RES '                                             C1R20400
MSGRLC1  DC    C'00000'                                                 C1R20410
         DC    C' RELO MODULES, '                                       C1R20420
MSGRLC2  DC    C'00000'                                                 C1R20430
         DC    C' RELO BLOCKS'                                          C1R20440
MSGRLCLN EQU   *-MSGRLCTX                                               C1R20450
*                                                                       C1R20460
         DS    0H                  SOURCE BOOK COUNT MESSAGE TEXT       C1R20470
MSGSBC   DC    AL2(MSGSBCLN)                                            C1R20480
MSGSBCTX DC    C'CVT11RES '                                             C1R20490
MSGSBC1  DC    C'00000'                                                 C1R20500
         DC    C' SOURCE BOOKS, '                                       C1R20510
MSGSBC2  DC    C'00000'                                                 C1R20520
         DC    C' SOURCE BLOCKS'                                        C1R20530
MSGSBCLN EQU   *-MSGSBCTX                                               C1R20540
*                                                                       C1R20550
*COUNT    DC    XL3'00'  **************************                     C1R20560
*                                                                       C1R20570
         DS    0H                  NORMAL END OF JOB MESSAGE            C1R20580
MSGEOJ   DC    AL2(MSGEOJLN)                                            C1R20590
MSGEOJTX DC    C'CVT11RES NORMAL EOJ '                                  C1R20600
MSGEOJLN EQU   *-MSGEOJTX                                               C1R20610
*                                                                       C1R20620
* CONSOLE CCB AND CCW                                                   C1R20630
*                                                                       C1R20640
CONSOLE  CCB   SYSLOG,CONCCW       WRITE ON OPERATOR CONSOLE            C1R20650
         SPACE 3                                                        C1R20660
CONCCWWR EQU   X'09'               CCW COMMAND CODE CONSOLE WRITE       C1R20670
CONCCW   CCW   CONCCWWR,CONMSG,X'00',L'CONMSG                           C1R20680
*                                                                       C1R20690
CONMSG   DC    C'CVT11RES  **SNO** '  THIS MESSAGE SHOULD NOT OCCUR     C1R20700
*                                                                       C1R20710
*                                                                       C1R20720
         TITLE 'CONVERT 2311 SYSRES TO 2314   LITERAL POOL'             C1R20730
*                                                                       C1R20740
******************* NO CODE PAST THIS POINT  *************************  C1R20750
*                                                                       C1R20760
         LTORG                                                          C1R20770
*                                                                       C1R20780
CODEEND  DS    0X                                                       C1R20790
         TITLE 'CONVERT 2311 SYSRES TO 2314   DIRECTORY STATUS RECORDS' C1R20800
*********************************************************************** C1R20810
*                                                                       C1R20820
* CONSTANTS IN THE THREE STATUS RECORDS BELOW REFLECT THE FIXED         C1R20830
* ALLOCATION FOR THE SYSRES BUILT BY THIS PROGRAM, WHICH ARE SET IN     C1R20840
* EQUATE STATEMENTS AT THE BEGINNING OF THE PROGRAM.                    C1R20850
*                                                                       C1R20860
* IN ADDITION TO BEING THE REFERENCE FOR THE RESPECTIVE LIBRARY         C1R20870
* ALLOCATIONS, THESE RECORDS ARE UPDATED AS PHASES, RELOCATABLE         C1R20880
* MODULES, AND SOURCE BOOKS ARE WRITTEN TO THE NEW SYSRES. AS EACH      C1R20890
* LIBRARY IS COPIED, THE APPROPRIATE LIBRARY STATUS RECORD IS UPDATED   C1R20900
* ON THE NEW SYSRES.                                                    C1R20910
*                                                                       C1R20920
*********************************************************************** C1R20930
         SPACE 3                                                        C1R20940
*********************************************************************** C1R20950
*                                                                       C1R20960
*  CORE IMAGE LIBRARY DIRECTORY STATUS RECORD                           C1R20970
*                                                                       C1R20980
*********************************************************************** C1R20990
         SPACE 1                                                        C1R21000
         DS    0D                  CORE IMAGE DIR/LIB STATUS RECORD     C1R21010
         DC    CL8'**CIST**'       EYECATCHER                           C1R21020
CISTATUS DS    0CL80                                                    C1R21030
*  CORE IMAGE DIRECTORY (CID) STATUS: START, NEXT, LAST                 C1R21040
*        DC    X'0000 0000 000A 01'    CID START BBCCHHR                C1R21050
CISDRST  DS    0CL7                CID STARTING BBCCHHRE                C1R21060
CISDRSTB DC    AL2(0)              ..CID STARTING BIN                   C1R21070
CISDRSTC DC    AL2(0)              ..CID STARTING CYL                   C1R21080
CISDRSTH DC    AL2(10)             ..CID STARTING HEAD                  C1R21090
CISDRSTR DC    AL1(1)              ..CID STARTING RECORD                C1R21100
*                                                                       C1R21110
CISDIR   DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R21120
CISDRNX  DS    0CL8                CID NEXT BBCCHHRE                    C1R21130
CISDRNXB DC    AL2(0)              ..CID NEXT AVAIL BIN                 C1R21140
CISDRNXC DC    AL2(0)              ..CID NEXT AVAIL CYL                 C1R21150
CISDRNXH DC    AL2(10)             ..CID NEXT AVAIL HEAD                C1R21160
CISDRNXR DC    AL1(1)              ..CID NEXT AVAIL RECORD              C1R21170
CISDRNXE DC    AL1(0)              ..CID NEXT AVAIL ENTRY               C1R21180
*                                                                       C1R21190
CISDRLS  DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R21200
***      DC    X'0000 0000 0013 11 11' LAST BBCCHHRE IN DIR             C1R21210
CISDRLSB DC    AL2(0)              ..CID ENDING BIN                     C1R21220
CISDRLSC DC    AL2(0)              ..CID ENDING CYL                     C1R21230
CISDRLSH DC    AL2(10+R14CIDTK-1)  ..CID ENDING HEAD                    C1R21240
CISDRLSR DC    AL1(G14CIDBT)       ..CID ENDING RECORD                  C1R21250
CISDRLSE DC    AL1(G14CIDBS/GXXCIDES-1)  ..CID ENDING ENTRY (REL 0)     C1R21260
*                                                                       C1R21270
*  CORE IMAGE LIBRARY (CIL) STATUS: START, NEXT, LAST                   C1R21280
*                                                                       C1R21290
***      DC    X'0000 0001 0000 01'   STARTING BBCCHHR IN CIL           C1R21300
CISLBST  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R21310
CISLBSTB DC    AL2(0)              ..CIL STARTING BIN                   C1R21320
CISLBSTC DC    AL2(1)              ..CIL STARTING CYL                   C1R21330
CISLBSTH DC    AL2(0)              ..CIL STARTING HEAD                  C1R21340
CISLBSTR DC    AL1(1)              ..CIL STARTING RECORD                C1R21350
*                                                                       C1R21360
***      DC    X'0000 0001 0000 01'  NEXT BBCCHHR IN CIL                C1R21370
CISLIB   DS    0CL7                                                     C1R21380
CISLBNX  DS    0CL7                HWORDS ARE ALIGNED IN THIS ONE       C1R21390
CISLBNXB DC    AL2(0)              ..CIL NEXT AVAIL BIN                 C1R21400
CISLBNXC DC    AL2(1)              ..CIL NEXT AVAIL CYL                 C1R21410
CISLBNXH DC    AL2(0)              ..CIL NEXT AVAIL HEAD                C1R21420
CISLBNXR DC    AL1(1)              ..CIL NEXT AVAIL RECORD              C1R21430
*                                                                       C1R21440
***      DC    X'0000 0017 0013 04'  ENDING BBCCHHR IN CIL              C1R21450
*                                  CIL ALWAYS ENDS ON CYL BOUNDARY      C1R21460
CISLBLS DS     0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R21470
CISLBLSB DC    AL2(0)              ..CIL LAST AVAIL BIN                 C1R21480
CISLBLSC DC    AL2(R14CILCL-1)     ..CIL LAST AVAIL CYL                 C1R21490
CISLBLSH DC    AL2(G14LSTTK)       ..CIL LAST AVAIL HEAD                C1R21500
CISLBLSR DC    AL1(G14CILBT)       ..CIL LAST AVAIL RECORD              C1R21510
*                                                                       C1R21520
*  LIBRARY BLOCK COUNTS                                                 C1R21530
CISDACT  DC    A(0)               COUNT OF ACTIVE DIRECTORY ENTRIES     C1R21540
CISLLEN  DC    A(R14CILTB)        TOTAL BLKS IN LIBRARY                 C1R21550
CISLACT  DC    A(0)               COUNT OF ACTIVE BLOCKS IN LIBRARY     C1R21560
CISLDEL  DC    A(0)               COUNT OF LIBRARY DELETED BLOCKS       C1R21570
CISLLEFT DC    A(R14CILTB)        BLOCKS AVAILABLE AFTER LAST USED      C1R21580
         DC    Y(0)               AUTO CONDENSE LIMIT, 0=NO AUTO        C1R21590
*                                                                       C1R21600
CISLCYLS DC    Y(R14CILCL)        CYLINDERS FOR DIR AND LIB             C1R21610
*                                                                       C1R21620
CISDTRKS DC    Y(R14CIDTK)        DIRECTORY TRACKS                      C1R21630
         DC    XL9'0'             RESERVED                              C1R21640
CISLBLCL DC    AL1(R14LBLCL)      CIL ONLY: ADDRESS OF LABEL CYL        C1R21650
*                                 SET TO 0 FOR ALL OTHER LIBS           C1R21660
*                                                                       C1R21670
         SPACE 3                                                        C1R21680
*********************************************************************** C1R21690
*                                                                       C1R21700
*  RELOCATABLE MODULE LIBRARY DIRECTORY STATUS RECORD                   C1R21710
*                                                                       C1R21720
*********************************************************************** C1R21730
         SPACE 1                                                        C1R21740
         DS    0D             RELOCATABLE MODULE DIR/LIB STATUS RECORD  C1R21750
         DC    CL8'**RLST**'       EYECATCHER                           C1R21760
RLSTATUS DS    0CL80                                                    C1R21770
*                                                                       C1R21780
*  RELOCATABLE MODULE DIRECTORY (RLD) STATUS: START, NEXT, LAST         C1R21790
*                                                                       C1R21800
*        DC    X'0000 0000 000A 01'    RLD START BBCCHHR                C1R21810
RLSDRST  DS    0CL7                RLD STARTING BBCCHHRE                C1R21820
RLSDRSTB DC    AL2(0)              ..RLD STARTING BIN                   C1R21830
RLSDRSTC DC    AL2(R14CILCL)       ..RLD STARTING CYL (END OF CIL)      C1R21840
RLSDRSTH DC    AL2(0)              ..RLD STARTING HEAD                  C1R21850
RLSDRSTR DC    AL1(1)              ..RLD STARTING RECORD                C1R21860
*                                                                       C1R21870
RLSDRNX  DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R21880
*        DC    X'0000 0038 0000 01'  RLD START BBCCHHR                  C1R21890
RLSDRNXB DC    AL2(0)              ..RLD NEXT AVAIL BIN                 C1R21900
RLSDRNXC DC    AL2(R14CILCL)       ..RLD NEXT AVAIL CYL                 C1R21910
RLSDRNXH DC    AL2(0)              ..RLD NEXT AVAIL HEAD                C1R21920
RLSDRNXR DC    AL1(1)              ..RLD NEXT AVAIL RECORD              C1R21930
RLSDRNXE DC    AL1(L'RLSTATUS/GXXRLDES) ..RLD NEXT AVAIL ENTRY          C1R21940
*                                                                       C1R21950
RLSDRLS  DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R21960
***      DC    X'0000 0038 0009 11 13' LAST BBCCHHRE IN DIR             C1R21970
RLSDRLSB DC    AL2(0)              ..RLD ENDING BIN                     C1R21980
RLSDRLSC DC    AL2(R14CILCL)       ..RLD ENDING CYL                     C1R21990
RLSDRLSH DC    AL2(R14RLDTK-1)     ..RLD ENDING HEAD                    C1R22000
RLSDRLSR DC    AL1(G14RLDBT)       ..RLD ENDING RECORD                  C1R22010
RLSDRLSE DC    AL1(G14RLDBS/GXXRLDES-1)  ..RLD ENDING ENTRY (REL 0)     C1R22020
*                                                                       C1R22030
*  RELOCATABLE MODULE LIBRARY (RLB) STATUS: START, NEXT, LAST           C1R22040
*                                                                       C1R22050
***      DC    X'0000 0038 000A 01'   STARTING BBCCHHR IN RLB           C1R22060
RLSLBST  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R22070
RLSLBSTB DC    AL2(0)              ..RLB STARTING BIN                   C1R22080
RLSLBSTC DC    AL2(R14CILCL)       ..RLB STARTING CYL                   C1R22090
RLSLBSTH DC    AL2(R14RLDTK)       ..RLB STARTING HEAD                  C1R22100
RLSLBSTR DC    AL1(1)              ..RLB STARTING RECORD                C1R22110
*                                                                       C1R22120
***      DC    X'0000 0038 000A 01'  NEXT BBCCHHR IN RLB                C1R22130
RLSLBNX  DS    0CL7                HWORDS ARE ALIGNED IN THIS ONE       C1R22140
RLSLBNXB DC    AL2(0)              ..RLB NEXT AVAIL BIN                 C1R22150
RLSLBNXC DC    AL2(R14CILCL)       ..RLB NEXT AVAIL CYL                 C1R22160
RLSLBNXH DC    AL2(R14RLDTK)       ..RLB NEXT AVAIL HEAD                C1R22170
RLSLBNXR DC    AL1(1)              ..RLB NEXT AVAIL RECORD              C1R22180
*                                                                       C1R22190
***      DC    X'0000 0084 0013 10'  ENDING BBCCHHR IN RLB              C1R22200
*                                  RLB ALWAYS ENDS ON CYL BOUNDARY      C1R22210
RLSLBLS  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R22220
RLSLBLSB DC    AL2(0)              ..RLB LAST AVAIL BIN                 C1R22230
RLSLBLSC DC    AL2(R14CILCL+R14RLBCL-1) ..RLB LAST AVAIL CYL            C1R22240
RLSLBLSH DC    AL2(G14LSTTK)       ..RLB LAST AVAIL HEAD                C1R22250
RLSLBLSR DC    AL1(G14RLBBT)       ..RLB LAST AVAIL RECORD              C1R22260
*                                                                       C1R22270
*  LIBRARY BLOCK COUNTS                                                 C1R22280
RLSDACT  DC    A(0)               COUNT OF ACTIVE DIRECTORY ENTRIES     C1R22290
RLSLLEN  DC    A(R14RLBTB)        TOTAL BLOCKS IN LIBRARY               C1R22300
RLSLACT  DC    A(0)               COUNT OF ACTIVE BLOCKS IN LIBRARY     C1R22310
RLSLDEL  DC    A(0)               COUNT OF LIBRARY DELETED BLOCKS       C1R22320
RLSLLEFT DC    A(R14RLBTB)        BLOCKS AVAILABLE AFTER LAST USED      C1R22330
         DC    Y(0)               AUTO CONDENSE LIMIT, 0=NO AUTO        C1R22340
*                                                                       C1R22350
RLSLCYLS DC    Y(R14RLBCL)        CYLINDERS FOR DIR AND LIB             C1R22360
*                                                                       C1R22370
RLSDTRKS DC    Y(R14RLDTK)        DIRECTORY TRACKS                      C1R22380
         DC    XL9'0'             RESERVED                              C1R22390
         DC    AL1(0)             CIL ONLY: ADDRESS OF LABEL CYL        C1R22400
*                                 SET TO 0 FOR ALL OTHER LIBS           C1R22410
         SPACE 3                                                        C1R22420
*********************************************************************** C1R22430
*                                                                       C1R22440
*  SOURCE STATEMENT LIBRARY DIRECTORY STATUS RECORD                     C1R22450
*                                                                       C1R22460
*********************************************************************** C1R22470
         SPACE 1                                                        C1R22480
         DS    0D            SOURCE STATEMENT LIBRARY STATUS RECORD     C1R22490
         DC    CL8'**SLST**'       EYECATCHER                           C1R22500
SLSTATUS DS    0CL80                                                    C1R22510
*  SOURCE STATEMENT DIRECTORY (SLD) STATUS: START, NEXT, LAST           C1R22520
*        DC    X'0000 0000 000A 01'    SLD START BBCCHHR                C1R22530
SLSDRST  DS    0CL7                SLD STARTING BBCCHHRE                C1R22540
SLSDRSTB DC    AL2(0)              ..SLD STARTING BIN                   C1R22550
SLSDRSTC DC    AL2(R14CILCL+R14RLBCL)  ..SLD STARTING CYL               C1R22560
SLSDRSTH DC    AL2(0)              ..SLD STARTING HEAD                  C1R22570
SLSDRSTR DC    AL1(1)              ..SLD STARTING RECORD                C1R22580
*                                                                       C1R22590
*                                  NOTE: HWORDS BELOW NOT ALIGNED       C1R22600
SLSDRNX  DS    0CL8                SLD NEXT BBCCHHRE                    C1R22610
SLSDRNXB DC    AL2(0)              ..SLD NEXT AVAIL BIN                 C1R22620
SLSDRNXC DC    AL2(R14CILCL+R14RLBCL) ..SLD NEXT AVAIL CYL              C1R22630
SLSDRNXH DC    AL2(0)              ..SLD NEXT AVAIL HEAD                C1R22640
SLSDRNXR DC    AL1(1)              ..SLD NEXT AVAIL RECORD              C1R22650
SLSDRNXE DC    AL1(L'SLSTATUS/GXXSLDES)   ..SLD NEXT AVAIL ENTRY        C1R22660
*                                                                       C1R22670
SLSDSLS  DS    0CL8                NOTE: HWORDS BELOW NOT ALIGNED       C1R22680
***      DC    X'0000 0000 0013 0F 11' LAST BBCCHHRE IN DIR             C1R22690
SLSDSLSB DC    AL2(0)              ..SLD ENDING BIN                     C1R22700
SLSDSLSC DC    AL2(R14CILCL+R14RLBCL) ..SLD ENDING CYL                  C1R22710
SLSDSLSH DC    AL2(R14SLDTK-1)     ..SLD ENDING HEAD                    C1R22720
SLSDSLSR DC    AL1(G14SLDBT)       ..SLD ENDING RECORD                  C1R22730
SLSDSLSE DC    AL1(G14SLDBS/GXXSLDES-1)  ..SLD ENDING ENTRY (REL 0)     C1R22740
*                                                                       C1R22750
*  SOURCE STATEMENT LIBRARY (SLB) STATUS: START, NEXT, LAST             C1R22760
*                                                                       C1R22770
***      DC    X'0000 0001 0000 01'   STARTING BBCCHHR IN SLB           C1R22780
SLSLBST  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R22790
SLSLBSTB DC    AL2(0)              ..SLB STARTING BIN                   C1R22800
SLSLBSTC DC    AL2(R14CILCL+R14RLBCL) ..SLB STARTING CYL                C1R22810
SLSLBSTH DC    AL2(R14SLDTK)       ..SLB STARTING HEAD                  C1R22820
SLSLBSTR DC    AL1(1)              ..SLB STARTING RECORD                C1R22830
*                                                                       C1R22840
***      DC    X'0000 0001 0000 01'  NEXT BBCCHHR IN SLB                C1R22850
SLSLBNX  DS    0CL7                HWORDS ARE ALIGNED IN THIS ONE       C1R22860
SLSLBNXB DC    AL2(0)              ..SLB NEXT AVAIL BIN                 C1R22870
SLSLBNXC DC    AL2(R14CILCL+R14RLBCL)  ..SLB NEXT AVAIL CYL             C1R22880
SLSLBNXH DC    AL2(R14SLDTK)       ..SLB NEXT AVAIL HEAD                C1R22890
SLSLBNXR DC    AL1(1)              ..SLB NEXT AVAIL RECORD              C1R22900
*                                                                       C1R22910
***      DC    X'0000 0017 0013 04'  ENDING BBCCHHR IN SLB              C1R22920
*                                  SLB ALWAYS ENDS ON CYL BOUNDARY      C1R22930
SLSLBLS  DS    0CL7                NOTE: HWORDS BELOW NOT ALIGNED       C1R22940
SLSLBLSB DC    AL2(0)              ..SLB LAST AVAIL BIN                 C1R22950
SLSLBLSC DC    AL2(R14CILCL+R14RLBCL+R14SLBCL-1) SLB LAST AVAIL CYL     C1R22960
SLSLBLSH DC    AL2(G14LSTTK)       ..SLB LAST AVAIL HEAD                C1R22970
SLSLBLSR DC    AL1(G14SLBBT)       ..SLB LAST AVAIL RECORD              C1R22980
*                                                                       C1R22990
*  LIBRARY BLOCK COUNTS                                                 C1R23000
SLSDACT  DC    A(0)               COUNT OF ACTIVE DIRECTORY ENTRIES     C1R23010
SLSLLEN  DC    A(R14SLBTB)        TOTAL BLOCKS IN LIBRARY               C1R23020
SLSLACT  DC    A(0)               COUNT OF ACTIVE BLOCKS IN LIBRARY     C1R23030
SLSLDEL  DC    A(0)               COUNT OF LIBRARY DELETED BLOCKS       C1R23040
SLSLLEFT DC    A(R14SLBTB)        BLOCKS AVAILABLE AFTER LAST USED      C1R23050
         DC    Y(0)               AUTO CONDENSE LIMIT, 0=NO AUTO        C1R23060
*                                                                       C1R23070
SLSLCYLS DC    Y(R14SLBCL)        CYLINDERS FOR DIR AND LIB             C1R23080
*                                                                       C1R23090
SLSDTRKS DC    Y(R14SLDTK)        DIRECTORY TRACKS                      C1R23100
         DC    XL9'0'             RESERVED                              C1R23110
         DC    AL1(0)             CIL ONLY: ADDRESS OF LABEL CYL        C1R23120
*                                 SET TO 0 FOR ALL OTHER LIBS*          C1R23130
*                                                                       C1R23140
* IN A NUMBER OF PLACES IN THE LIBRARY STATUS RECORD AND IN RELO        C1R23150
* DIRECTORY ENTRYS, THE CYLINDER AND TRACK NUMBERS ARE NOT ALIGNED.     C1R23160
* IF ALIGNMENT IS REQUIRED, THE CODE USES THE FOLLOWING FIELD           C1R23170
*                                                                       C1R23180
         DS    0H                 FORCE ALIGNMENT                       C1R23190
WORKHW   DS    H                  HALFWORD FOR ALIGNMENT                C1R23200
         TITLE 'CONVERT 2311 SYSRES TO 2314   READ/WRITE CURSOR BLOCKS' C1R23210
*********************************************************************** C1R23220
*                                                                       C1R23230
*  CURSORS FOR I/O TO 2311 AND 2314 LIBRARIES AND DIRECTORIES.          C1R23240
*                                                                       C1R23250
*  LAYOUT OF EACH MUST MATCH THAT OF CRSRDS DSECT, ABOVE.               C1R23260
*                                                                       C1R23270
*********************************************************************** C1R23280
         SPACE 1                                                        C1R23290
*                                                                       C1R23300
* CURSOR FOR 2311 LIBRARY OPERATIONS.  THE SAME CURSOR IS USED FOR      C1R23310
* RES2311 AND SLB2311.                                                  C1R23320
*                                                                       C1R23330
         DS    0D                  FORCE DOUBLEWORD ALIGNMENT           C1R23340
         DC    CL8'*11*LIB*'       DUMP EYECATCHER                      C1R23350
D1BCHR   DS    0CL7                2311 SEEK/SEARCH TARGET              C1R23360
D1BIN    DC    XL2'0'              DATA CELL BIN NR.                    C1R23370
D1CYL    DC    XL2'0'              CYLINDER NR.                         C1R23380
D1HEAD   DC    XL2'1'              HEAD NR.                             C1R23390
D1REC    DC    X'0'                RECORD NR.                           C1R23400
D1FLAG   DC    X'0'                FLAG FOR NXTCHR, 0 = DATA OPS        C1R23410
D1DTF    DC    A(RES2311)          POINTER TO DTFPH                     C1R23420
D1TPC    DC    A(G11TKCYL)         2311 TRACKS PER CYLINDER             C1R23430
         ORG   D1TPC               BACK TO START OF TPC                 C1R23440
         DS    H                   FILLER - UPPER HW OF TPC             C1R23450
D1TPCH   DS    H                   LOW HALFWORD OF TPC                  C1R23460
D1SOE    DS    0CL6                START OF EXTENT                      C1R23470
D1SOEBB  DC    X'FFFF'             ..BIN                                C1R23480
D1SOECC  DC    X'0000'             ..CYLINDER                           C1R23490
D1SOEHH  DC    X'0000'             ..HEAD (OR TRACK)                    C1R23500
D1EOE    DS    0CL6                END OF EXTENT                        C1R23510
D1EOEBB  DC    X'FFFF'             ..BIN                                C1R23520
D1EOECC  DC    X'0000'             ..CYLINDER                           C1R23530
D1EOEHH  DC    X'0000'             ..HEAD (OR TRACK)                    C1R23540
D1STK    DC    H'0'                STARTING TRACK OF LIBRARY            C1R23550
D1RPT    DC    H'0'                RECORDS PER TRACK                    C1R23560
         SPACE 3                                                        C1R23570
*                                                                       C1R23580
* CURSOR FOR 2311 DIRECTORY OPERATIONS.  THE SAME CURSOR IS USED FOR    C1R23590
* RES2311 AND SLB2311.                                                  C1R23600
*                                                                       C1R23610
         DS    0D                  FORCE HALFWORD ALIGNMENT             C1R23620
         DC    CL8'*11*DIR*'       DUMP EYECATCHER                      C1R23630
D1DBCHR  DS    0CL7                2311 SEEK/SEARCH TARGET              C1R23640
D1DBIN   DC    XL2'0'              DATA CELL BIN NR.                    C1R23650
D1DCYL   DC    XL2'0'              CYLINDER NR.                         C1R23660
D1DHEAD  DC    XL2'1'              HEAD NR.                             C1R23670
D1DREC   DC    X'0'                RECORD NR.                           C1R23680
D1DFLAG  DC    X'0'                FLAG FOR NXTCHR, 0 = DATA OPS        C1R23690
D1DDTF   DC    A(RES2311)          POINTER TO DTFPH                     C1R23700
D1DTPC   DC    A(G11TKCYL)         2311 TRACKS PER CYLINDER             C1R23710
         ORG   D1DTPC              BACK TO START OF TPC                 C1R23720
         DS    H                   FILLER - UPPER HW OF TPC             C1R23730
D1DTPCH  DS    H                   LOW HALFWORD OF TPC                  C1R23740
D1DSOE   DS    0CL6                START OF EXTENT                      C1R23750
D1DSOEBB DC    X'FFFF'             ..BIN                                C1R23760
D1DSOECC DC    X'0000'             ..CYLINDER                           C1R23770
D1DSOEHH DC    X'0000'             ..HEAD (OR TRACK)                    C1R23780
D1DEOE   DS    0CL6                END OF EXTENT                        C1R23790
D1DEOEBB DC    X'FFFF'             ..BIN                                C1R23800
D1DEOECC DC    X'0000'             ..CYLINDER                           C1R23810
D1DEOEHH DC    X'0000'             ..HEAD (OR TRACK)                    C1R23820
D1DSTK   DC    H'0'                STARTING TRACK OF LIBRARY            C1R23830
D1DRPT   DC    H'0'                RECORDS PER TRACK, SET AS NEEDED     C1R23840
         SPACE 3                                                        C1R23850
*                                                                       C1R23860
* CURSOR FOR 2314 LIBRARY OPERATIONS                                    C1R23870
*                                                                       C1R23880
         DS    0D                  FORCE DOUBLEWORD ALIGNMENT           C1R23890
         DC    CL8'*14*LIB*'       DUMP EYECATCHER                      C1R23900
D4BCHR   DS    0CL7                SEEK/SEARCH BBCCHHR FOR 2314         C1R23910
D4SEEK   DS    0CL6                TARKET FOR CCW SEEK OPS IS BBCCHH    C1R23920
D4BIN    DC    XL2'0'              BIN NR, ALWAYS ZERO                  C1R23930
D4SRCH   DS    0CL5                TARGET FOR CCW SEARCH ID IS CCHHR    C1R23940
D4CYL    DC    XL2'0'              ..CYLINDER NR                        C1R23950
D4HEAD   DC    XL2'1'              ..HEAD NR                            C1R23960
D4REC    DC    X'0'                ..RECORD NR                          C1R23970
D4FLAG   DC    X'0'                FLAG FOR NXTCHR, 0 = DATA OPS        C1R23980
*                                  1 = FORMATTING WRITES (WRITE CKD)    C1R23990
D4DTF    DC    A(RES2314)          ADDRESS OF DTFPH FOR FILE            C1R24000
D4TPC    DC    A(G14TKCYL)         2314 TRACKS PER CYLINDER             C1R24010
         ORG   D4TPC               BACK TO START OF TPC                 C1R24020
         DS    H                   FILLER - UPPER HW OF TPC             C1R24030
D4TPCH   DS    H                   LOW HALFWORD OF TPC                  C1R24040
D4SOE    DS    0CL6                START OF EXTENT                      C1R24050
D4SOEBB  DC    X'FFFF'             ..BIN                                C1R24060
D4SOECC  DC    X'0000'             ..CYLINDER                           C1R24070
D4SOEHH  DC    X'0000'             ..HEAD (OR TRACK)                    C1R24080
D4EOE    DS    0CL6                END OF EXTENT                        C1R24090
D4EOEBB  DC    X'FFFF'             ..BIN                                C1R24100
D4EOECC  DC    X'0000'             ..CYLINDER                           C1R24110
D4EOEHH  DC    X'0000'             ..HEAD (OR TRACK)                    C1R24120
D4STK    DC    H'0'                STARTING TRACK OF EXTENT             C1R24130
D4RPT    DC    H'0'                RECORDS PER TRACK                    C1R24140
*                                                                       C1R24150
* CURSOR FOR 2314 DIRECTORY OPERATIONS)                                 C1R24160
*                                                                       C1R24170
         DS    0D                  FORCE DOUBLEWORD ALIGNMENT           C1R24180
         DC    CL8'*14*DIR*'       DUMP EYECATCHER                      C1R24190
D4DBCHR  DS    0CL7                SEEK/SEARCH BBCCHHR FOR DIR OPS      C1R24200
D4DSEEK  DS    0CL6                TARGET FOR CCW SEEK OPS IS BBCCHH    C1R24210
D4DBIN   DC    XL2'0'              BIN NR, ALWAYS ZERO                  C1R24220
D4DSRCH  DS    0CL5                TARGET FOR CCW SEARCH ID IS CCHHR    C1R24230
D4DCYL   DC    XL2'0'              ..CYLINDER NR                        C1R24240
D4DHEAD  DC    XL2'1'              ..HEAD NR                            C1R24250
D4DREC   DC    X'0'                ..RECORD NR                          C1R24260
D4DFLAG  DC    X'0'                FLAG FOR NXTCHR, 0 = DATA OPS        C1R24270
*                                  1 = FORMATTING WRITES (WRITE CKD)    C1R24280
D4DDTF   DC    A(RES2314)          ADDRESS OF DTFPH FOR I/O             C1R24290
D4DTPC   DC    A(G14TKCYL)         2314 TRACKS PER CYLINDER             C1R24300
         ORG   D4DTPC              BACK TO START OF TPC                 C1R24310
         DS    H                   FILLER - UPPER HW OF TPC             C1R24320
D4DTPCH  DS    H                   LOW HALFWORD OF TPC                  C1R24330
D4DSOE   DS    0CL6                START OF EXTENT                      C1R24340
D4DSOEBB DC    X'FFFF'             ..BIN                                C1R24350
D4DSOECC DC    X'0000'             ..CYLINDER                           C1R24360
D4DSOEHH DC    X'0000'             ..HEAD (OR TRACK)                    C1R24370
D4DEOE   DS    0CL6                END OF EXTENT                        C1R24380
D4DEOEBB DC    X'FFFF'             ..BIN                                C1R24390
D4DEOECC DC    X'0000'             ..CYLINDER                           C1R24400
D4DEOEHH DC    X'0000'             ..HEAD (OR TRACK)                    C1R24410
D4DSTK   DC    H'0'                STARTING TRACK OF EXTENT             C1R24420
D4DRPT   DC    H'0'                RECORDS PER TRACK, SET AS NEEDED     C1R24430
         TITLE 'CONVERT 2311 SYSRES TO 2314: 2311 && 2314 LIBRARY INFO' C1R24440
*                                                                       C1R24450
*  WORKAREA USED TO HALFWORD-ALIGN DISK ADDRESSES WHICH EXIST IN THE    C1R24460
*  LIBRARY STATUS RECORD AS UNALIGNED.  USED AS THE PARAMETER TO AND    C1R24470
*  FROM THE XLATBCHR ADDRESS TRANSLATION ROUTINE.                       C1R24480
*                                                                       C1R24490
         DS    0D                   FORCE ALIGNMENT                     C1R24500
WORKBCHR DS    0XL7                 LIBRARY OR DIRECTORY BBCCHHR        C1R24510
WORKBB   DS    XL2                  ..BIN                               C1R24520
WORKCC   DS    XL2                  ..CYLINDER                          C1R24530
WORKHH   DS    XL2                  ..HEAD (OR TRACK)                   C1R24540
WORKREC  DS    XL1                  ..RECORD                            C1R24550
*                                                                       C1R24560
*  SAVE AREA FOR CID, RLD, AND SLD DIRECTORIES ON SYSRES.               C1R24570
*                                                                       C1R24580
         DS    0D                   FORCE ALIGNMENT                     C1R24590
CID1SCHR DS    CL8                  BBCCHHRE OF CID START               C1R24600
         DS    0D                   FORCE ALIGNMENT                     C1R24610
RLD1SCHR DS    CL8                  BBCCHHRE OF RLD START               C1R24620
*                                                                       C1R24630
*  DC  X'0000',X'0000',X'0001',X'00' WORKS ON THE 44K ASSEMBLER,        C1R24640
*  BUT NOT ON THE 6K ASSEMBLER INCLUDED IN THE DISTRIBUTION SYSTEM      C1R24650
*                                                                       C1R24660
C1BCHRH1 DS    0CL7                 BBCCHHR-1 OF SYSRES CTL RECS        C1R24670
         DC    X'0000'              ..BB (NOT USED ON 2311)             C1R24680
         DC    X'0000'              ..CC  CYLINDER ZERO                 C1R24690
         DC    X'0001'              ..HH, HEAD ONE                      C1R24700
         DC    X'00'                SEARCH R0 TO WRITE R1               C1R24710
*                                   USED FOR SEEK/SEARCH PRIOR REC      C1R24720
*                                                                       C1R24730
C4CILSTS DS    0CL7                 CIL STATUS REC BBCCHHR              C1R24740
         DC    X'0000'              ..BB (NOT USED ON 2311)             C1R24750
         DC    X'0000'              ..CC  CYLINDER ZERO                 C1R24760
         DC    X'0001'              ..HH, HEAD ONE                      C1R24770
         DC    X'01'                SEARCH R2 TO WRITE R1 DOTO          C1R24780
*                                                                       C1R24790
*  LIBRARY BLOCK COUNTS, USED TO CONTROL LOOPS THAT COPY RELO AND       C1R24800
*  SOURCE STATEMENT LIBRARIES                                           C1R24810
*                                                                       C1R24820
RLB1NBLK DC    H'0'                 RELO LIB BLOCK COUNT TO COPY        C1R24830
SLD1STRK DC    H'0'                 START TRACK OF 2311 SRC DIR         C1R24840
SLB1STRK DC    H'0'                 START TRACK OF 2311 SRC LIB         C1R24850
SLB1NBLK DC    H'0'                 SRC LIB BLOCK COUNT TO COPY         C1R24860
*                                                                       C1R24870
*  2314 LIBRARY STARTING TRACK NUMBERS, USED TO CALCULATE RELATIVE      C1R24880
*  BLOCK ADDRESSES OF 2314 LIBRARY MEMBERS                              C1R24890
*                                                                       C1R24900
RLD4STRK DC    AL2(R14RLBST*G14TKCYL) STR TRK OF 2314 RLD               C1R24910
RLB4STRK DC    AL2(R14CILCL*G14TKCYL+R14RLDTK) ST TRK OF 2314 REL LIB   C1R24920
SLD4STRK DC    AL2(R14SLBST*G14TKCYL) STR TRK OF 2314 SLD               C1R24930
SLB4STRK DC    AL2(R14SLBST*G14TKCYL+R14SLDTK) STR TRK OF 2314 SLB      C1R24940
*                                                                       C1R24950
         TITLE 'CONVERT 2311 SYSRES TO 2314: 2311 SYSRES CCW STRINGS'   C1R24960
*                                                                       C1R24970
* READ CKD R1-5 FROM 2311 FOR UPDATE AND TRANSFER TO NEW SYSRES.        C1R24980
*                                                                       C1R24990
CCW1HD1  CCW   READCKD,C0H1R1,CMDCHAIN,L'C0H1R1 READ CKD CL STATUS      C1R25000
         CCW   READCKD,C0H1R2,CMDCHAIN,L'C0H1R2 READ CKD RL STATUS      C1R25010
         CCW   READCKD,C0H1R3,CMDCHAIN,L'C0H1R3 READ CKD SL STATUS      C1R25020
         CCW   READCKD,C0H1R4,CMDCHAIN,L'C0H1R4 READ CKD REC NOT USED   C1R25030
         CCW   READCKD,C0H1R5,X'00',L'C0H1R5 READ CKD IPL PGM $$A$IPL2  C1R25040
*                                                                       C1R25050
*  CCWS TO READ CID, CIL, RLD, RLB, SLD, SLB DATA ONLY, NO CKD          C1R25060
*                                                                       C1R25070
CCW1CDRD CCW   READDATA,DIR1,X'00',G11CIDBS  READ CIL DIR RECORD        C1R25080
CCW1CLRD CCW   READDATA,*-*,X'00',G11CILBS   READ CI LIBRARY RECORD     C1R25090
CCW1RDRD CCW   READDATA,DIR1,X'00',G11RLDBS  READ RLD DIR RECORD        C1R25100
CCW1RLRD CCW   READDATA,IO+8,X'00',G11RLBBS  READ RL LIBRARY RECORD     C1R25110
*                                                                       C1R25120
*  ABOVE READ LEAVES ROOM FOR COUNT FIELD FOR FORMATTING WRITE          C1R25130
*  TO 2314 RLB                                                          C1R25140
*                                                                       C1R25150
CCW1SDRD CCW   READDATA,DIR1,X'00',G11SLDBS  READ SLD DIR RECORD        C1R25160
CCW1SLRD CCW   READDATA,IO+8,X'00',G11SLBBS  READ SL LIBRARY RECORD     C1R25170
*                                                                       C1R25180
*  ABOVE READ LEAVES ROOM FOR COUNT FIELD FOR FORMATTING WRITE          C1R25190
*  TO 2314 SLB                                                          C1R25200
*                                                                       C1R25210
* 2311 CHANNEL PROGRAM TO READ IPL BOOTSTRAP RECS FROM C0H0R1-2         C1R25220
* (C0H0R3 IS VOL1 LABEL FOR DISK PACK).  DATA ONLY, NOT CKD             C1R25230
*                                                                       C1R25240
CCW1T0   CCW   READDATA,C0H0R1,CMDCHAIN,24   READ D IPL BOOTSTRAP REC1  C1R25250
         CCW   READDATA,C0H0R2,X'00',144  READ D IPL BOOTSTRAP REC2     C1R25260
*                                                                       C1R25270
         TITLE 'CONVERT 2311 SYSRES TO 2314: 2314 SYSRES CCW STRINGS'   C1R25280
*                                                                       C1R25290
* WRITE CKD R1-5 TO 2314                                                C1R25300
*                                                                       C1R25310
CCW4HD1  CCW   WRITECKD,C0H1R1,CMDCHAIN,L'C0H1R1   CL STATUS            C1R25320
         CCW   WRITECKD,C0H1R2,CMDCHAIN,L'C0H1R2   RL STATUS            C1R25330
         CCW   WRITECKD,C0H1R3,CMDCHAIN,L'C0H1R3   SL STATUS            C1R25340
         CCW   WRITECKD,C0H1R4,CMDCHAIN,L'C0H1R4   NOT USED             C1R25350
         CCW   WRITECKD,C0H1R5,X'00',L'C0H1R5   IPL PROG.               C1R25360
*                                                                       C1R25370
*  CCWS TO READ CID, RLD, SLD, DATA ONLY, NO CKD.  USED TO READ         C1R25380
*  CID TO REBUILD THE 'SPECIAL' DIRECTORIES ON H5-8 OF CYL 0            C1R25390
*  AND TO READ RLD AND SLD BLOCK ONE TO UPDATE LIBRARY STATUS           C1R25400
*                                                                       C1R25410
CCW4CDRD CCW   READDATA,DIR4,X'00',G11CIDBS  READ CIL DIR RECORD        C1R25420
CCW4RDRD CCW   READDATA,DIR4,X'00',G11RLDBS  READ RLD DIR RECORD        C1R25430
CCW4SDRD CCW   READDATA,DIR4,X'00',G11SLDBS  READ SLD DIR RECORD        C1R25440
*                                                                       C1R25450
*  CCWS TO READ CORE IMAGE DIRECTORY STATUS RECORD, DATA ONLY           C1R25460
*                                                                       C1R25470
CCW4CSRD CCW   WRITEDAT,CISTATUS,X'00',G11CIDBS  WRITE CIL STATUS REC   C1R25480
*                                                                       C1R25490
*  CCWS TO WRITE CID, RLD, SLD, DATA ONLY, NO CKD.  USED TO UPDATE      C1R25500
*  THE DIRECTORY STATUS RECORD.                                         C1R25510
*                                                                       C1R25520
CCW4CSWD CCW   WRITEDAT,CISTATUS,X'00',L'CISTATUS  WRITE CIL STATUS REC C1R25530
CCW4CDWD CCW   WRITEDAT,DIR4,X'00',G14CIDBS  WRITE CIL DIR RECORD       C1R25540
CCW4RDWD CCW   WRITEDAT,DIR4,X'00',G14RLDBS  WRITE RL DIR RECORD        C1R25550
CCW4SDWD CCW   WRITEDAT,DIR4,X'00',G14SLDBS  WRITE SL DIR RECORD        C1R25560
*                                                                       C1R25570
*  CCWS TO WRITE CID, CIL, RLD, RLB, SLD, SLB.  ALL ARE FORMATTING      C1R25580
*  WRITES, COUNT-KEY-DATA.                                              C1R25590
*                                                                       C1R25600
CCW4CDFW CCW   WRITECKD,DIR4FW,X'00',G14CIDBS+8  WRITE CIL DIR REC      C1R25610
CCW4CLFW CCW   WRITECKD,IOPHFW,X'00',G14CILBS+8  WRITE CI LIB REC       C1R25620
CCW4CLSV CCW   WRITECKD,IOPHFW,X'00',G14CILBS+8  ORIGINAL OF WRT CLB    C1R25630
CCW4RDFW CCW   WRITECKD,IO,X'00',G14RLDBS+8  WRITE RLD DIR REC          C1R25640
CCW4RLFW CCW   WRITECKD,IO,X'00',G14RLBBS+8  WRITE RL LIB REC           C1R25650
CCW4SDFW CCW   WRITECKD,IO,X'00',G14SLDBS+8  WRITE SLD DIR REC          C1R25660
CCW4SLFW CCW   WRITECKD,IO,X'00',G14SLBBS+8  WRITE SL LIB REC           C1R25670
*                                                                       C1R25680
CCW4LBFW CCW   WRITECKD,IO,X'00',8     WRITE LABEL CYL EOF RECORD       C1R25690
*                                                                       C1R25700
* CHANNEL PROGRAM TO WRITE IPL BOOTSTRAP RECS FROM C0H0R1-2 OF          C1R25710
* 2311 TO SAME LOCATION ON 2314 (C0H0R3 IS VOL1 LABEL)                  C1R25720
*                                                                       C1R25730
CCW4T0R1 CCW   WRITEDAT,C0H0R1,X'00',24   WRITE IPL REC1                C1R25740
CCW4T0R2 CCW   WRITEDAT,C0H0R2,X'00',144  IPL REC2                      C1R25750
*                                                                       C1R25760
         TITLE 'CONVERT 2311 SYSRES TO 2314   PIOCS CONTROL BLOCKS'     C1R25770
*********************************************************************** C1R25780
*                                                                       C1R25790
*  DTFPH MACROS ARE REQUIRED TO CREATE DASD FORMAT-1 LABELS AND         C1R25800
*  ENABLE ACCESS IF DASD FILE PROTECTION IS ENABLED.                    C1R25810
*                                                                       C1R25820
*  THE FIRST BYTES OF DTFPH ARE THE CCB FOR I/O OPERATIONS.  BECAUSE    C1R25830
*  ALL I/O OPERATIONS ARE SYNCHRONOUS, WE CAN POINT ALL DTFPH AT THE    C1R25840
*  SAME LEAD-IN CCW STRING.                                             C1R25850
*                                                                       C1R25860
*  THE LEAD-IN CCW STRING INCLUDES SEEK, SEARCH, TIC *-8, FOLLOWED BY   C1R25870
*  A TIC TO THE CCW OR STRING THAT THE REQUESTOR WANTS EXECUTED.  THIS  C1R25880
*  MEANS THAT ONE SEEK/SEARCH AREA IS USED.                             C1R25890
*                                                                       C1R25900
*********************************************************************** C1R25910
         SPACE 3                                                        C1R25920
*                                                                       C1R25930
* COMMON LEAD-IN CCW STRING FOR I/O OPERATIONS FROM THIS PROGRAM        C1R25940
* THE I/O ROUTINE MODIFIES THE TARGET OF THE SECOND TIC CCW TO POINT    C1R25950
* TO THE CALLER-REQUESTED CCW OR CCW STRING.  BECAUSE ALL I/O IN THIS   C1R25960
* PROGRAM IS SYNCHRONOUS, ONE LEAD-IN STRING WILL SUFFICE.              C1R25970
*                                                                       C1R25980
CCWSTART CCW   SEEK,CCWSEEK,CMDCHAIN,6   SEEK                           C1R25990
         CCW   SRCHIDEQ,CCWSEEK+2,CMDCHAIN,5 SEARCH ID EQUAL            C1R26000
         CCW   TIC,*-8,0,0         SEARCH AGAIN, SKIPPED IF EQ          C1R26010
CCWTUSER CCW   TIC,*-*,0,0         TIC TO CALLER CCW STRING             C1R26020
*                                  ADDR FIELD MOD'D BEFORE EXCP         C1R26030
*                                                                       C1R26040
CCWSEEK  DC    XL8'0'              COMMON LEAD-IN SEEK/SEARCH ID        C1R26050
*                                                                       C1R26060
*  DTFPH FOR INPUT SYSRES ON 2311.  THIS SYSRES INCLUDES THE CORE       C1R26070
*  IMAGE AND RELOCATABLE LIBRARIES THAT WILL BE COPIED TO THE NEW       C1R26080
*  2314 SYSRES.                                                         C1R26090
*                                                                       C1R26100
RES2311  DTFPH TYPEFLE=INPUT,                                          -C1R26110
               MOUNTED=SINGLE,                                         -C1R26120
               DEVICE=2311,                                            -C1R26130
               DEVADDR=SYS004,                                         -C1R26140
               CCWADDR=CCWSTART                                         C1R26150
*                                                                       C1R26160
* DTFPH FOR SECOND INPUT SYSRES.  THIS SYSRES HAS A SOURCE STATEMENT    C1R26170
* LIBRARY THAT WILL BE COPIED TO THE OUTPUT 2314 SYSRES.                C1R26180
*                                                                       C1R26190
SLB2311  DTFPH TYPEFLE=INPUT,                                          -C1R26200
               MOUNTED=SINGLE,                                         -C1R26210
               DEVICE=2311,                                            -C1R26220
               DEVADDR=SYS005,                                         -C1R26230
               CCWADDR=CCWSTART                                         C1R26240
*                                                                       C1R26250
* DTFPH FOR OUTPUT SYSRES.  WILL CONTAIN CIL AND RLB FROM RES2311 AND   C1R26260
* SLB FROM SLB2311, ALL ORGANIZED AS A SINGLE SYSTEM RESIDENCE FILE     C1R26270
*                                                                       C1R26280
RES2314  DTFPH TYPEFLE=OUTPUT,                                         -C1R26290
               MOUNTED=SINGLE,                                         -C1R26300
               DEVICE=2314,                                            -C1R26310
               DEVADDR=SYS006,                                         -C1R26320
               CCWADDR=CCWSTART                                         C1R26330
*                                                                       C1R26340
         TITLE 'CONVERT 2311 SYSRES TO 2314    I/O AREAS'               C1R26350
*                                                                       C1R26360
* DIRECTORY BLOCK AREAS FOR 2311 AND 2314.  SPACE IS ALLOCATED FOR      C1R26370
* A CORE IMAGE DIRECTORY BLOCK.  RELO AND SOURCE DIRECTORY BLOCKS       C1R26380
* ARE SMALLER.                                                          C1R26390
*                                                                       C1R26400
         DS    0D                  FORCE DOUBLEWORD ALIGNMENT           C1R26410
         DC    CL8'**DIR1**'       EYECATCHER                           C1R26420
DIR1     DS    CL360               DIRECTORY I/O AREA.  360 IS          C1R26430
*                                                                       C1R26440
         DS    0D                  FORCE DOUBLEWORD ALIGNMENT           C1R26450
         DC    CL8'**DIR4**'       EYECATCHER                           C1R26460
DIR4FW   DS    CL8                 COUNT FIELD SPACE FOR WRITE CKD      C1R26470
DIR4     DS    CL(G14CIDBS)        CURRENT 2314 DIRECTORY RECORD        C1R26480
*                                                                       C1R26490
* I/O AREA FOR THE FOLLOWING OPERATIONS                                 C1R26500
* - TRANSFER OF C0H1R1 THROUGH C0H1R5 TO THE 2314 SYSRES                C1R26510
* - TRANSFER OF PHASES FROM 2311 TO 2314                                C1R26520
* - FORMATTING OF REMAINDER OF CORE IMAGE LIBRARY                       C1R26530
* - COPY OF RELO AND SOURCE LIBRARIES FROM 2311 TO 2314                 C1R26540
* - TRANSFER OF IPL BOOTSTRAP C0H0R1-2 FROM 2311 TO 2314                C1R26550
*                                                                       C1R26560
         DS    0D                  ALIGN TO DOUBLEWORD                  C1R26570
         DC    CL8'**IOAR**'       EYECATCHER                           C1R26580
IO       EQU   *                   MARK END OF PHASE, USED AS I/O AREA  C1R26590
C0H0R1   DS    CL24                                                     C1R26600
C0H0R2   DS    CL144                                                    C1R26610
         ORG   IO                                                       C1R26620
C0H1R1   DS    CL88                                                     C1R26630
C0H1R2   DS    CL88                                                     C1R26640
C0H1R3   DS    CL88                                                     C1R26650
C0H1R4   DS    CL88                                                     C1R26660
C0H1R5   DS    CL2504                                                   C1R26670
         ORG   IO                                                       C1R26680
IOPHFW   DS    CL8                 COUNT FIELD SPACE FOR PHASE WRITE    C1R26690
IOPH     EQU   *                   START OF READ DATA AREA FOR PHASES   C1R26700
LOADPT   EQU   *                                                        C1R26710
         ORG                                                            C1R26720
         END                                                            C1R26730

/*
// EXEC LNKEDT
/* RELEASE ASSIGNMENTS TO PREVENT 1A34D NO FREE JIBS
// RESET SYS001
// RESET SYS002
// RESET SYS003
* START EXEC CVT11RES
// ASSGN SYS004,X'130'
// DLBL RES2311,'DOS.SYSRES.FILE.VOLUME.1'
// EXTENT SYS004,RES11A
// ASSGN SYS005,X'131'
// DLBL SLB2311,'DOS.SYSRES.FILE.VOLUME.2'
// EXTENT SYS005,RES11B
// ASSGN SYS006,X'190'
// DLBL RES2314,'DOSRES.262.SYSTEM.RESIDENCE',0
// EXTENT SYS006,DOSRES,,,1,3979
// EXEC
/&
