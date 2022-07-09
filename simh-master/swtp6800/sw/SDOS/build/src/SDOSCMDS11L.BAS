!       "SDOSCOMMANDS" -- COMMAND INTERPRETER FOR SDOS 1.1
!
!       Things that could use a little work:
!           1) Garbage on the remainder of the command line should be diagnosed
!           2) Delete should assume the same device if multiple names
!              are given, to make life a little easier.
!
!^L
!       Revision History:
!
!       EDITED 01/20/79 CJM     TO ADD COPY <FILE> TO *
!       EDITED 03/26/79 CJM     TO CORRECT THE ".TIME 14:200 ..." PROBLEM
!       EDITED 08/07/79 CJM     TO PRINT DISK IO STATS ON DISMOUNT
!       EDITED 08/15/79 CJM     TO ADD FREE SPACE IN BYTES
!       EDITED 08/15/79 CJM     ALL READ'S REPLACED BY SYSCALLS
!       EDITED 09/16/79 CJM     MAKES DEFAULTPROGRAM LOOK AT 106,7 INSTEAD
!                               OF FC,B FOR TOP OF MEMORY; ALL LISTS WITH
!                               DEFAULT DESTINATION GO TO CHANNEL 0 INSTEAD
!                               OF OPENING CONSOLE:  (**CHEAP SHARE COMPAT***)
!       EDITED 10/10/79 CJM     ALLOWS TYPING "WD0:SDOSDISKVALIDATE" TO WORK
!                               (PROBLEM LIES IN LEAVING <CR> AROUND FOR
!                               PROGRAM TO PICK UP....)
!       EDITED 03/04/80 IDB     CONVERTED TO BASIC V1.4
!       EDITED 03/19/80 CJM     ADDED CRC COMMAND
!                               IMPROVED ERROR REPORTING ON DELETE
!                               DON'T PROMPT ".TIME" IN DO FILES...
!       EDITED 3/21/80 IDB      FIXED FREE CMD TO SET FILESCH=0
!                               FIXED DEFAULT ON "ASK BEFORE DELETE" TO BE YES
!       EDITED 1/20/81 IDB      CONVERTED TO BASIC14H. PRINT TIME$ ON FILES$.
!                               ADDED "USERSPACE" COMMAND.
!                               ADDED "MOUNT" COMMAND.
!                               FIX FILES TO PRINT UP TO 10^10 FILESIZE OK.
!                               DISMOUNT "DUMPBUFFERS" 1ST, THEN PRINTS STATUS
!                               INSTALLED PATHNAMEPREFIX LOGIC.
!       EDITED 12/2/81 JRY      HANDLE Activation Not In Buffer ERROR
!       EDITED 12/2/81 JRY      COPY FILE PROTECTION AND CREATION DATE DURING
!                               COPY COMMAND
!
!       EDITED 12/2/81 JRY      ADD setprotection AND clearprotection COMMANDS
!       EDITED 12/2/81 JRY      CHANGE DISMOUNT TO REPORT LAST PROBLEM SECTOR
!       EDITED 12/9/81 JRY      TELL files COMMAND ABOUT "B" PROTECTION FLAG
!       EDITED 12/9/81 JRY      FIX closelog COMMAND TO RE-OPEN LOG TO CONSOLE:
!                               IF IN do FILE
!       EDITED 12/9/81 JRY      DEFAULT EXTENSION TO PROGRAM BEING INVOKED
!                               IMPLICITLY IS CHANGED TO ".CMD"; IF
!                               <PROGRAM>.CMD CANNOT BE FOUND, THEN <PROGRAM>
!                               IS ATTEMPTED; IF THAT CANNOT BE FOUND, THEN
!                               AN ATTEMPT TO INVOKE <PROGRAM>.DO,
!                               AS A do FILE, IS MADE
!
!       EDITED 2/13/82 IDB      1) TRY <PROGRAM> FIRST; IF THAT FAILS, TRY
!                                  <PROGRAM>.CMD (IF NAME LENGTH <=12).
!                               2) IF EOF(0), THEN CLOSE #0,OPEN #0,"CONSOLE:"
!       EDITED 5/15/82 IDB      TO CHANGE TO "SDOSCMDS11I" TO DISTINGUISH
!                               BETWEEN "SDOSCMDS11C" (& D & H) THAT HAD COPY
!                               AND DEBUG BROKEN IN "SDOSCMDS11x.ASM"
!
!       EDITED 2/1/84  RCW      Version 1.1j
!                               Added Wildcard section from SDOSDISKBACKUP
!                               to fix Text*text*text problem
!                               Put new assembly subroutine in to
!                               perform faster directory searches
!                               Changed to search for .EXE instead of .CMD
!
!       EDITED 3/07/84 RCW      Version 1.1k
!                               fixed DEBUG command passing of arguments
!                               fixed COPY and RENAME bug when " TO " missing
!                               giving "Are you sure you want to write on
!                               disk?"
!                               Fixed appending file bug in SDOSCMDS11K.asm
!                               Added new CRC code
!       EDITED 7/16/85 IDB      Version 1.1l
!                               Added code to check for "ActivationNotInBuffer" error
!
!^L
        DIM     LASTKEYSTROKE$(1),CRFLG/0/, CHAR$(1)
        DIM     VERIFY, JUNK16$[16], SPACE$/" "/, I, MATCH
        DIM     STATS$[19], FREEFLAG/0/
        DIM     ERCODE, LASTER, SPARSE$/0/, ON$/0/, OFF$/1/
        DIM     FNAME$[16], HCN$[2], HCSIC$[1], LCNS$[2]
        DIM     FSIZE$[4], PROT$[1], EMPTY$[3]
        DIM     DELP$[20], TOTENTS, TOTLCNS
        DIM     BYTE, LCNS, X, SNAME$[17], SPOS, LISTCHAN/2/
        DIM     CNAME$[17], CPOS
        DIM     SECTOR$[100], BYTE$[1], FILE$[100], NAME$[16]
        DIM     PATHNAMEPREFIX$[100]
        DIM     COMPNTR/5/, COMSTR$[100]
        DIM     DLM$[4], LOC,LOCFLAG, CR$/:0D/, OP$[30], DIR$[32], DEVICE$[30]
        DIM     NCYL, TOTLCN, STEMP$[17], NCPOS, NSPOS
        DIM     CDATE$[3], CURERROR, MARGIN$/31/
        DIM     STIME$[6], TIME, NULL0$/""/, NULL4$[4]
        DIM     CRCREM$/0,0/

!       SYSCALL CODES:

        DIM     DISMOUNT$/14, :4, 1, :11/, DUMPBUFFERS$/14,:4,1,:1/
        DIM     UNLOCK$/14, :4, 1, :10/
        DIM     SETER$/:13, :4, 0, 0/, ISUSER$/:1A, 2/, SCGETTYPE$/:F, 14, 0, 4/
        DIM     GETSTATS$/:F,:E,1,:11/
        DIM     CLOSELOG$/:8, :2/, PRINTER$/:15, :2/
        DIM     GETPARAMS$/:F, 14, 2, 5/
        DIM     STATUS$[12], RENAM$/3, 14, 1/, CLOG$/7, 14/
        DIM     READB$/:B, :E, 1/, READB2$/:B, :E, 2/
        DIM     SETDEFAULTDISK$/:9, 14/
        DIM     READA$/:A, :E, 1, 1/, WRITEA$/:C, 8, 0/
        DIM     READA0$/:A,:E,0,1/
        DIM     GETLASTERR$/:14,:E/
        DIM     GETLASTBADLSN$/:F,:E,1,:10/, BADLSN$[3]
        DIM     SCGETFILEDATE$/:F,14,0,:10/,CCSETFILEDATE$/:E,8,0,:10/
        DIM     SCGETFILEPROT$/:F,14,0,:11/,CCSETFILEPROT$/:E,8,0,:11/

        DEF     MSB(X1)=INT(X1/256)
        DEF     LSB(X2)=INT(X2-256*MSB(X2))
!^L
SUBROUTINE EXTRACTPATHNAMEPREFIX( PATHNAME$ )
        REM EXTRACTS THE PATH NAME PREFIX (I.E., ALL BUT THE FILE NAME)
        REM REMOVES PATHNAME PREFIX FROM ARGUMENT
        FOR I=LEN(PATHNAME$) TO 1 STEP -1
                IF PATHNAME$(I,1)=':'
                THEN
                        PATHNAMEPREFIX$=LEFT$(PATHNAME$,I)
                        PATHNAME$=RIGHT$(PATHNAME$,I+1)
                        EXIT SUBROUTINE
                FI
        NEXT I
        LET PATHNAMEPREFIX$="DISK:"
        EXIT SUBROUTINE
END

SUBROUTINE VERIFYDISKDEVICE(CHANNELTHATMUSTBEDISK)
        REM VERIFIES THAT DEVICE OPENED ON CHANNELTHATMUSTBEDISK IS A DISK
        REM OTHERWISE ERRORS
        SYSCALL #CHANNELTHATMUSTBEDISK,SCGETTYPE$,'',BYTE$
        IF BYTE$(1)<>1 THEN ERROR 1058
        RETURN SUBROUTINE
END

Def MatchWildCardTo(WildCard$,MatchToString$)

    ! Function to match WildCard containing "*" to MatchToString
    ! Returns TRUE or FALSE based on match result

    Let i=find(WildCard$,"*")
    if i=0 then return MatchToString$=WildCard$ \ ! Wildcard has form: <text>

    ! WildCard$ has form: <text>*<moretext>
    if find(MatchToString$,WildCard$(1,i-1))<>1
    then return false \ ! Prefixes don't match
    MatchToPointer=i \ ! skip past matched prefix
    WildCardPointer=1 \ ! Set up for For loop in Repeat, below
    Repeat
        ! i holds offset from WildCardPointer to next "*" in WildCard$
        for WildCardPointer=WildCardPointer+i to len(WildCard$) ...
&           Until WildCard$(WildCardPointer,1)<>"*" ...
&           do ! Skip past matched text and possible multiple "*"s

        ! Now match something to the "*" skipped in the WildCard$
        if WildCardPointer>Len(WildCard$) then return true \ ! Matched !
        if MatchToPointer>Len(MatchToString$) then return false
        ! At this point, WildCard$ has form: <matchedtext>*<text>...
        Let i=find(Right$(WildCard$,WildCardPointer),"*")
        if i=0
        then
            ! Wildcard has form:  *<text>
            i=Len(WildCard$)-WildCardPointer+1 \ ! Amount to match
            if Len(MatchToString$)-MatchToPointer+1 < i
            then return false \ ! Tail of MatchToString$ is too short
            ! Tail is long enough, see if matches wildcard tail
            return MatchToString$[Len(MatchToString$)-i+1,i]=...
&                  Right$(WildCard$,WildCardPointer)
        fi
        ! WildCard$ has form: *<text>*<moretext>

        i1=find(Right$(MatchToString$,MatchToPointer),...
&               WildCard$[WildCardPointer,i-1])
        if i1=0 then return false \ ! Can't find text in MatchToString$

        ! MatchToPointer= MatchToPointer + where found (disp 0) + how long
        Let MatchToPointer= MatchToPointer + (i1-1) + (i-1)

    end
end
!^L
!
!       M A I N  C O D E  S T A R T S  H E R E
!
!       IS THERE AN AXE OVER OUR HEAD??
        SYSCALL(GETLASTERR$,'',OP$)
        LASTER=OP$[1]**8+OP$[2]
        CURERROR=LASTER
        ON ERROR GOTO 60000
        ERCODE=0\GOSUB 16000
!
!       SET MARGIN
        IF PEEK(:FC)**8+PEEK(:FD)>:5000 THEN LET MARGIN$[1]=:FF

!       HAS USER SET TIME???

        OPEN #1,"CLOCK:"
        READ    #1, STIME$
        IF STIME$[4]<>0 THEN 100
        ON ERROR GOTO 100
        CALL    SYSCALL(ISUSER$)
        ON ERROR GOTO 60000
        FILE$="TIME"
        PRINT ".";FILE$;" ";
        CLOSE #1
        GOTO 160
!^L
!       THIS IS THE HEAD OF THE COMMAND-PROCESSING LOOP

100     ON ERROR GOTO 110
        FOR I=1 TO 3
101     REM RE-ENABLE ERROR TRAP
                CLOSE #I
110     NEXT I

120     ON ERROR GOTO 60000
        GOSUB POP 0
140     PRINT ".";\DLM$=" "\GOSUB 21000\IF FILE$="" THEN 170
        IF FILE$="IFERROR" THEN GOSUB 22000\GOTO 18000
        IF CURERROR=0 THEN 160
        CURERROR=0
        ON ERROR GOTO 150\CALL SYSCALL(ISUSER$,NULL0$,NULL0$)
        ON ERROR GOTO 60000\GOTO 160
150     ON ERROR GOTO 60000
        REM ARE WE TALKING TO A CONSOLE: DEVICE ?
        SYSCALL #0,SCGETTYPE$,NULL0$,STATUS$
        IF STATUS$[1]=4 THEN GOSUB 17900
        GOSUB CLOSE0OPENCONSOLE
        PRINT "*** Do File Aborted ***"
        GOTO 100
!
CLOSE0OPENCONSOLE: ! CLOSE CHANNEL 0 AND RE-OPEN TO CONSOLE
        CLOSE #0\OPEN #0,"CONSOLE:"
        RETURN
!^L
!       THIS IS WHERE THE COMMANDS ARE RECOGNIZED

160     IF FILE$="COPY" THEN GOSUB 22000\GOTO COPY
        IF FILE$="RENAME" THEN GOSUB 22000\GOTO 2000
        IF FILE$="DELETE" THEN GOSUB 22000\GOTO 3000
        IF FILE$="LIST" THEN GOSUB 22000\GOTO 4000
        IF FILE$="FILES" THEN GOSUB 22000\GOTO FILES
        IF FILE$="MOUNT" THEN GOSUB 22000\GOTO MOUNT
        IF FILE$="DISMOUNT" THEN GOSUB 22000\GOTO DISMOUNT
        IF FILE$="TIME" THEN GOSUB 22000\GOTO 8000
        IF FILE$="FREE" THEN GOSUB 22000\GOTO 9000
        IF FILE$="VERSION" THEN GOSUB 22000\GOTO VERSION
        IF FILE$="LABEL" THEN GOSUB 22000\GOTO 13000
        IF FILE$="LOG" THEN GOSUB 22000\GOTO 14000
        IF FILE$="DO" THEN GOSUB 22000\GOTO 15000
        IF FILE$="CLOSELOG" THEN GOSUB 22000\GOTO 17000
        IF FILE$="GOTO" THEN GOSUB 22000\GOTO 19000
        IF FILE$="HELP" THEN GOSUB 22000\GOTO 20000
        IF FILE$="DEBUG" THEN GOSUB 21000\CALL LOADD(FILE$)
        IF FILE$="DEFAULTDISK" THEN GOSUB 22000\GOTO 30000
        IF FILE$="CRC" THEN
                GOSUB   22000
                GOTO    23000
        FI
        IF FILE$="USERSPACE"
        THEN GOSUB 22000\PRINT HEX$(PEEK(:FC)**8+PEEK(:FD))\ GOTO 100
        IF FILE$="SETPROTECTION" THEN GOSUB 22000\GOTO SETPROT
        IF FILE$="CLEARPROTECTION" THEN GOSUB 22000\GOTO CLEARPROT
        IF FILE$[1]=ASC("*") THEN GOSUB 22000\GOTO 100
!^L
!       THIS TREATS AN UNRECOGNIZED COMMAND AS: INVOCATION OF <COMMAND>,
!       <COMMAND>.EXE,<COMMAND>.$BAS, OR DO <COMMAND>.DO, AS APPROPRIATE

        IF ERROR WHEN CHAIN FILE$ THEN
            IF ERR<>1064 THEN 59999
            IF ERROR WHEN CHAIN FILE$ CAT ".EXE"
            THEN
                IF ERR <> 1064 AND ERR <> 1013 THEN 59999
                FILE$=FILE$ CAT ".DO"
                IF ERROR WHEN OPEN #1,FILE$
                THEN
                  IF ERR <> 1011 AND ERR<>1013 THEN 59999
                  LASTER=1064\CURERROR=LASTER\GOSUB 22000
                  GOTO 20050
                FI
                GOTO 15010
            FI
        FI
170     IF CRFLG=1 THEN 140
        GOSUB 22000
171     CURERROR=102
        LASTER=102
        GOTO 20050
!^L
!
!
!
!       OBJECT EXTRACTOR
!
!
300 !
        LOCFLAG=FIND(COMSTR$(COMPNTR,LEN(COMSTR$)-COMPNTR+1),DLM$)
        LOC=LOCFLAG+COMPNTR-1
        IF COMPNTR=LEN(COMSTR$)+1 THEN LET FILE$=""\RETURN
        IF LOC=COMPNTR-1 THEN LET LOC=LEN(COMSTR$)+1
        FILE$=COMSTR$(COMPNTR,LOC-COMPNTR)
        COMPNTR=LOC+LEN(DLM$)
        IF COMPNTR>LEN(COMSTR$) THEN LET COMPNTR=LEN(COMSTR$)+1
        RETURN
!^L
!
!       COPY .... FAST VERSION   USES AN ASSEMBLY SUBROUTINE WHICH
!               CAN DO BLOCK TRANSFERS
1030    DLM$=","\GOSUB 300\IF FILE$="" THEN
                IF STATUS$[1]=0 THEN
                        IF ERROR WHEN
                                 SYSCALL #1,CCSETFILEDATE$,STIME$
                                 SYSCALL #1,CCSETFILEPROT$,PROT$
                        THEN ! NOT IMPLEMENTED, SO DON'T DO IT!
                        ON ERROR GOTO 60000
                FI
                CLOSE #1
                GOTO 100
        ELSE OPEN #2,FILE$
1040    ON ERROR GOTO 1060
1050    CALL COPYF(MARGIN$)\ON ERROR GOTO 60000\CLOSE #2\GOTO 1030
1060    IF ERR <>:419 THEN 60000\! B/ NOT A DISK WRITE LOCKED ERROR
        PRINT "Are you sure you want to write on the disk DEVICE? ";
        GOSUB 40000
        IF SECTOR$<>"YES" THEN 100
        CALL SYSCALL(UNLOCK$)
        SPARSE$=OFF$
        RESTORE #2,0\GOTO 1050
!^L

COPY:   SPARSE$=ON$
        DLM$=" TO "\GOSUB 300
        IF LOCFLAG=0 Then error 102
        SECTOR$=FILE$
        IF FIND(FILE$,",") THEN LET FILE$=LEFT$(FILE$,FIND(FILE$,',')-1)
        CALL EXTRACTPATHNAMEPREFIX(FILE$)
        REM SAVE FILENAME OF SOURCE FILE IN CASE DESTINATION IS "*"
        LET SNAME$=FILE$
        GOSUB 300
        COMSTR$=SECTOR$\COMPNTR=1
        SECTOR$=FILE$\DLM$=","\GOSUB 300

!       CHANGE: ADD COPY <FILE> TO *
        EXTRACTPATHNAMEPREFIX(SECTOR$)
        IF FIND(PATHNAMEPREFIX$,"*")>0 THEN ERROR 102
        IF FIND(SECTOR$,"*")=1
        THEN
                IF SECTOR$="*"
                THEN SECTOR$=PATHNAMEPREFIX$ CAT SNAME$
                ELSE ERROR 102
        ELSE SECTOR$=PATHNAMEPREFIX$ CAT SECTOR$
1210    OPEN #2,FILE$\CREATE #1,SECTOR$
        SYSCALL #1,SCGETTYPE$,NULL0$,STATUS$
        IF STATUS$[1]<>0 THEN LET SPARSE$=OFF$
        ELSE
                ! DECIDE IF WE SHOULD COPY THE PROTECTION AND THE DATE
                SYSCALL #2,SCGETTYPE$,NULL0$,PROT$
                IF PROT$[1]=0
                THEN
                    IF ERROR WHEN
                         SYSCALL #2,SCGETFILEDATE$,NULL0$,STIME$
                         SYSCALL #2,SCGETFILEPROT$,NULL0$,PROT$
                    THEN STATUS$[1]=1\ ! DON'T COPY DATE OR PROTECTION
                    ON ERROR GOTO 60000
                ELSE STATUS$[1]=1 \ ! PREVENT DATA/PROT COPY
        FI
        CALL    COPYINIT(SPARSE$)
        GOTO 1040
!^L
!
!
!       SET FILE PROTECTION
!
!
!
SETPROT:
        DLM$=" ON "
        GOSUB 300
        IF FILE$="B" OR FILE$="W" OR FILE$="D" THEN SNAME$=FILE$ ELSE ERROR 102
        GOSUB 300
        CALL EXTRACTPATHNAMEPREFIX(FILE$)
        IF FIND(FILE$,":")=0 THEN FILE$=PATHNAMEPREFIX$ CAT FILE$
        OPEN #1,FILE$
        SYSCALL #1,SCGETFILEPROT$,NULL0$,PROT$
        IF SNAME$="B" THEN PROT$[1]=PROT$[1]!:01
        IF SNAME$="W" THEN PROT$[1]=PROT$[1]!:40
        IF SNAME$="D" THEN PROT$[1]=PROT$[1]!:80
        SYSCALL #1,CCSETFILEPROT$,PROT$
        GOTO 100
!^L
!
!
!       CLEAR FILE PROTECTION
!
!
!
CLEARPROT:
        DLM$=" ON "
        GOSUB 300
        IF FILE$="B" OR FILE$="W" OR FILE$="D" THEN SNAME$=FILE$ ELSE ERROR 102
        GOSUB 300
        CALL EXTRACTPATHNAMEPREFIX(FILE$)
        IF FIND(FILE$,":")=0 THEN FILE$=PATHNAMEPREFIX$ CAT FILE$
        OPEN #1,FILE$
        SYSCALL #1,SCGETFILEPROT$,NULL0$,PROT$
        IF SNAME$="B" THEN PROT$[1]=PROT$[1]&COM(:01)
        IF SNAME$="W" THEN PROT$[1]=PROT$[1]&COM(:40)
        IF SNAME$="D" THEN PROT$[1]=PROT$[1]&COM(:80)
        SYSCALL #1,CCSETFILEPROT$,PROT$
        GOTO 100
!^L
!
!
!       RENAME
!
!
2000    DLM$=" TO "
        GOSUB 300
        IF LOCFLAG=0 Then error 102
        CALL EXTRACTPATHNAMEPREFIX(FILE$)
        IF LEN(FILE$)>0
        THEN
                OPEN #1,PATHNAMEPREFIX$ CAT FILE$
                GOSUB 300
                IF FIND(FILE$,":")=0
                THEN FILE$=PATHNAMEPREFIX$ CAT FILE$
                CALL SYSCALL(RENAM$,FILE$,NULL4$)\GOTO 100
        ELSE
                REM WANT TO RENAME A DEVICE
                OPEN #1,PATHNAMEPREFIX$
                VERIFYDISKDEVICE(1)
                GOSUB 300
                IF LEN(FILE$)=0 OR LEN(FILE$)>32 THEN ERROR 102
                FOR I=LEN(FILE$)+1 TO 32 DO FILE$(I)=:20
                LET LEN(FILE$)=32
                SYSCALL #1,UNLOCK$
                WRITE #1@32,FILE$
                GOTO 100
        FI
!^L
!
!       DELETE
!
!
3000    VERIFY=-1
3010    DLM$=","\GOSUB 300\IF FILE$="" THEN GOTO 100
        IF 0=FIND(FILE$,"*") THEN 3070
        IF 0=FIND(FILE$,":") THEN LET DEVICE$=""\GOTO 3050
        DEVICE$=FILE$(1,FIND(FILE$,":"))\FILE$=RIGHT$(FILE$,LEN(DEVICE$)+1)
3050    DIR$=DEVICE$ CAT "DIRECTORY.SYS"
        GOSUB 11000\GOTO 3010
3070    ON ERROR GOTO 3100
        DELETE FILE$
        ON ERROR GOTO 60000
        GOTO 3010


3100    ! ERROR RECOVERY DURING DELETE
        IF ELN<>3070 AND ELN<>11040 THEN 60000
        IF ERR=1 THEN 60000
        ON ERROR GOTO 60000
        ERCODE  =ERR
        GOSUB   16000
        PRINT "'";
        CALL SYSCALL(PRINTER$)
        PRINT   "' ";
        IF      ELN=3070 AND COL(0)+LEN(FILE$)>45 ...
&       OR      ELN<>3070 AND COL(0)+LEN(DEVICE$)+LEN(CNAME$)>45
        THEN    PRINT
        PRINT   "occurred while deleting ";
        IF      ELN=3070
        THEN
                PRINT FILE$
                GOTO 3010
        ELSE
                PRINT DEVICE$;CNAME$
                GOTO    11041
        FI
!^L
!
!       LIST
!
4000    SPOS=FIND(COMSTR$," TO ")\DLM$=" TO "
        IF SPOS=0
        THEN    LET COMSTR$=COMSTR$ CAT " TO CONSOLE:"\
                LISTCHAN=0
        ELSE    LET LISTCHAN=2
        GOSUB 300
        IF 0<>FIND(FILE$," ") THEN ERROR 102
        OPEN #1,FILE$\GOSUB 300\IF LISTCHAN=2 THEN CREATE #2,FILE$
4040    ON ERROR GOTO 4099
        SYSCALL READA$,NULL0$,SECTOR$
4050    ON ERROR GOTO 60000
        PRINT #LISTCHAN,SECTOR$;
        GOTO 4040
4099    IF ERR=:773 THEN 4050 \ ! Ignore "Activation Not In Buffer" Error
        IF ERR<>:3E9 THEN 60000
        REM PRINT LAST PARTIAL LINE IF ANY
        PRINT #LISTCHAN,SECTOR$;
        IF LISTCHAN=2 THEN CLOSE #2
        GOTO 100
!^L
!       MOUNT   MOUNT A DISK DEVICE             
!
MOUNT: GOSUB 300
        EXTRACTPATHNAMEPREFIX(FILE$)
        IF LEN(FILE$)>0 THEN ERROR 102
        OPEN #1,PATHNAMEPREFIX$
        VERIFYDISKDEVICE(1)
        REM CAUSE SDOS TO MOUNT THE SELECTED DISK
        REM CAUSE BOOT CHECKSUM ERROR IF DISK IS NOT INITZD
        OPEN #2,PATHNAMEPREFIX$ CAT "BOOT.SYS"
        READ #2,SECTOR$
        PRINT "Disk ID is: ";SECTOR$(33,32)
        GOTO 100

!^L
!
!
!       DISMOUNT    REMOVE A DEVICE FROM USE
!
DISMOUNT:
        DLM$ = ","
        GOSUB 300
7090    OPEN #1,FILE$
        VERIFYDISKDEVICE(1)
        ON ERROR GOTO 7100
        REM DUMP BUFFERS BEFORE READING I/O STATUS
        SYSCALL DUMPBUFFERS$
7100    ON ERROR GOTO 7200
        CALL SYSCALL(GETSTATS$,NULL0$,STATS$)
        IF 0<>STATS$[1]!STATS$[2]!STATS$[5]!STATS$[6]!STATS$[9]!STATS$[10]
        THEN
                PRINT "Error totals:"
                PRINT
                PRINT "Operation","Error count","Status"
                PRINT "---------","-----------","------"
                PRINT "Read",STATS$[9]**8+STATS$[10], ...
&                       HEX$(STATS$[11]**8+STATS$[12])
                PRINT "Write",STATS$[5]**8+STATS$[6], ...
&                       HEX$(STATS$[7]**8+STATS$[8])
                PRINT "Seek",STATS$[1]**8+STATS$[2], ...
&                       HEX$(STATS$[3]**8+STATS$[4])
                PRINT
                PRINT "A total of";
                PRINT (STATS$[1]+STATS$[5]+STATS$[9])*256+ ...
&                       STATS$[2]+STATS$[6]+STATS$[10];
                PRINT "I/O errors out of";
        ELSE
                PRINT "A total of";
        FI
        PRINT STATS$[15]+STATS$[14]**8+STATS$[13]*256*256;
        PRINT "I/O operations on ";File$

        IF      STATS$(16)<>STATS$(17) OR ...
&               STATS$(17)<>STATS$(18) OR ...
&               STATS$(18)<>:FF THEN
                PRINT "Last Bad Sector (Soft) = :";
                FILE$   =HEX$(STATS$(16))
                PRINT   FILE$(4,2);
                FILE$   =HEX$(STATS$(17))
                PRINT   FILE$(4,2);
                FILE$   =HEX$(STATS$(18))
                PRINT   FILE$(4,2)
        FI
        CALL    SYSCALL(GETLASTBADLSN$,'',BADLSN$)

        IF      BADLSN$(1)<>BADLSN$(2) OR ...
&               BADLSN$(2)<>BADLSN$(3) OR ...
&               BADLSN$(3)<>:FF THEN
                PRINT "Last Bad Sector (Hard) = :";
                FILE$   =HEX$(BADLSN$(1))
                PRINT   FILE$(4,2);
                FILE$   =HEX$(BADLSN$(2))
                PRINT   FILE$(4,2);
                FILE$   =HEX$(BADLSN$(3))
                PRINT   FILE$(4,2)
        FI

7200    ON ERROR GOTO 60000
        CALL SYSCALL(DISMOUNT$,NULL0$,NULL0$)
        GOSUB 300
        IF FILE$="" THEN GOTO 100
        CLOSE #1
        PRINT
        GOTO 7090

!^L
!
!       CRC <FILE>
!

23000   GOSUB   300

!       Build CRC Table if room

        CALL    CRCINIT

        OPEN    #1, FILE$
        CRCREM$(1)=0
        CRCREM$(2)=0


23010   READ    #1, SECTOR$
        IF      SECTOR$="" THEN 23100
        CALL    CRC(SECTOR$,CRCREM$)
        GOTO    23010

23100   PRINT   "CRC= ";HEX$(CRCREM$(1)*256+CRCREM$(2))
        GOTO    100
!       TIME [HH:MM MM/DD/YY]

8000    IF COMPNTR>=LEN(COMSTR$) THEN 8160
        DLM$=":"\GOSUB 8200
        TIME=60^3*VAL(FILE$)
        DLM$=" "\GOSUB 8200
        TIME=TIME+60^2*VAL(FILE$)
        STIME$[3]=LSB(TIME)
        STIME$[2]=LSB(MSB(TIME))
        STIME$[1]=MSB(MSB(TIME))
        DLM$="/"\GOSUB 8200\FILE$=":" CAT FILE$
        STIME$[5]=INT(VAL(FILE$))
        GOSUB 8200\FILE$=":" CAT FILE$
        STIME$[4]=INT(VAL(FILE$))
        GOSUB 8200\FILE$=":" CAT FILE$
        STIME$[6]=INT(VAL(FILE$))
        OPEN #1,"CLOCK:"\LEN(STIME$)=6
        WRITE #1,STIME$\GOTO 100
8160    PRINT TIME$;" ";DATE$\GOTO 100


8200    GOSUB   300
        IF      LEN(FILE$)>2
                THEN    171
        RETURN
!^L
!
!       FREE SPACE ON DISK
!
!
9000    GOSUB 300
        EXTRACTPATHNAMEPREFIX(FILE$)
        OPEN #2,PATHNAMEPREFIX$
        VERIFYDISKDEVICE(2)
        OPEN #1,PATHNAMEPREFIX$ CAT "DISKMAP.SYS"
        TOTLCNS=0
        FREEFLAG=1
        ON ERROR GOTO 9060
9010    CALL SYSCALL(READB$,NULL0$,DIR$)
        FOR I=1 TO 32
                CALL CNTBYTE(DIR$[I,1])\TOTLCNS=TOTLCNS+DIR$[I]
        NEXT I
        GOTO 9010
9060    ON ERROR GOTO 60000
        IF ERR<>:3E9 THEN 60000
        CALL SYSCALL(READB2$,NULL0$,SECTOR$)
        PRINT "A total of";TOTLCNS;"free clusters";
        LET FILESCH=0\GOTO 5140
!^L
!
!       VERSION
!
!
VERSION: PRINT "SDOS Version 1.1  SDOSCMDS V1.1L  ";COPYRIGHT$
        GOTO 100
!
!       DEBUG
!

!^L
FILES:  DLM$="TO "
        GOSUB 300
        CALL EXTRACTPATHNAMEPREFIX(FILE$)
        REM WHY IS THE FOLLOWING NEEDED ?
        IF      FILE$<>"" AND FILE$(LEN(FILE$),1)=" "
        THEN    FILE$=LEFT$(FILE$,LEN(FILE$)-1)
        SNAME$=FILE$ CAT CR$
        GOSUB   300
        IF      FILE$="" THEN
                REM NO "TO" CLAUSE, OUTPUT "FILES" TO CONSOLE:
                FILESCH=0
        ELSE
                REM "TO" CLAUSE GIVEN, OUTPUT FILES TO SPECIFIED PLACE
                CREATE  #3, FILE$
                FILESCH =3
        FI
        FREEFLAG=0

        IF SNAME$=CR$ OR FIND(SNAME$,"*")>0
        THEN
           MULTFILESTOFIND = TRUE
        ELSE
           MULTFILESTOFIND =FALSE
        FI

        OPEN #2,PATHNAMEPREFIX$ \ REM THIS IS THE NAME OF A DISK DEVICE
        VERIFYDISKDEVICE(2)
        CALL SYSCALL(READB2$,NULL0$,SECTOR$)
        CALL SYSCALL(GETPARAMS$,NULL0$,STATUS$)
        CLOSE #2
        OPEN #2,PATHNAMEPREFIX$ CAT "DIRECTORY.SYS"\TOTLCNS=0\TOTENTS=0
        PRINT #FILESCH, "Files on...   ";SECTOR$[33,32];" at ";Time$;" ";Date$
        PRINT #FILESCH, "Filename";TAB(20);"# LCNs        Bytes    Prot      Date"
        LET LEN(DIR$)=32
5035    IF ERROR WHEN CALL FINDBUSYDIRECTORYENTRY(DIR$)
        THEN
          IF ERR = 1001 THEN 5135
          GOTO 60000
        FI
        ON ERROR GOTO 60000
        IF SNAME$ <> CR$ THEN
                LET CNAME$=DIR$[1,16]
                IF CNAME$[16]=ASC(" ") THEN LEN(CNAME$)=FIND(CNAME$," ")-1
                GOSUB 12000
                IF MATCH=0 THEN 5035
        FI
        REM DIRECTORY ENTRY MATCHES WILDCARD, DISPLAY IT!
        LCNS=DIR$[20]**8+DIR$[21]
        BYTE=DIR$[25]+DIR$[24]**8+DIR$[23]*256^2+DIR$[22]*256^3
        PRINT #FILESCH, DIR$[1,16];"   ";
        PRINT #FILESCH, USING "######   ##########  ",LCNS,BYTE;
        JUNK16$="        "
        IF DIR$[26]&:80<>0 THEN JUNK16$[1]=ASC("D")
        IF DIR$[26]&:40<>0 THEN JUNK16$[2]=ASC("W")
        IF DIR$[26]&:01<>0 THEN JUNK16$[3]=ASC("B")
        PRINT #FILESCH,"   ";JUNK16$[1,7];
        DELP$=HEX$(DIR$[28])\PRINT #FILESCH, DELP$[4,2];"/";
        DELP$=HEX$(DIR$[27])\PRINT #FILESCH, DELP$[4,2];"/";
        DELP$=HEX$(DIR$[29])\PRINT #FILESCH, DELP$[4,2]
        TOTENTS=TOTENTS+1\TOTLCNS=TOTLCNS+LCNS
        REM IF NO WILDCARD, THEN DISPLAY IS COMPLETE
        IF MULTFILESTOFIND THEN GOTO 5035


5135    PRINT #FILESCH, "Total of";TOTLCNS;"clusters in";TOTENTS;"files";
        GOTO 5145 \ ! Have rearranged channel usage, Status$ is already loaded

5140    CALL SYSCALL(GETPARAMS$,NULL0$,STATUS$)
5145    TOTLCN=(STATUS$[3]**8+STATUS$[4])*(STATUS$[5]**8+STATUS$[6])* ...
&       (STATUS$[7]**8+STATUS$[8])
        TOTLCN=INT(TOTLCN/SECTOR$[18])
        PRINT #FILESCH,  USING " for ###.#% of disk capacity",TOTLCNS/TOTLCN*100
        IF NOT(FREEFLAG) THEN 100
        PRINT #FILESCH, "(";TOTLCNS;"clusters =";
        PRINT #FILESCH, TOTLCNS*SECTOR$[18]*(STATUS$[1]**8+STATUS$[2]);
        PRINT #FILESCH, "bytes )"
        GOTO 100

11000   ! DIRECTORY SEARCH ROUTINE
        IF VERIFY<>-1 THEN 11010
        PRINT "Ask before doing the delete? ";\GOSUB 40000
        IF FIND(UPPERCASE$(SECTOR$),"N")=1 THEN LET VERIFY=0 ELSE LET VERIFY=1
11010   OPEN #2,DIR$
        RESTORE #2,0
        SNAME$=FILE$ CAT CR$
        LET LEN(DIR$)=32
11020   IF ERROR WHEN CALL FINDBUSYDIRECTORYENTRY(DIR$) THEN GOTO 11060

        FOR I=2 TO 16 UNTIL DIR$(I,1) = SPACE$ DO ! NOTHING
        CNAME$ = DIR$[1,I-1]
        GOSUB 12000
        LEN(CNAME$)=LEN(CNAME$)-1
        IF MATCH=0 THEN 11050
        IF VERIFY=0 THEN 11040
        PRINT "Delete ";CNAME$;"? ";\GOSUB 40000
        IF SECTOR$<>"YES" AND SECTOR$<>"Y" THEN 11050
11040   ON ERROR GOTO 3100
        DELETE DEVICE$ CAT CNAME$
        ON ERROR GOTO 11060
        IF VERIFY=0 THEN PRINT CNAME$
11041   ON ERROR GOTO 11060
11050   GOTO 11020
11060   ON ERROR GOTO 60000
        IF ERR<>:3E9 THEN 60000
        CLOSE #2
        RETURN
!^L
!
!       ACTUAL WILD CARD TESTER...
!
!       AN "*" CAN BE ANY LENGTH OF CHARS FROM 0 TO INFINITY
!

12000
        Match = 0
        Cname$ = Cname$ Cat CR$
        IF MatchWildCardTo (SNAME$,CNAME$) = True Then Let Match = 1
        Return

!^L
!       LABEL (LABEL) COMAND LINE...
!       DOES NOTHING!
13000   GOTO 100
!
!       LOG <LOGFILENAME>
!
14000   GOSUB 300
        GOSUB 14500
        GOTO 100

14500   ! ACTUAL LOG SUBROUTINE
        CALL SYSCALL(CLOG$,FILE$,NULL4$)
        RETURN
!
!       DO (COMMANDFILENAME)
!
15000   GOSUB 300
        OPEN #1,FILE$\ ! MAKE SURE FILE EXISTS!
15010   CLOSE #0
        OPEN #0,FILE$
        FILE$="CONSOLE:"
        ON ERROR GOTO 15030
        GOSUB 14500
15015   ERCODE=0\GOSUB 16000\GOTO 100
15030   IF ERR=:407 THEN 15015 ELSE 60000
!^L
!
!       SET ERROR TO ERCODE
!
16000   SETER$[3]=MSB(ERCODE)\SETER$[4]=LSB(ERCODE)
        CALL SYSCALL(SETER$,NULL0$,NULL0$)\RETURN
!
!       CLOSE LOG FILE
!
17000   GOSUB 17900
        IF ERROR WHEN SYSCALL(ISUSER$) THEN FILE$="CONSOLE:"\GOSUB 14500
        GOTO 100
17900   CALL SYSCALL(CLOSELOG$,NULL0$,NULL0$)
        RETURN
!^L
!
!       IF ERROR <N> <GOTOLABEL>
!
18000   GOSUB 300\ERCODE=VAL(FILE$)
        IF ERCODE<>LASTER THEN 100
        CURERROR=0
!
!       GOTO LABEL
!
19000   GOSUB 300\FILE$="LABEL " CAT FILE$
19010   PRINT ".";\GOSUB 40000
        IF SECTOR$=FILE$ THEN 100 ELSE 19010
!
!       HELP!
!
20000   GOSUB 300\IF FILE$="" THEN 20050
        ERCODE=VAL(FILE$)\GOSUB 16000
20020   CALL SYSCALL(PRINTER$,NULL0$,NULL0$)\PRINT
        GOTO 100
20050   ERCODE=LASTER\GOSUB 16000\GOTO 20020
!
!
!       DEFAULTDISK <DISKDEVICENAME>
!
!
30000   GOSUB 300
        CALL SYSCALL(SETDEFAULTDISK$,FILE$,NULL4$)
        GOTO 100
!^L
DEF KEYSTROKE
    ! RETURNS NUMERIC VALUE OF NEXT KEYSTROKE FROM INPUT STREAM
    IF ERROR WHEN SYSCALL(READA0$,'',LASTKEYSTROKE$)
    THEN IF ERR=1001
         THEN GOSUB CLOSE0OPENCONSOLE\SYSCALL(READA0$,'',LASTKEYSTROKE$)
         ELSEIF ERR<>1907 THEN ERROR
    ! IGNORE "ACTIVATION NOT IN BUFFER" ERROR
    LASTKEYSTROKE$=UPPERCASE$(LASTKEYSTROKE$)
    RETURN LASTKEYSTROKE$(1)
END
!
!       GET THE FIRST FIELD FROM INPUT
!
21000   FOR COMPNTR=1 TO 100
            FILE$(COMPNTR)=KEYSTROKE
            IF LASTKEYSTROKE$=" " THEN CRFLG=0\EXIT COMPNTR
            IF LASTKEYSTROKE$=CR$ THEN CRFLG=1\EXIT COMPNTR
        NEXT COMPNTR
21050   LEN(FILE$)=COMPNTR-1
        RETURN
!
!       GET REST OF LINE
!
22000   IF CRFLG=1 THEN LET COMSTR$=FILE$\RETURN
        GOSUB 40000\COMSTR$=FILE$ CAT " " CAT SECTOR$
        COMPNTR=COMPNTR+1\RETURN
!
40000   INPUT '' SECTOR$
        LET SECTOR$=UPPERCASE$(SECTOR$)
        RETURN
!^L
!       GENERAL ERROR HANDLING


59999   IF CRFLG=0 THEN INPUT "" COMSTR$
60000   LASTER=ERR\CURERROR=LASTER\GOTO 20050
!
        END
END
