!       SDOSDISKINIT.BAS -- PLACES INITIAL FILE STRUCTURE ON A DISK
!
!       EDITED 4/25/80  TO ALLOW COPYING OF BOOT AND SDOS.SYS FILES
!       EDITED 4/4/80   TO CHECK VALIDITY OF DISK ID
!       EDITED 3/6/80   CONVERT TO BASIC V1.4
!       EDITED 3/7/80   MORE CONVERSION AND UPGRADING
!       EDITED 4/1/80   DON'T COPY FIRST 16 BYTES OF SECTOR 0
!
!       1/28/81 V1.0h   CONVERTED TO BASIC14H
!                       DISMOUNTS DISK BEFORE ATTEMPTING INITZ
!                       MAKE <CR> IN RESPONSE TO "...tuning parameters? " LEGAL
!                       SET UP BOOT:DIRLSN AS DESCRIBED BY SDOS MANUAL
!                       ALLOW filename WHEN COPYING SDOS.SYS OR BOOT.SYS
!                       REMOVE "and hit return..." WAIT
!
!       12/22/82 V1.1g  Modified to match SDOS 1.1g
!                          Prompt dummies for device name & verify it is a disk
!                          Simplify construction of data disk
!                          Allow optional copying of SERIALNUMBER.SYS
!                          Allow optional copying of DEFAULTPROGRAM
!                          "Copy BOOT.SYS from..." changed to
!                                "File to copy for BOOT.SYS ... ?"
!                          Modified DiskMap logic to handle any size map
!                          Marks system files as Write and Backup protected
!                          Rehash DIRECTORY.SYS for fast opens
!                          Deletes SDOS.SYS if not a system disk
!
!       12/06/83 V1.1h  Installed FindBestMap code
!                       Changed size of SECTOR$ to 5200 (TestRead) to
!                       use from 200 - 5200 as reading buffer to find best
!                       map values.
!       12/13/83  V1.1h Made change to find best map when ? given only

!
!
        DIM Version$/"*** SDOS Disk Initialization V1.1h ***"/
!^L
        DIM SECTOR$[5200]/...
&       'DIRECTORY.SYS   ',0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0, ...
&       'SDOS.SYS        ',0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0, ...
&       'BOOT.SYS        ',0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0, ...
&       'DISKMAP.SYS     ',0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0, ...
&       'BADCLUSTERS.SYS ',0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0/

        DIM BOOT$[64]
        DIM INPUT$[100]
        DIM COPYDISK$[40]
        DIM DTE$[9],FF$/:FF/,CLOCK$[6]
        DIM BYTE$[1],Q$[40],OP$[12]
        DIM DEVICE$[40],DISKID$[32]
        DIM NSPC$[100]
        DIM DIRECTORYENTRY$[32],DIRECTORYENTRY2$[32]

!       SYSCALLS:

        DIM STAT$/:F,:14,1,5/,SETMAP$/:E,8,1,:12/
        DIM UNPROT$/14,:4,1,:10/
        DIM BUF$[20]/0,0/
        DIM RENAM$[14]/3,:E,1/
        DIM NULL$/""/
        DIM SCGETTYPE$/:F,14,:0,:4/,DEVICETYPE$(1),DVTYPDOTDSK/1/
        DIM DISMOUNT$/:E,4,0,:11/
        DIM SCGETFILEDATE$/:F,14,0,:10/,CCSETFILEDATE$/:E,8,0,:10/,FILEDATE$(6)

        Dim Disk/1/,Disk1/2/,Clock/3/
        Dim ReadBuffer$(16)
        Dim TwoByteString$(2),TimeString$(6)
        Dim SCGetParams$/:F,14,0,:5/,TestRead/5000/

Subroutine SetDiskMap(diskmap)
        len(TwoByteString$)=2
        TwoByteString$(1)=diskmap**-8
        TwoByteString$(2)=diskmap&:ff
        Syscall #disk, SetMap$, TwoByteString$
        exit subroutine
end

Subroutine Getparams
        Syscall #disk, SCGetparams$, "", ReadBuffer$
        NBPS=0 \ NSPT=0 \ NTPC=0 \ NCYL=0
        for i = 1 to 2 do NBPS = NBPS*256+ReadBuffer$(i)
        for i = 3 to 4 do NSPT = NSPT*256+ReadBuffer$(i)
        for i = 5 to 6 do NTPC = NTPC*256+ReadBuffer$(i)
        for i = 7 to 8 do NCYL = NCYL*256+ReadBuffer$(i)
        exit subroutine
end

Subroutine DismountDisk
        Syscall #disk, dismount$
        exit subroutine
end

Def GetTime
        read #clock, timeString$
        return (timestring$(1)*256 + timestring$(2))*256 + Timestring$(3)
end

DEF MSB(X1)=INT(X1/256)
DEF LSB(X2)=INT(X2-MSB(X2)*256)

DEF CEILING(CVALUE)
    IF CVALUE=INT(CVALUE) THEN RETURN CVALUE ELSE RETURN INT(CVALUE+1)
END

DEF GETVALFROMTERM(DEFAULTVALUE)
    REPEAT

        INPUT "" INPUT$
        IF INPUT$="" THEN RETURN DEFAULTVALUE
        IF INPUT$="?" THEN RETURN DEFAULTVALUE
        IF ERROR WHEN RETURN VAL(INPUT$)
        THEN
            IF ERR<>7
            THEN ERROR
            ELSE
                PRINT "That doesn't look like a number to me!"
                PRINT "Please enter a number: ";
           FI
        FI
    END
END
!^L
! ********** Start of Main program *********

        LEN(SECTOR$)=MAXLEN(SECTOR$)

10      REM PICK UP DEVICE NAME
        IF COL(0)>1
        THEN
                INPUT "" DEVICE$
                PRINT Version$
        ELSE
                PRINT Version$
                INPUT "Name of drive containing disk to initialize? " DEVICE$
                PRINT "Next time, you can type:"
                PRINT "    .sdosdiskinit ";DEVICE$
        FI
        OPEN #Disk,DEVICE$
        Rem Verify a disk drive has been selected
900     SYSCALL #1,SCGETTYPE$,NULL$,DEVICETYPE$
        IF DEVICETYPE$(1)<>DVTYPDOTDSK
        THEN ERROR 1058 \ ! Not a disk device
910     SYSCALL #1,DISMOUNT$
        READ #1@0,BOOT$
        CHECKSUM=0
        FOR I=17 TO 32 DO CHECKSUM=(CHECKSUM+BOOT$(I))&:FF
        IF CHECKSUM=:FF
        THEN
                PRINT "That device looks initialized to me."
                PRINT "The current disk ID is: "; BOOT$[33,32]
                INPUT "Are you sure you want to re-initialize it? " Q$
                IF Q$="" OR NOT(FIND("YES",UPPERCASE$(Q$))=1)
                THEN
                        PRINT "I didn't think so. Bye."
                        ERROR 104
                FI
        FI
!^L
920     CALL SYSCALL(STAT$,'',BUF$)
        NBPS=256*BUF$[1]+BUF$[2]
        NSPT=256*BUF$[3]+BUF$[4]
        NTPC=256*BUF$[5]+BUF$[6]
        NCYL=256*BUF$[7]+BUF$[8]
        MAXLSN=NSPT*NTPC*NCYL-1
12      INPUT "Disk id? " BOOT$
        IF      LEN(BOOT$)<=32 AND NOT(FIND(BOOT$,'"')) THEN 11

        PRINT   "Disk id's must be 32 characters or less in length and"
        PRINT   "contain no double quotes ('";'"';"')!"
        GOTO    12
11      INPUT "From what disk device can the tuning parameters be copied? " COPYDISK$
        DISKID$=BOOT$
        IF COPYDISK$<>"" AND UPPERCASE$(COPYDISK$)<>"NONE" THEN 1000
        REM Collect disk tuning parameters
        PRINT "Disk has ";(MAXLSN+1)*NBPS;"bytes."
!^L
        REM Compute cluster size needed to cover the disk
        FOR CSZ=1 TO 255 UNTIL MAXLSN<=(NBPS**-1)*CSZ*CSZ DO REM
1010    MAXLCN=INT(MAXLSN/CSZ)-1
312     PRINT "How many sectors per cluster (Default value is";CSZ;") ";
        NSPC=GETVALFROMTERM(CSZ)
310     IF NSPC<>INT(NSPC) THEN PRINT "No fractional clusters, please"
        ELSEIF NSPC<0 THEN PRINT "Negative cluster sizes are illegal!"
        ELSEIF NSPC>255 THEN PRINT "Cluster size is limited to 255!"
        ELSE 400
        GOTO 1010
400     IF NSPC*NBPS*(NBPS/2)<32*5 THEN
        ! DIRECTORY NEEDS SPACE FOR AT LEAST 5 SLOTS, BUT THIS NSPC
        ! WON'T ACCOMODATE IT
        PRINT "Selected NSPC does not allow directory to fit on the disk!"
        GOTO    1010
        FI
        TOTLSN=MAXLSN+1
        TOTLCN=INT(TOTLSN/NSPC)\MAXLCN=TOTLCN-1
        IF TOTLCN/8>NBPS*NSPC THEN
        ! SDOS RULE: DISKMAP MUST FIT INTO ONE CLUSTER
        PRINT "Selected NSPC does not allow Diskmap to be contained in one cluster!"
        GOTO    1010
        FI
        TEMP=TOTLSN-TOTLCN*NSPC
        IF TEMP<>0 THEN PRINT "That will waste";TEMP;"sectors"
        IF MAXLCN<=65534 THEN 450
        PRINT "With ";NSPC;" sectors per cluster,";
        PRINT (MAXLCN-65534)*NSPC;"sectors will be wasted"
        PRINT "because SDOS can only handle 65535 clusters per disk!"
410     INPUT "Are you sure? " Q$
        IF Q$="" THEN 1010
        IF "Y"=UPPERCASE$(Q$(1,1)) THEN 1010
450     TEMP=(NBPS*NSPC)**-1
        IF MAXLCN<=TEMP THEN 500
        PRINT "In this configuration, each file can cover only ";
        PRINT USING "##.##%",TEMP/MAXLCN*100;
        PRINT " of the disk."
        INPUT "Are you sure? "Q$
        IF Q$="" THEN 500
        IF "Y"=UPPERCASE$(Q$(1,1)) THEN 500 ELSE 1010
!^L
500     PRINT "Minimum allocation (Default value=1): ";
        MA      =GETVALFROMTERM(1)
        IF      MA<1 OR MA>65535 OR MA<>INT(MA) THEN
                PRINT   "That is not a valid minimum allocation"
                GOTO    500
        FI
511     PRINT "Minimum extension (Default value=1): ";
        MDA     =GETVALFROMTERM(1)
        IF      MDA<1 OR MDA>65535 OR MDA<>INT(MDA) THEN
                PRINT   "That is not a valid minimum extension"
                GOTO    511
        FI
512   Print "Map Algorithm (Default value :0001, ? to find best): ";

        MapAlg  =GETVALFROMTERM(1)
        If Input$ = "?" Then
                Gosub FindBestMap
                Print "Map algorithm (Default = ";Hex$(bestmap);"): ";
                Mapalg =GetValFromTerm(bestmap)
        Fi
        IF      MAPALG<1 OR MAPALG>65535 OR MAPALG<>INT(MAPALG) THEN
                PRINT   "That is not a valid Map algorithm"
                GOTO    512
        FI
        GOTO 2000

FindBestMap:
        open #clock, "clock:"
        dismountdisk
        getparams

! NSPT will not reflect true sector count on single density
! first tracks with double density on the rest.  The kludge
! below assumes when 9 or 13 this is the case

        If Nspt = 9 Then Nspt = 18
        If Nspt = 13 Then Nspt = 26

        If Ncyl > 10 then Ncyl = 10
        numberofreads = int(NBPS*NSPT*NTPC*NCYL/TestRead) + 1
        for i = 1 to nspt-1 do
                read #disk @ 0,j
                position #disk,0
                dismountdisk
                setdiskmap(i)
                elapsedtime = gettime
                for j = 1 to numberofreads do read #disk,sector$(200,TestRead)
                elapsedtime=gettime - elapsedtime
        print hex$(i);", ";numf$("###.##",elapsedtime/60);" sec,";
        Print numf$("#######",60/elapsedtime*numberofreads*TestRead);" bytes/sec"
        If i = 1
        then
                bestmap=1
                besttime=elapsedtime
        fi
        if elapsedtime<besttime
        then
                bestmap=i
                besttime=elapsedtime
        fi
        end

        for i = bestmap+256 to nspt*256 step 256 do
                read #disk @ 0,j
                position #disk,0
                dismountdisk
                setdiskmap(i)
                elapsedtime = gettime
                for j = 1 to numberofreads do read #disk, sector$(200,TestRead)
                elapsedtime=gettime - elapsedtime
        print hex$(i);", ";numf$("###.##",elapsedtime/60);" sec,";
        Print numf$("#######",60/elapsedtime*numberofreads*TestRead);" bytes/sec"
        if elapsedtime<besttime
        then
                bestmap=i
                besttime=elapsedtime
        fi
        end
        Close #clock
        Return
!^L
!   Steal parameters from named disk.
!
1000    OPEN #Disk1,COPYDISK$
        Rem Verify a disk has been selected.
        SYSCALL #2,SCGETTYPE$,NULL$,DEVICETYPE$
        IF DEVICETYPE$(1)<>DVTYPDOTDSK
        THEN ERROR 1058
        READ #Disk1 @0,BOOT$
        CLOSE #Disk1
        CHECKSUM=0
        FOR I=17 TO 32 DO CHECKSUM=(CHECKSUM+BOOT$(I))&:FF
        IF CHECKSUM<>:FF THEN ERROR 1000
        MA=BOOT$[19]**8+BOOT$[20]
        MDA=BOOT$[21]**8+BOOT$[22]
        MAPALG=BOOT$[23]**8+BOOT$[24]
        NSPC=BOOT$[18]\MAXLCN=INT((MAXLSN+1)/NSPC)-1
        TOTLCN=MAXLCN+1\TOTLSN=MAXLSN+1
!^L
2000    PRINT "NBPS=";NBPS;" NLSN=";TOTLSN;" NLCN=";TOTLCN;" NSPC=";NSPC;
        PRINT " Map algorithm=";HEX$(MAPALG)
2001    PRINT "How many files do you anticipate having on this disk? "
        PRINT "(Default value is";
        DIRSIZ=INT(NBPS*TOTLSN/8192)
        PRINT DIRSIZ;"): ";
        DIRSIZ  =GETVALFROMTERM(DIRSIZ)
        IF      DIRSIZ<5 THEN DIRSIZ=5
        DIRSIZ=CEILING((DIRSIZ*32)/(NBPS*NSPC))
        OPEN #Clock,"CLOCK:"
        READ #Clock, CLOCK$
        CLOSE #Clock
        VERSION=:10 \! FILE STRUCTURE VERSION NUMBER (1.0)
        BUF$[1]=MSB(MAPALG)\BUF$[2]=LSB(MAPALG)
        LEN(BUF$)=2
        CALL SYSCALL(SETMAP$,BUF$)
        CALL SYSCALL(UNPROT$)
!^L
!   Build initial DIRECTORY.SYS data cluster
!
1       SECTOR$[17]=MSB(TOTLCN**-1-1)
        SECTOR$[18]=LSB(TOTLCN**-1-1)
        TEMP=NSPC*NBPS*DIRSIZ
2       SECTOR$[25]=LSB(TEMP)\TEMP=MSB(TEMP)
        SECTOR$[24]=LSB(TEMP)\TEMP=MSB(TEMP)
        SECTOR$[23]=LSB(TEMP)
        SECTOR$[22]=MSB(TEMP)
        SECTOR$[49]=MSB(MAXLCN-1)
        SECTOR$[50]=LSB(MAXLCN-1)
        SECTOR$[21]=DIRSIZ+1
3       FOR I=1 TO 4 DO SECTOR$[53+I]=0\! SDOS'S SIZE=0 BYTES
31      SECTOR$[81]=MSB(MAXLCN)
32      SECTOR$[82]=LSB(MAXLCN)
        FOR I=1 TO 2 DO SECTOR$[85+I]=0
        SECTOR$[88]=MSB(NBPS)\SECTOR$[89]=LSB(NBPS)\! BOOT.SYS'S LEN= 1 SECTOR
33      SECTOR$[113]=MSB(MAXLCN-2)
34      SECTOR$[114]=LSB(MAXLCN-2)
        SECTOR$[145]=MSB(MAXLCN-3)
        SECTOR$[146]=LSB(MAXLCN-3)
!       SET DISKMAP'S LENGTH TO NUMBER OF VALID BYTES
        SECTOR$[118]=0
        TEMP=NBPS*INT((NBPS-1)/NBPS+(TOTLCN/8)/NBPS)
        SECTOR$[121]=LSB(TEMP)\TEMP=MSB(TEMP)
        SECTOR$[120]=LSB(TEMP)\SECTOR$[119]=MSB(TEMP)
!       SET THE DATES ON ALL THE FILES
        SECTOR$[27,3]=CLOCK$[4,3]
        SECTOR$[59,3]=CLOCK$[4,3]
        SECTOR$[91,3]=CLOCK$[4,3]
        SECTOR$[155,3]=CLOCK$[4,3]
        SECTOR$[123,3]=CLOCK$[4,3]

        IF NBPS<256 THEN LET NBPS2=256 ELSE LET NBPS2=NBPS

        FOR I=161 TO NBPS2 DO SECTOR$[I]=0
4       REM  Zero the remainder of DIRECTORY.SYS
        DIRLSN=(TOTLCN**-1)*NSPC
        WRITE #1@DIRLSN*NBPS,SECTOR$[1,NBPS2]
        FOR I=1 TO 128 DO SECTOR$[I]=0
        IF NSPC=1 AND DIRSIZ=1 THEN 2050
        ! Zero fill the remainder of DIRECTORY.SYS
        FOR I=1 TO (NBPS/128)*NSPC*DIRSIZ-(NBPS/128) DO WRITE #1,SECTOR$[1,128]
2050    !
        !   Construct header for DIRECTORY.SYS
        !
        SECTOR$[1]=MSB(TOTLCN**-1-1)
        SECTOR$[2]=LSB(TOTLCN**-1-1)
        TEMP=TOTLCN**-1
        FOR I=1 TO DIRSIZ
            SECTOR$[3+I*2-2]=MSB(TEMP)
            SECTOR$[4+I*2-2]=LSB(TEMP)
            TEMP=TEMP+1
        NEXT I
        FOR I=DIRSIZ*2+3 TO NBPS DO SECTOR$[I]=:FF
REM     WRITE OUT THE FIRST SECTOR OF THE HEADER...
        RESTORE #1,(TOTLCN**-1-1)*NSPC*NBPS
        WRITE #1,SECTOR$[1,NBPS]
5       !
        !  Construct SDOS.SYS header cluster
        !
        SECTOR$[1]=MSB(MAXLCN-1)
        SECTOR$[2]=LSB(MAXLCN-1)
        SECTOR$[3]=0
        SECTOR$[4]=1
        FOR I=5 TO NBPS DO SECTOR$[I]=:FF
REM     WRITE IT OUT
        RESTORE #1,(MAXLCN-1)*NSPC*NBPS
        WRITE #1,SECTOR$[1,NBPS]
6       !
        !  Construct BOOT.SYS header cluster
        !
        SECTOR$[1]=MSB(MAXLCN)
        SECTOR$[2]=LSB(MAXLCN)
        SECTOR$[3]=0
        SECTOR$[4]=0
        FOR I=5 TO NBPS DO SECTOR$[I]=:FF
        RESTORE #1,MAXLCN*NSPC*NBPS
        WRITE #1,SECTOR$[1,NBPS]
7       !
        !  Construct DISKMAP.SYS header cluster
        !
        SECTOR$[1]=MSB(MAXLCN-2)
        SECTOR$[2]=LSB(MAXLCN-2)
        SECTOR$[3]=MSB(TOTLCN**-1-2)
        SECTOR$[4]=LSB(TOTLCN**-1-2)
        FOR I=5 TO NBPS DO SECTOR$[I]=:FF
REM
        RESTORE #1,(MAXLCN-2)*NSPC*NBPS
        WRITE #1,SECTOR$[1,NBPS]
        !
        !  Construct BADCLUSTERS.SYS header cluster
        !
        SECTOR$[1]=MSB(MAXLCN-3)
        SECTOR$[2]=LSB(MAXLCN-3)
        FOR I=3 TO NBPS DO SECTOR$[I]=:FF
        RESTORE #1,(MAXLCN-3)*NSPC*NBPS
        WRITE #1,SECTOR$[1,NBPS]
!^L
8       ! Fill in first-round BOOT.SYS contents
        !
        LET LEN(BOOT$)=64
        FOR I=1 TO 16 DO BOOT$[I]=0 \ ! NULL FILL 1ST 16 BYTES
        BOOT$[17]=VERSION
        BOOT$[25,3]=CLOCK$[4,3]
        BOOT$[33,LEN(DISKID$)]=DISKID$
        FOR I=1 TO 32-LEN(DISKID$) DO BOOT$[32+LEN(DISKID$)+I]=:20
3000    BOOT$[18]=LSB(NSPC)
        BOOT$[19]=MSB(MA)\BOOT$[20]=LSB(MA)
        BOOT$[21]=MSB(MDA)\BOOT$[22]=LSB(MDA)
        BOOT$[23]=MSB(MAPALG)\BOOT$[24]=LSB(MAPALG)
        BOOT$[:1B+1]=MSB(MSB(DIRLSN)) \ ! FILL IN BOOTDIRLSN TEMPORARILY
        BOOT$[:1B+2]=LSB(MSB(DIRLSN))
        BOOT$[:1B+3]=LSB(LSB(DIRLSN))
        GOSUB DOCHECKSUMWRITEBOOT
!^L
! SET UP THE DISKMAP: MARK ALL OWNED LCNS
!
SUBROUTINE MARKLCNASOWNED(OWNEDLCN)
     ! SET A BIT IN THE DISKMAP
     READ #1@DMAP+INT(OWNEDLCN**-3),BYTE$
     BYTE$[1]=BYTE$[1]!1**(OWNEDLCN&7)
     WRITE #1@DMAP+INT(OWNEDLCN**-3),BYTE$
     RETURN SUBROUTINE
END
        DMAP=NBPS*NSPC*(TOTLCN**-1-2)

        ! FIRST, ZERO THE BIT MAP
        FOR I=1 TO NBPS
            SECTOR$[I]=0 \ ! MAKE A ZEROED SECTOR
        NEXT I

        RESTORE #1,DMAP
        FOR I=1 TO NSPC
            WRITE #1,SECTOR$[1,NBPS]
        NEXT I

        MARKLCNASOWNED(0) \ ! MARK BOOT.SYS DATA CLUSTER AS OWNED
        MARKLCNASOWNED(1) \ ! MARK SDOS.SYS DATA CLUSTER AS OWNED

        FOR LCN=TOTLCN**-1-2 TO TOTLCN**-1+DIRSIZ-1
            ! MARK DIRECTORY.SYS HEADER/DATA CLUSTERS AS OWNED
            MARKLCNASOWNED(LCN)
        NEXT LCN

        MARKLCNASOWNED(MAXLCN-3) \ ! MARK BADCLUSTER.SYS HEADER CLUSTER AS OWNED
        MARKLCNASOWNED(MAXLCN-2) \ ! MARK DISKMAP.SYS HEADER CLUSTER AS OWNED
        MARKLCNASOWNED(MAXLCN-1) \ ! MARK SDOS.SYS HEADER CLUSTER AS OWNED
        MARKLCNASOWNED(MAXLCN)   \ ! MARK BOOT.SYS HEADER CLUSTER AS OWNED

        ! COMPUTE # BITS IN BIT MAP, UP TO NEXT SECTOR BOUNDARY
        NBITSINMAP=8*NBPS*INT((NBPS-1)/NBPS+TOTLCN/(NBPS*8))
        ! FILL REST OF MAP BYTE
        FOR LCN=MAXLCN+1 TO NBITSINMAP-1 UNTIL LCN&:7=0 DO MARKLCNASOWNED(LCN)
        ! FILL REMAINDER OF DISKMAP.SYS WITH $FF
        POSITION #1,DMAP+LCN/8
        FOR LCN=LCN TO NBITSINMAP-1 STEP 8 DO WRITE #1,FF$
5000    CLOSE #Disk\! CLOSE THE DISK
!^L
!  REHASH ALL THE SYSTEM FILE NAMES TO ENSURE FAST OPENS
!
SUBROUTINE REHASH(REHASHNAME$)
    Q$=DEVICE$ CAT REHASHNAME$
    NSPC$=DEVICE$ CAT "TEMP.FOR.RENAME"
    RENAME Q$, NSPC$
    RENAME NSPC$, Q$
    RETURN SUBROUTINE
END
        REHASH("DISKMAP.SYS")
        REHASH("SDOS.SYS")
        REHASH("BOOT.SYS")
        REHASH("BADCLUSTERS.SYS")
        REHASH("DIRECTORY.SYS")
        ! NOW SEARCH THE DIRECTORY TO SEE WHERE DIRECTORY.SYS WENT...
        ! NOTE: WE MUST DO THIS ***BEFORE*** WE DISMOUNT THIS DISK!
        OPEN #Disk,DEVICE$ CAT "DIRECTORY.SYS"
        LET DIROFFSET=0 \ ! OFFSET INTO DIRECTORY WE ARE ABOUT TO INSPECT
        REPEAT
            READ #1,DIRECTORYENTRY$
            IF EOF(1)
            THEN
                PRINT "?? CAN'T FIND DIRECTORY.SYS !!"\ERROR 104
            FI
            IF DIRECTORYENTRY$(1,16)="DIRECTORY.SYS   " ...
&              AND DIRECTORYENTRY$(19)>0
            THEN
                ! WE FOUND NEW HASH LOCATION
                CLOSE #Disk \ ! LET GO OF DIRECTORY.SYS
                ! UPDATE BOOT SECTOR TO POINT TO THIS LSN ON DISK
                OPEN #Disk,DEVICE$
                CALL SYSCALL(UNPROT$) \ ! so we can write on the disk
                BOOTDIRLSN=DIRLSN+INT(DIROFFSET/NBPS) \ ! Must be contiguous
                BOOT$[:1B+1]=MSB(MSB(BOOTDIRLSN))
                BOOT$[:1B+2]=LSB(MSB(BOOTDIRLSN))
                BOOT$[:1B+3]=LSB(LSB(BOOTDIRLSN))
                GOSUB DOCHECKSUMWRITEBOOT
                IF (NBPS-1)&DIROFFSET<>0
                THEN
                    ! SIGH... DIDN'T LAND IN 1ST DIRECTORY ENTRY IN SECTOR
                    ! SWAP FOUND DIRECTORY.SYS AND 1ST DIR ENTRY IN SECTOR
                    SYSCALL #1,DISMOUNT$ \ ! Make SDOS let go of DIRECTORY.SYS
                    LEN(BUF$)=2\BUF$(1)=MSB(MAPALG)\BUF$(2)=LSB(MAPALG)
                    SYSCALL SETMAP$,BUF$ \ ! Write according to proper map
                    READ #1@BOOTDIRLSN*NBPS,DIRECTORYENTRY2$
                    WRITE #1@BOOTDIRLSN*NBPS,DIRECTORYENTRY$
                    WRITE #1@BOOTDIRLSN*NBPS+(DIROFFSET&(NBPS-1)),...
&                         DIRECTORYENTRY2$
                FI
                CLOSE #Disk \ ! NOW EVERYTHING SHOULD BE WONDERFUL
                GOTO FILESTRUCTUREPLANTED
            ELSE DIROFFSET=DIROFFSET+32
        END

!^L
FILESTRUCTUREPLANTED: ! FILE SYSTEM IS NOW LOGICALLY CONSISTENT
    INPUT "Is this to be a bootable system disk (Default = NO)? " COPYDISK$
    IF FIND(UPPERCASE$(COPYDISK$),"Y")<>1
    THEN
        DELETE DEVICE$ CAT "SDOS.SYS" \ ! This file clearly not needed.
        GOTO DONE
    FI
!^L
!   Copy various files if user desires:
!        BOOT.SYS
!        SDOS.SYS
!        SERIALNUMBER.SYS
!        DEFAULTPROGRAM

COPYBOOT:
    INPUT   "File to copy into BOOT.SYS (Default = None)? " COPYDISK$
    IF      COPYDISK$="" THEN COPYSDOS
    OPEN    #Disk, DEVICE$ CAT "BOOT.SYS"
    OPEN    #Disk1, COPYDISK$
    LEN(SECTOR$)=16 \ READ #2@0,SECTOR$(1,16)\ WRITE #1@0,SECTOR$(1,16)
    RESTORE #Disk1,:40
    RESTORE #Disk,:40
    GOSUB   COPY2TO1

COPYSDOS:
    INPUT   "File to copy into SDOS.SYS (Default = None)? " COPYDISK$
    IF      COPYDISK$="" THEN COPYSERIALNUMBER
    OPEN    #Disk, DEVICE$ CAT "SDOS.SYS"
    OPEN    #Disk1, COPYDISK$
    GOSUB   COPY2TO1

COPYSERIALNUMBER:
    INPUT   "File to copy for SERIALNUMBER.SYS (Default = None)? " COPYDISK$
    IF      COPYDISK$="" THEN COPYDEFAULT
    CREATE  #Disk, DEVICE$ CAT "SERIALNUMBER.SYS"
    OPEN    #Disk1, COPYDISK$
    GOSUB   COPY2TO1

COPYDEFAULT:
    INPUT   "File to copy for DEFAULTPROGRAM (Default = None)? " COPYDISK$
    IF      COPYDISK$="" THEN DONE
    CREATE  #Disk, DEVICE$ CAT "DEFAULTPROGRAM"
    OPEN    #Disk1, COPYDISK$
    GOSUB   COPY2TO1

!^L
DONE: ! DO LAST MINUTE TOUCH-UP AND QUIT
!
!   MARK ALL SYSTEM FILES AS "WRITE" AND "BACKUP" PROTECTED
!
SUBROUTINE PROTECT(PROTECTNAME$)
    ! SUBROUTINE TO MARK A FILE AS "WRITE" AND "BACKUP" PROTECTED
    DIM PROTECTIONBYTE$/:41/,CCSETFILEPROT$/:E,8,0,:11/
    OPEN #disk,DEVICE$ CAT PROTECTNAME$
    SYSCALL #Disk,CCSETFILEPROT$,PROTECTIONBYTE$
    CLOSE #Disk
    RETURN SUBROUTINE
END

    PROTECT("BOOT.SYS")
    IF ERROR WHEN PROTECT("SDOS.SYS")
    THEN
        IF ERR<>1011
        THEN ERROR
        ELSE ! IGNORE IF SDOS.SYS NOT PRESENT
    FI
    PROTECT("DIRECTORY.SYS")
    PROTECT("DISKMAP.SYS")
    PROTECT("BADCLUSTERS.SYS")
    IF ERROR WHEN PROTECT("SERIALNUMBER.SYS")
    THEN
        IF ERR<>1011
        THEN ERROR
        ELSE ! IGNORE IF SDOS.SYS NOT PRESENT
    FI

    PRINT "Disk initialization complete."
    EXIT

COPY2TO1: ! COPY CONTENT OF FILE ON CHANNEL 2 TO FILE ON CHANNEL 1
    REPEAT
        READ #2,SECTOR$\WRITE #1,SECTOR$
    UNLESS EOF(2) END
    SYSCALL #2, SCGETFILEDATE$,"",FILEDATE$
    SYSCALL #1, CCSETFILEDATE$,FILEDATE$
    CLOSE   #Disk,#Disk1
    RETURN

DOCHECKSUMWRITEBOOT: ! Compute checksum over boot info and write it out
    CHECKSUM=0
    FOR I=17 TO 31 DO CHECKSUM=CHECKSUM+BOOT$[I]
    BOOT$[32]=:FF XOR (CHECKSUM&:FF)
    WRITE #1@0,BOOT$
    RETURN

END
