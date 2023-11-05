               HP 92001B Real-Time Executive (RTE-II) Software Kit
               ===================================================
                        J. David Bryan <jdbryan@acm.org>
                             Last update: 2022-03-31


This package contains a bootable disc image of an HP RTE-II operating system
revision 2440.  The image is in SIMH format, with each 16-bit disc word in
little-endian format, i.e., lower-order byte first.  It is provided for use with
the SIMH HP 2100 simulator that is available at:

  http://simh.trailing-edge.com/hp/

This package requires Release 32 or later of the simulator.  Earlier releases,
or releases based on SCP 4.x, will not work.

Please refer to the following documents from the simulator distribution archive
file when using this kit:

  Filename                  Title
  -----------------------   ------------------------------------
  doc/hp2100_guide.pdf      the "HP 2100 Simulator User's Guide"
  doc/simh_doc.pdf          the "SIMH Users' Guide"
  doc/simh_supplement.pdf   the "SIMH Users' Guide Supplement"
  doc/hp2100_release.txt    the "SIMH/HP 2100 Release Notes"

Problems with the simulator or this software kit should be reported as indicated
in the Release Notes document.


This kit includes the following files:

  Filename                    Description (module name)
  -------------------------   -----------------------------------------------
  RTE-II.7900.disc            the bootable RTE-II system disc image
  RTE-II-Rev-2440.7900.disc   the RTE-II Rev.2440 system modules disc image

  29100-60019_Rev-A.abin      24K SIO Paper Tape Reader Driver  (!S4PHR)
  29100-60020_Rev-A.abin      24K SIO Paper Tape Punch Driver   (!S4PUN)
  29100-60050_Rev-A.abin      24K SIO Console Driver            (!S4TER)
  92001-16013_Rev-1631.abin   RTE-II System Generator Program   (!RTGEN)

  24129-60001_Rev-1643.rbin   ALGOL Compiler part 1             (%ALGOL)
  24129-60002_Rev-C.rbin      ALGOL Compiler part 2             (%ALGL1)
  24998-16001_Rev-1926.rbin   RTE/DOS Relocatable Library       (%RLIB1)
  24998-16002_Rev-1926.rbin   FORTRAN Formatter                 (%FF4.N)
  24998-16009_Rev-1926.rbin   RTE/DOS Relocatable Library       (%RLIB2)
  24998-16011_Rev-1926.rbin   RTE/DOS Relocatable Library       (%RLIB3)
  24999-16171_Rev-1752.rbin   Track Assignment Table Utility    (%LTAT)
  29013-60001_Rev-1710.rbin   7900 Disc Driver                  (%DVR31)
  29028-60002_Rev-1805.rbin   2767A Line Printer Driver         (%DVR12)
  29029-60001_Rev-2301.rbin   Terminal Driver                   (%DVR00)
  92001-16002_Rev-1732.rbin   RTE-II Relocating Loader          (%LDR2)
  92001-16004_Rev-1926.rbin   Power Fail Driver                 (%2DP43)
  92001-16005_Rev-1926.rbin   RTE System Library                (%SYLIB)
  92001-16012_Rev-1926.rbin   Core-Resident Operating System    (%CR2SY)
  92001-16014_Rev-1631.rbin   Auto Restart Program              (%AUTOR)
  92001-16020_Rev-1827.rbin   2607A Line Printer Driver         (%DVA12)
  92001-16029_Rev-1710.rbin   RTE-II Command Program            (%$CMD2)
  92001-16030_Rev-1726.rbin   RTE-II WHZAT Program              (%WHZT2)
  92001-16031_Rev-1926.rbin   RTE-II On-Line Generator          (%RT2G1)
  92002-12001_Rev-2001.rbin   Batch Monitor Program             (%BMPG1)
  92002-16006_Rev-2001.rbin   Batch Monitor Library             (%BMLIB)
  92002-16010_Rev-2140.rbin   RTE Interactive Editor            (%EDITR)
  92060-12004_Rev-1639.rbin   RTE Assembler                     (%ASMB)
  92060-12005_Rev-2140.rbin   Compiler Library                  (%CLIB)
  92060-16028_Rev-A.rbin      Assembler Cross-Referencer        (%XREF)
  92060-16038_Rev-1826.rbin   RTE Switch Program                (%SWTCH)
  92060-16092_Rev-2026.rbin   RTE FORTRAN IV Main               (%FTN4)
  92060-16093_Rev-1913.rbin   RTE FORTRAN IV Segment F          (%FFTN4)
  92060-16094_Rev-2026.rbin   RTE FORTRAN IV Segment 0          (%0FTN4)
  92060-16095_Rev-2001.rbin   RTE FORTRAN IV Segment 1          (%1FTN4)
  92060-16096_Rev-2026.rbin   RTE FORTRAN IV Segment 2          (%2FTN4)
  92060-16097_Rev-1913.rbin   RTE FORTRAN IV Segment 3          (%3FTN4)
  92060-16098_Rev-2026.rbin   RTE FORTRAN IV Segment 4          (%4FTN4)
  92060-16101_Rev-1913.rbin   RTE FORTRAN IV Segment 5          (%5FTN4)
  92202-16001_Rev-2341.rbin   7970 Magnetic Tape Driver         (%DVR23)

  RTE-II.sim                  a script to boot the disc and start the system
  RTGEN.sim                   a script to perform a new system generation
  INIT.sim                    a script to initialize a newly installed system

  pconfig.inc                 an included script to configure the processor

  RTGEN.7900.log              the console log of the system generation
  installation.tape           a magnetic tape image containing example files
  A92001.txt                  the RTE-II Software Numbering File
  readme.txt                  this documentation file

PDF manuals describing the RTE-II commands and utilities are available from
Bitsavers at:

  http://www.bitsavers.org/pdf/hp/

The manuals that pertain to this software and the Bitsavers subdirectories where
they are located are:

                  Manual
  Subdirectory    Number     Title
  ------------  -----------  -------------------------------------------------------
  21xx/rte      92001-93001  RTE-II Software System Programming and Operating Manual
  1000/RTE-III  02116-9072   HP ALGOL Reference Manual
  1000/RTE-III  92060-90023  RTE FORTRAN IV Reference Manual
  21xx/rteIII   92060-90020  RTE-II/III On-Line Generator Reference Manual

Additional PDF manuals are available from the HP Computer Museum at:

  http://www.hpmuseum.net/collection_document.php#CS

The manuals that pertain to this software are:

    Manual
    Number     Title
  -----------  ---------------------------------------
  92002-93001  Real-Time Executive Batch-Spool Monitor
  92060-90014  RTE Interactive Editor Reference Manual

This package was created from products available from:

  http://www.bitsavers.org/bits/HP/HP_1000_software_collection/

...and are provided under the terms of the HP 1000 and HP21XX SOFTWARE AGREEMENT
between Hewlett-Packard Company and the Computer History Museum.



---------------------------
Using the System Disc Image
---------------------------

After unpacking the archive into a directory, the disc image is ready to run
using the HP 2100 simulator and the "RTE-II.sim" simulator command file.
"RTE-II.sim" sets up the simulation environment, attaches the "printer.txt" file
to the simulated HP 2607 line printer, attaches the disc image to the simulated
HP 7900 disc drive, loads and executes the disc bootstrap to load the RTE-II
system and initiate system startup, and supplies the current date and time to
RTE.  At this point, the system console is connected to the simulation console
and is ready to accept RTE system commands.  Typically, the first command is
used to start the file manager, where most work is done.

RTE-II does not require a special system shutdown procedure.  Simply exit from
the file manager, if it is running, and then use CTRL+E to obtain the SCP prompt
and enter "exit" to end the simulation.

Once the HP 2100 simulator has been compiled, entering this command:

  hp2100 RTE-II

...should produce this output on the simulation console:

  SC  Device  Interface Description
  --  ------  -----------------------------------------------------------
  10   TBG    12539C Time Base Generator Interface
  11   DPD    13210A Disc Drive Interface Data Channel
  12   DPC    13210A Disc Drive Interface Command Channel
  13   PTR    12597A-002 Tape Reader Interface
  14   LPT    12845B Line Printer Interface
  15   TTY    12531C Buffered Teleprinter Interface
  16   PTP    12597A-005 Tape Punch Interface
  17   MSD    13183B Digital Magnetic Tape Unit Interface Data Channel
  20   MSC    13183B Digital Magnetic Tape Unit Interface Command Channel

  Loading the disc bootstrap.

  Programmed halt, T: 102077 (HLT 77), P: 02201 (LDA 2017,I)

  Booting the system.

  The character delete key is BACKSPACE.
  The line delete key is DEL.


  SET TIME

  *TM,1994,085,01,45,35
   :EX
   $END FMGR

...where the date and time reflect the current date and time of the host system
with a year selected from the 20th century whose days of the week and leap-year
status match those of the current year.

Two example programs are provided as source files on file manager cartridge 2.
The "TENA" program is an ALGOL program that calculates and prints the square
roots of the integers from 1 to 10.  The "TENF" program is the same program
but written in FORTRAN IV.  They may be compiled and run as RTE-II programs from
the system console after starting the file manager.

First press a key on the console (the space bar is the most convenient) to
obtain the system prompt, which is an asterisk.  Then enter the "ON" command to
schedule the file manager program, as follows:

  *ON,FMGR
  :

FMGR responds with its prompt (a colon).  To verify that the system is running
properly, run the "WHZAT" system status program:

  :RU,WHZAT
  19: 7:27: 60
  **********************************************************************
  PT SZ PRGRM,T,PRIOR*DRMT*SCHD*I/O *WAIT*MEMY*DISC*OPER * NEXT TIME   *
  **********************************************************************
   0 ** WHZAT*2*00001 ***** 1
   0 ** FMGR *3*00090 *************** 3, WHZAT
  **********************************************************************
  DOWN LU'S
  **********************************************************************
  DOWN EQT'S
  **********************************************************************
  19: 7:27: 60
  :

This display shows that program FMGR (a type 3, or background disc resident,
program at priority 90) is waiting for program WHZAT to complete, and program
WHZAT (a type 2, or foreground disc resident, program at priority 1) is
scheduled for execution.  It also shows that there are no logical units or
equipment table entries that are "down" (i.e., offline or unavailable).

Now set the logical list device to the system console (logical unit 1) and list
the cartridge and disc directories:

  :LL,1
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003

  :DL
  CR=00002
   ILAB=SYSTEM NXTR=0100 NXSEC=084 #SEC/TR=096 LAST TR= 0202 #DR TR=01

  NAME   TYPE #BLKS/LU OPEN TO

  WELCOM 00004 00001
  &TENA  00004 00002
  &TENF  00004 00002
  %LTAT  00005 00019
  #RTGEN 00004 00018

  CR=00003
   ILAB=AUX    NXTR=0000 NXSEC=000 #SEC/TR=096 LAST TR= 0202 #DR TR=01

  NAME   TYPE #BLKS/LU OPEN TO


  :

By default, the logical list device is LU 6, the line printer.  If the "LL"
command is not used, the directory list will appear there.  The list device may
be switched back and forth with "LL,1" and "LL,6" commands as desired.

To compile the TENA program with the ALGOL compiler, the source file must first
be moved to the logical source tracks, and a load-and-go area must be allocated
to receive the compiled object code:

  :MS,&TENA
  FMGR 015
   LS LU 2 TRACK 034
  :LG,1
  :

The "LG" command above allocates a one-track load-and-go area.  Larger
allocations are needed for larger programs.

Now run the ALGOL compiler, specifying source input from the logical source area
of the disc (the "2" parameter), listing output to the console ("1"), and
relocatable output to the load-and-go area of the disc ("99"), as follows:

  :RU,ALGOL,2,1,99

  PAGE  001


  001 00000 HPAL,L,"TENA"
  002 00000 BEGIN
  003 00001    COMMENT THIS PROGRAM PRINTS THE SQUARE ROOTS
  004 00001            OF THE FIRST TEN INTEGERS;
  005 00001    INTEGER I;
  006 00003    REAL R;
  007 00005    FORMAT F1(I3, 3X, F10.5);
  008 00014    WRITE(1, #("HELLO FROM ALGOL!  HERE ARE SOME SQUARE ROOTS:"/));
  009 00102    FOR I := 1 STEP 1 UNTIL 10 DO
  010 00110      BEGIN
  011 00110        R := SQRT (I);
  012 00116        WRITE (1, F1, I, R);
  013 00131      END;
  014 00135 END$

  PROGRAM= 000141  ERRORS=000
  $END ALGOL

  :

Link the program into an executable file with the relocating loader, specifying
input from the load-and-go area and system library (the "99" parameter) and list
output to the console ("1"):

  :RU,LOADR,99,1
    TENA   36002 36142

    FMTIO  36143 37441   24998-16002 REV.1926 790417
    ERR0   37442 37531   771122  24998-16001
    SQRT   37532 37633   780424  24998-16001
    .FLUN  37634 37651  750701  24998-16001
    .SBT   37652 37712   770518  24998-16001
    .PWR2  37713 37743  781106  24998-16001
    PNAME  37744 40011   771121  24998-16001
    ER0.E  40012 40012  750701  24998-16001
    FRMTR  40013 43450   24998-16002 REV.1926 790503
    FMT.E  43451 43451   24998-16002 REV.1901 781107
    REIO   43452 43556  92001-16005 780212
    .DFER  43557 43630  750701  24998-16001
    .ENTR  43631 43720  750701  24998-16001
    $SETP  43721 43745   781106  24998-16001
    .CFER  43746 44023  750701  24998-16001
    .XPAK  44024 44207  750701  24998-16001
    .LBT   44210 44240   770518  24998-16001

    ENTRY POINTS

     *TENA   36002
     *.DIO.  36652
     *.DTA.  37071
     *FLOAT 105120
     *SQRT   37532
     *ERR0   37442
     *.IOI.  36417
     *.IOR.  36440
     *EXEC   10374
     *.IOJ.  36430
     *.IIO.  36455
     *.JIO.  36457
     *.RIO.  36461
     *.XIO.  36463
     *.TIO.  36465
     *.IAR.  36477
     *.JAR.  36501
     *.RAR.  36503
     *.XAR.  36505
     *.TAR.  36507
     *.IAY.  36522
     *.JAY.  36524
     *.RAY.  36526
     *.XAY.  36530
     *.TAY.  36532
     *.BIO.  36772
     *NEWIO  37015
     *OLDIO  37022
     *CODE   36616
     *ACODE  36616
     *ITLOG  37311
     *ISTAT  37316
     *LGBUF  37332
     *.FRMN  40202
     *.LS2F  40226
     *.INPN  40211
     *.DTAN  40243
     *FMT.E  43403
     *PNAME  43654
     *REIO   43410
     *.SBT   37655
     *ER0.E  37744
     *.ZPRV  02001
     *.FLUN  37634
     *.PWR2  37713
     *.FMP  105040
     *.FAD  105000
     *.FDV  105060
     *.CFER  43746
     *.XPAK  44031
     *$SETP  43721
     *.ZRNT  02001
     *IFIX  105100
     *.LBT   44212
     *.DFER  43511
     *$LIBR  10606
     *$LIBX  11300
     *.ENTR  43572
     *.ENTP  43563
     *$OPSY  04332
   /LOADR:TENA  READY
   /LOADR:$END


  :

The program is now ready to run:

  :RU,TENA
  HELLO FROM ALGOL!  HERE ARE SOME SQUARE ROOTS:

    1      1.00000
    2      1.41421
    3      1.73205
    4      2.00000
    5      2.23607
    6      2.44949
    7      2.64575
    8      2.82843
    9      3.00000
   10      3.16228
  :

The FORTRAN IV compiler is a newer subsystem that supports direct specification
of the source, list, and object files.  To compile the TENF program, run the
FORTRAN IV compiler, specifying source input filename or LU, the listing output
filename or LU, and relocatable output filename or LU, as follows:

  :RU,FTN4,&TENF,1,%TENF

This says to compile the &TENF source file, produce a listing on LU 1 (the
console), and write the object code to the %TENF file.  The listing appears on
the console:

   PAGE 0001  FTN.   8:14 PM  FRI., 19  FEB., 1993

  0001  FTN4,L
  0002        PROGRAM TENF
  0003  C
  0004  C  THIS PROGRAM PRINTS THE SQUARE ROOTS
  0005  C    OF THE FIRST TEN INTEGERS.
  0006  C
  0007        WRITE (1,1)
  0008  1     FORMAT ("HELLO FROM FTN4!  HERE ARE SOME SQUARE ROOTS:"/)
  0009  C
  0010        DO 20 I=1,10
  0011        R = SQRT (FLOAT (I))
  0012        WRITE (1,10) I,R
  0013  10    FORMAT (I3, 3X, F10.5)
  0014  20    CONTINUE
  0015        END


    FTN4 COMPILER: HP92060-16092 REV. 2026 (800423)


    **  NO WARNINGS **  NO ERRORS **   PROGRAM = 00075      COMMON = 00000



   PAGE 0002  FTN.   8:14 PM  FRI., 19  FEB., 1993

  0016        END$


   $END FTN4: NO DISASTRS   NO ERRORS   NO WARNINGS
  :

The linker is an older subsystem that still uses the load-and-go area of the
disc for input.  So first allocate a track and then move the relocatable file
there:

  :LG,1
  :MR,%TENF
  :

Now link the program:

  :RU,LOADR,99,1
    TENF   36002 36114

    FMTIO  36115 37413   24998-16002 REV.1926 790417
    CLRIO  37414 37422  750701  24998-16001
    ERR0   37423 37512   771122  24998-16001
    SQRT   37513 37614   780424  24998-16001
    .FLUN  37615 37632  750701  24998-16001
    .SBT   37633 37673   770518  24998-16001
    .PWR2  37674 37724  781106  24998-16001
    ER0.E  37725 37725  750701  24998-16001
    FRMTR  37726 43363   24998-16002 REV.1926 790503
    FMT.E  43364 43364   24998-16002 REV.1901 781107
    REIO   43365 43471  92001-16005 780212
    .DFER  43472 43543  750701  24998-16001
    .ENTR  43544 43633  750701  24998-16001
    PNAME  43634 43701   771121  24998-16001
    $SETP  43702 43726   781106  24998-16001
    .CFER  43727 44004  750701  24998-16001
    .XPAK  44005 44170  750701  24998-16001
    .LBT   44171 44221   770518  24998-16001

    ENTRY POINTS

     *TENF   36002
     *.DST  104400
     *.DIO.  36624
     *.RIO.  36433
     *.IIO.  36427
     *.DTA.  37043
     *SQRT   37513
     *FLOAT 105120
     *ERR0   37423
     *EXEC   10374
     *CLRIO  37414
     *.IOI.  36371
     *.IOJ.  36402
     *.IOR.  36412
     *.JIO.  36431
     *.XIO.  36435
     *.TIO.  36437
     *.IAR.  36451
     *.JAR.  36453
     *.RAR.  36455
     *.XAR.  36457
     *.TAR.  36461
     *.IAY.  36474
     *.JAY.  36476
     *.RAY.  36500
     *.XAY.  36502
     *.TAY.  36504
     *.BIO.  36744
     *NEWIO  36767
     *OLDIO  36774
     *CODE   36570
     *ACODE  36570
     *ITLOG  37263
     *ISTAT  37270
     *LGBUF  37304
     *.FRMN  40163
     *.LS2F  40207
     *.INPN  40172
     *.DTAN  40224
     *FMT.E  43364
     *PNAME  43635
     *REIO   43371
     *.SBT   37636
     *ER0.E  37725
     *.ZPRV  02001
     *.FLUN  37615
     *.PWR2  37674
     *.FMP  105040
     *.FAD  105000
     *.FDV  105060
     *.CFER  43727
     *.XPAK  44012
     *$SETP  43702
     *.ZRNT  02001
     *IFIX  105100
     *.LBT   44173
     *.DFER  43472
     *$LIBR  10606
     *$LIBX  11300
     *.ENTR  43553
     *.ENTP  43544
     *$OPSY  04332
   /LOADR:TENF  READY
   /LOADR:$END


  :

 Finally, run the program:

  :RU,TENF
  HELLO FROM FTN4!  HERE ARE SOME SQUARE ROOTS:

    1      1.00000
    2      1.41421
    3      1.73205
    4      2.00000
    5      2.23607
    6      2.44949
    7      2.64575
    8      2.82843
    9      3.00000
   10      3.16228
  :

Both the TENA and TENF programs have been stored temporarily on system disc
tracks and so do not appear in the file manager listing:

  CR=00002
   ILAB=SYSTEM NXTR=0100 NXSEC=090 #SEC/TR=096 LAST TR= 0202 #DR TR=01

  NAME   TYPE #BLKS/LU OPEN TO

  WELCOM 00004 00001
  &TENA  00004 00002
  &TENF  00004 00002
  %LTAT  00005 00019
  #RTGEN 00004 00018
  %TENF  00005 00003

  :

However, they can be stored in the file manager area with a Save Program command
and then removed from the system disc area with an OFF command:

  :SP,TENA
  :SP,TENF
  :OF,TENA
  TENA  ABORTED
  :OF,TENF
  TENF  ABORTED
  :

A new directory listing for CR 2 now shows the programs have been saved:

  :DL,2
  CR=00002
   ILAB=SYSTEM NXTR=0102 NXSEC=014 #SEC/TR=096 LAST TR= 0202 #DR TR=01

  NAME   TYPE #BLKS/LU OPEN TO

  WELCOM 00004 00001
  &TENA  00004 00002
  &TENF  00004 00002
  %LTAT  00005 00019
  #RTGEN 00004 00018
  %TENF  00005 00003
  TENA   00006 00029
  TENF   00006 00029

  :

The programs have been removed from the system disc tracks, but they can
still be run from FMGR:

  :RU,TENA
  HELLO FROM ALGOL!  HERE ARE SOME SQUARE ROOTS:

    1      1.00000
    2      1.41421
    3      1.73205
    4      2.00000
    5      2.23607
    6      2.44949
    7      2.64575
    8      2.82843
    9      3.00000
   10      3.16228
  :RU,TENF
  HELLO FROM FTN4!  HERE ARE SOME SQUARE ROOTS:

    1      1.00000
    2      1.41421
    3      1.73205
    4      2.00000
    5      2.23607
    6      2.44949
    7      2.64575
    8      2.82843
    9      3.00000
   10      3.16228
  :

...but not from the system:

  :EX
   $END FMGR

  *ON,TENA
  NO SUCH PROG

  *ON,TENF
  NO SUCH PROG

Before exiting the simulator, exit the file manager (as above), and then the
simulator may be exited by pressing CTRL+E to get the SCP prompt and then
entering the "exit" command:

  scp> exit

  Simulation stopped, P: 02073 (JMP 2073)
  Goodbye
  Log file closed

A log of the system console session has been saved in the "RTE-II.log" file, and
the "printer.txt" file contains the output to the system line printer (LPT
device), which simulates an HP 2607.

The supplied disc image is divided into two file manager "cartridges,"
designated "CR 2" and "CR 3."  CR 2 corresponds to the area of the fixed platter
not used by the operating system.  CR 3 corresponds to the 7900 removable
platter and occupies the entire area.  The FMGR master security code is "RT".

The system also provides a paper tape punch as logical unit 4, a paper tape
reader as LU 5, a line printer as LU 6, and a magnetic tape unit as LU 8.



-------------------------
Using the Peripheral Disc
-------------------------

A second 7900 disc unit (unit 1) is supported.  LU 10 references the lower
platter, and LU 11 references the upper.  When the system is started with the
"RTE-II.sim" command file, unit 1 (simulator unit DPC1) is automatically
attached to the supplied disc image named "RTE-II-Rev-2440.7900.disc".  This
disc contains the RTE-II system modules and is used with the online system
generator to create new RTE-II systems.

Attaching is equivalent in hardware to inserting the disc pack into the drive
and setting the LOAD/UNLOAD switch to the LOAD position, with the exception that
the disc image contains both the upper and lower platters.  Once attached, the
FMGR :MC (Mount Cartridge) command is used to make the two platters available to
RTE:

  :MC,-10
  :MC,-11
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003
    10    0202     32767
    11    0202     00011

  :

The cartridge list shows that the two platters have cartridge reference numbers
32767 and 11.

If a different peripheral disc is preferred, press CTRL+E to get the SCP prompt
and then enter an ATTACH command that specifies the desired disc image; for
example:

  scp> attach -E DPC1 RTE-II-auxiliary-pack.7900.disc

The -E switch ensures that the disc image exists; the attach fails if it does
not.

If a previously attached disc image has been mounted with :MC commands, the
cartridges MUST be dismounted with :DC (Dismount Cartridge) commands before
swapping disc packs:

  :DC,-10
  LAST TRACK  0202
  :DC,-11
  LAST TRACK  0202
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003

  :

If a new, blank disc image is desired instead, use a command such as this:

  scp> attach -N DPC1 RTE-II-scratch-pack.disc

The -N switch creates a new, formatted disc image.

Once the image is attached, the mount commands must be followed by FMGR :IN
(Initialize) commands to set up the file systems on the cartridges:

  :MC,-10
  :MC,-11
  :LL,1
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003
    10    0202     00000  FMGR
    11    0202     00000  FMGR

  :

Note that before initialization, the cartridge reference numbers are zero, and
the cartridges are locked to the FMGR program.  After initialization:

  :IN,RT,-10,123,MYDISC
  :IN,RT,-11,456,YRDISC
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003
    10    0202     00123
    11    0202     00456

  :

...the cartridge numbers are initialized as directed, and the cartridges are
unlocked.

Cartridge mounting persists between RTE-II sessions.  Because the "RTE-II.sim"
command file always attaches the system modules disc, it is imperative that if
any other disc image is in use, it must be dismounted before shutting down RTE:

  :DC,-10
  LAST TRACK  0202
  :DC,-11
  LAST TRACK  0202
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003

  :

Then it is safe to exit FMGR and then the simulator:

  :EX
   $END FMGR

  scp> exit

  Simulation stopped, P: 02073 (JMP 2073)
  Goodbye
  Log file closed

If a different disc image had been inadvertently left mounted when the simulator
was exited, dismount the two cartridges immediately upon restarting before any
other disc activity is attempted.  Otherwise, the system modules disc image may
be corrupted.



----------------------------------
Generating a New System Disc Image
----------------------------------

RTE-II supports two ways of generating a new system: offline and online
generation.  Offline generation can be used to create an RTE system from
scratch.  It uses a generator program and system modules stored on paper tape
and writes the new system directly to the disc.  Online generation requires a
running RTE-II system to run the generator, which produces a new system file
that must be installed on the system disc as a separate step.  Offline
generation is faster and does not need a preexisting system.  Online generation
is easier, as generation scripts are stored and can be edited as normal FMGR
source files.

The supplied disc image was created with the offline system generator by running
the simulator with the "RTGEN.sim" command file:

  hp2100 RTGEN

The file performs a new system generation from paper tape, initializes the user
area of LU 2 and LU 3, and loads the example programs.  The new system is then
written to the "RTGEN.7900.disc" file.  The system generation console log
contained in the "RTGEN.7900.log" file may be examined to see how RTE-II was
configured.

The new disc image file must be manually renamed to "RTE-II.7900.disc" before
using the "RTE-II.sim" command file to boot RTE.  This allows a new system to
be generated without disturbing the existing system.

The example programs are loaded from the "installation.tape" image file.  This
file contains the source statements of each example program.


The same system may be generated online using the script stored in the #RTGEN
source file provided on CR 2.  The disc image containing the system modules must
be attached to DPC1 first (this happens automatically if the "RTE-II.sim"
command file is used) and then mounted as described in the previous section:

  :MC,10
  :MC,11
  :CL
    LU  LAST TRACK   CR   LOCK

    02    0202     00002
    03    0202     00003
    10    0202     32767
    11    0202     00011

  :

The RTE-II distribution files reside on CR 32767.  One extra utility program,
the Track Assignment Table Utility, is provided as file %LTAT on CR 2.

The online generator is then run from the system console as follows:

  :RU,RT2GN
  LIST FILE?
  TR,#RTGEN

    EST. # TRACKS IN OUTPUT FILE?
    35                        * EST # TRACKS

    OUTPUT FILE NAME?
    !RTGEN,,2,                * OUTPUT FILE

    TARGET DISK?
    7900                      * TARGET DISC

    MH DISC CHNL?
    [...]

The "TR" (Transfer) command issued in response to the LIST FILE? prompt tells
the generator to transfer control to the named file containing the answers to
all of the generator questions.  Once transferred, generation proceeds
automatically, listing its progress to the system console as well as to a log
file named 'RTGEN on CR 2.  The new system is stored in the file named !RTGEN on
CR 2.

Once generation is complete:

    [...]
    SYSTEM STORED ON DISC
    SYS SIZE: 33 TRKS, 036 SECS(10)

    RT2GN FINISHED
  RT2GN FINISHED
  :

...and presuming that no generation errors were reported, the new system may be
installed on the system disc with the supplied SWTCH program:

  :RU,SWTCH

                ******  W A R N I N G  ******
  ALL ACTIVITY MUST BE TERMINATED BEFORE SYSTEM TRANSFER PROCESS.

  FILE NAME OF NEW RTE SYSTEM?
  !RTGEN::2

  NEW SYSTEM I/O CONFIGURATION:

  CHANNEL 10 TBG
  CHANNEL 04 TYPE=43
  CHANNEL 11 TYPE=31
  CHANNEL 13 TYPE=01
  CHANNEL 14 TYPE=12
  CHANNEL 15 TYPE=00
  CHANNEL 16 TYPE=02
  CHANNEL 17 TYPE=23
  CHANNEL 36 TYPE=00
  CHANNEL 37 TYPE=00

  NEW SYSTEM (LU2) CHANNEL= 11  SUBCHANNEL= 00

  7900 LOGICAL SUBCHANNEL 0  FIRST TRACK# 0000  #TRACKS 0203

  TARGET CHANNEL FOR NEW SYSTEM?   (XX OR " "CR)
  11

  TARGET SUBCHANNEL(LOGICAL)/UNIT FOR NEW SYSTEM?   (X OR " "CR)
  2

  NOW IS THE TIME TO INSERT CORRECT CARTRIDGE IN
  TARGET SUBCHANNEL/UNIT.   (" "CR TO CONTINUE)

  [CTRL+E]

  scp> attach -n dpc1 RTGEN.7900.disc

  [enter a space and CR]

  SAVE FILES AT TARGET?   (Y OR N)
  NO
  PRESENT CONFIGURATION DOESN'T PERMIT AUTO BOOT-UP.
  READY TO TRANSFER. OK TO PROCEED?
  YES

  SWTCH FINISHED

  :

To complete the new system initialization and to load the example programs from
the "installation.tape" image file, exit FMGR and the simulator and then restart
the simulator with the "INIT.sim" command file:

  hp2100 INIT

As with offline generation, the new disc image file must be manually renamed to
"RTE-II.7900.disc" before using the "RTE-II.sim" command file to boot RTE.

If you prefer to initialize the new system manually, then instead of running the
"INIT.sim" command file, rename the disc image file as above and then start
RTE-II normally:

  hp2100 RTE-II

When a new system is started for the first time, the FMGR areas on the system
and auxiliary discs must be initialized.  FMGR prompts for this after bootup:

  Booting the system.

  The character delete key is BACKSPACE.
  The line delete key is DEL.


  SET TIME

  *TM,1994,085,01,45,35
  FMGR 002
  :

After entering an :IN (Initialize) command to initialize the system disc (LU 2),
FMGR prompts to initialize the auxiliary disc (LU 3) if it was defined in the
system generation:

  :IN,RT,-2,2,SYSTEM,100
  FMGR 003
  :

After initializing LU 3, FMGR reports a "file not found" error because the
system WELCOM file is not yet present:

  :IN,RT,-3,3,AUX,0
  FMGR-006
  :

The initialization requests occur only at the first bootup.  The FMGR-006 error
occurs each time until a WELCOM file is created.  For example:

  :CR,WELCOM::2:4:1
  :

This creates an empty WELCOM file, which will cause FMGR to remain running after
system startup.  If you prefer to have FMGR automatically exit after processing
commands in the WELCOM file, add an :EX command to the end of the file.  The
WELCOM file supplied with the distribution disc image consists of a single :EX
command:

  :LI,WELCOM
  WELCOM T=00004 IS ON CR00002 USING 00001 BLKS R=0000

  0001  :EX

  :



-----------
Usage Notes
-----------

 1. RTE-II is not year-2000 compliant.  While the system will accept and display
    years >= 2000 properly, leap-years will not be calculated correctly, and all
    subsystems will display years erroneously (typically, punctuation characters
    appear in the years, e.g., "19:0" for 2000).  The "RTE-II.sim" script
    selects a year in the 20th century whose days of the week and leap-year
    status are identical to the current year (for example, February 29, 2020 is
    a Saturday, as is February 29, 1992).

 2. The "INIT.sim" command file is run automatically by the "RTGEN.sim" file as
    a part of offline system generation.  It must be run manually only if online
    system generation is done.

 3. The programs preinstalled in the system area of the disc image are:

      * FMGR  -- the file manager
      * LOADR -- the relocating loader
      * WHZAT -- a utility to list program and partition status
      * LTAT  -- a utility to list the disc track allocation tables
      * EDITR -- the text editor
      * ASMB  -- the HP 21xx/1000 assembler
      * XREF  -- the assembler cross-referencer
      * ALGOL -- the ALGOL compiler
      * FTN4  -- the FORTRAN IV compiler
      * RT2GN -- the online system generator
      * SWTCH -- the online system installer

 4. Using the &AN2F0 and &AN2F5 answer files directly to generate and install
    new systems is not recommended.  These systems are configured for an HP 2644
    terminal as the system console, and the binary input and output devices (LUs
    5 and 4), which are normally the paper tape reader and punch, are redirected
    to the left and right cartridge tape units on the terminal.  Many system
    programs, such as LOADR, default to using the binary devices unless
    explicitly overridden.  Attempting a binary read on the terminal when it
    does not support CTUs, which most HP terminal emulators do not, will result
    in a hung terminal and no way to recover other than exiting and restarting
    the simulator.
