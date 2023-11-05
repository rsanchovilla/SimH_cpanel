                         HP 29016D RTE OPERATING SYSTEM
                   ===========================================
                   Roberto Sancho and J. David Bryan, Mar 2017


This package contains a bootable disc image of the first version of the
Real-Time Executive (RTE) Operating System for the HP 2116. It is available for
the fixed-head (FH) HP 2774A Drum Memory and the moving-head (MH) HP 7900
Cartridge Disc Drive.

RTE originally shipped in 1968 under product number 20688. In 1971, it was
re-released supporting the newly launched HP 7900 disc drive under product
number 29016. The version in this package is the 1971 one; the 1968 version is
lost.

This package runs on the HP2100 simulator, available from the SIMH project at:

  https://github.com/simh/simh

Release 25 (dated 2017-01-11) of the HP2100 simulator or later is required.
Sources and precompiled binaries are available from the project site.

This package includes:

  - Fixed-head and moving-head disc images with preinstalled software

  - The binary paper tape images and the console response listing used to
    generate the disc images.

  - The source paper tape images that can be assembled using RTE as a
    self-hosting environment.

  - Ready-to-use test run script files with ALGOL and HP Assembler "Hello
    World" programs that demonstrate compiling and running.

This package was created from products available from:

  http://www.bitsavers.org/bits/HP/HP_1000_software_collection/


------------------------
RUNNING UNDER SIMULATION
------------------------

Several SIMH command files are included:

  * rte_fh.ini - Run RTE-FH on the fixed-head disc image (768 KW)

  * rte_mh.ini - Run RTE-MH on the moving-head disc image (4.5 MB)

  * rtgen_fh.ini - Generate an RTE-FH disc image using the distribution
                   binary paper tape images

  * rtgen_mh.ini - Generate an RTE-MH disc image using the distribution
                   binary paper tape images

  * rte_build.ini - Generate the OS distribution binary paper tape images by
                    assembling the source tape images

System Generation, OS assembling, and test are fully automated. 

Files and folders in the package:

  - *.log       : SimH execution logs
  - *.ini       : simulator command files
  - RTE-FH.disc : disc image file with RTE-FH installed
  - RTE-MH.disc : disc image file with RTE-MH installed
  - Rtgen\      : directory with paper tape image files for system generation
  -    *.abs    : absolute tape images
  -    *.rel    : relocatable tape images
  -    sio\     : directory with SIO tape images needed for system generation
  -    sxl\     : directory with System Cross Loader tape images
  -    src\     : directory with RTE sources, FMP sources, STAT sources
  - Sample      : directory with sample source files for test runs during 
                  system genetation


-----------------
I/O CONFIGURATION
-----------------

The supplied operating system uses this I/O configuration:

  I/O slot 10 = 12597A Duplex Register and a 2748B Paper Tape Reader
  I/O slot 11 = 12539C Time Base Generator
  I/O slot 16 = 12531C TTY Interface (system console)
  I/O slot 17 = 12597A Duplex Register and a 2895B Paper Tape Punch
  I/O slot 20 = 12845B Line Printer Interface and a 2607/13/17/18 line printer
  I/O slot 22 = 12610B Drum Memory Interface and a 2774 drum (FH), or
                13210A Disc Drive Interface and a 7900 drive (MH)


------------------
INSTALLED PROGRAMS
------------------

For RTE-FH, these programs are installed:

  * ASMB  - assembler (product number 20874)

  * FTN   - Fortran compiler (product number 20875)

  * ALGOL - Algol compiler (product number 24129)

  * EDIT  - RTE text editor (product number 20805)


For RTE-MH, the four programs above plus these programs are installed:

  * FMP   - File Management Package (product number 29033 Rev A)

  * STATS - a WHZAT-like system status program

  * SXL   - System Cross Loader (product number 29103 Rev A)

The FMP package provides a file system for RTE that allows six-character named
files. Without this package, it is responsibility of programs to manage the
disc space track by track and sector by sector. Nevertheless, FMP is an
optional add-on to RTE (that later became a mandatory part of the system). FMP
is only installed for RTE-MH, so it is possible to experience the bare, basic
file-less installation in the RTE-FH system.

The FMP master security code is set to "RT".

SXL is the RTE System Cross Loader that allows RTE to relocate programs for RTE,
BCS, and TCE/3. A patch has been applied to the TERMF routine to allow correct
operation on the test runs.

STAT is a WHZAT-like command. It has no product number. It is an ALGOL program
listed on page 163 of the "Real-Time Multiprogramming System Student's Manual,
Level 1" (HP part number 5951-2134, January 1972). The source files are in the
"src" directory. The WHZAT program does not exist in the first RTE version.


---------
TEST RUNS
---------

The following test runs are done during system generation:

  * FH generation - Loads the "algol.alg.txt" sample "Hello world" program,
                    compiles it with the ALGOL compiler, relocates it with the
                    standard RTE loader, and then executes it.

  * MH generation - Loads the "asmb.alg.txt" sample "Hello world" program,
                    assembles it with ASMB, relocates it with the SXL cross
                    loader, and then executes it.


---------------------
BUILDING FROM SOURCES
---------------------

The "rte_build.ini" simulation script boots the RTE-MH system to assemble the
provided source files into the listed binary paper tape image files needed for
system generation:

  * STATS - a WHZAT-like system status program
              stats.rel

  * FMP   - the File Management Package
              29033-A_FMP.rel

  * DVR31 - the Moving Head Disc Driver
              29013-60001_RTE_DVR31.rel

  * DVR00 - the TTY, Paper Tape Reader, and Paper Tape Punch driver
              29029-60001_RTE_DVR00.rel

  * RTE   - the Real Time Executive Operating System
              29016-RTE_CORE_EXEC.rel
              29016-RTE_CORE_RTIOC.rel
              29016-RTE_CORE_SCHED.rel

  * ASMB  - the RTE/DOS Assembler main and segments
              20874-60001_ASMB.rel
              20874-60002_ASMBD.rel
              20874-60003_ASMB1.rel
              20874-60003_ASMB2.rel
              20874-60003_ASMB3.rel
              20874-60003_ASMB4.rel
              20874-60003_ASMB5.rel

These paper tape images are then loaded during system generation and stored on
the disc.


------------------------
ADDITIONAL DOCUMENTATION
------------------------

Many RTE manuals are available from the HP Computer Museum at:

  http://www.hpmuseum.net/display_item.php?hw=519

The manuals of most relevance to the included system are:

  Part Number  Manual Title
  -----------  ----------------------------------------------------
  02116-9139   Real Time Software, a Reference Text for Programmers
  29033-98000  Real-Time Executive File Manager System
  29103-93001  Real-Time Executive System Cross Loader
  02005-90001  Real-Time Executive Software System


--------------
SPECIAL THANKS
--------------

We must thank David Collins, the curator of the HP Computer Museum,
for his help with the restoration of RTE. The relocatable loader
paper tape is a key component of the operating system but is missing
from the HP 1000 Software Collection at Bitsavers. The HP Museum has
in its collection the only physical paper tape of this program known
to exist. David kindly dumped it to an electronic format to allow RTE
to "ride" again.
