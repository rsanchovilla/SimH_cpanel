                HP 92068A RTE-IVB REVISION 5010 OPERATING SYSTEM
                ================================================
                  J. David Bryan <jdbryan@acm.org>, 26 Jan 2014


This package contains a bootable disc image of an HP 1000 RTE-IVB revision 5010
operating system.  Included in the image are the full set of RTE relocatables,
the system generator program, and the generator "answer file" used to create the
system.  The image may be booted directly on an HP 1000 either by copying it to
an HP 7920H ICD Disc Drive or by emulating a 7920H with the HPDrive program.  If
the image is byte-swapped, it may also be run under the SIMH simulator with the
included simulator control file.

This package was created from products available from:

  http://www.bitsavers.org/bits/HP/HP_1000_software_collection/


-------------------
RUNNING ON HARDWARE
-------------------

To run this system on hardware, the following minimum configuration is required:

  - HP 1000 M/E/F CPU
  - 96K words of memory
  - 12992H ICD Disc Loader ROM
  - 12892B Memory Protect
  - 12897B Dual-Channel Port Controller
  - 12731A Memory Expansion Module and DMS firmware
  - 12539C Time Base Generator
  - 12821A HP-IB Disc Interface
  - 12966A BACI or 12531A/B/C/D TTY or 12880A CRT Interface
  - 7920H 50 MB ICD Disc Drive or equivalent emulation
  - an HP terminal or terminal emulator (BACI interface), Teletype (TTY
    interface), or serial terminal (CRT interface)

No additional firmware (e.g., EMA firmware) is required.

If a real 7920H is used, the HPDir program from Ansgar Kueckes:

  http://www.hp9845.net/9845/projects/hpdir/

...may be used to copy the included image to the drive with the "dup" command.
For example:

  hpdir -dup RTE-IVB-SYSTEM.U0.7920H.disc 700:

A 7925H drive may be used in lieu of the 7920H, although RTE will use only 50 MB
of the available 120 MB unless the system is regenerated to use the larger
capacity.  The drive must be set to respond on HP-IB address 0.

The HPDrive program, also from Ansgar Kueckes:

  http://www.hp9845.net/9845/projects/hpdrive/

...may be used to emulate a 7920H using a Windows PC and a GPIB card residing in
the PC; refer to the section titled "Configuring for the HP 1000" for details.
For example:

  hpdrive -nosleep -7920H RTE-IVB-SYSTEM.U0.7920H.disc

Once the GPIB card is connected to the 12821A Disc Interface and HPDrive is
started to emulate a 7920H, the procedure for booting and running RTE is
identical to that when using a real drive.


------------------------
RUNNING UNDER SIMULATION
------------------------

If the required hardware is not available, a PC running SIMH, the Computer
History Simulator, may be used to simulate the required HP 1000 and peripheral
hardware on a PC.  Version 3.9-0 or later is required; precompiled binaries of
the "HP2100" simulator (which also supports the 1000) are available.  See:

  http://simh.trailing-edge.com/

A SIMH command file is included to configure the simulator and boot the disc
image.  However, note that SIMH requires the disc image to be in little-endian
format, whereas the 7920H and HPDrive require big-endian format.  Therefore, the
supplied image must be byte-swapped before it can be used with SIMH.  The Unix
"dd" command, e.g., may be used for this:

  dd if=RTE-IVB-SYSTEM.U0.7920H.disc of=RTE-IVB-SIMH.U0.7920H.disc conv=swab status=noxfer

A Windows version of "dd" is available; download the GNU Coreutils "bin"
(binary) and "dep" (dependencies) packages from:

  http://sourceforge.net/projects/gnuwin32/files/coreutils/


-----------------
I/O CONFIGURATION
-----------------

The supplied operating system uses this I/O configuration:

  I/O slot 10 = 12791A Firmware Expansion Module
  I/O slot 11 = 12539C Time Base Generator
  I/O slot 12 = 12821A HP-IB Disc Interface
  I/O slot 13 = 13181A/13183A 7970 Magnetic Tape Data Interface
  I/O slot 14 = 13181A/13183A 7970 Magnetic Tape Control Interface
  I/O slot 15 = 12966A Buffered Asynchronous Communications Interface
  I/O slot 16 = 12880A CRT or 12531A/B/C/D TTY Interface (system console)
  I/O slot 17 = 12597A Duplex Register for 2748B Paper Tape Reader
  I/O slot 20 = 12597A Duplex Register for 2895B Paper Tape Punch

The FEM is typically found in E/F-Series systems, although it is not required.
If it is not present, the system may be reconfigured at boot time to place the
desired system console terminal interface in slot 10, or a 12777A Priority
Jumper Card may be used to fill the slot (note that a 1000 CPU must have no
empty slots between I/O cards; this restriction is not present under SIMH).

Similarly, if the mag tape is not present, the terminal card(s) may be moved to
fill the vacancy, with the system reconfigured at boot time as appropriate.  The
paper tape reader in slot 17 and the paper tape punch in slot 20 are intended
for use with SIMH as I/O devices for paper tape images from the Bitsavers' HP
1000 Software Collection, though, of course, real devices may be used if they
are available.

The CRT and TTY interfaces support non-HP terminals.  However, the hardware BACI
will not work with terminals that do not support the HP ENQ/ACK handshake
sequence.  A free HP 700/92 terminal emulator for Windows PCs is available from:

  http://www.aics-research.com/qcterm/

Under SIMH, the simulated BACI will work with non-HP Telnet clients, as the
simulator is configured by default to handle the handshake internally.  Terminal
emulators must be configured to send CR only as a line terminator.  CAPS LOCK
will be required, as RTE only accepts commands in uppercase.

A minimal I/O system would consist of the TBG, Disc Interface, and a terminal
interface in slots 10-12, or a FEM, TBG, Disc Interface, and a terminal
interface in slots 10-13.  The included system expects these interfaces in I/O
slots 11, 12, and 16, respectively; all others are not required.  If your 1000
I/O configuration differs from the above, an RTE reconfiguration (i.e., "slow")
boot will be required to reassign the I/O cards.  See page 5-2 of the "RTE-IVB
System Manager's Manual" for the standard boot procedure, and page 9-1 for the
reconfiguration boot procedure.

The system is configured for two 7920H drives, although only one is required.
The drive on HP-IB address 0 is the system disc and is subdivided into these
logical units:

  - LU  2 contains the bootable OS and the system programs
  - LU 11 contains files used for system generation (including Y2K replacements)
  - LU 12 contains the system generation listing and absolute files
  - LU 13 contains the Pascal/1000 and BASIC/1000D software modules
  - LU 14 contains the RTE-IVB software modules
  - LU 15 is blank and available for user files

The drive on unit 1 is available for disc space expansion, or to exchange data
with another system disc image.  The logical unit configuration is identical to
the system disc configuration to permit interchange mounting.


------------------
INSTALLED PROGRAMS
------------------

The following programs are preinstalled either permanently or as executable type
6 files on LU 2:

  * FMGR  -- the file manager
  * HELP  -- the system help facility
  * LOADR -- the relocating loader
  * LGTAT -- a utility to list the disc track allocation tables
  * WHZAT -- a utility to list program and partition status
  * LUPRN -- a utility to list logical unit assignments
  * EDIT  -- the text editor
  * ASMB  -- the HP 1000 assembler
  * XREF  -- the assembler cross-referencer
  * FTN4  -- the FORTRAN IV compiler
  * RT4GN -- the system generator
  * SWTCH -- the system installer
  * READR -- a simple tape archive reader
  * SAVER -- a simple tape archive writer
  * FC    -- a more complex tape archiver

The FMP master security code is RT.


---------------
YEAR 2000 FIXES
---------------

RTE-IVB Revision 5010 is not Y2K compliant.  All of the failures are in
subsystems; the operating itself (time-of-day clock) accommodates dates through
2059.  All of the errors are cosmetic.  Typically, punctuation characters appear
in the years, e.g., "19:0" for 2000.

The affected modules are:

  * FTIME (contained in %4SYLB 92067-16268 REV.2540)
    Returns the time of day in the form "10:03 AM  MON., 29  DEC., 1975".  Used
    by FMGR, LOGON, LOADR, et. al.

  * SUP.C (contained in %CLIB 92067-12001 REV.2226)
    Initializes the compiler library and returns the time of day in same form as
    FTIME.  Used by ASMB, XREF, FTN4, et. al.

  * EDIT0 (contained in %EDITA 92074-12001 REV.2440)
    Implements several EDIT commands, including time-of-day timestamps of the
    form "790524.1452".  Used by EDIT.

  * TIME (contained in $PLIB 92832-16700 REV.2101)
    Returns the time of day in either of the forms "Thu May 24, 1979  12:52 pm"
    or "790524.1452".  Used by the Pascal compiler and Pascal programs.

  * JULIA (contained in $RSLIB 92068-12006 REV.2540)
    Returns the time of day in either of the forms "23:05:15 06AUG76" or
    "2305 06AUG76".  Used by READR and SAVER.

Corrected versions of these modules are present on LU 11 (cartridge GF) and have
been generated into the system where appropriate.  The updated versions are
contained in the following files and have these headers corresponding to the
original and corrected versions, respectively:

  * %FTIME::GF
      FTIME 92067-1X301 REV.2013 780731
      FTIME 92070-1X195 REV.5000 860630

  * %SUP.C::GF
      SUP.C 92060-1X091 REV.2140 810821
      SUP.C 92060-1X091 REV.2140 810821 Y2K MOD 050420

  * %EDITA::GF
      EDIT0 92074-1X003 REV.2440 <850107.0858>
      EDIT0 92074-1X003 REV.2440 <850107.0858> Y2K MOD 131208

  * %TIME::GF
      TIME 92832-16700 REV.2101 801010
      TIME 92832-16700 REV.2101 801010 Y2K MOD 131208

  * $RSLIB::GF
      JULIA 92068-1X059 REV.2101 810225
      JULIA 92068-1X059 REV.2101 810225 Y2K MOD 131209

(Module FTIME was updated by substituting the corresponding RTE-6/VM module.)


---------
BUG FIXES
---------

The $BALC module in the system library has a bug that causes memory corruption.
This module is used by the ACCTS program and manifests itself by printing
gibberish after the PLEASE LOG ON: prompt.

Specifically, the internal MXEV routine performs a cross-store indirect via a
location in Table Area II (XSA $MAXI+0,I).  This fails because the indirect
chain is resolved in the user map, but TA II is not in the user map of
large-background programs.  Therefore, the location in the user map
corresponding to $MAXI in the system map is used as the pointer to the location
to store.

A corrected version of $BALC is present on cartridge GF:

  * %$BALC::GF
      $BALC 92067-1X311 REV.2540 850807
      $BALC 92067-1X311 REV.2540 850807 BUG FIX 131223


------------------------
ADDITIONAL DOCUMENTATION
------------------------

Many RTE-IVB manuals are available from the HP Computer Museum site:

  http://www.hpmuseum.net/

The full list of manuals applicable to this release are contained in the file
M92068 on LU 12.  The manuals of most relevance to the included system are:

  Part Number  Manual Title
  -----------  ----------------------------------------------
  92068-90002  RTE-IVB Terminal User's Reference Manual
  92068-90003  RTE-IVB Quick Reference Guide
  92068-90004  RTE-IVB Programmer's Reference Manual
  92068-90006  RTE-IVB System Manager's Manual
  92068-90007  RTE-IVB On-Line Generator Reference Manual
  92068-90010  RTE-IVB Utility Programs Reference Manual
  92068-90016  READR/SAVER Utility Reference Manual
  12791-90001  HP 1000 M/E/F-Series Firmware Installation
               and Reference Manual
  12992-90001  HP 12992 Loader ROMs Installation Manual
  24998-90001  DOS/RTE Relocatable Library Reference Manual
  92060-90023  FORTRAN IV Reference Manual
  92067-90003  RTE-IV Assembler Reference Manual
  92067-90005  RTE-IV Debug Subroutine Reference Manual
  92074-90001  EDIT/1000 User's Guide
