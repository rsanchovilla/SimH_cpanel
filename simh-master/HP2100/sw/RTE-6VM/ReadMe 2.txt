
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

  dd if=RTE-6VM-SYSTEM.U0.7920H.disc of=RTE-6VM-SIMH.U0.7920H.disc conv=swab status=noxfer

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
for use with SIMH as I/O devices for paper tape images from the Bitsavers\' HP
1000 Software Collection, though, of course, real devices may be used if they
are available.

The CRT and TTY interfaces support non-HP terminals.  However, the hardware BACI
will not work with terminals that do not support the HP ENQ/ACK handshake
sequence.  A free HP 700/92 terminal emulator for Windows PCs is available from:

  http://www.hpmuseum.net/display_item.php?sw=585

Under SIMH, the simulated BACI will work with non-HP Telnet clients, as the
simulator is configured by default to handle the handshake internally.  Terminal
emulators must be configured to send CR only as a line terminator.  CAPS LOCK
will be required, as RTE only accepts commands in uppercase.

A minimal I/O system would consist of the TBG, Disc Interface, and a terminal
interface in slots 10-12, or a FEM, TBG, Disc Interface, and a terminal
interface in slots 10-13.  The included system expects these interfaces in I/O
slots 11, 12, and 16, respectively; all others are not required.  If your 1000
I/O configuration differs from the above, an RTE reconfiguration (i.e., "slow")
boot will be required to reassign the I/O cards.  See page 6-2 of the "RTE-6/VM
System Manager's Reference Manual" for the standard boot procedure, and page
10-1 for the reconfiguration boot procedure.

The system is configured for two 7920H drives, although only one is required.
The drive on HP-IB address 0 is the system disc and is subdivided into these
logical units:

  - LU  2 contains the bootable OS and the system programs
  - LU 11 contains files used for system generation (including Y2K replacements)
  - LU 12 contains the system generation listing and absolute files
  - LU 13 contains the FORTRAN 77 software modules
  - LU 14 is blank and available for user files
  - LU 15 contains a CI file system that includes the RTE-6/VM software modules

The drive on unit 1 is available for disc space expansion, or to exchange data
with another system disc image.  The logical unit configuration is identical to
the system disc configuration to permit interchange mounting.


---------------
SESSION MANAGER
---------------

The accounting system is configured with a single account, MANAGER.SYS,
with the password HP.


------------------
INSTALLED PROGRAMS
------------------

The standard set of system utilities have been preinstalled, either permanently
or as executable .RUN files in /PROGRAMS.  These include:

  * FMGR  -- the file manager
  * CI    -- the command interpreter
  * LOADR -- the relocating loader
  * ACCTS -- the session accounts manager
  * LGTAT -- a utility to list the disc track allocation tables
  * WHZAT -- a utility to list program and partition status
  * RT6GN -- the system generator
  * SWTCH -- the system installer
  * LUPRN -- a utility to list logical unit assignments
  * EDIT  -- the text editor
  * MACRO -- the HP 1000 macro assembler
  * LINK  -- the relocating linker
  * LINDX -- the library indexer
  * HELP  -- the system help facility
  * READR -- a simple tape archive reader
  * SAVER -- a simple tape archive writer
  * FC    -- a more complex tape archiver
  * FST   -- a much more complex tape archiver

Due to memory size restrictions, the "TF" utility is not installed, and the "LI"
command is linked with EMA instead of VMA.

The FMP master security code is RT.


---------------
YEAR 2000 FIXES
---------------

RTE-6/VM Revision 6200 is Y2K compliant, except for the READR and SAVER
programs.  The affected module is:

  * JULIA (contained in $RSLIB 92068-12006 REV.2540)
    Returns the time of day in either of the forms "23:05:15 06AUG76" or
    "2305 06AUG76".

A corrected version of this module is present on LU 11 (cartridge GF) and is
contained in the following file and has these headers corresponding to the
original and corrected versions, respectively:

  * $RSLIB::GF
      JULIA 92068-1X059 REV.2101 810225
      JULIA 92068-1X059 REV.2101 810225 Y2K MOD 131209


---------------
MICROCODE PATCH
---------------

During bootup, RTE-6/VM checks the type of the processor.  If an E- or F-Series
machine is detected, the OS and VMA microcode self-test instructions are
executed.  If these fail, as they will if the microcode is not present, a HLT
21B occurs.

To enable this system to run on machines without OS/VMA microcode, the processor
test has been disabled.  The test is in OS module SCHD6 (92084-1X478 REV.5000)
at entry point $NOMI:

  * SEE IF M OR E/F 
        CCB 
        OCT 100060
  *  NOP BELOW TO RUN WITHOUT MICROCODE 
  $NOMI SZB           SKIP IF E OR F
        JMP TOIT0      NO OS/VMA MICROCODE IF M 
        cca           set a=-1 if os microcode
        OCT 105355     YES, IF IT PASSES SELF-TEST
        jmp badvm     didn't pass self test give HLT 21 
        STX $MCRO     =0 IF NO OS MICROCODE, ELSE "REV.CODE"
  * NOW DO VMA SELFTEST & PROCEED ONLY IF IT PASSES 
        CLA 
        CAY           Y= 0 FOR VMA SELFTEST ENTRY 
        OCT 105242
        JMP BADVM     IF NOT THERE YOU LOSE 

Opcode 100060 is TIMER, an undocumented E/F-Series instruction that increments
the B register and loops internally until B = 0.  On an M-Series, the
instruction decodes as a MPY and so returns to P+2.  So if the return is to P+1
with B = 0, the machine is an E- or F-Series; otherwise, it's an M-Series.

The test has been bypassed by changing the SZB instruction at $NOMI to NOP in
the disc image (bytes 0x4064 and 0x4065 were set to 0).  This ensures that
E/F-Series machines use the same software emulations of the OS and VMA
instructions as are used by the M-Series machines.

Note that if the system is regenerated, it will not run on an E/F-Series without
OS/VMA microcode until the patch is reapplied.


------------------------
ADDITIONAL DOCUMENTATION
------------------------

Many RTE-6/VM manuals are available from the HP Computer Museum site:

  http://www.hpmuseum.net/

The manuals can be found here.

The full list of manuals applicable to this release are contained in the file
M92084::RTE_6 on LU 15.  The manuals of most relevance to the included system
are:

  Part Number  Manual Title
  -----------  ----------------------------------------------
  92084-90003  RTE-6/VM Quick Reference Guide
  92084-90004  RTE-6/VM Terminal User's Reference Manual
  92084-90005  RTE-6/VM Programmer\'s Reference Manual
  92084-90007  RTE-6/VM Utility Programs Reference Manual
  92084-90009  RTE-6/VM System Manager's Reference Manual
  92084-90010  RTE-6/VM Online Generator Reference Manual
  92084-90036  RTE-6/VM CI User's Manual
  92068-90016  READR/SAVER Utility Reference Manual
  12791-90001  HP 1000 M/E/F-Series Firmware Installation
               and Reference Manual
  12992-90001  HP 12992 Loader ROMs Installation Manual
  92077-90037  Relocatable Libraries Reference Manual
  92059-90001  MACRO/1000 Reference Manual
  92074-90001  EDIT/1000 User's Manual
  92836-90001  FORTRAN 77 Programmer's Reference
