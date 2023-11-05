
  Single-user 31KW MSU BASIC
  ==========================

MSU BASIC is a four-user time-sharing BASIC system for a HP2116 
minicomputer from Montana State University.

For HP21xx minicomputers, or the SimH HP2100 simulator.
This is Rev C of msubasic.zip, but still preliminary.

This is a version of MSU BASIC modified to run with a single TTY console
and operate in a 31KW environment to permit larger programs and to be
compatible with HP-IPL/OS. I obtained the 16KW sources with the single-TTY
mod from Tim Riker's site at http://rikers.org/hp2100/ along with a custom
Asm21.pl assembler needed to assemble the sources. Information about
these files are at: http://rikers.org/wiki/MsuBasic

My 31KW mod for the "prep" program was fairly simple, moved ORG's up
by 36000 and changed two BLS instructions to RBL for 32KW-compatible
16-bit byte pointers. The "mon" program was not modified. Further mods
were done by patching the configured binary to make it start up without
having to log on and without clearing memory, and the BYE was changed
to HP-IPL/OS if present, otherwise runs the original logout code.

The modified MSU BASIC requires a HP21xx with at least 32KW memory,
a time-base generator board, and a TTY-style console interface board.
The HP-IPL/OS utility build for patching etc requires a HP21-MX with
at least 64KW memory, a console interface, a "regular" papertape reader
and punch (or emulations) are needed to modify and save binaries.


Files and Scripts
=================

msuprep.abs - 31KW version of the drivers and configurator
msumon.abs - the actual BASIC code (overlay with msuprep.abs)
msuraw.abs - output of configurator, slots set to CLK=10 TTY=11
msubasic.abs - version with the patches applied, CLK=10 TTY=11
prepare.ini - simh hp2100 script that runs the configurator
patch.ini - pathc to run without prompts
msuprep.asm - assembly source for msuprep.abs
msumon.asm - assembly source for msumon.abs

The msuprep.ini script prints instructions, runs msuprep.abs and msumon.abs,
and outputs to msuraw.abs. Only need to run this if a different configuration
is needed or the msuprep/msumon files are changed. The msuraw.abs binary here
was configured with no card, no papertape reader and one information line.

The msupatch.ini script prints instructions, attaches msuraw.abs for input
and msubasic.abs for output, and runs hposutil.abs to use for patching.
The procedure requires that the "raw" MSU BASIC be run using RUNABS and
logged on to, halted to return to HP-IPL/OS, the MSUPATCH utility run,
then AM2ABS run to punch the patched msubasic.abs file.

The hposutil.sim script runs hposutil.abs without files attached
for various uses including adding BASIC code to MSU BASIC then punching
a new abs file containing the interpreter and the BASIC code, altering
the TTY and CLK slots of an existing 31KW MSU BASIC binary, and general
binary hacking like loading one or more abs files, examining/changing
stuff and punching a new abs containing the changes. Can execute just
about any abs that runs in 31KW with a compatible slot/device config
while providing a way to return to HP-IPL/OS by swapping main and
alternate memory, so at any time the sim can be halted and "frozen".

Note... the scripts use the lines...
 d tty ttime 1000
 d tty ktime 40000
Originally ttime was 400 but sometimes it'd crash with an indirect loop
when listing so increased to 1000. The ktime setting slows down keyboard
polling to permit pasting code into the simulation without errors.
You might have to play with these numbers for proper operation.


Using MSU BASIC with a HP-IPL/OS disk system
============================================

For a stock HP-IPL/OS disk setup, attach msubasic.abs to PTR and
at the ? prompt enter: "MSU BASIC" ABS2F (adjust filename as needed)
To run enter: "MSU BASIC" XLOAD
Do whatever with it, enter BYE to return to HP-IPL/OS.
After returning a BASIC program can be saved by: "FILENAME" 174000 AM2F
When rerun using XLOAD the program listing will be intact.

It might be handy to load msupatch.ipl into a build that contains
altutil.ipl and optionally fcam.ipl to use for patching, for a IDE or
7906 system the hposmt6i.abs build is good for this. Although already
patched, the MSUPATCH utility can alter the start-up test line, change
the backspace character between _ and ascii-8, and alter the slots.
The patcher works on alt memory, either after returning from a MSU BASIC
session, or after doing "FILENAME" F2AM to copy from a file to altmem.
Run MSU PATCH then do "FILENAME" 174000 F2AM to save, or to replace
a file do "FILENAME" XDEL and press Y then do the save command.


Using MSUPATCH
==============

Notice! This will likely change, very new code and still learning
about the system.

To start, "run" the hposutil.sim script in hp2100 to start the utility.
Either hp2100 hposutil.sim at the prompt, or do hposutil.sim at the hp2100
sim prompt. Or rig up a script so the .sim file can be right-clicked
and run (in Windows just associate .sim files with hp2100.exe).

MSUPATCH operates on whatever is in alternate memory, if it isn't
the 31KW MSU BASIC presented here or extremely close it doesn't run.
Patch locations are hard-coded so if anything is done to the msuprep
or msumon sources/binaries that changes any offsets it won't work, all
the locations will have to be tracked down, msupatch.ipl edited and
reloaded back into the utility build (see end of "About" section).

Getting MSU BASIC into alt mem can be done several ways.. by using
RUNABS to run it then halt/run 77000 (or BYE if patched), by loading
directly from PTR using ZAM ABSLOAD, or if a disk system using F2AM.
Could do CLRHALT then at the sim prompt load the abs, when done
run 77000 to jump back to HP-IPL/OS, might be handy if things need
to be altered/configured first.

One handy trick to change an existing abs file is control-E or HLT
then attach ptr msufile.abs, attach ptp msufile.abs (same filename),
continue (or run 2), ZAM ABSLOAD, MSUPATCH, AM2ABS (Y to save N to halt).
Note.. this only works if the file size will be the same or larger,
otherwise specify a new file for PTP output.

When MSUPATCH runs it presents a menu...

SINGLE USER 31KW MSU BASIC PATCH UTILITY
[1] APPLY PATCHES
[2] CHANGE BACKSPACE
[3] CHANGE INFO LINES
[4] CONFIGURE SLOTS
PRESS KEY OR ANY OTHER TO EXIT: 

Press 1 to 4 for the desired function, just the key, no enter.

Note - do not use patch the "raw" MSU BASIC system unless it has been
run, logged onto, and exited from the READY prompt. The other options
can still be used to alter a "raw" setup but except to change the
backspace there'd be no point, just as easy to rerun the configurator.
The main use for MSUPATCH is for doing the initial patches, applying
improved patches to existing patched versions, and changing the info
line or reconfiguring the slots without disturbing the embedded listing.

The patches bypass log-on and initialization to preserve the program
in memory, and alter the BYE code patch so that it checks to see if
HP-IPL/OS' swapper is at 77000, otherwise runs the stock logout.

Option 2 simply flips the backspace character between the stock "_"
and a real ascii-8 backspace. When backspacing like this the chars
aren't removed from the screen but removed from the input buffer,
just ignore everything under and after the cursor and space it away
if bothersome.. still likely better than doing PRIIT__NT "HEK_Y".
In Windows it should work as-is, in Linux pick a keymap that puts
backspace on ascii-8, generally swapping BS and DEL so at the simh
prompt have to DEL to backspace. Konsole's VT420PC works for me.

Option 3 (loosly) permits changing the information line(s) displayed
when MSU BASIC starts. First it displays what's there, asks to change,
then prompts to enter new lines. Presently this option, while handy,
has serious limitations: it can't change the number of lines, and there
must be enough room for the string or else pointers get overwritten and
the alt mem image is pretty much trashed. Use option 3 again to make sure
this didn't happen (better to find out sooner than later). To avoid crashing
when preparing the "raw" MSU starter system be sure to space-pad the line(s)
to make sure there's room to make changes. The change lines option probably
needs to be rewritten to totally rebuild the table and pointers.. later...
All the pre-config'd binaries here all have one space-padded info line
to avoid this problem.

Option 4 permits changing the TTY and CLK slot assignments. For each device
displays the current assignment, just enter to not change or enter an octal
slot number between 10 and 25. For example, to run with SimH HP2100's stock
slots change CLK to 13. Presently there's no support for patching the reader
or card slots, neither work under simulation and probably require specific
hardware, just guessing but maybe a redirect to the TTY's paper-tape unit.
It is more practical to paste code into the console (adjust ktime as needed
to make that reliable) and copy listed code from the console, as long as
that works there's no need for card/reader.

Once done, press any key except 1-4 to exit the patch utility.

To punch an ABS with the patched results at the ? prompt enter: AM2ABS
That's for alt mem to abs. It scans memory and lists what it finds, padded
beyond the ranges to include locations that might need to be set to zero
and reduce the number of extents of memory to save. Press Y to save, then
if the output file is already attached to PTP press N to halt, if not
attached yet press Y to halt and attach ptp filename.abs then continue.
Should punch the binary, if halt was specified halts to detach ptp.


About the HP-IPL/OS Utility Build
=================================

This binary is configured with CLK=10, TTY=11, PTR=12, PTP=13 and BACI=22.
There is no baci under sim, present in case needed with real hardware.

CLK is called TBG in HP-IPL/OS and isn't used in this build, configured
anyway since that's what the sim script sets it to, many things that might
be loaded from the utility (like MSU BASIC) do use the clock, and the MKBCS
utility for making a BCS "make" script outputs a set dev for it.

To make a new utility build with different slot assignments do this...
Enter CONFIG and change the slots as needed (just enter to not change slot)
When it prompts for TTY or BACI press A for TTY (unless you really want BACI)
Enter HLT SYSALL
At the sim prompt attach ptp newfilename.abs and continue
Should punch the new build, when done control-E and exit.
HLT is a convenient way to make things halt first, feel free to attach
ptp any way you want as long as done before the SYSALL.

Summary of some of the available utilities...

ALTSAVE   Copies HP-IPL/OS to alt mem and puts a program at location 77000
           which swaps main and alt mem then runs from location 2.
ZAM       Zero alt mem, ALTSAVE ZAM zeros alt mem but leaves the swapper.
           Hint... if you might need to run the swapper (77000 RUN or ALTRUN)
           to test a binary loaded via ABSLOAD or whatever, do ALTSAVE ZAM
           before loading memory to make sure the swapper is there to run.
HLT       Halts the computer for swapping cables, attaching etc.
ABSLOAD   Loads ABS from PTR into alt mem (attach first or do HLT ABSLOAD).
RUNABS    Runs an ABS file from PTR, prompts to halt/attach if needed.
           Executes from location 2 if 2/3 defined, otherwise prompts for
           a run address. Automatically does ALTSAVE ZAM, so after operating
           the program, halt and run from 77000 to return to HP-IPL/OS.
AM2ABS    Scans alt memory and punches an ABS file from what it detects,
           prompts to halt for attaching output file.
[from] [to] ALTDUMP   Dumps alt memory to screen in octal to examine.
SHAM      Prompts for address and displays pageful of octal and ascii.
           Enter ALL at the ADR: prompt to dump 31KW from alt mem.
UNSHAM    Decodes a SHAM dump attached to MS in (PTR) and puts in alt mem.
[address] AGET PNUM   Prints octal value of location in alt mem.
[address] [value] APUT  Changes a location in alt mem.
ALTRUN    Runs whatever's in alt mem. Halt and run 77000 to exit back.
ALTHALT   Swaps main/alt then halts. This means overwriting location 3 to
           point to a halt, and it does not put it back - probably a flaw
           but I use this only for debugging. It prints what it was so
           if important do d 3 [normal run address] before doing run 77000
           to exit back to hpiplos. Don't do ALTHALT again and continue
           without fixing it, that would be an endless halt loop. (sorry:-)
CLRHALT   Saves HP-IPL/OS then halts to a clear 31KW machine except for
           a halt in location 2, run 77000 to exit back.
SIOPATCH  Patches the slots of a 16K SIO driver for running extasmb.abs
           or the FORTRAN or Algol compilers. HLT and attach ptr to existing
           sio driver, attach ptp to new sio.abs driver, then run SIOPATCH.
MKBCS     Generates a simh hp2100 "make" script for building BCS projects.
           Prompts for source names (without extension), press a key to
           indicate type, then enter the number of segments or press enter
           if just one. Press enter at the Filename: prompt when all the
           modules have been specified, it'll halt to attach ptp to the
           output script file, continue to write out script, detach ptp
           when it halts again. Sample run...
------------------------------------------------------------------
? MKBCS
Make BCS Build Script
Enter basename of each source file (no extension)
For each file press 1 2 or 3 for .asm .ftn or .alg
(must have those extensions) then enter the number of
segments (ENDs) in the source file (just enter for one)
Press Enter at filename prompt when all sources named,
will halt to attach ptp to output script, after continuing
will prompt for ABS output filename then halt to detach.
Filename: ASMSUB
1)Assembly 2)Fortran 3)Algol: 1
# Segments:
Filename: MYPROG
1)Assembly 2)Fortran 3)Algol: 2
# Segments:
Filename:
At the sim prompt enter: attach ptp scriptname
Then enter c to continue

HALT instruction 102007, P: 32625 (JMP 321,I)
sim> attach ptp myprog.sim
PTP: creating new file
sim> c
Output file (include .abs ext): MYPROG.ABS
Done. Detach ptp then c
HALT instruction 102007, P: 32625 (JMP 321,I)
sim> detach ptp
sim> c

? 
------------------------------------------------------------------

That test session outputted this script...
------------------------------------------------------------------
; created by MKBCS
set clk dev=000010
set tty dev=000011
set ptr dev=000012
set ptp dev=000013
d s 0
d 2-76777 0
load bin/sio.abs
load bin/fortran1.abs
attach ptr MYPROG.ftn
attach ptp MYPROG.tmp
run 100
load bin/fortran2.abs
attach ptr MYPROG.tmp
attach ptp MYPROG.rel
run 100
detach ptp
d 2-76777 0
load bin/sio.abs
load bin/extasmb.abs
attach ptr ASMSUB.asm
attach ptp ASMSUB.rel
run 100
attach ptr ASMSUB.asm
c
detach ptp
d 2-76777 0
load bin/bcs31k.abs
d s 40000
attach ptp MYPROG.ABS
attach ptr MYPROG.rel
run 2
attach ptr ASMSUB.rel
c
d s 40004
attach ptr bin/bcslib.rel
c
d 2 102066
c
detach ptp
------------------------------------------------------------------

Note that whatever is defined for the TBG slot is output to set clk dev=slot
even though SIO/BCS doesn't use it, could have outputted set clk disabled
but didn't, so important TBG is set to a non-conflicting slot using CONFIG.
Edit the output script as needed. I have .sim files associated to hp2100
via the following bash script...

---- hp2100sim ----------------------------------------------------------
#!/bin/bash
# open a .sim file using hp2100
cd `dirname "$1"`
scr_name=`basename "$1"`
konsole -e hp2100 "$scr_name"
# xterm -e hp2100 "$scr_name"
# gnome-terminal --window-with-profile="Reversed" -x hp2100 "$scr_name"
---- end hp2100sim ------------------------------------------------------

Adapt/edit as needed, with this or something similar all I have to do
is change my source files then right-click the "myprog.sim" script and
choose hp2100sim and a few seconds later I have an ABS to test. Without
the above script, just run hp2100 myprog.sim to do the same thing.

There are other utilities present in the build, see the following IPL
files for more information about the above and other utilities...

altutil.ipl
fcam.ipl
sioutil.ipl
mkbcs.ipl
conalt.ipl (utilities for dumping/loading binaries via the console)
log2abs.ipl (utility for turning console dump into an ABS file)

See: http://www.infionline.net/~wtnewton/oldcomp/hp2100/files.htm
     http://www.infionline.net/~wtnewton/oldcomp/hp2100/
     http://www.infionline.net/~wtnewton/hpiplos.html

In case the utility has to be rebuilt, here's the .sim script that made it...

----- make_util.sim ----------------------------------------------------
; this script guides the building of hposutil.abs
; instructions will be printed, do them then hit control-e for more
set cpu 21mx
set cpu 256k
set clk dev=10
set tty dev=11
set ptr dev=12
set ptp dev=13
load ../abs/hpiplos1.abs
echo =======================================================
echo Building the UTIL build in the hposutil.abs file.
echo After performing each task press control-E for more.
echo =======================================================
attach ptr ../ipl/extra.ipl
echo Enter <PTR to load extra.ipl then select 32K.
run 2
attach ptr ../ipl/oct70.ipl
echo Enter LOAD to load oct70.ipl
c
attach ptr ../ipl/create.ipl
echo Enter LOAD to load create.ipl
c
attach ptr ../ipl/debug.ipl
echo Enter LOAD to load debug.ipl
c
attach ptr ../ipl/version.ipl
echo Enter LOAD to load version.ipl
c
attach ptr ../ipl/baci.ipl
echo Enter LOAD to load baci.ipl
c
attach ptr ../ipl/config.ipl
echo Enter LOAD to load config.ipl
c
attach ptr ../ipl/cfge.ipl
echo Enter LOAD to load cfge.ipl
c
attach ptr ../ipl/dms.ipl
echo Enter LOAD to load dms.ipl
c
attach ptr ../ipl/mkword.ipl
echo Enter LOAD to load mkword.ipl
c
attach ptr ../ipl/double.ipl
echo Enter LOAD to load double.ipl
c
attach ptr ../ipl/bigshift.ipl
echo Enter LOAD to load bigshift.ipl
c
attach ptr ../ipl/float.ipl
echo Enter LOAD to load float.ipl
c
attach ptr ../ipl/floatext.ipl
echo Enter LOAD to load floatext.ipl
c
attach ptr ../ipl/hpscreen.ipl
echo Enter LOAD to load hpscreen.ipl
c
attach ptr ../ipl/altutil.ipl
echo Enter LOAD to load altutil.ipl
c
attach ptr ../ipl/conalt.ipl
echo Enter LOAD to load conalt.ipl
c
attach ptr ../ipl/log2abs.ipl
echo Enter LOAD to load log2abs.ipl
c
attach ptr ../ipl/fcam.ipl
echo Enter LOAD to load fcam.ipl
c
attach ptr ../ipl/mkbcs.ipl
echo Enter LOAD to load mkbcs.ipl
c
attach ptr ../ipl/sioutil.ipl
echo Enter LOAD to load sioutil.ipl
c
echo Enter TERMINAL to default to ANSI
c
echo Enter 270 10 PUT to default TBG slot
c
echo Enter VERSION, Y then name HP-IPL/OS UTIL 1.51
c
attach ptp hposutil.abs
echo Enter SYSALL to generate hposutil.abs
run 2
detach ptp
----- end make_util.sim ------------------------------------------------

I added the 270 10 PUT part after the fact to default the TBG slot.
Manually added sham.ipl and msupatch.ipl by attaching each to PTR and
in HP-IPL/OS entering LOAD, using VERSION to change to 1.51A, attaching
PTP to hposutil.abs and using SYSALL to punch the updated version.

To update with a new MSUPATCH (the last word in the build) do this:
FORGET MSUPATCH [press Y to confirm]
Control-E then..
 attach ptr msupatch.ipl
 attach ptp hposutil.abs (or another file)
 run 2
LOAD
SYSALL
Control-E and exit.

--------------------------------------------
History...
3/7/08 - added UNSHAM to hposutil.abs, moved Trek game to hposgames.zip
3/4/08 - was late.. msubasic.abs too big.. modified repatch instructions
 in hposutil.sim to include ZAM to erase alt mem first. While at it updated
 the SHAM utility so entering ALL dumps everything in alt mem.
3/3/08 - added MSUPATCH and SHAM to utility, updated configured binaries
3/1/08 - initial version (no BYE patch, bootup patch done manually)

Terry Newton <wtnewton@infionline.net>
