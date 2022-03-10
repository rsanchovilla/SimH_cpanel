# SimH_cpanel


**NEW!: IBM 360 Simulator**

The aim of this project is to add visuals to SimH emulator/simulator on an interactive GUI.

The visuals covers interactive control panels (lights, switches, buttons) but also
other devices: tapes, card readers, disk, etc.

IBM360 and i7000 work is based on Richard Cornwell own's SimH fork, and is implemented using SimH own SDL based modules. 
i701, i650 and NORC work is based on my own SimH simulators

<table>
<thead>
<tr>
<th>IBM 360/370</th>
<th>IBM 701</th>
<th>IBM 650</th>
</tr>
</thead>
<tbody>
<tr>
<td>
<p><img src="https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360_animated.gif" alt="IBM360" title="IBM 360" /></p>
</td>
<td>
<p><img src="https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i701/IBM701_animated.gif" alt="IBM701" title="IBM 701" /></p>
</td>
<td>
<p><img src="https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i650/IBM650_animate.gif" alt="IBM650" title="IBM 650" /></p>
</td>
</tr>
</tbody>
</table>

Currently supports visuals emulation for:

* IBM 360/370 (MAR/2022)
  * IBM 2401, 2415 and 3420 tapes
  * IBM 2450 and 3525 card reader punch
  * IBM 1403 and 3203 printer
  * IBM 2314 and 3330 DASD
  * IBM 1052 and 3210 printer-Keyboard Console
  * IBM 360 CPU Model 30, 40, 50 and 65
  * IBM 370 CPU Model 145 and 148

  Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%20360%20Panels%20Screen%20Shots.png) to preview control panels.
   Source code in simh-master folder

* IBM 701 Electronic Data Processing Machine (JUL/2021)
  * IBM 726 tapes, IBM 711 card reader, IBM 721 card punch, IBM 716 printer
  * Main console (as in 1953), Early console model (as in Early 1952)

  Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%20701%20Panels%20Screen%20Shots.png) to preview control panels.
   Source code in simh-master folder

* IBM NORC Naval Ordnance Research Calculator (UPDATED JUL/2021)
  * panels for tapes, printer, indicator panel and main console

  Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%20NORC%20Panels%20Screen%20Shots.png) to preview control panels.
   Source code in simh-master folder

* IBM 650 Magnetic Drum Data Processing machine (UPDATED JUL/2021). 
  * IBM 727 tapes, IBM 533 card read punch, and IBM 355 RAMAC disk storage
  * panels for IBM 653 Storage Unit and IBM 652 Control Unit

  Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%20650%20Panels%20Screen%20Shots.png) to preview control panels.
   Source code in simh-master folder


* IBM 7000 Series  
  * IBM 704 and 709 control panel. 
  * IBM 727 and 729 tapes
  * IBM 766 data synchorinizer
  * IBM 7090/7094 control panel. 
  * IBM 7617 Data channel console and IBM 729 tapes
  * IBM 711 card punch, IBM 7909 data channel panel, IBM 7631 file control panel and IBM 2302 disk 
       
   Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%207000%20Panels%20Screen%20Shots.png) to preview how control panels.
   Source code in sims-master folder

# Test Run Software Kits

## Common keys
Several ready to run sw kits are available to demo control panel operation (Windows only)

Hot keys are available when GUI window has the focus:
* Control E -> Halt
* Control T -> Toggles mark on clickable areas
* Control Y -> Toggles size of GUI (half size <-> full size)
* **+** (plus) and **-** (minus) keys -> Zoom In/Zoom Out (also Control + and Control -)
* Control I -> Toggle info panel 
* Control F -> Fast mode: while pressed, accelerates cpu to max speed
* Mouse:
  * Click on control panel image and drag mouse to move the window
  * Right click mouse to show a tooltip with image at 100% scale

Press power button to quit the simulator.
(On i7000 control panels, only ^E, ^Y and ^T available)

## IBM 370

### Model 145

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM370_VM370.zip) to download VM/370 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 370-145 VM370 R6 sysgen (512KB).bat` batch file to sysgen the operating system at real hw speed. Will need 1h aprox
* execute `run CPanel IBM 370-145 VM370 R6 CMS (1979).bat` batch file to IPL the system. Then drop jcl file into card reader to run it.
* execute `run IBM 370 VM370 R6 CMS (1979).bat` batch file to run a CMS interactive session (no control panel, max speed)
* execute `run IBM 370 VM370 R6 sysgen (512KB).bat` to sysgen the system at max speed, no control panel. Will need 30min aprox
 
  ![IBM 370 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM370M145.png)

### Model 148

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM370_OSVS1.zip) to download OS/VS1 Release 6 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 370-148 OSVS1 R6 sysgen (8MB).bat` batch file to sysgen the operating system at real hw speed. Will need 4h15min aprox
* execute `run CPanel IBM 370-148 OSVS1 R6 (1976).bat` batch file to IPL the system. Then drop jcl file into card reader to run it.
* execute `run IBM 370 OSVS1 R6 VS Assembler (1976).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 370 OSVS1 R6 sysgen (8MB).bat` to sysgen the system at max speed, no control panel. Will need 1h20min aprox

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM370_OSVS1R67.zip) to download OS/VS1 Release 6.7 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 370-148 OSVS1 R6.7 sysgen (8MB).bat` batch file to sysgen the operating system at real hw speed. Will need 6h45 aprox
* execute `run CPanel IBM 370-148 OSVS1 R6.7 (1979).bat` batch file to IPL the system. Then drop jcl file into card reader to run it.
* execute `run IBM 370 OSVS1 R6.7 Stanford Pascal (1979).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 370 OSVS1 R6.7 SNOBOL4.bat` same as above
* execute `run IBM 370 OSVS1 R6.7 BASIC 360.bat` same as above
* execute `run IBM 370 OSVS1 R6.7 sysgen (8MB).bat` to sysgen the system at max speed, no control panel. Will need 3h15min aprox
* execute `run IBM 370 OSVS1 R6.7 VTP.bat` to open an interactive VTP session  (no control panel, max speed)
 
  ![IBM 370 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM370M148.png)

## IBM 360

### Model 30 

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360_BOS360.zip) to download BOS/360 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 360-30 BOS360 sysgen (64KB).bat` batch file to sysgen the operating system at real hw speed. Will need 5h aprox. 
* execute `run CPanel IBM 360-30 BOS360 (1966).bat` batch file to IPL the system. Then drop jcl file into card reader and press Enter in console to run it.
* execute `run IBM 360 BOS360 Assembler 16K (1966).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 360 BOS360 COBOL-D (1966).bat` same as above
* execute `run IBM 360 BOS360 FORTRAN IV (1966).bat` same as above
* execute `run IBM 360 BOS360 RPG 16K V1.L0 (1966).bat` same as above
* execute `run IBM 360 BOS360 sysgen (64KB).bat` to sysgen the system at max speed, no control panel. Will need 1min aprox
 
  ![IBM 360 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360M30.png)

### Model 40 

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360_TOS360.zip) to download TOS/360 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 360-40 TOS360 r14 sysgen (64K).bat` batch file to sysgen the operating system at real hw speed. Will need 9h aprox. Be patient!
* execute `run CPanel IBM 360-40 TOS360 r14 (1973).bat` batch file to IPL the system. Then drop jcl file into card reader and press Enter in console to run it.
* execute `run IBM 360 TOS360 r14 Assembler (1973).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 360 TOS360 r14 Basic FORTRAN IV (1973).bat` same as above
* execute `run IBM 360 TOS360 r14 COBOL-D (1973).bat` same as above
* execute `run IBM 360 TOS360 r14 PLI (1973).bat` same as above
* execute `run IBM 360 TOS360 r14 RPG (1973).bat` same as above
* execute `run IBM 360 TOS360 r14 sysgen (64K).bat` to sysgen the system at max speed, no control panel. Will need 3min aprox
 
  ![IBM 360 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360M40.png)

### Model 50 

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360_DOS360.zip) to download DOS/360 test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 360-50 DOS360 r26.2 sysgen (256KB).bat` batch file to sysgen the operating system at real hw speed. Will need 4h aprox
* execute `run CPanel IBM 360-50 DOS360 r26.2 (1975).bat` batch file to IPL the system. Then drop jcl file into card reader to run it.
* execute `run IBM 360 DOS360 r26.2 Assembler (1975).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 360 DOS360 r26.2 ANS COBOL (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 COBOL-D (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 FORTRAN IV (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 IVP Basic FORTRAN (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 PLI-D (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 RPG (1975).bat` same as above
* execute `run IBM 360 DOS360 r26.2 sysgen (256KB).bat` to sysgen the system at max speed, no control panel. Will need 40min aprox
 
  ![IBM 360 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360M50.png)

### Model 65 

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360_OS360.zip) to download OS/360 MVT test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 360-65 OS360 MVT r21.8F sysgen (1MB).bat` batch file to sysgen the operating system at real hw speed. Will need 7h15min aprox
* execute `run CPanel IBM 360-65 OS360 MVT r21.8F (1974).bat` batch file to IPL the system. Then drop jcl file into card reader to run it.
* execute `run IBM 360 OS360 MVT r21.8F Assembler F (1971).bat` batch file to run a sample program (no control panel, max speed)
* execute `run IBM 360 OS360 MVT r21.8F ANS Cobol V2 LVL78 (1972).bat` same as above
* execute `run IBM 360 OS360 MVT r21.8F Cobol E (1967).bat` same as above
* execute `run IBM 360 OS360 MVT r21.8F Fortran H (1974).bat` same as above
* execute `run IBM 360 OS360 MVT r21.8F PLI-F v5.5 (1968).bat` same as above
* execute `run IBM 360 OS360 MVT r21.8F RPG V1M10 (1975).bat` same as above
* execute `run IBM 360 OS360 MVT r21.8F sysgen (1MB).bat` to sysgen the system at max speed, no control panel. Will need 45min aprox
* execute `run IBM 360 OS360 MVT r21.8F TSO (1974).bat` to open an interactive TSO session (max speed, no control panel)
 
  ![IBM 360 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/IBM360/IBM360M65.png)

## IBM 701

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i701/IBM701.zip) to download all the test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 701 demo.bat` batch file to demo computer control panel and its i/o equipement 
* execute `run IBM 701 NAA SpeedEx Assembler (1953).bat` batch file to assemble and run a sample program 
* execute `run IBM 701 NR9003 Assembly (1952).bat` batch file to assemble and run a sample program with Nathaniel Rochester Symbolic Assembler
* execute `run IBM 701 SO2 Regional Assembler (1952).bat` batch file to assemble and run a sample program with William F. McClelland Assembler  
 
  ![IBM 701 DEMO](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i701/i701.png)

## IBM NORC

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/NORC/NORC.zip) to download all the test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM NORC PERT.bat` batch file to run a sample PERT processing
* execute `run CPanel IBM NORC tape demo.bat` to demo tape rewind and forwards read
 
  ![IBM NORC PERT](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/NORC/NORC.png)

## IBM 650 

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i650/IBM650.zip) to download all the test run.

Once uncompressed, you will find several ready to run .bat files: 

* execute `run CPanel IBM 650 FORTRANSIT graph.bat` batch file to run a sample FORTRANSIT program
 
  ![IBM 650 FORTRANSIT](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i650/i650_fortansit.png)

* execute `run CPanel IBM 650 SORT 1 (only Tape).bat` to demo a tape sort

  ![IBM 650 SORT TAPE](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i650/i650_sort1.png)

* execute `run CPanel IBM 650 SORT 2 (Tape+Ramac).bat` to demo a tape + RAMAC sorting

  ![IBM 650 SORT TAPE+RAMAC](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i650/i650_sort2.png)

* execute `run CPanel IBM 650 SuperSoap.bat` to demo a SuperSoap assembly

* execute `run CPanel IBM 650 Test All (long run).bat` for a long run of demos (SOAP, Regional Assembler, Bell Interpretive System, Floating Decimal System, Super Soap, IT and FORTRANSIT)

## IBM 700/7000 
### IBM 704 

![IBM 704](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i704.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM704_UASAP.zip) to download SHARE UASAP Assembler test run.

### IBM 709

![IBM 709](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i709.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM709_9AP.zip) to download 9AP Assembler test run.

### IBM 7090

Compact control panel

![IBM 7090 Compact](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i7090_compact.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7090_Edit_Map_IBSYS.zip) to download IBSYS Edit Map using compact control panel.

Full control panel

![IBM 7090](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i7090.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7090_IBSYS_Build.zip) to download Build IBSYS from source files.

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7090_IBSYS_Fortran_II_cobol.zip) to download IBSYS Fortran II and COBOL test run.

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7090_Lisp_1.5.zip) to download Lisp 1.5 test run.

### IBM 7094

Regular control panel

![IBM 7094](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i7094.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7094_IBSYS_Fortran_IV.zip) to download IBSYS Fortran IV test run.

Control panel with full HW setup needed to run CTSS

![IBM 7094 CTSS](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/i7094_ctss.png)

Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/test_run/i7000/IBM7094_with_CTSS.zip) to download CTSS test run.

