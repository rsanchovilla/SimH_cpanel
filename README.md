# SimH_cpanel


**NEW!: IBM 650 Control Panels**

The aim of this project is to add visuals to SimH emulator/simulator on an interactive GUI.

The visuals covers interactive control panels (lights, switches, buttons) but also
other devices: tapes, card readers, disk, etc.

i7000 work is based on Richard Cornwell own's SimH fork, and is implemented using SimH own SDL based modules. 
i650 work is based on my own i650 SimH simulator

Currently suppots visuals emulation for:

* IBM 650 Magnetic Drum Data Processing machine. 
  * IBM 727 tapes, IBM 533 card read punch, and IBM 355 RAMAC Disk Storage
  * panels for IBM 653 Storage Unit and IBM 652 Control Unit

  Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%20650%20Panels%20Screen%20Shots.png) to preview control panels.
   Source code in simh-master folder


* IBM 7000 Series  
  * IBM 704 and 709 control panel. 
  * IBM 727 and 729 tapes
  * IBM 766 Data Synchorinizer
  * IBM 7090/7094 control panel. 
  * IBM 7617 Data Channel console and IBM 729 tapes
  * IBM 711 card punch, IBM 7909 Data Channel panel, IBM 7631 File Control panel and IBM 2302 Disk 
       
   Click [here](https://github.com/rsanchovilla/SimH_cpanel/blob/master/cpanel/bak/IBM%207000%20Panels%20Screen%20Shots.png) to preview how control panels.
   Source code in sims-master folder

# Test Run Software Kits

## Common keys
Several ready to run sw kits are available to demo control panel operation (Windows only)

Hot keys are available when GUI window has the focus:
* Control E -> Halt
* Control T -> Toggles mark on clickable areas
* Control Y -> Toggles size of GUI (half size <-> full size)
* **+** (plus) and **-** (minus) keys -> Zoom In/Zoom Out
* Control I -> Toggle info panel 
* Control F -> Fast mode: while pressed, accelerates cpu to max speed

Press power button to quit the simulator.
(On i7000 control panels, only ^E, ^Y and ^T available)

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

