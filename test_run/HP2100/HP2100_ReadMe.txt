
This is the ReadMe file for HP2100 simulator
(C) Roberto Sancho 2023
https://github.com/rsanchovilla/SimH_cpanel


System requirements
-------------------

  - Microsoft Windows 7 and above
  - Aprox 10 MB of free space

Quick run
---------

  - Decompress the HP2100.zip file. A HP2100 folder will be created.
  - Open HP2100 folder with Windows Explorer. Notice the "run*.bat" files.
  - Each one of these files starts the simulator to run
    the named software program:
      - the "run CPanel*.bat" files starts simulator with Control Panel GUI.
        programs runs at realistic speed.
      - the "run HP2100*.bat" files starts simulator on text mode 
        programs runs at max speed allowed by host.
  - Click on close window icon to quit the simulator.

Using Control Panel GUI
-----------------------

  HotKeys (^Y means pressing Control Y)

     on Control Panel window:
                 +/- Zoom In/Zoom Out (also ^+ and ^-)
                 ^Y  Toggle zoom 100%% <-> 50%%
                 ^T  Toggle flash on clickable controls
                 ^I  Toggle info panel      
     On Plotter window:   
                 +/- and ^+, ^-, ^Y as above           
                 ^F  Fast mode: while pressed, accelerates plotter drawind
                 ^P  Saves the current image to "Plotter_Image_NN.png" file
     On Scope window:               
                 +/- and ^+, ^-, ^Y as above           
                 ^I  Toggle info panel (readable at 100%% zoom)      
                 i/I Increase/decrease intensity
                 f/F Widens/narrows beam spot focus
                 p/P Increase/decrease tube persistence time 
                 e   Bluk Erase (only for Storage Scope) 

  Click on control panel image and drag mouse to move the window.
  Right click mouse to show a tooltip with image at 100% scale.

  the Complete doc on simulator is here:
  https://github.com/rsanchovilla/SimH_cpanel/blob/master/Doc/HP2100_doc.docx

Some cool programs to run
-------------------------

run CPanel HP21MX E-Series - RTE-IV BASIC1000 Tie Fighter Rotating Demo.bat

   Will simulate an HP21MX E-Series with control panel at realistic speed
   will simulate a HP1351A graphics translator with a HP1300 X-Y Display Scope 
   will run under RTE-IV the Basic-1000 program 
   ./HP2100/sw/RTE-IV/test_run/Tie_Fighter_Rotating_Demo.bas.txt
   use your favorite text editor to view/play with .txt basic source program
   will build up wireframe a Star Wars Tie Fighter and the rotate it
   play with Intensity, Focus and Persistence knobs for extra visual fun

run CPanel HP1000 M-Series - RTE-III BASIC1000 Plotter Demo Shuttle.bat

   Will simulate an HP1000 M-Series with control panel at realistic speed
   will simulate a HP7210A Flatbed plotter
   will run under RTE-III the Basic-1000 program 
   ./HP2100/sw/RTE-III/test_run/Plotter_HPGL_Interpreter.bas.txt
   with file ./HP2100/sw/RTE-III/test_run/Shuttle.hpgl.txt attached
   as paper tape input
   use your favorite text editor to view/play with .txt basic source program
   will draw on plotter the iconic HP shuttle demo image

run CPanel HP2100S - RTE-B Landscape Demo.bat

   Will simulate an HP2000S with control panel at realistic speed
   will run under RTE-B a Basic program to draw the same fractal 
   lanscape on all the simulated output devices:
   will show the landscape as 256x256 dots 8-colours image using 
   HP91200B TV interface, then will show the landscape on a green
   HP1300A X-Y scope, then will show the landscape on orange HP1331A
   storage scope, then will show the landscape on as vectors on a 
   HP1300 monitor using a HP1351A graphics translator, and finally  
   will show the landscape on a HP7210A Flatbed plotter.

   The source programs are located in ./HP2100/sw/RTE-B/test_run/
   Landscape_TV.bas.txt
   LandScape_Scope.bas.txt
   LandScape_Storage.bas.txt
   LandScape_Graphics_Translator.bas.txt
   Landscape_Plotter.bas.txt
   
run CPanel HP2116A - BCS Scope Demo.bat

   Will simulate an HP2116A with control panel at realistic speed
   will run under BCS a Fortran and assembler program to demo 
   an blue HP1300A X-Y scope. Will compile & assemble the programs
   at ./HP2100/sw/BCS/bcs_scope 

   22316-80001_Rev-A_SCOP3.asm.txt     
   22316-80001_Rev-A_SCP3D.ftn.txt
   22315-80001_Rev-A_SCOP1.asm.txt     
   22315-80001_Rev-A_SCPD1.ftn.txt
   22080-80001_B_HP2331A_X-Y_DISP_DRIVER.asm.txt   
   22080-80001_B_HP2331A_X-Y_TST.ftn.txt

run CPanel HP2116A - BCS Build and Run Algol Chess (1973).bat
run CPanel HP2116B - PaperTape Basic Scope Demo.bat
run CPanel HP2116B - PaperTape Basic Plotter Demo.bat
run CPanel HP2116B - PaperTape Basic Games.bat
run CPanel HP2100S - RTE-B Mandelbrot Demo on TV.bat
run CPanel HP2100S - RTE-B Sample Graphics Translator.bat
run CPanel HP2100S - RTE-B Sample Plotter.bat
run CPanel HP2100A - Plotter Diagnostics (1972).bat
run CPanel HP21MX M-Series - TV Diagnostics (1976).bat

   are other cool programs to try

Enjoy
Roberto Sancho
Nov 2023




