
This is the ReadMe file for IBM 701 simulator
(C) Roberto Sancho 2021
https://github.com/rsanchovilla/SimH_cpanel


System requirements
-------------------

  - Microsoft Windows 7 and above
  - Aprox 10 MB of free space

Quick run
---------

  - Decompress the IBM701.zip file. A IBM701 folder will be created.
  - Open IBM701 folder with Windows Explorer. Notice the "run*.bat" files.
  - Each one of these files starts the simulator to run
    the named software program:
      - the "run CPanel*.bat" files starts simulator with Control Panel GUI.
        programs runs at realistic speed.
      - the "run IBM 701*.bat" files starts simulator on text mode 
        programs runs at max speed allowed by host.
  - Click on close window icon to quit the simulator.

Using Control Panel GUI
-----------------------

  HotKeys on Control Panel window (^Y means pressing Control Y)

  +/- Zoom In/Zoom Out (also ^+ and ^-)
  ^Y  Toggle zoom 100%% <-> 50%%
  ^T  Toggle flash on clickable controls
  ^I  Toggle info panel      
  ^F  Fast mode: while pressed, accelerates cpu to max speed

  Click on control panel image and drag mouse to move the window.
  Right click mouse to show a tooltip with image at 100% scale.

  the Complete doc on simulator is here:
  https://github.com/rsanchovilla/SimH_cpanel/blob/master/Doc/i701_doc.docx

Some cool programs to run
-------------------------

run IBM 701 NAA SpeedEx Assembler (1953).bat

   Will run the NorthAmerican Aviation SpeedEx assembler program 
   and assemble ./IBM701/sw/HelloWorld/HelloWorld_naa.txt
   using original recovered program from source listing from 1953. 
   This is the oldest existing assembler of any IBM computer.
   Use your favorite text editor to view/play with .txt
   source program

run IBM 701 NR9003 Assembly (1952).bat

   Will run the Nathaniel Rochester NR9003 IBM produced assembler program 
   and assemble ./IBM701/sw/HelloWorld/HelloWorld_assembly.txt
   The assember has been rewritten using original doc from IBM.
   Use your favorite text editor to view/play with .txt
   source program

run CPanel IBM 701 demo.bat

   Will demo the assemblers using tapes 
   You can see all the hw working at real speed


Enjoy
Roberto Sancho
2021




