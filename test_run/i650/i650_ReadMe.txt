
This is the ReadMe file for IBM 650 simulator
(C) Roberto Sancho 2018
https://github.com/rsanchovilla/SimH_cpanel


System requirements
-------------------

  - Microsoft Windows 7 and above
  - Aprox 10 MB of free space

Quick run
---------

  - Decompress the IBM650.zip file. A IBM650 folder will be created.
  - Open IBM650 folder with Windows Explorer. Notice the "run*.bat" files.
  - Each one of these files starts the simulator to run
    the named software program:
      - the "run CPanel*.bat" files starts simulator with Control Panel GUI.
        programs runs at realistic speed.
      - the "run IBM 650*.bat" files starts simulator on text mode 
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
  https://github.com/rsanchovilla/SimH_cpanel/blob/master/Doc/i650_doc.docx

Some cool programs to run
-------------------------

run CPanel IBM 650 FORTRANSIT graph.bat

   Will compile and run the FORTRANSIT program 
   ./IBM650/sw/fortransit/fortransit_example_5_src.txt
   using original recovered compiler from 1957. This is the
   oldest existing fortran compiler.
   Use your favorite text editor to view/play with .txt
   source program

run CPanel IBM 650 SORT 2 (Tape+Ramac).bat

   Will demo a sort program using tapes and hard disk
   You can see all the hw working at real speed

run IBM 650 Bell Interpretive System (1956).bat

   Will run Bell Interpretive System by W.M Wolontis, recovered from
   original program dump. It is a direct evolution of SpeedCode
   interpretive system from IBM 701

run IBM 650 IT (1956).bat

   Will run Internal Translator created by A.J.perlis, recovered from
   original program listing. It is the first usable compiler
   acording to Donald Knuth.


Enjoy
Roberto Sancho
2018




