
This is the ReadMe file for Ferranti Mark I simulator
(C) Roberto Sancho 2023
https://github.com/rsanchovilla/SimH_cpanel


System requirements
-------------------

  - Microsoft Windows 7 and above
  - Aprox 10 MB of free space

Quick run
---------

  - Decompress the Ferranti_MarkI.zip file. A MarkI folder will be created.
  - Open MarkI folder with Windows Explorer. Notice the "run*.bat" files.
  - Each one of these files starts the MarkI simulator to run
    the named software program:
      - the "run CPanel*.bat" files starts simulator with Control Panel GUI.
        programs runs at realistic speed.
      - the "run MarkI*.bat" files open starts simulator on text mode 
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
  Keys A..Z, 0..9 can be used as keyboard printer keys if selected

  the Complete doc on simulator is here:
  https://github.com/rsanchovilla/SimH_cpanel/blob/master/Doc/MarkI_doc.docx

Some cool programs to run
-------------------------

run CPanel MarkI - Brooker Autocode Demo Sieve.bat

   Will compile and run the autocode program 
   ./MarkI/sw/Brooker_Autocode/Sample/Sieve.ba.txt
   using recovered compiler from original source code.
   use your favorite text editor to view/play with Sieve.ba.txt
   source program

run CPanel MarkI - Glennie Autocode Demo Sieve.bat

   Will compile and run the autocode program 
   ./MarkI/sw/Glennie_Autocode/Sample/Sieve.ga.txt
   using rewritten compiler based on original doc from Glennie
   use your favorite text editor to view/play with Sieve.ga.txt
   source program

run CPanel MarkI - Draughts Play (1952).bat

   For game instructions, read the excellent paper by David Link at 
   https://www.computerconservationsociety.org/resurrection/res60.htm#f

run CPanel MarkI - Love Letters (1952).bat

   Will load the program, and print a love letter.    
   when finished, (sim> prompt appears on MSwindow text console), 
   just press the KCS switch, the lower right one on GUI control panel.
   This will restart the program to generate a new letter

run CPanel MarkI - Scheme A HelloWorld.bat

   Will load Alan Turing's Scheme A, recovered from original listings, 
   one of the oldest system software known to exist.
   Then will automatically load a sample hello Word program 
   and run it. The printout is shown in MSwindow text console.

Enjoy
Roberto Sancho
May 2023




