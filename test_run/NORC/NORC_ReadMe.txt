
This is the ReadMe file for IBM NORC simulator
(C) Roberto Sancho 2021
https://github.com/rsanchovilla/SimH_cpanel


System requirements
-------------------

  - Microsoft Windows 7 and above
  - Aprox 10 MB of free space

Quick run
---------

  - Decompress the NORC.zip file. A MarkI folder will be created.
  - Open NORC folder with Windows Explorer. Notice the "run*.bat" files.
  - Each one of these files starts the simulator to run
    the named software program:
      - the "run CPanel*.bat" files starts simulator with Control Panel GUI.
        programs runs at realistic speed.
  - Click on close window icon to quit the simulator.

Using Control Panel GUI
-----------------------

  HotKeys on Control Panel window (^Y means pressing Control Y)

  +/- Zoom In/Zoom Out (also ^+ and ^-)
  ^Y  Toggle zoom 100%% <-> 50%%
  ^T  Toggle flash on clickable controls
  ^I  Toggle info panel      
  ^F  Fast mode: while pressed, accelerates cpu to max speed
  0-9/BackSpace
       Console keyboard digit (operative when GUI has focus, 
       cpu is stoped, keyboard entry switch set to reg1/reg2) 

  Click under tape window to open/close the tape cabinet and show 
  vaccum columns

  Click on control panel image and drag mouse to move the window.
  Right click mouse to show a tooltip with image at 100% scale.

  the Complete doc on simulator is here:
  https://github.com/rsanchovilla/SimH_cpanel/blob/master/Doc/NORC_doc.docx

Some cool programs to run
-------------------------

run CPanel IBM NORC PERT (1958).bat

   Will run PERT program on a event inputlist 
   using recovered binary from original printout dump.

run CPanel IBM NORC tape demo.bat

   Demoes tape forward and backward medium movement
   Demoes register CRT display (infinite count)

Enjoy
Roberto Sancho
Jan 2021




