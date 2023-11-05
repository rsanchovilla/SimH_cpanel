RTE-6/VM Rev 6200 kit

Running RTE-6/Vm under the simh emulator.

This document assumes the user is running windows. However, it should be
easily adaptable to any architecture that simh runs on by aquiring 
the appropriate simh executables. 

HOWEVER, please note that QCTerm runs only under windows. Without QCTerm,
you will be limited to a standard text terminal with no HP enhancements. 

Get the user documentation for simh at http://simh.trailing-edge.com/

Get QCTerm from AICS Research http://www.aics-research.com/qcterm/
Install it and set your connection settings to:
	Connection: "Network" 
	Network Type: "TELNET (standard)"
	Port: 10000
	Terminal Type: "hp"
	Host Name: 127.0.0.1
	Respond to ENQ: Checked

Unzip the rte6200.zip archive to a directory of your choice

run "hp2100 rte6200.sim" (windows users can just use rte6200.bat).

At this point you can log into RTE from the simh console which emulates 
a dumb terminal connected to the virtual 1000 console, or you can have QCTerm
connect with the above settings. This will give you a fairly decent HP terminal
emulation on a virtual BACI card.

The user ID is "manager.sys". The password is "hp".

The only initialized FMGR cartridge is LU 2.

Disc LUs 23 and 24 are used as CI volumes.

Disc LUs 20,21, and 22 are free for whatever you like.

There is a 2nd 7925 disc genned in as unit 3. If you uncomment the appropritate
line in the rte6200.sim file, you can get a large LU 25.

All of the simh (as of 3.8-0) virtual devices that RTE supports are genned into 
the system. The paper tape reader comes in handy for setting the time and
date on system boot. See rte6200.sim for details. The virtual tape device comes
in pretty handy too. See my file2tape and tape2file programs in the seperate
"utilities.zip" file. Also, remember that FST and TF know how to read and
write TAR files.  

I have the appropriate drivers genned in for the IPLO and IPLI simh devices. These
devices are designed for running timeshare basic and not RTE, but they may be
usable for something under RTE - I haven't had much time to experiment.

There is an 8 channel C mux genned in. Simh support for the 8 channel C mux is
coming in the near future.