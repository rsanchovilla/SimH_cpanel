	Tera Term Pro version 2.3
	for MS-Windows 95/NT
	T. Teranishi Mar 10, 1998

	Copyright (C) 1994-1998 T. Teranishi
	All Rights Reserved.

Index

  1. About Tera Term
  2. Notice
  3. Requirements
  4. Installation
  5. Usage
  6. Acknowledgment

-------------------------------------------------------------------------------
1. About Tera Term

Tera Term (Pro) is a free software terminal emulator (communication program)
which supports:

	- Serial port connections.
	- TCP/IP (telnet) connections.
	- VT100 emulation, and selected VT200/300 emulation.
	- TEK4010 emulation.
	- File transfer protocols (Kermit, XMODEM, ZMODEM, B-PLUS and
	  Quick-VAN).
	- Scripts using the "Tera Term Language".
	- Japanese and Russian character sets.

-------------------------------------------------------------------------------
2. Notice

Tera Term (Pro) is a free software.

There is no warranty for damages caused by using this application.

Without written permission by the author (Takashi Teranishi), you may
not distribute modified packages of Tera Term, and may not distribute
Tera Term for profit.

Before you send requests, questions and bug reports to the author,
please read carefully this file, Tera Term help, MACRO help, KEYCODE.TXT
and Tera Term home page.
You can find the e-mail address of the author in Tera Term help and
Tera Term home page. The URL of Tera Term home page is:

http://www.vector.co.jp/authors/VA002416/teraterm.html
(The address may be changed in future.)

The author will appreciate any kind of support from you
if it does not put the author under some obligation.

You may find the latest version of Tera Term at Tera Term home page and
the following sites:

ftp://riksun.riken.go.jp/pub/pc/misc/terminal/teraterm/
ftp://ftp.s.u-tokyo.ac.jp/PC/terminal/teraterm/
(These addresses may be changed in future.)

-------------------------------------------------------------------------------
3. Requirements

Supported operating systems:

	MS-Windows 95
	MS-Windows NT 3.51 and 4.0

	Note: For Windows 3.1, use Tera Term version 1.X.

-------------------------------------------------------------------------------
4. Installation

1) If you have installed an old version of Tera Term, uninstall it.

2) Copy all the distribution files to an empty floppy disk or temporary
directory (for example C:\TEMP).

3) Run SETUP.EXE and follow the instruction given by it.

4) After the installation, the distribution files are no longer needed.
you can delete them or may keep them in the floppy disk.

You can specify the following command line options for SETUP.EXE:

Format:
	SETUP.EXE [/S] [/L<language mode>] [<destination path>]

  where:
	/S			Skips user input dialog boxes

	/L<language mode>	Language mode of Tera Term.
	  /LE		English.
	  /LJ1		Japanese (IBM PC/AT keyboard).
	  /LJ2		Japanese (NEC PC98 keyboard; Windows 95).
	  /LJ3		Japanese (NEC PC98 keyboard; Windows NT).
	  /LR		Russian.

	<destination path>	Directory to which Tera Term is installed.

Example:
	SETUP.EXE /S /LE "C:\Program Files\TTERMPRO"

-------------------------------------------------------------------------------
5. Usage

1) For a telnet connection
  Enter the host name or IP address of the host in
  the "[File] New connection" dialog box.

2) For a serial port connection
  Select the serial port in the "[File] New connection" dialog box.
  Set appropriate parameters for the port in the "[Setup] Serial port
  dialog box.

See Tera Term help for detail.
See KEYCODE.TXT for the description of keyboard setup.
See MACRO help (MACRO.HLP) for the description of scripts.

-------------------------------------------------------------------------------
6. Acknowledgment

I would like to thank everyone who sent bug reports and suggestions.
I especially thank the people who have supported the development of
Tera Term from very early on. I also wish to thank the following people:

	Members of experimental nuclear physics groups in RIKEN and
		University of Tokyo for supporting the development.
	Akihiro Shirahashi for helping with distribution of Tera Term and
		maintaining the Tera Term mailing list.
	Hirofumi Fujii for giving useful information on terminal emulation
		and communication programming.
	Luigi M Bianchi for helping with the documentation included in this
		package.
	Yukio Mitsuyama and Yoshihiro Yamato for testing Tera Term with
		PC-98 keyboard.
	Vector Design company for providing a place for Tera Term home page.
	Ralf Schares for helping with the modification of the Tera Special
		font.
	Juri Tsibrovski and Serguei Lukyanov for giving information about
		Russian character code sets.
	Andrey Nikiforov for making a Russian code conversion table.
	Tatsuhiko Sakamoto for making new icons.
	Robert O'Callahan for helping with the development of Tera Term
		extension interface.