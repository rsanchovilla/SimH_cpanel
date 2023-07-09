
http://www.waveguide.se/?article=reading-flex-disk-images


FLEX disk layout
FLEX has a hard coded sector size of 256 bytes. Addressing sectors 
are done by track and sector numbers. Maximum number of tracks are 256 
and maximum number of sectors per track are 256, which means a single 
FLEX file system can be up to 256^3 bytes which is about 16MB raw capacity. 
Not very big by todays standards but 30 years ago this was huge. The 
sectors are linked together like linked lists where the first two bytes 
of each sector is a pointer to the next sector in the chain (actually some 
sectors on track 00 are exceptions from this but for the rest of the disk 
this is true). The first two bytes of every directory sector and file 
data sector contains the track and address of the next sector in the 
chain. End of chain is marked by setting track and sector to zero. 

A typical FLEX disk layout for a 40 track floppy with 20 sectors on each track:
TRACK 00 SECTOR 00 --- Boot sector
TRACK 00 SECTOR 01 --- Boot sector
TRACK 00 SECTOR 03 --- System Information Record (SIR)
TRACK 00 SECTOR 04 --- Not used
TRACK 00 SECTOR 05 --- Start of directory
 .
 .
TRACK 00 SECTOR 20 --- End of directory
TRACK 01 SECTOR 01 --- Start of file data
 .
 .
TRACK 39 SECTOR 20 --- End of file data (last sector on disk)

The first sectors are boot sectors that contain executable code for 
loading the FLEX kernel from disk and booting the system. They can be 
ignored by the image reader and non-bootable disks. Tracks appears 
to be numbered from zero and up, and sectors from one and up. The 
boot sectors are exceptions from this and appears to be numbered 00 
and 01. Track 00 has no sector 02. The first important sector is the 
System Information Record (SIR). The SIR contains basic information 
about the disk structure. After the SIR follows the directory sectors. 
The directory sectors are linked together to form one chain and the file 
data sectors are linked together to form another chain. The length of 
the directory can be varied and can span over multiple tracks if needed. 
The size of the directory is however determined when initializing the 
disk and it can usually not be changed at a later time.

Below is the structure of the System Information Record (SIR).
  11 byte --- Volume label
   2 byte --- Volume number
   1 byte --- First free track
   1 byte --- First free sector
   1 byte --- Last free track
   1 byte --- Last free sector
   2 byte --- Number of free sectors
   1 byte --- Date month
   1 byte --- Date day
   1 byte --- Date year
   1 byte --- End track
   1 byte --- End sector

The SIR structure is 24 bytes long and starts at byte 17 of the SIR 
sector. The first 16 bytes of the SIR sector is not used.

The directory structure (DIR) follows a similar pattern.
   8 byte --- File name
   3 byte --- File extension
   2 byte --- Not used
   1 byte --- Start track
   1 byte --- Start sector
   1 byte --- End track
   1 byte --- End sector
   2 byte --- Total number of sectors
   1 byte --- Random file flag
   1 byte --- Not used
   1 byte --- Date month
   1 byte --- Date day
   1 byte --- Date year

The DIR structure is also 24 bytes long. Just as with the SIR sector the 
first 16 bytes of every directory sector is not used. The DIR structure 
is repeated 10 times for each directory sector until the end of the sector 
chain has been reached (all directory sectors does have the 16 byte padding 
at the start). One thing that had me puzzled for a while is how FLEX actually 
marks a file as "deleted" in the directory. After some trial and error I 
have discovered that deleted file entries are marked by setting the file 
first byte of the file name to either 0x00 or 0xFF. Another thing that had 
me scratching my head was the flag byte. It is used for FLEX "Random files". 
I have not implemented support for this file type so the flag is simply ignored 
but displayed.

Each DIR structure contains a pointer to the first sector of the file it 
represents. Reading a file is a matter of simply walking the sector chain 
starting at the sector mentioned in the DIR structure.

Each data sector has the structure below.
   1 byte --- Next track
   1 byte --- Next sector
   2 byte --- Sector sequence number
 252 byte --- File data

Every sector contains a pointer to the next sector in the file, a sequence 
number and 252 bytes of actual file data. End of chain (and end of file) is 
marked by setting next track and next sector values to zero. The sequence 
number is a 16 bit value starting at 1 and is increased by 1 for every 
sector in the file. It can be used as as simple verification that the file 
chain appears to be correct. The last sequence number on the last sector of 
the file should have the same value as the total sectors value of the DIR 
structure.


Boot Sector disassembly

**DIS 2400,2500

2400 LDS   #$A07F
2403 BRA   $240D
2405 ***   01
2406 ***   01
2407 ***   00
2408 ***   00
2409 ***   00
240A ***   00
240B ***   00
240C ***   00
240D LDA A #$0B		
240F STA A $8018	WR 8018=0B=0000 1011 Set Step rate
2412 BSR   $2454	DELAY
2414 BSR   $246E	WAIT Drive Not Busy
2416 BSR   $2476	Load Track/Sect 2405/6 at A1D4-A2D4, IX=start of data
2418 BSR   $2456
241A CMP A #$02
241C BEQ   $2431
241E CMP A #$16
2420 BNE   $2418
2422 BSR   $2456
2424 STA A $A107
2427 BSR   $2456
2429 STA A $A108
242C LDX   $A107
242F JMP   0,X		JMP AD00
2431 BSR   $2456
2433 PSH A
2434 BSR   $2456
2436 PUL B
2437 STA A $A10A
243A STA B $A109
243D BSR   $2456
243F TAB
2440 BEQ   $2418
2442 PSH B
2443 BSR   $2456
2445 PUL B
2446 LDX   $A109
2449 STA A 0,X
244B INX
244C STX   $A109
244F DEC B
2450 BNE   $2442
2452 BRA   $2418

2454 BRA   $24CF

2456 LDX   $A10B
2459 CPX   #$A2D4
245C BEQ   $2463
245E LDA A 0,X
2460 INX
2461 BRA   $24A9
2463 LDX   #$A1D4
2466 LDA A 0,X
2468 LDA B 1,X
246A BSR   $247C
246C BRA   $2456

246E LDA B $8018	RD 8018=Status
2471 BIT B #$01		  Busy?
2473 BNE   $246E	  if Yes then wait
2475 RTS

Load Track A/B at A1D4-A2D4, IX=start of data

2476 LDA A $2405	A=01
2479 LDA B $2406	B=01
247C BSR   $24AD	Seek Track/Sector in A/B (at $4800 on disk image file)
247E LDA A #$8C
2480 STA A $8018	WR 8018=8C=Read side 1
2483 BSR   $24CF	DELAY
2485 CLR B
2486 LDX   #$A1D4	
2489 LDA A $8018	RD 8018=Drive Status
248C BIT A #$02		  Data to read?	
248E BNE   $2497	  jmp if true
2490 BIT A #$01		  Drive busy?
2492 BNE   $2489	  jmp if true
2494 JMP   $A100
2497 LDA A $801B	RD 801B=data Reg
249A STA A 0,X		  store data from disk
249C INX
249D DEC B
249E BNE   $2489
24A0 BSR   $246E	WAIT Drive Not Busy
24A2 BIT B #$1C
24A4 BNE   $2494
24A6 LDX   #$A1D8
24A9 STX   $A10B
24AC RTS

Seek Track/Sector in A/B

24AD CMP A $8019	RD 8019=Current Track
24B0 BEQ   $24C2
24B2 STA A $801B	WR 801B=Data register=A
24B5 BSR   $24CF	DELAY
24B7 LDA A #$18
24B9 STA A $8018	WR 8018=18=Seek Track in data register
24BC BSR   $24CF	DELAY
24BE PSH B
24BF BSR   $246E	WAIT Drive Not Busy
24C1 PUL B
24C2 STA B $801A	WR 801A=Set Current Sector
24C5 CLR A
24C6 CMP B #$0A
24C8 BLS   $24CC
24CA LDA A #$80
24CC STA A $8014	WR 8014=Motor on

24CF BSR   $24D1	DELAY
24D1 BSR   $24D3	DELAY
24D3 RTS




fdos boot sector:

2400 JSR   $240C
2403 LDX   $0B
2405 LDS   #$A049
2408 JSR   0,X
240A BRA   $2403
240C LDX   #$240C
240F STX   $0B
2411 CLR A
2412 STA A $0D
2414 STA A $00
2416 LDA A #$02
2418 STA A $01
241A LDX   #$2FFF
241D STX   $06
241F LDX   #$2600
2422 BSR   $242B
2424 LDX   #$2600
2427 STX   $0B
2429 JMP   0,X
242B JSR   $2523
242E JSR   $250E
2431 STX   $04
2433 CLR   $0003
2436 LDX   $04
2438 LDA B $01
243A STA B $801A
243D JSR   $2489
2440 LDA B #$9C
2442 STA B $8018
2445 JSR   $2489
2448 JSR   $2489
244B JSR   $2489
244E LDA B $8018
2451 BIT B #$01
2453 BEQ   $2465
2455 BIT B #$02
2457 BEQ   $244E
2459 LDA A $801B
245C STA A 0,X
245E CPX   $06
2460 BEQ   $246E	
2462 INX
2463 BRA   $244E
2465 BSR   $2476
2467 BNE   $2436
2469 JSR   $24F8
246C BRA   $242E
246E BSR   $2476
2470 BNE   $2436
2472 JSR   $24F8
2475 RTS
2476 LDA B $8018
2479 STA B $02
247B LDA A #$D0
247D STA A $8018
2480 JSR   $25D7
2483 LDA B $02
2485 AND B #$1C
2487 BNE   $248A
2489 RTS
248A AND B #$10
248C BEQ   $2497
248E LDA B $801A
2491 CMP B #$0A
2493 BNE   $2497
2495 CLR B
2496 RTS
2497 LDA B $03
2499 INC B
249A STA B $03
249C CMP B #$06
249E BNE   $2489
24A0 LDA B $801A
24A3 STA B $01
24A5 LDX   #$0000
24A8 BSR   $24F5
24AA BSR   $24F5
24AC LDX   #$24D4
24AF BSR   $24F2
24B1 LDX   #$24BE
24B4 BSR   $24F2
24B6 LDX   #$24C7
24B9 BSR   $24F2
24BB JMP   $2403
24BE SEC
24BF CLV
24C0 ***   15
24C1 COM A
24C2 ASL A
24C3 ***   45
24C4 COM A
24C5 ***   4B
24C6 ***   04
24C7 BRA   $24F6
24C9 BRA   $250F
24CB ROL A
24CC COM B
24CD ***   4B
24CE BRA   $2515
24D0 ***   52
24D1 ***   52
24D2 CLR A
24D3 ***   52
24D4 SEC
24D5 CLV
24D6 ***   15
24D7 ***   04
24D8 SEC
24D9 CLV
24DA ***   15
24DB ***   4E
24DC CLR A
24DD LSR B
24DE BRA   $2532
24E0 ***   45
24E1 ***   41
24E2 LSR A
24E3 ROL B
24E4 ***   04
24E5 SEC
24E6 CLV
24E7 ***   15
24E8 NEG B
24E9 ***   52
24EA CLR A
24EB LSR B
24EC ***   45
24ED COM A
24EE LSR B
24EF ***   45
24F0 LSR A
24F1 ***   04
24F2 JMP   $E07E
24F5 JMP   $E0C8
24F8 PSH B
24F9 LDA B $801A
24FC CMP B #$0A
24FE BNE   $2507
2500 CMP A 161,X
