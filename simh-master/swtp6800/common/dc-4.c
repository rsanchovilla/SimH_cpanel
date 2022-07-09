/*  dc4.c: SWTP DC-4 FDC Simulator

    Copyright (c) 2005-2012, William A. Beech

        Permission is hereby granted, free of charge, to any person obtaining a
        copy of this software and associated documentation files (the "Software"),
        to deal in the Software without restriction, including without limitation
        the rights to use, copy, modify, merge, publish, distribute, sublicense,
        and/or sell copies of the Software, and to permit persons to whom the
        Software is furnished to do so, subject to the following conditions:

        The above copyright notice and this permission notice shall be included in
        all copies or substantial portions of the Software.

        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
        WILLIAM A BEECH BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
        IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
        CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

        Except as contained in this notice, the name of William A. Beech shall not
        be used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from William A. Beech.

    MODIFICATIONS:

        23 Apr 15 -- Modified to use simh_debug

    NOTES:

        The DC-4 is a 5-inch floppy controller which can control up
        to 4 daisy-chained 5-inch floppy drives.  The controller is based on
        the Western Digital 1797 Floppy Disk Controller (FDC) chip. This
        file only emulates the minimum DC-4 functionality to interface with
        the virtual disk file.

        The floppy controller is interfaced to the CPU by use of 5 memory 
        addreses.  These are SS-30 slot numbers 5 and 6 (0x8014-0x801B).

        Address     Mode    Function
        -------     ----    --------

        0x8014      Read    Returns FDC interrupt status
        0x8014      Write   Selects the drive/head/motor control
        0x8018      Read    Returns status of FDC
        0x8018      Write   FDC command register
        0x8019      Read    Returns FDC track register
        0x8019      Write   Set FDC track register
        0x801A      Read    Returns FDC sector register
        0x801A      Write   Set FDC sector register
        0x801B      Read    Read data
        0x801B      Write   Write data

        Drive Select Read (0x8014):

        +---+---+---+---+---+---+---+---+
        | I | D | X | X | X | X | X | X |
        +---+---+---+---+---+---+---+---+

        I = Set indicates an interrupt request from the FDC pending.
        D = DRQ pending - same as bit 1 of FDC status register.

        Drive Select Write (0x8014):

        +---+---+---+---+---+---+---+---+
        | M | S | X | X | X | X | Device|
        +---+---+---+---+---+---+---+---+

        M = If this bit is 1, the one-shot is triggered/retriggered to 
            start/keep the motors on.
        S = Side select. If set, side one is selected otherwise side zero
            is selected.
        X = not used
        Device = value 0 thru 3, selects drive 0-3 to be controlled.

        Drive Status Read (0x8018):

        +---+---+---+---+---+---+---+---+
        | R | P | H | S | C | L | D | B |
        +---+---+---+---+---+---+---+---+

        B - When 1, the controller is busy.
        D - When 1, index mark detected (type I) or data request - read data 
            ready/write data empty (type II or III).
        H - When 1, track 0 (type I) or lost data (type II or III).
        C - When 1, crc error detected.
        S - When 1, seek (type I) or RNF (type II or III) error.
        H - When 1, head is currently loaded (type I) or record type/
            write fault (type II or III).
        P - When 1, indicates that diskette is write-protected.
        R - When 1, drive is not ready.

        Drive Control Write (0x8018) for type I commands:

        +---+---+---+---+---+---+---+---+
        | 0 | S2| S1| S0| H | V | R1| R0|
        +---+---+---+---+---+---+---+---+

        R0/R1 - Selects the step rate.
        V - When 1, verify on destination track.
        H - When 1, loads head to drive surface.
        S0/S1/S2 = 000 - home.
           001 - seek track in data register.
           010 - step without updating track register.
           011 - step and update track register.
           100 - step in without updating track register.
           101 - step in and update track register.
           110 - step out without updating track register.
           111 - step out and update track register.

        Drive Control Write (0x8018) for type II commands:

        +---+---+---+---+---+---+---+---+
        | 1 | 0 | T | M | S | E | B | A |
        +---+---+---+---+---+---+---+---+

        A - Zero for read, 1 on write deleted data mark else data mark.
        B - When 1, shifts sector length field definitions one place.
        E - When, delay operation 15 ms, 0 no delay.
        S - When 1, select side 1, 0 select side 0.
        M - When 1, multiple records, 0 for single record.
        T - When 1, write command, 0 for read.

        Drive Control Write (0x8018) for type III commands:

        +---+---+---+---+---+---+---+---+
        | 1 | 1 | T0| T1| 0 | E | 0 | 0 |
        +---+---+---+---+---+---+---+---+

        E - When, delay operation 15 ms, 0 no delay.
        T0/T1 - 00 - read address command.
                10 - read track command.
                11 - write track command.

        Tracks are numbered from 0 up to one minus the last track in the 1797!

        Track Register Read (0x8019):

        +---+---+---+---+---+---+---+---+
        |          Track Number         |
        +---+---+---+---+---+---+---+---+

        Reads the current 8-bit value from the track position.

        Track Register Write (0x8019):

        +---+---+---+---+---+---+---+---+
        |          Track Number         |
        +---+---+---+---+---+---+---+---+

        Writes the 8-bit value to the track register.

        Sectors are numbers from 1 up to the last sector in the 1797!

        Sector Register Read (0x801A):

        +---+---+---+---+---+---+---+---+
        |         Sector Number         |
        +---+---+---+---+---+---+---+---+

        Reads the current 8-bit value from the sector position.

        Sector Register Write (0x801A):

        +---+---+---+---+---+---+---+---+
        |         Sector Number         |
        +---+---+---+---+---+---+---+---+

        Writes the 8-bit value to the sector register.

        Data Register Read (0x801B):

        +---+---+---+---+---+---+---+---+
        |             Data              |
        +---+---+---+---+---+---+---+---+

        Reads the current 8-bit value from the data register.

        Data Register Write (0x801B):

        +---+---+---+---+---+---+---+---+
        |             Data              |
        +---+---+---+---+---+---+---+---+

        Writes the 8-bit value to the data register.

        A FLEX disk is defined as follows:
        (first sector on track is number 1)

        Track   Sector  Use
        0       1       Boot sector
        0       2       Boot sector (cont)
        0       3       Unused
        0       4       System Identity Record (explained below)
        0       5       Unused
        0       6-last  Directory - 10 entries/sector (explained below)
        1       1       First available data sector
        last-1  last    Last available data sector

        System Identity Record

        Byte    Use
        0x00    Two bytes of zeroes (Clears forward link)
        0x10    Volume name in ASCII(11 bytes)
        0x1B    Volume number in binary (2 bytes)
        0x1D    Address of first free data sector (Track-Sector) (2 bytes)
        0x1F    Address of last free data sector (Track-Sector) (2 bytes)
        0x21    Total number of data sectors in binary (2 bytes)
        0x23    Current date (Month-Day-Year) in binary
        0x26    Highest track number on disk in binary (byte)
        0x27    Highest sector number on a track in binary (byte)

        The following unit registers are used by this controller emulation:

        dc4_dsk_unit[cur_drv].u3        unit current flags
        dc4_dsk_unit[cur_drv].u4        unit current track
        dc4_dsk_unit[cur_drv].u5        unit current sector
        dc4_dsk_unit[cur_drv].pos       unit current sector byte index into buffer
        dc4_dsk_unit[cur_drv].filebuf   unit current sector buffer
        dc4_dsk_unit[cur_drv].fileref   unit current attached file reference
*/

#include <stdio.h>
#include "swtp_defs.h"

#define UNIT_V_ENABLE   (UNIT_V_UF + 0) /* Write Enable */
#define UNIT_ENABLE     (1 << UNIT_V_ENABLE)

/* emulate a SS FLEX disk with 72 sectors and 80 tracks */

#define NUM_DISK        4               /* standard 1797 maximum */
#define SECT_SIZE       256             /* standard FLEX sector */
#define NUM_SECT        72              /* sectors/track */
#define TRACK_SIZE      (SECT_SIZE * NUM_SECT) /* trk size (bytes) */
#define HEADS           1               /* handle as SS with twice the sectors */
#define NUM_CYL         80              /* maximum tracks */
#define DSK_SIZE        (NUM_SECT * HEADS * NUM_CYL * SECT_SIZE) /* dsk size (bytes) */

/* SIR offsets */
#define MAXCYL          0x26            /* last cylinder # */
#define MAXSEC          0x27            /* last sector # */

// current disk track and sector
#define TRK             dc4_dsk_unit[dc4.cur_dsk].u4
#define SECT            dc4_dsk_unit[dc4.cur_dsk].u5

/* 1797 status bits type I commands*/

#define NOTRDY          0x80
#define WRPROT          0x40
#define HEDLOD          0x20
#define SEEKERR         0x10
#define CRCERR          0x08
#define LOST            0x04
#define INDEX           0x02
#define BUSY            0x01

/* 1797 status bits type II/III commands*/

#define NOTRDY          0x80
#define WRPROT          0x40
#define WRTFALT         0x20
#define RECNF           0x10
#define CRCERR          0x08
#define LOST            0x04
#define DRQ             0x02
#define BUSY            0x01

/* function prototypes */

t_stat dc4_dsk_reset (DEVICE *dptr);
t_stat dc4_dsk_set_fmt(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat dc4_dsk_show_fmt(FILE *st, UNIT *uptr, int32 val, CONST void *desc);

/* SS-50 I/O address space functions */

int32 dc4_fdcdrv(int32 io, int32 data);
int32 dc4_fdccmd(int32 io, int32 data);
int32 dc4_fdctrk(int32 io, int32 data);
int32 dc4_fdcsec(int32 io, int32 data);
int32 dc4_fdcdata(int32 io, int32 data);

/* Local Variables */

struct {
    int32   fdcbyte;
    int32   intrq;                          /* interrupt request flag */
    int32   cur_dsk;                        /* Currently selected drive */
    int32   wrt_flag;                       /* FDC write flag */

    int32   spt;                            /* sectors/track */
    int32   trksiz;                         /* trk size (bytes) */
    int32   heds;                           /* number of heads */
    int32   cpd;                            /* cylinders/disk */
    int32   dsksiz;                         /* dsk size (bytes) */
    int32   sectsize;                       // Sector size (bytes)

    int32   multiple_sector;                // multiple read-write flag
    int32   index_countdown;                // index countdown for type I commands
    int32   busy_countdown;                 // busy flag countdown 
    int32   sector_base;                    // indicates is first sector on track is sector 1 or sector 0
    int32   fmt;                            // indicates the physicsl format to be used. 0=autodetect based on disk image file
} dc4 = {0};

// ***************************************************************
// important note: all disks attached should have the same geometry
// ***************************************************************

/* Floppy Disk Controller data structures

       dc4_dsk_dev        Disk Controller device descriptor
       dc4_dsk_unit       Disk Controller unit descriptor
       dc4_dsk_reg        Disk Controller register list
       dc4_dsk_mod        Disk Controller modifiers list

*/

UNIT dc4_dsk_unit[] = {
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE, 0)  }
};

REG dc4_dsk_reg[] = {
        { HRDATA (DISK, dc4.cur_dsk, 4) },
        { NULL }
};

MTAB dc4_dsk_mod[] = {
        { UNIT_ENABLE, UNIT_ENABLE, "RW", "RW", NULL },
        { UNIT_ENABLE, 0, "RO", "RO", NULL },
        {MTAB_XTD | MTAB_VDV, 0, "FMT", "FMT", &dc4_dsk_set_fmt, &dc4_dsk_show_fmt, NULL, "Set card format"},
        { 0 }
};

DEBTAB dc4_dsk_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow control" },
    { "READ", DEBUG_read, "Read Command" },
    { "WRITE", DEBUG_write, "Write Command"},
    { NULL }
};

DEVICE dc4_dsk_dev = {
    "DC-4",                             //name
    dc4_dsk_unit,                       //units
    dc4_dsk_reg,                        //registers
    dc4_dsk_mod,                        //modifiers
    NUM_DISK,                           //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    NULL,                               //examine
    NULL,                               //deposit
    &dc4_dsk_reset,                     //reset
    NULL,                               //boot
    NULL,                               //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    dc4_dsk_debug,                      //debflags
    NULL,                               //msize
    NULL                                //lname
};

/* Reset routine */

t_stat dc4_dsk_reset (DEVICE *dptr)
{
    int i;

    dc4.cur_dsk = 5;                        /* force initial SIR read */
    for (i=0; i<NUM_DISK; i++) {
        dc4_dsk_unit[i].u3 = 0;             /* clear current flags */
        dc4_dsk_unit[i].u4 = 0;             /* clear current cylinder # */
        dc4_dsk_unit[i].u5 = 0;             /* clear current sector # */
        dc4_dsk_unit[i].pos = 0;            /* clear current byte ptr */
        if (dc4_dsk_unit[i].filebuf == NULL) {
            dc4_dsk_unit[i].filebuf = malloc(SECT_SIZE); /* allocate buffer */
            if (dc4_dsk_unit[i].filebuf == NULL) {
                printf("dc-4_reset: Malloc error\n");
                return SCPE_MEM;
            }
        }
    }
    dc4.spt = 0;
    dc4.trksiz = 0;
    dc4.heds = 0;
    dc4.cpd = 0;
    dc4.dsksiz = 0;
    dc4.fmt=0;
    dc4.sectsize = SECT_SIZE; 
    dc4.multiple_sector=0;
    dc4.index_countdown=0;
    dc4.busy_countdown=0;
    dc4.sector_base=1; 
    return SCPE_OK;
}

struct {
    int         fmt;
    const char  *name;
    const char  *desc;
} disk_formats[] = {
    {0, "AUTO", "Autodetect disk format"},
    {1, "35x10x256-0", "256 bytes sector, 35 tracks, 10 sectors per track (first sector is number 0)"},
    {1, "FDOS", "SWTPC FDOS 1.0"},
    {2, "35x18x128-1", "128 bytes sector, 35 tracks, 18 sectors per track (first sector is number 1)"},
    {2, "FLEX1", "TSC FLEX 1.0"},
    {2, "CP68", "HEMENWAY CP/68"},
    {3, "SIRx256-1", "256 bytes sector, tracks and sectors defined in SIR record (first sector is number 1)"},
    {3, "FLEX2", "TSC FLEX 2.0"},
    {4, "35x18x128-1x2sides", "128 bytes sector, 35 tracks, 18 sectors per track (first sector is number 1), two sides"},
    {4, "SDOS", "SD SDOS 1.1"},
    {5, "35x18x128-0", "128 bytes sector, 35 tracks, 18 sectors per track (first sector is number 0)"},
    {5, "DOS68", "SSB DOS 68 5.1"},
    {0, 0},
};

t_stat dc4_dsk_show_fmt(FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    int f;

    for (f = 0; disk_formats[f].name != 0; f++) {
        if (dc4.fmt == disk_formats[f].fmt) {
            fprintf (st, "%s format [%s]", disk_formats[f].name, disk_formats[f].desc);
            return SCPE_OK;
        }
    }
    fprintf (st, "invalid format (fmt=%d)", dc4.fmt);
    return SCPE_OK;
}

t_stat dc4_dsk_set_fmt(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int f;

    if (uptr == NULL) return SCPE_IERR;
    if (cptr == NULL) return SCPE_ARG;
    for (f = 0; disk_formats[f].name != 0; f++) {
        if (strcmp (cptr, disk_formats[f].name) == 0) {
            dc4.fmt = disk_formats[f].fmt;
            return SCPE_OK;
            }
        }
    return SCPE_ARG;
}

/*  I/O instruction handlers, called from the MP-B2 module when a
   read or write occur to addresses 0x8004-0x8007. */

/* DC-4 drive select register routine - this register is not part of the 1797
*/

int32 dc4_fdcdrv(int32 io, int32 data)
{
    static long pos;
    static int32 err;
    uint8 * SIR; 
    int32  disk_image_size; 

    if (io) {                           /* write to DC-4 drive register */
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Drive selected %d cur_dsk=%d \n",
            data & 0x03, dc4.cur_dsk);
        if (dc4.cur_dsk == (data & 0x03)) 
            return 0;                   /* already selected */
        dc4.cur_dsk = data & 0x03;          /* only 2 drive select bits */
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Drive set to %d \n", dc4.cur_dsk);
        dc4_dsk_unit[dc4.cur_dsk].flags &= ~LOST;
        if ((dc4_dsk_unit[dc4.cur_dsk].flags & UNIT_ENABLE) == 0) {
            dc4_dsk_unit[dc4.cur_dsk].u3 |= WRPROT; /* set 1797 WPROT */
            sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Drive write protected \n");
        } else {
            dc4_dsk_unit[dc4.cur_dsk].u3 &= ~WRPROT; /* set 1797 not WPROT */
            sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Drive NOT write protected \n");
        }
        if (dc4_dsk_unit[dc4.cur_dsk].fileref==0) return 0; // no file attached
        dc4_dsk_unit[dc4.cur_dsk].u3 |= BUSY | DRQ; /* set DRQ & BUSY */
        dc4.busy_countdown=5; // start busy countdown
        dc4_dsk_unit[dc4.cur_dsk].pos = 0;      /* clear counter */
        // read SIR record at 0x200
        pos = 0x200;                    /* Read in SIR */
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Read pos = %ld ($%04X) \n",
             pos, (unsigned int) pos);
        sim_fseek(dc4_dsk_unit[dc4.cur_dsk].fileref, pos, SEEK_SET); /* seek to offset */
        sim_fread(dc4_dsk_unit[dc4.cur_dsk].filebuf, SECT_SIZE, 1, dc4_dsk_unit[dc4.cur_dsk].fileref); /* read in buffer */
        SIR = (uint8 * )(dc4_dsk_unit[dc4.cur_dsk].filebuf); 
        disk_image_size=sim_fsize(dc4_dsk_unit[dc4.cur_dsk].fileref);
        // determine disk image geometry
        if (dc4.fmt==0) {
            // autodetect format, based on image size or SIR record
            if (disk_image_size==35*10*256) { // 89600 bytes -> FDOS image
                dc4.fmt=1; //FDOS image
            } else if (disk_image_size==35*18*128) { // 80640 bytes -> FLEX 1.0 image
                dc4.fmt=2; //FLEX 1.0 image
                // note: DOS68 format (fmt=5) cannot be difierentiated from FLEX 1.0
            } else if (disk_image_size==35*18*128*2) { // 161280 bytes -> SDOS 1.1 image
                dc4.fmt=4; //SDOS 1.1 image
            } else if ((SIR[0]==0) && (SIR[1]==0)) {
                dc4.fmt=3; //FLEX 2.0 image
            } else {
                dc4.fmt=0; //Unknow OS image
            }
        }
        if (dc4.fmt==1) {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: FDOS Disk \n");
           dc4.heds = 1; // 1 side
           dc4.spt = 10; // 10 sectors
           dc4.cpd = 35; // 35 tracks
           dc4.sectsize = 256; 
           dc4.sector_base=0; // first sector in track is number ZERO
        } else if (dc4.fmt==2) {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: FLEX 1.0 Disk \n");
           dc4.heds = 1; // 1 side
           dc4.spt = 18; // 18 sectors
           dc4.cpd = 35; // 35 tracks
           dc4.sectsize = 128; 
           dc4.sector_base=1; // first sector in track is number ONE
        } else if (dc4.fmt==3) {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: FLEX 2.0 Disk \n");
           // FLEX disc has SIR record. on disk image offset $200
           dc4.heds = 1; // 1 side
           dc4.spt = SIR[MAXSEC]; // Highest numbero of tracks. As in FLEX sectors are numbered as 1,2,..Hi this is also the number of sectors per track
           dc4.cpd = SIR[MAXCYL]+1; // highest track number . Of FLEX, first track is track zero
           dc4.sectsize = 256; 
           dc4.sector_base=1; // first sector in track is number ONE
        } else if (dc4.fmt==4) {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: SDOS 1.1 Disk\n");
           dc4.heds = 2; // 2 sides
           dc4.spt = 18; // 18 sectors
           dc4.cpd = 35; // 35 tracks
           dc4.sectsize = 128; 
           dc4.sector_base=1; // first sector in track is number ONE
        } else if (dc4.fmt==5) {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: DOS-68 Disk \n");
           dc4.heds = 1; // 1 side
           dc4.spt = 18; // 18 sectors
           dc4.cpd = 35; // 35 tracks
           dc4.sectsize = 128; 
           dc4.sector_base=0; // first sector in track is number ZERO
        } else {
           sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Unknown type disk \n");
           dc4.heds = 1; // 1 side
           dc4.spt = 18; 
           dc4.sectsize = 128; 
           dc4.cpd = disk_image_size / (dc4.spt * dc4.sectsize); 
           dc4.sector_base=1; // first sector in track is number ONE
        }
        dc4.trksiz = dc4.spt * dc4.sectsize;
        dc4.dsksiz = dc4.trksiz * dc4.cpd * dc4.heds;
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Geometry: Sector Size %d, Sectors per Track %d, Tracks %d, Sides %d, Disk Size %d \n",
            dc4.sectsize, dc4.spt, dc4.cpd, dc4.heds, dc4.dsksiz);
        return 0;
    } else {                            /* read from DC-4 drive register */
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdrv: Drive read as %02X \n", dc4.intrq);
        return dc4.intrq;
    }
}

/* WD 1797 FDC command register routine */

int32 dc4_fdccmd(int32 io, int32 data)
{
    static int32 val = 0, val1 = NOTRDY;
    static long pos;
    static int32 err;
    int side = 0; 

    if ((dc4_dsk_unit[dc4.cur_dsk].flags & UNIT_ATT) == 0) { /* not attached */
        val = dc4_dsk_unit[dc4.cur_dsk].u3 |= NOTRDY; /* set not ready flag */
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Drive %d is not attached \n", dc4.cur_dsk);
        return 0x10; // return SEEK ERROR STATUS bit
    } else {
        dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(NOTRDY); /* clear not ready flag */
    }
    if (io) {                           /* write command to fdc */
        // on commands type I ...
        if ((data & 0x80)==0) {
            // ... set bits h V r1r0 to h=1 (home drive), V=0 (verify off), r1r0=11 (40msec track stepping)
            data = ((data & 0xF0) | 0x0B); 
            // and starts countdown for index status bit
            dc4.index_countdown=10; 
        } else {
            dc4.index_countdown=0; 
            if (((data & 0xC0) == 0x80) && (dc4.heds > 1)) {
                // type II command ... get side to work with 
                side = (data & 0x08) ? 0:1;
                data = data | 0x08; 
            }
        }
        // process command
        switch(data) {
            case 0x8C:                  //read sector command type II
            case 0x9C:                  //read multiple sectors command type II
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Read disk %d, track %d, sector %d, side %d \n", 
                    dc4.cur_dsk, TRK, SECT, side);
                if ((SECT - dc4.sector_base >= dc4.spt) || (SECT < dc4.sector_base)) {
                    dc4_dsk_unit[dc4.cur_dsk].u3 |= RECNF; /* set RECORD NOT FOUND */
                    break; 
                }
                dc4_dsk_unit[dc4.cur_dsk].u3 |= BUSY; /* set BUSY */
                dc4.busy_countdown=5; // start busy countdown
                pos = dc4.trksiz * TRK * dc4.heds; /* calculate file offset */
                if ((dc4.heds > 1) && (side > 0)) pos += dc4.trksiz; 
                pos += dc4.sectsize * (SECT - dc4.sector_base);
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Read pos = %ld ($%08X) \n",
                    pos, (unsigned int) pos);
                err = sim_fseek(dc4_dsk_unit[dc4.cur_dsk].fileref, pos, SEEK_SET); /* seek to offset */
                if (err) {
                    sim_printf("fdccmd: Seek error in read command\n");
                    return SCPE_IOERR;
                } 
                err = sim_fread(dc4_dsk_unit[dc4.cur_dsk].filebuf, dc4.sectsize, 1, dc4_dsk_unit[dc4.cur_dsk].fileref); /* read in buffer */
                if (err != 1) {
                    sim_printf("fdccmd: File error in read command\n");
                    return SCPE_IOERR;
                }
                dc4_dsk_unit[dc4.cur_dsk].u3 |= DRQ; /* set DRQ */
                dc4_dsk_unit[dc4.cur_dsk].pos = 0; /* clear counter */
                dc4.multiple_sector= (data == 0x9C) ? 1:0;
                break;
            case 0xAC:                  //write command type II
            case 0xBC:                  //write multiple sectors command type II
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Write disk %d, track %d, sector %d, side %d \n",
                    dc4.cur_dsk, TRK, SECT, side);
                if (dc4_dsk_unit[dc4.cur_dsk].u3 & WRPROT) {
                    sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Cannot write to write-protected disc %d \n",
                       dc4.cur_dsk);
                } else {
                    dc4_dsk_unit[dc4.cur_dsk].u3 |= BUSY;/* set BUSY */
                    dc4.busy_countdown=5; // start busy countdown
                    pos = dc4.trksiz * TRK * dc4.heds; /* calculate file offset */
                    if ((dc4.heds > 1) && (side > 0)) pos += dc4.trksiz; 
                    pos += dc4.sectsize * (SECT - dc4.sector_base);
                    sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Write pos = %ld ($%06X) \n",
                        pos, (unsigned int) pos);
                    err = sim_fseek(dc4_dsk_unit[dc4.cur_dsk].fileref, pos, SEEK_SET); /* seek to offset */
                    if (err) {
                        sim_printf("fdccmd: Seek error in write command\n");
                        return SCPE_IOERR;
                    } 
                    dc4_dsk_unit[dc4.cur_dsk].u3 |= DRQ;/* set DRQ */
                    dc4.wrt_flag = 1;           /* set write flag */
                    dc4_dsk_unit[dc4.cur_dsk].pos = 0; /* clear counter */
                }
                break;
            case 0x1B:                  //seek command type I
                TRK = dc4.fdcbyte; /* set track */
                dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY | DRQ); /* clear flags */
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Seek disk %d, track %d \n",
                    dc4.cur_dsk, TRK);
                break;
            case 0x0B:                  //restore command type I  
                TRK = 0;   /* home the drive */
                dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY | DRQ | RECNF); /* clear flags */
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Drive %d homed \n", dc4.cur_dsk);
                break;
            case 0xF0:                  //write track command type III
            case 0xF4:                  //write track command type III
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Write track command for drive %d \n",
                    dc4.cur_dsk);
                break;
            case 0xD0:                  //Force Interrupt - terminate current command
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Force Interrupt - terminate current command \n");
                dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY | DRQ); /* clear flags */
                break;
            default:
                sim_printf("Unknown FDC command %02X\n\r", data);
        }
    } else {                            /* read status from fdc */
        if (dc4.busy_countdown) {
            dc4.busy_countdown--; 
            // if busy countdown expires, set remove busy in status returned to cpu
            if (dc4.busy_countdown==0) {
                dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY); /* clear flags */
            }
        } 
        val = dc4_dsk_unit[dc4.cur_dsk].u3;     /* set return value */
        if (dc4.index_countdown) {
            dc4.index_countdown--;
            // if index countdown expires, set index flag in status returned to cpu
            if (dc4.index_countdown==0) val |= INDEX; 
        }
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdccmd: Drive %d status=%02X \n",
            dc4.cur_dsk, val);        
    }
    return val;
}

/* WD 1797 FDC track register routine */

int32 dc4_fdctrk(int32 io, int32 data)
{
    if (io) {
        dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(RECNF); /* clear flags */
        TRK = data & 0xFF;
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdctrk: Drive %d track set to %d \n",
            dc4.cur_dsk, TRK);
    }
    if (dc4.cur_dsk > 3) {
       sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdctrk: No drive selected yet \n");
       return 0; 
    }
    sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdctrk: Drive %d track reg read: current track %d \n",
        dc4.cur_dsk, TRK);
    return TRK;
}

/* WD 1797 FDC sector register routine */

int32 dc4_fdcsec(int32 io, int32 data)
{
    if (io) {
        dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(RECNF); /* clear flags */
        SECT = data & 0xFF;
        if (SECT < dc4.sector_base) SECT=dc4.sector_base; 
        // if (SECT == 0)  /* fix for swtp boot! */
        //    SECT = 1;
        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcsec: Drive %d sector set to %d \n",
            dc4.cur_dsk, SECT);
        return 0; 
    }
    sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcsec: Drive %d sector read as %d \n",
        dc4.cur_dsk, SECT);
    return SECT;
}

/* WD 1797 FDC data register routine */

int32 dc4_fdcdata(int32 io, int32 data)
{
    int32 val, err;

    if (dc4.cur_dsk >= NUM_DISK) return 0; 
    dc4.busy_countdown=5; // start busy countdown
    if (io) {                           /* write byte to fdc */
        dc4.fdcbyte = data;                 /* save for seek */
        if (dc4_dsk_unit[dc4.cur_dsk].pos < (t_addr) dc4.sectsize) { /* copy bytes to buffer */
            sim_debug (DEBUG_read, &dc4_dsk_dev, "fdcdata: Write byte %02X (dec=%d char='%c') to disk, Current Drive %d TRK %d SECT %d POS %d\n", 
                  data, data, (data < 32) ? '?':data, dc4.cur_dsk, TRK, SECT, dc4_dsk_unit[dc4.cur_dsk].pos);
            *((uint8 *)(dc4_dsk_unit[dc4.cur_dsk].filebuf) + dc4_dsk_unit[dc4.cur_dsk].pos) = data; /* byte into buffer */
            dc4_dsk_unit[dc4.cur_dsk].pos++;    /* step counter */
            if (dc4_dsk_unit[dc4.cur_dsk].pos == dc4.sectsize) {
                dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY | DRQ);
                if (dc4.wrt_flag) {         /* if initiated by FDC write command */
                    sim_fwrite(dc4_dsk_unit[dc4.cur_dsk].filebuf, dc4.sectsize, 1, dc4_dsk_unit[dc4.cur_dsk].fileref); /* write it */
                    dc4.wrt_flag = 0;       /* clear write flag */
                }
                sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdata: Sector write complete \n");
            }
        }
        return 0;
    } else {                            /* read byte from fdc */
        if (dc4_dsk_unit[dc4.cur_dsk].pos < (t_addr) dc4.sectsize) { /* copy bytes from buffer */
            val = *((uint8 *)(dc4_dsk_unit[dc4.cur_dsk].filebuf) + dc4_dsk_unit[dc4.cur_dsk].pos) & 0xFF;
            sim_debug (DEBUG_read, &dc4_dsk_dev, "fdcdata: Read byte %02X (dec=%d char='%c') from disk, Current Drive %d TRK %d SECT %d POS %d\n", 
                  val, val, (val < 32) ? '?':val, dc4.cur_dsk, TRK, SECT, dc4_dsk_unit[dc4.cur_dsk].pos);
            dc4_dsk_unit[dc4.cur_dsk].pos++;        /* step counter */
            if (dc4_dsk_unit[dc4.cur_dsk].pos == dc4.sectsize) { // sector finished
                if ((dc4.multiple_sector) && (SECT-dc4.sector_base < dc4.spt-1)) { // read multiple in progress
                    SECT++;
                    sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdata: Multiple read, reading next sector %d \n", 
                        SECT);
                    err = sim_fread(dc4_dsk_unit[dc4.cur_dsk].filebuf, dc4.sectsize, 1, dc4_dsk_unit[dc4.cur_dsk].fileref); /* read in buffer */
                    if (err != 1) {
                        sim_printf("fdccmd: File error in read command\n");
                        return SCPE_IOERR;
                    }
                    dc4_dsk_unit[dc4.cur_dsk].pos = 0; 
                } else {
                    SECT++;
                    dc4_dsk_unit[dc4.cur_dsk].u3 &= ~(BUSY | DRQ); /* clear flags */
                    if (dc4.multiple_sector) {
                        dc4.multiple_sector=0;
                        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdata: Multi sector read complete \n");
                    } else {
                        sim_debug (DEBUG_flow, &dc4_dsk_dev, "fdcdata: Sector read complete \n");
                    }
                }
            }
            return val;
        } else
        return 0;
    }
}

/* end of dc-4.c */
