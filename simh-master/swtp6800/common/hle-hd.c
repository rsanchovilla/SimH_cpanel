/*  hle-hd.c: SWTP High Level Emulator for Hard Disk 

    Copyright (c) 2022, Roberto Sancho

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

        Except as contained in this notice, the name of Roberto Sancho shall not
        be used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from Roberto Sancho .

    NOTES:

        High level EMULATOR (HLE) for swtpc 6800 harddisk with
        XIBEX S1410 Winchester Disk Controller and
        TANDOM TM-602S 5MB DRIVE

        Emulation is done thru fake-invented data&cmd registers.
        Much easier than deduct the register usage from https://deramp.com/swtpc.com/SASI/HARD-IO.TXT
        This is a quick and dirty emulator that matches hd driver made for SDOS, just to go on: life is hard
        Maybe in a future I can implement it the right way.

        The geometry IS emulated with accuracy

        4 surfaces
        153 cyclinders per surface
        64 sectors per clinder
        128 bytes per sector

        total capacity: 4 * 153 * 64 * 128 = 5.013.504 whooping bytes!

        Address     Mode    Function
        -------     ----    --------
        0x8010       HDCMD	PORTS FOR HARD DISK HIGH LEVEL EMULATOR INTERFACE: command reg
        0x8011       HDDATA	data reg


*/

#include <stdio.h>
#include "swtp_defs.h"

#define UNIT_V_ENABLE   (UNIT_V_UF + 0) /* Write Enable */
#define UNIT_ENABLE     (1 << UNIT_V_ENABLE)

#define NUM_DISK        1               
#define SECT_SIZE       128
#define NUM_SECT        64              /* sectors per track */
#define TRACK_SIZE      (SECT_SIZE * NUM_SECT) /* trk size (bytes) */
#define HEADS           4                /* number of surfaces */
#define NUM_CYL         153              /* maximum tracks */
#define DSK_SIZE        (NUM_SECT * HEADS * NUM_CYL * SECT_SIZE) /* dsk size (bytes) */


/* function prototypes */

t_stat              dsk_attach(UNIT *, CONST char *);

/* SS-50 I/O address space functions */
extern int32 nulldev(int32 io, int32 data);
extern void CPU_BD_put_mbyte(int32 addr, int32 val);
extern int32 CPU_BD_get_mbyte(int32 addr); 

int32 hd0cmd(int32 io, int32 data);
int32 hd1data(int32 io, int32 data);

/* Local Variables */

struct {
    int     cmd;    // cmd issued by cpu
    int     data;   // last data set by cpu/ready to be read by cpu
    int     surface, track, sector, addr; 
} hd = {0};

UNIT hd_unit = { 
        UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE+UNIT_ROABLE+UNIT_DIS, 0)  
    };

DEBTAB hd_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow control" },
    { "READ", DEBUG_read, "Read Command" },
    { "WRITE", DEBUG_write, "Write Command"},
    { NULL }
};

DEVICE hd_dev = {
    "HD",                               //name
    &hd_unit,                           //units
    NULL,                               //registers
    NULL,                               //modifiers
    1,                                  //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    NULL,                               //examine
    NULL,                               //deposit
    NULL,                               //reset
    NULL,                               //boot
    &dsk_attach,                        //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    hd_debug,                           //debflags
    NULL,                               //msize
    NULL                                //lname
};

//  I/O instruction handlers, called from the MP-B2 module 

void CheckSeek(void) 
{
    int loc, n; 

    hd.data=0; // status of operation
    if (hd.sector >= NUM_SECT) {
        hd.data=1; // error: sector invalid
        sim_debug (DEBUG_flow, &hd_dev, "ERROR: Sector invalid. Set to %d, should be less than %d \n", 
            hd.sector, NUM_SECT);
        return;
    }
    if (hd.track >= NUM_CYL) {
        hd.data=2; // error: track invalid
        sim_debug (DEBUG_flow, &hd_dev, "ERROR: Track invalid. Set to %d, should be less than %d \n", 
            hd.track, NUM_CYL);
        return;
    }
    if (hd.surface >= HEADS) {
        hd.data=3; // error: track invalid
        sim_debug (DEBUG_flow, &hd_dev, "ERROR: Surface number invalid. Set to %d, should be less than %d \n", 
            hd.surface, HEADS);
        return;
    }
    if ((hd.addr < 0) || (hd.addr > 0xFFFF-SECT_SIZE) || ((hd.addr > 0x8000-SECT_SIZE) && (hd.addr < 0x9000))) {
        hd.data=4; // error: addr invalid
        sim_debug (DEBUG_flow, &hd_dev, "ERROR: addr invalid (Set to %d) \n", hd.addr);
        return;
    }
    loc=(hd.sector + 
         NUM_SECT * (hd.track * 4 + hd.surface)) * SECT_SIZE; 
    n=sim_fseek(hd_unit.fileref, loc, SEEK_SET);
    if (n != 0) {
        hd.data=5; // error: seek error
        sim_debug (DEBUG_flow, &hd_dev, "ERROR: seek error %d \n", n);
        return;
    }
}

void sim_debug_dump_buf(char * buf, int buf_mem_addr, int len, int debug_flag)
{
    int cu, addr, c, d; 
    char sbuf[17];

    addr=0; cu=0;
    while(len>0) {
        if (cu==0) sim_debug (debug_flag, &hd_dev, "     %04X: ", buf_mem_addr+addr);
        d=(buf[addr++] & 0xFF);
        c=(d < 32) ? '?':d;
        sbuf[cu++]=c; 
        sim_debug (debug_flag, &hd_dev, " %02X", d);
        if (cu >= 16) {
            sbuf[cu]=0; 
            sim_debug (debug_flag, &hd_dev, "   %s \n", sbuf);
            cu=0;
        }
        len--;       
    }
}
   

int32 hd0cmd(int32 io, int32 data)
{
    char buf[SECT_SIZE];
    int i, tr, n, loc; 

    if ((hd_unit.flags & UNIT_DIS) || ((hd_unit.flags & UNIT_ATT) == 0)) {  
        // HD disabled or hd image file not attached, Respend as not connected
        return nulldev(io, data);
    }
    if (io==0) { 
        // io=0 when reading from cmd register return last cmd executed
        return hd.cmd; 
    } 
    // when writing, perform hd cmd
    switch(hd.cmd=data) {
        case 1: // HDCMD.HEARTBEAT
            sim_debug (DEBUG_flow, &hd_dev, "cmd HEARTBEAT \n");
            if (hd.data==0x55) hd.data=0x67; // the heartbeat expected reply
            break; 
        case 2: // HDCMD.SELDRV
            sim_debug (DEBUG_flow, &hd_dev, "cmd SELDRV \n");
            // for now this command is ignored. Only one HD drive supported
            break; 
        case 3: // HDCMD.SETSECT
            sim_debug (DEBUG_flow, &hd_dev, "cmd SETSECT %d \n", data);
            hd.sector = hd.data; // set the sector to read/write
            break; 
        case 4: // HDCMD.SETSURF
            sim_debug (DEBUG_flow, &hd_dev, "cmd SETSURF %d \n", data);
            hd.surface = hd.data; // set the surface to read/write
            break; 
        case 5: // HDCMD.SETTRACK
            sim_debug (DEBUG_flow, &hd_dev, "cmd SETTRACK %d \n", data);
            hd.track = hd.data; // set the track to read/write
            break; 
        case 6: // HDCMD.SETADDRH
            sim_debug (DEBUG_flow, &hd_dev, "cmd SETADDRH %d \n", data);
            hd.addr = (hd.data << 8) + (hd.addr & 0xFF); // set the MSB of addr of buf for read/write
            break; 
        case 7: // HDCMD.SETADDRL
            sim_debug (DEBUG_flow, &hd_dev, "cmd SETADDRL %d \n", data);
            hd.addr = (hd.addr & 0xFF00) + (hd.data) ; // set the LSB of addr of buf for read/write
            break; 
        case 8: // HDCMD.READ
            sim_debug (DEBUG_flow, &hd_dev, "cmd READ \n");
            CheckSeek();
            if (hd.data) return 0; // set hd.data with status of operation. If not 0 there is an error 
            // read hard disk image into buf
            n = sim_fread(buf, 1, SECT_SIZE, hd_unit.fileref);
            if (SECT_SIZE != n) {
                hd.data=8; // error: read error
            }
            // copy read buffer to addr in m6800 memory
            sim_debug (DEBUG_flow, &hd_dev, "Read surface %d, track %d, sector %d, addr %04x \n", 
                hd.surface, hd.track, hd.sector, hd.addr);
            for (i=0;i<SECT_SIZE;i++) CPU_BD_put_mbyte(hd.addr+i, buf[i]);
            tr=0; 
            sim_debug (DEBUG_read, &hd_dev, "Data read from surface %d, track %d, sector %d to addr %04x \n", 
                hd.surface, hd.track, hd.sector, tr=hd.addr); // little dirty trick to detect at near no cost the debug is beging used (considering <neurona usage during programming> metric)
            if (tr) sim_debug_dump_buf(buf, hd.addr, SECT_SIZE, DEBUG_read);
            break; 
        case 9: // HDCMD.WRITE
            sim_debug (DEBUG_flow, &hd_dev, "cmd WRITE \n");
            CheckSeek();
            if (hd.data) return 0; // set hd.data with status of operation. If not 0 there is an error 
            // copy write buffer from addr in m6800 memory
            sim_debug (DEBUG_flow, &hd_dev, "Write surface %d, track %d, sector %d, addr %04x \n", 
                hd.surface, hd.track, hd.sector, hd.addr);
            for (i=0;i<SECT_SIZE;i++) buf[i]=CPU_BD_get_mbyte(hd.addr+i);
            // write buf into hard disk image
            if (SECT_SIZE != sim_fwrite(buf, 1, SECT_SIZE, hd_unit.fileref)) {
                hd.data=9; // error: write error
            }
            tr=0; 
            sim_debug (DEBUG_write, &hd_dev, "Data write to surface %d, track %d, sector %d from addr %04x \n", 
                hd.surface, hd.track, hd.sector, tr=hd.addr); 
            if (tr) sim_debug_dump_buf(buf, hd.addr, SECT_SIZE, DEBUG_write);
            break; 
        default: 
            sim_debug (DEBUG_flow, &hd_dev, "Ignored unknown %d cmd \n", data);
            break; 
    }
}

int32 hd1data(int32 io, int32 data)
{
    if ((hd_unit.flags & UNIT_DIS) || ((hd_unit.flags & UNIT_ATT) == 0)) {  
        // HD disabled or hd image file not attached, Respend as not connected
        return nulldev(io, data);
    }
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        return hd.data; 
    } 
    // when writing, update internal hd data reg
    hd.data = data; 
    return 0;
}

t_stat dsk_attach(UNIT * uptr, CONST char *file)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    t_stat              r;
    int                 flen;
    char                buf[32*1024] = {0};
    int i;

    if ((r = attach_unit(uptr, file)) != SCPE_OK) return r;
    flen=sim_fsize(uptr->fileref);
    if (flen==0) {
        // init hard disk image
        for(i=0;i< (DSK_SIZE / sizeof(buf));i++) sim_fwrite(buf, 1, sizeof(buf), uptr->fileref);
    } else if (flen != DSK_SIZE) {
        sim_messagef (SCPE_IERR, "Invalid HD file size\n");
        detach_unit (uptr); 
    }

    return SCPE_OK;
}

