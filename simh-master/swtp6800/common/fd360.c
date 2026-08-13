/*  fd360.c: iCOM FD360 8inch Floppy Simulator

    Copyright (c) 2026, Roberto Sancho

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
        ROBERTO SANCHO BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
        IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
        CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

        Except as contained in this notice, the name of Roberto Sancho shall not
        be used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from Roberto Sancho .

    MODIFICATIONS:

    NOTES:

        The FD300 is a floppy controller which can control up
        to four 8-inch floppy drives.
        Floppy disk is single side, soft-sectored, IBM 3740 format (256K)

        The floppy controller is interfaced to the CPU by use of 6 memory 
        mapped addresses. Controller card is tailored to memory map of computer 
        where it is used:
           $F80X for ASTRAL 2000
           $F08X for Sphere Computer
           $EC0X for Motorola Exorciser

        Address   Name    Function
        -------   ----    --------

        XX00      DKDID   INPUT DATA FROM DISK
        XX01      DKDIC   INPUT DATA CONTROL
        XX02      DKCOD   OUTPUT COMMANDS TO DISK
        XX03      DKCOC   OUTPUT COMMAND CONTROL

        XX06      DKDOD   OUTPUT DATA TO DISK
        XX07      DKDOC   OUTPUT DATA CONTROL


        http://www.bitsavers.org/components/motorola/6800/exorciser/M68FD3601-3604_EXORdisk_Floppy_Disk_System_Users_Guide_1975.pdf
        INPUT DATA AND STATUS:

        +------+------+------+------+------+------+------+------+
        | Read |      |Drive |Drive |      | Unit | Unit |      |
        |  DD  |  -   | Fail |Write | CRC  |  #   |  #   |  -   |
        | mark |      |Error |prot'd|Error | MSB  | LSB  |      |
        +------+------+------+------+------+------+------+------+
            7     6      5      4      3      2      1      0      Bit

        OUTPUT DATA:

        If data is track address
        +------+------+------+------+------+------+------+------+
        |  -   |                     Track                      |
        +------+------+------+------+------+------+------+------+
            7     6      5      4      3      2      1      0      Bit

        If data is unit and sector address
        +------+------+------+------+------+------+------+------+
        |    Unit     |  -   |              Sector              |
        +------+------+------+------+------+------+------+------+
            7     6      5      4      3      2      1      0      Bit

        COMMANDS:

        +------+------+------+------+------+------+------+------+
        | CLR  | Read |  Data Line  |   Drive control    |      |
        |Drive | Data | Definition  |     Definition     |  -   |
        |Electr| Byte |    Bits     |        Bits        |      |
        +------+------+------+------+------+------+------+------+
            7     6      5      4      3      2      1      0      Bit

         CLR Drive Electr: Clear drive electronics: is the general controller and drive reset
                           clears data buffer and data buffer counters

         Read Data Byte:   0 -> output controler lines = drive status
                           1 -> output controler lines = data byte from buffer

         Data Line:        01 -> data is track address
                           10 -> data is unit and sector address
                           11 -> data is byte to be added to controller buffer

         Drive control:    001 -> read a 128 byte disk sector into controller buffer
                           010 -> write a 128 byte disk sector from controller buffer
                           011 -> verify 128 byte disk sector CRC against controller buffer CRC
                           100 -> seek given unit/track
                           101 -> clear error flag/abort current operation
                           110 -> return the selected unit to track 0
                           111 -> write Deleted data mark on next write operation


        FD360 Disk supports these operating systems

        - iCOM FDOS-I/6800 OEM  (1975): rebranded as
             Motorola EDOS-I    (1975)
             Sphere DOS         (1976)

        - iCOM FDOS-II/6800 OEM (1976): rebranded as
             Motorola EDOS-II   (1976)
             Astral DOS         (1976 ?)
             Sphere OS1         (1977)


        At start, units discs from controller are dissabled
        To use it:
            set icom enabled
            set icom iobase=f800        <-- set the IO address for controller
            att icom0 diskimage.dsk     <-- attach = insert disk image in unit 0
            att icom1 diskimageB.dsk    <-- attach in unit 1, and so on up to unit 3

*/

#include <stdio.h>
#include "swtp_defs.h"

#define UNIT_V_ENABLE   (UNIT_V_UF + 0) /* Write Enable */
#define UNIT_ENABLE     (1 << UNIT_V_ENABLE)

/* emulate a single-sided disk disk with 26 sectors and 77 tracks */

#define NUM_DISKS       4               
#define SECT_SIZE       128             /* sector data size */
#define NUM_SECTS       26              /* sectors/track */
#define NUM_TRACKS      77              /* maximum tracks */
#define DSK_SIZE        (NUM_SECTS * NUM_TRACKS * SECT_SIZE) /* dsk size (bytes) */

/* function prototypes */

t_stat fd360_dsk_reset (DEVICE *dptr);
t_stat fd360_attach (UNIT *, CONST char *);
t_stat fd360_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat fd360_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc); 

/* SS-50 I/O address space functions */

int32 fd360_dkdid(int32 io, int32 data);
int32 fd360_dkdic(int32 io, int32 data);
int32 fd360_dkcod(int32 io, int32 data);
int32 fd360_dkcoc(int32 io, int32 data);
int32 fd360_dkdod(int32 io, int32 data);
int32 fd360_dkdoc(int32 io, int32 data);
extern int Mode64kRAM; 

/* Local Variables */

int32 fd360_iobase = 0xF800;               // default addr for disk controller PIAs

struct {
    int32   unit,track,sector;              // Currently selected drive unit, track, sector
    int32   wBufPtr;                        // controller write buffer pointer
    uint8   wBuf[128];                      // controller write buffer 
    int32   rBufPtr;                        // controller read buffer pointer
    uint8   rBuf[128];                      // controller read buffer 
    uint8   Mode;                           // 0=read status from controller, 1=read sector data buffer from controller
    int32   DataIn;                         // data sent from CPU to disk controller
    int32   DataOut;                        // data sent from disk controller to CPU
    int32   Error;                          // =1 -> read/write error, =2 -> write protected
    uint8   Busy;                           // busy flag
} fd360 = {0};

/* Floppy Disk Controller data structures

       fd360_dsk_dev        Disk Controller device descriptor
       fd360_dsk_unit       Disk Controller unit descriptor
       fd360_dsk_reg        Disk Controller register list
       fd360_dsk_mod        Disk Controller modifiers list
*/

MTAB fd360_mod[] = {
    { MTAB_XTD | MTAB_VDV, 0, "IOBASE", "IOBASE", &fd360_set_iobase, &fd360_show, NULL, NULL},
    { UNIT_RO, UNIT_RO, "RO", "RO", NULL },
    { UNIT_RO,       0, "RW", "RW", NULL },
    { 0 }
};

UNIT fd360_dsk_unit[] = {
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE+UNIT_ROABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE+UNIT_ROABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE+UNIT_ROABLE, 0)  },
        { UDATA (NULL, UNIT_FIX+UNIT_ATTABLE+UNIT_DISABLE+UNIT_ROABLE, 0)  }
};

REG fd360_dsk_reg[] = {
        { HRDATA (IOBASE, fd360_iobase, 16) }, // 16 bits width
        { HRDATA (UNIT,   fd360.unit, 2) },  // 2 bits width
        { HRDATA (TRACK,  fd360.track,  7) },
        { HRDATA (SEC   TOR, fd360.sector, 5) },
        { NULL }
};

DEBTAB fd360_dsk_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow control" },
    { "READ", DEBUG_read, "Read Command" },
    { "WRITE", DEBUG_write, "Write Command"},
    { NULL }
};

DEVICE fd360_dsk_dev = {
    "ICOM",                             //name
    fd360_dsk_unit,                     //units
    fd360_dsk_reg,                      //registers
    fd360_mod,                          //modifiers
    NUM_DISKS,                          //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    NULL,                               //examine
    NULL,                               //deposit
    &fd360_dsk_reset,                   //reset
    NULL,                               //boot
    &fd360_attach,                      //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG | DEV_DIS | DEV_DISABLE,  //flags
    0,                                  //dctrl
    fd360_dsk_debug,                    //debflags
    NULL,                               //msize
    NULL                                //lname
};

/* Reset routine */

t_stat fd360_dsk_reset (DEVICE *dptr)
{
    sim_debug (DEBUG_flow, &fd360_dsk_dev, "Controller RESET \n");
    memset(&fd360, 0, sizeof(fd360));
    return SCPE_OK;
}

/*  I/O instruction handlers, called from the MP-B2 module when a
   read or write occur to addresses 0xXXX0-0xXXX7. */

extern char strCtrl[3]; 
extern char strAsc[4]; 

int32 fd360_dkdid(int32 io, int32 data) 
{  
    // INPUT DATA FROM DISK - Data register for PIA at address 0xXXX0
    // read disk status

    if (io==1) { 
        // io=1 -> writing data to i/o register
        if (data == 0) { /* clear, no effect */ }
        else sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Write $%02X to 0xXXX0 \n", data);
    } else {   
        // io=0 -> reading data from controller
        if (fd360.Mode==0) { 
            // Mode=0=read status from controller
            // +------+------+------+------+------+------+------+------+
            // | Read |      |Drive |Drive |      | Unit | Unit |      |
            // |  DD  |  -   | Fail |Write | CRC  |  #   |  #   |  -   |
            // | mark |      |Error |prot'd|Error | MSB  | LSB  |      |
            // +------+------+------+------+------+------+------+------+
            //     7     6      5      4      3      2      1      0      Bit
            data = 0;
            if (fd360.Error & 1) data |= 32; // notify drive fail error to 6800 program
            if (fd360.Error & 2) data |= (32+16+8); // notify write protected error to 6800 program also as CRC error
            data |= (fd360.unit << 1); // return current selected unit
            // DD mark and CRC not implemented 
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "Read Status $%02X from Controller: %s \n", 
               data, fd360.Error ? "Error":"No Error");
            return data;
        } else {
            // Mode=1=read sector data buffer from controller
            data=fd360.DataOut;
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "Read Data $%02X (%s) from Controller Buffer\n", 
               data, data==13 ? "<CR>": 
                     data==10 ? "<LF>": 
                     data==0  ? "<NUL>": 
                     data==26 ? "<^Z EOF>": 
                     data<32  ? (strCtrl[1]=data+'A'-1,strCtrl) :
                                (strAsc[1]=data, strAsc)  );
            return data; 
        }
    }
    return 0;

}

int32 fd360_dkdic(int32 io, int32 data) 
{ 
    // INPUT DATA FROM DISK - Control register for PIA at address 0xXXX1
    // Set to $00 then $04 in RESET PROM routine 
    // On read: bit 7=1 when busy goes from 1 to zero (controller has terminated the requested command)

    if (io==1) { 
        // io=1 -> writing data to i/o register,
        if (data == 0) { /* clear, no effect */ }
        else if (data == 4) { /* configure controller, no effect */ } 
        else sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Write $%02X to 0xXXX1 \n", data);
    } else {
        // io=0 -> reading from i/o register,
        if (fd360.Busy) { 
            // busy is set -> reset it to zero and return Bit7=1
            fd360.Busy=0; 
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "Command done (Busy goes to 0) \n", data);
            return 128; 
        }
    }
    return 0; 
}

int32 fd360_dkcod(int32 io, int32 data) 
{ 
    // OUTPUT COMMAND - Data register for PIA at address 0xXXX2

    int cmd, dat, loc; 
    int r,i; 
    UNIT * uptr = &fd360_dsk_unit[fd360.unit & 3]; 
    char strbuf[129];
    char c; 

    if (io==1) { // io=1 -> writing data 
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "Write Command $%02X to Controller, Current unit %d, track %d, sector %d \n", 
            data, fd360.unit, fd360.track, fd360.sector);

         //        +------+------+------+------+------+------+------+------+
         //        | CLR  | Read |  Data Line  |   Drive control    |      |
         //        |Drive | Data | Definition  |     Definition     |  -   |
         //        |Electr| Byte |    Bits     |        Bits        |      |
         //        +------+------+------+------+------+------+------+------+
         //            7     6      5      4      3      2      1      0      Bit
         if (data & 128) {
             // clear drive electronics
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "CLR Drive Electonics \n");
             fd360.Busy = 0; 
             fd360.Error = 0; 
             return 0; 
         } 
         if (data & 64) {
            // read data byte
            if (fd360.Mode==1) {  
                // 1=read sector data buffer from controller -> strobe buffer
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "Read data from controller buffer[%d], Shift buffer \n", 
                    fd360.rBufPtr);
                fd360.DataOut = fd360.rBuf[fd360.rBufPtr]; 
                fd360.rBufPtr = (fd360.rBufPtr + 1) & 127; // strobe buffer
            } else {
                // 0=read status from controller, 
                // no effect
            }
            return 0;
         }
         cmd = (data >> 1) & 7; // isolate Drive control bits
             // 001 -> read a 128 byte disk sector into controller buffer
             // 010 -> write a 128 byte disk sector from controller buffer
             // 011 -> verify 128 byte disk sector CRC against controller buffer CRC
             // 100 -> seek given unit/track
             // 101 -> clear error flag/abort current operation
             // 110 -> return the selected unit to track 0
             // 111 -> write Deleted data mark on next write operation
         dat = (data >> 4) & 3; // isolate Data Line bits
             // 01 -> data is track address
             // 10 -> data is unit and sector address
             // 11 -> data is byte to be added to controller buffer
         if ((cmd >= 1) && (cmd <= 3)) {
             // read/write/verify a 128 byte disk sector into/from controller buffer
             sim_debug (DEBUG_read, &fd360_dsk_dev, "%s disk sector at unit %d, track %d, sector %d \n", 
                 (cmd == 1) ? "Read": (cmd == 2) ? "Write" : "Verify", 
                 fd360.unit, fd360.track, fd360.sector);
             fd360.Busy=1; 
             if ((uptr->flags & UNIT_ATT) == 0) {  
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d has no file attached \n", fd360.unit);
                fd360.Error |=1;
                return 0; 
             } 
             // calculate location of current sector in disk image file
             loc=(fd360.track * NUM_SECTS + (fd360.sector-1) ) * SECT_SIZE; 
             if (loc >= uptr->u6) {
                // reading past disk image file current size -> error
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Sector outside disk image file \n");
                fd360.Error |=1;
                return 0; 
             }
             // seek sector in disk image
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Pos in disk image: %d ($%x) \n", loc, loc); 
             r=sim_fseek(uptr->fileref, loc, SEEK_SET);
             if (r) {
                 sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: in sim_fseek: r=%d \n", r);
                 fd360.Error |=1; // seek error
             }
             // perform the operation
             if (cmd == 1) { // read
                 r=sim_fread(&fd360.rBuf, 1, SECT_SIZE, uptr->fileref);
                 if (r!=128) {
                     sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: in sim_fread: r=%d \n", r);
                     fd360.Error |=1; // buffer not fully read
                 }
                 fd360.rBufPtr=0; 
                 for(i=0;i<128;i++) { c=fd360.rBuf[i]; strbuf[i]= (c<32) ? '.':c; } strbuf[128]=0; 
                 sim_debug (DEBUG_read, &fd360_dsk_dev, "Sector Read: {%s} \n", strbuf);
             } else if (cmd == 2) { // write
                 if (uptr->flags & UNIT_RO) {  
                    sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d is read-only \n", fd360.unit);
                    fd360.Error |=2;
                    return 0; 
                 } 
                 r=sim_fwrite(&fd360.wBuf, 1, SECT_SIZE, uptr->fileref);
                 if (r!=128) {
                     sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: in sim_fwrite: r=%d \n", r);
                     fd360.Error |=1; // buffer not fully written
                 }
                 fd360.wBufPtr=0; 
                 for(i=0;i<128;i++) { c=fd360.wBuf[i]; strbuf[i]= (c<32) ? '.':c; } strbuf[128]=0; 
                 sim_debug (DEBUG_write, &fd360_dsk_dev, "Sector Write: {%s} \n", strbuf);
             } else { // verify
                 // not implemented
             }
             return 0;
         } else if (cmd == 4) {
             // seek given unit/track/sector
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Seek unit %d, track %d, sector %d \n", 
                 fd360.unit, fd360.track, fd360.sector);
             fd360.Busy=1; 
             if ((uptr->flags & UNIT_ATT) == 0) {  
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d has no file attached \n", fd360.unit);
                fd360.Error |=1;
             } else if ((fd360.sector == 0) || (fd360.sector > NUM_SECTS) || (fd360.track >= NUM_TRACKS)) {
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Invalid track/sector \n", fd360.unit);
                fd360.Error |=1;
             }
             return 0;
         } else if (cmd == 5) {
             // clear error
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Clear Error \n");
             fd360.Error=0;
             fd360.Busy=0; 
             return 0;
         } else if (cmd == 6) {
             // Home
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Home (Return to track 0) \n");
             fd360.Busy=1; 
             if ((uptr->flags & UNIT_ATT) == 0) {  
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d has no file attached \n", fd360.unit);
                fd360.Error|=1;
                return 0; 
             } 
             fd360.track=0; 
             return 0;
         } else if (cmd == 7) {
             // Write deleted data mark (DDM) on next write sector command
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Write DDM (ignored) \n");
             fd360.Busy=1; 
             return 0;
         }  
         if (dat == 1) {
             // DataIn is <TRACK>
             fd360.track= (fd360.DataIn & 127); 
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Select track %d \n", 
                fd360.track);
             if ((uptr->flags & UNIT_ATT) == 0) {  
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d has no file attached \n", fd360.unit);
                fd360.Error|=1;
                return 0; 
             } 
             return 0;
         } else if (dat == 2) {
             // DataIn is <UNIT><SECTOR>
             fd360.unit= (fd360.DataIn >> 6) & 3; 
             fd360.sector= (fd360.DataIn & 31); 
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Select unit %d, sector %d \n", 
                fd360.unit, fd360.sector);
             uptr = &fd360_dsk_unit[fd360.unit & 3];
             if ((uptr->flags & UNIT_ATT) == 0) {  
                sim_debug (DEBUG_flow, &fd360_dsk_dev, "ERROR: Current unit %d has no file attached \n", fd360.unit);
                fd360.Error|=1;
                return 0; 
             } 
             return 0;
         } else if (dat == 3) {
             // add DataIn to write buffer
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "Add Data $%02X (%s) to Controller Write Buffer[%d], shift buffer\n", 
               fd360.DataIn, fd360.DataIn==13 ? "<CR>": 
                     fd360.DataIn==10 ? "<LF>": 
                     fd360.DataIn==0  ? "<NUL>": 
                     fd360.DataIn==26 ? "<^Z EOF>": 
                     fd360.DataIn<32  ? (strCtrl[1]=fd360.DataIn+'A'-1,strCtrl) :
                                        (strAsc[1]=fd360.DataIn, strAsc), 
               fd360.wBufPtr);
             fd360.wBuf[fd360.wBufPtr] = fd360.DataIn; 
             fd360.wBufPtr = (fd360.wBufPtr + 1) & 127; // incr buffer
             return 0;
         }
         if (cmd + dat == 0) {
             sim_debug (DEBUG_flow, &fd360_dsk_dev, "No Controller Command \n");
             return 0;
         }
         sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? unknown command Writen to 0xXXX2 \n", data);
    } else {
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Read Data from 0xXXX2 \n");
    }
    return 0; 
}

int32 fd360_dkcoc(int32 io, int32 data) 
{ 
    // OUTPUT COMMAND CONTROL - Control register for PIA at address 0xXXX3
    // determines what is returned when CPU reads PIA data at 0xXXX0
    // Set to $2C -> returns controller status
    // Set to $3C -> returns byte from sector buffer

    if (io==1) { 
        // io=1 -> writing data to i/o register,
        if (data == 0x2C) {
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "Set to read Status from Controller \n");
            fd360.Mode=0; // 0=read status from controller, 1=read sector data buffer from controller
        } else if (data == 0x3C) {
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "Set to read buffer from Controller \n");
            fd360.Mode=1; // 0=read status from controller, 1=read sector data buffer from controller
        } else if (data == 0) {
            /* clear, no effect */
        } else {
            sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Write $%02X to 0xXXX3 \n", data);
        }
    } else {
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Read Data from 0xXXX3 \n");
    }
    return 0; 
}

int32 fd360_dkdod(int32 io, int32 data) 
{ 
    // OUTPUT DATA TO DISK - Data register for PIA at address 0xXXX6

    if (io==1) { 
        // io=1 -> writing data to i/o register,
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "Write Data $%02X to Controller \n", data);
        fd360.DataIn=data; 
    } else {
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Read Data from 0xXXX6 \n");
    }
    return 0;
}

int32 fd360_dkdoc(int32 io, int32 data) 
{ 
    // OUTPUT DATA TO DISK - Control register for PIA at address 0xXXX7
    // Set to $00 then $04 RESET PROM routine    
    if (io==1) { // io=1 -> writing data to i/o register,
        if (data == 0) { /* clear, no effect */ }
        else if (data == 4) { /* configure controller, no effect */ } 
        else sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Write $%02X to 0xXXX7 \n", data);
    } else {
        sim_debug (DEBUG_flow, &fd360_dsk_dev, "??? Read Data from 0xXXX7 \n");
    }
    return 0; 
}

t_stat fd360_attach (UNIT * uptr, CONST char * file)
{
    t_stat r; 

    if ((r = attach_unit(uptr, file)) != SCPE_OK) return r;

    if (sim_switches & SWMASK ('N')) {            // new disk
        // create a 77*26*128=256256 bytes blank disk
        uint8 track[SECT_SIZE*NUM_SECTS];
        int i; 
        memset(track,0,sizeof(track));
        for (i=0; i<NUM_TRACKS; i++) sim_fwrite(track,sizeof(track),1,uptr->fileref);
    }

    uptr->u6 = sim_fsize(uptr->fileref);
    uptr->pos = 0; 
    return SCPE_OK;
}

t_stat fd360_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "iobase=%04x", fd360_iobase);
    return SCPE_OK;
}

t_stat fd360_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    if (Mode64kRAM==0) {
        sim_printf("IOBASE can be set only if 64K mode is active\n");
        return SCPE_ARG;
    }

    // set the IO address for disk controller PIAs
    num = (int32) get_uint (cptr, 16, 65536, &r); // value is given as hex value
    if (r != SCPE_OK) return r;
    fd360_iobase=num & 0xFFF8; 
    return SCPE_OK; 
}

