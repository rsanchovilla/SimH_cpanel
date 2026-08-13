/*  mp-s-port.c: SWTPC MP-S serial I/O card simulator, general I/O port

    Copyright (c) 2005-2012, William Beech
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
        Willaim Beech BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
        IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
        CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

        Except as contained in this notice, the name of William A. Beech shall not
        be used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from William A. Beech.


    NOTES:

        These functions support a Optional port on SWTPC, as a general
        8 bits RS-232-C port, that can be interfaced to a H/F 8-bit Paper Tape reader or
        audio cassette as AC-30. 
        
        Works as a second MP-S card in system
*/

#include    <stdio.h>
#include    <ctype.h>
#include    "swtp_defs.h"

/* local global variables */

int32 odata;
int32 status;
int32 RTS;
int32 InstrCount0;                  // to regulate the rate rx chars are returned to prog
int32 sio_port_iobase = 0x8000;     // default addr for ACIAs, when in 64k mode (on SWTPC mode set at $8000)
int ac30_mode = 0;                  // 0=off, 1=play, 2=rec, -1=stopped, 
int sio_port_mode = 0;              // 0=off, 1=on

/* function prototypes */

t_stat sio_port_reset (DEVICE *dptr);
t_stat sio_port_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat sio_port_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat sio_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc); 
t_stat ac30_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat ac30_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc); 

int32 sio0s_port(int32 io, int32 data);
int32 sio0d_port(int32 io, int32 data);
int32 sio1s_port(int32 io, int32 data);
int32 sio1d_port(int32 io, int32 data);
extern int Mode64kRAM; 

/* sio port data structures

   sio_port_dev        SIO device descriptor
   sio_port_unit       SIO unit descriptor
   sio_port_reg        SIO register list
   sio_port_mod        SIO modifiers list */

// strings for debug output
extern char strCtrl[3]; 
extern char strAsc[4]; 

#define strData(data)   data==13 ? "<CR>":  data==10 ? "<LF>":      \
                        data==0  ? "<NUL>": data==26 ? "<^Z EOF>":  \
                        data==0x11 ? "<^Q PTR ON>":  data==0x12 ? "<^R PTP ON>":  \
                        data==0x13 ? "<^S PTR OFF>": data==0x14 ? "<^T PTP OFF>":  \
                        data<32  ? (strCtrl[1]=data+'A'-1,strCtrl) : (strAsc[1]=data, strAsc)  

REG sio_port_reg[] = {
        { HRDATA (IOBASE, sio_port_iobase, 16) }, // 16 bits width
        { NULL }
};

#define UNIT_V_PTR_USE_RTS     (UNIT_V_UF+2)   /* ACIA can use RTS to start/stop chars being read on data register */
#define UNIT_PTR_USE_RTS       (1 << UNIT_V_PTR_USE_RTS)   

// debug for character based i/o devices
DEBTAB port_io_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow" },
    { "READ", DEBUG_read, "Read Data" },
    { "WRITE", DEBUG_write, "Write Data"},
    { NULL }
};

DEVICE sio_port_dev; 

MTAB sio_port_mod[] = {
    { MTAB_XTD | MTAB_VDV, 0, "IOBASE", "IOBASE", &sio_port_set_iobase, &sio_show, NULL, NULL},
    { MTAB_XTD | MTAB_VDV, 1, NULL,  "ON",        &sio_port_set, NULL, &sio_port_dev, "Activates the sio port"},
    { MTAB_XTD | MTAB_VDV, 0, NULL,  "OFF",       &sio_port_set, NULL, &sio_port_dev, "Deactivates sio port"},
    { UNIT_PTR_USE_RTS, UNIT_PTR_USE_RTS, "USERTS",  "USERTS", NULL }, // set sio UseRTS -> ACIA can use RTS to start/stop chars being read on data register from PTR
    { UNIT_PTR_USE_RTS, 0,                "NORTS",   "NORTS",  NULL }, // set sio NoRTS  -> ignores RTS state. Allways reads any available char from PTR (default)
    { 0 }
};

UNIT sio_port_unit = { UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE, 0), 0};

DEVICE sio_port_dev = {
    "SIO-PORT", &sio_port_unit, sio_port_reg, sio_port_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &sio_port_reset,
    NULL, NULL, NULL,
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    port_io_debug,                      //debflags
    NULL
};

DEVICE ac30_dev; 

/* create an AC30 cassette tape device as an alisas to SIO-PORT device */
MTAB ac30_mod[] = {

    {MTAB_XTD | MTAB_VDV, 0,  "STOP","STOP",   &ac30_set, &ac30_show, &ac30_dev, "Press stop button on cassete"},
    {MTAB_XTD | MTAB_VDV, 1,  NULL,  "PLAY",   &ac30_set, NULL, &ac30_dev, "Press play button on cassete"},
    {MTAB_XTD | MTAB_VDV, 2,  NULL,  "REC",    &ac30_set, NULL, &ac30_dev, "Press record button on cassete"},
    {MTAB_XTD | MTAB_VDV, 2,  NULL,  "RECORD", &ac30_set, NULL, &ac30_dev, "Press record button on cassete"},
    {MTAB_XTD | MTAB_VDV, 3,  NULL,  "REW",    &ac30_set, NULL, &ac30_dev, "Press rewind button on cassete"},
    {MTAB_XTD | MTAB_VDV, 3,  NULL,  "REWIND", &ac30_set, NULL, &ac30_dev, "Press rewind button on cassete"},
    {MTAB_XTD | MTAB_VDV, 21, NULL,  "ON",     &ac30_set, NULL, &ac30_dev, "Powers on the cassete interface"},
    {MTAB_XTD | MTAB_VDV, 20, NULL,  "OFF",    &ac30_set, NULL, &ac30_dev, "Powers off the cassete interface"},
    { 0 }
};

UNIT ac30_unit = { UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE, 0), 0};

DEVICE ac30_dev = {
    "AC30", &ac30_unit, NULL, ac30_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &sio_port_reset,
    NULL, NULL, NULL,
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    port_io_debug,                      //debflags
    NULL
};


/* Reset console */

t_stat sio_port_reset (DEVICE *dptr)
{
    odata  = 0;                    // Data buffer
    status = 0x02;                 // Status buffer
    ac30_mode = 0;                  // 0=off, 1=play, 2=rec, -1=stopped, 
    sio_port_mode = 0;              // 0=off, 1=on
    return SCPE_OK;
}

/*  I/O instruction handlers, called from the MP-B2 module when a
    read or write occur to addresses 0x8000-0x801F. */

// return <0 if no char received (on ptr or on keyb polling)
//        0..255 char read
int GetPortChar(void)
{
    UNIT * uptr; 
    DEVICE * dptr;  

    extern int32 InstrCount;                   // intructions executed count 
    int32  m; 
    int byte; 

    if (InstrCount0 == 0) InstrCount0=InstrCount; 
    m = (InstrCount - InstrCount0) ; // number of instr executed elapsed from last time this routine was called
    if ((m>=0) && (m<10)) return -1; // too few instr exec -> no time to receive anything new -> return no data received
    InstrCount0=InstrCount;

    uptr = (sio_port_mode ? &sio_port_unit : &ac30_unit); 
    dptr = (sio_port_mode ? &sio_port_dev : &ac30_dev); 

    if ((uptr->flags & UNIT_ATT) == 0) { // attached?
        return -1;               // no, no data
    }    
    if ((sio_port_mode) && (uptr->flags & UNIT_PTR_USE_RTS)) {
        // check RTS state
        if (RTS==0) {
           return -1;               // sender is asked to not send data to ACIA -> return no data
        }
    }
    if (feof(uptr->fileref)) {
        byte = EOF; 
    } else {
        byte = getc(uptr->fileref);
    }
    if (byte == EOF) { // end of file?
        byte = 0;               // and return byte zero
        sim_debug (DEBUG_read, dptr, "PORT In Char: EOF: End of input file (read 0 <NUL>)\n"); 
    } else {
        sim_debug (DEBUG_read, dptr, "PORT In Char: %d $%02X (%s) \n", 
            byte, byte, strData(byte)); 
    }
    return byte; 
}


// at 0x8000
int32 sio0s_port(int32 io, int32 data)
{
    int byte,tc; 
    DEVICE * dptr;  

    if ((sio_port_mode == 0) && (ac30_mode ==0 )) {
        // if not active, act as non connected mem
        if (io == 0)
            return (0xFF);
        return 0;
    }

    if (io == 0) {                      
        // status register read
        if (status & 0x01) {
            // RXF flag set, not yet cleared -> prev byte has not yet read from data reg -> do not read a new one
            return status; 
        }
        // RXF flag cleared -> data reg can be overwritten
        byte = GetPortChar(); 
        if (byte < 0) {             
            status &= 0xFE;  // no data received
        } else {
            status |= 0x01;  // Set RXF flag
            odata=byte; 
        }
        return (status); // return acia status
    }                       
    // control register write
    dptr = (sio_port_mode ? &sio_port_dev : &ac30_dev); 
    if ((data & 0x03) == 3) {       // reset port!
        status = 0x02;              // transmit data reg empty, receive flag clear
        odata = 0;
        sim_debug (DEBUG_flow, dptr, "Reset port\n");
    }
    tc = (data >> 5) & 3;
    if (tc == 2) {       
        // RTS=0 (-> Active state -> external device can send bytes to be received by SIO data port)
        if (RTS==1) {
           sim_debug (DEBUG_flow, dptr, "RTS set to 0 \n");
           RTS=0; 
        }
    } else {
        // RTS=1 ( -> external devide instructed to stop sending chars to SIO data port) 
        if (RTS==0) {
           sim_debug (DEBUG_flow, dptr, "RTS set to 1 \n");
           RTS=1; 
        }
    }
    InstrCount0=0; // when control reg is written, reset count for delay on rx chars into GetPtrConsoleChar
    return 0; 
}

// at 0x8001
int32 sio0d_port(int32 io, int32 data)
{
    extern int32 InstrCount;                   // intructions executed count 
    UNIT * uptr; 
    DEVICE * dptr;  

    if ((sio_port_mode == 0) && (ac30_mode ==0 )) {
        // if not active, act as non connected mem
        if (io == 0)
            return (0xFF);
        return 0;
    }

    if (io == 0) {                      
        // data register read
        status &= 0xFE;  // clear RXF bit
        return odata; 
    } else {                            
        // data register write
        uptr = (sio_port_mode ? &sio_port_unit : &ac30_unit); 
        dptr = (sio_port_mode ? &sio_port_dev : &ac30_dev); 
        if (uptr->flags & UNIT_ATT) { // enabled & attached?
           putc(data, uptr->fileref);
           sim_debug (DEBUG_write, dptr, "Out Char: %d $%02X (%s) \n", 
                    data, data, strData(data)); 
        } else {
           sim_debug (DEBUG_write, dptr, "Out Char: %d $%02X (char lost, no file attached) \n", 
                    data, data); 
        }
    }
    return (odata = 0);
}

int32 sio1s_port(int32 io, int32 data)
{
    if ((sio_port_mode == 0) && (ac30_mode ==0 )) {
        // if not active, act as non connected mem
        if (io == 0)
            return (0xFF);
        return 0;
    }
    return status;
}

int32 sio1d_port(int32 io, int32 data)
{
    if ((sio_port_mode == 0) && (ac30_mode ==0 )) {
        // if not active, act as non connected mem
        if (io == 0)
            return (0xFF);
        return 0;
    }
   return odata;
}

t_stat sio_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    if (sio_port_mode == 0) { fprintf (st, "off"); }
    else if (Mode64kRAM == 0) { fprintf (st, "on"); }
    else fprintf (st, "iobase=%04x", sio_port_iobase);
    return SCPE_OK;
}

t_stat sio_port_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    if (Mode64kRAM==0) {
        sim_printf("IOBASE can be set only if 64K mode is active\n");
        return SCPE_ARG;
    }

    // set the IO address for sio port ACIA
    num = (int32) get_uint (cptr, 16, 65536, &r); // value is given as hex value
    if (r != SCPE_OK) return r;
    sio_port_iobase=num & 0xFFFE; 
    return SCPE_OK; 
}

t_stat sio_port_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    if (uptr == NULL) return SCPE_IERR;
    if (val == 1) {
        // set sio-port on
        sio_port_mode = 1; 
        sim_debug (DEBUG_flow, &sio_port_dev, "Set sio-port on \n"); 
    } else if (val == 0) {
        // set sio-port off
        sio_port_mode = 0; 
        sim_debug (DEBUG_flow, &sio_port_dev, "Set sio-port off \n"); 
    } else return SCPE_ARG;
    return SCPE_OK; 
}

t_stat ac30_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    if (ac30_mode == 0) { fprintf (st, "off"); }
    else if (Mode64kRAM == 0) { fprintf (st, "on"); }
    else fprintf (st, "iobase=%04x", sio_port_iobase);
    return SCPE_OK;
}

t_stat ac30_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    if (uptr == NULL) return SCPE_IERR;
    if (val < 20) {
        if (ac30_mode == 0) {
            // ac30 cassete interface is turned off. Must be turned on before using it
            return sim_messagef(SCPE_IOERR, "AC30 Cassete must be turned on before used \n");
        }
    }
    if (val == 0) {
        // set ac30 stop
        ac30_mode=-1; 
        sim_debug (DEBUG_flow, &ac30_dev, "ac30 STOP key pressed \n"); 
    } else if (val == 1) {
        // set ac30 play
        if ((ac30_unit.flags & UNIT_ATT) == 0) { // attached?
            return SCPE_UNATT; // error, not attached
        }
        ac30_mode=1; 
        sim_debug (DEBUG_flow, &ac30_dev, "ac30 PLAY key pressed \n"); 
    } else if (val == 2) {
        // set ac30 record
        if ((ac30_unit.flags & UNIT_ATT) == 0) { // attached?
            return SCPE_UNATT; // error, not attached
        }
        ac30_mode=2; 
        sim_debug (DEBUG_flow, &ac30_dev, "ac30 RECORD key pressed \n"); 
    } else if (val == 3) {
        // set ac30 rew
        if ((ac30_unit.flags & UNIT_ATT) == 0) { // attached?
            return SCPE_UNATT; // error, not attached
        }
        ac30_mode=-1; 
        sim_fseek(ac30_unit.fileref, 0, SEEK_SET); /* seek to offset=0 */
        sim_debug (DEBUG_flow, &ac30_dev, "ac30 REWIND key pressed \n"); 
    } else if (val == 21) {
        // set ac30 on
        ac30_mode=-1; // stop mode
        sim_debug (DEBUG_flow, &ac30_dev, "Set AC30 on \n"); 
    } else if (val == 20) {
        // set ac30 off
        ac30_mode = 0; 
        sim_debug (DEBUG_flow, &ac30_dev, "Set AC30 off \n"); 
        // ((sim_deb != NULL) && ((dptr) != NULL) && ((dptr)->dctrl & (dbits))) 
    } else return SCPE_ARG;
    return SCPE_OK; 
}

