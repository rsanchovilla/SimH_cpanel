/*  mp-s.c: SWTP MP-S serial I/O card simulator

    Copyright (c) 2005-2012, William Beech

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

    MODIFICATIONS:

        24 Apr 15 -- Modified to use simh_debug
        Roberto Sancho: Jun 2022  -- general refactoring & clean up. Added PTR bin mode for SDOS PORT: simulation

    NOTES:

        These functions support a simulated SWTP MP-S interface card.
        The card contains one M6850 ACIA.  The ACIA implements one complete
        serial port.  It provides 7 or 8-bit ASCII RS-232 interface to Terminals
        or 20 mA current loop interface to a model 33 or 37 Teletype.  It is not
        compatible with baudot Teletypes.  Baud rates from 110 to 1200 are
        switch selectable from S! on the MP-S. The ACIA ports appear at all 
        4 addresses.  This fact is used by SWTBUG to determine the presence of the 
        MP-S vice MP-C serial card.  The ACIA interrupt request line can be connected
        to the IRQ or NMI interrupt lines by a jumper on the MP-S.

        All I/O is via either programmed I/O or interrupt controlled I/O.
        It has a status port and a data port.  A write to the status port
        can select some options for the device (0x03 will reset the port).
        A read of the status port gets the port status:

        +---+---+---+---+---+---+---+---+
        | I | P | O | F |CTS|DCD|TXE|RXF|
        +---+---+---+---+---+---+---+---+

        RXF - A 1 in this bit position means a character has been received
              on the data port and is ready to be read.
        TXE - A 1 in this bit means the port is ready to receive a character
              on the data port and transmit it out over the serial line.
     
        A read to the data port gets the buffered character, a write
        to the data port writes the character to the device.
*/

#include    <stdio.h>
#include    <ctype.h>
#include    "swtp_defs.h"

/* local global variables */

int32 odata;
int32 status;

int32 ptp_flag = 0;
int32 ptr_flag = 0;
int32 ptr_send_bin = 0;
int32 ptr_send_bin_byte; 
int32 ptp_send_bin = 0;
int32 ptp_send_bin_byte; 

/* function prototypes */

t_stat sio_svc (UNIT *uptr);
t_stat sio_reset (DEVICE *dptr);
t_stat ptr_reset (DEVICE *dptr);
t_stat ptp_reset (DEVICE *dptr);
t_stat sio_attach(UNIT * uptr, CONST char *file);

int32 sio0s(int32 io, int32 data);
int32 sio0d(int32 io, int32 data);
int32 sio1s(int32 io, int32 data);
int32 sio1d(int32 io, int32 data);

/* sio data structures

   sio_dev        SIO device descriptor
   sio_unit       SIO unit descriptor
   sio_reg        SIO register list
   sio_mod        SIO modifiers list */

UNIT sio_unit = { UDATA (&sio_svc, 0, 0), KBD_POLL_WAIT
};

REG sio_reg[] = {
    { ORDATA (DATA, odata, 8) },
    { ORDATA (STAT, status, 8) },
    { NULL }
};

DEVICE sio_dev = {
    "MP-S", &sio_unit, sio_reg, NULL,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &sio_reset,
    NULL, &sio_attach, NULL
};

/* paper tape reader data structures

   ptr_dev        PTR device descriptor
   ptr_unit       PTR unit descriptor
   ptr_reg        PTR register list
   ptr_mod        PTR modifiers list */

UNIT ptr_unit = { UDATA (NULL, UNIT_SEQ + UNIT_ATTABLE, 0), KBD_POLL_WAIT
};

DEVICE ptr_dev = {
    "PTR", &ptr_unit, NULL, NULL, 
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &ptr_reset,
    NULL, NULL, NULL
};

/* paper tape punch data structures

   ptp_dev        PTP device descriptor
   ptp_unit       PTP unit descriptor
   ptp_reg        PTP register list
   ptp_mod        PTP modifiers list */


UNIT ptp_unit = { UDATA (NULL, UNIT_SEQ + UNIT_ATTABLE, 0), KBD_POLL_WAIT
};
DEVICE ptp_dev = {
    "PTP", &ptp_unit, NULL, NULL,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &ptp_reset,
    NULL, NULL, NULL
};

/* console input service routine */

t_stat sio_svc (UNIT *uptr)
{
    int32 temp;

    sim_activate (&sio_unit, sio_unit.wait); // continue poll
    if (sio_unit.buf) return SCPE_OK; // last polled char not yet processed, so do not poll a new one (previous would be lost)
    if ((temp = sim_poll_kbd ()) < SCPE_KFLAG)
        return temp;                    // no char or error?
    sio_unit.buf = temp & 0xFF;         // Save char
    if (sio_unit.buf==127) {
        // convert BackSpace (ascii 127) to del char (ascii 8) for swtbug
        // also backspace cursor on console
        sio_unit.buf=8; 
    }
    /* Do any special character handling here */
    sio_unit.pos++;                     // step character count
    return SCPE_OK;
}

/* Reset console */

t_stat sio_reset (DEVICE *dptr)
{
    sio_unit.buf = 0;
    odata  = 0;                    // Data buffer
    status = 0x02;                 // Status buffer
    sio_unit.wait = 10000;
    sim_activate (&sio_unit, sio_unit.wait); // activate unit
    return SCPE_OK;
}

/* Reset paper tape reader */

t_stat ptr_reset (DEVICE *dptr)
{
    ptr_flag = 0;
    return SCPE_OK;
}

/* Reset paper tape punch */

t_stat ptp_reset (DEVICE *dptr)
{
    ptp_flag = 0;
    return SCPE_OK;
}

/*  I/O instruction handlers, called from the MP-B2 module when a
   read or write occur to addresses 0x8004-0x8007. */

// return <0 if no char received (on ptr or on keyb polling)
//        0..255 char read
int GetPtrConsoleChar(void)
{
    extern int32 InstrCount;                   // intructions executed count 
    static int32 InstrCount0=0;
    int32  m; 
    int byte; 

    m = (InstrCount - InstrCount0) ; // number of instr exectued elapsed from last time this routine was called
    if ((m>0) && (m<150)) return -1; // too few instr exec -> no time to receive anything new -> return no data received
    InstrCount0=InstrCount;

    if (ptr_flag==0) {                 
        // PRT disabled, new reading from console
        byte=sio_unit.buf;
        sio_unit.buf=0; // mark polled char as processed, so next polled char can be read
        if (byte==0) return -1; // char zero is no char read
        return byte; // return next char
    }
    // RDR is enabled
    if ((ptr_unit.flags & UNIT_ATT) == 0) { // attached?
        ptr_flag = 0;           // clear reader flag
        return -1;               // no, no data
    }
    if (ptr_send_bin == 0) {
        // normal ascii PTR read
        if (feof(ptr_unit.fileref)) {
            byte = EOF; 
        } else {
            byte = getc(ptr_unit.fileref);
        }
        if (byte == EOF) { // end of file?
            ptr_flag = 0;           // clear reader flag
            byte = 0;               // and return byte zero
        }
        ptr_unit.pos++;             // step character count
        return byte; 
    }
    // binary mode used by SDOS PORT: driver
    // read attached file to ptr as two hexdigits per byte, until eof (signaed as ^Z)
    if (ptr_send_bin<10) { 
        // start sending 10 0x55 chars to sync
        byte = 0x55; ptr_send_bin++; 
        ptr_send_bin_byte=0; 
    } else if (ptr_send_bin_byte == EOF) {
        byte = 26; // send ^Z to signal eof
    } else if (ptr_send_bin==10) {         
        ptr_send_bin_byte = getc(ptr_unit.fileref); 
        if (ptr_send_bin_byte == EOF) {
            byte=26; 
        } else {
            byte = (ptr_send_bin_byte >> 4); // get high nybble
            ptr_send_bin++;
        }
    } else {
        byte = (ptr_send_bin_byte & 0x0F); // get low nybble
        ptr_send_bin=10;
    }
    if (byte < 16) { // convert to hex digit 0..F
        byte = byte + ((byte < 10) ? '0':'A'-10);
    }
    return byte; 
}


// at 0x8004
int32 sio0s(int32 io, int32 data)
{
    int byte; 

    if (io == 0) {                      
        // status register read
        if (status & 0x01) {
            // RXF flag set, not yet cleared -> prev byte has not yet read from data reg -> do not read a new one
            return status; 
        }
        // RXF flag cleared -> data reg can be overwritten
        byte = GetPtrConsoleChar(); 
        if (byte < 0) {             
            status &= 0xFE;  // no data received
        } else {
            status |= 0x01;  // Set RXF flag
            odata=byte; 
        }
        return (status); // return acia status
    }                       
    // control register write
    if (data == 0x03) {             // reset port!
        status = 0x02;              // transmite data reg empty
        sio_unit.buf = 0;
        sio_unit.pos = 0;
        odata = 0;
    }
    return 0; 
}

// at 0x8005
int32 sio0d(int32 io, int32 data)
{
    extern int32 InstrCount;                   // intructions executed count 
    extern int32 PC; 


    if (io == 0) {                      
        // data register read
        status &= 0xFE;  // clear RXF bit
        return odata; 
    } else {                            
        // data register write

        if ((ptr_flag) && (data == 0x81)) {
            // send char 129 dec with ptr active -> hack to receive bin file from ptr
            // this is non-realistic, but very handly to implement SDOS PORT: device over PTP and PTR
            ptr_send_bin = 1; 
        } else {
            ptr_send_bin = 0; 
        }
        if (ptp_flag) {
            if (data == 0x82) {
                // send char 130 dec with ptp active -> hack to send ascii file to ptp without echo on console
                // this is non-realistic, but very handly to implement SDOS PORT: device over PTP and PTR
                ptp_send_bin = -1; 
            } else if (data == 0x83) {
                // send char 131 dec with ptp active -> hack to send bin file to ptp
                // this is non-realistic, but very handly to implement SDOS PORT: device over PTP and PTR
                ptp_send_bin = 1; // clear for reception of binary byte
                ptp_send_bin_byte=0; 
            } else if (ptp_send_bin<0) {
                if ((isprint(data) || data == '\r' || data == '\n' || data == 9) &&
                    (ptp_unit.flags & UNIT_ATT)) { 
                    putc(data, ptp_unit.fileref); // add printable char 
                    data =0x80; // data should not be printed again
                }
                if (data >= 32) data =0x80; // data should not be printed
            } else if (ptp_send_bin) {
                int n; 
                n=data  - (data <= '9' ? '0':'A'-10); // data has a hex digit 0..F?
                if ((n<0) || (n>15)) {
                    ptp_send_bin_byte=0;  // no, init byte to send for ptp to attached file
                    ptp_send_bin = 1; 
                } else {
                    ptp_send_bin_byte= (ptp_send_bin_byte << 4) + n;  // yes, add hex digit 
                    ptp_send_bin++; 
                    if (ptp_send_bin > 2) {
                        data = ptp_send_bin_byte; // full byte composed
                        ptp_send_bin_byte=0;  // clear to receive next byte
                        ptp_send_bin = 1;  
                        if (ptp_unit.flags & UNIT_ATT) { // PTP enabled & attached?
                           putc(data, ptp_unit.fileref); // add byte
                        }
                        data = 0x80;  
                    }
                }
                if (data >= 32) data =0x80; // data should not be printed
            }
        }
        if (isprint(data) || data == '\r' || data == '\n' || data == 8) { // printable?
            sim_putchar(data);          // print character on console
            if (ptp_flag && ptp_unit.flags & UNIT_ATT) { // PTP enabled & attached?
                putc(data, ptp_unit.fileref);
                ptp_unit.pos++;         // step character counter
            }
        } else {                        // control Reader/Punch
            switch (data) {
                case 0x11:              // PTR on (^Q)
                    ptr_flag = 1;
                    ptr_send_bin=0; 
//                    printf("Reader on\n");
                    break;
                case 0x12:              // PTP on (^R)
                    ptp_flag = 1;
                    ptp_send_bin=0; 
//                    printf("Punch on\n");
                    break;
                case 0x13:              // PTR off (^S)
                    ptr_flag = 0;
//                    printf("Reader off-%d bytes read\n", ptr_unit.pos);
                    break;
                case 0x14:              // PTP off (^T)
                    ptp_flag = 0;
//                    printf("Punch off-%d bytes written\n", ptp_unit.pos);
                    if (ptp_send_bin != 0) {
                        detach_unit(&ptp_unit); 
                    }
                    break;
                default:                // ignore all other characters
                    break;
            }
        }
    }

    return (odata = 0);
}

/*  because each port appears at 2 addresses and this fact is used
    to determine if it is a MP-C or MP-S repeatedly in the SWTBUG
    monitor, this code assures that reads of the high ports return
    the same data as was read the last time on the low ports.
*/

int32 sio1s(int32 io, int32 data)
{
    return status;
}

int32 sio1d(int32 io, int32 data)
{
    return odata;
}

t_stat sio_attach(UNIT * uptr, CONST char *file)
{
    t_stat              r;

    if ((r = attach_unit(uptr, file)) != SCPE_OK) return r;
    return r;
}

/* end of mp-s.c */

