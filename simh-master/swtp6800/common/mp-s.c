/*  mp-s.c: SWTPC MP-S serial I/O card simulator

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

        RXF - Receive register Full: A 1 in this bit position means a 
              character has been received on the data port and is ready to be read.
        TXE - Transmit register Empty: A 1 in this bit means the port is ready to 
              receive a character on the data port and transmit it out over the serial line.
     
        A read to the data port gets the buffered character, a write
        to the data port writes the character to the device.

        DCD - Modem Carrier Detect line (not simulated in SimH)
        CTS - Modem Clear to Send line (not simulated in SimH)
        F - A 1 indicates Framming error (not simulated in SimH)
        O - A 1 indicates Receiver overrun error (not simulated in SimH)
        P - A 1 Parity overrun error (not simulated in SimH)
        I - A 1 indicates IRQ reques    ted by ACIA (not simulated in SimH)

        Control port:

        +---+---+---+---+---+---+---+---+
        | I |  TC   |  Word Sel |  CDS  |
        +---+---+---+---+---+---+---+---+

        CDS - Counter Divide Select. Set clock divider
              00 div by 1 
              01 div by 16
              10 div by 64
              11 resets the ACIA

        Word Select - Indicates 7/8 bits, Odd/even parity, And 1/2 stop bits. 
              (not simulated in SimH)
              000 7 bits, Even parity, 2 stop bits
              001 7 bits, Odd parity, 2 stop bits
              010 7 bits, Even parity, 1 stop bit
              011 7 bits, Odd parity, 1 stop bit
              100 8 bits, No parity, 2 stop bits
              101 8 bits, No parity, 1 stop bit
              110 8 bits, Even parity, 1 stop bit
              111 8 bits, Odd parity, 1 stop bit

        TC - Sets RTS line state, enable/disables ACIA
             generating IRQ on byte transmission 
             (not simulated in SimH)
             00 RTS=1, Tx interrupt Disabled
             01 RTS=1, Tx interrupt Enabled
             10 RTS=0, Tx interrupt Disabled
             11 RTS=1, (tx break level), Tx interrupt Enabled

        I -  enable/disable ACIA generating IRQ on byte receive
             (not simulated in SimH)


        Usage of SIO in different ROMS

        - SWTBUG:  Before any OUTCH, Control Port = $11 -> 8N1, RTS=1 
                   Before any INCH, Control Port = $15 -> 8N1, RTS=1 
        - EXBUG:   Init Control Port to $11 -> 8N1, RTS=1  (based on SBITC = $51)
                   During PTR read, Control Port set to $51 -> 8N1, RTS=0
        - MINIBUG: Init Control Port to $B1 -> 8N1, RTS=1  
                   During PTR read, Control Port set to $D1 -> 8N1, RTS=0
*/

#include    <stdio.h>
#include    <ctype.h>
#include    "swtp_defs.h"

/* local global variables */

int32 odata;
int32 status;
int32 RTS = 0;
int32 InstrCount0 = 0; // to regulate the rate rx chars are returned to prog

int32 ptp_flag = 0;        // 1=Paper Tape Punch is ON (PTP ON)
int32 ptr_flag = 0;        // 1=Paper Tape Reader is ON (PTR ON)
int32 ptr_send_bin = 0;
int32 ptr_send_bin_byte; 
int32 ptp_send_bin = 0;
int32 ptp_send_bin_byte; 

/* function prototypes */

t_stat sio_svc (UNIT *uptr);
t_stat sio_reset (DEVICE *dptr);
t_stat ptr_reset (DEVICE *dptr);
t_stat ptp_reset (DEVICE *dptr);

int32 sio0s(int32 io, int32 data);
int32 sio0d(int32 io, int32 data);
int32 sio1s(int32 io, int32 data);
int32 sio1d(int32 io, int32 data);

/* sio data structures

   sio_dev        SIO device descriptor
   sio_unit       SIO unit descriptor
   sio_reg        SIO register list
   sio_mod        SIO modifiers list */

#define UNIT_V_SIO7BIT         (UNIT_V_UF)     /* SIO outputs only 7bits? */
#define UNIT_SIO7BIT           (1 << UNIT_V_SIO7BIT)
#define UNIT_V_PTP_NOECHO      (UNIT_V_UF+1)   /* disable echoing Punched chars to SimH console? */
#define UNIT_PTP_NOECHO        (1 << UNIT_V_PTP_NOECHO)
#define UNIT_V_PTR_USE_RTS     (UNIT_V_UF+2)   /* ACIA can use RTS to start/stop chars being read on data register from PTR */
#define UNIT_PTR_USE_RTS       (1 << UNIT_V_PTR_USE_RTS)   
#define UNIT_V_BINMODE         (UNIT_V_UF+3)   /* Hack to allow sending binary files as as stream of hexdigits pairs */
#define UNIT_BINMODE           (1 << UNIT_V_PTR_USE_RTS)   

// debug for character based i/o devices
DEBTAB char_io_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow control" },
    { "READ", DEBUG_read, "Read Command" },
    { "WRITE", DEBUG_write, "Write Command"},
    { NULL }
};

// strings for debug output
char strCtrl[3] = "^X"; 
char strAsc[4] = "'X'"; 

#define strData(data)   data==13 ? "<CR>":  data==10 ? "<LF>":      \
                        data==0  ? "<NUL>": data==26 ? "<^Z EOF>":  \
                        data==0x11 ? "<^Q PTR ON>":  data==0x12 ? "<^R PTP ON>":  \
                        data==0x13 ? "<^S PTR OFF>": data==0x14 ? "<^T PTP OFF>":  \
                        data<32  ? (strCtrl[1]=data+'A'-1,strCtrl) : (strAsc[1]=data, strAsc)  

UNIT sio_unit = { UDATA (&sio_svc, 0, 0), KBD_POLL_WAIT
};

REG sio_reg[] = {
    { ORDATA (DATA, odata, 8) },
    { ORDATA (STAT, status, 8) },
    { NULL }
};

MTAB sio_mod[] = {
    { UNIT_SIO7BIT, UNIT_SIO7BIT,       "7BITS", "7BITS", NULL }, // set mp-s 7bits --> to out only 7bit ascii to console
    { UNIT_SIO7BIT, 0,                  "8BITS", "8BITS", NULL },
    { 0 }  };

DEVICE sio_dev = {
    "MP-S", &sio_unit, sio_reg, sio_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &sio_reset,
    NULL, NULL, NULL,
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    char_io_debug,                      //debflags
    NULL
};

/* paper tape reader data structures

   ptr_dev        PTR device descriptor
   ptr_unit       PTR unit descriptor
   ptr_reg        PTR register list
   ptr_mod        PTR modifiers list */

UNIT ptr_unit = { UDATA (NULL, UNIT_SEQ + UNIT_ATTABLE, 0), KBD_POLL_WAIT
};

MTAB ptr_mod[] = {
    { UNIT_PTR_USE_RTS, UNIT_PTR_USE_RTS, "USERTS",  "USERTS", NULL }, // set ptr UseRTS -> ACIA can use RTS to start/stop chars being read on data register from PTR
    { UNIT_PTR_USE_RTS, 0,                "NORTS",   "NORTS",  NULL }, // set ptr NoRTS  -> ignores RTS state. Allways reads any available char from PTR (default)
    { UNIT_BINMODE, UNIT_BINMODE,         "BINMODE", "BINMODE", NULL }, // set ptr binmode -> hack for SDOS: allows sending and receiving 8-bit binary files thru paper tape
    { UNIT_BINMODE, 0,                    "NORMAL",  "NORMAL", NULL },  // set ptr normal  -> hack disabled (default)
    { 0 }  };

DEVICE ptr_dev = {
    "PTR", &ptr_unit, NULL, ptr_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &ptr_reset,
    NULL, NULL, NULL,
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    char_io_debug,                      //debflags
    NULL
};

/* paper tape punch data structures

   ptp_dev        PTP device descriptor
   ptp_unit       PTP unit descriptor
   ptp_reg        PTP register list
   ptp_mod        PTP modifiers list */


UNIT ptp_unit = { UDATA (NULL, UNIT_SEQ + UNIT_ATTABLE, 0), KBD_POLL_WAIT
};

MTAB ptp_mod[] = {
    { UNIT_PTP_NOECHO, UNIT_PTP_NOECHO, "NOECHO",  "NOECHO", NULL },  // set ptp noecho -> disables echoing Punched chars to SimH console 
    { UNIT_PTP_NOECHO, 0,               "ECHO",    "ECHO", NULL },    // set ptp echo -> enable echoing Punched chars to SimH console (default)
    { 0 }  };

DEVICE ptp_dev = {
    "PTP", &ptp_unit, NULL, ptp_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, &ptp_reset,
    NULL, NULL, NULL,
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    char_io_debug,                      //debflags
    NULL
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
    sim_debug (DEBUG_flow, dptr, "SIO reset \n"); 
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
    sim_debug (DEBUG_flow, dptr, "PTR reset \n"); 
    ptr_flag = 0;
    return SCPE_OK;
}

/* Reset paper tape punch */

t_stat ptp_reset (DEVICE *dptr)
{
    sim_debug (DEBUG_flow, dptr, "PTP reset \n"); 
    ptp_flag = 0;
    return SCPE_OK;
}


// binary mode used by SDOS PORT: driver        
// read attached file to ptr as two hexdigits per byte, until eof (signaled as ^Z)
int bin_get_byte_from_ptr(void) 
{
    int byte;

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

// binary mode used by SDOS PORT: driver        
// write to file attached to ptp an 8-bit byte as two hexdigits 
int bin_send_byte_to_ptp(int data) 
{
            if (data == 0x82) {
                // send char 130 dec with ptp active -> hack to send ascii file to ptp without echo on console.
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
            return data; 
}


/*  I/O instruction handlers, called from the MP-B2 module when a
    read or write occur to addresses 0x8000-0x801F. */

// return <0 if no char received (on ptr or on keyb polling)
//        0..255 char read
int GetPtrConsoleChar(void)
{
    extern int32 InstrCount;                   // intructions executed count 
    int32  m; 
    int byte; 

    if (InstrCount0 == 0) InstrCount0=InstrCount; 
    m = (InstrCount - InstrCount0) ; // number of instr executed elapsed from last time this routine was called
    if ((m>=0) && (m<150)) return -1; // too few instr exec -> no time to receive anything new -> return no data received
    InstrCount0=InstrCount;

    if (ptr_flag==0) {                 
        // PTR disabled, new reading from console (RTS state is ignored)
        byte=sio_unit.buf;
        sio_unit.buf=0; // mark polled char as processed, so next polled char can be read
        if (byte==0) return -1; // char zero is no char read
        sim_debug (DEBUG_read, &sio_dev, "Console In Char: %d $%02X (%s) \n", 
             byte, byte, strData(byte)); 
        return byte; // return next char
    }
    // RDR is enabled
    if ((ptr_unit.flags & UNIT_ATT) == 0) { // attached?
        ptr_flag = 0;           // clear reader flag
        return -1;               // no, no data
    }
    if (ptr_send_bin) {
        // binary mode used by SDOS PORT: driver
        // read attached file to ptr as two hexdigits per byte, until eof (signaled as ^Z)
        byte = bin_get_byte_from_ptr(); 
        return byte; 
    } else {
        // normal ascii PTR read
        if (ptr_unit.flags & UNIT_PTR_USE_RTS) {
           // check RTS state
           if (RTS==0) {
              return -1;               // PTR is asked to not send data to ACIA -> return no data
           }
        }
        if (feof(ptr_unit.fileref)) {
            byte = EOF; 
        } else {
            byte = getc(ptr_unit.fileref);
        }
        if (byte == EOF) { // end of file?
            ptr_flag = 0;           // clear reader flag
            byte = 0;               // and return byte zero
            sim_debug (DEBUG_read, &sio_dev, "PTR In Char: EOF: End of input Tape file (read 0 <NUL>), turn PTR OFF\n"); 
        } else {
            sim_debug (DEBUG_read, &sio_dev, "PTR In Char: %d $%02X (%s) \n", 
                byte, byte, strData(byte)); 
        }
        ptr_unit.pos++;             // step character count
        return byte; 
    }
}


// at 0x8004
int32 sio0s(int32 io, int32 data)
{
    int byte, tc; 

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
    sim_debug (DEBUG_flow, &sio_dev, "Configure port: $%02X \n", data);
    if ((data & 0x03) == 3) {       // reset port!
        status = 0x02;              // transmit data reg empty, receive flag clear
        sio_unit.buf = 0;
        sio_unit.pos = 0;
        odata = 0;
        sim_debug (DEBUG_flow, &sio_dev, "Reset port\n");
    }
    // get TC bits to Sets RTS line state, enable/disables ACIA
    // 00 RTS=1, Tx interrupt Disabled
    // 01 RTS=1, Tx interrupt Enabled
    // 10 RTS=0, Tx interrupt Disabled
    // 11 RTS=1, (tx break level), Tx interrupt Enabled
    tc = (data >> 5) & 3;
    if (tc == 2) {       
        // RTS=0 (-> Active state -> external device can send bytes to be received by SIO data port)
        if (RTS==1) {
           sim_debug (DEBUG_flow, &sio_dev, "RTS set to 0 \n");
           RTS=0; 
        }
    } else {
        // RTS=1 ( -> external devide instructed to stop sending chars to SIO data port) 
        if (RTS==0) {
           sim_debug (DEBUG_flow, &sio_dev, "RTS set to 1 \n");
           RTS=1; 
        }
    }

    InstrCount0=0; // when control reg is written, reset count for delay on rx chars into GetPtrConsoleChar
    return 0; 
}

// at 0x8005
int32 sio0d(int32 io, int32 data)
{
    extern int32 InstrCount;                   // intructions executed count 

    if (io == 0) {                      
        // data register read
        status &= 0xFE;  // clear RXF bit
        return odata; 
    } else {                            
        // data register write
        if ((ptr_flag) &&       // if PTR ON
            (data == 0x81) &&   // and $81 written on data register 
            (ptr_unit.flags & UNIT_BINMODE)) { // and "set ptr binmode" SCP command has been issued            
            // send char 129 dec with ptr active -> hack to receive bin file from ptr
            // this is non-realistic, but very handly to implement SDOS PORT: device over PTP and PTR
            ptr_send_bin = 1; 
        } else {
            ptr_send_bin = 0; 
        }
        if ((ptp_flag) &&       // if PTP ON
            (ptr_unit.flags & UNIT_BINMODE)) { // and "set ptr binmode" SCP command has been issued            
            // write to file attached to ptp an 8-bit byte as two hexdigits 
            data = bin_send_byte_to_ptp(data);
        }
        if (sio_unit.flags & UNIT_SIO7BIT) {
            data &= 127;    // use only plain 7bit ascii
        }
        sim_debug (DEBUG_write, &sio_dev, "Out Char: %d $%02X (%s) \n", 
             data, data, strData(data)); 
        if (isprint(data) || data == '\r' || data == '\n' || data == 8 || data == 26) { // printable?
            if (data != 26) {
                if ((ptp_flag) && (ptp_unit.flags & UNIT_PTP_NOECHO)) {
                    // do not echo punch characters on console 
                } else {
                    sim_putchar(data);          // echo character on console (except ^Z)
                    sim_debug (DEBUG_write, &sio_dev, "Show in console \n");
                }
            }
            if (ptp_flag && ptp_unit.flags & UNIT_ATT) { // PTP enabled & attached?
                putc(data, ptp_unit.fileref);
                ptp_unit.pos++;         // step character counter
                sim_debug (DEBUG_write, &sio_dev, "PTP Out: punch char in paper tape \n");
            }
        } else {                        // control Reader/Punch
            switch (data) {
                case 0x11:              // PTR on (^Q)
                    ptr_flag = 1;
                    ptr_send_bin=0; 
                    sim_debug (DEBUG_flow, &sio_dev, "Set PTR on \n");
                    break;
                case 0x12:              // PTP on (^R)
                    ptp_flag = 1;
                    ptp_send_bin=0; 
                    sim_debug (DEBUG_flow, &sio_dev, "Set PTP on \n");
                    break;
                case 0x13:              // PTR off (^S)
                    ptr_flag = 0;
                    sim_debug (DEBUG_flow, &sio_dev, "Set PTR off \n");
                    break;
                case 0x14:              // PTP off (^T)
                    ptp_flag = 0;
                    sim_debug (DEBUG_flow, &sio_dev, "Set PTP off \n");
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


/* end of mp-s.c */

