/*  mp-t.c: SWTPC MP-LA parallel interface data card for printer for Printer simulator

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

        Address     Mode    Function
        -------     ----    --------

        0x801C      pia1    mp-la parallel interface data card for printer
        0x801D      pia1

*/

#include <stdio.h>
#include "swtp_defs.h"

/* emulate MP-LA parallel interface card connected to Line Printer */

/* function prototypes */
t_stat par_reset (DEVICE *dptr);
t_stat lpt_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat lpt_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat lpt_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc); 

/* SS-50 I/O address space functions */
extern int32 nulldev(int32 io, int32 data);

int32 par0pia(int32 io, int32 data);  // par0pia and par1pia simulates SWTPC PR-40 line printer
int32 par1pia(int32 io, int32 data);
int32 par26pia(int32 io, int32 data); // par26pia and par27pia simulates exorciser line printer
int32 par27pia(int32 io, int32 data); 
extern int Mode64kRAM; 

/* Local Variables */

int32 lpt_iobase = 0x801c;              // default addr for PR40 line printer PIA
int32 lpt_type   = 0;                   // 0=disabled, 1=pr40 printer, 2=exorciser printer (M68SP702 702 Printer with MEX68PI Printer Interface I/O Module)

struct {
    int     Data, Strobe;                       
    int     active;                     // 1=parallel board PIA I/O configured
} par = {0};

DEVICE lpt_dev; 

MTAB lpt_mod[] = {
    { MTAB_XTD | MTAB_VDV, 0, "IOBASE", "IOBASE", &lpt_set_iobase, &lpt_show, NULL, NULL},
    { MTAB_XTD | MTAB_VDV, 0,  NULL,  "NONE",          &lpt_set, NULL, &lpt_dev, NULL},
    { MTAB_XTD | MTAB_VDV, 1,  NULL,  "PR40",          &lpt_set, NULL, &lpt_dev, NULL},
    { MTAB_XTD | MTAB_VDV, 2,  NULL,  "EXORPRINTER",   &lpt_set, NULL, &lpt_dev, NULL},
    { 0 }
};

UNIT lpt_unit = { UDATA (NULL, UNIT_SEQ + UNIT_ATTABLE, 0)
    };

DEVICE lpt_dev = {
    "LPT",                              //name
    &lpt_unit,                          //units
    NULL,                               //registers
    lpt_mod,                            //modifiers
    1,                                  //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    NULL,                               //examine
    NULL,                               //deposit
    &par_reset,                         //reset
    NULL,                               //boot
    NULL,                               //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    NULL,                               //debflags
    NULL,                               //msize
    NULL                                //lname
};

/* Reset routine */

t_stat par_reset (DEVICE *dptr)
{
    memset(&par, 0, sizeof(par));
    return SCPE_OK;
}

/* send char to printer */

extern char strAsc[4]; 

void par_out_char(int32 data)
{
    int i; 
 
    if ((lpt_unit.flags & UNIT_ATT)==0) return; // check LPT enabled & attached
    data = data & 127;
    if (data == 9) {
        // tab is 3 spaces
        for (i=0;i<3;i++) putc(32, lpt_unit.fileref); 
        sim_debug (DEBUG_write, &lpt_dev, "Print Char $09 (<TAB>) as spaces\n"); 
    } else if ((data >= 32) || (data == 13) || (data ==10)) {
        putc(data, lpt_unit.fileref); // send byte to printer file
        sim_debug (DEBUG_write, &lpt_dev, "Print Char $%02X (%s) sent by Parallel interface card\n", 
                     data, data==13 ? "<CR>": 
                           data==10 ? "<LF>": (strAsc[1]=data, strAsc)  );
    } else {
        sim_debug (DEBUG_write, &lpt_dev, "Non-printable Char $%02X ignored\n", data);
    }
}

//  I/O instruction handlers, called from the MP-B2 module 

// par0pia and par1pia simulates SWTPC PR40 printer

int32 par0pia(int32 io, int32 data)
{
    if (lpt_unit.flags & UNIT_DIS) {  
        // if printer disabled respond as not connected
        return nulldev(io, data);
    }
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o data register)
        return par.Data; 
    } 
    // when writing
    par.Data = data; 
    return 0; 
}

int32 par1pia(int32 io, int32 data)
{
    int Strobe; 

    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o control register)
        return 128; // allways returns ready
    } 
    if (lpt_unit.flags & UNIT_DIS) {  
        // if printer disabled respond as not connected
        return nulldev(io, data);
    }

    // writing to control register
    Strobe = data & 8; // get the strobe bit
    if ((par.Strobe != 0) && (Strobe == 0)) {
        // strobe bit going low -> send char to line printer
        par_out_char(par.Data); 
    }
    par.Strobe = Strobe; // save strobe state
    return 0;
}

// par26pia and par27pia simulates exorciser line printer

int32 par26pia(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o status register)
        return 2; // allways returns ready
    } 
    return 0; 
}

int32 par27pia(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o data register)
        return par.Data; // allways returns contents of data register
    } 
    par.Data = data; 
    par_out_char(data); 
    return 0; 
}

t_stat lpt_show(FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    if (lpt_type == 0) { fprintf (st, "none"); return SCPE_OK; }
    if (Mode64kRAM == 0) { fprintf (st, "on"); }
    else fprintf (st, "iobase=%04x", lpt_iobase);
    fprintf (st, ", type %s", 
       (lpt_type==1) ? "PR40" : (lpt_type==2) ? "ExorPrinter" : "No Printer"  );
    return SCPE_OK;
}

t_stat lpt_set_iobase(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    if (Mode64kRAM==0) {
        // if Mode64kRAM=0, PR40 PIA port is allways at 801C
        sim_printf("IOBASE can be set only if 64K mode is active\n");
        return SCPE_ARG;
    }

    // set the IO address for line printer PIA
    num = (int32) get_uint (cptr, 16, 65536, &r); // value is given as hex value
    if (r != SCPE_OK) return r;
    lpt_iobase=num & 0xFFFE; 
    return SCPE_OK; 
}

t_stat lpt_set(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    if (uptr == NULL) return SCPE_IERR;
    if (val == 0) {
        lpt_type=0; 
        sim_debug (DEBUG_flow, &lpt_dev, "No printer active \n"); 
    } else if (val == 1) {
        lpt_type=1; 
        sim_debug (DEBUG_flow, &lpt_dev, "PR40 active \n"); 
    } else if (val == 2) {
        lpt_type=2; 
        sim_debug (DEBUG_flow, &lpt_dev, "Exorciser Printer active \n"); 
    } else return SCPE_ARG;
    return SCPE_OK; 
}