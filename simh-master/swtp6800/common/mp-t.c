/*  mp-t.c: SWTP MP-T Timer simulator

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

    MODIFICATIONS:

    NOTES:

        Address     Mode    Function
        -------     ----    --------

        0x8012      pia1    mp-t timer card data port      
        0x8013      pia2

*/

#include <stdio.h>
#include "swtp_defs.h"

/* emulate MP-T timer card  */

extern int32 IRQ;
extern int32 InstrCount;                   // intructions executed count 
    

#define TIMER_INTERVAL  1000      // for svr call 

/* function prototypes */

t_stat timer_reset (DEVICE *dptr);
t_stat timer_svc (UNIT *uptr);

/* SS-50 I/O address space functions */

int32 timer0pia(int32 io, int32 data);
int32 timer1pia(int32 io, int32 data);

/* Local Variables */

struct {
    int     active;                     // 1=timer board PIA I/O configured
    int     unit;                       // 0=timer not running, 1=Interval var unit is usec, 2=msec
    uint32  Interval;
    uint32  Tm0;                 
} timer = {0};

UNIT timer_unit = { UDATA (&timer_svc, UNIT_SEQ, 0)
    };

DEVICE timer_dev = {
    "MP-T",                             //name
    &timer_unit,                         //units
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
    &timer_reset,                       //reset
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

t_stat timer_reset (DEVICE *dptr)
{
    timer.unit=0;
    return SCPE_OK;
}

t_stat timer_svc (UNIT *uptr)
{
    uint32 tnow; 
    uint32 msec; 

    if (timer.unit==0) return SCPE_OK; 
    if (timer.unit==1) {
        // interrupt interval in usec -> use intructions count to measure time
        // at a rate of aprox 0.500 MIPS 
        sim_activate(&timer_unit, timer.Interval / 2);
        timer.Tm0 = (uint32) InstrCount; 
    } else {
        // interrupt interval in msec, less than 1min -> use sim_os_msec
        sim_activate(&timer_unit, TIMER_INTERVAL * ((timer.Interval > 1000) ? 500:1));
        // check if msec has elapsed from last call
    	tnow = sim_os_msec(); 
        msec = tnow - timer.Tm0; 
        if (msec < timer.Interval) return SCPE_OK; // do not interrupt yet
        timer.Tm0 = tnow; 
    }
    // signal IRQ pending from timer!
    IRQ=100; 
    return SCPE_OK; 
}

//  I/O instruction handlers, called from the MP-B2 module 

int32 timer0pia(int32 io, int32 data)
{
    int nibble;

    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        // clear the interrupt line if comming from timer
        if (IRQ==100) IRQ=0;
        return 0; 
    } 
    // when writing
    // bit7=1 -> stop timer
    // lower nibble
	//	 00 1 usec      04 10 msec     08 100 sec    0C no output
	//	 01 10 usec     05 100 msec    09 1 min      0D no output
	//	 02 100 usec    06 1 sec       0A 1 hour     0E 20 msec
	//	 03 1 msec      07 10 sec      0B 10 min     0F no output
		
    if (timer.active==0) return 0; 

    nibble=data & 0x0F; 
    if ((data & 0x80) || (nibble == 0xC) || (nibble == 0xD) || (nibble == 0xF)) {
        // stop timer
        timer.unit=0;
        sim_cancel(&timer_unit);
        return 0;
    }
    // start timer
    if (nibble <= 3) {
        timer.unit=1; // unit is microsecond (usec)
        timer.Interval=1; 
        while (nibble > 0) {
            timer.Interval=timer.Interval*10;
            nibble--;
        }       
        sim_activate(&timer_unit, timer.Interval / 2);
        timer.Tm0 = InstrCount; 
    } else {
        timer.unit=2; // unit is millisecond (msec)
        switch (nibble) {
            case  4: timer.Interval=        10; break;
            case  5: timer.Interval=       100; break;
            case  6: timer.Interval=      1000; break;
            case  7: timer.Interval=   10*1000; break;
            case  8: timer.Interval=  100*1000; break;
            case  9: timer.Interval=   60*1000; break;
            case 10: timer.Interval=60*60*1000; break;
            case 11: timer.Interval=10*60*1000; break;
            case 14: timer.Interval=        20; break;
        }
        sim_activate(&timer_unit, TIMER_INTERVAL);
        timer.Tm0 = sim_os_msec(); 
    }
    return 0; 
}

int32 timer1pia(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        return 0; 
    } 
    // when writing 3D, activate timer operation (i.e. configure PIA I/O lines)
    timer.active = ((data == 0x3D) ? 1:0);
    return 0; 
}


