/* NORC_lp.c: IBM NORC Line Printer

   Copyright (c) 2020, Roberto Sancho

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

*/

#include "NORC_defs.h"

#define UNIT_LP         UNIT_ATTABLE  | UNIT_DISABLE

extern t_int64             CRT[3600]; // CRT main Memory 
extern int                 PRT;       // printer ready flag 

#define OPTION_NOECHO         (1 << (UNIT_V_UF + 5))
#define OPTION_NOPRINT        (1 << (UNIT_V_UF + 6))

/* Definitions */
uint32              lp_cmd(UNIT *, uint16, uint16);
t_stat              lp_srv(UNIT *);
t_stat              lp_reset(DEVICE *);
t_stat              lp_attach(UNIT *, CONST char *);
t_stat              lp_detach(UNIT *);
const char          *lp_description (DEVICE *dptr);

UNIT                lp_unit[3] = {
    {UDATA(&lp_srv, UNIT_LP, 0), 0}, /* IBM 407 offline Printer */
    {UDATA(&lp_srv, UNIT_LP, 0), 0}, /* Printer A / 1 */
    {UDATA(&lp_srv, UNIT_LP, 0), 0}, /* Printer B / 2 */
};

MTAB lp_mod[] = {
    { OPTION_NOECHO, 0,               "display each printed line in console", "ECHO", NULL },
    { OPTION_NOECHO, OPTION_NOECHO,   "No echo printed line to console",      "NOECHO", NULL },
    { OPTION_NOPRINT, 0,              "add line on printer file",             "PRINT", NULL },
    { OPTION_NOPRINT, OPTION_NOPRINT, "do not use printer file",              "NOPRINT", NULL },
    { 0 }
    };


DEVICE              lp_dev = {
    "LP", lp_unit, NULL, lp_mod,
    3, 8, 15, 1, 8, 8,
    NULL, NULL, &lp_reset, NULL, &lp_attach, &lp_detach,
    &lp_dib, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, NULL, NULL, NULL, &lp_description
};

// buffer to hold last printed lines on lp1
char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
int    lptPrintOutCount = 0; // total number of lines printed

// IBM NORC tape/printer control Unit internal state
extern int bFastMode;              // =1 for FAST operation

// lpt print routine. If line is NULL prints empty lines
// add CR LF at end of each line
// if error (unit disabled), return -1
int lpt_printline(UNIT * uptr, char * line)
{
    int i; 
    static char sCR[3] = {13, 10, 0};

    if (uptr->flags & UNIT_DIS) return -1; // disabled

    if ((uint32)(uptr->flags & OPTION_NOPRINT)) {
        // do not use printer file
    } else if (uptr->flags & UNIT_ATT) {
        // printout will be directed to file attached to unit, if any
        if ((line) && (line[0] > 0)) {
            sim_fwrite(line, 1, strlen(line), uptr->fileref);
        }
        sim_fwrite(sCR, 1, 2, uptr->fileref);
    }

    if ((uint32)(uptr->flags & OPTION_NOECHO)) {
        // do not echo to console
    } else {
        // echo to console
        if ((line) && (line[0] > 0)) {
            for (i=0;line[i];i++) {
                if (i==75) {
                    if (line[i]!=32) {sim_putchar('.');sim_putchar('.');sim_putchar('.');}
                    break;  
                }
                sim_putchar(line[i]);
            }
        }
        sim_putchar(13);
        sim_putchar(10);
    }

    if (uptr == &lp_unit[1]) {
        // last lptPrintOutMAX printed lines will be saved on circular buffer lptPrintOut
        // acts as mapped memory for 20 last lines printed
        int nlen = (line) ? strlen(line):0; 
        int n; 
        char c; 
        n = LPT_COLUMNS * (lptPrintOutCount % lptPrintOutMAX); 
        for (i=0;i<LPT_COLUMNS;i++) {
            c = (i<nlen) ? line[i] : ' ';
            lptPrintOut[n++]=c;
        }
        lptPrintOutCount++; // incr num of lines printed
    }
    return 0;
}


/* Start off a lpt command */
uint32 lp_cmd(UNIT * uptr, uint16 cmd, uint16 fast)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = uptr - &lp_unit[0];
    int                 i, nW, n, time;
    t_int64             d; 

    char buf[80];
    char line[250]; 

    bFastMode = fast;
    
    // cmd=1 for normal print, =2 for special print
    sim_debug(DEBUG_CMD, dptr, "Printer LP%d: init cycle %s\n", unit, (cmd==2) ? "special":"");
    sim_debug(DEBUG_CMD, dptr, "     0001: %08d%08d  %08d%08d %08d%08d %08d%08d\n", 
                                            printfw(CRT[1]), printfw(CRT[2]), printfw(CRT[3]), printfw(CRT[4]));
    sim_debug(DEBUG_CMD, dptr, "     0005: %08d%08d  %08d%08d %08d%08d \n", 
                                            printfw(CRT[5]), printfw(CRT[6]), printfw(CRT[7]));
    line[0]=0;
    for (nW=1;nW<=7;nW++) {
        d = CRT[nW]; // get word to print
        buf[0]=0;
        if (d==0) {
            // if word is zero, print spaces
            for (i=0; i<16; i++) buf[i]=' '; 
        } else {
            // if word not zero, print its 16 digits + space
            for (i=0; i<16; i++) {
                buf[i]='0' + (int) Shift_Digits(&d, 1);
            }
        }
        buf[16]=' '; 
        buf[17]=0; 
        // add buf to line
        n=strlen(line);
        for(i=0;buf[i];i++) {
            if (n<sizeof(line)-1) line[n++] = buf[i];
        }
        if (n<sizeof(line)-1) line[n] = 0; 
        line[sizeof(line)-1]=0; // sanity
    }
    n=lpt_printline(uptr, line);
    if (n<0) {
        sim_debug(DEBUG_CMD, dptr, "Printer %d: disabled\n", unit);
        return STOP_PRINTER; 
    }

    // schedulle printer command termination
    if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
        sim_debug(DEBUG_CMD, dptr, "Printer %d: ready (fast mode)\n", unit);
        return SCPE_OK; 
    }   
    PRT=0; // set printer ready flag OFF
    time = 400*1000;
    sim_activate(uptr, time);
    sim_debug(DEBUG_DETAIL, dptr, "... Printer %d cycle will take %d msec\n", unit, time / 1000); 
    return SCPE_OK;
}

/* Handle processing of printer requests. */
t_stat lp_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);

    PRT=1; 
    sim_debug(DEBUG_CMD, dptr, "Printer %d cycle terminated\n", unit);
    return SCPE_OK;
}

t_stat lp_reset(DEVICE * dptr)
{
    return SCPE_OK;
}

t_stat lp_attach(UNIT * uptr, CONST char *file)
{
    t_stat              r;

    if ((r = attach_unit(uptr, file)) != SCPE_OK) return r;
    return SCPE_OK;
}

t_stat lp_detach(UNIT * uptr)
{
    sim_cancel(uptr); // cancel any pending command
    lpt_printline(uptr, NULL); // print a blank line as a separator
    return detach_unit(uptr);
}

const char * lp_description(DEVICE *dptr)
{
   return "IBM NORC Line Printer";
}


