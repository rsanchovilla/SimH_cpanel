/* i701_lp.c: IBM 701 Line Printer

   Copyright (c) 2021, Roberto Sancho

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

#include "i701_defs.h"

#define UNIT_LP         UNIT_ATTABLE  | UNIT_DISABLE

#define OPTION_NOECHO         (1 << (UNIT_V_UF + 5))
#define OPTION_NOPRINT        (1 << (UNIT_V_UF + 6))

/* Definitions */
uint32              lp_cmd(UNIT *, uint16, uint16);
t_stat              lp_srv(UNIT *);
t_stat              lp_reset(DEVICE *);
t_stat              lp_attach(UNIT *, CONST char *);
t_stat              lp_detach(UNIT *);
const char          *lp_description (DEVICE *dptr);
t_stat              lp_set_wiring (UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat              lp_show_wiring (FILE *st, UNIT *uptr, int32 val, CONST void *desc);

UNIT                lp_unit[1] = {
    {UDATA(&lp_srv, UNIT_LP, 0), 0}, 
};

MTAB lp_mod[] = {
    { OPTION_NOECHO, 0,               "display each printed line in console", "ECHO", NULL },
    { OPTION_NOECHO, OPTION_NOECHO,   "No echo printed line to console",      "NOECHO", NULL },
    { OPTION_NOPRINT, 0,              "add line on printer file",             "PRINT", NULL },
    { OPTION_NOPRINT, OPTION_NOPRINT, "do not use printer file",              "NOPRINT", NULL },
    { MTAB_XTD | MTAB_VUN, 0,         "WIRING", "WIRING", &lp_set_wiring, &lp_show_wiring, NULL, "Set printer control panel Wiring"},
    { 0 }
    };


DEVICE              lp_dev = {
    "LP", lp_unit, NULL, lp_mod,
    1, 8, 15, 1, 8, 8,
    NULL, NULL, &lp_reset, NULL, &lp_attach, &lp_detach,
    &lp_dib, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, NULL, NULL, NULL, &lp_description
};

extern t_int64             MQ;                          // M-Q register

// buffer to hold last printed lines on lp
char   lptPrintOut[80 * lptPrintOutMAX];
int    lptPrintOutCount = 0; // total number of lines printed

// IBM 701 tape/printer control Unit internal state
int bFastMode;              // =1 for FAST operation

// write line to print image as set by IBM 701
t_int64 lp_LineImage[26]; 
int lp_LineImage_index = 0; // next word to be write with copy instrction. if <0 -> should write a new line
int lp_hub = 0; // printer hubs active (bit0 set=hub 1 active, bit 1 set = hub 2 active, and so on) 
int lp_wiring = 0; 

// timing variables
t_int64 lp_timing_StartTickCount  = 0; // value of GlobalTickCount when card read cycle starts
t_int64 lp_timing_DisconTickCount = 0; // value of GlobalTickCount when card read disconnects if no data is provied with COPY opcode
extern int IOTicks;                    // ticks needed to execute i/o operation
extern int IOREMAIN;                   // cpu ticks ramaining to disconnect IO device

struct lp_wirings {
    uint32      mode;
    const char  *name;
};

// simulator available printer wirings
struct lp_wirings wirings[] = {
    {WIRING_NONE,        "NONE"},
    {WIRING_NAA_SpeedEx, "SPEEDEX"},
    {WIRING_NR9003,      "NR9003"},
    {WIRING_SO2,         "SO2"},
    {WIRING_PACT,        "PACT"},
    {0, 0},
};

// lpt print routine. If line is NULL prints empty lines
// add CR LF at end of each line
// if bNoEcho = 1 -> never echo to console,  = 0 -> echo to console depending on OPTION_NOECHO flag
void lpt_printline(UNIT * uptr, char * line, int bNoEcho)
{
    int i, nlen, n; 
    static char sCR[3] = {13, 10, 0};
    char c; 

    if ((uint32)(uptr->flags & OPTION_NOPRINT)) {
        // do not use printer file
    } else if (uptr->flags & UNIT_ATT) {
        // printout will be directed to file attached to unit, if any
        if ((line) && (line[0] > 0)) {
            sim_fwrite(line, 1, strlen(line), uptr->fileref);
        }
        sim_fwrite(sCR, 1, 2, uptr->fileref);
    }

    if (((uint32)(uptr->flags & OPTION_NOECHO)) || (bNoEcho)) {
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

    // last lptPrintOutMAX printed lines will be saved on circular buffer lptPrintOut
    // acts as mapped memory for 20 last lines printed
    nlen = (line) ? strlen(line):0; 
    n = 80 * (lptPrintOutCount % lptPrintOutMAX); 
    for (i=0;i<80;i++) {
        c = (i<nlen) ? line[i] : ' ';
        lptPrintOut[n++]=c;
    }
    lptPrintOutCount++; // incr num of lines printed
}

void PrintLine(UNIT * uptr)
{
    // fill printer line with printable ascii chars based on LineImage words 
    uint16 image[80];
    int i, n;
    char c, line[131]; 

    // fill binary card uint16 image[80] from t_int64 CardImage[24] words 
    PrepareCardImage(image, lp_LineImage, lp_LineImage_index, 0);

    sim_debug(DEBUG_CMD, &lp_dev, "PRINT\n");
    if (lp_wiring == WIRING_NAA_SpeedEx) {
        // two printer hubs used. Activated programatically with sense instruction
        // hub 7 -> print negative sign
        // hub 10 -> print /   / 
        n=0;
        for (i=0; i<80; i++) {
            c=sim_hol_to_ascii(image[i]);
            c=sim_toupper(c);
            if ((i==6) || (i==8) || (i==15)  || (i==23)) {
                // insert space between columns
                line[n++] = ' ';
            } else if (i==10) {
                // insert space between columns, convert ---- to /   /
                line[n++] = ' ';
                if ((c=='-') && (lp_hub & (1 << 9))) {
                    line[n++] = '/'; 
                    line[n++] = ' '; 
                    line[n++] = ' '; 
                    line[n++] = ' '; 
                    line[n++] = '/'; 
                    i += 4; 
                    continue; 
                }
            } else if (i==21) {
                // insert space and sign
                line[n++] = ' ';
                line[n++]= (lp_hub & (1 << 7)) ? '-':'+';
            } else if (i==37) {
                // delete extra space
                n -= 7;
            }
            line[n++] = c; 
        }
        line[n++] = 0; // terminate string
    } else if (lp_wiring == WIRING_NR9003) {
        // two printesr hubs used. Activated programatically with sense instruction
        // hub 6 -> print a sign + or -
        // hub 7 -> print negative sign
        n=0;
        for (i=0; i<80; i++) {
            c=sim_hol_to_ascii(image[i]);
            c=sim_toupper(c);
            if ((i==3) || (i==5) || (i==10)  || (i==12)) {
                // insert dots between columns
                if (lp_hub & (1 << 6)) {
                    line[n++] = '.';
                } else {
                    line[n++] = ' ';
                }
            } else if (i==26) {
                // insert space 
                line[n++] = ' ';
            } else if (i==24) {
                // insert space and sign
                line[n++] = ' ';
                if (lp_hub & (1 << 6)) {
                    line[n++]= (lp_hub & (1 << 7)) ? '-':'+';
                } else {
                    line[n++] = ' ';
                }
                line[n++] = ' ';
            } else if (i==20) {
                // delete extra space
                n -= 4;
            } else if (i==36) {
                // delete extra space
                n -= 3;
            }
            line[n++] = c; 
        }
        line[n++] = 0; // terminate string
    } else if (lp_wiring == WIRING_SO2) {
        // no printer hubs used. 
        // just add extra spaces between cols
        n=0;
        for (i=0; i<80; i++) {
            c=sim_hol_to_ascii(image[i]);
            c=sim_toupper(c);
            if ((i==1) || (i==56)) {
                // insert triple space 
                line[n++] = ' '; line[n++] = ' '; 
            } else if ((i==4) || (i==8) || (i==11) || (i==14) || (i==26) || (i==28)) {
                // insert space 
                line[n++] = ' ';
            }
            line[n++] = c; 
        }
        line[n++] = 0; // terminate string
    } else {
        // default wiring to none
        for (i=0; i<80; i++) {
            c=sim_hol_to_ascii(image[i]);
            c=sim_toupper(c);
            line[i] = c; 
        }
        line[80] = 0; // terminate string
    }

    sim_debug(DEBUG_DETAIL, &lp_dev, "Print Line: %s\n", sim_trim_endspc(line));
    lpt_printline(uptr, line, 0);
    
}
/* Start off a lpt command */
uint32 lp_cmd(UNIT * uptr, uint16 cmd, uint16 fast)
{
    int n,msec; 

    IOTicks = 0; // duration of operation in Ticks

    if (cmd==OP_STOP) {
        // flush line being printed, if any. Disconect printer (will need a new WRITE
        // instruction to select again lp)
        if (lp_LineImage_index > 0) {
            sim_debug(DEBUG_CMD, &lp_dev, "Disconnecting ... flush line being printed \n");
            PrintLine(uptr); 
        }
        lp_LineImage_index=-1;
        lp_hub = 0; 
        return SCPE_OK;
    }

    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, &lp_dev, "Printer Unit disabled\n");
        return SCPE_UDIS;
    }   
    if (cmd==OP_WRITE) { 
        // start a new printing cycle

        // Timing calculation
        // calc msec = time for printer to finish current in progress line printing cycle
        // calc nTick = number to ticks WRITE needs to be executed by cpu
        // print line cycle: 400 msec (150 lines per minute)
        if (lp_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            lp_timing_StartTickCount = 0;
        }
        if ((lp_timing_StartTickCount == 0) || ((msec = msec_elapsed(lp_timing_StartTickCount, GlobalTicksCount)) > 400)) {
            msec = -1; // printer not in motion
        }
        if (msec == -1) {
            // printer not in motion. Set start of motion as current tickcount
            lp_timing_StartTickCount = GlobalTicksCount;
            // first 9-left copy must be given before 280 msec counting after start of motion
            lp_timing_DisconTickCount = lp_timing_StartTickCount + msec_to_ticks(280); 
            // time needed for WRITE to execute and start printer motion: 220 msec
            IOTicks = msec_to_ticks(220); 
            sim_debug(DEBUG_CMD, &lp_dev, "PRINTER: Start Motion\n");
        } else {
            // printer in motion. Wait to finish current cycle 
            msec = 400-msec; // time remaining for current cycle to end
            IOTicks = msec_to_ticks(msec); 
            // printer in motion. Next cycle is 400 msec after the previous one
            lp_timing_StartTickCount += msec_to_ticks(400); 
            // first 9-left copy must be given before 173 msec counting after start of cycle
            lp_timing_DisconTickCount = lp_timing_StartTickCount + msec_to_ticks(173); 
            // time needed for WRITE to execute in continuous motion: 100 msec (educated guess)
            IOTicks += msec_to_ticks(100); 
            sim_debug(DEBUG_CMD, &lp_dev, "PRINTER: Continuous print (wait %d msec to start printing cycle\n", msec);
        }
        // set the max time (in tick counts) to auto-disconnect device
        IOREMAIN = (int) (lp_timing_DisconTickCount - GlobalTicksCount + 10); 

        if (lp_LineImage_index > 0) PrintLine(uptr); 
        lp_LineImage_index=0;
        lp_hub = 0;
    } else if ((cmd & 31) == OP_SENSE) {
        // set printer hub
        int n = cmd >> 5; // hub number 1..
        lp_hub = lp_hub | (1 << n);
    } else if (cmd==OP_COPY) {
        if (lp_LineImage_index < 0) {
            sim_debug(DEBUG_CMD, &lp_dev, "COPY issued without previous WRITE\n");
            return STOP_COPYCHECK;
        }
        lp_LineImage[lp_LineImage_index++] = MQ; 

        // Timing calculation
        // calc msec = time remaining for printer to accept next data from COPY
        // calc nTick = number to ticks COPY needs to be executed by cpu
        if (lp_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            lp_timing_DisconTickCount = GlobalTicksCount; 
            lp_timing_StartTickCount = 0;
        }
        msec = msec_elapsed(GlobalTicksCount, lp_timing_DisconTickCount);
        if ((n = (int) (lp_timing_DisconTickCount - GlobalTicksCount)) <0) {
            sim_debug(DEBUG_CMD, &lp_dev, "COPY is %d Ticks (%d msec) too late. Printer disconnected\n", -n, -msec);
            return STOP_COPYCHECK;
        } else {
            sim_debug(DEBUG_CMD, &lp_dev, "COPY has been waiting for %d Ticks (%d msec)\n", n, msec);
        }
        // wait for printer to be able to accept COPY data
        IOTicks = n; 
        if (lp_LineImage_index & 1) {
            lp_timing_DisconTickCount += usec_to_ticks(540); // right copy must be given within 540 microsec. 
        } else {
            lp_timing_DisconTickCount += msec_to_ticks(13); // left copy must be given within 13 msec. 
        }
        lp_timing_DisconTickCount += 20; // add 20 ticks as margin
        IOREMAIN = (int) (lp_timing_DisconTickCount - GlobalTicksCount); 

        sim_debug(DEBUG_CMD, &lp_dev, "Send word %d to Printer \n", lp_LineImage_index);
        if (lp_LineImage_index == 24) {
            // line to be printed full completed -> print it
            PrintLine(uptr); 
            lp_LineImage_index = -1; // ... and disconnect 
            lp_hub = 0;
        }
    } else {
        sim_debug(DEBUG_CMD, &lp_dev, "Unknown LP command\n");
        return SCPE_IERR;
    }
    return SCPE_OK;
}

/* Handle processing of printer requests. */
t_stat lp_srv(UNIT * uptr)
{
    return SCPE_OK;
}

/* Set printer control panel wiring */
t_stat lp_set_wiring (UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int f;

    if (uptr == NULL) return SCPE_IERR;
    if (cptr == NULL) return SCPE_ARG;
    for (f = 0; wirings[f].name != 0; f++) {
        if (strcmp (cptr, wirings[f].name) == 0) {
            lp_wiring = wirings[f].mode;
            return SCPE_OK;
            }
        }
    return SCPE_ARG;
}

/* Show printer control panel wiring */
t_stat lp_show_wiring (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    int f;

    for (f = 0; wirings[f].name != 0; f++) {
        if (lp_wiring == wirings[f].mode) {
            fprintf (st, "%s wiring", wirings[f].name);
            return SCPE_OK;
        }
    }
    fprintf (st, "invalid control panel wiring (%d)", lp_wiring);
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
    lp_LineImage_index=-1; 
    lp_hub = 0;
    lp_wiring = 0;
    memset(lp_LineImage, 0, sizeof(lp_LineImage));
    return SCPE_OK;
}

t_stat lp_detach(UNIT * uptr)
{
    sim_cancel(uptr); // cancel any pending command

    if (lp_LineImage_index > 0) PrintLine(uptr); 
    lp_LineImage_index=-1; 
    lp_hub = 0;
    lp_wiring = 0;

    lpt_printline(uptr, NULL, 0); // print a blank line as a separator
    return detach_unit(uptr);
}

const char * lp_description(DEVICE *dptr)
{
   return "IBM 716 Line Printer";
}


