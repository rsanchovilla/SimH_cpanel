/* i701_cdp.c: IBM 701 Card punch.

   Copyright (c) 2018-2021, Roberto Sancho

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

   This is the standard card punch.

*/

#include "i701_defs.h"
#include "sim_card.h"

#define UNIT_CDP        UNIT_ATTABLE | MODE_BIN

/* std devices. data structures

   cdp_dev      Card Punch device descriptor
   cdp_unit     Card Punch unit descriptor
   cdp_reg      Card Punch register list
   cdp_mod      Card Punch modifiers list
*/

uint32              cdp_cmd(UNIT *, uint16, uint16);
t_stat              cdp_srv(UNIT *);
t_stat              cdp_attach(UNIT *, CONST char *);
t_stat              cdp_detach(UNIT *);
t_stat              cdp_help(FILE *, DEVICE *, UNIT *, int32, const char *);
t_stat              cdp_set_echo (UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat              cdp_show_echo (FILE *st, UNIT *uptr, int32 val, CONST void *desc);
const char         *cdp_description(DEVICE *dptr);

UNIT                cdp_unit[1] = {
    {UDATA(cdp_srv, UNIT_CDP, 0), 600}
};

MTAB                cdp_mod[] = {
    {MTAB_XTD | MTAB_VUN, 0, "FORMAT",    "FORMAT",  &sim_card_set_fmt, &sim_card_show_fmt, NULL, "Set card format"},
    {MTAB_XTD | MTAB_VUN, 0, "ECHOLEVEL", "ECHOLEVEL",    &cdp_set_echo,     &cdp_show_echo,     NULL, "Set console printout for punched cards"},
    {0}
};

DEVICE              cdp_dev = {
    "CDP", cdp_unit, NULL, cdp_mod,
    1, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, NULL, &cdp_attach, &cdp_detach,
    &cdp_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cdp_help, NULL, NULL, &cdp_description
};

extern t_int64             MQ;                          // M-Q register

// vars that notify the state of Card Punching process
int nCardInPunchStacker = 0;          // number of cards in punch deck (=number of cards punched)
uint32 tm0CardInPunchStacker = 0;     // set to sim_os_msec by cdp_svr when card entering take hopper, as start of animation time origin
int bCardPunchError = 0;               // simulated h/w error on punching card (no file atached, write host error)

// write card image as set by IBM 701
t_int64 cdp_CardImage[26]; 
int cdp_CardImage_index = 0; // next word to be write with copy instrction. if <0 -> should write a new card
int cdp_EchoLevel = 0; // echo card read to console. 0=no echo, 1=only text, 2=text+octal, 3=text+octal+binary

// timing variables
t_int64 cdp_timing_StartTickCount  = 0; // value of GlobalTickCount when card read cycle starts
t_int64 cdp_timing_DisconTickCount = 0; // value of GlobalTickCount when card read disconnects if no data is provied with COPY opcode
extern int IOTicks;                     // ticks needed to execute i/o operation
extern int IOREMAIN;                    // cpu ticks ramaining to disconnect IO device


// fill binary card uint16 image[80] from t_int64 CardImage[24] words 
// prepare a card image. get index full 36bit words from array CardImage
// these are the bitmap of punched card. 
//
//                              ------------------------------------------------------+
//                           /    | <-                 72 columns                  -> |
//                           |    +-------------------------+-------------------------+
//            Row Y          |    |      CardImage[22]      |      CardImage[23]      |
//                           |    +-------------------------+-------------------------+
//                X          |    |                         |                         |
//                0          |    | <-    36 columns     -> |                         |
//                ..         |    |                         |                         |
//                6          |    |                         |                         |
//                           |    +-------------------------+-------------------------+
//                7          |    |      CardImage[4]       |           ...           |
//                           |    +-------------------------+-------------------------+
//                8          |    |      CardImage[2]       |      CardImage[3]       |
//                           |    +-------------------------+-------------------------+
//                9          |    |      CardImage[0]       |      CardImage[1]       |
//                           +----+-------------------------+-------------------------+
//
void PrepareCardImage(uint16 * image, t_int64 * CardImage, int index, int SkipCols)
{
    int i, n;
    int vmask, col; 
    t_int64 w; 

    memset(image, 0, 80*sizeof(*image));
    n=0;
    vmask=1; 
    while (vmask <= 0x800) {
        for (col=0;col<2;col++) {
            if (n>=index) {
                w=0;
            } else {
                w=CardImage[n]; 
                CardImage[n]=0; // clear image buffer
                n++;
            }
            for (i=0;i<36;i++) {
                if (w & (1LL << (35-i))) {
                    image[SkipCols+i+col*36] |= vmask; 
                }
            }
        }
        vmask = vmask << 1; 
    }
}

void WriteCards(UNIT * uptr)
{
    uint16 image[80];

    // fill binary card uint16 image[80] from t_int64 CardImage[24] words 
    PrepareCardImage(image, cdp_CardImage, cdp_CardImage_index, cdr_skip_cols());

    sim_debug(DEBUG_CMD, &cdp_dev, "PUNCH CARD\n");
    echo_cardimage(&cdp_dev, image, cdp_EchoLevel, "Punched card", NULL);
    sim_punch_card(uptr, image);

    nCardInPunchStacker = sim_card_output_hopper_count(uptr);
    if (tm0CardInPunchStacker==0) {
        tm0CardInPunchStacker = sim_os_msec();  // this is the time stamp (in real word msec) when starts the animation of punched-card entering card-punch-stacker) 
    }

}


/* Card punch routine */
uint32 cdp_cmd(UNIT * uptr, uint16 cmd, uint16 addr)
{
    int n,msec; 

    IOTicks = 0; // duration of operation in Ticks

    if (cmd==OP_STOP) {
        // flush card being punched, if any. Disconect puncher (will need a new WRITE
        // instruction to select again cdp)
        if (cdp_CardImage_index > 0) {
            sim_debug(DEBUG_CMD, &cdp_dev, "Disconnecting ... flush card being punched \n");
            WriteCards(uptr); 
        }
        cdp_CardImage_index=-1; 
        return SCPE_OK;
    }

    bCardPunchError = 0; 

    /* Test ready */
    if ((uptr->flags & UNIT_ATT) == 0) {
        sim_debug(DEBUG_EXP, &cdp_dev, "No file attached to Card Punch Unit\n");
        bCardPunchError = 1; 
        return SCPE_UNATT;
    }
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, &cdp_dev, "Card Punch Unit disabled\n");
        return SCPE_UDIS;
    }   

    if (cmd==OP_WRITE) {
        // prepare a new punched card on card puncher

        // Timing calculation
        // calc msec = time for card punch to finish current in progress cycle
        // calc nTick = number to ticks WRITE needs to be executed by cpu
        // punch card cycle: 600 msec (100 cards per minute)
        if (cdp_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            cdp_timing_StartTickCount = 0;
        }
        if ((cdp_timing_StartTickCount == 0) || ((msec = msec_elapsed(cdp_timing_StartTickCount, GlobalTicksCount)) > 600)) {
            msec = -1; // card punch not in motion
        }
        if (msec == -1) {
            // card punch not in motion. Set start of motion as current tickcount
            cdp_timing_StartTickCount = GlobalTicksCount;
            // first 9-left copy must be given before 400 msec counting after start of motion
            cdp_timing_DisconTickCount = cdp_timing_StartTickCount + msec_to_ticks(400); 
            // time needed for WRITE to execute and start card punch motion: 330 msec
            IOTicks = msec_to_ticks(330); 
            sim_debug(DEBUG_CMD, &cdp_dev, "PUNCH CARD: Start Card Motion\n");
        } else {
            // card punch in motion. Wait to finish current cycle 
            msec = 600-msec; // time remaining for current cycle to end
            IOTicks = msec_to_ticks(msec); 
            // card punch in motion. Next cycle is 600 msec after the previous one
            cdp_timing_StartTickCount += msec_to_ticks(600); 
            // first 9-left copy must be given before 95 msec counting after start of cycle
            cdp_timing_DisconTickCount = cdp_timing_StartTickCount + msec_to_ticks(95); 
            // time needed for WRITE to execute in continuous motion: 40 msec (educated guess)
            IOTicks += msec_to_ticks(40); 
            sim_debug(DEBUG_CMD, &cdp_dev, "PUNCH CARD: Continuous Card Punch (wait %d msec to start card cycle\n", msec);
        }
        // set the max time (in tick counts) to auto-disconnect device
        IOREMAIN = (int) (cdp_timing_DisconTickCount - GlobalTicksCount + 10); 

        if (cdp_CardImage_index > 0) WriteCards(uptr); 
        cdp_CardImage_index=0;

    } else if (cmd==OP_COPY) {
        if (cdp_CardImage_index < 0) {
            sim_debug(DEBUG_CMD, &cdp_dev, "COPY issued without previous WRITE\n");
            return STOP_COPYCHECK;
        }
        cdp_CardImage[cdp_CardImage_index++] = MQ; 

        // Timing calculation
        // calc msec = time remaining for card punch to accept next data from COPY
        // calc nTick = number to ticks COPY needs to be executed by cpu
        if (cdp_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            cdp_timing_DisconTickCount = GlobalTicksCount; 
            cdp_timing_StartTickCount = 0;
        }
        msec = msec_elapsed(GlobalTicksCount, cdp_timing_DisconTickCount);
        if ((n = (int) (cdp_timing_DisconTickCount - GlobalTicksCount)) <0) {
            sim_debug(DEBUG_CMD, &cdp_dev, "COPY is %d Ticks (%d msec) too late. Card punch disconnected\n", -n, -msec);
            return STOP_COPYCHECK;
        } else {
            sim_debug(DEBUG_CMD, &cdp_dev, "COPY has been waiting for %d Ticks (%d msec)\n", n, msec);
        }
        // wait for card punch to be able to accept COPY data
        IOTicks = n; 
        if (cdp_CardImage_index & 1) {
            cdp_timing_DisconTickCount += usec_to_ticks(540); // right copy must be given within 540 microsec. 
        } else {
            cdp_timing_DisconTickCount += msec_to_ticks(31); // left copy must be given within 31 msec. 
        }
        cdp_timing_DisconTickCount += 20; // add 20 ticks as margin
        IOREMAIN = (int) (cdp_timing_DisconTickCount - GlobalTicksCount); 

        sim_debug(DEBUG_CMD, &cdp_dev, "Write word %d to Card Punch \n", cdp_CardImage_index);
        if (cdp_CardImage_index == 24) {
            // card full -> write
            WriteCards(uptr); 
            cdp_CardImage_index = -1;   // ... and disconnect 
        }
    } else {
        sim_debug(DEBUG_CMD, &cdp_dev, "Unknown CDP command\n");
        return SCPE_IERR;
    }

    return SCPE_OK;

}

/* Handle transfer of data for card punch */
t_stat cdp_srv(UNIT *uptr) {

    // I/O is synchronous. No need to set up srv
    return SCPE_OK;
}

/* Set card punch echo to console */
t_stat cdp_set_echo (UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    t_stat              r;
    int                 num;

    if (uptr == NULL) return SCPE_IERR;
    if (cptr == NULL) {
        cdp_EchoLevel = 1;  // no param means set (=1)
    } else {
        num = (int) get_uint (cptr, 10, 3, &r);
        if (r != SCPE_OK) return r;
        cdp_EchoLevel=num; 
    }
    return SCPE_OK;
}

t_stat cdp_show_echo (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "ECHOLEVEL Set to %d (%s) ", cdp_EchoLevel, 
        (cdp_EchoLevel == 0) ? "No Echo" : (cdp_EchoLevel == 1) ? "Echo text" : 
        (cdp_EchoLevel == 2) ? "Echo text + octal" : "Echo text + octal + binary" 
        );
    return SCPE_OK;
}

t_stat cdp_attach(UNIT * uptr, CONST char *file)
{
    t_stat              r;

    r = sim_card_attach(uptr, file);
    if (SCPE_BARE_STATUS(r) != SCPE_OK)
       return r;
    uptr->u5 = 0;

    nCardInPunchStacker = sim_card_output_hopper_count(uptr);
    tm0CardInPunchStacker = 0;

    memset(cdp_CardImage, 0, sizeof(cdp_CardImage));
    cdp_CardImage_index=-1; 

    return SCPE_OK;
}

t_stat cdp_detach(UNIT * uptr)
{
    nCardInPunchStacker = 0;
    tm0CardInPunchStacker = 0;
    if (cdp_CardImage_index > 0) WriteCards(uptr); 
    cdp_CardImage_index=-1; 
 
    return sim_card_detach(uptr);
}

t_stat cdp_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", cdp_description(dptr));
   fprintf (st, "The IBM 721 Card Punch writes cards using the selected\n");
   fprintf (st, "control panel wiring to set the format of punched cards.\n");

   sim_card_attach_help(st, dptr, uptr, flag, cptr);
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * cdp_description(DEVICE *dptr)
{
   return "IBM 721 Card Punch Unit";
}


