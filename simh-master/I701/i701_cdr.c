/* i701_cdr.c: IBM 701 Card reader.

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

   This is the standard card reader.
*/

#include "i701_defs.h"
#include "sim_card.h"

#define UNIT_CDR        UNIT_ATTABLE | UNIT_RO | MODE_026 | MODE_LOWER

#define OPTION_SKIPCOLS18         (1 << (UNIT_V_UF + 5))

/* std devices. data structures

   cdr_dev      Card Reader device descriptor
   cdr_unit     Card Reader unit descriptor
   cdr_reg      Card Reader register list
   cdr_mod      Card Reader modifiers list
*/

uint32              cdr_cmd(UNIT *, uint16, uint16);
t_stat              cdr_srv(UNIT *);
t_stat              cdr_attach(UNIT *, CONST char *);
t_stat              cdr_detach(UNIT *);
t_stat              cdr_help(FILE *, DEVICE *, UNIT *, int32, const char *);
t_stat              cdr_set_echo (UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat              cdr_show_echo (FILE *st, UNIT *uptr, int32 val, CONST void *desc);
int                 cdr_skip_cols (void);
const char         *cdr_description(DEVICE *dptr);

// vars that notify the state of Card Reading process
int nCardInReadHopperMax = 0;     // size of input deck (= sim_card_inpput_hopper_count(uptr) when attached); 
int nCardInReadHopper = 0;        // number of cards left in read hopper (= sim_card_inpput_hopper_count(uptr)); 
int nCardInReadStacker = 0;       // number of cards in readed deck whose animation ended, this is not sim_card_output_hopper_count(uptr); 
uint32 tm0CardInReadStacker = 0;  // set to sim_os_msec by cdr_svr when card entering take hopper, as start of animation time origin
int bCardReadError = 0;           // simulated h/w error on reading card (no file atached, read host error)

UNIT                cdr_unit[1] = {
   {UDATA(cdr_srv, UNIT_CDR, 0), 300}   
};

REG cdr_reg[] = {
    {DRDATAD(CDR1RDHOPPER, nCardInReadHopper, 16, "Number of cards in CDR1 hopper pending to be read"), REG_FIT|REG_RO},
    {NULL}
};

MTAB                cdr_mod[] = {
    { OPTION_SKIPCOLS18, 0,                   "IGNORE_COL_72-80 Ignore columns 72-80 when reading punched card",   "IGNORE_COL_72-80", NULL },
    { OPTION_SKIPCOLS18, OPTION_SKIPCOLS18,   "SKIP_COL_1-8 Skip columns 1-8 when reading punched card",           "SKIP_COL_1-8", NULL },
    {MTAB_XTD | MTAB_VUN, 0, "FORMAT",    "FORMAT", &sim_card_set_fmt, &sim_card_show_fmt, NULL,     "Set card format"},
    {MTAB_XTD | MTAB_VUN, 0, "ECHOLEVEL", "ECHOLEVEL",  &cdr_set_echo,     &cdr_show_echo,     NULL, "Set console printout for punched cards"},
    {0}
};

DEVICE              cdr_dev = {
    "CDR", cdr_unit, cdr_reg, cdr_mod,
    1, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, NULL, &cdr_attach, &cdr_detach,
    &cdr_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cdr_help, NULL, NULL, &cdr_description
};

extern t_int64             MQ;                          // M-Q register

// read card image as seen by IBM 701
t_int64 cdr_CardImage[26]; 
int cdr_CardImage_index = 0; // next word to be read with copy instrction. if <0 -> should read a card
int cdr_EchoLevel = 0; // echo card read to console. 0=no echo, 1=only text, 2=text+octal, 3=text+octal+binary

// timing variables
t_int64 cdr_timing_StartTickCount  = 0; // value of GlobalTickCount when card read cycle starts
t_int64 cdr_timing_DisconTickCount = 0; // value of GlobalTickCount when card read disconnects if no data is provied with COPY opcode
extern int IOTicks;                     // ticks needed to execute i/o operation
extern int IOREMAIN;                    // cpu ticks ramaining to disconnect IO device

// buffer to hold read cards in take hopper (cards just read)
// to be printed by carddeck echolast command
// is a circular buffer of MAX_CARDS_IN_READ_STAKER_HOPPER cards
uint16 ReadStaker[MAX_CARDS_IN_READ_STAKER_HOPPER * 80];
int    ReadStakerLast; // index of last card read into circular buffer


int cdr_skip_cols (void)
{
    if ((uint32)(cdr_unit[0].flags & OPTION_SKIPCOLS18)) return 8; 
    return 0; 
}

/*
 * Device entry points for card reader.
 */
uint32 cdr_cmd(UNIT * uptr, uint16 cmd, uint16 addr)
{
    uint16 image[80];
    int i, n, SkipCols;
    int vmask, col, msec; 
    t_int64 w; 

    IOTicks = 0; // duration of operation in Ticks

    if (cmd==OP_STOP) {
        // discard card being read, if any. Disconect reader (will need a new READ
        // instruction to select again cdr)
        cdr_CardImage_index=-1; 
        return SCPE_OK;
    }

    bCardReadError = 0;

    /* Test ready */
    if ((uptr->flags & UNIT_ATT) == 0) {
        sim_debug(DEBUG_EXP, &cdr_dev, "No file attached to Card Read Unit\n");
        bCardReadError=1; 
        return SCPE_UNATT;
    }
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, &cdr_dev, "Card Read Unit disabled\n");
        return SCPE_UDIS;
    }   

    if (cmd==OP_READ) {
        // read a punched card from card reader
        cdr_CardImage_index=0;
        sim_debug(DEBUG_CMD, &cdr_dev, "READ CARD\n");
        switch(sim_read_card(uptr, image)) {
        case CDSE_EOF:
        case CDSE_EMPTY:
            sim_debug(DEBUG_EXP, &cdr_dev, "EOF\n");
            cdr_CardImage_index = 25; // signal EOF
            return SCPE_OK;
        case SCPE_UNATT:
            sim_debug(DEBUG_EXP, &cdr_dev, "Not Attached\n");
            bCardReadError=1; 
            return SCPE_UNATT;
        case CDSE_OK:
            break;
        default:
            sim_debug(DEBUG_EXP, &cdr_dev, "Card Read IO ERR\n");
            bCardReadError=1; 
            return STOP_IOERROR;
        }

        // data for cpanel animation
        nCardInReadHopper = sim_card_input_hopper_count(uptr);
        if (tm0CardInReadStacker==0) {
            tm0CardInReadStacker = sim_os_msec();  // this is the time stamp (in real word msec) when starts the animation of read-card entering card-read-stacker) 
        }

        // Timing calculation
        // calc msec = time for card reader to finish current in progress cycle
        // calc nTick = number to ticks READ needs to be executed by cpu
        // reader card cycle: 400 msec (150 cards per minute)
        if (cdr_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            cdr_timing_StartTickCount = 0;
        }
        if ((cdr_timing_StartTickCount == 0) || ((msec = msec_elapsed(cdr_timing_StartTickCount, GlobalTicksCount)) > 400)) {
            msec = -1; // card reader not in motion
        }
        if (msec == -1) {
            // card reader not in motion. Set start of motion as current tickcount
            cdr_timing_StartTickCount = GlobalTicksCount;
            // first 9-left copy must be given before 270 msec counting after start of motion
            cdr_timing_DisconTickCount = cdr_timing_StartTickCount + msec_to_ticks(270); 
            // time needed for READ to execute and start card reader motion: 220 msec
            IOTicks = msec_to_ticks(220); 
            sim_debug(DEBUG_CMD, &cdr_dev, "READ CARD: Start Card Motion\n");
        } else {
            // card reader in motion. Wait to finish current cycle 
            msec = 400-msec; // time remaining for current cycle to end
            IOTicks = msec_to_ticks(msec); 
            // card reader in motion. Next cycle is 400 msec after the previous one
            cdr_timing_StartTickCount +=msec_to_ticks(400); 
            // first 9-left copy must be given before 120 msec counting after start of cycle
            cdr_timing_DisconTickCount = cdr_timing_StartTickCount + msec_to_ticks(120); 
            // time needed for READ to execute in contonuous motion: 80 msec (educated guess)
            IOTicks += msec_to_ticks(80); 
            sim_debug(DEBUG_CMD, &cdr_dev, "READ CARD: Continuous Card Read (wait %d msec to start card cycle\n", msec);
        }
        // set the max time (in tick counts) to auto-disconnect device
        IOREMAIN = (int) (cdr_timing_DisconTickCount - GlobalTicksCount + 10); 
        
        // advance read circular buffer last card 
        ReadStakerLast = (ReadStakerLast + 1) % MAX_CARDS_IN_READ_STAKER_HOPPER;
        // save card in read card hopper buffer
        memcpy(&ReadStaker[ReadStakerLast * 80], image, sizeof(image));

        /* read the cards */

        // uint16 image[] array that holds the actual punched rows on card
        // using this codification:
        //
        //  Row Name    value in image[]    comments
        //
        //  Y     0x800               Hi Punch Y(12)
        //  X     0x400               Minus Punch X(11)
        //  0     0x200               also called T (Ten, 10)
        //  1     0x100
        //  2     0x080
        //  3     0x040
        //  4     0x020
        //  5     0x010
        //  6     0x008
        //  7     0x004
        //  8     0x002
        //  9     0x001
        //
        // If several columns are punched, the values are ORed: eg char A is represented as a punch 
        // on row Y and row 1, so it value in image array will be 0x800 | 0x100 -> 0x900

        // fill CardImage with words in binary card. cdr_CardImage contains the hole bitmap
        // of card. bit=1 -> hole. The map is as follows: 
        //                              ------------------------------------------------------+
        //                           /    | <-                 72 columns                  -> |
        //                           |    +-------------------------+-------------------------+
        //                           |    |    cdr_CardImage[22]    |    cdr_CardImage[23]    |
        //                           |    +-------------------------+-------------------------+
        //                           |    |                         |                         |
        //                           |    | <-    36 columns     -> |                         |
        //                           |    |                         |                         |
        //                           |    +-------------------------+-------------------------+
        //                           |    |    cdr_CardImage[4]     |           ...           |
        //                           |    +-------------------------+-------------------------+
        //                           |    |    cdr_CardImage[2]     |    cdr_CardImage[3]     |
        //                           |    +-------------------------+-------------------------+
        //                           |    |    cdr_CardImage[0]     |    cdr_CardImage[1]     |
        //                           +----+-------------------------+-------------------------+
        //                                   Left MQ  |  Right MQ       Left MQ  |  Right MQ   
        //
        //  cdr_CardImage[n] = MQ value from COPY instruction (36 bits)  
        //

        // fill in cdr_CardImage buffer with data read from card
        // buffer has 24 36-bits words 
        // each word will be returned in sequence with COPY intructions into MQ register
        // MQ has 72 bits, so can only read 72 columns
        //
        // normally columns 1-72 from punched card are read in MQ register. 
        // If OPTION_SKIPCOLS18 flag is set, MQ gets contentes of columns 9-80 (columns 1-8 are 
        // skipped)

        SkipCols = cdr_skip_cols();

        n=0;
        vmask=1; 
        while (vmask <= 0x800) {
            for (col=0;col<2;col++) {
                w=0;
                for (i=0;i<36;i++) {
                    if (image[SkipCols+i+col*36] & vmask) w = w | (1LL << (35-i));
                }
                cdr_CardImage[n++]=w; 
            }
            vmask = vmask << 1; 
        }

        echo_cardimage(&cdr_dev, image, cdr_EchoLevel, "Read Card", NULL);

    } else if (cmd==OP_COPY) {
        if (cdr_CardImage_index < 0) {
            sim_debug(DEBUG_CMD, &cdr_dev, "COPY issued without previous READ\n");
            return STOP_COPYCHECK;
        }
        if (cdr_CardImage_index == 25) {
            sim_debug(DEBUG_CMD, &cdr_dev, "Signal EOF (end of card deck)\n");
            cdr_CardImage_index = -1;    // ... and disconnect 
            return SCPE_EOF; // signal EOF (end of card deck)
        } else if (cdr_CardImage_index == 24) {
            sim_debug(DEBUG_CMD, &cdr_dev, "Signal End of Record (end of card)\n");
            cdr_CardImage_index = -1;   // ... and disconnect 
            return SCPE_RO; // signal End of record (end of card)
        } else {
            MQ = cdr_CardImage[cdr_CardImage_index++]; 
            sim_debug(DEBUG_CMD, &cdr_dev, "Read word %d from Card Reader \n", cdr_CardImage_index);
        }

        // Timing calculation
        // calc msec = time remaining for card reader to accept next data from COPY
        // calc nTick = number to ticks COPY needs to be executed by cpu
        if (cdr_timing_DisconTickCount == 0) {
            // =0 when exiting fast mode -> resync with global tick count
            cdr_timing_DisconTickCount = GlobalTicksCount; 
            cdr_timing_StartTickCount = 0;
        }

        msec = msec_elapsed(GlobalTicksCount, cdr_timing_DisconTickCount);
        if ((n = (int) (cdr_timing_DisconTickCount - GlobalTicksCount)) <0) {
            sim_debug(DEBUG_CMD, &cdr_dev, "COPY is %d Ticks (%d msec) too late. Card reader disconnected\n", -n, -msec);
            return STOP_COPYCHECK;
        } else {
            sim_debug(DEBUG_CMD, &cdr_dev, "COPY has been waiting for %d Ticks (%d msec)\n", n, msec);
        }
        // wait for card reader to be able to accept COPY data
        IOTicks = n; 
        if (cdr_CardImage_index & 1) {
            cdr_timing_DisconTickCount += usec_to_ticks(540); // right copy must be given within 540 microsec. 
        } else {
            cdr_timing_DisconTickCount += msec_to_ticks(15); // left copy must be given within 15 msec. 
        }
        cdr_timing_DisconTickCount += 20; // add 20 ticks as margin
        IOREMAIN = (int) (cdr_timing_DisconTickCount - GlobalTicksCount); 

    } else {
        sim_debug(DEBUG_CMD, &cdr_dev, "Unknown CDR command\n");
        return SCPE_IERR;
    }

    return SCPE_OK;
}

/* Handle transfer of data for card reader */

t_stat cdr_srv(UNIT *uptr) {

    // I/O is synchornous
    return SCPE_OK;
}

/* Set card punch echo to console */
t_stat cdr_set_echo (UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    t_stat              r;
    int                 num;

    if (uptr == NULL) return SCPE_IERR;
    if (cptr == NULL) {
        cdr_EchoLevel = 1;  // no param means set (=1)
    } else {
        num = (int) get_uint (cptr, 10, 3, &r);
        if (r != SCPE_OK) return r;
        cdr_EchoLevel=num; 
    }
    return SCPE_OK;
}

t_stat cdr_show_echo (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "ECHOLEVEL Set to %d (%s) ", cdr_EchoLevel, 
        (cdr_EchoLevel == 0) ? "No Echo" : (cdr_EchoLevel == 1) ? "Echo text" : 
        (cdr_EchoLevel == 2) ? "Echo text + octal" : "Echo text + octal + binary" 
        );
    return SCPE_OK;
}

t_stat cdr_attach(UNIT * uptr, CONST char *file)
{
    t_stat              r;

    if (uptr->flags & UNIT_ATT)         // remove current deck in read hopper before attaching
       sim_card_detach(uptr);           // the new one

    r = sim_card_attach(uptr, file);
    if (SCPE_BARE_STATUS(r) != SCPE_OK) return r;
    uptr->u5 = 0;
    uptr->u4 = 0;
    uptr->u6 = 0;

    // clear read card take hopper buffer 
    ReadStakerLast = 0;
    memset(ReadStaker, 0, sizeof(ReadStaker));

    // clear read card take hopper buffer 
    nCardInReadHopper = nCardInReadHopperMax = sim_card_input_hopper_count(uptr);
    nCardInReadStacker = sim_card_output_hopper_count(uptr);
    tm0CardInReadStacker = 0; // no animation

    cdr_CardImage_index = -1; 

    return SCPE_OK;
}

t_stat cdr_detach(UNIT * uptr)
{
    nCardInReadHopper = nCardInReadHopperMax = 0;
    nCardInReadStacker = 0;
    tm0CardInReadStacker = 0;

    cdr_CardImage_index = -1; 
    return sim_card_detach(uptr);
}


t_stat cdr_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", cdr_description(dptr));
   fprintf (st, "The IBM 711 Card Read \n");
   sim_card_attach_help(st, dptr, uptr, flag, cptr);
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * cdr_description(DEVICE *dptr)
{
   return "IBM 711 Card Read-Punch Unit";
}


