/* i650_mt.c: IBM 650 Magnetic tape 

   Copyright (c) 2018-2020, Roberto Sancho

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

   Magnetic tapes are represented as a series of variable records
   of the form:

        32b byte count
        byte 0
        byte 1
        :
        byte n-2
        byte n-1
        32b byte count

   If the byte count is odd, the record is padded with an extra byte
   of junk.  File marks are represented by a byte count of 0.


*/

#include "i650_defs.h"
#include "sim_tape.h"

#define UNIT_MT         UNIT_ATTABLE | UNIT_ROABLE | UNIT_DISABLE

// Tape MT0 is the one at IBM 650 address 8010, address MT1 is 8011 and so on

/* in u3 is tape medium length used on current position (unit = inches x 1000) */
/* in u4 is tape medium max length (in inches. 28800 for 2400 ft reel) */
/* in u5 holds the command being executed by tape unit */

extern uint8   StopIOError;             // flag to signal device that caused STOP_IO error: 1=CDR/CDP, 2=TAPE, 3=RAMAC Address, 4=RAMAC IO
extern int     CpuSpeed_Acceleration;   // cpu speed multiplier

/* Definitions */
uint32              mt_cmd(UNIT *, uint16, int *, uint16);
t_stat              mt_srv(UNIT *);
void                mt_ini(UNIT *, t_bool);
t_stat              mt_reset(DEVICE *);
t_stat              mt_attach(UNIT *, CONST char *);
t_stat              mt_detach(UNIT *);
t_stat              mt_rew(UNIT * uptr, int32 val, CONST char *cptr,void *desc);
t_stat              mt_set_len (UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat              mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc);
t_stat              mt_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr);
const char          *mt_description (DEVICE *dptr);

UNIT                mt_unit[6] = {
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 0 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 1 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 2 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 3 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 4 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 5 */
};

MTAB                mt_mod[] = {
    {MTUF_WLK,            0, "write enabled", "WRITEENABLED", NULL, NULL, NULL, "Write ring in place"},
    {MTUF_WLK,     MTUF_WLK, "write locked", "LOCKED",        NULL, NULL, NULL, "No write ring in place"},
    {MTAB_XTD | MTAB_VUN, 0, "FORMAT", "FORMAT",              &sim_tape_set_fmt, &sim_tape_show_fmt, NULL, 
                                                                                "Set/Display tape format (SIMH, E11, TPC, P7B)"},
    {MTAB_XTD | MTAB_VUN, 0, "LENGTH", "LENGTH",              &mt_set_len, &mt_show_len, NULL,
                                                                                "Set tape medium length (50 to 10000 foot)" },
    {MTAB_XTD | MTAB_VUN, 0, NULL,     "REWIND",              &mt_rew, NULL, NULL, "Rewind tape"},
    {0}
};

DEVICE              mt_dev = {
    "MT", mt_unit, NULL, mt_mod,
    6, 8, 15, 1, 8, 8,
    NULL, NULL, &mt_reset, NULL, &mt_attach, &mt_detach,
    &mt_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &mt_help, NULL, NULL, &mt_description
};

// IBM 652 Control Unit internal state
int LastTapeSelected  = -1;   // last tape selected. =0 to 5, -1=none yet
int LastTapeSignal    = 0;    // the signal senses with OP_NTS/OP_NEF opcode
int LastTapeCmd       = 0;    // last tape command executed
int bFastMode = 0;            // =1 for FAST operation

// extended info for tapes. Holds detailed info of tape command operation
mtinforec mt_info[6];

const char * TapeSignalStr[11] = { "OK",
                                   "WRITE PROTECTED", 
                                   "IO CHECK",
                                   "END OF FILE",
                                   "END OF TAPE",
                                   "LONG RECORD",
                                   "SHORT RECORD",
                                   "NO TAPE UNIT AT THIS ADDRESS",
                                   "NO REEL LOADED",
                                   "NOT READY", 
                                   "BAD CHAR"};
                                                             
// return 1 if tape unit n (0..5) is ready to receive a command
//        0                       is busy executing some command
//       -1                       cannot receive command
int mt_ready(int n)
{
    if ((n < 0) || (n > 5)) return -1; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return -1; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return -1; // not attached
    if (mt_unit[n].u5 & MT_RDY) return 1; // tape ready
    return 0; // tape busy
}

// set the ready flag on tape
void mt_set_ready(int n)
{
    if ((n < 0) || (n > 5)) return; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return; // not attached
    mt_unit[n].u5 |= MT_RDY; // set tape ready
}

// retrieve last cmd sent to tape. Negative if tape indicator set
int mt_get_last_cmd(int n)
{
    int cmd; 

    if ((n < 0) || (n > 5)) return 0; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return 0; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return 0; // not attached
    cmd=mt_unit[n].u5 & MT_CMDMSK;
    if (mt_unit[n].u5 & MT_IND) {
        // tape indicator on -> make cmd negative to signal it to caller
        cmd = -cmd;
    }
    return cmd; 
}

/* Rewind tape drive */
t_stat mt_rew(UNIT * uptr, int32 val, CONST char *cptr, void *desc)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = uptr - &mt_unit[0];

    /* If drive is offline or not attached return not ready */
    if ((uptr->flags & UNIT_ATT) == 0)
        return SCPE_UNATT;
    mt_info[unit].numrec = 0; // tape positioned at first record
    uptr->u3 = 0;      // tape at begin of medium
    uptr->u5 = MT_RDY; // clear indicator flag, clear last command, set ready flag
    return sim_tape_rewind(uptr);
}

int mt_read_numeric_word(uint8 * buf, t_int64 * d, int * ZeroNeg)
{
    int i, neg; 
    char c;

    neg = 0;
    *d = 0;
    if (ZeroNeg != NULL) *ZeroNeg = 0;
    for (i=0;i<10;i++) {
        c = *buf++;
        if (i==9) { // is last word digit 
            if ((c >= '0') && (c <= '9')) return MT_IND_BADCHAR;          // last digit should have sign
            if (c == '?') c = '0';                                        // +0
            if ((c >= 'A') && (c <= 'I')) c = c - 'A' + '1';              // +1 to +9
            if ((c >= 'J') && (c <= 'R')) {c = c - 'J' + '1'; neg=1;}     // -1 to -9
            if (c == '!') {c = '0'; neg=1;}                               // -0
        } 
        if ((c < '0') || (c > '9')) return MT_IND_BADCHAR;
        *d = *d * 10 + (c - '0');
    }
    if (neg) *d = -*d;
    if (ZeroNeg != NULL) *ZeroNeg = ((neg) && (*d == 0)) ? 1:0;
    return 0;
}

int mt_read_alpha_word(uint8 * buf, t_int64 * d)
{
    int i, n; 
    char c;

    *d = 0;
    for (i=0;i<5;i++) {
        c = *buf++;
        n = ascii_to_NN(c);
        if ((n==0) && (c != ' ')) return MT_IND_BADCHAR;
        *d = *d * 100 + n;
    }
    return 0;
}

int mt_transfer_tape_rec_to_IAS(uint8 * buf, t_mtrlnt reclen, char mode)
{
    int n,ic,r, ZeroNeg;
    t_int64 d, CtrlWord;
    char s[6];
    t_mtrlnt expected_reclen; 
    
    if (mode == 'N') {
        // numeric mode
        expected_reclen = (60 - IAS_TimingRing) * 10; // record len expected
        // does expected record len match read record from tape?
        if (expected_reclen != reclen) {
            return (reclen > expected_reclen) ? MT_IND_LONG_REC : MT_IND_SHORT_REC;
        }
        // yes, record length match -> load IAS with tape record data
        ic = 0;
        while (1) {
            // read numeric word from tape
            r = mt_read_numeric_word(&buf[ic], &d, &ZeroNeg);
            if (r) return r;
            ic += 10;
            // store into IAS
            IAS[IAS_TimingRing] = d;
            IAS_NegativeZeroFlag[IAS_TimingRing] = ZeroNeg;
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape to IAS %04d: %06d%04d%c '%s'\n", 
                        IAS_TimingRing+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            // incr IAS_TimingRing, exit if arrived to end of IAS
            IAS_TimingRing = (IAS_TimingRing + 1) % 60;
            if (IAS_TimingRing == 0) break;
        }
        return 0;
    }
    // alphabetic mode
    // check tape record size limits
    if (reclen < 10 + 9*5) return MT_IND_SHORT_REC;
    if (reclen > 10 + 9*10) return MT_IND_LONG_REC;
    ic = 0;
    while(1) {
        // get control word
        if (ic + 10 > (int)reclen) return MT_IND_SHORT_REC;
        r = mt_read_numeric_word(&buf[ic], &CtrlWord, NULL);
        if (r) return r;
        ic += 10;
        // store it in IAS[nnn9]
        n = (IAS_TimingRing / 10) * 10 + 9;
        IAS[n] = CtrlWord;
        IAS_NegativeZeroFlag[n] = 0;
        // load rest of words
        for (n=0;n<9;n++) {
            if ((CtrlWord % 10) != 8) {
                // read a numeric word form tape
                if (ic + 10 > (int)reclen) return MT_IND_SHORT_REC;
                r = mt_read_numeric_word(&buf[ic], &d, &ZeroNeg);
                if (r) return r;
                ic += 10;
            } else {
                // read alphanumeric word from tape
                if (ic + 5 > (int)reclen) return MT_IND_SHORT_REC;
                r = mt_read_alpha_word(&buf[ic], &d); ZeroNeg=0;
                if (r) return r;
                ic += 5;
            }
            CtrlWord = CtrlWord / 10;
            // store into IAS
            IAS[IAS_TimingRing] = d;
            IAS_NegativeZeroFlag[IAS_TimingRing] = ZeroNeg;
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape to IAS %04d: %06d%04d%c '%s'\n", 
                        IAS_TimingRing+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            // incr IAS_TimingRing, exit if arrived to end of IAS
            IAS_TimingRing = (IAS_TimingRing + 1) % 60;
            if (IAS_TimingRing == 0) return MT_IND_LONG_REC;
        }
        IAS_TimingRing = (IAS_TimingRing + 1) % 60; // skip control word
        if ((IAS_TimingRing == 0) && (ic != reclen)) return MT_IND_LONG_REC;
        if (ic == reclen) {
            if (IAS_TimingRing != 0) return MT_IND_SHORT_REC;
            break;
        }
    }
    return 0;
}

void mt_write_numeric_word(uint8 * buf, t_int64 d, int ZeroNeg)
{
    int i, neg; 
    char c;

    neg = 0;
    if (d < 0) {neg=1; d=-d;}
    if (ZeroNeg) neg=1;
    for (i=0;i<10;i++) {
        c = Shift_Digits(&d,1) + '0';
        if (i==9) {
            if (neg==0) { // last digit has sign
                if (c == '0') c = '?';                                        // +0
                if ((c >= '1') && (c <= '9')) c = c - '1' + 'A';              // +1 to +9
            } else {
                if ((c >= '1') && (c <= '9')) {c = c - '1' + 'J';}            // -1 to -9
                if (c == '0') {c = '!';}                                      // -0
            }
        } 
        *buf++ = c;
    }
}

void mt_write_alpha_word(uint8 * buf, t_int64 d)
{
    int i, n; 
    char c;

    for (i=0;i<5;i++) {
        n = Shift_Digits(&d,2);
        c = mem_to_ascii[n];
        *buf++ = c;
    }
}

void mt_transfer_IAS_to_tape_rec(uint8 * buf, t_mtrlnt * reclen, char mode)
{
    int n,ic,ZeroNeg;
    t_int64 d, CtrlWord;
    char s[6];

    if (mode == 'N') {
        // numeric mode
        ic = 0;
        while (1) {
            // read IAS
            d = IAS[IAS_TimingRing];
            ZeroNeg = IAS_NegativeZeroFlag[IAS_TimingRing];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IAS %04d to Tape: %06d%04d%c '%s'\n", 
                        IAS_TimingRing+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            // write numeric to tape buf
            mt_write_numeric_word(&buf[ic], d, ZeroNeg);
            ic += 10;
            // incr IAS_TimingRing, exit if arrived to end of IAS
            IAS_TimingRing = (IAS_TimingRing + 1) % 60;
            if (IAS_TimingRing == 0) break;
        }
        *reclen = (t_mtrlnt) ic;
        return;
    }
    // alphabetic mode
    ic = 0;
    while(1) {
        // get control word form IAS[nnn9]
        n = (IAS_TimingRing / 10) * 10 + 9;
        CtrlWord = IAS[n];
        // write control word in tape buf
        mt_write_numeric_word(&buf[ic], CtrlWord, 0);
        ic += 10;
        // write rest of words
        for (n=0;n<9;n++) {
            // read from IAS
            d = IAS[IAS_TimingRing];
            ZeroNeg = IAS_NegativeZeroFlag[IAS_TimingRing];
            if ((CtrlWord % 10) != 8) {
                // write a numeric word to tape buf
                mt_write_numeric_word(&buf[ic], d, ZeroNeg);
                ic += 10;
            } else {
                // write alphanumeric word to tape buf
                mt_write_alpha_word(&buf[ic], d); 
                ic += 5;
            }
            CtrlWord = CtrlWord / 10;
            // incr IAS_TimingRing, exit if arrived to end of IAS
            IAS_TimingRing = (IAS_TimingRing + 1) % 60;
            if (IAS_TimingRing == 0) break;
        }
        if (IAS_TimingRing == 0) break;
        IAS_TimingRing = (IAS_TimingRing + 1) % 60; // skip control word
        if (IAS_TimingRing == 0) break;
    }
    *reclen = (t_mtrlnt) ic;
}

/* Start off a mag tape command */
uint32 mt_cmd(UNIT * uptr, uint16 cmd, int * total_msec, uint16 fast)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = uptr - &mt_unit[0];
    int                 i, ic, time, total_msec2, u3;
    t_stat              r;   
    uint8               buf[1024];
    char                cbuf[100];
    t_mtrlnt            reclen  = 0;  // number of chars in tape record. (1 word = 10 chars)
    int32               recsize = 1; 

    *total_msec=0;   // time in msec for TCI (IL_Tape) interlock
    total_msec2 = 0; // extra time in msec to terminate command (without interlock) so tape become ready again
    
    /* Make sure valid drive number */
    if ((unit > 5) || (unit < 0)) return STOP_ADDR;
    // init IBM 652 Control Unit internal registers
    LastTapeSelected  = unit;  // select the tape. This will be show in ibm 652 panel
    LastTapeCmd = cmd;         // op on selected tape. This will be show in ibm 652 panel 
    LastTapeSignal = 0;        // init tape signal to be sensed by NTS/NEF opcode
    bFastMode = fast;
    /* If tape unit disabled return error */
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, dptr, "Tape %d: command %02d attempted on disabled tape\n", unit, cmd);
        LastTapeSignal = MT_IND_DIS;
        // not stated in manual: what happends if command to non existant tape?
        // what the simulator do: stops the cpu, lit the tape checking light on control unit.
        // Do not do uptr->u5 |= MT_IND to turn on indicator light on tape cabinet, as unit is disabled
        StopIOError = 2; // lit tape checking light on control unit
        return SCPE_OK;
    }
    /* If tape has no file attached return error */
    if ((uptr->flags & UNIT_ATT) == 0) {
        sim_debug(DEBUG_EXP, dptr, "Tape %d: command %02d attempted on tape without file attached\n", unit, cmd);
        LastTapeSignal = MT_IND_NOATT;
        StopIOError = 2;    // lit tape checking light on control unit, but do not stop prog execution (do not return STOP_IO)
        uptr->u5 |= MT_IND; // turn indicator light on tape cabinet to signal to operator the faulting tape
        return SCPE_OK;
    }
    uptr->u5 &= ~(MT_CMDMSK | MT_RDY | MT_IND); // remove last command sent to tape, ready flag, tape indicator. Do not change the just attached indicator
    uptr->u5 |= cmd; // set current command in execution
    switch (cmd) {
    case OP_RTC:
    case OP_BST:
    case OP_RTA:
    case OP_RTN:
        // actual simulated tape read
        reclen = 0;
        if (cmd == OP_BST) {
            sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init backstep record\n", unit);
            if (sim_tape_bot(uptr)) {
               sim_debug(DEBUG_EXP, dptr, "Tape unit %d: tape at BOT\n", unit);
               uptr->u3 = 0; // tape pos at beginning of medium
               uptr->u5 |= MT_RDY;
               mt_info[unit].numrec = 0; // tape positioned at first record
               return SCPE_OK;
            }
            r = sim_tape_sprecr(uptr, &reclen);
            recsize  = -(int32) ((reclen * 0.005 + 0.75) * 1000);            
        } else {
            sim_debug(DEBUG_DATA, dptr, "Tape unit %d: init read\n", unit);
            r = sim_tape_rdrecf(uptr, buf, &reclen, sizeof(buf));
            // each char uses 0,005 inches. at the end of record the IRG (inter gap record) uses 3/4 inchs (0.75)
            // tape medium has a lenght of 28800 inches
            // scaled x1000 to use integer values
            recsize = (int32) ((reclen * 0.005 + 0.75) * 1000);            
        }
        // process result conditions
        if (r == MTSE_TMK) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: tape mark sensed\n", unit);
            LastTapeSignal = MT_IND_EOF;
            uptr->u5 |= MT_IND; 
        } else if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: end of tape sensed\n", unit);
            StopIOError = 2; // lit tape checking light, but do not stop prog execution (do not return STOP_IO)
            LastTapeSignal = MT_IND_EOT;
            uptr->u5 |= MT_IND; 
        } else if ((r == MTSE_RECE) || (r == MTSE_INVRL)) {
            // record header contains error flag
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: longitudinal or vertical check error\n", unit);
            StopIOError = 2; // lit tape checking light
            LastTapeSignal = MT_IND_IOCHECK;
            uptr->u5 |= MT_IND; 
            // this is simulated read error, so signal is reported to cpu but execution
            // is not halted
        } else if (r != MTSE_OK) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: read error %d\n", unit, r);
            StopIOError = 2; // lit tape checking light
            LastTapeSignal = MT_IND_IOCHECK;
            uptr->u5 |= MT_RDY;
            return STOP_IO;
            // this is real host i/o error. If this error occurs, something is wrong 
            // on simulator level, so stop to allow user to notice
        } else {
            // tape read ok
            mt_info[unit].numrec += ((cmd == OP_BST) ? -1:1); // tape positioned at next record 
            uptr->u3 += recsize; // calc tape medium pos:
            // debug output: display buf as 50 chars per line
            sim_debug(DEBUG_DETAIL, dptr, "Read record (%d chars, used %0.2f inches) from tape:\n", 
                (int) reclen, recsize / 1000.0);
            ic = 0;
            while (1) {
                cbuf[50] = 0;
                for (i=0;i<50;i++) {
                    cbuf[i] = 0;
                    if (ic == reclen) break;
                    cbuf[i] = buf[ic++];
                }
                sim_debug(DEBUG_DETAIL, dptr, "... '%s'\n", cbuf);
                if (ic == reclen) break;
            }
            // calc wordcount time needed to finish tape operation
            // time for tape mark (1 char reclen) = 11 msec
            // time for 1 word tape record (10 digits = 10 chars) = 12 msec
            // time for 8 word tape record (80 chars) = 16 msec
            // time for 60 word tape record (600 chars) = 52 msec
            *total_msec = (int)(11 + reclen * 0.068); 
            // transfer read data to IAS if no tape signal
            if (((cmd == OP_RTA) || (cmd == OP_RTN)) && (LastTapeSignal == 0)) {            
                LastTapeSignal = mt_transfer_tape_rec_to_IAS(buf, reclen, (cmd == OP_RTN) ? 'N':'A');
                if (LastTapeSignal) {
                    sim_debug(DEBUG_EXP, dptr, "Tape unit %d: decode error %s\n", unit, 
                                               TapeSignalStr[LastTapeSignal]); 
                    uptr->u5 |= MT_IND; 
                }
            }
        }
        // set extended info. 
        mt_info[unit].recsize=recsize;
        mt_info[unit].cmd_msec=*total_msec;
        mt_info[unit].cmd_tm0=sim_os_msec();
        mt_info[unit].numrw++;
        break;
    case OP_WTM:
    case OP_WTA:
    case OP_WTN:
        reclen=1;
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init write\n", unit);
        if (cmd == OP_WTM) {
            r = sim_tape_wrtmk(uptr);
            // calc tape pos:
            uptr->u3 += (int32) ((1 * 0.005 + 0.75) * 1000); // Tape Mark is 1 word long
            
            sim_debug(DEBUG_DETAIL, dptr, "Write Tape Mark\n");
        } else {
            sim_debug(DEBUG_DETAIL, dptr, "IAS TimingRing is %d\n", IAS_TimingRing+9000);
            mt_transfer_IAS_to_tape_rec(buf, &reclen, (cmd == OP_WTN) ? 'N':'A');
            // actual simulated tape write
            r = sim_tape_wrrecf(uptr, buf, reclen);
            // calc tape pos:
            recsize = (int32) ((reclen * 0.005 + 0.75) * 1000);
            // debug output: display buf as 50 chars per line
            sim_debug(DEBUG_DETAIL, dptr, "Write record (%d chars, used %0.2f inches) to tape:\n", 
               (int) reclen, recsize / 1000.0);
            ic = 0;
            while (1) {
                cbuf[50] = 0;
                for (i=0;i<50;i++) {
                    cbuf[i] = 0;
                    if (ic == reclen) break;
                    cbuf[i] = buf[ic++];
                }
                sim_debug(DEBUG_DETAIL, dptr, "... '%s'\n", cbuf);
                if (ic == reclen) break;
            }
            sim_debug(DEBUG_DETAIL, dptr, "     IAS TimingRing is %d\n", IAS_TimingRing+9000);
        }
        // process result conditions
        if (r == MTSE_WRP) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: cannot write, tape read protected\n", unit);
            LastTapeSignal = MT_IND_WRT_PROT;
            uptr->u5 |= MT_IND; 
        } else if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: cannot write, tape ended\n", unit);
            LastTapeSignal = MT_IND_EOT;
            uptr->u5 |= MT_IND; 
        } else if (r != MTSE_OK) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: write error %d\n", unit, r);
            StopIOError = 2; // lit tape checking light
            uptr->u5 |= MT_RDY;
            return STOP_IO; 
            // this is real host i/o error. If this error occurs, something is wrong 
            // on simulator level, so stop to allow user to notice
        } else {
            // write ok
            mt_info[unit].numrec++; // tape positioned at next record 
            uptr->u3 += recsize;
        }
        // calc wordcount time needed
        *total_msec = (int)(11 + reclen * 0.068); // time to remove Tape Control interlock
        // set extended info. 
        mt_info[unit].recsize=recsize;
        mt_info[unit].cmd_msec=*total_msec;
        mt_info[unit].cmd_tm0=sim_os_msec();
        mt_info[unit].numrw++;
        break;
    case OP_RWD:
        /* Check if at load point, quick return if so */
        if (sim_tape_bot(uptr)) {
            sim_debug(DEBUG_CMD, dptr, "Tape unit %d: already at BOT\n", unit);
            uptr->u3 = 0;
            uptr->u5 |= MT_RDY;
            mt_info[unit].numrec = 0; // tape positioned at first record
            return SCPE_OK;
        }
        u3=uptr->u3; 
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init rewind (current medium used %0.2f inches)\n", 
                  unit, u3 / 1000.0);
        sim_tape_rewind(uptr);
        uptr->u3 = 0;
        mt_info[unit].numrec = 0; // tape positioned at first record
        // calculate rew duration
        // use a rought aprox. 
        total_msec2 = 10000 + u3 / (500 * 1000);  // rew at mean speed of 500 inch/sec
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: aprox rewind time (%d sec)\n", unit, total_msec2 / 1000);
        // set extended info. 
        mt_info[unit].recsize=u3;              // on rewind cmd, recsize holds the ammount of tape medium on L reel at begginning of command
        break;
    default:
        sim_debug(DEBUG_EXP, dptr, "Tape %d: unknown command %02d\n", unit, cmd);
        // should never occurs. just to catch it if so.
    }
    if ((CpuSpeed_Acceleration<0) && (cmd == OP_RWD)) {
       // Rewing operation while ^F pressed (CpuSpeed_Acceleration == -1) ...
       // do not exit with operation done as other tape operation does when ^F
        sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d rewind (^F mode), used %4.2f%% of medium (%0.2f inches)\n", 
            unit,
            (uptr->u3 / (uptr->u4*1000.0))*100.0, 
            uptr->u3 / 1000.0
            );
    } else if ((bFastMode) || (CpuSpeed_Acceleration<0)) {
        // fast mode or ^F -> unit does not need time to execute coomand so set unit ready to accept new commands
        // set cpu speed=max does not go here 
        uptr->u5 |= MT_RDY;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d done (fast mode), used %4.2f%% of medium (%0.2f inches)\n", 
            unit,
            (uptr->u3 / (uptr->u4*1000.0))*100.0, 
            uptr->u3 / 1000.0
            );
        return SCPE_OK; 
    }
    if(*total_msec < 35) *total_msec = 35;   // 35 msec to remove Tape Control interlock
    time = msec_to_wordtime(*total_msec + total_msec2);
    sim_cancel(uptr);
    // if justattached = 0 means that magtape refresh is being executed, so
    // a rewind animation can be done. 
    if ( (cmd == OP_RWD) && (mt_info[unit].justattached == 0)) {
        // do not issue sim_activate to schedulle rew termination
        // rew termination will be schedulled by magtape refresh when animation ends
        // this is the only case cpanel controlling the device operation
    } else {
        // schedulle tape comand termination
        sim_activate(uptr, time);
    }
    sim_debug(DEBUG_DETAIL, dptr, "... Tape %d command will take %d msec, %d WordTimes\n", 
                                   unit, *total_msec + total_msec2, time); 
    return SCPE_OK_INPROGRESS;
}

/* Handle processing of tape requests. */
t_stat mt_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 cmd = uptr->u5 & MT_CMDMSK;

    switch (cmd) {
    case OP_RTC:
    case OP_BST:
    case OP_RTA:
    case OP_RTN:
    case OP_WTM:
    case OP_WTA:
    case OP_WTN:
        // command finished
        goto tape_done;
        break;
    case OP_RWD:
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: done rewind\n", unit);
        goto tape_done;
        break;
    default: 
        return SCPE_ARG; // should never occurs. just to catch it if so.
    tape_done:
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: ready\n", unit);
        sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d done, used %4.2f%% of medium (%0.2f inches)\n", 
            unit,
            (uptr->u3 / (uptr->u4*1000.0))*100.0, 
            uptr->u3 / 1000.0
            );
        // set unit ready to accept new commands
        uptr->u5 |= MT_RDY;
        break;
    }
    return SCPE_OK;
}

void mt_ini(UNIT * uptr, t_bool f)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);

    if (uptr->flags & UNIT_ATT) {
        uptr->u5 = MT_RDY;
    } else {
        uptr->u5 = 0;
    }
    uptr->u3 = 0;
    if (uptr->u4 == 0) uptr->u4 = 28800; // default 2400 ft reel; 1 foot = 12 inches; 2400 ft = 28800 inches
    mt_info[unit].numrec = mt_info[unit].numrw = 0;
} 

t_stat mt_reset(DEVICE * dptr)
{
    int i;
    for (i = 0; i < 6; i++) {
        mt_ini(&mt_unit[i], 0);
    }
    LastTapeSignal=0;
    LastTapeSelected=-1;
    return SCPE_OK;
}

t_stat mt_attach(UNIT * uptr, CONST char *file)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    t_stat              r;

    if ((r = sim_tape_attach(uptr, file)) != SCPE_OK)
        return r;
    uptr->u3 = 0;
    uptr->u5 = MT_RDY ;
    mt_info[unit].justattached = 1; 
    mt_info[unit].numrw = 0;
    mt_info[unit].numrec = 0;
    return SCPE_OK;
}

t_stat mt_detach(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);

    uptr->u3 = 0;
    uptr->u5 = 0;
    mt_info[unit].numrec = 0;
    sim_cancel(uptr); // cancel any pending command
    return sim_tape_detach(uptr);
}

/* Set tape length */

t_stat mt_set_len (UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int len;
    t_stat r;

    if ((cptr == NULL) || (*cptr == 0))  return SCPE_ARG;
    len = (int) get_uint (cptr, 10, 10000, &r);
    if (r != SCPE_OK) return SCPE_ARG;
    if (len < 50) return SCPE_ARG;
    uptr->u4 = 28800 * len / 2400;
    return SCPE_OK;
}

/* Show tape length */

t_stat mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "length %d foot", uptr->u4 * 2400 / 28800);
    return SCPE_OK;
}


t_stat mt_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", mt_description(dptr));
   fprintf (st, "The magnetic tape assumes that all tapes are 7 track\n");
   fprintf (st, "with valid parity. Tapes are assumed to be 200 characters per\n");
   fprintf (st, "inch. \n\n");
   sim_tape_attach_help (st, dptr, uptr, flag, cptr);
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * mt_description(DEVICE *dptr)
{
   return "IBM 727 Magnetic Tape Unit";
}


