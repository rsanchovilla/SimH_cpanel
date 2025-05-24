/* i701_mt.c: IBM 701 Magnetic Tape.

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

#include "i701_defs.h"
#include "sim_tape.h"

#define UNIT_MT         UNIT_ATTABLE | UNIT_ROABLE | UNIT_DISABLE

// Tape MT0 is the one at IBM 701 address 256, address MT1 is 257 and so on

/* in u3 is tape medium length used on current position (unit = inches x 1000) */
/* in u4 is tape medium max length (in inches. 14400 for 1200 ft reel) */
/* in u5 holds the command being executed by tape unit */

// tape density: 100 chars of 6-bit each per inch, each fullword needs 6 chars
// tape length is 1200 feet -> 14400 inch -> 14400 * 100 -> 1.44 Mega chars
// 1.44M / 6 = 240K fullwords is the max tape capacity, and the max record size  
#define TAPE_MAX_REC_LEN   1440000             // chars
#define TAPE_EOR        (1LL << 41)            // marks in tape readed buffer to signal
#define TAPE_EOF        (1LL << 42)            // end of record or end of file

/* Definitions */
uint32              mt_cmd(UNIT *, uint16, uint16);
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

UNIT                mt_unit[4] = {
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 0 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 1 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 2 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 3 */
};

MTAB                mt_mod[] = {
    {MTUF_WLK,            0, "write enabled", "WRITEENABLED", NULL, NULL, NULL, "Write ring in place"},
    {MTUF_WLK,     MTUF_WLK, "write locked", "LOCKED",        NULL, NULL, NULL, "No write ring in place"},
    {MTAB_XTD | MTAB_VUN, 0, "FORMAT", "FORMAT",              &sim_tape_set_fmt, &sim_tape_show_fmt, NULL, 
                                                                                "Set/Display tape format (SIMH, E11, TPC, P7B)"},
    {MTAB_XTD | MTAB_VUN, 0, "LENGTH", "LENGTH",              &mt_set_len, &mt_show_len, NULL,
                                                                                "Set tape medium length (50 to 10000 feet)" },
    {MTAB_XTD | MTAB_VUN, 0, NULL,     "REWIND",              &mt_rew, NULL, NULL, "Rewind tape"},
    {0}
};

DEVICE              mt_dev = {
    "MT", mt_unit, NULL, mt_mod,
    4, 8, 15, 1, 8, 8,
    NULL, NULL, &mt_reset, NULL, &mt_attach, &mt_detach,
    &mt_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &mt_help, NULL, NULL, &mt_description
};

extern t_int64             MQ;                          // M-Q register
extern int IOTicks;                     // ticks needed to execute i/o operation
extern int IOREMAIN;                    // cpu ticks ramaining to disconnect IO device

// extended info for tapes. Holds detailed info of tape command operation
mtinforec mt_info[4];

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

// retrieve last cmd sent to tape. if n=10..13 also clear last cmd
int mt_get_last_cmd(int n)
{
    int cmd; 

    if (n>=10) {
        n=n-10; 
        cmd=mt_unit[n].u5 & MT_CMDMSK;
        mt_unit[n].u5 &=  ~MT_CMDMSK;
        return cmd; 
    }

    if ((n < 0) || (n > 5)) return 0; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return 0; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return 0; // not attached
    cmd=mt_unit[n].u5 & MT_CMDMSK;
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
    uptr->u5 = MT_RDY; // clear last command, set ready flag
    return sim_tape_rewind(uptr);
}


void Set_TapeRecord_buf(int unit, int n, t_int64 d)
{
    int i, c; 

    if (mt_info[unit].TapeRecord_buf==NULL) return;   // safety
    if ((n+1)*6 >= TAPE_MAX_REC_LEN) return;          // safety
    if (n<0) return;                                  // safety
    // store 36 bit value on d as 6 chars of 6 bits
    n=n*6; 
    for (i=0; i<6; i++) {
        if (d==TAPE_EOF) c = 129; else 
        if (d==TAPE_EOR) c = 128; else 
        c = d & 077;
        mt_info[unit].TapeRecord_buf[n++] = c; 
        d=d>>6; 
    }
}

t_int64 Get_TapeRecord_buf(int unit, int n)
{
    int i, c; 
    t_int64 d; 

    if (mt_info[unit].TapeRecord_buf==NULL) return TAPE_EOF;   // safety
    if ((n+1)*6 >= TAPE_MAX_REC_LEN) return TAPE_EOF;          // safety
    if (n<0) return TAPE_EOF;                                  // safety
    // read 6 chars of 6 bits, compose a 36 bits value to return
    n=n*6; d=0;
    for (i=0; i<6; i++) {
        c = mt_info[unit].TapeRecord_buf[n++]; 
        if (c==129) return TAPE_EOF;
        if (c==128) return TAPE_EOR;
        d |= ((t_int64) (c & 077)) << (i*6); 
    }
    return d; 
}

t_stat ReadTapeRecord(UNIT * uptr, int cmd, int unit)
{
    t_mtrlnt reclen; 
    t_int64 d; 
    int recsize, n; 
    t_stat r; 

    if (mt_info[unit].TapeRecord_buf==NULL) return SCPE_IERR;

    // actual simulated tape read
    reclen = 0;
    if (cmd == OP_READ_B) {
        sim_debug(DEBUG_CMD, &mt_dev, "Tape %d: init backward read\n", unit);
      rdbck:
        if (sim_tape_bot(uptr)) {
           sim_debug(DEBUG_EXP, &mt_dev, "Tape unit %d: tape at BOT\n", unit);
           uptr->u3 = 0; // tape pos at beginning of medium
           mt_info[unit].numrec = 0; // tape positioned at first record
           mt_info[unit].TapeRecord_index = 0; 
           Set_TapeRecord_buf(unit, 0, TAPE_EOF);
           return SCPE_OK;
        }
        // on IBM 726, only ONE End-Of_file is allowd. It works as it was a software end of medium.
        // when reading backwards, EOF is skipped, and BOT is interpreted as EOF
        r = sim_tape_rdrecr(uptr, mt_info[unit].TapeRecord_buf, &reclen, TAPE_MAX_REC_LEN-1);
        if (r == MTSE_TMK) {
            sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: end of file mark sensed, but skipped\n", unit);
            goto rdbck; 
        }
        // reverse order of words read backward
        n=reclen / 6; // reclen is num of chars in tape record, n is number of words
        if (n>1) {
            int i; 
            t_int64 d1, d2; 
            // swap words to reverse order because record has been read backwards
            for(i=0;i<n/2;i++) {
                d1=Get_TapeRecord_buf(unit, i);
                d2=Get_TapeRecord_buf(unit, n-i-1);
                Set_TapeRecord_buf(unit, i, d2);
                Set_TapeRecord_buf(unit, n-i-1, d1);
            } 
        }
    } else {
        sim_debug(DEBUG_CMD, &mt_dev, "Tape %d: init read\n", unit);
        r = sim_tape_rdrecf(uptr, mt_info[unit].TapeRecord_buf, &reclen, TAPE_MAX_REC_LEN-1);
    }
    // density is 100 chars per inch -> each char needs 0.01 inches
    // IRG (inter gap record) is 1 inch
    // scaled x1000 to use integer values
    recsize = (int32) ((reclen * 0.01 + 1) * 1000);            
    // process result conditions
    if (r == MTSE_TMK) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: end of file mark sensed\n", unit);
        Set_TapeRecord_buf(unit, 0, TAPE_EOF);
        r=SCPE_OK; // do not report error, execution will continue. EOF will be evaluated in COPY instr
    } else if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: end of tape sensed\n", unit);
        Set_TapeRecord_buf(unit, 0, TAPE_EOF);
        r=SCPE_OK; // do not report error, execution will continue. EOF will be evaluated in COPY instr
    } else if ((r == MTSE_RECE) || (r == MTSE_INVRL)) {
        // record header contains error flag
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: parity check error\n", unit);
        // this is simulated read error, tape check light is lit, 
        // and cpu is halted
        return STOP_TAPECHECK; 
    } else if (r != MTSE_OK) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: read error %d\n", unit, r);
        return STOP_IOERROR;
        // this is real host i/o error. If this error occurs, something is wrong 
        // on simulator level, so stop to allow user to notice
    } else {
        // tape read ok
        mt_info[unit].numrec += ((cmd == OP_READ_B) ? -1:1); // tape positioned at next record 
        // set end of record 
        Set_TapeRecord_buf(unit, reclen / 6, TAPE_EOR);
        // debug output: display buf as 4 full words per line
        sim_debug(DEBUG_DETAIL, &mt_dev, "Read record (%d chars, %d fullwords, used %0.2f inches) from tape (octal):\n", 
                (int) reclen, reclen / 6, recsize / 1000.0);
        n=0;
        while(1) {
            d=Get_TapeRecord_buf(unit, n);
            if ((d==TAPE_EOR) || (d==TAPE_EOF)) break; 
            if ((n % 4) == 0) {
                if (n) sim_debug(DEBUG_DETAIL, &mt_dev, "\n");
                sim_debug(DEBUG_DETAIL, &mt_dev, "%3d: ", n+1);
            }
            sim_debug(DEBUG_DETAIL, &mt_dev, "%06o %06o   ", printfw_oct(d));
            n++;
        }

        sim_debug(DEBUG_DETAIL, &mt_dev, "\n");
        // calc tickcount time needed to read the whole record from tape 
        // time for start-stop-start time is 15 msec
        // density: 100 chars per in, speed is 75 inch per sec -> 7500 chars per sec 
        // -> 7.5 chars per msec 
        // each char hast 6 bits -> one 36 bits word need 6 chars -> 7500/6=1250 words per sec
        // -> 1.25 words per msec -> 0,8 msec per word
    }
    return r;
}

t_stat WriteTapeRecord(UNIT * uptr, int cmd, int unit)
{
    t_mtrlnt reclen; 
    t_int64 d; 
    int recsize, n; 
    t_stat r; 

    if (cmd==OP_WRITE_EF) {
        reclen=recsize=1; 
        r = sim_tape_wrtmk(uptr);
        sim_debug(DEBUG_DETAIL, &mt_dev, "Write end of file mark\n");
    } else {
        reclen=mt_info[unit].TapeRecord_index * 6; 
        // actual simulated tape write
        r = sim_tape_wrrecf(uptr, mt_info[unit].TapeRecord_buf, reclen);
        // calc tape pos:
        recsize = (int) ((reclen * 0.01 + 1) * 1000);   
        // debug output: display buf as 4 full words per line
        sim_debug(DEBUG_DETAIL, &mt_dev, "Write record (%d chars, %d fullwords, used %0.2f inches) to tape (octal):\n", 
                (int) reclen, reclen / 6, recsize / 1000.0);
        for(n=0;n<(int) (reclen/6);n++) {
            d=Get_TapeRecord_buf(unit, n);
            if ((d==TAPE_EOR) || (d==TAPE_EOF)) break; 
            if ((n % 4) == 0) {
                if (n) sim_debug(DEBUG_DETAIL, &mt_dev, "\n");
                sim_debug(DEBUG_DETAIL, &mt_dev, "%3d: ", n+1);
            }
            sim_debug(DEBUG_DETAIL, &mt_dev, "%06o %06o   ", printfw_oct(d));
        }
        sim_debug(DEBUG_DETAIL, &mt_dev, "\n");
    }
    // process result conditions
    if (r == MTSE_WRP) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: cannot write, tape read protected\n", unit);
        return STOP_TAPECHECK;
    } else if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: cannot write, tape ended\n", unit);
        return STOP_TAPECHECK;
    } else if (r != MTSE_OK) {
        sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: write error %d\n", unit, r);
        return STOP_IOERROR; 
        // this is real host i/o error. If this error occurs, something is wrong 
        // on simulator level, so stop to allow user to notice
    } else {
        // write ok
        mt_info[unit].numrec++; // tape positioned at next record 
    }
    return r;
}


/* Start off a mag tape command */
uint32 mt_cmd(UNIT * uptr, uint16 cmd, uint16 fast)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = uptr - &mt_unit[0];
    int                 ioop, n;
    t_stat              r;   
    t_int64             d; 
    int                 cmd_usec_duration, cmd_usec_ready, recsize, cmd2; 
    int32               u3; 

    IOTicks = 0; // duration of operation in Ticks
    cmd_usec_duration = cmd_usec_ready = recsize = 0; 
    r = SCPE_OK;

    /* Make sure valid drive number */
    if ((unit > 4) || (unit < 0)) return SCPE_IERR;

    if (cmd==OP_STOP) {
        // if writing, flush current in-progress tape record, if any. Then Disconect tape 
        if ((mt_info[unit].TapeStatus == OP_WRITE) && (mt_info[unit].TapeRecord_index > 0)) {
            sim_debug(DEBUG_CMD, &mt_dev, "Disconnecting ... flush tape record being written \n");
            r=WriteTapeRecord(uptr, 0, unit); 
        }
        mt_info[unit].TapeRecord_index=-1; 
        mt_info[unit].TapeStatus = 0;       // ... and set tape status to neutral 
        return r;
    }

    uptr->u5 |= MT_RDY; // set ready. Only command that clears MT_RDY id OP_REWIND

    /* If tape unit disabled return error */
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, dptr, "Tape %d: opcode %02d attempted on disabled tape\n", unit, cmd);
        return STOP_TAPECHECK;
    }
    /* If tape has no file attached return error */
    if ((uptr->flags & UNIT_ATT) == 0) {
        if (cmd==OP_REWIND) {
            // rewind a non attached tape -> ignore command
            return SCPE_OK;
        }
        sim_debug(DEBUG_EXP, dptr, "Tape %d: opcode %02d attempted on tape without file attached\n", unit, cmd);
        return STOP_TAPECHECK;
    }

    if (mt_info[unit].TapeRecord_buf==NULL) return SCPE_IERR;

    if (cmd==OP_REWIND) {
        // Check if at load point, quick return if so 
        if (sim_tape_bot(uptr)) {
            sim_debug(DEBUG_CMD, dptr, "Tape unit %d: already at BOT\n", unit);
            uptr->u3 = 0;
            mt_info[unit].numrec = 0; // tape positioned at first record
            return SCPE_OK;
        }
        u3=uptr->u3; 
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init rewind (current medium used %0.2f inches)\n", 
                  unit, u3 / 1000.0);
        sim_tape_rewind(uptr);
        // set extended info. 
        uptr->u3 = 0;
        mt_info[unit].numrec = 0;              // tape positioned at first record
        mt_info[unit].recsize=u3;              // on rewind cmd, recsize holds the ammount of tape medium on L reel at begginning of command
        if ((mt_info[unit].justattached == 1) || (FAST) || (CpuSpeed_Acceleration == 0)) {
            // justattached = 1 means that magtape refresh not yet exec so no a rewind animation can be done
            // FAST = 1 means "set cpu fast" scp command issued to run as fast host can, so no rew animation
            // CpuSpeed_Acceleration = 0 means sey cpu speed=max, so no rew animation
            uptr->u5 |= MT_RDY;
        } else {
            // clear ready flag to enable rew animation. next tape command will stall the CPU
            // if ^F pressed (signaled with CpuSpeed_Acceleration == -1) DO the rew animation, 
            // into the rew aninamtion step processing, ^F will be sensed again
           uptr->u5 &= ~MT_RDY;                
        }
        cmd_usec_duration = 50 * 1000;         // duration of command execution (in microsecond)
        cmd_usec_ready    = 50 * 1000;         // time in usec for tape to be ready for a new command
        mt_info[unit].cmd_tm0=0;               // signal no tape animation for regular /w 
    } else if (cmd==OP_WRITE_EF) {
        // prepare for write an end of file mark
        // if tape in read status, this oreation is NOOP. Next COPY will fail
        if (mt_info[unit].TapeStatus == OP_READ) {
            sim_debug(DEBUG_EXP, dptr, "Tape %d: WARNING: WRITE_EF instruction issued, but Tape in READ status. WRITE_EF is ignored \n", unit);
            return SCPE_OK; 
        }
        if (uptr->u3 > uptr->u4*1000) {
            sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: cannot write, tape ended\n", unit);
            r=STOP_TAPECHECK;
        } else {
            r=WriteTapeRecord(uptr, OP_WRITE_EF, unit);     // write enf of file mark
        }
        mt_info[unit].TapeRecord_index=-1;  // ... disconnect from tape
        mt_info[unit].TapeStatus = 0;       // ... and set tape status to neutral 
        cmd_usec_duration = 30 * 1000;      // duration of command execution (in microsecond)
        cmd_usec_ready    = 30 * 1000;      // time in usec for tape to be ready for a new command (time needed for tape medium to stop)
        recsize = 2 * 1000;                 // Tape End of file GAP is 2 inches long
        uptr->u3 += recsize;                // set u3 = inches x1000 tape unwind
    } else if (cmd==OP_WRITE) {
        // prepare for write a new record
        // if tape in read status, this oreation is NOOP. Next COPY will fail
        if (mt_info[unit].TapeStatus == OP_READ) {
            sim_debug(DEBUG_EXP, dptr, "Tape %d: WARNING: WRITE instruction issued, but Tape in READ status. Next COPY will fail \n", unit);
            return SCPE_OK; 
        }
        if (uptr->u3 > uptr->u4*1000) {
            sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: cannot write, tape ended\n", unit);
            r=STOP_TAPECHECK;
        } 
        // if tape in neutral status set it in write status
        mt_info[unit].TapeStatus = OP_WRITE;
        // start a new fresh tape buf
        mt_info[unit].TapeRecord_index = 0; 
        cmd_usec_duration = 8 * 1000;       // duration of command execution (in microsecond)
        cmd_usec_ready    = 15 * 1000;      // time in usec for tape to be ready for a new command
        recsize = 1 * 1000;                 // IRG (inter record gap) is 1 inch long
        uptr->u3 += recsize; 
        // set extended info
        mt_info[unit].numrw++;
    } else if ((cmd==OP_READ) || (cmd==OP_READ_B)) {
        // prepare for read a new record
        // if tape in write status, this oreation is NOOP. Next COPY will fail
        if (mt_info[unit].TapeStatus == OP_WRITE) {
            sim_debug(DEBUG_EXP, dptr, "Tape %d: WARNING: READ instruction issued, but Tape in WRITE status. Next COPY will fail \n", unit);
            return SCPE_OK; 
        }
        // if tape in neutral status set it in read status
        mt_info[unit].TapeStatus = OP_READ;
        if (uptr->u3 > uptr->u4*1000) {
            sim_debug(DEBUG_EXP, &mt_dev, "Tape %d: cannot read, tape ended\n", unit);
            r=STOP_TAPECHECK;
        } else {
            r=ReadTapeRecord(uptr, cmd, unit); 
        }
        // position index on start of read tape buf
        mt_info[unit].TapeRecord_index = 0; 
        cmd_usec_duration = 6 * 1000;       // duration of command execution (in microsecond)
        cmd_usec_ready    = 15 * 1000;      // time in usec for tape to be ready for a new command
        recsize = 1 * 1000;                 // IRG (inter record gap) is 1 inch long
        uptr->u3 += recsize; 
        // set extended info
        mt_info[unit].numrw++;
    } else if (cmd==OP_COPY) {
        if (mt_info[unit].TapeRecord_index >= TAPE_MAX_REC_LEN) return sim_messagef(SCPE_IERR, "Tape record buffer overflow \n");
        if (mt_info[unit].TapeRecord_index < 0) {
            sim_debug(DEBUG_CMD, dptr, "COPY issued without previous READ/WRITE\n");
            return STOP_COPYCHECK;
        }
        // tape timing: transfer rate: 1250 words per sec -> 800 msec per word
        // 700 msec of useful calculating time between COPYs -> 100 msec of time for COPY execution
        cmd_usec_duration = 100;           // duration of COPY execution (in microsecond)
        cmd_usec_ready    = 800;           // time in usec for tape to be ready for a COPY command
        recsize = (int) (0.06 * 1000);     // tape density: 100 chars of 6-bit each per inch, each fullword needs 6 chars -> 1 word needs 0.06 inch
        GetIOADDR(NULL, &ioop); 
        if (ioop == OP_WRITE) {
            if (mt_info[unit].TapeStatus != OP_WRITE) {
                sim_debug(DEBUG_EXP, dptr, "Tape %d: Tape Status is not WRITE\n", unit);
                return STOP_COPYCHECK; 
            }
            n=mt_info[unit].TapeRecord_index++;
            Set_TapeRecord_buf(unit, n, MQ); 
            sim_debug(DEBUG_CMD, &mt_dev, "Write record word %d to Tape %d \n", n+1, unit);
            // density is 100 chars/inch -> 0.01 inches per char
            // one word is 6 chars. u3 has value in inches x1000
            uptr->u3 += (int) (6 * 0.01 * 1000);   
        } else if ((ioop == OP_READ) || (ioop == OP_READ_B)) {
            if (mt_info[unit].TapeStatus != OP_READ) {
                sim_debug(DEBUG_EXP, dptr, "Tape %d: Tape Status is not READ\n", unit);
                return STOP_COPYCHECK; 
            }
            n=mt_info[unit].TapeRecord_index++;
            d=Get_TapeRecord_buf(unit, n);
            if (d == TAPE_EOF) {
                sim_debug(DEBUG_CMD, &cdr_dev, "Signal EOF (end of Tape %d file)\n", unit);
                mt_info[unit].TapeRecord_index=-1;  // ... and disconnect 
                mt_info[unit].TapeStatus = 0;       // ... and set tape status to neutral 
                return SCPE_EOF; // signal EOF (end of card deck)
            } else if (d == TAPE_EOR) {
                sim_debug(DEBUG_CMD, &cdr_dev, "Signal end of record (Tape %d)\n", unit);
                mt_info[unit].TapeRecord_index=-1;  // ... and disconnect 
                mt_info[unit].TapeStatus = 0;       // ... and set tape status to neutral 
                return SCPE_RO; // signal End of record (end of card)
            } else {
                MQ=d; 
                sim_debug(DEBUG_CMD, &mt_dev, "Read record word %d from Tape %d \n", n+1, unit);
            }
            uptr->u3 += (int) (6 * 0.01 * 1000 * ((ioop == OP_READ_B) ? -1:1));   
        } else {
            sim_debug(DEBUG_EXP, dptr, "Tape %d: Not READ/WRITE instruction issued before COPY\n", unit);
            return STOP_COPYCHECK; 
        }
    }

    // Timing calculation
    // calc msec = time remaining for tape to accept command
    // calc nTick = number to ticks command needs to be executed by cpu
    if ((FAST) || (CpuSpeed_Acceleration<=0) ||
        (mt_info[unit].ReadyTickCount == 0) || (GlobalTicksCount > mt_info[unit].ReadyTickCount)) {
        // =0 when exiting fast mode or tape disconnected -> resync with global tick count
        mt_info[unit].ReadyTickCount = GlobalTicksCount; 
        IOTicks = 0; 
    } else {
        IOTicks = (int) (mt_info[unit].ReadyTickCount - GlobalTicksCount);

        sim_debug(DEBUG_CMD, &mt_dev, "Tape command has been waiting for %d Ticks\n", IOTicks);
    }
    // next command must be given within this time. If given earlier, cpu waits
    // if given later, tape disconnects
    mt_info[unit].ReadyTickCount += usec_to_ticks(cmd_usec_ready);  
    IOTicks += usec_to_ticks(cmd_usec_duration); 
    IOREMAIN = (int) (mt_info[unit].ReadyTickCount - GlobalTicksCount + 10); 

    // set extended info
    uptr->u5 &=  ~MT_CMDMSK;
    if (cmd==OP_COPY) {
        cmd2 = ioop; // set current command in execution
    } else {
        cmd2 = cmd; // set current command in execution
    }
    uptr->u5 |= cmd2; 

    // set info for tape animation
    if ((cmd2==OP_WRITE_EF) || (cmd2==OP_WRITE) || (cmd2==OP_READ) || (cmd2==OP_READ_B)) { 
       if (cmd2==OP_READ_B) {
           recsize=-recsize;
       }
       if ( (mt_info[unit].cmd_tm0 == 0) || 
           ((mt_info[unit].cmd_tm0 != 0) && (cmd2==OP_READ_B) && (mt_info[unit].recsize > 0)) ||
           ((mt_info[unit].cmd_tm0 != 0) && (cmd2!=OP_READ_B) && (mt_info[unit].recsize < 0)) ) {
           // if tape medium not in motion ... or ...
           //    read backward but tape motion is in forward direction ... or ...
           //    read/write forward but tape motion is in backward direction ... then 
           // start new tape medium movement animation
           mt_info[unit].recsize=recsize;          // amount of tape medium advancing for cpanel animation
           mt_info[unit].cmd_usec=cmd_usec_ready;  // time needed to advance tape medium
           mt_info[unit].cmd_tm0=1;                // signal new data for tape animation
       } else {
           // tape medium is in motion, and continue motion on same direction ...
           mt_info[unit].recsize+=recsize;         // amount of tape medium advancing for cpanel animation
           mt_info[unit].cmd_usec+=cmd_usec_ready; // time needed to advance tape medium
           mt_info[unit].cmd_tm0=2;                // signal incremented data for already setup tape animation
           // avoid execsive accumulation of pending medium movement
           if (mt_info[unit].cmd_usec > 500000) {
               mt_info[unit].recsize=mt_info[unit].recsize / 2; 
               mt_info[unit].cmd_usec = mt_info[unit].cmd_usec / 2; 
           }
       }

    }

    return r;
}

/* Handle processing of tape requests. */
t_stat mt_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 cmd = uptr->u5 & MT_CMDMSK;

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
    if (uptr->u4 == 0) uptr->u4 = 14400; // default 1200 ft reel; 1 foot = 12 inches; 1200 ft = 14400 inches
    mt_info[unit].numrec = mt_info[unit].numrw = 0;
    mt_info[unit].TapeRecord_index=-1; 
    mt_info[unit].TapeStatus = 0;
    mt_info[unit].TapeCheck = 0;
} 

t_stat mt_reset(DEVICE * dptr)
{
    int i;
    for (i = 0; i < 4; i++) {
        mt_ini(&mt_unit[i], 0);
    }
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
    mt_info[unit].justdetached = 0; 
    mt_info[unit].numrw = 0;
    mt_info[unit].numrec = 0;
    mt_info[unit].TapeRecord_index=-1; 
    mt_info[unit].TapeStatus = 0;
    mt_info[unit].TapeCheck = 0;
    mt_info[unit].ReadyTickCount = 0;
    mt_info[unit].recsize = 0;

    // allocate a buffer for the biggest possible tape records
    if (mt_info[unit].TapeRecord_buf==NULL) {
        mt_info[unit].TapeRecord_buf = 
            (uint8 *) malloc(TAPE_MAX_REC_LEN * sizeof(t_int64)); 
        if (mt_info[unit].TapeRecord_buf==NULL) return sim_messagef(SCPE_IERR, "Tape record buffer malloc failed \n");
    }

    return SCPE_OK;
}

t_stat mt_detach(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);

    if (mt_info[unit].TapeRecord_index > 0) WriteTapeRecord(uptr, 0, unit); 
    mt_info[unit].justattached = 0; 
    mt_info[unit].justdetached = 1; 
    mt_info[unit].TapeRecord_index=-1; 
    mt_info[unit].TapeStatus = 0;
    mt_info[unit].TapeCheck = 0;
    mt_info[unit].recsize = uptr->u3; // save current unwind medium
    uptr->u3 = 0;
    uptr->u5 = 0;
    mt_info[unit].numrec = 0;
    if (mt_info[unit].TapeRecord_buf) free(mt_info[unit].TapeRecord_buf); 
    mt_info[unit].TapeRecord_buf = NULL; 
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
    uptr->u4 = 14400 * len / 1200;
    return SCPE_OK;
}

/* Show tape length */

t_stat mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "length %d feet", uptr->u4 * 1200 / 14400);
    return SCPE_OK;
}


t_stat mt_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", mt_description(dptr));
   fprintf (st, "The magnetic tape assumes that all tapes are 7 track\n");
   fprintf (st, "with valid parity. Tapes are assumed to be 100 characters per\n");
   fprintf (st, "inch. \n\n");
   sim_tape_attach_help (st, dptr, uptr, flag, cptr);
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * mt_description(DEVICE *dptr)
{
   return "IBM 726 Magnetic Tape Unit";
}




