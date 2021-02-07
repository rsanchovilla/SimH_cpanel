/* NORC_mt.c: IBM NORC Magnetic tape 

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

#include "NORC_defs.h"
#include "sim_tape.h"

#define UNIT_MT         UNIT_ATTABLE | UNIT_ROABLE | UNIT_DISABLE

// Tapes in NORC have address assigned by the console. So tape P can be tied to address 00-12
// on startup, tape P=00 is assigned to MT0, P=01 to MT1 and son on up to MT07

/* in u3 is tape medium length used on current position (unit = inches x 1000) */
/* in u4 is tape medium max length (in inches. 16800 for 1400 ft reel) */
/* in u5 holds the command being executed by tape unit */

extern int                 V;                           // Address to be accesed in CRT memory
extern t_int64             REG1, REG2;                  // Storage registers
extern int                 Console_Sw_Write_Output;     // selection switch for WRITE OUTPUT =0 -> set to off, =1 -> set to on
extern int                 Console_Sw_CRTCHKStop;       // selection switch for CRT CHECK STOPS: =0 -> set to proceed, =1 -> set to stop

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

UNIT                mt_unit[9] = {
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 0      - CTC tape unit */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 1 to 8 - NORC tape units */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 2 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 3 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 4 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 5 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 6 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 7 */
    {UDATA(&mt_srv, UNIT_MT, 0), 0}, /* 8 */
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
    9, 8, 15, 1, 8, 8,
    NULL, NULL, &mt_reset, NULL, &mt_attach, &mt_detach,
    &mt_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &mt_help, NULL, NULL, &mt_description
};

// IBM NORC tape/printer control Unit internal state
int LastTapeCheck    = 0;    
int bFastMode = 0;            // =1 for FAST operation

tapedata * TapeData = NULL; // holds words read/writen to/from tape

// save words exchanged with tape in TapeData array. Save 1 word each 10 exchanged words
// one word needs 18 digits, each digit needs 14 microsec -> one word needs 252 microsec
// saving 1 word of ten in array -> one array entry each 2.5 millisec
// a tape reel has 16800 inches, with density 510 digits/inch -> 8.5 M digits
// each word need 18 digits -> each reel holds 476 K Words
// as we are saving 1 word each ten, a full read reel will need 48 K words aprox
// if mode = 0 -> malloc dymanic mem for var, mode = 1 -> free mem
//         = 2 -> init to read first block
//         = 3 -> set to read next block
//         = 4 -> add tape word / address
//         = 5 -> terminate read block
void SetTapeData(int mode, t_int64 d) 
{
    int n; 

    if (mode==0) {
        // alloc mem and inirçt
        TapeData = (tapedata *) malloc(sizeof(tapedata));
        SetTapeData(2,0);
        return; 
    }
    if (TapeData==NULL) return; 
    if (mode==1) {
        // free mem
        free(TapeData);
        TapeData=NULL;
    } else if (mode==2) {
        // init for first block
        // block gap is 8 msec, half gap for start of block = 4000 microsec + 14 microsc for begin of block char
        TapeData->Ticks = GlobalTicksCount + 4000 + 14; 
        TapeData->Count=0;
        TapeData->index=0;
    } else if (mode==3) {
        // add next block
        // end half gap + 14x2 microsec for start/end block character  + half gap for next block
        TapeData->Ticks += 14 + 4000 + 4000 + 14; 
        TapeData->index=0;
    } else if (mode==4) {
        // add word
        // each digit needs 14 microsec, each word needs 18 characters
        TapeData->Ticks += 14 * 18; 
        n=(TapeData->index++);
        if ((n==0) || (n>2)) {
            // save into array 1 each five
            TapeData->index=1; // reset 
            n=(TapeData->Count++);
            if (n < MAX_TAPEDATA) {
                //save the values register would have at TapeData->Ticks time, so it can be
                // shown on control panel refresh
                TapeData->TicksCount[n] = TapeData->Ticks;
                TapeData->REG1[n]=d; 
                TapeData->REG2[n]=REG2; 
                TapeData->V[n]=V; 
            }
        }
    } else if (mode==5) {
        // terminate block
        TapeData->index=0;
    }
}

// return 1 if tape unit n (0..8) is ready to receive a command
//        0                       is busy executing some command
//       -1                       cannot receive command
int mt_ready(int n)
{
    if ((n < 0) || (n > 8)) return -1; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return -1; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return -1; // not attached
    if (mt_unit[n].u5 & MT_RDY) return 1; // tape ready
    return 0; // tape busy
}

// set the ready flag on tape
void mt_set_ready(int n)
{
    if ((n < 0) || (n > 8)) return; // invalid tape number
    if (mt_unit[n].flags & UNIT_DIS) return; // disabled
    if ((mt_unit[n].flags & UNIT_ATT) == 0) return; // not attached
    mt_unit[n].u5 |= MT_RDY; // set tape ready
}

// retrieve last cmd sent to tape.
int mt_get_last_cmd(int n)
{
    int cmd; 

    if ((n < 0) || (n > 8)) return 0; // invalid tape number
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


// each word in buf has 18 characters: char bitcount check (ignored) + 16 digits + 'C' as end of word char
// return 1 if not this format
int getword(uint8 * buf, t_int64 * d)
{
    char c; 
    int i; 

    if (buf[17] != 'C') return 1; // does not ends with end of word char
    *d=0;
    for (i=1;i<17;i++) {
        c=buf[i];
        if ((c<'0')||(c>'9')) return 1; // not a digit
        *d=(*d) * 10 + c - '0';
    }
    #if defined(CPANEL)
    SetTapeData(4, *d);
    #endif
    return 0; 
}

int mt_transfer_tape_rec_to_CRT(DEVICE *dptr, uint8 * buf, t_mtrlnt reclen, 
                                int cmd, int V1, int V2, int BlockNumToRead)
{
    int i, ic, BlockNum, bEndOfBuf, nW, Addr, AddrIncr, nBlockWords;
    t_int64 d;
    char c, sBuf[80]; 
    
    // decode record read from tape
    // each word has 18 characters: 
    //    char bitcount check (ignored) + 16 digits + 'C' as end of word char
    // each record (= block in norc terminology) starts and ends with a 'D' char
    // thus, a block of n data words has 
    //    reclen = 18(n+2)+2 = number of chars in tape 
    //    ((reclen-2)/18)-2 = num data words in record

    for (i=0;;i++) {
        if (i==reclen) { sBuf[i]=0;  break; }
        if (i==70) { sBuf[i++]='.';  sBuf[i++]='.';  sBuf[i++]='.';  sBuf[i]=0;  break; }
        c=buf[i];
        if (c<' ') c = '?';
        sBuf[i]=c;
    }
    sim_debug(DEBUG_DETAIL, dptr, "Tape record contents: %s\n", sBuf);

    if (reclen < 1 + 18 + 1) {
        sim_debug(DEBUG_EXP, dptr, "Bad record len. Minimum is 20 chars, but reclen=%d\n", reclen);
        return MT_IND_BLKLEN; 
    }
    // blocks should start with D and end with D 
    if ((buf[0] != 'D') || (buf[reclen-1] != 'D')) {
        // block does not starts and ends with the begin/end block character (code 13 -> 'D')
        sim_debug(DEBUG_EXP, dptr, "Block start char should be 'D' but is '%c', \n", buf[0]);
        sim_debug(DEBUG_EXP, dptr, "Block end char should be 'D' but is '%c', \n", buf[reclen-1]);
        return MT_IND_BLKLEN; 
    }
    // get start of block word
    if (getword(&buf[1], &d)) return MT_IND_BADCHAR;
    // store start of block word in storage register. None of this is stated in any manual
    // but this is the (most logical) way to check the start and end of block word have the same
    // block number as states in prog manual p36
    // and is very handly to see what block are cpu reading
    REG1=REG2=d; 
    // check if is end of file mark
    if (reclen == 1+18+1) {
        // only one word -> should be a end of file mark
        sim_debug(DEBUG_EXP, dptr, "End Of File mark sensed: %08d%08d \n", printfw(REG1));
        return MT_IND_EOF; 
    }
    sim_debug(DEBUG_EXP, dptr, "Start Of Block Word: %08d%08d\n", printfw(REG1));
    BlockNum=REG1 % D4; // this is the block number of the block being read
    // determine directorion of read
    if ((cmd==OP_READ_BWRD) || (cmd==OP_VER_BWRD)) {
        AddrIncr=-1; 
        ic = (int) reclen -1-18-18; // if reading backwards, start at end of block buffer
    } else {
        AddrIncr=1; 
        ic=1+18; // char position of first data word of block, after start of block word
    }
    // check block number if it is not blank
    if ((BlockNumToRead) && (BlockNumToRead!=BlockNum)) { 
        // not reading the wanted block number -> verify the block without storing anythin
        // in CRT mem and return, indicating the block has been passed over 
        sim_debug(DEBUG_EXP, dptr, "Block number expected is %04d, but current block number is %04d. Skip block \n", BlockNumToRead, BlockNum);
        while (1) {
            if ((ic < 1) || (ic+18 >= (int) reclen)) return MT_IND_WORDLEN; // no, exit with error
            // get word from tape rec
            if (getword(&buf[ic], &d)) return MT_IND_BADCHAR;
            // is end of block?
            if ((ic==1) || (ic+18 == (int) reclen - 1)  ) break; // yes, exit
            // no, continu scanning tape record
            ic += 18 * AddrIncr;
        }
        return MT_IND_BLK_PASSED_OVER; 
    }
    sim_debug(DEBUG_EXP, dptr, "Read Tape Block number %04d\n", BlockNum);
    // determine if should store read data from tape or just check it for errors
    // set Addr = 0 if no word has to be stored in CRT
    if ((cmd==OP_VERIFY) || (cmd==OP_VER_BWRD) || (V1==0) ) {
        Addr=0; 
        sim_debug(DEBUG_EXP, dptr, "Verify %s\n", (AddrIncr>0)?"forward":"backward");
    } else {
        Addr=V1;   // first addr to store word
        sim_debug(DEBUG_EXP, dptr, "Read %s addr %04d to %04d\n", (AddrIncr>0)?"forward":"backward", V1, V2);
    }

    // now, parse data words on record read from tape
    bEndOfBuf=nW=nBlockWords=0;  
    while (ic < (int) reclen) {
        // record has 18 chars left to compose a word?
        if ((ic < 1) || (ic+18 >= (int) reclen)) return MT_IND_WORDLEN; // no, exit with error
        // get word from tape rec
        if (getword(&buf[ic], &d)) return MT_IND_BADCHAR;
        // is end of block?
        if ((ic==1) || (ic+18 == (int) reclen - 1)  ) { 
            // yes, this is last word of block, so it will be interpreted as end of block word
            sim_debug(DEBUG_EXP, dptr, "End Of Block Word: %08d%08d, data words in block: %d\n", printfw(d), nBlockWords);
            // if reading from given V1 to given V2, check length match
            if ((Addr) && (V2) && (bEndOfBuf==0)) {
                sim_debug(DEBUG_EXP, dptr, "Block in tape too short, nothing to store at addr %04d\n", Addr);
                return MT_IND_BLKLEN; 
            }
            // check start of block word (saved in REG2) is same as end of block word
            if (d != REG2) return MT_IND_BLKNUM;
            // fine, tape block processed
            break;
        }
        // is a spacing word to skip?
        if (buf[ic]== ' ') {
            // yes, skip the word
            nW++; 
            if (nW==1) sim_debug(DEBUG_EXP, dptr, "Tape spacing word %d (ignored) \n", nW);
            ic += 18 * AddrIncr; 
            continue; 
        }
        if (nW>0) sim_debug(DEBUG_EXP, dptr, "Tape spacing word %d (ignored) \n", nW);
        nW=0;
        // increment
        ic += 18 * AddrIncr;  // incr in tape char record
        nBlockWords++;        // incr number of data words read
        // should store word in CRT ?
        if (Addr==0) {
            // no, because Addr=0. Just word has been read to verify it
            sim_debug(DEBUG_EXP, dptr, "Tape verify word %d: %08d%08d \n", nBlockWords, printfw(d));
            continue; 
        }
        // yes, should store word in CRT.
        // Now check if more data in tape block that will overflow V2 address 
        if (bEndOfBuf) {
            sim_debug(DEBUG_EXP, dptr, "Block in tape too long\n");
            return MT_IND_BLKLEN; 
        }
        // Now chewck is addr in CRT to store read word is valid
        // manual does not states what happends if storing tape data to invalid address
        // what simulator does: stop with invalid addr
        if ((Addr<=0) || (Addr >= MemSize())) {
            sim_debug(DEBUG_EXP, dptr, "Cannot store tape read word at addr invalid addr %04d\n", Addr);
            // Invalid address. Should stop the program?
            if (Console_Sw_CRTCHKStop) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because CRT Check set to stop\n");
                return MT_IND_ADDR;
            }
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because CRT Check set to proceed\n");
            // if no stop -> terminate tape transfer without error
            return 0; 
        } 
        // store tape read word, set V reg as we are accesing CRT
        CRT[V=Addr]=d; 
        sim_debug(DEBUG_EXP, dptr, "Tape to CRT %04d: %08d%08d \n", Addr, printfw(d));
        // check if reach end addr V2 to store data
        if ((V2) && (Addr==V2)) {
            // yes, do not inc addr, instead set end of buffer flag
            bEndOfBuf=1; 
        } else {
            // no, incr addr
            Addr+=AddrIncr;
        }
    }
    return 0;
}

void setword(uint8 * buf, t_int64 d)
{
    char c; 
    int i; 

    #if defined(CPANEL)
    SetTapeData(4, d);
    #endif
    buf[0]='*'; // character checksum
    for (i=16;i>0;i--) {
        c=(d % 10) + '0'; 
        d=d / 10; 
        buf[i]=c;
    }
    buf[17] = 'C';
}

int mt_transfer_CRT_to_tape_rec(DEVICE *dptr, uint8 * buf, t_mtrlnt * reclen, 
                                int bUseTapeSpacing, int nWordLimit, t_int64 TapeIREG)
{
    int ic, nWord, nW, nBlockWords; 
    int P,Q, V1, V2, BlockNumToWrite;

    // get tape operation parameters
    DecodeInst(TapeIREG, &P, &Q, &V1, &V2, &BlockNumToWrite, 0);

    // check if writing a end of file mark
    if (V1==0) {
        // yes, generate eof mark on buf
        buf[0]='D'; // begin of block char
        setword(&buf[1], TapeIREG);
        sim_debug(DEBUG_EXP, dptr, "End Of File mark: 02d %02d %04d %04d %04d\n", P,Q,V1,V2,BlockNumToWrite);
        buf[19]='D'; // begin of block char
        *reclen=20;
        return MT_IND_EOF; 
    }

    nW=nWord=nBlockWords=0;
    if ((Q==OP_WRI_OUTPUT) || (Q==OP_DEL_OUTPUT) || (bUseTapeSpacing)) {
        // set write output space each 98 words to allow CTC processing
        nW=nWord=98; 
    }

    buf[0]='D'; // begin of block char
    setword(&buf[1], TapeIREG); // begin of block word
    sim_debug(DEBUG_EXP, dptr, "Write from %04d to %04d as Block number %04d\n", V1, V2, BlockNumToWrite);
    sim_debug(DEBUG_EXP, dptr, "Start Of Block: %02d %02d %04d %04d %04d\n", P,Q,V1,V2,BlockNumToWrite);
    ic=19;
    V=V1;
    while (1) {
        if (V >= MemSize()) {
            sim_debug(DEBUG_EXP, dptr, "Cannot read from invalid addr %04d\n", V);
            // Invalid address. Should stop the program?
            if (Console_Sw_CRTCHKStop) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because CRT Check set to stop\n");
                return MT_IND_ADDR;
            }
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because CRT Check set to proceed\n");
            // if no stop -> terminate tape transfer without error
            break; 
        }
        nBlockWords++;
        REG1=CRT[V];
        sim_debug(DEBUG_EXP, dptr, "CRT %04d to Tape: %08d%08d \n", V, printfw(REG1));
        setword(&buf[ic], REG1); 
        // check if reached end addr to be saved on tape
        ic += 18;
        if (V==V2) break; 
        V++;
        // check if reached the max num of words to be saved on tape
        if (nWordLimit) {
            nWordLimit--;
            if (nWordLimit == 0) break;
        }
        // check if must insert space for CTC
        nW--;
        if (nW==0) {
            // yes, should insert space. how much?
            // a gap of 1.5 inch must be generated. Density is 510 digits/inch 
            // so gap is 765 digits (=chars)
            // each word in tape uses 18 chars -> gap is 42,5 words
            // insert spacing words. On real hw, spacing words are ... just blank space
            // in simulation, a fake word counting 1..43 in added, so it can be visually identified
            int nSpacing=43; 
            sim_debug(DEBUG_EXP, dptr, "Tape spacing: insert %d words\n", nSpacing);
            for (nW=1;nW<=nSpacing;nW++) {
                setword(&buf[ic], nW); 
                buf[ic]=' '; // set space as checksum to identify word to skip
                ic += 18;
            }
            // reset counter
            nW=nWord;
        }
    }
    setword(&buf[ic], TapeIREG); // end of block word
    sim_debug(DEBUG_EXP, dptr, "End Of Block: %02d %02d %04d %04d %04d (%d words)\n", P,Q,V1,V2,BlockNumToWrite,nBlockWords);
    ic += 18; 
    buf[ic]='D';
    *reclen=ic+1;
    return 0;
}

// Max record size in chars in tape_ tape commands can store up to 3600 words at once
// if write output enabled, each 98 words, 43 words are inserted as spacing
// this means int(3600/98)*43+3600 = 5148 words max in a record
// chars = (num words + 2)*18+2 = 92702 chars

// Start off a mag tape command. ape command is exec intantanelly: tape is read and data
// stored in CRT mem, or tape is written with data from CRT
// return on result/LastTapeCheck operation result. If no fast mode, schedule tape operation
// duration (if any). During this time, RDY flag si removed from tape
//
// possible results:
//
//    reason        LastTapeCheck
//
//    STOP_TAPE     0               tape unit disabled/file not attached
//    STOP_TAPE     0               cannot write, tape write protected
//    STOP_TAPE     MT_IND_RDERR    simulated read error in tape
//    STOP_TAPE     MT_IND_BLKLEN   Bad read tape record len. Minimum is 20 chars, but len is < 20
//    STOP_TAPE     MT_IND_BLKLEN   Block start char/end char should be 'D', but read tape record has another char
//    STOP_TAPE     MT_IND_BLKLEN   when reading with R & S fields set, tape record must contain S-R+1 words
//    STOP_TAPE     MT_IND_BLKNUM   start of block word is not the same as end of block word 
//    STOP_TAPE     MT_IND_WORDLEN  tape records ended in a middle of a word
//    STOP_TAPE     MT_IND_BADCHAR  word contains a non digit char
//    STOP_EOT      0               trying to read backward but tape at begin of tape
//    STOP_EOT      0               trying to read forward but tape at end of tape
//    STOP_EOT      0               trying to write tape at end of medium
//    STOP_IOERROR  0               host i/o read error / other read error condition                      
//    STOP_IOERROR  0               host i/o write error / other write error condition                      
//    STOP_ADDR     0               read tape data but trying to store it at invalid CRT addr
//    STOP_ADDR     0               trying to read data from invaidl CRT addr to write it to tape 
//    0             0               read/write ok
//    0             MT_IND_EOF      read end of file mark (not an error)
//
uint32 mt_cmd(UNIT * uptr, uint16 cmd, uint16 fast)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = uptr - &mt_unit[0];
    int                 u3;
    t_stat              r, reason;   
    uint8               buf[92703];
    t_mtrlnt            reclen  = 0;  // number of chars in tape record. 
    int32               recsize = 0;  // number of inches x1000 used by tape record
    int                 time; 
    extern t_int64      IREG;           

    // init extended info. 
    mt_info[unit].recsize=0;
    mt_info[unit].cmd_usec1=0;
    mt_info[unit].cmd_usec2=0;
    mt_info[unit].cmd_tm0=sim_os_msec();
    
    SetTapeData(2, 0); // Init TapeData

    LastTapeCheck = 0;        // init tape check to be displayed in console
    reason = SCPE_OK; 
    bFastMode = fast;

    /* If tape unit disabled return error */
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, dptr, "Tape %d: command %02d attempted on disabled tape\n", unit, cmd);
        return STOP_TAPE;
    }
    /* If tape has no file attached return error */
    if ((uptr->flags & UNIT_ATT) == 0) {
        sim_debug(DEBUG_EXP, dptr, "Tape %d: command %02d attempted on tape without file attached\n", unit, cmd);
        return STOP_TAPE;
    }
    uptr->u5 &= ~(MT_CMDMSK); // remove last command sent to tape, ready flag. Do not change the just attached indicator
    uptr->u5 |= cmd; // set current command in execution
    // tape operation is fully completed right now: all needed records are read/written from tape,
    // tape medium is advanced (or rewinded), read data is loaded on CRT mem, final tape is set
    switch (cmd) {
    case OP_READ:          // 94  // read forward fro tape P, store words of block T ç at addr S-R
    case OP_READ_BWRD:     // 95  // read backward
    case OP_VERIFY:        // 96
    case OP_VER_BWRD:      // 97
    Do_Read:
        reclen = recsize = 0;
        if ((cmd == OP_READ_BWRD) || (cmd == OP_VER_BWRD)) {
            sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init backward read\n", unit);
            if (sim_tape_bot(uptr)) {
               sim_debug(DEBUG_EXP, dptr, "Tape unit %d: tape at BOT\n", unit);
               uptr->u3 = 0; // tape pos at beginning of medium
               mt_info[unit].numrec = 0; // tape positioned at first record
               reason = STOP_EOT;
               break; 
            }
            r = sim_tape_rdrecr(uptr, buf, &reclen, sizeof(buf));
            recsize = -(int32) ((reclen * 0.001961 + 1.5) * 1000);            
        } else {
            sim_debug(DEBUG_DATA, dptr, "Tape unit %d: init read\n", unit);
            r = sim_tape_rdrecf(uptr, buf, &reclen, sizeof(buf));
            // reel diameter is 10.5 inch
            // writing density is 510 digits per inch (in fact 510 bits per inch on 4 paralel tracks)
            // each char uses 0,001961 inches. at the end of record (a block) 
            // the IRG (inter gap record) uses 1.5 inches (prog manual says 1.5. faster faster book says 1.25)
            // tape medium has a lenght of 16800 inches
            // scaled x1000 to use integer values
            recsize = (int32) ((reclen * 0.001961 + 1.5) * 1000);            
        }
        // process result conditions
        if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: end of tape sensed\n", unit);
            reason = STOP_EOT;
            break; 
        } else if ((r == MTSE_RECE) || (r == MTSE_INVRL)) {
            // record header contains error flag
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: error in record\n", unit);
            LastTapeCheck = MT_IND_RDERR; 
            reason = STOP_TAPE;
            break; 
            // this is simulated read error, so signal is reported to cpu 
        } else if (r != MTSE_OK) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: read error %d\n", unit, r);
            reason = STOP_IOERROR;
            break; 
            // this is real host i/o error. If this error occurs, something is wrong 
            // on simulator level, so stop to allow user to notice
        }
        // tape read ok
        mt_info[unit].numrec += (((cmd == OP_READ_BWRD) || (cmd == OP_VER_BWRD)) ? -1:1); // tape positioned at next block 
        mt_info[unit].recsize+=recsize;  // calc recsize for  this tape command execution
        uptr->u3 += recsize;             // calc tape medium pos
        mt_info[unit].numrw++;
        // calc time needed to finish tape operation
        // a block on n words needs reclen = (18*(n + 2) + 2) characters; 
        // time for characters = 14 microsec
        // time to accel to r/w speed = 8 millisec (prog manual says 10 msec, faster faster book says 8 msec)
        mt_info[unit].cmd_usec2 = 8000 + reclen * 14; 
        sim_debug(DEBUG_DETAIL, dptr, "Read tape record (%d chars, used %0.2f inches, needs %d msec) from tape %d\n", 
            (int) reclen, recsize / 1000.0, mt_info[unit].cmd_usec2 / 1000, unit);
        // transfer read data to CRT if no tape signal
        r = mt_transfer_tape_rec_to_CRT(dptr, buf, reclen, 
                                        cmd, mt_info[unit].V1, mt_info[unit].V2, mt_info[unit].BlockNum);
        if (r == MT_IND_BLK_PASSED_OVER) {
            // block read, but we are looking for another block number. So continue reading tape
            sim_debug(DEBUG_EXP, dptr, "Continue scanning tape\n"); 
            mt_info[unit].cmd_usec1 += mt_info[unit].cmd_usec2; // accumulate read time usec2 in scan time usec1
            mt_info[unit].cmd_usec2 = 0;
            SetTapeData(3, 0); // add a new block to TapeData
            goto Do_Read;
        }
        if (r == MT_IND_ADDR) {
            reason = STOP_ADDR; // error because reading from tape to invalid CRT addr 
        } else if (r == MT_IND_EOF) {
            LastTapeCheck=r; // set LastTapeCheck but not STOP_TAPE
        } else if (r) {
            LastTapeCheck=r;
            reason = STOP_TAPE;
        }
        break;
    case OP_WRITE:        // 90  // write on tape P addr S to R as block T
    case OP_WRI_OUTPUT:   // 91  // as 90, leave spsce between groups of words for CTC processing
    case OP_DELETE:       // 92  // delete on tape P space for block S-R
    case OP_DEL_OUTPUT:   // 93  // as 92 for words written by 91
        reclen=1;
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: init write\n", unit);
        r = mt_transfer_CRT_to_tape_rec(dptr, buf, &reclen, 
                                        ((Console_Sw_Write_Output) || (cmd==OP_WRI_OUTPUT) || (cmd==OP_DEL_OUTPUT)) ? 1 : 0, 
                                        0, IREG);
        if (r == MT_IND_ADDR) {
            reason = STOP_ADDR; // error because writting to tape from invalid CRT addr 
            break; 
        } else if (r == MT_IND_EOF) {
            LastTapeCheck=r; // set LastTapeCheck but not STOP_TAPE
        }
        // calc tape pos:
        recsize = (int32) ((reclen * 0.001961 + 1.5) * 1000);            
        // actual simulated tape write
        if ((cmd == OP_DELETE) || (cmd == OP_DEL_OUTPUT)) {
            r = sim_tape_errecf(uptr, reclen);
        } else {
            r = sim_tape_wrrecf(uptr, buf, reclen);
        }
        // process result conditions
        if (r == MTSE_WRP) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: cannot write, tape write protected\n", unit);
            reason = STOP_TAPE;
            break; 
        } else if ((r == MTSE_EOM) || (uptr->u3 > uptr->u4*1000)) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: cannot write, end of tape sensed\n", unit);
            reason = STOP_EOT;
            break; 
        } else if (r != MTSE_OK) {
            sim_debug(DEBUG_EXP, dptr, "Tape unit %d: write error %d\n", unit, r);
            reason = STOP_IOERROR; 
            break; 
            // this is real host i/o error. If this error occurs, something is wrong 
            // on simulator level, so stop to allow user to notice
        } 
        // write ok
        mt_info[unit].numrec++; // tape positioned at next record 
        uptr->u3 += recsize;        
        // set extended info. 
        mt_info[unit].recsize=recsize;
        mt_info[unit].cmd_usec1 = 8000 + reclen * 14; // calc time needed for write
        mt_info[unit].numrw++;
        sim_debug(DEBUG_DETAIL, dptr, "Write block (%d chars, used %0.2f inches, needs %d msec) to tape %d\n", 
                                       (int) reclen, recsize / 1000.0, mt_info[unit].cmd_usec1 / 1000, unit);
        break;
    case OP_REWIND:       // 98
        /* Check if at load point, quick return if so */
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
        uptr->u3 = 0;
        mt_info[unit].numrec = 0; // tape positioned at first record
        // calculate rew duration of rewind. Programming manual nor faster faster book describes
        // the timming for rewinding (except speed is same as regular r/w: 140 inch/sec). 
        // as an educated guess, let say cpu needs 40 microsec to send command to tape to initiate
        // rewinding, then cpu is free to continue execution. Also as an educated guess, cpu
        // will be wait (intelocked) if trying to send a command to a tape while it is rewinding
        mt_info[unit].cmd_usec1 = 1000 * u3 / 140;  // rew at speed of 140 inch/sec. there are NO hi speed rew
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: rewind time (%d sec)\n", unit, mt_info[unit].cmd_usec2 / 1000*000);
        // set extended info. 
        mt_info[unit].recsize=u3;              // on rewind cmd, recsize holds the ammount of tape medium on L reel at begginning of command
        break;

    default:
        sim_debug(DEBUG_EXP, dptr, "Tape %d: unknown command %02d\n", unit, cmd);
        // should never occurs. just to catch it if so.
    }
    SetTapeData(5,0); // terminate TapeData block
    // not schedulle event to handle busy flag on tape
    if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
        uptr->u5 |= MT_RDY;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d done (fast mode), used %4.2f%% of medium (%0.2f inches)\n", 
            unit,
            (uptr->u3 / (uptr->u4*1000.0))*100.0, 
            uptr->u3 / 1000.0
            );
        return reason; 
    }
    // schedulle tape command termination
    time = mt_info[unit].cmd_usec1 + mt_info[unit].cmd_usec2; // microsecs needed to complete tape command
    if (time == 0) {
        sim_debug(DEBUG_DETAIL, dptr, "... Tape %d command done \n", unit); 
        return reason; 
    }
    sim_cancel(uptr);
    sim_activate(uptr, time);
    uptr->u5 &= ~(MT_RDY); // remove ready flag
    sim_debug(DEBUG_DETAIL, dptr, "... Tape %d command will take %d msec\n", unit, time / 1000); 
    return SCPE_OK;
}

/* Handle processing of tape requests. */
t_stat mt_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 cmd = uptr->u5 & MT_CMDMSK;

    switch (cmd) {
    case OP_READ:         // 94  // read forward fro tape P, store words of block T ç at addr S-R
    case OP_READ_BWRD:    // 95  // read backward
    case OP_VERIFY:       // 96
    case OP_VER_BWRD:     // 97
    case OP_WRITE:        // 90  // write on tape P addr S to R as block T
    case OP_WRI_OUTPUT:   // 91  // as 90, leave spsce between groups of words for CTC processing
    case OP_DELETE:       // 92  // delete on tape P space for block S-R
    case OP_DEL_OUTPUT:   // 93  // as 92 for words written by 91
        // command finished
        goto tape_done;
        break;
    case OP_REWIND:       // 98
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
    if (uptr->u4 == 0) uptr->u4 = 16800; // default 1400 ft reel; 1 foot = 12 inches; 1400 ft = 16800 inches
    mt_info[unit].numrec = mt_info[unit].numrw = 0;
} 

t_stat mt_reset(DEVICE * dptr)
{
    int i;
    for (i = 0; i <= 8; i++) {
        mt_ini(&mt_unit[i], 0);
    }
    return SCPE_OK;
}

t_stat mt_attach(UNIT * uptr, CONST char *file)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    t_stat              r;

    sim_debug(DEBUG_EXP, dptr, "Tape unit %d: attach file %s\n", unit, file);

    if (uptr->flags & UNIT_ATT)         // remove current tape attacehd before attaching
       mt_detach(uptr);                 // the new one

    if ((r = sim_tape_attach(uptr, file)) != SCPE_OK)
        return r;
    uptr->u3 = 0;
    uptr->u5 = MT_RDY ;
    mt_info[unit].justattached = 1; 
    mt_info[unit].numrw = 0;
    mt_info[unit].numrec = 0;
    mt_info[unit].result1 = 0; 
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
    uptr->u4 = 16800 * len / 1400;
    return SCPE_OK;
}

/* Show tape length */

t_stat mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    fprintf (st, "length %d foot", uptr->u4 * 1400 / 16800);
    return SCPE_OK;
}


t_stat mt_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", mt_description(dptr));
   fprintf (st, "The magnetic tape assumes that all tapes are 7 track\n");
   fprintf (st, "with valid parity. Tape density is 510 chars per\n");
   fprintf (st, "inch. \n\n");
   sim_tape_attach_help (st, dptr, uptr, flag, cptr);
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * mt_description(DEVICE *dptr)
{
   return "IBM NORC Magnetic Tape Unit (evolution of IBM 726)";
}


