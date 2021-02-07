/* NORC_mt.c: IBM NORC Card to Tape to Card Machine

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
#include "sim_card.h"
#include "sim_tape.h"

#define UNIT_CTC        UNIT_ATTABLE | MODE_026 | MODE_LOWER | UNIT_RO

/* Definitions */
uint32              ctc_cmd(UNIT *up, uint16 cmd, uint16 dev);
t_stat              ctc_srv(UNIT *);
t_stat              ctc_attach(UNIT *, CONST char *);
t_stat              ctc_detach(UNIT *);
const char          *ctc_description (DEVICE *dptr);

UNIT                ctc_unit[1] = {
    {UDATA(&ctc_srv, UNIT_CTC, 0), 0}, 
};

MTAB ctc_mod[] = {
    { MTAB_XTD | MTAB_VDV, 0,         "FORMAT",                       "FORMAT",  &sim_card_set_fmt, &sim_card_show_fmt, NULL, "Set card format"},
    { 0 }
    };


DEVICE              ctc_dev = {
    "CTC", ctc_unit, NULL, ctc_mod,
    1, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, NULL, &ctc_attach, &ctc_detach,
    &ctc_dib, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, NULL, NULL, NULL, &ctc_description
};

extern int                 V;                           // Address to be accesed in CRT memory

// use tape routines
extern int mt_transfer_CRT_to_tape_rec(DEVICE *dptr, uint8 * buf, t_mtrlnt * reclen, int bUseTapeSpacing, int nWordLimit, t_int64 TapeIREG);
extern int getword(uint8 * buf, t_int64 * d);

// IBM NORC Card to Tape internal state

// vars where card is encoded for punching
char card_buf[120];
int card_nbuf;

// vars where card is encoded for printing
char card_lpt[120];
int card_nlpt;

void encode_char(int cPunch, int cLpt)
{
    if ((cPunch) && (card_nbuf < 80)) {
        card_buf[card_nbuf++] = cPunch;
    }
    if ((cLpt) && (card_nlpt < 120)) {
        card_lpt[card_nlpt++] = cLpt;
    }
}

void encode_lpt_spc(int nSpaces)
{
    while (nSpaces-- >0) encode_char(0, 32); 
}

void encode_lpt_str(const char * buf)
{
    while (*buf) encode_char(0, *buf++); 
}

void encode_lpt_num(t_int64 d, int l)
{
    char s[20];
    int i,n;
    char pad;
    
    if (l < 0) {
        l=-l; pad = ' '; // if l < 0 pad with space
    } else {
        pad = '0';       // if l > 0 pag with zero
    }
    for (i=15;i>=0;i--) {
        n = (int) (d % 10);
        d = d / 10;
        s[i] = '0' + n;
    }
    s[16] = 0;
    if (pad == ' ') {
        for(i=0;i<l;i++) {
            if (s[i] != '0') break;
            s[i] = ' ';
        }
    }
    encode_lpt_str(&s[16-l]);
}

#define     wf_sNNNN                    0  // [-] 16 digits 
#define     wf_NNNN                     1  // 16 digits without sign
#define     wf_NN_N_N_NNNN_NNNN_NNNN    2  // float word
#define     wf_NN_NN_NNNN_NNNN_NNNN     3  // instruction word
#define     wf_nnnN                     4  // left zeroes blank excep last digit
#define     wf_nnnn                     4  // left zeroes blank included last (so, all zeroes are blanked)
#define     wf_Float                    5  // float number: [-] N.NNN E [-] NN
#define     wf_Fix                      6  // fixed number: [-] NNN.NNN  (only if exp 0..12, else float)

void encode_lpt_word(t_int64 d, int wFormat)
{
    int n,e,i;
    int neg=0;

    if (d < 0) {d=-d; neg=1;} 
    if (wFormat == wf_sNNNN) {
        encode_char(0, neg ? '-':' '); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
    } else if (wFormat == wf_NNNN) {
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); 
    } else if (wFormat == wf_NN_N_N_NNNN_NNNN_NNNN) {
        n = (int) Shift_Digits(&d, 2); encode_lpt_num(n, 2); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 1); encode_lpt_num(n, 1); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 1); encode_lpt_num(n, 1); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
    } else if (wFormat == wf_NN_NN_NNNN_NNNN_NNNN) {
        n = (int) Shift_Digits(&d, 2); encode_lpt_num(n, 2); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 2); encode_lpt_num(n, 2); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
    } else if (wFormat == wf_nnnN) {
        encode_lpt_num(d,-15);  // replace 15 leading zeroes by spaces
    } else if (wFormat == wf_nnnn) {
        // replace 16 leading zeroes by spaces -> if value is zero, all converted to spaces
        encode_lpt_num(d,-16);  
    } else if (wFormat == wf_Float) {
        // float is +|- N.NNN E +|-NN
        e  = (int) Shift_Digits(&d, 2); 
        neg= (int) Shift_Digits(&d, 1); 
        encode_char(0, neg ? '-':' '); 
        n = (int) Shift_Digits(&d, 1); encode_lpt_num(n, 1); encode_char(0,'.');
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        n = (int) Shift_Digits(&d, 4); encode_lpt_num(n, 4); encode_lpt_spc(1);
        encode_char(0,'E');
        if (e > 50) {
            encode_char(0, '-'); 
            encode_lpt_num(-(e-100), 2); 
        } else {
        encode_char(0, (e<0) ? '-':' '); 
            encode_char(0, ' '); 
            encode_lpt_num(e, 2); 
        }
        encode_lpt_spc(1);
    } else if (wFormat == wf_Fix) {
        // is +|- NNN.NNN depending on exponent. If exponent < 12 or > 12 use float
        e = (int) (d / D14);
        if (e>12) {
            encode_lpt_word(neg ? -d:d, wf_Float);
            return; 
        }
        e  = (int) Shift_Digits(&d, 2); 
        neg= (int) Shift_Digits(&d, 1); 
        encode_char(0, neg ? '-':' '); 
        for(i=0;i<=e-1; i++) {
            n = (int) Shift_Digits(&d, 1); 
            if (n==0) encode_lpt_spc(1);
            else      encode_lpt_num(n, 1); 
        }
        n = (int) Shift_Digits(&d, 1); encode_lpt_num(n, 1); 
        encode_char(0,'.');
        for(i=e;i<=12; i++) {
            n = (int) Shift_Digits(&d, 1); encode_lpt_num(n, 1); 
        }
        encode_lpt_spc(1);
    }
}

// set word d (16 digits) in card image[80] to be punched later
// word is punched on a position depending on how much words already punched
// if nWordsAlreadyPunched=0 -> punched as word1
// if nWordsAlreadyPunched=1 -> punched as word2 and so on up to word4
// if bHi=1, will set a X (minus) punch on word's leftmost digit, 
// if bHi=2, will set a X (minus) punch on word's rightmost digit, 
// if bHi=3, will set a X (minus) punch on word's both leftmost and rightmost digit, 
// column number:          1         2         3         4         5         6         7         8
//                12345678901234567890123456789012345678901234567890123456789012345678901234567890
//      contents:            [    word 1    ][    word 2    ][    word 3    ][    word 4    ]
void PunchWord(int bHi, t_int64 d, int nWordsAlreadyPunched, uint16 * image)
{
    int digits; 
    char c; 
    uint16 h;

    for (digits=0;digits<16;digits++) {
        c = d % 10 + '0'; d=d / 10; 
        h = sim_ascii_to_hol(c);
        if ((digits==0) && ((bHi & 2)!=0)) h |= 0x400; // set hi punch
        if ((digits==15) && ((bHi & 1)!=0)) h |= 0x400; // set hi punch
        image[11 + nWordsAlreadyPunched * 16 + (15-digits)] = h;
    }
}

// put word d on tape record buf. bHi controls start of block (bHi=1), end of 
// block (bHi=2) or End of file mark (bHi=3). If bHi=2 (end of block) or = 3 (end of file)
// then save buf to tape MT0. 
t_stat word_to_tape(uint8 * buf, int bHi, t_int64 d)
{
    static int bIntoBlock, nWordsInBlock, nTotalWords;
    static t_int64 TapeIREG; 
    int bSaveBlock;
    t_mtrlnt reclen  = 0;  // number of chars in tape record. 
    int P,Q,R,S,T;
    t_stat r; 

    if (buf == NULL) {
        // init/done tape write vars
        if (bHi==0) { // init
            bIntoBlock=0;
            nTotalWords=0;
            return 0; 
        } else { // done
            // check if end of block has been saved to tape
            if (bIntoBlock) {
                return sim_messagef (SCPE_IERR, "Missing End of Block word\n");
            }
            // return number of words written as negative number
            return -nTotalWords; 
        }
    }
    if (bHi) {
        bSaveBlock=0;
        if (bIntoBlock==0) {
            // start of block
            if (bHi==2) return sim_messagef (SCPE_IERR, "End of Block without previous Start Of Block\n");
            TapeIREG=d; 
            DecodeInst(TapeIREG, &P, &Q, &R, &S, &T, 0);
            if (bHi==3) {
                // is end of file
                if ((R!=0) || (S!=0)) return sim_messagef (SCPE_IERR, "End of File Mark R=%04d S=%04d: Invalid R or S address (both must be 0000)\n", R,S);
                bSaveBlock=1; 
            } else if ((R >= MemSize()) || (R==0) || (S==0) || (S >= MemSize())) {
                return sim_messagef (SCPE_IERR, "Start of Block R=%04d S=%04d: Invalid R or S address (must be in 0001..3599 range)\n", R,S);
            } else {
                // regular start of block
                V=R;
                bIntoBlock=1; 
                nWordsInBlock=0;
            }
        } else {
            // end of block
            if (bHi==1) return sim_messagef (SCPE_IERR, "Start of Block without previous End Of Block\n");
            if (bHi==3) return sim_messagef (SCPE_IERR, "End of File Mark without previous End Of Block\n");
            // save reg to tape
            bSaveBlock=1;
        }
        if (bSaveBlock) {
            mt_transfer_CRT_to_tape_rec( NULL, buf, &reclen, 1, nWordsInBlock, TapeIREG);
            r = sim_tape_wrrecf(&mt_unit[0], buf, reclen);
            if (r == MTSE_WRP) {
                return sim_messagef (SCPE_IERR, "Tape unit MT0 cannot write, tape read protected\n");
            } else if (r == MTSE_EOM) {
                return sim_messagef (SCPE_IERR, "Tape unit MT0 cannot write, tape ended\n");
            } else if ((r == MTSE_RECE) || (r == MTSE_INVRL)) {
                return sim_messagef (SCPE_IERR, "Tape unit MT0: error in record\n");
            } else if (r != MTSE_OK) {
                return sim_messagef (SCPE_IERR, "Tape unit MT0: write error %d\n", r);
            }
            // write ok, init again to await a new block
            bIntoBlock=0; 
       }
       return 0;
    }
    if (bIntoBlock==0) {
        if (d==0) return 0; // outside a block only allow blank words, that are skipped
        return sim_messagef (SCPE_IERR, "Missing Start of Block word\n");
    } else if (V>=MemSize()) {
        return sim_messagef (SCPE_IERR, "Block too long. max allowed %d words\n", MemSize());
    } else {
        CRT[V++]=d; 
        nWordsInBlock++;
        nTotalWords++;
    }
    return 0;
}

t_stat ctc_card_to_tape()
{
    UNIT               *uptr = ctc_unit; 
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                unit = uptr - &lp_unit[0];

    uint16 image[80];
    uint16 h;
    int i, bHi, n, nDigits;
    char cbuf[81]; 
    char c; 
    t_stat r; 
    t_int64 d; 

    uint8 buf[92703]; // tape record buffer for simh

    // cmd==1 is the card to tape mode
    // attach card file to CTC device
    // attach tape file to MT0 device
    // set CTC echo to display read cards in console
    // set CTC print to log read cards. must also attach the print file to LP0 device

    if ((uptr->flags & UNIT_ATT) == 0) {
        return sim_messagef (SCPE_IERR, "No card file attached to CTC Unit\n");
    }
    if (uptr->flags & UNIT_DIS) {
        return sim_messagef (SCPE_IERR, "CTC Unit disabled\n");
    }   
    if ((mt_unit[0].flags & UNIT_ATT) == 0) {
        return sim_messagef (SCPE_IERR, "No tape file attached to MT0 Unit\n");
    }
    if (mt_unit[0].flags & UNIT_DIS) {
        return sim_messagef (SCPE_IERR, "MT0 Unit disabled\n");
    }   
    sim_printf("Read %d cards in CTC \n", sim_card_input_hopper_count(ctc_unit));

    memset(CRT, 0, sizeof(CRT));
    V=0;
    word_to_tape(NULL, 0,0); // init
    // read all cards 
    while (1) {
        r = sim_read_card(uptr, image);
        if ((r == CDSE_EOF) || (r == CDSE_EMPTY)) {
            r = SCPE_OK; break;             // normal termination on card file read finished
        } else if (r != CDSE_OK) {
            return sim_messagef (SCPE_IERR, "Error %d reading cards\n", r);
            break;                          // abnormal termination on error
        }

        // make local copy of card for debug output
        for (i=0; i<80; i++) {
            cbuf[i] = sim_hol_to_ascii(image[i]);
        }
        cbuf[80] = 0; // terminate string
        sim_debug(DEBUG_DETAIL, dptr, "Read Card: %s\n", sim_trim_endspc(cbuf));

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

        // format binary 4words per card as result of keypunch
        i=11; 
        for (n=0;n<4;n++) {
            d=0; bHi=0;
            for(nDigits=0;nDigits<16;nDigits++) {
                h = image[i];
                c = sim_hol_to_ascii(h  & 0x1ff);
                if ((c < '0') || (c > '9')) c = '0';
                if (h & 0x400) bHi |= (nDigits < 8) ? 1:2; 
                i++;
                d=d * 10 + c -'0';
            }
            r = word_to_tape(buf, bHi, d);
            if (r) return r;
        }
    }
    r=word_to_tape(NULL, 1,0); // done
    if (r>0) return r; 
    sim_printf("Written %d words in Tape \n", -r);

    return SCPE_OK;
}


void word_to_card(int Mode, int bHi, t_int64 d)
{
    static uint16 image[80];
    static int nWords; 
    int i; 
    char cbuf[81];
    DEVICE             *dptr = find_dev_from_unit(ctc_unit);

    if (Mode<0) {
        // init statics
        nWords=0;
        memset(image, 0, sizeof(image));
        return;
    } else if (Mode>0) {
        if (nWords>0) goto Do_punch; 
        return; 
    }
    // punch card
    if (nWords==0) memset(image, 0, sizeof(image));
    // punch the word in card
    PunchWord(bHi, d, nWords, image);
    nWords++;
    if (nWords==4) {
      Do_punch:
        // make local copy of card for debug output
        for (i=0; i<80; i++) {
            cbuf[i] = sim_hol_to_ascii(image[i]);
        }
        cbuf[80] = 0; // terminate string
        sim_debug(DEBUG_DETAIL, dptr, "Punch Card: %s\n", sim_trim_endspc(cbuf));
        // do the punch
        sim_punch_card(ctc_unit, image);
        nWords=0;
        return;
    }
}

t_stat ctc_tape_to_card(int bDataOnly)
{
    UNIT               *uptr = ctc_unit; 
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                unit = uptr - &lp_unit[0];

    int i, bHi, ic;
    t_stat r; 
    t_int64 d; 
    t_mtrlnt reclen;

    uint8 buf[92703]; // tape record buffer for simh

    if ((uptr->flags & UNIT_ATT) == 0) {
        return sim_messagef (SCPE_IERR, "No card file attached to CTC Unit\n");
    }
    if (uptr->flags & UNIT_DIS) {
        return sim_messagef (SCPE_IERR, "CTC Unit disabled\n");
    }   
    if ((mt_unit[0].flags & UNIT_ATT) == 0) {
        return sim_messagef (SCPE_IERR, "No tape file attached to MT0 Unit\n");
    }
    if (mt_unit[0].flags & UNIT_DIS) {
        return sim_messagef (SCPE_IERR, "MT0 Unit disabled\n");
    }   
    memset(CRT, 0, sizeof(CRT));
    V=0;
    // read all tape blocks until end of file
    while (1) {
        r = sim_tape_rdrecf(&mt_unit[0], buf, &reclen, sizeof(buf));
        if (r == MTSE_EOM)  {
            // end of tape, just terminate without error
            break; 
        } else if ((r == MTSE_RECE) || (r == MTSE_INVRL)) {
            return sim_messagef (SCPE_IERR, "Tape unit MT0: error in record\n");
        } else if (r != MTSE_OK) {
            return sim_messagef (SCPE_IERR, "Tape unit MT0: read error %d\n", r);
        } 
        // tape read ok, decode record read from tape
        if (reclen < 1 + 18 + 1) {
            return sim_messagef (SCPE_IERR, "Bad MT0 block size. Minimum is 20 chars, but reclen=%d\n", reclen);
        }
        // blocks should start with D and end with D 
        if ((buf[0] != 'D') || (buf[reclen-1] != 'D')) {
            // block does not starts and ends with the begin/end block character (code 13 -> 'D')
            return sim_messagef (SCPE_IERR, "Block start and end char should be 'D' but is '%c' and '%c'\n", buf[0], buf[reclen-1]);
        }
        ic=1; // char position of first word 
        V=0;
        while (ic < (int) reclen) {
            // record has 18 chars left to compose a word?
            if (ic+18 >= (int) reclen) return sim_messagef (SCPE_IERR, "Invalid word len \n");
            // is a spacing word to skip?
            if (buf[ic]== ' ') {
                // yes, skip the word
                ic += 18; 
                continue; 
            }
            // get word from tape rec
            if (getword(&buf[ic], &d)) return sim_messagef (SCPE_IERR, "Invalid word \n");
            if (V >= MemSize()) return sim_messagef (SCPE_IERR, "Too many words in CRT mem\n");
            CRT[V++]=d; 
            ic += 18;
            // is end of block?
            if (buf[ic] == 'D') break;
        }
        
        // block in tape is now in CRT mem, punch it
        word_to_card(-1,0,0); // init card punch
        for (i=0;i<V;i++) {
            d=CRT[i];
            bHi=0;
            if (i==0)   bHi |=1; 
            if (i==V-1) bHi |=2; 
            if (bDataOnly) { 
                // if bDataOnly set then do not punch start and end of block
                if (bHi) continue;
            }
            word_to_card(0,bHi,d); // punch card
        }
        word_to_card(1,0,0); // flush last punched card
        // if end of file, terminate
        if (V==1) break; 
    }
    return SCPE_OK;
}

// simulates the operation of CTC (card to tape to card) device
// CTC CARD deck_in.dck TO TAPE mt_out.tap
//               read input deck file previously prepared with Punch command
//               write it on tape file mt_out.tap (created as new file)
// CTC TAPE mt_in.tap TO CARD deck_out.dck [ DATAONLY ] 
//               read input tape file mt_in.tap 
//               punch card deck file deck_out.dck (created as new file)
//               will punch all block in tape, until end of file mark 
//               if DATAONLY clause is present, then does not punch words for 
//               block start/end
t_stat ctc_op_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    char fnInput[4*CBUFSIZE];
    char fnOutput[4*CBUFSIZE];
    char ToCardTape; 
    int bDataOnly = 0; 
    t_stat              r;

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    if (cptr==NULL) return sim_messagef (SCPE_ARG, "Expected CARD or TAPE\n");

    cptr = get_glyph (cptr, gbuf, 0); // get CARD or TAPE
    if (strcmp(gbuf, "CARD") == 0)    { ToCardTape = 'T'; } else 
    if (strcmp(gbuf, "TAPE") == 0)    { ToCardTape = 'D'; } else {
        return sim_messagef (SCPE_ARG, "Expected CARD or TAPE\n");
    }

    // get input filename
    cptr = get_glyph_quoted (cptr, fnInput, 0);             // read using get_glyph_quoted to do not 
                                                            // change the capitalization of file name
    if (fnInput[0] == 0) return sim_messagef (SCPE_ARG, "Missing input filename\n");

    // get TO separator
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missign TO \n");;

    // get CARD/TAPE separator
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if ((ToCardTape=='T') && (strcmp(gbuf, "TAPE") != 0)) return sim_messagef (SCPE_ARG, "Missign TAPE \n");;
    if ((ToCardTape=='D') && (strcmp(gbuf, "CARD") != 0)) return sim_messagef (SCPE_ARG, "Missign CARD \n");;

    cptr = get_glyph_quoted (cptr, fnOutput, 0);   
    if (fnOutput[0] == 0) return sim_messagef (SCPE_ARG, "Missing output filename\n");

    // get optional DATAONLY clause
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0); // get clause
    if (strcmp(gbuf, "DATAONLY") == 0) {
        bDataOnly=1; 
    }

    // attach files
    sim_switches |= SWMASK ('Q');
    if (ToCardTape=='T') {
        // CARD to TAPE
        // attach deck for read
        sim_switches &= ~SWMASK ('N');
        r=ctc_attach(ctc_unit, fnInput);
        if (r) return sim_messagef (r, "Cannot open for read card deck file \"%s\"\n", fnInput);
        // attach tape for write 
        sim_switches |= SWMASK ('N');
        r=mt_attach(&mt_unit[0], fnOutput);
        if (r) return sim_messagef (r, "Cannot open for write tape file \"%s\"\n", fnOutput);
        // perform the CTC operation 
        r=ctc_card_to_tape();
        if (r) return r;
    } else {
        // TAPE to CARD 
        // attach tape for read
        sim_switches &= ~SWMASK ('N');
        r=mt_attach(&mt_unit[0], fnInput);
        if (r) return sim_messagef (r, "Cannot open for read tape file \"%s\"\n", fnInput);
        // attach taep for write 
        sim_switches |= SWMASK ('N');
        r=ctc_attach(ctc_unit, fnOutput);
        if (r) return sim_messagef (r, "Cannot open for write card deck file \"%s\"\n", fnOutput);
        // perform the CTC operation 
        r=ctc_tape_to_card(bDataOnly);
    }
    // detach
    ctc_detach(ctc_unit);
    mt_detach(&mt_unit[0]);
    if (r) return r;
    
    return SCPE_OK;    
}

// return 0 if parsed ok, 1 if not parsed (comment, header, blank line, ...)
//                       -1 if invalid   
// 1 word per line text input file:
// 
// ; comment
// [-]NN NN NNNN NNNN NNNN[-]  comment
//
//   words are 16 digis or less, can be separated by any number of spaces. 
//   if less than 16 digits, a zero is assumed on the non indicated ones
//   Comment allowed on col 30 and beyond. If last card has less than 4 words, 
//   it will be left unpunched. 
//
// punched card format
// 
// column number:          1         2         3         4         5         6         7         8
//                12345678901234567890123456789012345678901234567890123456789012345678901234567890
//      contents:            [    word 1    ][    word 2    ][    word 3    ][    word 4    ]
//
//
int text_to_1w_per_card(char * lbuf, uint16 * image, int * nWordsAlreadyPunched)
{
    int i, bHi, digits, nWords; 
    char c;
    t_int64 d; 

    if ((lbuf == NULL) || (lbuf[0] == ';')) return 1; // no text line, or starts with ";" (commment)

    d=0; bHi=0; digits=0; nWords=0; 
    for (i=0;i<29;i++){
        c=lbuf[i];
        if ((c==0) || (c==10) || (c==13)) break; 
        if ((c==9) || (c ==' ')) continue; // ignore tab/spaces 
        if (nWords) {
            if ((c>='0') && (c<='9')) {sim_messagef (SCPE_ARG, "More that 16 digits in word (at pos %d)\n", i+1);}
            else sim_messagef (SCPE_ARG, "Invalid char '%c' (code %d) at pos %d. Comment area starts at column 30\n", (c<32) ? 32:c, c, i+1);
            return -1;
        }
        if (c=='-') {
            if ((digits==0) && (bHi==0)) {
                bHi=1; 
                continue; // minus before word
            }
            sim_messagef (SCPE_ARG, "Minus sign not at the beggining of word (at pos %d)\n", i+1);
            return -1;
        }
        if ((c<'0') || (c>'9')) {
            // non digits on word area generates an error
            sim_messagef (SCPE_ARG, "Invalid char '%c' (code %d) at pos %d\n", (c<32) ? 32:c, c, i+1);
            return -1;
        }
        d=d * 10 + c - '0';
        digits++;
        if (digits<16) continue;
        c=lbuf[i+1];
        if (c=='-') {
            bHi |=2;
            i++;
        }
        if ((c>='0') && (c<='9')) {
            sim_messagef (SCPE_ARG, "More that 16 digits in word (at pos %d)\n", i+1);
            return -1;
        }
        nWords++;
    }
    if (digits == 0) return 1; // no word to be punched;
    // complete word up to 16 digits adding zeroes on the right 
    while (digits<16) { d=d*10; digits++; }
    // punch the word in card
    PunchWord(bHi, d, *nWordsAlreadyPunched, image);
    *nWordsAlreadyPunched += 1; // incr number of words punched in image
    return 0;
}


// return 0 if parsed ok, 1 if not parsed (comment, header, blank line, ...)
//                       -1 if invalid   
// 4 word per line text input file:
// 
// ; comment
// [-]NNNNNNNNNNNNNNNN[-]  [-]NNNNNNNNNNNNNNNN[-]  [-]NNNNNNNNNNNNNNNN[-]  [-]NNNNNNNNNNNNNNNN[-]   comment
//
// words must be exactly 16 digits each. Spaces into words are NOT allowed. If minus
// sign is present, must be placed next to digit without spaces
// spaces before/after words are allowed. can have less than 4 words (missing words assumed to be zero)
//
// punched card format
// 
// column number:          1         2         3         4         5         6         7         8
//                12345678901234567890123456789012345678901234567890123456789012345678901234567890
//      contents:            [    word 1    ][    word 2    ][    word 3    ][    word 4    ]
//
//
int text_to_4w_per_card(char * lbuf, uint16 * image)
{
    int i, nWords, bHi, digits; 
    char c;
    uint16 h;
    t_int64 d; 

    if ((lbuf == NULL) || (lbuf[0] == ';')) return 1; // no text line, or starts with ";" (commment)

    for (i=0;i<80;i++) image[i]=0;

    d=0; bHi=0; digits=0; nWords=0; 
    for (i=0;;i++){
        c=lbuf[i];
        if ((c==0) || (c==10) || (c==13)) break; 
        if ((c==9) || (c ==' ')) {
            if (digits==0) continue; // skip tab/spaces between words
            // spaces between words generates an error
            sim_messagef (SCPE_ARG, "Space in the middle of a word not allowed (at pos %d)\n", i+1);
            return -1;
        }
        if (c=='-') {
            if ((digits==0) && (bHi==0)) {
                bHi=1; 
                continue; // minus before word
            }
            sim_messagef (SCPE_ARG, "Minus sign not at the beggining of word (at pos %d)\n", i+1);
            return -1;
        }
        if ((c<'0') || (c>'9')) {
            if (digits==0) break; // non digits -> comments -> end of words in line
            // non digits between words generates an error
            sim_messagef (SCPE_ARG, "Invalid char '%c' (code %d) at pos %d\n", (c<32) ? 32:c, c, i+1);
            return -1;
        }
        d=d * 10 + c - '0';
        digits++;
        if (digits<16) continue;
        c=lbuf[i+1];
        if (c=='-') {
            bHi |=2;
            i++;
        }
        if ((c>='0') && (c<='9')) {
           sim_messagef (SCPE_ARG, "More that 16 digits in word (at pos %d)\n", i+1);
            return -1;
        }
        if (nWords==4) {
           sim_messagef (SCPE_ARG, "More that 4 words in line (at pos %d)\n", i+1);
           return -1;
        }
        // punch the word in card
        for (digits=0;digits<16;digits++) {
            c = d % 10 + '0'; d=d / 10; 
            h = sim_ascii_to_hol(c);
            if ((digits==0) && ((bHi & 2)!=0)) h |= 0x400; // set hi punch
            if ((digits==15) && ((bHi & 1)!=0)) h |= 0x400; // set hi punch
            image[11 + nWords * 16 + (15-digits)] = h;
        }
        d=0; bHi=0; digits=0; nWords++;
    }
    if (digits) {
        sim_messagef (SCPE_ARG, "Partial word with only %d digits out of 16 \n", digits);
        return -1;
    }
    if (nWords==0) return 1; // no words punched;
    return 0;

}


int GetNum(char * lbuf, int buf0, int buf1, int nNum)
{
    int i, n, val; 
    char c;

    n = strlen(lbuf); 
    if (n <= buf0) return 0; // lbuf str too short
    if (n <= buf1) buf1=n-1; 

    // skip leading spaces
    while (1) {
        if (lbuf[buf0]!=' ') break; 
        buf0++; 
        if (buf0 > buf1) return 0;
    }

    val = 0;
    if (nNum < 0) {
        // allow 1 decimal, return number x10. 
        nNum=0;
        for (i=buf0; i<=buf1; i++) {
            c=lbuf[i]; 
            if ((c >= '0') && (c <= '9')) {
                if (nNum==0) {
                   val = val * 10 + c - '0'; // regular digit
                   if (val > 1000*1000) break; // max 6 digits
                } else {
                   val = val * 10 + c - '0'; // decimal digit
                   return val; 
                }
            } else if ((c == '.') && (nNum==0)) {
                nNum=1; 
            } else {
                break; // no 0..9 or '.'
            }
        }
        return val * 10; // end of number, no decimal digit
    }
    // if nNum > 1, skip group of digits
    while (1) {
        if (nNum == 1) break; 
        // skip group of digits
        while ((lbuf[buf0]>='0') && (lbuf[buf0]<='9')) {
           buf0++; 
           if (buf0 > buf1) return 0;
        }
        // skip non digits that follows the group of digits (number separation)
        while ((lbuf[buf0]<'0') || (lbuf[buf0]>'9')) {
           buf0++; 
           if (buf0 > buf1) return 0;
        }
        nNum--;
    }
    // now buf0 point to first digit of group of digits number nNum
    for (i=buf0; i<=buf1; i++) {
        c=lbuf[i]; 
        if ((c >= '0') && (c <= '9')) {
            val = val * 10 + c -'0';
            if (val > 1000*1000) break; // max 6 digits
        } else {
            break; 
        }
    }
    return val; 
}


// put value val (with given digits, max 6, no sign) in cbuf at column nCol (=cbuf[nCol-1])
// if ndigits<0, pad with zero
void PutNum(char * cbuf, int val, int nCol, int digits)
{ 
    int n, Pad; 

    if (digits < 0) {
        Pad=1; digits=-digits;
    } else Pad=0;

    n=nCol-1+(digits-1);
    while (1) {
        if (digits==0) break; 
        cbuf[n]=val % 10 + '0';
        val = val / 10; 
        if ((val==0) && (Pad==0)) break; 
        digits--;
        n--;
    }
}

// return 0 if parsed ok, 1 if not parsed (comment, header, blank line, ...)
// PERT text input file:
// 
// ; comment
// For office        |  Activity Identification  |   Time Interval Estimates   |                 | 
// use only          +-------+-------------+-----+---------+---------+---------+    Schedulled   |
//                   |  Beggining  |  Ending     | Opti-   | Most    | Pessi-  |   Completition  | Remarks
//             Report|  Event No.  |  Event No.  | mistic  | Likely  | mistic  |      Date       |
//               code|             |             | (weeks) | (weeks) | (weeks) |                 |
// (1) (2) (3) | (4) |     (B)     |     (C)     |   (D)   |   (E)   |  (F)    |       (G)       |   (H)
// ------------+-----+-------------+-------------+---------+---------+---------+-----------------+-----------
// 12   13-16  |  43 | 18   -   26 | 34   -   42 | 44 - 47 | 48 - 51 | 52 - 55 | 60     -     65 |
// ------------+-----+-------------+-------------+---------+---------+---------+-----+-----+-----+-----------
// x    xx xx  |  x  | xxx-xxx-xxx | xxx-xxx-xxx |  xxx.x  |  xxx.x  |  xxx.x  | Mo. | Day | Yr. |
// ------------+-----+-------------+-------------+---------+---------+---------+-----------------+-----------
// 1                   022-106-000   022-106-745    038.0     041.0     044.6    03    18    60    New activity
//
//           1         2         3         4         5         6         7         8         9
// 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
// position in lbuf[n]
//
//
// punched card format
// 
// column number:          1         2         3         4         5         6         7         8
//                12345678901234567890123456789012345678901234567890123456789012345678901234567890
//      contents:            1     022106000       022106745 038004100446    031860
//
int text_to_pert_card(char * lbuf, uint16 * image)
{
    int blank, i, n1, n2, n3; 
    char cbuf[81];
    char c;
    uint16 h;

    // init resulting card buff in ascii to spaces
    for (i=0;i<80; i++) cbuf[i]=' '; 
    cbuf[80]=0;  // end of string (for debug and tracing)

    if ((lbuf == NULL) || (lbuf[0] == ';')) return 1; // no text line, or starts with ";" (commment)
    if (lbuf[0] == 1) {
        // start of block
        // each transaction is 4 words wide. CTC cand hold 100 words. 2 words are needed for begin/
        // end block word, so 98 remains. 98 div 4 -> 24 transactions max per block
        // so 24 x 4 = 96 words per block
        PutNum(cbuf, 191, 60,-4); // tape 01 opcode 91 (write output)
        PutNum(cbuf, 101, 64,-4); // start addr 
        PutNum(cbuf, 196, 68,-4); // end addr 
        PutNum(cbuf, 000, 72,-4); // block num
        cbuf[59]='-';             // hi punch in col 60 (=cbuf[59]) to mark start of block
    } else if (lbuf[0] == 2) {
        // end of block
        PutNum(cbuf, 191, 12,-4); // tape 01 opcode 91 (write output)
        PutNum(cbuf, 101, 16,-4); // start addr 
        PutNum(cbuf, 196, 20,-4); // end addr 
        PutNum(cbuf, 000, 24,-4); // block num
        cbuf[26]='-';             // hi punch in col 27 (=cbuf[26]) to mark end of block
    } else if (lbuf[0] == 3) {
        // end of file mark
        PutNum(cbuf, 191, 12,-4); // tape 01 opcode 91 (write output)
        PutNum(cbuf, 000, 16,-4); // start addr 
        PutNum(cbuf, 000, 20,-4); // end addr 
        PutNum(cbuf, 000, 24,-4); // block num
        cbuf[11]='-';             // hi punch in col 12 (=cbuf[11]) 
        cbuf[26]='-';             // hi punch in col 27 (=cbuf[26]) 
    } else {
        blank=1; 
        for (i=0;i<88; i++) {
            c=lbuf[i]; 
            if ((c==0) || (c==10) || (c==13)) break; 
            if (c==9) c = ' '; // convert tab to space
            if ((c=='-') && (lbuf[i+1]=='-')) {blank=1; break;} // double -- -> header line, ignore it
            if ((c=='|') && (c=='+'))   {blank=1; break;} // + or | -> header line, ignore it
            if ((c != ' ') && (c != '.') && (c != '-') && 
                ((c < '0') || (c > '9')) ) {blank=1; break;} // alpha char -> comment/header line
            if ((c >= '0') && (c <= '9')) blank=0;           // some data in line -> not blank
        }
        if (blank) return 1; 

        n1=GetNum(lbuf, 0, 11, 1); // get first int in range lbuf[0..11] columns (1) card code = transaction code
        n2=GetNum(lbuf, 0, 11, 2); // get second             lbuf[0..11]         (2) RS = Resource Code
        n3=GetNum(lbuf, 0, 11, 3); // third                  lbuf[0..11]         (3) PR = Preferece Rate
        PutNum(cbuf, n1, 12,-1);   // put n1 (1 digit) in cbuf at column 12 (=cbuf[11]), pad with zero
        if (n2) PutNum(cbuf, n2, 13,2);    // put n2 (2 digits) in cbuf at column 13-14 (=cbuf[12]), 
        if (n2) PutNum(cbuf, n3, 15,2);    // put n3 (2 digits) in cbuf at column 15-16 (=cbuf[14])

        n1=GetNum(lbuf, 13, 17, 1); // column (4) report code
        PutNum(cbuf, n1, 43, 1);    // put report code in cbuf at column 43

        n1=GetNum(lbuf, 19, 31, 1); // get first int in range lbuf[13..25] column (B) beggining event
        n2=GetNum(lbuf, 19, 31, 2); // get second             lbuf[13..25]
        n3=GetNum(lbuf, 19, 31, 3); // third                  lbuf[13..25]
        if (n1+n2+n3) {
            PutNum(cbuf, n1, 18,-3);    // put n1 (3 digits) in cbuf at column 18 (=cbuf[17])
            PutNum(cbuf, n2, 21,-3);    // put n2 (3 digits) 
            PutNum(cbuf, n3, 24,-3);    // put n3 (3 digits) 
        }

        n1=GetNum(lbuf, 33, 45, 1); //  column (C) ending event event number
        n2=GetNum(lbuf, 33, 45, 2);     
        n3=GetNum(lbuf, 33, 45, 3); 
        if (n1+n2+n3) {
            PutNum(cbuf, n1, 34,-3);   
            PutNum(cbuf, n2, 37,-3);  
            PutNum(cbuf, n3, 40,-3);  
        }
       
        n1=GetNum(lbuf, 47, 55, -4); // column (D) optimistic weeks (x10, 4 digits, allow 1 decimal)
        n2=GetNum(lbuf, 57, 65, -4); // column (E) most likely weeks 
        n3=GetNum(lbuf, 67, 75, -4); // column (F) pessimistic weeks 
        PutNum(cbuf, n1, 44, 4);   
        PutNum(cbuf, n2, 48, 4);   
        PutNum(cbuf, n3, 52, 4);   

        n1=GetNum(lbuf, 77, 81, 1); // column (G) month  
        n2=GetNum(lbuf, 83, 87, 1); //            day    
        n3=GetNum(lbuf, 89, 93, 1); //            year   
        if (n1+n2+n3) {
            PutNum(cbuf, n1, 60,-2);   
            PutNum(cbuf, n2, 62,-2);   
            PutNum(cbuf, n3, 64,-2);   
        }
    }
    // now cbuf has the contents of card to punch
    /* punch the cards */
    for (i=0; i<80; i++) {
        c = cbuf[i];
        if (c == ' ') {
            // no punch
            image[i] = 0;
        } else if (c == '-') {
            // zero with hi punch
            image[i] = 0x400;
        } else {
            // punch char
            h = sim_ascii_to_hol(c);
            image[i] = h;
        }
    }
    return 0;
}

// print in string n value as [-]NNN.N, blank if zero
// leave spc spaces afterwards as separator
void PutNNNN(char * line, int n, int nPad)
{
    char c; 

    if (n==0) {
        // NNN.N- -> 6 chars
        sprintf(&line[strlen(line)], "      ");
    } else {
        if (n<0) {c='-';n=-n;} else c=' ';
        sprintf(&line[strlen(line)], 
            "%3d.%1d%c", 
            n / 10, n % 10,c);
    }
    n = strlen(line);
    while (nPad > 0) {
        line[n++] = ' '; line[n]=0;
        nPad--;
    }
}

// add a CR at the beggining of line
void PutLineBreak(char * line)
{
    char line2[1024]; 
    sprintf(line2, "\n%s", line);
    strcpy(line, line2);
}

// PERT text output:
// sumulates the wiring for offline printer + paper form background
// +-------------+---------+---------+---------+---------+------+---------------------------
// |     EVENT   |   TE    |   TL    |  TL-TE  |   TS    |  PR  | Schedulled date (MM/DD/YY)
// +-------------+---------+---------+---------+---------+------+---------------------------
// | xxx-xxx-xxx |  xxx.x  |  xxx.x  |  xxx.x  |  xxx.x  |  NN  | MM / DD / YY
// +-------------+---------+---------+---------+---------+------+-------------------
//   027-110-001      1.1-     29.3         0      12.1- |  99  | 09 / 26 / 58
//
// punched card format
// 
// column number:          1         2         3         4         5         6         7         8
//                12345678901234567890123456789012345678901234567890123456789012345678901234567890
//      contents:             027110001 10011002930000010121  99092658
//
//                           0022200001000030002270019600022700500621580000000000000000000000
void pert_card_to_text(char * cbuf, char * line)
{
    int ev1, ev2, ev3, rep, te, tl, tlte, ts, pr, mm, dd, yy; 
    static int last_ev1; 
    static int last_ev2; 

    if (cbuf == NULL) {
        // return header
        sprintf(line, "+-------------+-------+-------+-------+-------+----+----------------------\n" 
                      "|    EVENT    |  TE   |  TL   | TL-TE |  TS   | PR | Sched date (MM/DD/YY)\n"
                      "+-------------+-------+-------+-------+-------+----+----------------------");
        last_ev1 = last_ev1 = -1; 
        return; 
    }
    // parse cbuf to get info
    ev1=GetNum(cbuf, 12, 14, 1); // get each 3 digit group for event code
    ev2=GetNum(cbuf, 15, 17, 1);
    ev3=GetNum(cbuf, 18, 20, 1);
    rep=GetNum(cbuf, 21, 21, 1); // get report code
    te =GetNum(cbuf, 23, 26, 1); // get TE
    if (GetNum(cbuf, 22, 22, 1) == 1) te = -te; // apply sign
    tl =GetNum(cbuf, 28, 31, 1); // get TL
    if (GetNum(cbuf, 27, 27, 1) == 1) tl = -tl; // apply sign
    tlte =GetNum(cbuf, 33, 36, 1); // get TL-TE
    if (GetNum(cbuf, 32, 32, 1) == 1) tlte = -tlte; // apply sign
    ts =GetNum(cbuf, 39, 42, 1); // get TS
    if (GetNum(cbuf, 38, 38, 1) == 1) ts = -ts; // apply sign
    pr =GetNum(cbuf, 45, 46, 1); // get PR
    mm =GetNum(cbuf, 47, 48, 1); // get schedulle month
    dd =GetNum(cbuf, 49, 50, 1); // get           day
    yy =GetNum(cbuf, 51, 52, 1); // get           year

    sprintf(&line[strlen(line)], "  %03d-%03d-%03d   ", ev1, ev2, ev3); 
    PutNNNN(line, te, 2);
    PutNNNN(line, tl, 2);
    PutNNNN(line, tlte, 2);
    PutNNNN(line, ts, 2);
    sprintf(&line[strlen(line)], "%2d  ", pr);
    if (mm + dd + yy) {
       sprintf(&line[strlen(line)], " %02d / %02d / %02d",     mm, dd, yy);
    }
    if ((last_ev1 >= 0) && (last_ev1 != ev1)) {
        // change in 3 first digits of event -> add 3 blank lines
        PutLineBreak(line); // add a CR at the beggining of line
        PutLineBreak(line); 
        PutLineBreak(line); 
    } else if ((last_ev2 >= 0) && (last_ev2 != ev2)) {
        // change in second group of digits of event -> add blank lines
        PutLineBreak(line); // add a CR at the beggining of line
    }
    last_ev1=ev1; 
    last_ev2=ev2; 
              
}

// simulates the key punching process from textfile to deck 
// this deck will be processed by CTC to create a tape
// PUNCH PERT inputfile.txt TO deck.dck
//               read input file in readable text format 
//               generates a deck ready to be loaded in CTC
//               deck uses the fixed format stated in p54 of PERT Report Phase 2 pdf
//               cards are grouped in max 98 cards per bock. Will add automatically an
//               start of block, end of block and end of file cards
// PUNCH 4WORDS inputfile.txt TO deck.dck
//               input text format is 4 words per line
// PUNCH 1WORD inputfile.txt TO deck.dck
//               input text format is 1 word per line
t_stat keypunch_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    char fnText[4*CBUFSIZE];
    char fnDeck[4*CBUFSIZE];
    int format = 0; // formats to be used for Inputfile
    int nCards, nlin, nWords; 
    FILE *fText;
    char lbuf[255];
    uint16 image[80];    
    t_stat              r;

    // get formatting
    if (cptr==NULL) return sim_messagef (SCPE_ARG, "Missign KeyPunch formatting\n");;
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 

    cptr = get_glyph (cptr, gbuf, 0); // get formatting
    if (strcmp(gbuf, "PERT") == 0)      { format = 1; } else 
    if (strcmp(gbuf, "4WORDS") == 0)    { format = 2; }
    if (strcmp(gbuf, "1WORD") == 0)     { format = 3; }
    if (format == 0) {
        return sim_messagef (SCPE_ARG, "Unknown format \"%s\"\n", gbuf);
    }
        
    // get text input filename
    cptr = get_glyph_quoted (cptr, fnText, 0);              // read using get_glyph_quoted to do not 
                                                            // change the capitalization of file name
    if (fnText[0] == 0) return sim_messagef (SCPE_ARG, "Missing InputFile.txt filename\n");

    // get TO separator
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missign TO \n");;

    // get card deck output filename
    cptr = get_glyph_quoted (cptr, fnDeck, 0);
    if (fnDeck[0] == 0) return sim_messagef (SCPE_ARG, "Missing deck.dck filename\n");

    // open for read fnText
    fText = sim_fopen (fnText, "r");
    if (fText == NULL) {                         // open failed? 
        return sim_messagef (SCPE_ARG, "Cannot open for read \"%s\" filename\n", fnText);
    }

    // attach output deck file to ctc punch
    sim_switches |= SWMASK ('N');
    sim_switches |= SWMASK ('Q');
    r=ctc_attach(ctc_unit, fnDeck);
    if (r) {
        return sim_messagef (r, "Cannot open for write \"%s\" filename\n", fnDeck);
    }

    r=SCPE_OK;
    if (format==1) {
        // read PERT input file lines, parse them
        // add begin of block / end of block / end of file cards as needed. These cards does
        // not need to be in input text file
        nCards=0; 
        lbuf[0]=1; 
        text_to_pert_card(lbuf, image); // begin of block word
        r=sim_punch_card(ctc_unit, image);
        while ((r==0) && fgets (lbuf, sizeof(lbuf)-1, fText)) {
            r = text_to_pert_card(lbuf, image);
            if (r) {r=0; continue;} // comment, blank line, header ... ignore and process next text line            
            r = sim_punch_card(ctc_unit, image);
            if (r != CDSE_OK) break;    // abnormal termination on error
            nCards++;
            if (nCards >= 24) {
                nCards=0;
                lbuf[0]=2; 
                text_to_pert_card(lbuf, image); // end of block word
                r=sim_punch_card(ctc_unit, image);
                lbuf[0]=1; 
                text_to_pert_card(lbuf, image); // begin of block word
                r=sim_punch_card(ctc_unit, image);
            }
        }
        lbuf[0]=2; 
        text_to_pert_card(lbuf, image); // end of block word
        r=sim_punch_card(ctc_unit, image);
        // add end of file 
        lbuf[0]=3; 
        text_to_pert_card(lbuf, image); // end of file 
        r=sim_punch_card(ctc_unit, image);
    } else if (format==2) {
        // read 4words per line input file, parse them
        // DO NOT add begin of block / end of block / end of file cards. These cards should be
        // explicitelly present in input file
        nlin=0;
        nCards=0; 
        r=0;
        while ((r==0) && fgets (lbuf, sizeof(lbuf)-1, fText)) {
            nlin++;
            r = text_to_4w_per_card(lbuf, image);
            if (r==1) {
                r=0; continue; // lbuf is a comment, blank line, header ... ignore and process next text line
            } else if (r==-1) {
               // some error occurred during parsing of lbuf
               r=sim_messagef (SCPE_IERR, "Error in line %d: %s \n", nlin, lbuf);
               break;
            }
            r = sim_punch_card(ctc_unit, image);
            if (r != CDSE_OK) break;    // abnormal termination on error
            nCards++;
        }
    } else if (format==3) {
        // read 1 word per line input file, parse them
        // DO NOT add begin of block / end of block / end of file cards. These cards should be
        // explicitelly present in input file
        nlin=0;
        nCards=0; 
        r=0;
        memset(image, 0, sizeof(image)); nWords=0;
        while ((r==0) && fgets (lbuf, sizeof(lbuf)-1, fText)) {
            nlin++;
            r = text_to_1w_per_card(lbuf, image, &nWords);
            if (r==1) {
                r=0; continue; // lbuf is a comment, blank line, header ... ignore and process next text line
            } else if (r==-1) {
               // some error occurred during parsing of lbuf
               r=sim_messagef (SCPE_IERR, "Error in line %d: %s \n", nlin, lbuf);
               break;
            }
            if (nWords==4) {
                // 4 words generated, can punch a card
                r = sim_punch_card(ctc_unit, image);
                if (r != CDSE_OK) break;    // abnormal termination on error
                nCards++;
                // prepare new empty card
                memset(image, 0, sizeof(image)); nWords=0;
            }
        }
        if (nWords) {
            // punch last card if some words remains to be punched
            r = sim_punch_card(ctc_unit, image);
            nCards++;
        }
    } else return sim_messagef (SCPE_ARG, "Invalid format \"%d\"\n", format);

    fclose (fText);
    ctc_detach(ctc_unit);

    sim_printf("Punched %d cards \n", nCards);
    return r;
}

// simulates the offline printing process of a card deck generated by ctc
// if Outputfile.txt is present, then printout is stored in given file
// if CONSOLE is present, then printout is show in console
// at least one must de present
// PRINT PERT deck.dck TO [ Outputfile.txt ] [ CONSOLE ]
//                read input generated by CTC
//                deck uses the fixed format stated in p78 of PERT Report Phase 2 pdf
//                print out uses Beweekly report output format stated in p43 of PERT Report Phase 2 pdf
// PRINT 4WORDS deck.dck [ TO Outputfile.txt ]
//                print 4 words per line
// PRINT 1WORD deck.dck [ TO Outputfile.txt ]
//                print 1 word per line
t_stat printcards_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    char fnDeck[4*CBUFSIZE];
    char fnText[4*CBUFSIZE];
    int format = 0; // formats to be used for Inputfile
    FILE *fText;
    char cbuf[81], line[1024];
    uint16 image[80], h;   
    int i, n, nDigits, bHi, bBlank, nLin, bConsole;
    char c;
    t_int64 d;
    t_stat              r;

    // get formatting
    if (cptr==NULL) return sim_messagef (SCPE_ARG, "Missign Print-out formatting\n");;
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 

    cptr = get_glyph (cptr, gbuf, 0); // get formatting
    if (strcmp(gbuf, "PERT") == 0)      { format = 1; } else 
    if (strcmp(gbuf, "4WORDS") == 0)    { format = 2; }
    if (strcmp(gbuf, "1WORD") == 0)     { format = 3; }
    if (format == 0) {
        return sim_messagef (SCPE_ARG, "Unknown format \"%s\"\n", gbuf);
    }

    // get card deck input filename
    cptr = get_glyph_quoted (cptr, fnDeck, 0);              // read using get_glyph_quoted to do not 
                                                            // change the capitalization of file name
    if (fnDeck[0] == 0) return sim_messagef (SCPE_ARG, "Missing deck.dck filename\n");

    bConsole=0; fText=NULL;

    // get TO separator
    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missign TO \n");;
    // get text output filename
    cptr = get_glyph_quoted (cptr, fnText, 0);
    if (fnText[0] == 0) return sim_messagef (SCPE_ARG, "Missing Outputfile.txt filename\n");
    if (strcmp(gbuf, "CONSOLE") == 0) {
        bConsole=1; 
    } else {
        fText = sim_fopen (fnText, "w");
        if (fText == NULL) {                         // open failed? 
            return sim_messagef (SCPE_ARG, "Cannot open for write \"%s\" filename\n", fnText);
        }
        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        cptr = get_glyph (cptr, gbuf, 0); // get TO
        if (strcmp(gbuf, "CONSOLE") == 0) bConsole=1; 
    }

    // attach input deck file to ctc read
    sim_switches = SWMASK ('Q');
    r=ctc_attach(ctc_unit, fnDeck);
    if (r) {
        return sim_messagef (r, "Cannot open for read \"%s\" filename\n", fnDeck);
    }
    
    // read all cards 
    nLin=0;
    while (1) {
        if (nLin==0) { // print header, if any
            if (format==1) {
                // print PERT EVENT Biweekly report 
                pert_card_to_text(NULL, line);
                goto DoPrintLine;
            }
        }
        r = sim_read_card(ctc_unit, image);
        if ((r == CDSE_EOF) || (r == CDSE_EMPTY)) {
            r = SCPE_OK; break;             // normal termination on card file read finished
        } else if (r != CDSE_OK) {
            return sim_messagef (SCPE_IERR, "Error %d reading cards\n", r);
            break;                          // abnormal termination on error
        }
        // convert to ascii
        for (i=0; i<80; i++) {
            cbuf[i] = sim_hol_to_ascii(image[i]);
        }
        cbuf[80] = 0; // terminate string
        line[0]=0;
        if (format==1) {
            // print PERT EVENT Biweekly report 
            pert_card_to_text(cbuf, line);
        } else if ((format==2) || (format==3)) {
            // print 1word or 4words per line 
            i=11; 
            for (n=0;n<4;n++) {
                d=0; bHi=0; bBlank=1; 
                for(nDigits=0;nDigits<16;nDigits++) {
                    h = image[i];
                    c = sim_hol_to_ascii(h);
                    if (c != ' ') bBlank=0;
                    c = sim_hol_to_ascii(h  & 0x1ff);
                    if ((c < '0') || (c > '9')) c = '0';
                    if (h & 0x400) bHi |= (nDigits < 8) ? 1:2; 
                    i++;
                    d=d * 10 + c -'0';
                }
                if (bBlank) {
                    if (format==3) continue; ; // if 1 word per line, skip blank words
                    sprintf(&line[strlen(line)], " %18s", "");  // blank word on card is printed also as blank
                } else {
                    sprintf(&line[strlen(line)], " %c%08d%08d%c", 
                       (bHi & 1) ? '-':' ', printfw(d), (bHi & 2) ? '-':' ');
                }
                if (format==3) sprintf(&line[strlen(line)], "\r\n");  // if 1 word per line, add end of line after each word                          
            }
            // remove last end of line, if any
            while (((n = strlen(line)) > 0) && (line[n-1] < 32)) line[n-1]=0;
        } else return sim_messagef (SCPE_ARG, "Invalid format \"%d\"\n", format);
        // lin is ready to be printed
     DoPrintLine:
        sprintf(&line[strlen(line)], "\r\n"); // add end of line
        if (fText) {
            sim_fwrite(line, 1, strlen(line), fText); // to output text file
        }
        if (bConsole) {
            sim_printf(line); // to console
        }
        nLin++;
    }
    if (fText) fclose (fText);

    return SCPE_OK;

}

/* Handle processing of ctc requests. */
uint32 ctc_cmd(UNIT *up, uint16 cmd, uint16 dev)
{
    return SCPE_OK;
}

t_stat ctc_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 cmd = uptr->u5 & MT_CMDMSK;

    return SCPE_OK;
}

t_stat ctc_attach(UNIT * uptr, CONST char *file)
{
    t_stat              r;

    if (uptr->flags & UNIT_ATT)         // remove current deck in read hopper before attaching
       sim_card_detach(uptr);           // the new one

    if (sim_switches & SWMASK ('N')) {                      
        uptr->flags &= ~UNIT_RO;
    } else {
        uptr->flags |= UNIT_RO;
    }

    r = sim_card_attach(uptr, file);
    if (SCPE_BARE_STATUS(r) != SCPE_OK) return r;

    return SCPE_OK;
}

t_stat ctc_detach(UNIT * uptr)
{
    return sim_card_detach(uptr);
}

const char * ctc_description(DEVICE *dptr)
{
   return "IBM NORC Card to Tape to Card Machine";
}


