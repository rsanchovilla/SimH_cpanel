/* MarkI_iodev.c: Ferranti Mark I computer PaperTape Photoread/Punch TelePrinter simulator

   Copyright (c) 2023, Roberto Sancho

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
   ROBERT M SUPNIK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   PTR          PaperTape Reader
   PTP          PaperTape Punch
   LPT          Teleprinter
   PERFORATOR   Offline Perforator
   DRUM

*/

#include "MarkI_defs.h"

DEVICE ptr_dev, ptp_dev, lpt_dev;

t_stat ptr_attach (UNIT *uptr, CONST char *cptr);
t_stat ptr_detach (UNIT *uptr);

t_stat ptp_attach (UNIT *uptr, CONST char *cptr);
t_stat ptp_detach (UNIT *uptr);

t_stat drum_attach (UNIT *uptr, CONST char *cptr);


/* PTR data structures

   ptr_dev      PTR device descriptor
   ptr_unit     PTR unit descriptor
*/

char  ptr_FileName[MAX_PATH+1];
int32 ptr_FileSize; 
char  ptr_buf[MAX_PTR_BUF] = {0};
uint32 ptr_move_start_tm0 = 0; // when paper tape reader movement starts ...

UNIT ptr_unit = {
    UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE+UNIT_RO, 0)
    };

MTAB ptr_mod[] = {
    { OPTION_ECHO, 0,             "Do not display in console chars read from ptr",     "NOECHO", NULL },
    { OPTION_ECHO, OPTION_ECHO,   "Do not display in console each char read from ptr", "ECHO", NULL },
    { 0 }
    };

DEVICE ptr_dev = {
    "PTR", &ptr_unit, NULL, ptr_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, NULL, 
    NULL, &ptr_attach, &ptr_detach
    };

/* PTP data structures

   ptp_dev      PTP device descriptor
   ptp_unit     PTP unit descriptor
*/

UNIT ptp_unit = {
        UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE+UNIT_DISABLE, 0) 
    };

DEVICE ptp_dev = {
    "PTP", &ptp_unit, NULL, NULL,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, NULL,
    NULL, &ptp_attach, &ptp_detach
    };

// computer request to punch a char (ascii) into papertape punch
void ptp_char(int ch)
{
    if ((ptp_unit.flags & UNIT_ATT) == 0)                   /* attached? */
        return;                                             /* no, ignore punch request */
    putc (ch, ptp_unit.fileref); 
    if (ch==10) {
        // new line -> reset counter
        ptp_unit.pos=0; 
    }
    ptp_unit.pos++; 
    if (ptp_unit.pos>=LPT_COLUMNS-1) {
        // add a CR LF each 69 chars punched
        // to allow view the punched file in a text editor
        // <CR><LF> are skipped by ptr when reading 
        putc (13, ptp_unit.fileref); 
        putc (10, ptp_unit.fileref); 
        ptp_unit.pos=0; 
    }
}

t_stat ptp_attach (UNIT *uptr, CONST char *cptr)
{
    uptr->pos=0; 
    return attach_unit (uptr, cptr);
}

t_stat ptp_detach (UNIT *uptr)
{
    uptr->pos=-1; 
    return detach_unit (uptr);                        /*   and attached the indicated file */
}

/* LPT data structures

   lpt_dev      lpt device descriptor
   lpt_unit     lpt unit descriptor
   lpt_mod      lpt modifiers list
*/


UNIT lpt_unit = { 
    UDATA (NULL, UNIT_SEQ | UNIT_ATTABLE | UNIT_DISABLE, 0) 
    };
    
MTAB lpt_mod[] = {
    { OPTION_NOECHO, 0,               "display each printed line in console", "ECHO", NULL },
    { OPTION_NOECHO, OPTION_NOECHO,   "No echo printed line to console",      "NOECHO", NULL },
    { OPTION_NOPRINT, 0,              "add line on printer file",             "PRINT", NULL },
    { OPTION_NOPRINT, OPTION_NOPRINT, "do not use printer file",              "NOPRINT", NULL },
    { 0 }
    };

DEVICE lpt_dev = {
    "LPT", &lpt_unit, NULL, lpt_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, NULL,
    NULL, NULL, NULL,
    NULL, 0
    };

// buffer to hold last printed lines on lp
char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
int    lptPrintOutCount = 0; // total number of lines printed
int    lptPrintOutChCount = 0; // total number of chars printed in last line; 
int    lpt_chariage_pos = 0; // carriage pos 0..68
uint32 lpt_CR_start_tm0 = 0; // when <CR> movement starts ...
int    lpt_CR_from_pos = 0;  // ... from this pos

// computer request to read a char (ascii) from papertape photoreader
t_stat ptr_char(int *ch)
{
    int c, i; 

    ptr_move_start_tm0=0; 

    if (ptr_buf[0]) {
        // something has been fed to the ptr. Get it
        c=ptr_buf[0]; 
        // move buffer ahead
        for (i=1; i<(int) strnlen(ptr_buf, MAX_PTR_BUF); i++) {
            ptr_buf[i-1]=ptr_buf[i]; 
        }
        ptr_buf[strnlen(ptr_buf, MAX_PTR_BUF)-1]=0; 
        // return char
        *ch=c;
        if (ptr_unit.flags & OPTION_ECHO) sim_putchar(c); 
        return SCPE_OK; 
    }
    if ((ptr_unit.flags & UNIT_ATT) == 0)                   /* attached? */
        return STOP_PTR_UNATT;                                  /* nope */
    RdChar:
    c = getc (ptr_unit.fileref); 
    if (c==EOF) return STOP_EOF;                                    
    // skip space, <tab>, <cr> and <lf> or control chars
    if (c<=32) goto RdChar; 
    // is a comment?
    if (c==';') {
        // yes, skip the current line til end
        while (1) {
            c = getc (ptr_unit.fileref); 
            if (c==EOF) return STOP_EOF;                                
            if ((c==13) || (c==10)) break; 
        }
        goto RdChar; 
    }
    ptr_unit.pos = ftell (ptr_unit.fileref);

    if (ptr_FileSize < 0) ptr_FileSize=sim_fsize(ptr_unit.fileref);
    ptr_move_start_tm0=sim_os_msec(); // timestamp when char has been readed from papertape reader. used in cpanel

    if (ptr_unit.flags & OPTION_ECHO) sim_putchar(c); 
    *ch=c; 
    return SCPE_OK; 
}

    
t_stat ptr_attach (UNIT *uptr, CONST char *cptr)
{
    char *c;

    // set filename+extension of attached paper tape file, to be show in cpanel info panel
    c = sim_filepath_parts (cptr, "nx");
    strlcpy (ptr_FileName, c, sizeof (ptr_FileName));
    free (c);
    ptr_FileSize=-1; 
    ptr_unit.pos=-1; 

    return attach_unit (uptr, cptr);
}

t_stat ptr_detach (UNIT *uptr)
{
    ptr_FileName[0] = 0; // clear string
    ptr_FileSize=-1; 
    ptr_unit.pos=-1; 
    ptr_buf[0] = 0; // removes anything feed to ptr
    return detach_unit (uptr);                        /*   and attached the indicated file */
}

// coputer request to print a char (ascii)
void lpt_char(int ch)
{
    char sLin[3];
    int i,n; 

    if (ch==26) {
        // reset printer
        if (lptPrintOutChCount) {
            lptPrintOutCount++; // incr num of lines printed
            lptPrintOutChCount=0; // line is empty 
        }
        return; 
    }

    sLin[0]=(char) ch; sLin[1]=0;
    if ((uint32)(lpt_unit.flags & OPTION_NOPRINT)) {
        // do not use printer file
    } else if (lpt_unit.flags & UNIT_ATT) {
        // printout will be directed to file attached to unit, if any
        sim_fwrite(sLin, 1, 1, lpt_unit.fileref);
    }

    if (lpt_unit.flags & OPTION_NOECHO) {
        // do not echo to console
    } else {
        // echo to console
        sim_putchar(sLin[0]);
    }

    // last lptPrintOutMAX printed lines will be saved on circular buffer lptPrintOut
    // acts as mapped memory for 20 last lines printed
    if (ch==13) {
        // new line
        lptPrintOutCount++; // incr num of lines printed
        lptPrintOutChCount=0; // line is empty 
        n = LPT_COLUMNS * (lptPrintOutCount % lptPrintOutMAX); 
        for (i=0;i<LPT_COLUMNS;i++) lptPrintOut[n++]=32; // clear line in buffer
    } else if (ch >= 32) {
        // add char to last line
        n = LPT_COLUMNS * (lptPrintOutCount % lptPrintOutMAX) + lptPrintOutChCount; 
        lptPrintOut[n]=ch;
        lptPrintOutChCount++;
        if (lptPrintOutChCount >= LPT_COLUMNS) { // safety. Should not occurs, as printer generates autocr at col 68
            lptPrintOutCount++; // incr num of lines printed
            lptPrintOutChCount=0; // line empty
        }
    }
}

/* PERFORATOR data structures

   perforator_dev      PTP device descriptor
   perforator_unit     PTP unit descriptor
   perforator_mod      PTP modifiers
*/

UNIT perforator_unit = {
        UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE, 0) 
    };

MTAB perforator_mod[] = {
    { OPTION_NOECHO, 0,                 "display each printed line in console", "ECHO", NULL },
    { OPTION_NOECHO, OPTION_NOECHO,     "No echo printed line to console",      "NOECHO", NULL },
    { OPTION_USE_WC_J, 0,               "use Warning Char K when punching the routine", "USE_WC_K", NULL },
    { OPTION_USE_WC_J, OPTION_USE_WC_J, "use Warning Char J when punching the routine", "USE_WC_J", NULL },
    { 0 }
    };

DEVICE perforator_dev = {
    "PERFORATOR", &perforator_unit, NULL, perforator_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, NULL,
    NULL, NULL, NULL
    };

uint32 perforator_start_tm0 = 0; // when perforator usage starts ...

// Simulates typing on onfline perforator the text in sLin
// Perforator will punch the text to a papertape, to be attached later to computer ptr
// Will echo text to SimH console (if echo set)
// will echo Input line (bEchoInput=1) or output line (if bEchoInput=0)
// if bQuietMode=1 -> will not output echo
t_stat perforator_typein(char * sLinInput, int bEchoInput, int bQuietMode)
{
    int len; 
    char sLin[1024]; 
    char c; 

    if ((perforator_unit.flags & UNIT_ATT) == 0)                   /* attached? */
        return sim_messagef (SCPE_NOATT, "No file attached to receive punching\n");

    // look for any comment (char ';')
    len=0; 
    while (c=sLinInput[len]) {
        if (c=='|') c=' '; // allways convert | to space
        if (c==';') break; // start of comment (; char) ends the line 
        sLin[len++]=c;
        if (len >= 1000) break; 
    }
    sLin[len]=0; 

    // punch sLin on attached file
    sim_fwrite(sLin, 1, len, perforator_unit.fileref);
    sim_fwrite("\r\n", 1, 2, perforator_unit.fileref);

    // print sLin on console
    if ((bQuietMode) || (perforator_unit.flags & OPTION_NOECHO)) {
        // do not echo to console
    } else if (bEchoInput) {
        // echo input line
        sim_printf("%s\r\n", sLinInput);
    } else {
        // echo output to console
        sim_printf("%s\r\n", sLin);
    }
    return SCPE_OK; 
}

/* DRUM data structures

   drum_dev      DRUM device descriptor
   drum_unit     DRUM unit descriptor
*/

UNIT drum_unit = {
        UDATA (NULL, UNIT_ATTABLE, 0) 
    };

MTAB drum_mod[] = {
    { OPTION_NOPERSISTENT, 0,                    "DRUM writes are saved in drum file",                   "PERSISTENT", NULL },
    { OPTION_NOPERSISTENT, OPTION_NOPERSISTENT,  "DRUM writes done in RAM, but not saved in drum file",  "NOPERSISTENT", NULL },
    { 0 }
    };

DEVICE drum_dev = {
    "DRUM", &drum_unit, NULL, drum_mod,
    1, 10, 31, 1, 8, 8,
    NULL, NULL, NULL,
    NULL, &drum_attach, NULL
    };

void drum_write_track(int trk)
{
    int pos, flen; 

    if (trk >= DRUM_TRACKS) {
        return; // writing on non-existant track
    }
    if ((drum_unit.flags & UNIT_ATT) == 0)                   /* attached? */
        return;                                      /* no, just exit. drum in operating in ram */

    if (drum_unit.flags & OPTION_NOPERSISTENT)              /* are drum writes persistent? */
        return;                                     /* no, just exit. drum in operating in ram */

    if (trk < 0) {
        // dump the whole DRUM in drum file
        flen = pos = 0; 
    } else {
        // each track holds 128 20-bit words (mapped as 128 32bit value on file)
        pos = trk * 128 * sizeof(&DRUM[0]);	
        flen = sim_fsize(drum_unit.fileref);
    }
    if (flen <= pos) {
        // expand file by dumping the whole DRUM var file
        sim_fseek(drum_unit.fileref, 0, SEEK_SET);
        sim_fwrite(&DRUM, 1, sizeof(DRUM), drum_unit.fileref); 
    } else {
        // save track
        sim_fseek(drum_unit.fileref, pos, SEEK_SET);
        sim_fwrite(&DRUM[trk*128], 128, sizeof(&DRUM[0]), drum_unit.fileref); 
    }
}

t_stat drum_attach (UNIT *uptr, CONST char *cptr)
{
    t_stat r; 
    int flen; 
    
    r=attach_unit (uptr, cptr);
    if (r) return r; 
    
    // read the drum on mem
    flen = sim_fsize(drum_unit.fileref);
    if (flen == 0) {
       // new file       
    } else {
       // read drum file into DRUM var ram
       sim_fread(&DRUM, 1, sizeof(DRUM), drum_unit.fileref); 
    }
    return SCPE_OK; 
}

