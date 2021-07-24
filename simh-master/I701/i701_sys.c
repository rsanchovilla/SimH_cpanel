/* i701_sys.c: IBM 701 Simulator system interface.

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
#include "sim_card.h"
#include <ctype.h>

/* SCP data structures and interface routines

sim_name             simulator name string
sim_PC               pointer to saved PC register descriptor
sim_emax             number of words for examine
sim_devices          array of pointers to simulated devices
sim_stop_messages    array of pointers to stop messages
sim_load             binary loader
*/

char                sim_name[] = "IBM 701";

REG                *sim_PC = &cpu_reg[0];

int32               sim_emax = 1;

DEVICE             *sim_devices[] = {
    &cpu_dev,
    &cdr_dev,
    &cdp_dev,
    &mt_dev,
    &lp_dev,
#if defined(CPANEL)
    &cp_dev,
#endif
    NULL
};

/* Device addressing words */
DIB  cdr_dib = { 1, &cdr_cmd, NULL };
DIB  cdp_dib = { 1, &cdp_cmd, NULL };
DIB  mt_dib  = { 4, &mt_cmd, &mt_ini };
DIB  lp_dib  = { 1, &lp_cmd, NULL };

/* Simulator stop codes */
const char         *sim_stop_messages[] = {
    "Unknown error",
    "Program Stop",
    "Host I/O Error",
    "Unknown Opcode",
    "Division by Zero", 
    "Division Check",
    "Copy Check",
    "Tape Check",
    0
};


static t_stat deck_cmd(int32 arg, CONST char *buf);
t_stat i701_exdep_cmd (int32 flag, CONST char *cptr);
t_stat set_csw_btn_cmd(int32 flag, CONST char *cptr); 

#define HLP_STEP        "*Commands Running_A_Simulated_Program STEP"

static CTAB aux_cmds [] = {
    /*    Name          Action Routine     Argument   Help String */
    /*    ----------    -----------------  ---------  ----------- */
    { "STEP",       &run_cmd,          RU_STEP,   HLP_STEP,       NULL, &run_cmd_message },
    { "DEPOSIT",    &i701_exdep_cmd,   EX_D,      "*Commands Examining_and_Changing_State",    NULL, NULL },
    { "EXAMINE",    &i701_exdep_cmd,   EX_E,      "*Commands Examining_and_Changing_State",    NULL, NULL },
    { "CARDDECK",   &deck_cmd,         0,         "Card Deck Operations"    },
    { "SWITCH",     &set_csw_btn_cmd,  1,         "Set switch",                          },
    { "DEPRESS",    &set_csw_btn_cmd,  2,         "Press button on Console",             },
    { "PRESS",      &set_csw_btn_cmd,  2,         "Press button on Console",             },
    { NULL }
};

/* Simulator debug controls */
DEBTAB              dev_debug[] = {
    {"CMD", DEBUG_CMD},
    {"DATA", DEBUG_DATA},
    {"DETAIL", DEBUG_DETAIL},
    {"EXP", DEBUG_EXP},
    {0, 0}
};


uint16          ascii_to_hol[128] = {
    /* Control                              */
    0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,    /*0-37*/
    /*Control*/
    0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,
    /*Control*/
    0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,
    /*Control*/
    0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,0xf000,
    /*  sp      !      "      #      $      %      &      ' */
    /* none   Y28    78     T28    Y38    T48    XT     48  */
    0x000, 0x600, 0x006, 0x282, 0x442, 0x222, 0xA00, 0x022,     /* 40 - 77 */
    /*   (      )      *      +      ,      -      .      / */
    /* T48    X48    Y48    X      T38    T      X38    T1  */
    0x222, 0x822, 0x422, 0x800, 0x242, 0x400, 0x842, 0x300,
    /*   0      1      2      3      4      5      6      7 */
    /* T      1      2      3      4      5      6      7   */
    0x200, 0x100, 0x080, 0x040, 0x020, 0x010, 0x008, 0x004,
    /*   8      9      :      ;      <      =      >      ? */
    /* 8      9      58     Y68    X68    38     68     X28 */
    0x002, 0x001, 0x012, 0x40A, 0x80A, 0x042, 0x00A, 0x882,
    /*   @      A      B      C      D      E      F      G */
    /*  82    X1     X2     X3     X4     X5     X6     X7  */
    0x022, 0x900, 0x880, 0x840, 0x820, 0x810, 0x808, 0x804,     /* 100 - 137 */
    /*   H      I      J      K      L      M      N      O */
    /* X8     X9     Y1     Y2     Y3     Y4     Y5     Y6  */
    0x802, 0x801, 0x500, 0x480, 0x440, 0x420, 0x410, 0x408,
    /*   P      Q      R      S      T      U      V      W */
    /* Y7     Y8     Y9     T2     T3     T4     T5     T6  */
    0x404, 0x402, 0x401, 0x280, 0x240, 0x220, 0x210, 0x208,
    /*   X      Y      Z      [      \      ]      ^      _ */
    /* T7     T8     T9     X58    X68    T58    T78     28 */
    0x204, 0x202, 0x201, 0x812, 0x20A, 0x412, 0x406, 0x082,
    /*   `      a      b      c      d      e      f      g */
    0x212, 0xB00, 0xA80, 0xA40, 0xA20, 0xA10, 0xA08, 0xA04,     /* 140 - 177 */
    /*   h      i      j      k      l      m      n      o */
    0xA02, 0xA01, 0xD00, 0xC80, 0xC40, 0xC20, 0xC10, 0xC08,
    /*   p      q      r      s      t      u      v      w */
    0xC04, 0xC02, 0xC01, 0x680, 0x640, 0x620, 0x610, 0x608,
    /*   x      y      z      {      |      }      ~    del */
    /*                     Y78     X78    78     79         */
    0x604, 0x602, 0x601, 0x406, 0x806, 0x006, 0x005,0xf000
};

uint16   sim_ascii_to_hol(char c)
{
    return ascii_to_hol[c & 127];
}

char     sim_hol_to_ascii(uint16 hol)
{
    int c;
    hol = hol & 0x0fff; // ignore extra high bits, if any
    if (hol == 0xa00) return '?'; // +0
    if (hol == 0x600) return '!'; // -0
    for (c=31;c<127;c++) {
        if (ascii_to_hol[c] == hol) {
            // take in consideration the aliases between hol and ascii to return 
            // char as for 026 FORT charset
            // hol = 0x022   -> 8-4   punches -> "-" or "'" or "@".   Must be "-"
            // hol = 0x222   -> 0-8-4 punches -> "(" or "%".          Must be "("  
            if (c == '%') {c = '(';} else
                if (c == '@') {c = '-';} else
                    if (c == '\'') {c = '-';};
            return c;
        }
    }
    return ' ';
}

// if Echo = 0 -> only print card image in debug log
//           1 -> print card image as text in console
//           2 -> print card image as text+octal in console
//           3 -> print card image as text+octal+binary in console
// if cardtext not NULL, return card's text in it
void echo_cardimage(DEVICE * dptr, uint16 * image, int EchoLevel, char * msg, char * cardtext)
{
    char crow[15] = "9876543210XY"; 
    char CardHoles[80 * 12];
    char cbuf[100], cbuf1[10], cbuf2[10];
    uint16 hol;
    int col, row, i, n1, n2, SkipCols; 
    int d, w[4];
    char c; 

    SkipCols = cdr_skip_cols();

    for (i=0; i<80; i++) {
        cbuf[i] = c = sim_hol_to_ascii(image[i]);
        if (cardtext) cardtext[i] = c;
    }
    cbuf[80] = 0; // terminate string
    if (cardtext) cardtext[80] = 0;

    if (EchoLevel > 0) {  
        if ((msg) && (msg[0]) && (EchoLevel > 1)) {
            cbuf[65]=0; // shorten to fit in line
            sim_printf("%s: %s\n", msg, cbuf);
        } else {
            cbuf[79]=0; // no msg -> shorten 1 to avoid extra cr
            sim_printf("%s\n", cbuf);
        }
    }

    if (SkipCols==0) { 
        sim_debug(DEBUG_DETAIL, dptr, "Text                                        "
            "%.18s %.18s %.18s %.18s %s\n",
            &cbuf[0], &cbuf[18], &cbuf[36], &cbuf[54], &cbuf[72]);
    } else {
        sim_debug(DEBUG_DETAIL, dptr, "Text                                        "
            "%.8s %.18s %.18s %.18s %s\n",
            &cbuf[0], &cbuf[8], &cbuf[26], &cbuf[44], &cbuf[62]);
    }

    for (col=0; col<80; col++) {
        hol=image[col];
        for (row=0;row<12;row++){
            if (hol & (1 << (row))) c = 'O'; else c = '.';
            CardHoles[col + 80 * row] = c; 
        }
    }
    for (row=11;row>=0;row--) {
        if (SkipCols==0) {
            cbuf1[0]=0;
            for (i=0; i<8; i++) cbuf2[i]=CardHoles[72 + i + row*80]; 
            cbuf2[8]=0;
        } else {
            for (i=0; i<8; i++) cbuf1[i]=CardHoles[i + row*80]; 
            cbuf1[8]=32; cbuf1[9]=0;
            cbuf2[0]=0;
        }
        i=0;
        for (n1=0;n1<4;n1++) {
            d=0;
            for (n2=0;n2<18;n2++) {
                c=CardHoles[SkipCols + n2 + n1*18 + row*80];
                d = (d << 1) + ((c=='.') ? 0:1);
                cbuf[i++]=c; 
            }
            cbuf[i++]=' ';
            w[n1]=d; 
        }
        cbuf[i++]=0;
        if (EchoLevel==2) {
            // if Echo = 2 -> print card image as octal in console
            sim_printf(" Row %c %s %s %06o %06o %06o %06o\n", 
                crow[row], (row==11) ? "octal":"     ", (row==11) ? " /":"| ",
                w[0], w[1], w[2], w[3]);
        } else if (EchoLevel==3) {
            // if Echo = 3 -> print card image as octal+binary in console
            cbuf[38]=0;
            sim_printf("Row %c: %06o %06o %06o %06o %s %s \n", 
                crow[row], 
                w[0], w[1], w[2], w[3], (row==11) ? " /":"| ", cbuf);
        } else {
            // if Echo = 0 ->  print full card image only in debug log
            sim_debug(DEBUG_DETAIL, dptr, "Row %c octal: %06o %06o %06o %06o %s %s%s%s\n", 
                crow[row], w[0], w[1], w[2], w[3], (row==11) ? " /":"| ", cbuf1, cbuf, cbuf2);
        }
    }
    if (EchoLevel>=2) sim_printf(" \n");

}


/* Initialize vm  */
void vm_init(void) {
    static int inited = 0;
    extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
    extern mtinforec mt_info[4];
    int n; 

    if (inited == 1) return;   /* Be sure to only do these things once */        
    inited = 1;

    // Initialize vm memory to zero 
    crt_reset();

    // init specific commands
    sim_vm_cmd = aux_cmds;                       /* set up the auxiliary command table */

    memset(lptPrintOut, 0, sizeof(lptPrintOut));

    for (n=0;n<4;n++) mt_info[n].TapeRecord_buf = NULL; 
}

/* image file into memory.  */

int sLinToInt(int nlin, char * slin,  int n, int len, int octal)
{
    int nn, i; 
    char c; 

    nn=0;
    for (i=0;i<len; i++) {
        c=slin[n++];
        if ((c==' ') || (c=='[') || (c==']')) c='0';
        if ((c<'0') || (c>'9')) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Invalid char '%c' position %d\n", c, n);
            return -1;
        }
        if (octal) {
            if (c>='8') {
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("Digit '%c' position %d not allowed in octal numbers\n", c, n);
                return -1;
            }
            nn = nn*8 + (c-'0'); 
        } else {
            nn = nn*10 + (c-'0'); 
        }
    }
    return nn; 
}

t_stat sim_load(FILE * fileref, CONST char *cptr, CONST char *fnam, int flag)
{
    int lo_addr, hi_addr, addr, nlin, i, blank, n;
    int octal, alpha, digits; 
    t_value d; 
    t_stat r; 
    char c_svrem; 

    char slin[1024];
    char c;

    if (*cptr != 0) return SCPE_ARG;
    if (flag != 0) return sim_messagef (SCPE_NOFNC, "Command Not Implemented\n");

    memset(CRT_Symbolic_Buffer, 0, sizeof(CRT_Symbolic_Buffer));            // clear symbolic info

    octal=0; // defaults to decimal numbers
    lo_addr=hi_addr=-1; 
    addr=0; nlin=0; // init lowest/highest addr loaded
    while (fgets (slin, sizeof(slin)-1, fileref)) {
        nlin++; 
        // set lines to 30 chars len minimum
        i=strlen(slin);
        if (i<30) {
            while (i<30) slin[i++]=' '; // pad with spaces
            slin[i]=0;                  // new oend of line
        }
        // remove non printable chars
        i=strlen(slin);
        for (i=0;i<(int) strlen(slin);i++) {
            if (slin[i] < 32) slin[i]=' ';
        }

        // extract program info. Line format:
        // 0         1         2         3
        // 01234567890123456789012345678901234567890
        // OCT | DEC
        // NNNN S OPNAME   OP ADDR    Remarks...(max 79 chars)
        // 7777 + store mq 10 7777    instruction
        // 7777 + store mq    7777    instruction
        // 7777 + store mq 10 [  ]    instruction
        // 7777 + store mq    [  ]    instruction
        // 7777 +          10 7777    instruction
        // 7777            10 [  ]    instruction
        // 7777 - 123456              half word  
        // 7777   123456789012        full word
        // check if blank line
        blank=1; alpha=0; digits=0;
        if (sim_strncasecmp(slin, "NNNN ",  5) == 0) {
            // if line start with NNNN ... ignore it
        } else if (slin[0] == ';') {
            // if line start with ";" ... ignore it
        } else if (sim_strncasecmp(slin, "OCT",  3) == 0) {
            // if line start with OCT ... use octal numbers 
            octal=1;
        } else if (sim_strncasecmp(slin, "DEC",  3) == 0) {
            // if line start with DEC ... use decimal numbers
            octal=0;
        } else for (i=0;i<23;i++) {
            c=slin[i];
            if (c<32) break;
            c=toupper(c);
            if ((octal) && (i<23) && ((c=='8') || (c=='9'))) {
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("Invalid octal digit %c at pos %d\n", c, i+1);
                break;
            }
            if ((c>='0') && (c<='9')) {
                blank=0; 
                if (digits==0) { 
                    if ((i>5) && (i<16)) digits++; 
                    if (i>=16) alpha=1; // digits on op colum counts as opcode, so alpha set to 1
                } else {
                    digits++;
                }
            } 
            if ((c>='A') && (c<='Z')) {blank=0; alpha=1;} 
            if ((c=='[') || (c==']')) blank=0;
            if ((c=='+') || (c=='-')) blank=0;
            if (c=='?') {
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("Warning: Has a ? character\n", c, c, i+1);
                break;
            }
            if ((c!=' ') && (c!='?') && (c!='[') && (c!=']') && (c!='+') && (c!='-') && ((c<'0') || (c>'9')) && ((c<'A') || (c>'Z'))) {
                blank=1; 
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("invalid char '%c' (code %d) at pos %d\n", (c<32) ? 32:c, c, i+1);
                break;
            } // if text in prog area -> blank line
        }
        if (blank) continue; // ignore blank lines
        // get address to load to
        n=sLinToInt(nlin, slin,  0, 4, octal); // address
        if (n<0) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Missing load address\n");
            continue; 
        } else {
            addr=n;
        }
        if (addr >= 4096) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Cannot store at address %d\n", addr);
            continue; 
        }
        if ((lo_addr == -1) || (lo_addr > addr)) lo_addr = addr;  
        if ((hi_addr == -1) || (hi_addr < addr)) hi_addr   = addr;  
        // safety. Put an end of line before remarks
        c_svrem=slin[26]; 
        slin[26]=0; 
        // get data to load at this addr
        r = parse_sym(&slin[5], addr, NULL, &d, 
            (alpha ? SWMASK('M'): (digits>6) ? SWMASK('F') : 0) | (octal ?  SWMASK('O'):0));
        slin[26]=c_svrem; // restore
        if (r) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Invalid instruction\n");
            continue; 
        }
        // store word
        if ((alpha==0) && (digits>6)) {
            WriteAddr(-addr, d, NULL);
        } else {
            WriteAddr(addr, d, NULL);
        }
        // store symbolic info
        memset(&CRT_Symbolic_Buffer[addr * 80], 0, 80); // clear
        sim_strlcpy(&CRT_Symbolic_Buffer[addr * 80], &slin[27], 79);

    }
    if (lo_addr < 0) {
        sim_printf("Nothing loaded. No program found into %s \n", fnam);
    } else {
        sim_printf("loaded %s from %04d to %04d address\n", fnam, lo_addr, hi_addr);
    }
    return SCPE_OK;
}

/* Opcodes */
t_opcode  base_ops[34] = {            
    // opcode     name          cycles     modif  
    {OP_STOP,     "STOP",       4,         1},
    {OP_TR,       "TR",         4,         1},
    {OP_TR_OV,    "TR OV",      4,         1},
    {OP_TR_PLUS,  "TR +",       4,         1},
    {OP_TR_ZR,    "TR 0",       4,         1},
    {OP_SUB,      "SUB",        5,         1},
    {OP_R_SUB,    "R SUB",      5,         1},
    {OP_SUB_AB,   "SUB AB",     5,         1},
    {OP_NOOP,     "NO OP",      4,         1},
    {OP_ADD,      "ADD",        5,         1},
    {OP_R_ADD,    "R ADD",      5,         1},
    {OP_ADD_AB,   "ADD AB",     5,         1},
    {OP_STORE,    "STORE",      5,         2},
    {OP_STORE_A,  "STORE A",    5,         2},
    {OP_STORE_MQ, "STORE MQ",   5,         2},
    {OP_LOAD_MQ,  "LOAD MQ",    5,         2},
    {OP_MPY,      "MPY",       38,         0},
    {OP_MPY_R,    "MPY R",     38,         0},  
    {OP_DIV,      "DIV",       38,         0},
    {OP_ROUND,    "ROUND",      4,         1},
    {OP_L_LEFT,   "L LEFT",     4,         3},
    {OP_L_RIGHT,  "L RIGHT",    4,         3},
    {OP_A_LEFT,   "A LEFT",     4,         3},
    {OP_A_RIGHT,  "A RIGHT",    4,         3},
    {OP_READ,     "READ",       4,         5},
    {OP_READ_B,   "READ B",     4,         5},
    {OP_WRITE,    "WRITE",      4,         5},
    {OP_WRITE_EF, "WRITE EF",   4,         5},
    {OP_REWIND,   "REWIND",     4,         5},
    {OP_SET_DR,   "SET DR",     4,         1},
    {OP_SENSE,    "SENSE",      4,         1},
    {OP_COPY,     "COPY",       5,         4},
    {OP_EXTR,     "EXTR",       5,         2},
    {0}
};

/* Symbolic decode

Inputs:
*of     =       output stream
addr    =       current PC
*val    =       pointer to values
*uptr   =       pointer to unit
sw      =       switches
Outputs:
return  =       status code
*/

t_stat fprint_sym(FILE * of, t_addr addr, t_value * val, UNIT * uptr, int32 sw)
{
    t_int64 d = *val;
    int octal, hword;
    int c, opcode, opaddr;
    t_opcode            *tab;
    extern int          NCYCLE;                      // indicates current machine cycle to be executed. 


    // -M to display:       -|+   op name   opcode   address    
    // -F to display fullwords (defaults to display halfwords). addr must be even
    // -O to display octal numbers (defaults to decimal)

    octal = (sw & SWMASK('O') ? 1:0); 
    hword = (sw & SWMASK('F') ? 0:1); 

    /* Print value in decimal or octal, halfword or fullword*/
    fputs("   ", of);
    if (hword==0) {
        if (octal) {
            fprintf(of, "%06o%06o", printfw_oct(d));  // fprintf unsigned octal 36 bit word (12 digits)
        } else {
            fprintf(of, "%c%05d%06d", printfw(d));    // fprintf signed decimal 36 bit word (12 chars: sign + 11 digits)
        }   
    } else {
        if (octal) {
            fprintf(of, "%06o", d);                   // fprintf unsigned octal 18 bit word (6 digits)
        } else {
            fprintf(of, "%c%06d", (char) ( ((d) >> 17) ? '-':'+'), (int) (d & 0377777));  // fprintf signed decimal 18 bit word (7 chars: sign + 6 digits)
        }   
    }

    if (sw & SWMASK('M')) {
        c = (d >> 17) ? '-':'+';
        fprintf(of, " %c ", c);
        opcode = (d >> 12) & 63; 
        if (opcode != 13+32) opcode = opcode & 31; 
        opaddr = d & 07777; 
        // find opcode
        tab = base_ops;
        while (1) {
            if (tab->opbase == opcode) {
                fprintf(of, "%-8s ", tab->name);
                break; 
            }
            tab++;
        }       
        if (octal) {
            fprintf(of, "%02o %04o", opcode, opaddr);
        } else {
            fprintf(of, "%02d %04d", opcode, opaddr);
        }
        if (cpu_unit.flags & OPTION_STEP_HALFCYCLE) {
            fprintf(of, " - %s TIME", 
                   (NCYCLE == 0) ? "INSTR":"EXEC");     

        }
    }

    return SCPE_OK;
}

/* read n digits

Inputs:
*cptr   =       pointer to input string
octal   =       1-> octal number, 0-> decimal number
spaces  =       1-> allow embebbed space
sign    =       1-> allow leading sign
Outputs:
d       =       parsed value
return cptr     
*/

CONST char * parse_n(t_int64 *d, CONST char *cptr, int octal, int spaces, int sign)
{
    int i = 0;
    int neg=0;
    char c; 

    *d = 0;

    while (isspace(*cptr)) cptr++; // skip blanks
    if (sign) {
        if (*cptr == '+') { cptr++; } else
            if (*cptr == '-') { cptr++; neg=1;}
    }

    while (1) {
        c = *cptr; 
        if (spaces && (isspace(c))) {
            cptr++;  // allow spaces
            continue;
        }
        if (octal) {
            if (c < '0' || c > '7') break;
            *d = (*d * 8) + (c - '0');
        } else {
            if (c < '0' || c > '9') break;
            *d = (*d * 10) + (c - '0');
        }
        cptr++;
        if (++i == (octal ? 12:11)) break; // allow max 11 digits on decimal numbers, 12 on octal numbers
    }
    if (neg) *d=-(*d); 
    while (isspace(*cptr)) cptr++; // skip blanks
    return cptr;
}


/* Symbolic input

Inputs:
*cptr   =       pointer to input string
addr    =       current PC
uptr    =       pointer to unit
*val    =       pointer to output values
sw      =       switches
Outputs:
status  =       error status
*/


t_stat parse_sym(CONST char *cptr, t_addr addr, UNIT * uptr, t_value * val, int32 sw)
{
    t_int64             d;
    int                 neg, octal, oplen, opcode, opaddr, hword;
    char                c;
    t_opcode            *tab;

    // -M to enter:  [-|+] op name  [opcode] address       name can contain spaces, opcode/address cannot. 
    //                                                     address is mandatory (can be "[ ]" with is interpreted as zero). sets a halfword
    // -O to use octal numbers (defaults to decimal)
    // -F store full word

    octal = (sw & SWMASK('O') ? 1:0); // = 1 if octal base
    hword = (sw & SWMASK('F') ? 0:1); // = 1 if handling halfword
    while (isspace(*cptr)) cptr++;

    d = 0;
    if (sw & SWMASK('M')) {

        neg = 0; 
        if (hword==0) return sim_messagef (SCPE_ARG, "Instruction can only be stored on a halfword\n");

        c=*cptr; 
        if ((c == '+') || (c == '-')) {
            neg=(c == '-') ? 1:0; // minus before opname
            cptr++; 
            while (isspace(*cptr)) cptr++; // skip blanks
        }
        if (*cptr == 0) return sim_messagef (STOP_UUO, "Missing operation name\n");
        // find opcode name
        // start from end of opcode table going up to the beginning, so STORE A is checked before STORE
        tab = NULL; 
        if ((*cptr < '0') || (*cptr > '9')) {
            // mnemonic to be searched 
            tab = base_ops + 32;
            while (1) {
                oplen = strlen(tab->name); // opcode lenght
                if ((sim_strncasecmp(cptr, tab->name, oplen) == 0) && 
                    ((cptr[oplen] == 32) || (cptr[oplen] == 0)) ) {
                        cptr += oplen; 
                        while (isspace(*cptr)) cptr++; // skip blanks
                        break; // opcode found
                }
                if (tab->opbase == 0) return STOP_UUO; // reached top of opcode table but opcode not found -> return error
                tab--;
            }       
        }
        // read addr or opcode
        if (*cptr == '[') {
            // no opcode, just [ ] in address part
            opcode=tab->opbase; // get opcode from op name found
            opaddr=0;           // blank address part [   ] as zero
        } else if ((*cptr < '0') || (*cptr > '9')) {
            // no numeric opcode/address part
            return sim_messagef (STOP_UUO, "No numeric operation/code address part\n"); 
        } else {
            cptr = parse_n(&d, cptr, octal, 0,0); // get first number
            if (*cptr == '[') {
                // blank addr part:  opcode  [    ] 
                opcode=(int) d; 
                if ((tab) && (opcode != tab->opbase)) return sim_messagef (STOP_UUO, "Operation name is code %d, but operation code suplied is %d\n", tab->opbase, opcode); 
                opaddr=0;
            } else if ((*cptr >= '0') && (*cptr <= '9')) {
                // two numbers: opcode opaddr
                opcode=(int) d; 
                if ((tab) && (opcode != tab->opbase)) return sim_messagef (STOP_UUO, "Operation name is code %d, but operation code suplied is %d\n", tab->opbase, opcode); 
                cptr = parse_n(&d, cptr, octal, 0,0);
                opaddr=(int) d;
            } else {
                // only one number
                if (tab == NULL) {
                    return sim_messagef (STOP_UUO, "Missing Opcode or Opaddr\n"); 
                }
                // there is opcode so the number is the address 
                opcode=tab->opbase; // get opcode from op name found
                opaddr=(int) d;
            }
        }
        if (opaddr >> 12) return sim_messagef (STOP_UUO, "Operation address has ore than 12 bits\n"); 

        // set instr: sign (1 bit) + opcode (5 bits) + address (12 bits): total 18 bits 
        d = (opcode << 12) + opaddr; 
        if (neg) d |= (1 << 17); 
        d = d & 0777777; // safety
    } else {
        // numeric value
        cptr = parse_n(&d, cptr, octal, 1,1);
        // 36 bits fullword signed constant, either decimal or octal. Allow embebbend spaces
        if (hword==0) {
            if (d<0) {
                d = SGN | ((-d) & MASK35BITS) ;
            } else {
                d = d & MASK35BITS;
            }
        } else {
            // 18 bits halfword signed constant, either decimal or octal. Allow embebbend spaces
            if ((d > 0777777) || (d < -(0777777))) {
                return sim_messagef (SCPE_ARG, "Number does not fit in a halfword\n");
            }
            if (d<0) {
                d = (-d) & 0377777;
                d |= (1 << 17);
            } else {
                d = d & 0777777;
            }
        }
    }
    * val = d; 
    return SCPE_OK;
}

/* Helper functions */


// additional SCP commands to press/set switches

struct {
    int id;              // 0=button, >1 switch ident
    const char *name;    // name of button/switch to be used in scp command
} csw_btn_def[] = {
    // buttons
    {100, "Load"},
    {120, "Printer Carriage"}, 
    // switches
    {  1, "AutomaticManual"},     
    {  2, "Load Selector"},     
    {  3, "Instruction"},     
    {  4, "Address"},     
    {  5, "MQ"},     
    { 11, "Option 1"},
    { 12, "Option 2"},
    { 13, "Option 3"},
    { 14, "Option 4"},
    { 15, "Option 5"},
    { 16, "Option 6"},
    {-1}
};

// implements the following SCP commands
//    switch "name" to "label"
//    switch "name" to nnnn 
//    press "name"
//    depress "name"
t_stat set_csw_btn_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    int n, id; 
    const char * cptr2 = NULL; 
    int octal, addr; 
    t_int64 d; 
    t_stat r; 

    extern int                 IC;                          // Instruction Counter
    extern int                 NCYCLE;                      // indicates current machine cycle to be executed. 
    extern t_int64             MQ;                          // M-Q register
    extern int                 SENSE_IN;                    // Sense input operator panel option switches

    extern int Console_Sw_LoadSelector;      // console load selector: 0=card/1=tape/2=drum
    extern int Console_Sw_AutoManual;        // console switch 0=Automatic/1=Manual
    extern int Console_Sw_IR_Entry;          // instruction entry
    extern int Console_Sw_MQ_Entry;          // M-Q entry

    cptr = get_glyph_quoted (cptr, gbuf, 0);   // get next param (name of button/switch, quoted)
    if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote
    n=strlen(gbuf);
    if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0; // remove trailing quote

    // find the button/swith name in table
    for(n=0;;n++) {
        id = csw_btn_def[n].id;
        if (id<0) {
            return sim_messagef (SCPE_ARG, "Unknown button/switch name \"%s\"\n", gbuf); 
        }
        if (sim_strncasecmp(gbuf, csw_btn_def[n].name, 32) != 0) continue; // loop if not this name
        // found, check button/switch vs command
        if ((flag == 2) && (id < 100)) {
            return sim_messagef (SCPE_ARG, "Unknown button name \"%s\"\n", gbuf); // because pressing a switch
        } else if ((flag == 1) && (id >= 100)) {
            return sim_messagef (SCPE_ARG, "Unknown switch name \"%s\"\n", gbuf); // because switching a button
        }
        // found
        break;
    }

    if (id == 100) {
        // press "Load"
        ClearInterlocks();
        if (Console_Sw_LoadSelector==0) {
            // load from card
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Load Button - card selected\n");
            SetIOADDR(2048, OP_READ); 
            r=cdr_cmd(&cdr_unit[0], OP_READ, 0);
            if (r) return r; 
            r=cdr_cmd(&cdr_unit[0], OP_COPY, 0);
            if (r) return r; 
        } else if (Console_Sw_LoadSelector==1) {
            // load from tape
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Load Button - tape selected\n");
            SetIOADDR(256, OP_READ); 
            r=mt_cmd(&mt_unit[0], OP_READ, 0);
            if (r) return r; 
            r=cdr_cmd(&mt_unit[0], OP_COPY, 0);
            if (r) return r; 
        } else if (Console_Sw_LoadSelector==2) {
            // load from drum
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Load Button - drum selected\n");
            SetIOADDR(128, OP_READ); 
            dr_cmd(0, OP_SET_DR, 0); // read from drum 0 -> reset drum addr 
            dr_cmd(0, OP_READ, 0);
        } else  return SCPE_ARG;  
        addr=Console_Sw_IR_Entry & 07776; 
        // if (addr==0) addr=-4096 -> to store a full word at addr 0 (as it was addr -0)
        WriteAddr((addr==0) ? -4096:-addr, MQ, "Load Full Word from Device to Mem at Addr");
        IC=addr; 
        NCYCLE=0;
        r=run_cmd(RU_GO, cptr);
        return r; 
    } else if (id == 120) {
        // press "Printer Carriage"
        // print an empty line
        lpt_printline(lp_unit, "", 0); // print empty line

    } else if (id >= 100) return sim_messagef (SCPE_ARG, "Unknown button name\n"); 
    if (id >= 100) return SCPE_OK; 

    // is a set switch .. to "label"
    // get TO separator
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missign TO \"label\"\n");;

    // get label
    cptr2=cptr; // save label start
    cptr = get_glyph_quoted (cptr, gbuf, 0);   // get next param (name of switch label, quoted)
    if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote, if any
    n=strlen(gbuf);
    if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0; // remove trailing quote, if any

    // set switch
    if (id==1) {
        // switch AutomaticManual to Automatic|Manual
        // Console_Sw_AutoManual=0 -> Switch at 'AUTOMATIC', =1 -> Switch at 'MANUAL'
        if (sim_strncasecmp(gbuf, "MANUAL",    32) == 0) n=1; else
            if (sim_strncasecmp(gbuf, "AUTOMATIC", 32) == 0) n=0; else
                return SCPE_ARG; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Switch Automatic/Manual to %s\n", gbuf);
        Console_Sw_AutoManual=n; 
    } else if (id==2) {
        // switch "Load selector" to Card|Tape|Drum
        // Console_Sw_LoadSelector=0 -> Switch at 'CARD', =1 -> Switch at 'TAPE', =2 -> Switch 'DRUM'
        if (sim_strncasecmp(gbuf, "DRUM", 32) == 0) n=2; else
            if (sim_strncasecmp(gbuf, "TAPE", 32) == 0) n=1; else
                if (sim_strncasecmp(gbuf, "CARD", 32) == 0) n=0; else
                    return SCPE_ARG; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Switch Load Selector to %s\n", gbuf);
        Console_Sw_LoadSelector=n; 
    } else if (id==3) {
        t_value d; 
        // switch "instruction" to [oct] [-] [nn] nnnn
        // if OCT present, number in octal. If not present, number in decimal
        // if two number groups, there are opcode opaddr
        // if only one group, there is the 18bits instruction half word
        if (sim_strncasecmp(gbuf, "OCT", 32) == 0) {
            octal=1; 
        } else {
            octal=0;
            cptr=cptr2; // restore start
        }
        parse_sym(cptr, 0, NULL, &d, SWMASK('M') | (octal ?  SWMASK('O'):0));
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Switch Instruction Entry to %c %d %d\n", 
            (d >> 17) ? '-':'+', (int) ((d >> 12) & 31), (int) (d & 4095));
        Console_Sw_IR_Entry=(int) d; 
    } else if (id==4) {
        // switch "Address" to [oct] nnnn
        // if OCT present, number in octal. If not present, number in decimal
        // set address part of instruction entry switches
        if (sim_strncasecmp(gbuf, "OCT", 32) == 0) {
            octal=1; 
        } else {
            octal=0;
            cptr=cptr2; // restore start
        }
        parse_n(&d, cptr, octal, 0,0);
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Switch Instruction Address to %d\n", (int) d);
        Console_Sw_IR_Entry = (Console_Sw_IR_Entry & 0770000) | (d & 07777); 
    } else if (id==5) {
        // switch "MQ" to [oct] nnnn
        // if OCT present, number in octal. If not present, number in decimal
        // set MQ entry switches
        if (sim_strncasecmp(gbuf, "OCT", 32) == 0) {
            octal=1; 
        } else {
            octal=0;
            cptr=cptr2; // restore start
        }
        parse_n(&d, cptr, octal, 0,0);
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Switch MQ Entry to %d\n", (int) d);
        Console_Sw_MQ_Entry = (int) d; 
    } else if ((id>=11) && (id <= 16)) {
        // switch "Option 1" to Up|Down|Off|On|0|1 
        // switch "Option 2" to Up|Down|Off|On|0|1 
        // switch "Option 3" to Up|Down|Off|On|0|1 
        // switch "Option 4" to Up|Down|Off|On|0|1 
        // switch "Option 5" to Up|Down|Off|On|0|1 
        // switch "Option 6" to Up|Down|Off|On|0|1 
        if (sim_strncasecmp(gbuf, "On", 32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "Down", 32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "1", 32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "Off", 32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "Up", 32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "0", 32) == 0) n=0; else
        return SCPE_ARG; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set Option Switch %d to %d\n", id-10, n);
        if (n) {
            SENSE_IN |= (1 << (id-11)); 
        } else {
            SENSE_IN &= ~(1 << (id-11)); 
        }
    } else return sim_messagef (SCPE_ARG, "Unknown switch name\n"); 
    return SCPE_OK;
}

/* deck operations 

   carddeck [-q] <operation> <parameters...>

                        allowed operations are join, list, print, punch, sort, ident

                        default format for card files is AUTO, this allow to intermix source decks
                        with different formats. To set the format for carddeck operations use

                           set cdr0 -format xxxx      for cards being read
                           set cdp0 -format xxxx      for cards being punched  
                        
                        this will apply to all operations, both on reading and writing deck files

   carddeck join        join several deck files into a new one

                        carddeck join <file1> <file2> ... as <file>

                        <file1>  first source deck file
                        <file2>  second source deck file
                        ...
                        <file>   destination deck file

                        any source file <file1>, <file2>, etc can have same name as destination file <file>.
                        Each source file is completly read in turn by SimH in its internal buffer (room for 10K cards) 
                        and then written on destination file. This allos to append une deck on the top/end of 
                        another one.

   carddeck list        display card deck in console       
   
                        carddeck list [oct|bin] <file>  

                        Allways shows text representation of card. If the card column has
                        punches that does not have an printable char equivalent, no char
                        is shown.

                        If OCT specified, print also octal representation of card. 
                        If BIN indicated, print also binary representation.

   carddeck echolast    display last card(s) already read in card reader unit
   
                        carddeck echolast [oct|bin] <count>  

                        Shows last <coun>t cards from read take hopper that are
                        already read by card reader unit

                        OCT and BIN works same as in carddeck list command

   carddeck print       print card deck contents in line printer
   
                        carddeck print <file>  

                        Prints text representation of card. If the card column has
                        punches that does not have an printable char equivalent, 
                        no char is printed. 

   carddeck ident       (punch) an identification on each card of deck
   
                        carddeck ident <file> as "text" at <col> [ count at <col1> <col2> [ start at <num>] ]

                        examples:

                        carddeck ident <file> as "hola" at 72
                        
                           punch text "hola" starting at col 72 on each card of deck in give file
                           (first column of card is column 1)

                        carddeck ident <file> as "01 112 01" at 1 count at 8 9
                        
                           punch text "01 112 00" starting at col 1 on each card of deck in give file.
                           set card count starting at 1 (pad left with zero) from columns 8 to 9 (2 digits)
                           if start at <num> is given, card count starts at num

   carddeck sort        sort cards of deck ascending or descending on contents of column interval
   
                        carddeck sort <file1> asc|desc <col1> <col2> [ to <file2> ]
                        
                        sort input deck in <file1>, resulting sorted deck is <file2>
                        can be the same filename. If to is not present, destination file
                        is same as input file

                        sort ascending or descending based on data in cards from column
                        col1 up to col2, both columns included (first column of card is column 1)

   carddeck punch       create (punch) a new single card in specied file. 

                        carddeck punch <file> text|oct|dec <data>

                        examples: 

                        carddeck punch <file> text "Hola adios" [ at <col> ]    
                        
                           punch this text starting at col 1. Format of card file 
                           created is TEXT. 

                        carddeck punch <file> oct 777777 777777  ...   
                        carddeck punch <file> dec 123 +456 -789  ...   
                        
                           punch a card with 1 up to 48 halfwords. Format of card file 
                           created is BINARY.

                           Halfword can be octal (1 to 6 octal 0-7 digits, no sign) or can
                           be decimal (1 to 6 decimal 0-9 digits, with optional sign)

                           These halfwords are punched starting at row 9 (bottom of card)
                           and filling it up left to right, upside direction, 4 halfwords per row
                           up to a max of 48 halfwords. If less thatn 48 are provided, a zero (no
                           punch) is assumed

                                      ------------------------------------------------------+
                                    /   |            |            |  ...       | hword 48   |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |            |            |            |            |
                                   |    |  hword 5   |  ...       |            |            |
                                   |    |  hword 1   |  hword 2   |  hword 3   |  hword 4   |
                                   +----+------------+------------+------------+------------+


                        carddeck punch <file> bincards <n>

                           punch n binary cards with memroy contents starting at location 0.
                           Each card holds 24 fullwords

switches:            if present must be just after carddeck and before deck operation
    -Q                  quiet return status. 
           
*/

typedef struct {
    int csize; // max numbers of cards that can hold *p 
    uint16 *p;   // mem for deck
    int nCards;  // number of cards in deck
} Deck;

// init a new deck image
void deck_init(Deck * DeckImage)
{
    DeckImage->csize=0;
    DeckImage->nCards=0;
    DeckImage->p=NULL;
}

// free mem for deck image
void deck_free(Deck * DeckImage)
{
    if (DeckImage->p) free(DeckImage->p);
    deck_init(DeckImage);
}

// add card to DeckImage, inc nCards, alloc more mem for DeckImage if needed 
// return 0 if error (and frees DeckImage mem)
int add_to_deck(uint16 * image, Deck * DeckImage)
{
    int i;
    uint16 c;    
    uint16 * NewDeckImage;

    if (!DeckImage->p) {
        // init DeckImage if its pointer is NULL
        DeckImage->nCards=0;
        DeckImage->csize=100;
        DeckImage->p = (uint16 *) malloc((size_t)(DeckImage->csize * 80 * sizeof(*DeckImage->p)));
        if (!DeckImage->p) {
            sim_messagef (SCPE_IERR, "deck_load() memory allocation error\n");
            return 0;
        }
    } else {
        // make sure there is room in DeckImage, get more mem if needed
        if (DeckImage->nCards+1 >= DeckImage->csize) {
            DeckImage->csize += 100;
            NewDeckImage = (uint16 *) realloc(DeckImage->p, (size_t) (DeckImage->csize * 80 * sizeof(*DeckImage->p)));
            if (!NewDeckImage) {
                deck_free(DeckImage);
                sim_messagef (SCPE_IERR, "deck_load() memory reallocation error\n");
                return 0;
            }
            DeckImage->p=NewDeckImage; 
        }
    }
    // add card read to deck
    for (i=0; i<80; i++) {
        c = image[i];
        DeckImage->p[DeckImage->nCards * 80 + i] = c;
    }
    DeckImage->nCards = DeckImage->nCards + 1;
    return 1;
}

// get set an individual card in deck (depending on cMode 'S' or 'G')
// return 0 if error (and frees DeckImage mem)
int get_set_card_in_deck(char cMode, uint16 * image, Deck * DeckImage, int n) 
{
    int i; 

    if (!DeckImage->p) {
        // init DeckImage if its pointer is NULL
        return 0; 
    }
    if ((n < 0) || (n >= DeckImage->nCards)) {
        deck_free(DeckImage);
        return 0; // get invalid card number
    }
    if (cMode == 'S') {
        // set card from image into deck
        for(i=0;i<80;i++) DeckImage->p[n * 80 + i] = image[i];
    } else {
        // get card in image from deck
        for(i=0;i<80;i++) image[i] = DeckImage->p[n * 80 + i];
    }
    return 1; 
}

// load card file fn and add its cards to DeckImage, up to a max of MAX_CARDS_IN_DECK 
// (as sanity check). uses cdr0 device/unit (if error frees DeckImage mem)
t_stat deck_load(CONST char *fn, Deck * DeckImage)
{
    UNIT *              uptr = &cdr_unit[0];
    uint16 image[80];    
    t_stat              r, r2;
    int32 sv_sim_switches = sim_switches; 

    // attach file to cdr unit 0

    sim_switches |= SWMASK ('Q');
    r = (cdr_dev.attach)(uptr, fn);
    sim_switches = sv_sim_switches; 

    if (r != SCPE_OK) {
        deck_free(DeckImage);
        return r;
    }

    // read all cards from file
    while (1) {

        if (DeckImage->nCards >= MAX_CARDS_IN_DECK) { // sanity check
            r = sim_messagef (SCPE_IERR, "Too many cards\n"); 
            break;
        }
        r = sim_read_card(uptr, image);
        if ((r == CDSE_EOF) || (r == CDSE_EMPTY)) {
            r = SCPE_OK; break;             // normal termination on card file read finished
        } else if (r != CDSE_OK) {
            break;                          // abnormal termination on error
        }

        // add card read to deck
        if (!add_to_deck(image, DeckImage)) {
            r = SCPE_IERR;
            break;
        }
    }

    // deattach file from cdr unit 0
    sim_switches |= SWMASK ('Q');
    r2 = (cdr_dev.detach)(uptr);
    sim_switches = sv_sim_switches; 
    if (r == SCPE_OK) r = r2; 
    if (r != SCPE_OK) {
        deck_free(DeckImage);
    }

    return r;
}

// write nCards starting at card from DeckImage array to file fn
// if nCards < 0 -> save whole deck
// uses cdp0 device/unit. do not frees DeckImage even if error
t_stat deck_save(CONST char *fn, Deck * DeckImage, int card, int nCards)
{
    UNIT *              uptr = &cdp_unit[0];
    t_stat              r;
    int nc;
    int32 sv_sim_switches = sim_switches; 

    if (nCards < 0) nCards=DeckImage->nCards;
    if (card + nCards > DeckImage->nCards) {
        return sim_messagef (SCPE_IERR, "Reading outside of Deck\n");
    }

    // set flags for create new file
    uptr->flags &= ~UNIT_RO; 
    sim_switches |= SWMASK ('N');

    // attach file to cdr unit 0
    sim_switches |= SWMASK ('Q') ;
    r = (cdp_dev.attach)(uptr, fn);
    sim_switches = sv_sim_switches; 
    if (r != SCPE_OK) return r;

    // write cards to file
    for  (nc=0;nc<nCards;nc++) {
        r = sim_punch_card(uptr, &(DeckImage->p[(nc + card) * 80]));
        if (r != CDSE_OK) break;    // abnormal termination on error
    }

    // deattach file from cdr unit 0
    sim_switches |= SWMASK ('Q');
    (cdp_dev.detach)(uptr);
    sim_switches = sv_sim_switches; 

    return r;
}

// echo/print DeckImage array 
// uses cdp0 device/unit
void deck_print_echo(Deck * DeckImage, int bPrint, int EchoLevel)
{
    char line[81]; 
    int nc;
    uint16 * image;

    for (nc=0; nc<DeckImage->nCards; nc++) {
        // read card, check and, store in line
        image = &(DeckImage->p[nc * 80]);
        echo_cardimage(&cdr_dev, image, EchoLevel, "", line);
        if (bPrint) {
            // print empty line to separate printout done from following lines. Inhibit echo 
            // to console of printed lines
            lpt_printline(lp_unit, line, 1); 
        }
    }
    if (bPrint) {
        // print empty line to separate printout done from following lines
        lpt_printline(lp_unit, 0, 1); 
    }

}


// carddeck join <file1> <file2> ... as <file>
static t_stat deck_join_cmd(CONST char *cptr)
{
    char fnSrc[4*CBUFSIZE];
    char fnDest[4*CBUFSIZE];
    CONST char *cptr0;
    CONST char *cptrAS;
    char gbuf[4*CBUFSIZE];
    t_stat r;

    Deck DeckImage;
    int i,nDeck, nCards0;

    cptr0 = cptr;
    // look for "as"
    while (*cptr) {
        while (sim_isspace (*cptr)) cptr++;                 // trim leading spc 
        cptrAS = cptr; // mark position of AS
        cptr = get_glyph_quoted (cptr, gbuf, 0);            // get next param
        if (gbuf[0] == 0) return sim_messagef (SCPE_ARG, "AS <file> not found\n");
        for (i=0;i<2;i++) gbuf[i] = sim_toupper(gbuf[i]);
        if (strcmp(gbuf, "AS") == 0) break;
    }

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph_quoted (cptr, fnDest, 0);              // get next param: destination filename 
    if (fnDest[0] == 0) return sim_messagef (SCPE_ARG, "Missing destination filename\n");
    if (*cptr) return sim_messagef (SCPE_ARG, "Extra unknown parameters after destination filename\n");

    cptr = cptr0;                                           // restore cptr to scan source filenames
    nDeck = 0;
    deck_init(&DeckImage);
    while (1) {

        while (sim_isspace (*cptr)) cptr++;                 // trim leading spc 
        if (cptrAS == cptr) break;                          // break if reach "AS"
        cptr = get_glyph_quoted (cptr, fnSrc, 0);           // get next param: source filename 
        if (fnSrc[0] == 0) return sim_messagef (SCPE_ARG, "Missing source filename\n");

        // read source deck
        nCards0 = DeckImage.nCards;
        r = deck_load(fnSrc, &DeckImage);
        if (r != SCPE_OK) return sim_messagef (r, "Cannot read source deck (%s)\n", fnSrc);
        nDeck++;

        if ((sim_switches & SWMASK ('Q')) == 0) {
            sim_printf ("Source Deck %d has %d cards (%s)\n", nDeck, DeckImage.nCards - nCards0, fnSrc);
        }
    }
    r = deck_save(fnDest, &DeckImage, 0, -1);
    if (r != SCPE_OK) {
        sim_messagef (r, "Cannot write destination deck (%s)\n", fnDest);
    } else if ((sim_switches & SWMASK ('Q')) == 0) {
        sim_printf ("Destination Deck has %d cards (%s)\n", DeckImage.nCards, fnDest);
    }
    deck_free(&DeckImage);

    return r;
}


// carddeck print <file> 
// carddeck list [oct|bin] <file> 
// carddeck echolast [oct|bin] <count> 
static t_stat deck_print_cmd(CONST char *cptr, char cMode)
{
    char fn[4*CBUFSIZE];
    char gbuf[4*CBUFSIZE];
    const char *cptr2;
    int EchoLevel = 1; 
    int nCards, nh, nc; 
    t_stat r;

    Deck DeckImage; 

    if (cMode != 'P') {
        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        cptr2 = get_glyph (cptr, gbuf, 0);                      // get next param
        if (strcmp(gbuf, "OCT") == 0) EchoLevel=2; else
        if (strcmp(gbuf, "BIN") == 0) EchoLevel=3; else EchoLevel=1;
        if (EchoLevel>1) cptr=cptr2; 
    }

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    if (cMode != 'E') {
        cptr = get_glyph_quoted (cptr, fn, 0);                  // get next param: source filename 
        if (fn[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename\n");
        if (*cptr) return sim_messagef (SCPE_ARG, "Extra unknown parameters after filename\n");
        nCards = 0;
    } else {
        cptr = get_glyph (cptr, gbuf, 0);                       // get next param
        nCards = (int) get_uint (gbuf, 10, MAX_CARDS_IN_READ_STAKER_HOPPER, &r);
        if (r != SCPE_OK) return sim_messagef (SCPE_ARG, "Invalid count value\n");
        if (nCards == 0) return sim_messagef (SCPE_ARG, "Count cannot be zero\n");
        fn[0]=0;
    }

    // read deck to be printed 
    deck_init(&DeckImage);
    if (cMode != 'E') {
        r = deck_load(fn, &DeckImage);
        if (r != SCPE_OK) return sim_messagef (r, "Cannot read deck (%s)\n", fn);
    } else {
        // get last nCards cards, so
        // first card to echo is count ones before last one
        nh = MAX_CARDS_IN_READ_STAKER_HOPPER + ReadStakerLast - (nCards-1);                 
        nh = nh % MAX_CARDS_IN_READ_STAKER_HOPPER;
        nc=0; 
        while(nc<nCards) {
            // copy card form read hopper buf to deck image
            if (!add_to_deck(&ReadStaker[nh * 80], &DeckImage)) {
                return SCPE_IERR;
            }
            // get previous read card
            nh = (nh + 1) % MAX_CARDS_IN_READ_STAKER_HOPPER;
            nc++;
        }
    }

    if (cMode !=  'P') {
        deck_print_echo(&DeckImage, 0,EchoLevel);
    } else {
        deck_print_echo(&DeckImage, 1,0);
    }
    if (((sim_switches & SWMASK ('Q')) == 0) && (cMode != 'E')) {
        sim_printf ("Printed Deck with %d cards (%s)\n", DeckImage.nCards, fn);
    }
    deck_free(&DeckImage);
    return SCPE_OK;
}

// carddeck punch <file> text|oct|dec <data> [ at <col> ]
// carddeck punch <file> bincards <n>
static t_stat deck_punch_cmd(CONST char *cptr)
{
    char fnDest[4*CBUFSIZE];
    char gbuf[4*CBUFSIZE];
    char cMode = '?'; 
    uint16 image[80];
    t_int64 CardImage[26];
    t_int64 d; 
    int i,n,octal, col, nCards, nc, loc;
    char c; 
    char ident[90]; 
    t_stat r;

    Deck DeckImage; 

    cptr = get_glyph_quoted (cptr, fnDest, 0);                  // get next param: destination filename 
    if (fnDest[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename\n");

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    if (strcmp(gbuf, "TEXT") == 0) cMode='T'; else
    if (strcmp(gbuf, "OCT") == 0)  cMode='O'; else
    if (strcmp(gbuf, "DEC") == 0)  cMode='D'; else 
    if (strcmp(gbuf, "BINCARDS") == 0)  cMode='B'; else 
    return sim_messagef (SCPE_ARG, "Unknown format %s. Must be TEXT, OCT, or DEC\n", gbuf);

    deck_init(&DeckImage);
    nCards=1; 

    n=0;
    if (cMode == 'T') {
        // text
        while (sim_isspace (*cptr)) cptr++;                // trim leading spc 
        cptr = get_glyph_quoted (cptr, gbuf, 0);           // get text to punch
        if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote
        n=strlen(gbuf);
        if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0; // remove trailing quote
        n=strlen(gbuf);
        if (n > 80) {
            return sim_messagef (SCPE_ARG, "Text too long (%d chars). Max 80 chars \n", n);
        }
        sim_strlcpy(ident, gbuf, sizeof(ident)); 

        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        if (*cptr) {
            cptr = get_glyph (cptr, gbuf, 0);                       // get next param
            if (strcmp(gbuf, "AT") != 0) return sim_messagef (SCPE_ARG, "Missing AT \n");

            cptr = get_glyph (cptr, gbuf, 0);                       // get ident column
            col = (int) get_uint (gbuf, 10, 80, &r);
            if (r != SCPE_OK) return r;
            if ((col < 1) || (col > 80)) return sim_messagef (SCPE_ARG, "Invalid column (%d). Must be 1 to 80 \n", col);
        } else {
            col = 1; 
        }

        // fill card image with text converted to holtering
        memset(image, 0, sizeof(image)); 
        for (i=0;i<80;i++) {
            c=ident[i];
            if (c==0) break; 
            c=toupper(c);
            if (col+i > 80) break; 
            image[col-1+i]=sim_ascii_to_hol(c);
        }
    } else if (cMode == 'B') {
        // bin cards
        cptr = get_glyph (cptr, gbuf, 0);                       // get ident column
        nCards = (int) get_uint (gbuf, 10, 85, &r);
        if (r != SCPE_OK) return r;
        if ((nCards < 1) || (nCards > 85)) return sim_messagef (SCPE_ARG, "Invalid number of cards (%d). Must be 1 to 85 \n", nCards);
        memset(CardImage, 0, sizeof(CardImage));
        loc=nc=0;
        while(1) {
            ReadAddr(loc, &d, NULL); 
            n = loc % 48; 
            if ((n & 1)==0) {
                // even address -> left half
                CardImage[n >> 1] = (d << 18) | (CardImage[n >> 1] & 0777777); 
            } else {
                // odd address -> right half
                CardImage[n >> 1] = (CardImage[n >> 1] & 0777777000000) | d; 
            }
            loc++;
            // fill binary card uint16 image[80] from t_int64 CardImage[24] words 
            if (loc % 48 == 0) {
                // 48 halfwords in card -> prepare whole card image
                PrepareCardImage(image, CardImage, 24, cdr_skip_cols());
                // and add this image to deck
                if (!add_to_deck(image, &DeckImage)) {
                    // Location mode has already loaded deckimage. Here we load 
                    // deckimage for text, dec, oct modes
                    deck_free(&DeckImage);
                    return sim_messagef (SCPE_IERR, "Error creating card \n");
                }
                // if all requested cards are generated, exit loop
                nc++;
                if (nc==nCards) break; 
            }
        }
    } else {
        // octal or decimal
        memset(CardImage, 0, sizeof(CardImage));
        octal=(cMode='O') ? 1:0;
        n=0;
        while(1) {
            c=*cptr;
            if (c==0) break; 
            if ((c<'0') || (c>'9')) return sim_messagef (SCPE_ARG, "Non numeric digit '%c' (ascii %d) found \n", (c<32) ? 32:c, c);
            if ((octal) && (c>'7')) return sim_messagef (SCPE_ARG, "Non octal digit '%c' found \n", c);
            if (n>=48) return sim_messagef (SCPE_ARG, "Too many values (max 48 halfword 18bit values) \n");
            if (octal) {
                cptr=parse_n(&d, cptr, 1, 0, 0);
            } else {
                cptr=parse_n(&d, cptr, 0, 0, 1);
            }
            if ((d > 0777777) || (d < -0777777)) return sim_messagef (SCPE_ARG, "Value too big (should be 18bit values) \n");
            if (d<0) d=(1LL << 17) | (-d);
            d = d & 0777777; // safety
            if ((n & 1)==0) {
                // even address -> left half
                CardImage[n >> 1] = (d << 18) | (CardImage[n >> 1] & 0777777); 
            } else {
                // odd address -> right half
                CardImage[n >> 1] = (CardImage[n >> 1] & 0777777000000) | d; 
            }
            n++;
        }
        // fill binary card uint16 image[80] from t_int64 CardImage[24] words 
        PrepareCardImage(image, CardImage, n >> 1, cdr_skip_cols());
    } 

    if ((cMode != 'B') && (!add_to_deck(image, &DeckImage))) {
        // Bincard mode has already loaded deckimage. Here we load 
        // deckimage for text, dec, oct modes
        deck_free(&DeckImage);
        return sim_messagef (SCPE_IERR, "Error creating card \n");
    }

    r = deck_save(fnDest, &DeckImage, 0, -1);
    if (r != SCPE_OK) {
        sim_messagef (r, "Cannot write destination deck (%s)\n", fnDest);
    } else if ((sim_switches & SWMASK ('Q')) == 0) {
        sim_printf ("Created %d card%s deck (%s)\n", 
            nCards, (nCards == 1) ? "":"s", fnDest);
    }
    deck_free(&DeckImage);
    return SCPE_OK;
}


// carddeck sort <file1> asc|desc col1 col2 [ to <file2> ]
static t_stat deck_sort_cmd(CONST char *cptr)
{
    char fnSrc[4*CBUFSIZE];
    char fnDest[4*CBUFSIZE];
    char gbuf[4*CBUFSIZE];
    char cMode = '?'; 
    uint16 image[80], image2[80];
    int i,n,nMin, nCurrent, IsBlank;
    int col1, col2;
    char c; 
    t_stat r;
    char line[81], minlin[81];
    uint16 h; 

    Deck DeckImage, DeckImage2; 

    cptr = get_glyph_quoted (cptr, fnSrc, 0);                  // get next param: source filename 
    if (fnSrc[0] == 0) return sim_messagef (SCPE_ARG, "Missing source filename \n");

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    if (strcmp(gbuf, "ASC") == 0) cMode='A'; else
        if (strcmp(gbuf, "DESC") == 0)  cMode='D'; else
            return sim_messagef (SCPE_ARG, "Unknown sorting order %s. Must be ASC or DESC\n", gbuf);

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    col1 = (int) get_uint (gbuf, 10, 80, &r);
    if (r != SCPE_OK) return r;
    if ((col1 < 1) || (col1 > 80)) return sim_messagef (SCPE_ARG, "Invalid sort column 1 (%d). Must be 1 to 80 \n", col1);

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    col2 = (int) get_uint (gbuf, 10, 80, &r);
    if (r != SCPE_OK) return r;
    if ((col2 < 1) || (col2 > 80)) return sim_messagef (SCPE_ARG, "Invalid sort column 2 (%d). Must be 1 to 80 \n", col2);

    if (col1 > col2) return sim_messagef (SCPE_ARG, "Sort column 1 (%d) must be lower or equal to sort column 2 (%d) \n", col1, col2);

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    if (*cptr == 0) {
        // missimg TO. fnDest is same as fnSrc
        sim_strlcpy(fnDest, fnSrc, sizeof(fnDest));
    } else {
        cptr = get_glyph (cptr, gbuf, 0);                       // get next param
        if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missing TO <file>\n");

        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        cptr = get_glyph_quoted (cptr, fnDest, 0);              // get next param: destination filename 
        if (fnDest[0] == 0) return sim_messagef (SCPE_ARG, "Missing destination filename \n");
    }

    deck_init(&DeckImage);
    r = deck_load(fnSrc, &DeckImage);
    if (r != SCPE_OK) return sim_messagef (r, "Cannot read deck (%s)\n", fnSrc);
    if (DeckImage.nCards < 1) {
        deck_free(&DeckImage);
        return sim_messagef (r, "Cannot sort empty deck (%s)\n", fnSrc);
    }

    // sort using very simple-quick-and dirty compare all with all 
    if (DeckImage.nCards > 1) for (nCurrent=0; nCurrent<DeckImage.nCards-1; nCurrent++) { 
        // get in minlin the lowest/highest card on cols 1..cols2 according to cMode
        nMin=-1; minlin[0]=0;
        for(n=nCurrent;n<DeckImage.nCards; n++) {
            // get columns to sort
            for (i=col1; i<=col2; i++) {
                h=DeckImage.p[n * 80 + i-1];
                c=sim_hol_to_ascii(h);
                c=sim_toupper(c);
                line[i-col1] = c; 
            }
            line[col2-col1+1]=0;
            // check if its the lowest/upper card
            if (strcmp(line, minlin) == ((cMode == 'A') ? -1:1)){
                sim_strlcpy(minlin, line, 80);
                nMin=n; 
            }
        }
        // swap cards
        if (nMin != nCurrent) {
            get_set_card_in_deck('G', image, &DeckImage, nMin);
            get_set_card_in_deck('G', image2, &DeckImage, nCurrent);
            get_set_card_in_deck('S', image2, &DeckImage, nMin);
            get_set_card_in_deck('S', image, &DeckImage, nCurrent); 
        }
    }

    // copy cards to destination deck except blank ones
    deck_init(&DeckImage2);
    for(n=0;n<DeckImage.nCards; n++) {
        get_set_card_in_deck('G', image, &DeckImage, n);
        // check if columns to sort are blank
        IsBlank=1; 
        for (i=col1; i<=col2; i++) {
            c=sim_hol_to_ascii(image[i]);
            if (c!=' ') {IsBlank=0; break; }
        }
        // discard blank cards on sorted columns
        if (IsBlank) continue; 
        // add card to destination deck
        add_to_deck(image, &DeckImage2); 
    }
    r = deck_save(fnDest, &DeckImage2, 0, -1);
    if (r != SCPE_OK) {
        sim_messagef (r, "Cannot write destination deck (%s)\n", fnDest);
    } else if ((sim_switches & SWMASK ('Q')) == 0) {
        sim_printf ("Deck with %d cards sorted on columns %d to %d %s (%s) \n", 
            DeckImage.nCards, col1, col2, (cMode == 'A') ? "ascending":"descending", fnDest);
    }
    deck_free(&DeckImage);
    deck_free(&DeckImage2);
    return SCPE_OK;
}

// carddeck ident <file> as "text" at <col> [ count at <col1> <col2> [ start at <num>] ]
static t_stat deck_ident_cmd(CONST char *cptr)
{
    char fnSrc[4*CBUFSIZE];
    char gbuf[4*CBUFSIZE];
    char ident[10];
    uint16 image[80];
    int i,n;
    int col, col1, col2, idlen, idnum0, idnum;
    char c; 
    t_stat r;

    Deck DeckImage, DeckImage2; 

    cptr = get_glyph_quoted (cptr, fnSrc, 0);                  // get next param: source filename 
    if (fnSrc[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename \n");

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    if (strcmp(gbuf, "AS") != 0) return sim_messagef (SCPE_ARG, "Missing AS \n");

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph_quoted (cptr, gbuf, 0);                // get next param: "text"
    if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote
    n=strlen(gbuf);
    if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0;           // remove trailing quote
    if (gbuf[0] == 0) return sim_messagef (SCPE_ARG, "Missing identification\n");
    idlen = strlen(gbuf); 
    if (idlen > 8) {
        return sim_messagef (SCPE_ARG, "Identification too long (len %d). Max 8 characters\n", idlen);
    }
    sim_strlcpy(ident, gbuf, sizeof(ident));

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    cptr = get_glyph (cptr, gbuf, 0);                       // get next param
    if (strcmp(gbuf, "AT") != 0) return sim_messagef (SCPE_ARG, "Missing AT \n");

    cptr = get_glyph (cptr, gbuf, 0);                       // get ident column
    col = (int) get_uint (gbuf, 10, 80, &r);
    if (r != SCPE_OK) return r;
    if ((col < 1) || (col > 80)) return sim_messagef (SCPE_ARG, "Invalid identification column (%d). Must be 1 to 80 \n", col);

    while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
    if (*cptr) {
        cptr = get_glyph (cptr, gbuf, 0);                       // get next param
        if (strcmp(gbuf, "COUNT") != 0) return sim_messagef (SCPE_ARG, "Missing COUNT \n");
        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        cptr = get_glyph (cptr, gbuf, 0);                       // get next param
        if (strcmp(gbuf, "AT") != 0) return sim_messagef (SCPE_ARG, "Missing AT \n");

        cptr = get_glyph (cptr, gbuf, 0);                       // get card count column1
        col1 = (int) get_uint (gbuf, 10, 80, &r);
        if (r != SCPE_OK) return r;
        if ((col1 < 1) || (col1 > 80)) return sim_messagef (SCPE_ARG, "Invalid column (%d). Must be 1 to 80 \n", col);

        cptr = get_glyph (cptr, gbuf, 0);                       // get card count column2
        col2 = (int) get_uint (gbuf, 10, 80, &r);
        if (r != SCPE_OK) return r;
        if ((col2 < 1) || (col2 > 80)) return sim_messagef (SCPE_ARG, "Invalid column (%d). Must be 1 to 80 \n", col);

        if (col1 > col2) return sim_messagef (SCPE_ARG, "Count column 1 (%d) must be lower or equal to column 2 (%d) \n", col1, col2);
        if (col2-col1+1 > 8) return sim_messagef (SCPE_ARG, "Too many columns for card count (%d). max 8 \n", col2-col1+1);

        idnum0=idnum=1; 

        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        if (*cptr) {
            cptr = get_glyph (cptr, gbuf, 0);                       // get next param
            if (strcmp(gbuf, "START") != 0) return sim_messagef (SCPE_ARG, "Missing START \n");
            while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
            cptr = get_glyph (cptr, gbuf, 0);                       // get next param
            if (strcmp(gbuf, "AT") != 0) return sim_messagef (SCPE_ARG, "Missing AT \n");

            cptr = get_glyph (cptr, gbuf, 0);                       // get card starting number 
            idnum = (int) get_uint (gbuf, 10, 99999999, &r);
            if (r != SCPE_OK) return r;
            idnum0=idnum; 
        }

    } else {
        col1=col2=idnum0=idnum=0; 
    }

    deck_init(&DeckImage);
    r = deck_load(fnSrc, &DeckImage);
    if (r != SCPE_OK) return sim_messagef (r, "Cannot read deck (%s)\n", fnSrc);
    if (DeckImage.nCards < 1) {
        deck_free(&DeckImage);
        return sim_messagef (r, "Cannot identify an empty deck (%s)\n", fnSrc);
    }

    // copy cards to destination deck 
    deck_init(&DeckImage2);
    for(n=0;n<DeckImage.nCards; n++) {
        get_set_card_in_deck('G', image, &DeckImage, n);
        // set identification text 
        for (i=0; i<idlen; i++) {
            c = ident[i]; 
            c = toupper(c); 
            if (col+i > 80) continue; 
            image[col-1+i]=sim_ascii_to_hol(c);
        }
        // set card count if any
        if (col1>0) {
            sprintf(gbuf, "%08d", idnum++); 
            for (i=0; i<col2-col1+1; i++) {
                c = gbuf[7-i]; 
                if (col2-i < 1 ) continue; 
                if (col2-i > 80) continue; 
                image[col2-1-i]=sim_ascii_to_hol(c);
            }
        }
        // add card to destination deck
        add_to_deck(image, &DeckImage2); 
    }
    r = deck_save(fnSrc, &DeckImage2, 0, -1);
    if (r != SCPE_OK) {
        sim_messagef (r, "Cannot write destination deck (%s)\n", fnSrc);
    } else if ((sim_switches & SWMASK ('Q')) == 0) {
        sim_printf ("Deck with %d cards identified as %s at column %d (%s) \n", 
                DeckImage.nCards, ident, col, fnSrc);
    }
    deck_free(&DeckImage);
    deck_free(&DeckImage2);
    return SCPE_OK;
}


static t_stat deck_cmd(int32 arg, CONST char *buf)
{
    char gbuf[4*CBUFSIZE];
    const char *cptr;

    cptr = get_glyph (buf, gbuf, 0);                   // get next param
    if (strcmp(gbuf, "-Q") == 0) {
        sim_switches |= SWMASK ('Q');
        cptr = get_glyph (cptr, gbuf, 0);
    }

    if (strcmp(gbuf, "JOIN") == 0) {
        return deck_join_cmd(cptr);
    }    
    if (strcmp(gbuf, "PRINT") == 0) {
        return deck_print_cmd(cptr, 'P');
    }
    if (strcmp(gbuf, "LIST") == 0) {
        return deck_print_cmd(cptr, 'L');
    }
    if (strcmp(gbuf, "ECHOLAST") == 0) {
        return deck_print_cmd(cptr, 'E');
    }
    if (strcmp(gbuf, "PUNCH") == 0) {
        return deck_punch_cmd(cptr);
    }
    if (strcmp(gbuf, "SORT") == 0) {
        return deck_sort_cmd(cptr);
    }
    if (strcmp(gbuf, "IDENT") == 0) {
        return deck_ident_cmd(cptr);
    }
    return sim_messagef (SCPE_ARG, "Unknown deck command operation\n");
}

// intercept    deposit IC or IR
// DEP IC  nnn    -> will reset to interpretation half-cycle, so next GO will fetch the instr from mem (from addr IC) to IR reg and then execute it 
// DEP IR  nnn    -> will reset to execution half-cycle, so next GO will execute instr in IR reg
// EX  HALFCYCLE  -> will display the next half cycle (either I or R) to be done 
t_stat i701_exdep_cmd (int32 flag, CONST char *cptr)
{
    char gbuf[CBUFSIZE];
    FILE *ofile;
    extern FILE    *sim_ofile ;
    extern int     NCYCLE;                   
    const char * cptr2; 

    cptr2 = get_glyph (cptr, gbuf, 0);                       // get param    
    while (gbuf[0]=='-') {
        cptr2 = get_glyph (cptr2, gbuf, 0);                  // skip any option -Switch
    }

    if (flag==EX_D) {
        if (strlen(gbuf) == 2) {
            if (strncmp(gbuf, "IC", 2)==0) {
                // deposit IC instruction counter 
                NCYCLE = 0; 
            } else if (strncmp(gbuf, "IR", 2)==0)  {
                // deposit IR instruction register
                NCYCLE = 1;
            }
        } 
    } else if (flag==EX_E) {
        if ((strlen(gbuf) == 9) && (strncmp(gbuf, "HALFCYCLE", 9)==0)) { 
            // examine halfcycle -> will display the half cycle to be executed next
            ofile = sim_ofile? sim_ofile: stdout;        /* no ofile? use stdout */
            fprintf (ofile, "HALFCYCLE: %s\n", 
                (NCYCLE == 0) ? "Interpretation":"Execution");
            if (sim_ofile) fclose (sim_ofile);                      /* close output file */
            return SCPE_OK;
        }
    }
    return exdep_cmd (flag, cptr);
}


