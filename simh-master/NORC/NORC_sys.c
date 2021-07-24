/* norc_sys.c: IBM NORC Simulator system interface.

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

#include "norc_defs.h"
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

char                sim_name[] = "IBM NORC";

REG                *sim_PC = &cpu_reg[0];

int32               sim_emax = 1;

DEVICE             *sim_devices[] = {
    &cpu_dev,
    &mt_dev,
    &lp_dev,
    &ctc_dev,
#if defined(CPANEL)
    &cp_dev,
#endif
    NULL
};

/* Device addressing words */
DIB  mt_dib  = { 8, &mt_cmd, &mt_ini };
DIB  lp_dib  = { 2, &lp_cmd, NULL };
DIB  ctc_dib = { 1, &ctc_cmd, NULL };

/* Simulator stop codes */
const char         *sim_stop_messages[] = {
    "Unknown error",
    "Check STOP",
    "Host I/O Error",
    "Tape Check",
    "End Of Tape Check",
    "Division by Zero", 
    "Invalid Float Index", 
    "Invalid Float Sign", 
    "Invalid Address", 
    "Unknown Opcode",
    "Printer Error",
    "Program STOP",
    0
};

/* Simulator SubOperations full names */
const char         *sim_subop_names[] = {
    "???",
    "Read Instruction", 
    "Decode",
    "Read Operand 1",
    "Read Operand 2",
    "Float Operands",
    "Add",
    "Sub",
    "Convert",
    "Mult",
    "Div",
    "Transfer 1 to 2",
    "Reset 2",
    "Shift Result",
    "Reset 1",
    "Round",
    "Word Add",
    "Word Shift",
    "Change M4",
    "Change M6",
    "Change M8",
    "Write Block",
    "Read Block",
    "Scan",
    "Terminate",
    "Operation Completed",
    0
};

t_stat NORC_exdep_cmd (int32 flag, CONST char *cptr);
t_stat set_csw_btn_cmd(int32 flag, CONST char *cptr); 

#define HLP_STEP        "*Commands Running_A_Simulated_Program STEP"

static CTAB aux_cmds [] = {
/*    Name          Action Routine     Argument   Help String */
/*    ----------    -----------------  ---------  ----------- */
    { "STEP",       &run_cmd,          RU_STEP,   HLP_STEP,       NULL, &run_cmd_message },
    { "DEPOSIT",    &NORC_exdep_cmd,   EX_D,      "*Commands Examining_and_Changing_State",    NULL, NULL },
    { "EXAMINE",    &NORC_exdep_cmd,   EX_E,      "*Commands Examining_and_Changing_State",    NULL, NULL },
    { "CTC",        &ctc_op_cmd,       0,         "CTC operation",                  },
    { "PUNCH",      &keypunch_cmd,     0,         "Punch cards to be used by CTC",  },
    { "PRINT",      &printcards_cmd,   0,         "Off-Line printing of Punched cards",  },
    { "SWITCH",     &set_csw_btn_cmd,  1,         "Set switch",                     },
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

// representation of word digit 0-9 in card including Y(12) and X(11) punchs
char    digits_ascii[31] = {
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',   /* 0-9 */  
          '?', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',   /* 0-9 w/HiPunch Y(12) */
          '!', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',   /* 0-9 w/Negative Punch X(11) */
          0};

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
    return '~';
}


/* Initialize vm  */
void vm_init(void) {
    static int inited = 0;
    extern int                 V;                           // Address to be accesed in CRT memory
    extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];

    if (inited == 1) return;   /* Be sure to only do these things once */        
    inited = 1;

    // Initialize vm memory to zero 
    crt_reset();

    // init specific commands
    sim_vm_cmd = aux_cmds;                       /* set up the auxiliary command table */

    memset(lptPrintOut, 0, sizeof(lptPrintOut));
}

/* image file into memory.  */

int sLinToInt(int nlin, char * slin,  int n, int len)
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
            return 0;
        }
        nn = nn*10 + (c-'0'); 
    }
    return nn; 
}

t_stat sim_load(FILE * fileref, CONST char *cptr, CONST char *fnam, int flag)
{
    int lo_addr, hi_addr, addr, nlin, i, blank, n;
    t_int64 P,Q,R,S,T;
    char slin[1024];
    char c;
    extern int V;                           // Address to be accesed in CRT memory

    if (*cptr != 0) return SCPE_ARG;
    if (flag != 0) return sim_messagef (SCPE_NOFNC, "Command Not Implemented\n");

    memset(CRT_Symbolic_Buffer, 0, sizeof(CRT_Symbolic_Buffer));            // clear symbolic info

    lo_addr=hi_addr=addr=0; nlin=0; // init lowest/highest addr loaded
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
        // NNNN PP QQ RRRR SSSS TTTT symbolic info text...(max 79 chars)
        // check if blank line
        blank=1;
        if (sim_strncasecmp(slin, "NNNN ",  5) == 0) {
            // if line start with NNNN ... ignore it
        } else if (sim_strncasecmp(slin, "LABEL",  5) == 0) {
            // if line start with LABEL ... issue error: possibly loading an assembler source code
            // instead of absolute binary program
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Line starts with 'LABEL': Possible assembler source code line instead of aboslute binary program\n");
            return SCPE_IERR;
        } else if (slin[0] == ';') {
            // if line start with ";" ... ignore it
        } else for (i=0;i<25;i++) {
            c=slin[i];
            if (c<32) break;
            if ((c>='0') && (c<='9')) blank=0;
            if ((c=='[') || (c==']')) blank=0;
            if (c=='?') {
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("Warning: Has a ? character\n", c, c, i+1);
                break;
            }
            if ((c!=' ') && (c!='?') && (c!='[') && (c!=']') && ((c<'0') || (c>'9'))) {
                blank=1; 
                sim_printf("line %d: %s\n", nlin, slin); 
                sim_printf("invalid char '%c' (code %d) at pos %d\n", (c<32) ? 32:c, c, i+1);
                break;
            } // if text in prog area -> blank line
        }
        if (blank) continue; // ignore blank lines
        // get address to load to
        n=sLinToInt(nlin, slin,  0, 4); // address
        if ((n==0) && (addr==0)) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Missing initial load address\n");
            continue; 
        } else if (n==0) { 
            addr++;
        } else {
            addr=n;
        }
        if (addr >= 3600) {
            sim_printf("line %d: %s\n", nlin, slin); 
            sim_printf("Cannot store at address %d\n", addr);
            continue; 
        }
        if ((lo_addr == 0) || (lo_addr > addr)) lo_addr = addr;  
        if ((hi_addr == 0) || (hi_addr < addr)) hi_addr   = addr;  
        // get data to load at this addr
        P=sLinToInt(nlin, slin,  5, 2); 
        Q=sLinToInt(nlin, slin,  8, 2); 
        R=sLinToInt(nlin, slin, 11, 4); 
        S=sLinToInt(nlin, slin, 16, 4); 
        T=sLinToInt(nlin, slin, 21, 4); 
        // store word
        CRT[addr] = P*D14 + Q*D12 + R*D8 + S*D4 + T;
        // store symbolic info
        memset(&CRT_Symbolic_Buffer[addr * 80], 0, 80); // clear
        sim_strlcpy(&CRT_Symbolic_Buffer[addr * 80], &slin[26], 79);

    }
    sim_printf("loaded %s from %04d to %04d address\n", fnam, lo_addr, hi_addr);

    // set V to highest loaded addr in CRT men
    V=hi_addr; 
    return SCPE_OK;
}

/* Opcodes */
t_opcode  base_ops[100] = {            
        // opcode         name            
        {OP_ADD,         "Add",                   0}, 
        {OP_ADDM,        "Add Minus",             0}, 
        {OP_SUB,         "Sub",                   0}, 
        {OP_SUBM,        "Sub Minus",             0}, 
        {OP_MUL,         "Mult",                  0}, 
        {OP_MULM,        "Mult Minus",            0}, 
        {OP_DIV,         "Div",                   0}, 
        {OP_DIVM,        "Div Minus",             1}, 
        {OP_ABS,         "Abs",                   1}, 
        {OP_TRUNC,       "Trunc",                 1}, 
        {OP_ADD_SPE,     "Add Special",           1}, 
        {OP_ADDM_SPE,    "Add Minus Special",     1},  
        {OP_SUB_SPE,     "Sub Special",           1}, 
        {OP_SUBM_SPE,    "Sub Minus Special",     1}, 
        {OP_MUL_SPE,     "Mult Special",          1}, 
        {OP_MULM_SPE,    "Mult Minus Special",    1}, 
        {OP_DIV_SPE,     "Div Special",           1}, 
        {OP_DIVM_SPE,    "Div Minus Special",     1}, 
        {OP_ABS_SPE,     "Abs Special",           1}, 
        {OP_TRUNC_SPE,   "Trunc Special",         1}, 
        {OP_META_ADD,    "Meta Add",              1}, 
        {OP_META_SUB,    "Meta Sub",              1}, 
        {OP_META_MASK,   "Meta Mask",             1}, 
        {OP_CHMOD,       "Change Modifiers",      0}, 
        {OP_CHMOD_CL4,   "Change Mod Clr M4",     0}, 
        {OP_CHMOD_CL6,   "Change Mod Clr M6",     0}, 
        {OP_CHMOD_CL46,  "Change Mod Clr M46",    0}, 
        {OP_CHMOD_CL8,   "Change Mod Clr M8",     0}, 
        {OP_CHMOD_CL48,  "Change Mod Clr M48",    0}, 
        {OP_CHMOD_CL68,  "Change Mod Clr M68" ,   0}, 
        {OP_CHMOD_CL468, "Change Mod Clr M468",   0}, 
        {OP_ADDM4_TRNS,  "Add R to M4, Tr NEQ S", 2}, 
        {OP_TR_MOD,      "Transfer M4 M6 M8 to REG1",1},
        {OP_TR,          "Transfer",              1}, 
        {OP_TR_STOP,     "Transfer Stop",         1}, 
        {OP_TR_ROUND,    "Round and transfer",    1}, 
        {OP_TR_SGN,      "Tr on Sign",            1}, 
        {OP_TR_OV,       "Tr on Overflow",        1}, 
        {OP_TR_AI,       "Tr on Adjuted Index",   1}, 
        {OP_TR_ZR,       "Tr on Zero Result",     1}, 
        {OP_TR_EOF,      "Tr on EOF",             0}, 
        {OP_TR_TCHK,     "Tr on Tape Check",      0}, 
        {OP_TR_PRT,      "Tr on PRT ready",       0}, 
        {OP_TR_EQ,       "Tr on Equal",           1}, 
        {OP_TR_EQ_STOP,  "Tr Stop on Equal",      1}, 
        {OP_TR_NEQ,      "Tr on Not Equal",       1}, 
        {OP_TR_NEQ_STOP, "Tr Stop on Not Equal",  1}, 
        {OP_TSTC74,      "Tr On Cond Sw 74",      1}, 
        {OP_TSTC75,      "Tr On Cond Sw 75",      1}, 
        {OP_TSTC76,      "Tr On Cond Sw 76",      1}, 
        {OP_TSTC77,      "Tr On Cond Sw 77",      1}, 
        {OP_TSTC78,      "Tr On Cond Sw 78",      1}, 
        {OP_TSTC79,      "Tr On Cond Sw 79",      1}, 
        {OP_WAIT_PRT,    "Wait Printer",          1}, 
        {OP_PRT1,        "Print on 1",            1}, 
        {OP_PRT2,        "Print on 2",            1}, 
        {OP_PRT1_SPE,    "Print on 1 special",    1}, 
        {OP_PRT2_SPE,    "Print on 2 special",    1}, 
        {OP_WRITE,       "Write Tape",            0}, 
        {OP_WRI_OUTPUT,  "Write Output",          0}, 
        {OP_DELETE,      "Delete Tape",           0}, 
        {OP_DEL_OUTPUT,  "Delete Output",         0}, 
        {OP_READ,        "Read Tape Forward",     0}, 
        {OP_READ_BWRD,   "Read Tape Backward",    0}, 
        {OP_VERIFY,      "Verify Forward",        0}, 
        {OP_VER_BWRD,    "Verify Backward",       0}, 
        {OP_REWIND,      "Rewind",                0}, 
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

t_stat
fprint_sym(FILE * of, t_addr addr, t_value * val, UNIT * uptr, int32 sw)
{
    t_int64 d;
    int mode;
    int n;

    extern int                 NSUBOP;                      // indicates current machine suboperation to be executed, 0=exec whole instruction

    d = *val; 
    mode = 3; // plain 16 digits word

    // overide mod if user uses a switch    
    if (sw & SWMASK('W') ) mode=2; else   // decimal word
    if (sw & SWMASK('M') ) mode=1; else   // machime instr
    if (sw & SWMASK('F') ) mode=0;        // float number
    
    /* Print value */
    
    if (mode == 3) {
        // decimal word
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
    } else if (mode == 2) {
        // decimal word
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d ", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d ", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d ", n);    
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
    } else if (mode==0) {
        // float number
        n = (int) Shift_Digits(&d, 2);
        fprintf(of, "%02d ", n);    // fprintf Decimal Index
        n = (int) Shift_Digits(&d, 1);
        fprintf(of, "%1d ", n);     // sign
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);  // mantissa  
        n = (int) Shift_Digits(&d, 4); fprintf(of, "%04d", n);    
        n = (int) Shift_Digits(&d, 5); fprintf(of, "%05d", n);    
    } else {
        // machine instruction
        n = (int) Shift_Digits(&d, 2);
        fprintf(of, "%02d ", n);    // fprintf P
        n = (int) Shift_Digits(&d, 2);
        fprintf(of, "%02d ", n);    // fprintf Q
        n = (int) Shift_Digits(&d, 4);
        fprintf(of, "%04d ", n);    // fprintf R
        n = (int) Shift_Digits(&d, 4);
        fprintf(of, "%04d ", n);    // fprintf S
        n = (int) Shift_Digits(&d, 4);
        fprintf(of, "%04d", n);     // fprintf T
        if (NSUBOP > 1) {
            fprintf(of, " - NSUBOP %d", NSUBOP);     // SubOperation number
        }
    }
    return SCPE_OK;
}

/* read n digits, optionally with embebbed spaces

   Inputs:
        *cptr   =       pointer to input string
        n       =       max number of digits allowed
   Outputs:
        d       =       parsed value
        nDigits =      number of digits parsed
        return cptr     
*/

CONST char * parse_n(t_int64 *d, CONST char *cptr, int n, int * nDigits)
{
    int i = 0;

    *d = 0;
    if (nDigits) *nDigits = 0;
    while (1) {
        if ((n == 16) && (isspace(*cptr))) {
            cptr++;  // on 16 digit words, allow spaces
            continue;
        }
        if (*cptr < '0' || *cptr > '9') break;
        if (i++ > n) {
            cptr++;
        } else {
            *d = (*d * 10) + (*cptr++ - '0');
            if (nDigits) (*nDigits) += 1; // incr number of digits parsed
        }
    }
    if (n ==  4) {*d = *d % D4; }
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

    while (isspace(*cptr)) cptr++;
    d = 0; 
    cptr = parse_n(&d, cptr, 16, NULL);

    *val = (t_value) d; 
    return SCPE_OK;
}

/* Helper functions */


// additional SCP commands to press/set switches

struct {
    int id;              // 0=button, >1 switch ident
    const char *name;    // name of button/switch to be used in scp command
} csw_btn_def[] = {
    // buttons
    {100, "Keyboard"},   
    {101, "Register 1 to CRT"},
    {102, "Register 1 from CRT"},
    {103, "Register 1 from REG2"},
    {104, "Register 1 reset"},
    {105, "Register 2 to CRT"},
    {106, "Register 2 from CRT"},
    {107, "Register 2 from REG1"},
    {108, "Register 2 reset"},
    {110, "V Entry to V"},
    {111, "V to U"},
    {112, "U to V"},
    {113, "U+1 to V"},
    {114, "V to M4"},
    {115, "V to M6"},
    {116, "V to M8"},
    {117, "REG1 to IREG"},
    {120, "Carriage A"},  
    {121, "Carriage B"},  
    {130, "SubOp Reset"},  
    {131, "Indicator Reset"},
    {140, "CRT Reset"},  
    {150, "Read Forward"},  
    {151, "Read Backward"},  
    {152, "Rewind"},  
    // switches
    {  1, "SW74"},     
    {  2, "SW75"},     
    {  3, "SW76"},     
    {  4, "SW77"},     
    {  5, "SW78"},     
    {  6, "SW79"},     
    {  7, "Overflow"},     
    {  8, "Adjust Index"},     
    {  9, "Zero Result"},     
    { 10, "Tape end of file"},     
    { 11, "Tape Check"},     
    { 12, "Write Output"},  
    { 13, "Floated Index"},  
    { 14, "Program Check"},  
    { 15, "CRT Check"},  
    { 16, "PrinterCheck"},  
    { 21, "Tape address 01"},  
    { 22, "Tape address 02"},  
    { 23, "Tape address 03"},  
    { 24, "Tape address 04"},  
    { 25, "Tape address 05"},  
    { 26, "Tape address 06"},  
    { 27, "Tape address 07"},  
    { 28, "Tape address 08"},  
    { 29, "Tape address 09"},  
    { 30, "Tape address 10"},  
    { 31, "Tape address 11"},  
    { 32, "Tape address 12"},  
    { 40, "Keyboard Entry"},  
    { 41, "Register 1 CRT Address"},  
    { 42, "Register 2 CRT Address"},  
    { 43, "V Entry"},  
    { 44, "Source of Instruction"},  
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
    t_int64 d, reg; 

    extern int SaveOperand(int Addr, t_int64 d, int * CpuTicksUsed);
    extern t_int64 ReadOperand(int Addr, int * CpuTicksUsed);

    extern int                 U;                           // Saved Program counter during opcode execution
    extern int                 V;                           // Address to be accesed in CRT memory
    extern int                 M4,M6,M8;                    // address modifier registers
    extern t_int64             IREG;                        // instruction register
    extern t_int64             REG1, REG2;                  // Storage registers
    extern int                 NSUBOP;                      // indicates current machine suboperation to be executed, 0=exec whole instruction
    extern int                 ESUBOP;                      // already executed suboperations bitamap 
    extern int                 P,Q;                         // decoded IREG
    extern t_int64             CRT_BUS;                     // last value in crt bus to be read/write to CRT          

    extern int Console_CurrentSubOp;         // already executed suboperations for indicator panel display
    extern int Console_Sw74[6]; // selection switch for opcodes 74..79: =0 -> set to stop, =1 -> set to transfer, =2 -> set to off
    extern int Console_Sw64[5]; // selection switch =0 -> set to proceed, =1 -> set to stop
    extern int Console_Sw_ProgCHKStop;  // selection switch =0 -> set to proceed, =1 -> set to stop
    extern int Console_Sw_CRTCHKStop;       // selection switch for CRT CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
    extern int Console_Sw_PrinterCHKStop;   // selection switch for PRINTER CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
    extern int Console_Tape_Addr[12];  // tape addr selection addr
    extern int Console_Sw_Write_Output;   // selection switch for WRITE OUTPUT =0 -> set to off, =1 -> set to on
    extern int Console_Sw_Floated_Index;  // selection switch for FLOATED INDEX =0 -> set to off, =1 -> set to on
    extern int Console_Sw_Reg1_CRTAddr; // selection switch for Register 1 CRT Address
    extern int Console_Sw_Reg2_CRTAddr; // selection switch for Register 2 CRT Address
    extern int Console_Sw_VEntry_Addr;   // selection switch for V Entry Address
    extern int Console_Sw_KeyboardEntry; // selection switch =1 -> digits typed on keyboard goes to reg1, =2 -> to reg 2, =0 -> to none
    extern int Console_Sw_SourceOfInstr; // selection switch =0 -> source of instr V, =1 -> source U, =2 -> source IREG
    extern int Console_Sw_TapeUnitSelector; // tape unit select for read/read backwds/rew button on console

    extern int Console_LiPrtChk;             // console lights for priter check flag (because writting to 0001-0007 while print cycle in progress)
    extern int Console_LiAddrChk;            // console lights for address check flag 

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
        // press keyboard N ...
        // N can be from one to 16 digits. Separation spaces permited
        if (*cptr==0) return sim_messagef (SCPE_ARG, "Missing digits to type on console keyboard\n");;
        cptr = parse_n(&d, cptr, 16, &n); // return d=value types, n=number of digits
        if (Console_Sw_KeyboardEntry==1) reg=REG1; else
            if (Console_Sw_KeyboardEntry==2) reg=REG2; else return sim_messagef (SCPE_ARG, "Keyboard entry set to OFF. Keyboard entry ignored\n");;
        // insert digits in REG on right side, shifting current register value to left
        Shift_Digits(&reg, n);
        reg=reg+d;
        if (Console_Sw_KeyboardEntry==1) {
            REG1=reg; 
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Use keyboard to set REG1: %08d%08d \n", printfw(REG1));
        } else {
            REG2=reg;
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Use keyboard to set REG2: %08d%08d \n", printfw(REG2));
        }
        return SCPE_OK;
    } else if ((id == 101) || (id == 105)) {
        // press "Register 1 to CRT"
        // press "Register 2 to CRT"
        d=(id==101) ? REG1:REG2;
        V = (id==101) ? Console_Sw_Reg1_CRTAddr : Console_Sw_Reg2_CRTAddr;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: REG%d=%08d%08d to CRT[%d]\n",
            (id==101) ? 1:2, printfw(d), V);
        if (V >= MemSize()) {
            Console_LiAddrChk=1; 
            return STOP_ADDR;     
        }
        CRT[V]=CRT_BUS=d; 
    } else if ((id == 102) || (id == 106)) {
        // press "Register 1 from CRT"
        // press "Register 2 from CRT"
        V = (id==102) ? Console_Sw_Reg1_CRTAddr : Console_Sw_Reg2_CRTAddr;
        if (V >= MemSize()) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: REG%d from CRT[%d]\n",
                (id==102) ? 1:2, V);
            Console_LiAddrChk=1; 
            return STOP_ADDR;     
        }
        d=CRT_BUS=CRT[V]; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: REG%d=%08d%08d from CRT[%d]\n",
            (id==102) ? 1:2, printfw(d), V);
        if (id==102) REG1=d; else REG2=d; 
    } else if (id == 103) {
        // press "Register 1 from REG 2"
        REG1=REG2;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: REG1 from REG2: %08d%08d \n", printfw(REG1));
    } else if (id == 104) {
        // press "Register 1 reset"
        REG1=0;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Reset REG1\n");
    } else if (id == 107) {
        // press "Register 2 from REG 1"
        REG2=REG1; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: REG2 from REG1: %08d%08d \n", printfw(REG2));
    } else if (id == 108) {
        // press "Register 2 reset"
        REG2=0;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Reset REG2\n");
    } else if (id == 110) {
        // press "V Entry to V"
        V=Console_Sw_VEntry_Addr;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set V from V Entry: %04d \n", V);
    } else if (id == 111) {
        // press "V to U"
        U=V;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set U from V: %04d \n", U);
    } else if (id == 112) {
        // press "U to V"
        V=U;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set V from U: %04d \n", V);
    } else if (id == 113) {
        // press "U+1 to V"
        V=U+1;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set V from U+1: %04d \n", V);
    } else if (id == 114) {
        // press "V to M4"
        M4=V;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set M4 from V: %04d \n", V);
    } else if (id == 115) {
        // press "V to M6"
        M6=V;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set M6 from V: %04d \n", V);
    } else if (id == 116) {
        // press "V to M8"
        M8=V;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set M8 from V: %04d \n", V);
    } else if (id == 117) {
        // press "REG1 to IREG"
        IREG=REG1;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Set IREG from REG1: %08d%08d \n", printfw(REG1));
    } else if ((id == 120) || (id == 121)) {
        // press "carriage A | B"
        n=lpt_printline(&lp_unit[id-120+1], "");
        if (n<0) {
            Console_LiPrtChk=1; 
            return STOP_PRINTER;
        }
    } else if (id == 130) {
        // press "SubOp Reset"
        // also resets subop executed displayed into indicator panel 
        Console_CurrentSubOp = NSUBOP = ESUBOP = 0;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: SubOp RESET \n");
    } else if (id == 131) {
        // press "Indicator Reset"
        cpu_reset_indicator_lights();
    } else if (id == 140) {
        // press "CRT Reset"
        crt_reset();
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: CRT RESET \n");
    } else if ((id >= 150) && (id <= 153)) {
        // manual real operations
        // press "Read Forward" MTn
        // press "Read Backward" MTn
        // press "Rewind" MTn
        // where MTn id MT1 to MT8
        char sMT[4] = "MTn";
        int nMT = -1; 
        if (*cptr==0) return sim_messagef (SCPE_ARG, "Missing tape (MT1 to MT8)\n");
        for (n=1;n<=8;n++) {
            sMT[2]='0' + n;
            if (sim_strncasecmp(cptr, sMT,  32) == 0) {
                nMT=n; break; 
            }
        }
        if (nMT < 0) return sim_messagef (SCPE_ARG, "Invalid tape (should be MT1 to MT8)\n");;
        Console_Sw_TapeUnitSelector = nMT;
        P=-1; 
        Q=(id == 150) ? OP_VERIFY : (id == 151) ? OP_VER_BWRD : OP_REWIND;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Console: Manual %s %s\n", 
            (id == 150) ? "Read Forward" : (id == 151) ? "Read Backward" : "Rewind", sMT);
        run_cmd(RU_GO, "");
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
    if ((id>=1) && (id<=6)) {
        // switch sw74 to transfer|off|stop
        // Console_Sw74[n]=2 -> Switch at 'OFF', =1 -> Switch at 'TRANSFER' . =0 -> Switch at 'STOP'
        if (sim_strncasecmp(gbuf, "OFF",      32) == 0) n=2; else
        if (sim_strncasecmp(gbuf, "TRANSFER", 32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "STOP",     32) == 0) n=0; else
        return SCPE_ARG; 
        Console_Sw74[id-1]=n;
    } else if ((id>=7) && (id<=11)) {
        // switch "Overflow"|... to proceed|stop
        // Console.PrSw64[n]=0 -> Switch at 'PROCEED', =1 -> Switch at 'STOP'
        // sw64[0] is overflow flag, sw64[1] is Index Adjust, [2] is Zero result, 
        // sw64[3] is Tape End of File, sw64[4] is Tape check
        if (sim_strncasecmp(gbuf, "PROCEED",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "STOP",     32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw64[id-7]=n;
    } else if (id==12) {
        // switch "Write Output" to on|off
        if (sim_strncasecmp(gbuf, "OFF",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "ON",   32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw_Write_Output=n;
    } else if (id==13) {
        // switch "Floated Index" to on|off
        if (sim_strncasecmp(gbuf, "OFF",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "ON",   32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw_Floated_Index=n;
    } else if (id==14) {
        // switch "Program Check" to proceed|stop
        if (sim_strncasecmp(gbuf, "PROCEED",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "STOP",     32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw_ProgCHKStop=n;
    } else if (id==15) {
        // switch "CRT Check" to proceed|stop
        if (sim_strncasecmp(gbuf, "PROCEED",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "STOP",     32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw_CRTCHKStop=n;
    } else if (id==16) {
        // switch "Printer Check" to proceed|stop
        if (sim_strncasecmp(gbuf, "PROCEED",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "STOP",     32) == 0) n=1; else
        return SCPE_ARG; 
        Console_Sw_PrinterCHKStop=n;
    } else if ((id>=21) && (id<=32)) {
        // switch "Tape address NN" to MTn|none
        // where NN=01..12, n=1..8
        char sMT[4] = "MTn";
        int nMT = -1; 
        for (n=1;n<=8;n++) {
            sMT[2]='0' + n;
            if (sim_strncasecmp(gbuf, "NONE",  32) == 0) nMT=0; else
            if (sim_strncasecmp(gbuf, sMT,  32) == 0) {
                nMT=n; break; 
            }
        }
        if (nMT < 0) return SCPE_ARG; 
        Console_Tape_Addr[id-21]=nMT;
    } else if (id==40) {
        // switch "Keyboard Entry" to Reg1|Reg2|off
        if (sim_strncasecmp(gbuf, "OFF",  32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "REG1", 32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "REG2", 32) == 0) n=2; else
        return SCPE_ARG; 
        Console_Sw_KeyboardEntry=n;
    } else if ((id>=41) && (id<=43)) {
        // switch "Register 1 CRT Address" to NNNN
        // switch "Register 2 CRT Address" to NNNN
        // switch "V Entry" to NNNN
        // where NNNN is four digit number
        parse_n(&d, gbuf, 4, NULL);
        if (id==41) Console_Sw_Reg1_CRTAddr=(int) d;
        if (id==42) Console_Sw_Reg2_CRTAddr=(int) d;
        if (id==43) Console_Sw_VEntry_Addr=(int) d;
    } else if (id==44) {
        // switch "Source of Intructions" to V|U|IREG
        // also does an implicit SubOp reset
        if (sim_strncasecmp(gbuf, "V",    32) == 0) n=0; else
        if (sim_strncasecmp(gbuf, "U",    32) == 0) n=1; else
        if (sim_strncasecmp(gbuf, "IREG", 32) == 0) n=2; else
        return SCPE_ARG; 
        Console_Sw_SourceOfInstr=n;
    } else return sim_messagef (SCPE_ARG, "Unknown switch name\n"); 
    return SCPE_OK;
}


// intercept    deposit V
// DEP V nnn    -> will reset SubOperation, so next GO will execute the opcode from start
// EX  NSUBOP   -> will display the suboperations already done
t_stat NORC_exdep_cmd (int32 flag, CONST char *cptr)
{
    char gbuf[CBUFSIZE];
    FILE *ofile;
    extern FILE    *sim_ofile ;
    extern int     NSUBOP;                      // indicates current machine suboperation number to be executed, 0=exec whole instruction
    extern int     ESUBOP;                      // already executed suboperations bitamap 
    int n;
    const char * cptr2; 

    cptr2 = get_glyph (cptr, gbuf, 0);                       // get param    
    while (gbuf[0]=='-') {
        cptr2 = get_glyph (cptr2, gbuf, 0);                  // skip any option -Switch
    }

    if (flag==EX_D) {
        if ((strlen(gbuf) == 1) && (strncmp(gbuf, "V", 1)==0))  {
            // deposit V 
            NSUBOP = ESUBOP = 0;
        } 
    } else if (flag==EX_E) {
        if ((strlen(gbuf) == 6) && (strncmp(gbuf, "NSUBOP", 6)==0)) { 
            // examine nsub -> will display the suboperations already done
            ofile = sim_ofile? sim_ofile: stdout;        /* no ofile? use stdout */
            fprintf (ofile, "NSUBOP: %d\n", NSUBOP);
            for (n=1; n<=25; n++) {
                if (ESUBOP & (1 << n)) fprintf (ofile, "- %s \n", sim_subop_names[n]);
            }
            if (sim_ofile) fclose (sim_ofile);                      /* close output file */
            return SCPE_OK;
        }
    }
    return exdep_cmd (flag, cptr);
}


