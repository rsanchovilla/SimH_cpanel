/* MarkI_sys.c: Ferranti computers Simulator system interface.

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
ROBERTO SANCHO BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

#include "MarkI_defs.h"
#include <ctype.h>

/* SCP data structures and interface routines

sim_name             simulator name string
sim_PC               pointer to saved PC register descriptor
sim_emax             number of words for examine
sim_devices          array of pointers to simulated devices
sim_stop_messages    array of pointers to stop messages
sim_load             binary loader
*/

char                sim_name[] = "Ferranti Mark I";

REG                *sim_PC = &cpu_reg[0];

int32               sim_emax = 1;

DEVICE             *sim_devices[] = {
    &cpu_dev,
    &ptr_dev,
    &ptp_dev,
    &lpt_dev,
    &drum_dev,
    &perforator_dev,
#if defined(CPANEL)
    &cp_dev,
#endif
    NULL
};

/* Simulator stop codes */
const char         *sim_stop_messages[] = {
    "Unknown error",
    "Program Stop",
    "Host I/O Error",
    "Unknown Opcode",
    "PaperTape EOF",
    "No Tape in PTR",
    "Continuous Hoot",
    "Dynamic Stop",
    "Transfer to //// instruction", 
    0
};

t_stat appendtotape_cmd(int32 flag, CONST char *cptr);
t_stat feed_ptr_cmd(int32 flag, CONST char *cptr);
t_stat MarkI_exdep_cmd (int32 flag, CONST char *cptr);
t_stat perforator_typein_cmd(int32 flag, CONST char *cptr);


#define HLP_STEP        "*Commands Running_A_Simulated_Program STEP"

static CTAB aux_cmds [] = {
    /*    Name          Action Routine     Argument   Help String */
    /*    ----------    -----------------  ---------  ----------- */
    { "STEP",         &run_cmd,          RU_STEP,   HLP_STEP,       NULL, &run_cmd_message },
    { "DEPOSIT",      &MarkI_exdep_cmd,  EX_D,      "*Commands Examining_and_Changing_State",    NULL, NULL },
    { "FEEDPTR",      &feed_ptr_cmd,     0,         "Send chars to PaperTape reader"       }, 
    { "TYPEIN",       &perforator_typein_cmd, 0,    "Type in perforator to punch a tape"   },
    { "APPENDTOTAPE", &appendtotape_cmd, 0,         "Append paper tape"                    }, 
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


int charset_MarkI[42][2] = {
     0, '/', //  00000     equivalence between chars and 'backwards binary used in MarkI
     1, 'E', //  10000     LSB is leftmost bit, MSB is rightmost bit
     2, '@', //  01000     See Ferranti Mark I Programming Manual, first edition - CHM 102724592-05-01-acc.pdf
     3, 'A', //  11000     page 3
     4, ':', //  00100
     5, 'S', //  10100
     6, 'I', //  01100
     7, 'U', //  11100
     8, '#', //  00010     MarkI uses here typewrite character 1/2 (one half) for value 8
     9, 'D', //  10010     and char pound for value 0x1F. unfortunatelly, such chars are 
    10, 'R', //  01010     not represented in stantard 7bit ascii. we use char 
    11, 'J', //  11010     ascii # and $ as synomym
    12, 'N', //  00110
    13, 'F', //  10110     Other synonyms stated in programming manual page 3 are:
    14, 'C', //  01110     % for /, 1/2 for 1/4, $ for pound
    15, 'K', //  11110     2 for @, 4 for :, 8 for 1/4, 5 for ", 0 for pound
    16, 'T', //  00001
    17, 'Z', //  10001
    18, 'L', //  01001
    19, 'W', //  11001
    20, 'H', //  00101
    21, 'Y', //  10101
    22, 'P', //  01101
    23, 'Q', //  11101
    24, 'O', //  00011
    25, 'B', //  10011
    26, 'G', //  01011
    27, '"', //  11011
    28, 'M', //  00111
    29, 'X', //  10111
    30, 'V', //  01111
    31, '$', //  11111

    31, '£', // 31 11111 0 is synomym of pound 
     8, '¼', // 8  00010 8 is synomym of 1/4 
     8, '½', // 8  00010 8 is synomym of 1/4 

     0, '%', // 0  00000 % is synonym of /
     2, '2', // 2  01000 2 is synonym of @
     4, '4', // 4  00100 4 is synonym of :
     8, '8', // 8  00010 8 is synomym of 1/4 
    27, '5', // 27 11011 5 is synonym of "
    31, '0', // 31 11111 0 is synomym of pound 

    -1         // end of table
};

char FigureShift_charset_MarkI[2][32] = {
    "/E@A:SIU#DFZ" "LWY"      "PMQ\"$",   // letter shift
    "0123456789. " "\r\n\x01" "+-=\"$"    // figure shift
}; 

char * charset_BAutocode[][2] = {
  // typedin   punched  
     "1",      "E", 
     "2",      "@",                                       
     "*",      "A",                                       
     "4",      ":",                                       
     "(",      "S",                                       
     ")",      "I",                                       
     "7",      "U",                                       
     "8",      "#",                                       
     "!=",     "D",                                       
     "=",      "R",                                     
     "-",      "J",                                    
     "v",      "N",                                    
     "\n",     "F",  // line feed                                
     ",",      "K",  // comma                                        
     "0",      "T",                           
     ">",      "Z",                           
     ">=",     "L",                           
     "3",      "W",                           
     "j",      "H",                           
     "5",      "Y",                           
     "6",      "P",                           
     "/",      "Q",  // division operator
     "x",      "O",  // for circled cross -> this is the multiply operator
     "9",      "B",                          
     "+",      "G",                          
     ".",      "M",                           
     "n",      "X",                          
     "\r",     "V",  // carriage return
                    
     "%",      "Q",  // alternate typein char for division operator
                       
     "A",      "\"E/", 
     "B",      "\"@/", 
     "C",      "\"A/", 
     "D",      "\":/", 
     "E",      "\"S/", 
     "F",      "\"I/", 
     "G",      "\"U/", 
     "H",      "\"#/", 
     "I",      "\"D/", 
     "J",      "\"R/", 
     "K",      "\"J/", 
     "L",      "\"N/", 
     "M",      "\"F/", 
     "N",      "\"C/", 
     "O",      "\"K/", 
     "P",      "\"T/", 
     "Q",      "\"Z/", 
     "R",      "\"L/", 
     "S",      "\"W/",               
     "T",      "\"H/", 
     "U",      "\"Y/", 
     "V",      "\"P/", 
     "W",      "\"Q/", 
     "X",      "\"O/", 
     "Y",      "\"B/", 
     "Z",      "\"G/", 
     "?",      "\"X/",               
     "$",      "\"V/",               
     0,0};


// translate sLinIn to sLinOut (must have at least 1000 chars size)
// uses an internal buffer, so sLinIn and sLinOut can be the same string on caller
// if nLin>0, if error a msg is printed indicating the offening nLin line number
t_stat convert_bautocode_to_stdcharset(int nLin, char * sLinIn, char * sLinOut)
{
    char sLin[1024]; 
    int seq_len, n, i, len, found, r; 
    char c,c2; 
    char *pLinIn; 
    extern char * charset_BAutocode[][2];

    r=SCPE_OK; 
    pLinIn=sLinIn; // pointer to read line input
    len=0; // output len
    while (c=*pLinIn++) {
        if (c==';') break; // comment ends autocode source line
        if (c<=32) continue; // ignore spaces/tabs/etc
        // first proceed with 2-chars input sequences (as >= or !=), then with single char input sequences
        c2=*pLinIn;
        found=0; 
        for (seq_len=2;seq_len>0; seq_len--) {
            for(i=0; charset_BAutocode[i][0]; i++) {
                if (strlen(charset_BAutocode[i][0]) != seq_len) continue; 
                if (charset_BAutocode[i][0][0] != c) continue; 
                if ((seq_len==2) && (charset_BAutocode[i][0][1] != c2)) continue; 
                // typed-in input string match 
                found=1;
                if (seq_len > 1) pLinIn++; // if matched a 2-chars sequence, advance again input pointer
                // add the translated char(s) to sLin
                for(n=0; n<10; n++) {
                    c=charset_BAutocode[i][1][n]; 
                    if (c==0) break; 
                    sLin[len++]=c;
                }
                break;
            }
            if (found) break; 
        }
        if (found==0) {
            // char from sLinIn not in autocode charset -> signal error
            sim_printf("\r\n"); 
            if (nLin) sim_printf("line %d: %s\n", nLin, sLinIn); 
            sim_printf("Invalid autocode char '%c' at pos %d\n", *pLinIn, pLinIn-sLinIn+1); 
            r=SCPE_IERR; 
            break; 
        }
        if (len >= 1000) break; 
    }
    if (len) {
        // if some chars translated, add autocode <cr> <lf> 
        sLin[len++]='V';
        sLin[len++]='F';
    }
    sLin[len]=0; 

    // copies the source line as comment and terminates
    // leave 30 first chars for chars translated
    while (len<30) sLin[len++]=32;
    sLin[len++]=';';
    sLin[len++]=' ';
    // copy the autocode source line 
    pLinIn=sLinIn; // pointer to read line input
    while (c=*pLinIn++) {
        if ((c==13) || (c==10)) continue; 
        sLin[len++]=c;
        if (len >= 1000) break; 
    }
    sLin[len]=0; 
    // copies sLin to sLinOut
    strcpy(sLinOut, sLin); 
    return r; 
}


// translate sLinIn to sLinOut (must have at least 1000 chars size)
// uses an internal buffer, so sLinIn and sLinOut can be the same string on caller
// if nLin>0, if error a msg is printed indicating the offening nLin line number
t_stat convert_gautocode_to_stdcharset(int nLin, char * sLinIn, char * sLinOut)
{
    char sLin[1024]; 
    int i, len, found, r; 
    char c; 
    char *pLinIn; 
    char charset_BAutocode[40] = "/E@A:SIU#DRJNFCKTZLWHYPQOBG\"MXV$"; 

    r=SCPE_OK; 
    pLinIn=sLinIn; // pointer to read line input
    len=0; // output len
    while (c=*pLinIn++) {
        if (c==';') break; // comment ends autocode source line
        if ((c >= 'a') && (c <= 'z')) c = c - 'a' + 'A'; // to uppercase
        if ((c=='-') && (*pLinIn == '>')) {
           c = '"'; // convert 2-chars input '->' to '"'
           pLinIn++; 
        }
        if ((c==13) ||(c==10)) break; // end of line
        if (c <= 32) {
           if ((*pLinIn) && (*pLinIn <= 32)) continue; // ignore double spaces
           c = '/'; // convert space to separator
        }
        if (c=='+') c = 'P'; else if (c=='-') c = 'M'; // convert sign to char
        if (c=='.') c = 'F'; // convert decimal point to char
        if ((c>='0') && (c <= '9')) {
           c = charset_BAutocode[c - '0']; // convert digit to regular perforator equivalent
        }
        // check if char belongs to perforator charset
        found = 0; 
        for (i=0; i<32; i++) {
           if  (c==charset_BAutocode[i]) { found=1; break; }
        }
        if (found) {
            sLin[len++]=c;
        } else {
            // char from sLinIn not in autocode charset -> signal error
            sim_printf("\r\n"); 
            if (nLin) sim_printf("line %d: %s\n", nLin, sLinIn); 
            sim_printf("Invalid autocode char '%c' at pos %d\n", *pLinIn, pLinIn-sLinIn+1); 
            r=SCPE_IERR; 
            break; 
        }
        if (len >= 1000) break; 
    }
    if (len) {
        // if some chars translated, add 8 separator chars / to end
        // any START or FRACTIONS command
        for (i=0; i<8; i++) {
           sLin[len++]='/';
        }
    }
    sLin[len]=0; 

    // copies the source line as comment and terminates
    // leave 30 first chars for chars translated
    while (len<30) sLin[len++]=32;
    sLin[len++]=';';
    sLin[len++]=' ';
    // copy the autocode source line 
    pLinIn=sLinIn; // pointer to read line input
    while (c=*pLinIn++) {
        if ((c==13) || (c==10)) continue; 
        sLin[len++]=c;
        if (len >= 1000) break; 
    }
    sLin[len]=0; 
    // copies sLin to sLinOut
    strcpy(sLinOut, sLin); 
    return r; 
}



/* Initialize vm  */
void vm_init(void) {
    static int inited = 0;
    extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];

    if (inited == 1) return;   /* Be sure to only do these things once */        
    inited = 1;

    // init specific commands
    sim_vm_cmd = aux_cmds;                       /* set up the auxiliary command table */

    memset(lptPrintOut, 0, sizeof(lptPrintOut));

    // execute this code to add the current time to seed to make the sequence 
    // of random values returned by /W different in each session run
    {
        struct timespec now; 
        struct tm *tmnow; 
        unsigned int seed; 
        clock_gettime (CLOCK_REALTIME, &now); // we fill the struct with unix-like raw time stamp
        tmnow = localtime(&(now.tv_sec));     // we cook the raw time to get meaningfull values
        seed = (tmnow->tm_yday + tmnow->tm_min + tmnow->tm_sec + sim_os_msec()); 
        seed = seed % RAND_MAX; 
        sim_srand(seed); 
    }
}

// load a prog directly in Storage Tube
// format:
//
//    [Routine NAME ]   <--- [Command  is case sensitive
//
//    [Col /          E ]                    can have optionally a second column (this is routine2)
//    [Col /          E  Track 3L ]          [Col /          E  Track 3R ]  and optionally a track to store to
//                                     
//    | //// | / |      |                    | //// | / |      |  comment
//    | TE/: | T | YAAA |  comment           | //// | E |      |                                           
//    | L/T: | Z | I/// |                    ... 
//    ...
//
//    [End]  <-- is optional. Only needed if later comment contains | char
//
//    comment
//  ; comment
//
// alternate format:
//
//    [Routine NAME ]   <--- [Command  is case sensitive
//
//    [Col /     ]                             can have optionally a second column (this is routine2)
//    [Col /     ]                             [ E  ]       
//    [Col /     ]                             [ E  Track 3L ]  and optionally a track to store to
//                                     
//    | / |      |                             | / |      |  comment
//    | T |   Y  |  comment                    | E |      |                                           
//    | Z | I/// |                             ... 
//    ...
//
//    [End]  <-- is optional. Only needed if later comment contains | char
//
//    comment
//  ; comment
//
int sim_load_routine[64]; // rutine loaded
t_stat sim_load(FILE * fileref, CONST char *cptr, CONST char *fnam, int flag)
{
    int addr, addr1, addr2, nlin, mode, len;
    int d, d1, d2, ad1, ad2; 
    int nCol; // current column
    int nCols; // max num of cols in this line
    struct {
       int addr1, addr2, track_addr; 
    } col[2]; 

    char slin[1024];
    const char * pch;
    int load_ok=1; //=0 if an error has been detected
    int save_drum=0; // =1 if data has been saved to drum
    int save_data=0; // =1 if data is to be save in mem/drum/sim_load_routine array

    #define skip_spaces         {while ((*pch==32) || (*pch==9)) pch++;}
    #define get_value(da,nchrs) {const char * pch2; \
                                 pch2=parse_n(&da, pch, 2, nchrs); \
                                 if (da<0) {sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; \
                                            sim_printf("Invalid teleprinter char '%c' at pos %d\n", *pch2, pch2-slin+1); \
                                            continue; } \
                                 pch=pch2; } 
    #define get_pipe            {skip_spaces; \
                                 if (*pch != '|') { sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; \
                                                    sim_printf("Expected '|' at pos %d\n", pch-slin+1); \
                                                    continue; } \
                                 pch++; skip_spaces; }


    int nRoutine=0;
    int sim_load_routine_at_col=-1; 
    int verbose=1; 
    int sim_debug_lin=0; // generating a debug line  

    if (flag > 10) {
        // save the nRoutine loaded into sim_load_routine
        nRoutine = flag-10; 
        flag=0; // same as regular LOAD scp command
        verbose=0; // but no output
        memset(sim_load_routine, 0, sizeof(sim_load_routine));
    } else {
        if (*cptr != 0) return SCPE_ARG;
        if (flag != 0) return sim_messagef (SCPE_NOFNC, "Command Not Implemented\n");
    }

    if (verbose) sim_printf("Load file %s \n", fnam);

    mode=0; addr=0; nlin=0; nCols=0; 
    col[0].addr1 = col[0].addr2 = -1;
    col[1].addr1 = col[1].addr2 = -1;
    while (fgets (slin, sizeof(slin)-1, fileref)) {
        slin[sizeof(slin)-1]=0; // safety
        nlin++; 
        nCol=0; 
        sim_debug_lin=0; 
        // remove trailing spaces
        while ((len=strlen(slin)) && (slin[len-1] <= 32)) slin[len-1]=0; 
        // skip leading spaces/tabs
        pch=slin; skip_spaces;
        // scan for column mode
        if (strncmp(pch, "[Routine ",9)==0) {
            const char * name; 
            pch += 9; 
            name=pch; 
            // scan for end of routine name
            while (*pch) {
                if (*pch==']') { slin[pch-slin]=0; break; }
                pch++; 
            }            
            if (verbose) sim_printf(". Routine: %s \n", name);
            continue; 
        }
       NextChar:
        if ((*pch==0) || (*pch==';')) continue; // comment -> ignore line
        if (strncmp(pch, "[End]",5)==0) {
            // [End] terminated column read
            mode=0; nCols=0; 
            continue; 
        }
        // [Col  ch1    ch2    ]   or  [Col  ch1     ch2   Track nL|R ]    -> mode 1
        // [Col  ch1 ]   [ ch2 ]   or  [Col  ch1 ] [ ch2   Track nL|R ]    -> mode 2
        if (strncmp(pch, "[Col ",5)==0) {
            if (nCol==0) sim_load_routine_at_col=-1; // init
            nRoutine--; 
            if (nRoutine==0) {
                sim_load_routine_at_col=nCol; //is the col to save
            }
            if (nCol>1) {
                sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                sim_printf("Too many columns specifers at pos %d\n", pch-slin+1); 
                continue; 
            }
            // init track addr to store data
            col[nCol].track_addr=-1; 
            col[nCol].addr1 = col[nCol].addr2 = -1;
            pch += 4; 
            get_value(d1,1); // read one MarkI teleprinter char (ch1 -> the addr of first col)
            if (d1 > 15) {
                sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                sim_printf("Invalid Column at pos %d: max left colum allowed is C \n", pch-slin+1); 
                continue; 
            }
            if ((d1 & 1)==1) {
                sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                sim_printf("Invalid Column number at pos %d: first column must be even (/ @ .. )\n", pch-slin+1); 
                continue; 
            }
            col[nCol].addr1 = d1 * 32; // addr of first col
            mode=1; 
            if (*pch==']') {
                mode=2; 
                while (*pch) {
                    if ((*pch=='[') || (*pch==';')) break;
                    pch++; 
                }
                if (*pch!='[') { // there is not a second column 
                    sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                    sim_printf("Missing second column specifier\n", pch-slin+1); 
                    continue; 
                }
                pch++;
            }
            get_value(d2,1); // read one MarkI teleprinter char (the addr of second col)
            if (d1+1 != d2) {
                sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                sim_printf("Invalid Column number at pos %d: first and second columns must be in sequence\n", pch-slin+1); 
                continue; 
            }
            col[nCol].addr2 = d2 * 32; // addr of second col
            if (verbose) sim_printf(". . Load into Storage Tube S%d (Col %c %c)", 
                    col[nCol].addr1/64, 
                    charset_MarkI[(col[nCol].addr1/32) & 31][1], charset_MarkI[(col[nCol].addr2/32) & 31][1]);
            if (strncmp(pch, "Track ",6)==0) {
                pch += 5; 
                pch=parse_n(&d, pch, 0, 0); 
                if (d >= DRUM_TRACKS) {
                    sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                    sim_printf("Track number %d > MAX_TRACKS (%d) at pos %d\n", d, DRUM_TRACKS, pch-slin+1); 
                    continue; 
                }
                if (*pch=='L') { col[nCol].track_addr=d * 128; } else 
                if (*pch=='R') { col[nCol].track_addr=d * 128 + 64; } else {
                    sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                    sim_printf("Invalid Track number at pos %d\n", pch-slin+1); 
                    continue; 
                }
                if (verbose) sim_printf(", into Track %d%c ", 
                    col[nCol].track_addr/128, (col[nCol].track_addr & 64) ? 'R':'L');
            }
            nCol++; nCols=nCol; 
            // scan slin to see if there is a second column specifier
            while (*pch) {
                if ((*pch=='[') || (*pch==';')) break;
                pch++; 
            }
            if (*pch=='[') { // there is a second column 
               if (verbose) sim_printf("\n");
               goto NextChar; 
            }
            if (verbose) sim_printf("\n");
            continue; 
        } 
        // parse:   | data | addr | data | symbolic info -> mode 1
        // parse:   | addr | data | symbolic info | addr | data | symbolic info  -> mode 2
        if (mode==1) {
            while ((*pch) && (*pch!='|')) pch++; // skip until first |
            if (*pch == 0) continue; // line without first pipe
            get_pipe; 
            if (*pch=='|') d1=0; else get_value(d1, 4); 
            get_pipe;  
            get_value(d, 1); ad1=ad2=d;
            get_pipe; 
            if (*pch=='|') d2=0; else get_value(d2, 4); 
            get_pipe; 
            goto SaveData; 
        } else if (mode==2) {
            while ((*pch) && (*pch!='|')) pch++; // skip until first |
            if (*pch == 0) continue; // line without first pipe
            get_pipe; 
            get_value(ad1, 1); // addr first col 
            get_pipe; 
            if (*pch=='|') d1=0; else get_value(d1, 4); // data
            get_pipe; 
            // skip text between cols
            ad2=-1; 
            while ((*pch) && (*pch!='|')) pch++; // until | of second col if any
            if (*pch=='|') {
                get_pipe; 
                get_value(ad2, 1); // addr second col
                get_pipe; 
                if (*pch=='|') d2=0; else get_value(d2, 4); // data
                get_pipe; 
            }
          SaveData:
            save_data=1; 
            ad1 &=31; if (ad2>=0) ad2 &= 31; //safety
            if (sim_load_routine_at_col==nCol) {
                // ... in sim_load_routine_loaded
                sim_load_routine[ad1] = d1; 
                if (ad2>=0) sim_load_routine[32 + ad2] = d2; 
            } else {
                // ... in Storage Tube
                if (col[nCol].addr1 < 0) {
                    sim_printf("\r\nline %d: %s\n", nlin, slin); load_ok=0; 
                    sim_printf("Column address not defined with [Col ... ]\n"); 
                    continue; 
                }
                addr1 = col[nCol].addr1 + ad1; CRT[addr1]=d1;  
                if ((ad2<0) || (col[nCol].addr2 < 0)) {
                    addr2=-1; 
                } else {
                    addr2 = col[nCol].addr2 + ad2; CRT[addr2]=d2;  
                }
                sim_debug(DEBUG_DETAIL, &cpu_dev, " %3d MEM[%d]=%7d | %3d MEM[%d]=%7d | [%c%c]=%c%c%c%c | [%c%c]=%c%c%c%c ", 
                    ad1, addr1, d1, ad2, addr2, d2, 
                    charset_MarkI[addr1 & 31][1], charset_MarkI[(addr1 >> 5) & 31][1], 
                    charset_MarkI[d1 & 31][1], charset_MarkI[(d1 >> 5) & 31][1], charset_MarkI[(d1 >> 10) & 31][1], charset_MarkI[(d1 >> 15) & 31][1],
                    charset_MarkI[addr2 & 31][1], charset_MarkI[(addr2 >> 5) & 31][1], 
                    charset_MarkI[d2 & 31][1], charset_MarkI[(d2 >> 5) & 31][1], charset_MarkI[(d2 >> 10) & 31][1], charset_MarkI[(d2 >> 15) & 31][1]
                );
                sim_debug_lin=1; 
                // store data in drum track
                if (col[nCol].track_addr >= 0) {
                    addr1=col[nCol].track_addr + ad1; DRUM[addr1]=d1; 
                    if (ad2 < 0) {
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "DRUM[%d]=%7d ", 
                            addr1, d1);
                    } else {
                        addr2=col[nCol].track_addr + 32 + ad2; DRUM[addr2]=d2; 
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "DRUM[%d]=%7d DRUM[%d]=%7d ", 
                            addr1, d1, addr2, d2);
                    }
                    save_drum=1; 
                    sim_debug_lin=1; 
                }
                if (ad1==31) col[nCol].addr1=-1; // if stored last line, invalidate 
                if (ad2==31) col[nCol].addr2=-1; 
            }
            // scan for end of line
            len=0; 
            while (*pch) {
                if (*pch=='|') break;
                pch++; len++; 
            }            
            // prepare for next column of data
            nCol++; 
            if (nCol < nCols) goto NextChar; 
            if (sim_debug_lin) sim_debug(DEBUG_DETAIL, &cpu_dev, "\n");
        }
    }
    if (save_data==0) {
        sim_printf("\r\nNothing loaded. No Routine found into %s \n", fnam); load_ok=0; 
    }
    if (load_ok==0) return SCPE_IERR | SCPE_NOMESSAGE;
    if (save_drum) drum_write_track(-1); // save full drum
    return SCPE_OK;
}


/* Opcodes */
t_opcode  base_ops[63] = {            
    // opcode           name                        beats (=240 usec)     
    {OP_MAG_INSTR_H,    "H as magnetic instr",     10},
    {OP_MAG_INSTR_S,    "S as magnetic instr",      4},
    {OP_LD_DRUM,        "Load line 65th",           4},

    {OP_RND,            "Randon number",           24},
    {OP_MSB_POS,        "Standarise",               5},
    {OP_POP_COUNT,      "Sideways adder",           5},
    {OP_NOP,            "Dummy",                    4},

    {OP_STOP_L,         "Stop /L",                  4},
    {OP_STOP_G,         "Stop /G",                  4},
    {OP_HOOT,           "Hoot",                     4},

    {OP_TR_IF_B,        "B-cond direct transfer",   4},
    {OP_TR_IF_A,        "A-cond direct transfer",   4},
    {OP_STORE_Z,        "S=0",                      4},
    {OP_TR,             "Uncond direct transfer",   4}, 
    {OP_TR_REL,         "Uncond rel transfer",      4}, 
    {OP_TR_REL_IF_B,    "B-cond rel transfer",      4}, 
    {OP_TR_REL_IF_A,    "A-cond rel transfer",      4},

    {OP_STORE_H,        "S=H",                      4},
    {OP_STORE_B,        "S=B",                      4},
    {OP_STORE_B_NF,     "S=B (no B addition)",      4},
    {OP_STORE_AM,       "S=AM",                     5},
    {OP_STORE_AM_CLAM,  "S=AM, AM=0",               4},
    {OP_STORE_AL,       "S=AL",                     4},
    {OP_STORE_AL_CLA,   "S=AL, A=0",                4},

    {OP_LOAD_B,         "B=S",                      4},
    {OP_LOAD_B_NF,      "B=S (no B addition)",      4},
    {OP_U_LOAD_D,       "D=S unsigned",             4},
    {OP_S_LOAD_D,       "D=S signed",               4},
    {OP_U_LOAD_A,       "A=S",                      4},
    {OP_S_LOAD_A,       "A=S signed",               4},
    {OP_S_LOAD_MA,      "A=-S signed",              4},

    {OP_MOVE_CLAM,      "S=AL, AL=AM, AM=0",        4},
    {OP_SWAP_AM_AL,     "Swap AL <-> AM",           4},
    {OP_CLA,            "A=0",                      4},

    {OP_ADD_AM,         "AM=AM+S",                  5},
    {OP_U_MPY_ADD,      "A=A+D*S unsigned",         9},
    {OP_S_MPY_ADD,      "A=A+D*S signed",           9},
    {OP_U_ADD_A,        "A=A+S unsigned",           5},
    {OP_S_SUB_A,        "A=A-S signed",             5},
    {OP_S_ADD_A,        "A=A+S signed",             5},
    {OP_S_LOAD_2A,      "A=2*S signed",             5},

    {OP_SUB_B,          "B=B-S",                    5},
    {OP_SUB_B_same,     "B=B-S",                    5},
    {OP_SUB_B_NF,       "B=B-S (no B addition)",    5},
    {OP_SUB_B_NF_same,  "B=B-S (no B addition)",    5},

    {OP_OR_A,           "A=S OR A",                 5},
    {OP_AND_A,          "A=S AND A",                5},
    {OP_XOR_A,          "A=S NEQ A",                5},

    {OP_U_MPY_SUB,      "A=A-D*S unsigned",         9},
    {OP_S_MPY_SUB,      "A=A-D*S signed",           9},
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
    int i,n,opcode; 
    const char * opname;
    int bOut=0; 
    int Bindex; 
    char sBindex[10] = "B0 ";

    // ex -B nnnn    to display       10000 11111 01000 11000                              backwards binary (20 bits)
    // ex -T nnnn    to display       XXXX                                                 X=MarkI teleprinter code (20 bits)
    // ex -M nnnn    to display       AA XXXX (addr=nnnn Bn func=XX) cccccccccccc          ccccc=opcode equation (20 bits), AA the address 
    // ex -S nnnn                     mmmm                                                 decimal value, signed (20 bits)
    // ex nnnn                        mmmm                                                 decimal value, unsigned (20 bits)

    if (sw & SWMASK('B')) {
        for (i=0;i<4;i++) {
            for (n=0;n<5;n++) {
                fprintf(of, "%c", (d & 1) ? '1':'0');
                d=d>>1; 
            }
            fprintf(of, " ");
        }
        fprintf(of, "  ");
        d = *val;
        bOut=1; 
    }
    if (sw & SWMASK('M')) {
        fprintf(of, "%c%c  ",
            charset_MarkI[addr & 31][1], charset_MarkI[(addr >> 5) & 31][1]);
        bOut=1; 
    }
    if ((sw & SWMASK('T')) || (sw & SWMASK('M'))) {
        fprintf(of, "%c%c%c%c ",
            charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d >> 10) & 31][1], charset_MarkI[(d >> 15) & 31][1]);
        bOut=1; 
    }
    if (sw & SWMASK('S')) {
        if (d >> 19) d=(int) (d | MASK_20UPPERBITS); // sign extend d
        fprintf(of, "%d ", (int) d);
        d = *val;
        bOut=1; 
    }
    if (sw & SWMASK('M')) {
        opcode=(int)((d >> 14) & 63); 
        Bindex=(int)((d >> 10) & 7); 
        if (Bindex) { sBindex[0]='B'; sBindex[1]='0'+Bindex; } else { sBindex[0]=0; }
        fprintf(of, "(addr=%04d %sfunc=%c%c) ", 
                (int)(d & MASK_10BITS), sBindex, 
                charset_MarkI[(opcode & 1)<<4][1], charset_MarkI[opcode >> 1][1]);
        opname=NULL; 
        for (i=0; base_ops[i].name; i++) {
            if (base_ops[i].opbase == opcode) {
                opname = base_ops[i].name;
                break;
            }
        }
        fprintf(of, "%s",  (opname == NULL) ? "???":opname);
        bOut=1; 
    }
    if (bOut==0) {
        // no switch selected
        fprintf(of, "%d ", (int) d);
    }

    return SCPE_OK;
}

/* read char input 

Inputs:
*cptr   =       pointer to input string
bFer    =       =0 -> read decimal digits 0..9, 
                =1 -> read teleprinter digits /..$, 
                =2 -> same, return -1 if invalid char
nchars  =       number of chars to read 
Outputs:
d       =       parsed value
return cptr     
*/

CONST char * parse_n(int *d, CONST char *cptr, int bFer, int nchars)
{
    int i, n, mult, neg;
    char c; 

    *d = 0;
    while (isspace(*cptr)) cptr++; // skip blanks
    if (bFer==0) {
        // decimal number, may beggin by - or +
        neg=0; 
        if (*cptr == '-') { neg=1; cptr++; } else 
        if (*cptr == '+') { cptr++; }
        while (1) {
            c = *cptr; 
            if ((c<'0') || (c>'9')) break;
            *d = (*d) * 10 + c - '0'; 
            cptr++;
        }
        if (neg) *d=-(*d); 
    } else {
        // MarkI teleprinter chars
        mult=1; 
        while (nchars) {
            c = *cptr; 
            n=-1;
            for (i=0;charset_MarkI[i][0]>=0; i++) {
                if (charset_MarkI[i][1]!=c) continue; 
                n=charset_MarkI[i][0]; break; 
            }
            if (n<0) {
                if (bFer==2) {
                    *d=-1; // signal invalid char found
                    return cptr;
                }
                break; 
            }
            *d += n * mult; 
            mult *= 32; 
            nchars--;
            cptr++;
        }
    }
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
    int                 d;
    int                 mult;
    char                c;

    // dep -B addr   10000 11111 01000 11000                              backwards binary (20 bits)
    // dep -T addr   XXXX                                                 X=MarkI teleprinter code (20 bits)
    // dep addr      [-|+]nnnnn                                           decimal value (20 bits), signed

    while (isspace(*cptr)) cptr++;
    if (*cptr == 0) return sim_messagef (SCPE_ARG, "Missing value\n");

    d = 0;
    if (sw & SWMASK('B')) {
        mult=1; 
        while (c=*cptr) {
            if (c==0) {
                break;
            } else if (c==';') {
                break;
            } else if (c==' ') {
                cptr++; 
                continue; 
            } else if (c=='0') {
                // zero
            } else if (c=='1') {
                d=d | mult; 
            }
            mult *= 2; 
            cptr++; 
        } 
    } else if (sw & SWMASK('T')) {
        cptr=parse_n(&d, cptr, 1, 4); // use MarkI teleprinter chars
        c=*cptr; 
        if ((c) && (c!=32) && (c!=';')) return sim_messagef (SCPE_ARG, "Invalid char\n");
    } else {
        cptr=parse_n(&d, cptr, 0, 0); // signed decimal 20 bits
        c=*cptr; 
        if ((c) && (c!=32) && (c!=';')) return sim_messagef (SCPE_ARG, "Invalid char\n");
    }
    * val = d; 
    return SCPE_OK;
}


/* Helper functions */

// simulate typing on perforator a line or a whole routine
// Perforator setup is done with 
//     ATT PERFORATOR punched_file.txt  -> the file that simulates paper tape where routine is to be punched
//     SET PERFORATOR ECHO|NOECHO       -> see what is being typed on simh console
//     SET PERFORATOR USE_WC_K|USE_WC_J -> punches on J convention or K convention 
// TYPEIN [-Q] LINE  [|]  K AK KK ...   [; comment]  the char | is interpreted as space. Used to signal the beggining of non skippable spaces
// TYPEIN [-Q] ROUTINE[1..8] file.txt [S4|S5|...]    defaults load for S4. This param only applies when using K warning char
// TYPEIN [-Q] BAUTOCODE LINE  autocode source line or input data  [; comment]  
// TYPEIN [-Q] BAUTOCODE SOURCE file.txt             file.txt expected to use brooker's autocode charset
// TYPEIN [-Q] GAUTOCODE LINE  autocode source line or input data  [; comment]  
// TYPEIN [-Q] GAUTOCODE SOURCE file.txt             file.txt expected to use glennie's autocode charset
//                                                   -q forces no echo only for this command
t_stat perforator_typein_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    char fnam[4*CBUFSIZE];
    char sLin[1000];
    FILE *fpin = NULL;
    int n,n1,n2,d,d1,d2, nStore; 
    int Mode, bEchoInput, bQuietMode; 
    extern UNIT perforator_unit; 
    t_stat r; 

    Mode=0; // 0=standard teletype code, 1=brookers autocode teletype
    cptr=get_glyph (cptr, gbuf, 0);                   // get next param
    bQuietMode=0;
    if (strcmp(gbuf, "-Q") == 0) {
        bQuietMode=1; 
        cptr=get_glyph (cptr, gbuf, 0);                   // get next param
    }
    if (strcmp(gbuf, "BAUTOCODE") == 0) {
        // simulate typing a brooker's autocode prog in autocode-prepared perforator
        Mode=1; 
        cptr=get_glyph (cptr, gbuf, 0);                   // get next param
    } else if (strcmp(gbuf, "GAUTOCODE") == 0) {
        // simulate typing a glennie's autocode prog in regular perforator, using the language conventions
        Mode=2; 
        cptr=get_glyph (cptr, gbuf, 0);                   // get next param
    }
    if (strcmp(gbuf, "LINE") == 0) {
        // type the text line into output punch file and in console
        {while ((*cptr==32) || (*cptr==9)) cptr++;}
        // interpret leading | as a space that marks the beggining of non skippable spaces
        if (*cptr) {
            if (Mode==0) { 
                perforator_typein((char *) cptr, 0, bQuietMode); 
            } else if ((Mode==1) ||(Mode==2)) {
                bEchoInput=1; 
                if (Mode==1) r=convert_bautocode_to_stdcharset(0, (char *)cptr, sLin); else
                if (Mode==2) r=convert_gautocode_to_stdcharset(0, (char *)cptr, sLin); 
                if (r) return r; 
                // perforator_typein will echo to console (if enabled) the input line with
                // autocode source code as comment
                perforator_typein(sLin, 1, bQuietMode); 
            }
        }
        return SCPE_OK; 
    } else if (strncmp(gbuf, "SOURCE", 6) == 0) {
        // typing brooker autocode source or data
        if (Mode == 0) {
            sim_messagef (SCPE_ARG, "SOURCE action needs BAUTOCODE or GAUTOCODE modifier\n");
        }
        cptr = get_glyph_quoted (cptr, fnam, 0); 
        if (fnam[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename\n");
        fpin = sim_fopen (fnam, "r"); 
        if (fpin == NULL) return sim_messagef (SCPE_IOERR, "Cannot open %s \n", fnam);
        r=0; n=0;
        perforator_start_tm0=sim_os_msec(); // signal start using autocode perforator
        // load it 
        while (fgets (sLin, sizeof(sLin)-1, fpin)) {
            sLin[sizeof(sLin)-1]=0; // safety
            n++; 
            if (Mode==1) r=convert_bautocode_to_stdcharset(0, sLin, sLin); else
            if (Mode==2) r=convert_gautocode_to_stdcharset(0, sLin, sLin); 
            if (r) break; 
            perforator_typein((char *) sLin, 1, bQuietMode); 
        }
        fclose(fpin); 
        if (r) return r; 
    } else if (strncmp(gbuf, "ROUTINE", 7) == 0) {
        // typing standard routine coding sheets
        // get routine number, if present (default=1)
        if (Mode != 0) {
            sim_messagef (SCPE_ARG, "ROUTINE action cannot be used with autocode input, Use SOURCE action to type-in an autocode prog\n");
        }
        n=gbuf[7]-'0'; if ((n<1) || (n>8)) n=1; 
        while (sim_isspace (*cptr)) cptr++;                     // trim leading spc 
        // get next param: routine source filename to type in
        cptr = get_glyph_quoted (cptr, fnam, 0); 
        if (fnam[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename\n");
        nStore=4; 
        cptr=get_glyph (cptr, gbuf, 0);                   // get next param
        if ((gbuf[0]=='s') || (gbuf[0]=='S')) {
            nStore=(gbuf[1] - '0'); 
            if ((n<0) || (n>=8)) return sim_messagef (SCPE_ARG, "Invalid tube to store '%s'\n", gbuf);
        }
        fpin = sim_fopen (fnam, "r"); 
        if (fpin == NULL) return sim_messagef (SCPE_IOERR, "Cannot open %s \n", fnam);
        // load it 
        perforator_start_tm0=sim_os_msec(); // signal start using normal perforator
        r=sim_load(fpin, NULL, fnam, 10+n); 
        fclose(fpin); 
        if (r) return r; 
        // simulate the operator typing the routine body in perforator to 
        // punch a papertape using warning characters J and K. This is
        // called in original doc the "proper punch"
        if (perforator_unit.flags & OPTION_USE_WC_J) {
            // use warning char J
            if (nStore != 4) {
                return sim_messagef (SCPE_ARG, "Cannot use warning char J to load in other place than S4 \n");
            }
            for (n=0; n<32; n++) {
                d1=sim_load_routine[n];
                d2=sim_load_routine[n+32];
                sprintf(sLin, "J %c%c%c%c %c %c%c%c%c J", 
                    charset_MarkI[d1 & 31][1], charset_MarkI[(d1 >> 5) & 31][1], charset_MarkI[(d1>>10) & 31][1], charset_MarkI[(d1 >> 15) & 31][1], 
                    charset_MarkI[n & 31][1],
                    charset_MarkI[d2 & 31][1], charset_MarkI[(d2 >> 5) & 31][1], charset_MarkI[(d2>>10) & 31][1], charset_MarkI[(d2 >> 15) & 31][1]);
                perforator_typein(sLin, 0, bQuietMode); 
            }
        } else {
            // use warning char K
            n=nStore*64; 
            for (n1=0; n1<8; n1++) {
                sprintf(sLin, "K %c%c # ", 
                    charset_MarkI[n & 31][1], charset_MarkI[(n >> 5) & 31][1]);
                for (n2=0; n2<8; n2++) {
                    d=sim_load_routine[n1*8+n2]; n++;
                    sprintf(&sLin[strlen(sLin)], "%c%c%c%c ", 
                        charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d>>10) & 31][1], charset_MarkI[(d >> 15) & 31][1]);
                }
                perforator_typein(sLin, 0, bQuietMode); 
            }
        }
    } else return sim_messagef (SCPE_ARG, "Unknown TYPEIN action %s. Must be LINE, ROUTINE, ROUTINEn or SOURCE\n", gbuf);
    return SCPE_OK;
}

// inyect chars to PaperTape reader stream
// FEEDPTR xxx x x x x xxxx
// FEEDPTR -N  <-- clear feed buffer
t_stat feed_ptr_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    int n; 
    char ch; 

    get_glyph (cptr, gbuf, 0);                   // get next param
    if (strcmp(gbuf, "-N") == 0) {
        ptr_buf[0]=0; 
        return SCPE_OK; 
    }

    n=strnlen(ptr_buf, MAX_PTR_BUF);
    while (1) {
        if (n >= MAX_PTR_BUF-1) {
            return sim_messagef (SCPE_ARG, "Too much data fed to PTR\n");
        }
        ch = *cptr++; 
        if (ch==0) break; // end of data to feed
        if (ch==32) continue; // ignore spaces
        if (ch==9) continue; // ignore tabs
        if (ch==';') break; // coment marks end of data to feed

        ptr_buf[n++]=ch; ptr_buf[n]=0; 
    }
    return SCPE_OK; 
}

// APPENDTOTAPE fname_of_tape_to_append_to.txt  fname.txt
t_stat appendtotape_cmd(int32 flag, CONST char *cptr)
{
    char fnIn[4*CBUFSIZE];
    char fnOut[4*CBUFSIZE];
    FILE *fpIn = NULL;
    FILE *fpOut = NULL;
    int n,c; 

   // get next param: punched tape to append to
   cptr = get_glyph_quoted (cptr, fnOut, 0); 
   if (fnOut[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename of tape to append to \n");
   cptr = get_glyph_quoted (cptr, fnIn, 0); 
   if (fnIn[0] == 0) return sim_messagef (SCPE_ARG, "Missing filename of tape to be added\n");
   fpOut = sim_fopen (fnOut, "a"); 
   if (fpOut == NULL) return sim_messagef (SCPE_IOERR, "Cannot open %s \n", fnOut);
   fpIn = sim_fopen (fnIn, "r"); 
   if (fpIn == NULL) {
       fclose(fpOut); 
       return sim_messagef (SCPE_IOERR, "Cannot open %s \n", fnIn);
   }
   // read fpIn, char by char. 
   // Discard chars < 33, format un 70cols block
   
   n=0; 
   while (feof(fpIn)==0) {
      c=getc(fpIn); 
      if (c < 33) continue; 
      putc(c, fpOut);
      n++;
      if (n>=LPT_COLUMNS-1) {
         putc(13, fpOut); putc(10, fpOut);
         n=0;
      }
   }
   if (n) {
      putc(13, fpOut); putc(10, fpOut);
   }
   fclose(fpIn);
   fclose(fpOut);
   return SCPE_OK; 
}  


// intercept    deposit IC or IR
// DEP C  nnn     -> will reset to scan beat, so next GO will fetch the instr from mem (from addr C) to PI reg and then execute it 
// DEP PI  nnn    -> will reset to execution beat, so next GO will execute instr in PI reg
// DEP H tttt     -> deposit to H allows to deposit nnn decimal value OR tttt four teletype-char value
t_stat MarkI_exdep_cmd (int32 flag, CONST char *cptr)
{
    char gbuf[CBUFSIZE];
    extern FILE    *sim_ofile ;
    extern int     NCYCLE;                   
    const char * cptr2; 
    int d; 

    cptr2 = get_glyph (cptr, gbuf, 0);                       // get param    
    while (gbuf[0]=='-') {
        cptr2 = get_glyph (cptr2, gbuf, 0);                  // skip any option -Switch
    }


    if (flag==EX_D) {
        if ((strlen(gbuf) == 1) && (strncmp(gbuf, "C", 1)==0)) {
            // deposit C instruction counter reset instr cycle
            NCYCLE = 0; 
        } else if ((strlen(gbuf) == 2) && (strncmp(gbuf, "B", 1)==0) && 
                   ((gbuf[1]>='0') && (gbuf[1]<='7')) ) {
            // deposit B index also reset instr cycle
            NCYCLE = 0; 
        } else if ((strlen(gbuf) == 2) && (strncmp(gbuf, "PI", 2)==0)) {
            // deposit PI instruction register
            // set in intr cycle to skip fetch and go directly to decode and execute
            NCYCLE = 1;
        } else if ((strlen(gbuf) == 1) && (strncmp(gbuf, "H", 1)==0)) {
            // deposit H , check if 4 teletype-chars follows
            cptr2 = get_glyph (cptr2, gbuf, 0);                       // get param    
            parse_n(&d, gbuf, 2, 4); // get MarkI 4-teleprinter chars, return -1 if not teleprinter chars
            if (d>=0) {
                // parsed a valid 4-teleprinter chars argument for DEP H
                extern int H;    // value Hand Switches (20 bits)
                H=d;             // set the hand switches
                return SCPE_OK;  // and return
            }
        }
    }
    return exdep_cmd (flag, cptr);
}


