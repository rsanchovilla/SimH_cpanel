/* MarkI_cpu.c: Ferranti Mark I computer CPU simulator

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

   cpu          Ferranti Mark I 

   From Wikipedia: 

   The Ferranti Mark 1, also known as the Manchester Electronic Computer in its 
   sales literature, and thus sometimes called the Manchester Ferranti, was 
   produced by British electrical engineering firm Ferranti Ltd. It was the world's 
   first commercially available general-purpose digital computer. It was "the 
   tidied up and commercialised version of the Manchester Mark I". The first 
   machine was delivered to the Victoria University of Manchester in 
   February 1951 (publicly demonstrated in July) ahead of the UNIVAC I, which 
   was sold to the United States Census Bureau on 31 March 1951, although not 
   delivered until late December the following year

   speed 

         year  machine cycle      simple int and logic       mult              other instr      approx instr 
               (microsec)  clock    (cycles) microsec   (cycles) microsec   (cycles) microsec   per second (no I/O)
 Mark I  1950    10       100 KHz    24x5      1200      24x9     2160       24x4     960        700 IPS



*/

#include <math.h>
#include "MarkI_defs.h"
#if defined(CPANEL)
#include "cpanel.h"
int cpanel_gui_supported = 1;     
#else
int cpanel_gui_supported = 0;     // so disp cpanelgui returns 0 on scp
#endif

t_stat              cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_reset(DEVICE * dptr);
t_stat              cpu_kec_clear_everthing(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_kac_clear_acc(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_active_stop_switches(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_display_tubes(UNIT *uptr, int32 value, CONST char *cptr, void *desc); 
t_stat              cpu_set_speed(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_show_speed(FILE *st, UNIT *uptr, int32 value, CONST void *desc);
t_stat              cpu_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr);
const char          *cpu_description (DEVICE *dptr);


// Memory
int                 CRT[8*64]                        = {0}; // Electrostaic Tube main Memory: 8 tubes x 64 words/tube (each of 20 bits)
int                 DRUM[DRUM_TRACKS * 128]          = {0}; // Drum: 256 tracks x 2 pages/track x 64 word/page (each of 20 bits)
int                 CRT_line65[8]                    = {0}; // the 65th line of CRT (Marker Line)

// increment number of beats counts elapsed from starting of simulator -> this is the global time measurement
// Mark I: 1 Beat = 240 usec (MicroSec)

t_int64 GlobalBeatsCount=1;

// struct for SET CPU SPEED
int CpuSpeed_Acceleration = 0;   // parameter: cpu speed multiplier: 
                                 // 100->original harware speed, 50->half speed, 200->speed x2, 0=max speed
struct {
    // speed control variables
    uint32 msec;                  // constant: to calculate BeatsMax (= number of Beats that can be executed on given msec)
    int BeatsMax;                 // calculated: number of Beats that can be executed during msec at current speed setting
    int BeatsCount;               // count of Beats already executed 
    int BeatsObjectivePerSec;     // calculated: value to achieve
    // speed measurement for scp/debug
    uint32 start_tm0;             // sim_os_msec() when run starts
    uint32 end_tm0;               // sim_os_msec() when run ends
    t_int64 InstrCount;           // full intructions executed count (=0 on start of run)
    t_int64 GlobalBeatsCount0;    // count of beats already executed when run starts
    // measurement for speed control
    struct {
        t_int64 Beats0;           // num Beats already executed when starting speed measurement
        uint32 tm0;               // sim_os_msec() when starting speed measurement
        int BeatsPerSec;          // speed measurement result: mean num of Beats executed per second
    } measurement;
} CpuSpeed = {15};                // adjust cpu speed each 15 msec


// cpu registers
int                 C;                           // Instruction Number register. 10 bits (also know as register C). Holds 20 bits, but only lower 10 are used as Prog Counter
int                 PI;                          // Present Instruction register. 20 bits (also known as Actual Instruction)
t_int64             AL;                          // Accumulator Least Significat bits: 40 bits
t_int64             AM;                          // Accumulator Most Significat bits: 40 bits
t_int64             D;                           // Multiplicand register: 40 bits
int                 Dsigned;                     // =1 -> D register is signed value, =0 -> D register is unsigned
int                 S;                           // Memory select register: 20 bits
int                 B[8];                        // 8 Index register: 20 bits   
int                 Qneg;                        // =1 -> B has become negative
int                 H;                           // value Hand Switches (20 bits)
int                 NCYCLE;                      // indicates current machine cycle to be executed. 
int32               rand_seed;                   // rand seed for OPTION_REPEATABLE_RANDOM

uint8               StopReason      = 0;         // saved stop reason to be displayed in control panel

// console ligths and switches that are checked during cpu instruction execution
int Console_Sw_DummyStop      = 0;               // console G (bit 0) and L (bit 1) dummy stop active
int Console_Sw_ManualMode     = 0;               // =1 -> manual mode active
int Console_Sw_Instruction    = 0;               // instruction set in console switches
int Console_Sw_PrepulseSpeed  = 0;               // =0 -> full speed, =1 -> execute at a 50 instr/sec rate, =2 -> single prepulse
int Console_Sw_KeyClear       = 0;               // bit0=KAC switch on, bit1=KBC, bit2=KCC, bit3=KDC, bit7=KEC, Bit6="update KAC switch", bits 8.. = countdown for set cpu KAC
int Console_Sw_ManKbdPtrToLpt = 0;               // =0 -> Printer input from computer, =1 -> from Ptr, =2 -> from its own keyboard
/* CPU data structures

   cpu_dev      CPU device descriptor
   cpu_unit     CPU unit descriptor
   cpu_reg      CPU register list
   cpu_mod      CPU modifiers list
*/

UNIT cpu_unit =
    { UDATA(NULL, 0, 0), 10  };

 
REG cpu_reg[] = {
    {DRDATAD(C, C, 10, "Control"), REG_FIT}, // Control register is the program counter
    {DRDATAD(PI, PI, 20, "Program Instruction Register"), REG_FIT},
    {DRDATAD(AL, AL, 40, "Accumulator LSB"), REG_FIT},
    {DRDATAD(AM, AM, 40, "Accumulator MSB"), REG_FIT},
    {DRDATAD(D, D, 40, "Multiplicand Register"), REG_FIT},
    {DRDATAD(B0, B[0], 20, "Index Register 0"), REG_FIT},
    {DRDATAD(B1, B[1], 20, "Index Register 1"), REG_FIT},
    {DRDATAD(B2, B[2], 20, "Index Register 2"), REG_FIT},
    {DRDATAD(B3, B[3], 20, "Index Register 3"), REG_FIT},
    {DRDATAD(B4, B[4], 20, "Index Register 4"), REG_FIT},
    {DRDATAD(B5, B[5], 20, "Index Register 5"), REG_FIT},
    {DRDATAD(B6, B[6], 20, "Index Register 6"), REG_FIT},
    {DRDATAD(B7, B[7], 20, "Index Register 7"), REG_FIT},
    {DRDATAD(H, H, 20, "Hand Switches"), REG_FIT},
    {ORDATAD(CPANELGUI, cpanel_gui_supported, 1, "Control Panel GUI supported flag"), REG_FIT|REG_RO},
    {NULL} 
};

MTAB cpu_mod[] = {
    {OPTION_FAST,                0,                         "Real Time execution",               "REALTIME",     NULL},
    {OPTION_FAST,                OPTION_FAST,               "Fast Execution",                    "FAST",         NULL},
    {OPTION_REPEATABLE_RANDOM,   0,                         "Use true Random",                   "TRUE_RANDOM",  NULL},
    {OPTION_REPEATABLE_RANDOM,   OPTION_REPEATABLE_RANDOM,  "Repeatable deterministic Random",   "REPEATABLE_RANDOM",    NULL},
    {OPTION_BRK1_HOOT,           0,                         "No break on Hoot",                  "NOBRKHOOT",    NULL},
    {OPTION_BRK1_HOOT,           OPTION_BRK1_HOOT,          "Break on first Hoot",               "BRKHOOT",      NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                         "Key Everything Clear",              "KEC",          &cpu_kec_clear_everthing, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                         "Key Accumulator Clear",             "KAC",          &cpu_kac_clear_acc, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                         "Stop Switch Active",                "STOP",         &cpu_active_stop_switches, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                         "Storage to be displayed in cpanel", "DISPLAY",      &cpu_display_tubes, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                         "Speed",                             "SPEED",        &cpu_set_speed, &cpu_show_speed},
    {0}
};

DEVICE cpu_dev = {
    "CPU", &cpu_unit, cpu_reg, cpu_mod,
    1, 10, 16, 1, 10, 64,
    &cpu_ex, &cpu_dep, &cpu_reset, NULL, NULL, NULL,
    NULL, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cpu_help, NULL, NULL, &cpu_description
};

void sim_debug_acc(char * smsg)
{
    if ((smsg) && (smsg[0])) {
        sim_debug(DEBUG_DATA, &cpu_dev, "%s \n", smsg);
    }
    sim_debug(DEBUG_DATA, &cpu_dev,
        "... AL: %d%04d (%c%c%c%c %c%c%c%c) "
            "AM: %d%04d (%c%c%c%c %c%c%c%c) \n",
        (int)(AL / 10000), (int)(AL % 10000), 
        charset_MarkI[AL & 31][1], charset_MarkI[(AL >> 5) & 31][1], charset_MarkI[(AL>>10) & 31][1], charset_MarkI[(AL >> 15) & 31][1], 
        charset_MarkI[(AL >> 20) & 31][1], charset_MarkI[(AL >> 25) & 31][1], charset_MarkI[(AL >> 30) & 31][1], charset_MarkI[(AL >> 35) & 31][1], 

        (int)(AM / 10000), (int)(AM % 10000), 
        charset_MarkI[AM & 31][1], charset_MarkI[(AM >> 5) & 31][1], charset_MarkI[(AM>>10) & 31][1], charset_MarkI[(AM >> 15) & 31][1], 
        charset_MarkI[(AM >> 20) & 31][1], charset_MarkI[(AM >> 25) & 31][1], charset_MarkI[(AM >> 30) & 31][1], charset_MarkI[(AM >> 35) & 31][1]
     );
}

void sim_debug_d(t_int64 d, char * smsg)
{
    sim_debug(DEBUG_DATA, &cpu_dev,
        "%s: %d%04d (%c%c%c%c %c%c%c%c) \n",
        smsg, (int)(d / 10000), (int)(d % 10000), 
        charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d>>10) & 31][1], charset_MarkI[(d >> 15) & 31][1], 
        charset_MarkI[(d >> 20) & 31][1], charset_MarkI[(d >> 25) & 31][1], charset_MarkI[(d >> 30) & 31][1], charset_MarkI[(d >> 35) & 31][1] 
     );
}

void sim_debug_b(int Bindex, char * smsg)
{
    int d;

    d=B[Bindex]; 
    if (d >> 19) d = (~MASK_20BITS) | d; // show index as signed integer
    sim_debug(DEBUG_DATA, &cpu_dev,
        "%s B%d: %d (%c%c%c%c) \n",
        smsg, Bindex, d, 
        charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d>>10) & 31][1], charset_MarkI[(d >> 15) & 31][1]
     );
}


// show debug for data read/writed from/to mem
void sim_debug_rw(int addr, int d, char * msg)
{
    sim_debug(DEBUG_DATA, &cpu_dev, "%s %04d (%c%c): %d (%c%c%c%c) at Tube S%d%c line %d \n", 
            msg, 
            addr, charset_MarkI[addr & 31][1], charset_MarkI[(addr >> 5)& 31][1], 
            d,    charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d >> 10) & 31][1], charset_MarkI[(d >> 15) & 31][1], 
            (addr >> 6), (addr & 32) ? 'R':'L', (addr & 31)
     );
}

// write data d in crt mem at address addr
// input: d is 20bits word (a short line), regular binary
//        addr is 0 .. 1023 (10 bits), regular binary
//        bmsg =1 -> add sim_debug msg entry
//        S is updated with 20 bits word writen from CRT, regular binary
void WriteAddr(int addr, int d, int bmsg)
{
    if (Console_Sw_KeyClear & 128)  {
        // if KEC switch is up, do not write to mem
        sim_debug(DEBUG_DATA, &cpu_dev, "... Write to addr %d ignored as KEC switch is active \n", addr);
        return; 
    }

    addr &= 1023; 
    if (addr >= 512) {
        // from book Introduction to programming on the manchester Electronic Digital Computer made by Ferranti (D.G. Prinz) 
        // on p7, item (9): the machine behaves as if it has 16 tubes, of with 8 last permanetly contains zero
        sim_debug(DEBUG_DATA, &cpu_dev, "... Writing to addr %d of tube S8-S16 not installed \n", addr);
        return; 
    }

    d &= MASK_20BITS; 
    CRT[addr] = S = d; 
    if (bmsg && (sim_deb)) sim_debug_rw(addr,d, "... Write");
}

// read data from crt mem at address addr
// return: d as 20bits word (a short line), regular binary
//        addr is 0 .. 1023 (10 bits), regular binary
//        S is updated with 20 bits word read from CRT, regular binary
int ReadAddr(int addr, int bmsg)
{
    int d; 

    if (Console_Sw_KeyClear & 128)  {
        // if KEC switch is up, mem read returns allways zero (it is being cleared)
        sim_debug(DEBUG_DATA, &cpu_dev, "... Reading zero from addr %d as KEC switch is active \n", addr);
        return 0; 
    }

    addr &= 1023; 
    if (addr >= 512) {
        // from book Introduction to programming on the manchester Electronic Digital Computer made by Ferranti (D.G. Prinz) 
        // on p7, item (9): the machine behaves as if it has 16 tubes, of with 8 last permanetly contains zero
        sim_debug(DEBUG_DATA, &cpu_dev, "... Reading from addr %d of tube S8-S16 not installed \n", addr);
        d = S = 0; 
    } else {
        d = S = CRT[addr]; 
    }
    if (sim_deb) {
        if (bmsg==2) {
            sim_debug_rw(addr,d, "Fetch");
        } else if (bmsg==1) {
            sim_debug_rw(addr,d, "... Read");
        }
    }
    return d; 
}


// write 40 bits value
void WriteAddr40b(int addr, t_int64 d)
{
    int dL, dH; 
    dL=(int)(d & MASK_20BITS); 
    dH=(int)(d >> 20); 
    // write first 20 bits value to first (short) line at address addr
    // will be the low part of 40 bits value
    WriteAddr(addr,dL, 1);
    // write second 20 bits value to next line 
    // will be the high part of 40 bits value
    // check if addr is last addr of 64 lines (words) of tube
    if ((addr & 63) != 63) {
        // addr is not the last line of a tube -> next line to be paired is line at next addr
        WriteAddr(addr+1,dH, 1);
    } else {
        // addr is the last line of a tube -> next line to be paired is first line of same tube
        addr = addr & (MASK_10BITS - 63); 
        WriteAddr(addr,dH,1);
    }
}

// read 40 bits value
t_int64 ReadAddr40b(int addr)
{
    t_int64 d, dL, dH; 
    // read first 20 bits value from first (short) line at address addr
    // will be the low part of 40 bits value
    dL=ReadAddr(addr,1);
    // read second 20 bits value from next line 
    // will be the high part of 40 bits value
    // check if addr is last addr of 64 lines (words) of tube
    if ((addr & 63) != 63) {
        // addr is not the last line of a tube -> next line to be paired is line at next addr
        dH=ReadAddr(addr+1,1);
    } else {
        // addr is the last line of a tube -> next line to be paired is first line of same tube
        addr = addr & (MASK_10BITS - 63); 
        dH=ReadAddr(addr,1);
    }
    d = (dH << 20) + dL; 
    return d;
}

// opcode decode 
//         returns entry in base_ops table
CONST char * DecodeInst(int d, int * opcode, int * Bindex, int * opaddr, 
                        int * nBeats, int bReturnOpname)
{
    CONST char * opname = NULL;
    int i;     

    // decode instr
    // addr: bits  0..9 (10 bits)
    // B   : bits 10..12 (3 bits)
    // function:  14..19 (6 bits)

    *opcode = (d >> 14) & 63; // function
    *Bindex = (d >> 10) & 7;
    *opaddr =  d & MASK_10BITS;

    if (bReturnOpname) {
        for (i=0; base_ops[i].name; i++) {
            if (base_ops[i].opbase == *opcode) {
                opname  = base_ops[i].name;
                if (nBeats) *nBeats += base_ops[i].nBeats;
                break;
            }
        }
    }
    
    return opname;
}

// Perform Character I/O
// return its duration in nBeats
t_stat CharIoInstr(int func, int * nBeats)
{
    static int last_ch = 0; // last teleprinter char punched/read/printed
    static int lpt_FigureShift_flag = 0; 
    static int ptp_start_tm0, lpt_start_tm0, ptr_start_tm0;
    static int lpt_insert_spaces = 0; 
    int op, n, ptr_ch, ptp_ch, lpt_ch, i, found; 
    int ch=0; 
    uint32 tnow, msec, msec1, msec2; 
    t_stat r; 

    ptr_ch=ptp_ch=lpt_ch=-1; 
    // decode operation
    if (Console_Sw_ManKbdPtrToLpt) {
        char sFigs[20]   = "0123456789. +-=";  
        char sLetter[40] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ/@:\"#$";
        // Console_Sw_ManKbdPtrToLpt=0 -> Printer input from computer, =1 -> from Ptr, =2 -> from its own keyboard
        if (func==1) {            // connect PTR output to Printer input. Affected by printer off setting
            r=ptr_char(&ch); 
            if (r) return SCPE_OK; // nothing read from ptr
            op=-1; // just print
            // check if teleprinter code
            if (strchr(sLetter, ch)==NULL) return SCPE_OK; // invalid char -> ignore
            if (lpt_FigureShift_flag) {
                // do a figureshift char translation
                for (i=0; FigureShift_charset_MarkI[0][i]; i++) {
                    if (FigureShift_charset_MarkI[0][i] != ch) continue; 
                    ch = FigureShift_charset_MarkI[1][i]; 
                    found = 1; 
                    break; 
                }
                if (found == 0) ch='~'; // smugge charater for undefined figureshift
            }
            lpt_ch=ch; // char to print
        } else if ((func & 0xFF) ==2) {  // connect printer keyboard locally to Printer input
            ch=(func >> 8);              // get char typed on printer keyboard
            op=4;                        // not affected by printer off setting
            if ((ch >= 'a') && (ch <= 'z')) ch=ch+'A'-'a';
            // first, scan to see if is a figure shift char
            if (ch==26) { // ^Z sets printer in figure shift mode
                lpt_FigureShift_flag=1; 
                return SCPE_OK; 
            } else if (ch==12) { // ^L sets printer in letter shift mode
                lpt_FigureShift_flag=0; 
                return SCPE_OK; 
            } else if ((ch==13) || (ch==10)) { 
                // print <CR> or <LF>
            } else {
                // check is the typed key belong to figure shift charset
                if (strchr(sFigs, ch)) {
                    lpt_FigureShift_flag=1; // set implicit fig mode
                } else if (strchr(sLetter, ch)) {
                   // check is the typed key belong to letter shift charset
                    lpt_FigureShift_flag=0; // set implicit letter shift mode
                } else {
                    return SCPE_OK; // char not belongs to printer charset 
                }
            }
            lpt_ch=ch; // char to print
        } else {
            // char io called from cpu instr while in lpt in manual mode -> ignore the instr
            return SCPE_OK; 
        }
    } else if (func==0) {
        // reset static vars. Called from reset
        last_ch = lpt_chariage_pos = lpt_insert_spaces = 0; 
        lpt_FigureShift_flag = 0; 
        ptp_start_tm0=lpt_start_tm0=ptr_start_tm0=0;
        lpt_char(26); // init print buffer      
        return SCPE_OK; 
    } else if (func==16) {           // code T    output char to ptp/lpt               
        ptp_ch=(AM >> 35) & 31; // char to punch/print, as teleprinter code
        last_ch=ptp_ch; // save the punched char, so it can be checked 
        op=3; // ptp+lpr ch as teleprinter code
        // 62 beats (if punch and print), 38 beats if only punch
        if (LPT_ON) { 
            *nBeats += 62; 
        } else {
            *nBeats += 38; 
        }
    } else if (func==17) {    // code Z    space to lpt
        lpt_ch=32; // space ascii code
        op=2; // lpr ch as ascii code       
        *nBeats += 62; 
    } else if (func==18) {    // code L    carriage return to lpt
        lpt_ch=13; // <cr> ascii code
        op=2; // lpr ch as ascii code       
        *nBeats += 62; 
    } else if (func==19) {    // code W    line feed to lpt
        lpt_ch=10; // <lf> ascii code
        op=2; // lpr ch as ascii code       
        *nBeats += 62; 
    } else if (func==20) {    // code H    figure shift at lpt
        lpt_ch=2; // set figure shift
        op=2; // lpr ch as ascii code       
        lpt_FigureShift_flag=1;
        *nBeats += 62; 
    } else if (func==21) {    // code Y    letter shift at lpt
        lpt_ch=1; // set letter shift
        op=2; // lpr ch as ascii code       
        lpt_FigureShift_flag=0;
        *nBeats += 62; 
    } else if (func==24) {    // code O    input char from ptr
        op=1; // input ch as ferranti code
        *nBeats += 28; 
    } else if (func==25) {    // code B    check (return last char read/printed/punched)
        op=0; // input ch using last_ch
        *nBeats += 5; 
    } else if (func==31) {    // code $    dummy
        *nBeats += 4; 
        sim_debug(DEBUG_DATA, &cpu_dev, "Dummy Magnetic Instr\n");
        return SCPE_OK; 
    } else {
        // from book Introduction to programming on the manchester Electronic Digital Computer made by Ferranti (D.G. Prinz) 
        // p26, item (4) PQG"MXV has no effect and can be considered dummy
        *nBeats += 4; 
        sim_debug(DEBUG_DATA, &cpu_dev, "Dummy Magnetic Instr\n");
        return SCPE_OK; 
    }
    // check if should wait end of previous on-goind operation
    tnow=sim_os_msec(); 
    if (op==3) {
        // op=ptp+lpt
        // ptp_ch = ferranti teleprinter code
        msec1=msec2=0; 
        if (PTP_ON) { // ptp active
            // check if ptp is punching something (punch 16 chars per sec)
            msec1=tnow - ptp_start_tm0; // msec elapsed from punch start
            if ((ptp_start_tm0==0) || (msec1 > 1000/16)) {
                ptp_start_tm0=0; // ptp has finished/has not started
                msec1=0; 
            } else {
                // ptp is ongoing. will Add msec = time needed to finish char punching 
                // to time needed to complete the instruction
            }
        }
        if (LPT_ON) { // lpt active
            // check if lpt is printing something (prints 6 chars per second) 
            msec2=tnow - lpt_start_tm0; // msec elapsed from punch start
            if ((lpt_start_tm0==0) || (msec2 > 1000/6)) {
                lpt_start_tm0=0; // lpt has finished/has not started
                msec2=0; 
            } else {
                // lpt is ongoing. will Add msec = time needed to finish char printing
                // to time needed to complete the instruction
            }
            // convert ptp_ch (teleprinter code) to lpt_ch
            lpt_ch = charset_MarkI[ptp_ch][1]; 
            if (lpt_FigureShift_flag) {
                // do a figureshift char translation
                for (i=0; FigureShift_charset_MarkI[0][i]; i++) {
                    if (FigureShift_charset_MarkI[0][i] != lpt_ch) continue; 
                    lpt_ch = FigureShift_charset_MarkI[1][i]; 
                    found = 1; 
                    break; 
                }
                if (found == 0) lpt_ch='~'; // smugge charater for undefined figureshift
            }
        }
        msec = (msec1 > msec2) ? msec1:msec2; // keep the longest time
        // ptp is ongoing. will Add msec = longer time needed to finish char printing/punching
        *nBeats += msec * 1000 / 240;
        // set start of new printing/punching char
        if (PTP_ON) { ptp_start_tm0 = tnow + msec; } // ptp punching start of this char
        if (LPT_ON) { lpt_start_tm0 = tnow + msec; } // lpt printing start of this char
    } else if ((op==2) && (LPT_ON)) {
        // op=lpt only, and lpt active
        // lpt_ch = ascii char 32, 13, 10, 1=set letter shift, 2= set figure shift
        // check if lpt is printing something (prints 6 chars per second) 
        msec=tnow - lpt_start_tm0; // msec elapsed from print start
        if ((lpt_start_tm0==0) || (msec > 1000/6)) {
            lpt_start_tm0=0; // lpt has finished/has not started
            msec=0; 
        } else {
            // lpt is ongoing. will Add msec = time needed to finish char printing
            // to time needed to complete the instruction
        }
        *nBeats += msec * 1000 / 240;
        lpt_start_tm0 = tnow + msec; 
    } else if (op==1) {
        // op=ptr read (move paper)
        // return ch as teleprinter code, save it in last_ch
        // check if ptr is reading something (170 chars per sec)
        // printer has 69 columns (0..68). next char is overprinted on last col. There is no automatic CR
        msec=tnow - ptr_start_tm0; // msec elapsed from punch start
        if ((ptr_start_tm0==0) || (msec > 1000/170)) {
            ptr_start_tm0=0; // ptr has finished/has not started
            msec=0; 
        } else {
            // ptr is ongoing. will Add msec = time needed to finish char reading 
            // to time needed to complete the instruction
        }
        *nBeats += msec * 1000 / 240;
        ptr_start_tm0 = tnow + msec; 
            // op=ptr read 
            sim_debug(DEBUG_DATA, &cpu_dev, "... Read PTR char ");
            r=ptr_char(&ch); 
            if (r!=SCPE_OK) {
                sim_debug(DEBUG_DATA, &cpu_dev, "\n");
                return r; 
            }
            sim_debug(DEBUG_DATA, &cpu_dev, "'%c' ", ch);
            // ch is ascii but should be teleprinter code -> must translate
            // first, scan to see if is a figure shift
            for (i=0; FigureShift_charset_MarkI[1][i]; i++) {
                if (FigureShift_charset_MarkI[1][i] != ch) continue; 
                ch = FigureShift_charset_MarkI[0][i]; // it is -> use the letter shift equivalent
                break; 
            }
            // translate char
            n=-1; 
            for (i=0;charset_MarkI[i][0]>=0; i++) {
                if (charset_MarkI[i][1]!=ch) continue; 
                n=charset_MarkI[i][0]; break; 
            }
            if (n<0) {
                n=0; // undefined char gest code zero
                sim_debug(DEBUG_DATA, &cpu_dev, "is UNDEF ");
            }
            n=n & 31; // safety
            sim_debug(DEBUG_DATA, &cpu_dev, "code %d \n", n);
            // return in 5 most significant bits of acc the read char
            AM=AM | (((t_int64) n) << 35); 
            last_ch=n; // save the read char, so it can be checked 
    } else if (op==0) {
        // op=check
        // check last char read/printed vs AM by XORing last char sent/recv with 
        // 5 Most significant bytes of AM
        sim_debug(DEBUG_DATA, &cpu_dev, "... Check %d \n", last_ch);
        AM=AM ^ (((t_int64) last_ch) << 35); 
    }
    // output char
    if (((lpt_ch>=0) && (LPT_ON)) || (op==4)) {
        if (lpt_ch == 2) {
            sim_debug(DEBUG_DATA, &cpu_dev, "... Printer set Figure Shift \n");
        } else if (lpt_ch == 1) {
            sim_debug(DEBUG_DATA, &cpu_dev, "... Printer set Letter Shift \n");
        } else {
            int DoCR=0; // do a <CR> flag
            if (lpt_ch==10) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Print char 10 <LF> Line Feed\n");
                // printing a <LF> generates a new line 
                lpt_char(13); 
                lpt_char(10); 
                // just one line down: keep track of spaces to insert on new line to set the
                // printing just where it was (just in following line)
                lpt_insert_spaces=lpt_chariage_pos; 
            } else if (lpt_ch==13) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Print char 13 <CR> Carriage Return\n");
                DoCR=1; 
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Print char %d '%c'\n", lpt_ch, (lpt_ch < 32) ? ' ':lpt_ch);
                if (lpt_insert_spaces) {
                    while (lpt_insert_spaces) {
                        lpt_char(' '); 
                        lpt_insert_spaces--;
                    }
                }
                lpt_CR_start_tm0=0; // reset time to terminate any <CR> animation in progress
                lpt_char(lpt_ch); // print char
                lpt_chariage_pos++;
                if (lpt_chariage_pos >= LPT_COLUMNS-1) {
                   sim_debug(DEBUG_DATA, &cpu_dev, "... Auto Carriage Return\n");
                   lpt_char(13); 
                   lpt_char(10); 
                   DoCR=1;  
                }
            }
            if (DoCR) {
                if (lpt_CR_start_tm0==0) {
                    lpt_CR_start_tm0=sim_os_msec(); // signal start of <CR> movement
                    lpt_CR_from_pos = lpt_chariage_pos; 
                }
                lpt_chariage_pos=0; 
                lpt_insert_spaces=0; 
            }
        }
    }
    if ((ptp_ch>=0) && (PTP_ON)) {
        // convert ptp_ch (teleprinter code) to ascii
        ch = charset_MarkI[ptp_ch][1]; 
        sim_debug(DEBUG_DATA, &cpu_dev, "... Punch '%c' value %d\n", ch, ptp_ch);
        ptp_char(ch); // punch char
    }
    return SCPE_OK;
}

// Perform Magnetic Instruction
// return its duration in nBeats
t_stat MagInstr(int Mag, int * nBeats)
{
    int track, tube, func; 
    int mode, cmd, same, c, n, d, a,b, op; 
    char lr; 
    struct {
        int a,b; // start address to transfer (a1->a2, and b1->b2)
    } xfer[2];

    // decode magnetic intruction
    track= Mag & 255; 
    func = (Mag >> 10) & 31; 
    tube = (Mag >> 16) & 7; // tube 
    sim_debug(DEBUG_DATA, &cpu_dev, "... Magnetic Instruction %c%c%c%c (tr=%d, func=%d, tube=%d) \n", 
        charset_MarkI[Mag & 31][1], charset_MarkI[(Mag >> 5) & 31][1], charset_MarkI[(Mag>>10) & 31][1], charset_MarkI[(Mag >> 15) & 31][1],
        track, func, tube
        );

    if (Console_Sw_KeyClear & 128)  {
        // if KEC switch is up, do not exec mag instr (mem is being cleared)
        sim_debug(DEBUG_DATA, &cpu_dev, "... Mag Instr ignored as KEC switch is active \n");
        return SCPE_OK; 
    }

    mode = func & 3; // Left Track to tube, ...
    cmd  = func >> 2; // 0=read, 1=check read, 2=write, 3=check write

    if (cmd < 4) {
        // Magnetic store drum wheel operations
        // set what to operate 
        if (mode==0) {        // is code /      Left Track to tube
            xfer[0].a = track * (2*64);  
            xfer[0].b = tube  * 64; 
            xfer[1].a = -1;              
            xfer[1].b = -1; 
        } else if (mode==1) { // is code E      Rigth Track to tube
            xfer[0].a = track * (2*64)+64;  
            xfer[0].b = tube  * 64; 
            xfer[1].a = -1;              
            xfer[1].b = -1; 
        } else if (mode==2) { // is code @      Left-Right Track to Even-Odd tube
            tube=tube & 6; // make tube number even
            xfer[0].a = track * (2*64);  
            xfer[0].b = tube  * 64; 
            xfer[1].a = track * (2*64)+64;  
            xfer[1].b = tube  * 64+64; 
        } else if (mode==3) { // is code A      Left-Right Track to Odd-Even tube
            tube=tube & 6; // make tube number even
            xfer[0].a = track * (2*64);  
            xfer[0].b = tube  * 64+64; 
            xfer[1].a = track * (2*64)+64;  
            xfer[1].b = tube  * 64; 
        }
        // determine i/o execution time
        // drum read is synchornous: execution suspended during i/o
        // read track: 35 msec, write track: 90 msec, 1 beat=240 usec
        if (cmd==0) {
            op=0; // read
            *nBeats += 35000/240; 
        } else if (cmd==2) {
            op=2; // write
            *nBeats += 90000/240; 
        } else {
            op=1; // check
            *nBeats += 35000/240; 
        }
        // if read operation, set the 65th CRT line (the marker line)
        if (cmd==0) {
            for (c=0; c<2;c++) {
                a=xfer[c].a; b=xfer[c].b; 
                if (a < 0) continue; 
                track=a/(2*64); lr=(a & 64) ? 'R':'L'; tube=b/64; 
                // from book Introduction to programming on the  manchester Electronic Digital Computer made by Ferranti (D.G. Prinz) 
                // on p24, item (2): .. the first 2 chars specify the track from wich the trasnfer was made, 
                // the next char denotes the half track (0 for left, 1 for right) and the last char is 1/2.
                CRT_line65[tube] = track + 32*32 * ((lr == 'R') ? 1:0) + 32*32*32*8; 
            }
        }
        // execute the operation
        same=1; 
        for (c=0; c<2;c++) {
            a=xfer[c].a; b=xfer[c].b; 
            track=a/(2*64); lr=(a & 64) ? 'R':'L'; tube=b/64; 
            if (track >= DRUM_TRACKS) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... ERROR: Read: from Track %d > MAX_TRACKS (%d) \n", track, DRUM_TRACKS);
                return STOP_IOERROR; 
            }
            if (a < 0) continue; 
            if (cmd==0) { // read: data from a (wheel) to b (tube)
                sim_debug(DEBUG_DATA, &cpu_dev, "... Read: from Track %d%c to Tube S%d \n", track, lr, tube);
            } else if (cmd == 2) { // write: data from b (tube) to a (wheel) 
                sim_debug(DEBUG_DATA, &cpu_dev, "... Write: from Tube S%d to Track %d%c \n", tube, track, lr);
            } else if (cmd == 1) { // Read check
                sim_debug(DEBUG_DATA, &cpu_dev, "... Read Check: Track %d%c vs Tube S%d \n", track, lr, tube);
            } else if (cmd == 3) { // Write check
                sim_debug(DEBUG_DATA, &cpu_dev, "... Write Check: Tube S%d vs Track %d \n", tube, track, lr);
            }
            for (n=0; n<64; n++) {
                if (op==0) { // read: data from a (wheel) to b (tube)
                    d=CRT[b+n]=DRUM[a+n];
                } else if (op == 2) { // write: data from b (tube) to a (wheel) 
                    d=DRUM[a+n]=CRT[b+n];
                } else { // check
                    if (DRUM[a+n] != CRT[b+n]) same=0;
                }
                if (op != 1) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, 
                        "    %2d| %2d (%c%c) %s DRUM[%2d]: %7d (%c%c%c%c)\n", 
                        n, b+n, charset_MarkI[(b+n) & 31][1], charset_MarkI[((b+n) >> 5)& 31][1], 
                        (cmd==0)?"<-":"->", a+n, 
                        d,    charset_MarkI[d & 31][1], charset_MarkI[(d >> 5) & 31][1], charset_MarkI[(d >> 10) & 31][1], charset_MarkI[(d >> 15) & 31][1]
                    );
                }
            }
            if (op==2) drum_write_track(track); // save written to track in drum file
        }
        if ((op==1) && (same==1)) C=C+2; // check succeeds -> skip 2 intructions
        return SCPE_OK; 
    }
    //Input/Output operations
    if (func >= 16) {
        return CharIoInstr(func, nBeats); 
    }
    return SCPE_OK; 
}

// opcode execution 
// input: fast mode flag, parts of instruction: opcode opaddr
// output: CpuBeats: incremented with number of additional clock beats used on execution
//         cTransferTo: set to 'N' for next instr, 'A' transfer to addr (C reg (=prog counter) already set)
t_stat ExecOpcode(int opcode, int opaddr, int Bindex,
                  int bFastMode,  
                  char * cTransferTo, 
                  int * CpuBeats)
{
    int n, Sneg, Dneg, Rneg;
    t_int64 d, lo, hi;  
    t_stat r; 
    static int Hoot_count = 0; 
    int sv_C = C; 

    #define Check_KAC  if ((Console_Sw_KeyClear & 1) || (Console_Sw_KeyClear & 128)) { AL=AM=0; }  // if KAC or KEC switch is up, reset accum at start on instr execution                                          
    #define Check_KBC  if ((Console_Sw_KeyClear & 2) || (Console_Sw_KeyClear & 128)) { B[0]=B[1]=B[2]=B[3]=B[4]=B[5]=B[6]=B[7]=0; }  // if KBC or KEC switch is up, reset B line at start on instr execution                                          
    #define Check_KDC  if ((Console_Sw_KeyClear & 4) || (Console_Sw_KeyClear & 128)) { D=0; }  // if KDC or KEC switch is up, reset D reg at start on instr execution                                          

    // check if there is a countdown to switch back KAC off 
    n = ((Console_Sw_KeyClear & 0xFFFFFF00) >> 8); 
    if (n) {  // is there a countdown in progress?
        int n1, n2; // yes, extract countdown
        n1= (n & 0xFFF); n2=(n >> 12); //n1=countdown to activate KAC, n2=countdoen to release KAC
        if (n1) { 
            n1--; // decr countdown waiting to activate KAC
            if (n1==0) {                         // if countdown expires ...
                AL=AM=0;                         // ... clear acc 
                Console_Sw_KeyClear |= (1 + 64); // ... set clear KAC, and set "update KAC switch" bit to sync the state of switch in cpanel
            }
        } else {
            n2--; // decr countdown waiting to bring KAC switch off 
            if (n2==0) {                         // if countdown expires ...
                Console_Sw_KeyClear &= ~(1);     // ... remove clear KAC bit
                Console_Sw_KeyClear |= 64;       // ... set "update KAC switch" bit to sync the state of switch in cpanel
            }
        }
        Console_Sw_KeyClear = (Console_Sw_KeyClear & 0xFF) + (n1 << 8) + (n2 << 20); 
    }
    // reset continuos hoot detection
    if ((opcode != OP_HOOT) && (opcode != OP_TR)) Hoot_count = 0; 

    *cTransferTo='N';
    switch(opcode) {
        case OP_NOP:               // T$  111111    Dummy (no B addition)
            break;
        case OP_STOP_L:            // /L  001001    Stop if Swith /L set (dummy stop)
            // console G (bit 0) and H (bit 1) dummy stop
            if (Console_Sw_DummyStop & 2) return STOP_HALT;    
            break; 
        case OP_STOP_G:            // /G  001011    Stop if Swith /G set (dummy stop)
            // console G (bit 0) and H (bit 1) dummy stop
            if (Console_Sw_DummyStop & 1) return STOP_HALT;    
            break; 
        case OP_HOOT:              // /V  001111    Audio hoot
            if (cpu_unit.flags & OPTION_BRK1_HOOT) {
                // a "set cpu brkhoot" scp command has been issued. This will breal on 
                // first hoot instr. It clears itselft so subsequent hoot instr does not
                // break (hoot are usuallu executed in a thight loop). To break again in a
                // hoot instr, a new "set cpu brkhoot" command should be issued again
                cpu_unit.flags &= ~OPTION_BRK1_HOOT;
                return SCPE_STEP;
            }
            Hoot_count++; 
            if (Hoot_count > 10) {
                return STOP_HOOT; 
            }
            break;
        case OP_RND:               // /W  011001    AL (lower 20 bits) = randon number generator
            if (cpu_unit.flags & OPTION_REPEATABLE_RANDOM) {
               // if "SET CPU REPEATABLE_RANDOM" command has been issued, then we use the variable
               // rand seed, so the random sequence will be repeatable across simulation sessions.
               // KEC resets the repeatable randon sequence to start
               sim_srand(rand_seed); 
               n=rand_seed=sim_rand();       
            } else {
               n=sim_rand();
            }
            AL=(n & MASK_20BITS);
            sim_debug_acc("... Random Acc");
            break; 
        case OP_CLA:               // T:  100100    Clear A
            AL=AM=0;
            sim_debug_acc("... Clear Acc");
            break; 
        case OP_SWAP_AM_AL:        // /I  001100    Swap AL <-> AM
            Check_KAC;
            d=AL; AL=AM; AM=d; 
            sim_debug_acc("... AL/AM swapped");
            break; 
        case OP_U_LOAD_A:          // T/  100000    AL=S, Clear AM
            d=ReadAddr40b(opaddr);
            AL=d; 
            AM=0;
            sim_debug_acc("... AM clear");
            break; 
        case OP_S_LOAD_A:          // T#  100010    AL=S, sign extend into AM
            d=ReadAddr40b(opaddr);
            AL=d; 
            AM=(d >> 39) ? MASK_40BITS:0; 
            sim_debug_acc("... Load Signed");
            break; 
        case OP_S_LOAD_MA:         // TF  110110    A=-(sign extended S)
            d=ReadAddr40b(opaddr);
            d=(-d) & MASK_40BITS;  
            AL=d; 
            AM=(d >> 39) ? MASK_40BITS:0; 
            sim_debug_acc("... Load with minus sign");
            break; 
        case OP_STORE_AL:          // /S  010100    S=AL (least significant accum 40 bits) 
            Check_KAC;
            WriteAddr40b(opaddr, AL);
            break;
        case OP_STORE_AL_CLA:      // TA  111000    S=AL, Clear A
            Check_KAC;
            WriteAddr40b(opaddr, AL);
            AL=AM=0;
            sim_debug_acc("");
            break;
        case OP_STORE_AM:          // /E  010000    S=AM (most significant accum 40 bits) 
            Check_KAC;
            WriteAddr40b(opaddr, AM);
            break;
        case OP_STORE_AM_CLAM:     // /A  011000    S=AM, clear AM
            Check_KAC;
            WriteAddr40b(opaddr, AM);
            AM=0;
            sim_debug_acc("... Clear AM");
            break;
        case OP_MOVE_CLAM:         // /U  011100    S=AL, AL=AM, clear AM
            Check_KAC;
            WriteAddr40b(opaddr, AL);
            AL=AM;
            AM=0;
            sim_debug_acc("... AL=AM, AM Clear");
            break;
        case OP_MSB_POS:           // /@  001000    AM=pos of MSB of contents of S (standarise) 
            Check_KAC;
            d=ReadAddr40b(opaddr);
            if (d==0) {
                n=63;
            } else {
                n=-1; 
                while (d) {
                    d = d >> 1; 
                    n++; 
                }
            }
            AM=(AM+n) & MASK_40BITS; 
            sim_debug_acc("... Standarise");
            break;
        case OP_POP_COUNT:         // /R  001010    Population Count: AM=AM+number of ones in addr s (sideways adder)
            Check_KAC;
            d=ReadAddr40b(opaddr);
            n=0;
            while (d) {
                if (d & 1) n++;
                d = d >> 1; 
            }
            AM=(AM+n) & MASK_40BITS; 
            sim_debug_acc("... Sideways add");
            break;
        case OP_ADD_AM:            // /J  011010    AM=AM+S (Add Upper)
            Check_KAC;
            d=ReadAddr40b(opaddr);
            d=AM + d;
            AM=d & MASK_40BITS; 
            sim_debug_acc("... AM add");
            break; 
        case OP_U_ADD_A:           // TI  101100    AL=AL+S, if carry inc AM (unsigned)
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            hi=0;
            goto add_lo_hi; 
        case OP_S_ADD_A:           // TC  101110    A=A+(sign extended S)
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            hi=(lo >> 39) ? MASK_40BITS:0; // hi=lo sign extended
          add_lo_hi:
            d=AL + lo;
            AL=d & MASK_40BITS; 
            d=d >> 40; 
            d=AM + hi + d;  
            AM=d & MASK_40BITS; 
            sim_debug_acc("");
            break; 
        case OP_S_SUB_A:           // TN  100110    A=A-(sign extended S)
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            lo=(-lo) & MASK_40BITS; 
            hi=(lo >> 39) ? MASK_40BITS:0; // hi=lo sign extended
            sim_debug(DEBUG_DATA, &cpu_dev,"... Sub to Acc \n"); 
            goto add_lo_hi; 
        case OP_S_LOAD_2A:          // TK  111110    A=2*(sign extended S)
            lo=ReadAddr40b(opaddr);
            lo=lo*2; 
            AM=(lo >> 40) ? MASK_40BITS:0; // hi=lo sign extended
            AL = lo & MASK_40BITS; 
            sim_debug_acc("");
            break;
        case OP_U_LOAD_D:          // /C  001110    D = S (multiplicand reg) unsigned
            Dsigned=0; 
            D=ReadAddr40b(opaddr);
            sim_debug_d(D, "... Load Unsigned D");
            break; 
        case OP_S_LOAD_D:          // /K  011110    D = S (multiplicand reg) signed
            Dsigned=1; 
            D=ReadAddr40b(opaddr);
            sim_debug_d(D, "... Load Signed D");
            break; 
        case OP_U_MPY_ADD:         // /N  000110    A = A + D * S unsigned
        case OP_U_MPY_SUB:         // /#  000010    A = A - D * S unsigned
        case OP_S_MPY_ADD:         // /F  010110    A = A + D * S signed
        case OP_S_MPY_SUB:         // /D  010010    A = A - D * S signed
            Check_KAC;
            Check_KDC;
            d=ReadAddr40b(opaddr);  Sneg=0; 
            if ((opcode == OP_S_MPY_ADD) || (opcode == OP_S_MPY_SUB) ) {
                // S is signed. Check if negative 
                if (d >> 39) {
                    d = (-d) & MASK_40BITS; 
                    Sneg=1; 
                }
            }
            // get in hi upper 20 bits, lo holds only lower 20 bits
            hi=d >> 20; lo=d & MASK_20BITS;
            // prepare multiplicand
            d = D; Dneg=0; 
            if (Dsigned) {
                sim_debug_d(D, "... Mult by Signed D");
                // D is signed. Check if negative 
                if (D >> 39) {
                    d = (-d) & MASK_40BITS; 
                    Dneg=1; 
                }
            } else {
                sim_debug_d(D, "... Mult by Unsigned D");
            }
            // multiply to obtain higher and lower part of result
            lo = d * lo; // resulting lo has 40 bits+20 bits = 60 bits result (lower part)
            hi = d * hi; // resulting hi has 40 bits+20 bits = 60 bits result (high part)
            // compose 80bit result in mhi (hight part) and mlo (lower part)
            {
                t_int64 mlo, mhi; int cy; 
                mlo = ((hi << 20) & MASK_40BITS) + (lo & MASK_40BITS); // lower 40 bits
                cy  = (mlo >> 40) ? 1:0; // carry
                mlo = mlo & MASK_40BITS;
                mhi = ((hi >> 20) + (lo >> 40) + cy) & MASK_40BITS; // upper 40 bits
                // mlo = ((hi << 20) + lo) & MASK_40BITS; // lower 40 bits
                // mhi = ((hi + (lo >> 20)) >> 20) & MASK_40BITS; // upper 40 bits
                lo=mlo; hi=mhi; 
                sim_debug_d(lo, "... lo 40bits result");
                sim_debug_d(hi, "... hi 40bits result");
            }
            // compute sign of product
            Rneg = ((opcode == OP_U_MPY_SUB) || (opcode == OP_S_MPY_SUB) ) ? 1:0; 
            if (Sneg) Rneg=1-Rneg; 
            if (Dneg) Rneg=1-Rneg; 
            if (Rneg) {
                // change sign of lo/hi (the product result) 
                lo = (~lo) & MASK_40BITS; 
                hi = (~hi) & MASK_40BITS; 
                d=lo+1; 
                lo = d & MASK_40BITS; 
                d=d >> 40; 
                hi=(hi+d) & MASK_40BITS; 
                sim_debug(DEBUG_DATA, &cpu_dev,"... Sub to Acc \n"); 
            }
            goto add_lo_hi; 
        case OP_AND_A:             // TR  101010    A=S extended AND A
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            hi=(lo >> 39) ? MASK_40BITS:0; // hi=lo sign extended
            AM=AM & hi; 
            AL=AL & lo; 
            sim_debug_acc("... AND");
            break; 
        case OP_OR_A:              // TD  110010    A=S extended OR A
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            hi=(lo >> 39) ? MASK_40BITS:0; // hi=lo sign extended
            AM=AM | hi; 
            AL=AL | lo; 
            sim_debug_acc("... OR");
            break; 
        case OP_XOR_A:             // TJ  111010    A=S extended NEQ A
            Check_KAC;
            lo=ReadAddr40b(opaddr);
            hi=(lo >> 39) ? MASK_40BITS:0; // hi=lo sign extended
            AM=AM ^ hi; 
            AL=AL ^ lo; 
            sim_debug_acc("... XOR");
            break; 
        case OP_LOAD_B:            // TT  100001    B=S
        case OP_LOAD_B_NF:         // TO  100011    B=S (no B addition)
            n=B[Bindex]=ReadAddr(opaddr, 1);
            Qneg = (n >> 19) ? 1:0; // copy sign bit of index
            sim_debug_b(Bindex, "... Write");
            break; 
        case OP_STORE_B:           // TZ  110001    S=B
        case OP_STORE_B_NF:        // TB  110011    S=B (no B addition)
            Check_KBC;
            n=B[Bindex];
            Qneg = (n >> 19) ? 1:0; // copy sign bit of index
            sim_debug_b(Bindex, "... Read");
            WriteAddr(opaddr, n, 1);
            break; 
        case OP_SUB_B:             // TL  101001    B=B-S
        case OP_SUB_B_same:        // TW  111001    B=B-S
        case OP_SUB_B_NF:          // TG  101011    B=B-S (no B addition)
        case OP_SUB_B_NF_same:     // T"  111011    B=B-S (no B addition)
            Check_KBC;
            n=B[Bindex]-ReadAddr(opaddr, 1);
            n=n & MASK_20BITS; 
            Qneg = (n >> 19) ? 1:0; // copy sign bit of index
            B[Bindex]=n;
            sim_debug_b(Bindex, "... Write");
            break; 
        case OP_TR_REL:            // /Q  011101    C=C+S+1 (relative jump). Unconditional relative transfer control
          Transfer_Relative:
            C=C+1+ReadAddr(opaddr, 1); 
            goto Transfer_done; 
            break;
        case OP_TR:                // /P  001101    C=S (jump to S). Unconditional direct transfer control
          Transfer:
            C=ReadAddr(opaddr, 1)+1; 
          Transfer_done:
            if (Console_Sw_ManualMode) {
                // manual mode: does not increment C on transfers
                C=C-1; 
            }
            C &= MASK_20BITS; // C holds 20 bits, but only 10 lower bits are used for prog counter
            sim_debug(DEBUG_DATA, &cpu_dev, "... Transfer to %d \n", (C & MASK_10BITS));
            *cTransferTo='A';
            if ((sv_C & MASK_10BITS) == (C & MASK_10BITS)) {
                return STOP_DYNAMIC_STOP; 
            }
            if (ReadAddr(C & MASK_10BITS, 0) == 0) {
                return STOP_TR_NOPRG; 
            }
            break;
        case OP_TR_IF_A:           // /H  000101    if A>=0 then C=S (C=IR=program counter). A conditional direct transfer control
            if (AM >> 39) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... AM<0 -> Transfer NOT taken \n");
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... AM>=0 -> Transfer taken \n");
                goto Transfer; 
            }
            break;
        case OP_TR_REL_IF_A:       // /M  000111    if A>=0 then C=C+S+1. Conditional relative transfer control
            if (AM >> 39) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... AM<0 -> Transfer NOT taken \n");
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... AM>=0 -> Transfer taken \n");
                goto Transfer_Relative; 
            }
            break;
        case OP_TR_IF_B:           // /T  000001    if B>=0 then C=S (C=IR=program counter). B conditional direct transfer control
            if (Qneg) { // Q bit set if negative
                sim_debug(DEBUG_DATA, &cpu_dev, "... B<0 -> Transfer NOT taken \n");
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... B>=0 -> Transfer taken \n");
                goto Transfer; 
            }
            break;
        case OP_TR_REL_IF_B:       // /O  000011    if B>=0 then C=C+S+1. B conditional relative transfer control
            if (Qneg) { // Q set when B becomes negative
                sim_debug(DEBUG_DATA, &cpu_dev, "... B<0 -> Transfer NOT taken \n");
            } else { // Q=0 when B positive
                sim_debug(DEBUG_DATA, &cpu_dev, "... B>=0 -> Transfer taken \n");
                goto Transfer_Relative; 
            }
            break;
        case OP_STORE_Z:           // /Y  010101    S=0 (20 bits) clear short line
            // this instruction is described in Ferranti Mark I Programming Manual first Edition
            // in summary of instructions, at end of document. The instr is not described elsewhere, 
            // nor on second and third edition.
            // So this instruction should be an "un-official" instruction. We implement anyway 
            WriteAddr(opaddr, 0, 1);
            break;
        case OP_STORE_H:           // /Z  010001    S=H (=handswitches value) 
            WriteAddr(opaddr, H, 1);
            break;
        case OP_MAG_INSTR_H:       // //  000000    H (=handswitches value) as magnetic instr (drum or I/O transfer)
            r=MagInstr(H, CpuBeats); 
            if (r!=SCPE_OK) return r; 
            break;
        case OP_MAG_INSTR_S:       // /:  000100    S as magnetic instr (drum or I/O transfer)
            n=ReadAddr(opaddr, 1);
            r=MagInstr(n, CpuBeats); 
            if (r!=SCPE_OK) return r; 
            break;
        case OP_LD_DRUM:           // T@  101000    Load drum addr of page (line 65) into AL, Clear AM
            // from book Introduction to programming on the  manchester Electronic Digital Computer made by Ferranti (D.G. Prinz) 
            // on p26, item (7) and (8) describes the behaviour implemented
            if ((opaddr & 31) ==0) {
                n=opaddr / 32; // get the 65th line for tube n
                if (n < 8) {
                    d = CRT_line65[n]; 
                } else d=0; 
                AL = d; AM = 0; // put it in lower 20 bits, clear rest of whole accum
                sim_debug_acc("... AL=Line 65th, AM clear");
            } else {
                // read into AL short line at address opaddr-1 (weird but true)
                d=ReadAddr(opaddr, 1);
                AL=d; AM=0;
                sim_debug_acc("... AL=Short Line (of addr-1), AM clear");
            }
            break; 
        default:
            // on doc it is not stated what the computer does when an undefined instruction is executed
            // On SimH we simply halts the CPU simulation
            sim_debug(DEBUG_DATA, &cpu_dev, "undefined Instruction, Halt te CPU\n");
            return STOP_UUO;
            break;
    }
    if (Console_Sw_PrepulseSpeed==1) {
        // if user selected the semi-continuous mode on control panel, instr will be executed at 
        // a rate of 50 instr/sec. This is 1sec/50 = 20 msec per instruction
        // 1 beat take 240 usec -> to last 20msec, each instr should take 83.3 beats
        *CpuBeats = 83; 
    }
    return SCPE_OK;
}


// measure cpu speed, calc CpuSpeed.measurement.BeatsPerSec
// used to control cpu speed (into Control_CpuSpeed()), and to display 
// current cpu speed if ^I is pressed on CPanel
// if Mode = 0 -> init measurement
//         = 1    calc current speed, return CpuSpeed.measurement.BeatsPerSec
//         = 2    just return CpuSpeed.InstrCount = number of instr executed in this run
int Measure_CpuSpeed(int Mode)
{
    t_int64 nBeats; 
    uint32  TmInterval; 
    uint32 tnow; 

    tnow=sim_os_msec();
    if (Mode==1) {
        // process measurement made 
        nBeats  = GlobalBeatsCount - CpuSpeed.measurement.Beats0;         // Beats executed ...
        TmInterval = tnow - CpuSpeed.measurement.tm0;                     // ... during wall clock msec
        if (TmInterval < 1) TmInterval=1; 
        CpuSpeed.measurement.BeatsPerSec = (int) ((nBeats * 1000) / TmInterval);
        return  CpuSpeed.measurement.BeatsPerSec;
    } else if (Mode==2) {
        return (int) CpuSpeed.InstrCount;
    } else if (Mode==0) {
        // init measurement cycle        
        CpuSpeed.measurement.tm0         = tnow;
        CpuSpeed.measurement.Beats0      = GlobalBeatsCount;
        CpuSpeed.measurement.BeatsPerSec = 0;
        CpuSpeed.BeatsCount=0;
    }
    return 0;
}

// speed measurement for scp/debug. measures the speed achieved during the 
// whole run (entering and leaving sim_instr())
// if Mode = 1, save sim_os_msec() as end of run, use it for measurement 
//         = 0, use saved value of sim_os_msec()
// returns TPS = Beats per Second, IPS = Instructions per Second
void Measure_run_CpuSpeed(t_int64 * elapsed, int * TPS, int * IPS, char * sBuf, int Mode)
{
    if (Mode==1) CpuSpeed.end_tm0 = sim_os_msec(); 

    *elapsed = CpuSpeed.end_tm0 - CpuSpeed.start_tm0;
    if ((*elapsed < 1) || (CpuSpeed.InstrCount < 2) || (CpuSpeed.start_tm0 == 0)) {
        // if run elapsed <1 msec or less than 2 instruction executed
        // then there are no data measured
        *elapsed = 0;
        return; 
    }
    // Beats per second achieved
    *TPS = (int) ((GlobalBeatsCount - CpuSpeed.GlobalBeatsCount0) * 1000 / *elapsed); 
    // Instructions per second achieved
    *IPS = (int) (CpuSpeed.InstrCount * 1000 / *elapsed);
    // duration of execution run in text
    if (*elapsed < 1000) {
        sprintf(sBuf, "%d msec", (int) (*elapsed));
    } else if (*elapsed < 100*1000) {
        sprintf(sBuf, "%0.1f sec (%d msec)", *elapsed /1000.0  , (int) (*elapsed));
    } else if (*elapsed < 1000*1000) {
        sprintf(sBuf, "%d min %d sec (%d sec)", (int)(*elapsed /60000), (int) (*elapsed % 60000) / 1000, (int) (*elapsed / 1000));
    } else if (*elapsed < 3600*1000) {
        sprintf(sBuf, "%d min", (int) (*elapsed / 60000));
    } else { 
        sprintf(sBuf, "%d h %d min", (int) (*elapsed / 3600000), (int) (*elapsed % 3600000) / 60000);
    }
}

// yield to OS 15msec, 30msec, 45msec or 60msec to slow down the CPU
// if this wait not enought to catch the target speed, returns 1 to 
// signal simulated cpu is going too fast
int Control_CpuSpeed(int bFastMode)
{
    int nMaxWaitLoop, bTooFast;
    
    if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {
        // no cpu speed control. Running at full speed, as fast as host can
        // Just poll the console keyboard to sense ^E break on console
        CpuSpeed.BeatsCount++;
        // poll each 10000 beats
        if (CpuSpeed.BeatsCount < 10000) return 0; 
        CpuSpeed.BeatsCount=0;
        if (sim_os_msec() - sim_last_poll_kbd_time > 100) {
            // check console to see if user has pressed the break char (normally Control-E)
            sim_poll_kbd();
        }
        return 0; 

    }
    CpuSpeed.BeatsCount++;
    if (CpuSpeed.BeatsCount < CpuSpeed.BeatsMax) return 0; 
    CpuSpeed.BeatsCount=0;
    // executed max number of Beats allowed during time interval

    nMaxWaitLoop = 3; // safety max wait 15msec x 3
    bTooFast=0; 
    // see if going too fast or too slow 
    while (CpuSpeed.BeatsObjectivePerSec < Measure_CpuSpeed(1)) {
        sim_os_ms_sleep(15);
        if (nMaxWaitLoop-- <0) {
            bTooFast=1; 
            CpuSpeed.BeatsCount = CpuSpeed.BeatsMax; // so next call to control_speed will wait again
            break; 
        }
    }
    // poll the console keyboard to sense ^E break on console
    if (sim_os_msec() - sim_last_poll_kbd_time > 100) {
        // check console to see if user has pressed the break char (normally Control-E)
        sim_poll_kbd();
    }
    return bTooFast; 
}  



t_stat sim_instr(void)
{
    t_stat              reason;
    int                 halt_cpu_requested_reason, bTooFast;
    int                 instr_count = 0;  /* Number of instructions to execute */
    const char *        opname;          /* points to opcode name */               

    int pc=0; 
    int opcode, Bindex, opaddr; 
    char cTransferTo; 

    int bFastMode; 
    int CpuBeatsUsed; 

    /* How CPU execution is simulated

    CPU clock is 100 KHz (1 cycle = 10 microseconds). But CPU is serial, 
    so CPU operates in beats, being a beat = 24 cycles = 240 usec.
    Any intruction or I/O operation needs a given number of beats.

    sim_interval count one simulated machine beat time (240 usec) 
    Each instr last a given number of beats = needs CpuBeatsUsed to be executed
    
    After I/O is initiated by a // or /: instr, the CPU will continue to exec
    intructions, until a new I/O is requested. At this point, the CPU pauses
    until first I/O operation finishes. 
    
    */

    reason = halt_cpu_requested_reason = 0;
    CpuBeatsUsed = 0;
    opname=NULL;
    cTransferTo='N'; 

    bFastMode = FAST; 
    CpuSpeed.start_tm0  = sim_os_msec();  // sim_os_msec() when run starts
    CpuSpeed.InstrCount = 0;              // init instr executed in this run
    CpuSpeed.GlobalBeatsCount0 = GlobalBeatsCount; // initial value to get the number of Beats executed on run termination
    Measure_CpuSpeed(0);                  // init cpu speed measurement
    sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because start of run\n");

    if (sim_step != 0) {
        // step scp command. We will count instructions, not beats
        instr_count = sim_step;
        sim_cancel_step();
    }

    while (reason == SCPE_OK) {       
        /* loop until halted */

        if (sim_interval <= 0) {        /* event queue? */
            reason = sim_process_event();
            if (reason != SCPE_OK)  {
                // if reason is another (error?) then stop inmediatelly
                break;                                  
            }
        }
        sim_interval -= 1;                       /* count down one sim_interval tick */
        if (CpuBeatsUsed > 0) CpuBeatsUsed--;    /* one sim_interval tick last a beat time = 240 usec on real hw */

        #if defined(CPANEL)
        if (cpanel_on) {
            // do control panel event processing 
            reason = cpanel_interval_refresh();  
            if (reason != SCPE_OK) break;  
        }
        #endif

        // control cpu speed, slowdown and yield a max of 45 msec time to OS if needed (with 
        // sim_os_ms_sleep). polling for console ^E is done into Control_CpuSpeed
        // return 1 if even yielding 60 msec the cpu is going too fast 

        bTooFast=Control_CpuSpeed(bFastMode);
        if (stop_cpu) {                                // stop_cpu set by scp on SIGINT (^C) or ^E on console
            if (halt_cpu_requested_reason == SCPE_STOP) {
                reason = SCPE_STOP;
                break;                                  // if ^E pressed twice, break if stalled/waitng fot end of tape movement
            }
            halt_cpu_requested_reason = SCPE_STOP;      // signal it so cpu is halted on end of current intr execution cycle
        }

        // increment number of beats count elapsed from starting of simulator -> this is the global time measurement
        if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
            // FAST mode -> increment GlobalBeatsCount at once
            GlobalBeatsCount += CpuBeatsUsed;
            CpuBeatsUsed = 0; 
        } else {
            // increment GlobalBeatsCount, loop if should wait
            GlobalBeatsCount++;
            CpuSpeed.BeatsCount++;
            if (CpuBeatsUsed > 0) continue; // if subop exec not finished then should still waiting, so loop again
            // if cpu is executing too fast, loop 
            if (bTooFast) continue; 
        }

        if (NCYCLE == 0) {
            if ((Console_Sw_KeyClear & 128) || (Console_Sw_KeyClear & 4)) {
                // if KEC or KCC switch are active, reset C to zero at start of instr execution
                C=0; PI=0;
                sim_debug(DEBUG_DATA, &cpu_dev, "... Reset C to zero as KEC or KCC switch is active \n");
            } else if (Console_Sw_ManualMode==0) {
                // auto mode: scan beat fetchs instruction to execute from C mem addr to Present Instruction register
                // only lower 10 bits are significant
                pc = C & MASK_10BITS; 
                PI = ReadAddr(pc, 2); // Fetch Intr to execute from CRT mem
            } else {
                // manual mode: scan beat fetch instruction to execute from console's Instruction Switches 
                PI = Console_Sw_Instruction; 
            }

            // Decode PI as tentative intruction
            DecodeInst(PI, &opcode, &Bindex, &opaddr, NULL, 0);
            if ((opcode & 0x31)==0x31) {
                // intructions is not B-modified
            } else {
                // modify intruction by adding B index value
                if ((Bindex==0) && (B[0]==0)) {
                    // using B[0]=0 -> B-modification changes nothing
                } else {
                    int Q = B[Bindex]; 
                    // add to debug trace original instr before being modified by index addition
                    sim_debug(DEBUG_CMD, &cpu_dev, "Inst %c%c%c%c (addr=%04d B%d func=%c%c)\n", 
                        charset_MarkI[PI & 31][1], charset_MarkI[(PI >> 5) & 31][1], charset_MarkI[(PI >> 10) & 31][1], charset_MarkI[(PI >> 15) & 31][1], 
                        opaddr, Bindex, 
                        charset_MarkI[(opcode & 1) << 4][1], charset_MarkI[opcode >> 1][1]);
                    // modify instruction into PI register by adding index
                    sim_debug(DEBUG_CMD, &cpu_dev, "... Add Index B%d: %d (%c%c%c%c)\n", 
                        Bindex, Q, 
                        charset_MarkI[Q & 31][1], charset_MarkI[(Q >> 5) & 31][1], charset_MarkI[(Q >> 10) & 31][1], charset_MarkI[(Q >> 15) & 31][1]);
                    PI=(PI + Q) & MASK_20BITS;
                }
            }
            // then decode actual intruction
            opname=DecodeInst(PI, &opcode, &Bindex, &opaddr, &CpuBeatsUsed, 1);
            if (sim_deb) {
                char sBindex[10]="B0 "; 
                if (Bindex) { sBindex[0]='B'; sBindex[1]='0'+Bindex; } else { sBindex[0]=0; }
                sim_debug(DEBUG_CMD, &cpu_dev, "Exec %c%c%c%c (addr=%04d %sfunc=%c%c) %s\n", 
                    charset_MarkI[PI & 31][1], charset_MarkI[(PI >> 5) & 31][1], charset_MarkI[(PI >> 10) & 31][1], charset_MarkI[(PI >> 15) & 31][1], 
                    opaddr, sBindex, 
                    charset_MarkI[(opcode & 1) << 4][1], charset_MarkI[opcode >> 1][1], 
                    (opname == NULL) ? "???":opname);
            }
            NCYCLE= 1; 
        }
        if (NCYCLE == 1)  {
            // action beat: execute instr
            DecodeInst(PI, &opcode, &Bindex, &opaddr, NULL, 0);
            reason = ExecOpcode(opcode, opaddr, Bindex, bFastMode, &cTransferTo, &CpuBeatsUsed);            
            if (reason != SCPE_OK) {
                // stop cpu at NCYCLE=1 -> when operating KCS switch or issuing step/go scp command
                // the instruction will be re-executed. This is OK for paper tape reading. But on Stop
                // we want the stop to be continued, so we set NCYCLE=2. 
                if (reason == STOP_HALT) NCYCLE=2; 
                break; 
            }
            NCYCLE= 2; 
        }
        if (NCYCLE >= 2)  {
            // action beat: execute instr
            // cTransferTo: set to 'N' for next instr, 'A' transfer to addr set in C
            // next cycle depends on what addr to transfer to at inst execution end
            if (cTransferTo == 'N') {
                if (Console_Sw_ManualMode) {
                    // manual mode: does not increment C at end of instr
                } else {
                    // auto mode
                    C=(C+1) & MASK_20BITS; 
                }
            }

            // one instruction fully executed
            NCYCLE=0;
            CpuSpeed.InstrCount++; 
            // break if stepping instructions
            if (instr_count != 0 && --instr_count == 0) {
                reason = SCPE_STEP; 
                break;
            }
            // break is console sw prepulse in single mode
            if (Console_Sw_PrepulseSpeed==2) {
                reason = SCPE_STEP; 
                break;
            }
            // break at end of instr if requested to
            if (halt_cpu_requested_reason) {
                reason = halt_cpu_requested_reason; // restore the reason value
                break;
            }
        }

    } /* end while */
    StopReason = reason; // save stop reason to be displayed in control panel
    // flush console log and debug file
    if (sim_deb) fflush(sim_deb); 
    if (sim_log) fflush(sim_log); 

    #if defined(CPANEL)
    // post-run refresh
    // if ((cpanel_on) && (reason != SCPE_EXIT)) {
        // terminate in-progress I/O printer/tape if any
        // no need for post-run refresh 
    // }
    #endif
    {
        t_int64 elapsed;
        int TPS, IPS;
        char sBuf[80];

        Measure_run_CpuSpeed(&elapsed, &TPS, &IPS, sBuf, 1);
        if (elapsed > 0) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Beats per second\n", TPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Speed is x%0.1f relative to original hardware\n", TPS * 240 / 1000000.0);  // real hw speed: 4166 beats per second (each beat = 240 usec)
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Instructions executed: %.0f\n", 1.0 * CpuSpeed.InstrCount);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Instructions per second (IPS) achieved\n", IPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Run elapsed %s\n", sBuf);
        }
    }

    // flush printer and tape punch
    {
        if ((lpt_unit.flags & UNIT_ATT) && (lpt_unit.fileref)) {
            fflush(lpt_unit.fileref); 
        }
        if ((ptp_unit.flags & UNIT_ATT) && (ptp_unit.fileref)) {
            fflush(ptp_unit.fileref); 
        }
    }

    /* Simulation halted */
    return reason;
}

/* Reset routine */
t_stat cpu_reset(DEVICE * dptr)
{
    vm_init ();

    sim_brk_types = sim_brk_dflt = SWMASK('E');

    memset(CRT, 0, sizeof(CRT)); 
    memset(CRT_line65, 0, sizeof(CRT_line65));

    C = PI = S = 0;
    AL = AM = D = Dsigned = Qneg = 0; 
    B[0] = B[1] = B[2] = B[3] = B[4] = B[5] = B[6] = B[7] = B[8] = 0;
    NCYCLE = 3; // so when pressing Main/Single prepulse (or issuing a go/step), cpu incrs C and instr to 
                // be exec is fetched from addr 0001. This is needed for SchemeC to start as stated in doc
    StopReason = 0;
    CharIoInstr(0, NULL); // reset printer lettershift

    return SCPE_OK;
}

// clears all: regs, memory, symbolic mem, reset NCYCLE
t_stat cpu_kec_clear_everthing(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    cpu_reset(&cpu_dev);
    rand_seed=2; // init repeatable read random
    return SCPE_OK;
}


// clears accumulator, set countdown to simulate KAC switch set on, then 
// set back KAC switch for off
t_stat cpu_kac_clear_acc(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    int n1, n2; 
    // set countdown. Real hw speed approx 700 instr/sec while not doing I/O
    // so we set two countdown to operate KAC: one for off -> on, the seconed for back to off position
    n1=700; // off -> on
    n2=700; // back to off
    Console_Sw_KeyClear = (Console_Sw_KeyClear & 0xFF) + (n1 << 8) + (n2 << 20); 
    return SCPE_OK; 
}

// SET CPU DisplayTube=Sn/Sm
// if cpanel active, set the Storage to be displayed in Left and Right tube
t_stat cpu_display_tubes(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    char c, n; 
    if (cptr == NULL) return SCPE_ARG;
    
    n=0; 
    // scan line searching for
    while (*cptr) {
        c=sim_toupper(*cptr++); 
        if (c=='S') continue; 
        if (c=='/') continue; 
        if (c==';') break; // is a comment
        if ((c==' ') || (c==9)) continue; 
        if ((c>='0') && (c<'8')) {
#if defined(CPANEL)
            extern int Reg_RVisibleStoreTube; 
            extern int Reg_LVisibleStoreTube; 
            if (n==0) Reg_LVisibleStoreTube=c-'0'; else
            if (n==1) Reg_RVisibleStoreTube=c-'0'; 
            else return SCPE_ARG;
            n++; 
#endif
            continue; 
        }
        return SCPE_ARG;
    }
    return SCPE_OK; 
}


// SET CPU STOP=[/G][/L] | none | <none>
// Set the Dummy Stop Switches (/G and/or /L) that are active (at ON position)
t_stat cpu_active_stop_switches(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    char c; 
    if (cptr == NULL) return SCPE_ARG;
    
    // scan line searching for
    while (*cptr) {
        c=sim_toupper(*cptr++); 
        if (c=='<') continue; 
        if (c=='/') continue; 
        if (c==';') break; // is a comment
        if (c=='N') { Console_Sw_DummyStop &= 3; break;} // none or <NONE>
        if (c=='L') { Console_Sw_DummyStop |= 2; continue; }
        if (c=='G') { Console_Sw_DummyStop |= 1; continue; }
        return SCPE_ARG;
    }
    return SCPE_OK; 
}

/* Memory examine */

t_stat cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw)
{
    int d; 

    d = ReadAddr((int) addr, 0);
    *vptr = d; 
    return SCPE_OK;
}

/* Memory deposit */

t_stat cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw)
{   
    WriteAddr((int) addr, (int) val, 0); 
    return SCPE_OK;
}


t_stat cpu_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr) {
    fprint_set_help(st, dptr);
    fprint_show_help(st, dptr);
    return SCPE_OK;
}

t_stat cpu_set_speed(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    float num; 
    int c, MachineBeatDuration_usec;
    CONST char *tptr;

    if (cptr == NULL) return SCPE_ARG;
    
    if ((*cptr == 'M') || (*cptr == 'm')) {
        // SET CPU SPEED=MAX -> max cpu speed
        CpuSpeed_Acceleration=0;
        CpuSpeed.BeatsMax=0;
        return SCPE_OK;
    }
    // SET CPU SPEED=n.nn -> max cpu speed (100.0 .. 0.01) decimal digits optional
    num = (float) strtotv (cptr, &tptr, 0);
    if (num > 100.0) return SCPE_ARG;
    if (cptr == tptr) return SCPE_ARG;
    if (*tptr == '.') {
        ++tptr;
        c=*tptr;
        if ((c<'0') || (c>'9')) return SCPE_ARG;
        num = num + ((c-'0') / (float) 10.0);
        ++tptr;
        c=*tptr;
        if ((c<'0') || (c>'9')) {
            if ((c) && (c!=32)) return SCPE_ARG;
        } else {
           num = num + ((c-'0') / (float) 100.0);
        }
    }
    if (num==0.0) return SCPE_ARG;

    // calculate values
    CpuSpeed_Acceleration = (int) (num * 100); 
    // Machine Beat Duration 240 microseconds 
    // so machine speed is 4166.6 beats per second  
    MachineBeatDuration_usec = 240; 
    // CpuSpeed.BeatsMax = beats to do in CpuSpeed.msec mseconds
    CpuSpeed.BeatsMax  = (int) (CpuSpeed.msec * CpuSpeed_Acceleration * 1000 / (100.0 * MachineBeatDuration_usec));
    CpuSpeed.BeatsObjectivePerSec = (int) (1000000.0 * CpuSpeed_Acceleration / (MachineBeatDuration_usec*100.0) );
    return SCPE_OK;
}

t_stat cpu_show_speed(FILE *st, UNIT *uptr, int32 value, CONST void *desc)
{
    t_int64 elapsed;
    int TPS, IPS;
    char sBuf[80];

    if (CpuSpeed_Acceleration<=0) {
        fprintf (st, "Speed set to MAX\n");
    } else {
        fprintf (st, "Speed set to %0.2f (Clock at %0.1f KHz), relative to original hardware\n", 
            (float) CpuSpeed_Acceleration / 100,
            (float) (100.0 * CpuSpeed_Acceleration / 100.0));
    }

    Measure_run_CpuSpeed(&elapsed, &TPS, &IPS, sBuf, 0);
    // TPS = Beats Per second. 1 Mark I beat = 240 usec
    if (elapsed == 0) {
        fprintf (st, "Measured speed: no data\n");
    } else {
        fprintf (st, "Measured speed: Speed is x%0.1f relative to original hardware\n", TPS * 240 / 1000000.0);  // real hw speed: 1 beat = 240 usec
        fprintf (st, "                %d Instructions per second (IPS) achieved\n", IPS);
        fprintf (st, "                run elapsed %s\n", sBuf);
    }
    return SCPE_OK; 
}

const char * cpu_description (DEVICE *dptr) { 
    return "Ferranti Mark I Electronic Computer";     
}   
 

