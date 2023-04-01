/* norc_cpu.c: IBM NORC CPU simulator

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

   cpu          IBM NORC central processor

   From Wikipedia: The IBM Naval Ordnance Research Calculator (NORC) was a 
   one-of-a-kind first-generation (vacuum tube) computer built by IBM for the 
   United States Navy's Bureau of Ordnance. It went into service in December 
   1954 and was likely the most powerful computer at the time. The Naval 
   Ordnance Research Calculator (NORC), was built at the Watson Scientific 
   Computing Laboratory under the direction of Wallace Eckert.

   In 1955 NORC was moved to the Naval Proving Ground at Dahlgren, Virginia. 
   It was their main computer until 1958, when more modern computers were acquired. 
   It continued to be used until 1963.

   Doc on NORC known to exist but not found yet
   
   Kozarsky, K. "Naval Ordnance Research Calculator (NORC) compiler" Naval Proving Ground, 1955, AD62333
       NORC assembler. Supossed to define mnemonics
       
   F. M. Urvinitka "NORC high-speed CRT printer programming manual" Naval Proving Ground, 1958, NPG report no. 1620
       Additionar Opcode for CRT
       
   A. V. Hershey "Subroutines for the NORC CRT printer",1960, Naval Weapons Laboratory report No 1686 

   NORCTRAN - Fortan IV compiler + sort merge generator 1965

   From http://www.columbia.edu/cu/computinghistory/norc.html#notes:

   The first supercomputer in the sense that it was the first whose declared purpose was to 
   surpass all other computers and that there was a significant number of other computers to 
   surpass (thus one would not call ENIAC or ASCC a supercomputer); [...]

   speed comparitions

         year  machine cycle        integer add         float add           float mult        instr per
               (microsec)  clock    (cycles) microsec   (cycles) microsec   (cycles) microsec  second
    701  1952    12        83 KHz       5       60                                               10 KIPS
    650  1954    96      10.4 KHz       5      480      10-        960       27-     2592         0.2 KIPS
   NORC  1954     1       1.0 MHz      44       44      46-         46       72-       80        14 KIPS
    704  1955    12        83 KHz       2       24       6.4 avg    77       14.2 avg 170        40 KIPS
    709  1958    12        83 KHz       2       24       6.4 avg    77       14.2 avg 170        40 KIPS  
   7090  1959     2.4     416 KHz       2        5       6-         16       11-       35       240 KIPS 
   7030  1961     0.6     1.6 MHz       1        0.6     1-          1        4-        3       1.2 MIPS



*/

#include <math.h>
#include "NORC_defs.h"	        
#if defined(CPANEL)
#include "cpanel.h"
int cpanel_gui_supported = 1;     
#else
int cpanel_gui_supported = 0;     // so disp cpanelgui returns 0 on scp
#endif

t_stat              cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_reset(DEVICE * dptr);
t_stat              cpu_clear_crt_mem(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_set_speed(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_show_speed(FILE *st, UNIT *uptr, int32 value, CONST void *desc);
t_stat              cpu_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr);
const char          *cpu_description (DEVICE *dptr);


// CRT Memory
t_int64             CRT[3600]                        = {0}; // CRT main Memory 
char                CRT_Symbolic_Buffer[3600 * 80]   = {0}; // does not exists on real hw. Used to keep symbolic info 

// increment number of clock ticks counts elapsed from starting of simulator -> this is the global time measurement
t_int64 GlobalTicksCount=1;

// struct for SET CPU SPEED
int CpuSpeed_Acceleration = 0;   // parameter: cpu speed multiplier: 
                                 // 100->original harware speed, 50->half speed, 200->speed x2, 0=max speed
struct {
    // speed control variables
    uint32 msec;                  // constant: to calculate TicksMax (= Ticks that can be executed on given msec)
    int TicksMax;                 // calculated: number of Ticks that can be executed during msec at current speed setting
    int TicksCount;               // count of ticks already executed 
    int TicksObjectivePerSec;     // calculated: value to achieve
    // speed measurement for scp/debug
    uint32 start_tm0;             // sim_os_msec() when run starts
    uint32 end_tm0;               // sim_os_msec() when run ends
    t_int64 InstrCount;           // full intructions executed count (=0 on start of run)
    t_int64 GlobalTicksCount0;    // count of ticks already executed when run starts
    // measurement for speed control
    struct {
        t_int64 Ticks0;           // num word times already executed when starting speed measurement
        uint32 tm0;               // sim_os_msec() when starting speed measurement
        int TicksPerSec;          // speed measurement result: mean ticks executed per second
    } measurement;
} CpuSpeed = {15};                // adjust cpu speed each 15 msec


// cpu registers
int                 IC;                          // Added register not part of cpu. Has addr of current intr in execution, just for displaying purposes. IBM NORC has no dedicated program counter
int                 U;                           // Saved Program counter during opcode execution
int                 V;                           // Address to be accesed in CRT memory
int                 M4,M6,M8;                    // Address modifier registers
t_int64             IREG;                        // instruction register
t_int64             REG1, REG2;                  // Storage registers
int                 OV;                          // Overflow flag 
int                 ZR;                          // Zero result flag (on float result operation) 
int                 IA;                          // index Adjust flag 
int                 PRT;                         // printer ready flag 
int                 TCHK;                        // tape check flag 
int                 TEOF;                        // tape end of file flag 
int                 NSUBOP;                      // indicates current machine suboperation number to be executed, 0=exec whole instruction
int                 ESUBOP;                      // already executed suboperations bitamap 
t_int64             CRT_BUS;                     // last value in crt bus to be read/write to CRT          

uint8               StopReason      = 0;         // saved stop reason to be displayed in control panel
int                 P,Q,R,S,T = 0;               // decoded IREG
int                 TU = 0;                      // currently/last selected tape unit (TU) for tape operation

// console ligths and switches that are checked during cpu instruction execution
int Console_CurrentSubOp;         // already executed suboperations for indicator panel display
int Console_Li64;                 // console lights to signal the instruction 64..68has been executed
int Console_Li74;                 // console lights to signal the instruction has been executed
int Console_Sw74[6] = {0};        // selection switch for opcodes 74..79: =0 -> set to stop, =1 -> set to transfer, =2 -> set to off
int Console_LiOV, Console_LiIA, Console_LiZR, Console_LiTEOF, Console_LiTCHK; // condition check flag
int Console_Sw64[5] = {0};        // selection switch for OV,IA,ZR,TEOF,TCHK CONDITION STOP: =0 -> set to proceed, =1 -> set to stop
int Console_LiPrtChk;             // console lights for priter check flag (because writting to 0001-0007 while print cycle in progress)
int Console_LiAddrChk;            // console lights for address check flag 
int Console_LiIndexChk, Console_LiSignChk, Console_LiZeroDivChk, Console_LiTEOT;        // console lights for index/sign/zero div/end of tape check flag 
int Console_TMOV = 0;             // tape moving flag and tick counter for tape operation to finish 
int Console_PADV = 0;             // program advancing flag 
int Console_Tape_Addr[12] = {-1}; // tape addr selection addr
int Console_Sw_ProgCHKStop = 0;   // selection switch for PROGRAM CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
int Console_Sw_CRTCHKStop = 0;    // selection switch for CRT CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
int Console_Sw_PrinterCHKStop = 0;// selection switch for PRINTER CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
int Console_Sw_Write_Output = 0;  // selection switch for WRITE OUTPUT =0 -> set to off, =1 -> set to on
int Console_Sw_Floated_Index = 0; // selection switch for FLOATED INDEX =0 -> set to off, =1 -> set to on

// console ligths and that are just SET during cpu instruction execution
int Console_LiManualRead = 0;     // indicates a manual read operation in progress
int Console_PRT_Instr = 0;        // Indicate last printer opcode
int Console_Stall = 0;            // reason for cpu stalled (not tape moving, not prog advancing)
                                  //    0   no stall
                                  //    1	print instr stalled because printer cycle not finished
                                  //    2	tape instr stalled because printer cycle not finished
                                  //    3	tape instr stalled because tape unit not ready

// console ligths and switches that are NOT checked during cpu instruction execution
// used on real hw as helpers to set cpu state when cpu is halted
int Console_Sw_Reg1_CRTAddr = 0;  // selection switch for Register 1 CRT Address NNNN
int Console_Sw_Reg2_CRTAddr = 0;  // selection switch for Register 2 CRT Address NNNN
int Console_Sw_VEntry_Addr  = 0;  // selection switch for V Entry Address NNNN
int Console_Sw_KeyboardEntry = 0; // selection switch =1 -> digits typed on keyboard goes to reg1, =2 -> to reg 2, =0 -> to none
int Console_Sw_SourceOfInstr = 0; // selection switch =0 -> start execution using V, =1 -> use U, =2 use current contents of IREG 
int Console_Sw_TapeUnitSelector = 0; // tape unit select for read/read backwds/rew button on console

/* CPU data structures

   cpu_dev      CPU device descriptor
   cpu_unit     CPU unit descriptor
   cpu_reg      CPU register list
   cpu_mod      CPU modifiers list
*/

UNIT cpu_unit =
    { UDATA(NULL, 0, 0), 10  };

 
REG cpu_reg[] = {
    {DRDATAD(IC, IC, 16, "Current Instruction"), REG_FIT|REG_RO},
    {DRDATAD(IREG, IREG, 64, "Intruction Code Register"), REG_FIT},
    {DRDATAD(REG1, REG1, 64, "Register 1"), REG_VMIO|REG_FIT},
    {DRDATAD(REG2, REG2, 64, "Register 2"), REG_VMIO|REG_FIT},
    {DRDATAD(U, U, 16, "U Register"), REG_FIT},
    {DRDATAD(V, V, 16, "V Register"), REG_FIT},
    {DRDATAD(M4, M4, 16, "M4 Address Modifier"), REG_FIT},
    {DRDATAD(M6, M6, 16, "M6 Address Modifier"), REG_FIT},
    {DRDATAD(M8, M8, 16, "M8 Address Modifier"), REG_FIT},
    {ORDATAD(OV, OV, 1, "Overflow"), REG_FIT},
    {ORDATAD(ZR, ZR, 1, "Zero Result"), REG_FIT},
    {ORDATAD(IA, IA, 1, "Index Adjusted"), REG_FIT},
    {ORDATAD(TEOF, TEOF, 1, "End Of File"), REG_FIT},
    {ORDATAD(TCHK, TCHK, 1, "Tape Check"), REG_FIT},
    {ORDATAD(PRT, PRT, 1, "Printer Ready"), REG_FIT},
    {DRDATAD(NSUBOP, NSUBOP, 4, "SubOp number"), REG_FIT|REG_RO},
    {ORDATAD(CPANELGUI, cpanel_gui_supported, 1, "Control Panel GUI supported flag"), REG_FIT|REG_RO},
    {NULL} 
};

MTAB cpu_mod[] = {
    {OPTION_2K,            OPTION_2K,          "2000",                    "2K",          NULL},
    {OPTION_2K,            0,                  "3600",                    "3K",          NULL},    
    {OPTION_FAST,          0,                  NULL,                      "REALTIME",    NULL},
    {OPTION_FAST,          OPTION_FAST,        "Fast Execution",          "FAST",        NULL},
    {OPTION_STEP_SUBOP,    0,                  "Step Instructions",       "STEPINSTR",   NULL},
    {OPTION_STEP_SUBOP,    OPTION_STEP_SUBOP,  "Step SubOperations",      "STEPSUBOP",   NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,            "Clear CRT Memory",        "CRTMEMCLEAR", &cpu_clear_crt_mem, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,            "SPEED",                   "SPEED",       &cpu_set_speed, &cpu_show_speed},
    {0}
};

DEVICE cpu_dev = {
    "CPU", &cpu_unit, cpu_reg, cpu_mod,
    1, 10, 16, 1, 10, 64,
    &cpu_ex, &cpu_dep, &cpu_reset, NULL, NULL, NULL,
    NULL, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cpu_help, NULL, NULL, &cpu_description
};


// return max number of word in CRT mem: 2000 or 2600
int MemSize(void)
{
    return (MEM2K) ? 2000:3600;
}

// shift d value for nDigits positions 
// if nDigit > 0 shift left, if < 0 then shift right
// return value of shifted digits (without sign)
t_int64 Shift_Digits(t_int64 * d, int nDigits)  
{
    int i;
    t_int64 n;

    if (nDigits == 0) return 0;                           // no shift

    n = 0;
    if ((nDigits > 31) || (nDigits < -31)) {              // shift left/right 32 or more digits
        *d = 0;                                           // -> all digits overflows 
        return 0;
    } else if (nDigits > 15) {                            // shift left 16 or more digits
        n = *d; *d=0;                                     // all or some digits of d goes to result
        nDigits -=16; 
    } else if (nDigits < -15) {                           // shift right 16 or more digits
        n = *d; *d=0;                                     // all or some digits of d goes to result
        nDigits +=16; 
    } 
    if (nDigits > 0) {                                    // shift left
        for (i=0;i<nDigits;i++) {
            n  = n * 10 + (*d / D15);              
            *d = (*d % D15) * 10;      
            n = n % D16;
        }
    } else {   
        for (i=0;i<-nDigits;i++) {
            n=n / 10;         
            n = n + (*d % 10) * D15;
            *d = *d / 10;      
        }
    }
    return n;
}

// perform word integer arithmetics
// cOp can be '+' (add d1 to d2), '0' (return ten-complement of d1)
// use nDigits digits (4 or 16)
// retur4n -1 is something wrong with params
t_int64 IntegerArith(char cOp, t_int64 d1, t_int64 d2, int nDigits)
{
    if (cOp == '0') {
        d1 = D16 * 10 - d1;
    } else if (cOp == '+') {
        d1 = d1 + d2;
    } else return -1; 
    if (nDigits == 16) {
        return d1 % D16;
    } else if (nDigits == 4) {
        return d1 % D4;
    } else return -1; 
}


// opcode decode 
// input: prior to call DecodeOpcode InstReg cpu register must be loaded with the word to decode 
// output: decoded instruction as P, Q, R, S, T parts
//         returns opname: points to opcode name or NULL if undef opcode

CONST char * DecodeInst(t_int64 d, int * P, int * Q, int * R, int * S, int * T, int bReturnOpname)
{
    CONST char * opname = NULL;
    int i; 

    *P      = (int) Shift_Digits(&d, 2);          // decimal index
    *Q      = (int) Shift_Digits(&d, 2);          // opcode
    
    *R      = (int) Shift_Digits(&d, 4);          // instr parameter
    *S      = (int) Shift_Digits(&d, 4);          // instr parameter
    *T      = (int) Shift_Digits(&d, 4);          // instr parameter

    if (bReturnOpname) {
        for (i=0; base_ops[i].name; i++) {
            if (base_ops[i].opbase == *Q) {
                opname  = base_ops[i].name;
                break;
            }
        }
    }
    
    return opname;
}

// float value format:
//    decimal index: ee=0..30 and 99..70 for float exponent 0..+30 and -1..-30
//                   decimal index = 100 complement of exponent  
//    sign: 0 = positive, 1 = negative
//    word digits: 
//       ee s M mmmm mmmm mmmm  = M.m x 10^(ee)  
//                                Mmmmmmm = mantissa
//                                ee      = decimal index (== exponent)
//                                range: -9.9999 x 10^(-30) .. +9.999 x 10^(+30)
//    min value possible: 70 0 0 0000 0000 0001 = 1x10^(-42)
//    max value possible: 30 0 9 9999 9999 9999 = 9.9x10^30

// unpack float number word to its components: signed exponent (-30..+30), 
// sign (0=positive, 1=negative), 13 digits mantissa
void UnPackFloat(t_int64 d, int * exp, int * sgn, t_int64 * mant)
{
    *exp = (int) (d / D14);        // get decimal index
    if (*exp>=50) *exp=(*exp)-100; // convert to signed exponent
    d = d % D14;                   // remove exponent
    *sgn = (int) (d / D13);        // get sign, 
    *mant = d % D13;               // get 13 digits unsigned mantissa    
    if (*mant==0) *sgn=*exp=0;     // normalize zero
}

// pack float number word from its components: signed exponent (-30..+30), 
// sign (0=positive, 1=negative), 13 digits mantissa
void PackFloat(t_int64 * d, int exp, int sgn, t_int64 mant)
{
    if (exp < 0) exp=exp+100;
    *d = exp * D14 + sgn * D13 + mant;
}

// convert float word d to double 
double ToDouble(t_int64 d)
{
    int exp, sgn;
    double f; 
    UnPackFloat(d, &exp, &sgn, &d);
    if (d == 0) return 0;
    f=(d * 1.0) / D12; // f=n.nnnn mantissa as double 0..<10
    f=f*pow((double) 10,exp);
    if (sgn) f=-f; 
    return f; 
}


// convert specified index to float index
t_stat FloatOperands(t_int64 *dd, int bNormalize, int * CpuTicksUsed)
{
    int e, sgn; 
    t_int64 d;

    if (*dd==0) return 0;
    UnPackFloat(*dd, &e, &sgn, &d);
    if ((e > 30) || (e < -30)) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "index Invalid: is %d, should be 0..30 or 70..99 \n", (e<0)?e+100:e);
        Console_LiIndexChk=1; 
        // exponent invalid. Should stop the program?
        if (Console_Sw_ProgCHKStop) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because Prog Check set to stop\n");
            return STOP_INDEX;
        }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because Prog Check set to proceed\n");
        // if no stop -> use exponent as it is 
    }
    if (sgn > 1) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Sign Invalid: is %d, should be 0 (positive float number) or 1 (negative) \n", sgn);
        Console_LiSignChk=1; 
        // sign invalid. Should stop the program?
        if (Console_Sw_ProgCHKStop) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because Prog Check set to stop\n");
            return STOP_SIGN;
        }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because Prog Check set to proceed\n");
        // no stop -> zero is positive, non zero is negative
        sgn=1;
    }
    if (d==0) {
        *dd=0;
        return 0; // zero mantissa -> float = zero
    }
    // normalize mantissa: loop until first left digit of mantissa (i.e. digit 13) is non zero 
    if (bNormalize) {
        *CpuTicksUsed += 1; 
        while (1) {
            if (d / D12) break; 
            d=d*10;e=e-1;
            *CpuTicksUsed += 1; 
        }
        if (e<-42) {
            *dd=0; // underflow -> use zero
            return 0; // zero mantissa 
        }
        // re-convert exponent to dec index
        if (e<0) e=e+100;
    }
    // pack float normalized
    PackFloat(dd, e, sgn, d);
    return 0; 
}

// add REG1 and REG2 as float 
// result in REG1. REG2 holds mantissa digits 14-16 on leftmost digits)
void AddFloat(int bSubtract, int bAbsvalues, int bMinusResult, int * CpuTicksUsed)
{
    int n1,n2,sgn1, sgn2, sgn, d,d1,d2, carry, ncarry;
    t_int64 res;

    *CpuTicksUsed += 54-8-14;

    UnPackFloat(REG1, &n1, &sgn1, &REG1); // REG1/2 holds mantissa only
    UnPackFloat(REG2, &n2, &sgn2, &REG2);

    if ((REG1==0) && (REG2==0)) {
        *CpuTicksUsed -= 20;              // both operands zero -> result zero
        return; 
    }

    //sign analysis
    sgn=0;
    if (bAbsvalues) sgn1=sgn2=0;
    if ((sgn1) && (sgn2)) {
        sgn1=sgn2=0; bMinusResult = (bMinusResult) ? 0:1;
    } else if ((sgn1==0) && (sgn2)) {
        sgn1=sgn2=0; bSubtract = (bSubtract) ? 0:1;
    } else if ((sgn1) && (sgn2==0)) {
        sgn1=sgn2=0; bSubtract = (bSubtract) ? 0:1; bMinusResult = (bMinusResult) ? 0:1;
    }
    // now sgn1=sgn2=0, sign of result depends on bSubtract, bMinusResult and actual values of operands

    // special cases
    if (REG1 == 0) {
        if (bSubtract)    sgn2 = 1;
        if (bMinusResult) sgn2 = (sgn2) ? 0:1;
        PackFloat(&REG1, n2, sgn2, REG2);   // compose float in reg1 with data from reg2
        REG2 = 0;
        *CpuTicksUsed -= 16;
        return; 
    }
    if (REG2 == 0) {
        if (bMinusResult) sgn1 = 1;
        PackFloat(&REG1, n1, sgn1, REG1);   // compose float in reg1 
        REG2 = 0;
        *CpuTicksUsed -= 18;
        return; 
    }
    // REG1/2 holds mantissa of 13 digits. Add space for 1 extra digit -> mantissa holds 14 digits
    REG1=REG1 * 10; 
    REG2=REG2 * 10;

    if (bSubtract) {
        // if there is a subtraction with exp that differs more than 10^13, a 1 is subtracted from 
        // lower digit of bigger operand, as stated in page 8-9 of programming manual
        if (n1 <= n2-13) {
            REG2-=1; 
        } else if (n2 <= n1-13) {
            REG1-=1; 
        }
    }

    // equalize indices
    if (n1 != n2) {
        *CpuTicksUsed += 1;
        if (n1 < n2) {
            while(1) {
                REG1=REG1 / 10; // shift right
                n1++; 
                *CpuTicksUsed += 1;
                if (n1==n2) break; 
                if (REG1==0) {
                    if (bSubtract)    sgn2 = 1;
                    if (bMinusResult) sgn2 = (sgn2) ? 0:1;
                    PackFloat(&REG1, n2, sgn2, REG2);   // compose float in reg1 with data from reg2
                    REG2 = 0;
                    *CpuTicksUsed -= 16;
                    return; 
                }
            }
        } else if (n2 < n1) {
            while(1) {
                REG2=REG2 / 10; // shift right
                n2++; 
                *CpuTicksUsed += 1;
                if (n2==n1) break; 
                if (REG2==0) {
                    if (bMinusResult) sgn1 = 1;
                    PackFloat(&REG1, n1, sgn1, REG1);   // compose float in reg1 
                    *CpuTicksUsed -= 18;
                    return; 
                }
            }
        }
    }


    // now mantissas are aligned
    // add 14 digits mantissa into 15 digits result 
    res=0; 
    carry=ncarry=0;
    for (n2=0;n2<14;n2++) { 
        d1 = REG1 % 10; REG1=REG1 / 10; // shift right, get digit shifted
        d2 = REG2 % 10; REG2=REG2 / 10; // shift right, get digit shifted
        if (bSubtract) {
            d=d1-d2-ncarry; ncarry=0;
            if (d<0) {
                d=d+10; 
                ncarry=1;
            }
        } else {
            d=d1+d2+carry; carry=0;
            if (d>9) {
                d=d % 10; 
                carry=1;
            }        
        }
        res=(res / 10) + (d * D14); // res has 15 significative digits to allow hold a carry on sum
    }
    if (carry) {
        // incr exponent to take into account carry
        n1++; 
        res=D14 + res/10;
    } else if (ncarry) {
        // subtraction result is negative, do 10's complement of 14 digit mantissa
        res=(D15-res) % D15;
        sgn=1; 
        *CpuTicksUsed += 16;
    }
    if (bMinusResult) sgn = (sgn) ? 0:1;

    if (res == 0) {
        REG1 = REG2 = 0;
    } else {
        // compose float using 13 digits mantissa. As res has 15 digits, should remove 
        // last two digits by dividing by 100. these last digits goes on left of REG2        
        PackFloat(&REG1, n1, sgn, res/100); 
        REG2 = (res % 100) * D14; 
    }
    // printf("REG1=%08d %08d\n", printfw(REG1));
    // printf("REG2=%08d %08d\n", printfw(REG2));

}

// adjust float in reg1 + reg2 to the Float specification P
// if P=...30,70..90 -> shift reg1 to match this exponent (specified decimal index)
// else              -> normalize float to have fist digit of mantisa non zero
// Set ZR, IA, OV flags depending on special mode
// round is asked for
t_stat ShiftFloat(int P, int bSpecial, int bRound, int * CpuTicksUsed)
{
    int spec, e, n1, sgn1; 

    // calc float exp desired
    if ((REG1 == 0) && (REG2 == 0)) {
        *CpuTicksUsed += 1;
        Console_LiZR=ZR=1; return 0; 
    }
    if (P>=70) {
        e=P-100; 
        spec=1; // use specified index -> shift until exponen match e
    } else if (P <= 30) {
        e=P;
        spec=1; // use specified index -> shift until exponen match e
    } else {
        spec=0;
    }
    // selection switch for FLOATED INDEX =0 -> set to off, =1 -> set to on
    // if set to on, allways float even if specified index
    if (Console_Sw_Floated_Index) spec=0; 
    // unpack float
    UnPackFloat(REG1, &n1, &sgn1, &REG1); // REG1 holds mantissa only
    if (n1<-42) { // value too small, return zero
        REG1=0; 
        Console_LiZR=ZR=1; // signal zero result
        *CpuTicksUsed += 1;
        return 0;
    }
    REG1=REG1 * 1000 + Shift_Digits(&REG2, 3); // take 3 digits form reg2 to have 16 digits mantisa
    // shift mantissa
    if (spec) {
        // specified index: shift until reg1 exp match desired exp e
        while (n1 != e) {
            if (n1 > e) { 
                // should shift to left, reduce n1. Shift reg1+reg2 as a whole
                if ((REG1 / D15) > 0) {
                    // digit at top left of mantissa: will overflow if shifted
                    if (bSpecial) {
                        // special artih + specified index: all overflow digits discarded, and
                        // signal overflow indicator
                        Console_LiOV = OV=1; 
                    } else {
                        // standard artih + specified index: stop adjusting index (=exponent)
                        // signal Index Adjusted indicator
                        Console_LiIA = IA=1;
                        break;
                    }
                }
                Shift_Digits(&REG1, 1); REG1+=Shift_Digits(&REG2, 1);
                if ((REG1 == 0) && (REG2==0)) {
                    n1=e; 
                    break; 
                }
                *CpuTicksUsed += 1;
                n1--; 
            } else {
                // should shift to right, increase n1. Shift reg1+reg2 as a whole
                Shift_Digits(&REG2, -1); REG2+=Shift_Digits(&REG1, -1);
                if (REG1==0) {
                    n1=e; 
                    break;
                }
                *CpuTicksUsed += 1;
                n1++; 
            }
        }
    } else {
        // floating index: shift left until first digit of mantissa is not zero
        while (1) {
            if ((REG1 / D15) > 0) break; // first digit of mantissa not null -> exit
            if (n1 <= -30) break; // lowest exp reached, cannot shift any more
            // should shift to left, reduce n1. Shift reg1+reg2 as a whole
            Shift_Digits(&REG1, 1); REG1+=Shift_Digits(&REG2, 1);
            if ((REG1==0) && (REG2==0)) {
                n1=0;
                break;
            }
            *CpuTicksUsed += 1;
            n1--;
        }
        // for exponents < -30, allow left digit zero
        while (n1<-30) {
            // should shift to right, increase n1. Shift reg1+reg2 as a whole
            Shift_Digits(&REG2, -1); REG2+=Shift_Digits(&REG1, -1);
            if (REG1==0) {
                n1=e; 
                break;
            }
            *CpuTicksUsed += 1;
            n1++; 
        }
    }
    // conver back again REG1 fromn 16 digit mantissa to 13 digits mantissa
    Shift_Digits(&REG2, -3); REG2 += Shift_Digits(&REG1, -3); 
    // round if needed
    if (bRound) {
        // roundg: REG1 (mantissa) shold be >0 and last digit even
        // in this case, if discarde part of mantissa (in REG2) > 0, then 
        // increase last digit of REG1. As it is even, will not need to propagate any carry
        if ((REG1 > 0) && (REG2 > 0)) {
            if ((REG1 % 2) == 0) {
                REG1 += 1; 
                *CpuTicksUsed += 1;
            }
        }
    }
    // compose result
    if ((REG1 == 0) || (n1 < -30)) {
        REG1=0; // sanity
        Console_LiZR=ZR=1; // signal zero result
        *CpuTicksUsed += 1;
        return 0;
    } 
    PackFloat(&REG1, n1, sgn1, REG1); // compose float in reg1
    // not define in manual if ZR reset if result non zero. but seems logical to avoid
    // reseting the flag before each arith operation
    ZR=0; 
    return 0;
}

t_stat MultDivFloat(int bIsDiv, int bMinusResult, int * CpuTicksUsed)
{
    int n1,n2,sgn1, sgn2, sgn;

    *CpuTicksUsed += (bIsDiv) ? 268:72;
    *CpuTicksUsed += -8-14;

    UnPackFloat(REG1, &n1, &sgn1, &REG1); // REG1/2 holds mantissa only
    UnPackFloat(REG2, &n2, &sgn2, &REG2);

    //sign result analisys
    if ( ((sgn1==0) && (sgn2)) || ((sgn1) && (sgn2==0)) ) {
        sgn=1;
    } else {
        sgn=0;
    }
    if (bMinusResult) sgn = (sgn) ? 0:1;
    // now sgn is sign of result, sign of operands can be ignored

    // special cases
    if (REG2 == 0) {
        if (bIsDiv) {
            // Div by zero. Should stop the program?
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Divide By Zero \n");
            Console_LiZeroDivChk=1; 
            if (Console_Sw_ProgCHKStop) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because Prog Check set to stop\n");
                return STOP_DIVBYZERO;
            }  
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because Prog Check set to proceed\n");
            // no stop -> return zero
        } 
        REG1 = REG2 = 0;
        *CpuTicksUsed += (bIsDiv) ? -234:-38;
        return 0; 
    }
    if (REG1 == 0) {
        REG1 = REG2 = 0;
        *CpuTicksUsed += (bIsDiv) ? -234:-38;
        return 0; 
    }

    {
        double f1, f2; 
        f1=(REG1 * 1.0) / D12; // f=n.nnnn mantissa as double 0..<10
        f2=(REG2 * 1.0) / D12; 
        if (bIsDiv) {
           f1=f1/f2; 
           n1=n1-n2;
        } else {
           f1=f1*f2; 
           n1=n1+n2;
        }
        while (f1 >= 10) {f1=f1 / 10; n1++;}           
        while (f1 < 1) {f1=f1 * 10; n1--;}     
        // f1 ranges 0.0 .. 9.9999
        if (n1<-42) {
            f1=0; // underflow
        }
        if (n1>49) n1=49;
        f1=f1*D12; 
        REG1 = (t_int64) (f1);
        REG2 = (t_int64) ((f1-REG1)*D16);
        if (REG2 >= D16) REG2 = REG2 / 10; // safety
    }
    PackFloat(&REG1, n1, sgn, REG1); // compose float 
    // printf("Result: %02d %d %d %04d %04d %04d\n", printff(REG1));
    // printf("        %08d%08d\n", printfw(REG2));
    return 0;
}

// apply modifier M4 M6 M8 to addr if >= 4000
// increment CpuTickCount with the number of microsecs needed to perform the addr modification
// set InvalidAddr to 1 if invalid address
int ApplyModifier(int Addr, int * CpuTicksUsed, int * InvalidAddr)
{
    int M, m; 

    if (InvalidAddr) *InvalidAddr = 0; 
    // time to apply mem modiers: 8 usec
    if (Addr>= 8000) {M=M8; m=8; } else  // apply modifiers
    if (Addr>= 6000) {M=M6; m=6; } else 
    if (Addr>= 4000) {M=M4; m=4; } else M=-1;
    if (M>=0) {
        if (CpuTicksUsed) *CpuTicksUsed += 8;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "M%d Address Modifier: %04d \n", m, M);
        Addr=Addr + M;
    }
    if (MEM2K) {
        Addr = Addr % 2000;           // if mem is 2K, modulo 2000
    } else {
        Addr = Addr % 4000;           // if mem is 3600 wods, modulo 4000
        if ((Addr >= 3600) && (InvalidAddr)) {
            *InvalidAddr = 1; 
        }
    }
    return Addr; 
} 


// read operand from CRT from given addr. Apply address modifiers
// increment CpuTickCount with the number of microsecs needed to perform the operand read
// return -1 if reading at invalid address
t_int64 ReadOperand(int Addr, int * CpuTicksUsed)
{
    int InvalidAddr; 
    t_int64 d;

    // V reg is the address selecton for CRT memory
    V=ApplyModifier(Addr, CpuTicksUsed, &InvalidAddr);
    if (InvalidAddr) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Read %04d: Invalid Address \n", V);
        Console_LiAddrChk=1; 
        // Invalid address. Should stop the program?
        if (Console_Sw_CRTCHKStop) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because CRT Check set to stop\n");
            return -1;
        }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because CRT Check set to proceed\n");
        // if no stop -> return zero
        return 0; 
    }
    d=CRT_BUS=CRT[V];                   // read CRT mem (access time: 8 usec)
    if (CpuTicksUsed) *CpuTicksUsed += 8;
    return d; 
}

// save operand to CRT at given addr. Apply address modifiers
// increment CpuTickCount  with the number of microsecs needed to perform the operand read
// return -1 if writing at invalid address
int SaveOperand(int Addr, t_int64 d, int * CpuTicksUsed)
{
    int InvalidAddr; 

    // V reg is the address selecton for CRT memory
    V=ApplyModifier(Addr, CpuTicksUsed, &InvalidAddr);
    if (InvalidAddr) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Write %04d: Invalid Address \n", V);
        Console_LiAddrChk=1; 
        // Invalid address. Should stop the program?
        if (Console_Sw_CRTCHKStop) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because CRT Check set to stop\n");
            return -1;
        }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because CRT Check set to proceed\n");
        // if no stop -> ignore write
        return 0;
    }
    CRT[V]=CRT_BUS=d;
    if (CpuTicksUsed) *CpuTicksUsed += 8;
    sim_debug(DEBUG_DETAIL, &cpu_dev, "Write %04d: %08d%08d \n", V, printfw(d));
    if ((PRT==0) && (V<=7)) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Write at %04d while printer cycle in progress \n", V);
        // printer check. Should stop the program?
        Console_LiAddrChk=Console_LiPrtChk=1; // printer check flag because writting to 0001-0007 while print cycle in progress
        if (Console_Sw_PrinterCHKStop) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because Printer Check set to stop\n");
            return -1;
        }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because Printer Check set to proceed\n");
    }
    return 0; 
}

// get the tape unit 1..8 from tape address P and console tape address setting
// P=tape addr 01 to 12. 
int GetTapeUnit(int P)
{
    int unit; 

    P=P % 20; if (P>12) P-=10; 
    if (P==0) {
        unit = -1; 
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Tape Address P cannot be zero \n", P);
    } else {
        unit = Console_Tape_Addr[P-1];
        if (unit < 0) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Tape Address P %02d not set \n", P);
        }
    }
    return unit;
}

// opcode execution 
// input: fast mode flag, parts of instruction: P, opcode (=Q), R, S, T 
//        nSubOp: execute subop number n
// output: CpuTicksUsed: incremented with number of clock ticks used on execution
//         CurrentSubOp: or'ed with subops executed
//         cTransferTo: set to 'N' for next instr, or 'T' 'R' 'S' to transfer to address in these registers
t_stat ExecOpcode(int P, int opcode, int R, int S, int T, int nSubOp, 
                  int bFastMode,  
                  char * cTransferTo, 
                  int * CpuTicksUsed, int * CurrentSubOp)
{
    #define SUBOP(n, SO)   ( (nSubOp == (n)) ? (*CurrentSubOp|=(SO),1) : 0 ) 

    int n, i;
    t_int64 d;  
    int * flag; int * M; 
    int bStop, bPrint, SubOp; 
    char cRWop; 

    bStop=bPrint=0; 
    switch(opcode) {
        case OP_META_MASK:   // 42  //  Apply to C(R) the mask on C(S), then shift result on P
            // instruction 42 is not defined in programming manual nor faster faster book
            // nevertheless, it is used in PERT programs many times
            // logic of instruction deducted from code: digit in operand 2 are scanned. if digit is zero, the corresponding
            // digit in result is zeroed. If digit in operand 2 is NOT zero (any value), the digit in result is taken
            // unchanged from operand 1
        case OP_META_ADD:    // 40  //   C(R) + C(S) to T      Meta - 16 digit integer
        case OP_META_SUB:    // 41  //   C(R) - C(S) to T      Meta - 16 digit integer
            if ((R>0) && SUBOP(3, SO_Read_Operand_1)) {        // first subop is the 3dr (read_instr, decode, then this one)
                d=ReadOperand(R, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read R %04d: %08d%08d \n", V, printfw(d));
                REG1=d; 
                *CpuTicksUsed += 8;
            }
            if ((S>0) && SUBOP(4, SO_Read_Operand_2)) {        // second subop: read operand 2
                d=ReadOperand(S, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read S %04d: %08d%08d \n", V, printfw(d));
                REG2=d; 
            }
            if SUBOP(5, SO_WordAdd) {                            // third subop: word add
                if (opcode == OP_META_MASK) {
                    d=0;
                    for(i=0;i<16;i++) {
                        n = (int) Shift_Digits(&REG1, 1);
                        d=d * 10 + ( ((int) Shift_Digits(&REG2, 1)) ? n : 0); 
                    }
                    REG1=d;
                } else {
                    if (opcode == OP_META_SUB) {
                        REG2=IntegerArith('0', REG2, 0, 16);         // return 10 complement
                    }
                    REG1=IntegerArith('+', REG1, REG2, 16);     // RE1+REG2, 16 digits
                }
                *CpuTicksUsed += 48-14;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Result: %08d%08d \n", printfw(REG1));
            }
            if ((P>0) && SUBOP(6, SO_WordShift)) {     
                // programming manual states shift control as none for meta-arithmetics
                // nevertheless, shift in 42 instr is used many times, and
                // shift in 40 instr is used in re-read routines 
                if (P <= 55) n=-P;                               // P=0..55 -> right shift P digits, 56..99 -> left of 100-P digits
                        else n=100-P;
                REG2=Shift_Digits(&REG1, n);
                if (n!=0) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Word Shift %d digits%s: REG1: %08d%08d \n", 
                                                      (n<0) ? -n:n, (n>0) ? " left":" right",
                                                      printfw(REG1));
                }
                *CpuTicksUsed += (n<0) ? -n:n;
            }
            if SUBOP(7, SO_Terminate) {                          // last subop: store result
                REG2=REG1;
                if (T>0) {
                    n=SaveOperand(T, REG1, CpuTicksUsed);
                    if (n<0) return STOP_ADDR;
                }
            }
            break; 
        case OP_CHMOD:       // 50  // Add R S T to M4 M6 M8            
        case OP_CHMOD_CL4:   // 51  // Clear M4 then add R S T to M4 M6 M8
        case OP_CHMOD_CL6:   // 52  // Clear M6 then add R S T to M4 M6 M8
        case OP_CHMOD_CL46:  // 53  // Clear M4 M6 then add R S T to M4 M6 M8
        case OP_CHMOD_CL8:   // 54  // Clear M8 then add R S T to M4 M6 M8
        case OP_CHMOD_CL48:  // 55  // Clear M4 M8 then add R S T to M4 M6 M8
        case OP_CHMOD_CL68:  // 56  // Clear M6 M8 then add R S T to M4 M6 M8
        case OP_CHMOD_CL468: // 57  // Clear M4 M6 M8 then add R S T to M4 M6 M8
            if SUBOP(3, SO_Change_M4) {         // first subop: reset Modifiers register and change M4
                *CpuTicksUsed += 8;
                if ((opcode % 10) & 1) {
                    M4=0;  // 51,53,55,57 -> clears M4
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Clear M4\n"); 
                } 
                M4=(int) IntegerArith('+', M4, R, 4); // M4=M4+R, 4 digits only
                sim_debug(DEBUG_DETAIL, &cpu_dev, "M4: %04d \n", M4);
            }
            if SUBOP(4, SO_Change_M6) {         // second subop: change M6
                *CpuTicksUsed += 8;
                if ((opcode % 10) & 2) {
                    M6=0;  // 52,53,56,57 -> clears M6
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Clear M6\n"); 
                } 
                M6=(int) IntegerArith('+', M6, S, 4);
                sim_debug(DEBUG_DETAIL, &cpu_dev, "M6: %04d \n", M6);
            }                
            if SUBOP(5, SO_Change_M8) {         // third subop: change M8
                *CpuTicksUsed += 8;
                if ((opcode % 10) & 4) {
                    M8=0;  // 54..57 -> clears M8
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Clear M8\n"); 
                } 
                M8=(int) IntegerArith('+', M8, T, 4);
                sim_debug(DEBUG_DETAIL, &cpu_dev, "M8: %04d \n", M8);
            }                
            if SUBOP(6, SO_Terminate) {         // last subop: nothing, just go to next inst
                // just go to next instr
                // total durantion in faster, faster book is 40 microsec
                // 4 msec for subops readinst, 10 for decode (educated guess, seems reasonable values)
                // 8 msec for each addr modifier register addition (same as R modified in 40/41 instr)
                // 2 msec for terminate
                // so 4+10+8+8+8+2=40
                *CpuTicksUsed += 2;
            }                
            break; 
        case OP_TR_MOD:       // 59  // M4 M6 M8 to REG1
            // instruction 59 is not defined in programming manual nor faster faster book
            // nevertheless, it is used in PERT program update at addr 0120
            // Transfer Modifiders to REG1: 0000 M4 M6 M8 -> REG1
            if SUBOP(3, SO_Terminate) {        
                REG2=REG1 = (t_int64) (M4) * D8 + (t_int64) (M6) * D4 + (t_int64) (M8); 
                sim_debug(DEBUG_DETAIL, &cpu_dev, "00 M4 M6 M8 to REG1: 0000 %04d %04d %04d\n", M4,M6,M8);
            }      
            break; 
        case OP_ADDM4_TRNS:   // 58  // M4+R to M4, if M4 = S continue next instr, else (M4!=S) transfer to T
            // on programming manual and faster faster book, instruction 58 is tied to M4
            // nevertheless, on PERT program it is used with P signaling the address modifier to
            // use. P=04/00 for M4, P=06 for M6 and P=08 for M8. 
            // Simulation will stop on address error if other P value is used
            if ((P==0) || (P==4)) {SubOp = SO_Change_M4; M=&M4; n=4;}
            else if (P==6)        {SubOp = SO_Change_M6; M=&M6; n=6;}
            else if (P==8)        {SubOp = SO_Change_M8; M=&M8; n=8;}
            else return STOP_OPCODE; // invalid Modifier to use on Instr 58 (Loop)
            if SUBOP(3, SubOp) {         // first subop: change M4/6/8
                *CpuTicksUsed += 10;
                *M=(int) IntegerArith('+', *M, R, 4); // M4=M4+R, 4 digits only
                sim_debug(DEBUG_DETAIL, &cpu_dev, "M%d: %04d \n", n, *M);
            }
            if SUBOP(4, SO_Terminate) {         // last subop: set next inst
                *CpuTicksUsed += 22-15;
                if (*M!=S) {
                    // M not reached loop limit -> transfer 
                    *cTransferTo='T'; 
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to T \n");
                }
            }                
            break; 
        case OP_TR_PRT:     // 69  // if printer ready=0 transfer to next instr, else as instr 60
            flag=&PRT; 
            goto Do_Tr_Cond;
        case OP_TR_TCHK:    // 68  // if Tape Check failure=0 transfer to next instr, else as instr 60
            flag=&TCHK;
            goto Do_Tr_Cond;
        case OP_TR_EOF:     // 67  // if End of file=0 transfer to next instr, else as instr 60
            flag=&TEOF;
            goto Do_Tr_Cond;
        case OP_TR_ZR:      // 66  // if Zero result=0 transfer to next instr, else as instr 60
            flag=&ZR; 
            goto Do_Tr_Cond;
        case OP_TR_AI:      // 65  // if Adjusted Index=0 transfer to next instr, else as instr 60
            flag=&IA; 
            goto Do_Tr_Cond;
        case OP_TR_OV:      // 64  // if OV=0 transfer to next instr, else as instr 60
            flag=&OV; 
        Do_Tr_Cond:
            // if condition flag not set, then first subop (SUBOP 3) will be set as SO_Terminate
            // so the instr execturion terminates
            if ((*flag==0) && SUBOP(3, SO_Terminate)) { 
                *CpuTicksUsed += 24-14;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Condition not present \n");
                break;            // OV=0 -> condition not present -> exec SubOp terminate
            }
            if (*flag) {
                // if OV=1, reset OV, turn on Prog Light 64, exec inst 60
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Reset condition present \n");
                *flag=0; // reset contition flag
            }
            goto Do_TR;
        case OP_TR_STOP:    // 61  // do instr 60, stop           
            bStop=1; 
            goto Do_TR;
        case OP_TR_ROUND:   // 62  // Shift C(R) on P, move result to S rounding, transfer to T, 
        case OP_TR:         // 60  // Shift C(R) on P, move result to S, transfer to T
        Do_TR:
            // if field R not blank then read operand 1
            if ((R>0) && SUBOP(3, SO_Read_Operand_1)) {
                d=ReadOperand(R, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read R %04d: %08d%08d \n", V, printfw(d));
                REG1=d; REG2=0;
                *CpuTicksUsed += 4;
            }
            if ((P>0) && SUBOP(4, SO_WordShift)) {                          // second subop: shift 
                if (P <= 55) n=-P;                               // P=0..55 -> right shift P digits, 56..99 -> left of 100-P digits
                        else n=100-P;
                REG2=Shift_Digits(&REG1, n);
                if (n!=0) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Word Shift %d digits%s: REG1: %08d%08d \n", 
                                                      (n<0) ? -n:n, (n>0) ? " left":" right",
                                                      printfw(REG1));
                }
                *CpuTicksUsed += (n<0) ? -n:n;
            }
            if ((opcode==OP_TR_ROUND) &&                         // third subop (optional): round
                (REG1 > 0) && (REG2 > 0) && (P>0) &&
                ((REG1 % 2) == 0) &&
                SUBOP(5, SO_Round)   ) {  
                // if (ROUND opcode) and (shift right) 
                //    and (reg1 and reg2 not zero) <- value of reg1 can be rounded
                //    and (reg1 is even) <-- value of reg1 should be rounded
                // then subop can round
                // if this subop done, bSubOp=3
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Round: REG2: %08d%08d -> round REG1\n", 
                                                  printfw(REG2));
                REG1=REG1+1;
                *CpuTicksUsed += 1;
            }
            if SUBOP(6, SO_Terminate) {                          // last subop: store result, set transfer address
                *CpuTicksUsed += 21-14;
                REG2=REG1;
                if (T>0) {
                    *cTransferTo='T'; 
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to T \n");
                }
                if (S>0) {
                    n=SaveOperand(S, REG1, CpuTicksUsed);
                    if (n<0) return STOP_ADDR;
                }
                if (bStop) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Programmed STOP \n");
                    return STOP_PROG;
                }
                if (bPrint) { // set when transfer called from print opcode
                    Console_PRT_Instr=opcode; // Indicate last printer opcode
                    *CpuTicksUsed += 10*1000; // needs 10 msec to transmit words 0001 to 0007 to printer
                    n=2-(opcode & 1); // n=1 -> printer 1, n=2 -> printer 2
                    n = lp_cmd(&lp_unit[n], bPrint, bFastMode); // take into account user has pressed ^F on GUI
                    // printer operation result processing
                    if (n==STOP_PRINTER) {
                        Console_LiPrtChk=1; 
                        if (Console_Sw_PrinterCHKStop) {
                            sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because Printer Check set to stop\n");
                            return n;
                        }
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because Printer Check set to proceed\n");
                    }
                }
            }
            break; 
        case OP_TR_SGN:     // 63  // if Reg >0 Transfer to R, =0 transfer to S, <0 transfer to T
            if SUBOP(3, SO_Terminate) {    
                // 
                sim_debug(DEBUG_DETAIL, &cpu_dev, "REG1: %08d%08d\n", printfw(REG1));
                if (REG1==0) {
                    *cTransferTo='S'; 
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to S (REG1=0)\n");
                } else {
                    d=REG1;
                    if ((Shift_Digits(&d, 3) % 10) == 0) {          // digit 14=sign 0=positive, 1=negative
                        *cTransferTo='R'; 
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to R (REG1>0 because digit14=0)\n");
                    } else {
                        *cTransferTo='T'; 
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to T (REG1<0 because digit14!=0)\n");
                    }
                }
                *CpuTicksUsed += 24-15;
            }
            break;
        case OP_TR_NEQ_STOP: // 73  // if C(R)<>C(S) as 16 digit words, use T as next instr, stop
        case OP_TR_EQ_STOP:  // 71  // if C(R)=C(S) as 16 digit words, use T as next instr, stop
            bStop=1; 
        case OP_TR_NEQ:      // 72  // if C(R)<>C(S) as 16 digit words, use T as next instr
        case OP_TR_EQ:       // 70  // if C(R)-C(S)=0 as 16 digit words, use T as next instr
            if ((R>0) && SUBOP(3, SO_Read_Operand_1)) {        // first subop: read operand 1                
                d=ReadOperand(R, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read R %04d: %08d%08d \n", V, printfw(d));
                REG1=d; 
                *CpuTicksUsed += 8;
            }
            if ((S>0) && SUBOP(4, SO_Read_Operand_2)) {        // second subop: read operand 2
                d=ReadOperand(S, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read S %04d: %08d%08d \n", V, printfw(d));
                REG2=d; 
            }
            if SUBOP(5, SO_WordAdd) {                            // third subop: word add
                REG2=IntegerArith('0', REG2, 0, 16);             // return 10 complement
                REG1=REG2=IntegerArith('+', REG1, REG2, 16);     // RE1+REG2, 16 digits
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Diff: REG1: %08d%08d\n", printfw(REG1));
                *CpuTicksUsed += 48-15;
            }
            if SUBOP(6, SO_Terminate) {                          // last subop: store result
                n=0;
                if ((opcode == OP_TR_EQ) || (opcode == OP_TR_EQ_STOP)) n = (REG1==0) ? 1:0;
                                                                  else n = (REG1!=0) ? 1:0; 
                if ((n) && (T>0)) {
                   *cTransferTo='T'; 
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Transfer to T \n");
                   *CpuTicksUsed += 2;
                   if (bStop) {
                       sim_debug(DEBUG_DETAIL, &cpu_dev, "Programmed STOP \n");
                       return STOP_PROG;   
                   }
                }
            }
            break; 
        case OP_TSTC74: // 74  // if Sw74 off transfer to next instr, 
        case OP_TSTC75: // 75     if Sw74 is Trasnfer, do instr 60,
        case OP_TSTC76: // 76     if Sw74 is stop do instr 61 
        case OP_TSTC77: // 77   
        case OP_TSTC78: // 78  
        case OP_TSTC79: // 79  
            n=opcode - OP_TSTC74; // 0 for 74, 1 for 75 ...
            flag=&n;
            if (Console_Sw74[n]==2) {        // set to off
                n=0;
            } else if (Console_Sw74[n]==1) { // set to transfer
                n=1; 
            } else if (Console_Sw74[n]==0) {    // set to stop
                n=1;
                bStop=1;
            }
            goto Do_Tr_Cond; 
        case OP_MUL:           // 24  //   C(R) x C(S) to T      Ordinary - rounded
        case OP_MULM:          // 25  // -[C(R) x C(S)] to T     Ordinary - rounded
        case OP_MUL_SPE:       // 34  //   C(R) x C(S) to T      Special - unrounded
        case OP_MULM_SPE:      // 35  // -[C(R) x C(S)] to T     Special - unrounded
            SubOp = SO_Mult;
            goto Do_Arith; 
        case OP_DIV:           // 26  //   C(R) / C(S) to T      Ordinary - rounded
        case OP_DIVM:          // 27  // -[C(R) / C(S)] to T     Ordinary - rounded
        case OP_DIV_SPE:       // 36  //   C(R) / C(S) to T      Special - unrounded
        case OP_DIVM_SPE:      // 37  // -[C(R) / C(S)] to T     Special - unrounded
            SubOp = SO_Div;
            goto Do_Arith; 
        case OP_ADD:           // 20  //   C(R) + C(S) to T      Ordinary - rounded
        case OP_ADDM:          // 21  // -[C(R) + C(S)] to T     Ordinary - rounded
        case OP_ADD_SPE:       // 30  //   C(R) + C(S) to T      Special - unrounded
        case OP_ADDM_SPE:      // 31  // -[C(R) + C(S)] to T     Special - unrounded
            SubOp = SO_Add;
            goto Do_Arith; 
        case OP_SUB:           // 22  //   C(R) - C(S) to T      Ordinary - rounded
        case OP_SUBM:          // 23  // -[C(R) - C(S)] to T     Ordinary - rounded
        case OP_ABS:           // 28  //  |C(R)| - |C(S)| to T   Ordinary - rounded
        case OP_SUB_SPE:       // 32  //   C(R) - C(S) to T      Special - unrounded
        case OP_SUBM_SPE:      // 33  // -[C(R) - C(S)] to T     Special - unrounded
        case OP_ABS_SPE:       // 38  //  |C(R)| - |C(S)| to T   Special - unrounded
            SubOp = SO_Sub; 
            Do_Arith:
            if ((R>0) && SUBOP(3, SO_Read_Operand_1)) {        // first subop is the 3dr (read_instr, decode, then this one)
                d=ReadOperand(R, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read R %04d: %08d%08d \n", V, printfw(d));
                REG1=d; 
                *CpuTicksUsed += 8;
            }
            if ((S>0) && SUBOP(4, SO_Read_Operand_2)) {        // second subop: read operand 2
                d=ReadOperand(S, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read S %04d: %08d%08d \n", V, printfw(d));
                REG2=d; 
            }
            if SUBOP(5, SO_FloatOperands) {                    // convert operands to float if needed
                n=FloatOperands(&REG1, 1, CpuTicksUsed);
                if (n) return n; // STOP_INDEX or STOP_SIGN
                n=FloatOperands(&REG2, 1, CpuTicksUsed); 
                if (n) return n; // STOP_INDEX or STOP_SIGN
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Float: REG1: %02d %d %d %04d %04d %04d (%f)\n", 
                                                          printff(REG1), ToDouble(REG1));
                sim_debug(DEBUG_DETAIL, &cpu_dev, "       REG2: %02d %d %d %04d %04d %04d (%f)\n", 
                                                          printff(REG2), ToDouble(REG2));
            }
            if SUBOP(6, SubOp) {
                if ((SubOp == SO_Add) || (SubOp == SO_Sub)) {
                    // note: this subop also performs convert subop if needed. not isolated as
                    // subop to no complicate the code
                    AddFloat(
                        /* subtract? */     (SubOp == SO_Sub) ? 1:0,
                        /* abs values? */   ((opcode == OP_ABS) || (opcode == OP_ABS_SPE)) ? 1:0,
                        /* minus result? */ ((opcode % 2) == 1) ? 1:0, 
                        CpuTicksUsed); 
                } else {
                    n=MultDivFloat(
                        /* IsDiv */ (SubOp == SO_Div) ? 1:0,
                        /* minus result? */ ((opcode % 2) == 1) ? 1:0, 
                        CpuTicksUsed); 
                    if (n) return n; // STOP_DIVBYZERO
                }
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Result: %02d %d %d %04d %04d %04d %08d %08d (%f)\n", 
                                                           printff(REG1), printfw(REG2), ToDouble(REG1));
            }
            Do_Terminate_Arith:
            if SUBOP(7, SO_ShiftResult) {
                // note: this subop also performs round subop if needed. not isolated as
                // subop to no complicate the code
                n=ShiftFloat(P, /* standard or special */ (opcode >= 30) ? 1:0, 
                                /* round */ (opcode < 30) ? 1:0, 
                                CpuTicksUsed);
                if (n) return n; // STOP_INDEX or STOP_SIGN

                sim_debug(DEBUG_DETAIL, &cpu_dev, "Shift : %02d %d %d %04d %04d %04d %08d %08d (%f)\n", 
                                                           printff(REG1), printfw(REG2), ToDouble(REG1));
            }
            if SUBOP(8, SO_Terminate) {                          // last subop: store result
                REG2=REG1;
                if (T>0) {
                    n=SaveOperand(T, REG1, CpuTicksUsed);
                    if (n<0) return STOP_ADDR;
                }
                if ((OV) && (Console_Sw64[0])) {
                    // if switch OVERFLOW set to STOP (instead of proceed), then stop on overflow
                    return STOP_HALT;
                }
                if ((IA) && (Console_Sw64[1])) {
                    // if switch INDEX ADJUST set to STOP (instead of proceed), then stop on index adjust
                    return STOP_HALT;
                }
                if ((ZR) && (Console_Sw64[2])) {
                    // if switch ZERO RESULT set to STOP (instead of proceed), then stop on index adjust
                    return STOP_HALT;
                }
            }
            break; 
        case OP_TRUNC:         // 29  // truncating transfer C(R) with C(S) to T   Ordinary - rounded
        case OP_TRUNC_SPE:     // 39  // truncating transfer C(R) with C(S) to T   Special - unrounded
            if ((R>0) && SUBOP(3, SO_Read_Operand_1)) {        // first subop is the 3dr (read_instr, decode, then this one)
                d=ReadOperand(R, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read R %04d: %08d%08d \n", V, printfw(d));
                REG1=d; 
                *CpuTicksUsed += 8;
            }
            if ((S>0) && SUBOP(4, SO_Read_Operand_2)) {        // second subop: read operand 2
                d=ReadOperand(S, CpuTicksUsed);
                if (d<0) return STOP_ADDR;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Read S %04d: %08d%08d \n", V, printfw(d));
                REG2=d; 
            }
            if SUBOP(5, SO_FloatOperands) {                    // convert operands to float if needed
                if (REG1==0) {
                    REG1=REG2; REG2=0;
                } else {
                    REG2=(REG2 % D13) * 1000; // get REG2 mantissa (=decimal coeficient)
                }
                *CpuTicksUsed += 36-8-15;
                n=FloatOperands(&REG1, 0, CpuTicksUsed);
                if (n) return n; // STOP_INDEX or STOP_SIGN
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Float: REG1: %02d %d %d %04d %04d %04d\n", printff(REG1));
                sim_debug(DEBUG_DETAIL, &cpu_dev, "       REG2: %08d %08d\n", printfw(REG2));
            }
            goto Do_Terminate_Arith;
        // printer operation codes
        case OP_WAIT_PRT:     // 80  // as 60, exec delayed until printer ready is on
            goto Do_TR;
        case OP_PRT1:         // 81  // as 80, start printing on printer 1
        case OP_PRT2:         // 82  // as 80, start printing on printer 2
            bPrint=1;
            goto Do_TR;
        case OP_PRT1_SPE:     // 83  // as 80, start printing on printer 1 special function
        case OP_PRT2_SPE:     // 84  // as 80, start printing on printer 2 special function
            bPrint=2;
            goto Do_TR;
        // tape operation codes
        // There is no evidence in programming manual about tape i/o done while cpu execute instrctions
        // Faster, faster book states that tape/priter i/o is done thru register 1, so it seems that
        // no other opeation can take place simulaneosly to a tape transfer
        case OP_WRITE:        // 90  // write on tape P addr S to R as block T
        case OP_WRI_OUTPUT:   // 91  // as 90, leave spsce between groups of words for CTC processing
        case OP_DELETE:       // 92  // delete on tape P space for block S-R
        case OP_DEL_OUTPUT:   // 93  // as 92 for words written by 91
            cRWop = 'W'; // set the type of tape operation: write
            goto Do_Tape; 
        case OP_REWIND:       // 98
            cRWop = '*'; // set the type of tape operation: rewind to begin of tape
            goto Do_Tape; 
        case OP_VERIFY:       // 96
        case OP_VER_BWRD:     // 97
        case OP_READ:         // 94  // read forward from tape P, store words of block T at addr S to R
        case OP_READ_BWRD:    // 95  // read backward
            cRWop = 'R'; // set the type of tape operation: read 
            Do_Tape:
            if ((TU<1) || (TU>8)) return STOP_ADDR; // sanity check 
            if SUBOP(3, 0) {  
                // do not set the current subop yet. do the tape operation to determine the first
                // subop type: blockread, scan, blockwrite
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Tape operation on MT%d, Addr %04d->%04d, Block Number %04d\n", TU, R,S,T);
                // the prog manual does not details the TCHK flag behaviour. On PERT code it is possible to found several rewind
                // commands without checking tape. This means that it is likelly that each tape instr resets the tape check flag
                TCHK=0;
                TEOF=0; 
                // exec the tape command
                mt_info[TU].V1=R; 
                mt_info[TU].V2=S; 
                mt_info[TU].BlockNum=T; 
                mt_info[TU].result1=0; 
                n = mt_cmd(&mt_unit[TU], opcode, bFastMode); // take into account user has pressed ^F on GUI
                mt_info[TU].result1=n; 
                // on NORC, tape read/write is NOT async (there are no channels, DMA, etc) 
                // the CPU is blocked until tape read/write finishess. So the SubOperation for
                // Scan/Read/Write can last for up to few minutes. 
                // as an educated guess, let say cpu needs 40 microsec to send command to tape 
                *CpuTicksUsed += 40; 
            }
            if ((cRWop == 'R') &&    
                (mt_info[TU].cmd_usec1 > 0) && 
                SUBOP(4, SO_Scan)    ) { 
                // if (READ TAPE opcode) and (need time to read tape scanning for the block number to read) 
                // then this subop is exec       
                // tape operation in progress: set tape moving flag with time needed to do the tape op
                // this is the time in microseconds needed to scan tape
                Console_TMOV = mt_info[TU].cmd_usec1; 
            }
            if ((cRWop == 'R') &&    
                (mt_info[TU].cmd_usec2 > 0) && 
                SUBOP(5, SO_ReadBlock)    ) { 
                // if (READ TAPE opcode) and (need time for tape read) 
                // then this subop is exec
                // this is time needed for block read to CRT mem
                Console_TMOV = mt_info[TU].cmd_usec2; 
            }
            if ((cRWop == 'W') &&    
                (mt_info[TU].cmd_usec1 > 0) && 
                SUBOP(4, SO_WriteBlock)    ) { 
                // if (WRITE TAPE opcode) and (need time for write tape) 
                // then this subop is exec
                // this is the time in microseconds needed to write tape
                Console_TMOV = mt_info[TU].cmd_usec1; 
            }
            if SUBOP(6, SO_Terminate) {                         
                // tape operation terminated
                REG2=REG1; 
                // result processing
                n = mt_info[TU].result1;
                // process checks that are not program-checks (so they will allways stop cpu)
                if (n==STOP_ADDR) {
                    Console_LiAddrChk=1; return n; 
                } else if (n==STOP_IOERROR) {
                    return n; // host i/o error or unhandled sim_tape result
                } else if (n) {
                    Console_LiTCHK=TCHK=1; 
                    if (n==STOP_EOT) Console_LiTEOT=1;
                } else if (LastTapeCheck == MT_IND_EOF) {
                    Console_LiTEOF=TEOF=1; 
                } 
                // process program checks (switches can be set to stop or proceed)
                if ((TEOF) && (Console_Sw64[3])) {
                    // if switch END OF FILE set to STOP (instead of proceed), then stop on End of file detected
                    return STOP_HALT;
                }
                if (TCHK) {  // because mt_cmd returned STOP_TAPE or STOP_EOT
                    // note: stop on tape check is controlled by switch TAPE CHECK, not by PROGRAM CHECK switch
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Tape Check signaled\n");
                    if (Console_Sw64[4]) {
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "Stop CPU because switch TAPE CHECK set to STOP (instead of proceed)\n");
                        return n;
                    }
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "Continue because switch TAPE CHECK set to proceed\n");
                }
                REG2=REG1; 
            }
            break;
        default:
            return STOP_OPCODE;
            break;
    }
    return SCPE_OK;
}


// measure cpu speed, calc CpuSpeed.measurement.TicksPerSec
// used to control cpu speed (into Control_CpuSpeed()), and to display 
// current cpu speed if ^I is pressed on CPanel
// if Mode = 0 -> init measurement
//         = 1    calc current speed, return CpuSpeed.measurement.TickCountPerSec
//         = 2    just return CpuSpeed.InstrCount = number of instr executed in this run
int Measure_CpuSpeed(int Mode)
{
    t_int64 nTicks; 
    uint32  TmInterval; 
    uint32 tnow; 

    tnow=sim_os_msec();
    if (Mode==1) {
        // process measurement made 
        nTicks  = GlobalTicksCount - CpuSpeed.measurement.Ticks0;         // ticks executed ...
        TmInterval = tnow - CpuSpeed.measurement.tm0;                     // ... during wall clock msec
        if (TmInterval < 1) TmInterval=1; 
        CpuSpeed.measurement.TicksPerSec = (int) ((nTicks * 1000) / TmInterval);
        return  CpuSpeed.measurement.TicksPerSec;
    } else if (Mode==2) {
        return (int) CpuSpeed.InstrCount;
    } else if (Mode==0) {
        // init measurement cycle        
        CpuSpeed.measurement.tm0         = tnow;
        CpuSpeed.measurement.Ticks0      = GlobalTicksCount;
        CpuSpeed.measurement.TicksPerSec = 0;
        CpuSpeed.TicksCount=0;
    }
    return 0;
}

// speed measurement for scp/debug. mesaures the speed achieved during the 
// whole run (entering and leaving sim_instr())
// if Mode = 1, save sim_os_msec() as end of run, use it for measurement 
//         = 0, use save value of sim_os_msec()
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
    // Ticks per second achieved
    *TPS = (int) ((GlobalTicksCount - CpuSpeed.GlobalTicksCount0) * 1000 / *elapsed); 
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
        CpuSpeed.TicksCount++;
        // poll each 10000 Ticks
        if (CpuSpeed.TicksCount < 10000) return 0; 
        CpuSpeed.TicksCount=0;
        if (sim_os_msec() - sim_last_poll_kbd_time > 100) {
            // check console to see if user has pressed the break char (normally Control-E)
            sim_poll_kbd();
        }
        return 0; 

    }
    CpuSpeed.TicksCount++;
    if (CpuSpeed.TicksCount < CpuSpeed.TicksMax) return 0; 
    CpuSpeed.TicksCount=0;
    // executed max number of Ticks allowed during time interval

    nMaxWaitLoop = 3; // safety max wait 15msec x 3
    bTooFast=0; 
    // see if going too fast or too slow 
    while (CpuSpeed.TicksObjectivePerSec < Measure_CpuSpeed(1)) {
        sim_os_ms_sleep(15);
        if (nMaxWaitLoop-- <0) {
            bTooFast=1; 
            CpuSpeed.TicksCount = CpuSpeed.TicksMax; // so next call to control_speed will wait again
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


// symbolc trace commands: have the form of **cmd as comment in load command file
// Symbolic_Trace_tm0=sim_os_msec() when trace starts
// sTrace points to **cmd string
// return 1 if must reset Symbolic_Trace_tm0 value
int Symbolic_Trace(uint32 Symbolic_Trace_tm0, char * Symbolic_Buffer, char * sTrace)
{
    uint32 msec; 
    int trace, addr, len, reset_tm0;
    t_int64 d; 
    char buf[80];
    char c;

    buf[0]=0;
    reset_tm0 = trace = 0; 
    msec = sim_os_msec() - Symbolic_Trace_tm0;
    if(strstr(sTrace, "**time")) {
        // the "**time" string in symbolic buffer initialize the timestamp for <TRACE>
        Symbolic_Trace_tm0 = sim_os_msec();
        msec = 0; reset_tm0 = 1;
        trace = 1; 
    } else if(strstr(sTrace, "**trace")) {
        // the "**trace" string in symbolic buffer prints on console the time elapsed
        // from previous **time followed by line to be executed
        trace = 1;
    } else if(strstr(sTrace, "**echo")) {
        // the "**echo xxx" string in symbolic buffer prints on console xxx (cannot have spaces)
        // if contains ":", append contents of REG1
        sTrace +=6;
        addr=len=0;
        while (c=*sTrace++) {
            if (c==32) {
                if (len==0) continue; // skip leading spaces
                break; // space after xxx ends string to echo
            }
            if (c == ':') addr=1; // signal to append REG1
            if (len>20) break; 
            buf[len++] = c; 
        }
        buf[len]=0;
        if (addr) sprintf(&buf[len], " %08d%08d", printfw(REG1));
        trace = 1;
    } else if(strstr(sTrace, "**regs")) {
        // the "**regs" string in symbolic buffer prints on console the registers
        // the values are the ones BEFORE opcode execution
        sprintf(buf, "REG1: %08d%08d, M4: %04d, M6: %04d, M8: %04d", 
                     printfw(REG1), M4, M6, M8);
        trace = 1;
    } else if(strstr(sTrace, "**flags")) {
        // the "**flags" string in symbolic buffer prints on console the flags
        // the values are the ones BEFORE opcode execution
        sprintf(buf, "OV: %d, IA: %d, ZR: %d, TCHK: %d, TEOF: %d, PRT: %d",  
                     OV, IA, ZR, TCHK, TEOF, PRT);
        trace = 1;
    } else if(strstr(sTrace, "**m")) {
        // the "**mNNNN-NN" string in symbolic buffer prints on console the contents
        // of crt memory starting at NNNN. prints NN words
        // if NNNN >= 4000, use M4, if >= 6000 use M6, if >= 8000 use M8
        sTrace +=3;
        addr=len=0;
        while (c=*sTrace++) {
            if ((c<'0') || (c>'9')) break;
            addr = addr * 10 + c - '0';
        }
        if (c=='-') while (c=*sTrace++) {
            if ((c<'0') || (c>'9')) break;
            len = len * 10 + c - '0';
        }
        if (len==0) {sprintf(buf, "len=0"); } else 
        if (addr >= 8000) { addr = addr - 8000 + M8; trace=2; } else 
        if (addr >= 6000) { addr = addr - 6000 + M6; trace=2; } else 
        if (addr >= 4000) { addr = addr - 4000 + M4; trace=2; } else 
        if (addr<3600)    { trace=2; } else sprintf(buf, "invalid addr=%d", addr);
    }
    if (trace) {
        int hh,mm,ss;
        ss = msec / 1000; msec = msec - ss * 1000;
        mm = ss   / 60;   ss   = ss   - mm * 60;
        hh = ss   / 60;   hh   = hh   - hh * 60;
        sim_printf("%02d:%02d:%02d:%03d %04d: %s\n", hh,mm,ss,msec,IC,buf); 
        if (trace == 2) while(len>0) {
            if (addr<3600) d=CRT[addr]; else break;
            sim_printf("             %04d: %08d%08d\n", 
                addr, printfw(d)); 
            addr++;
            len--;       
        }
    } 
    return reset_tm0;  
}

void ClearInterlocks(void) 
{ 
    int unit; 

    // clear any pending event in queue
    for (unit=0;unit<=8;unit++) sim_cancel(&mt_unit[unit]);
    for (unit=0;unit<2;unit++) sim_cancel(&lp_unit[unit]);
    // init all tapes with ready flag on
    for (unit=0;unit<=8; unit++) mt_unit[unit].u5 |= MT_RDY; // set tape ready 
    // signal cpu that there is no pending tape operation
    Console_TMOV=0;
    // make printer ready
    PRT=1; 
}

t_stat sim_instr(void)
{
    t_stat              reason;
    int                 halt_cpu_requested_reason, bTooFast;
    int                 instr_count = 0; /* Number of instructions to execute */
    int                 subop_count = 0; /* number of suboperations to execute */
    const char *        opname;          /* points to opcode name */               
    char *              Symbolic_Buffer;
    uint32              Symbolic_Trace_tm0; 
    char *              sTrace;

    t_int64 d; 

    int bFastMode; 
    int CpuTicksUsed; 

    int SourceOfInstr, nTick;
    int bStallMsg; t_int64 bStallTick0;
    char cTransferTo; 

    /* How CPU execution is simulated

    Sim Interval count one simulated microsecond
    A NORC instruction is divided un suboperations, Each suboperation takes a given 
    number of microseconds time (variable CpuTicksUsed). 
    
    The Tape I/O is completelly synchonous. The suboperation for tape read, write 
    or scan (scan = reading tape but skipping its contents) remains blocked until the tape operation
    finishes. 
    
    A read or write suboperation will handle at most a block size of
    3600 words. On tape, a block of this size needs 92702 characters that uses 183.29 inches. To
    read or write this a time of 1309 miliseconds is needed

    A tape read instruction can ask to read a block situated at very end of tape reel. 
    Tape has medium has 16800 inches, read speed is 140 inch/sec. So the scan suboperation can 
    last as much as 16800/140=120 secs. 
    
    On a tape i/o instructuion decode SubOperation, the tape unit is checked. If it is 
    rewinding, the suboperation will wait for rew termination. Rewind is done at regular
    r/w speed (no high speed rew!). Thus decode subopeation can take up to 16800/140=120 secs.

    User can select to execute the instructions one by one, on an full instruction at once basis or 
    just in a suboperation by suboperation basis.


    */

    reason = halt_cpu_requested_reason = 0;
    CpuTicksUsed = 0;
    opname=Symbolic_Buffer=NULL;

    ClearInterlocks();
    Console_PADV=1; // program advancing
    Console_TMOV=0; // tape not moving
    Console_Stall=0; 

    bFastMode = FAST; 
    CpuSpeed.start_tm0  = sim_os_msec();  // sim_os_msec() when run starts
    CpuSpeed.InstrCount = 0;              // init instr executed in this run
    CpuSpeed.GlobalTicksCount0 = GlobalTicksCount; // initial value to get the number of ticks executed on run termination
    Measure_CpuSpeed(0);                  // init cpu speed measurement
    sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because start of run\n");

    Symbolic_Trace_tm0 = CpuSpeed.start_tm0; // sim_os_msec() when run starts
    bStallMsg = 0;

    if (P<0) {   
        // if P<0 -> manual operation: tape command issued from console
        SourceOfInstr = 0;
        instr_count = 1; 
        Console_LiManualRead=1;                 // indicates a manual read operation in progress
        P=R=S=T=0; TU = Console_Sw_TapeUnitSelector; 
        Console_CurrentSubOp = ESUBOP = 0; NSUBOP=3;     // directly start instr execution in Q field
    } else {
        // P>=0 -> regular instr. 
        SourceOfInstr = Console_Sw_SourceOfInstr; // get current console setting for source of instr
        if (sim_step != 0) {
            // step scp command. Can be step full instr or step subop
            if (cpu_unit.flags & OPTION_STEP_SUBOP) {
                subop_count = sim_step;
            } else {
                instr_count = sim_step;
            }
            sim_cancel_step();
        }
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
        sim_interval -= 1;         /* count down one microsecond */
        if (CpuTicksUsed > 0) CpuTicksUsed--;
        if (Console_TMOV) Console_TMOV--;

        #if defined(CPANEL)
        if (cpanel_on) {
            // do control panel refresh and user clicks event processing 
            reason = cpanel_interval_refresh();  
            if (reason == SCPE_STOP) {
                if (cpu_unit.flags & OPTION_STEP_SUBOP) break; // is subop set then break now, else ...
                halt_cpu_requested_reason = reason;     // if stop cpu requested, does not do it inmediatelly; save reason and go on
                reason = SCPE_OK;                       // signal it so cpu is halted on end of current intr execution cycle
            } else if (reason != SCPE_OK) {
                break;                                  // if reason is another (error?) then stop inmediatelly
            }
            {
                int b = bFastMode;
                bFastMode = (FAST) || (CpuSpeed_Acceleration<0);  // take into account user has pressed ^F on GUI
                if ((b) && (bFastMode==0)) {
                    // user has released ^F to exit fast mode 
                    // during fast mode, CpuTicksUsed (microseconds that an instruction need to execute) and
                    // Console_TMOV still being calculated, but there is no wait on these values
                    // when exiting fast mode should clear interlock and CpuTicksUsed values, 
                    // so cpu does not start waiting on fast mode exit!
                    CpuTicksUsed = 0;
                    ClearInterlocks();
                }
            }
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

        // increment number of ticks count elapsed from starting of simulator -> this is the global time measurement
        if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
            // FAST mode -> increment GlobalTicksCount at once
            nTick=CpuTicksUsed; if (nTick < Console_TMOV) nTick = Console_TMOV; // get max value of both
            GlobalTicksCount += nTick;
            CpuTicksUsed = Console_TMOV = 0; 
        } else {
            // increment GlobalTicksCount, loop if should wait
            GlobalTicksCount++;
            CpuSpeed.TicksCount++;
            if (CpuTicksUsed + Console_TMOV > 5000) {
                // if wait is > 1 msec -> update on indicator panel current instr/subops executed 
                // so the used can see where inst is stalled / doing and i/o
                Console_CurrentSubOp = ESUBOP; 
            }
            if (CpuTicksUsed > 0) continue; // if subop exec not finished then should still waiting, so loop again
            if (Console_TMOV > 0) continue; // if Tape Movemet exec not finished then should still waiting, so loop again

            // if cpu is executing too fast, loop 
            if (bTooFast) continue; 
        }

        if (NSUBOP <= 1) {
            // SubOp 1: read instruction to execute into IREG
            ESUBOP = 0;                                 // init SubOp (no read instruction, beacuse IREG already loaded)
            if (SourceOfInstr==2) {                     // use current contents of intruction register ...
                NSUBOP=2;                               // ... so directly continue to decode subop
            } else {
                NSUBOP=1;
                if (SourceOfInstr==1) V=U;              // read instr from U
                // read instr from V
                ESUBOP = SO_Read_Instr;                 // init SubOp read instruction
                CpuTicksUsed += 4;                      // init value of ticks used by instruction (execution time in microsec)
                                                        // set 4 usec for initial inst setup (educated guess)
                V=V % 4000;                             // normalize V (V does not use addr modifiers on reading instr)
                d=ReadOperand(V, &CpuTicksUsed);        // read instr
                if (d<0) {        
                    Console_LiAddrChk=1; 
                    reason = STOP_ADDR;                 // Invalid address to fetch instr from 
                    break;
                }
                IREG=d;                                 // set CPU instruction register
            }
            IC=V;                                       // Here V is the program counter. Save it for SCP
            U=V;                                        // copy V (the PC) to U (=saved PC)
            SourceOfInstr=0;                            // set source of instr the V register for next instr
        }                        
        if (NSUBOP == 2) {
            // SubOp 2: decode instruction reg IREG
            if ((ESUBOP & SO_Decode)==0) {
                // not decoded yet -> decide IREG in its fields P Q R S T 
                CpuTicksUsed += 10;                         // educated guess
                // get symbolic info if any
                if (U * 80 > sizeof(CRT_Symbolic_Buffer)) {
                    Symbolic_Buffer=NULL;
                } else {
                    Symbolic_Buffer = &CRT_Symbolic_Buffer[U * 80];
                }
                // Decode IREG SubOperation
                opname=DecodeInst(IREG, &P, &Q, &R, &S, &T, 1);
                sim_debug(DEBUG_CMD, &cpu_dev, "Exec %04d: %02d %02d %04d %04d %04d %s \n", 
                                               IC, P, Q, R, S, T,
                                               (Symbolic_Buffer) ? Symbolic_Buffer : (opname) ? opname : "???");
                if ((Symbolic_Buffer) && (sTrace=strstr(Symbolic_Buffer, "**"))) {
                    int n; 
                    n = Symbolic_Trace(Symbolic_Trace_tm0, Symbolic_Buffer, sTrace); 
                    if (n) Symbolic_Trace_tm0=sim_os_msec(); // reset time origin for trace printouts
                }

                if (opname == NULL) {
                    // invalid opcode allways stops the program.
                    sim_debug(DEBUG_CMD, &cpu_dev, "Invalid Opcode\n");
                    reason = STOP_OPCODE;    // break beacuse undefined opcode
                    break; 
                }
                // set console light for executed condition
                if ((Q >= 64) && (Q <= 68)) Console_Li64 |= (1 << (Q-64)); // bit0 set if Q=64, bit1 set if Q=65 ...
                if ((Q >= 74) && (Q <= 79)) Console_Li74 |= (1 << (Q-74)); // bit0 set if Q=74, bit1 set if Q=75 ...
                // decode IREG done
                ESUBOP |= SO_Decode;     
            }

            // check if calculator stall execution because interlocked by I/O
            Console_Stall=0;
            if ((Q >= 80) && (Q <= 84)) {                
                if (PRT==0) Console_Stall=1; // print instr stalled because printer cycle not finished
            } else if ((Q >= 90) && (Q <= 98)) {
                // get the Tape Unit selected (1..8) from tape address P and console tape address setting
                // P=tape addr 01 to 12. manual do not states what happends if P=00. What the  simulator does: stop 
                TU = GetTapeUnit(P);
                if ((TU<1) || (TU>8)) {
                    Console_LiAddrChk=1; 
                    reason=STOP_ADDR; // invalid tape address 
                    break;
                }
                // tape instr stalled because printer cycle not finished
                if (PRT==0) Console_Stall=2; else            // tape instr stalled because printer cycle not finished                
                if (mt_ready(TU)==0) Console_Stall=3;      // tape instr stalled because tape unit not ready
            }
            if (Console_Stall) {                
                if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
                    ClearInterlocks();    
                    Console_Stall=0;
                } else {
                    if (bStallMsg==0) {
                        sim_debug(DEBUG_CMD, &cpu_dev, "Stalled by %s \n", 
                            (Console_Stall == 1) ? "Print instr stalled because printer cycle not finished" :
                            (Console_Stall == 2) ? "Tape instr stalled because printer cycle not finished" :
                            (Console_Stall == 3) ? "Tape instr stalled because tape unit not ready" :
                            "???");
                        bStallMsg=1; 
                        bStallTick0=GlobalTicksCount; 
                    }
                    CpuTicksUsed += 10*1000; // wait 10 msec
                    Console_PADV=0; // remove program advancing flag
                    continue;
                }
            }
            
            if (bStallMsg) {
                sim_debug(DEBUG_CMD, &cpu_dev, "... execution resumed (stalled for %d msec) \n", 
                    (int) ((GlobalTicksCount - bStallTick0) / 1000)  );
                bStallMsg=0; 
            }
            Console_PADV=1; // set flag program advancing
        }                        
        if (NSUBOP >= 3)  {
            // SubOp 3 and following: execute instruction already decoded
            int sv; 
            int bInstrFastMode = ((bFastMode) || (CpuSpeed_Acceleration<=0));

            cTransferTo='N'; // default transfer to next instr 
            // exect next suboperation
          Do_SubOp: 
            sv=ESUBOP; 
            reason = ExecOpcode(P, Q, R, S, T, NSUBOP, bInstrFastMode,
                                &cTransferTo, &CpuTicksUsed, &ESUBOP);
            if ((reason == 0) && (sv==ESUBOP)) {
               NSUBOP++; // no subop done!, try next nsubop
               goto Do_SubOp; 
            }
            if (ESUBOP & SO_Terminate) {
                // SubOp terminate executed -> instr terminated whatever reason value is
                // goto execute second part of terminate subop
                NSUBOP=10; 
            } else if (reason != SCPE_OK) {
                break; // if error in subop other than terminate, then exit
            }
       }
       if (NSUBOP == 10) {
           // second part of terminate subop (common to all instr)
           ESUBOP |= SO_OprnCompleted;     
           Console_CurrentSubOp = ESUBOP; // set the suboperations of whole instr executed to be shown in indicator panel
           NSUBOP=0; //reset subop counter
           if ((reason == 0) || (reason == STOP_PROG)) {
                // if instr finished ok or a programmed stop 
                // prepare for next inst as final part of terminate suboperation
                // time needed for that is included in SO_terminate time
                if (cTransferTo=='R') T=R; else if (cTransferTo=='S') T=S;
                if (cTransferTo=='N') {
                    V=U+1; // next inst from saved prog counter + 1
                } else if (T==0) {
                    V=U+1; // if T=0 go to next inst instr
                } else {
                    // apply addr modifiers if any   
                    // time needed for that is included in SO_terminate time
                    // do not check for invalid addr. Will be checked on start of next inst
                    V=ApplyModifier(T, NULL, NULL); 
                }    
                // round inst duration to next multiple of 8 microsec
                CpuTicksUsed = ((CpuTicksUsed + 7) / 8) * 8;
                // clean up a manual read operation in progress light
                Console_LiManualRead=0;
                // one instruction fully executed
                CpuSpeed.InstrCount++; 
                // break if stepping instructions
                if (instr_count != 0 && --instr_count == 0) {
                    reason = SCPE_STEP; 
                    break;
                }
                // break at end of instr if requested to
                if (halt_cpu_requested_reason) {
                    reason = halt_cpu_requested_reason; // restore the reason value
                    break;
                }
            } else {
                break; // inst execution terminated with error or stop -> exit intr loop
            }
        }
        // exec next subop
        NSUBOP++;
        // break if stepping suboperations
        if (subop_count != 0 && --subop_count == 0) {
            reason = SCPE_STEP; 
            break;
        }

    } /* end while */
    Console_CurrentSubOp = ESUBOP; // set the suboperations of instr/subops executed to be shown in indicator panel

    #if defined(CPANEL)
    // post-run refresh
    if ((cpanel_on) && (reason != SCPE_EXIT)) {
        // terminate in-progress I/O printer/tape if any
        extern int bTapeAnimInProgress;
        extern int bShowInfo;
        extern uint32 ShowInfoTm0; 
        int i, bIOInProgress, IOInProgress=0; 
        char buf[80];
        int RefreshCount0=0; 
        uint32  tnow = sim_os_msec(); // start refresh wall cock time for wait for animation end

        ShowInfoTm0=0; // reset IPS value, now ending the I/O of last instr
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Start Wait for end of CPANEL animation\n");
        while (1) {       
            bIOInProgress=0;
            // refresh
            if (cpanel_on==0) break; // to handle cpanel closed during post-run refresh
            cpanel_interval_refresh();  
            // update sim events, sense printer and tape tied to sim events
            if (sim_interval <= 0) sim_process_event();
            sim_interval -= 1;      
            for(i=1;i<=2;i++) if (sim_is_active(&lp_unit[i])) {bIOInProgress=1; IOInProgress |= 1; } // printer animation in progress
            for(i=1;i<=8;i++) if (sim_is_active(&mt_unit[i])) {bIOInProgress=1; IOInProgress |= 2; } // tape animation in progress
            if (bTapeAnimInProgress) {bIOInProgress=1; IOInProgress |= 2;    } // tape animation in progress
            // control cpu speed, slowdown and yield time to OS if needed (with sim_os_ms_sleep)
            GlobalTicksCount++;
            Control_CpuSpeed(bFastMode);
            // determine if should return control to scp
            if (sim_os_msec() - tnow > 2000) break; // sanity check: max 2000 msec (2 sec wall time clock) waiting for animation to end
            if (bIOInProgress) continue;                           // some animation in progress, do not exit yet
            if (RefreshCount0 == 0) {                              // draw 2 frames after animation ends to allow
               RefreshCount0 = Refresh_Frames_Count;                  // for light to terminate the tickcount based partial 
               if (bShowInfo) bShowInfo=2;                         // assure show 0 ips if info has been enabled with ^I
               continue;                                           // illumination
            }
            if (Refresh_Frames_Count - RefreshCount0 < 2) continue; 
            break;
        }
        for(i=1;i<=8;i++) {
            // if any tape not ready because rew sim_activate not finished make the tape ready anyway
            // and cancel pending sim_activate
            if ((mt_ready(i)==0) && (mt_get_last_cmd(i)==OP_REWIND)) {
                mt_set_ready(i); 
                IOInProgress |= 2; 
                sim_cancel(&mt_unit[i]);
            }
        }
        if (bShowInfo>1) bShowInfo=1; // restore
        buf[0]=0;
        if (IOInProgress & 1) strcat(buf, "Printer/");
        if (IOInProgress & 2) strcat(buf, "TAPE/");
        if (buf[0]==0) { strcat(buf, "NO"); } else {buf[strlen(buf)-1]=0; }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "End Wait for %s animation (%d msec)\n",
            buf, sim_os_msec() - tnow);
         Measure_CpuSpeed(0);                  // reset cpu speed measurement because end of run
         sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: reset cpu speed measurement because end of run\n");
    }
    #endif
    {
        t_int64 elapsed;
        int TPS, IPS;
        char sBuf[80];

        Measure_run_CpuSpeed(&elapsed, &TPS, &IPS, sBuf, 1);
        if (elapsed > 0) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Ticks per second\n", TPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: Clock at %0.2f MHz\n", TPS / 1000000.0);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Speed is x%0.1f relative to original hardware\n", TPS / 1000000.0);  // real hw speed: 1M ticks per second, as clock is set to 1 MHz
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Instructions executed: %.0f\n", 1.0 * CpuSpeed.InstrCount);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Instructions per second (IPS) achieved\n", IPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Run elapsed %s\n", sBuf);
        }
    }

    // flush online printers 
    {
        int i; 
        for (i=0;i<2; i++) {
            if ((lp_unit[i].flags & UNIT_ATT) && (lp_unit[i].fileref)) {
                fflush(lp_unit[i].fileref); 
            }
        }
    }
    StopReason = reason; // save stop reason to be displayed in control panel

    /* Simulation halted */
    return reason;
}


/* Reset routine */
t_stat cpu_reset(DEVICE * dptr)
{
    vm_init ();

    IC = V;
    
    // reset does not clear the memory nor V register

    sim_brk_types = sim_brk_dflt = SWMASK('E');

    U = M4 = M6 = M8 = 0;
    IREG = REG1 = REG2 = 0; 
    Console_CurrentSubOp = NSUBOP = ESUBOP = 0;
    TU = 0;
    
    ClearInterlocks();
    
    cpu_reset_indicator_lights();

    return SCPE_OK;
}

// clears Indicator Lights
void cpu_reset_indicator_lights(void)
{
    Console_Li64=Console_Li74=0;  // reset console lights to signal the instrction executed
    Console_LiOV=Console_LiIA=Console_LiZR=0;
    Console_LiTEOF=Console_LiTCHK=0; // console light for condition check flag
    Console_LiPrtChk=Console_LiAddrChk=0;
    Console_LiIndexChk=Console_LiSignChk=Console_LiZeroDivChk=Console_LiTEOT=0;
    if (sim_is_running==0) {
        OV=IA=ZR=TEOF=TCHK=0; 
        Console_PRT_Instr=0;
    }
}

// clears CRT memory, symbolic mem, set V=0, reset SubOp counter
void crt_reset(void)
{
    memset(CRT, 0, sizeof(CRT)); 
    memset(CRT_Symbolic_Buffer, 0, sizeof(CRT_Symbolic_Buffer));            // clear symbolic info
    CRT_BUS=0;
    V = 0; 
    // also resets subop executed displayed into indicator panel 
    Console_CurrentSubOp = NSUBOP = ESUBOP = 0;
}

t_stat cpu_clear_crt_mem(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    crt_reset();
    return SCPE_OK;
}

/* Memory examine */

t_stat cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw)
{
    if (((int) addr >= MemSize()) || (addr < 0)) return SCPE_NXM;
    if (vptr != NULL) {
        *vptr = (t_value) (CRT_BUS=CRT[addr]);
    }

    return SCPE_OK;
}

/* Memory deposit */

t_stat cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw)
{
    if (((int) addr >= MemSize()) || (addr < 0)) return SCPE_NXM;
    CRT[addr]=CRT_BUS=val;
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
    int c, MachineCycle_usec;
    CONST char *tptr;

    if (cptr == NULL) return SCPE_ARG;
    
    if ((*cptr == 'M') || (*cptr == 'm')) {
        // SET CPU SPEED=MAX -> max cpu speed
        CpuSpeed_Acceleration=0;
        CpuSpeed.TicksMax=0;
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
    // machine cycle: 1 microseconds 
    MachineCycle_usec = 1; 
    CpuSpeed.TicksMax  = (int) (CpuSpeed.msec * CpuSpeed_Acceleration * 1000 / (100.0 * MachineCycle_usec));
    // Clock set to 1MHz (ticks per sec)
    CpuSpeed.TicksObjectivePerSec = (int) (1000000.0 * CpuSpeed_Acceleration / (MachineCycle_usec * 100.0));
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
        fprintf (st, "Speed set to %0.2f (Clock at %0.2f MHz), relative to original hardware\n", 
            (float) CpuSpeed_Acceleration / 100,
            (float) (1.0 * CpuSpeed_Acceleration / 100.0));
    }

    Measure_run_CpuSpeed(&elapsed, &TPS, &IPS, sBuf, 0);
    if (elapsed == 0) {
        fprintf (st, "Measured speed: no data\n");
    } else {
        fprintf (st, "Measured speed: Clock at %0.2f MHz\n", TPS / 1000000.0);
        fprintf (st, "                Speed is x%0.1f relative to original hardware\n", TPS / 1000000.0);  // real hw speed: 1M ticks per second, as clock is set to 1 MHz
        fprintf (st, "                %d Instructions per second (IPS) achieved\n", IPS);
        fprintf (st, "                run elapsed %s\n", sBuf);
    }
    return SCPE_OK; 
}


const char * cpu_description (DEVICE *dptr) { 
    return "IBM Naval Ordnance Research Calculator (NORC) CPU";     
}   
 
// en NORC
// poner en manual nombre de instr correctas
// nuevo sw: compiler (que aplique a los prog hechos en crossasembler)



    
   
