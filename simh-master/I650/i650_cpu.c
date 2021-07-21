/* i650_cpu.c: IBM 650 CPU simulator

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

   cpu          IBM 650 central processor

   From Wikipedia: The IBM 650 Magnetic Drum Data-Processing Machine is one of
   IBM's early computers, and the world’s first mass-produced computer. It was
   announced in 1953 and in 1956 enhanced as the IBM 650 RAMAC with the 
   addition of up to four disk storage units. Almost 2,000 systems were 
   produced, the last in 1962.

   The 650 was a two-address, bi-quinary coded decimal computer (both data and
   addresses were decimal), with memory on a rotating magnetic drum. Character 
   support was provided by the input/output units converting punched card 
   alphabetical and special character encodings to/from a two-digit decimal 
   code.

   Rotating drum memory provided 1,000, 2,000, or 4,000 words of memory (a 
   signed 10-digit number or five characters per word) at addresses 0000 to 
   0999, 1999, or 3999 respectively.

   Instructions read from the drum went to a program register (in current 
   terminology, an instruction register). Data read from the drum went through 
   a 10-digit distributor. The 650 had a 20-digit accumulator, divided into 
   10-digit lower and upper accumulators with a common sign. Arithmetic was 
   performed by a one-digit adder. The console (10 digit switches, one sign 
   switch, and 10 bi-quinary display lights), distributor, lower and upper 
   accumulators were all addressable; 8000, 8001, 8002, 8003 respectively.

   The 650 instructions consisted of a two-digit operation code, a four-digit 
   data address and the four-digit address of the next instruction. The sign 
   was ignored on the basic machine, but was used on machines with optional 
   features. The base machine had 44 operation codes. Additional operation 
   codes were provided for options, such as floating point, core storage, 
   index registers and additional I/O devices. With all options installed, 
   there were 97 operation codes.

   known to exist but not found yet:

   IBM FORTRAN Compiler
   ONMICODE - First languaje designed to be portes (IBM 702, IBM 650)
   IPL-V - First processing list languaje
   Info on negative opcodes
   SCHAPPERT , H. , Alfo language for IBM 650 - Algol Implementation (1960)
   SOAP I (1957)


   The programmer visible system state for the IBM 650 is:

   CSW <10:1>            Console Display Switches 
   ACC[0] <10:1>         Lower Accumulator register
   ACC[1] <10:1>         Upper Accumulator register
   DIST <10:1>           Distributor
   OV<0:0>               Overflow flag

   The 650 had one basic instuction format.
   Intructions are stores as 10 digits (0-9) words in drum memory

   10 9 | 8 7 6 5 | 4 3 2 1 |  0
   -----+---------+---------+-----
   op   |   Data  |  Instr  | Sign
   code |   Addr  |  Addr

   First two digits are opcodes
   digits 8-5 is data address referenced by opcode
   digits 4-1 is instruction address: address of next instruction

   Instruction support as described in BitSavers 22-6060-2_650_OperMan.pdf

   IBM 653 Storage Unit can be enabled as an option. This simulates the following 
     - Immediate Access Storage (IAS)
     - Index registers 
     - Floating Point support
     - Synchronizers 2 & 3

   Memory Map

   0000-1999 Drum Locations (0000-3999 on Model4)
   2000-3999 Location indexed with IRA
   4000-5999 Location indexed with IRB
   6000-7999 Location indexed with IRC
   8000      Console Switch Register
   8001      Distributor Register
   8002      Lower Accumulator Register
   8003      Upper Accumulator Register
   8005      Index Register A (IRA)
   8006      Index Register B (IRB)
   8007      Index Register C (IRC)
   9000-9059 IAS Storage
   9200-9259 Location indexed with IRA
   9400-9459 Location indexed with IRB
   9600-9659 Location indexed with IRC


*/

#include "i650_defs.h"
#if defined(CPANEL)
#include "cpanel.h"
int cpanel_gui_supported = 1;     
#else
int cpanel_gui_supported = 0;     // so disp cpanelgui returns 0 on scp
#endif



#define UNIT_V_MSIZE    (UNIT_V_UF + 0)
#define UNIT_MSIZE      (7 << UNIT_V_MSIZE)
#define UNIT_V_CPUMODEL (UNIT_V_UF + 4)
#define UNIT_MODEL      (0x01 << UNIT_V_CPUMODEL)
#define CPU_MODEL       ((cpu_unit.flags >> UNIT_V_CPUMODEL) & 0x01)
#define MODEL(x)        (x << UNIT_V_CPUMODEL)
#define MEMAMOUNT(x)    (x << UNIT_V_MSIZE)
#define OPTION_STOR     (1 << (UNIT_V_CPUMODEL + 1))
#define OPTION_CNTRL    (1 << (UNIT_V_CPUMODEL + 2))
#define OPTION_SOAPMNE  (1 << (UNIT_V_CPUMODEL + 3))
#define OPTION_FAST     (1 << (UNIT_V_CPUMODEL + 4))
#define OPTION_TLE      (1 << (UNIT_V_CPUMODEL + 5))
#define OPTION_1DSKARM  (1 << (UNIT_V_CPUMODEL + 6))

t_stat              cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw);
t_stat              cpu_reset(DEVICE * dptr);
t_stat              cpu_set_size(UNIT * uptr, int32 val, CONST char *cptr, void *desc);
t_stat              cpu_set_speed(UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat              cpu_show_speed(FILE *st, UNIT *uptr, int32 value, CONST void *desc);
t_stat              cpu_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr);
const char          *cpu_description (DEVICE *dptr);

// DRUM Memory
t_int64             DRUM[MAXDRUMSIZE]                        = {0};
int                 DRUM_NegativeZeroFlag[MAXDRUMSIZE]       = {0};
char                DRUM_Symbolic_Buffer[MAXDRUMSIZE * 80]   = {0}; // does not exists on real hw. Used to keep symbolic info 
char                IAS_Symbolic_Buffer[60 * 80]             = {0}; // does not exists on real hw. Used to keep symbolic info 

// IO Synchronizer for card read-punch buffer
t_int64             IOSync[10]                               = {0};
int                 IOSync_NegativeZeroFlag[10]              = {0};

// IAS Memory
t_int64             IAS[60]                                  = {0};
int                 IAS_NegativeZeroFlag[60]                 = {0};
int                 IAS_TimingRing                           = 0;

// interlock counters
int InterLockCount[8]                                        = {0};                   

// address where rotating drum is currently positioned (0-49)
int DrumAddr;                                   

// increment umber of word counts elapsed from starting of simulator -> this is the global time measurement
t_int64 GlobalWordTimeCount=1;

// struct for SET CPU SPEED
int CpuSpeed_Acceleration = 0;   // parameter: cpu speed multiplier: 
                                 // 100->original harware speed, 50->half speed, 200->speed x2, 0=max speed
struct {
    // speed control variables
    uint32 msec;                  // constant: calculate WordTimeMax speeed cpu for Word times that can be executed on 50 msec
    int WordTimeMax;              // calculated: number of wordtimes that can be executed during msec at current speed setting
    int WordTimeCount;            // count of word times already executed 
    int WordTimeObjectivePerSec;  // calculated: value of WordTimeCountPerSec to achieve
    // speed measurement for scp/debug
    uint32 start_tm0;             // sim_os_msec() when run starts
    uint32 end_tm0;               // sim_os_msec() when run ends
    t_int64 InstrCount;           // full intructions executed count (=0 on start of run)
    t_int64 GlobalWordTimeCount0; // count of word times already executed when run starts
    // measurement for speed control
    struct {
        t_int64 WordTime0;        // num word times already executed when starting speed measurement
        uint32 tm0;               // sim_os_msec() when starting speed measurement
        int WordTimeCountPerSec;  // speed measurement result: mean word times executed per second
    } measurement;
} CpuSpeed = {15};                // adjust cpu speed each 15 msec


// cpu registers
uint16              IC;                          // Added register not part of cpu. Has addr of current intr in execution, just for displaying purposes. IBM 650 has no program counter
uint16              PROP;                        // Added register not part of cpu. Has operation code of current intr in execution, just for scp scripting purposes. Contains the two higher digits of PR register
t_int64             ACC[2];                      /* lower, upper accumulator. 10 digits (=one word) each*/
t_int64             DIST;                        /* ditributor. 10 digits */
t_int64             PR;                          /* Program Register: hold current instr in execution, 10 digits*/
uint16              AR;                          /* Address Register: address references to drum */
int16               IR[3];                       // Index registers. Are 4 digits as AR register, but signed
uint8               OV;                          /* Overflow flag */
int AccNegativeZeroFlag  = 0;                    // set to 1 if acc has a negative zero
int DistNegativeZeroFlag = 0;                    // set to 1 if distributor has a negative zero
char                IMachineCycle   = 'I';       // indicates current machine cycle to be executed: 'D' for D-Cycle, 'I' for I-Cycle
t_int64             CSW = 0;                     /* Console Switches, 10 digits */
int                 CSWProgStop     = 1;         // Console programmed stop switch. =1 -> stop on 01 Operation
int                 CSWOverflowStop = 0;         // Console stop on overflow switch- =1 -> stop on overflow set 
int                 CSWHalfCycle    = 0;         // Console run/half cycle set to 0 for normal run, =1 to execute 1 half-cycle and stop
int                 CSWAddrStop     = -1;        // Console Switch with address stop selected (-1 if none)

uint8               StopReason      = 0;         // saved stop reason to be displayed in control panel
uint8               StopIOError     = 0;         // flag to signal device that caused STOP_IO error: 1=CDR/CDP, 2=TAPE, 3=RAMAC Address, 4=RAMAC IO

/* CPU data structures

   cpu_dev      CPU device descriptor
   cpu_unit     CPU unit descriptor
   cpu_reg      CPU register list
   cpu_mod      CPU modifiers list
*/

UNIT cpu_unit =
    { UDATA(NULL, MEMAMOUNT(0)|MODEL(0x0), 0), 10  };

 
REG cpu_reg[] = {
    {DRDATAD(IC, IC, 16, "Current Instruction"), REG_FIT|REG_RO},
    {DRDATAD(PROP, PROP, 16, "Program Register Operation Code"), REG_FIT|REG_RO},
    {HRDATAD(DIST, DIST, 64, "Distributor"), REG_VMIO|REG_FIT},
    {HRDATAD(ACCLO, ACC[0], 64, "Lower Accumulator"), REG_VMIO|REG_FIT},
    {HRDATAD(ACCUP, ACC[1], 64, "Upper Accumulator"), REG_VMIO|REG_FIT},
    {HRDATAD(PR, PR, 64, "Program Register"), REG_VMIO|REG_FIT},
    {DRDATAD(AR, AR, 16, "Address Register"), REG_FIT},
    {ORDATAD(OV, OV, 1, "Overflow"), REG_FIT},
    {HRDATAD(CSW, CSW, 64, "Console Switches"), REG_VMIO|REG_FIT},
    {ORDATAD(CSWPS, CSWProgStop, 1, "Console Switch Program Stop"), REG_FIT},
    {ORDATAD(CSWOS, CSWOverflowStop, 1, "Console Switch Overflow Stop"), REG_FIT},
    {ORDATAD(HALF, CSWHalfCycle, 1, "Console Switch Half Cycle"), REG_FIT}, 
    {ORDATAD(CPANELGUI, cpanel_gui_supported, 1, "Control Panel GUI supported flag"), REG_FIT|REG_RO},
    {NULL} 
};

MTAB cpu_mod[] = {
    {UNIT_MSIZE,     MEMAMOUNT(0),    "1K", "1K", &cpu_set_size},
    {UNIT_MSIZE,     MEMAMOUNT(1),    "2K", "2K", &cpu_set_size},
    {UNIT_MSIZE,     MEMAMOUNT(2),    "4K", "4K", &cpu_set_size},
    {OPTION_STOR,    0,               NULL,                    "NOSTORAGEUNIT", NULL},
    {OPTION_STOR,    OPTION_STOR,     "Storage Unit",          "STORAGEUNIT",   NULL},
    {OPTION_CNTRL,   0,               NULL,                    "NOCNTRLUNIT",   NULL},   
    {OPTION_CNTRL,   OPTION_CNTRL,    "Control Unit",          "CNTRLUNIT",     NULL},
    {OPTION_SOAPMNE, 0,               NULL,                    "DEFAULTMNE",    NULL},
    {OPTION_SOAPMNE, OPTION_SOAPMNE,  "Using SOAP Mnemonics",  "SOAPMNE",       NULL},
    {OPTION_FAST,    0,               NULL,                    "REALTIME",      NULL},
    {OPTION_FAST,    OPTION_FAST,     "Fast Execution",        "FAST",          NULL},
    {OPTION_TLE,     0,               NULL,                    "NOTLE",         NULL},
    {OPTION_TLE,     OPTION_TLE,      "Table Lookup on Equal", "TLE",           NULL},
    {OPTION_1DSKARM, 0,               NULL,                    "NOTLE",         NULL},
    {OPTION_1DSKARM, OPTION_1DSKARM,  "Enable 1 ARM RAMAC",    "1DSKARM",       NULL},
    {OPTION_1DSKARM, 0,               "Enable all ARM RAMAC",  "3DSKARM",       NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,   "SPEED",                 "SPEED",         &cpu_set_speed, &cpu_show_speed},
    {0}
};

DEVICE cpu_dev = {
    "CPU", &cpu_unit, cpu_reg, cpu_mod,
    1, 10, 16, 1, 10, 64,
    &cpu_ex, &cpu_dep, &cpu_reset, NULL, NULL, NULL,
    NULL, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cpu_help, NULL, NULL, &cpu_description
};

// convert time in msec to number of word times taking into account cpu acceleleration
// 100 msec -> 1041 word times
// 100 msec -> 502 word time if acceleration is 50%
int msec_to_wordtime(int msec)
{
    if ((CpuSpeed_Acceleration > 0) && (CpuSpeed_Acceleration < 100)) {
        // if cpu speed slower than real harware, scale down : less wordtimes per milisecond
        return (int) ((msec / 0.096) * (CpuSpeed_Acceleration / 100.0)); 
    } else {
        // original hw speed of faster 
        return (int) (msec / 0.096); 
    }
}


// return 0 if addr invalid, 1 if addr valid depending on allowed addrs given by ValidDA
// set the ias TimingRing to AR is IAS is accessed
int IsDrumAddrOk(int AR, int ValidDA) 
{
    // check if AR should be 9000 
    if ((STOR) && (ValidDA & vda_9000)) 
        return (AR == 9000) ? 1:0;
    // Drum address
    if ((AR >= 0) && (AR < DRUMSIZE)) 
        return (ValidDA & vda_D) ? 1:0; 
    // cpu registers: acc (lo&hi), distibutor, console switch reg: ok to check for Addr validity, ok to read, cannot write to it
    if ((AR >= 8000) && (AR <= 8003)) 
        return (ValidDA & vda_A) ? 1:0; 
    // index registers (ir) present if Storage Unit is enabled: ok to check for Addr validity, ok to read, cannot write to it
    if ((STOR) && (AR >= 8005) && (AR <= 8007))
        return (ValidDA & vda_I) ? 1:0; 
    // tape address present is tapes are enabled: ok to check for Addr validity, cannot read/write to it
    if ((CNTRL) && (AR >= 8010) && (AR <= 8015))
        return (ValidDA & vda_T) ? 1:0; 
    // inmediate access storage (ias) if Storage Unit is enabled: ok to check for Addr validity, ok to read/write
    if ((STOR) && (AR >= 9000) && (AR <= 9059)) {   
        if (ValidDA & vda_S) {
            IAS_TimingRing = AR - 9000;                 // set Timing ring on address accesed
            return 1;
        }
    }
    // none of the above -> invalid address or address/mode combination
    return 0;
}

// return 0 if write addr invalid
int WriteAddr(int AR, t_int64 d, int NegZero)
{
    if (d) NegZero = 0; // sanity check on Minus Zero
    if ((STOR) && (AR >= 9000) && (AR <= 9059)) {   // IAS is available at addr 9000-9059
        IAS_TimingRing = AR - 9000;                 // not necessary as before any call to WriteAddr IsAddrOk is invoked. But ... just in case
        IAS[IAS_TimingRing] = d;
        IAS_NegativeZeroFlag[IAS_TimingRing] = NegZero;
        return 1;
    } else if ((AR >= 0) && (AR < DRUMSIZE) && (AR < MAXDRUMSIZE)) {
        if (d) NegZero = 0; // sanity check on Minus Zero
        DRUM[AR] = d;
        DRUM_NegativeZeroFlag[AR] = NegZero;
        return 1;
    }
    // none of the above -> invalid address or address/mode combination
    return 0;
}

// return 0 if read addr invalid
int ReadAddr(int AR, t_int64 * d, int * NegZero)
{
    int neg;

    // read from drum?
    if ((AR >= 0) && (AR < DRUMSIZE)) {
        *d  = DRUM[AR]; 
        neg = DRUM_NegativeZeroFlag[AR];
        if (*d) DRUM_NegativeZeroFlag[AR] = 0;
    } else
    // read from cpu registers?
    if (AR == 8000) {*d = CSW;    neg=0;                    } else 
    if (AR == 8001) {*d = DIST;   neg=DistNegativeZeroFlag; } else
    if (AR == 8002) {*d = ACC[0]; neg=AccNegativeZeroFlag;  } else
    if (AR == 8003) {*d = ACC[1]; neg=AccNegativeZeroFlag;  } else 
    // read index registers (ir) ? 
    if ((STOR) && (AR == 8005)) {*d = IR[0]; neg=0;         } else
    if ((STOR) && (AR == 8006)) {*d = IR[1]; neg=0;         } else
    if ((STOR) && (AR == 8007)) {*d = IR[2]; neg=0;         } else
    // tape address ? 
    if ((CNTRL) && (AR >= 8010) && (AR <= 8015)) {   
        // cannot read/write to tape addresses
        return 0;
    } else 
    // read inmediate access storage (ias)?
    if ((STOR) && (AR >= 9000) && (AR <= 9059)) {   
        IAS_TimingRing = AR - 9000; 
        *d  = IAS[IAS_TimingRing];
        neg = IAS_NegativeZeroFlag[IAS_TimingRing];
        if (*d) IAS_NegativeZeroFlag[IAS_TimingRing] = 0;
    } else {
        // none of the above -> invalid address for read
        return 0;
    }
    if (*d) neg = 0; // sanity check on Minus Zero
    if (NegZero != NULL) *NegZero = neg;
    return 1;
}

// shift acc 1 digit. If direction > 0 to the left, if direction < 0 to the right. 
// Return digit going out of acc (with sign)
int ShiftAcc(int direction)
{
    t_int64 a0, a1;
    int neg = 0;
    int n, m;

    n = 0;
    a1 = ACC[1]; if (a1 < 0) {a1 = -a1; neg = 1;}
    a0 = ACC[0]; if (a0 < 0) {a0 = -a0; neg = 1;}
    if ((AccNegativeZeroFlag) && (ACC[0] == 0) && (ACC[1] == 0)) neg = 1;

    if (direction > 0) {                          // shift left
        n = Shift_Digits(&a1, 1);                 // n = Upper Acc high digit shifted out on the left
        m = Shift_Digits(&a0, 1);                 // m = intermediate digit that goes from one acc to the other
        a1 = a1 + (t_int64) m;
    } else if (direction < 0) {                   // shift right
        m = Shift_Digits(&a1, -1);                // m = intermediate digit that goes from one acc to the other
        n = Shift_Digits(&a0, -1);                // n = Lower Acc units digit shifted out on the right
        a0 = a0 + (t_int64) m * (1000000000L);     
    }
    if (neg) {a1=-a1; a0=-a0; n=-n;}

    ACC[0] = a0; 
    ACC[1] = a1; 
    if ((neg == 1) && (a0 == 0) && (a1 == 0)) AccNegativeZeroFlag = 1; 
    return n;
}


// float value format = mmmmmmmcc = 0.m x 10^(c-50)
//                      mmmmmmm = mantissa
//                      cc      = modified characteristic (== exponent)
// get modified characteristic of float d 
int GetExp(t_int64 d)
{
    return (AbsWord(d) % 100);
}

// set modified characteristic of float d 
t_int64 SetExp(t_int64 d, int exp)
{
    int neg = 0;

    if (d < 0) {neg=1; d=-d;}
    d = ((d / 100) * 100) + (exp % 100);
    if (neg) d=-d;
    return d;
}

// set result into ACC[1] and ACC[0] for float mult and division
// get a 10 digits mantissa en ACC[1] 
// round to the 8th digit
// add the modified characteristic (exp)
// add sign, check for zero
void MantissaRoundAndNormalizeToFloat(int * CpuStepsUsed, int neg, int exp)
{
    // if high order digit of mantissa is zero, shift left once
    if (Get_HiDigit(ACC[1]) == 0) {       
        ShiftAcc(1);
        *CpuStepsUsed = *CpuStepsUsed + 2;
        if (exp == 0) {
            OV = 1; 
        } else {
            exp--;
        }
    }
    // round mantissa in ACC[1] to the 8th digit
    if (GetExp(ACC[1]) >= 50) {
        ACC[1] = ACC[1] + 100;
        if (ACC[1] >= D10) {
            ACC[1] = ACC[1] / 10;
            *CpuStepsUsed = *CpuStepsUsed + 2;
            if (exp == 99) {
                OV = 1; 
            } else {
                exp++;
            }
        }
    }
    ACC[1] = SetExp(ACC[1], 0); 
    // normalize mantissas
    while (( ACC[1] != 0) && (Get_HiDigit(ACC[1]) == 0)) {       
        if (exp == 0) {                       
            OV = 1; 
            break;                  // if zero, underflow
        } else {
            exp--;
        }
        ACC[1] = ACC[1] * 10;
        *CpuStepsUsed = *CpuStepsUsed + 2;
    }
    // set result
    if (exp <  0) {exp += 100; OV = 1;}
    if (exp > 99) {exp -= 100; OV = 1;}
    ACC[1] = neg * SetExp(ACC[1], exp);
    ACC[0] = 0;
    if ((ACC[1] / 100) == 0) ACC[1] = 0; // if mantissa is zero, all is zero
    AccNegativeZeroFlag = 0;
}


// add float to accumulator, set Overflow
// return number of steps used
int AddFloatToAcc(int bSubstractFlag, int bAbsFlag, int bNormalizeFlag)
{
    int nSteps;
    int n, neg;
    t_int64 d;

    AccNegativeZeroFlag = 0; 
    nSteps = 0;

    n = GetExp(ACC[1]) - GetExp(DIST);
    if (n == 0) {
        // no decimal aligning necessary. Mantissas ready to being added
    } else if ( n > 8) {
        DIST = ACC[1]; ACC[1] = 0;
    } else if ( n < -8) {
        ACC[1] = 0;
    } else {
        if (n < 0) {                            // if between -1 and -8
            n = -n;                             // just remove sign on number of shifts to be done 
        } else {                                // if between 1 and 8
            d = ACC[1]; 
            ACC[1] = DIST; DIST = d;            // exchange distrib and upper acc
            nSteps += 2;
        }
        ACC[1] = SetExp(ACC[1], 0);             // modified characteristic of upper set to zero
        while (n>0) {                           // shift right n digits
            ShiftAcc(-1);
            nSteps += 2;
            n--;
        }
        if (GetExp(ACC[1]) >= 50) {             // should round?
            ACC[1] = ACC[1] + ((ACC[1] >= 0) ? 100:-100);
        }
    }
    d = DIST;
    if (bAbsFlag) if (d < 0) d = -d;
    if (bSubstractFlag) d = -d;

    if (((ACC[1] > 0) && (d < 0)) || ((ACC[1] < 0) && (d > 0))) nSteps += 4; 

    ACC[1] = (ACC[1] / 100) + (d / 100);        // add/substract mantissas (positions 10-3) 
    n = GetExp(DIST);                           // get  modified characteristic from dist
    if (ACC[1] < 0) {
        ACC[1] = -ACC[1]; neg=-1;
    } else {
        neg=1;
    }

    if (ACC[1] >= D8) {                         // overflow?
        if ((ACC[1] % 10) >= 5) {               // should round?
            ACC[1] = ACC[1] / 10 + 1;           // yes, shift right, keep extra 1 in high pos, add rounding
            nSteps += 4; 
        } else {
            ACC[1] = ACC[1] / 10;               // no, just shift
        }
        n++;                                    // add 1 to dist modified characteristic 
        if (n > 99) {OV = 1; n=0;}              // overflow. Set modified characteristic to zero
        nSteps += 4;
    }

    if (ACC[1] == 0) {
        n = 0;                                  // if mantissa is zero, mod. char is set to zero also
        bNormalizeFlag = 0;                     // must not normalize
        nSteps += 2;
    }
    ACC[1] = SetExp(neg * ACC[1] * 100, n);     // insert modified characteristic of dist into upper acc       
    ACC[0] = 0;                                 // lower acc set to zero
    if (bNormalizeFlag) {
        while(Get_HiDigit(ACC[1]) == 0) {       // while hi digit (digit 10) is zero -> normalize 
            n = GetExp(ACC[1]);                 // get modified characteristic
            if (n == 0) {                       
                OV = 1; break;                  // if zero, underflow
            }
            n--;
            ACC[1] = SetExp(ACC[1]/100 * 1000, n);  // left shift mantissa, set modified characteristic  
            nSteps += 3;
        }
    }
    return nSteps;
}

int bAccNegComplement; // flag to signals acc has complemented a negative ass (== sign adjust) 
                       // needed to compute execution cycles taken by the intruction

// add to accumulator, set Overflow
void AddToAcc(t_int64 a1, t_int64 a0, int bSetOverflow) 
{
    AccNegativeZeroFlag = 0; 
    bAccNegComplement = 0;
    
    ACC[0] += a0;
    ACC[1] += a1;

    // adjust carry from Lower ACC to Upper Acc
    if (ACC[0] >=  D10) { ACC[0] -= D10; ACC[1]++; }
    if (ACC[0] <= -D10) { ACC[0] += D10; ACC[1]--; }

    // ajust sign
    if ((ACC[0] > 0) && (ACC[1] < 0)) {
        ACC[0] -= D10; ACC[1]++; 
        bAccNegComplement = 1;
    }
    if ((ACC[0] < 0) && (ACC[1] > 0)) {
        ACC[0] += D10; ACC[1]--; 
        bAccNegComplement = 1;
    }

    // check overflow
    if (bSetOverflow) {
        if ((ACC[1] >= D10) || (ACC[1] <= -D10)) { 
            ACC[1] = ACC[1] % D10; 
            OV=1; 
        }
    }
}


t_int64 SetDA(t_int64 d, int DA) 
{
    int neg = 0;

    int op, nn, IA;

    if (DA < 0) DA=-DA;
    if (d < 0) {d=-d; neg=1;}

    // extract parts of word
    op = Shift_Digits(&d, 2);          
    nn = Shift_Digits(&d, 4);          // discard current DA
    IA = Shift_Digits(&d, 4);         
    // rebuild word with new DA
    d = (t_int64) op * D8 + 
        (t_int64) DA * D4 + 
        (t_int64) IA;
    if (neg) d=-d;
    return d;
}

// set last 4 digits in d with IA contents
t_int64 SetIA(t_int64 d, int IA) 
{
    int neg = 0;
    
    if (IA < 0) IA=-IA;
    if (d < 0) {d=-d; neg=1;}
    d = d - ( d % D4);
    d = d + (IA % D4);
    if (neg) d=-d;
    return d;
}

// set last 2 digits in d with IA contents
t_int64 SetIA2(t_int64 d, int n) 
{
    int neg = 0;
    
    if (n < 0) n=-n;
    if (d < 0) {d=-d; neg=1;}
    d = d - ( d % 100);
    d = d + ( n % 100);
    if (neg) d=-d;
    return d;
}

// normalize to 4 digits, 10 complements
void NormalizeAddr(int * addr, int bAllowNegativeValue)
{
    while (*addr >= 10000) *addr -= 10000; 
       if (bAllowNegativeValue) {
          while (*addr <= -10000) *addr += 10000; 
       } else {
          while (*addr < 0) *addr += 10000; 
       }
}

// apply index register to a tagged address
// removes tag, replace value with developed address
// return 1 if address was tagged, and has been replaced by developed addr
int ApplyIndexRegister(int * addr)
{
    int n = 0;

    // check for tag and untag
    if ((*addr >= 2000) && (*addr < 4000)) {n = 1; *addr -= 2000; } else
    if ((*addr >= 4000) && (*addr < 6000)) {n = 2; *addr -= 4000; } else
    if ((*addr >= 6000) && (*addr < 8000)) {n = 3; *addr -= 6000; } else
    if ((*addr >= 9200) && (*addr < 9400)) {n = 1; *addr -= 200; } else
    if ((*addr >= 9400) && (*addr < 9600)) {n = 2; *addr -= 400; } else
    if ((*addr >= 9600) && (*addr < 9800)) {n = 3; *addr -= 600; } else
    return 0;   // address not tagged

    *addr = *addr + IR[n-1];
    NormalizeAddr(addr, 0);

    return 1;
}

// apply index register to a tagged address for Model 4
// removes tag, replace value with developed address
// return 1 if address was tagged, and has been replaced by developed addr
int ApplyIndexRegisterModel4(int * DA, int * IA)
{
    int n, tagDA, tagIA, nIndexApplied;
    
    tagDA = tagIA = 0;

    nIndexApplied = 0;
    if ((*DA >= 9200) && (*DA < 9800)) {
        nIndexApplied += ApplyIndexRegister(DA);
        if ((*IA >= 9200) && (*IA < 9800)) {
            nIndexApplied += ApplyIndexRegister(IA);
        }
        return nIndexApplied;
    } 
    if ((*DA >= 4000) && (*DA < 8000)) {
        *DA -= 4000; // remove tag on DA address
        tagIA = 1;
    }
    if ((*IA >= 4000) && (*IA < 8000)) {
        *IA -= 4000; // remove tag on IA address
        tagDA = 1;
    } else if (  ((*IA >= 8800) && (*IA < 8900)) || ((*IA >= 9800) && (*IA < 9900))  ) {
        *IA -= 800; // remove tag on IA address
        tagDA = 1;
    }

    n = tagDA + 2 * tagIA;
    if (n) {
        *DA = *DA + IR[n-1];
        NormalizeAddr(DA, 0);
        nIndexApplied++;
    }
    if ((*IA >= 9200) && (*IA < 9800)) {
        nIndexApplied += ApplyIndexRegister(IA);
    }

    return nIndexApplied;
}

// opcode decode 
// input: prior to call DecodeOpcode PR cpu register must be loaded with the word to decode 
// output: decoded instruction as opcode, DA, IA parts
//         returns opname: points to opcode name or NULL if undef opcode
CONST char * DecodeOpcode(t_int64 d, int * opcode, int * DA, int * IA)
{
    CONST char * opname;
    int opt;

    *opcode = Shift_Digits(&d, 2);          // current inste opcode
    *DA     = Shift_Digits(&d, 4);          // addr of data used by current instr
    *IA     = Shift_Digits(&d, 4);          // addr of next instr

    opname  = (cpu_unit.flags & OPTION_SOAPMNE) ? base_ops[*opcode].name2 : base_ops[*opcode].name1;
    opt     = base_ops[*opcode].option;     // cpu option needed to have the opcode available
    if (opt == opStorUnit) {
        // opcode available if IBM 653 Storage Unit is present
        if (STOR == 0) return NULL;
    } else if (opt == opCntrlUnit) {
        // opcode available if IBM 652 Control Unit is present
        if (CNTRL == 0) return NULL;
    } else if (opt == opTLE) {
        // opcode available if Table LookUo Feature is present
        if ((cpu_unit.flags & OPTION_TLE) == 0) return NULL;
    } 
    return opname;
}

// transfer (copy words) between IAS and DRUM
// dir = "D->I" or "I->D"
// bEOB = 1 -> End of IAS band terminated transfer
// return number of words transfered
int TransferIAS(CONST char * dir, int bEOB)
{
    int n, f0, t0, f1, t1, ec, ZeroNeg;
    t_int64 d;
    char s[6];

    n = f0 = t0 = f1 = t1 = ec = 0;
    while (1) {
        if (dir[0] == 'D') { 
            // copy drum to ias
            d = IAS[IAS_TimingRing] = DRUM[AR];
            ZeroNeg = IAS_NegativeZeroFlag[IAS_TimingRing] = DRUM_NegativeZeroFlag[AR];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... DRUM %04d to IAS %04d: %06d%04d%c '%s'\n", 
                        AR, IAS_TimingRing+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            if (n==0) {f0=AR; t0=IAS_TimingRing+9000;}
            f1=AR; t1=IAS_TimingRing+9000;
            // copy symbolic info from drum to ias (so code copies to ias to be executed faster
            // keeps its symbolic info)
            memset(&IAS_Symbolic_Buffer[IAS_TimingRing * 80], 0, 80);            // clear ias symbolic info
            sim_strlcpy(&IAS_Symbolic_Buffer[IAS_TimingRing * 80], 
                        &DRUM_Symbolic_Buffer[AR * 80], 80);
        } else {
            // copy ias to drum
            d = DRUM[AR] = IAS[IAS_TimingRing];
            ZeroNeg = DRUM_NegativeZeroFlag[AR] = IAS_NegativeZeroFlag[IAS_TimingRing];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IAS %04d to DRUM %04d: %06d%04d%c '%s'\n", 
                        IAS_TimingRing+9000, AR, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            if (n==0) {t0=AR; f0=IAS_TimingRing+9000;}
            t1=AR; f1=IAS_TimingRing+9000;
        }
        n++;
        if ((AR % 50) == 49)                        { ec = 0; break; }
        if (IAS_TimingRing == 59)                   { ec = 1; break; }
        if ((bEOB) && ((IAS_TimingRing % 10) == 9)) { ec = 2; break; }
        AR++; IAS_TimingRing++;
    }
    sim_debug(DEBUG_DATA, &cpu_dev, " ... Copy %04d-%04d to %04d-%04d (%d words)\n",
                                               f0, f1, t0, t1, n);
    sim_debug(DEBUG_DATA, &cpu_dev, "     ended by end of %s condition\n",
                                               (ec == 0) ? "Drum band" : (ec == 1) ? "IAS" : "IAS Block");
    IAS_TimingRing = (IAS_TimingRing + 1) % 60; // incr timing ring at end of transfer
    return n; 
}


// opcode execution 
// input: opcode, DA (data address), DrumAddr (current word under the r/w heads. Needed to 
//        calculate time used on instr execution) prior to call ExecOpcode DIST cpu register 
//        must be loaded with the needed data for inst execution, fast mode flag
// output: bBranchToDA: =1 if next inst must be taken from DA register instead of DA
//         CpuStepsUsed: number of steps (=word time) used on program execution
t_stat ExecOpcode(int opcode, int DA, int bFastMode, 
                  int * bBranchToDA, 
                  int DrumAddr, 
                  int * CpuStepsUsed)
{
    t_stat reason = 0;
    t_int64 d;
    int i, n, neg, SvOV; 
    int bUsingIAS, total_msec;

    *bBranchToDA  = 0; 
    *CpuStepsUsed = 0;

    switch(opcode) {
        case OP_NOOP   :   // No operation 
            if ((IC == 0) && ((PR % D4) == 0)) reason = STOP_HALT; // if loop on NOOP on addr zero -> machine idle -> stop cpu
            break;
        case OP_STOP   :   // Stop if console switch is set to stop, otherwise continue as a NO-OP 
            if (CSWProgStop) {
                reason = STOP_PROG;
                // stops has the consequence to prevent AR to be set with IA contents (to point to next instruction).
                // so must set a flag so next setp/go scp command will take next inst to execute from 
                // IA field in PR reg instead of AR
            }
            break;
        // arithmetic
        case OP_RAL:   // Reset and Add into Lower
        case OP_RSL:   // Reset and Subtract into Lower
        case OP_RAABL: // Reset and Add Absolute into Lower
        case OP_RSABL: // Reset and Subtract Absolute into Lower
            d = DIST;
            if ((opcode == OP_RAABL) || (opcode == OP_RSABL)) d = AbsWord(d);
            if ((opcode == OP_RSL)   || (opcode == OP_RSABL)) d = -d;
            AccNegativeZeroFlag = 0;
            ACC[1] = 0;
            ACC[0] = d;
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            // sequence chart for Add/Substract
            // (1)     (0..49)   (1)      (0/1)     (2)       (0/2)       (1)
            // Enable  Search    Data to  Wait      Dist to   Complement  Remove A
            // Dist    Data      dist     for even  Acc       Neg Sum     interlock
            //                                      (1)       (1)         (1)          (0..49)
            //                                      Restart   IA to AR    Enable PR    Search next
            //                                      Signal                             Inst
            *CpuStepsUsed = 1+1+2+1
                            +(DrumAddr % 2); // using lower acc -> wait for even 
            // no need to complement neg sum
            break;
        case OP_AL: // Add to Lower
        case OP_SL: // Subtract from Lower
        case OP_AABL: // Add Absolute to lower
        case OP_SABL: // Subtract Absolute from lower
            if ((opcode == OP_AL) && (ACC[1] == 0) && (ACC[0] == 0) && (AccNegativeZeroFlag) && 
                (DIST == 0) && (DistNegativeZeroFlag)) {
                // special case as stated in Operation manual 22(22-6060-2_650_OperMan.pdf), page 95
                // Acc result on minus zero if acc contains minus zero and AU or AL with a drum 
                // location that contains minus zero
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: 0000000000 0000000000- (Minus Zero), OV: 0\n");
                // acc keeps the minus zero it already has
                break; 
            }
            d = DIST;
            if ((opcode == OP_AABL) || (opcode == OP_SABL)) d = AbsWord(d);
            if ((opcode == OP_SL)   || (opcode == OP_SABL)) d = -d;
            AddToAcc(0,d,1);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            *CpuStepsUsed = 1+1+2+1
                            +(DrumAddr % 2)                 // using lower acc -> wait for even 
                            +(bAccNegComplement ? 2:0);     // acc sign change -> need to complement neg sum (two steps)
            break;
        case OP_RAU: // Reset and Add into Upper
        case OP_RSU: // Reset and Subtract into Upper
        case OP_AU:  // Add to Upper
        case OP_SU:  // Substract from Upper
            if ((opcode == OP_AU) && (ACC[1] == 0) && (ACC[0] == 0) && (AccNegativeZeroFlag) && 
                (DIST == 0) && (DistNegativeZeroFlag)) {
                // special case as stated in Operation manual 22(22-6060-2_650_OperMan.pdf), page 95
                // Acc result on minus zero if acc contains minus zero and AU or AL with a drum 
                // location that contains minus zero
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: 0000000000 0000000000- (Minus Zero), OV: 0\n");
                // acc keeps the minus zero it already has
                break; 
            }
            d = DIST;
            if ((opcode == OP_RAU) || (opcode == OP_RSU)) ACC[1] = ACC[0] = 0;
            if ((opcode == OP_SU)  || (opcode == OP_RSU)) d = -d;
            AddToAcc(d,0,1);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                 OV);
            *CpuStepsUsed = 1+1+2+1
                            +((DrumAddr+1) % 2)             // using upper acc -> wait for odd
                            +(bAccNegComplement ? 2:0);     // acc sign change -> need to complement neg sum (two steps)
            break;
        // Multiply/divide
        case OP_MULT: // Multiply
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Mult ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            sim_debug(DEBUG_DETAIL, &cpu_dev,   "...  by DIST: %06d%04d%c\n", 
                                                               printfd);
            if ((ACC[1] == 0) && (ACC[0] == 1) && (DIST == 0) && (DistNegativeZeroFlag)) {
                // special case as stated in Operation manual 22(22-6060-2_650_OperMan.pdf), page 95
                // Acc result on minus zero if a drum location that contains minus zero
                // is multiplied by +1
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... Mult result ACC: 0000000000 0000000000- (Minus Zero), OV: 0\n");
                // acc set to minus zero 
                ACC[1] = ACC[0] = 0;
                AccNegativeZeroFlag = 1;
                break; 
            }
            *CpuStepsUsed = 0;
            SvOV=OV; OV=0;
            neg = (DIST < 0) ? 1:0; if (AccNegative) neg = 1-neg;
            d      = AbsWord(DIST); 
            ACC[0] = AbsWord(ACC[0]); 
            ACC[1] = AbsWord(ACC[1]); 
            for(i=0;i<10;i++) {
                n = ShiftAcc(1);
                *CpuStepsUsed = *CpuStepsUsed + 2;
                while (n-- > 0) {
                    AddToAcc(0, d, 1);
                    *CpuStepsUsed = *CpuStepsUsed + 18;
                    if (OV) break;
                }
                if (OV) break;
            }
            if (neg) {
                ACC[0] = -ACC[0]; 
                ACC[1] = -ACC[1]; 
            }
            if (SvOV==1) OV=1; // if overflow was set at beginning of opcode execution, keeps its state
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa, 
                OV);
            // sequence chart for Multiply/Divide
            // (1)     (0..49)   (1)      (0/1)     (20..200)  (1)
            // Enable  Search    Data to  Wait      Mult/Div   Remove A
            // Dist    Data      dist     for even  loop       interlock
            //                                      (1)       (1)         (1)          (0..49)
            //                                      Restart   IA to AR    Enable PR    Search next
            //                                      Signal                             Inst
            *CpuStepsUsed = 1+1+1+1
                            +(DrumAddr % 2)                 // wait for even 
                            +*CpuStepsUsed;                 // i holds the number of loops done
            break;
        case OP_DIV: // Divide
        case OP_DIVRU: // Divide and reset upper accumulator
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Div ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... by DIST: %06d%04d%c\n", 
                                                               printfd);
            SvOV=OV;
            if (DIST == 0) {
                OV = 1;
                sim_debug(DEBUG_EXP, &cpu_dev, "Divide By Zero -> OV set and ERROR\n");
                reason = STOP_OV; // divisor zero allways stops the machine
            } else if (AbsWord(DIST) <= AbsWord(ACC[1])) {
                OV = 1;
                sim_debug(DEBUG_EXP, &cpu_dev, "Quotient Overflow -> OV set and ERROR\n");
                reason = STOP_OV; // quotient overfow allways stops the machine
            } else {
                *CpuStepsUsed = 0;
                OV = 0;
                neg = (DIST < 0) ? 1:0; if (AccNegative) neg = 1-neg;
                d      = AbsWord(DIST); 
                ACC[0] = AbsWord(ACC[0]); 
                ACC[1] = AbsWord(ACC[1]); 
                for(i=0;i<10;i++) {
                    n = ShiftAcc(1);
                    ACC[1] = ACC[1] + n * D10;
                    *CpuStepsUsed = *CpuStepsUsed + 2;
                    while (d <= ACC[1]) {
                        AddToAcc(-d, 0, 0);
                        *CpuStepsUsed = *CpuStepsUsed + 18;
                        ACC[0]++;
                    }
                }
                if (neg) {
                    ACC[0] = -ACC[0]; 
                    ACC[1] = -ACC[1]; 
                }
                if (opcode == OP_DIVRU) {
                    ACC[1] = 0;
                }
                *CpuStepsUsed = 1+1+1+1                    
                                +(DrumAddr % 2)                 // wait for even 
                                +*CpuStepsUsed + 40;            // i holds the number of loops done
            }
            if (SvOV==1) OV=1; // if overflow was set at beginning of opcode execution, keeps its state
            // remainder in upper acc, quotient in lower accum
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Div result ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            break;
        // shift
        case OP_SLT: // Shift Left
        case OP_SRT: // Shift Right
        case OP_SRD: // Shift Right and Round
            n = DA % 10;                            // number of digits to shift
            if (opcode == OP_SRD) if (n == 0) n=10; // SRD 0000 means 10 sifts. SRT/SLT 0000 means no shifts
            d = 0;
            while (n-- > 0) {
                d = ShiftAcc((opcode == OP_SLT) ? 1:-1);
            }
            if (opcode == OP_SRD) {
                if (d <= - 5) AddToAcc(0,-1,0);
                if (d >=   5) AddToAcc(0,+1,0);
            }
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            // sequence chart for shift
            // (1)       (0/1)      (2)      (1)
            // Enable    Wait       Per      Remove A
            // Sh count  for even   shift    interlock
            //                      (0/1)    (1)         (1)          (0..49)
            //                      Restart  IA to AR    Enable PR    Search next
            //                      Signal                            Inst
            *CpuStepsUsed = 1+1+1
                            +(DrumAddr % 2)                 // wait for even 
                            + 2*(DA % 10)                   // number of shifts done 
                            + ((opcode == OP_SRD) ? 1:0);
            break;
        case OP_SCT    :   // Shift accumulator left and count 
            n = DA % 10;  
            if (n>0) n=10-n; // shift count (ten's complement of unit digit of DA, or zero if digit is zero)
            neg = AccNegative; // save acc sign
            ACC[0] = AbsWord(ACC[0]);
            ACC[1] = AbsWord(ACC[1]);
            i=0;
            if (Get_HiDigit(ACC[1]) > 0) {
                // no shift, two low orfer digits replaced by zero
                ACC[0] = SetIA2(ACC[0], 0); // replace last two digits by 00
            } else {
                while (Get_HiDigit(ACC[1]) == 0)  {
                    if (n==10) {
                        OV = 1;
                        break;
                    }
                    ShiftAcc(1); // shift left
                    i++;         // number of shift
                    n++;         // count
                }
                ACC[0] = SetIA2(ACC[0], n); // replace last two digits by count n
            }
            AccNegativeZeroFlag = 0;
            if (neg) {ACC[0] = -ACC[0]; ACC[1] = -ACC[1]; }
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            *CpuStepsUsed = 1+1+1
                            +(DrumAddr % 2)                 // wait for even 
                            + 2*i;                  // number of shifts done 
            break;
        // load and store
        case OP_STL: // Store Lower in Mem
        case OP_STU: // Store Upper in Mem
            if ((ACC[0] == 0) && (ACC[1] == 0) && (AccNegativeZeroFlag)) {
                DistNegativeZeroFlag = 1;
            } else {
                DistNegativeZeroFlag = 0;
            }
            DIST = (opcode == OP_STU) ? ACC[1] : ACC[0];

            // sequence chart for store
            // (1)       (0/1)      (1)      (0..49)  (1)     (1)        (1)
            // Enable    Wait       L/U acc  Search   Store   IA to AR   Enable PR
            // Dist      for even   to dist  data     data
            //           or odd     
            *CpuStepsUsed = 1+1+1+1+1+                   
                            + (((opcode == OP_STU) ? DrumAddr:DrumAddr+1) % 2);   // wait for odd/even depending on STU/STL opcode
            break;
        case OP_STD: // store distributor
            *CpuStepsUsed = 1+1+1+1;                    
            break;
        case OP_STDA: // Store Lower Data Address
            n  = ((ACC[0] / D4) % D4);     // get data addr xxDDDDxxxx from lower Acc
            d = SetDA(DIST, n);            // replace it in distributor       
            if ((d == 0) && ((DIST < 0) || ( (DIST == 0) && (DistNegativeZeroFlag) ))) {
                // if dist results in zero but was negative or negative zero before replacing digits 
                // then it is set to minus zero 
                DistNegativeZeroFlag = 1; 
            } else {
                DistNegativeZeroFlag = 0;
            }
            DIST = d; 
            *CpuStepsUsed = 1+1+1+1
                            +(DrumAddr % 2);                 // wait for even 
            break;
        case OP_STIA: // Store Lower Instruction Address
            n = (ACC[0] % D4);            // get inst addr xxyyyyAAAA
            d = SetIA(DIST, n);           // replace it in distributor       
            if ((d == 0) && ((DIST < 0) || ( (DIST == 0) && (DistNegativeZeroFlag) ))) {
                // if dist results in zero but was negative or negative zero before replacing digits 
                // then it is set to minus zero 
                DistNegativeZeroFlag = 1; 
            } else {
                DistNegativeZeroFlag = 0;
            } 
            DIST = d; 
            *CpuStepsUsed = 1+1+1+1
                            +(DrumAddr % 2);                 // wait for even 
            break;
        case OP_LD:  // Load Distributor
            *CpuStepsUsed = 1+1+1+1;      
            break;
        case OP_TLE:   // Table lookup on equal
        case OP_TLU:   // Table lookup 
            {
                char s[6];
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... Search DIST: %06d%04d%c '%s'\n", 
                            printfd, 
                            word_to_ascii(s, 1, 5, DIST));

                bUsingIAS = (AR >= 9000) ? 1:0;
                if (bUsingIAS) {
                    AR = DA; // if TLU is searching on IAS, search starts at given addr
                } else {
                    AR = (DA / 50) * 50; // set AR to start of drum band based on DA
                }
                AR--; n=-1;
                while (1) {
                    AR++; n++;
                    if (0==IsDrumAddrOk(AR, vda_DS)) {
                        sim_debug(DEBUG_EXP, &cpu_dev, "Invalid AR addr %d ERROR\n", AR);
                        reason = STOP_ADDR;
                        break;
                    }
                    if ((bUsingIAS == 0) && ((AR % 50) > 47)) continue; // skip addr 48 & 49 of band that cannot be used for tables
                    ReadAddr(AR, &d, NULL);       // read table argument
                    if ( (opcode == OP_TLU) ? 
                              (AbsWord(d) >= AbsWord(DIST)) : 
                              (AbsWord(d) == AbsWord(DIST))
                        ) {
                        sim_debug(DEBUG_DETAIL, &cpu_dev, "...  Found %04d: %06d%04d%c '%s'\n", 
                            AR, printfw(d,0), 
                            word_to_ascii(s, 1, 5, d));
                        break; // found
                    }
                }
                // if tlu on ias, incr timing ring at end of instr execution
                if (bUsingIAS) IAS_TimingRing = (IAS_TimingRing + 1) % 60;
                // set the result as xxNNNNxxxx in lower acc
                ACC[0] = SetDA(ACC[0], DA+n);                 
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... Result ACC: %06d%04d %06d%04d%c, OV: %d\n",
                    printfa,
                    OV);
            }
            *CpuStepsUsed = 1+1+1+1+1+1
                            +(DrumAddr % 2)                 // wait for even 
                            + n;                            // number of reads to find the argument searched for
            break;
        // branch
        case OP_BRD1: case OP_BRD2: case OP_BRD3: case OP_BRD4: case OP_BRD5: // Branch on 8 in distributor positions 1-10 
        case OP_BRD6: case OP_BRD7: case OP_BRD8: case OP_BRD9: case OP_BRD10:
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Check DIST: %06d%04d%c\n", 
                                                              printfd);
            d = AbsWord(DIST); 
            n = opcode - OP_BRD10; if (n == 0) n = 10;
            while (--n > 0) d = d / 10;
            d = d % 10;
            if (d == 8) {
               sim_debug(DEBUG_DETAIL, &cpu_dev, "Digit is %d -> Branch Taken\n", (int32) d);
               *bBranchToDA = 1; // IA (next instr addr) will be taken from DA. Branch taken
            } else if (d == 9) {
               // IA kept as already set. Branch not taken
               sim_debug(DEBUG_DETAIL, &cpu_dev, "Digit is %d -> Branch Not Taken\n", (int32) d);
            } else {
               // any other value for tested digit -> stop
               sim_debug(DEBUG_EXP, &cpu_dev, "Digit is %d -> Branch ERROR\n", (int32) d);
               reason = STOP_ERRO;
               break;
            }
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        case OP_BRNZU: // Branch on Non-Zero in Upper
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            if (ACC[1] != 0) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Upper ACC not Zero -> Branch Taken\n");
                *bBranchToDA = 1;
            }
            *CpuStepsUsed = 1+1                    
                            +(DrumAddr % 2)             // wait for even 
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        case OP_BRNZ: // Branch on Non-Zero 
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            if ((ACC[1] != 0) || (ACC[0] != 0)) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Not Zero -> Branch Taken\n");
                *bBranchToDA = 1; 
            }
            *CpuStepsUsed = 1                    
                            +((DrumAddr+1) % 2)         // wait for odd
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        case OP_BRMIN: // Branch on Minus
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            if (AccNegative) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Is Negative -> Branch Taken\n");
                *bBranchToDA = 1; 
            }
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        case OP_BROV: // Branch on Overflow
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Check OV: %d\n", OV);
            if (OV) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "OV Set -> Branch Taken\n");
                *bBranchToDA = 1; 
            }
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            // BOV resets overflow
            OV=0;
            break;
        // Card I/O
        case OP_RD:   // Read a card 
        case OP_RD2:
        case OP_RD3:
        case OP_RC1:
        case OP_RC2:
        case OP_RC3:
            bUsingIAS = (AR >= 9000) ? 1:0;
            {
                char s[6];
                int nUnit, area, nIL;

                if ((opcode == OP_RD2) || (opcode == OP_RC2)) {
                    nUnit = 2; nIL = IL_RD23; area = 13;
                } else if ((opcode == OP_RD3) || (opcode == OP_RC3)) {
                    nUnit = 3; nIL = IL_RD23; area = 13;
                } else {
                    nUnit = 1; nIL = IL_RD1; area = 1;
                } 
                if (bUsingIAS == 0) {
                    AR = (DA / 50) * 50 + area; // Drum Read Band is XX01 to XX10 or XX51 to XX60
                }

                reason = cdr_cmd(&cdr_unit[nUnit], 0, AR);
                if (reason == SCPE_NOCARDS) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "No Cards available to be read\n"); 
                    reason = STOP_IO;
                    break;
                } else if (reason != SCPE_OK) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "IO Error reason %d\n", reason); 
                    break;
                }
                // copy card data from IO Sync buffer to drum/ias
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... Read Card Unit CDR%d\n", nUnit);
                for (i=0;i<10;i++) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "... Read Card %04d: %06d%04d%c '%s'\n", 
                        AR+i, printfw(IOSync[i],IOSync_NegativeZeroFlag[i]), 
                        word_to_ascii(s, 1, 5, IOSync[i]));
                    if (bUsingIAS == 0) {
                        DRUM[AR + i] = IOSync[i];
                        DRUM_NegativeZeroFlag[AR + i] = IOSync_NegativeZeroFlag[i];
                    } else {
                        n = AR - 9000 + i;
                        IAS[n] = IOSync[i];
                        IAS_NegativeZeroFlag[n] = IOSync_NegativeZeroFlag[i];
                        if ((n % 10) == 9) break; // hit ias end of block, terminate read even if transfered less than 10 words
                    }                   
                }
                if (bUsingIAS) IAS_TimingRing = DA; // is using ias, set timing ring on instr completition
                if (cdr_unit[1].u5 & URCSTA_LOAD) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "... Is a LOAD Card\n");
                    *bBranchToDA = 1;      // load card -> next instr is taken from DA
                }
                // set interlock 300 msec for card read processing
                InterLockCount[nIL] = msec_to_wordtime(300); 
                // 300 msec read cycle, 270 available for computing
                *CpuStepsUsed = msec_to_wordtime(30); // 30 msec div 0.096 msec word time;                    
            }
            break;
        case OP_PCH:   // Punch a card 
        case OP_WR2:
        case OP_WR3:
            bUsingIAS = (AR >= 9000) ? 1:0;
            {
                char s[6];
                int nUnit, area, nIL;

                if (opcode == OP_WR2) {
                    nUnit = 2; nIL = IL_WR23; area = 39;
                } else if (opcode == OP_WR3) {
                    nUnit = 3; nIL = IL_WR23; area = 39;
                } else {
                    nUnit = 1; nIL = IL_WR1; area = 27;
                } 
                if (bUsingIAS == 0) {
                    AR = (DA / 50) * 50 + area; // Drum Read Band is XX27 to XX36 or XX77 to XX86
                }

                // clear IO Sync buffer
                for (i=0;i<10;i++) IOSync[i] = IOSync_NegativeZeroFlag[i] = 0;
                // copy card data to IO Sync buffer from drum/ias
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... Punch Card Unit CDP%d\n", nUnit);
                for (i=0;i<10;i++) {
                    if (bUsingIAS == 0) {
                        IOSync[i] = DRUM[AR + i];
                        IOSync_NegativeZeroFlag[i] = DRUM_NegativeZeroFlag[AR + i];
                    } else {
                        n = AR - 9000 + i;
                        IOSync[i] = IAS[n];
                        IOSync_NegativeZeroFlag[i] = IAS_NegativeZeroFlag[n];
                        IAS_TimingRing = n;
                    }                   
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "... Punch Card %04d: %06d%04d%c '%s'\n", 
                        AR+i, printfw(IOSync[i],IOSync_NegativeZeroFlag[i]), 
                        word_to_ascii(s, 1, 5, IOSync[i]));
                    if (bUsingIAS) {
                        // punching from IAS. If hit ias end of block, terminate even 
                        // if transfered less than 10 words (rest of words were filled with zeroes)
                        if ((n % 10) == 9) break; 
                    }
                }

                reason = cdp_cmd(&cdp_unit[nUnit], 0,AR);
                if (reason == SCPE_NOCARDS) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "No Cards available to be punched\n"); 
                    reason = STOP_IO;
                    break;
                } else if (reason != SCPE_OK) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "IO Error reason %d\n", reason); 
                    break;
                }
                if (bUsingIAS) IAS_TimingRing = (IAS_TimingRing + 1) % 60; // incr timing ring at end of pch

                // set interlock 600 msec for card punch processing 
                InterLockCount[nIL] = msec_to_wordtime(600); 
                // 600 msec punch cycle, 565 available for computing
                *CpuStepsUsed = msec_to_wordtime(35); // 35 msec div 0.096 msec word time;                    
            }
            break;
        // IAS - Immediate Access Storage
        case OP_SET: // Set IAS Timing Ring
            *CpuStepsUsed = 1+1+1;                    
            break;
        case OP_LDI: // Load IAS (from Drum)
            n = TransferIAS("D->I", 0); // transfer drum to ias, end of ias block does not terminate transfer
            *CpuStepsUsed = 1+1+1+n;                    
            break;
        case OP_STI: // Store IAS (to Drum)
            n = TransferIAS("I->D", 0); // transfer ias to drum, end of ias block does not terminate transfer
            *CpuStepsUsed = 1+1+1+n;                    
            break;
        case OP_LIB: // Load IAS Block (from Drum)
            n = TransferIAS("D->I", 1); // transfer drum to ias, end of ias block does terminate transfer
            *CpuStepsUsed = 1+1+1+n;                    
            break;
        case OP_SIB: // Store IAS Block (to Drum)
            n = TransferIAS("I->D", 1); // transfer ias to drum, end of ias block does terminate transfer
            *CpuStepsUsed = 1+1+1+n;                    
            break;
        // Index Register
        case OP_AXA: // Add/Substract [with reset] to IRA
        case OP_SXA:
        case OP_RAA:
        case OP_RSA:
            n = IR[0];
            if  ((opcode == OP_RAA) || (opcode == OP_RSA)) n = 0;
            if (DA >= 8000) {
               ReadAddr(DA, &d, NULL);
               DIST=d; DistNegativeZeroFlag=0;
               sim_debug(DEBUG_DATA, &cpu_dev, "... Read %04d: %06d%04d%c\n", DA, printfd);
               i = (int) (d % D4); 
            } else {
               i = DA;
            }
            n = n + (((opcode == OP_AXA) || (opcode == OP_RAA)) ? i : -i);
            NormalizeAddr(&n, 1);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IRA: %04d%c\n", 
                                                        abs(n), n<0?'-':'+');
            IR[0] = n;
            *CpuStepsUsed = 1+1+1;                    
            break;
        case OP_AXB: // Add/Substract [with reset] to IRB
        case OP_SXB:
        case OP_RAB:
        case OP_RSB:
            n = IR[1];
            if  ((opcode == OP_RAB) || (opcode == OP_RSB)) n = 0;
            if (DA >= 8000) {
               ReadAddr(DA, &d, NULL);
               DIST=d; DistNegativeZeroFlag=0;
               sim_debug(DEBUG_DATA, &cpu_dev, "... Read %04d: %06d%04d%c\n", DA, printfd);
               i = (int) (d % D4); 
            } else {
               i = DA;
            }
            n = n + (((opcode == OP_AXB) || (opcode == OP_RAB)) ? i : -i);
            NormalizeAddr(&n, 1);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IRB: %04d%c\n", 
                                                        abs(n), n<0?'-':'+');
            IR[1] = n;
            *CpuStepsUsed = 1+1+1;                    
            break;
        case OP_AXC: // Add/Substract [with reset] to IRC
        case OP_SXC:
        case OP_RAC:
        case OP_RSC:
            n = IR[2];
            if  ((opcode == OP_RAC) || (opcode == OP_RSC)) n = 0;
            if (DA >= 8000) {
               ReadAddr(DA, &d, NULL);
               DIST=d; DistNegativeZeroFlag=0;
               sim_debug(DEBUG_DATA, &cpu_dev, "... Read %04d: %06d%04d%c\n", DA, printfd);
               i = (int) (d % D4); 
            } else {
               i = DA;
            }
            n = n + (((opcode == OP_AXC) || (opcode == OP_RAC)) ? i : -i);
            NormalizeAddr(&n, 1);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IRC: %04d%c\n", 
                                                        abs(n), n<0?'-':'+');
            IR[2] = n;
            *CpuStepsUsed = 1+1+1;                    
            break;
        case OP_BMA: // Branch on IR Minus
        case OP_BMB:
        case OP_BMC:
            i = ((opcode == OP_BMA) ? 0 : (opcode == OP_BMB) ? 1 : 2);
            n = IR[i];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IR%c: %04d%c\n", 
                                                      i+'A', abs(n), n<0?'-':'+');
            if (n<0) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Is Negative -> Branch Taken\n");
                *bBranchToDA = 1; 
            }
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        case OP_NZA: // Branch on IR Zero
        case OP_NZB:
        case OP_NZC:
            i = ((opcode == OP_NZA) ? 0 : (opcode == OP_NZB) ? 1 : 2);
            n = IR[i];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IR%c: %04d%c\n",                                                       
                                                      i+'A', abs(n), n<0?'-':'+');
            if (n!=0) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Is Non Zero -> Branch Taken\n");
                *bBranchToDA = 1; 
            }
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        // floating point
        case OP_FAD: // FP Add
        case OP_UFA: // Unnormalized FP Add
        case OP_FSB: // FP Sub
        case OP_FAM: // FP Add Absolute value
        case OP_FSM: // FP Sub Absolute
            n = AddFloatToAcc((opcode == OP_FSB) || (opcode == OP_FSM), // subtract?
                              (opcode == OP_FAM) || (opcode == OP_FSM), // absolute value?
                              (opcode != OP_UFA) // normalize?
                              );
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... ACC: %06d%04d %06d%04d%c, OV: %d, DIST: %06d%04d%c\n", 
                printfa, OV, printfd);
            *CpuStepsUsed = 1+1
                            +(DrumAddr % 2)             // using upper acc -> wait for even
                            +2+2+2+1
                            +n;                         // Float Add steps 
            break;
        case OP_FMP: // Float Multiply
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Mult ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            sim_debug(DEBUG_DETAIL, &cpu_dev,   "...  by DIST: %06d%04d%c\n", 
                                                               printfd);
            SvOV=OV; OV = 0;
            if (((ACC[1] / 100) == 0) || ((DIST / 100) == 0)) {
                // if any mantissa is zero -> multiply by zero -> result = 0
                ACC[1] = ACC[0] = 0;
            } else {
                int exp = GetExp(DIST) + GetExp(ACC[1]) - 50;

                neg = (DIST < 0) ? -1:1; if (AccNegative) neg = -neg;
                ACC[1] = SetExp(AbsWord(ACC[1]), 0); 
                d      = SetExp(AbsWord(DIST),   0); 
                // mult mantissas
                for(i=0;i<10;i++) {
                    n = ShiftAcc(1);
                    *CpuStepsUsed = *CpuStepsUsed + 2;
                    while (n-- > 0) {
                        AddToAcc(0, d, 1);
                        *CpuStepsUsed = *CpuStepsUsed + 18;
                        if (OV) break;
                    }
                    if (OV) break;
                }
                MantissaRoundAndNormalizeToFloat(CpuStepsUsed, neg, exp);
            }
            if (SvOV==1) OV=1; // if overflow was set at beginning of opcode execution, keeps its state
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... FP Mult result ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa, 
                OV);
            *CpuStepsUsed = 1+1+2+2+2+1+ *CpuStepsUsed 
                            +(DrumAddr % 2);                // wait for even 
            break;        
        case OP_FDV: // Float Divide 
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Div ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... by DIST: %06d%04d%c\n", 
                                                               printfd);            
            SvOV=OV; OV = 0;
            if ((DIST / 100) == 0) {    // check mantissa for zero, not exponent
                OV = 1;
                sim_debug(DEBUG_DETAIL, &cpu_dev, "Divide By Zero -> OV set and ERROR\n");
                reason = STOP_OV; // float divisor zero allways stops the machine
            } else if ((ACC[1] / 100) == 0) {
                // if dividend is zero -> result = 0
                ACC[1] = ACC[0] = 0;
            } else {
                int exp = GetExp(ACC[1]) - GetExp(DIST) + 50;                
                neg = (DIST < 0) ? -1:1; if (AccNegative) neg = -neg;

                ACC[1] = AbsWord(ACC[1]) / 100; 
                d      = AbsWord(DIST)   / 100; 
                // div mantissas 
                for(i=0;;i++) {
                    while (d <= ACC[1]) {
                        AddToAcc(-d, 0, 0);
                        *CpuStepsUsed = *CpuStepsUsed + 18;
                        ACC[0] = ACC[0] + 10; // add to second position of lower
                    }
                    if (i > 8) break;
                    if ((i == 8) && (Get_HiDigit(ACC[0]))) {exp++; break;}
                    n = ShiftAcc(1);
                    ACC[1] = ACC[1] + n * D10; // extra digit
                    *CpuStepsUsed = *CpuStepsUsed + 2;
                }
                ACC[1] = ACC[0];
                MantissaRoundAndNormalizeToFloat(CpuStepsUsed, neg, exp);                
            }
            if (SvOV==1) OV=1; // if overflow was set at beginning of opcode execution, keeps its state
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... FP Div result ACC: %06d%04d %06d%04d%c, OV: %d\n", 
                printfa,
                OV);
            *CpuStepsUsed = 1+1+2+2+16+2+1+ *CpuStepsUsed 
                            +(DrumAddr % 2);                // wait for even 
            break;
        // tape opcodes
        case OP_RTC: // Read Tape Check
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d read check\n", DA % 10);
            goto tape_opcode;
        case OP_RTA: // Read Tape Alphanumeric
        case OP_RTN: // Read Tape Numeric
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d read at IAS: %04d\n", DA % 10, IAS_TimingRing + 9000);
            goto tape_opcode;
        case OP_WTN: // Write Tape Numeric
        case OP_WTA: // Write Tape Alphabetic
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d write from IAS: %04d\n", DA % 10, IAS_TimingRing + 9000);
            goto tape_opcode;
        case OP_WTM: // Write Tape Mark
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d write tape mark\n", DA % 10);
            goto tape_opcode;
        case OP_BST: // BackStep Tape 
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d backspace record\n", DA % 10);
            goto tape_opcode;
        case OP_RWD: // rewind
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape %d rewind\n", DA % 10);
        tape_opcode:
            n = (DA % 10);
            if ((n < 0) || (n > 5)) {
               sim_debug(DEBUG_EXP, &cpu_dev, "Invalid Tape addr %d ERROR\n", AR);
               reason = STOP_ADDR;
               break;
            }
            reason = mt_cmd(&mt_unit[n], opcode, 
                            &total_msec, bFastMode); // take into account user has pressed ^F on GUI
            if (reason == SCPE_OK) {
                // tape command terminated
            } else if (reason == SCPE_OK_INPROGRESS) {
                // tape command in progress. 
                // Set interlock on Control Unit. Will be removed by mt_svr when tape operation terminates
                InterLockCount[IL_Tape]    = msec_to_wordtime(total_msec); 
                // Set interlock on IAS if read/write from/to IAS. Will be removed by mt_svr when tape operation terminates
                if ((opcode == OP_RTN) ||  (opcode == OP_RTA) || (opcode == OP_WTN) || (opcode == OP_WTA)){
                    InterLockCount[IL_IAS]  = InterLockCount[IL_Tape]; 
                }
                reason = SCPE_OK;
            } else {
                // other reason are unexpected errors and terminates the opcode execution
                sim_debug(DEBUG_DETAIL, &cpu_dev, "IO Error reason %d\n", reason); 
                break;
            }
            *CpuStepsUsed = 1+1+1+1+1;
            break;
        case OP_NTS: // Branch on No Tape Signal
        case OP_NEF: // Branch on No End of File
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... Tape Signal is %s\n", 
                                                   TapeSignalStr[LastTapeSignal]); 
            if ((opcode == OP_NTS) && (LastTapeSignal == 0)) {
                sim_debug(DEBUG_DETAIL, &cpu_dev, "No Tape Signal -> Branch Taken\n");
                *bBranchToDA = 1; 
            } 
            if (opcode == OP_NEF) {
                if (LastTapeSignal != MT_IND_EOF) {
                    sim_debug(DEBUG_DETAIL, &cpu_dev, "No End of File -> Branch Taken\n");
                    *bBranchToDA = 1; 
                } else {
                    LastTapeSignal = 0; // NEF opcode clears end of file signal
                    // clear tape indicator on tape unit
                    if ((LastTapeSelected >= 0) && (LastTapeSelected <= 5)) {
                        mt_unit[LastTapeSelected].u5 &= ~(MT_IND);
                    }
                }
            } 
            *CpuStepsUsed = 1+1                    
                            + ((*bBranchToDA) ? 1:0);   // one extra step needed if branch taken
            break;
        // disk opcodes
        case OP_SDS: // seek
        case OP_RDS: // seek
        case OP_WDS: // seek
            LastDiskCmd = opcode; 
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... DIST: %06d%04d%c\n", printfd);
            n = abs((int)(DIST % D8)) % 1000000; // ramac operation address
            {
                int ad, unit, dsk, tr, arm; 
                ad = n; 
                arm  = ad % 10;  ad=ad / 10; 
                tr   = ad % 100; ad=ad / 100; 
                dsk  = ad % 100; ad=ad / 100; 
                unit = ad % 10;  
                sim_debug(DEBUG_DETAIL, &cpu_dev, "... RAMAC %s on Unit %d, Disk %d, Track %d, Arm %d started\n", 
                         (opcode == OP_SDS) ? "SEEK" : (opcode == OP_RDS) ? "READ" : "WRITE",
                         unit, dsk, tr, arm); 
                if (arm > 2) {
                   sim_debug(DEBUG_EXP, &cpu_dev, "Arm out of range (should be 0..2)\n");
                   reason = STOP_IO; // selected arm or unit out of range
                   StopIOError = 3; 
                }
                if (unit > 3) {
                   sim_debug(DEBUG_EXP, &cpu_dev, "Unit out of range (should be 0..3)\n");
                   reason = STOP_IO; // selected arm or unit out of range
                   StopIOError = 3; 
                }
            }
            if (cpu_unit.flags & OPTION_1DSKARM) {
                // if 1 arm per disk enabled, aliase all disk comands to be executed on arm 0
                n = (n / 10)*10;
            }
            // return into i duration of disk command execution in msec, also take into account fast mode
            reason = dsk_cmd(opcode, n, &total_msec, bFastMode);        
            if (reason == SCPE_OK) {
                // disk command terminated
            } else if (reason == SCPE_OK_INPROGRESS) {
                // disk command in progress. 
                // Set interlock on Ramac Disk Control Unit. Will be removed by dsk_svr when disk operation terminates
                InterLockCount[IL_RamacUnit]    = msec_to_wordtime(75); 
                    // if cpu speed slower than real harware, scale down the interlock wait to 
                    // avoid waiting forever
                // Set interlock on IAS for duration of disk read/write command (seek not needed as it does not uses the IAS mem)
                if ((opcode == OP_RDS) || (opcode == OP_WDS)) {
                    InterLockCount[IL_IAS]  = msec_to_wordtime(total_msec); 
                }
                reason = SCPE_OK;
            } else {
                // other reason are unexpected errors and terminates the opcode execution
                sim_debug(DEBUG_DETAIL, &cpu_dev, "IO Error reason %d\n", reason); 
                StopIOError = 4; 
                break;
            }
            *CpuStepsUsed = 1+1+1+1+1;
            break;
        default:
            reason = STOP_UUO;
            break;
    }
    if ((reason == 0) && (OV) && (CSWOverflowStop)) reason = STOP_OV;

    return reason;
}

// return 2 if must wait for drum rotation, return 1 if must wait for IAS interlock release
//          if must wait for drum rotation return in DrumAddrWait the number of rotations
int WaitForStorage(int AR, int * DrumAddrWait)
{
   if ((AR >= 0) && (AR < DRUMSIZE)) {
       if ((AR % 50) != DrumAddr) {
           *DrumAddrWait = (50 + (AR % 50) - DrumAddr) % 50;
           return 2; // yes, must wait for drum 
       }
   } else if ((STOR) && (AR >= 9000) && (AR < 9060)) {
       if (InterLockCount[IL_IAS] > 0) return 1; // yes, IAS was interlocked. Must wait until interlock is released
   }
   *DrumAddrWait = 0; 
   return 0;
}

// return 1 if must wait for interlock release
int WaitForInterlock(int nInterlock)
{
    int n, arm; 

    // handle combined interlocks
    if (nInterlock == IL_Tape_and_Unit_and_IAS) {
       if (WaitForInterlock(IL_IAS)) return 1;
       if (WaitForInterlock(IL_Tape)) return 1;
       if (WaitForInterlock(-1)) return 1; // check for interlock on tape unit
       return 0;
    } else if (nInterlock == IL_Tape_and_Unit) {
       if (WaitForInterlock(IL_Tape)) return 1;
       if (WaitForInterlock(-1)) return 1; // check for interlock on tape unit
       return 0;
    } else if (nInterlock == IL_RamacUnit_and_Arm_and_IAS) {
       if (WaitForInterlock(IL_IAS)) return 1;
       if (WaitForInterlock(IL_RamacUnit)) return 1;
       if (WaitForInterlock(-2)) return 1; // check for interlock on disk unit
       return 0;
    } else if (nInterlock == IL_RamacUnit_and_Arm) {
       if (WaitForInterlock(IL_RamacUnit)) return 1;
       if (WaitForInterlock(-2)) return 1; // check for interlock on ramac disk unit arm
       return 0;
    }
    // handle interlock on tape unit
    if (nInterlock == -1) {
        // get tape unit referenced by current opcode from intruction DA 
        n = (PR / D4) % D4; if (n<0) n=-n;
        if ((n<8010) || (n>8015)) return 0; // invalid tape addr -> no interlock wait
        n = n % 10;
        return (mt_ready(n)==0) ? 1:0;  // if tape busy -> return 1 -> to wait for tape unit 
    }     
    // handle interlock on disk unit arm
    if (nInterlock == -2) {
        // get disk unit and arm referenced by current DIST (distributor) value
        n = abs((int)(DIST % D8));
        arm = n % 10;
        n   = n / 100000;
        if ((arm > 2) || (n > 3)) return 0; // invalid arm/disk unit -> no interlock wait
          if (cpu_unit.flags & OPTION_1DSKARM) {
            // if 1 arm per disk enabled, alisase all disck comands to be executed on arm 0
            arm=0;
        }
        return (dsk_ready(n, arm)==0) ? 1:0;  // if disk unit arm busy -> return 1 -> wait for disk arm
    }

    // handle single interlock
    return InterLockCount[nInterlock];
}

// measure cpu speed, calc CpuSpeed.measurement.WordTimeCountPerSec
// used to control cpu speed (into Control_CpuSpeed()), and to display 
// current cpu speed if ^I is pressed on CPanel
// if Mode = 0 -> init measurement
//         = 1    calc current speed, return CpuSpeed.measurement.WordTimeCountPerSec
//         = 2    just return CpuSpeed.InstrCount = number of instr executed in this run
int Measure_CpuSpeed(int Mode)
{
    t_int64 nWordTime; 
    uint32  TmInterval; 
    uint32 tnow; 

    tnow=sim_os_msec();
    if (Mode==1) {
        // process measurement made 
        nWordTime  = GlobalWordTimeCount - CpuSpeed.measurement.WordTime0;           // wordtimes executed ...
        TmInterval = tnow - CpuSpeed.measurement.tm0;                                // ... during wall clock msec
        if (TmInterval < 1) TmInterval=1; 
        CpuSpeed.measurement.WordTimeCountPerSec = (int) ((nWordTime * 1000) / TmInterval);
        return  CpuSpeed.measurement.WordTimeCountPerSec;
    } else if (Mode==2) {
        return (int) CpuSpeed.InstrCount;
    } else if (Mode==0) {
        // init measurement cycle        
        CpuSpeed.measurement.tm0                 = tnow;
        CpuSpeed.measurement.WordTime0           = GlobalWordTimeCount;
        CpuSpeed.measurement.WordTimeCountPerSec = 0;
        CpuSpeed.WordTimeCount=0;
    }
    return 0;
}

// speed measurement for scp/debug. mesaures the speed achieved during the 
// whole run (entering and leaving sim_instr())
// if Mode = 1, save sim_os_msec() as end of run, use it for measurement 
//         = 0, use save value of sim_os_msec()
void Measure_run_CpuSpeed(t_int64 * elapsed, int * WTS, int * IPS, char * sBuf, int Mode)
{
    if (Mode==1) CpuSpeed.end_tm0 = sim_os_msec(); 

    *elapsed = CpuSpeed.end_tm0 - CpuSpeed.start_tm0;
    if ((*elapsed < 1) || (CpuSpeed.InstrCount < 2) || (CpuSpeed.start_tm0 == 0)) {
        // if run elapsed <1 msec or less thatn 2 instruction executed
        // then there are no data measured
        *elapsed = 0; 
        return; 
    }
    // WordTimes per second achieved
    *WTS = (int) ((GlobalWordTimeCount - CpuSpeed.GlobalWordTimeCount0) * 1000 / *elapsed); 
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
        CpuSpeed.WordTimeCount++;
        // poll each 10000 WordTimes
        if (CpuSpeed.WordTimeCount < 10000) return 0; 
        CpuSpeed.WordTimeCount=0;
        if (sim_os_msec() - sim_last_poll_kbd_time > 100) {
            // check console to see if user has pressed the break char (normally Control-E)
            sim_poll_kbd();
        }
        return 0; 

    }
    CpuSpeed.WordTimeCount++;
    if (CpuSpeed.WordTimeCount < CpuSpeed.WordTimeMax) return 0; 
    CpuSpeed.WordTimeCount=0;
    // executed max number of wordtimes allowed during time interval

    nMaxWaitLoop = 3; // safety max wait 15msec x 3
    bTooFast=0; 
    // see if going too fast or too slow 
    while (CpuSpeed.WordTimeObjectivePerSec < Measure_CpuSpeed(1)) {
        sim_os_ms_sleep(15);
        if (nMaxWaitLoop-- <0) {
            bTooFast=1; 
            CpuSpeed.WordTimeCount = CpuSpeed.WordTimeMax; // so next call to control_speed will wait again
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

void sim_debug_interlock(int il)
{
    static int nInterlock = 0;
    int n; 

    if (il==0) {
        nInterlock = 0;    // allow debug msg for wait interlock
        return; 
    }
    n = 1 << ((il < 100) ? il : il-100+10);
    if (nInterlock & n) return; // already has signaled the debug message, return
    nInterlock |= n; 
    if (il < 100) {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Wait for interlock on %s (for %d WordTimes)\n", 
                            (il==IL_RD1)  ? "RD1" : 
                            (il==IL_WR1)  ? "WR1" : 
                            (il==IL_RD23) ? "RD23" : 
                            (il==IL_WR23) ? "WR23" : 
                            (il==IL_IAS)  ? "IAS" : 
                            (il==IL_Tape) ? "TCI (Tape)" : 
                            (il==IL_RamacUnit) ? "RAMAC Unit" : 
                             "???", InterLockCount[il]);
    } else {
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Wait for interlock on %s\n", 
                            (il==IL_Tape_and_Unit_and_IAS) ? "IAS+TCI+Tape Unit ready" : 
                            (il==IL_Tape_and_Unit) ? "TCI+Tape Unit ready" : 
                            (il==IL_RamacUnit_and_Arm) ? "RAMAC Unit+Arm" : 
                            (il==IL_RamacUnit_and_Arm_and_IAS) ? "IAS+RAMAC Unit+Arm" : 
                             "???");
    }
}

// symbolc trace commands: have the form of **cmd as comment in soap code
// Symbolic_Trace_tm0=sim_os_msec() when trace starts
// sTrace points to **cmd string
// return 1 if must reset Symbolic_Trace_tm0 value
int Symbolic_Trace(uint32 Symbolic_Trace_tm0, char * Symbolic_Buffer, char * sTrace)
{
    uint32 msec; 
    int trace, addr, len, reset_tm0;
    t_int64 d; 
    char buf[80];
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
    } else if(strstr(sTrace, "**regs")) {
        // the "**regs" string in symbolic buffer prints on console the registers
        // the values are the ones BEFORE opcode execution
        sprintf(buf, "ACC: %06d%04d %06d%04d%c, DIST: %06d%04d%c",  
            printfa, printfd);
        trace = 1;
    } else if(strstr(sTrace, "**index")) {
        // the "**index" string in symbolic buffer prints on console the index registers
        // the values are the ones BEFORE opcode execution
        sprintf(buf, "IRA: %04d, IRB: %04d, IRC: %04d",  
            IR[0], IR[1], IR[2]);
        trace = 1;
    } else if(strstr(sTrace, "**m")) {
        // the "**mNNNN-NN" string in symbolic buffer prints on console the contents
        // of drum/ias memory starting at NNNN. prints NN words
        char c;
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
        if (len==0) sprintf(buf, "len=0"); else
        if ((addr<4000) || ((addr>=9000) && (addr<9060))) {
            trace=2;
        } else sprintf(buf, "invalid addr=%d", addr);
    }
    if (trace) {
        int hh,mm,ss;
        ss = msec / 1000; msec = msec - ss * 1000;
        mm = ss   / 60;   ss   = ss   - mm * 60;
        hh = ss   / 60;   hh   = hh   - hh * 60;
        sim_printf("%02d:%02d:%02d:%03d TRACE:%s\r\n", hh,mm,ss,msec,Symbolic_Buffer); 
        if (buf[0]) {
            sim_printf("             %s\r\n", buf); 
        }
        if (trace == 2) while(len>0) {
            if (addr<4000) d=DRUM[addr]; else
            if ((addr>=9000) && (addr<9060)) d=IAS[addr-9000]; else break;
            sim_printf("             %04d: %06d%04d%c\r\n", 
                addr, printfw(d,0)); 
            addr++;
            len--;
        }
    }
    return reset_tm0;
}

t_stat sim_instr(void)
{
    t_stat              reason;
    int                 opcode, halt_cpu_requested_reason;
    int                 bReadData, bWriteDrum, bBranchToDA, bTooFast;
    int                 instr_count = 0; /* Number of instructions to execute */
    const char *        opname;          /* points to opcode name */               
    char *              Symbolic_Buffer;
    uint32              Symbolic_Trace_tm0; 

    int IA = 0;                                         // Instr Address: addr of next inst 
    int DA = 0;                                         // Data Address; addr of data to be used by current inst

    int MachineCycle, il, bFastMode; 
    int bHalfCycle; 
    int CpuStepsUsed, DrumAddrWait; 

    /* How CPU execution is simulated

    A cpu instruction is executed in real hw in several steps. Some os these steps involves waiting for rotating 
    drum to be positioned on requested address (register AR). Other steps can involve waiting a Interlock to be released.

    User can select in real hw control panel to execute the instructions one by one. The execution is not done
    on full instruction (a full cycle), but rather in instruction half-cycles: I-Cycle and D-Cycle.
    During I-Cycle, the instruction is fetched from drum and decoded. During D-Cycle instruction is performed.

    The simulator models this using the concept of MachineCycles, that groups several steps on opcode execution

    SimH             Real hw equivalent
    machine cycle    half cycle
    0                I-Cycle        WAIT FOR INSTR: 
                                       wait for drum to be positioned at address given by AR cpu register 
    1                I-Cycle        FETCH INST: 
                                       read the drum to get instr to PR register, 
                                       decode PR as opcode, DA, IA, 
                                       apply index tags if needed, write back to PR
                                       check if opcode must wait for interlock release
    2                D-Cycle        WAIT FOR DATA READ:
                                       wait for interlock release if needed
                                       wait for drum to be positioned at AR address if decoded opcode reads data from drum
    3                D-Cycle        EXEC:
                                       get data from storage into DIST if needed
                                       set interlock if needed
                                       execute opcode operation
    4                D-Cycle        WAIT FOR DATA WRITE:
                                       wait opcode excution time 
                                       wait for drum to be positioned at AR address if executed opcode writes data to drum
    5                D-Cycle        WRITEBACK:
                                       if executed opcode writes data to drum, write DIST to drum 
    6                d-Cycle        SET ADDR OF NEXT INST:
                                       set AR=IA to read next instruction 

    */

    if (sim_step != 0) {
        instr_count = sim_step;
        sim_cancel_step();
    }

    reason = halt_cpu_requested_reason = 0;
    opcode=bReadData=bWriteDrum=bBranchToDA=0;
    opname=Symbolic_Buffer=NULL;

    MachineCycle = CpuStepsUsed = DrumAddrWait = 0;
    DrumAddr = 0;

    if (IMachineCycle=='D') {
        // if should exec D-Cycle, directly go to MachineCycle=2
        MachineCycle=2; 
    } else if (IMachineCycle=='d') {
        // take next Inst from IA. This is continuation from last inst that has an stop condition
        // but can continue the excution (formelly: HLT and OV error)
        IA = (uint16) abs((int) (PR % D4)); 
        MachineCycle=6; 
    }

    memset(&InterLockCount[0], 0, sizeof(InterLockCount));
    StopReason=0;
    StopIOError=0;
    sim_debug_interlock(0); // reset flag to allow debug msg for interlock wait

    bFastMode = FAST; 
    CpuSpeed.start_tm0  = sim_os_msec();  // sim_os_msec() when run starts
    CpuSpeed.InstrCount = 0;              // init instr executed in this run
    CpuSpeed.GlobalWordTimeCount0 = GlobalWordTimeCount; // initial value to get the number of wordstimes executed on run termination
    Measure_CpuSpeed(0);                  // init cpu speed measurement
    sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because start of run\n");


    Symbolic_Trace_tm0 = CpuSpeed.start_tm0; // sim_os_msec() when run starts
    bHalfCycle = CSWHalfCycle; // get current value of CSWHalfCycle

    while (reason == SCPE_OK) {       
        /* loop until halted */

        if (sim_interval <= 0) {        /* event queue? */
            reason = sim_process_event();
            if (reason == SCPE_STOP) {
                halt_cpu_requested_reason = reason;     // if stop cpu requested, does not do it inmediatelly
                reason = SCPE_OK;                       // signal it so cpu is halted on end of current intr execution cycle
            } else if (reason != SCPE_OK) {
                break;                                  // if reason is another (error?) then stop inmediatelly
            }
        }
        /* Main instruction fetch/decode loop */
        sim_interval -= 1;         /* count down */


        #if defined(CPANEL)
        if (cpanel_on) {
            // do control panel refresh and user clicks event processing 
            reason = cpanel_interval_refresh();  
            if (reason == SCPE_STOP) {
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
                    // during fast mode, CpuStepsUsed (wordtimes that an opcode need to execute) and
                    // InterLockCount still being calculated, but there is no wait on these values
                    // when exiting fast mode should clear interlock and CpuStep values, 
                    // so cpu does not start waiting on fast mode exit!
                    CpuStepsUsed = DrumAddrWait = 0;
                    memset(&InterLockCount[0], 0, sizeof(InterLockCount));
                }
            }

        }
        #endif

        // control cpu speed, slowdown and yield a max of 45 msec time to OS if needed (with sim_os_ms_sleep)
        // polling for console ^E is done into Control_CpuSpeed
        // return 1 if even yielding 60 msec the cpu is going too fast 
        bTooFast=Control_CpuSpeed(bFastMode);
        if (stop_cpu) {                                // stop_cpu set by scp on SIGINT (^C) or ^E on console
           halt_cpu_requested_reason = SCPE_STOP;      // signal it so cpu is halted on end of current intr execution cycle
        }

        // housekeeping at beggining of inst execution cycle
        if (MachineCycle == 0) {
            // save current instr addr in pseudoregister IC
            // init current instr opcode in pseudoregister PROP
            IC = AR;  PROP = 0;
        
            /* Only check for break points during actual fetch */
            if (sim_brk_summ && sim_brk_test(IC, SWMASK('E'))) {
                reason = STOP_IBKPT;
                break;
            }
        }
        if (MachineCycle <= 2) {
            // only check for ^E on GUI before opcode execution (the opcode can be restarted)
            if (halt_cpu_requested_reason) {
                reason = halt_cpu_requested_reason; // restore the reson value
                break;
            }
        }

        if (bFastMode) {  // FAST mode -> increment GlobalWordTimeCount / DrumAddr at once
            int n, iln;   // depending on how much must wait 

            if (DrumAddrWait) {n=DrumAddrWait;} else   // how much wordtimes to wait for drum rotation
            if (CpuStepsUsed) {n=CpuStepsUsed;} else { // how much wordtimes to wait for opcode execution finish
                n = 0;                                 // how much wordtimes to wait for fist interlock to get free
                for (il=0;il < sizeof(InterLockCount)/ sizeof(InterLockCount[0]) ;il++) {
                    iln=InterLockCount[il];
                    if (iln == 0) continue;
                    if ((n==0) || (iln < n)) n = iln; 
                }
            }
            if (n==0) n=1; // at least ... wait one wordtime

            DrumAddrWait -= n; if (DrumAddrWait<0) DrumAddrWait=0;
            CpuStepsUsed -= n; if (CpuStepsUsed<0) CpuStepsUsed=0;
            for (il=0;il < sizeof(InterLockCount)/ sizeof(InterLockCount[0]) ;il++) {
                iln=InterLockCount[il] - n;
                if (iln < 0) iln = 0;
                InterLockCount[il] = iln; 
            }
            DrumAddr = (DrumAddr+n) % 50;
            GlobalWordTimeCount += n;

            #if defined(CPANEL)
            if ((cpanel_on) && (CpuSpeed_Acceleration == -1)) {
                // if Key Control-F (^F, Ctrl F) being pressed and punched card processing ongoing
                // then wait wall clock msec time to avoid processing read cards too fast
                extern int nCardInReadHopper; 
                static int nCardInReadHopper0 = 0; 
                static uint32 nCardInReadHopperTm0 = 0; 
                int msec; 

                if (nCardInReadHopper==0) {
                    // no cards in read hopper, clear delay timestamp
                    nCardInReadHopperTm0 = 0;
                } else {
                    // if cards in read hopper, init delay timestamp
                    if (nCardInReadHopperTm0==0) {
                        nCardInReadHopperTm0 = sim_os_msec();
                        continue; 
                    } 
                    // time to process one card in fast mode
                    msec=2; // 2 msec per card -> 500 cards per sec -> 2000 cards needs 4 sec
                    if (sim_os_msec() - nCardInReadHopperTm0 < (uint32) msec) continue; 
                    // card processed, reset when card gone
                    if (nCardInReadHopper!=nCardInReadHopper0) {
                        nCardInReadHopper0=nCardInReadHopper; 
                        nCardInReadHopperTm0=0;
                    }
                }
            }
            #endif


        } else {
            // simulate the rotating drum: incr current drum position
            DrumAddr = (DrumAddr+1) % 50;
            // increment number of word counts elapsed from starting of simulator -> this is the global time measurement
            GlobalWordTimeCount++;

            // if any interlock set, decrease it 
            for (il=0;il < sizeof(InterLockCount)/ sizeof(InterLockCount[0]) ;il++) {
                if (InterLockCount[il] > 0) InterLockCount[il]--;
            }
            // decrease pending to execute step intruction count
            if (CpuStepsUsed > 0) CpuStepsUsed--;

            // if cpu is executing wordtime too fast, loop (has executed one word time)
            if (bTooFast) continue; 
        }

        // WAIT FOR INSTR
        if (MachineCycle == 0) {
            // should wait for storage to fetch inst?
            il=WaitForStorage(AR, &DrumAddrWait);
            if (il==1) sim_debug_interlock(IL_IAS);
            if (il>0) continue; // yes, wait for storage to fetch inst
            sim_debug_interlock(0); // reset flag to allow debug msg for interlock wait

            MachineCycle = 1; 
        }
        // FETCH INST
        if (MachineCycle == 1) {
            char * sTrace;
            // get current intruction from storage, save current instr addr in IC
            IC = AR;
            if (0==ReadAddr(AR, &PR, NULL)) {
                reason = STOP_ADDR; 
                break;
            }
            // decode inst
            opname = DecodeOpcode(PR, &opcode, &DA, &IA);
            // get symbolic info if any
            if ((AR < MAXDRUMSIZE) && (DRUM_Symbolic_Buffer[AR * 80] > 0)) {
                // drum symb info
                Symbolic_Buffer = &DRUM_Symbolic_Buffer[AR * 80];
            } else if ((AR >= 9000) && (AR < 9060)) {
                // ias symb info
                Symbolic_Buffer = &IAS_Symbolic_Buffer[(AR - 9000) * 80];
            } else {
                Symbolic_Buffer = 0;
            }
            sim_debug(DEBUG_CMD, &cpu_dev, "Exec %04d: %02d %-6s %04d %04d %s%s\n", 
                                           IC, opcode, (opname == NULL) ? "???":opname, DA, IA,
                                           (Symbolic_Buffer) ? "            symb: ": "", 
                                           (Symbolic_Buffer) ? Symbolic_Buffer     : "");
            if ((Symbolic_Buffer) && (sTrace=strstr(Symbolic_Buffer, "**"))) {
                int n; 
                n = Symbolic_Trace(Symbolic_Trace_tm0, Symbolic_Buffer, sTrace); 
                if (n) Symbolic_Trace_tm0=sim_os_msec(); // reset time origin for trace printouts
            }

            PROP = (uint16) opcode;
            if (opname == NULL) {
                reason = STOP_UUO; 
                break;
            }
            // if DA or IA tagged, modify DA or IA to remove tag and set the developed address in PR
            if (STOR) {
                int nIndexsApplied; 
                if (DRUM4K) {
                    nIndexsApplied = ApplyIndexRegisterModel4(&DA, &IA);
                } else {
                    nIndexsApplied = ApplyIndexRegister(&DA) + ApplyIndexRegister(&IA);
                }
                if (nIndexsApplied > 0) {
                    CpuStepsUsed += nIndexsApplied;
                    PR = (t_int64) opcode * D8 + (t_int64) DA * D4 + (t_int64) IA;
                    sim_debug(DEBUG_CMD, &cpu_dev, "Exec %04d: %02d %-6s %04d %04d %s\n", 
                                           IC, opcode, (opname == NULL) ? "???":opname, DA, IA, 
                                           " (developed addr)");
                }
            }

            AR = DA;    // allways trasnfer DA to AR even if drum will be not read. This is why 
                        // all opcodes must have a valid DA address even if not used to read drum (eg SRT 0003 to shift)
    
            IMachineCycle = 'D'; // next cycle (DA) ready to be exec   
            MachineCycle = 2; 

            // simulates the machine working on half cycles
            if (bHalfCycle) {        // if I-Half finished, about to exec D-Half
                reason = SCPE_STEP;    // then break beacuse I-Half finished
                break; 
            } 
            // stop if address stop set and match
            if (CSWAddrStop == (int) AR) {
                reason = SCPE_STOP;
                break;
            }
        }
        // WAIT FOR DATA READ
        if (MachineCycle == 2) {
            // should wait before exec the inst (time for address untagging) ?
            if (CpuStepsUsed > 0) continue; // yes
            // should wait for interlock release for opcode execution?
            opcode = abs((int) (PR / D8));
            il = base_ops[opcode].opInterLock;
            if ((il) && (WaitForInterlock(il))) {
                sim_debug_interlock(il);  
                continue; // yes, wait for interlock
            }
            // should wait for storage to fetch data?
            bReadData = (base_ops[opcode].opRW & opReadDA) ? 1:0;
            if (bReadData) {
                il=WaitForStorage(AR, &DrumAddrWait);
                if (il==1) sim_debug_interlock(IL_IAS);
                if (il>0) continue; // yes, wait for drum rotation/IAS ready
            }
            sim_debug_interlock(0); // reset flag to allow debug msg for interlock wait

            MachineCycle = 3;             
        }
        // EXEC
        if (MachineCycle == 3) {
            // decode again PR register to reload internal register DA, IA, AR again. Needed if we are executing half cycles  
            opname = DecodeOpcode(PR, &opcode, &DA, &IA);
            AR = DA;
            if (opname == NULL) {
                reason = STOP_UUO; 
                break;
            }
            // even if no data is fetched, DA addr must be a valid one for this opcode
            if (0==IsDrumAddrOk(AR, base_ops[opcode].validDA)) {  
               sim_debug(DEBUG_DETAIL, &cpu_dev, "... %04d: Invalid addr ERROR\n", AR);
               reason = STOP_ADDR;
               break;
            }
            // get data from DA if needed
            bReadData = (base_ops[opcode].opRW & opReadDA) ? 1:0;
            if (bReadData) {
                ReadAddr(AR, &DIST, &DistNegativeZeroFlag);
                sim_debug(DEBUG_DATA, &cpu_dev, "... Read %04d: %06d%04d%c\n", 
                                                           AR,   printfd);
            }
            reason = ExecOpcode(opcode, DA, bFastMode, 
                                &bBranchToDA, 
                                DrumAddr, &CpuStepsUsed);
            if (reason != 0) {
                if ((reason == STOP_PROG) || (reason == STOP_OV)) {
                    // these errors allows to continue program, so set IMachineTime to 'd' to
                    // resume on MacineState=6 instead of MachineState=2
                    IMachineCycle = 'd'; // next cycle (IA) ready to be exec. Take inst addr from AI on PR
                }
                break;
            }

            if (bBranchToDA) IA = DA; 

            MachineCycle = 4;
        }
        // WAIT FOR DATA WRITE
        if (MachineCycle == 4) {
            // should wait to exec the inst (opcode execution) ?
            if (CpuStepsUsed > 0) continue; // yes
            // should wait for storage to store data?
            opcode = abs((int) (PR / D8));
            bWriteDrum = (base_ops[opcode].opRW & opWriteDA) ? 1:0;
            if (bWriteDrum) {
                il=WaitForStorage(AR, &DrumAddrWait);
                if (il==1) sim_debug_interlock(IL_IAS);
                if (il>0) continue; // yes 
            }
            sim_debug_interlock(0); // reset flag to allow debug msg for interlock wait

            MachineCycle = 5; 
        }
        // WRITEBACK
        if (MachineCycle == 5) {
            opcode = abs((int) (PR / D8));
            bWriteDrum = (base_ops[opcode].opRW & opWriteDA) ? 1:0;
            if (bWriteDrum) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Write %04d: %06d%04d%c\n",  
                                                            AR,   printfd);
                if (0==WriteAddr(AR, DIST, DistNegativeZeroFlag)) {
                    reason = STOP_ADDR; 
                    break;
                }
            }
            MachineCycle = 6; 
        }
        // SET ADDR OF NEXT INST:
        if (MachineCycle == 6) {
            // set AR to point to next instr
            AR = IA;   
            IC = AR; 
            CpuSpeed.InstrCount++; // one instruction opcode fully executed

            IMachineCycle = 'I'; // next cycle (IA) ready to be exec. Take inst addr from AR
            MachineCycle = 0; 

            // simulates the machine working on half cycles
            if (bHalfCycle) {        // if I-Half finished, about to exec D-Half
                reason = SCPE_STEP;    // then break beacuse I-Half finished
                break; 
            } 
            // stop if address stop set and match
            if (CSWAddrStop == (int) AR) {
                reason = SCPE_STOP;
                break;
            }

        }

        if (instr_count != 0 && --instr_count == 0) {
            reason = SCPE_STEP; 
            break;
        }

    } /* end while */

    #if defined(CPANEL)
    // post-run refresh
    if ((cpanel_on) && (reason != SCPE_EXIT)) {
        // terminate in-progress I/O card/disk/tape if any
        extern int Refresh_Frames_Count; // frames done counter. At 60 FPS. Wraps after 9942 hours
        extern uint32 tm0CardInReadStacker;
        extern uint32 tm0CardInPunchStacker;
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
            // sense punched card animation tied to refresh
            if ((tm0CardInReadStacker) || (tm0CardInPunchStacker)) {bIOInProgress=1; IOInProgress |= 1; } // card animation in progress
            // update sim events, sense ramac and tape tied to sim events
            if (sim_interval <= 0) sim_process_event();
            sim_interval -= 1;      
            for(i=0;i<4;i++) if (sim_is_active(&dsk_unit[i])) {bIOInProgress=1; IOInProgress |= 2; } // ramac animation in progress
            for(i=0;i<6;i++) if (sim_is_active( &mt_unit[i])) {bIOInProgress=1; IOInProgress |= 4; } // tape animation in progress
            if (bTapeAnimInProgress) {bIOInProgress=1; IOInProgress |= 4;    } // tape animation in progress
            // control cpu speed, slowdown and yield time to OS if needed (with sim_os_ms_sleep)
            GlobalWordTimeCount++;
            Control_CpuSpeed(bFastMode);
            // determine if should return control to scp
            if (sim_os_msec() - tnow > 2000) break; // sanity check: max 2000 msec (2 sec wall time clock) waiting for animation to end
            if (bIOInProgress) continue;                           // some animation in progress, do not exit yet
            if (RefreshCount0 == 0) {                              // draw 2 frames after animation ends to allow
               RefreshCount0 = Refresh_Frames_Count;               // for light to terminate the tickcount based partial 
               if (bShowInfo) bShowInfo=2;                         // assure show 0 ips if info has been enabled with ^I
               continue;                                           // illumination
            }
            if (Refresh_Frames_Count - RefreshCount0 < 2) continue; 
            break;
        }
        for(i=0;i<6;i++) {
            // if any tape not ready because rew sim_activate not finished make the tape ready anyway
            // and cancel pending sim_activate
            if ((mt_ready(i)==0) && (mt_get_last_cmd(i)==OP_RWD)) {
                mt_set_ready(i); 
                IOInProgress |= 4; 
                sim_cancel(&mt_unit[i]);
            }
        }
        if (bShowInfo>1) bShowInfo=1; // restore
        buf[0]=0;
        if (IOInProgress & 1) strcat(buf, "Punched Card/");
        if (IOInProgress & 2) strcat(buf, "RAMAC/");
        if (IOInProgress & 4) strcat(buf, "TAPE/");
        if (buf[0]==0) { strcat(buf, "NO"); } else {buf[strlen(buf)-1]=0; }
        sim_debug(DEBUG_DETAIL, &cpu_dev, "End Wait for %s animation (%d msec)\n",
            buf, sim_os_msec() - tnow);
         Measure_CpuSpeed(0);                  // reset cpu speed measurement because end of run
         sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: reset cpu speed measurement because end of run\n");
    }
    #endif

    {
        t_int64 elapsed;
        int WTS, IPS;
        char sBuf[80];

        Measure_run_CpuSpeed(&elapsed, &WTS, &IPS, sBuf, 1);
        if (elapsed > 0) {
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d WordTimes per second\n", WTS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Speed is x%0.1f relative to original hardware\n", 
                                                 WTS / 10416.0);  // real hw speed: 10416 word times per second
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Instructions executed: %.0f\n", 1.0 * CpuSpeed.InstrCount);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Instructions per second (IPS) achieved\n", IPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Run elapsed %s\n", sBuf);
        }
    }
     
    // flush 407 printout 
    if ((cdp_unit[0].flags & UNIT_ATT) && (cdp_unit[0].fileref)) {
        fflush(cdp_unit[0].fileref); 
    }
    StopReason = reason; // save stop reason to be displayed in control panel

    /* Simulation halted */
    return reason;
}


/* Reset routine */
t_stat cpu_reset(DEVICE * dptr)
{
    vm_init ();

    if (cpu_unit.capac==0) {
        // if no capacity defined, set drum to 2K
        cpu_set_size(&cpu_unit,  MEMAMOUNT(1), NULL, NULL);
    }

    ACC[0] = ACC[1] = DIST = 0;
    PR = AR = OV = 0; 
    AccNegativeZeroFlag = 0;
    DistNegativeZeroFlag = 0;
    IMachineCycle = 'I';
    IC = 0;
    IAS_TimingRing = 0;
    IR[0] = IR[1] = IR[2] = 0;
    StopReason = 0;
    StopIOError = 0;
    memset(&InterLockCount[0], 0, sizeof(InterLockCount)); // clear interlocks

    sim_brk_types = sim_brk_dflt = SWMASK('E');
    CpuSpeed.measurement.WordTimeCountPerSec = 0;

    return SCPE_OK;
}

/* Memory examine */

t_stat cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw)
{
    t_int64  d;
    int NegZero; 
    t_value val;

    if (0==ReadAddr(addr, &d, &NegZero)) {
        return SCPE_NXM;
    }
    if (vptr != NULL) {
        if (NegZero) {
            val = NEGZERO_value; // val has this special value to represent -0 (minus zero == negative zero) 
        } else {
            val = (t_value) d;
        }
        *vptr = val;
    }

    return SCPE_OK;
}

/* Memory deposit */

t_stat cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw)
{
    t_int64 d; 
    int NegZero;

    if (val == NEGZERO_value) {
        d = 0;
        NegZero = 1;
    } else {
        d = val;
        NegZero = 0;
    }

    if (0==WriteAddr(addr, d, NegZero)) {
        return SCPE_NXM;
    }
    return SCPE_OK;
}

t_stat cpu_set_size(UNIT * uptr, int32 val, CONST char *cptr, void *desc)
{
    uint32              i;
    int32               v;

    v = val >> UNIT_V_MSIZE;
    if (v == 0) {v = 1000;} else 
    if (v == 1) {v = 2000;} else 
    if (v == 2) {v = 4000;} else v = 0;
    if ((v <= 0) || (v > MAXDRUMSIZE))
        return SCPE_ARG;
    cpu_unit.flags &= ~UNIT_MSIZE;
    cpu_unit.flags |= val;
    cpu_unit.capac = 9990 + (v / 1000);
    for (i=0;i<MAXDRUMSIZE * 80;i++) 
        DRUM_Symbolic_Buffer[i] = 0;                // clear drum symbolic info
    for (i = DRUMSIZE; i < MAXDRUMSIZE; i++)
        DRUM[i] = DRUM_NegativeZeroFlag[i] = 0;
    for(i = 0; i < 60; i++) IAS[i] = IAS_NegativeZeroFlag[i] = 0;
    return SCPE_OK;
}

t_stat cpu_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr) {
    fprintf (st, "These switches are recognized when examining or depositing in CPU memory:\r\n\r\n");
    fprintf (st, "      -c      examine/deposit characters, 5 per word\r\n");
    fprintf (st, "      -m      examine/deposit IBM 650 instructions\r\n\r\n");
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
        CpuSpeed.WordTimeMax=0;
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
    // machine cycle: 96 microseconds as word time
    MachineCycle_usec = 96; 
    CpuSpeed.WordTimeMax  = (int) (CpuSpeed.msec * CpuSpeed_Acceleration * 1000 / (100.0 * MachineCycle_usec));
    // 10416 Word times per second at x1.0 speed
    CpuSpeed.WordTimeObjectivePerSec = (int) (1000000 * CpuSpeed_Acceleration / (MachineCycle_usec * 100));
    return SCPE_OK;
}

t_stat cpu_show_speed(FILE *st, UNIT *uptr, int32 value, CONST void *desc)
{
    t_int64 elapsed;
    int WTS, IPS;
    char sBuf[80];

    if (CpuSpeed_Acceleration<=0) {
        fprintf (st, "Speed set to MAX\n");
    } else {
        fprintf (st, "Speed set to %0.2f (%d WordTimes per sec), relative to original hardware\n", 
            (float) CpuSpeed_Acceleration / 100,
            CpuSpeed.WordTimeMax * 1000 / CpuSpeed.msec);
    }

    Measure_run_CpuSpeed(&elapsed, &WTS, &IPS, sBuf, 0);
    if (elapsed == 0) {
        fprintf (st, "Measured speed: no data\n");
    } else {
        fprintf (st, "Measured speed: %d WordTimes per second\n", WTS);
        fprintf (st, "                Speed is x%0.1f relative to original hardware\n", WTS / 10416.0);  // real hw speed: 10416 word times per second
        fprintf (st, "                %d Instructions per second (IPS) achieved\n", IPS);
        fprintf (st, "                run elapsed %s\n", sBuf);
    }
    return SCPE_OK; 
}

const char * cpu_description (DEVICE *dptr) {
    return "IBM 650 CPU";
}

