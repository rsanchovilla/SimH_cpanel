/* i701_cpu.c: IBM 701 CPU simulator

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

   cpu          IBM 701 central processor

   From Wikipedia: 

   The IBM 701 Electronic Data Processing Machine, known as the Defense Calculator 
   while in development, was IBM’s first commercial scientific computer and its 
   first series production mainframe computer, which was announced to the public 
   on April 29, 1952. It was invented and developed by Jerrier Haddad and 
   Nathaniel Rochester based on the IAS machine at Princeton.

   The IBM 701 was the first computer in the IBM 700/7000 series, which was responsible 
   for bringing electronic computing to the world and for IBM's dominance in the 
   mainframe computer market during the 1960s and 1970s that continues today.
   The series were IBM’s high-end computers until the arrival of the IBM System/360 
   in 1964.

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
#include "i701_defs.h"
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


// Memory
t_int64             CRT[2048]                        = {0}; // CRT main Memory 
char                CRT_Symbolic_Buffer[4096 * 80]   = {0}; // does not exists on real hw. Used to keep symbolic info 
t_int64             DRUM[4*2048];                           // 4 x 2048 fullwords 36-bits Drum
int                 DRUM_Addr;                              // current drum address selected



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
int                 IC;                          // Instruction Counter
int                 IR;                          // Intruction register
t_int64             ACC;                         // Accumulator
t_int64             MQ;                          // M-Q register
t_int64             MR;                          // Memory register
int                 OV;                          // Overflow flag 
int                 SENSE_OUT;                   // Sense output operator panel lights
int                 SENSE_IN;                    // Sense input operator panel option switches
int                 NCYCLE;                      // indicates current machine cycle to be executed. 

int                 IOADDR = 0;                  // currently/last selected i/o address for operation
int                 IOREMAIN = 0;                // cpu ticks remaining to disconnect IO device
int                 IOTicks = 0;                 // cpu ticks needed to perform current IO operation

uint8               StopReason      = 0;         // saved stop reason to be displayed in control panel

// console ligths and switches that are checked during cpu instruction execution
int Console_Sw_LoadSelector = 0;                 // console load selector: 0=card/1=tape/2=drum
int Console_Sw_AutoManual   = 0;                 // console switch 0=Automatic/1=Manual
int Console_Sw_IR_Entry     = 0;                 // instruction entry
int Console_Sw_MQ_Entry     = 0;                 // M-Q entry

/* CPU data structures

   cpu_dev      CPU device descriptor
   cpu_unit     CPU unit descriptor
   cpu_reg      CPU register list
   cpu_mod      CPU modifiers list
*/

UNIT cpu_unit =
    { UDATA(NULL, 0, 0), 10  };

 
REG cpu_reg[] = {
    {DRDATAD(IC, IC, 12, "Instruction Counter"), REG_FIT},
    {DRDATAD(IR, IR, 18, "Intruction Register"), REG_FIT},
    {DRDATAD(ACC, ACC, 38, "Accumulator"), REG_FIT},
    {DRDATAD(MQ, MQ, 36, "M-Q Register"), REG_FIT},
    {DRDATAD(MR, MR, 36, "Memory Register"), REG_FIT},
    {ORDATAD(OV, OV, 1, "Overflow"), REG_FIT},
    {DRDATAD(SENSE_OUT, SENSE_OUT, 4, "Sense out Lights"), REG_FIT|REG_RO},
    {DRDATAD(SENSE_IN, SENSE_IN, 6, "Sense in option Switches"), REG_FIT},
    {DRDATAD(HALFCYCLE, NCYCLE, 4, "Half Cycle"), REG_FIT|REG_RO},
    {ORDATAD(CPANELGUI, cpanel_gui_supported, 1, "Control Panel GUI supported flag"), REG_FIT|REG_RO},
    {NULL} 
};

MTAB cpu_mod[] = {
    {OPTION_FAST,                0,                     "Real Time execution", "REALTIME",    NULL},
    {OPTION_FAST,                OPTION_FAST,           "Fast Execution",      "FAST",        NULL},
    {OPTION_STEP_HALFCYCLE,      0,                     "Step Instructions",   "STEPINSTR",   NULL},
    {OPTION_STEP_HALFCYCLE,      OPTION_STEP_HALFCYCLE, "Step Half-Cycle",     "STEPHALF",   NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                     "Clear CRT Memory",    "CRTMEMCLEAR", &cpu_clear_crt_mem, NULL},
    {MTAB_XTD|MTAB_VDV|MTAB_NMO, 0,                     "SPEED",               "SPEED",       &cpu_set_speed, &cpu_show_speed},
    {0}
};

DEVICE cpu_dev = {
    "CPU", &cpu_unit, cpu_reg, cpu_mod,
    1, 10, 16, 1, 10, 64,
    &cpu_ex, &cpu_dep, &cpu_reset, NULL, NULL, NULL,
    NULL, DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &cpu_help, NULL, NULL, &cpu_description
};

// return time elapsed between 12-microseconds t_int64 ticks, in form of int msec 
int msec_elapsed(t_int64 nTick1, t_int64 nTick2)    
{
    return ((int) ((nTick2-nTick1) * 12 / 1000));
}

// convert time in millisec to number of 12-microseconds t_int64 tick 
int msec_to_ticks(int msec)
{
    return (int) (abs(msec) * 1000 / 12); 
}

// convert time in microsec to number of 12-microseconds t_int64 tick 
int usec_to_ticks(int usec)
{
    return (int) (abs(usec) / 12); 
}


// input: d is 36bits fullword or 18bits half word in ibm 701 format
//        addr is -4095 .. 4096. if addr==-4096 -> write full word at addr 0 (as addr was -0)
// return 0 if write addr invalid
//        MR is updated with 36 bits word writen to CRT
int WriteAddr(int addr, t_int64 d, char * msg)
{
    if ((addr > 4095) || (addr < -4096)) return 0;
    if (addr < 0) {
        // write full word
        if (addr == -4096) addr=0;
        CRT[ (-addr) >> 1] = MR = d; 
        if (msg) {
            sim_debug(DEBUG_DATA, &cpu_dev, "%s %04d: %c%05d%06d (octal %04o: %06o%06o) \n", 
                msg, -addr, printfw(d), -addr, printfw_oct(d));
        }
    } else {
        MR=(d & 0777777);
        if (addr & 1) {
             // write right half word
            CRT[addr >> 1] = (CRT[addr >> 1] & 0777777000000) | MR; 
        } else {
             // write left half word
            CRT[addr >> 1] = (MR << 18) | (CRT[addr >> 1] & 0777777) ; 
        }
        MR = (MR << 18); 
        if (msg) {
            sim_debug(DEBUG_DATA, &cpu_dev, "%s %04d: %c%d (octal %04o: %06o) \n", 
                msg, addr, (char) ( ((d) >> 17) ? '-':'+'), (int) (d & 0377777), addr, (int) d);
        }
    }
    return 1;
}

// return 0 if read addr invalid
//        d is 36bits fullword or 18bits half word in ibm 701 format
//        MR is updated with 36 bits word readed from CRT
int ReadAddr(int addr, t_int64 * d, char * msg)
{
    if ((addr > 4095) || (addr < -4096)) return 0;
    if (addr < 0) {
        // read full word
        if (addr == -4096) addr=0;
        *d = MR = CRT[ (-addr) >> 1]; 
        if (msg) {
            sim_debug(DEBUG_DATA, &cpu_dev, "%s %04d: %c%05d%06d (octal %04o: %06o %06o) \n", 
                msg, -addr, printfw(MR), -addr, printfw_oct(MR));
        }
    } else {
        if (addr & 1) {
            // return right half word
            *d = MR = ( CRT[addr >> 1] & 0777777);
        } else {
            // return left half word
            *d = MR = ( CRT[addr >> 1] >> 18 );
        }
        if (msg) {
            sim_debug(DEBUG_DATA, &cpu_dev, "%s %04d: %c%d (octal %04o: %06o) \n", 
                msg, addr, (char) ( ((*d) >> 17) ? '-':'+'), (int) ((*d) & 0377777), addr, (int) (*d));
        }
        // MR has the 18bits reads on ibm 701 lower bits: bits on left part of word
        MR = (MR << 18); 
    }
    return 1;
}



// opcode decode 
//         returns entry in base_ops table

CONST char * DecodeInst(int d, int * opcode, int * opaddr, int bReturnOpname)
{
    CONST char * opname = NULL;
    int i, neg; 

    neg = (d >> 17) ? 1:0;
    *opcode = (d >> 12) & 31;
    if ((*opcode == 13) && (neg)) *opcode = 32+13; 
    *opaddr = d & 07777;
    if (neg) *opaddr = -*opaddr; 
    
    if (bReturnOpname) {
        for (i=0; base_ops[i].name; i++) {
            if (base_ops[i].opbase == *opcode) {
                opname  = base_ops[i].name;
                break;
            }
        }
    }
    
    return opname;
}

// shift right 2+35+35 bits
void ShiftRight(t_int64 * hi, t_int64 * lo, int n)
{
    while (n>0) { // shift right
        if ((*hi==0) && (*lo==0)) break; 
        *lo = ((*hi & 1) << 34) | (*lo >> 1); 
        *hi = (*hi >> 1); 
        n--;
    }
}

// shift left 2+35+35 bits
// if a 1 crosses 35bit boundary of hi during shift, return 1
int ShiftLeft(t_int64 * hi, t_int64 * lo, int n)
{
    int ovbit=0; 
    while (n>0) { // shift left
        if ((*hi==0) && (*lo==0)) break; 
        if ((*hi >> 34) & 1) ovbit=1;
        *hi = (*hi << 1) | (*lo >> 34); 
        *lo = (*lo << 1) & MASK35BITS; 
        n--;
    }
    return ovbit; 
}
        
// Add int d to int n, check overflow
int AddSetOv(t_int64  * d, t_int64 n) 
{
    int ov=0; 
    // determine of sum overflows 35 bits value
    if ((n>0) && (*d>0)) {
        if (((n & MASK35BITS) + (*d & MASK35BITS)) >> 35) ov=1;
    } else if ((n<0) && (*d<0)) {
        if ((((-n) & MASK35BITS) + ((-*d) & MASK35BITS)) >> 35) ov=1;
    }
    *d=*d + n; 
    return ov; 
}

// desconnect IO operations, terminating the data transfer 
// set IOADDR to zero
void DisconnectIO(void)
{
    int ioaddr, ioop, n; 

    if (IOADDR == 0) return; // not connected to any io device

    GetIOADDR(&ioaddr, &ioop);

    if (ioaddr == 2048) {
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from Card Reader \n");
        cdr_cmd(cdr_unit, OP_STOP, 0);
    } else if (ioaddr == 1024) {
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from Card Punch \n");
        cdp_cmd(cdp_unit, OP_STOP, 0);
    } else if (ioaddr == 512) {
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from Printer \n");
        lp_cmd(lp_unit, OP_STOP, 0);
    } else if ((ioaddr >=  256) && (ioaddr <= 259)) {
        n=ioaddr &3; 
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from Tape %d \n", n);
        mt_cmd(&mt_unit[n], OP_STOP, 0);
    } else if ((ioaddr >=  128) && (ioaddr <= 131)) {
        // disconned from drum
        n=ioaddr &3; 
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from Drum %d \n", n);
    } else {
        sim_debug(DEBUG_DATA, &cpu_dev, "... Disconect from unknow device at IO addr %d \n", ioaddr);

    }
    IOADDR=0;
    IOREMAIN=0;                // cpu ticks remaining to disconnect IO device

}

// pack/unpack IO Addr and IO operation
void SetIOADDR(int opaddr, int opcode)
{
    char cbuf[4];
    int ioaddr; 

    opaddr = abs(opaddr); // ignore sign of instr
    if ((opaddr == 2052) && (opcode == OP_WRITE) ) {
        // delay instruction: do the folllowing things as side effect:
        // - wait so MQ finisheds its transfer (if any in progress) and 
        // - disconect device from MQ. A new READ/WRITE should be issued 
        ioaddr = IOADDR & 07777;
        if (ioaddr == 0) {
           sim_debug(DEBUG_DATA, &cpu_dev, "... No Delay: no IO device connected \n");
        } else {
           sim_debug(DEBUG_DATA, &cpu_dev, "... Delay: Wait until MQ disconnect from IO Address %d\n", ioaddr);
        }
        DisconnectIO(); 
        return; 
    }
        
    DisconnectIO(); 
    IOADDR = opaddr + ((opcode & 31) << 12); 

    cbuf[0]='0'+(opaddr&3);
    cbuf[1]=0; 

    sim_debug(DEBUG_DATA, &cpu_dev, "... Set IO Address %d (%s%s)\n", opaddr,
                (opaddr == 2048) ? "Card Reader" : 
                (opaddr == 1024) ? "Card Punch" : 
                (opaddr ==  512) ? "Printer" : 
                ((opaddr >=  256) && (opaddr <= 259)) ? "Tape " : 
                ((opaddr >=  128) && (opaddr <= 131)) ? "Drum " : 
                "??? unknown", 
                ( ((opaddr >=  256) && (opaddr <= 259)) || ((opaddr >=  128) && (opaddr <= 131)) ) ? cbuf : 
                "");
}

void GetIOADDR(int * ioaddr, int * ioop)
{
    if (ioaddr) *ioaddr = IOADDR & 07777;
    if (ioop)   *ioop   = (IOADDR >> 12) & 31; 
}

// drum has no device. This function services READ/WRITE to drum ans set drum address
void dr_cmd(int unit, int cmd, int addr)
{
    if (cmd == OP_SET_DR) {
        DRUM_Addr=abs(addr); // ignore address sign
        DRUM_Addr=(DRUM_Addr & 07776) + 0x8000;  // make drum addr even, mark as addr just set
        sim_debug(DEBUG_DATA, &cpu_dev, "Set Drum Address %d \n", DRUM_Addr & 07776);
        IOREMAIN=0; // any number of instr can be exec after SET DRUM instr and frist COPY
    } else {
        if ((DRUM_Addr & 0x8000) != 0) { 
            // this is the first COPY issued after SET DR 
            // calc delay based on drum rotation. 
            // delay is 0 msec for Drum addr % 32 == 0, is 20 msec for Dum Addr % 32 == 31
            // each 1250 msec drum rotates one addr. n is rotation in range 0..31
            int n = (GlobalTicksCount / usec_to_ticks(1250)) & 31; 
            IOREMAIN+=usec_to_ticks(20*1000) * n / 32; 
        }
        addr=(DRUM_Addr & 07776); // make sure is even, remove makr as dr addr just been set
        if (cmd == OP_READ) {
             sim_debug(DEBUG_DATA, &cpu_dev, "Read Drum %d Address %d \n", unit, addr);
             MQ=DRUM[unit * 2048 + (addr / 2)];
        } else if (cmd == OP_WRITE) {
             sim_debug(DEBUG_DATA, &cpu_dev, "Write Drum %d Address %d \n", unit, addr);
             DRUM[unit * 2048 + (addr / 2)]=MQ;
        }
        // incr current drum addr to point to next word
        DRUM_Addr=(addr+2) & 07776;
        // drum timing
        // calc wait time due to drum rotation. Must wait drum make availabe selected address
        // transfer rate: 800 words per sec -> 1.25 msec per word, 1 msec of usefull calculation
        IOTicks = usec_to_ticks(250) + IOREMAIN; 
        IOREMAIN += usec_to_ticks(1250); 
    }
}

// opcode execution 
// input: fast mode flag, parts of instruction: opcode opaddr
// output: CpuTicksUsed: incremented with number of clock ticks used on execution
//         cTransferTo: set to 'N' for next instr, 'A' transfer to addr, '1' '2' to transfer skip 1 or 2 next instrs
t_stat ExecOpcode(int opcode, int opaddr, 
                  int bFastMode,  
                  char * cTransferTo, 
                  int * CpuTicksUsed)
{
    int n, i, sgn, neg;
    t_int64 d,acc, lo, hi;  
    int ioaddr, ioop, unit; 
    t_stat r; 

    // minus zero is addr -4096
    if ((opaddr==0) && (IR >> 17)) opaddr=-4096; 
    *CpuTicksUsed += (base_ops[opcode & 31].nCycles - 2); 

    *cTransferTo='N';
    switch(opcode) {
        case OP_NOOP:        //  8  // No Operation
            break;
        case OP_STOP:        //  0  // Stop and transfer
            *cTransferTo='A';
            return STOP_PROG;
            break;
        case OP_TR:          //  1  // Transfer 
            *cTransferTo='A';
            break;
        case OP_TR_OV:       //  2  // Transfer on overflow: Transfer if OV=1 (and reset ov)
            sim_debug(DEBUG_DATA, &cpu_dev, "... Check OV: %d\n", OV);
            if (OV) {
                OV=0;        // reset overflow indicator when tested
                sim_debug(DEBUG_DATA, &cpu_dev, "OV Set -> Transfer\n");
                *cTransferTo='A';
            }
            break;
        case OP_TR_PLUS:     //  3  // Transfer on plus: Transfer if ACC>0 
            n = (ACC & ACCSGN) ? 1:0;
            sim_debug(DEBUG_DATA, &cpu_dev, "... Check ACC sign bit: %d\n", n);
            if (n==0) {
                sim_debug(DEBUG_DATA, &cpu_dev, "Sign not set, ACC is positive -> Transfer\n");
                *cTransferTo='A';
            }
            break;
        case OP_TR_ZR:       //  4  // Transfer on zero: Transfer if ACC=0 
            sim_debug_acc_ov; 
            if ((ACC & ~ACCSGN) ==0) { // also branch if ACC = -0 (minus zero)
                *cTransferTo='A';
            }
            break;
        case OP_SUB:         //  5  // Subtract: Acc = Acc - C(x)
        case OP_R_SUB:       //  6  // Reset and Subtract: Acc = - C(x)
        case OP_SUB_AB:      //  7  // Subtract Absolute value: Acc = Acc - |C(x)|
        case OP_ADD:         //  9  // Add: Acc = Acc + C(x)
        case OP_R_ADD:       // 10  // Reset and Add: Acc = C(x)
        case OP_ADD_AB:      // 11  // Add Absolute value: Acc = Acc + |C(x)|
            // get C(x). Use MR that holds the 36bits read data (either half or full word)
            ReadAddr(opaddr, &d, "... Read");
            // get operand sign and magnitude
            d = MR & MASK35BITS;  // get magnitude of operand            
            sgn = (MR & SGN) ? 1 : 0;
            if ((opcode != OP_SUB_AB) && (opcode != OP_ADD_AB)) {
                if (sgn) d=-d; // get sign of operand
            }
            // get acc sign and magnitude
            if ((opcode == OP_R_SUB) || (opcode == OP_R_ADD)) {
                acc=0;
                neg = (opcode == OP_R_ADD) ? sgn:1-sgn; 
            } else {
                acc = ACC & MASK37BITS;     // get magnitude of acc, including PQ overflow bits
                neg = (ACC & ACCSGN) ? 1:0; // get sign of acc
                if (neg) acc=-acc; // set sign of acc
            }
            // is subtraction change operand sign
            if (opcode <= OP_SUB_AB) d=-d; 
            // arith operation
            if (AddSetOv(&acc, d)) OV=1;
            // set ACC with sign and magnitude
            if (acc < 0) {
                ACC = ACCSGN | (-acc & MASK37BITS);
            } else if (acc == 0) {
                // if result is zero, sign of result is sign of accum before add/sub
                ACC = (neg) ? ACCSGN:0;  
            } else {
                ACC = (acc & MASK37BITS);
            }
            // set debug info 
            sim_debug_acc_ov; 
            break;
        case OP_MPY:         // 16  // Multiply: Acc:MQ = C(x) * MQ. Acc=35 most significants bits + sign
        case OP_MPY_R:       // 17  // Multiply and Round: same as above, followed by round
            // get C(x). Use MR that holds the 36bits read data (either half or full word)
            ReadAddr(opaddr, &d, "... Read");
            // get operand sign and magnitude
            d = MR & MASK35BITS;  // get magnitude of operand
            sgn = (MR & SGN) ? 1:0; // get sign of operand
            // get multiplier sign and magnitude
            sim_debug(DEBUG_DATA, &cpu_dev, "... Mult by MQ: %c%05d%06d \n", printfw(MQ));
            lo = MQ & MASK35BITS;  // get magnitude of operand
            sgn = (MQ & SGN) ? 1-sgn:sgn; // get sign of mult result
            // get in hi upper 19 bits, lo holds only lower 16 bits
            hi=lo >> 15; lo=lo & 077777;
            // multiply to obtain higher and lower part of result
            lo = d * lo; // resulting lo has 35 bits+15 bits = 50 bits result (lower part)
            hi = d * hi; // resulting hi has 35 bits+20 bits = 55 bits result (high part)
            // compose 70bit result in ACC (hight part) and MQ (lower part)
            MQ  = ((hi << 15) + lo) & MASK35BITS; // lower 35 bits
            ACC = ((hi + (lo >> 15)) >> 20) & MASK35BITS; // upper 35 bits
            // round if asked for
            if (opcode == OP_MPY_R) {
                if (MQ >> 34) { // if MSB of MQ is 1 -> incr ACC
                    ACC++; // no worries as overflow PQ bits are zero
                    if (ACC >> 35) OV=1;
                }
            }
            // add sign bit if result is negative
            if (sgn) {
                MQ  |= SGN; 
                ACC |= ACCSGN; 
            }
            // set debug info 
            sim_debug_acc_mq_ov; 
            break;
        case OP_DIV:         // 18  // Divide: MQ = Acc:MQ / C(x), Acc=remainder   
            // get C(x). Use MR that holds the 36bits read data (either half or full word)
            sim_debug_acc_mq_ov;
            ReadAddr(opaddr, &d, "... Div by Read");
            // get divisor sign and magnitude
            d = MR & MASK35BITS;  // get magnitude of divisor
            sgn = (MR & SGN) ? 1:0; // get sign of divisor
            if (d==0) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Div by ZERO\n");
                return STOP_DIVBYZERO; 
            }
            // get divident sign and magnitude
            hi = ACC & MASK37BITS;  // get magnitude of divident (hi part)
            lo = MQ & MASK35BITS;   // get magnitude of divident (lo part)
            sgn = (ACC & ACCSGN) ? 1-sgn:sgn; // get sign of div result
            if (hi>d) {
                // dividend too big. Quotiend does not if in 35 bits acc
                sim_debug(DEBUG_DATA, &cpu_dev, "... Div Check\n");
                return STOP_DIVCHECK; 
            }
            MQ=0;  // quotient
            ACC=0; // remainder
            for(i=70-1;i>=0;i--) {
                ACC = ACC << 1; 
                if (ShiftLeft(&hi, &lo, 1)) ACC += 1;
                if (ACC >= d) {
                    ACC = ACC - d; 
                    MQ |= (1LL << i); 
                }
            }
            // add sign bit if result is negative
            if (sgn) {
                MQ  |= (1LL << 35); 
                ACC |= (1LL << 37); 
            }
            // set debug info 
            sim_debug_acc_mq_ov; 
            break;
        case OP_ROUND:       // 19  // Round: if leftmost bit of MQ=1 incr Acc magnitude
            if ((MQ >> 34) & 1) { // if MSB of MQ is 1 -> incr ACC magnitude, including PQ bits
                acc = ACC & MASK37BITS; 
                if (AddSetOv(&acc, 1)) OV=1; 
                ACC=(ACC & ACCSGN) | (acc & MASK37BITS); 
                // set debug info 
                sim_debug(DEBUG_DATA, &cpu_dev, "... Rounded \n");
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Not Rounded \n");
            }
            sim_debug_acc_ov; 
            break;
        case OP_STORE:       // 12  // Store: Store accumulator at given address: C(x)= Acc
            d = ((ACC & ACCSGN) >> 2) | (ACC & MASK35BITS); // remove PQ overflow bits, put acc sign in bit36
            if (opaddr >= 0) d = d >> 18; // half word is stored on left part of acc 
            WriteAddr(opaddr, d, "... Write"); 
            break; 
        case OP_STORE_A:     // 13  // Store Address: Store 12 rightmost bits from Acc at addr x (halfword)
            ReadAddr(opaddr, &d, "... Read");
            d = MR >> 18; 
            d = (d & 0770000) | (ACC >> 18) & 07777; // get left halfword on acc, get address part (12 bits)
            WriteAddr(opaddr, d, "... Write"); 
            break; 
        case OP_EXTR:        // (13+32) // Extract: C(x) = C(x) AND Acc
            ReadAddr(opaddr, &d, "... Read");
            acc = ((ACC & ACCSGN) >> 2) | (ACC & MASK35BITS); // remove PQ overflow bits, put acc sign in bit36
            d = acc & MR; 
            WriteAddr(opaddr, d, "... Write"); 
            break; 
        case OP_STORE_MQ:    // 14  // Store MQ: C(x) = MQ
            d = (opaddr >= 0) ? MQ >> 18 : MQ; // half word is stored on left of MQ
            WriteAddr(opaddr, d, "... Write"); 
            break; 
        case OP_LOAD_MQ:     // 15  // Load MQ: MQ = C(x)
            ReadAddr(opaddr, &d, "... Read");
            MQ = MR;
            break; 
        case OP_A_LEFT:      // 22  // Accumulator Left Shift: Acc << x, Acc sign unchanged
            n = IR & 255; // number of shifs
            hi = ACC & ~ACCSGN;    // remove sign, keep PQ overflow bits
            lo = 0;
            if (ShiftLeft(&hi, &lo, n)) OV=1; 
            ACC = (ACC & ACCSGN) | (hi & MASK37BITS);        // keep current ACC sign
            sim_debug_acc_ov; 
            break;
        case OP_A_RIGHT:     // 23  // Accumulator Right Shift: Acc >> x, Acc sign unchanged
            n = IR & 255; // number of shifs
            hi = ACC & ~ACCSGN;    // remove sign, keep PQ overflow bits
            lo = 0;
            ShiftRight(&hi, &lo, n); 
            ACC = (ACC & ACCSGN) | hi;        // keep current ACC sign
            sim_debug_acc_ov; 
            break;
        case OP_L_LEFT:      // 20  // Long Left Shift: Acc:MQ << x, ACC sign taken from MQ
            n = IR & 255; // number of shifs
            hi = ACC & ~ACCSGN;    // remove sign, keep PQ overflow bits
            lo = MQ  & MASK35BITS; // remove sign
            if (ShiftLeft(&hi, &lo, n)) OV=1; 
            ACC = ((MQ & SGN) << 2) | (hi & MASK37BITS);      // set same sign as MQ
            MQ  = (MQ & SGN) | lo;            // keep current MQ sign
            sim_debug_acc_mq_ov; 
            break;
        case OP_L_RIGHT:     // 21  // Long Right Shift: Acc:MQ >> x, MQ sign taken from Acc 
            n = IR & 255; // number of shifs
            hi = ACC & ~ACCSGN;    // remove sign, keep PQ overflow bits
            lo = MQ  & MASK35BITS; // remove sign
            ShiftRight(&hi, &lo, n); 
            MQ  = ((ACC & ACCSGN) >> 2) | lo; // set same sign as ACC
            ACC = (ACC & ACCSGN) | hi;        // keep current ACC sign
            sim_debug_acc_mq_ov; 
            break;
        case OP_READ_B:      // 25  // Prepare to Read Backward. Start mechanical movement backward
        case OP_WRITE:       // 26  // Prepare to Write. Start mechanical movement 
        case OP_READ:        // 24  // Prepare to Read. Start mechanical movement forward
        case OP_WRITE_EF:    // 27  // Write End of File. 
        case OP_REWIND:      // 28  // rewind, of course
            // device addresses:  tape units (1 to 4): 0256  0257 0258 0259  
            //                    drums (1 to 4)     : 0128  0129 0130 0131
            //                    printer            : 0512
            //                    card reader        : 2048
            //                    card punch         : 1024
            r = SCPE_OK; 
            SetIOADDR(opaddr, opcode);
            GetIOADDR(&ioaddr, &ioop);
            IOTicks=0;
            unit = ioaddr & 3; 
            if (ioop == OP_READ) {
                if (ioaddr==2048) {
                    r=cdr_cmd(cdr_unit, OP_READ, 0);
                } else if ((ioaddr>=256) && (ioaddr<=259)) {
                    MQ=0; // read from tape clears MQ
                    r=mt_cmd(&mt_unit[unit], OP_READ, 0);
                } else if ((ioaddr>=128) && (ioaddr<=131)) {
                    dr_cmd(unit, OP_SET_DR, 0); // read from drum -> reset drum addr 
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... WARNING: OPCODE interpreted as NOOP because invalid IO Address %d, Next COPY will fail \n", ioaddr);
                    DisconnectIO();
                }
            } else if (ioop == OP_READ_B) {
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    MQ=0; // read from tape clears MQ
                    r=mt_cmd(&mt_unit[unit], OP_READ_B, 0);
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... WARNING: OPCODE interpreted as NOOP because invalid IO Address %d, Next COPY will fail \n", ioaddr);
                    DisconnectIO();
                }
            } else if (ioop == OP_WRITE) {
                if (ioaddr==2052) {
                    // delay instruction: do the folllowing things as side effect:
                    // - wait so MQ finisheds its transfer (if any in progress) and 
                    // - disconect device from MQ. A new READ/WRITE should be issued 
                    DisconnectIO(); 
                } else if (ioaddr==1024) {
                    r=cdp_cmd(cdp_unit, OP_WRITE, 0);
                } else if (ioaddr==512) {
                    r=lp_cmd(lp_unit, OP_WRITE, 0);
                } else if ((ioaddr>=256) && (ioaddr<=259)) {
                    r=mt_cmd(&mt_unit[unit], OP_WRITE, 0);
                } else if ((ioaddr>=128) && (ioaddr<=131)) {
                    dr_cmd(unit, OP_SET_DR, 0); // write to drum -> reset drum addr
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... WARNING: OPCODE interpreted as NOOP because invalid IO Address %d, Next COPY will fail \n", ioaddr);
                    DisconnectIO();
                }
            } else if (ioop == OP_WRITE_EF) {
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    r=mt_cmd(&mt_unit[unit], OP_WRITE_EF, 0);
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... WARNING: OPCODE interpreted as NOOP because invalid IO Address %d, Next COPY will fail \n", ioaddr);
                    DisconnectIO();
                }
            } else if (ioop == OP_REWIND) {
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    r=mt_cmd(&mt_unit[unit], OP_REWIND, 0);
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... WARNING: OPCODE interpreted as NOOP because invalid IO Address %d, Next COPY will fail \n", ioaddr);
                    DisconnectIO();
                }
            }
            *CpuTicksUsed += IOTicks; 
            if (r) {
                // if error in tape signal tape check
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    mt_info[unit].TapeCheck = 1;
                }
                // return on error
                return r;  
            }
            if (IOREMAIN) {
               sim_debug(DEBUG_DATA, &cpu_dev, "... Execution time for I/O instr: %d ticks (%d msec)\n"
                                               "... program should exec first COPY instr in following %d ticks (%d msec)\n",
                                                    IOTicks, msec_elapsed(0, IOTicks),
                                                    IOREMAIN-IOTicks, msec_elapsed(0, IOREMAIN-IOTicks));
            }
            break; 
        case OP_COPY:        // 31  // Copy and Skip
            r = SCPE_OK; 
            GetIOADDR(&ioaddr, &ioop);
            unit = ioaddr & 3; 
            if (ioaddr==0) {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Not connected to an IO device\n");
                r=STOP_COPYCHECK; 
                return r; 
            }
            sim_debug(DEBUG_DATA, &cpu_dev, "... COPY will wait for IOREMAIN=%d (%d msec) for device ready\n", 
                                                 IOREMAIN, msec_elapsed(0, IOREMAIN));
            if (ioop == OP_READ) {
                if (ioaddr==2048) {
                    r=cdr_cmd(&cdr_unit[0], OP_COPY, 0);
                rdrec:
                    if (r==SCPE_EOF) {
                        // end of file 
                        sim_debug(DEBUG_DATA, &cpu_dev, "End of file sensed -> Skip next instruction\n");
                        *cTransferTo='1'; // skip next instr
                        r = SCPE_OK; 
                    } else if (r==SCPE_RO) {
                        // end of record (end of card)
                        sim_debug(DEBUG_DATA, &cpu_dev, "End of record sensed -> Skip next two instructions\n");
                        *cTransferTo='2'; // skip two next instr
                        r = SCPE_OK; 
                    } else if (r==SCPE_OK) {
                        // MQ cointains the read fullword. Store at opaddr
                        if (opaddr > 0) {
                            // if storing halfword, on odd addr store right 18 bits from MQ
                            //                         even addr store left 18 bits from MQ
                            d = (opaddr & 1) ? MQ & 0777777 : MQ >> 18; 
                        } else d = MQ; 
                        WriteAddr(opaddr, d, "... MQ to Mem"); 
                    } else {
                        r=STOP_COPYCHECK;                     
                    } 
                } else if ((ioaddr>=256) && (ioaddr<=259)) {
                    r=mt_cmd(&mt_unit[unit], OP_COPY, 0);
                    goto rdrec; 
                } else if ((ioaddr>=128) && (ioaddr<=131)) {
                    dr_cmd(unit, OP_READ, 0);
                    r = SCPE_OK; 
                    goto rdrec; 
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... Invalid IO Address %d \n", ioaddr);
                    r=STOP_COPYCHECK; 
                }
            } else if (ioop == OP_READ_B) {
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    r=mt_cmd(&mt_unit[unit], OP_COPY, 0);
                    goto rdrec; 
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... Invalid IO Address %d \n", ioaddr);
                    r=STOP_COPYCHECK; 
                }
            } else if (ioop == OP_WRITE) {
                if (ioaddr==1024) {
                    ReadAddr(opaddr, &d, "... Mem to Card Punch");
                    MQ = MR;
                    r=cdp_cmd(cdp_unit, OP_COPY, 0);
                    if (r!=SCPE_OK) {
                        r=STOP_COPYCHECK;                     
                    }
                } else if (ioaddr==512) {
                    ReadAddr(opaddr, &d, "... Mem to Printer");
                    MQ = MR;
                    r=lp_cmd(lp_unit, OP_COPY, 0);
                } else if ((ioaddr>=256) && (ioaddr<=259)) {
                    ReadAddr(opaddr, &d, "... Mem to Tape");
                    MQ = MR;
                    r=mt_cmd(&mt_unit[unit], OP_COPY, 0);
                } else if ((ioaddr>=128) && (ioaddr<=131)) {
                    ReadAddr(opaddr, &d, "... Mem to Drum");
                    MQ = MR;
                    dr_cmd(unit, OP_WRITE, 0);
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... Invalid IO Address %d \n", ioaddr);
                    r=STOP_COPYCHECK; 
                }
            } else {
                r=SCPE_IERR; 
            }
            *CpuTicksUsed += IOTicks; 
            if (r) {
                // if error in tape signal tape check
                if ((ioaddr>=256) && (ioaddr<=259)) {
                    mt_info[unit].TapeCheck = 1;
                }
                // return on error
                return r;  
            }
            sim_debug(DEBUG_DATA, &cpu_dev, "... Execution time for COPY instr: %d ticks (%d msec)\n"
                                            "... program should exec next COPY instr in following %d ticks (%d msec)\n",
                                                 IOTicks, msec_elapsed(0, IOTicks),
                                                 IOREMAIN-IOTicks, msec_elapsed(0, IOREMAIN-IOTicks));
            break;
        case OP_SENSE:       // 30  // Sense and Skip
            // device addresses:  printer input          : 0522
            //                    printer output         : 0512..0521
            //                    card punch input       : none
            //                    card punch output      : 1024, 1025
            //                    operator's panel input : 0069..0074 - sense input switches - operator panel option switches number 1 to 6
            //                    operator's panel output: 0065..0068 - operator panel neon lights
            if ((opaddr >= 69) && (opaddr <= 74)) {
                // Sense input operator panel option switches number 1 to 6
                n=opaddr-69+1;
                if (SENSE_IN & (1 << (n-1))) {
                    *cTransferTo='1';
                    sim_debug(DEBUG_DATA, &cpu_dev, "... Option Switch %d set -> Skip next instruction\n", n);
                } else {
                    sim_debug(DEBUG_DATA, &cpu_dev, "... Option Switch %d not set\n", n);
                }
            } else if ((opaddr >= 65) && (opaddr <= 68)) {
                // turn on operator panel lights 1 to 4
                n = opaddr-65+1; 
                SENSE_OUT |= (1 << (n-1));
                sim_debug(DEBUG_DATA, &cpu_dev, "... Operator Panel neon light %d set to ON\n", n);
            } else if (opaddr == 64) {
                // turn off all operator panel lights (1 to 4)
                SENSE_OUT=0;
                sim_debug(DEBUG_DATA, &cpu_dev, "... All operator Panel neon light OFF \n");
            } else if ((opaddr >= 512) && (opaddr <= 521)) {
                n = opaddr-512+1; 
                sim_debug(DEBUG_DATA, &cpu_dev, "... Set printer hub %d\n", n);
                lp_cmd(lp_unit, OP_SENSE + (n << 5), 0);
            } else if (opaddr == 522) {
                // sense printer input hub ob -> test presence of printer control panel board
                *cTransferTo='1';
                sim_debug(DEBUG_DATA, &cpu_dev, "... Input printer hub set -> Skip next instruction\n");
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Invalid SENSE address\n");
                return STOP_COPYCHECK; 
            }
            break; 
        case OP_SET_DR:      // 29  // Ser Drum Address
            GetIOADDR(&ioaddr, &ioop);
            unit = ioaddr & 3; 
            if ((ioaddr>=128) && (ioaddr<=131)) {
                dr_cmd(unit, OP_SET_DR, opaddr); // set DR address
            } else {
                sim_debug(DEBUG_DATA, &cpu_dev, "... Not connected to drum (IO Address is %d). SET DR interpreted as NOOP\n", ioaddr);
            }
            break;
        default:
            return STOP_UUO;
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


// symbolc trace commands: have the form of **cmd as comment in load'ed file
// Symbolic_Trace_tm0=sim_os_msec() when trace starts
// sTrace points to **cmd string
// return 1 if must reset Symbolic_Trace_tm0 value
int Symbolic_Trace(uint32 Symbolic_Trace_tm0, char * Symbolic_Buffer, char * sTrace)
{
    uint32 msec; 
    int trace, addr, len, reset_tm0, octal;
    t_int64 d; 
    char buf[300];
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
        // the "**echo xxx[:[f|q|o]]" string in symbolic buffer prints on console xxx (cannot have spaces)
        // if ":" is present, append contents of ACC (left halfword decimal)
        // if "f" is present, print ACC as fullword (both octal and decimal)
        // if "o" is present print ACC left halfword in octal numbers
        // if "q" is present print MQ fullword instead of ACC (both octal and decimal)
        sTrace +=6;
        addr=len=octal=0;
        while (c=*sTrace++) {
            if (c==32) {
                if (len==0) continue; // skip leading spaces
                break; // space after xxx ends string to echo
            }
            if (c == ':') addr=1; // signal to append ACC as left haflword (decimal)
            if ((c == 'o') && (addr > 0)) octal=1; // signal to append ACC as left haflword (octal)
            if ((c == 'f') && (addr > 0)) addr=2; // signal to append ACC as fullword
            if ((c == 'q') && (addr > 0)) addr=3; // signal to append MQ as fullword
            if (len>20) break; 
            buf[len++] = c; 
        }
        buf[len]=0;
        if (addr==1) { 
            d = (ACC & MASK35BITS) >> 18; 
            if (ACC & ACCSGN) {
                if (octal) d = d + 0400000;
                else       d = -d; 
            }
            if (octal) sprintf(&buf[len], " %06o", (int) d);
            else       sprintf(&buf[len], " %d", (int) d);
        } else if (addr==2) {
            sprintf(&buf[len], " %c%d%06d (%c%d|%c%d octal %d %d%d %06o|%06o)", 
                                 printfw_acc(ACC));
        } else if (addr==3) {
            sprintf(buf+len, " %c%05d%06d  (octal %06o %06o)", 
                                 printfw(MQ), printfw_oct(MQ));
        }
        trace = 1;
    } else if(strstr(sTrace, "**regs")) {
        // the "**regs" string in symbolic buffer prints on console the registers
        // the values are the ones BEFORE opcode execution
        sprintf(&buf[0], "ACC: %c%d%06d (%c%d|%c%d octal %d %d%d %06o|%06o), \n"
                         "                    OV: %d, MQ: %c%05d%06d (octal %06o %06o)", 
                               printfw_acc(ACC), OV, printfw(MQ), printfw_oct(MQ));

        trace = 1;
    } else if(strstr(sTrace, "**m")) {
        // the "**m[o]NNNN[-LL][f][:xxxx]" string in symbolic buffer prints on console the contents
        // of crt memory starting at NNNN. prints LL half words (defaults to 1). 
        // if "f" is present, print fullwords
        // if "o" is present, NNNN and LL are octal numbers
        // if ":" is present, prints on console xxx (cannot have leading spaces, nor embebbed spaces, max 20 chars)
        sTrace +=3;
        addr=len=octal=0;
        if (*sTrace=='o') {
            sTrace++;
            octal=1; 
        }
        while (c=*sTrace++) {
            if (octal) {
                if ((c<'0') || (c>'7')) break;
                addr = addr * 8 + c - '0';
            } else {
                if ((c<'0') || (c>'9')) break;
                addr = addr * 10 + c - '0';
            }
        }
        if (c=='-') while (c=*sTrace++) {
            if (octal) {
                if ((c<'0') || (c>'7')) break;
                len = len * 8 + c - '0';
            } else {
                if ((c<'0') || (c>'9')) break;
                len = len * 10 + c - '0';
            }
        }
        if (len==0) len=1;
        if (c=='f') {addr=-(addr & 0777776); c=*sTrace++; }
        sprintf(buf, "Mem dump %d", addr);
        if (c==':') {
            int len=0;
            while (c=*sTrace++) {
                if (c==32) break; // space after xxx ends string to echo
                if (len>20) break; 
                buf[len++] = c; 
            }
            buf[len]=0;
        }
        if ((addr < -4095) || (addr > 4095)) sprintf(buf, "invalid addr=%d", addr);
        trace=2; 
    }
    if (trace) {
        int hh,mm,ss;
        ss = msec / 1000; msec = msec - ss * 1000;
        mm = ss   / 60;   ss   = ss   - mm * 60;
        hh = ss   / 60;   hh   = hh   - hh * 60;
        sim_printf("%02d:%02d:%02d:%03d %04d: %s\n", hh,mm,ss,msec,IC,buf); 
        if (trace == 2) while(len>0) {
            ReadAddr(addr, &d, NULL);
            if (addr<0) {
                sim_printf("             %04d: %c%05d%06d (octal %04o: %06o %06o) \n", 
                                         -addr, printfw(d), -addr, printfw_oct(d));
                addr-=2; 
                if (addr < -4095) break; 
            } else {
                sim_printf("             %04d: %c%d (octal %04o: %06o) \n", 
                                         addr, (char) ( (d >> 17) ? '-':'+'), (int) (d & 0377777), addr, (int) d);
                addr++; 
                if (addr > 4095) break; 
            }
            len--;       
        }
    } 
    return reset_tm0;  
}

void ClearInterlocks(void) 
{ 
    extern mtinforec mt_info[4];

    int unit; 

    for (unit=0;unit<=3;unit++) {
        mt_info[unit].ReadyTickCount=0;
        if (mt_info[unit].TapeCheck) {
            mt_info[unit].TapeCheck=0; // if tape check, clear check and offending command
            mt_unit[unit].u5 = MT_RDY;
        }
    }
    // init all tapes with ready flag on
    for (unit=0;unit<4; unit++) mt_unit[unit].u5 |= MT_RDY; // set tape ready 

}

t_stat sim_instr(void)
{
    t_stat              reason;
    int                 halt_cpu_requested_reason, bTooFast;
    int                 instr_count = 0;  /* Number of instructions to execute */
    int                 ncycle_count = 0; /* Number of cycles to execute */
    const char *        opname;          /* points to opcode name */               
    char *              Symbolic_Buffer;
    uint32              Symbolic_Trace_tm0; 
    char *              sTrace;

    t_int64 d; 
    int opcode, opaddr, Stall; 
    char cTransferTo; 

    int bFastMode; 
    int CpuTicksUsed; 
     
    int bStallMsg; t_int64 bStallTick0;

    /* How CPU execution is simulated

    Sim Interval count one simulated machine cycle time (12 msec) = one tick
    A IBM 701 instruction is divided into interpretation cycle and execution cycle
    and needs CpuTicksUsed ticks to be executed
    
    The Tape I/O is completelly synchonous. The COPY instruction remains blocked until 
    the I/O operation finishes. 
    
    User can select to execute the instructions one by one, on an full instruction at once basis or 
    just in a half-cycle by half-cycle basis.

    */

    reason = halt_cpu_requested_reason = 0;
    CpuTicksUsed = 0;
    opname=Symbolic_Buffer=NULL;

    ClearInterlocks();

    bFastMode = FAST; 
    CpuSpeed.start_tm0  = sim_os_msec();  // sim_os_msec() when run starts
    CpuSpeed.InstrCount = 0;              // init instr executed in this run
    CpuSpeed.GlobalTicksCount0 = GlobalTicksCount; // initial value to get the number of ticks executed on run termination
    Measure_CpuSpeed(0);                  // init cpu speed measurement
    sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because start of run\n");

    Symbolic_Trace_tm0 = CpuSpeed.start_tm0; // sim_os_msec() when run starts
    Stall = bStallMsg = 0;
    StopReason = 0; 

    if (sim_step != 0) {
        // step scp command. Can be step full instr or step half-cycles
        if (cpu_unit.flags & OPTION_STEP_HALFCYCLE) {
            ncycle_count = sim_step;
        } else {
            instr_count = sim_step;
        }
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
        sim_interval -= 1;         /* count down one tick */
        if (CpuTicksUsed) {
            CpuTicksUsed--;
            // check if must disconnect I/O device because too much time passed from last COPY
            if (IOREMAIN) {
                IOREMAIN --;
                if ((IOREMAIN == 0) && (IOADDR)) {
                   sim_debug(DEBUG_DATA, &cpu_dev, "... Too much time elapsed from previous COPY instr \n");
                   // disconnect from device
                   DisconnectIO();
                }
            }
        }

        #if defined(CPANEL)
        if (cpanel_on) {
            // do control panel refresh and user clicks event processing 
            reason = cpanel_interval_refresh();  
            if (reason == SCPE_STOP) {
                if (cpu_unit.flags & OPTION_STEP_HALFCYCLE) break; // is set then break now, else ...
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
                    // during fast mode, CpuTicksUsed (Ticks that an instruction need to execute) and
                    // IOREMAIN still being calculated, but there is no wait on these values
                    CpuTicksUsed = 0; 
                    ClearInterlocks();
                }
            }
            {
               // set info for ShowInfo
               extern int nShowInfoCpuTick; 
               extern int nShowInfoStall; 
               nShowInfoCpuTick = CpuTicksUsed; 
               nShowInfoStall   = Stall; 
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
                break;                                  // if ^E pressed twice, break if stalled/waitng for end of tape movement
            }
            halt_cpu_requested_reason = SCPE_STOP;      // signal it so cpu is halted on end of current intr execution cycle
        }

        // increment number of ticks count elapsed from starting of simulator -> this is the global time measurement
        if ((bFastMode) || (CpuSpeed_Acceleration<=0)) {  
            // FAST mode -> increment GlobalTicksCount at once
            int n = CpuTicksUsed; 
            GlobalTicksCount += n;
            CpuTicksUsed = 0;
            // FAST mode -> decrement IOREMAIN at once
            if (IOREMAIN) {
                IOREMAIN -= n;
                if (IOREMAIN <=0) {
                    IOREMAIN = 1; CpuTicksUsed = 1; // so will disconect I/O device on next instr
                }
            } 
        } else {
            // increment GlobalTicksCount, loop if should wait
            GlobalTicksCount++;
            CpuSpeed.TicksCount++;
            if (CpuTicksUsed > 0) continue; // if subop exec not finished then should still waiting, so loop again
            // if cpu is executing too fast, loop 
            if (bTooFast) continue; 
        }

        if (NCYCLE == 0) {
            // Interpretation half-cycle: fetch instruction to execute from IC mem addr to IR reg
            IC = IC & 07777; 
            ReadAddr(IC, &d, NULL); 
            IR = (int) d; 
            CpuTicksUsed += 2; 

            // get symbolic info if any
            if (IC * 80 > sizeof(CRT_Symbolic_Buffer)) {
                Symbolic_Buffer=NULL;
            } else {
                Symbolic_Buffer = &CRT_Symbolic_Buffer[IC * 80];
            }
            // Decode IR
            opname=DecodeInst(IR, &opcode, &opaddr,1);
            sim_debug(DEBUG_CMD, &cpu_dev, "Exec %04d: %c %-8s %02d %04d (octal %04o %02o %04o) %s \n", 
                                            IC, (IR >> 17) ? '-':'+', opname, opcode, (opaddr<0)? -opaddr:opaddr, 
                                            IC, IR >> 12, IR & 07777, 
                                            (Symbolic_Buffer) ? Symbolic_Buffer:"");

            if ((Symbolic_Buffer) && (sTrace=strstr(Symbolic_Buffer, "**"))) {
                int n; 
                n = Symbolic_Trace(Symbolic_Trace_tm0, Symbolic_Buffer, sTrace); 
                if (n) Symbolic_Trace_tm0=sim_os_msec(); // reset time origin for trace printouts
            }

            NCYCLE= 1; 
            // break if stepping half cycle 
            if (ncycle_count != 0 && --ncycle_count == 0) {
                reason = SCPE_STEP; 
                break;
            }
        }                        
        if (NCYCLE == 1) {
            int unit = -1;
            // Execution half-cycle
            // check if calculator stall execution because interlocked by I/O
            Stall=0;
            opcode = ((IR >> 12) & 31); 
            opaddr = IR & 4095; 
            if ((opaddr >= 256) && (opaddr <= 259)) unit=opaddr &3; 
            if ((opcode >= OP_READ) && (opcode <= OP_WRITE_EF) && (unit>=0)) {
                // check tape not ready because rewing in progress
                if (mt_ready(unit)==0) Stall=10+unit; 
            }
            if (Stall) {                
                if (bStallMsg==0) {
                    sim_debug(DEBUG_CMD, &cpu_dev, "Stalled by %s \n", 
                        ((Stall >= 10) && (Stall <= 13)) ? "Tape instr stalled because tape unit not ready" :
                        "???");
                    bStallMsg=1; 
                    bStallTick0=GlobalTicksCount; 
                }
                CpuTicksUsed += 10*1000; // wait 10 msec
                continue;
            }
            
            if (bStallMsg) {
                sim_debug(DEBUG_CMD, &cpu_dev, "... execution resumed (stalled for %d msec) \n", 
                    (int) ((GlobalTicksCount - bStallTick0) / 1000)  );
                bStallMsg=0; 
            }
            NCYCLE=2; 
        }                        
        if (NCYCLE == 2)  {
            DecodeInst(IR, &opcode, &opaddr,0);
            reason = ExecOpcode(opcode, opaddr, bFastMode,
                                &cTransferTo, &CpuTicksUsed);

            // cTransferTo: set to 'N' for next instr, 'A' transfer to addr, '1' '2' to transfer skip 1 or 2 next instrs
            // next cycle depends on what addr to transfer to at inst execution end
            if (cTransferTo == 'A') NCYCLE=3; else
            if (cTransferTo == '1') NCYCLE=5; else 
            if (cTransferTo == '2') NCYCLE=6; else NCYCLE=4; 
            if (reason != SCPE_OK) {
                break; // if error in subop other than terminate, then exit
            }
            continue; 
        }                        
        if (NCYCLE == 3) { IC = IR & 07777; }       else // transfer to opaddr
        if (NCYCLE == 4) { IC = (IC + 1) & 07777; } else // transfer to next instr
        if (NCYCLE == 5) { IC = (IC + 2) & 07777; } else // skip next instr
        if (NCYCLE == 6) { IC = (IC + 3) & 07777; }      // skip next 2 instr
            
        if (NCYCLE > 2)  {
            // one instruction fully executed
            NCYCLE=0;
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
        }
        // break if stepping half cycle (break at end of exec half cycle)
        if (ncycle_count != 0 && --ncycle_count == 0) {
            reason = SCPE_STEP; 
            break;
        }

    } /* end while */
    StopReason = reason; // save stop reason to be displayed in control panel
    if (reason) {
        sim_debug(DEBUG_CMD, &cpu_dev, "CPU STOP reason %d - %s\n", reason, 
            (reason == SCPE_STOP) ? "^E pressed in console" :
            (reason <= STOP_TAPECHECK) ? sim_stop_messages[reason]: "");
    }

    #if defined(CPANEL)
    // post-run refresh
    if ((cpanel_on) && (reason != SCPE_EXIT)) {
        // terminate in-progress I/O printer/tape if any
        extern int bTapeAnimInProgress;
        extern int bSwitchAnimInProgress;
        extern int bShowInfo;
        extern uint32 ShowInfoTm0; 
        int bIOInProgress, IOInProgress=0;    
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
            if (bTapeAnimInProgress) {bIOInProgress=1; IOInProgress |= 2;    } // tape animation in progress
            if (bSwitchAnimInProgress) {bIOInProgress=1; IOInProgress |= 4;    } // console up/down switch animation in progress
            // control cpu speed, slowdown and yield time to OS if needed (with sim_os_ms_sleep)
            // do not increment GlobalTicksCount to avoid missing the delay to send data to cdr/cdp
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
        if (bShowInfo>1) bShowInfo=1; // restore
        buf[0]=0;
        if (IOInProgress & 1) strcat(buf, "Printer/");
        if (IOInProgress & 2) strcat(buf, "TAPE/");
        if (IOInProgress & 4) strcat(buf, "Up Down Switch/");
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
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Speed is x%0.1f relative to original hardware\n", TPS * 12 / 1000000.0);  // real hw speed: 83K ticks per second (each tick = 12 usec)
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Instructions executed: %.0f\n", 1.0 * CpuSpeed.InstrCount);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: %d Instructions per second (IPS) achieved\n", IPS);
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Run elapsed %s\n", sBuf);
        }
    }

    // flush online printer and card punch
    {
        if ((reason != SCPE_STEP) && (reason != SCPE_STOP)) {
            // if not stepping/stop, disconnect IO device being connected to MQ
            DisconnectIO();
        }
        if ((lp_unit[0].flags & UNIT_ATT) && (lp_unit[0].fileref)) {
            fflush(lp_unit[0].fileref); 
        }
    }

    /* Simulation halted */
    return reason;
}


/* Reset routine */
t_stat cpu_reset(DEVICE * dptr)
{
    vm_init ();

    // reset does not clear the memory 

    sim_brk_types = sim_brk_dflt = SWMASK('E');

    IC = IR = 0;
    MR = ACC = MQ = 0; 
    NCYCLE = 0;
    OV = SENSE_OUT = 0;
    StopReason = 0;
        
    IOADDR = 0; 
    IOREMAIN = 0; 

    ClearInterlocks();
    
    return SCPE_OK;
}

// clears CRT memory, symbolic mem, set OV=0, reset NCYCLE
void crt_reset(void)
{
    memset(CRT, 0, sizeof(CRT)); 
    memset(CRT_Symbolic_Buffer, 0, sizeof(CRT_Symbolic_Buffer));            // clear symbolic info
    MR=0;
    OV=0; 
    NCYCLE = 0;
    StopReason=0; 
}

t_stat cpu_clear_crt_mem(UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    crt_reset();
    return SCPE_OK;
}

/* Memory examine */

t_stat cpu_ex(t_value * vptr, t_addr addr, UNIT * uptr, int32 sw)
{
    t_int64 d; 
    int ad;

    if (sw & SWMASK('F')) {
        if (addr & 1) return sim_messagef (SCPE_ARG, "Can only read fullwords from even address\n");
        ad = -((int)addr);
    } else {
        ad = (int) addr; 
    }

    if (0==ReadAddr(ad, &d, NULL)) {
        return SCPE_NXM;
    }
    *vptr = d; 
    return SCPE_OK;
}

/* Memory deposit */

t_stat cpu_dep(t_value val, t_addr addr, UNIT * uptr, int32 sw)
{
    int ad;

    if (sw & SWMASK('F')) {
        // sypring a fullword 
        ad = - ((int)addr); 
    } else {
        ad = (int)addr; 
    }
    if (0==WriteAddr(ad, (t_int64) val, NULL)) {
        return SCPE_NXM;
    }
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
    // machine cycle: 12 microseconds 
    MachineCycle_usec = 12; 
    CpuSpeed.TicksMax  = (int) (CpuSpeed.msec * CpuSpeed_Acceleration * 1000 / (100.0 * MachineCycle_usec));
    // Clock set to 83 KHz (ticks per sec)
    CpuSpeed.TicksObjectivePerSec = (int) (1000000.0 * CpuSpeed_Acceleration / (MachineCycle_usec*100.0) );
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
            (float) (83.3 * CpuSpeed_Acceleration / 100.0));
    }

    Measure_run_CpuSpeed(&elapsed, &TPS, &IPS, sBuf, 0);
    if (elapsed == 0) {
        fprintf (st, "Measured speed: no data\n");
    } else {
        fprintf (st, "Measured speed: Speed is x%0.1f relative to original hardware\n", TPS * 12 / 1000000.0);  // real hw speed: 83K ticks per second (each tick is 12 usec)
        fprintf (st, "                %d Instructions per second (IPS) achieved\n", IPS);
        fprintf (st, "                run elapsed %s\n", sBuf);
    }
    return SCPE_OK; 
}


const char * cpu_description (DEVICE *dptr) { 
    return "IBM 701 Electronic Data Processing Machine";     
}   
 

// en i701
// poner LIST/READ originales como progs de la libreria de PACT
// cambiar pact.exe por addlib.exe
// actualizar manual con novedades


