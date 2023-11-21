/* i701_defs.h: IBM 701 simulator definitions

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

#include "sim_defs.h"                                   /* simulator defns */

/* Simulator stop codes */

#define STOP_PROG       1               /* Check STOP */
#define STOP_IOERROR    2               /* host OS - i/o error */
#define STOP_UUO        4               /* unknown opcode */
#define STOP_DIVBYZERO  5               /* Division by zero */
#define STOP_DIVCHECK   6               /* Division check */
#define STOP_COPYCHECK  7               /* Copy check */
#define STOP_TAPECHECK  8               /* Copy check */

/* Memory */
extern t_int64          CRT[2048];
extern char             CRT_Symbolic_Buffer[4096 * 80];

#define OPTION_FAST             (1 << (UNIT_V_UF + 5))
#define OPTION_STEP_HALFCYCLE   (1 << (UNIT_V_UF + 7))  

#define FAST            ((uint32)(cpu_unit.flags & OPTION_FAST) ? 1:0)        // return non zero if set cpu fast option set

extern int WriteAddr(int addr, t_int64 d, char * msg);
extern int ReadAddr(int addr, t_int64 * d, char * msg);
extern void crt_reset(void);  // clears CRT memory, symbolic mem, and set V=0
extern void ClearInterlocks(void);
extern void SetIOADDR(int opaddr, int opcode);
extern void GetIOADDR(int * ioaddr, int * ioop);

// increment number of Clock Tick Counts elapsed from starting of simulator -> this is the 
// global time measurement. Increases at rate of 83.3 KHz (basic machine cycle is 12 msec)
extern t_int64 GlobalTicksCount;
extern int     CpuSpeed_Acceleration;       // cpu speed multiplier


/* Device information block */
struct dib {
        uint8   upc;                        // Number of Units in device 
        uint32  (*cmd)(UNIT *up, uint16 cmd, uint16 dev);/* Issue command. */
        void    (*ini)(UNIT *up, t_bool f);
};

typedef struct dib DIB;

/* Debuging controls */
#define DEBUG_CMD       0x0000010       /* Show device commands */
#define DEBUG_DETAIL    0x0000020       /* Show details */
#define DEBUG_EXP       0x0000040       /* Show error conditions */
#define DEBUG_DATA      0x0000080       /* Show data details */

extern DEBTAB dev_debug[];

/* Global device definitions */
#if defined(CPANEL)
extern DEVICE       cp_dev;
#endif

#define MAX_CARDS_IN_DECK  10000            // max number of cards in deck for carddeck internal command
#define MAX_CARDS_IN_READ_STAKER_HOPPER 10  // max number of cards in card reader take 

extern void         dr_cmd(int, int, int);

extern DIB          cdr_dib;
extern DEVICE       cdr_dev;
extern uint32       cdr_cmd(UNIT *, uint16, uint16);
extern UNIT         cdr_unit[1];
extern uint16       ReadStaker[MAX_CARDS_IN_READ_STAKER_HOPPER * 80];
extern int          ReadStakerLast;

extern DIB          cdp_dib;
extern DEVICE       cdp_dev;
extern uint32       cdp_cmd(UNIT *, uint16, uint16);
extern UNIT         cdp_unit[1];
extern int          cdr_skip_cols (void);

extern DIB          mt_dib;
extern DEVICE       mt_dev;
extern uint32       mt_cmd(UNIT *, uint16, uint16);
extern UNIT         mt_unit[4];
extern int          mt_ready(int n);
extern void         mt_set_ready(int n);
extern int          mt_get_last_cmd(int n);
extern void         mt_ini(UNIT * uptr, t_bool f);
extern t_stat       mt_attach(UNIT * uptr, CONST char *file);
extern t_stat       mt_detach(UNIT * uptr);

/* Tape Indicator status - these are the posible signals from tape*/

#define MT_IND_RDERR             5    // simulated read error
#define MT_IND_EOF              11    // signal EOF found


#define MT_CMDMSK   0x00FF      /* Command being run */
#define MT_RDY      0x0100      /* Unit is ready for command */

// extended info for tapes
typedef struct {
    uint8 * TapeRecord_buf;                 // Tape record buffer. Allocated with dinamic mem on attach, released on detach
    int TapeRecord_index;                   // -1 if no record. >=0 if tape record being written
    int TapeStatus;                         // 0 for neutral status, OP_WRITE or OP_READ
    int TapeCheck;                          // signal tape error

    int justattached;                       // 1=just attached. Set to 1 by attach
    int justdetached;                       // 1=just detached. Set to 1 by detach
    int numrw;                              // number of read/write cmds exec on tape on attached reel
    int numrec;                             // record number where the tape is positioned on (+1 on r/w, -1 on backspace)
    int recsize;                            // on rewind, holds the u3 value before rewinding, record size (inches x1000), <0 if reading backwards
    int cmd_usec;                           // time needed to exec cmd (microseconds! not milliseconds)
    int cmd_tm0;                            // signal tape cmd animation pending to be done
    t_int64 ReadyTickCount;                 // value of GlobalTickCount (=simulated time) when tape will be ready to accept command
} mtinforec;
// extended info for tapes. Holds detailed info of tape command operation
extern mtinforec mt_info[4];


extern char    digits_ascii[31];
extern char    mem_to_ascii[101];
extern int     ascii_to_NN(int ch);
extern uint16  sim_ascii_to_hol(char c);
extern char    sim_hol_to_ascii(uint16 hol);
extern void    echo_cardimage(DEVICE * dptr, uint16 * image, int EchoLevel, char * msg, char * cardtext);
extern void    PrepareCardImage(uint16 * image, t_int64 * CardImage, int index, int SkipCols);

extern DIB          lp_dib;
extern DEVICE       lp_dev;
extern uint32       lp_cmd(UNIT *, uint16, uint16);
extern UNIT         lp_unit[1];

#define LPT_COLUMNS        80   // colums in printout   
#define lptPrintOutMAX     20    // last lptPrintOutMAX lines printed will be saved on circular buffer lptPrintOut
extern void lpt_printline(UNIT * uptr, char * line, int bNoEcho);

#define WIRING_NONE         0
#define WIRING_NAA_SpeedEx  1    // Morth American Aviation Assembler
#define WIRING_NR9003       2    // Symbolic Assembler
#define WIRING_SO2          3    // SO2 - Regional Assembler - RAL
#define WIRING_PACT         4 

/* Generic devices common to all */
extern DEVICE      cpu_dev;
extern UNIT        cpu_unit;
extern REG         cpu_reg[];

extern const char *cpu_description(DEVICE *dptr);
extern t_stat cpu_reset(DEVICE * dptr);
extern t_stat cpu_clear_crt_mem(UNIT *uptr, int32 value, CONST char *cptr, void *desc);

// Instruction Opcodes (total: 33 opcodes)
// C(x) = contents of address x
#define OP_STOP           0  // Stop and transfer
#define OP_TR             1  // Transfer 
#define OP_TR_OV          2  // Transfer on overflow: Transfer if OV=1 (and reset ov)
#define OP_TR_PLUS        3  // Transfer on plus: Transfer if ACC>0 
#define OP_TR_ZR          4  // Transfer on zero: Transfer if ACC=0 
#define OP_SUB            5  // Subtract: Acc = Acc - C(x)
#define OP_R_SUB          6  // Reset and Subtract: Acc = - C(x)
#define OP_SUB_AB         7  // Subtract Absolute value: Acc = Acc - |C(x)|
#define OP_NOOP           8  // No Operation
#define OP_ADD            9  // Add: Acc = Acc + C(x)
#define OP_R_ADD         10  // Reset and Add: Acc = C(x)
#define OP_ADD_AB        11  // Add Absolute value: Acc = Acc + |C(x)|
#define OP_STORE         12  // Store: Store accumulator at given address: C(x)= Acc
#define OP_STORE_A       13  // Store Address: Store 12 rightmost bits from Acc at addr x (halfword)
#define OP_EXTR      (13+32) // Extract: C(x) = C(x) AND Acc
#define OP_STORE_MQ      14  // Store MQ: C(x) = MQ
#define OP_LOAD_MQ       15  // Load MQ: MQ = C(x)
#define OP_MPY           16  // Multiply: Acc:MQ = C(x) * MQ. Acc=35 most significants bits + sign
#define OP_MPY_R         17  // Multiply and Round: same as above, followed by round
#define OP_DIV           18  // Divide: MQ = Acc:MQ / C(x), Acc=remainder   
#define OP_ROUND         19  // Round: if leftmost bit of MQ=1 incr Acc
#define OP_L_LEFT        20  // Long Left Shift: Acc:MQ << x, ACC sign taken from MQ
#define OP_L_RIGHT       21  // Long Right Shift: Acc:MQ >> x, MQ sign taken from Acc 
#define OP_A_LEFT        22  // Accumulator Left Shift: Acc << x, Acc sign unchanged
#define OP_A_RIGHT       23  // Accumulator Right Shift: Acc >> x, Acc sign unchanged
#define OP_READ          24  // Prepare to Read. Start mechanical movement forward
#define OP_READ_B        25  // Prepare to Read Backward. Start mechanical movement backward
#define OP_WRITE         26  // Prepare to Write. Start mechanical movement 
#define OP_WRITE_EF      27  // Write End of File. 
#define OP_REWIND        28  // you guessed!
#define OP_SET_DR        29  // Ser Drum Address
#define OP_SENSE         30  // Sense and Skip
#define OP_COPY          31  // Copy and Skip


/* Symbol tables */
typedef struct 
{
    uint16             opbase;          // opcode number
    const char         *name;           // opcode name 
    int                nCycles;         // fundamental number of cycles needed to be executed
    int                ModCycles;       // modification type to number of cycles 
}
t_opcode;

extern t_opcode  base_ops[34];

#define SGN          (1LL << 35)        // bit35 in intel notation (bit0 is LSB) is IBM 701 sign for fullwords
#define ACCSGN       (1LL << 37)        // bit37 in intel notation (bit0 is LSB) is IBM 701 sign for acc
#define MASK35BITS   (SGN - 1)          // mask to 1 all bits except ibm 701 fullword sign bit
#define MASK37BITS   (ACCSGN - 1)       // mask to 1 all acc bits except sign bit (35 magnitude bits + PQ overflow bits)

// printf signed decimal 36 bit word. To be used with %c%05d%06d
#define printfw(d)         (char) ( ((d) & SGN) ? '-':'+'), (int) (((d) & MASK35BITS) / 1000000LL), (int) (((d) & MASK35BITS) % 1000000LL)
// printf undigned octal 36 bit word. To be used with %06o%06o
#define printfw_oct(d)     (int) ( ((d) >> 18) & 0777777), (int) ( (d) & 0777777)
// printf acc signed 36 bit word with 2 overflow bits. To be used with %c%d%06d (%c%d|%c%d octal %d %d%d %06o|%06o)
#define printfw_acc(d)     (char) ( ((d) & ACCSGN) ? '-':'+'), (int) ( ((d) & MASK35BITS) / 1000000), (int) ( ((d) & MASK35BITS) % 1000000), \
                           (char) ( ((d) & ACCSGN) ? '-':'+'), (int) (  (((d) & MASK35BITS) >> 18) ), \
                           (char) ( (((d) >> 17) & 1) ? '-':'+'), (int) ( ((d) & 0377777) ), \
                           (int) ( ((d) & ACCSGN) ? 1:0), (int) ((d >> 36) & 1), (int) ((d >> 35) & 1), \
                           (int) (  (((d) & MASK35BITS) >> 18) ), (int) ( ((d) & 0777777) )

#define sim_debug_acc_ov      sim_debug(DEBUG_DATA, &cpu_dev, "... ACC: %c%d%06d (%c%d|%c%d octal %d %d%d %06o|%06o), OV: %d \n", \
                                                           printfw_acc(ACC), OV)
#define sim_debug_acc_mq_ov   sim_debug(DEBUG_DATA, &cpu_dev, "... ACC: %c%d%06d (%c%d|%c%d octal %d %d%d %06o|%06o), MQ: %c%05d%06d, OV: %d \n", \
                                                           printfw_acc(ACC), printfw(MQ), OV)

// return time elapsed between 12-microseconds t_int64 ticks, in form int msec 
extern int msec_elapsed(t_int64 nTick1, t_int64 nTick2);
// convert time in millisec/microsec to number of 12-microseconds t_int64 tick 
extern int msec_to_ticks(int msec); 
extern int usec_to_ticks(int usec); 

extern void vm_init(void);  /* One time initialization activities now called in cpu_reset() */

