/* MarkI_defs.h: Ferranti Mark I computer simulator definitions

   Copyright (c) 2023, Roberto Sancho

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to thprie following conditions:

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

#define MASK_40BITS         0xFFFFFFFFFF                // mask for 40 bits
#define MASK_20BITS         0xFFFFF                     // mask for 20 bits
#define MASK_20UPPERBITS    ((t_int64) (0xFFFFF00000))  // mask for 20 upper bits
#define MASK_10BITS         0x003FF                     // mask for 10 bits

extern int charset_MarkI[42][2]; 
extern char FigureShift_charset_MarkI[2][32]; 

/* Simulator stop codes */

#define STOP_HALT           1               /* Halt on /G or /L instr */
#define STOP_IOERROR        2               /* host OS - i/o error */
#define STOP_UUO            3               /* unknown opcode */
#define STOP_EOF            4               /* Paper tape EOF */
#define STOP_PTR_UNATT      5               /* Paper tape not attached */
#define STOP_HOOT           6               /* Stop on continuous hoot */
#define STOP_DYNAMIC_STOP   7               /* Stop on dynamic stop (transfer instr to itself) */ 
#define STOP_TR_NOPRG       8               /* Stop on transfer to //// instruction) */ 

#define DRUM_TRACKS         256             // max number of tracks on drum
/* Memory */
extern int              CRT[8*64];      // each Electrostic Store conatins 20 bin digits or 4 teleprinter char
extern int              DRUM[DRUM_TRACKS * 128];  

#define OPTION_FAST               (1 << (UNIT_V_UF + 5))
#define OPTION_REPEATABLE_RANDOM  (1 << (UNIT_V_UF + 6))
#define OPTION_BRK1_HOOT          (1 << (UNIT_V_UF + 7))
#define OPTION_USE_WC_J           (1 << (UNIT_V_UF + 8))
#define OPTION_ECHO               (1 << (UNIT_V_UF + 9))
#define OPTION_NOECHO             (1 << (UNIT_V_UF + 10))
#define OPTION_NOPRINT            (1 << (UNIT_V_UF + 11))
#define OPTION_NOPERSISTENT       (1 << (UNIT_V_UF + 12))

#define FAST            ((uint32)(cpu_unit.flags & OPTION_FAST) ? 1:0)        // return non zero if set cpu fast option set

extern void WriteAddr(int addr, int d, int bmsg);
extern int ReadAddr(int addr, int bmsg);
extern t_stat cpu_reset(DEVICE * dptr); 

// increment number of cpu Beats Count elapsed from starting of simulator -> this is the 
// global time measurement. Machine clock is 10 usec (100Khz), 1 beat = 24 clock cycles = 240 usec
extern t_int64 GlobalBeatsCount;
extern int     CpuSpeed_Acceleration;       // cpu speed multiplier

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

/* Generic devices common to all */
extern DEVICE      cpu_dev;
extern UNIT        cpu_unit;
extern REG         cpu_reg[];

extern const char *cpu_description(DEVICE *dptr);

// Instruction Opcodes (total: 33 opcodes)
//                       Regular   Code Backwards 
//                        Binary         Binary
#define OP_MAG_INSTR_H    0x00  //   //  000000    H (=handswitches value) as magnetic instr (drum or I/O transfer)
#define OP_STORE_AM       0x02  //   /E  010000    S=AM (most significant accum 40 bits) 
#define OP_MSB_POS        0x04  //   /@  001000    AM=pos of MSB of contents of S (standarise) 
#define OP_STORE_AM_CLAM  0x06  //   /A  011000    S=AM, clear AM
#define OP_MAG_INSTR_S    0x08  //   /:  000100    S as magnetic instr (drum or I/O transfer)
#define OP_STORE_AL       0x0A  //   /S  010100    S=AL (least significant accum 40 bits) 
#define OP_SWAP_AM_AL     0x0C  //   /I  001100    Swap AL <-> AM
#define OP_MOVE_CLAM      0x0E  //   /U  011100    S=AL, AL=AM, clear AM
#define OP_U_MPY_SUB      0x10  //   /#  000010    A = A - D * S unsigned
#define OP_S_MPY_SUB      0x12  //   /D  010010    A = A - D * S signed
#define OP_POP_COUNT      0x14  //   /R  001010    Population Count: AM=AM+number of ones in addr s (sideways adder)
#define OP_ADD_AM         0x16  //   /J  011010    AM=AM+S (Add Upper)
#define OP_U_MPY_ADD      0x18  //   /N  000110    A = A + D * S unsigned
#define OP_S_MPY_ADD      0x1A  //   /F  010110    A = A + D * S signed
#define OP_U_LOAD_D       0x1C  //   /C  001110    D = S (multiplicand reg) unsigned
#define OP_S_LOAD_D       0x1E  //   /K  011110    D = S (multiplicand reg) signed
#define OP_TR_IF_B        0x20  //   /T  000001    if B>=0 then C=S (C=IR=program counter). B conditional direct transfer control
#define OP_STORE_H        0x22  //   /Z  010001    S=H (=handswitches value) 
#define OP_STOP_L         0x24  //   /L  001001    Stop if Swith /L set (dummy stop)
#define OP_RND            0x26  //   /W  011001    AL (lower 20 bits) = randon number generator
#define OP_TR_IF_A        0x28  //   /H  000101    if A>=0 then C=S (C=IR=program counter). A conditional direct transfer control
#define OP_STORE_Z        0x2A  //   /Y  010101    S=0 (20 bits) clear a short line
#define OP_TR             0x2C  //   /P  001101    C=S (jump to S). Unconditional direct transfer control
#define OP_TR_REL         0x2E  //   /Q  011101    C=C+S+1 (relative jump). Unconditional relative transfer control
#define OP_TR_REL_IF_B    0x30  //   /O  000011    if B>=0 then C=C+S+1. B conditional relative transfer control
// unassigned             0x32  //   /B  010011
#define OP_STOP_G         0x34  //   /G  001011    Stop if Swith /G set (dummy stop)
// unassigned             0x36  //   /"  011011
#define OP_TR_REL_IF_A    0x38  //   /M  000111    if A>=0 then C=C+S+1. Conditional relative transfer control
// unassigned             0x3A  //   /X  010111
#define OP_HOOT           0x3C  //   /V  001111    Audio hoot
// unassigned             0x3E  //   /$  011111
#define OP_U_LOAD_A       0x01  //   T/  100000    AL=S, Clear AM
// unassigned             0x03  //   TE  110000
#define OP_LD_DRUM        0x05  //   T@  101000    Load drum addr of page (line 65) into AL, Clear AM
#define OP_STORE_AL_CLA   0x07  //   TA  111000    S=AL, Clear A
#define OP_CLA            0x09  //   T:  100100    Clear A
// unassigned             0x0B  //   TS  110100
#define OP_U_ADD_A        0x0D  //   TI  101100    AL=AL+S, if carry inc AM (unsigned)
// unassigned             0x0F  //   TU  111100
#define OP_S_LOAD_A       0x11  //   T#  100010    AL=S, sign extend into AM
#define OP_OR_A           0x13  //   TD  110010    A=S extended OR A
#define OP_AND_A          0x15  //   TR  101010    A=S extended AND A
#define OP_XOR_A          0x17  //   TJ  111010    A=S extended NEQ A
#define OP_S_SUB_A        0x19  //   TN  100110    A=A-(sign extended S)
#define OP_S_LOAD_MA      0x1B  //   TF  110110    A=-(sign extended S)
#define OP_S_ADD_A        0x1D  //   TC  101110    A=A+(sign extended S)
#define OP_S_LOAD_2A      0x1F  //   TK  111110    A=2*(sign extended S)
#define OP_LOAD_B         0x21  //   TT  100001    B=S
#define OP_STORE_B        0x23  //   TZ  110001    S=B
#define OP_SUB_B          0x25  //   TL  101001    B=B-S
#define OP_SUB_B_same     0x27  //   TW  111001    same as TL
// unassigned             0x29  //   TH  100101
// unassigned             0x2B  //   TY  110101
// unassigned             0x2D  //   TP  101101
// unassigned             0x2F  //   TQ  111101
#define OP_LOAD_B_NF      0x31  //   TO  100011    B=S (no B addition)
#define OP_STORE_B_NF     0x33  //   TB  110011    S=B (no B addition)
#define OP_SUB_B_NF       0x35  //   TG  101011    B=B-S (no B addition)
#define OP_SUB_B_NF_same  0x37  //   T"  111011    same as TG (no B addition)
// unassigned             0x39  //   TM  100111    (no B addition)
// unassigned             0x3B  //   TX  110111    (no B addition)
// unassigned             0x3D  //   TV  101111    (no B addition)
#define OP_NOP            0x3F  //   T$  111111    Dummy (no B addition)

/* Symbol tables */
typedef struct 
{
    uint16             opbase;          // opcode number
    const char         *name;           // opcode name 
    int                nBeats;          // number of beats needed to be executed
}
t_opcode;

extern t_opcode  base_ops[63];

extern void vm_init(void);  /* One time initialization activities now called in cpu_reset() */

extern DEVICE       ptr_dev;
extern UNIT         ptr_unit;
#define             MAX_PTR_BUF  1024
extern char         ptr_buf[MAX_PTR_BUF]; // buffer for FEEDPTR command
extern t_stat       ptr_char(int *ch);
extern char         ptr_FileName[MAX_PATH+1];
extern int32        ptr_FileSize; 
extern uint32       ptr_move_start_tm0; // start of ptr tape animation on cpanel

extern DEVICE       ptp_dev;
extern UNIT         ptp_unit;
extern void         ptp_char(int ch);

extern DEVICE       lpt_dev;
extern UNIT         lpt_unit; 
extern void         lpt_char(int ch); 
extern int          lpt_chariage_pos; // current printer carriage pos
extern uint32       lpt_CR_start_tm0; // start of carriage return animation on cpanel
extern int          lpt_CR_from_pos;  // animating carriage movement from this pos

#define LPT_ON     ((lpt_unit.flags & UNIT_DIS) ? 0:1)
#define PTP_ON     ((ptp_unit.flags & UNIT_DIS) ? 0:1)

#define LPT_COLUMNS        69    // colums in printout buffer
#define lptPrintOutMAX     20    // last lptPrintOutMAX lines printed will be saved on circular buffer lptPrintOut

extern DEVICE       perforator_dev;
extern UNIT         perforator_unit;
extern t_stat       perforator_typein(char * sLinInput, int bEchoInput, int bQuietMode); 
extern uint32       perforator_start_tm0; 

extern DEVICE       drum_dev; 
void drum_write_track(int trk);

extern CONST char * parse_n(int *d, CONST char *cptr, int bFer, int nchars);
