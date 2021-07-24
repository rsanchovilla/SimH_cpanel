/* norc_defs.h: IBM NORC simulator definitions

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

#include "sim_defs.h"                                   /* simulator defns */

/* Simulator stop codes */

#define STOP_HALT       1               /* Check STOP */
#define STOP_IOERROR    2               /* host OS - i/o error */
#define STOP_TAPE       3               /* tape operative check error */
#define STOP_EOT        4               /* end of tape check error */
#define STOP_DIVBYZERO  5               /* Division by zero */
#define STOP_INDEX      6               /* Float number index is invalid */
#define STOP_SIGN       7               /* Float number sign is invalid */
#define STOP_ADDR       8               /* Invalid Address */
#define STOP_OPCODE     9               /* Operation code check */
#define STOP_PRINTER   10               /* Printer operative check */
#define STOP_PROG      11               // Programed STOP - should be last error in list

/* Memory */
extern t_int64          CRT[3600];
extern char             CRT_Symbolic_Buffer[3600 * 80];

#define OPTION_FAST         (1 << (UNIT_V_UF + 5))
#define OPTION_2K           (1 << (UNIT_V_UF + 6))
#define OPTION_STEP_SUBOP   (1 << (UNIT_V_UF + 7))

#define FAST            ((uint32)(cpu_unit.flags & OPTION_FAST) ? 1:0)        // return non zero if set cpu fast option set
#define MEM2K           ((uint32)(cpu_unit.flags & OPTION_2K) ? 1:0)          // return 0 for 3600 words mem, return 1 for 2000 words mem

extern CONST char * DecodeInst(t_int64 d, int * P, int * Q, int * R, int * S, int * T, int bReturnOpname);
extern int MemSize(void); 
extern void crt_reset(void);  // clears CRT memory, symbolic mem, and set V=0
extern void cpu_reset_indicator_lights(void);
extern void ClearInterlocks(void);

/* digits contants */
#define D16      (10000000000000000LL)      // 
#define D15       (1000000000000000LL)      // 
#define D14        (100000000000000LL)      // (14 zeroes)
#define D13         (10000000000000LL)      // 
#define D12          (1000000000000LL)      // (12 zeroes)
#define D8                (100000000L)      // eight digits (8 zeroes)
#define D4                    (10000L)      // four digits (4 zeroes)

// increment number of Clock Tick Counts elapsed from starting of simulator -> this is the 
// global time measurement. Imcreases at rate of 1 MHz (machine clock)
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

extern DIB          mt_dib;
extern DEVICE       mt_dev;
extern uint32       mt_cmd(UNIT *, uint16, uint16);
extern UNIT         mt_unit[9];
extern int          mt_ready(int n);
extern void         mt_set_ready(int n);
extern int          mt_get_last_cmd(int n);
extern void         mt_ini(UNIT * uptr, t_bool f);
extern t_stat       mt_attach(UNIT * uptr, CONST char *file);
extern t_stat       mt_detach(UNIT * uptr);

/* Tape Indicator status - these are the posible signals from tape*/

#define MT_IND_BLKLEN            1    // block len read from tape does not match read instruction
#define MT_IND_BLKNUM            2    // start of block word does not match end of word block
#define MT_IND_WORDLEN           3    // bad word lenght in tape record (word = chksum char + 16 digits + 'C' char)
#define MT_IND_BADCHAR           4    // found digit greater than nine / block does not starts/ends with 'D' char
#define MT_IND_RDERR             5    // simulated read error
#define MT_IND_ADDR              6    // read from/write to invalid CRT addr during tape operation
#define MT_IND_BLK_PASSED_OVER  10    // signal block number read from tape does not match read instruction, the block is passed over and skipped
#define MT_IND_EOF              11    // signal EOF found


#define MT_CMDMSK   0x00FF      /* Command being run */
#define MT_RDY      0x0100      /* Unit is ready for command */

// extended info for tapes
typedef struct {
    int justattached;                       // 1=just attached. Set to 1 by attach
    int numrw;                              // number of read/write cmds exec on tape on attached reel
    int numrec;                             // record number where the tape is positioned on (+1 on r/w, -1 on backspace)
    int V1,V2,BlockNum;                     // params for current/last cmd executed
    int recsize;                            // record size (inches x1000), <0 if reading backwards
    int cmd_usec1;                          // time needed to exec tape cmd (in microseconds), for read/verify: usec1=scan time, usec2=read time
    int cmd_usec2;                          //       for write/delete: usec1=write time, usec2=0; for rewind: usec1=time for rewinding, usec2 = 0;
    int result1;                            // result of tape operation
    int cmd_tm0;                            // sim_os_msec when cmd started
} mtinforec;

// extended info for tapes. Holds detailed info of tape command operation
mtinforec mt_info[9];
extern int LastTapeCheck;    

// read words from tape
#define   MAX_TAPEDATA   150000             // max entries in tapedate
typedef struct {
    t_int64 Ticks;                          // value of global tick count when starting to read/write the current word from/to tape
    int index;                              // to count 1..10 / to mark current item in array
    int Count;                              // total number of entries in array
    t_int64 TicksCount[MAX_TAPEDATA];       // value of global tick count when for this word in index
    t_int64 REG1[MAX_TAPEDATA];             // word value read/write from/to tape
    t_int64 REG2[MAX_TAPEDATA];             // block to wich word belongs
    int V[MAX_TAPEDATA];                    // addr in CRT for word 
} tapedata;
extern void SetTapeData(int mode, t_int64 d);

extern char    digits_ascii[31];
extern char    mem_to_ascii[101];
extern int     ascii_to_NN(int ch);
extern uint16  sim_ascii_to_hol(char c);
extern char    sim_hol_to_ascii(uint16 hol);

extern DIB          lp_dib;
extern DEVICE       lp_dev;
extern uint32       lp_cmd(UNIT *, uint16, uint16);
extern UNIT         lp_unit[3];

#define LPT_COLUMNS        120   // colums in printout   
#define lptPrintOutMAX     20    // last lptPrintOutMAX lines printed will be saved on circular buffer lptPrintOut
extern int lpt_printline(UNIT * uptr, char * line);

extern DIB          ctc_dib;
extern DEVICE       ctc_dev;
extern uint32       ctc_cmd(UNIT *, uint16, uint16);
extern UNIT         ctc_unit[1];

extern t_stat keypunch_cmd(int32 flag, CONST char *cptr);
extern t_stat printcards_cmd(int32 flag, CONST char *cptr);
extern t_stat ctc_op_cmd(int32 flag, CONST char *cptr);

/* Generic devices common to all */
extern DEVICE      cpu_dev;
extern UNIT        cpu_unit;
extern REG         cpu_reg[];

extern const char *cpu_description(DEVICE *dptr);

// Instruction Opcodes (total: 67 opcodes)
// C(X) = contents of X
#define OP_ADD           20  //   C(R) + C(S) to T      Ordinary - rounded
#define OP_ADDM          21  // -[C(R) + C(S)] to T     Ordinary - rounded
#define OP_SUB           22  //   C(R) - C(S) to T      Ordinary - rounded
#define OP_SUBM          23  // -[C(R) - C(S)] to T     Ordinary - rounded
#define OP_MUL           24  //   C(R) x C(S) to T      Ordinary - rounded
#define OP_MULM          25  // -[C(R) x C(S)] to T     Ordinary - rounded
#define OP_DIV           26  //   C(R) / C(S) to T      Ordinary - rounded
#define OP_DIVM          27  // -[C(R) / C(S)] to T     Ordinary - rounded
#define OP_ABS           28  //  |C(R)| - |C(S)| to T   Ordinary - rounded
#define OP_TRUNC         29  // truncating transfer C(R) with C(S) to T   Ordinary - rounded
#define OP_ADD_SPE       30  //   C(R) + C(S) to T      Special - unrounded
#define OP_ADDM_SPE      31  // -[C(R) + C(S)] to T     Special - unrounded
#define OP_SUB_SPE       32  //   C(R) - C(S) to T      Special - unrounded
#define OP_SUBM_SPE      33  // -[C(R) - C(S)] to T     Special - unrounded
#define OP_MUL_SPE       34  //   C(R) x C(S) to T      Special - unrounded
#define OP_MULM_SPE      35  // -[C(R) x C(S)] to T     Special - unrounded
#define OP_DIV_SPE       36  //   C(R) / C(S) to T      Special - unrounded
#define OP_DIVM_SPE      37  // -[C(R) / C(S)] to T     Special - unrounded
#define OP_ABS_SPE       38  //  |C(R)| - |C(S)| to T   Special - unrounded
#define OP_TRUNC_SPE     39  // truncating transfer C(R) with C(S) to T   Special - unrounded
#define OP_META_ADD      40  //  C(R) + C(S) to T      Meta - 16 digit integer
#define OP_META_SUB      41  //  C(R) - C(S) to T      Meta - 16 digit integer
#define OP_META_MASK     42  // Apply to C(R) the mask on C(S), then shift result on P
#define OP_CHMOD         50  // Add R S T to M4 M6 M8
#define OP_CHMOD_CL4     51  // Clear M4 then add R S T to M4 M6 M8
#define OP_CHMOD_CL6     52  // Clear M6 then add R S T to M4 M6 M8
#define OP_CHMOD_CL46    53  // Clear M4 M6 then add R S T to M4 M6 M8
#define OP_CHMOD_CL8     54  // Clear M8 then add R S T to M4 M6 M8
#define OP_CHMOD_CL48    55  // Clear M4 M8 then add R S T to M4 M6 M8
#define OP_CHMOD_CL68    56  // Clear M6 M8 then add R S T to M4 M6 M8
#define OP_CHMOD_CL468   57  // Clear M4 M6 M8 then add R S T to M4 M6 M8
#define OP_ADDM4_TRNS    58  // M4+R to M4, if M4 = S continue next instr, else (M4!=S) transfer to T
#define OP_TR_MOD        59  // M4 M6 M8 to REG1
#define OP_TR            60  // Shift C(R) on P, move result to S, transfer to T
#define OP_TR_STOP       61  // do instr 60, stop
#define OP_TR_ROUND      62  // Shift C(R) on P, move result to S rounding, transfer to T, 
#define OP_TR_SGN        63  // if Reg >0 Transfer to R, =0 transfer to S, <0 transfer to T
#define OP_TR_OV         64  // if OV=0 transfer to next instr, else as instr 60
#define OP_TR_AI         65  // if Adjusted Index=0 transfer to next instr, else as instr 60
#define OP_TR_ZR         66  // if Zero result=0 transfer to next instr, else as instr 60
#define OP_TR_EOF        67  // if End of file=0 transfer to next instr, else as instr 60
#define OP_TR_TCHK       68  // if Tape Check failure=0 transfer to next instr, else as instr 60
#define OP_TR_PRT        69  // if printer ready=0 transfer to next instr, else as instr 60
#define OP_TR_EQ         70  // if C(R)=C(S) as 16 digit words, use T as next instr
#define OP_TR_EQ_STOP    71  // if C(R)=C(S) as 16 digit words, use T as next instr, stop
#define OP_TR_NEQ        72  // if C(R)<>C(S) as 16 digit words, use T as next instr
#define OP_TR_NEQ_STOP   73  // if C(R)<>C(S) as 16 digit words, use T as next instr, stop
#define OP_TSTC74        74  // if Sw74 off transfer to next instr, if Sw74 is Trasnfer, do instr 60. if Sw74 is stop do instr 61 
#define OP_TSTC75        75  // if Sw75 ...
#define OP_TSTC76        76  // if Sw76 ...
#define OP_TSTC77        77  // if Sw77 ...
#define OP_TSTC78        78  // if Sw78 ...
#define OP_TSTC79        79  // if Sw79 ... 
#define OP_WAIT_PRT      80  // as 60, exec delayed until printer ready is on
#define OP_PRT1          81  // as 80, start printing on printer 1
#define OP_PRT2          82  // as 80, start printing on printer 2
#define OP_PRT1_SPE      83  // as 80, start printing on printer 1 special function
#define OP_PRT2_SPE      84  // as 80, start printing on printer 2 special function
#define OP_WRITE         90  // write on tape P addr S to R as block T
#define OP_WRI_OUTPUT    91  // as 90, leave space between groups of words for CTC processing
#define OP_DELETE        92  // delete on tape P space for block S-R
#define OP_DEL_OUTPUT    93  // as 92 for words written by 91
#define OP_READ          94  // read forward from tape P, store words of block T at addr S-R
#define OP_READ_BWRD     95  // read backward
#define OP_VERIFY        96  // verify forward 
#define OP_VER_BWRD      97  // verify backward
#define OP_REWIND        98  // err ... any doubt?

/* Symbol tables */
typedef struct 
{
    uint16              opbase;         // opcode number
    const char         *name;           // opcode name 
    int                a;   
}
t_opcode;

extern t_opcode  base_ops[100];

// suboperations
#define SO_Read_Instr      (1 <<  1)
#define SO_Decode          (1 <<  2)
#define SO_Read_Operand_1  (1 <<  3)
#define SO_Read_Operand_2  (1 <<  4)
#define SO_FloatOperands   (1 <<  5)
#define SO_Add             (1 <<  6)
#define SO_Sub             (1 <<  7)
#define SO_Convert         (1 <<  8)
#define SO_Mult            (1 <<  9)
#define SO_Div             (1 << 10)
#define SO_Transfer1to2    (1 << 11)
#define SO_Reset2          (1 << 12)
#define SO_ShiftResult     (1 << 13)
#define SO_Reset1          (1 << 14)
#define SO_Round           (1 << 15)
#define SO_WordAdd         (1 << 16)
#define SO_WordShift       (1 << 17)
#define SO_Change_M4       (1 << 18)
#define SO_Change_M6       (1 << 19)
#define SO_Change_M8       (1 << 20)
#define SO_WriteBlock      (1 << 21)
#define SO_ReadBlock       (1 << 22)
#define SO_Scan            (1 << 23)
#define SO_Terminate       (1 << 24)
#define SO_OprnCompleted   (1 << 25)


#define printfw(d)     (int) (d/D8), (int) (d%D8)  // word format (16 digits)
#define printff(d)     (int) (d/D14), (int) ((d%D14) / (D13)), (int) ((d%D13) / (D12)), (int) ((d%D12) / (D8)), (int) ((d%D8) / (D4)), (int) (d%D4) // float format

/* Decimal helper functions */
extern t_int64 Shift_Digits(t_int64 * d, int nDigits);
extern t_int64 IntegerArith(char cOp, t_int64 d1, t_int64 d2, int nDigits);
extern char * word_to_ascii(char * buf, int CharStart, int CharLen, t_int64 d);

extern void vm_init(void);  /* One time initialization activities now called in cpu_reset() */

