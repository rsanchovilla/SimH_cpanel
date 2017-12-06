/* cpanel.c: simulator control panel simulation

   Copyright (c) 2017, Roberto Sancho

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
   ROBERT M SUPNIK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of Robert M Supnik shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from Robert M Supnik.

   10-Nov-17    RSV     Created for IBM 7000 family control panel support

   This module implements the following control panels:

   IBM 709  + IBM 766 Data Synchronizer for channels A and B + IBM 729 Magnetic Tapes
   IBM 704  + IBM 727 Magnetic Tape
   IBM 7090 + IBM 7617 Data Channel console + IBM 727 Magnetic Tape
   IBM 7094 with same configuration as above
   IBM 7094 same as above + IBM 711 Card Reader + IBM 7631 File Control + IBM 7909 Data Channel + IBM 2302 Disk

*/


#include "cpanel.h"
#include "sim_tape.h"
#include "i7000_defs.h"
#include <math.h>

// cpu state
extern uint16              IC;                         /* program counter */
extern t_uint64            M[MAXMEMSIZE];              /* memory */
extern t_uint64            AC, MQ;                     /* registers */
extern uint16              XR[8];                      /* Index registers */
extern uint16              IC;                         /* program counter */
extern uint16              IR;                         /* Instruction register */
extern t_uint64            ID;                         /* Indicator registers */
extern t_uint64            SR;                         /* Internal temp register */
extern t_uint64            KEYS;                       /* Console Keys */
extern uint8               SL;                         /* Sense lights */
extern uint16              SW;                         /* Sense switch */
extern uint8               MTM;                        /* Multi Index mode */
extern uint8               TM;                         /* Trap mode */
extern uint8               STM;                        /* Special trap mode */ 
extern uint8               CTM;                        /* Copy trap mode */
extern uint8               FTM;                        /* Floating trap mode */
extern uint8               nmode;                      /* Storage null mode */
extern uint8               smode;                      /* Signifigance mode */
extern uint8               itrap;                      /* Can take io traps */
extern uint8               dcheck;                     /* Divide check */
extern uint8               acoflag;                    /* AC Overflow */
extern uint8               mqoflag;                    /* MQ Overflow */
extern uint8               iocheck;
extern uint8               hltinst;                    /* Executed halt instruction */
extern uint8               exe_KEYS;                   /* Execute one instruction */
extern uint8               bcore;                      /* Access to B core memory */
extern uint8               dualcore;                   /* Set to true if dual core in use */

// channel state
extern uint16              wcount[NUM_CHAN];         /* Word count */
extern uint16              caddr[NUM_CHAN];          /* Channel memory address */
extern uint16              location[NUM_CHAN];       /* Pointer to next opcode */
extern uint8               chan_irq[NUM_CHAN];       /* Channel has a irq pending */
extern uint8               cmd[NUM_CHAN];            /* Current command */
extern uint8               sms[NUM_CHAN];            /* Channel mode infomation */
extern uint8               counter[NUM_CHAN];        /* Channel counter */
extern t_stat              chan_reset(DEVICE * dptr);

// mag tape state
extern UNIT                mta_unit[];

// card reader state
extern UNIT                cdr_unit[];

// disk state
extern UNIT                dsk_unit[];
extern uint16              arm_cyl[NUM_DEVS_DSK * 4];
extern uint32              sense[NUM_CHAN * 2];
extern uint8               cmd_buffer[NUM_CHAN];       /* Command buffer per channel */
extern uint8               cmd_mod[NUM_CHAN];          /* Command module per channel */
extern uint32              cmd_option[NUM_CHAN];       /* Command option per channel */
extern uint16              cmd_count[NUM_CHAN];        /* Number of chars recieved */

CP_DEF IBM70X_cp[];
void IBM70X_Init(void);
void IBM70X_Reset(void);
void IBM70X_TickIntensityCount(void);
void IBM70X_Refresh(void);
void IBM70X_OnClick(void);
void IBM70X_OnClick_Sw(void);

// cpanel types constants
#define IS_IBM704          1 
#define IS_IBM709          2 
#define IS_IBM7090         4
#define IS_IBM7094         8 
#define IS_IBM7094_CTSS   16 

#define IS_IBM709X          (IS_IBM7090 | IS_IBM7094 | IS_IBM7094_CTSS)


// control panel available types
CP_TYPE cp_types[] = {
    { "IBM704",       IS_IBM704,       IBM70X_cp,  &IBM70X_Refresh,     &IBM70X_Init,   &IBM70X_Reset,  &IBM70X_TickIntensityCount},
    { "IBM709",       IS_IBM709,       IBM70X_cp,  &IBM70X_Refresh,     &IBM70X_Init,   &IBM70X_Reset,  &IBM70X_TickIntensityCount},
    { "IBM7090",      IS_IBM7090,      IBM70X_cp,  &IBM70X_Refresh,     &IBM70X_Init,   &IBM70X_Reset,  &IBM70X_TickIntensityCount},
    { "IBM7094",      IS_IBM7094,      IBM70X_cp,  &IBM70X_Refresh,     &IBM70X_Init,   &IBM70X_Reset,  &IBM70X_TickIntensityCount},
    { "IBM7094_CTSS", IS_IBM7094_CTSS, IBM70X_cp,  &IBM70X_Refresh,     &IBM70X_Init,   &IBM70X_Reset,  &IBM70X_TickIntensityCount},
    { NULL }
    };

// struct to hold the GUI controls ids
static struct {
   int Reg_AC, Reg_IC, Reg_IN, Reg_MQ, Reg_STOR, Reg_XR, Reg_Sense;
   int Reg_CHN_TAP_CHK, Reg_CHN_SEL;
   int Light_Trap, Light_IOCheck;
   int Light_Select_Tape, Light_Select_Drum, Light_Select_Cards, Light_Select_Direct_Data, Light_Select_Printer;
   int Light_Program_Stop, Light_RW_Select, Light_Divide_Check, Light_Automatic, Light_RW_Check;
   int Light_Simulate, Light_AC_Overflow, Light_Q_Overflow;
   int BTN_Power, BTN_Reset, BTN_Start, BTN_Clear;
   int BTN_Load_Card, BTN_Load_Tape, BTN_Load_Drum, BTN_Single_Step, BTN_Multiple_Step;
   int BTN_Display_A, BTN_Display_B, BTN_Display_C, BTN_Display_Effective_Address;
   int BTN_Enter_MQ, BTN_Enter_Instruction, BTN_Display_Storage, BTN_Display_Indicator;
   int SW_Auto_Manual, SW_Data, SW_Sense;
   // for 709X
   int Light_CHN_Trap, Reg_CHN_Command_Trap, Reg_TAP_CHK_Trap;
   int Reg_XR_A, Reg_XR_B, Reg_XR_C, Reg_XR_D, Reg_XR_E, Reg_XR_F, Reg_XR_G, Light_Multiple_Tag_Mode;
   int SW_Sense_K, SW_Data_K, SW_Reset;
   // for 7094 ctss
   int Light_Relocate_Mode, Light_Protect_Mode, Light_I_Mode, Light_E_Mode;
   int SW_32K_64K, SW_MEM_A_MEM_B;
   // card reader
   int CR_input_tray, CR_output_tray, CR_Light_Sel, CR_Light_Ready;
   // Data Synchronizer
   int DS_AB;
   struct {
      int Data, WC, Addr, Loc, Triggers, Flag_1, Flag_2;
   } DS[2];
   // tapes
   int MT[10], MT_num[10], MT_chan[10], MT_lights[10], MT_head[10], 
	   MT_L[10], MT_R[10], MT_L_VacCol[10], MT_R_VacCol[10];
   // dasd
   int DASD_Head[2];
   // file control
   int FC_FLAG[5], FC_Avail, FC_Ring, FC_IO_Reg, FC_AccMod_Reg, FC_Trk[2], FC_Rec, FC_Chk;
} IBM70X;
 
// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF IBM70X_cp[] = {
    { &IBM70X.Light_Select_Tape,             "Light_Select_Tape",             NULL,     IS_IBM709 }, // class select only in 709
    { &IBM70X.Light_Select_Drum,             "Light_Select_Drum",             NULL,     IS_IBM709 }, 
    { &IBM70X.Light_Select_Cards,            "Light_Select_Cards",            NULL,     IS_IBM709 },
    { &IBM70X.Light_Select_Direct_Data,      "Light_Select_Direct_Data",      NULL,     IS_IBM709 },
    { &IBM70X.Light_Select_Printer,          "Light_Select_Printer",          NULL,     IS_IBM709 },
    { &IBM70X.Light_Program_Stop,            "Light_Program_Stop",            NULL,     0 },
    { &IBM70X.Light_RW_Select,               "Light_RW_Select",               NULL,     0 },
    { &IBM70X.Light_Divide_Check,            "Light_Divide_Check",            NULL,     0 },
    { &IBM70X.Light_Automatic,               "Light_Automatic",               NULL,     0 },
    { &IBM70X.Light_Simulate,                "Light_Simulate",                NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.Light_AC_Overflow,             "Light_AC_Overflow",             NULL,     0 },
    { &IBM70X.Light_Q_Overflow,              "Light_Q_Overflow",              NULL,     0 },
    { &IBM70X.Light_IOCheck,                 "Light_IOCheck",                 NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.Light_RW_Check,                "Light_RW_Check",                NULL,     IS_IBM704 },
    { &IBM70X.Reg_CHN_SEL,                   "Reg_CHN_SEL",                   NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.Reg_CHN_TAP_CHK,               "Reg_CHN_TAP_CHK",               NULL,     0 },
    { &IBM70X.Reg_XR,                        "Reg_XR",                        NULL,     IS_IBM704 | IS_IBM709 },
    { &IBM70X.Reg_STOR,                      "Reg_STOR",                      NULL,     0 },
    { &IBM70X.Reg_AC,                        "Reg_AC",                        NULL,     0 },
    { &IBM70X.Reg_MQ,                        "Reg_MQ",                        NULL,     0 },
    { &IBM70X.Reg_IC,                        "Reg_IC",                        NULL,     0 },
    { &IBM70X.Light_Trap,                    "Light_Trap",                    NULL,     0 },
    { &IBM70X.Reg_IN,                        "Reg_IN",                        NULL,     0 },
    { &IBM70X.Reg_Sense,                     "Reg_Sense",                     NULL,     0 },
    { &IBM70X.BTN_Power,                     "BTN_Power",                     &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Clear,                     "BTN_Clear",                     &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Reset,                     "BTN_Reset",                     &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Start,                     "BTN_Start",                     &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Single_Step,               "BTN_Single_Step",               &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Multiple_Step,             "BTN_Multiple_Step",             &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Load_Card,                 "BTN_Load_Card",                 &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Load_Tape,                 "BTN_Load_Tape",                 &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Load_Drum,                 "BTN_Load_Drum",                 &IBM70X_OnClick,     IS_IBM704 | IS_IBM709 },
    { &IBM70X.BTN_Display_A,                 "BTN_Display_A",                 &IBM70X_OnClick,     IS_IBM704 | IS_IBM709 },
    { &IBM70X.BTN_Display_B,                 "BTN_Display_B",                 &IBM70X_OnClick,     IS_IBM704 | IS_IBM709 },
    { &IBM70X.BTN_Display_C,                 "BTN_Display_C",                 &IBM70X_OnClick,     IS_IBM704 | IS_IBM709 },
    { &IBM70X.BTN_Display_Effective_Address, "BTN_Display_Effective_Address", &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Enter_MQ,                  "BTN_Enter_MQ",                  &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Enter_Instruction,         "BTN_Enter_Instruction",         &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Display_Storage,           "BTN_Display_Storage",           &IBM70X_OnClick,     0 },
    { &IBM70X.BTN_Display_Indicator,         "BTN_Display_Indicator",         &IBM70X_OnClick,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.SW_Auto_Manual,                "SW_Auto_Manual",                &IBM70X_OnClick,     0 },
    { &IBM70X.SW_Sense,                      "SW_Sense",                      &IBM70X_OnClick_Sw,  0 },
    { &IBM70X.SW_Data,                       "SW_Data",                       &IBM70X_OnClick_Sw,  0 },
    // IBM 709X lights
    { &IBM70X.Light_CHN_Trap,                "Light_CHN_Trap",                NULL,     IS_IBM709X },
    { &IBM70X.Reg_CHN_Command_Trap,          "Reg_CHN_Command_Trap",          NULL,     IS_IBM709X },
    { &IBM70X.Reg_TAP_CHK_Trap,              "Reg_TAP_CHK_Trap",              NULL,     IS_IBM709X },
    { &IBM70X.Reg_XR_A,                      "Reg_XR_A",                      NULL,     IS_IBM709X },
    { &IBM70X.Reg_XR_B,                      "Reg_XR_B",                      NULL,     IS_IBM709X },
    { &IBM70X.Reg_XR_C,                      "Reg_XR_C",                      NULL,     IS_IBM709X },
    { &IBM70X.Reg_XR_D,                      "Reg_XR_D",                      NULL,     IS_IBM7094 | IS_IBM7094_CTSS },
    { &IBM70X.Reg_XR_E,                      "Reg_XR_E",                      NULL,     IS_IBM7094 | IS_IBM7094_CTSS },
    { &IBM70X.Reg_XR_F,                      "Reg_XR_F",                      NULL,     IS_IBM7094 | IS_IBM7094_CTSS },
    { &IBM70X.Reg_XR_G,                      "Reg_XR_G",                      NULL,     IS_IBM7094 | IS_IBM7094_CTSS },
    { &IBM70X.Light_Multiple_Tag_Mode,       "Light_Multiple_Tag_Mode",       NULL,     IS_IBM7094 | IS_IBM7094_CTSS },
    // IBM 709X keys
    { &IBM70X.SW_Sense_K,                    "SW_Sense_K",                    NULL,     IS_IBM709X },
    { &IBM70X.SW_Data_K,                     "SW_Data_K",                     NULL,     IS_IBM709X },
    { &IBM70X.SW_Reset,                      "SW_Reset",                      &IBM70X_OnClick, IS_IBM709X },
    // IBM 7094 ctss hw modification
    { &IBM70X.Light_Relocate_Mode,           "Light_Relocate_Mode",           NULL,     IS_IBM7094_CTSS },
    { &IBM70X.Light_Protect_Mode,            "Light_Protect_Mode",            NULL,     IS_IBM7094_CTSS },
    { &IBM70X.Light_I_Mode,                  "Light_I_Mode",                  NULL,     IS_IBM7094_CTSS },
    { &IBM70X.Light_E_Mode,                  "Light_E_Mode",                  NULL,     IS_IBM7094_CTSS },
    { &IBM70X.SW_32K_64K,                    "SW_32K_64K",                    &IBM70X_OnClick,     IS_IBM7094_CTSS },
    { &IBM70X.SW_MEM_A_MEM_B,                "SW_MEM_A_MEM_B",                &IBM70X_OnClick,     IS_IBM7094_CTSS },
    // card reader
    { &IBM70X.CR_input_tray,                 "CR_input_tray",                 NULL,     IS_IBM7094_CTSS },
    { &IBM70X.CR_output_tray,                "CR_output_tray",                NULL,     IS_IBM7094_CTSS },
    { &IBM70X.CR_Light_Sel,                  "CR_Light_Sel",                  NULL,     IS_IBM7094_CTSS },
    { &IBM70X.CR_Light_Ready,                "CR_Light_Ready",                NULL,     IS_IBM7094_CTSS },
    // Data Synchronizer
    { &IBM70X.DS[0].Data,                    "DSA_Data",                      NULL,     IS_IBM709 | IS_IBM709X }, // 709 shows channel A & B
    { &IBM70X.DS[0].WC,                      "DSA_WC",                        NULL,     IS_IBM709 | IS_IBM709X }, // 7090 and 7094 shows channel A only
    { &IBM70X.DS[0].Addr,                    "DSA_Addr",                      NULL,     IS_IBM709 | IS_IBM709X }, // 7094_ctss shows channel C bind to DSB controls
    { &IBM70X.DS[0].Loc,                     "DSA_Loc",                       NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.DS[0].Triggers,                "DSA_TRGS",                      NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.DS[0].Flag_1,                  "DSA_FLAG_1",                    NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.DS[0].Flag_2,                  "DSA_FLAG_2",                    NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.DS_AB,                         "DS_AB",                         NULL,     IS_IBM709 | IS_IBM709X },
    { &IBM70X.DS[1].Data,                    "DSB_Data",                      NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].WC,                      "DSB_WC",                        NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].Addr,                    "DSB_Addr",                      NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].Loc,                     "DSB_Loc",                       NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].Triggers,                "DSB_TRGS",                      NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].Flag_1,                  "DSB_FLAG_1",                    NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    { &IBM70X.DS[1].Flag_2,                  "DSB_FLAG_2",                    NULL,     IS_IBM709 | IS_IBM7094_CTSS },
    // file control
    { &IBM70X.FC_FLAG[0],                    "FC_FLAG_0",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_FLAG[1],                    "FC_FLAG_1",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_FLAG[2],                    "FC_FLAG_2",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_FLAG[3],                    "FC_FLAG_3",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_FLAG[4],                    "FC_FLAG_4",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Avail,                      "FC_AVAIL",                      NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Ring,                       "FC_DIGITS_RING",                NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_IO_Reg,                     "FC_IO_REG",                     NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_AccMod_Reg,                 "FC_ACC_REG",                    NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Trk[0],                     "FC_TRK_0",                      NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Trk[1],                     "FC_TRK_1",                      NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Rec,                        "FC_REC",                        NULL,     IS_IBM7094_CTSS },
    { &IBM70X.FC_Chk,                        "FC_CHK",                        NULL,     IS_IBM7094_CTSS },
    // dasd
    { &IBM70X.DASD_Head[0],                  "DASD_1_Head",                   NULL,     IS_IBM7094_CTSS },
    { &IBM70X.DASD_Head[1],                  "DASD_2_Head",                   NULL,     IS_IBM7094_CTSS },
    // tapes
    { &IBM70X.MT[1],                         "MT_1",                          NULL,     0 },
    { &IBM70X.MT_num[1],                     "MT_1_number",                   NULL,     0 },
    { &IBM70X.MT_chan[1],                    "MT_1_channel",                  NULL,     0 },
    { &IBM70X.MT_lights[1],                  "MT_1_lights",                   NULL,     0 },
    { &IBM70X.MT_L[1],                       "MT_1_L",                        NULL,     0 },
    { &IBM70X.MT_R[1],                       "MT_1_R",                        NULL,     0 },
    { &IBM70X.MT_head[1],                    "MT_1_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[1],                "MT_1_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[1],                "MT_1_R_VacCol",                 NULL,     0 },
	{ &IBM70X.MT[2],                         "MT_2",                          NULL,     0 },
    { &IBM70X.MT_num[2],                     "MT_2_number",                   NULL,     0 },
    { &IBM70X.MT_chan[2],                    "MT_2_channel",                  NULL,     0 },
    { &IBM70X.MT_lights[2],                  "MT_2_lights",                   NULL,     0 },
    { &IBM70X.MT_L[2],                       "MT_2_L",                        NULL,     0 },
    { &IBM70X.MT_R[2],                       "MT_2_R",                        NULL,     0 },
    { &IBM70X.MT_head[2],                    "MT_2_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[2],                "MT_2_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[2],                "MT_2_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[3],                         "MT_3",                          NULL,     0 },
    { &IBM70X.MT_chan[3],                    "MT_3_channel",                  NULL,     0 },
    { &IBM70X.MT_num[3],                     "MT_3_number",                   NULL,     0 },
    { &IBM70X.MT_lights[3],                  "MT_3_lights",                   NULL,     0 },
    { &IBM70X.MT_L[3],                       "MT_3_L",                        NULL,     0 },
    { &IBM70X.MT_R[3],                       "MT_3_R",                        NULL,     0 },
    { &IBM70X.MT_head[3],                    "MT_3_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[3],                "MT_3_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[3],                "MT_3_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[4],                         "MT_4",                          NULL,     0 },
    { &IBM70X.MT_chan[4],                    "MT_4_channel",                  NULL,     0 },
    { &IBM70X.MT_num[4],                     "MT_4_number",                   NULL,     0 },
    { &IBM70X.MT_lights[4],                  "MT_4_lights",                   NULL,     0 },
    { &IBM70X.MT_L[4],                       "MT_4_L",                        NULL,     0 },
    { &IBM70X.MT_R[4],                       "MT_4_R",                        NULL,     0 },
    { &IBM70X.MT_head[4],                    "MT_4_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[4],                "MT_4_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[4],                "MT_4_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[5],                         "MT_5",                          NULL,     0 },
    { &IBM70X.MT_num[5],                     "MT_5_number",                   NULL,     0 },
    { &IBM70X.MT_chan[5],                    "MT_5_channel",                  NULL,     0 },
    { &IBM70X.MT_lights[5],                  "MT_5_lights",                   NULL,     0 },
    { &IBM70X.MT_L[5],                       "MT_5_L",                        NULL,     0 },
    { &IBM70X.MT_R[5],                       "MT_5_R",                        NULL,     0 },
    { &IBM70X.MT_head[5],                    "MT_5_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[5],                "MT_5_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[5],                "MT_5_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[6],                         "MT_6",                          NULL,     0 },
    { &IBM70X.MT_chan[6],                    "MT_6_channel",                  NULL,     0 },
    { &IBM70X.MT_num[6],                     "MT_6_number",                   NULL,     0 },
    { &IBM70X.MT_lights[6],                  "MT_6_lights",                   NULL,     0 },
    { &IBM70X.MT_L[6],                       "MT_6_L",                        NULL,     0 },
    { &IBM70X.MT_R[6],                       "MT_6_R",                        NULL,     0 },
    { &IBM70X.MT_head[6],                    "MT_6_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[6],                "MT_6_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[6],                "MT_6_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[7],                         "MT_7",                          NULL,     0 },
    { &IBM70X.MT_chan[7],                    "MT_7_channel",                  NULL,     0 },
    { &IBM70X.MT_num[7],                     "MT_7_number",                   NULL,     0 },
    { &IBM70X.MT_lights[7],                  "MT_7_lights",                   NULL,     0 },
    { &IBM70X.MT_L[7],                       "MT_7_L",                        NULL,     0 },
    { &IBM70X.MT_R[7],                       "MT_7_R",                        NULL,     0 },
    { &IBM70X.MT_head[7],                    "MT_7_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[7],                "MT_7_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[7],                "MT_7_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[8],                         "MT_8",                          NULL,     0 },
    { &IBM70X.MT_num[8],                     "MT_8_number",                   NULL,     0 },
    { &IBM70X.MT_chan[8],                    "MT_8_channel",                  NULL,     0 },
    { &IBM70X.MT_lights[8],                  "MT_8_lights",                   NULL,     0 },
    { &IBM70X.MT_L[8],                       "MT_8_L",                        NULL,     0 },
    { &IBM70X.MT_R[8],                       "MT_8_R",                        NULL,     0 },
    { &IBM70X.MT_head[8],                    "MT_8_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[8],                "MT_8_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[8],                "MT_8_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[9],                         "MT_9",                          NULL,     0 },
    { &IBM70X.MT_num[9],                     "MT_9_number",                   NULL,     0 },
    { &IBM70X.MT_chan[9],                    "MT_9_channel",                  NULL,     0 },
    { &IBM70X.MT_lights[9],                  "MT_9_lights",                   NULL,     0 },
    { &IBM70X.MT_L[9],                       "MT_9_L",                        NULL,     0 },
    { &IBM70X.MT_R[9],                       "MT_9_R",                        NULL,     0 },
    { &IBM70X.MT_head[9],                    "MT_9_head",                     NULL,     0 },
	{ &IBM70X.MT_L_VacCol[9],                "MT_9_L_VacCol",                 NULL,     0 },
	{ &IBM70X.MT_R_VacCol[9],                "MT_9_R_VacCol",                 NULL,     0 },
    { &IBM70X.MT[0],                         "MT_10",                         NULL,     0 },
    { &IBM70X.MT_num[0],                     "MT_10_number",                  NULL,     0 },
    { &IBM70X.MT_chan[0],                    "MT_10_channel",                 NULL,     0 },
    { &IBM70X.MT_lights[0],                  "MT_10_lights",                  NULL,     0 },
    { &IBM70X.MT_L[0],                       "MT_10_L",                       NULL,     0 },
    { &IBM70X.MT_R[0],                       "MT_10_R",                       NULL,     0 },
    { &IBM70X.MT_head[0],                    "MT_10_head",                    NULL,     0 },
	{ &IBM70X.MT_L_VacCol[0],                "MT_10_L_VacCol",                NULL,     0 },
	{ &IBM70X.MT_R_VacCol[0],                "MT_10_R_VacCol",                NULL,     0 },
    { NULL }
};

//======================= IBM709 ==============================
int Index_Reg_Selected = 0;
int Multiple_Step_Mode = 0;
int Clearing_Main_Mem_Mode = 0;
double MT_pos_reel_R[10];
int MT_L_reel_color[10], MT_R_reel_color[10], 
    MT_load_animation[10], // 0 -> Load animation can start it tape file attached, 1-> Load animation in progress, -1 -> Load Animation Done
    MT_rew_animation[10];  // 0 -> Rew animation not in progress and can start if tape command given, 1 -> Rew animation in progress. 
    MT_unitAtCabinet[10]; // indicated the mta_unit entry (chan+unit) that is shown on cabinet i
struct {
   int FrameCount;       // indicates the animation's current frame count 
   int Stage;            // stage of animation
   int StageFrameStart;  // Frame this stage started
   int Data;             // data to be kept for the current stage
} MT_animation[10];      // pesrsistent data for animation progress
int arm_cyl_current[NUM_DEVS_DSK * 4]; // current arms positions on last refresh
int DASD_wanted_cyl[2];  // state where the disk animation head should go
int DASD_current_cyl[2]; // current state for disk animation head 
int crd_ncards_in_output_tray; // number of cards readed counted by read animation


int bDataChannelVisible;
int bTapesVisible, bTapeInitCabinets; 
int bDASDVisible;
int bCardReadVisible; 
int bIbm727Tape;

void IBM70X_Init(void)
{
    char MT_color_opt[16] = "MTx";
    char c;
    int i, j, n1, n2, chan, unit;

    if (IsOption("OnlyMainCpu")) {
        bDataChannelVisible = bTapesVisible = bTapeInitCabinets = bDASDVisible = bCardReadVisible = 0;
    } else {
        bTapesVisible = 1; bTapeInitCabinets = 0;
        bDataChannelVisible = (cpanel_on != IS_IBM704) ? 1 : 0;
        bDASDVisible = bCardReadVisible = (cpanel_on == IS_IBM7094_CTSS) ? 1 : 0;
    }
    if (bTapesVisible) {
        for (i=0;i<10;i++) {
            // iterate on tape cabinets to define for each of then the tape attached: channel, unit, color of reels
            MT_color_opt[2] = '0' + i;
            chan = unit = n1 = n2 = 0;
            if ((IsOption(MT_color_opt)) && (IsOptionParam) && (*IsOptionParam++ == '/')) {
                // syntax: option=mt1/b2/YB <- tape cabinet 1 is set to tape unit 2 in channel B, reels color Yellow (Left) and Blue (irght)
                c = *IsOptionParam++; // get channel
                if ((c == 0) || (c == ',')) goto end_mtn_opts;
                if ((c >= 'a') && (c <= 'z')) c = c - 'a' + 'A';
                if ((c >= 'A') & (c <= 'H')) { 
                    chan = c + 1 - 'A'; 
                    if (chan > NUM_DEVS_MT) {
                        fprintf(stderr, "Invalid set cpanel option=%s: higher channel allowed %c, ussing %c\n", MT_color_opt, NUM_DEVS_MT + 'A'-1, c);
                        chan = -1; 
                    }
                } else {
                    fprintf(stderr, "Invalid set cpanel option=%s: invalid channel %c\n", MT_color_opt, c);
                    chan = -1;
                }
                c = *IsOptionParam++; // get cabinet number (0-9)
                if ((c == 0) || (c == ',')) goto end_mtn_opts;
                if ((c >= '0') & (c <= '9')) { unit = c - '0'; } else {
                    fprintf(stderr, "Invalid set cpanel option=%s: invalid unit %c (must be 0 to 9)\n", MT_color_opt, c);
                    chan = -1;
                }
                c = *IsOptionParam++; // get second '/' separator
                if ((c == 0) || (c == ',')) goto end_mtn_opts;
                if (c != '/') goto end_mtn_opts;
                c = *IsOptionParam++; // get left reel color
                if ((c == 0) || (c == ',')) goto end_mtn_opts;
                if ((c >= 'a') && (c <= 'z')) c = c - 'a' + 'A';
                if (c == 'Y') {n1 = 1; /* yellow reel */ } else if (c == 'B') {n1 = 2; /* blue/black reel */ }
                if (c == 'G') {n1 = 1; /* gray reel */ } else if (c == 'D') {n1 = 2; /* dark reel */ }
                c = *IsOptionParam++; 
                if ((c == 0) || (c == ',')) goto end_mtn_opts;
                if ((c >= 'a') && (c <= 'z')) c = c - 'a' + 'A';
                if (c == 'Y') {n2 = 1; /* yellow reel */ } else if (c == 'B') {n2 = 2; /* blue/black reel */ };
                if (c == 'G') {n2 = 1; /* gray reel */ } else if (c == 'D') {n2 = 2; /* dark reel */ }
            }
         end_mtn_opts:
            MT_L_reel_color[i] = n1; 
            MT_R_reel_color[i] = n2; 
            MT_load_animation[i] = 0;
            MT_rew_animation[i] = 0;
            MT_pos_reel_R[i] = 0;
            if (chan < 0) {
                MT_unitAtCabinet[i] = -1; // bad configured tape cabinet 
            } else if (chan == 0) {
                MT_unitAtCabinet[i] = -2; // this cabined is not defined, use defaults; 
            } else {
                MT_unitAtCabinet[i] = (chan - 1) * 10 + unit;
                if (i>0) for (j=0;j<i;j++) if (MT_unitAtCabinet[j] == MT_unitAtCabinet[i]) {
                    // this tape already assigned to a previous cabinet, set as bad configured
                    MT_unitAtCabinet[i] = -1; // bad configured tape cabinet
                    fprintf(stderr, "Invalid set cpanel option=%s: tape already assigned at cabinet %d\n", MT_color_opt, j);
                    break;
                }
                
            }
        }
        // if default unit for cabinet is defined in another cabinet
        for (i=0;i<10;i++) if ( MT_unitAtCabinet[i] == -2) {
            for (j=0;j<10;j++) if (MT_unitAtCabinet[j] == i) {
               // this tape already assigned to a previous cabinet, set as bad configured
               MT_unitAtCabinet[i] = -1; // bad configured tape cabinet
               fprintf(stderr, "Tape in cabinet %d already assigned at cabinet %d\n", i, j);
               break;
            }
        }
        // populate defaults cabined with unit i channel A
        for (i=0;i<10;i++) if (MT_unitAtCabinet[i] == -2) MT_unitAtCabinet[i] = i;
    }
    if (bDASDVisible) {
        for (i=0;i<2;i++) DASD_wanted_cyl[i] = DASD_current_cyl[i]=0;
        for (i=0;i<NUM_DEVS_DSK * 4;i++) arm_cyl_current[i] = 0;
    }
    Index_Reg_Selected = 1;
    Multiple_Step_Mode = Clearing_Main_Mem_Mode = 0;
    cpanel_Press_and_Release = 1;    // OnClick events for key press and release; 
    if (cpanel_on == IS_IBM704) {
        bIbm727Tape = 1;
    } else {
        bIbm727Tape = 0; // use 727 tape model
    }
    crd_ncards_in_output_tray = 0;
}

void IBM70X_Reset(void)
{
}

int last_fc_cmd_count = 0;
int fc_ring(int chan) 
{
    int i, n;
    i = cmd_count[chan];
    if (last_fc_cmd_count != i) {
        last_fc_cmd_count = i;
        n = 1;
    } else {
        n = 0;
    }
    if (i > 5) {
        n |= 4; // on light on as counter wraps to 6+7 that is same light than 0+1
        i = i - 6;
    }
    n |= (1 << (i + 3));
    return n;
}


void IBM70X_TickIntensityCount(void)
{
    int i,n,chan;
    // count times bits set in register to be able to calc on refresh callback
    // the intensity on light on each bits
    if (cpanel_on <= IS_IBM709) {
        TickCount(IBM70X.Reg_XR, XR[Index_Reg_Selected]);
    } else if (cpanel_on == IS_IBM7090) {
        TickCount(IBM70X.Reg_XR_A, XR[1]);
        TickCount(IBM70X.Reg_XR_B, XR[2]);
        TickCount(IBM70X.Reg_XR_C, XR[4]);
    } else {
        TickCount(IBM70X.Reg_XR_A, XR[1]);
        TickCount(IBM70X.Reg_XR_B, XR[2]);
        TickCount(IBM70X.Reg_XR_C, XR[4]); // not an error: XR_C is index 4, XR_D is index 3
        TickCount(IBM70X.Reg_XR_D, XR[3]);
        TickCount(IBM70X.Reg_XR_E, XR[5]);
        TickCount(IBM70X.Reg_XR_F, XR[6]);
        TickCount(IBM70X.Reg_XR_G, XR[7]);
    }
    TickCount(IBM70X.Reg_STOR, SR);
    TickCount(IBM70X.Reg_AC, AC);
    TickCount(IBM70X.Reg_MQ, MQ);
    TickCount(IBM70X.Reg_IC, IC);
    TickCount(IBM70X.Light_AC_Overflow, acoflag);     /* AC Overflow */
    TickCount(IBM70X.Reg_IN, IR);
    // data synchronizer lights
    if (bDataChannelVisible) {
        n = (cpanel_on == IS_IBM709) ? 2 : 1;   // IBM 709 -> IBM 766 data synchronizer shoes Chan A & B, 
                                                // IBM 709X -> IBM 7617 data channel console only shows one channel: chan A
        for (i=0;i<n;i++) {
            TickCount(IBM70X.DS[i].Data,    assembly[i+1]);     // data for channel A: DS[0]=chan A, assembly[1]=chan A
            TickCount(IBM70X.DS[i].WC,      wcount[i+1]);       // word count
            TickCount(IBM70X.DS[i].Addr,    caddr[i+1]); 
            TickCount(IBM70X.DS[i].Loc,     location[i+1]); 
        }
    }
    if (bDASDVisible) {
        // regs for chammel C goes to DS[1] 
        chan = 3;
        TickCount(IBM70X.DS[1].Data,    assembly[chan]);
        TickCount(IBM70X.DS[1].WC,      wcount[chan]
            + ((sms[chan] & 020) ? (1 << 15) : 0)        /* BCD Xlat mode */
            + ((sms[chan] & 040) ? (1 << 16) : 0)        /* Read backward */
        );   
        TickCount(IBM70X.DS[1].Addr,    caddr[chan]); 
        TickCount(IBM70X.DS[1].Loc,     location[chan]); 

        TickCount(IBM70X.FC_Ring, fc_ring(3)); 
        TickCount(IBM70X.FC_IO_Reg, assembly[chan] & 077); 
    }
}

void DASD_Refresh(void)
{
    int i,n,chan;
    int dcmd, dacc, dopt, dopt2, fc_flag[5], sel, schan, chk; 

    // data for Data Synchronizer 7909 

    chan = 3; // shows channel C on IBM 7909 control panel
    // channel registers
    SetStateWithIntensity(IBM70X.DS[1].Data, assembly[chan]); 
    SetStateWithIntensity(IBM70X.DS[1].Loc, location[chan]); 
    SetStateWithIntensity(IBM70X.DS[1].Addr, caddr[chan]); 
    SetStateWithIntensity(IBM70X.DS[1].WC, wcount[chan]  
            + ((sms[chan] & 020) ? (1 << 15) : 0)       // BCD Xlat mode 
            + ((sms[chan] & 040) ? (1 << 16) : 0)       // Read backward 
        ); 
    
    n = chan_flags[chan];
    n = ((cmd[chan] >> 2) | (cmd[chan] & 1))
      + (counter[chan] << 6) 
      + ((n & SNS_IOCHECK)  ? (1 << 12) : 0) 
      + ((n & SNS_SEQCHECK) ? (1 << 13) : 0) 
      + ((n & SNS_UEND)     ? (1 << 14) : 0) 
      + ((n & SNS_ATTN1)    ? (1 << 15) : 0) 
      + ((n & SNS_ATTN2)    ? (1 << 16) : 0) 
      + ((n & SNS_ADCHECK)  ? (1 << 17) : 0);
    SetState(IBM70X.DS[1].Flag_1, n); 

    n = chan_flags[chan];
    n = // XXX select 2 light is missing
      + ((n & CTL_PREAD)  ? (1 << 12) : 0)  // Prepare to read 
      + ((n & CTL_PWRITE) ? (1 << 13) : 0)  // Prepare to write 
      + ((n & CTL_READ)   ? (1 << 14) : 0)  // Read Status 
      + ((n & CTL_WRITE)  ? (1 << 15) : 0)  // Write Status 
      + ((n & SNS_IRQ)    ? (1 << 16) : 0)  // IRQ
      ; // XXX Wait light is missing
    SetState(IBM70X.DS[1].Flag_2, n); 

    // shows 2302 disk arm for access 0 and 1
    for (i=0;i<NUM_DEVS_DSK * 4;i++) {
        if (arm_cyl_current[i] == arm_cyl[i]) continue;
        n = arm_cyl_current[i] = arm_cyl[i]; 
        while (n >= 22) n -= 22;
        DASD_wanted_cyl[i & 1] = n; // disk head on cpanel wants to go to pos that has changed from last refresh
    }
    for (i=0;i<2;i++) {
        //  realistic mode: use access 0&1 from disk 0
        //  up to 256 cyl, cyl 000 = outermost 
        //  n = arm_cyl[i];
        //  n = (256 - n) * 22 / 256; 
        // pretty nice unrealistic mode: use access 0&1 from any disk
        // only 22 cyl
        n = DASD_wanted_cyl[i]; 
        n = n - DASD_current_cyl[i];
        if (n > 4) {n = 4;} else if (n < -4) n = -4;
        DASD_current_cyl[i] += n;
        SetState(IBM70X.DASD_Head[i], DASD_current_cyl[i]);
    }

    // shows 7631 file control

    n = cmd_count[chan];
    dcmd  = (n <  2) ? -1 : cmd_buffer[chan];
    dacc  = (n <  4) ? -1 : cmd_mod[chan];
    dopt  = (n <  8) ? -1 : cmd_option[chan] >> 16;
    dopt2 = (n < 10) ? -1 : cmd_option[chan] & 07777;

    if (chan_flags[chan] & DEV_SEL) {
        n = 1; // chan conected -> file control availability=1
    } else {
        dcmd = -1;  // channed chan disconected -> no command in progress
        n = 4;      // chan disconected -> file control released
    }
    SetState(IBM70X.FC_Avail, n);

    fc_flag[1] = fc_flag[2] = fc_flag[3] = fc_flag[4] = 0; // flags for light on command executed

    n = (chan_flags[chan] & CTL_SNS)  ? 2 : 0 
      + (chan_flags[chan] & CTL_CNTL) ? 1 : 0 
      + (chan_flags[chan] & SNS_UEND) ? 8 : 0;
    fc_flag[0] = n;
    if (n == 0) {
        n = (chan_flags[chan] & CTL_READ)  ? 1 : 0 
          + (chan_flags[chan] & CTL_WRITE) ? 2 : 0
          + (dsk_unit[0].u5 & 0x0200) ? 4 : 0 
          + ((chan_flags[chan] & SNS_ATTN1) || (chan_flags[chan] & SNS_ATTN2)) ? 16 : 0;
        fc_flag[1] |= n;    
    }

    switch(dcmd) {
        case 0x00: i=1; n=32; break; // #define DNOP            0x00            /* Nop */
        case 0x80: i=3; n= 4; break; // #define DSEK            0x80            /* Seek */
        case 0x87: i=3; n= 1; break; // #define DSAI            0x87            /* Set Access Inoperative */
        case 0x86: i=2; n=32; break; // #define DWRC            0x86            /* Prepare to Write Check */
        case 0x89: i=2; n=16; break; // #define DVHA            0x89            /* Prepare to Verify home addr */
        case 0x88: i=2; n= 4; break; // #define DVTA            0x88            /* Prepare to Verify track with addr */
        case 0x85: i=4; n=32; break; // #define DVCY            0x85            /* Prepare to Verify Cyl Operation*/
        case 0x83: i=2; n= 8; break; // #define DWRF            0x83            /* Prepare to Write Format */
        case 0x82: i=2; n= 1; break; // #define DVSR            0x82            /* Prepare to Verify single record */
        case 0x84: i=2; n= 2; break; // #define DVTN            0x84            /* Prepare to Verify track no addr */
        default: i=-1;
    }
    if (i >= 0) fc_flag[i] |= n;

    chan = UNIT_G_CHAN(dsk_unit[0].flags);
    sel = (dsk_unit[0].flags & UNIT_SELECT) ? 1 : 0;
    schan = (chan * 2) + sel;
    chk = sense[schan];
    if ((chk & 0x04)==0) fc_flag[3] |= 2;                 // STAT_SIXBIT 0x00004
    if ((chk & 0x10020) == 0x10020) fc_flag[0] |= 4;      // #define EXPT_DSKCHK     0x10020         /* Disk storage error */
    if ((chk & 0x10040) == 0x10040) fc_flag[4] |= 1;      // #define STAT_NOTRDY     0x10040         /* Disk no ready */
    if ((chk & 0x10080) == 0x10080) fc_flag[4] |= 2;      // #define STAT_OFFLINE    0x10080         /* Disk offline */

    SetStateWithIntensity(IBM70X.FC_IO_Reg, assembly[chan] & 077); 
    if (dacc < 0) {
       n = 0;
    } else {
       // doc 223-2766-1_7631_File_Control_CE_Instruction_Maintenance_Sep64.pdf says only 5 modules can be
       // attached to IBM 7631 File control (p9). Also on CE panel photo (p119) there is only 5 modules
       // ctss uses 6 modules on channel c, so an extra module light is added to cpanel
       n = 1 << ((dacc & 0x0f) + 2);
       n += (dacc & 0xf0) ? 2 : 1;
    }
    SetState(IBM70X.FC_AccMod_Reg, n);

    if (dopt < 0) {
        SetState(IBM70X.FC_Trk[0], 0);
        SetState(IBM70X.FC_Trk[1], 0);
    } else {
        n = (dopt & 0xff) * 2;
        SetState(IBM70X.FC_Trk[0], n);
        n = (dopt >> 8) * 2;
        SetState(IBM70X.FC_Trk[1], n);
    }

    n = (dopt2 < 0) ? 0: dopt2; 
    SetState(IBM70X.FC_Rec, n);

    for (i=0; i<5; i++) 
        SetState(IBM70X.FC_FLAG[i], fc_flag[i]);
    
    SetStateWithIntensity(IBM70X.FC_Ring, fc_ring(chan)); 

    n = ((chk & 0x41000) == 0x41000) ?   1 : 0     //  #define PROG_NOREC      0x41000         /* No record found */
      + ((chk & 0x48000) == 0x48000) ?   2 : 0     //  #define PROG_INVSEQ     0x48000         /* Invalid sequence */
      + ((chk & 0x20100) == 0x20100) ?   4 : 0     //  #define DATA_PARITY     0x20100         /* Data parity error */
      + ((chk & 0x40800) == 0x40800) ?   8 : 0     //  #define PROG_INVADDR    0x40800         /* Invalid seek address */
      + ((chk & 0x10010) == 0x10010) ?  16 : 0     //  #define EXPT_FILECHK    0x10010         /* File control check error */
      + ((chk & 0x42000) == 0x42000) ?  32 : 0     //  #define PROG_FMTCHK     0x42000         /* Format check */
      + ((chk & 0x20200) == 0x20200) ?  64 : 0     //  #define DATA_CHECK      0x20200         /* Compare error */
      + ((chk & 0x44000) == 0x44000) ? 128 : 0     //  #define PROG_INVCODE    0x44000         /* Invalid code */
      + ((chk & 0x20400) == 0x20400) ? 256 : 0;    //  #define DATA_RESPONSE   0x20400         /* Response check */
    SetState(IBM70X.FC_Chk, n);

    // light not handled in file control panel:
    // stop, ph sel a, ph sel b, acu sel, write/read gate, read data, format states, bit ring, serial register
}

int decode_chan, decode_unit_type, decode_unit_num, decode_unit_bcd;

void decode_ioaddr(int ioaddr)
{
    int addr;
    decode_chan = (ioaddr >> 9) & 017;
    addr = (ioaddr & 0377);
    decode_unit_type = 0; decode_unit_num = 0; decode_unit_bcd = 0;
    switch (addr) {
        case 0201: case 0202: case 0203: case 0204: case 0205: 
        case 0206: case 0207: case 0210: case 0211: case 0212:  // tape, units 1-10, BCD
            decode_unit_type = 1; decode_unit_num = (addr - 0201) + 1; decode_unit_bcd = 1; break;
        case 0221: case 0222: case 0223: case 0224: case 0225: 
        case 0226: case 0227: case 0230: case 0231: case 0232:  // tape, units 1-10, Binary
            decode_unit_type = 1; decode_unit_num = (addr - 0221) + 1; decode_unit_bcd = 0; break;
        case 0321:                                              // card reader
            decode_unit_type = 2; break;
        case 0341:                                              // card punch
            decode_unit_type = 3; break;
        case 0361:                                              // printer, BCD
            decode_unit_type = 4; decode_unit_bcd = 1; break;
        case 0362:                                              // printer, Binary
            decode_unit_type = 4; decode_unit_bcd = 0; break;
        case 0301: case 0302: case 0303: case 0304:             // drum
        case 0305: case 0306: case 0307: case 0310: 
            decode_unit_type = 5; break;
        case 0030:                                              // CRT (direct data)
            decode_unit_type = 6; break; 
    }
}

void Data_Channel_Refresh(void)
{
    int i, chan, dcmd, ioaddr, n1, n2, nChan;
    int tape_eof, tape_err, tape_bot, tape_eot, trigger;
    UNIT *uptr;

    nChan = (cpanel_on == IS_IBM709) ? 2 : 1;   // IBM 709 -> IBM 766 data synchronizer shoes Chan A & B, 
                                                // IBM 709X -> IBM 7617 data channel console only shows one channel: chan A
    for (chan=1;chan<=nChan;chan++) {
        // channel registers
        i = chan-1;
        SetStateWithIntensity(IBM70X.DS[i].Data, assembly[chan]); 
        SetStateWithIntensity(IBM70X.DS[i].WC, wcount[chan]); 
        SetStateWithIntensity(IBM70X.DS[i].Addr, caddr[chan]); 
        SetStateWithIntensity(IBM70X.DS[i].Loc, location[chan]); 
        trigger = ((cmd[chan] & 070) > 3) + 
                    16 * ((cmd[chan] & 1) ? 1:0) + 
                    32 * ((cmd[chan] & 4) ? 1:0);
        SetState(IBM70X.DS[i].Triggers, trigger); 
        // tape status
        tape_err = tape_eof = tape_bot = tape_eot = 0;
        n1 = chan_unit[chan].u6; // last command executed in channel
        dcmd   = (n1 >> 8);
        ioaddr = (n1 & 0377);
        decode_ioaddr(ioaddr);
        if (sim_is_running == 0) decode_unit_type = 0; // if CPU halted, all dev are disconnected
        if (decode_unit_type == 1) {                   // tape
            i = decode_unit_num; if (i == 10) i=0; // unit 10 is [0]
            uptr = &mta_unit[i + 10*(chan-1)];
            if (uptr->us10) tape_err = 1; 
            if ((uptr->u5 & 037) == 0) dcmd = 0; // MT_CMDMSK -> last command executed in channel has finished
            if (sim_tape_bot(uptr)) tape_bot = 1;
            if (sim_tape_eot(uptr)) tape_eot = 1;
            tape_eof = 0; // XXX how do I test this?
            n1 = 0;
            if (dcmd == IO_RDS)     n1 |= (1 << 0);
            if (dcmd == IO_WRS)     n1 |= (1 << 1);
            if (dcmd == IO_WEF)     n1 |= (1 << 3);
            if (dcmd == IO_BSR)     n1 |= (1 << 5);
            if (dcmd == IO_BSF)     n1 |= (1 << 6);
            if (dcmd == IO_REW)     {n1 |= (1 << 4); tape_bot = 0;} // if rew in progress, bot is not reliable
            if ((n1 == 0) && (tape_err == 0))  {
                tape_eof = tape_bot = tape_eot = 0; // no dcmd -> clear all lights
            } else {
                n1 |= (1 << (7 + decode_unit_num)); // unit selected that executed dcmd command
                if (decode_unit_bcd)  n1 |= (1 << 2);
            }
            SetState(IBM70X.DS[chan-1].Flag_1, n1);
        } else {
            // clear tape flags
            SetState(IBM70X.DS[chan-1].Flag_1, 0);
        }
        n2 = 0;
        if (decode_unit_type == 2) {            // card reader
            if (dcmd == IO_RDS)     n2 |= (1 << 0);
            if (dcmd == IO_WRS)     n2 |= (1 << 1);
        } else if (decode_unit_type == 3) {     // card punch
            if (dcmd == IO_WRS)     n2 |= (1 << 2);
        } else if (decode_unit_type == 4) {     // printer
            if (dcmd == IO_RDS)     n2 |= (1 << 3);
            if (dcmd == IO_WRS)     n2 |= (1 << 4);
        }
        if (sim_is_running) {
            if (tape_err)               n2 |= (1 << 10);
            if (tape_eot)               n2 |= (1 << 11);
            if (tape_bot)               n2 |= (1 << 12);
            if (tape_eof)               n2 |= (1 << 13);
        }
        if (wcount[chan] == 0)          n2 |= (1 << 14);
        SetState(IBM70X.DS[chan-1].Flag_2, n2);
    }
}

void CardReader_Refresh(void)
{
    int cdr_ready, cdr_selected;
    int n, nDeck, nRead;  

    cdr_selected = ((decode_unit_type == 2) && (chan_flags[1] & DEV_SEL)) ? 1 : 0;

    cdr_ready = (cdr_unit[0].flags & UNIT_DIS) ? 0 : 1;

    if (cdr_unit[0].flags & UNIT_ATT) {
        n = cdr_unit[0].u6;
        nDeck = (n >> 16);    // total number of cards in deck
        nRead = (n & 0xFFFF); // number of read cards 
        n = nDeck - nRead;
        if (n == 1) {
            // if read in progress, last card into the readed (not in input tray)
            // if no read in progress, last card in input tray
            n = (cdr_unit[0].u5 & URCSTA_READ) ? 0 : 1; 
        } else if ((n > 0) && (n < 3)) {
            n = 1; 
        } else {
            n = n / 3;
        }
        if (n > 23) {n = 23;} else if (n < 0) n = 0;
        SetState(IBM70X.CR_input_tray, n);
    } else {
        // no file attached -> no card in reader
        nDeck = nRead = 0; 
        SetState(IBM70X.CR_input_tray, 0);
        SetState(IBM70X.CR_output_tray, 0);
    }
    // set lights
    SetState(IBM70X.CR_Light_Sel, cdr_selected);
    SetState(IBM70X.CR_Light_Ready, cdr_ready);
    // do card processing animation
    n = (int) GetState(IBM70X.CR_output_tray);
    if ((n == 8) | (n == 15)) {
        // end of card read animation -> leave card in output tray
        n = 1;
        crd_ncards_in_output_tray++; // incr cards in putput tray;
    } else if (n >= 2) {
        // card read animation in progress
        n++;
    } else if (cdr_unit[0].u5 & URCSTA_READ) {
        // no card animation, but read in progress. Should we start a new card read aimation?
        if (nDeck <= nRead) {
            // no, because all cards from deck already read (so no cards waiting in input tray)
        } else if (((nRead > 0) && (nRead <= crd_ncards_in_output_tray)) ||
                   ((nRead == 0) && (crd_ncards_in_output_tray > 0))) {
            // no, because output tray holds the same card count as readed cards
        } else {
            // start new card read animation
            if (n == 0) {n = 2;} else {n = 9;}
        }
    } else {
        // no read in progress. Have we missed some reads?
        if ((n == 0) && (nRead > 0)) {
            // no cards in out tray but some have been read -> set state as end of animation for card read
            n = 1;
        }
    }
    SetState(IBM70X.CR_output_tray, n);
}

void get_MT_cabinet_chan_unit(int i, int * chan, int * unit)
{
    if (((cpu_unit.flags >> (16 + 4)) & 0x3) == 0) {    // if (CPU_MODEL == CPU_704) {
        *chan = 0; // IBM 704 has no channels -> use pseudo channel 0
    } else {
        *chan = (MT_unitAtCabinet[i] / 10) + 1;
    }
    *unit = MT_unitAtCabinet[i] % 10;
}       

int get_MT_cabinet_mta_unit(int i)
{
    if (((cpu_unit.flags >> (16 + 4)) & 0x3) == 0) {    // if (CPU_MODEL == CPU_704) {
        // IBM 704 tapes for MT device are defined as units 30..39
        return 30 + MT_unitAtCabinet[i] % 10;  
    } else {
        return MT_unitAtCabinet[i];
    }
}

void MagTape_InitCabinets() 
{
    int i, chan, unit; 

    bTapeInitCabinets = 1;
    for (i=0;i<10;i++) {
        if (cpanel_on ==  IS_IBM704) { if (i == 0) continue; } // IBM 704 -> only tapes 1->9 displayed
        if (cpanel_on >= IS_IBM7090) { if (i == 0) continue; } // IBM 709X -> only tapes 1->9 displayed
        if (MT_unitAtCabinet[i] < 0) { 
            // no tape in this cabinet
            SetState(IBM70X.MT_head[i], 11); // r/w tape head wide open
            SetState(IBM70X.MT_num[i], 10);    // number no lit on tape
        } else {
            get_MT_cabinet_chan_unit(i, &chan, &unit);
            SetState(IBM70X.MT_num[i], unit);    // unit number lit on tape
            if (chan > 1) {
                SetState(IBM70X.MT_chan[i], chan);    // show channel letter on tape cabinet near lights if B or more
            }
        }
    }
}

int TapePos(int n, int m)
{
    n = n & 63; // remove color info
    while (1) {
        if (n >= m) {n -= m;} else
        if (n <   0) {n += m;} else break;
    }
    return n;
}

t_addr last_wanted_pos[10]; // holds the last used wanted pos for tape
int vacuum_col[10][2];
double ReelSpeed[10][2];
double ReelPos[10][2];

#define MT_Anim_Load            1
#define MT_Anim_UnLoad          2
#define MT_Anim_Move_Fwd        3
#define MT_Anim_Move_Backwrd    4


int MagTape_Vacuum_Colums_simulation(int i, int * MT_L_State,  int * MT_R_State, 
											        int * MT_L_VacCol, int * MT_R_VacCol, 
													t_addr wanted_pos, int MT_State, int AnimationFlag, 
                                                    int bTapeSpinning)
{
    int tape_feed_max        = 20; // amount of tape feed/take when reel is full
    int tape_feed_min        = 10;  // amount of tape feed/take when reel is empty
	int tape_medium_incr     = 200; // amount of tape moved under r/w head on each frame on reading fwd
	int tape_col_feed_sensor = 500; // if amount of tape lower than this -> ask reel to feed tape
	int tape_col_take_sensor = 2000; // if amount of tape greater than this -> ask reel to take tape
	int tape_col_max         = 2200; // max tape amount in col (is > tape_col_take_sensor)
	int tape_max_speed = 12; 
	int tape_max_speed_for_load_unload = 5; 
	int tape_accel = 12; 
	double tape_break = 1.1;      
	double tape_speed_to_pos = 0.10; // max speed * this gives reel pos increment
	int n, nCol, bHasSpinBlur;
	int ReelSpin[2], ReelFeed_incr[2];
	double d; 
    int nResult = 0;

	if (MT_State == 0) {
		vacuum_col[i][0] = -1;
        last_wanted_pos[i] = 0;
		return 0; // no tape in reels -> mark invalid pos in vaccum and exit
	}
	if (vacuum_col[i][0] < 0) {
        if (AnimationFlag == MT_Anim_Load) {
            // init tape for load animation 
		    // first movement of tape, init values for just-loaded tape
		    vacuum_col[i][0] = vacuum_col[i][1] = 0;
        } else {
            // tape still not in movement
		    if (bTapeSpinning == 0) return 0; // no movement and no command -> tape idle -> return
		    // first movement of tape, init values for just-mounted tape
		    vacuum_col[i][0] = (int) (tape_col_feed_sensor * 1.1);
		    vacuum_col[i][1] = (int) (tape_col_take_sensor * 0.9);
        }
		ReelSpeed[i][0] = ReelSpeed[10][0] = 0;
		ReelPos[i][0] = *MT_L_State;
		ReelPos[i][1] = *MT_R_State;
		return 0;
	}
    // if load/unload, reels turns at constant speed on opposite directions
    if ((AnimationFlag == MT_Anim_Load) || (AnimationFlag == MT_Anim_UnLoad)) {
        tape_max_speed = tape_max_speed_for_load_unload;
        n = (AnimationFlag == MT_Anim_UnLoad) ? -1 : 1; // in unload, reels move on opposite directon
        ReelSpeed[i][0] = +tape_max_speed * n;
        ReelSpeed[i][1] = -tape_max_speed * n;
    }

	// calculate how much tape is feed on reel rotation give the amount of tape rolled on each reel
    // MT_State =  1 -> all tape on left reel, none on right
    // MT_State = 23 -> all tape on right reel, none on left
    // calculate how much tape in left reel. 1.0=full, 0.0=empty, 0.5=half
    d = (24 - MT_State) / 22.0; 
 	ReelFeed_incr[0] = tape_feed_min + (int) ((tape_feed_max - tape_feed_min) * d);
    d = (MT_State) / 22.0; 
 	ReelFeed_incr[1] = tape_feed_min + (int) ((tape_feed_max - tape_feed_min) * d);
	// calculate medium level on vacuum column based on reel speed
	vacuum_col[i][0] += (int) (ReelFeed_incr[0] * ReelSpeed[i][0]) ;
	vacuum_col[i][1] -= (int) (ReelFeed_incr[1] * ReelSpeed[i][1]) ; // minus as right tape (nCol==1) takes medium from column when goinf clockwise (speed > 0)
	// calculate how much tape is moved form left column to right column by r/w head
    // and update current tape pos
    if ((AnimationFlag == MT_Anim_Load) ||(AnimationFlag == MT_Anim_UnLoad)) {
    	// during load operation medium is not moved from one column to the other by r/w head 
        // slow down the medium loading prior to medium visible on column to simulate feeding of upper part of column (not visible)
	    for (nCol=0;nCol<2;nCol++) {
            d = (vacuum_col[i][nCol] < tape_col_feed_sensor) ? -0.85 : -0.65;
            if (nCol == 1) d = -d;
            vacuum_col[i][nCol] += (int) (ReelFeed_incr[nCol] * ReelSpeed[i][nCol] * d);
        }
        if (AnimationFlag == MT_Anim_Load) {
            // return 1 when both reels has tape medium loaded in vacuum column
            if ((vacuum_col[i][0] > tape_col_feed_sensor) && (vacuum_col[i][1] > tape_col_feed_sensor)) {
                nResult = 1;
            }
        } else {
            // return 1 when none has medium loaded in vacuum columns
            if ((vacuum_col[i][0] < tape_col_feed_sensor) && (vacuum_col[i][1] < tape_col_feed_sensor)) {
                nResult = 1;
            }
        }
    } else if ((AnimationFlag == MT_Anim_Move_Fwd) || (AnimationFlag == MT_Anim_Move_Backwrd)) {
	    n = tape_medium_incr; // > 0 -> tape going to left fron vacuum_col[0] to vacuum_col[1] as medium is read forward
        if (AnimationFlag == MT_Anim_Move_Backwrd) {
	    	n = -n;	   // should go backwards
	    }	
    	vacuum_col[i][0] -= n; // medium is reduced in left column as it goes to r/w head
    	vacuum_col[i][1] += n; // medium is increased in right column as goes out r/w head 
    } else {
        n = wanted_pos - last_wanted_pos[i]; // > 0 -> tape going to left fron vacuum_col[0] to vacuum_col[1] as medium is read forward
        if (n < -tape_medium_incr) {n = -tape_medium_incr;} else if (n > tape_medium_incr) {n = tape_medium_incr;}
        if ((bTapeSpinning == 0) || (sim_is_running == 0)) {
            n = 0;
        } else if (bTapeSpinning == 1) {
            if (n <= 0) n = tape_medium_incr; // correct n as tape going forward
        } else if (bTapeSpinning == 2) {
            if (n >= 0) n = -tape_medium_incr; // correct n tape going backwards
        }
    	vacuum_col[i][0] -= n; // medium is reduced in left column as it goes to r/w head
    	vacuum_col[i][1] += n; // medium is increased in right column as goes out r/w head 
        last_wanted_pos[i] = wanted_pos;
    }
	// calculate medium tape in vacumm colum
	for (nCol=0;nCol<2;nCol++) {
	    // d=0.0 -> at top of col, d=1.0 at max lower position
	    d = (((double) (vacuum_col[i][nCol])) / tape_col_max);
        // d=0.0 maps to states 0-44
    	n = (int) (d * 44);
	    if (n > 44) {n = 44;} else if (n < 1) n = 0;
		if (nCol == 0) {
			* MT_L_VacCol = n;
		} else {
			* MT_R_VacCol = n;
		}
	}
	// now check if vacuum columns wants to move reels to feed/take medium 
	for (nCol=0;nCol<2;nCol++) {
		n=vacuum_col[i][nCol];
		ReelSpin[nCol] = 0;
		if (n < tape_col_feed_sensor) {
   			// vaccum sense on top of colum says reels needs to feed more tape to column
   			// if nCol == 0 (left column), the Left Reel must move clockwise (+1) to feed tape
   			// if nCol == 1 (right column), the right Reel must move couterclockwise (-1) to feed tape
   			ReelSpin[nCol] = (nCol == 0) ? +tape_accel : -tape_accel;
   			if (n < 0) vacuum_col[i][nCol] = 0; // avoid too much feed to catch up
		} else  if (n > tape_col_take_sensor) {
   			// vaccum sense on bottom of colum says reels needs to take some tape from column
   			// if nCol == 0 (left column), the Left Reel must move counterclockwise (-1) to take tape
   			// if nCol == 1 (right column), the right Reel must move clockwise (+1) to take tape
   			ReelSpin[nCol] = (nCol == 0) ? -tape_accel : +tape_accel;
   			if (n > tape_col_max) vacuum_col[i][nCol] = tape_col_max; // avoid too much take to catch up
		} else {
			// no need to feed or take medium from column. Brake the reel to prevent spinning on inertia and mess up column
			d = ReelSpeed[i][nCol];
			if (d > 0) { 
				d = d - tape_break;
				if (d < 0) d = 0;
			} else if (d < 0) {
				d = d + tape_break;
				if (d > 0) d = 0;
			}
			ReelSpeed[i][nCol] = d;
		}
	}
    // if loading or unloading, reels does not react on take/feed sensors, just notify this to caller via nResult
    if ((AnimationFlag == MT_Anim_Load) || (AnimationFlag == MT_Anim_UnLoad)) {
        ReelSpin[0] = ReelSpin[1] = 1;
    }
	// compute reel speed and position
	for (nCol=0;nCol<2;nCol++) {
		d = ReelSpeed[i][nCol] + ReelSpin[nCol];
		if (d >  tape_max_speed) d = tape_max_speed;
		if (d < -tape_max_speed) d = -tape_max_speed;
		ReelSpeed[i][nCol] = d;
		bHasSpinBlur = (fabs(d) > tape_max_speed / 2) ? 1 : 0;
        if ((AnimationFlag == MT_Anim_Load) || (AnimationFlag == MT_Anim_UnLoad)) bHasSpinBlur = 0;
		d = ReelPos[i][nCol] + d * tape_speed_to_pos;
		while (1) if (d < 0) {d=d+24;} else if (d >= 24) {d=d-24;} else break;
		n = (int) (ReelPos[i][nCol] = d);
		if (bHasSpinBlur) n += 24;
		if (nCol == 0) {
			*MT_L_State = n;
		} else {
			*MT_R_State = n;
		}
	}
    return nResult;
}

void AnimationStartStage(int i, int nStage) 
{
    MT_animation[i].Stage = nStage;
    MT_animation[i].StageFrameStart  = MT_animation[i].FrameCount;
    MT_animation[i].Data = 0;
}

void MagTape_HeadAnimation(int i, int t_msec_elapsed, int * MT_head, int bOpenCloseFlag)
{
    int t_msec_for_head_closing_727        = 1500; // time in msec to close the tape r/w head
    int t_msec_for_head_closing_729        =  700; // time in msec to close the tape r/w head
    int t_msec_for_head_closing; 

    if (bIbm727Tape) {
        t_msec_for_head_closing = t_msec_for_head_closing_727; // IBM 704 cpanel has 727 tapes 
    } else {
        t_msec_for_head_closing = t_msec_for_head_closing_729;  // other cpanels uses 729 tapes
    }
    if (bOpenCloseFlag == -1) {
        // close head
        *MT_head = 1 + (int) (10.0 * (t_msec_for_head_closing - t_msec_elapsed)/ t_msec_for_head_closing);
    } else {
        // open head
        *MT_head = 1 + (int) (10.0 * t_msec_elapsed / t_msec_for_head_closing);
    }
    if (*MT_head < 1) *MT_head = 1;
    if (*MT_head > 11) *MT_head = 11;
}

// return OS time elapsed in msec from begin of current animation stage
int AnimElapsedTime1(int i) 
{
    return (MT_animation[i].FrameCount - MT_animation[i].StageFrameStart) * (1000 / FPS); // OS time elapsed in msec 
}

// return 1 if t_msec_elapsed msec has elapsed counting OS time (based on frames displayed at current FPS)
// or on simulated CPU time (usfin sim_grtime). Returns 1 on whatever occurs first
// MT_animation[i].Data == 0 starts the time measurement for time elapsed. 
int AnimElapsedTime2(int i, int t_msec_elapsed) 
{
    int n1, n2;
    if (MT_animation[i].Data == 0) {
        MT_animation[i].Data = (int) sim_grtime(); // use to hold the simulated SimH time
        MT_animation[i].StageFrameStart = MT_animation[i].FrameCount; // reset start frame to count 1 sec od OS time 
    }
    n1 = AnimElapsedTime1(i); // OS time elapsed in msec 
    n2 = (  ((int) (sim_grtime())) - MT_animation[i].Data) / us_to_ticks(1000); // SimH time elapsed in msec
    if ((n1 < 0) || (n2 < 0) || (n1 >= t_msec_elapsed) || (n2 >= t_msec_elapsed)) {
        // time elapsed, reset time and notify
        MT_animation[i].Data = 0;
        return 1;
    }
    return 0;
}

char MT_unitAtCabinet_str[3];

void MagTape_RewAnimation (int i, int * MT_L_State, int * MT_R_State, int * MT_head,  
                                         int * MT_L_VacCol, int * MT_R_VacCol, int * MT_State,
						                 int MT_L_State0, int MT_R_State0, 
                                         int bUnLoad)
{
    int t_msec_hi_speed_rew_acceleration = 300;
    int t_msec_for_hi_speed_rew = 20000;
    int t_msec_hi_speed_rew_deceleration = 300;
    int t_msec_for_low_speed_rew = 3000;

    int t_msec_elapsed, n; 
    int bRewFinished = 0;

    if (MT_animation[i].FrameCount == 0) {
        // the load animation is about to start. 
        // increment animation frame to show
        MT_animation[i].FrameCount++; 
        if (*MT_State < 2) {
            AnimationStartStage(i, 20);  // rew on regular backwards movement
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) starts Rew/Run Animation (Low speed)\n", i, MT_unitAtCabinet_str);
        } else if (*MT_State < 3) {
            AnimationStartStage(i, 6);  // rew on low speed
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) starts Rew/Run Animation (Low speed)\n", i, MT_unitAtCabinet_str);
        } else {
            AnimationStartStage(i, 1); // start rew at high speed: unload -> hi speed rew -> load -> low speed rew
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) starts Rew/Run Animation (Hi speed)\n", i, MT_unitAtCabinet_str);
        }
    } else {
        MT_animation[i].FrameCount++; 
}
    // animation core: set the head, reels, etc to the pos corresponding at the animated elapsed time t_msec_epased
    if ((MT_animation[i].Stage == 1) || (MT_animation[i].Stage == 30)) {
        // unload medium: open head and move reels opposite direction
        *MT_L_State = MT_L_State0;
        *MT_R_State = MT_R_State0;
        t_msec_elapsed = AnimElapsedTime1(i); // animation stage time elapsed in msec 
        MagTape_HeadAnimation(i, t_msec_elapsed, MT_head, +1 /* open */ );
        if (*MT_head > 5) {
            // slowly move reels on opposite direction to simulate removing the medium in vacuum column 
            n = MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                0 /* wanted pos */, 1 /* MT_State=1 as tape is loading as just mounted */, 
                                                MT_Anim_UnLoad /* AnimationFlag */, 1 /* bTapeSpinning */); 
            if (n) {
                if (MT_animation[i].Stage == 30) {
                    bRewFinished = 1;
                } else {
                    AnimationStartStage(i, 2); // unload stage finished -> next stage: accelerate to speed rew
                }
            }
        }
    }    
    if (MT_animation[i].Stage == 2) {
        // accelerate to hi speed rew
        t_msec_elapsed = AnimElapsedTime1(i); // animation stage time elapsed in msec 
        if (t_msec_elapsed > t_msec_hi_speed_rew_acceleration) {
            AnimationStartStage(i, 3); // accel stage finished -> next stage: full hi speed rew
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation - hi speed rew\n", i, MT_unitAtCabinet_str);
        } else if (t_msec_elapsed > t_msec_hi_speed_rew_acceleration * 1 / 2) {
            *MT_L_State = 24 + TapePos(MT_L_State0 - 2, 24); // start rew
            *MT_R_State = 24 + TapePos(MT_R_State0 - 3, 24); 
        } else {
            *MT_L_State = MT_L_State0; // wait to start
            *MT_R_State = MT_R_State0;
        }
    }
    if (MT_animation[i].Stage == 3) {
        // hi speed rew
        if (AnimElapsedTime2(i, t_msec_for_hi_speed_rew / 21)) { // time for rew one MT_State
            *MT_State = *MT_State - 1;  // each second, a state is dec (so on GUI, reel L grows each second)
        }
        *MT_L_State = 48 + TapePos(MT_L_State0 + 1, 8); // rewindiwng at full fast speed cycles last 8 states 
        *MT_R_State = 48 + TapePos(MT_R_State0 + 1, 8); 
        *MT_head = 11; // r/w head open
        *MT_L_VacCol = *MT_R_VacCol = 0;
		vacuum_col[i][0] = vacuum_col[i][1] = 0;
        if (*MT_State <= 2) {
            AnimationStartStage(i, 4); // hi rew stage finished -> next stage decelerate
        }
    }    
    if (MT_animation[i].Stage == 4) {
        // decelerate
        t_msec_elapsed = AnimElapsedTime1(i); // animation stage time elapsed in msec 
        if (t_msec_elapsed > t_msec_hi_speed_rew_deceleration) {
            *MT_State = 2;
            AnimationStartStage(i, 5); // decel stage finished -> next stage: load tape again
        } else if (t_msec_elapsed > t_msec_hi_speed_rew_deceleration * 3 / 4) {
            *MT_L_State = MT_L_State0; // wait to stop
            *MT_R_State = MT_R_State0;
        } else if (t_msec_elapsed > t_msec_hi_speed_rew_deceleration * 2 / 4) {
            *MT_L_State = 24 + TapePos(MT_L_State0 - 1, 24); // end deceleration
            *MT_R_State = 24 + TapePos(MT_R_State0 - 2, 24); 
        } else {
            *MT_L_State = 24 + TapePos(MT_L_State0 - 2, 24); // start deceleration
            *MT_R_State = 24 + TapePos(MT_R_State0 - 3, 24); 
        }
    }
    if (MT_animation[i].Stage == 5) {
        // load: close head and move reels opposite direction
        *MT_L_State = MT_L_State0;
        *MT_R_State = MT_R_State0;
        t_msec_elapsed = AnimElapsedTime1(i); // animation stage time elapsed in msec 
        MagTape_HeadAnimation(i, t_msec_elapsed, MT_head, -1 /* close */ );
        if (*MT_head < 5) {
            // slowly move reels on opposite direction to simulate entering the medium in vacuum column 
            n = MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                0 /* wanted pos */, 1 /* MT_State=1 as tape is loading as just mounted */, 
                                                MT_Anim_Load /* AnimationFlag */, 1 /* bTapeSpinning */); 

            if (n) {
                AnimationStartStage(i, 6); // load stage finished -> next stage: low speed rew
                sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation - going to low speed\n", i, MT_unitAtCabinet_str);
            }
        }
    }
    if (MT_animation[i].Stage == 6) {
        // low speed rew
        *MT_head = 1;
        MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                0 /* wanted pos */, 
                                                1 /* MT_State=1 as tape is loading as just mounted */, 
                                                MT_Anim_Move_Backwrd /* AnimationFlag */, 1 /* bTapeSpinning */); 

        // calculate duration of state to end it
        if (AnimElapsedTime2(i, t_msec_for_low_speed_rew)) { 
            if (bUnLoad) {
                AnimationStartStage(i, 30); // it a rew+unload -> next stage: unload
                sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation - going to unload\n", i, MT_unitAtCabinet_str);
            } else {
                bRewFinished = 1; // at last! rew finised. All medium on left reel 
            }
        }
    }
    if (MT_animation[i].Stage == 20) {
        // rew on reel L quasy full -> rew on regular backwards movement
        *MT_L_State = MT_L_State0; 
        *MT_R_State = MT_R_State0; 
	    *MT_L_VacCol = (int) GetState(IBM70X.MT_L_VacCol[i]);
		*MT_R_VacCol = (int) GetState(IBM70X.MT_R_VacCol[i]);
		MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                  0 /* pos_wanted = 0 -> load point at beggining of tape */, *MT_State, 
                                                  0, 1 /* bTapeSpinning */); 
        if (AnimElapsedTime2(i, t_msec_for_low_speed_rew)) { 
            bRewFinished = 1; // at last! rew finised. All medium on left reel 
        } else if (mta_unit[get_MT_cabinet_mta_unit(i)].pos == 0) { 
            // tape at the load point -> end of rew
            bRewFinished = 1;
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation - Rew finished, tape at load point\n", i, MT_unitAtCabinet_str);
        } 
        if ((bRewFinished) && (bUnLoad)) {
            bRewFinished = 0;
            AnimationStartStage(i, 30); // it a rew+unload -> next stage: unload
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation - start unload\n", i, MT_unitAtCabinet_str);
        }
    }
    if (bRewFinished) {
        // rew finised. cleanup
        *MT_State = 1; // al tape medium on L reel
        *MT_head  = 1; // r/w tape head closed
        MT_rew_animation[i]=0; // animation done -> not in progress
        MT_animation[i].FrameCount=0;  // init load animation frame counter for next animation
        last_wanted_pos[i] = 0;
        sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Rew/Run Animation end\n", i, MT_unitAtCabinet_str);
    }
}

// do animation (MT_animation counts number of animation frames displayed)
void MagTape_LoadAnimation(int i, int * MT_L_State, int * MT_R_State, int * MT_head,  
                                         int * MT_L_VacCol, int * MT_R_VacCol,
						                 int MT_L_State0, int MT_R_State0)
{
    int t_msec_reel_one_revolution_on_loading_column = 1200; // one revolution on 1,2 sec
    int t_msec_for_vacuum_column_fill = 1500;
    int t_msec_for_load_point_search_fwd = 800;
    int t_msec_for_load_point_search_bkwrd = 500;

    int j, n, bStartAnimation;
    int t_msec_elapsed; 
    int t_msec_for_load_point_search;

    if (MT_animation[i].FrameCount == 1) {
        // the load animation is about to start. Init animation values
        AnimationStartStage(i, 0);
        // first check if a another tape is starting the 
        // load animation. If so, hold on to prevent simultaneous synced animimations
        bStartAnimation = 1;
        for (j=0;j<10;j++) if ((MT_load_animation[j] > 0) && (MT_animation[j].FrameCount * (1000 / FPS) < 500)) {
            // another tape is starting animating (not reached 500msec of animation). 
            // Who wins? (continue) or lose (has to hold on and wait his turn to do its animation)
            if (MT_animation[j].FrameCount > 1) bStartAnimation = 0; // the other wins (is more advanced in animation progress)
            if (j > i) bStartAnimation = 0; // the other wins (same advance in animation, but has lower tape number)
            break;
        }
        if (bStartAnimation) {
            // increment animation frame to show
            MT_animation[i].FrameCount++; 
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) starts Load Animation\n", i, MT_unitAtCabinet_str);
        } else {
            // freeze this animation in the current frame instead of advancing to next frame
        }
    } else {
        MT_animation[i].FrameCount++; //continue on animation
    }
    // calculate the animation elapsed time in OS wall clock msec 
    t_msec_elapsed = MT_animation[i].FrameCount * (1000 / FPS); // animation time elapsed in msec
    // animation core: set the head, reels, etc to the pos corresponding at the animated elapsed time t_msec_epased
    // compute r/w head pos
    if (t_msec_elapsed < 300) {
        // 300 first msec, tape head fixed at full open position
        *MT_head = 11; 
        *MT_L_VacCol = *MT_R_VacCol = 0;
		vacuum_col[i][0] = -1;
    } else {
        // then, take t_msec_for_head_closing msec to full close the r/w head
        MagTape_HeadAnimation(i, t_msec_elapsed-300, MT_head, -1 /* close head */);
        if ((*MT_head < 5) && (MT_animation[i].Stage == 0)) {
            AnimationStartStage(i, 1); // start stage 1: load vacuum col when head is about to close
        }
    }
    // compute reel spinning in oposite directiong while loading tape into vacuum column
    if (MT_animation[i].Stage == 1) {
        // slowly move reels on opposite direction to simulate entering the medium in vacuum column 

        n = MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                0 /* wanted pos */, 1 /* MT_State=1 as tape is loading as just mounted */, 
                                                MT_Anim_Load /* AnimationFlag */, 
                                                1 /* bTapeSpinning */); 
        if (n) {
           AnimationStartStage(i, 2); // stage 1 finished -> start stage 2: position tape on load point
        }
    } 
    // move the r/w head to read and position tape at load point
    if ((MT_animation[i].Stage == 2) || (MT_animation[i].Stage == 3)) {
        MagTape_Vacuum_Colums_simulation(i,MT_L_State, MT_R_State, MT_L_VacCol, MT_R_VacCol, 
                                                0 /* wanted pos */, 
                                                1 /* MT_State=1 as tape is loading as just mounted */, 
                                                (MT_animation[i].Stage == 2) ? MT_Anim_Move_Fwd : MT_Anim_Move_Backwrd /* AnimationFlag */,  
                                                1 /* bTapeSpinning */); 

        // calculate duration of state to end it
        t_msec_elapsed = (MT_animation[i].FrameCount - MT_animation[i].StageFrameStart) * (1000 / FPS); // animation stage 1 time elapsed in msec 
        t_msec_for_load_point_search = 
            (MT_animation[i].Stage == 2) ? t_msec_for_load_point_search_fwd : t_msec_for_load_point_search_bkwrd + i*17;
        if (t_msec_elapsed > t_msec_for_load_point_search) {
            // alternate between stages 2 and 3 to look for load point reading forwards and backwards
            n = MT_animation[i].Data;
            AnimationStartStage(i, (MT_animation[i].Stage == 2) ? 3 : 2); 
            MT_animation[i].Data = n+1; // Data holds the number of alternations between states 2 and 3
        }
        if (MT_animation[i].Data > 2) {
            // done all the animation, mark load animation as done.
            MT_load_animation[i] = -1; 
            MT_animation[i].FrameCount=0;  // init load animation frame counter for next animation
            sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) Load Animation end\n", i, MT_unitAtCabinet_str);
            return; 
        }
    }
}


#define     MT_has_no_tape      0
#define     MT_is_rewinding     1
#define     MT_is_loading_tape  2
#define     MT_is_using_tape    3

// return 0 if tape i (0-9) on channel A in stopped (not spinning)
//        1 if tape is spinning (read or write)
//        -1 if rew
//        -2 if run (rew + unload)
int get_mt_current_command(int i)
{
    int mt_cmd;
    UNIT *uptr;
    uptr = &mta_unit[get_MT_cabinet_mta_unit(i)];

    if (uptr->u5 & 004000) return -2;   // #define MT_UNLOAD 004000      /* Unload when rewind done */
    mt_cmd = (uptr->u5 & 037); // #define MT_CMDMSK 0037
    if ((mt_cmd == 17) || (mt_cmd == 16)) {
        // #define MT_HREW 17 /* High speed rewind */ MT_LREW 16
        return -1;  
    }
    if ((mt_cmd ==  1) || (mt_cmd ==  2) || (mt_cmd ==  3) || (mt_cmd ==  4) ||
        (mt_cmd ==  5) || (mt_cmd == 13) || (mt_cmd == 14) || (mt_cmd == 15)) {
        // #define MT_RDS          1   // read
        // #define MT_RDSB         2
        // #define MT_WRS          3   // write
        // #define MT_WRSB         4
        // #define MT_WEF          5   // write end of file
        // #define MT_RDB          15  
        // #define MT_ERG          14  // erase
        // #define MT_SKR          13  // skip
        return 1; // tape going forward
    }
    if ((mt_cmd == 6) || (mt_cmd == 7)) {
        // #define MT_BSR          6   // back record
        // #define MT_BSF          7   // back file
        return 2;  // tape going backwards
    }
    return 0;
}

void MagTape_Refresh(void)
{
    int i, n1, ioaddr, density, tape_ready, att, selected, unit, chan;
    int MT_is;
    int MT_State,  MT_State0, 
		MT_L_State,  MT_R_State, MT_L_State0_23, MT_R_State0_23, MT_L_State00, MT_R_State00,
		MT_L_VacCol, MT_R_VacCol;
    int MT_head, MT_head0;
    int mt_cmd;
    t_addr pos, capac; 
    UNIT *uptr;

    if (bTapeInitCabinets == 0) MagTape_InitCabinets();
    // MT_State holds the amount of tape in eache reel. =1 -> all tape on L reel, =23 -> all tape on R reel, =0 -> no tape, reel unmounted
    // MT_L_State/MT_R_State holds the rotational position of reel, the speed ot reel, and the reel's colour
    //    0..23 -> reel rotated 0gr, 15gr, ... 345gr. To be used when reel is not moving
    //   24..47 -> same reel rotated 0gr, 15ge ... but with some spin blur. To be used when reel is moving
    //   48..55 -> reel rotalted 0gr, 45gr, ... whith greater spin blur. To be used when reel is rewinding at fast pace
    //   64..   -> same but with another reel color. There are 3 reels colors.
    // iterate on visible MT cabinets (1 to 10, numbered 0 to 9)
    for (i=0;i<10;i++) {
        if (cpanel_on ==  IS_IBM704) { if (i == 0) continue; } // IBM 704 -> only tapes 1->9 displayed
        if (cpanel_on >= IS_IBM7090) { if (i == 0) continue; } // IBM 709X -> only tapes 1->9 displayed
        // get the channel/unit that is displayed on the MT cabinet
        if (MT_unitAtCabinet[i] < 0) { 
            // no tape in this cabinet
            MT_unitAtCabinet_str[0] = '?'; MT_unitAtCabinet_str[1] = 0;
            continue; 
        }
        // set MT_unitAtCabinet_str to be availble for sim_debug detail
        get_MT_cabinet_chan_unit(i, &chan, &unit);
        MT_unitAtCabinet_str[0] = 'A' + chan - 1; MT_unitAtCabinet_str[1] = '0' + unit; MT_unitAtCabinet_str[2] = 0;
        // get current cpanel tape state
        uptr = &mta_unit[get_MT_cabinet_mta_unit(i)];
        tape_ready = 1; att = 1; 
        MT_State   = MT_State0   = (int) GetState(IBM70X.MT[i]); 
        MT_L_State = MT_L_State00 = (int) GetState(IBM70X.MT_L[i]);
        MT_R_State = MT_R_State00 = (int) GetState(IBM70X.MT_R[i]);
        MT_L_State0_23 = TapePos(MT_L_State00, 24); // remove color info & still/normal speed/fast motion from state
        MT_R_State0_23 = TapePos(MT_R_State00, 24); // thus values are from 0 to 23
        MT_head = MT_head0    = (int) GetState(IBM70X.MT_head[i]);
		MT_L_VacCol = MT_R_VacCol = 0;
        // check att and load animation state
        att = ((uptr->flags & UNIT_DIS) || ((uptr->flags & (UNIT_ATT)) == 0)) ? 0 : 1;
        mt_cmd = get_mt_current_command(i);
        if ((mt_cmd != 0) || (uptr->pos)) {
            // if mt command or tape moved terminate any load animation in progress
            if (MT_load_animation[i] != -1) {
                MT_load_animation[i] = -1; 
                MT_animation[i].FrameCount=0;  // init load animation frame counter for next animation
                sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) command ends Load Animation in progress\n", i, MT_unitAtCabinet_str);
            }
        } else if ((att) && (MT_load_animation[i] == 0) && (MT_State0 == 0)) {
            // file attached to unit, no tape commands, no tape on reel, and load animation not done
            // -> this unit can start load animation
           MT_load_animation[i]=1;        // set flag as load animation in progress
           MT_animation[i].FrameCount=1;  // init load animation frame counter 
        }
        // check for rew animation state
        if (mt_cmd >= 0) {
            // if not rew mt command terminate any rew animation in progress
            if (MT_rew_animation[i] > 0) {
                MT_rew_animation[i]=0; 
                MT_animation[i].FrameCount=0;  // clear animation frame counter 
                last_wanted_pos[i] = 0;
                sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) command ends Rew Animation in progress\n", i, MT_unitAtCabinet_str);
            }
        } else if (MT_rew_animation[i] == 0) {
           MT_load_animation[i]=-1;          // rew mark load anim as done
           MT_rew_animation[i]=1;            // set flag as rew animation in progress. Holds the gtime when rew starts
           MT_animation[i].FrameCount=0;     // init rew animation frame counter 
        }
        // what the tape is doing?
        if (att == 0) {
            MT_is = MT_has_no_tape;
        } else if (mt_cmd < 0) {
            MT_is = MT_is_rewinding;
        } else if (MT_load_animation[i] >= 1) {
            MT_is = MT_is_loading_tape;
        } else {
            MT_is = MT_is_using_tape;
        }
        // do what the tape is doing
        if (MT_is == MT_has_no_tape) {
            // tape reel unmounted
            if (MT_State != 0) {
                sim_debug(CP_DETAIL, &cp_dev, "MT%d (%s) set to no tape\n", i, MT_unitAtCabinet_str);
            }
            MT_State = 0;    // no medium on reels
            MT_head = 11;    // r/w tape head wide open
            MT_L_State = MT_L_State00; MT_R_State = MT_R_State00;  // keep reels at pos they had
            MT_L_VacCol = MT_R_VacCol = 0; // no tape media in vacuum columns
            tape_ready = 0;  // tape light ready not lit
            MT_load_animation[i] = 0;           // 0= animation can be done on next refresh if file is attached to mt unit 
            MT_rew_animation[i] = 0;            // 0= no rew animation in progress
            MT_animation[i].FrameCount=0;       // init load animation frame counter for next animation
            last_wanted_pos[i] = 0;
        } else if (MT_is == MT_is_loading_tape) {
            MT_State = 1;    // all medium left reels (just mounted)
            MT_L_State = MT_L_State0_23; 
            MT_R_State = MT_R_State0_23; 
			MT_L_VacCol = (int) GetState(IBM70X.MT_L_VacCol[i]);
			MT_R_VacCol = (int) GetState(IBM70X.MT_R_VacCol[i]);
            MagTape_LoadAnimation(i, &MT_L_State, &MT_R_State, &MT_head, &MT_L_VacCol, &MT_R_VacCol, 
														  MT_L_State0_23, MT_R_State0_23);
            tape_ready = 0;  // loading tape - tape light ready not lit
        } else if (MT_is == MT_is_rewinding) {
            // rew in progress ... if dev activation is about to end, extend duration
            if (sim_activate_time(uptr) < us_to_ticks(5*1000*1000)) {
                sim_cancel(uptr);
                sim_activate(uptr, us_to_ticks(10*1000*1000));   
            }
            // calculate tape pos based on where and when rew started
            // each sec of SimH time or OS time the state tape is decremented, thus transfering tape from R reel to L reel
            // SimH and OS time are checked, the faster one wins. This allows rew not stalled if
            // cpu is stopped while rew in progress
            MagTape_RewAnimation(i, &MT_L_State, &MT_R_State, &MT_head, &MT_L_VacCol, &MT_R_VacCol, &MT_State, 
											MT_L_State0_23, MT_R_State0_23, 
                                            (mt_cmd == -2) ? 1 : 0);
            tape_ready = 0; // during rew tape ready light is off (not lit)
            if (MT_rew_animation[i] == 0) { 
                uptr->u5 &= ~(037); // MT_CMDMSK -> remove any command
                uptr->u5 |= 16; // MT_LREW;
                uptr->u3 = 0;
                sim_cancel(uptr);
                sim_activate(uptr, 1);          // process the end of rew 
            }
        } else {
            // calculate how much tape has gone from one reel to the other
            capac = (uptr->capac); 
            if (capac == 0) capac =4*1024*1024; // use 4Mb as default tape capa
            pos = (uptr->u3);
            if (pos > capac) pos = capac;
            MT_State = 1 + (int) ((22 * pos) / capac);
            // now set the reels position based on vacuum columns
            MT_L_State = MT_L_State0_23; 
            MT_R_State = MT_R_State0_23; 
			MT_L_VacCol = (int) GetState(IBM70X.MT_L_VacCol[i]);
			MT_R_VacCol = (int) GetState(IBM70X.MT_R_VacCol[i]);
			MagTape_Vacuum_Colums_simulation(i, &MT_L_State, &MT_R_State, 
                                                       &MT_L_VacCol, &MT_R_VacCol, 
                                                       uptr->u3, MT_State, 0, 
                                                       mt_cmd /* bTapeSpinning: 0=idle, 1=fwd, 2=bckward*/); 
            // do not forget to set r/w head
            MT_head     = 1; 
            MT_animation[i].FrameCount=0;  // init load animation frame counter for next animation
        }
        // update tape control states
        if ((MT_State0 != MT_State) || (MT_L_State00 != MT_L_State) || (MT_R_State00 != MT_R_State) || 
            (MT_head0 != MT_head)) {
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM70X.MT[i], MT_State);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM70X.MT_L[i], MT_L_State + 64 * MT_L_reel_color[i]);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM70X.MT_R[i], MT_R_State + 64 * MT_R_reel_color[i]);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM70X.MT_head[i], MT_head); 
        }
		SetState(IBM70X.MT_L_VacCol[i], MT_L_VacCol);
		SetState(IBM70X.MT_R_VacCol[i], MT_R_VacCol);
        // set tape lights
        density = (MT_DENS (uptr->dynflags)) ? 0 : 1;
        selected = 0;
        ioaddr = (chan_unit[chan].u6 & 0377); // u6 = last command executed in channel 
        decode_ioaddr(ioaddr);
        if ((sim_is_running) && (decode_unit_type == 1)) {  
            if (decode_unit_num == 10) decode_unit_num=0; // unit 10 is [0]
            if (decode_unit_num == unit) selected = 1;
        }
        if (bIbm727Tape) {
            // lights for IBM 727 Magnetic Tape
            n1 = 1 * (selected) +                         // light_0  ; Select light = tape selected by external source
                 2 * (tape_ready) +                       // light_1  ; Ready light = tape in automatic mode (off when rew)
                 4 * (sim_tape_wrp(uptr) ? 1 : 0) +       // light_2  ; File Protect = tape reel is write protected
                 8 * (sim_tape_eot(uptr) ? 1 : 0);        // light_3  ; Tape Indicator = tape reached end of tape
        } else {
            // lights for IBM 729 Magnetic Tape
            n1 = 1 * (selected) +                         // light_0  ; Select light = tape selected by external source
                 2 * (tape_ready) +                       // light_1  ; Ready light = tape in automatic mode (off when rew, loading)
                 4 * (density ? 0 : 1) +                  // light_2  ; High Density
                 8 * (density ? 1 : 0) +                  // light_3  ; Low Density
                 16 * (sim_tape_wrp(uptr) ? 1 : 0) +      // light_4  ; File Protect = tape reel is write protected
                 32 * (sim_tape_eot(uptr) ? 1 : 0);       // light_5  ; Tape Indicator = tape reached end of tape
        }
        if (att == 0) n1 = 0;
        SetState(IBM70X.MT_lights[i], n1);
    }
}


void SetState7090(int CArrayId_K, t_uint64 state, int nBits)
// set state (len nbits bits) in control array CArrayId_K based on key changes on fronteer
{
    int i, n, CId;

    for (i=0; i<nBits-1;i++) {
        n = state & 3;                      // get the next 2 bits of state
        if (i & 1) {
            if ((n == 1) || (n == 2)) n = 3-n;         // if control number is even, reverse
        }
        state = state >> 1;                 // but discard only first bit as procesed
        CId = GetCArrayCId(CArrayId_K, i);  // control for first key fronteer
        SetState(CId, n);                   // set state 00 01 10 11
    }
}

void IBM70X_Refresh(void)
{
    int chan, i, n1, n2, n3, nRW, nCh, iochk;
    int Tape_check[9]; // channel tape check indicator
    UNIT *uptr;

    for (chan=0;chan<=8;chan++) {     // set tape check indicators
        Tape_check[chan] = 0;
        if ((chan == 0) || (chan > NUM_DEVS_MT)) continue;
        for (i=0;i<10;i++) {
            uptr = &mta_unit[i + 10*(chan-1)];
            if (!uptr) continue;
            if (iocheck == 0) uptr->us10 = 0;
            if (uptr->us10) Tape_check[chan] = 1;
        }
    }


	if (Clearing_Main_Mem_Mode > 0) {
		n1 = (1000 / FPS); // n1=msec time between GUI refresh
		n1 = (n1 * 1000) / 12; // at 12us memory cycle, n1=memory cycles done
		if (IC + n1 > 32767) {
			// IC cycled all 32k mem, end of clearing
			IC = 0;
			Clearing_Main_Mem_Mode = 0;
		} else {
			for (n2=0;n2<n1;) {	// IC cycles n1 times, in steps of 16
				IC = (IC & 0xFFF0) + 16;
				if (n2 & 16) IC |= 15;
				TickCount(IBM70X.Reg_IC, IC);
				n2 += 16;
			}
		}
	}
    if (cpanel_on >= IS_IBM709) {
        // sim lits if any of the compatibility mode set 
        SetState(IBM70X.Light_Simulate, ((nmode) || (CTM) || (STM) ) ? 1 : 0); 
    }
    SetState(IBM70X.Light_Automatic, (sim_is_running) && (0==GetState(IBM70X.SW_Auto_Manual)) && (Clearing_Main_Mem_Mode == 0) );  // Light automatic when running depending on manual/automatich switch
    SetState(IBM70X.Light_Program_Stop, hltinst); // on if halt instr executed
    SetState(IBM70X.Light_Divide_Check, dcheck);     /* Divide check */
    SetStateWithIntensity(IBM70X.Light_AC_Overflow, acoflag);     /* AC Overflow */
    SetState(IBM70X.Light_Q_Overflow, mqoflag);      /* MQ Overflow */
    SetState(IBM70X.Reg_Sense, SL); // sense lights 
    nCh = (cpanel_on < IS_IBM7090) ? 6 : 8;

    if (((cpu_unit.flags >> (16 + 4)) & 0x3) == 0) {    // if (CPU_MODEL == CPU_704) {
        iochk = 0; // on 704, R/W check goes on on excesive timming for CPY opcode. This is not simulated, so check never occurs
    } else {
        iochk = iocheck;
    }
    if (cpanel_on != IS_IBM704) {
        SetState(IBM70X.Light_IOCheck, iochk);
        n1 = n2 = n3 = nRW = 0;
        for (chan=nCh;chan>0;chan--) {
            n1 = n1 << 1;
            n2 = n2 << 1;
            n3 = n3 << 1;
            if (chan_flags[chan] & DEV_SEL) {
                n1 = n1 + 1;
                nRW = 1;
                decode_ioaddr(chan_unit[chan].u6 & 0377);
            }
            if (Tape_check[chan]) n2 = n2 + 1;
            if (chan_irq[chan]) n3 = n3 + 1;
        }
        if (nRW == 0) {
            // suposses select class light on during chan transfer, not only on select opcode execution
            decode_unit_type = 0;
        }
        if (cpanel_on == IS_IBM709) {
            SetState(IBM70X.Light_Select_Tape, (decode_unit_type == 1) ? 1 : 0); 
            SetState(IBM70X.Light_Select_Cards, ((decode_unit_type == 2) || (decode_unit_type == 3)) ? 1 : 0); 
            SetState(IBM70X.Light_Select_Printer, (decode_unit_type == 4) ? 1 : 0); 
            SetState(IBM70X.Light_Select_Drum, (decode_unit_type == 5) ? 1 : 0); 
            SetState(IBM70X.Light_Select_Direct_Data, (decode_unit_type == 6) ? 1 : 0); 
        }
        SetState(IBM70X.Reg_CHN_SEL, n1);
        SetState(IBM70X.Reg_CHN_TAP_CHK, n2); 
        SetState(IBM70X.DS_AB, n1 & 3);   // channels selected in data synchronizer lights
        SetState(IBM70X.Light_RW_Select, nRW); 
        if (cpanel_on >= IS_IBM7090) {
            SetState(IBM70X.Reg_CHN_Command_Trap, n3); 
        }
    } else {
        SetState(IBM70X.Light_RW_Check, iochk); // on IBM 704, io check light is RW check
        n1 = n2 = nRW = 0; 
        for (chan=0;chan<=nCh;chan++) {
            // inspect chan 0 (pseudo 704 channel) and other chanels for activity
            // just report if something is transfering or 
            if (chan_flags[chan] & DEV_SEL) {
                nRW = 1;
            }
            if (Tape_check[chan]) n2 = 1;
        }
        SetState(IBM70X.Light_RW_Select, nRW); 
        SetState(IBM70X.Reg_CHN_TAP_CHK, n2); 
    }
    if (cpanel_on <= IS_IBM709) {
        SetStateWithIntensity(IBM70X.Reg_XR, XR[Index_Reg_Selected]);
    } else if (cpanel_on == IS_IBM7090) {
        SetStateWithIntensity(IBM70X.Reg_XR_A, XR[1]);
        SetStateWithIntensity(IBM70X.Reg_XR_B, XR[2]);
        SetStateWithIntensity(IBM70X.Reg_XR_C, XR[4]);
    } else {
        SetState(IBM70X.Light_Multiple_Tag_Mode, MTM ? 1 : 0);
        SetStateWithIntensity(IBM70X.Reg_XR_A, XR[1]);
        SetStateWithIntensity(IBM70X.Reg_XR_B, XR[2]);
        SetStateWithIntensity(IBM70X.Reg_XR_C, XR[4]);  // not an error: XR_C is index 4, XR_D is index 3
        SetStateWithIntensity(IBM70X.Reg_XR_D, XR[3]);
        SetStateWithIntensity(IBM70X.Reg_XR_E, XR[5]);
        SetStateWithIntensity(IBM70X.Reg_XR_F, XR[6]);
        SetStateWithIntensity(IBM70X.Reg_XR_G, XR[7]);
    }
    SetStateWithIntensity(IBM70X.Reg_STOR, SR);
    SetStateWithIntensity(IBM70X.Reg_AC, AC);
    SetStateWithIntensity(IBM70X.Reg_MQ, MQ);
    if (Clearing_Main_Mem_Mode > 0) {
        n1 = sim_is_running; // set sim is running during the duration of refresh. If not set 
		sim_is_running = 1;  // SetStateWithIntensity behaves like SetState ignoring TickCounts used in clear_Main_Mem IC loop
		SetStateWithIntensity(IBM70X.Reg_IC, IC);
		sim_is_running = n1; // restore saved value
	} else {
        SetStateWithIntensity(IBM70X.Reg_IC, IC);
	}
    SetState(IBM70X.Light_Trap, TM);
    SetStateWithIntensity(IBM70X.Reg_IN, IR);
    if (cpanel_on == IS_IBM7094_CTSS) {
        // set lights for 7094 ctss hw modification
        n1 = ((bcore & 4) && dualcore) ? 1 : 0;
        SetState(IBM70X.Light_Protect_Mode, n1);
        n1 = ((bcore & 8) && dualcore) ? 1 : 0;
        SetState(IBM70X.Light_Relocate_Mode, n1);
        n1 = ((bcore & 2) && dualcore) ? 1 : 0;
        SetState(IBM70X.Light_I_Mode, n1);
        n1 = ((bcore & 8) && dualcore) ? 1 : 0;
        SetState(IBM70X.Light_I_Mode, n1);
        SetState(IBM70X.SW_32K_64K, dualcore ? 1:0);
    }
    // set switches so if SW or KEYS regs are changes by SCP command, the change is shown 
    if (cpanel_on < IS_IBM7090) {
        SetState(IBM70X.SW_Sense, SW); 
        SetState(IBM70X.SW_Data,  KEYS); 
    } else {
        SetState7090(IBM70X.SW_Sense_K, SW, 6);
        SetState7090(IBM70X.SW_Data_K, KEYS, 36);
    }
    if ((sim_is_running) && (1==GetState(IBM70X.SW_Auto_Manual))) {
        SetState(IBM70X.SW_Auto_Manual, 0);
    }
    // data synchronizer lights
    if (bDataChannelVisible) 
        Data_Channel_Refresh(); 
    // tapes
    if (bTapesVisible)
        MagTape_Refresh();
    // card reader
    if (bCardReadVisible) 
        CardReader_Refresh();
    // dasd
    if (bDASDVisible) 
        DASD_Refresh();
    // last (but no least) check if multiple step mode enabled and issue a step scp command
    if ((sim_is_running == 0) && (Multiple_Step_Mode == 1)) {
        DoSCP("step");
        return;
    }
}

void IBM70X_OnClick_Sw(void)
{
    // Sitches can be clicked allways (are mechanical) 
    int n;
    t_uint64 SR;

    if (CP_Click.KeyPress_KeyRelease == 2) {
        // release mouse button -> ignore
        return;
    }

    if ((CP_Click.CArrayId == IBM70X.SW_Data) || (CP_Click.CArrayId == IBM70X.SW_Sense)) {

        if (CP_Click.CArrayId == IBM70X.SW_Sense)       {SR = SW; }     // get current SW register setting
        else if (CP_Click.CArrayId == IBM70X.SW_Data)   {SR = KEYS; }   // get current console keys register
        // get sw clicked bit number
        n = CP_Click.CArrayItem; 
        // update the switch register
        SR = SR ^ (((t_uint64) 1) << n);
        SetState(CP_Click.CArrayId, SR);
        if (CP_Click.CArrayId == IBM70X.SW_Sense)       {SW = (uint16) SR; }  // set SW register with new switches setting
        else if (CP_Click.CArrayId == IBM70X.SW_Data)   {KEYS = SR; }         // set KEYS register with new switches setting
    }
}

void clear_IBM704_simulation_mode(void)
{
    nmode = CTM = STM = 0;
}

void IBM70X_OnClick(void)
{
    int i, n, manual = 0;
    int use_core = 0;

    if (CP_Click.CId == IBM70X.BTN_Power) {
       // DoSCP("set cpanel off");         
       // DoSCP stops the cpu to allow command to be executed by scp
       // is set cpanel off is sent no more commands can be stacked after because if
       // cpanel is off, pendign scp commands issued by cpanel are not processed
        DoSCP("bye");
       return;
    }

    if ((CP_Click.CId == IBM70X.SW_Auto_Manual) || (CP_Click.CId == IBM70X.SW_32K_64K) || (CP_Click.CId == IBM70X.SW_MEM_A_MEM_B)) {
        // are switches, not buttons 
        if (CP_Click.KeyPress_KeyRelease == 2) {
            // release mouse button -> ignore
            return;
        }
    } else if (CP_Click.KeyPress_KeyRelease == 2) {
        // release mouse button -> set again gui control state 0 for button control
        SetState(CP_Click.CId, 0);
        if (CP_Click.CId == IBM70X.BTN_Multiple_Step) {
            Multiple_Step_Mode = 0;   // release Multiple Step button -> terminate multiple step mode
        } else if (CP_Click.CId == IBM70X.SW_Reset) {
            // reset switch pressed -> on reset se release also release all data sw to default (zero) position
            KEYS = 0;
		} else if (Clearing_Main_Mem_Mode < 0) {
			Clearing_Main_Mem_Mode = 1; // on 709, clear mem start on key release
		}
        return;
    } else if (CP_Click.KeyPress_KeyRelease == 0) {
        // click button from scp set cpanel press command
        SetState(CP_Click.CId, 1);
        SetStateAutoNext(CP_Click.CId, 0, 150); // ... and schedule return to initial position again in 150 msec
    } else {
        // press mouse button -> set button control state 1 to show button as pressed 
        SetState(CP_Click.CId, 1);
    }

    // Check if computer is in auto or manual mode. manual = 1 -> Manual mode; manual = 0 -> automatic mode
    // when cpu is stopped, computer can be in manual or auto mode
    // when cpu is in execution, it is supposed to be in auto mode. seeting the manual mode stops the cpu
    manual = GetState(IBM70X.SW_Auto_Manual) ? 1 : 0;
    use_core = 0; // normal mode: 32K, only one core mem (core A)
    if (cpanel_on == IS_IBM7094_CTSS) {
        if (dualcore) {
            use_core = 2; // 65K mode selected
        } else if (GetState(IBM70X.SW_MEM_A_MEM_B)) {
            use_core = 1; // 32K mode, MEM B selected
        } else {
            use_core = 0; // 32K mode, MEM A selected
        }
    }

    if (CP_Click.CId == IBM70X.SW_32K_64K) {
        // click on 32k-64k switch, allways possible
        dualcore = dualcore ? 0 : 1;
        if (dualcore == 0) bcore = 0;
        return;
    }
    if (CP_Click.CId == IBM70X.SW_MEM_A_MEM_B) {
        // click on MEM A/B switch, allways possible
        n = (int) GetState(IBM70X.SW_MEM_A_MEM_B);
        n = n ? 0 : 1; 
        SetState(IBM70X.SW_MEM_A_MEM_B, n);
        return;
    }

    if (CP_Click.CId == IBM70X.SW_Auto_Manual) {
        // click on Auto-Manual switch, allways possible
        manual = 1 - manual;
        SetState(IBM70X.SW_Auto_Manual, manual);  // toggle sw
        if ((sim_is_running) && (manual == 1)) {
            // request a cpu stop, without disabling interactive mode
            cp_stop_flag = 2; 
        }
        return;
    }

    // if cpu running, only clear button and sw to manual are allowed to stop the CPU 
    if ((manual == 0) && (CP_Click.CId == IBM70X.BTN_Clear)) {
        // clear pressed
        int a1, a2;
        if   (use_core == 0) { a1 =       0; a2 = 0077777; } else   // erase core A
        if   (use_core == 1) { a1 = 0100000; a2 = 0177777; }        // erase core B
        else                 { a1 =       0; a2 = 0177777; }        // erase core A & B
        for (i=a1;i<=a2;i++) M[i]=0;
        for (i=0;i<8;i++) XR[i]=0;
        SR = 0; AC = 0; MQ = 0; IC = 0; IR = 0;
        SL = TM = 0; MTM = 1;
        hltinst = dcheck = acoflag = mqoflag = iocheck = 0;
        clear_IBM704_simulation_mode();
		chan_reset(&chan_dev);
        if (sim_is_running) {
            // request a cpu stop, without disabling interactive mode
            cp_stop_flag = 2; 
        }
		// real hw uses IC reg to loop on mem erasing each word. This makes a visible IC flash when going from 
		// 00000 to 77777 (and the 00000) at 12us rate. This is simulated with 
		// with Clearing_Main_Mem_Mode variable. =-1, will start on clear key release, =1 -> crearing in progress
		if ((cpanel_on == IS_IBM704) || (cpanel_on == IS_IBM709)) {
			Clearing_Main_Mem_Mode = -1; 
		} else {
			Clearing_Main_Mem_Mode = 1;
		}
        return;
    }

    if (sim_is_running) return; 

    /* key operations:  sim_is_running==1   sim_is_running==0
    Mode Name                              auto       manual
    SW_Auto_Manual      -> manual: halt     ok          ok
    BTN_Clear           -> sim run=0        ok          nop
    BTN_Start           nop                 ok          nop
    BTN_Load_Card       nop                 ok          nop
    BTN_Load_Tape       nop                 ok          nop
    BTN_Load_Drum       nop                 ok          nop
    BTN_Reset           nop                 nop         ok
    BTN_Multiple_Step   nop                 nop         ok  
    BTN_Single_Step     nop                 nop         ok
    BTN_Display_A/B/C   nop                 nop         ok
    BTN_Display_E.Addr  nop                 nop         ok
    BTN_Display_Storage nop                 nop         ok
    BTN_Display_Ind.    nop                 nop         ok
    BTN_Enter_MQ        nop                 nop         ok
    BTN_Enter_Instr.    nop                 nop         ok
    SW_32K_64K          ok                  ok          ok
    SW_MEM_A_MEM_B      ok                  ok          ok

    */
    // now check the buttons that works in auto mode but sim_is_running == 0
    if (manual == 0) {
        clear_IBM704_simulation_mode();
        if (CP_Click.CId == IBM70X.BTN_Start) {
            DoSCP("go");
            return;
        } 
        if (CP_Click.CId == IBM70X.BTN_Load_Card) {
            DoSCP("boot cdr1");
            return;
        } 
        if (CP_Click.CId == IBM70X.BTN_Load_Tape) {
            DoSCP("boot mta1");
            return;
        } 
        if (CP_Click.CId == IBM70X.BTN_Load_Drum) {
            DoSCP("boot dr");
            return;
        } 
        return;
    }
    // now check the buttons on manual mode
    if (CP_Click.CId == IBM70X.BTN_Reset) {
        IC = 0;  // reset scp command implementation does not clears IC, but manual says console reset key clears also IC
        DoSCP("reset");
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Single_Step) {
        // manual: "if an instruction is executed which causes an I/O device to be connected to the computer,
        // the computer operates in automatic mode until the i/o unit is disconnected": what does this means?
        // but step does freeze the computer after execution of inst. Channel remains connected after inst 
        // execution
        DoSCP("step");
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Multiple_Step) {
        Multiple_Step_Mode = 1;   // press & hold on Multiple Step button -> start multiple step mode
		// Multiple step mode is terminates on button release
    }
    n = (CP_Click.CId == IBM70X.BTN_Display_A) ? 1 : // index numeber in XR array
        (CP_Click.CId == IBM70X.BTN_Display_B) ? 2 : 
        (CP_Click.CId == IBM70X.BTN_Display_C) ? 4 : 
        -1;
    if (n >= 0) {
        Index_Reg_Selected = n; // index will be displayed by refresh routine
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Enter_MQ) {
        SR = 0; // manual says "SR is destroyed"
        MQ = KEYS;
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Enter_Instruction) {
        exe_KEYS = 1; 
        DoSCP("step");
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Display_Storage) {
        SR = M[KEYS & 077777 + (use_core == 2) ? 0100000: 0];
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Display_Indicator) {
        SR = ID;
        return;
    } 
    if (CP_Click.CId == IBM70X.BTN_Display_Effective_Address) {
        uint8 tag;
        uint16 xr;
        tag = (uint8)((SR >> 15) & 07);
        xr = (((tag)) ? ((MTM) ? (XR[(tag)&04] | XR[(tag)&02] | XR[(tag)&01]) : XR[(tag)]) : 0);
        SR = (uint16)(077777 & (SR - xr));
        return;
    } 

}


