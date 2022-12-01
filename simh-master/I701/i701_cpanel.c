/* cpanel.c: simulator control panel simulation

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

   Except as contained in this notice, the name of Robert M Supnik shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from Robert M Supnik.

   May-20    RSV     IBM 650 control panel support
   Jan-21    RSV     IBM NORC control panel support
   Jun-21    RSV     IBM 701 control panel support

   This module implements the following control panels:

   IBM 701 Console (pre-production and production version)
   IBM 711 Card Read 
   IBM 721 Card Punch
   IBM 716 Printer
   IBM 726 Magnetic Tape
  
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "i701_defs.h"	        
#include "sim_tape.h"
#include <math.h>

// cpu registers
extern int                 IC;                          // Instruction Counter
extern int                 IR;                          // Intruction register
extern t_int64             ACC;                         // Accumulator
extern t_int64             MQ;                          // M-Q register
extern t_int64             MR;                          // Memory register
extern int                 OV;                          // Overflow flag 
extern int                 SENSE_OUT;                   // Sense output operator panel lights
extern int                 SENSE_IN;                    // Sense input operator panel option switches
extern int                 NCYCLE;                      // indicates current machine cycle to be executed. 
extern int                 IOADDR;                      // currently/last selected i/o address for operation

extern uint8               StopReason;                  // saved stop reason to be displayed in control panel

// console ligths and switches that are checked during cpu instruction execution
extern int Console_Sw_LoadSelector;      // console load selector: 0=card/1=tape/2=drum
extern int Console_Sw_AutoManual;        // console switch 0=Automatic/1=Manual
extern int Console_Sw_IR_Entry;          // instruction entry
extern int Console_Sw_MQ_Entry;          // M-Q entry

extern int     CpuSpeed_Acceleration;       // cpu speed multiplier
extern int     Measure_CpuSpeed(int);       // return measured cpu speed 

// card reader state
extern int nCardInReadHopperMax; 
extern int nCardInReadHopper;
extern int nCardInReadStacker;
extern uint32 tm0CardInReadStacker;
extern int bCardReadError;           

// card punch state
extern int nCardInPunchStacker;
extern uint32 tm0CardInPunchStacker;
extern int bCardPunchError;           

// mt tape state

// printout state
extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
extern int    lptPrintOutCount;


CP_DEF IBM701_cp[];			// declare forwards
void IBM701_Init(void);
void IBM701_Done(void);
void IBM701_Reset(void);
void IBM701_Refresh(void);
void IBM701_TickIntensityCount(void);
void IBM701_DropFile(int CId, char * FileName);
void IBM701_OnClick_Sw(void);
void IBM701_OnClick_Sw2(void);
void IBM701_OnClick_BTN(void);
void IBM701_OnClick_BTN2(void);

// cpanel types constants
#define IS_IBM701	1	// define CP type contants. must start on 1

// control panel available types
CP_TYPE cp_types = {
     IBM701_cp, 
     &IBM701_Init, &IBM701_Done, &IBM701_Reset, 
     &IBM701_Refresh, &IBM701_TickIntensityCount, &IBM701_DropFile
};

// struct to hold the GUI controls Ids
static struct {
   int CtrlInfoPanel;
   // IBM 701 Console
   // console registers
   int Reg_MR, Reg_ACC, Reg_MQ, Reg_IC, Reg_IR;
   // squared lights
   int LI_Program_Stop, LI_Manual, LI_Instr_Time, LI_Exec_Time, LI_Overflow, LI_Input_Output;
   // sense lights and switches
   int Reg_Sense, SW_Sense[7];
   // Intruction Entry, MQ Entry, Manual Auto Switches
   int SW_IR_Entry[19], SW_MQ_Entry[19], SW_AutoManual[2];
   // Buttons and lower lights
   int BTN_Power_Off, BTN_Reset, BTN_Reset_clr_mem;
   int BTN_Start, BTN_Load, BTN_Mem_Display, BTN_Enter_MQ, BTN_Enter_Instr;
   int LI_Ready, LI_Operating; 
   int LI_Copy_Check, LI_Tape_Check, LI_Div_Check;
   // rotating switch and lower push buttons
   int RSwitch, BTN_Half_Step, BTN_Multiple_Step;
   // IBM 711 card Read 
   int ReadHopper, ReadHopperRear, ReadHopperFront, InputDeck; 
   int ReadStacker, ReadDeck;
   int LI_Ready_CardReader, LI_Select_CardReader, LI_FeedStop_CardReader; 
   int Drop_InputDeckFile; 
   // IBM 721 card Punch
   int PunchStacker, OutputDeck;
   int LI_Ready_CardPunch, LI_Select_CardPunch, LI_FeedStop_CardPunch; 
   // IBM 716 print out
   int Paper, PaperBackground, PrinterCharSet;
   int LI_Ready_Printer, LI_Select_Printer; 
   // IBM 726 Tapes
   int MT_InfoPanel;
   int MT_DoorOpen[4];
   int MT[4], MT_L[4], MT_R[4], MT_head[4], MT_head_medium[4];
   int MT_L_VacCol[4], MT_R_VacCol[4];
   int MT_LI_Read[4], MT_LI_Neutral[4], MT_LI_Write[4], MT_LI_Rewind[4], MT_LI_Select[4], MT_LI_Ready[4];
   int MT_BTN_UnLoad[4], MT_BTN_Backward[4];
   int MT_VacColumn, MT_VacColMedium; 
   int Drop_MT_File[4];
} IBM701 = {0}; // must be init to zero

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF IBM701_cp[] = {
    // IBM 701
    { &IBM701.CtrlInfoPanel,             "CtrlInfoPanel",                     NULL},
    { &IBM701.Reg_MR,                    "Reg_MR",                            NULL},
    { &IBM701.Reg_ACC,                   "Reg_ACC",                           NULL},
    { &IBM701.Reg_MQ,                    "Reg_MQ",                            NULL},
    { &IBM701.Reg_IC,                    "Reg_IC",                            NULL},
    { &IBM701.Reg_IR,                    "Reg_IR",                            NULL},
    { &IBM701.LI_Program_Stop,           "LI_Program_Stop",                   NULL},
    { &IBM701.LI_Manual,                 "LI_Manual",                         NULL},
    { &IBM701.LI_Instr_Time,             "LI_Instr_Time",                     NULL},
    { &IBM701.LI_Exec_Time,              "LI_Exec_Time",                      NULL},
    { &IBM701.LI_Overflow,               "LI_Overflow",                       NULL},
    { &IBM701.LI_Input_Output,           "LI_Input_Output",                   NULL},
    { &IBM701.Reg_Sense,                 "Reg_Sense",                         NULL},
    { &IBM701.SW_Sense[1],               "SW_Sense_1",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_Sense[2],               "SW_Sense_2",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_Sense[3],               "SW_Sense_3",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_Sense[4],               "SW_Sense_4",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_Sense[5],               "SW_Sense_5",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_Sense[6],               "SW_Sense_6",                        &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[1],            "SW_IR_Entry_1",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[2],            "SW_IR_Entry_2",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[3],            "SW_IR_Entry_3",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[4],            "SW_IR_Entry_4",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[5],            "SW_IR_Entry_5",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[6],            "SW_IR_Entry_6",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[7],            "SW_IR_Entry_7",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[8],            "SW_IR_Entry_8",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[9],            "SW_IR_Entry_9",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[10],           "SW_IR_Entry_10",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[11],           "SW_IR_Entry_11",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[12],           "SW_IR_Entry_12",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[13],           "SW_IR_Entry_13",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[14],           "SW_IR_Entry_14",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[15],           "SW_IR_Entry_15",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[16],           "SW_IR_Entry_16",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[17],           "SW_IR_Entry_17",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_IR_Entry[18],           "SW_IR_Entry_18",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[1],            "SW_MQ_Entry_1",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[2],            "SW_MQ_Entry_2",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[3],            "SW_MQ_Entry_3",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[4],            "SW_MQ_Entry_4",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[5],            "SW_MQ_Entry_5",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[6],            "SW_MQ_Entry_6",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[7],            "SW_MQ_Entry_7",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[8],            "SW_MQ_Entry_8",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[9],            "SW_MQ_Entry_9",                     &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[10],           "SW_MQ_Entry_10",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[11],           "SW_MQ_Entry_11",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[12],           "SW_MQ_Entry_12",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[13],           "SW_MQ_Entry_13",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[14],           "SW_MQ_Entry_14",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[15],           "SW_MQ_Entry_15",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[16],           "SW_MQ_Entry_16",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[17],           "SW_MQ_Entry_17",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_MQ_Entry[18],           "SW_MQ_Entry_18",                    &IBM701_OnClick_Sw},
    { &IBM701.SW_AutoManual[1],          "SW_AutoManual",                     &IBM701_OnClick_Sw},
    { &IBM701.BTN_Power_Off,             "BTN_Power_Off",                     &IBM701_OnClick_BTN},
    { &IBM701.BTN_Reset,                 "BTN_Reset",                         &IBM701_OnClick_BTN},
    { &IBM701.BTN_Reset_clr_mem,         "BTN_Reset_and_clear_mem",           &IBM701_OnClick_BTN},
    { &IBM701.BTN_Start,                 "BTN_Start",                         &IBM701_OnClick_BTN},
    { &IBM701.BTN_Load,                  "BTN_Load",                          &IBM701_OnClick_BTN},
    { &IBM701.BTN_Mem_Display,           "BTN_Mem_Display",                   &IBM701_OnClick_BTN},
    { &IBM701.BTN_Enter_MQ,              "BTN_Enter_MQ",                      &IBM701_OnClick_BTN},
    { &IBM701.BTN_Enter_Instr,           "BTN_Enter_Instr",                   &IBM701_OnClick_BTN},
    { &IBM701.LI_Ready,                  "LI_Ready",                          NULL },
    { &IBM701.LI_Operating,              "LI_Operating",                      NULL },
    { &IBM701.LI_Copy_Check,             "LI_Copy_Check",                     NULL },
    { &IBM701.LI_Tape_Check,             "LI_Tape_Check",                     NULL },
    { &IBM701.LI_Div_Check,              "LI_Div_Check",                      NULL },
    { &IBM701.RSwitch,                   "RSwitch",                           &IBM701_OnClick_Sw2},
    { &IBM701.BTN_Half_Step,             "BTN_Half_Step",                     &IBM701_OnClick_BTN},
    { &IBM701.BTN_Multiple_Step,         "BTN_Multiple_Step",                 &IBM701_OnClick_BTN},
    // IBM 711 Card Reader
    { &IBM701.InputDeck,                 "InputDeck",                         NULL, "ibm711/1"  },
    { &IBM701.ReadHopper,                "ReadHopper",                        NULL, "ibm711/1"  },
    { &IBM701.ReadHopperRear,            "ReadHopperRear",                    NULL, "ibm711/1"  },
    { &IBM701.ReadHopperFront,           "ReadHopperFront",                   NULL, "ibm711/1"  },
    { &IBM701.ReadDeck,                  "ReadDeck",                          NULL, "ibm711/1"  },
    { &IBM701.ReadStacker,               "ReadStacker",                       NULL, "ibm711/1"  },
    { &IBM701.LI_Ready_CardReader,       "LI_Ready_CardReader",               NULL, "ibm711/1"  },
    { &IBM701.LI_Select_CardReader,      "LI_Select_CardReader",              NULL, "ibm711/1"  },
    { &IBM701.LI_FeedStop_CardReader,    "LI_FeedStop_CardReader",            NULL, "ibm711/1"  },
    { &IBM701.Drop_InputDeckFile,        "Drop_InputDeckFile",                NULL, "ibm711/1"  },
    // IBM 721 Card Punch
    { &IBM701.PunchStacker,              "PunchStacker",                      NULL, "ibm721/1"  },
    { &IBM701.OutputDeck,                "OutputDeck",                        NULL, "ibm721/1"  },
    { &IBM701.LI_Ready_CardPunch,        "LI_Ready_CardPunch",                NULL, "ibm721/1"  },
    { &IBM701.LI_Select_CardPunch,       "LI_Select_CardPunch",               NULL, "ibm721/1"  },
    { &IBM701.LI_FeedStop_CardPunch,     "LI_FeedStop_CardPunch",             NULL, "ibm721/1"  },
    // IBM 716 Printer
    { &IBM701.Paper,                     "Paper",                             NULL, "ibm716/1"  },
    { &IBM701.PaperBackground,           "PaperBackground",                   NULL, "ibm716/1"  },
    { &IBM701.PrinterCharSet,            "PrinterCharSet",                    NULL, "ibm716/1"  },
    { &IBM701.LI_Ready_Printer,          "LI_Ready_Printer",                  NULL, "ibm716/1"  },
    { &IBM701.LI_Select_Printer,         "LI_Select_Printer",                 NULL, "ibm716/1"  },
    // IBM 726 Tape
    { &IBM701.MT_InfoPanel,              "MT_InfoPanel",                      NULL, "ibm726/1"  },
    { &IBM701.MT_VacColumn,              "MT_VacColumn",                      NULL, "ibm726/1"  },
    { &IBM701.MT_VacColMedium,           "MT_VacColMedium",                   NULL, "ibm726/1"  },
    { &IBM701.MT_DoorOpen[0],            "MT_01_door_L",                      NULL, "ibm726/1"  },
    { &IBM701.MT[0],                     "MT_0",                              NULL, "ibm726/1"  },
    { &IBM701.MT_L[0],                   "MT_0_L",                            NULL, "ibm726/1"  },
    { &IBM701.MT_R[0],                   "MT_0_R",                            NULL, "ibm726/1"  },
    { &IBM701.MT_head[0],                "MT_0_head",                         NULL, "ibm726/1"  },
    { &IBM701.MT_head_medium[0],         "MT_0_head_medium",                  NULL, "ibm726/1"  },
    { &IBM701.Drop_MT_File[0],           "Drop_MT0_File",                     NULL, "ibm726/1"  },
        { &IBM701.MT_L_VacCol[0],        "MT_0_L_VacCol",                     NULL, "ibm726/1"  },
        { &IBM701.MT_R_VacCol[0],        "MT_0_R_VacCol",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Read[0],      "MT_0_LI_Read",                      NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Neutral[0],   "MT_0_LI_Neutral",                   NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Write[0],     "MT_0_LI_Write",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Rewind[0],    "MT_0_LI_Rewind",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Select[0],    "MT_0_LI_Select",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Ready[0],     "MT_0_LI_Ready",                     NULL, "ibm726/1"  },
           { &IBM701.MT_BTN_UnLoad[0],   "MT_0_BTN_UnLoad",                   &IBM701_OnClick_BTN2, "ibm726/1"  },
           { &IBM701.MT_BTN_Backward[0], "MT_0_BTN_Backward",                 &IBM701_OnClick_BTN2, "ibm726/1"  },
    { &IBM701.MT_DoorOpen[1],            "MT_01_door_R",                      NULL, "ibm726/1"  },
    { &IBM701.MT[1],                     "MT_1",                              NULL, "ibm726/1"  },
    { &IBM701.MT_L[1],                   "MT_1_L",                            NULL, "ibm726/1"  },
    { &IBM701.MT_R[1],                   "MT_1_R",                            NULL, "ibm726/1"  },
    { &IBM701.MT_head[1],                "MT_1_head",                         NULL, "ibm726/1"  },
    { &IBM701.MT_head_medium[1],         "MT_1_head_medium",                  NULL, "ibm726/1"  },
    { &IBM701.Drop_MT_File[1],           "Drop_MT1_File",                     NULL, "ibm726/1"  },
        { &IBM701.MT_L_VacCol[1],        "MT_1_L_VacCol",                     NULL, "ibm726/1"  },
        { &IBM701.MT_R_VacCol[1],        "MT_1_R_VacCol",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Read[1],      "MT_1_LI_Read",                      NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Neutral[1],   "MT_1_LI_Neutral",                   NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Write[1],     "MT_1_LI_Write",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Rewind[1],    "MT_1_LI_Rewind",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Select[1],    "MT_1_LI_Select",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Ready[1],     "MT_1_LI_Ready",                     NULL, "ibm726/1"  },
           { &IBM701.MT_BTN_UnLoad[1],   "MT_1_BTN_UnLoad",                   &IBM701_OnClick_BTN2, "ibm726/1"  },
           { &IBM701.MT_BTN_Backward[1], "MT_1_BTN_Backward",                 &IBM701_OnClick_BTN2, "ibm726/1"  },
    { &IBM701.MT_DoorOpen[2],            "MT_23_door_L",                      NULL, "ibm726/1"  },
    { &IBM701.MT[2],                     "MT_2",                              NULL, "ibm726/1"  },
    { &IBM701.MT_L[2],                   "MT_2_L",                            NULL, "ibm726/1"  },
    { &IBM701.MT_R[2],                   "MT_2_R",                            NULL, "ibm726/1"  },
    { &IBM701.MT_head[2],                "MT_2_head",                         NULL, "ibm726/1"  },
    { &IBM701.MT_head_medium[2],         "MT_2_head_medium",                  NULL, "ibm726/1"  },
    { &IBM701.Drop_MT_File[2],           "Drop_MT2_File",                     NULL, "ibm726/1"  },
        { &IBM701.MT_L_VacCol[2],        "MT_2_L_VacCol",                     NULL, "ibm726/1"  },
        { &IBM701.MT_R_VacCol[2],        "MT_2_R_VacCol",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Read[2],      "MT_2_LI_Read",                      NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Neutral[2],   "MT_2_LI_Neutral",                   NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Write[2],     "MT_2_LI_Write",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Rewind[2],    "MT_2_LI_Rewind",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Select[2],    "MT_2_LI_Select",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Ready[2],     "MT_2_LI_Ready",                     NULL, "ibm726/1"  },
           { &IBM701.MT_BTN_UnLoad[2],   "MT_2_BTN_UnLoad",                   &IBM701_OnClick_BTN2, "ibm726/1"  },
           { &IBM701.MT_BTN_Backward[2], "MT_2_BTN_Backward",                 &IBM701_OnClick_BTN2, "ibm726/1"  },
    { &IBM701.MT_DoorOpen[3],            "MT_23_door_R",                      NULL, "ibm726/1"  },
    { &IBM701.MT[3],                     "MT_3",                              NULL, "ibm726/1"  },
    { &IBM701.MT_L[3],                   "MT_3_L",                            NULL, "ibm726/1"  },
    { &IBM701.MT_R[3],                   "MT_3_R",                            NULL, "ibm726/1"  },
    { &IBM701.MT_head[3],                "MT_3_head",                         NULL, "ibm726/1"  },
    { &IBM701.MT_head_medium[3],         "MT_3_head_medium",                  NULL, "ibm726/1"  },
    { &IBM701.Drop_MT_File[3],           "Drop_MT3_File",                     NULL, "ibm726/1"  },
        { &IBM701.MT_L_VacCol[3],        "MT_3_L_VacCol",                     NULL, "ibm726/1"  },
        { &IBM701.MT_R_VacCol[3],        "MT_3_R_VacCol",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Read[3],      "MT_3_LI_Read",                      NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Neutral[3],   "MT_3_LI_Neutral",                   NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Write[3],     "MT_3_LI_Write",                     NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Rewind[3],    "MT_3_LI_Rewind",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Select[3],    "MT_3_LI_Select",                    NULL, "ibm726/1"  },
           { &IBM701.MT_LI_Ready[3],     "MT_3_LI_Ready",                     NULL, "ibm726/1"  },
           { &IBM701.MT_BTN_UnLoad[3],   "MT_3_BTN_UnLoad",                   &IBM701_OnClick_BTN2, "ibm726/1"  },
           { &IBM701.MT_BTN_Backward[3], "MT_3_BTN_Backward",                 &IBM701_OnClick_BTN2, "ibm726/1"  },

    { NULL }  
};

int bCardReadVisible;                   // ibm 711 is visible
int bCardPunchVisible;                  // ibm 721 is visible
int bTapesVisible;                      // tape visible
int bPrintOutVisible;                   // printout visible

// animation state vars (for dynamic state draw)
// for card reader
int hInputDeck                 = 0; // height of dynamically calculated input card deck
int hOutputDeck                = 0; // height of dynamically calculated output card deck

// for main cpu
int CpuSpeed_Acceleration_save = 0; // save value during HotKey ^F Max Speed
int bShowInfo                  = 0; // flag to show info for ^I 
uint32 ShowInfoTm0             = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                 = 0; // instr executed on ShowInfoTm0
int FramesCount0               = 0; // num of frames on ShowInfoTm0
int bSwitchAnimInProgress      = 0; // indicates is switch is moving up or down
int nShowInfoCpuTick           = 0; // Ticks remaining to be exec for current instr 
int nShowInfoStall             = 0; // if >0, the device that is stalling the CPU

// for tape cabinet
#define MT_anim_sequence_len   500        // max number of animation sequence steps for tape
struct mtcabrec {                         // mtcab[0..5].reel[0..1] is arecord that holds the tape reel states
   int mt_is;                             // current visual animation being done
   // state of tape elements
   int rew_u3;                            // amount of tape medium on R reel during rewind operation (used to show info with ^I)
   int rw_tm0, rw_msec, rw_recsize;       // r/w operation start (sim_os_msec time), duration left (msec), record size left (x1000 inch)
   struct mtreelrec {               
       int VacCol_h;                      // ammount of medium (x1000 inch) in vacuum column. 0=no medium, >0 medium loop going down into the vaccol in direction of upper sensor
       double rpm;                        // reel current revolutions per second (0=stopped, <0=backwards)
       int ang;                           // reel current angular position (but NOT normalizaed to 0..360 degrees! normalize before using)
       int motor;                         // reel motor operation in progress (-1=accelerate backwards, 0=decelerate, 1=accelerate forward)
       int n;                             // val of reel control state n 
       int tm0;                           // timestamp when reel motor start accelerating/decelerating (in sim_os_msec time). =0 when motor stoped
       double rpm0;                       // revolutions per second (0=stopped, <0=backwards) when reel motor start accelerating/decelerating 
       int ang0;                          // reel angular position (0..360 degrees) when reel motor start accelerating/decelerating
       double revs;                       // reel revoltions done after motor start accel/decel (1 revolution = 360 gr)
   } reel[2];
   // last value of heigh of tape medium into vaccum colum used to detect h has changed respect 
   // previous refresh frame and redraw it
   int L_VacColMedium_h0, R_VacColMedium_h0; 
   // animation sequence data
   int nseq;
   int nseq_tm0;
   struct { // animation sequence
      int msec; // time needed to perform this step
      int hint; // hint: type of sequence step
      int MT_Reel_Amount, L_ang_inc, R_ang_inc, MT_head, L_VacCol_inc, R_VacCol_inc; 
   } seq[MT_anim_sequence_len];
} mtcab[4];
int bTapeAnimInProgress;                    // signals tape medium/reels are moving
int bTapeLastButtonPressedCId =0;           // to detect pressing BackWatd then UnLoad on tape unit to detach

// for printer printout
int lptPrintOutDoneCount       = -1; // number of lines already printed on cpanel paper
int hPaperBackgroundOffset;          // offset of background image on paper image

// Control Panel callbacks
void IBM701_Init(void)
{

    bTapesVisible = bPrintOutVisible = 0;
    bCardReadVisible = bCardPunchVisible = 0;

    if ((IsOption("Console")) || (IsOption("EarlyConsole"))) {
    } else {
        bTapesVisible = bPrintOutVisible = 1;
        bCardReadVisible = bCardPunchVisible = 1;
    }

    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }
    memset (mtcab, 0, sizeof mtcab);
    bTapeAnimInProgress=0;

    hInputDeck = hOutputDeck = 0;
    lptPrintOutDoneCount=-1; // number of lines already printed on cpanel paper
}

void IBM701_Done(void)
{
}

void IBM701_Reset(void)
{
}


void IBM701_TickIntensityCount(void)
{
    // count for each register, each bit value so we can draw a half-intensity image on refresh

    // refresh callback the intensity on light on each bits
    TickCount(IBM701.Reg_MR, MR);
    TickCount(IBM701.Reg_MQ, ACC);
    TickCount(IBM701.Reg_ACC, MQ);
    TickCount(IBM701.Reg_IC, IC);
    TickCount(IBM701.Reg_IR, IR);

    if (NCYCLE == 0) { 
        // Interpretation half cycle
        TickCount(IBM701.LI_Instr_Time, 1);
        TickCount(IBM701.LI_Exec_Time, 0);
    } else { 
        // Execution half cycle
        TickCount(IBM701.LI_Instr_Time, 0);
        TickCount(IBM701.LI_Exec_Time, 1);
    }
    TickCount(IBM701.LI_Overflow, OV);

    // lit on while connected to I/O device
    TickCount(IBM701.LI_Input_Output, (IOADDR==0) ? 0:1); 
}

// handle up/down switch 
// swCIdArray -> is the switch Control Id array
// CSWvar = pointer to cpu variable that holds value corresponding to switches. Cannot be NULL
// nSWNum = number of switches in array (max 31). If <0 bit 0 at left
void SetUpDwSwitch(int * swCIdArray, int * CSWvar, int nSWNum) 
{
    int i, n, CId, motion_in_progress, csw, update_CSWvar, CSWar_sync, bits_lr; 
    #define  bitn   ( (bits_lr == 0) ? nSWNum-i : i-1)

    if (nSWNum > 0) {
       bits_lr = 0; 
    } else {
       bits_lr = 1; 
       nSWNum = -nSWNum; 
    }

    motion_in_progress=0;
    csw = *CSWvar;    // current value of cpu var corresponfing to swtiches up/down
    update_CSWvar=0;  // <>0 -> wich has finished its motion, shoud update CSWvar bit using current switches position
    CSWar_sync   =1;  // 1=CSWvar is synced with switches positions up/down, 
                      // 0=not synced -> should update current switches position using CSWvar value
    for (i=1; i<=nSWNum; i++) {
        CId = swCIdArray[i];
        n = (int) GetState(CId);
        if (n >= 2) motion_in_progress=1; 
        if ((n == 2) || (n == 3)) {
            SetState(CId, n+1); // direction up motion -> state 2->4 then 0
        } else if (n == 4) {
            SetState(CId, 0); // switch is up -> state is zero
            update_CSWvar = -i; 
        } else if ((n == 5) || (n == 6)) {
            SetState(CId, n+1); // direction up motion -> state 5->7 then 1
        } else if (n == 7) {
            SetState(CId, 1); // switch is down -> state is one
            update_CSWvar = i; 
        } else if ((n==0) || (n==1)) {
            // switch position is full up/full down. Vheck if matches with cpu variable corresponding bit
            if (((csw >> bitn) & 1) != n) CSWar_sync = 0; 
        }
    }
    if (motion_in_progress) {
        // motion in progress. 
        bSwitchAnimInProgress=1; // signal there are at leas one switch moving
        // check if should update CSWvar bit using current switches position
        if (update_CSWvar>0) {
            // set bit to one
            i = update_CSWvar;
            *CSWvar = csw | (1 << bitn); 
        } else if (update_CSWvar<0) {
            // set bit to zero
            i = -update_CSWvar;
            *CSWvar = csw & (~(1 << bitn)); 
        }
    } else {
        // no motion in progress. 
        // check if switches and cpu var are synced. 
        // If not should update switches position using current CSWvar bit value
        if (CSWar_sync == 0) {
            for (i=1; i<=nSWNum; i++) {
                CId = swCIdArray[i];
                n = ((csw >> bitn) & 1);
                SetState(CId, n);
            }
        }
    }
}



#define     MT_is_loading_tape      1
#define     MT_is_rewinding         2
#define     MT_is_unloading_tape    3

int PARAM_MaxSlice_msec  =   100;  // max time considered for tape hop calculations
int PARAM_Reel_Diameter  =   267;  // reel diameter in mm 
int PARAM_RPM            =  1600;  // reel forward/backwards motor revolutions per minute
int PARAM_VacCol_h_Low   = 16200;  // upper vacuum colum sensor (inches x 1000) triggers reel load medium on colum 
int PARAM_VacCol_h_Hi    = 32000;  // lower vacuum colum sensor (inches x 1000) triggers reel take medium from colum 
int PARAM_RWSpeed        =    75;  // tape head operates at 75 inches per sec
int PARAM_AccelTime      =   105;  // accel time in msec. clutch must obtain 2/3 of maximum speed in 0.135-0.150 seconds with a full reel of tape
int PARAM_DecelTime      =   115;  // decel time in msec. the stop clutch must stop a full reel of tape from full speed in 0.145-0.160 sec
                                   // note: the accel/decel time increases when tape unit
                                   // get used. Lower values makes jerky spins with thigh 
                                   // oscilation of tape loop in vacuum columns. Higher values
                                   // makes smother spins, with more ample oscilations of tape loop
int PARAM_HeadOpenTime   =   700;  // time in msec needed by r/w tape head to fully open or close
int PARAM_TakeMotor_RPM  =    40;  // take motor revolutions per minute


#define     MT_anim_step_nop       -1             // do nothing, just keeps current MT state values
#define     MT_anim_step_inc        0             // incremental animation step
#define     MT_anim_step_rw         1             // read/write tape animation step 
#define     MT_anim_finished       99             // this is the final step that signals the animation sequence has finished
#define     MT_anim_no_change    (1<<30)          // no change the current value.

void mt_reels_mov(int unit, int cmd, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot, 
                  int * MT_Reel_Amount, int * MT_head);

// execute animation step. return 1 if animation terminated
int mt_do_animation_seq(int unit, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot, 
                  int * MT_Reel_Amount, int * MT_head)
{
    int time, nseq, msec, hint, recsize, ang, n, m, u3, p; 
    int L_inc, R_inc, L_into_col, R_into_col; 
    int tnow = Refresh_tnow; 

    time = tnow - mtcab[unit].nseq_tm0;
    if (time < 0) return 1; // end of sequence
    nseq=0;
    for(;;) {
        msec=mtcab[unit].seq[nseq].msec;
        if (msec==0) return 1;              // exit beacuse end of sequence
        // if time>0 -> we have past this animation step. Only execute pending incremental steps
        // if time < 0 -> we are at this amimation step, so execute it and return 
        time=time-msec;                     
        // type of step
        hint = mtcab[unit].seq[nseq].hint;  
        if (hint == MT_anim_finished) return 1; // exit beacuse end of sequence
        if ((hint == MT_anim_step_nop) && (time < 0)) {
            // set current values for this point of sequence, do nothing
            // already done incremental step. Just keep current state values
            *MT_Reel_Amount = (int) GetState(IBM701.MT[unit]); 
            *MT_L_Rot = (int) GetState(IBM701.MT_L[unit]); // keeps blur
            *MT_R_Rot = (int) GetState(IBM701.MT_R[unit]);                           
            *MT_head =  (int) GetState(IBM701.MT_head[unit]);  
            if (GetState(IBM701.MT_head_medium[unit]) == 0) *MT_head = *MT_head + 100;  // signal no tape medium in capstains
            if (GetState(IBM701.MT_DoorOpen[unit]))         *MT_head = *MT_head + 1000; // signal cabient door open
            *L_VacColMedium_h = mtcab[unit].reel[0].VacCol_h;
            *R_VacColMedium_h = mtcab[unit].reel[1].VacCol_h;
            return 0; // exit after step execution. tnow is in this step
        } else if (hint == MT_anim_step_inc) { 
            // This is incremental animation step, should be done allways.
            // Execute it and mark as done to avoid issuing it more than once
            mtcab[unit].seq[nseq].hint=MT_anim_step_nop; 
            // apply this step tape medium increment into vacuum cols
            L_inc=mtcab[unit].seq[nseq].L_VacCol_inc; // how much tape medium enters (if >0) or are removed (if <0) from VacCol
            R_inc=mtcab[unit].seq[nseq].R_VacCol_inc;
            L_into_col= (mtcab[unit].reel[0].VacCol_h < 0) ? 0:1; // =0 if no tape medium into vacuum column
            R_into_col= (mtcab[unit].reel[1].VacCol_h < 0) ? 0:1; 
            if ((L_inc < 0) && (R_inc < 0)) {
                // removing medium from column, determine if one col is empty
                if ((L_into_col) && (R_into_col)) {
                    // both columns with tape medium inside
                } else if (L_into_col) { 
                    // R column empty, all movement goes to column L
                    L_inc += R_inc; R_inc=0; 
                } else if (R_into_col) { 
                    // L column empty, all movement goes to column R
                    R_inc += L_inc; L_inc=0; 
                } else { 
                    // L and R columns empty -> terminate tape unloading from columns
                    // So mark as done any following step inc that still ask to remove medium from VacCol
                    for (n=nseq+1;;n++) {
                        if (mtcab[unit].seq[n].hint != MT_anim_step_inc) break; // not an incremental animation step
                        if ((mtcab[unit].seq[n].L_VacCol_inc >= 0) || (mtcab[unit].seq[n].R_VacCol_inc >=0)) break; // not removing medium from vaccol  
                        mtcab[unit].seq[n].hint=MT_anim_step_nop; // mark as done
                        mtcab[unit].seq[n].msec=1; // only stands for 1 msec
                    }
                    // set to zero to avoid negative values
                    mtcab[unit].reel[0].VacCol_h = L_inc = 0; 
                    mtcab[unit].reel[1].VacCol_h = R_inc = 0; 
                }
            } 
            // apply this step tape medium increment into vacuum cols
            mtcab[unit].reel[0].VacCol_h += L_inc; 
            mtcab[unit].reel[1].VacCol_h += R_inc; 
            // apply this step angular increment to reels 
            // (but only if thereis some medium into vaccol to be moved)
            if (L_into_col) {
                ang = mtcab[unit].reel[0].ang + mtcab[unit].seq[nseq].L_ang_inc; 
                ang = ang % 360; if (ang < 0) ang +=360;
                mtcab[unit].reel[0].ang = ang; 
            }
            if (R_into_col) {
                ang = mtcab[unit].reel[1].ang + mtcab[unit].seq[nseq].R_ang_inc; 
                ang = ang % 360; if (ang < 0) ang +=360;
                mtcab[unit].reel[1].ang = ang; 
            }
            // set the tape position of its elements
            if (MT_anim_no_change & mtcab[unit].seq[nseq].MT_Reel_Amount) *MT_Reel_Amount = (int) GetState(IBM701.MT[unit]); 
            else *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            *MT_L_Rot = mtcab[unit].reel[0].n = (mtcab[unit].reel[0].ang % 60) * 12 / 60;
            *MT_R_Rot = mtcab[unit].reel[1].n = (mtcab[unit].reel[1].ang % 60) * 12 / 60;
            *MT_head  = mtcab[unit].seq[nseq].MT_head; 
            *L_VacColMedium_h = mtcab[unit].reel[0].VacCol_h;
            *R_VacColMedium_h = mtcab[unit].reel[1].VacCol_h;
            // clear 
            mtcab[unit].reel[0].motor = mtcab[unit].reel[1].motor = 0;
            mtcab[unit].reel[0].tm0 = mtcab[unit].reel[1].tm0 = 0;
            mtcab[unit].reel[0].rpm0 = mtcab[unit].reel[1].rpm0 = 0;
            mtcab[unit].reel[0].ang0 = mtcab[unit].reel[1].ang0 = 0;
            mtcab[unit].reel[0].revs = mtcab[unit].reel[1].revs = 0;
            mtcab[unit].reel[0].rpm = mtcab[unit].reel[1].rpm = 0;
            // do not update mtcab[unit].L|R_VacColMedium_h0
            mtcab[unit].rw_msec = mtcab[unit].rw_recsize = 0;
            // incremental animation step done. Now check time variable. if time < 0 
            // then this is the current amimation step, so return 
            // on the contrary, it time >= 0 this is a pending incremental step that has
            // been executed. continue the main loop execute any other pending incremental
            // step until we arrive at the current animantion step
            if (time < 0) return 0; // exit after step execution. tnow is in this step
        } else if ((hint == MT_anim_step_rw) && (time < 0)) {
            // this step is regular tape mevement made by the r/w header, either stoped, backwards
            // or forwards, at 140 inch/sec. Simulate also reel spinning to take/load medium in vac col
            // animation variables usage
            //     temporary: seq[nseq].MT_Reel_Amount
            //        to hold tm0 value, to determine the time elapsed from previous refresh
            //     parameter: seq[nseq].MT_head  
            //        to hold the tape movement direction. Can be 1 (forward), -1 (backwards)
            //        or zero (tape movement stoped, but update reels spinning if some not
            //        yet stopped)
            //     parameter: seq[nseq].L_VacCol_inc  
            //        if >0 is the amount of medium to rew at low speed rew/to move fwd or backwrds. 
            //        update mtcab[unit].rew_u3 with the amount of tape remaining to be rew
            //     parameter: seq[nseq].R_VacCol_inc  
            //        if =0 is low speed rew -> update rew_u3
            //        if =1 is moving tape forward/backwards -> update u3;
            u3 = mtcab[unit].seq[nseq].L_VacCol_inc;
            m  = msec+time;  // m=msec+time = time elapsed in this step. msec = this step duration
            time = tnow - mtcab[unit].seq[nseq].MT_Reel_Amount;
            mtcab[unit].seq[nseq].MT_Reel_Amount = tnow;
            // max time considered between refreshes 
            if ((time < 0) || (time > PARAM_MaxSlice_msec)) time=PARAM_MaxSlice_msec; 
            n=mtcab[unit].seq[nseq].MT_head; 
            recsize  = n * time * PARAM_RWSpeed; // inches x1000, 
            recsize = recsize / 2; 
            mtcab[unit].reel[0].VacCol_h -= recsize;
            mtcab[unit].reel[1].VacCol_h += recsize;
            mt_reels_mov(unit, 0, 
                     L_VacColMedium_h, R_VacColMedium_h, 
                     MT_L_Rot, MT_R_Rot, 
                     MT_Reel_Amount, MT_head);
            if (u3) {
                if (mtcab[unit].seq[nseq].R_VacCol_inc==0) {
                    // update mtcab[unit].rew_u3 to signal progress of rewind
                    p = 1000 - 1000*m / msec; 
                    if (p>1000) p=1000; if (p<0) p=0; // m=0..1000 is % x10 of time of step remaining
                    // calculate rew_u3 = how much medium has been rew 
                    mtcab[unit].rew_u3 = (int) (((t_int64) (u3)) * ((t_int64) (p)) / 1000);
                    *MT_Reel_Amount = 1 + (int) (50 * (mtcab[unit].rew_u3 / (mt_unit[unit].u4*1000.0))); 
                    if (*MT_Reel_Amount > 51) *MT_Reel_Amount = 51;
                } else {
                    // update mt_unit[unit].u3 to signal progress of r/w
                    mt_unit[unit].u3 = (int) (1.0 * u3 * (n < 0 ? msec-m:m) / msec); // to avoid int overflow
                }
                // check fast mode
                if (CpuSpeed_Acceleration == -1) {
                    // if Key Control-F (^F, Ctrl F) being pressed 
                    // reduce duration of this step, make it done when no duration left
                    time = msec - m; // time remaining 
                    m = mtcab[unit].seq[nseq].msec - time / 20; // reduce a fraction of remaining time
                    if (m > 0) {
                        mtcab[unit].seq[nseq].msec = m; 
                    } else {
                        mtcab[unit].seq[nseq].hint=MT_anim_step_nop; 
                        mtcab[unit].seq[nseq].msec = 1; 
                    }
                }
            }       
            return 0; // exit after step execution. tnow is in this step
        }
        nseq++; 
    }
    return 0;
}

// dump to console the current animation
void mt_dump_animation_seq(int unit)
{
    int i; 

    printf("Animation set for unit %d\r\n", unit); 

    for(i=0;i<mtcab[unit].nseq;i++) {
        printf("nseq %d, hint %d, msec %d, Amnt %d, La=%d, Ra=%d, Lh=%d, Rh=%d, Hd %d \r\n", 
            i, mtcab[unit].seq[i].hint, mtcab[unit].seq[i].msec, mtcab[unit].seq[i].MT_Reel_Amount,
            mtcab[unit].seq[i].L_ang_inc, mtcab[unit].seq[i].R_ang_inc, 
            mtcab[unit].seq[i].L_VacCol_inc, mtcab[unit].seq[i].R_VacCol_inc, 
            mtcab[unit].seq[i].MT_head);
    }
}

// add animation sequence step 
void AddSeq(int unit, int msec, int hint, 
            int L_VacCol_inc, int R_VacCol_inc, 
            int L_ang_inc, int R_ang_inc,
            int MT_Reel_Amount, int MT_head)
{
    int i; 
    i=mtcab[unit].nseq++; // inc sequence
    if (i>=MT_anim_sequence_len) {
        fprintf(stderr, "Cannot add MT sequence step. Animation array full\n");
        mtcab[unit].seq[MT_anim_sequence_len-1].msec = 0; // mark last as end of sequence
        return;
    }
    if ((msec == 0) || (hint == MT_anim_finished)) {
        msec = 0; 
        hint = MT_anim_finished;
    }

    mtcab[unit].seq[i].msec = msec; 
    mtcab[unit].seq[i].hint = hint; 
    mtcab[unit].seq[i].MT_Reel_Amount = MT_Reel_Amount; 
    mtcab[unit].seq[i].L_ang_inc = L_ang_inc; 
    mtcab[unit].seq[i].R_ang_inc = R_ang_inc; 
    mtcab[unit].seq[i].L_VacCol_inc = L_VacCol_inc; 
    mtcab[unit].seq[i].R_VacCol_inc = R_VacCol_inc; 
    mtcab[unit].seq[i].MT_head = MT_head;
}

// add to the current animation sequence the load animation:
//    - open tape cabinet door
//    - open r/w head
//    - set medium in tape capstains
//    - close r/w head
//    - close tape cabinet door
//    - spin reels slowly to load medium on vacuum columns
void mt_add_load_seq(int unit)
{
    int MT_head; // state of controls at end of of redraw
    int i, L_h, R_h, L_inc_h, R_inc_h, msec, ang_inc, r1, r2;

    // open MT cabinet door
    AddSeq(unit, 150, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            0 /* MT_Reel_Amount */, 1112 /* MT_head + Open Door + no medium */);        

    // user needs time to open magnetc tape r/w head assembly
    // prepare animation sequence each given msec
    // states 12..1 = closed head .. open head
    MT_head          = 12;   // head full closed
    msec= PARAM_HeadOpenTime / 12; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0 /* MT_Reel_Amount */, 1100 + MT_head /* MT_head + Open Door + no medium */);        
        if (MT_head==1) break; 
        MT_head--;
    }

    // add reels and set medium in r/w head capstain
    AddSeq(unit, 500, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            +15 /* L_ang_inc */, +15 /* R_ang_inc */, 
            1 /* MT_Reel_Amount - all medium on Left */, 1000 + MT_head /* MT_head open + Open Door + tape medium in capstains*/);        

    // user closes magnetc tape r/w head assembly
    // prepare animation sequence each given msec
    // states 1..12 = open to closed head, open door
    msec= PARAM_HeadOpenTime / 12; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                1 /* MT_Reel_Amount - all medium on Left */, 1000 + MT_head);       
        if (MT_head==12) break; 
        MT_head++;
    }

    // close MT cabinet door
    AddSeq(unit, 750, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            1 /* MT_Reel_Amount */, 12 /* MT_head closed */);        

    // reels rotates 360 + 90 + 45 gr during this time (take motor goes at 46 rpm)
    // prepare animation sequence each given msec
    msec=33;                                   // time for animation step 
    ang_inc = (msec * PARAM_TakeMotor_RPM * 360) / (60 * 1000); // angle reel spins on each animation step

    // calculate the amount of tape medium that reel loads into vaccol on each step given reel rotation
    r1=(50 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel empty
    r2=(90 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel full
    r1=(int) (0.0393701 * r1 * 1000);          // reel radius in inches x 1000
    r2=(int) (0.0393701 * r2 * 1000);            
    L_inc_h = (int) (r2 * 2 * 3.1416 * ang_inc / 360); // beacuse L reel is full. 
    R_inc_h = (int) (r1 * 2 * 3.1416 * ang_inc / 360); // beacuse R reel is empty
    L_inc_h = L_inc_h  / 2;                            // tape loop position is half on medium loaded
    R_inc_h = R_inc_h  / 2; 

    // tape medium enter on both vac cols at same time. Side L feeds faster 
    // because L reel is full (so more medium enters in vaccol for same angular reel speed)
    // terminates when both coulums are fed
    L_h = 0;  // amount of tape medium in each column
    R_h = 0;

    for(i=0;;i++) {
        // reel rotation
        AddSeq(unit, msec, MT_anim_step_inc, 
                L_inc_h  /* L_VacCol_inc */, R_inc_h /* R_VacCol_inc */, 
                +ang_inc /* L_ang_inc */, -ang_inc /* R_ang_inc */, 
                1 /* MT_Reel_Amount */, 12 /* MT_head */ );       
        L_h += L_inc_h; 
        R_h += R_inc_h; 
        if ((R_h > PARAM_VacCol_h_Low) && (L_h > PARAM_VacCol_h_Low)) break; 
    }
}

// add to the current animation sequence the unload animation:
//    - spin reels slowly to unload medium from vacuum columns
//    - open tape cabinet door
//    - open r/w head
//    - remove medium from tape capstains
//    - close r/w head
//    - close tape cabinet door
void mt_add_unload_seq(int unit)
{
    int MT_head; // state of controls at end of of redraw
    int i, L_h, R_h, L_inc_h, R_inc_h, msec, ang_inc, r1, r2;

    // prepare animation sequence each given msec
    msec=33;                                   // time for animation step 
    ang_inc = (msec * PARAM_TakeMotor_RPM * 360) / (60 * 1000); // angle reel spins on each animation step

    // calculate the amount of tape medium that reel unloads into vaccol on each step given reel rotation
    r1=(50 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel empty
    r2=(90 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel full
    r1=(int) (0.0393701 * r1 * 1000);          // reel radius in inches x 1000
    r2=(int) (0.0393701 * r2 * 1000);            
    L_inc_h = (int) (r2 * 2 * 3.1416 * ang_inc / 360); // beacuse L reel is full. 
    R_inc_h = (int) (r1 * 2 * 3.1416 * ang_inc / 360); // beacuse R reel is empty
    L_inc_h = L_inc_h  / 2;                            // tape loop position is half on medium loaded
    R_inc_h = R_inc_h  / 2; 

    L_h = R_h = (int) (PARAM_VacCol_h_Low * 1.1 * 1.2); // maximum teorical value of tape medium in vacCol + 20% of safety margin

    // remove tape medium from both vac cols. 
    // terminates when both are empty
    for(i=0;;i++) {
        // reel rotation
        AddSeq(unit, msec, MT_anim_step_inc, 
                -L_inc_h  /* L_VacCol_inc */, -R_inc_h /* R_VacCol_inc */, 
                -ang_inc /* L_ang_inc */, +ang_inc /* R_ang_inc */, 
                1 /* MT_Reel_Amount */, 12 /* MT_head */ );       
        L_h -= L_inc_h; 
        R_h -= R_inc_h; 
        if ((R_h < 0) && (L_h < 0)) break; 
    }

    // open MT cabinet door
    AddSeq(unit, 750, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            1 /* MT_Reel_Amount - all medium on Left */, 1012 /* MT_head + Open Door + medium */);        

    // user needs time to open magnetc tape r/w head assembly
    // prepare animation sequence each given msec
    // states 12..1 = closed head .. open head
    MT_head          = 12;   // head full closed
    msec= PARAM_HeadOpenTime / 12; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                1 /* MT_Reel_Amount - all medium on Left */, 1000 + MT_head /* MT_head open + Open Door + tape medium in capstains*/);        
        if (MT_head==1) break; 
        MT_head--;
    }

    // remove reels and set medium in r/w head capstain
    AddSeq(unit, 500, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            +15 /* L_ang_inc */, +15 /* R_ang_inc */, 
            0 /* MT_Reel_Amount */, 1100 + MT_head /* MT_head open + Open Door + no medium */);        

    // user closes magnetc tape r/w head assembly
    // prepare animation sequence each given msec
    // states 1..12 = open to closed head, open door
    msec= PARAM_HeadOpenTime / 12; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0 /* MT_Reel_Amount */, 1100 + MT_head);       
        if (MT_head==12) break; 
        MT_head++;
    }

    // close MT cabinet door
    AddSeq(unit, 750, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            0 /* MT_Reel_Amount */, 1112 /* MT_head closed */);        

    // mt_dump_animation_seq(unit);
}

// calculate and store in animation array the animation sequence for rewind
void mt_set_rew_seq(int unit, int bStartEndSeqFlag)
{
    int u3 = mt_info[unit].recsize;  // use recsize to get u3 value on rewind start 
    int msec, time; 

    if (bStartEndSeqFlag) {
        mtcab[unit].nseq=0;                 // init sequence
        mtcab[unit].nseq_tm0=Refresh_tnow;  // when animation starts
    } else if (u3==0) {
        // nothing to rewind, so exit
        return; 
    }

    // rewind at low speed (as it was reading backeard)
    msec = u3 / PARAM_RWSpeed;              // time to rewind tape in msec
    AddSeq(unit, msec, MT_anim_step_rw,  u3 /* update recsize */,0,0,0,0, 
                                         -1 /* read backwards */); 



    if (bStartEndSeqFlag) {
        // end sequence
        AddSeq(unit, 0,  MT_anim_finished, 0,0,0,0,0,0); 
    }

    // log on debug precise rew time 
    {
       DEVICE *dptr = find_dev_from_unit(&mt_unit[unit]);
       int i; 
       time = 0; 
       for (i=0;i<MT_anim_sequence_len;i++) {
           msec = mtcab[unit].seq[i].msec; 
           if ((msec == 0) || (mtcab[unit].seq[i].hint == MT_anim_finished)) break;
           time += msec;
       }
       sim_debug(DEBUG_CMD, dptr, "Tape unit %d: rewind time needed (%d sec)\n", 
                                   unit, time/1000);
    }

    // mt_dump_animation_seq(unit);
}

// calculate and store in animation array the animation for reels
// if cmode = 'L' -> load animation: tape is mounted (attached) to tape unit. 
// If cmode = 'F' or 'B' load animation will be move all medium forwards/backwards
// if cmode = 'U' -> unload animation: tape is dismounted (detached) to tape unit. 
// during animations, from simulated cpu point of wiew: the tape is at its final state
//                                                      animation can be interrupted at any moment by tape command
void mt_set_anim_seq(int unit, char cmode)
{
    int n, msec, r, R_h;
    int tm0, tm1; 

    // on load, user places the medium over vacuum columns, and manually 
    // closes the head assembly. Then the reels turns slowly to feed medium 
    // into columns. Then goes backwards to read read record backwards 
    // (locate the load point)

    mtcab[unit].nseq=0;                 // init sequence
    mtcab[unit].nseq_tm0=Refresh_tnow;  // when animation starts

    if ((cmode == 'F') || (cmode == 'B')) {
        // forward all tape sequence
        msec = mt_unit[unit].u4 * 1000 / PARAM_RWSpeed;
        if (cmode == 'B') {
            mt_unit[unit].u3 = mt_unit[unit].u4 * 1000;
            n=-1; 
        } else {
            n= 1;
        }
        AddSeq(unit, msec, MT_anim_step_rw,  mt_unit[unit].u4 * 1000 ,1,
                                             0,0,0, n /* read fwd/bckwrd */); 
        // end sequence
        AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 
        return; 
    }

    // start delay depends on other loads/unloads already in progress 
    // This is done to avoid animations to be synchonized
    // determine when last animation started
    tm0=0;
    for (n=0;n<4;n++) {
        if (n==unit) continue; 
        if (((mtcab[n].mt_is == MT_is_loading_tape) || (mtcab[n].mt_is == MT_is_unloading_tape)) &&
             (mtcab[n].seq[0].MT_head == 0)) {
            // load/unload anim in progress in unit n. Let's know when it started
            tm1=mtcab[n].nseq_tm0; 
            if (mtcab[n].seq[0].MT_head==0) tm1 += mtcab[n].seq[0].msec; // add initial wait if any
            if (tm0 < tm1) {
                tm0 = tm1; // time when last anim started (after any initial wait time)
            }
        }
    }
    // determine wait time to start
    msec=0; 
    if (tm0) {
        // there is an ongoing animation started (after initial wait) at tm0
        tm0 += 1750; // add delay to anim to not be sync with previous one 
        if (tm0 > mtcab[unit].nseq_tm0) {
            // should wait 
            msec = tm0 - mtcab[unit].nseq_tm0; 
        }
    }
    if (msec == 0) msec=10; // min waiting time. Needed to identify it is an unload/load waiting
    AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                (cmode == 'U') ? MT_anim_no_change:0  /* MT_Reel_Amount */, 
                (cmode == 'U') ? 12:0 /* MT_head */ ); // head closed + medium on unload, close no meduim on load


    if (cmode == 'U') {
        mt_set_rew_seq(unit,0);
        mt_add_unload_seq(unit);
        // end sequence
        AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 
        return; 
    }

    // normal load sequence (asumed cmode = 'L')
    
    // make sure no medium on vac col
    mtcab[unit].reel[0].VacCol_h = 0;  // amount of tape medium in each column
    mtcab[unit].reel[1].VacCol_h = 0;

    // add the load animation:
    mt_add_load_seq(unit);

    // Now sense load point by reading backwards
    // Duration of it  depends on how much the user has spinned reel R when 
    // preparing the tape for load
    // when reel is empty, it holds aprox 29,6 inches of tape medium in each revolution
    // r/w heads moves the tape medium at 140 inches/sec 
    r=(50 * PARAM_Reel_Diameter / 2) / 100;  // radius when reel empty
    r=(int) (0.0393701 * r * 1000);          // reel radius in inches x 1000
    R_h = (int) (r * 2 * 3.1416);            // ammount to medium on R reel circunference. 
    msec = R_h / PARAM_RWSpeed;              // time to read medium on reel circunference. 

    // Asume user has done 5-15 rev when preparing the tape
    msec = 5 * msec + 10 * msec * (sim_rand() & 0xFF) / 256;
    AddSeq(unit, msec, MT_anim_step_rw,  0,0,0,0,0, 
                                         -1 /* read backwards */); 
    // end sequence
    AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 
}


// tape r/w head & reels & vacuum columns simulation
// update tape controls for loop position on VacCols based on reels movement, loop 
// past the vaccols upper/lower sensors, r/w head reading forwards or backwards
void mt_reels_mov(int unit, int cmd, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot, 
                  int * MT_Reel_Amount, int * MT_head)
{
    uint32 tnow = Refresh_tnow;   
    static uint32 old_tnow = 0; 

    int time, time1, recsize, ireel, r, r1, r2, p, h,
        ang_inc, ang, n, tm0, msec,  motor0, motor;
    double rpm, rpm0, rpmMax, rpsMax, a, am, revs; 
    
    struct oldrec {
        int h, ang, ang_inc, n;
        double rpm, revs;  
    } old[2]; 

    // save state of reels
    for(ireel=0;ireel<2;ireel++) {  
        old[ireel].h   = mtcab[unit].reel[ireel].VacCol_h;
        old[ireel].ang = mtcab[unit].reel[ireel].ang;
        old[ireel].ang_inc = 0;
        old[ireel].n = mtcab[unit].reel[ireel].n;
        old[ireel].rpm = mtcab[unit].reel[ireel].rpm;
        old[ireel].revs = mtcab[unit].reel[ireel].revs;
    }

    // calc percent 0..100 of reel full
    p = (int) ((mt_unit[unit].u3 / (mt_unit[unit].u4*1000.0))*100); // r=0 -> reel L full, p=100 -> reel R full
    if (p>100) p=100;

    // calc radius of reel based on how much tape medium stored is in it 
    // reel diameter = 267mmm -> radius = 133mm. 
    r1=(50 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel empty
    r2=(90 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel full
    r1=(int) (0.0393701 * r1 * 10);            // reel radius in inches x 1000
    r2=(int) (0.0393701 * r2 * 10);            // reel radius in inches x 1000

    // calc max reel spin speed. Take into account reduction by motor pulleys of 2/5
    rpsMax = (PARAM_RPM * 2.0 / 5.0) / 60;  // reel revolutions per second at full speed

    // if a r/w operation in progress, then move tape medium under tape head 
    // to/from vacuum colums
    if (mtcab[unit].rw_msec) { 
        // calc time elapsed on r/w cmd
        time = tnow - mtcab[unit].rw_tm0;
        if (time > mtcab[unit].rw_msec) time = mtcab[unit].rw_msec;
        // calc medium (in inches x1000) that has been moved under r/w head
        recsize = (mtcab[unit].rw_recsize * time / mtcab[unit].rw_msec);
        mtcab[unit].rw_recsize -= recsize;
        mtcab[unit].rw_msec    -= time; 
        mtcab[unit].rw_tm0      = tnow;
        // take/give medium to vacuum columns
        // the maximun time slice allowed to avoid too big hops on tape medium is PARAM_MaxSlice_msec
        // PARAM_RWSpeed is the inches per second tape head moves the medium when reading/writing
        // this means the maximum linear increment reading or writing (in inches x1000) is
        n=PARAM_RWSpeed * PARAM_MaxSlice_msec;
        if (recsize > n) recsize = n;
        if (recsize < -n) recsize = -n;
        recsize = recsize / 2; // when adding/removing medium, loop position in vacuum colum moves half distance
        mtcab[unit].reel[0].VacCol_h -= recsize;
        mtcab[unit].reel[1].VacCol_h += recsize;
        bTapeAnimInProgress=1; // signal tape medium movement in progress 
    }
    // check if a new r/w operation can be schedulled 
    if ((mtcab[unit].rw_msec == 0) && (cmd==1) && (mt_info[unit].cmd_tm0)) { 
        // if no r/w op being animated (rw_msec=0), and now a r/w operation is being done by tape (cmd_tm0 > 0)
        mt_info[unit].cmd_tm0=0;   // mark this as being animated (to avoid animating it twice)
        mtcab[unit].rw_tm0     = tnow; 
        mtcab[unit].rw_msec    = mt_info[unit].cmd_usec / 1000;
        mtcab[unit].rw_recsize = mt_info[unit].recsize;
        bTapeAnimInProgress=1; // signal tape medium movement started
    }
    
    // calc reel algular position base on motor acceleratin or decelerating the reel
    for(ireel=0;ireel<2;ireel++) {  
        // get initial motor status
        motor = mtcab[unit].reel[ireel].motor;
        tm0   = mtcab[unit].reel[ireel].tm0;
        if (tm0 == 0) continue;      // motor not started, skip 
        // time is elapsed time accelerating/decelerating (in msec) from motor start (this is not the refresh time slice)
        time = tnow - tm0;           
        // calc influence in acceleration/deceleration time of quantity of tape medium 
        // currently wound in the reel
        // this is a very simple aproximation. 
        // When reel is full of medium then am=1.0 (keep same accel time as in PARAM)
        // when reel is empty, am=0.3 (time is PARAM *0.3 -> accelerates/decelerates faster)
        am=0.3 + 0.7 * ( (ireel==0) ? 100-p:p) / 100;     
        // calc new angular position
        rpm0 = mtcab[unit].reel[ireel].rpm0;
        if (motor != 0) {
            // motor is accelerating reel forwards/backwards
            rpmMax = rpsMax * 360.0 / 1000;             // ang incr (gr 0..360) per msec at full speed
            if (motor < 0) rpmMax=-rpmMax;              // going backwards
            if (ireel)  rpmMax=-rpmMax;                 // R reel rotates CCW then loading medium (motor=1)
            a = rpmMax / (PARAM_AccelTime * am);        // angular acceleration per msec. >0 turning CW
            // calc spinining speed (rpm = angular increment in degrees per msec )
            rpm = a * time + rpm0;                      
            // check not exceeding max rpm's
            if (  ((rpm > 0) && (rpm > rpmMax)) ||
                  ((rpm < 0) && (rpm < rpmMax)) ) { 
                // if exceeded max speed, recalculate accel time 
                msec  = (int) ((rpmMax - rpm0) / a);    // time accerating
                time1 = time - msec;                    // time at full speed
                time  = msec;                           // time accelerating
                rpm   = rpmMax; 
                // check that time does not exceed
            } else time1 = 0;
            // calc how many revolutions has done the reel counting from start of motor accelerating
            // = revolutions done while accelerating (during time msecs) + revs done at full speed (during time1 msecs)
            revs = ((a * time * time / 2 + rpm0 * time) + (rpmMax * time1)) / 360; 
        } else {
            // reel is decelerating
            rpmMax = rpsMax * 360.0 / 1000;             // ang incr (gr 0..360) per msec at full speed
            a = rpmMax / (PARAM_DecelTime * am);        // deceleration per msec
            if (rpm0 < 0) a = -a;                       // decelerating while spinning backwards
            // calc time accelerationg (time var), time at full speed (time1) and reel rpm speed
            rpm = -a * time + rpm0;                     // calc reel speed 
            // check if stopped (rpm diferent sign than rpm0)
            if (  ((rpm < 0) && (rpm0 > 0)) ||
                  ((rpm > 0) && (rpm0 < 0)) ||
                  (rpm == 0) || (rpm0 == 0)  ) { 
                // stopped, recalculate decel time 
                time  = (int) (rpm0 / a);               // time decerating (a and rpm allways have same sign)
                rpm   = 0; 
            }
            // calc how many revolutions has done the reel counting from start of motor decelerating
            // = revolutions done while decelerating (during time msecs) (plus nothing more: no revs done when reel stoped)
            revs = (-a * time * time / 2 + rpm0 * time) / 360; 
            mtcab[unit].reel[ireel].rpm = rpm; 
            // check if reel stoped
            if (rpm == 0) {
                mtcab[unit].reel[ireel].tm0 = 0;
            }
        }
        mtcab[unit].reel[ireel].rpm  = rpm;  // save current reel speed
        mtcab[unit].reel[ireel].revs = revs; // save current revolutions done
        // calc how much degrees reel has rotated in this refresh frame. Not normalized! can be <-360 or >360 
        ang_inc = (int) ((revs - old[ireel].revs) * 360); 
        old[ireel].ang_inc = ang_inc; // save here for later use
        // calc new angular position of reel: its original position when accel/decel was 
        // started + revolutions done during accel/decel (0..360 gr)
        ang = (int) (mtcab[unit].reel[ireel].ang0 + revs * 360);
        ang = ang % 360; if (ang < 0) ang += 360; 
        mtcab[unit].reel[ireel].ang  = ang;  // save current angular position
        // calc radius of reel based on how much tape medium stored is in it 
        r=r1*100 + (r2-r1)* ( (ireel==0) ? 100-p:p);     // reel current radius in inches x 1000
        // the maximun time slice allowed to avoid too big hops on tape medium is PARAM_MaxSlice_msec
        // rpsMax = (PARAM_RPM * 2.0 / 5.0) / 60  is reel revolutions per second at full speed
        // this means the maximum angular increment at full motor rpm is
        ang = (int) (360 * rpsMax * PARAM_MaxSlice_msec / 1000); 
        if (ang_inc > ang) ang_inc = ang; 
        if (ang_inc < -ang) ang_inc = -ang; 
        // calc medium (in inches x1000) that has been moved from/to reel
        recsize = (int) (r * 2 * 3.1416 * ang_inc / 360);
        recsize = recsize / 2; // when adding/removing medium, loop position in vacuum column moves half distance
        // recsize and ang have the sing of ang_inc (indicates if load/take medium)
        // when L reel turns forwards (CW direction, that is rpm > 0, ang value increases, 
        // revs > 0) then recsize > 0 -> medium feed/loaded in vaccol. 
        // when R reel turns forwards (CW direction, that is rpm > 0, ang value increases, 
        // revs > 0) then recsize > 0 -> medium take/unloaded from vaccol. 
        // if R reel then change sign of recsize
        if (ireel) recsize = -recsize;
        // take/give medium to reels vacuum columns
        mtcab[unit].reel[ireel].VacCol_h += recsize; 
    }
   
    // sense medium in vac cols and act on reel motors
    for(ireel=0;ireel<2;ireel++) {  
        // get initial motor status
        motor0 = mtcab[unit].reel[ireel].motor;
        // get heigh of medium in vac col (0 = no medium loaded, nnn = these inches of medium loaded
        h = mtcab[unit].reel[ireel].VacCol_h;
        // ammount of medium in vac col is measured from top of column to tape medium loop
        // when no much medium is in vac col, h is low, when colum is full of medium then h is high
        if (h < PARAM_VacCol_h_Low) {              // Upper column sensor h position
            motor=1; // accelerate forward, should load medium in colum
        } else if (h > PARAM_VacCol_h_Hi) {        // Lower column sensor h postion
            motor=-1; // accelerate backwards, should take medium from column
        } else {
            motor=0;   // decelerate motor (break reel)
        }
        // has motor changed its state?
        if (motor0!=motor) {
            // yes!, motor start (accelerate) or motor brake (decelerate). 
            time = 0;
            // check % of tape loop has passed the sensor
            if ((old[ireel].h < PARAM_VacCol_h_Low) && (PARAM_VacCol_h_Low < h)) {
                // tape loop in VacCol going down from very top (reel load medium into near empty column), 
                // tape loop pass over upper sensor
                r = 100 * (PARAM_VacCol_h_Low - old[ireel].h) / (h - old[ireel].h);
            } else if ((h < PARAM_VacCol_h_Hi) && (PARAM_VacCol_h_Hi < old[ireel].h)) {
                // tape loop in VacCol going up from very bottom (reel takes medium from near full column), 
                // tape loop pass over lower sensor
                r = 100 * (PARAM_VacCol_h_Hi - old[ireel].h) / (h - old[ireel].h);
            } else {
                r = 100; // tape loop has not passed over a sensor. 
            }
            if (r<0) r=0; if (r>100) r=100; // safety
            if (r < 100) {
                // interpolate rpm, ang, h based on p, calc time = msecs ago tape loop passed sensor
                time = (tnow - old_tnow) * (100 - r) / 100; 
                old[ireel].ang_inc = old[ireel].ang_inc * r / 100; 
                ang = old[ireel].ang + old[ireel].ang_inc; 
                ang = ang % 360; if (ang<0) ang += 360; 
                mtcab[unit].reel[ireel].ang = ang; 
                mtcab[unit].reel[ireel].rpm      = (mtcab[unit].reel[ireel].rpm - old[ireel].rpm) 
                                                       * r / 100 + old[ireel].rpm;
                mtcab[unit].reel[ireel].VacCol_h = (h - old[ireel].h) 
                                                       * r / 100 + old[ireel].h;
            }
            // set new state of motor
            mtcab[unit].reel[ireel].motor = motor;
            // save time (tnow) when motor starts accelerating/decelerating
            mtcab[unit].reel[ireel].tm0   = tnow - time; 
            // init number of revolutions done
            mtcab[unit].reel[ireel].revs = 0; 
            // save reel initial angular position when motor starts accelerating/decelerating
            mtcab[unit].reel[ireel].ang0 = mtcab[unit].reel[ireel].ang;
            // save reel initial reel rpm speed when motor starts accelerating/decelerating
            mtcab[unit].reel[ireel].rpm0 = mtcab[unit].reel[ireel].rpm;             
        }
    }

    // set reel/vacuum columns control state on cpanel
    for(ireel=0;ireel<2;ireel++) {       
        // get heigh of medium in vac col (0 = no medium loaded, nnn = these inches of medium loaded
        h = mtcab[unit].reel[ireel].VacCol_h;  
        // set height of medium 
        if (ireel) *R_VacColMedium_h = h; else *L_VacColMedium_h = h;
        // get angluar position of reel
        ang = mtcab[unit].reel[ireel].ang; 
        // calc state that match with angular position of reel
        // each state is rotated 5 gr
        n = 12 * (ang % 60) / 60; 
        // set speed blur
        rpm = mtcab[unit].reel[ireel].rpm;
        if (rpm < 0) rpm =-rpm; // only abs values
        rpmMax = rpsMax * 360.0 / 1000;             // ang incr (gr 0..360) per msec at full speed
        if (rpm < 0.35 * rpmMax) { 
            // no blur, use calculated state
            mtcab[unit].reel[ireel].n = n; 
        } else {
            // blur, set reel control at previous reel angular position respect
            // rotation direction. 
            r = n + ((old[ireel].ang_inc > 0) ? -1:1);
            r = (r % 12); if (r<0) r+=12; 
            if (r != old[ireel].n) n = r; // use it if not same as last reel pos
            mtcab[unit].reel[ireel].n = n; 
            if (rpm < 0.50 * rpmMax) { 
                n = 13 + n % 3; // use blur states 13-15
            } else {
                n = 16 + n % 3; // use blur states 16-18
            }
            bTapeAnimInProgress=1;  // signal reel blur started 
        }
        // set angular position state
        if (ireel) {
            *MT_R_Rot = n; 
        } else {
            *MT_L_Rot = n;
        }
    }
    // set head/reel amount control state on cpanel
    // % of tape medium used
    *MT_Reel_Amount = 1 + (int) (50 * (mt_unit[unit].u3 / (mt_unit[unit].u4*1000.0))); 
    if (*MT_Reel_Amount > 51) *MT_Reel_Amount = 51;
    // tape head on closed position
    *MT_head = 12; 

    // save last tnow used to know how much time elapsed since last refresh
    old_tnow = tnow;  
}

// dynamic draw of tape medium into vaccum column control CId_VacCol
// if VacColMedium_h = PARAM_VacCol_h_Low -> tape medium loop positioned over upper vacuum colum sensor
// if VacColMedium_h = PARAM_VacCol_h_Hi  -> tape medium loop positioned over lower vacuum colum sensor
void mt_VacColSetDynamicState(int VacColMedium_h, int * VacColMedium_h0, int CId_VacCol)
{
    int h, y;

    if (VacColMedium_h == -1) {
        // force redraw
        VacColMedium_h = *VacColMedium_h0;
    } else {
        if (VacColMedium_h == *VacColMedium_h0) return; // no changes in height. nothing to draw
        *VacColMedium_h0 = VacColMedium_h; 
    }

    // tape medium control is 700 pixels height 
    // on vac col h=280 -> upper sensor, h=600 -> lower sensor (h=0 -> top) 
    h= 280 + ((610-280) * (VacColMedium_h-PARAM_VacCol_h_Low) / (PARAM_VacCol_h_Hi-PARAM_VacCol_h_Low));
    if (h<0) h=0; 
    // h is vertical position of tape loop base, convert to y coord
    // y=700 <-> VacColMedium_h = 0 (medium outside vac col)
    // y=610 <-> VacColMedium_h = PARAM_VacCol_h_Low (medium over upper sensor)
    // y=280 <-> VacColMedium_h = PARAM_VacCol_h_Hi  (medium over lower sensor)

    y=700-h; if (y<0) y=0; 
    // Dynamically generate State 0 for CId_VacCol (=IBM701.MT_L|R_VacCol[unit])

    //  - Copy State 0 from control MT_VacColumn (background) to state 0 of CId (state displayed) 
    CopyControlImage(IBM701.MT_VacColumn, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol, 0,                0, 0);       // ToCId, ToState,     x1, y1
    //  - Copy State 0 from MT_VacColMedium (height dependes on medium position) on State 0 -> now vaccol with tape medium
    CopyControlImage(IBM701.MT_VacColMedium, 0,    0, y, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                      CId_VacCol,                   0, 0, 0);    // ToCId, ToState,     x1, y1
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId_VacCol, 0);

}


// return 0 if tape is idle (not moving the medium), 1 if tape is doing read/write, -1 if rew
int get_mt_current_command(int unit)
{
    int cmd;

    cmd=mt_get_last_cmd(unit);
    if (cmd == OP_REWIND) {
        return -1;  
    }
    if ((cmd ==  OP_WRITE) || (cmd ==  OP_WRITE_EF) || 
        (cmd ==  OP_READ) || (cmd ==  OP_READ_B) ) {
        return 1; // tape read/write operation
    }
    return 0;
}

void Refresh_MagTape(void)
{
    int unit, n;
    UNIT *uptr;

    int MT_Reel_Amount0, MT_L_Rot0, MT_R_Rot0; // state of controls at beggining of redraw
    int MT_Reel_Amount,  MT_L_Rot,  MT_R_Rot,  MT_head, MT_head_medium, MT_DoorOpen; // state of controls at end of of redraw
    int L_VacColMedium_h, R_VacColMedium_h; // annount of tape medium in vaccol
    int cmd, mt_is;
    int ioaddr, ioop, unit_selected;
    
    GetIOADDR(&ioaddr, &ioop); 
    unit_selected = ioaddr-256; 
    if ((unit_selected <0) || (unit_selected >3)) unit_selected=-1; 

    bTapeAnimInProgress=0;
    for(unit=0;unit<4;unit++) {
        uptr=&mt_unit[unit];
        // check if unit disabled/no file attached
        if ((uptr->flags & UNIT_DIS) || ((uptr->flags & UNIT_ATT) == 0)) {
            if (mt_info[unit].justdetached==1)  {  // 1 -> just detached -> should start unload animation
                if (mtcab[unit].mt_is == MT_is_unloading_tape) {
                    // unload amination in progress, continue
                } else {
                    // start unload amination
                    mtcab[unit].mt_is = MT_is_unloading_tape; 
                    mt_set_anim_seq(unit, 'U'); 
                }
            } else {
                if ((GetState(IBM701.MT[unit])==0) && (GetState(IBM701.MT_head[unit])==0)) continue; 
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT[unit], 0);          // no magnetic medium on reels
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_head[unit], 0);     // head transparent (seen as closed)
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_L[unit], GetState(IBM701.MT_L[unit]) % 12); // keep the angular position of reel          
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_R[unit], GetState(IBM701.MT_R[unit]) % 12);          
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_head_medium[unit], 0); // no tape medium into read/write head
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Read[unit], 0);     // all ligths off
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Neutral[unit], 0); 
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Write[unit], 0); 
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Rewind[unit], 0); 
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Select[unit], 0); 
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_LI_Ready[unit], 0); 
                // cabinet door closed
                cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_DoorOpen[unit],0);     
                // empty vac cols
                mtcab[unit].L_VacColMedium_h0=0;
                mtcab[unit].R_VacColMedium_h0=0;
                mt_VacColSetDynamicState(-1, &mtcab[unit].L_VacColMedium_h0, IBM701.MT_L_VacCol[unit]);
                mt_VacColSetDynamicState(-1, &mtcab[unit].R_VacColMedium_h0, IBM701.MT_R_VacCol[unit]);
                continue; 
            }
        } 
        // tape has file attached (=tape reel mounted)
        // check if just being attached

        if (mt_info[unit].justattached) {  // 1 -> just attached -> should read reel tape options, if any 
            // read reel color options, if any. reel color options are processed each time a tape file is attached to MT device, so
            // reel color options must be set before the attach scp command
            char MT_cab_opt[16] = "MTx";
            char c, cmode;

            memset (&mtcab[unit], 0, sizeof (mtcab[0]) );
            cmode = ' '; 

            MT_cab_opt[2] = '0' + unit;
            if ((IsOption(MT_cab_opt)) && (IsOptionParam) && (*IsOptionParam++ == '/')) {
                c = *IsOptionParam++; 
                c = sim_toupper(c);
                if ((c == 'R') || (c == 'F')) {
                    cmode = c; // load animation will be Forward/backwards all reel, or R
                }
                RemoveOption(MT_cab_opt); // remove this option, as it is being executed now so will not apply on next attach of tape
            }
            // tape cabinet options set. 
            mt_info[unit].justattached=0;
            // Now init the cabinet states and ... 
            mtcab[unit].reel[0].ang = (sim_rand() & 511) % 360; // reel mounted at random angular position
            mtcab[unit].reel[1].ang = (sim_rand() & 511) % 360;
            mtcab[unit].L_VacColMedium_h0 = -1; // init to -1 last state to force redraw of vaccol 
            mtcab[unit].R_VacColMedium_h0 = -1; 
            // ... signal the load animation can begin
            mtcab[unit].mt_is = MT_is_loading_tape; 
            if (cmode != 'R') {
                // normal load animation, F 
                 mt_set_anim_seq(unit, cmode);
            } else {
                // R rewind animation 
                // start rewind. This is nice!
                mt_info[unit].recsize = mt_unit[unit].u4 * 1000;
                mt_set_rew_seq(unit,1);
            }
        }
        // tape is about to be painted on cpanel
        // MT state holds the amount of tape medium in each reel. 
        //   =1  -> all tape on L reel, 
        //   =51 -> all tape on R reel, 
        //   =0  -> no medium on tape reel 
        // MT_L/MT_R state holds the rotational position of reel, the speed ot reel, and the reel's colour
        //    0..11 -> reel rotated 0gr, 5gr, 10gr, ... 55gr. To be used when reel is not moving
        //   12..13 -> same reel rotated 0gr, 90gr ... but with some spin blur. To be used when reel is moving
        //   14..15 -> reel rotalted 0gr, 90gr, ... whith greater spin blur. To be used when reel is rewinding at fast pace
        // MT_head holds the position of r/w head
        //   =0  -> transparent (so head closed)
        //   =1  -> head open, prepared to manualy remove tape medium
        //   =12 -> closed head, prepared to read or write tape medium
        // MT_head_medium signals if there is tape medium in read/write head capstains
        //  = 0 -> no tape medium
        //  = 1 -> there is tape medium
        // MT_DoorOpen signals the state of tape door
        //  = 0 -> closed
        //  = 1 -> open
        // MT_LI_Read/Neutral/Write/Rewind/Select/Ready signals tate of tape light indicators
        //  = 0 -> light off
        //  = 1 -> light on

        // get the current tape control state. If this state changes, then should redraw
        MT_Reel_Amount0   = (int) GetState(IBM701.MT[unit]); 
        MT_L_Rot0         = (int) GetState(IBM701.MT_L[unit]);
        MT_R_Rot0         = (int) GetState(IBM701.MT_R[unit]);

        // what tape is doing now?
        cmd = get_mt_current_command(unit); // current tape cmd being executed: -1=rew, 0=idle, 1=read/write
        mt_is =  mtcab[unit].mt_is;         // the current animation being done (visual state of tape)

        // check if load/unload/rew animation in progress should be aborted
        if (mt_is == MT_is_loading_tape) {
            if (mt_info[unit].numrw > 0) {
                // if any mt r/w command issued by cpu then abort any load animation in progress
                mtcab[unit].mt_is = mt_is = 0;
                // position of medium on vaccol as it should be at end of load
                mtcab[unit].reel[0].VacCol_h = (int) (PARAM_VacCol_h_Hi  * 0.9); 
                mtcab[unit].reel[1].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
            }
        } else if (mt_is == MT_is_unloading_tape) {
            // no reason to abort unload anim. if tape attached, mt_is will be set to MT_is_loading
        } else if (mt_is == MT_is_rewinding) {
            if (  (cmd == 1) || (mt_ready(unit)==1) ) {
                // any mt r/w command in progress terminates any rew animation in progress
                // if tape ready then terminates any rew animation in progress (the rew command has terminated its execution)
                mtcab[unit].mt_is = mt_is = 0;
                // position of medium on vaccol as it should be at end of load
                mtcab[unit].reel[0].VacCol_h = (int) (PARAM_VacCol_h_Hi  * 0.9); 
                mtcab[unit].reel[1].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
            }
        }
        
        // check if should start rew animation
        if ((mt_is == 0) && (cmd < 0) && (mt_ready(unit)==0)) {
               // if (no animation in progress) and (last tape cmd is rew) and (last cmd in execution) 
               mtcab[unit].mt_is = mt_is = MT_is_rewinding;
               mt_set_rew_seq(unit,1);
        }

        // advance animation if any 
        if (mt_is > 0) {
            n = mt_do_animation_seq(unit,              
                         &L_VacColMedium_h, &R_VacColMedium_h, 
                         &MT_L_Rot, &MT_R_Rot, 
                         &MT_Reel_Amount, &MT_head);
            if (n==1) {
                mtcab[unit].mt_is = 0; // normal animation termination
                if (mt_is == MT_is_rewinding) {
                    // if rew terminates, set again MT_RDY
                    mt_set_ready(unit);
                } else if (mt_is == MT_is_unloading_tape) {
                    // if unload terminates, clear justdetached flag
                    mt_info[unit].justdetached=0;
                    continue; // skip tape state update, just continue so next frame will show no tape mounted
                }
                mt_is = 0; // set to zero to allow normal tape processing
            }
        }

        // if no animation, simulate reel/vacuum col movement
        if (mt_is == 0) {
            // if tape at end of file, do not perform cmd 
            if (uptr->u3 > uptr->u4*1000) cmd = 0;
            mt_reels_mov(unit, cmd, 
                         &L_VacColMedium_h, &R_VacColMedium_h, 
                         &MT_L_Rot, &MT_R_Rot, 
                         &MT_Reel_Amount, &MT_head);
        }
       
        // decode extra info coded in MT_head = DMnn (decimal)
        //    nn = 00..99 head state, D = 0..1 door open, M = 0..1 no medium on capstains
        // if MT_head < 100, show tape medium 
        MT_head_medium = (((MT_head / 100) % 10) == 0) ? 1:0; if (MT_head == 0) MT_head_medium=0;
        // if MT_head < 1000, show tape medium 
        MT_DoorOpen    = (((MT_head / 1000) % 10) == 0) ? 0:1; 
        MT_head = MT_head % 100;


        SetState(IBM701.MT_DoorOpen[unit], MT_DoorOpen);
        if (cpanel_State_has_changed) {
            // MT cabinet door changed its state -> invalidate all the MT controls so they are redraw again
            MT_Reel_Amount0 = MT_L_Rot0 = MT_R_Rot0 = -1;
            cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT[unit], 0);
            cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_L[unit], 0);
            cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_R[unit], 0);
            cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_head_medium[unit], 0); 
            cpanel_ControlRedrawNeeded = 1; SetState(IBM701.MT_head[unit], 0); 
        }

        // update tape control states
        if ((MT_Reel_Amount0 != MT_Reel_Amount) || 
            (MT_L_Rot0 != MT_L_Rot) || (MT_R_Rot0 != MT_R_Rot) ) {
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM701.MT[unit], MT_Reel_Amount);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM701.MT_L[unit], MT_L_Rot);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM701.MT_R[unit], MT_R_Rot);
        }
        SetState(IBM701.MT_head_medium[unit], MT_head_medium); 
        SetState(IBM701.MT_head[unit], MT_head); 
        
        // set dynamic state for vacCol controls 
        mt_VacColSetDynamicState(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, IBM701.MT_L_VacCol[unit]);
        mt_VacColSetDynamicState(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, IBM701.MT_R_VacCol[unit]);

        // set tape lights for IBM 726 Magnetic Tape
        if (mt_info[unit].TapeCheck) {
            cmd=mt_get_last_cmd(unit); // if tape check get but no clear last exec command
        } else {
            cmd=mt_get_last_cmd(10+unit); // get & clear last exec command
        }
        if ((cmd == OP_READ) || (cmd==OP_READ_B)) cmd = 1; else 
        if ((cmd == OP_WRITE) || (cmd==OP_WRITE_EF)) cmd = 2; else cmd = 0;
        SetState(IBM701.MT_LI_Read[unit], (cmd == 1) ? 1:0);
        SetState(IBM701.MT_LI_Neutral[unit], (cmd == 0) ? 1:0);
        SetState(IBM701.MT_LI_Write[unit], (cmd == 2) ? 1:0);
        SetState(IBM701.MT_LI_Ready[unit], 
            ((mt_unit[unit].flags & UNIT_DIS) || ((mt_unit[unit].flags & UNIT_ATT) == 0) || (mt_is != 0))  ? 0:1); // Ready light on -> dev enabled and file attached and no animation in progress
        SetState(IBM701.MT_LI_Select[unit], ((unit == unit_selected) || (cmd != 0)) ? 1:0); // Ready light on -> device under control of calculator
        SetState(IBM701.MT_LI_Rewind[unit], mtcab[unit].mt_is == MT_is_rewinding);
    }
}


int PARAM_char_ww2       =    13;  // horizontal spacing between first pixel of a char and first pixel of next char in next char same line
int PARAM_char_hh2       =    23;  // height between top line of one char and top line of char in next text line
int PARAM_xPaperMargin   =    52;  // left and right margin on printed paper
int PARAM_ink0           =   255;  // ammount of ink on top of char (0..255, 0=no ink, 255=full black)
int PARAM_ink1           =    50;  // ammount of ink on bottom top of char 

// draw paper background on paper from y0 to y1-1
void lpt_set_paper_background(int paper_y0, int paper_y1)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int x,y,bg_x,bg_y;
    
    surface0=GetControlSurface(IBM701.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM701.PaperBackground, 0, &bg_ww, &bg_hh); // paper backgtound should have same width that paper control

    // pad background on paper, taking into account offset
    for (y=paper_y0; y<paper_y1; y++) {
        bg_y = (y + hPaperBackgroundOffset) % bg_hh; 
        for (x=0; x<paper_ww; x++) {
            bg_x = x % bg_ww;
            surface0[x + y * paper_ww] = surface1[bg_x + bg_y * bg_ww];
        }
    }
}

void lpt_print_line(int y0, char * sLin)
{
    static char sCharSet[50] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ,.()=*+-/$";
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // character to be printed on paper
    int paper_ww, paper_hh; // printed paper control size
    int char_ww, char_hh;   // character to be printed size
    int x,y, x0, nChar, i,ic, n, ink, p, r, g, b, ink0, ink1;
    char c; 

    surface0=GetControlSurface(IBM701.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM701.PrinterCharSet, 0, &char_ww, &char_hh); 

    nChar = 0;
    x0    = PARAM_xPaperMargin;
    while(1) {
        c = sLin[nChar++];
        if (c < ' ') break;         // end of string to print
        // locate the code (the state on CharSet control) correcponfing to char to print
        c = sim_toupper(c);
        ic = -1; 
        for (i=0;sCharSet[i];i++) if (c==sCharSet[i]) {ic = i; break;};
        if (ic>=0) {
            // char printable
            for (y=0;y<char_hh-1;y++) {
                // calc ink color. PARAM_ink0=intensity of ink (0..100) at top of char
                //                 PARAM_ink1=intensity of ink (0..100) at bottom of char
                ink0 = PARAM_ink0;
                ink1 = PARAM_ink1;
                ink = ink0 + ((ink1 - ink0) * y) / (char_hh-1);
                ink = 256-ink;
                for(x=1;x<char_ww;x++) {
                    p = x + y * char_ww + ic * char_ww * char_hh; 
                    get_surface_rgb_color(surface1[p], &r, &g, &b);
                    n = (r + g + b) / 3;  // n=256 -> use bg color, n=0-> ink color
                    if (x0+x >= paper_ww - PARAM_xPaperMargin) break; // check if outside paper margins
                    p = x0 + x + (y0 + y) * paper_ww;
                    get_surface_rgb_color(surface0[p], &r, &g, &b);
                    // apply ink to paper, ink=255->dark, ink=128->half dark, ink=0-> no ink
                    if (ink < r) r=r + (ink-r) * (256-n) / 256;  
                    if (ink < g) g=g + (ink-g) * (256-n) / 256; 
                    if (ink < b) b=b + (ink-b) * (256-n) / 256; 
                    surface0[p]=surface_rgb_color(r,g,b);
                }
            }
        }
        // advance to next char position in line
        x0 += PARAM_char_ww2; 
        if (x0 >= paper_ww - PARAM_xPaperMargin) break; // check if outside paper margins
    }
}


void Refresh_PrintOut(void)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int y, nLin, nLinMax, iLin, n;
    char sLin[LPT_COLUMNS+1];


    SetState(IBM701.LI_Ready_Printer, (lp_unit[0].flags & UNIT_DIS) ? 0:1); // Ready light on -> device under control of calculator
    SetState(IBM701.LI_Select_Printer, ((IOADDR & 4095) == 512) ? 1:0); // Select light on -> calculator gives a write

    if (lptPrintOutCount == lptPrintOutDoneCount)  return; // nothing to update

    surface0=GetControlSurface(IBM701.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM701.PaperBackground, 0, &bg_ww, &bg_hh); // paper background should have same width that paper control

    nLin = lptPrintOutCount - lptPrintOutDoneCount, // nLin = number of char lines to print
    nLinMax = paper_hh  / PARAM_char_hh2;           // calc how much char lines fits in printed page visible

    if (lptPrintOutDoneCount < 0) {
        // first page setup. copy background image
        hPaperBackgroundOffset = 6; 
        lpt_set_paper_background(0, nLinMax * PARAM_char_hh2);
        lptPrintOutDoneCount=0;
    }
    if (nLin<0) {
        // nLin < 0 when lptPrintOutCount has been set to zero -> new printing 
        hPaperBackgroundOffset=6; 
    } else {
        hPaperBackgroundOffset = (hPaperBackgroundOffset + nLin * PARAM_char_hh2) % bg_hh;
        if (hPaperBackgroundOffset<0) hPaperBackgroundOffset += bg_hh; 
    }

    if ((nLin >= nLinMax) || (nLin <= 0)) {
        nLin = nLinMax; 
    } else {
        // scroll up paper image
        y = nLin * PARAM_char_hh2; // first image line of char line that goes to the top of screen
        memcpy(&surface0[0], 
               &surface0[y * paper_ww], 
               sizeof(*surface0) * paper_ww * (paper_hh - y));
    }

    if (lptPrintOutCount <= 0) {
        iLin = 0;
    } else {
        iLin = (lptPrintOutCount - 1) % lptPrintOutMAX; // last line printed, the one at bottom of paper
    }
    // draw nLin lines, starting at bottom of paper and goindg upwards
    // chars to print comes from lptPrintOut[iLin]
    n = nLinMax-1;
    while (nLin-- > 0) {
        memcpy(sLin, &lptPrintOut[iLin * LPT_COLUMNS], LPT_COLUMNS); sLin[LPT_COLUMNS] = 0;
        iLin = (iLin-1) % lptPrintOutMAX;
        if (iLin<0) iLin += lptPrintOutMAX; 
        lpt_set_paper_background(n*PARAM_char_hh2, (n+1)*PARAM_char_hh2);
        lpt_print_line(n*PARAM_char_hh2, sLin);
        n--;
    }

    lptPrintOutDoneCount=lptPrintOutCount;
    cpanel_ControlRedrawNeeded = 1;
    SetState(IBM701.Paper, 0);
}


// animate control CId cycling states 0..LastStae during msec (start timestamp is Tm0)
// set 1 in has_changed if CId state has changed
// set 1 in AnimationFinished if animation has finished
void AnimateCard(int tm0, int CId, int FirstState, int LastState, int msec, int * has_changed, int * AnimationFinished)
{
    int n; 
    *has_changed = *AnimationFinished = 0;

    if (tm0==0) {
        // no read card in entering read stack
        SetState(CId, 0); // set initial state (no card going out)
        *has_changed = cpanel_State_has_changed;
    } else {
        n=(int) GetState(CId);
        if (n==0) {
            // start animation sequence
            SetState(CId, FirstState);
            *has_changed = cpanel_State_has_changed;
        } else if (n>=LastState) {
            // end sequence
            SetState(CId, 0); // set initial state (no card going out)
            *has_changed = cpanel_State_has_changed;
            *AnimationFinished = 1;
        } else {
            // sequence go on
            n = (int) (sim_os_msec()-tm0); // number of msec elapsed from start of animation
            if ((n<0) || (n>10000)) n=10000;
            n = (LastState * n) / msec;  
            if (n<1) {n=1;} else if (n>LastState) n=LastState;
            SetState(CId, n);
            *has_changed = cpanel_State_has_changed;
        }
    }
}

void Refresh_CardRead(void)
{
    int has_changed, AnimationFinished, n, h;

    SetState(IBM701.LI_Ready_CardReader, ((cdr_unit[0].flags & UNIT_DIS) || ((cdr_unit[0].flags & UNIT_ATT) == 0)) ? 0:1); // Ready light on -> device under control of calculator
    SetState(IBM701.LI_Select_CardReader, ((IOADDR & 4095) == 2048) ? 1:0); // Select light on -> calculator gives a read 
    SetState (IBM701.LI_FeedStop_CardReader, bCardReadError);

    // animation sequence: read card going out to already-read-card-deck on card read take stacker 
    // 13 states animation
    // duration of card animation: 200 msec this value is a guess; used because it looks good
    AnimateCard(tm0CardInReadStacker, IBM701.ReadStacker, 2,13, 200, &has_changed, &AnimationFinished);
    if (AnimationFinished) {
        tm0CardInReadStacker=0;          // signal end of animation
        nCardInReadStacker++;            // one more card on take stacker
    }
    // put already-read deck in front to animated card if needed (because frame has_changed)
    if (nCardInReadStacker==0) {
        if (has_changed) cpanel_ControlRedrawNeeded=1;
        SetState(IBM701.ReadDeck, 0);
    } else {
        n = (nCardInReadStacker / 10) + 1;
        if (n>5) n=5;
        if (has_changed) cpanel_ControlRedrawNeeded=1;
        SetState(IBM701.ReadDeck, n);
    }

    // now generate the dynamic states
    // dynamic state image: read hopper + input cards to read deck

    // Dynamically generate State 1 for ReadHopper, with this sequence
    //  - calculate heigh of input card deck (h=40 -> zero cards in input deck, h=113 -> input deck full)
    //    deck is full if it has 70 cards (or the number of cards initially attached if it was > 70)
    if (nCardInReadHopperMax==0) {
        h=1;                                                             // no input cards in read hopper
    } else {
        n = (nCardInReadHopperMax < 70) ? 70 : nCardInReadHopperMax;     // number of cards when input hopper is full
        h = ((113 - 40) * nCardInReadHopper) / n + 40;                   // visible height for Input Deck image
    }
    // - h var (height of input deck) act as state of dynamic control
    if (hInputDeck == h) {
        // input deck is already at calculated height. No need to generate a new dynamic state image
    } else {

        //  - Copy State 0 (background) to state 1 (state displayed)
        CopyControlImage(IBM701.ReadHopper, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM701.ReadHopper, 1,       0, 0);       // ToCId, ToState,     x1, y1
        //  - Copy ReadHopperRear on State 1 -> now Hopper without cards
        CopyControlImage(IBM701.ReadHopperRear, 0,   0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM701.ReadHopper, 1,       0, 76);      // ToCId, ToState,     x1, y1
        //  - Copy InputDeck (height dependes on num of cards) on State 1 -> now hopper with input cards
        CopyControlImage(IBM701.InputDeck, 0,        0, 0, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                         IBM701.ReadHopper, 1,       6, 113-h);   // ToCId, ToState,     x1, y1
        //  - Copy ReadHopperFront on State 1 -> now Hopper with triangular protections
        CopyControlImage(IBM701.ReadHopperFront, 0,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM701.ReadHopper, 1,       58, 76);     // ToCId, ToState,     x1, y1
        //  - set dynamically generated image to be redraw 
        cpanel_ControlRedrawNeeded=1; 
        SetState(IBM701.ReadHopper, 1);
    }
}

void Refresh_CardPunch(void)
{
    int has_changed, AnimationFinished, n, h;

    SetState(IBM701.LI_Ready_CardPunch, ((cdp_unit[0].flags & UNIT_DIS) || ((cdp_unit[0].flags & UNIT_ATT) == 0)) ? 0:1); // Ready light on -> device under control of calculator
    SetState(IBM701.LI_Select_CardPunch, ((IOADDR & 4095) == 1024) ? 1:0); // Select light on -> calculator gives a write
    SetState (IBM701.LI_FeedStop_CardPunch, bCardPunchError);

    // animation sequence: punched card going out to already-punched-card-deck on card punch take stacker 
    // 4 states animation
    // duration of card animation: 120 msec this value is a guess; used because it looks good

    AnimateCard(tm0CardInPunchStacker, IBM701.PunchStacker, 1,4, 120, &has_changed, &AnimationFinished);
    if (AnimationFinished) {
        tm0CardInPunchStacker=0;         // signal end of animation
        nCardInPunchStacker++;           // one more card on take stacker
    }
    // dynamic state image: output cards on punch stacker

    // Dynamically generate State 1 for OutputDeck
    //    State 0 = empty punched card deck, state 2 = full punched card deck
    //    deck is full if it has 200 cards. If more cards punched, the output deck is set to empty again
    // with this sequence
    //  - calculate heigh of output card deck (h=20 -> zero cards in output deck, h=220 -> ouput deck full)
    n = nCardInPunchStacker; while (n > 200) n=n % 200;
    h = ((220 - 20) * n) / 200 + 20;                 // visible height for Input Deck image
    // - h var (heigh of ouput deck) act as state of dynamic control

    if (hOutputDeck == h) {
        // output deck is already at calculated height. No need to generate a new dynamic state image
    } else {

        hOutputDeck = h;
        //  - Copy State 0 (background) to state 1 (state displayed) -> now hopper empty
        CopyControlImage(IBM701.OutputDeck, 0,   0, 0, 0, 0,      // FromCId, FromState, x0, y0, w, h, 
                         IBM701.OutputDeck, 1,   0, 0);           // ToCId, ToState,     x1, y1
        //  - Copy State 2 (OuputDeck height dependes on num of cards) on State 1 -> now hopper with ouput cards
        CopyControlImage(IBM701.OutputDeck, 2,   0, 220-h, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                         IBM701.OutputDeck, 1,   0, 9);           // ToCId, ToState,     x1, y1
        //  - set dynamically generated image to be redraw 
        cpanel_ControlRedrawNeeded=1; 
        SetState(IBM701.OutputDeck, 1);
    }

}


void Refresh_Console(void)
{
    // set register contents in display panel
    IBM701_TickIntensityCount(); 
    SetStateWithIntensity(IBM701.Reg_MR, MR);
    SetStateWithIntensity(IBM701.Reg_ACC, ACC);
    SetStateWithIntensity(IBM701.Reg_MQ, MQ);
    SetStateWithIntensity(IBM701.Reg_IC, IC);
    SetStateWithIntensity(IBM701.Reg_IR, IR);

    if (NCYCLE == 0) { 
        // Interpretation half cycle
        SetStateWithIntensity(IBM701.LI_Instr_Time, 1);
        SetStateWithIntensity(IBM701.LI_Exec_Time, 0);
    } else { 
        // Execution half cycle
        SetStateWithIntensity(IBM701.LI_Instr_Time, 0);
        SetStateWithIntensity(IBM701.LI_Exec_Time, 1);
    }
    SetStateWithIntensity(IBM701.LI_Overflow, OV);

    // lit on while connected to I/O device
    SetStateWithIntensity(IBM701.LI_Input_Output, (IOADDR==0) ? 0:1); 

    SetState(IBM701.LI_Program_Stop, (StopReason == STOP_PROG) ? 1:0);
    SetState(IBM701.LI_Manual, Console_Sw_AutoManual);

    // sense lights 
    SetState(IBM701.Reg_Sense, SENSE_OUT);

    // up/down switches motion
    bSwitchAnimInProgress=0;
    SetUpDwSwitch(IBM701.SW_Sense, &SENSE_IN, -6);
    SetUpDwSwitch(IBM701.SW_IR_Entry, &Console_Sw_IR_Entry, 18);
    SetUpDwSwitch(IBM701.SW_MQ_Entry, &Console_Sw_MQ_Entry, 18);
    SetUpDwSwitch(IBM701.SW_AutoManual, &Console_Sw_AutoManual, 1);
    
    // ready and operating lights
    if (sim_is_running == 0) { 
        SetState(IBM701.LI_Ready, 1);
        SetState(IBM701.LI_Operating, 0);
    } else { 
        SetState(IBM701.LI_Ready, 0);
        SetState(IBM701.LI_Operating, 1);
    }

    // check lights
    SetState(IBM701.LI_Copy_Check, (StopReason == STOP_COPYCHECK) ? 1:0);
    SetState(IBM701.LI_Tape_Check, (StopReason == STOP_TAPECHECK) ? 1:0);
    SetState(IBM701.LI_Div_Check,  ((StopReason == STOP_DIVBYZERO) || (StopReason == STOP_DIVCHECK)) ? 1:0);

    // rotating switch
    if (GetState(IBM701.RSwitch) > 2) {
        // do not set, rotationg motion initiated
    } else {
        SetState(IBM701.RSwitch, Console_Sw_LoadSelector);
    }

    // check if Multiple Steps pressed
    if (GetState(IBM701.BTN_Multiple_Step) == 1) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual == 0)) {
            // Multiple Step while cpu running or console switch to Auto -> ignore
        } else {              
            cpu_unit.flags |= OPTION_STEP_HALFCYCLE; // Set step on half-cycles
            cpanel_interactive=1;  // set interactive mode
            DoSCP("<NOECHO1>"); // to avoid echoing the next scp step command
            DoSCP("step"); 
        }
    }
    
}

void process_HotKeys(void)
{
    if (vid_keyb.KeyPress == 'f'-'a'+1) { // Control-F (^F) is being pressed
        if (CpuSpeed_Acceleration != -1) { 
            // accelerate to max speed max while keydown
            CpuSpeed_Acceleration_save = CpuSpeed_Acceleration;
            CpuSpeed_Acceleration=-1;        // set cpu speed=max & set cpu fast
            Measure_CpuSpeed(0); ShowInfoTm0 = 0; // reset speed measurement because ^F pressed
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F pressed\n");
        }
    } else if (CpuSpeed_Acceleration == -1) {
        // return to previos cpu speed setting
        CpuSpeed_Acceleration = CpuSpeed_Acceleration_save;
        CpuSpeed_Acceleration_save=0; 
        // return to previous fast/realtime setting
        Measure_CpuSpeed(0); ShowInfoTm0 = 0; // reset speed measurement because ^F released
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F released\n");
    }
    if (vid_keyb.LastKeyPress ==  ('i'-'a'+1)) { // Tab/Control-I (^I) has been pressed
        // toggle show Info on GUI 
        vid_keyb.LastKeyPress = 0; // clear key as it is processed
        bShowInfo = (bShowInfo) ? 0:1; // toggle value
        if (bShowInfo==0) {
            // put again background thus removing info from gui
            SetState(IBM701.CtrlInfoPanel, 0); 
            if (bTapesVisible) SetState(IBM701.MT_InfoPanel, 0); 
        } else {
            // init IPS measurement
            ShowInfoTm0 = 0;
        }
    }
}


// draw text on given Control Id infopanel 
void ShowInfo_DrawText(int CId, char * buf)
{
    int scale, ww,hh,ww2,hh2, chrsz;
    int ncp; 

    GetControlSurface(CId, 1, &ww, &hh);
    ncp=GetControlInfo(CId, CINFO_NCP);  // control panel window number where info is displayed
    scale=cpanel_scale(ncp,0);           // get scale of control panel window 
    if (scale >= 100) {
        ww2 = ww/2; hh2=hh/2; chrsz=1; // use half panel
    } else {
        ww2=ww; hh2=hh; chrsz=2; // use full panel, char size x2
    }

    if (ww2<ww) {
        //  - Copy State 0 (image background) to state 1 (state displayed), erasing black box where text will be placed
        CopyControlImage(CId, 0, 0, 0, 0, 0,      // FromCId, FromState, x0, y0, w, h, 
                         CId, 1,       0, 0);     // ToCId, ToState,     x1, y1
    }
    //  - Copy State 2 (back background) to state 1 (state displayed), creating black box where text will be placed
    CopyControlImage(CId, 2,     0, 0, ww2, hh2,  // FromCId, FromState, x0, y0, w, h, 
                     CId, 1,               0, 0); // ToCId, ToState,     x1, y1

    //  - Draw text on State 1
    DrawTextImage   (CId, 1,   2,1, ww2, hh2,     // ToCId, ToState, x0, y0, w0, h0: where to draw text
                               255,255,255,       // r, g, b: text color
                               buf, chrsz);       // str to draw, char size
    //  - signal dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId, 1);      
}

void Refresh_ShowInfo(int bOnlyInit) 
{
    char buf[300];
    int TickCountPerSec; 
    int InstrExecPerSec, n, fps; 
    int ips_unit; double ips; 
    uint32 msec; 

    // set dynamic contents of CtrlInfoPanel and MT_InfoPanel

    if (bShowInfo>1) {
        TickCountPerSec =  InstrExecPerSec = fps = 0;
    } else {
        TickCountPerSec =  Measure_CpuSpeed(1); 
        // limit speed display to 83333 (clock 83 KHz)
        if ((TickCountPerSec < 0) || (TickCountPerSec > 83333 * 1000)) TickCountPerSec = 83333 * 1000;

        InstrExecPerSec =  fps = 0;
        msec = Refresh_tnow - ShowInfoTm0; 
        if ((msec > 0) && (msec < 1000)) {
            n = Measure_CpuSpeed(2)- InstrExec0;
            if (n<0) n=0;
            InstrExecPerSec = n * 1000 / msec;
            n = Refresh_Frames_Count  - FramesCount0;
            fps             = n * 1000 / msec;
            if (fps>60) fps=60;
            if (fps<0) fps=0;
        }
        ShowInfoTm0  = Refresh_tnow;
        InstrExec0   = Measure_CpuSpeed(2); 
        FramesCount0 = Refresh_Frames_Count ; 
    }
    if (bOnlyInit) return; // only init values for Next call 

    if (InstrExecPerSec > 500 * 1000) {
        ips = InstrExecPerSec / 1000000.0; ips_unit = 2; // unit MIPS if ips > 0.5 MIPS
    } else if (InstrExecPerSec >1000) {
        ips = InstrExecPerSec / 1000.0; ips_unit = 1;    // unit KIPS if ips > 1.0 KPIS
    } else {
        ips = InstrExecPerSec; ips_unit = 0; 
    }

    // Info show:
    // if cpu is executing code: 
    //    FPS: nnn CPU SPEED: xn.nn (nnnn IPS)
    //       FPS = GUI Window refresh rate (FPS = Frames Per second)
    //       CPU SPEED = x1.0 -> real hardware speed, x2.0 -> twice as fast as real hw
    //       IPS = Instructions Per Second -> Instr executed each sec
    // if cpu is waiting for i/o device,
    //    OPOP waiting nnn msec for DEV
    //       IOOP is the I/O operation, can be READ, WRITE, REWIND, COPY
    //       DEV can be MT0 to MT3, CDR, CDP, LP, DR0 to DR3
    //       nnn = simulated time (milliseconds) remaining for i/o device to comunicate with cpu 
    // if cpu is stalled (cpu not running)
    //    FPS: nnn STALL: aaaaa 
    //       cpu is waiting, aaaaa is the stall reason:
    //          MTn UNIT NOT READY       Tape instr stalled because tape unit not ready

    sprintf(buf, "FPS: %d, Cpu Speed: x%0.2f (%0.1f %s)\n"
                 "Cards in read hopper: %d", 
            fps, 
            TickCountPerSec * 12 / 1000000.0,  
            ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS",
            nCardInReadHopper);
  
    if (nShowInfoStall) {
        if ((nShowInfoStall >= 10) && (nShowInfoStall <= 13)) {
            sprintf(&buf[strlen(buf)], ", STALL: MT%d UNIT NOT READY", nShowInfoStall-10);
        } else {
            sprintf(&buf[strlen(buf)], ", STALL: ??? (%d)", nShowInfoStall);
        }
    } else if (nShowInfoCpuTick > 100) {
        extern void GetIOADDR(int * ioaddr, int * ioop); 
        int ioaddr, ioop; 
        char ns[3];

        GetIOADDR(&ioaddr, &ioop); 
        if (ioaddr > 256) {ns[0]=0;} else {ns[0] = (ioaddr & 3) + '0'; ns[1]=0;}
        ioaddr = ioaddr & 0xFFF0; 
        ioop = (IR >> 12) & 31; 

        if (ioaddr) {
            sprintf(&buf[strlen(buf)], ", %s waiting %d msec for %s%s", 
                 (ioop == OP_COPY) ? "COPY" : 
                 ((ioop == OP_READ) || (ioop == OP_READ_B)) ? "READ" : 
                 ((ioop == OP_WRITE) || (ioop == OP_WRITE_EF)) ? "WRITE" : 
                 (ioop == OP_REWIND) ? "REWIND" : "???", 
                 msec_elapsed(0, nShowInfoCpuTick), 
                 (ioaddr == 2048) ? "CDR" : (ioaddr == 1024) ? "CDP" : (ioaddr == 512) ? "LP" : 
                 (ioaddr == 256)  ? "MT"  : (ioaddr == 128) ? "DR" : "???", ns);
        }
    }

    ShowInfo_DrawText(IBM701.CtrlInfoPanel, buf);

    if (bTapesVisible) {
        // set tape info panel
        double p_MT_used[4]; char c_MT[4];
        int i, n;
        char c; 
        for(i=0;i<4;i++) { 
            c = ' ';
            if (((mt_unit[i].flags & UNIT_DIS) || ((mt_unit[i].flags & UNIT_ATT) == 0)) &&
                 (mtcab[i].mt_is != MT_is_unloading_tape)) {
                n = 0;
            } else if ((mtcab[i].mt_is == MT_is_rewinding) || 
                       ((mtcab[i].mt_is == MT_is_loading_tape) && (mtcab[i].rew_u3 > 0)) ||
                       ((mtcab[i].mt_is == MT_is_unloading_tape) && (mtcab[i].rew_u3 > 0))  ) {
                // if rewinding then the ammount of tape medium used is in variable recsize, not in u3
                // if loading tape AND rew_u3 > 0 then the tape is showing a nice rew animation started with R
                // if rewinding the ammount of tape medium used is in variable recsize, not in u3
                n = mtcab[i].rew_u3; 
                c = 'R';
            } else {
                // use regular value from mt_into to get the amount of medium used 
                n = mt_unit[i].u3; // amount of reel used in inches x1000
            } 
            p_MT_used[i] = n / (mt_unit[i].u4*10.0); 
            c_MT[i] = c; 
        }

        sprintf(buf, "            MT0     MT1     MT2     MT3 \n"
                     " record   %5d   %5d   %5d   %5d \n"
                     " %% unwind   %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c",
            mt_info[0].numrec, mt_info[1].numrec, mt_info[2].numrec, mt_info[3].numrec, 
            p_MT_used[0], c_MT[0], p_MT_used[1], c_MT[1], p_MT_used[2], c_MT[2], p_MT_used[3], c_MT[3]
        );
        ShowInfo_DrawText(IBM701.MT_InfoPanel, buf);

    }

}

void IBM701_Refresh(void)
{
    // refresh console
    Refresh_Console();

    // IBM 711
    if (bCardReadVisible) {
        Refresh_CardRead(); 
    }
    // IBM 721
    if (bCardPunchVisible) {
        Refresh_CardPunch(); 
    }
    // IBM 716
    if (bPrintOutVisible) {
        Refresh_PrintOut();
    }

    // tapes
    if (bTapesVisible) {
        Refresh_MagTape();
    }

    // show info
    if (  ((bShowInfo) && ((Refresh_Frames_Count % 8)==0)) || (bShowInfo==2)  ) {
        // show info for ^I (Ctrl I) at 60 FPS / 8 -> 8 FPS
        // if (bShowInfo==2) ips will be shown each frame
        Refresh_ShowInfo(0);
    }
    // process HotKeys on Control Panel GUI window (Hot Keys)
    process_HotKeys();
    // Must be called after Refresh_ShowInfo. when ^F is pressed it resets speed measurement vars
    // so if Refresh_ShowInfo is called inmedatelly after, a no tickcount are yet executed will display a speed of zero 
    if ((bShowInfo==1) && (ShowInfoTm0==0)) Refresh_ShowInfo(1);
}


// drag and drop a file handling. Called when a file of given filename is droped on given CId control 
void IBM701_DropFile(int CId, char * FileName)
{
    extern t_addr   sim_card_input_hopper_count(UNIT *uptr);
    extern t_stat   cdr_attach(UNIT *, CONST char *);
    extern t_stat   mt_attach(UNIT * uptr, CONST char *file);
    int32  sv; 
    int n; 

    if (CId ==IBM701.Drop_InputDeckFile) {
        // drag and drop a file on card reader -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        // if already cards in reader, stack the new ones on the top
        if (sim_card_input_hopper_count(&cdr_unit[0]) > 0) sim_switches |= SWMASK ('S'); 
        cdr_attach(&cdr_unit[0], FileName);
        sim_switches=sv; 
    } else for (n=0;n<4;n++) if (CId == IBM701.Drop_MT_File[n]) {
        // drag and drop a file on tape -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        mt_attach(&mt_unit[n], FileName);
        sim_switches=sv; 
    }
}

// buttons for tape detach
void IBM701_OnClick_BTN2(void)
{
    int n; 
    int32  sv; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        return;
    }
    // set button to unpressed state (except Power Button, that remains pressed)
    SetState(CP_Click.CId, 0);

    // to detach a tape, must click on BackWards button and inmediatelly after on unload button
    for (n=0;n<4;n++) {
        if (CP_Click.CId == IBM701.MT_BTN_Backward[n]) {
            // Save last BackWard tape button
            bTapeLastButtonPressedCId=CP_Click.CId; 
            return; 
        } else if (CP_Click.CId == IBM701.MT_BTN_UnLoad[n]) {
            if (bTapeLastButtonPressedCId != IBM701.MT_BTN_Backward[n]) {
                // last button pressed is not backward on this tape unit
                // do not detach
                bTapeLastButtonPressedCId=0; 
                return; 
            } 
            if ((mt_unit[n].flags & UNIT_ATT) == 0) {
                // already detached. Do not detach again
                return; 
            }
            // click to detach tape
            sv=sim_switches; // save current switches
            sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
            mt_detach(&mt_unit[n]);
            sim_switches=sv; 
            bTapeLastButtonPressedCId=0; 
        }
    }
    // clear last tape button pressed
}

// handle Rotating Switch 
void IBM701_OnClick_Sw2(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press button -> start switch rotation motion
        n = (int) (GetState(IBM701.RSwitch));
        n = n % 3;
        SetState(IBM701.RSwitch, n+3);
        return;
    }
    // set new position
    n = (int) (GetState(IBM701.RSwitch));
    n = n % 3 + 1;
    if (n > 2) n = 0;
    Console_Sw_LoadSelector = n;  
    SetState(IBM701.RSwitch, Console_Sw_LoadSelector);
}


// handle Up/Down switch
void IBM701_OnClick_Sw(void)
{
    if (CP_Click.KeyPress_KeyRelease == 1) {
        if ((CP_Click.CId == IBM701.SW_AutoManual[1]) && (GetState(CP_Click.CId) == 0)) {
            // moving Auto man switch from auto to manual
            if (sim_is_running) { 
                // request a cpu stop, without disabling interactive mode
                cpanel_stop_flag = 2;
                // and set interactive mode
                cpanel_interactive=1; 
            }
        }
        // press mouse button -> start switch up/down movement 
        if (GetState(CP_Click.CId) == 1) {
            // non zero state -> switch is down -> start direction up motion -> state 2->4
            SetState(CP_Click.CId, 2);
        } else if (GetState(CP_Click.CId) == 0) {
            // zero state -> switch is up -> start direction down motion -> state 5->7
            SetState(CP_Click.CId, 5);
        }
        return;
    } 

}

// Handle Button
void IBM701_OnClick_BTN(void)
{
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        bTapeLastButtonPressedCId=0; // clear las button pressed on tape value, because this event handler routine is only for non-tape buttons
        return;
    }
    // set button to unpressed state (except Power Button, that remains pressed)
    if (CP_Click.CId != IBM701.BTN_Power_Off) {
        SetState(CP_Click.CId, 0);
    }

    if (CP_Click.CId == IBM701.BTN_Power_Off) {
        cpanel_stop_flag = 4; // request cpanel off and quit
        return;
    } else if (CP_Click.CId == IBM701.BTN_Reset) {
        if (sim_is_running == 0) cpu_reset(&cpu_dev); // reset while cpu is running is ignored
        return;
    } else if (CP_Click.CId == IBM701.BTN_Reset_clr_mem) {
        if (sim_is_running == 0) cpu_clear_crt_mem(&cpu_unit, 0, NULL, NULL); // reset while cpu is running is ignored
        return;
    } else if (CP_Click.CId == IBM701.BTN_Start) {
        if (sim_is_running != 0) {
            // Start while cpu running -> ignore
        } else if (Console_Sw_AutoManual != 0) {  // console switch 0=Automatic/1=Manual
            // Start while mode is manual -> ignore
        } else {
            // run the program 
            // disable interactive mode so when 
            // terminating prog the script can continue
            cpanel_interactive=0; 
            StopReason=0;
            // send command to scp
            DoSCP("go"); 
        }
    } else if (CP_Click.CId == IBM701.BTN_Load) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual != 0)) {
            // Load while cpu running or console switch to Manual-> ignore
        } else {  
            cpanel_interactive=0; 
            // send command to scp
            DoSCP("press \"load\""); 
        }
    } else if (CP_Click.CId == IBM701.BTN_Mem_Display) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual == 0)) {
            // Enter Instr while cpu running or console switch to Auto -> ignore
        } else {  
            int addr; 
            t_int64 d; 
            addr=Console_Sw_IR_Entry & 07776; 
            // if (addr==0) addr=-4096 -> to read a full word from addr 0 (as it was addr -0)
            // read addr in MR as full word
            ReadAddr((addr==0) ? -4096:-addr, &d, NULL);
        }
    } else if (CP_Click.CId == IBM701.BTN_Enter_MQ) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual == 0)) {
            // Enter MQ while cpu running or console switch to Auto -> ignore
        } else {  
            MQ = ((t_int64) Console_Sw_MQ_Entry) << 18; 
        }
    } else if (CP_Click.CId == IBM701.BTN_Enter_Instr) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual == 0)) {
            // Enter Instr while cpu running or console switch to Auto -> ignore
        } else {  
            IR = Console_Sw_IR_Entry; 
            cpanel_interactive=1;  // set interactive mode
            NCYCLE=1; // exec instr present in IR register
            // send command to scp
            DoSCP("step"); 
        }
    } else if (CP_Click.CId == IBM701.BTN_Half_Step) {
        if ((sim_is_running != 0) || (Console_Sw_AutoManual == 0)) {
            // Half Step while cpu running or console switch to Auto -> ignore
        } else {              
            cpu_unit.flags |= OPTION_STEP_HALFCYCLE; // Set step on half-cycles
            cpanel_interactive=1;  // set interactive mode
            DoSCP("step"); 
        }
    } 
}

// Matches the ifdef(CPANEL) at beginning of file
#endif

