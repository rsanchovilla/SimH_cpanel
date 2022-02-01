/* cpanel.c: simulator control panel simulation

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

   Except as contained in this notice, the name of Robert M Supnik shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from Robert M Supnik.

   May-20    RSV     IBM 650 control panel support

   This module implements the following control panels:

   IBM 650 CPU control panel (1954 & 1955 version)
   IBM 533 Card Read Punch
   IBM 727 Magnetic Tape
   IBM 355 RAMAC
   IBM 652 Control Unit
   IBM 653 Storage Unit
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "i650_defs.h"	        
#include "sim_tape.h"
#include <math.h>

// cpu state
extern t_int64 ACC[2];                      /* lower, upper accumulator. 10 digits (=one word) each*/
extern t_int64 DIST;                        /* ditributor. 10 digits */
extern t_int64 CSW;                         /* Console Switches, 10 digits */
extern t_int64 PR;                          /* Program Register: hold current instr in execution, 10 digits*/
extern uint16  AR;                          /* Address Register: address references to drum */
extern uint8   OV;                          /* Overflow flag */
extern char    IMachineCycle;               // indicates current machine cycle to be executed: 'D' for D-Cycle, 'I' for I-Cycle
extern uint8   StopReason;                  // saved stop reason to be displayed in control panel
extern uint8   StopIOError;                 // flag to signal device that caused STOP_IO error: 1=CDR/CDP, 2=TAPE, 3=RAMAC Address, 4=RAMAC IO
extern int     CSWProgStop;                 /* Console programmed stop switch */
extern int     CSWOverflowStop;             /* Console stop on overflow switch */
extern int     CSWHalfCycle;                // Console run/half cycle set to 0 for normal run, =1 to execute 1 half-cycle and stop
extern int     CSWAddrStop;                 // Console Switch with address stop selected (-1 if none)
extern int     AccNegativeZeroFlag;         // set to 1 if acc has a negative zero
extern int     DistNegativeZeroFlag;        // set to 1 if distributor has a negative zero
extern int     InterLockCount[8];           // interlocks
extern int     CpuSpeed_Acceleration;       // cpu speed multiplier
extern int     Measure_CpuSpeed(int);       // return measured cpu speed 

// printout state
extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
extern int    lptPrintOutCount;

// card reader state
extern int nCardInReadHopperMax; 
extern int nCardInReadHopper;
extern int nCardInReadStacker;
extern uint32 tm0CardInReadStacker;

// card punch state
extern int nCardInPunchStacker;
extern uint32 tm0CardInPunchStacker;

// disk state
extern armrec Arm[4][3];

// tape state
extern UNIT mt_unit[6];
extern mtinforec mt_info[6];

CP_DEF IBM650_cp[];			// declare forwards
void IBM650_Init(void);
void IBM650_Reset(void);
void IBM650_TickIntensityCount(void);
void IBM650_Refresh(void);
void IBM650_DropFile(int CId, char * FileName);
void IBM650_OnClick_Sw(void);
void IBM650_OnClick_BTN(void);
void IBM650_OnClick_BTN2(void);

// cpanel types constants
#define IS_IBM650	1	// define CP type contants. must start on 1

// control panel available types
CP_TYPE cp_types = {
    IBM650_cp, 
    &IBM650_Init, NULL, &IBM650_Reset, 
    &IBM650_Refresh, &IBM650_TickIntensityCount, &IBM650_DropFile
};

// struct to hold the GUI controls Ids
static struct {
   int CtrlInfoPanel;
   int Reg_DISP_0T9, Reg_DISP_N;
   int Reg_OP_0T9, Reg_OP_N;
   int Reg_AR_0T9, Reg_AR_N;
   int OperatingPanel, Reg_OPER, Reg_CHK;
   int DispSw[11], DispSwSgn, DispSwNum[11]; // 11 because controls used are [1] to [10] 
   int ProgSw, HalfCycleSw;
   int AddrSw[5], AddrSwNum[5]; // 5 because controls used are [1] to [4] 
   int CtrlSw, DisplaySw, OverflowSw, ErrorSw;
   int BTN_Power;
   int BTN_Transfer, BTN_ProgStart, BTN_ProgStop; 
   int BTN_ProgReset, BTN_ComputerReset, BTN_AccumReset; 
   int BTN_ErrorReset, BTN_ErrorSenseReset, BTN_Switch_Power;
   // IBM 653
   int Reg_IAS_OPER;
   int Reg_IAS_BLOCK_N, Reg_IAS_WORD_N;
   // IBM 652
   int Reg_MT_SEL_N, Reg_MT_OPER;
   int Reg_INQUIRY;
   int Reg_DS_UNIT_N, Reg_DS_DISK_N, Reg_DS_TRACK_N, Reg_DS_ARM_N;
   int Reg_DS_OPER;
   int DsUnitSw[2], DsUnitSwNum[2], DsArmSw[2], DsArmSwNum[2]; // 2 because controls used are [1]
   // IBM 407 print out
   int Paper, PaperBackground, PrinterCharSet;
   // IBM 533 card Read Punch
   int ReadHopper, ReadHopperRear, ReadHopperFront, InputDeck; 
   int ReadStacker, ReadDeck, PunchStacker, OutputDeck;
   int Drop_InputDeckFile; 
   // IBM 355 RAMAC
   int ArmRunArea, Arm, ArmWires, ArmWireRunArea;
   int VerticalBarGuide1, VerticalBarGuide2, DiskStackMoire, ArmShadow, ArmWiresShadow, MovingHeadBarShadow;
   // IBM 727 Magnetic Tape
   int MT_InfoPanel; 
   int MT[6], MT_num[6], MT_lights[6], MT_head[6];
   int MT_L[6], MT_R[6], MT_L_VacCol[6], MT_R_VacCol[6];
   int MT_VacColumn, MT_VacColMedium; 
   int MT_BTN_UnLoad[6], Drop_MT_File[6];
} IBM650 = {0}; // must be init to zero

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF IBM650_cp[] = {
    { &IBM650.CtrlInfoPanel,          "CtrlInfoPanel",                     NULL},
    { &IBM650.Reg_DISP_0T9,           "Reg_DISP_0T9",                      NULL},
    { &IBM650.Reg_DISP_N,             "Reg_DISP_N",                        NULL},
    { &IBM650.Reg_OP_0T9,             "Reg_OP_0T9",                        NULL},
    { &IBM650.Reg_OP_N,               "Reg_OP_N",                          NULL},
    { &IBM650.Reg_AR_0T9,             "Reg_AR_0T9",                        NULL},
    { &IBM650.Reg_AR_N,               "Reg_AR_N",                          NULL},
    { &IBM650.OperatingPanel,         "OperatingPanel",                    NULL},
    { &IBM650.Reg_OPER,               "Reg_OPER",                          NULL},
    { &IBM650.Reg_CHK,                "Reg_CHK",                           NULL},
    { &IBM650.DispSw[10],             "DispSW_10",                         &IBM650_OnClick_Sw},
    { &IBM650.DispSw[9],              "DispSW_9",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[8],              "DispSW_8",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[7],              "DispSW_7",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[6],              "DispSW_6",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[5],              "DispSW_5",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[4],              "DispSW_4",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[3],              "DispSW_3",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[2],              "DispSW_2",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSw[1],              "DispSW_1",                          &IBM650_OnClick_Sw},
    { &IBM650.DispSwSgn,              "DispSW_SGN",                        &IBM650_OnClick_Sw},
    { &IBM650.DispSwNum[10],          "DispSwNum_10",                      NULL},
    { &IBM650.DispSwNum[9],           "DispSwNum_9",                       NULL},
    { &IBM650.DispSwNum[8],           "DispSwNum_8",                       NULL},
    { &IBM650.DispSwNum[7],           "DispSwNum_7",                       NULL},
    { &IBM650.DispSwNum[6],           "DispSwNum_6",                       NULL},
    { &IBM650.DispSwNum[5],           "DispSwNum_5",                       NULL},
    { &IBM650.DispSwNum[4],           "DispSwNum_4",                       NULL},
    { &IBM650.DispSwNum[3],           "DispSwNum_3",                       NULL},
    { &IBM650.DispSwNum[2],           "DispSwNum_2",                       NULL},
    { &IBM650.DispSwNum[1],           "DispSwNum_1",                       NULL},
    { &IBM650.ProgSw,                 "ProgSw",                            &IBM650_OnClick_Sw},
    { &IBM650.HalfCycleSw,            "HalfCycleSw",                       &IBM650_OnClick_Sw},
    { &IBM650.AddrSw[4],              "AddrSw_4",                          &IBM650_OnClick_Sw},
    { &IBM650.AddrSw[3],              "AddrSw_3",                          &IBM650_OnClick_Sw},
    { &IBM650.AddrSw[2],              "AddrSw_2",                          &IBM650_OnClick_Sw},
    { &IBM650.AddrSw[1],              "AddrSw_1",                          &IBM650_OnClick_Sw},
    { &IBM650.AddrSwNum[4],           "AddrSwNum_4",                       NULL},
    { &IBM650.AddrSwNum[3],           "AddrSwNum_3",                       NULL},
    { &IBM650.AddrSwNum[2],           "AddrSwNum_2",                       NULL},
    { &IBM650.AddrSwNum[1],           "AddrSwNum_1",                       NULL},
    { &IBM650.CtrlSw,                 "CtrlSw",                            &IBM650_OnClick_Sw},
    { &IBM650.DisplaySw,              "DisplaySw",                         &IBM650_OnClick_Sw},
    { &IBM650.OverflowSw,             "OverflowSw",                        &IBM650_OnClick_Sw},
    { &IBM650.ErrorSw,                "ErrorSw",                           &IBM650_OnClick_Sw},
    { &IBM650.BTN_Power,              "BTN_Power",                         &IBM650_OnClick_BTN},
    { &IBM650.BTN_Transfer,           "BTN_Transfer",                      &IBM650_OnClick_BTN},
    { &IBM650.BTN_ProgStart,          "BTN_ProgStart",                     &IBM650_OnClick_BTN},
    { &IBM650.BTN_ProgStop,           "BTN_ProgStop",                      &IBM650_OnClick_BTN},
    { &IBM650.BTN_ProgReset,          "BTN_ProgReset",                     &IBM650_OnClick_BTN},
    { &IBM650.BTN_ComputerReset,      "BTN_ComputerReset",                 &IBM650_OnClick_BTN},
    { &IBM650.BTN_AccumReset,         "BTN_AccumReset",                    &IBM650_OnClick_BTN},
    { &IBM650.BTN_ErrorReset,         "BTN_ErrorReset",                    &IBM650_OnClick_BTN},
    { &IBM650.BTN_ErrorSenseReset,    "BTN_ErrorSenseReset",               &IBM650_OnClick_BTN},
    { &IBM650.BTN_Switch_Power,       "BTN_Switch_Power",                  &IBM650_OnClick_BTN},
    // IBM 653
    { &IBM650.Reg_IAS_OPER,           "Reg_IAS_OPER",                      NULL,               "ibm653/1"  },
    { &IBM650.Reg_IAS_BLOCK_N,        "Reg_IAS_BLOCK_N",                   NULL,               "ibm653/1"  },
    { &IBM650.Reg_IAS_WORD_N,         "Reg_IAS_WORD_N",                    NULL,               "ibm653/1"  },
    // IBM 652
    { &IBM650.Reg_MT_SEL_N,           "Reg_MT_SEL_N",                      NULL,               "ibm652/1"  },
    { &IBM650.Reg_MT_OPER,            "Reg_MT_OPER",                       NULL,               "ibm652/1"  },
    { &IBM650.Reg_INQUIRY,            "Reg_INQUIRY",                       NULL,               "ibm652/1"  },
    { &IBM650.Reg_DS_UNIT_N,          "Reg_DS_UNIT_N",                     NULL,               "ibm652/1"  },
    { &IBM650.Reg_DS_DISK_N,          "Reg_DS_DISK_N",                     NULL,               "ibm652/1"  },
    { &IBM650.Reg_DS_TRACK_N,         "Reg_DS_TRACK_N",                    NULL,               "ibm652/1"  },
    { &IBM650.Reg_DS_ARM_N,           "Reg_DS_ARM_N",                      NULL,               "ibm652/1"  },
    { &IBM650.Reg_DS_OPER,            "Reg_DS_OPER",                       NULL,               "ibm652/1"  },
    { &IBM650.DsUnitSw[1],            "DsUnitSw",                          &IBM650_OnClick_Sw, "ibm652/1"  },
    { &IBM650.DsUnitSwNum[1],         "DsUnitSwNum",                       NULL,               "ibm652/1"  },
    { &IBM650.DsArmSw[1],             "DsArmSw",                           &IBM650_OnClick_Sw, "ibm652/1"  },
    { &IBM650.DsArmSwNum[1],          "DsArmSwNum",                        NULL,               "ibm652/1"  },
    // IBM 407 Print out
    { &IBM650.Paper,                  "Paper",                             NULL,               "ibm407/1"  },
    { &IBM650.PaperBackground,        "PaperBackground",                   NULL,               "ibm407/1"  },
    { &IBM650.PrinterCharSet,         "PrinterCharSet",                    NULL,               "ibm407/1"  },
    // IBM 533 Card Read Punch
    { &IBM650.InputDeck,              "InputDeck",                         NULL,               "ibm533/1"  },
    { &IBM650.ReadHopper,             "ReadHopper",                        NULL,               "ibm533/1"  },
    { &IBM650.ReadHopperRear,         "ReadHopperRear",                    NULL,               "ibm533/1"  },
    { &IBM650.ReadHopperFront,        "ReadHopperFront",                   NULL,               "ibm533/1"  },
    { &IBM650.ReadDeck,               "ReadDeck",                          NULL,               "ibm533/1"  },
    { &IBM650.ReadStacker,            "ReadStacker",                       NULL,               "ibm533/1"  },
    { &IBM650.PunchStacker,           "PunchStacker",                      NULL,               "ibm533/1"  },
    { &IBM650.OutputDeck,             "OutputDeck",                        NULL,               "ibm533/1"  },
    { &IBM650.Drop_InputDeckFile,     "Drop_InputDeckFile",                NULL,               "ibm533/1"  },
    // IBM 355 RAMAC
    { &IBM650.ArmRunArea,             "ArmRunArea",                        NULL,               "ibm355/1"  },
    { &IBM650.Arm,                    "Arm",                               NULL,               "ibm355/1"  },
    { &IBM650.ArmWires,               "ArmWires",                          NULL,               "ibm355/1"  },
    { &IBM650.ArmWireRunArea,         "ArmWireRunArea",                    NULL,               "ibm355/1"  },
    { &IBM650.VerticalBarGuide1,      "VerticalBarGuide1",                 NULL,               "ibm355/1"  },
    { &IBM650.VerticalBarGuide2,      "VerticalBarGuide2",                 NULL,               "ibm355/1"  },
    { &IBM650.DiskStackMoire,         "DiskStackMoire",                    NULL,               "ibm355/1"  },
    { &IBM650.ArmShadow,              "ArmShadow",                         NULL,               "ibm355/1"  },
    { &IBM650.ArmWiresShadow,         "ArmWiresShadow",                    NULL,               "ibm355/1"  },
    { &IBM650.MovingHeadBarShadow,    "MovingHeadBarShadow",               NULL,               "ibm355/1"  },    
    // IBM 727
    { &IBM650.MT_InfoPanel,           "MT_InfoPanel",                      NULL,               "ibm727/1"  },
    { &IBM650.MT[0],                  "MT_0",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[0],              "MT_0_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[0],           "MT_0_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[0],                "MT_0_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[0],                "MT_0_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[0],             "MT_0_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[0],           "Drop_MT0_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[0],        "MT_0_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[0],        "MT_0_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[0],      "MT_0_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT[1],                  "MT_1",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[1],              "MT_1_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[1],           "MT_1_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[1],                "MT_1_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[1],                "MT_1_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[1],             "MT_1_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[1],           "Drop_MT1_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[1],        "MT_1_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[1],        "MT_1_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[1],      "MT_1_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT[2],                  "MT_2",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[2],              "MT_2_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[2],           "MT_2_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[2],                "MT_2_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[2],                "MT_2_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[2],             "MT_2_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[2],           "Drop_MT2_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[2],        "MT_2_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[2],        "MT_2_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[2],      "MT_2_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT[3],                  "MT_3",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[3],              "MT_3_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[3],           "MT_3_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[3],                "MT_3_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[3],                "MT_3_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[3],             "MT_3_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[3],           "Drop_MT3_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[3],        "MT_3_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[3],        "MT_3_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[3],      "MT_3_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT[4],                  "MT_4",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[4],              "MT_4_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[4],           "MT_4_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[4],                "MT_4_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[4],                "MT_4_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[4],             "MT_4_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[4],           "Drop_MT4_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[4],        "MT_4_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[4],        "MT_4_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[4],      "MT_4_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT[5],                  "MT_5",                              NULL,               "ibm727/1"  },
    { &IBM650.MT_num[5],              "MT_5_number",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_lights[5],           "MT_5_lights",                       NULL,               "ibm727/1"  },
    { &IBM650.MT_L[5],                "MT_5_L",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_R[5],                "MT_5_R",                            NULL,               "ibm727/1"  },
    { &IBM650.MT_head[5],             "MT_5_head",                         NULL,               "ibm727/1"  },
    { &IBM650.Drop_MT_File[5],           "Drop_MT5_File",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_L_VacCol[5],        "MT_5_L_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_R_VacCol[5],        "MT_5_R_VacCol",                  NULL,               "ibm727/1"  },
        { &IBM650.MT_BTN_UnLoad[5],      "MT_5_BTN_UnLoad",              &IBM650_OnClick_BTN2, "ibm727/1"  },
    { &IBM650.MT_VacColumn,           "MT_VacColumn",                      NULL,               "ibm727/1"  },
    { &IBM650.MT_VacColMedium,        "MT_VacColMedium",                   NULL,               "ibm727/1"  },
    { NULL }
};

int bCardReadVisible;                   // ibm 533 is visible
int bPrintOutVisible;                   // ibm 407 is visible
int bIasVisible, bStorageVisible;       // ibm 653/652 is visible
int bTapesVisible;                      // tape visible
int bRamacVisible;                      // ramac disk visible
int bcpu1955;                           // =1 if cou control panel has 9 lights in opetationg panel 
                                        // instead of 6 lights in basic 650
// control panel rotating switches
t_int64 CSWAddress     = 0;
int CSWControl         = 0; // 0=Run, 1=Manual Operation, 2=Address Stop
int CSWDisplay         = 0; // 0=PR, 1=Read-Out, 2=Read-In, 3=AccLo, 4=AccUp, 5=Distrib
int CSWErrorSw         = 0; // not used 
int CSWneg             = 0; // 0=positive, 1=negative;
t_int64 CSWDisplayUnit = 0;
t_int64 CSWDisplayArm  = 0;

// animation state vars (for dynamic state draw)
// for card reader
int hInputDeck                 = 0; // height of dynamically calculated input card deck
int hOutputDeck                = 0; // height of dynamically calculated output card deck
// for ramac
int nRamacArm, yRamacArm       = 0; // dynamic calculated ramac arm position
int vRamacArm, vvRamacArm      = 0; // dynamic calculated ramac arm vertical motion speed
uint32 Tm0Moire                = 0; // start of ramac disk stack rotation moire effect
// for main cpu
int CpuSpeed_Acceleration_save = 0; // save value during HotKey ^F Max Speed
int bShowInfo                  = 0; // flag to show info for ^I 
uint32 ShowInfoTm0             = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                 = 0; // instr executed on ShowInfoTm0
int FramesCount0               = 0; // num of frames on ShowInfoTm0

// for ibm 407 printer printout
int lptPrintOutDoneCount       = -1; // number of lines already printed on cpanel paper
int hPaperBackgroundOffset;          // offset of background image on paper image

// for tape cabinet
#define MT_anim_sequence_len   500        // max number of animation sequence steps for tape
struct mtcabrec {                         // mtcab[0..5].reel[0..1] is a record that holds the tape reel states
   int mt_is;                             // current visual animation being done
   // state of tape elements
   int rew_u3;                            // amount of tape medium on R reel during rewind operation (used to show info with ^I)
   int rw_tm0, rw_msec, rw_recsize;       // r/w operation start (sim_os_msec time), duration left (msec), record size left (x1000 inch)
   struct mtreelrec {               
       int color;                         // reel color
       int VacCol_h;                      // ammount of medium (x1000 inch) in vacuum column. 0=no medium, >0 medium loop going down into the vaccol in direction of upper sensor
       double rpm;                        // reel current revolutions per second (0=stopped, <0=backwards)
       int ang;                           // reel current angular position (but NOT normalizaed to 0..360 degrees! normalize before using)
       int motor;                         // reel motor operation in progress (-1=accelerate backwards, 0=decelerate, 1=accelerate forward)
       int n;                             // value of reel control state n 
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
} mtcab[6];
int bTapeAnimInProgress;                    // signals tape medium/reels are moving

// Control Panel callbacks
void IBM650_Init(void)
{
    bCardReadVisible = bIasVisible   = bStorageVisible  = 0;
    bTapesVisible    = bRamacVisible = bPrintOutVisible = 0;

    if (IsOption("CardReadPunch")) {
        bCardReadVisible = bPrintOutVisible = 1; 
    } else if (IsOption("cpupanels")) {
        bIasVisible = bStorageVisible = 1;
    } else if (IsOption("IAS")) {
        bCardReadVisible = bPrintOutVisible = bIasVisible = 1; 
    } else if (IsOption("Tape")) {
        bCardReadVisible = bIasVisible = bStorageVisible = bTapesVisible = 1; 
    } else if (IsOption("Ramac")) {
        bCardReadVisible = bIasVisible = bStorageVisible = bTapesVisible = bRamacVisible = 1; 
    }
    bcpu1955 =  (GetState(IBM650.OperatingPanel)==0) ? 0:1; // if of opertaing control on DF has InitialState=1 -> cpu panel fpr year 1955

    CSW=0;
    CSWAddress=0;
    CSWControl=0; // 0=Run, 1=Manual Operation, 2=Address Stop
    CSWDisplay=5; // 0=PR, 1=Read-Out, 2=Read-In, 3=AccLo, 4=AccUp, 5=Distrib
    CSWErrorSw=0;
    CSWDisplayUnit = 0;
    CSWDisplayArm  = 0;
    
    hInputDeck = hOutputDeck = 0;
    nRamacArm = yRamacArm = vRamacArm = vRamacArm = 0; 

    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }

    memset (mtcab, 0, sizeof mtcab);
    bTapeAnimInProgress=0;

    lptPrintOutDoneCount=-1; // number of lines already printed on cpanel paper

}

void IBM650_Reset(void)
{
}

// get in d1 d2 the values to display in Reg_DISP_0T9/Reg_DISP_N 
// Reg 0T9 controls the lights for values 0-4 and 5-9 (and sign)
// Reg N controls the light for each digit for value 0-4
// a bit set to 1 illuminates the corresponding controñ panel light
void Get_DISP_lights(t_int64 * dd1, t_int64 * dd2)
{
    t_int64 DISP, d, d1, d2;
    int NegativeZeroFlag, i, n, neg;

    // get value to be displayed in control panel DISPLAY light 
    // CSWDisplay=0 -> Switch at 'PROGRAM REGISTER' (pos 1), 
    //           =1 -> Switch at 'READ-OUT STORAGE' (pos 2), --> also display DIST
    //           =2 -> Switch at 'READ-IN STORAGE' (pos 3),  --> also display DIST
    //           =3 -> Switch at 'LOWER ACCUM' (pos 7), 
    //           =4 -> Switch at 'UPPER ACCUM' (pos 8), 
    //           =5 -> Switch at 'DISTRIB' (pos 9), 

    NegativeZeroFlag=0;
    if (CSWDisplay==0) {
        DISP=PR; 
        NegativeZeroFlag=0;
    } else if (CSWDisplay==3) {
        DISP=ACC[0]; 
        NegativeZeroFlag=AccNegativeZeroFlag;
    } else if (CSWDisplay==4) {
        DISP=ACC[1]; 
        NegativeZeroFlag=AccNegativeZeroFlag;
    } else {
        DISP=DIST; 
        NegativeZeroFlag=DistNegativeZeroFlag;
    }

    // convert DISP value to control panel lights (one bit per light)
    // Reg_DISP_0T9 = 22 bits
    //                bit 0 = light 0-4 digit 10, bit 1 = light 5-9 digit 10
    //                ...
    //                bit 18 = light 0-4 digit 1, bit 19 = light 5-9 digit 1
    //                bit 20 = plus sign, bit 21 = minus sign
    // Reg_DISP_N   = 50 bits
    //                bit 0 = light 0 digit 10, bit 1 = light 1 digit 10, bit 2 = light 2 digit 10, bit 3 = light 3 digit 10, bit 4 = light 4 digit 10, 
    //                bit 5 = light 0 digit 9, ...

    d1=d2=0;
    if (DISP<0) {
        neg=1; d=-DISP;
    } else {
        neg=0; d=DISP;
    }
    if ((DISP==0) && (NegativeZeroFlag)) neg=1;
    for(i=0;i<10;i++) {
        n = (int)(d % 10); d=d / 10; 
        d1=(d1 << 2) + ( (n>4) ? 2:1 );
        if (n>4) n=n-5;
        d2=(d2 << 5) + (t_int64)( 1 << n );
    }
    d=1;
    d1 = d1 + ((neg) ? (d<<21) : (d<<20)); // sign light
    *dd1=d1; *dd2=d2; 
}

// get in d1 d2 the values to display in Reg_XX_0T9/Reg_XX_N 
void Get_Digits_lights(t_int64 * dd1, t_int64 * dd2, t_int64 d, int ndigits)
{
    t_int64 d1, d2;
    int i, n;

    // convert value to control panel lights (one bit per light)
    if (d<0) d=-d;
    d1=d2=0;
    for(i=0;i<ndigits;i++) {
        n = (int)(d % 10); d=d / 10; 
        d1=(d1 << 2) + ( (n>4) ? 2:1 );
        if (n>4) n=n-5;
        d2=(d2 << 5) + (t_int64)( 1 << n );
    }
    *dd1=d1; *dd2=d2; 
}

// get in d1 d2 the values to display in Reg_OPER/Reg_CHK 
void Get_Operating_lights(t_int64 * dd1, t_int64 * dd2)
{
    t_int64 d1, d2, OP, d;

    //               light on operating metal panel

    // bit on d1     on 1954 cpu          on 1955 cpu
    //  0            data address         data address
    //  1            program              program
    //  2            punch                input-output
    //  3            instruction addr     instruction address
    //  4            accumulator          accumulator
    //  5            read                 overflow
    //  6            (not used)           inquiry
    //  7            (not used)           disk storage
    //  8            (not used)           magnetic tape

    //               light on checking panel

    // bit on d2     on 1954 cpu          on 1955 cpu
    //  0            program register     program register
    //  1            storage selection    storage selection
    //  2            distributor          distributor
    //  3            overflow             accumulator
    //  4            (not used)           control unit
    //  5            clocking             storage unit
    //  6            accumulator          clocking
    //  7            error sense          error sense

    d=1;
    d1=d2=0;
    if (IMachineCycle != 'I') {
        d1 |= (d << 0); // set data address light on operating metal panel -> DA cycle ready to be exec -> set bit 0
    } else {
        d1 |= (d << 3); // set instruction address light on operating metal panel -> IR cycle ready to be exec -> set bit 0
    }

    // set program light
    if ((StopReason == STOP_ADDR) || (StopReason == STOP_HALT) || 
        (StopReason == STOP_PROG) || (StopReason == SCPE_STOP)) {
        d1 |= (d << 1); 
    }

    // set accumulator light
    if (StopReason == 0) {
        if ((AR==8002) || (AR==8003)) d1 |= (d << 4); // being used as Data Address or Instr Address (DA/IA)
        OP=PR / D8; // get current operation code from Program Register
        if ((base_ops[OP].UseAccumulator) && (IMachineCycle != 'I')) {
            d1 |= (d << 4); // being used in opcode processing (artihmetic, shift, etc)
        }
    }

    if ((StopReason == 0) || (StopReason == STOP_IO)) {
        if (bcpu1955 == 0) {
            // set punch light if interlock set 
            if (InterLockCount[IL_WR1]) d1 |= (d << 2); 
            // set read light if interlock set 
            if (InterLockCount[IL_RD1]) d1 |= (d << 5); 
        } else {
            // set input-output light if interlock set 
            if ((InterLockCount[IL_WR1] ) || (InterLockCount[IL_RD1])   ||
                (InterLockCount[IL_WR23]) || (InterLockCount[IL_RD23])) d1 |= (d << 2); 
        }
    }

    // overflow
    if (bcpu1955 == 0) {
        // on 1954 cpu front panel, overflow light is in checking section
        if (OV) d2 |=  (d << 3); 
    } else {
        // on 1955 cpu front panel, overflow light is in operating section
        if (OV) d1 |=  (d << 5); 
    }

    // set disk storage, magnetic tape only in 1955 control panel
    if (bcpu1955) {
        // set disk storage
        if ((StopReason == 0) || (StopIOError == 3) || (StopIOError == 4)) {
           if (InterLockCount[IL_RamacUnit]) d1 |= (d << 7); 
        }
        // set magnetic tape
        if ((StopReason == 0) || (StopIOError == 2)) {
           if (InterLockCount[IL_Tape]) d1 |= (d << 8); 
        }
    }

    // set storage selection
    if (StopReason == STOP_ADDR) {
        d2 |= (d << 1); 
    }

    *dd1=d1; *dd2=d2; 
}

// get in d1 d2 the values to display in Reg_IAS_BLOCK_N/Reg_IAS_WORD_N
void Get_IAS_lights(t_int64 * dd1, t_int64 * dd2)
{
    t_int64 d1, d2, d;
    int n;
    
    d=1;

    n = (LastDiskCmd / 100);
    if (n > 0) { 
        // ramac is accessing the IAS memory for data transfer
        d1 = (d << (n-1));
        d2 = 0;
    } else {
        // regular TimingRing display
        d1 = (d << (IAS_TimingRing / 10));
        d2 = (d << (IAS_TimingRing % 10));
    }
    *dd1=d1; *dd2=d2; 
}

void Get_IAS_Operating_lights(t_int64 * dd1)
{
    t_int64 d1, OP;

    //               light on operating metal panel ibm 653 storage unit

    // bit on d1     
    //  0            magnetic tape
    //  1            immediate access storage
    
    d1=0;

    if (StopReason == 0) {
        // set magnetic tape
        if (InterLockCount[IL_Tape]) d1 |= 1; 
        // set ias
        if ((AR>=9000) ) d1 |= 2; // being used as Data Address or Instr Address (DA/IA)
        OP=PR / D8; // get current operation code from Program Register
        if ((base_ops[OP].opInterLock==IL_IAS) && (IMachineCycle=='D')) {
            d1 |= 2; // being used in opcode processing (transfer drum <-> ias)
        }
    }

    if (InterLockCount[IL_IAS]) d1 |= 2; 
    *dd1=d1; 
}

// get in d1 d2 the values to display in Reg_MT_SEL_N/Reg_MT_OPER
void Get_MT_lights(t_int64 * dd1, t_int64 * dd2)
{
    t_int64 d1, d2, d;
    
    d=1;
    d1=0; d2=0;
    if ((LastTapeSelected >= 0) && (LastTapeSelected  <= 5)) {
        // selected tape light
        d1 = (d << (LastTapeSelected));
        // load point light
        if ((mt_unit[LastTapeSelected].u3==0) && (mt_ready(LastTapeSelected)==1)) {
           d2 |= 32; 
        }

    }
    if ((StopIOError == 2) || (InterLockCount[IL_Tape])) {
        // operating lights
        if ((LastTapeCmd==OP_RTC) || (LastTapeCmd==OP_RTA) || (LastTapeCmd==OP_RTN)) {
            d2 |= 1; // read light
        } else if ((LastTapeCmd==OP_WTM) || (LastTapeCmd==OP_WTA) || (LastTapeCmd==OP_WTN)) {
            d2 |= 2; // write light
        } else if (LastTapeCmd==OP_BST) {
            d2 |= 4; // backspace light
        }
        if ((LastTapeCmd==OP_RTA) || (LastTapeCmd==OP_RTN) || 
            (LastTapeCmd==OP_WTA) || (LastTapeCmd==OP_WTN)) {
            d2 |= 8; // execute  light -> trasfering data betwwen ias and tape
        }
    }
    *dd1=d1; *dd2=d2; 
}

// get in d1 .. d4 the values to display in Reg_DS_UNIT_N/DISK/TRACK/ARM
void Get_DS_lights(t_int64 * dd1, t_int64 * dd2, t_int64 * dd3, t_int64 * dd4)
{
    t_int64 d1, d2, d3, d4, d, unit, arm, dsk, tr, cmd;
    
    // get value to be displayed in control panel DISK STORAGE ADDRESS lights
    // CSWDisplayUnit=0..3 -> Unit to be show in DISK/TRACK lights
    // CSWDisplayArm=0..2 -> Arm of Unit to be show in DISK/TRACK lights

    d=1;

    // Reg_DS_UNIT_N show active units (=unit with an active arm (=arm with an operation in progress))
    d1=0;
    for(unit=0;unit<4;unit++) {
        cmd=0;
        for(arm=0;arm<3;arm++) {
            if (Arm[unit][arm].cmd > 0) cmd=1; 
        }
        if (cmd) d1 |= (d << unit);

    }

    // Reg_DS_ARM_N show active arm (=arm with an operation in progress) for the selected unit on switch
    // Reg_DS_DISK_N/TRACK show current position of selected unit/arm on switches
    unit=CSWDisplayUnit;
    if ((unit<0) || (unit>3)) unit=0; // sanity check
    d4=0;
    for(arm=0;arm<3;arm++) {
        if (Arm[unit][arm].cmd > 0) {
            d4 |= (d << arm);
        }
    }

    // Reg_DS_DISK_N/TRACK show current position of selected unit/arm on switches
    arm=CSWDisplayArm;
    if ((arm<0) || (arm>2)) arm=0; // sanity check
    tr = Arm[unit][arm].current_track;
    dsk = Arm[unit][arm].current_disk;

    d2 = (dsk< 0) ? 0 : ((d << (dsk % 10)) << 10) + ( d << (dsk / 10));
    d3 = (tr < 0) ? 0 : ((d << (tr  % 10)) << 10) + ( d << (tr  / 10));

    *dd1=d1; *dd2=d2; *dd3=d3; *dd4=d4; 
}

// get in d1d2 the values to display in Reg_DS_OPER
void Get_DS_Operating_lights(t_int64 * dd1)
{
    t_int64 d1, arm, unit, tr, cmd;

    unit=CSWDisplayUnit;
    if ((unit<0) || (unit>3)) unit=0; // sanity check
    arm=CSWDisplayArm;
    if ((arm<0) || (arm>2)) arm=0; // sanity check

    cmd = Arm[unit][arm].cmd; 
    tr = Arm[unit][arm].current_track;
    
    if ((StopIOError==3) || (StopIOError==4)) {
        cmd = LastDiskCmd % 100;
    }

    d1 = 0; 
    if (tr < 0) d1 |= 1; // run = arm is going up or down to change selected disk
    d1 |= (cmd == OP_SDS) ? 2 : (cmd == OP_RDS) ? 4 : (cmd == OP_WDS) ? 8 : 0;
    // if bad unit/arm in dist when OP_SDS/RDS/WDS was issued, set checking light
    if (StopIOError==4) d1 |= 16;  // i/o error (or not attached, or diabled) -> light arm
    if (StopIOError==3) d1 |= 32;  // address light

    *dd1=d1; 
}


void IBM650_TickIntensityCount(void)
{
    t_int64 d1, d2, d3, d4, OP;
    // count times digits set in register to be able to calc on refresh callback
    // the intensity on light on each bits

    Get_DISP_lights(&d1, &d2);
    TickCount(IBM650.Reg_DISP_0T9, d1);
    TickCount(IBM650.Reg_DISP_N, d2);
    
    OP=PR / D8; // get current operation code from Program Register
    Get_Digits_lights(&d1, &d2, OP, 2);
    TickCount(IBM650.Reg_OP_0T9, d1);
    TickCount(IBM650.Reg_OP_N, d2);

    Get_Digits_lights(&d1, &d2, AR, 4);
    TickCount(IBM650.Reg_AR_0T9, d1);
    TickCount(IBM650.Reg_AR_N, d2);

    Get_Operating_lights(&d1, &d2);
    TickCount(IBM650.Reg_OPER, d1);
    TickCount(IBM650.Reg_CHK, d2);

    // IBM 653
    if (bIasVisible) {
        Get_IAS_Operating_lights(&d1);
        TickCount(IBM650.Reg_IAS_OPER, d1);

        Get_IAS_lights(&d1, &d2);
        TickCount(IBM650.Reg_IAS_BLOCK_N, d1);
        TickCount(IBM650.Reg_IAS_WORD_N, d2);
    }
    // IBM 652
    if (bStorageVisible) {
        Get_MT_lights(&d1, &d2);
        TickCount(IBM650.Reg_MT_SEL_N, d1);
        TickCount(IBM650.Reg_MT_OPER, d2);

        Get_DS_lights(&d1, &d2, &d3, &d4);
        TickCount(IBM650.Reg_DS_UNIT_N, d1);
        TickCount(IBM650.Reg_DS_DISK_N, d2);
        TickCount(IBM650.Reg_DS_TRACK_N, d3);
        TickCount(IBM650.Reg_DS_ARM_N, d4);

        Get_DS_Operating_lights(&d1);
        TickCount(IBM650.Reg_DS_OPER, d1);
    }
}

// replace in d1 digit at <pos> pos with new digit <digit>
t_int64 SetDigit(t_int64 d1, int pos, int digit) 
{
    t_int64 d2 = 0;
    int neg = 0;
    int n,i;

    if (d1<0) {d1=-d1; neg=1;}

    for(i=10;i>0;i--) {
        n = Shift_Digits(&d1, 1);
        if (i==pos) n=digit;
        d2 = d2*10+n;
    }
    if (neg) d2=-d2;
    return d2;
}

// handle rotating swtich for 0-9 digit
// Mode = 1 -> to be used when mouse button pressed -> start switch rotation movement
//      = 2 -> to be used when mouse button released -> set end switch rotation and set varible with new value
//      = 3 -> to be used in refresh -> read varible with current value and set switch position 
// swCIdArray, swCIdArrayNum -> is a pointer to array of Control Ids with switches/Switch Number Window
// CSWvar = pointer to variable that holds value corresponding to switches
// nDigits = number of digits (number of switch controls on control array)
void SetSwitchNum(int Mode, int * swCIdArray, int * swCIdArrayNum, t_int64 * CSWvar, int nDigits) 
{
    int CId, i, n; 
    t_int64 d;

    if (Mode == 1) {
        // press mouse button -> start switch rotation movement
        for (i=1; i<=nDigits;i++) {
            // click on switch?
            if (CP_Click.CId != swCIdArray[i]) continue; 
            // yes, get its state
            n = (int) GetState(CP_Click.CId);
            if (n>9) return;
            // set state to initiate rotation movement
            n=n+10;
            SetState(CP_Click.CId, n);
            SetState(swCIdArrayNum[i], n);
            return;
        }
    } else if (Mode == 2) {
        // release mouse button -> end switch rotation movement
        for (i=1; i<=nDigits;i++) {
            if (CP_Click.CId != swCIdArray[i]) continue; 
            n = (int) GetState(CP_Click.CId); 
            if (n>=10) n=n-10;
            // increment digit selected on switch
            n++;
            if (n>9) n=0;
            // set new state acording to digit selected
            SetState(CP_Click.CId, n);
            // set number at little window on top of switch
            SetState(swCIdArrayNum[i], n);
            // set digits in variable
            d = SetDigit(*CSWvar, i, n); 
            *CSWvar=d; 
            return;
        }
    } else if (Mode == 3) {
        // refresh switches based on current CSWvar value
        d=*CSWvar; 
        for (i=1; i<=nDigits;i++) {
            CId = swCIdArray[i];
            n = (int) GetState(CId);
            if (n < 10) { // if movement not initiated, do refresh switch and number on top
               n = Shift_Digits(&d, -1);
               SetState(CId, n);
               SetState(swCIdArrayNum[i], n);
           } else {
               Shift_Digits(&d, -1);
           }
        }
    }
}

// handle rotating swtich for multi-selection
// Mode = 1 -> to be used when mouse button pressed -> start switch rotation movement
//      = 2 -> to be used when mouse button released -> set end switch rotation and set varible with new value
//      = 3 -> to be used in refresh -> read varible with current value and set switch position 
// swCId -> is the switch Control Id 
// CSWvar = pointer to variable that holds value corresponding to switches
// sAllowedPos = postions of swtich allowed (e.j -/+ switch is "19" -> when switch at pos "1" then CSWVar set to 0, pos "9" then CSWVar set to 1
void SetSwitch(int Mode, int swCId, int * CSWvar, char * sAllowedPos) 
{
    int n; 

    if (Mode == 1) {
        // press mouse button -> start switch rotation movement
        n = (int) GetState(swCId);
        if (n<10) n=n+10;
        SetState(swCId, n);
    } else if (Mode == 2) {
        // release mouse button -> select nect position in switch
        *CSWvar=(*CSWvar)+1;
        Mode=4; 
    }
    if ((Mode == 3) || (Mode == 4)) {
        // refresh switches based on current CSWvar value        
        n = (int) GetState(swCId);
        if ((n>9) && (Mode==3)) return; // switch in moving -> do not refresh
        if (n>9) n=n-10;
        if (sAllowedPos == NULL) {
            n=0; // no string
        } else if (*CSWvar >= (int) (strlen(sAllowedPos)) ) {
            n=sAllowedPos[0] - '0'; // last allowed pos passed, return to first allowed pos
            *CSWvar=0;
        } else {
            n=sAllowedPos[*CSWvar] - '0';
        }
        SetState(swCId, n);
    }
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

void Refresh_CardReadPunch(void)
{
    int has_changed, AnimationFinished, n, h;

    // animation sequence: read card going out to already-read-card-deck on card read take stacker 
    // 13 states animation
    // duration of card animation: 200 msec this value is a guess; used because it looks good
    AnimateCard(tm0CardInReadStacker, IBM650.ReadStacker, 2,13, 200, &has_changed, &AnimationFinished);
    if (AnimationFinished) {
        tm0CardInReadStacker=0;          // signal end of animation
        nCardInReadStacker++;            // one more card on take stacker
    }
    // put already-read deck in front to animated card if needed (because frame has_changed)
    if (nCardInReadStacker==0) {
        if (has_changed) cpanel_ControlRedrawNeeded=1;
        SetState(IBM650.ReadDeck, 0);
    } else {
        n = (nCardInReadStacker / 10) + 1;
        if (n>5) n=5;
        if (has_changed) cpanel_ControlRedrawNeeded=1;
        SetState(IBM650.ReadDeck, n);
    }

    // animation sequence: punched card going out to already-punched-card-deck on card punch take stacker 
    // 4 states animation
    // duration of card animation: 120 msec this value is a guess; used because it looks good
    AnimateCard(tm0CardInPunchStacker, IBM650.PunchStacker, 1,4, 120, &has_changed, &AnimationFinished);
    if (AnimationFinished) {
        tm0CardInPunchStacker=0;         // signal end of animation
        nCardInPunchStacker++;           // one more card on take stacker
    }

    // now generate the dynamic states
    // dynamic state image: read hopper + input cards to read deck

    // Dynamically generate State 1 for ReadHopper, with this sequence
    //  - calculate heigh of input card deck (h=28 -> zero cards in input deck, h=155 -> input deck full)
    //    deck is full if it has 500 cards (or the number of cards initially attached if it was > 500)
    if (nCardInReadHopperMax==0) {
        h=1;                                                            // no input cards in read hopper
    } else {
        n = (nCardInReadHopperMax < 500) ? 500 : nCardInReadHopperMax; // number of cards when input hopper is full
        h = (155 - 30) * nCardInReadHopper / n + 30;                   // visible height for Input Deck image
    }
    // - h var (heigh of input deck) act as state of dynamic control
    if (hInputDeck == h) {
        // input deck is already at calculated height. No need to generate a new dynamic state image
    } else {
        hInputDeck = h;
        //  - Copy State 0 (background) to state 1 (state displayed)
        CopyControlImage(IBM650.ReadHopper, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM650.ReadHopper, 1,       0, 0);       // ToCId, ToState,     x1, y1
        //  - Copy ReadHopperRear on State 1 -> now Hopper without cards
        CopyControlImage(IBM650.ReadHopperRear, 0,   0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM650.ReadHopper, 1,       0, 126);     // ToCId, ToState,     x1, y1
        //  - Copy InputDeck (height dependes on num of cards) on State 1 -> now hopper with input cards
        CopyControlImage(IBM650.InputDeck, 0,        0, 0, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                         IBM650.ReadHopper, 1,       7, 167-h);   // ToCId, ToState,     x1, y1
        //  - Copy ReadHopperFront on State 1 -> now Hopper with triangular protections
        CopyControlImage(IBM650.ReadHopperFront, 0,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         IBM650.ReadHopper, 1,       0, 145);     // ToCId, ToState,     x1, y1
        //  - set dynamically generated image to be redraw 
        cpanel_ControlRedrawNeeded=1; 
        SetState(IBM650.ReadHopper, 1);
    }

    // dynamic state image: output cards on punch stacker

    // Dynamically generate State 1 for OutputDeck
    //    State 0 = empty punched card deck, state 2 = full punched card deck
    //    deck is full if it has 1500 cards. If more than 1500 cards punched, the output deck is set to empty again
    // with this sequence
    //  - calculate heigh of output card deck (h=13 -> zero cards in output deck, h=164 -> ouput deck full)
    n = nCardInPunchStacker; while (n > 1500) n=n % 1500;
    h = (164 - 13) * n / 1500 + 13;                 // visible height for Input Deck image
    // - h var (heigh of ouput deck) act as state of dynamic control
    if (hOutputDeck == h) {
        // output deck is already at calculated height. No need to generate a new dynamic state image
    } else {
        hOutputDeck = h;
        //  - Copy State 0 (background) to state 1 (state displayed) -> now hopper empty
        CopyControlImage(IBM650.OutputDeck, 0,   0, 0, 0, 0,      // FromCId, FromState, x0, y0, w, h, 
                         IBM650.OutputDeck, 1,   0, 0);           // ToCId, ToState,     x1, y1
        //  - Copy State 2 (OuputDeck height dependes on num of cards) on State 1 -> now hopper with ouput cards
        CopyControlImage(IBM650.OutputDeck, 2,   0, 164-h, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                         IBM650.OutputDeck, 1,   0, 0);           // ToCId, ToState,     x1, y1
        //  - set dynamically generated image to be redraw 
        cpanel_ControlRedrawNeeded=1; 
        SetState(IBM650.OutputDeck, 1);
    }

}

void Refresh_Ramac(void)
{
    int tr, dsk, n, y, x, nWire;
    int y0, y1, ww, hh, p0, r,g,b, x0, x1;
    uint32 * surface0; 
    uint32 col0;
    double nd, xd;


    // Simulate the visuals of disk spinning with the image moire
    // each state image has moire shifted 2 pixels towards the center of disk
    // state images are shown in loop state 0,1,2 then 2,1,0 again
    // this animation is tracked with real time clock, so if fast mode is set (^F), the read/write arm will 
    // move faster, but the spinning (i.e. the moire effect) remains the same and it is not accelerated
    // IBM 355 disk spins at 1200 RPM, this is one revolution each 50 msec. 
    // To look goog the moire effect should cycle each 500 msec (10 times slower)
    if (Tm0Moire==0) {
        Tm0Moire = Refresh_tnow; // start moire sequence
    } else {
        int CPMoireStates=5;  // moire states 0..4 defined in contol panel .ini fule
        int msec=500;     // total msec time for moire cycling
        int NumStates;
        NumStates=CPMoireStates * 2 -2;  // moire states 0..4 then 3..1
        n = (int) (Refresh_tnow-Tm0Moire); // number of msec elapsed from start of animation
        if ((n<0) || (n>1000000)) n=1000000;
        n = (NumStates * n) / msec;  
        if (n>=NumStates) {
            SetState(IBM650.DiskStackMoire,0);
            Tm0Moire=Refresh_tnow; // reset moire sequence
        } else {
            if (n > (NumStates / 2)) n=NumStates-n; // to make flow 0..4 then 3..1
            SetState(IBM650.DiskStackMoire,n);
        }
    }

    // get pos for first arm in first unit - the one animated

    tr = Arm[0][0].current_track;
    dsk = Arm[0][0].current_disk % 50;      // 2 disk surfaces per physical disk

    y0=-7; y1=463;                          // y coord for arm image: -7 ... 463 relative to ArmRunArea control y origin
    y = ((y1-y0) * dsk / 50) + y0;          // dsk ranges from 0..49
    n = (30 * (tr+1)) / 101;                // arms ranges from track -1 (out of disk) ..99)

    if (n>0) {
        // arm into disk surface, so there is no vertical/horizontal oscilation possible
        vRamacArm=vvRamacArm=0; 
    } if (yRamacArm == y) {
        // arm stopped. check vertical oscillation
        if (vvRamacArm>0) {
            vvRamacArm--; 
            if (vRamacArm==2) vRamacArm=-1; else
            if (vRamacArm==-2) vRamacArm=1; else vRamacArm=-vRamacArm;
        } else {
           vRamacArm=0;
        }
        if (vvRamacArm<0) {
            vvRamacArm++; 
            if (vvRamacArm & 1) n=32; // oscilate horizontalli
        }
    } else {
        // arm is running up or down
        if (yRamacArm > y) vRamacArm=2;   // incr vertical motion speed to the top of disk  
        if (yRamacArm < y) vRamacArm=-2;  // incr vertical motion speed to the bottom of disk  
        vvRamacArm=3; // number of vertical oscilations to do when arm stops
    }
    switch(vRamacArm) {
        case  2: n=34; break; // going up fast
        case  1: n=33; break; 
        case -1: n=31; break; 
        case -2: n=30; break; // going down fast
    }
    if ((n==0) && (nRamacArm>0) && (nRamacArm < 30) && (yRamacArm == y)) {
        // if arm is outside disk, but comming from inside, set to oscilate horizontally
        vvRamacArm=-3;
    }

    if ((yRamacArm == y) && (nRamacArm == n)) return;  // arm already at the correct pos, no need to update dynamic image 

    yRamacArm = y;  // if something wrong, arm painted outside area
    nRamacArm = n;  // if something wrong, arm disapears
    
    nWire = 25 - 26 * dsk / 50; 

    //  - Copy State 0 (background) to state 1 (state displayed)
    CopyControlImage(IBM650.ArmRunArea, 0,       0, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,       0, 0);         // ToCId, ToState,     x1, y1

    //  - Draw the Arm shadow (depending on track accesed) on Run Area at appropriate 
    //    vertical position (depending on disk accesed)
    CopyControlImage(IBM650.ArmShadow, 0,      100, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,     210, yRamacArm+100); // ToCId, ToState,     x1, y1

    //  - Draw the Arm wires shadow (depending on track accesed) on Run Area at appropriate 
    //    vertical position (depending on disk accesed)
    CopyControlImage(IBM650.ArmWiresShadow, 0,   0, 30+(25-nWire)*5, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,     120, yRamacArm+100); // ToCId, ToState,     x1, y1

    //  - Draw the Arm Moving head bar that enters into disks. Carefully crafted so that the
    //    shadow is only draw over metalic rectangular box
    n= (nRamacArm >= 30) ? 0:nRamacArm; 
    ww=107-n*4; 
    hh=510-yRamacArm; 
    if ( n < 8) {
        x0=32-n*4; ww-=x0;
    } else x0=0; 
    if (yRamacArm < 178) {
        y0=178-yRamacArm;
    } else y0=0;
    if (ww >= 0) {
        CopyControlImage(IBM650.MovingHeadBarShadow, 0,  x0+0, y0+50, ww, hh,    // FromCId, FromState, x0, y0, w, h, 
                         IBM650.ArmRunArea, 1,     x0+10+n*4, y0+yRamacArm+85); // ToCId, ToState,     x1, y1
    }

    //  - Draw the appropiate Arm Wires background image (depending on disk accesed) on Run Area at appropriate 
    //    vertical position (depending on disk accesed)
    CopyControlImage(IBM650.ArmWireRunArea, 0,   0, 0, 0, yRamacArm-90, // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,       122, 194);             // ToCId, ToState,     x1, y1

    //  - Draw the appropiate Arm image (depending on track accesed) on Run Area at appropriate 
    //    vertical position (depending on disk accesed)
    CopyControlImage(IBM650.Arm, nRamacArm,      0, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,       2, yRamacArm); // ToCId, ToState,     x1, y1


    //  - Draw the appropiate Arm Wires image (depending on disk accesed) on Run Area at appropriate 
    //    vertical position (depending on disk accesed)
    CopyControlImage(IBM650.ArmWires, nWire,     0, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,       124, yRamacArm+130); // ToCId, ToState, x1, y1

    // - Draw upper part of cabstaint cable
    //   is two pixels wide (one full pixel, 2 al 50% intensity)
    surface0 = GetControlSurface(IBM650.ArmRunArea, 1, &ww, &hh);    // ToCId, ToState, &ww, &hh
    col0 = surface_rgb_color(20,20,20);
    y0=60; y1=yRamacArm+78;
    x0=254; 
    for (y=y0; y<y1; y++) {
       p0=x0 + y * ww; // position of cable pixel
       get_surface_rgb_color(surface0[p0-1], &r, &g, &b); // convert to rgb
       surface0[p0-1] = surface_rgb_color(r/2,g/2,b/2);   // set left pixel at half intensity 
       get_surface_rgb_color(surface0[p0+1], &r, &g, &b); // convert to rgb
       surface0[p0+1] = surface_rgb_color(r/2,g/2,b/2);   // set right pixel at half intensity 
       surface0[p0] = col0;
    }
    // - Draw lower part of capstaint cable
    //   use antialiasing
    y0=yRamacArm+130; y1=615;
    x0=259; x1=268;
    for (y=y0; y<y1; y++) {
       xd = x0 + (x1-x0) * (double) (y-y0) / (double) (y1-y0);
       x  = (int) xd;
       nd = (xd - x);                                              // antialiase to look great
       p0=x + y * ww; // position of cable pixel
       for(n=0;n<3;n++) {
          // nd   n   0     1     2
          // 0.0      col0  col0  100% background 
          // 0.5      50%   col0  50%
          // 1.0      100%  col0  col0
          if (n==1) {
             surface0[p0] = col0;
          } else {
             if (n==2) nd=1-nd;
             get_surface_rgb_color(surface0[p0], &r, &g, &b); // convert to rgb
             r=(int)((r-20)*nd) + 20; // col0=20
             g=(int)((g-20)*nd) + 20; // col0=20
             b=(int)((b-20)*nd) + 20; // col0=20
             surface0[p0] = surface_rgb_color(r,g,b);   
          }
          p0++;
       }
    }

    // - Draw vertical metal bar guide on top (as bar is nearest to the observer than the arm)    
    CopyControlImage(IBM650.VerticalBarGuide2, 0,    0, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,           99, 0);        // ToCId, ToState,     x1, y1
    CopyControlImage(IBM650.VerticalBarGuide1, 0,    0, 0, 0, 0,    // FromCId, FromState, x0, y0, w, h, 
                     IBM650.ArmRunArea, 1,           104, 20);      // ToCId, ToState,     x1, y1

    //  - signal dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(IBM650.ArmRunArea, 1);
    
}


#define     MT_is_loading_tape      1
#define     MT_is_rewinding         2
#define     MT_is_unloading_tape    3


int PARAM_MaxSlice_msec  =   100;  // max time considered for tape hop calculations
int PARAM_Reel_Diameter  =   267;  // reel diameter in mm
int PARAM_RPM            =  1140;  // reel forward/backwards motor revolutions per minute
int PARAM_VacCol_h_Low   = 15000;  // upper vacuum colum sensor (inches x 1000) triggers reel load medium on colum 
int PARAM_VacCol_h_Hi    = 40000;  // lower vacuum colum sensor (inches x 1000) triggers reel take medium from colum 
int PARAM_RWSpeed        =    75;  // tape head operates at 75 inches per sec
int PARAM_AccelTime      =   145;  // accel time in msec. clutch must obtain 2/3 of maximum speed in 0.135-0.150 seconds with a full reel of tape
int PARAM_DecelTime      =   155;  // decel time in msec. the stop clutch must stop a full reel of tape from full speed in 0.145-0.160 sec
                                   // note: the accel/decel time increases when tape unit
                                   // get used. Lower values makes jerky spins with thigh 
                                   // oscilation of tape loop in vacuum columns. Higher values
                                   // makes smother spins, with more ample oscilations of tape loop
int PARAM_HeadOpenTime   =  1700;  // time needed by r/w tape head to fully open or close
int PARAM_TakeMotor_RPM  =    40;  // take motor revolutions per minute
int PARAM_HiSpeedRwdSpeed =  500;  // High Speed rewind at 500 inches/sec 


#define     MT_anim_step_nop       -1             // do nothing, just keeps current MT state values
#define     MT_anim_step_inc        0             // incremental animation step
#define     MT_anim_step_rw         1             // read/write tape animation step 
#define     MT_anim_step_HiRew      2             // High Speed Rewind animation step 
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
    int time, nseq, msec, hint, recsize, ang, n, m, n1, n2, u3, u3_dec, p; 
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
            *MT_Reel_Amount = (int) GetState(IBM650.MT[unit]); 
            *MT_L_Rot = (int) GetState(IBM650.MT_L[unit]) % 64; // keeps blur
            *MT_R_Rot = (int) GetState(IBM650.MT_R[unit]) % 64;                           
            *MT_head =  (int) GetState(IBM650.MT_head[unit]);  
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
            if (MT_anim_no_change & mtcab[unit].seq[nseq].MT_Reel_Amount) *MT_Reel_Amount = (int) GetState(IBM650.MT[unit]); 
            else *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            if (mtcab[unit].reel[0].color == 1) {
               *MT_L_Rot = mtcab[unit].reel[0].n = (mtcab[unit].reel[0].ang % 120) * 24 / 120;
            } else {
               *MT_L_Rot = mtcab[unit].reel[0].n = mtcab[unit].reel[0].ang * 24 / 360; // get current angular position of reels
            }
            if (mtcab[unit].reel[1].color == 1) {
               *MT_R_Rot = mtcab[unit].reel[1].n = (mtcab[unit].reel[1].ang % 120) * 24 / 120;
            } else {
               *MT_R_Rot = mtcab[unit].reel[1].n = mtcab[unit].reel[1].ang * 24 / 360;
            }
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
            // this step is regualar tape mevement made by the r/w header, either stoped, backwards
            // or forwards, at 75 inch/sec. Simulate also reel spinning to take/load medium in vac col
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
        } else if ((hint == MT_anim_step_HiRew) && (time < 0)) {
            // high speed rewind 
            // animation variables usage
            //     parameter: seq[nseq].MT_head  
            //        acceleration flag. Can be 1 (acceelerate), -1 (decelerate),
            //        or zero (full speed)
            //     parameter: seq[nseq].L_VacCol_inc  
            //        is the amount of medium at beginning of step
            //     parameter: seq[nseq].R_VacCol_inc  
            //        is the amount of medium to rew 
            //        update mtcab[unit].rew_u3 with the amount of tape remaining to be rew
            u3     = mtcab[unit].seq[nseq].L_VacCol_inc;
            u3_dec = mtcab[unit].seq[nseq].R_VacCol_inc;
            n=mtcab[unit].seq[nseq].MT_head; 
            *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            n1 = (int) GetState(IBM650.MT_L[unit]) % 64; 
            n2 = (int) GetState(IBM650.MT_R[unit]) % 64; 
            // msec+time = msecs elapsed of this animation step
            // -time     = msecs remaining to be done on this animation steop until it ends
            // use it on acceleration/deceleration 
            m = msec+time; 
            if (n!=0) { // acceleration/deceleration
                int d;
                if (n<0) m = msec-m; // deceleration -> reverse m time used
                if (mtcab[unit].reel[0].color == 1) {
                    d = (m<msec/3) ? 4 : (m<2*msec/3) ? 5 : 9;
                } else {
                    d = (m<msec/3) ? 2 : (m<2*msec/3) ? 3 : 5;
                }
                n1-=d;
                while (n1 < 24) n1+=24; while (n1 >= 48) n1-=24; 
                if (mtcab[unit].reel[1].color == 1) {
                    d = (m<msec/3) ? 4 : (m<2*msec/3) ? 5 : 9;
                } else {
                    d = (m<msec/3) ? 2 : (m<2*msec/3) ? 3 : 5;
                }
                n2-=d;
                while (n2 < 24) n2+=24; while (n2 >= 48) n2-=24; 
            } else { // full speed
                n1-=3;
                while (n1 < 48) n1+=48; while (n1 >= 48+8) n1-=8; 
                n2-=3;
                while (n2 < 48) n2+=48; while (n2 >= 48+8) n2-=8; 
            }
            if (u3) {
                // update mtcab[unit].rew_u3 to signal progress of rewind
                // msec+time = time elapsed in this step. msec = this step duration
                m = msec+time; 
                m = 100*m / msec; if (m>100) m=100; if (m<0) m=0; // m=0..100 is % of time of step elapsed
                // calculate rew_u3 = how much medium has been rew 
                mtcab[unit].rew_u3 = u3 - u3_dec * m / 100;
                // printf("unit %d, rew_u3 %d elapsed %d msec %d\t\n", unit, mtcab[unit].rew_u3, msec+time, msec);
            }
            // check fast mode
            if (CpuSpeed_Acceleration == -1) {
                // if Key Control-F (^F, Ctrl F) being pressed then set duration to 400 msec
                mtcab[unit].seq[nseq].msec = 400;
                n1=56; // make L reel transparent
            }
            *MT_head  = 11;               // head wide open
            *MT_L_Rot = n1;
            *MT_R_Rot = n2;
            *L_VacColMedium_h = 0;        // vac col empty
            *R_VacColMedium_h = 0;        // vac col empty
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
//    - spin reels slowly to load medium on vacuum columns while closing r/w head
void mt_add_load_seq(int unit)
{
    int MT_Reel_Amount,  MT_head; // state of controls at end of of redraw
    int i, L_h, R_h, L_inc_h, R_inc_h, nHead, msec, ang_inc, r1, r2;

    MT_Reel_Amount   = 1;    // all tape medium on L tape
    MT_head          = 11;   // head full open

    // head needs 1800 msec to go from wide open to close
    // reels rotates 360 + 90 + 45 gr during this time (take motor goes at 46 rpm)
    // prepare animation sequence each given msec
    msec=33;                                   // time for animation step 
    ang_inc = (msec * PARAM_TakeMotor_RPM * 360) / (60 * 1000); // angle reel spins on each animation step
    nHead   = PARAM_HeadOpenTime / msec;      // number of steps in animation of head closing

    // calculate the amount of tape medium that reel loads into vaccol on each step given reel rotation
    r1=(50 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel empty
    r2=(90 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel full
    r1=(int) (0.0393701 * r1 * 1000);          // reel radius in inches x 1000
    r2=(int) (0.0393701 * r2 * 1000);            
    L_inc_h = (int) (r2 * 2 * 3.1416 * ang_inc / 360); // beacuse L reel is full. 
    R_inc_h = (int) (r1 * 2 * 3.1416 * ang_inc / 360); // beacuse R reel is empty
    L_inc_h = L_inc_h  / 2;                            // tape loop position is half on medium loaded
    R_inc_h = R_inc_h  / 2; 

    // tape medium loads first on R vacuum colum. When tape pass upper vaccol sensor
    // the medium starts to enter on L column
    L_h = 0;  // amount of tape medium in each column
    R_h = 0;

    for(i=0;;i++) {
        // head position 
        MT_head = 1 + 10 * (nHead-i) / nHead; 
        if (MT_head<1) MT_head=1;
        // reel rotation
        AddSeq(unit, msec, MT_anim_step_inc, 
                L_inc_h  /* L_VacCol_inc */, R_inc_h /* R_VacCol_inc */, 
                +ang_inc /* L_ang_inc */, -ang_inc /* R_ang_inc */, 
                MT_Reel_Amount /* MT_Reel_Amount */, MT_head /* MT_head */ );       
        L_h += L_inc_h; 
        R_h += R_inc_h; 
        if ((R_h > PARAM_VacCol_h_Low) && (L_h > PARAM_VacCol_h_Low)) break; 
    }
}

// add to the current animation sequence the un load animation:
// u3 param is the ammount (inches x1000) of tape medium on reel R   
//    - spin reels slowly to un load medium on vacuum columns 
//    - open r/w head
//    (if u3<0, also ...)
//    - remove left reel, no tape medium on it
//    - close r/w head
void mt_add_unload_seq(int unit, int u3)
{
    int MT_head; // state of controls 
    int i, L_h, R_h, L_inc_h, R_inc_h, msec, ang_inc, r1, r2, rL, rR, p;
    int bUnLoadReel = 0; 

    MT_head        = 1;    // head closed

    if (u3<0) {
        u3=0; bUnLoadReel=1; 
    }

    // calc percent 0..100 of L reel 
    p = (int) ((u3 / (mt_unit[unit].u4*1000.0))*100); 
    if (p>100) p=100; // p=100 -> reel L full, p=0 -> reel L empty

    msec=33;                                   // time for animation step 
    ang_inc=(msec * PARAM_TakeMotor_RPM * 360) / (60 * 1000); // angle reel spins on each animation step

    // calculate the amount of tape medium that reel loads into vaccol on each step 
    // (duration msec) given reel ammount of tape medium in it
    r1=(50 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel empty
    r2=(90 * PARAM_Reel_Diameter / 2) / 100;   // radius when reel full
    rL=r1*100 + (r2-r1)*(100-p);               // L reel current radius in mm x 100
    rR=r1*100 + (r2-r1)*p;                     // R reel current radius in mm x 100
    rL=(int) (0.0393701 * rL * 10);            // reel radius in inches x 1000
    rR=(int) (0.0393701 * rR * 10);            
    L_inc_h = (int) (rL * 2 * 3.1416 * ang_inc / 360); // tape medium taken by L reel each step 
    R_inc_h = (int) (rR * 2 * 3.1416 * ang_inc / 360); // tape medium taken by R reel each step 
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
                MT_anim_no_change /* MT_Reel_Amount */, 1 /* MT_head closed */ );       
        L_h -= L_inc_h; 
        R_h -= R_inc_h; 
        if ((R_h < 0) && (L_h < 0)) break; 
    }

    // spin both reels forward 45gr to retension tape
    for(i=0;i< (45 / ang_inc) ; i++) {
        AddSeq(unit, msec, 0, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            +ang_inc /* L_ang_inc */, +ang_inc /* R_ang_inc */, 
            MT_anim_no_change, 1 /* MT_head closed */ );       
    }

    // pause 0.5 sec
    msec=500;
    AddSeq(unit, msec, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            MT_anim_no_change, 1 /* MT_head closed */ );       

    // now open r/w head
    msec = PARAM_HeadOpenTime / 10; 
    for(i=0;i<10; i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_anim_no_change, MT_head);        
        MT_head++;
    }

    // check if unload terminated with same reel, r/w open
    if (bUnLoadReel==0) return; 

    // close r/w head
    msec = PARAM_HeadOpenTime / 10; 
    MT_head=11; // open head
    for(;;) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0, MT_head);     // no tape medium on reels   
        if (MT_head==2) break; 
        // last value set it MT_head=2 (about to close), so at NoTapeAttached label 
        // will enter first time to set the tape cabinet as detached (and terminate closing tape head)
        MT_head--;
    }


}

// calculate and store in animation array the animation sequence for rewind
void mt_set_rew_seq(int unit, int bStartEndSeqFlag)
{
    int MT_Reel_Amount,  MT_head;    // state of controls at end of of redraw
    int u3 = mt_info[unit].recsize;  // use recsize to get u3 value on rewind start 
    int msec, time, bHi, u3_dec; 

    MT_Reel_Amount = 3 + (int) (20 * (u3 / (mt_unit[unit].u4*1000.0))); 
    if (MT_Reel_Amount > 23) MT_Reel_Amount = 23;
    MT_head        = 1;    // head closed

    if (bStartEndSeqFlag) {
        mtcab[unit].nseq=0;                 // init sequence
        mtcab[unit].nseq_tm0=Refresh_tnow;  // when animation starts
    } else if (u3==0) {
        // nothing to rewind, so exit
        return; 
    }

    bHi=0; // flag high/low speed rew
    mtcab[unit].rew_u3 = u3; // amount on tape medium (inch x1000) in reel R that should be rewinded
                             // original mt_unit[].u3 is set to 0 on OP_RWD command start in mt_cmd()

    // check if rew at high speed. Hi Speed rew is done when at least 450 feet in take reel
    // 450 feet -> 5400 inches
    bHi=0;
    if (u3 > 5400 * 1000) { //n has inches x1000 of tape medium used
        // yes, do high speed rew
        bHi=1; 
        // add the unload animation:
        //    - spin reels slowly to unload medium on vacuum columns
        //    - open r/w head
        mt_add_unload_seq(unit, u3);
        MT_head        = 11;    // head open
                
        // pause 0.5 sec
        msec=500;
        AddSeq(unit, msec, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            MT_Reel_Amount, MT_head);        

        // now start hi speed rew
        // rewind at 500 inch/sec (average) until 400 inches left
        u3 -= 400*1000;                     // u3 in inches x1000 = ammount of medium inches to rewind at hi speed
        time= u3 / PARAM_HiSpeedRwdSpeed;   // hi speed rew total time in msec

        // hi rew with at least 5400 inches (leaves 400 for low speed res)
        // so min amount of reel rew at hi speed is 5000
        // this means 10 sec at 500 inc/sec
        // reduce this time with hi speed rew acceleration deceleration time
        time = time - 1000 - 3000;

        // 1000 msec to accelerate. This is an educated guess
        msec = 1000; 
        AddSeq(unit, msec, MT_anim_step_HiRew, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, 1 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        

        // full speed rewind
        // calculate time of each step and how much mediun is rewinded on each step (u3_dec)
        if (MT_Reel_Amount <= 3) {
            MT_Reel_Amount=3;               // safety
            msec = time; 
            u3_dec = u3;  // amount of medium rewinded on each animation step
        } else {
            msec = time / (MT_Reel_Amount-3 +1);   // time to decrease reel input by one
            u3_dec = u3 / (MT_Reel_Amount-3 +1); 
        }
        if (CpuSpeed_Acceleration == -1) msec = 100; // if ^F only 100 msec per decr of reel_amount

        u3 = mtcab[unit].rew_u3; // position of medium at beginning of hi speed rew;
                                 // at end of hi speeed rew this should have the value of 400x1000
        for(;;) {
            AddSeq(unit, msec, MT_anim_step_HiRew, 
                u3 /* L_VacCol_inc */, u3_dec /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, 0 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        
            u3 -= u3_dec;
            if (MT_Reel_Amount==3) break;
            MT_Reel_Amount--;
        }

        // 3000 msec to decelerate. This is an educated guess
        msec=3000;
        AddSeq(unit, msec, MT_anim_step_HiRew, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, -1 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        

        // pause 0.5 sec
        msec=500;
        AddSeq(unit, msec, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            MT_Reel_Amount, MT_head);        

        // load tape animation
        mt_add_load_seq(unit);
        MT_head        = 1;    // head closed
        
        u3 = 400*1000; // after hi speed rew, this is the amount of tape medium in reel
    }
    // finish rewinding at low speed
    msec = u3 / PARAM_RWSpeed;              // time to read remaining of tape. 
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
        sim_debug(DEBUG_CMD, dptr, "Tape unit %d: %s rewind time needed (%d sec)\n", 
                                   unit, (bHi ? "High Speed":"Low Speed"), time/1000);
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

    // on load, reels rotates at low speed to feed vacuum columns with tape medium
    // while r/w head is closing. Then goes backwards to read read record backwards 
    // (locate the load point)

    mtcab[unit].nseq=0;                 // init sequence
    mtcab[unit].nseq_tm0=Refresh_tnow; // when animation starts

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
    for (n=0;n<6;n++) {
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
                (cmode == 'U') ? 1:11 /* MT_head */ ); 

    if (cmode == 'U') {
        mt_set_rew_seq(unit, 0); 
        mt_add_unload_seq(unit, -1);
        // end sequence
        AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 
        return; 
    }

    // normal load sequence (asumed cmode = 'L')
    
    // make sure no medium on vac col
    mtcab[unit].reel[0].VacCol_h = 0;  // amount of tape medium in each column
    mtcab[unit].reel[1].VacCol_h = 0;

    // add the load animation:
    //    - spin reels slowly to load medium on vacuum columns
    //    - close r/w head
    mt_add_load_seq(unit);

    // Now sense load point by reading backwards
    // Duration of it  depends on how much the user has spinned reel R when 
    // preparing the tape for load
    // when reel is empty, it holds aprox 29,6 inches of tape medium in each revolution
    // r/w heads moves the tape medium at 75 inches/sec -> each revolution done by
    // the user will need 400 msec
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
    p = (int) ((mt_unit[unit].u3 / (mt_unit[unit].u4*1000.0))*100); // r=0 -> reel L full, p=100 -> reel R rull
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
        if ((time < 0) || (time > mtcab[unit].rw_msec)) time = mtcab[unit].rw_msec;
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
    if ((mtcab[unit].rw_msec == 0) && (cmd==1) && (mt_info[unit].cmd_tm0)){ 
        // if no r/w op being animated (rw_msec=0), and now a r/w operation is being done by tape (cmd_tm0 > 0)
        mt_info[unit].cmd_tm0=0;   // mark this as being animated (to avoid animating it twice)
        mtcab[unit].rw_tm0     = tnow; 
        mtcab[unit].rw_msec    = mt_info[unit].cmd_msec;
        mtcab[unit].rw_recsize = mt_info[unit].recsize;
        bTapeAnimInProgress=1; // signal tape medium movement started
    }
    
    // calc reel algular position base on motor acceleratin or decelerating the reel
    for(ireel=0;ireel<2;ireel++) {  
        // get initial motor status
        motor = mtcab[unit].reel[ireel].motor;
        tm0   = mtcab[unit].reel[ireel].tm0;
        if (tm0 == 0) continue;      // motor not started, skip 
        time = tnow - tm0;           // time is elapsed time accelerating/decelerating (in msec)
        if ((time <= 0) || (time > 10*1000)) continue; // safety
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
            if (r < 100) {
                // interpolate rpm, ang, h based on p, calc time = msecs ago tape loop passed sensor
                time = (tnow - old_tnow) * (100 - r) / 100; 
                if ((time < 0) || (time > 10000)) time = 30; // safety
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
        if (mtcab[unit].reel[ireel].color == 1) {
            // grey reel (when color == 1) states covers 120 gr because repeats itself 
            // echa state is rotated 5 gr
            n = 24 * (ang % 120) / 120; 
        } else {
            // white/dark grey reel states covers 360 gr. Each state is rotated 15 gr
            n = 24 * ang / 360;              
        }
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
            r = (r % 24); if (r<0) r+=24; 
            if (r != old[ireel].n) n = r; // use it if not same as last reel pos
            mtcab[unit].reel[ireel].n = n; 
            n = 24 + n; // use blur states 24-47
            bTapeAnimInProgress=1;  // signal reel blur started 
        }
        // set angular position state
        if (ireel) *MT_R_Rot = n; else *MT_L_Rot = n;
    }
    // set head/reel amount control state on cpanel
    // % of tape medium used
    *MT_Reel_Amount = 3 + (int) (20 * (mt_unit[unit].u3 / (mt_unit[unit].u4*1000.0))); 
    if (*MT_Reel_Amount > 23) *MT_Reel_Amount = 23;
    // tape head on closed position
    *MT_head = 1; 

    // save last tnow used to know how much time elapsed since last refresh
    old_tnow = tnow;  
}

// dynamic draw of tape medium into vaccum column control CId_VacCol
// if VacColMedium_h = 15000 = PARAM_VacCol_h_Low -> tape medium loop positioned over upper vacuum colum sensor
// if VacColMedium_h = 40000 = PARAM_VacCol_h_Hi  -> tape medium loop positioned over lower vacuum colum sensor
void mt_VacColSetDynamicState(int VacColMedium_h, int * VacColMedium_h0, int CId_VacCol)
{
    int h, y;
    if (VacColMedium_h == *VacColMedium_h0) return; // no changes in height. nothing to draw
    *VacColMedium_h0 = VacColMedium_h; 

    // calculate h
    // control has upper sensor on h=30, lower sensor on h=100, 
    // even if colum control is 151 pixels heigh, only visible a height of 114
    h= 30 + ((100-30) * (VacColMedium_h-PARAM_VacCol_h_Low) / (PARAM_VacCol_h_Hi-PARAM_VacCol_h_Low));
    if (h<0) h=0; 
    // h is vertical position of tape loop base, converto to y coord
    y=152-h; if (y<0) y=0; 

    // Dynamically generate State 0 for CId_VacCol (=IBM650.MT_L|R_VacCol[unit])

    //  - Copy State 0 from control MT_VacColumn (background) to state 0 of CId (state displayed) 
    CopyControlImage(IBM650.MT_VacColumn, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol, 0,                0, 0);       // ToCId, ToState,     x1, y1
    //  - Copy State 0 from MT_VacColMedium (height dependes on medium position) on State 0 -> now vaccol with tape medium
    CopyControlImage(IBM650.MT_VacColMedium, 0,    0, y, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol,                   0, 0, 0);    // ToCId, ToState,     x1, y1
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId_VacCol, 0);

}


// return 0 if tape is idle (not moving the medium), 1 if tape is doing read/write, -1 if rew
//        mt_indicator=1 if tape has set the indicator light
int get_mt_current_command(int unit, int * mt_indicator)
{
    int cmd;

    cmd=mt_get_last_cmd(unit);
    if (cmd < 0) {
        *mt_indicator = 1; 
        cmd=-cmd; 
    } else *mt_indicator = 0;
    
    if (cmd == OP_RWD) {
        return -1;  
    }
    if ((cmd ==  OP_RTC) || (cmd ==  OP_RTA) || (cmd ==  OP_RTN) ||
        (cmd ==  OP_WTM) || (cmd ==  OP_WTA) || (cmd ==  OP_WTN) ||
        (cmd ==  OP_BST)) {
        return 1; // tape read/write operation
    }
    return 0;
}

void Refresh_MagTape(void)
{
    int unit, n;
    UNIT *uptr;

    int MT_Reel_Amount0, MT_L_Rot0, MT_R_Rot0, MT_head0; // state of controls at beggining of redraw
    int MT_Reel_Amount,  MT_L_Rot,  MT_R_Rot,  MT_head; // state of controls at end of of redraw
    int L_VacColMedium_h, R_VacColMedium_h; // annount of tape medium in vaccol
    int cmd, mt_is, mt_indicator_light;

    bTapeAnimInProgress=0;
    for(unit=0;unit<6;unit++) {
        uptr=&mt_unit[unit];
        // check if unit disabled
        if (uptr->flags & UNIT_DIS) {
            SetState(IBM650.MT_num[unit], 10);     // yes, unit disabled -> tape number no lit on cabinet
           NoTapeAttached:
            if ((GetState(IBM650.MT[unit]) == 0) && (GetState(IBM650.MT_head[unit])==1)) continue; 
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT[unit], 0);          // no magnetic medium on reels
            n=(GetState(IBM650.MT_L[unit]) & 31); if (n>23) n=0;                   // keep current angular position 0..23 for left reel
            n=n+(GetState(IBM650.MT_L[unit]) & (64+128));                          // keep current color for  leftreel
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_L[unit], n);        // Left reel defaults to grey 
            n=(GetState(IBM650.MT_R[unit]) & 31); if (n>23) n=0;                   // keep current angular position 0..23 for right reel
            n=n+(GetState(IBM650.MT_R[unit]) & (64+128));                          // keep current color for  right reel
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_R[unit], n);                           
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_lights[unit], 0);   // lights off
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_head[unit], 1);     // r/w tape head closed
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_L_VacCol[unit], 0); // no magnetic medium on vacuum col
            cpanel_ControlRedrawNeeded = 1; SetState(IBM650.MT_R_VacCol[unit], 0); // no magnetic medium on vacuum col
            continue; 
        } 
        // unit enabled
        SetState(IBM650.MT_num[unit], unit);     // unit enabled -> its number is lit on tape
        // check if tape file attached
        if ((uptr->flags & UNIT_ATT) == 0) {
            if (mt_info[unit].justdetached==1)  {  // 1 -> just detached -> should start unload animation
                if (mtcab[unit].mt_is == MT_is_unloading_tape) {
                    // unload amination in progress, continue
                } else {
                    // start unload amination
                    mtcab[unit].mt_is = MT_is_unloading_tape; 
                    mt_set_anim_seq(unit, 'U'); 
                }
            } else {
                goto NoTapeAttached; // no file attached -> clean up tape 
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
            mtcab[unit].reel[0].color = 1; /* Left reel defaults to white */
            mtcab[unit].reel[1].color = 1; /* Right reel defaults to white */
            cmode = ' ';

            MT_cab_opt[2] = '0' + unit;
            if ((IsOption(MT_cab_opt)) && (IsOptionParam) && (*IsOptionParam++ == '/')) {
                // syntax: option=mt1/WG <- tape 1 is set have reels color White(Left) and Gray (right). 
                //                          the third possible color is D (dark gray)                
                c = *IsOptionParam++; // get left reel color
                c = sim_toupper(c);
                mtcab[unit].reel[0].color = (c == 'G') ? 1 /* gray reel */ : (c == 'D') ? 2 /* dark grey reel */ : 0; /* else defaults to white */
                c = *IsOptionParam++; 
                c = sim_toupper(c);
                mtcab[unit].reel[1].color = (c == 'G') ? 1 /* gray reel */ : (c == 'D') ? 2 /* dark grey reel */ : 0; /* else defaults to white */
                c = *IsOptionParam++; 
                if (c == '*') {
                    c = *IsOptionParam++; 
                    c = sim_toupper(c);
                    if ((c == 'R') || (c == 'F') || (c == 'B')) {
                        cmode = c; // load animation will be Forward/backwards all reel, or *R
                    }
                    RemoveOption(MT_cab_opt); // remove this option, as it is being executed now so will not apply on next attach of tape
                }                
            }
            // reel color set. 
            mt_info[unit].justattached=0;
            // Now init the cabinet states and ... 
            mtcab[unit].reel[0].ang = (sim_rand() & 511) % 360; // reel mounted at random angular position
            mtcab[unit].reel[1].ang = (sim_rand() & 511) % 360;
            mtcab[unit].L_VacColMedium_h0 = -1; // init to -1 last state to force redraw of vaccol 
            mtcab[unit].R_VacColMedium_h0 = -1; 
            // ... signal the load animation can begin
            mtcab[unit].mt_is = MT_is_loading_tape; 
            if (cmode != 'R') {
                // normal load animation, *F or *B 
                 mt_set_anim_seq(unit, cmode);
            } else {
                // *R rewind animation 
                // position of medium on vaccol as it should be at end of write forward
                mtcab[unit].reel[0].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
                mtcab[unit].reel[1].VacCol_h = (int) (PARAM_VacCol_h_Hi  * 0.9); 
                // put all medium on right reel
                mt_unit[unit].u3 = mt_unit[unit].u4 * 1000;
                SetState(IBM650.MT[unit], 23); 
                // start hi speed rewind. This is nice!
                mt_info[unit].recsize = mt_unit[unit].u4 * 1000;
                mt_set_rew_seq(unit, 1);
            }
        }
        // tape is about to be painted on cpanel
        // MT state holds the amount of tape medium in each reel. 
        //   =1  -> all tape on L reel, 
        //   =23 -> all tape on R reel, 
        //   =0  -> no tape, reel unmounted
        // MT_L/MT_R state holds the rotational position of reel, the speed ot reel, and the reel's colour
        //    0..23 -> reel rotated 0gr, 15gr, ... 345gr. To be used when reel is not moving
        //   24..47 -> same reel rotated 0gr, 15gr ... but with some spin blur. To be used when reel is moving
        //   48..55 -> reel rotated 0gr, 45gr, ... whith greater spin blur. To be used when reel is rewinding at fast pace
        //   64..   -> same but with another reel color. There are 3 reels colors.
        // MT_head holds the position of r/w head
        //   =0  -> no head image
        //   =1  -> head closed, prepared to read or write tape medium
        //   =11 -> open head, prepared to manualy remove tape medium 

        // get the current tape control state. If this state changes, then should redraw
        MT_Reel_Amount0  = (int) GetState(IBM650.MT[unit]); 
        MT_L_Rot0        = (int) GetState(IBM650.MT_L[unit]);
        MT_R_Rot0        = (int) GetState(IBM650.MT_R[unit]);
        MT_head0         = (int) GetState(IBM650.MT_head[unit]);

        // what tape is doing now?
        cmd = get_mt_current_command(unit, &mt_indicator_light); // current tape cmd being executed: -1=rew, 0=idle, 1=read/write
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
               mt_set_rew_seq(unit, 1);
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
                    // if rew terminates, notify mt_svr 
                    sim_cancel(&mt_unit[unit]);
                    sim_activate(&mt_unit[unit], 1);
                } else if (mt_is == MT_is_unloading_tape) {
                    // if unload terminates, clear justdetached flag
                    mt_info[unit].justdetached=0;
                    continue; // skip tape state update, just continue so next frame will show no tape mounted
                }
                mt_is = 0;
            }
        }

        // if no animation, simulate reel/vacuum col movement
        if (mt_is == 0) {
            // if tape indicator light on, do not perform cmd 
            if (mt_indicator_light) cmd = 0;
            mt_reels_mov(unit, cmd, 
                         &L_VacColMedium_h, &R_VacColMedium_h, 
                         &MT_L_Rot, &MT_R_Rot, 
                         &MT_Reel_Amount, &MT_head);
        }

        // set reel color
        MT_L_Rot = (MT_L_Rot % 64) + 64 *  mtcab[unit].reel[0].color;
        MT_R_Rot = (MT_R_Rot % 64) + 64 *  mtcab[unit].reel[1].color ;
        // update tape control states
        if ((MT_Reel_Amount0 != MT_Reel_Amount) || 
            (MT_L_Rot0 != MT_L_Rot) || (MT_R_Rot0 != MT_R_Rot) || 
            (MT_head0 != MT_head)) {
            // if some state has changed, redraww all controls
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM650.MT[unit], MT_Reel_Amount);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM650.MT_L[unit], MT_L_Rot);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM650.MT_R[unit], MT_R_Rot);
            cpanel_ControlRedrawNeeded = 1;
            SetState(IBM650.MT_head[unit], MT_head); 
        }
        // set dynamic state for vacCol controls 
        mt_VacColSetDynamicState(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, IBM650.MT_L_VacCol[unit]);
        mt_VacColSetDynamicState(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, IBM650.MT_R_VacCol[unit]);
        // set tape lights for IBM 727 Magnetic Tape
        n = ((LastTapeSelected==unit)            ? 1 : 0) +      // light_0  ; Select light = tape selected by external source
            (((mt_ready(unit)==1) && (mt_is==0)) ? 2 : 0) +      // light_1  ; Ready light = tape in automatic mode (off when rew/load)
            (sim_tape_wrp(uptr)                  ? 4 : 0) +      // light_2  ; File Protect = tape reel is write protected
            (mt_indicator_light                  ? 8 : 0);       // light_3  ; Tape Indicator = tape reached end of tape

        SetState(IBM650.MT_lights[unit], n);
    }
}

int PARAM_char_ww2       =    13;  // horizontal spacing between first pixel of a char and first pixel of next char in next char same line
int PARAM_char_hh2       =    23;  // height between top line of one char and top line of char in next text line
int PARAM_xPaperMargin   =    50;  // left and right margin on printed paper
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
    
    surface0=GetControlSurface(IBM650.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM650.PaperBackground, 0, &bg_ww, &bg_hh); // paper backgtound should have same width that paper control

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

    surface0=GetControlSurface(IBM650.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM650.PrinterCharSet, 0, &char_ww, &char_hh); 

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

    if (lptPrintOutCount == lptPrintOutDoneCount)  return; // nothing to update

    surface0=GetControlSurface(IBM650.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(IBM650.PaperBackground, 0, &bg_ww, &bg_hh); // paper background should have same width that paper control

    nLin = lptPrintOutCount - lptPrintOutDoneCount, // nLin = number of char lines to print
    nLinMax = paper_hh  / PARAM_char_hh2;           // calc how much char lines fits in printed page visible

    if (lptPrintOutDoneCount < 0) {
        // first page setup. copy background image
        hPaperBackgroundOffset = 6; 
        lpt_set_paper_background(0, nLinMax * PARAM_char_hh2);
        lptPrintOutDoneCount=0;
    }
    if (nLin<0) {
        // nLin < 0 when lptPrintOutCount has been set to zero beacise cdp detache -> new printing 
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
    SetState(IBM650.Paper, 0);
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
            SetState(IBM650.CtrlInfoPanel, 0); 
            if (bTapesVisible) SetState(IBM650.MT_InfoPanel, 0); 
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
    int WordTimeCountPerSec; 
    int InstrExecPerSec, n, fps; 
    int ips_unit; double ips; 
    int wps_unit; int wps; 
    uint32 msec; 

    // set dynamic contents of CtrlInfoPanel and MT_InfoPanel

    if (bShowInfo>1) {
        WordTimeCountPerSec =  InstrExecPerSec = fps = 0;
    } else {
        WordTimeCountPerSec =  Measure_CpuSpeed(1); 
        InstrExecPerSec = fps = 0;
        msec = Refresh_tnow - ShowInfoTm0; 
        if ((msec > 0) && (msec < 1000)) {
            n = Measure_CpuSpeed(2)- InstrExec0;
            if (n<0) n=0;
            InstrExecPerSec = n * 1000 / msec;
            n = Refresh_Frames_Count - FramesCount0;
            fps             = n * 1000 / msec;
            if (fps>60) fps=60;
            if (fps<0) fps=0;
        }
        ShowInfoTm0 = Refresh_tnow;
        InstrExec0  = Measure_CpuSpeed(2); 
        FramesCount0 = Refresh_Frames_Count; 
    }
    if (bOnlyInit) return; // only init values for Next call 

    if (WordTimeCountPerSec > 1000 * 1000) {
        wps = WordTimeCountPerSec / 1000000; wps_unit = 2; // unit MIPS if ips > 0.5 MIPS
    } else if (WordTimeCountPerSec > 50000) {
        wps = WordTimeCountPerSec / 1000; wps_unit = 1;    // unit KIPS if ips > 1.0 KPIS
    } else {
        wps = WordTimeCountPerSec; wps_unit = 0; 
    }

    if (InstrExecPerSec > 500 * 1000) {
        ips = InstrExecPerSec / 1000000.0; ips_unit = 2; // unit MIPS if ips > 0.5 MIPS
    } else if (InstrExecPerSec >1000) {
        ips = InstrExecPerSec / 1000.0; ips_unit = 1;    // unit KIPS if ips > 1.0 KPIS
    } else {
        ips = InstrExecPerSec; ips_unit = 0; 
    }

    sprintf(buf, "FPS: %d \n"
        "Cpu Speed: %d%s (x%0.1f) %0.1f %s\n"
        "Cards in read hopper: %d", 
        fps, 
        wps, (wps_unit == 2) ? "M" : (wps_unit == 1) ? "K" : "",
        WordTimeCountPerSec / 10416.0, 
        ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS",
        nCardInReadHopper);

    ShowInfo_DrawText(IBM650.CtrlInfoPanel, buf);

    if (bTapesVisible) {
        // set tape info panel
        double p_MT_used[6]; char c_MT[6];
        int i, n;
        char c; 

        for(i=0;i<6;i++) { 
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

        sprintf(buf, "          MT0     MT1     MT2     MT3     MT4     MT5 \n"
                     "record  %5d   %5d   %5d   %5d   %5d   %5d \n"
                     "%% unwind  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c",
            mt_info[0].numrec, mt_info[1].numrec, mt_info[2].numrec, mt_info[3].numrec, mt_info[4].numrec, mt_info[5].numrec, 
            p_MT_used[0], c_MT[0], p_MT_used[1], c_MT[1], p_MT_used[2], c_MT[2], 
            p_MT_used[3], c_MT[3], p_MT_used[4], c_MT[4], p_MT_used[5], c_MT[5]
        );
        
        ShowInfo_DrawText(IBM650.MT_InfoPanel, buf);
    }
}

void IBM650_Refresh(void)
{
    t_int64 d1, d2, d3, d4, OP;

    // set DISPLAY lighs
    Get_DISP_lights(&d1, &d2);
    SetStateWithIntensity(IBM650.Reg_DISP_0T9, d1);
    SetStateWithIntensity(IBM650.Reg_DISP_N, d2);

    // set OPERATION lighs
    OP=PR / D8; // get current operation code from Program Register
    Get_Digits_lights(&d1, &d2, OP, 2);
    SetStateWithIntensity(IBM650.Reg_OP_0T9, d1);
    SetStateWithIntensity(IBM650.Reg_OP_N, d2);

    // set ADDRESS lighs
    Get_Digits_lights(&d1, &d2, AR, 4);
    SetStateWithIntensity(IBM650.Reg_AR_0T9, d1);
    SetStateWithIntensity(IBM650.Reg_AR_N, d2);

    // set OPERATING & CHECKING lighs
    Get_Operating_lights(&d1, &d2);
    SetStateWithIntensity(IBM650.Reg_OPER, d1);
    SetStateWithIntensity(IBM650.Reg_CHK, d2);

    // set Display Switches based on CSW register value
    SetSwitchNum(3, &IBM650.DispSw[0], &IBM650.DispSwNum[0], &CSW, 10);
    // set sign switch
    if (CSW>0) {CSWneg=0; } else if (CSW<0) {CSWneg=1; }
    SetSwitch(3, IBM650.DispSwSgn, &CSWneg, "19"); 
    // CSWneg=0 -> Switch at '+' (pos 1), =1 -> Switch at '-' (pos 9), 

    // set programmed switch
    SetSwitch(3, IBM650.ProgSw, &CSWProgStop, "19");  
    // CSWProgStop=0 -> Switch at 'RUN' (pos 1), =1 -> Switch at 'STOP' (pos 9), 

    // set half cycle switch
    SetSwitch(3, IBM650.HalfCycleSw, &CSWHalfCycle, "19");  
    // CSWHalfCycle=0 -> Switch at 'RUN' (pos 1), =1 -> Switch at 'STOP' (pos 9), 

    // set Display Switches based on CSW register value
    SetSwitchNum(3, &IBM650.AddrSw[0], &IBM650.AddrSwNum[0], &CSWAddress, 4);
    // if address stop selected, use contents of Address Switch for AddresStop addr
    CSWAddrStop = (int) ((CSWControl==2) ? CSWAddress : -1);   

    // set control switch: CSWControl: 0=Run, 1=Manual Operation, 2=Address Stop
    SetSwitch(3, IBM650.CtrlSw, &CSWControl, "019");  
    // CSWControl=0 -> Switch at 'RUN' (pos 0), =1 -> Switch at 'Manual Operation' (pos 1), =2 -> Switch at 'Address Stop' (pos 9)

    // set display switch: CSWDisplay: 0=PR, 1=Read-Out, 2=Read-In, 3=AccLo, 4=AccUp, 5=Distrib
    SetSwitch(3, IBM650.DisplaySw, &CSWDisplay, "123789");  
    // CSWDisplay=0 -> Switch at 'PROGRAM REGISTER' (pos 1), 
    //           =1 -> Switch at 'READ-OUT STORAGE' (pos 2), 
    //           =2 -> Switch at 'READ-IN STORAGE' (pos 3), 
    //           =3 -> Switch at 'LOWER ACCUM' (pos 7), 
    //           =4 -> Switch at 'UPPER ACCUM' (pos 8), 
    //           =5 -> Switch at 'DISTRIB' (pos 9), 

    // set overflow switch
    SetSwitch(3, IBM650.OverflowSw, &CSWOverflowStop, "19");  
    // CSWOverflowStop=0 -> Switch at 'SENSE' (pos 1), =1 -> Switch at 'STOP' (pos 9), 

    // set overflow switch (not implemented)
    SetSwitch(3, IBM650.ErrorSw, &CSWErrorSw, "19");  
    // CSWErrorSw=0 -> Switch at 'SENSE' (pos 1), =1 -> Switch at 'STOP' (pos 9), 


    // IBM 653
    if (bIasVisible) {
        d1=1; d2 = (d1 << (IAS_TimingRing / 10));
        SetStateWithIntensity(IBM650.Reg_IAS_BLOCK_N, d1);

        Get_IAS_Operating_lights(&d1);
        SetStateWithIntensity(IBM650.Reg_IAS_OPER, d1);

        Get_IAS_lights(&d1, &d2);
        SetStateWithIntensity(IBM650.Reg_IAS_BLOCK_N, d1);
        SetStateWithIntensity(IBM650.Reg_IAS_WORD_N, d2);
    }
    // IBM 652
    if (bStorageVisible) {
        Get_MT_lights(&d1, &d2);
        SetStateWithIntensity(IBM650.Reg_MT_SEL_N, d1);
        SetStateWithIntensity(IBM650.Reg_MT_OPER, d2);

        Get_DS_lights(&d1, &d2, &d3, &d4);
        SetStateWithIntensity(IBM650.Reg_DS_UNIT_N, d1);
        SetStateWithIntensity(IBM650.Reg_DS_DISK_N, d2);
        SetStateWithIntensity(IBM650.Reg_DS_TRACK_N, d3);
        SetStateWithIntensity(IBM650.Reg_DS_ARM_N, d4);

        // set Display Switches based on CSW register value
        SetSwitchNum(3, &IBM650.DsUnitSw[0], &IBM650.DsUnitSwNum[0], &CSWDisplayUnit, 1);
        SetSwitchNum(3, &IBM650.DsArmSw[0],  &IBM650.DsArmSwNum[0],  &CSWDisplayArm, 1);

        Get_DS_Operating_lights(&d1);
        SetStateWithIntensity(IBM650.Reg_DS_OPER, d1);
    }
    // IBM 533
    if (bCardReadVisible) {
        Refresh_CardReadPunch(); 
    }
    if (bPrintOutVisible) {
        Refresh_PrintOut();
    }
    // IBM 727
    if (bTapesVisible) {
        Refresh_MagTape();
    }
    // IBM 355
    if (bRamacVisible) {
        Refresh_Ramac();
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
void IBM650_DropFile(int CId, char * FileName)
{
    extern t_addr   sim_card_input_hopper_count(UNIT *uptr);
    extern t_stat   cdr_attach(UNIT *, CONST char *);
    extern t_stat   mt_attach(UNIT * uptr, CONST char *file);
    int32  sv; 
    int n; 

    if (CId ==IBM650.Drop_InputDeckFile) {
        // drag and drop a file on card reader -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        // if already cards in reader, stack the new ones on the top
        if (sim_card_input_hopper_count(&cdr_unit[1]) > 0) sim_switches |= SWMASK ('S'); 
        cdr_attach(&cdr_unit[1], FileName);
        sim_switches=sv; 
    } else for (n=0;n<6;n++) if (CId == IBM650.Drop_MT_File[n]) {
        // drag and drop a file on tape -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        mt_attach(&mt_unit[n], FileName);
        sim_switches=sv; 
    }
}

// buttons for tape detach
void IBM650_OnClick_BTN2(void)
{
    extern t_stat   mt_detach(UNIT * uptr);
    int n; 
    int32  sv; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        return;
    }
    // set button to unpressed state (except Power Button, that remains pressed)
    SetState(CP_Click.CId, 0);

    // to detach a tape, must click on unload button
    for (n=0;n<6;n++) {
        if (CP_Click.CId == IBM650.MT_BTN_UnLoad[n]) {
            // click to detach tape
            if ((mt_unit[n].flags & UNIT_ATT) == 0) {
                // already detached. Do not detach again
                return; 
            }
            sv=sim_switches; // save current switches
            sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
            mt_detach(&mt_unit[n]);
            sim_switches=sv; 
        }
    }
}

// on click event handler for 10-position rotating switch
void IBM650_OnClick_Sw(void)
{
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> start switch rotation movement
        SetSwitchNum(1, &IBM650.DispSw[0], &IBM650.DispSwNum[0], &CSW, 10);
        // check Address switches
        SetSwitchNum(1, &IBM650.AddrSw[0], &IBM650.AddrSwNum[0], &CSWAddress, 4);
        if (bStorageVisible) {
            SetSwitchNum(1, &IBM650.DsUnitSw[0], &IBM650.DsUnitSwNum[0], &CSWDisplayUnit, 1);
            SetSwitchNum(1, &IBM650.DsArmSw[0],  &IBM650.DsArmSwNum[0],  &CSWDisplayArm, 1);
        }
        // check n-postion switches
        if ((CP_Click.CId == IBM650.DispSwSgn)   ||  // check display sign
            (CP_Click.CId == IBM650.ProgSw)      ||  // check programmed switch
            (CP_Click.CId == IBM650.HalfCycleSw) ||  // check Half Cycle switch
            (CP_Click.CId == IBM650.CtrlSw)      ||  // check control switch
            (CP_Click.CId == IBM650.DisplaySw)   ||  // check display switch
            (CP_Click.CId == IBM650.OverflowSw)  ||  // check Overflow switch
            (CP_Click.CId == IBM650.ErrorSw)) {      // check Error switch
            // press mouse button -> start switch rotation movement
            SetSwitch(1, CP_Click.CId, NULL, NULL) ;
            return;
        }
        // no know control clicked. Never should exit this way. Just in case
        return;
    } 

    // release mouse button -> do the action
    SetSwitchNum(2, &IBM650.DispSw[0], &IBM650.DispSwNum[0], &CSW, 10);
    SetSwitchNum(2, &IBM650.AddrSw[0], &IBM650.AddrSwNum[0], &CSWAddress, 4);
    if (bStorageVisible) {
        SetSwitchNum(2, &IBM650.DsUnitSw[0], &IBM650.DsUnitSwNum[0], &CSWDisplayUnit, 1);
        SetSwitchNum(2, &IBM650.DsArmSw[0],  &IBM650.DsArmSwNum[0],  &CSWDisplayArm, 1);
        if (CSWDisplayUnit > 3) CSWDisplayUnit=0;
        if (CSWDisplayArm > 2) CSWDisplayArm=0;
    }

    if (CP_Click.CId == IBM650.DispSwSgn) {
        SetSwitch(2, CP_Click.CId, &CSWneg, "19"); 
        if ((CSWneg==1) && (CSW>0)) CSW=-CSW; 
        if ((CSWneg==0) && (CSW<0)) CSW=-CSW;
    } else if (CP_Click.CId == IBM650.ProgSw) {
        SetSwitch(2, CP_Click.CId, &CSWProgStop, "19"); 
    } else if (CP_Click.CId == IBM650.HalfCycleSw) {
        SetSwitch(2, CP_Click.CId, &CSWHalfCycle, "19"); 
    } else if (CP_Click.CId == IBM650.CtrlSw) {
        SetSwitch(2, CP_Click.CId, &CSWControl, "019"); 
    } else if (CP_Click.CId == IBM650.DisplaySw) {
        SetSwitch(2, CP_Click.CId, &CSWDisplay, "123789"); 
    } else if (CP_Click.CId == IBM650.OverflowSw) {
        SetSwitch(2, CP_Click.CId, &CSWOverflowStop, "19"); 
    } else if (CP_Click.CId == IBM650.ErrorSw) {
        SetSwitch(2, CP_Click.CId, &CSWErrorSw, "19"); 
    }
}

// ob click event handler for 2-position button press
void IBM650_OnClick_BTN(void)
{
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        return;
    }
    // release mouse button -> do the action

    // set button to unpressed state (except Button Switch power, that remains pressed)
    if (CP_Click.CId != IBM650.BTN_Switch_Power) {
        SetState(CP_Click.CId, 0);
    }

    if ((CP_Click.CId == IBM650.BTN_Switch_Power) || (CP_Click.CId == IBM650.BTN_Power)) {
       // DoSCP stops the cpu to allow command to be executed by scp
       // if "set cpanel off" is sent via DoSCP, then no more commands can be stacked after.
       // this is because being cpanel off, pendign scp commands issued by cpanel are not processed       
       cpanel_stop_flag = 4; // request cpanel off and quit
       return;
    } else if (CP_Click.CId == IBM650.BTN_Transfer) {
        // CSWControl: 0=Run, 1=Manual Operation, 2=Address Stop
        if (CSWControl != 1) return; // this button only operates if Control Switch set to Manual operation
        AR=(uint16) CSWAddress;
    } else if (CP_Click.CId == IBM650.BTN_ProgStart) {        
        if (sim_is_running != 0) {
            // Prog Start while cpu running -> ignore
        } else if (CSWControl == 1) { // CSWControl: 0=Run, 1=Manual Operation, 2=Address Stop
            // Prog Start in Manual operation -> Displaying/Entering information from/to drum           
            if (CSWDisplay == 1) { // CSWDisplay: 1=read-out storage
                // Display information stored in address (passes thru distrib)
                if (0==ReadAddr(AR, &DIST, &DistNegativeZeroFlag)) {
                    StopReason = STOP_ADDR;
                }
            } else if (CSWDisplay == 2) { // CSWDisplay: 2=read-in storage
                // Enter information to address (passes thru distrib)
                DIST=CSW;
                DistNegativeZeroFlag= ((CSW==0) && (CSWneg)) ? 1:0;
                if (0==WriteAddr(AR, DIST, DistNegativeZeroFlag)) {
                    // invalid address
                    StopReason=STOP_ADDR;
                }
            }
        } else {
            // run the program if cpu stopped
            // if no CSWHalfCycle==0 (so in run mode) then disable interactive mode so when 
            // terminating prog with HLT or out of input cards the script can continue
            if (CSWHalfCycle==0) cpanel_interactive=0; 
            // if CSWControl == 2=Address Stop then set CSWAddrStop
            CSWAddrStop = (CSWControl == 2) ? (int) CSWAddress : -1; 
            // send command to scp
            DoSCP("go"); 
        }
    } else if (CP_Click.CId == IBM650.BTN_ProgStop) {
        if (sim_is_running) { 
            // request a cpu stop, without disabling interactive mode
            cpanel_stop_flag = 2;
            // and set interactive mode
            cpanel_interactive=1; 
            return; 
        }
    } else if ((CP_Click.CId == IBM650.BTN_ProgReset) || (CP_Click.CId == IBM650.BTN_ComputerReset)) {
        StopReason = 0; 
        StopIOError = 0;
        LastTapeSelected = -1;
        LastTapeSignal = 0;
        memset(&InterLockCount[0], 0, sizeof(InterLockCount)); // clear interlocks
        IMachineCycle = 'I';  // to force execution of I-cycle
        PR = PR % D8; // blank operation code in program register
        if (CSWControl == 1) {
            // Prog Reset in Manual operation -> Blanks to Program register and to Address register
            AR = 0;
        } else {
            // its a very early form of user initiated program interrupt
            AR = 8000; 
            // manual does not state what happends if computer reset is pressed while
            // cpu is running. What SimH does is to reset PR to NOP 8000 8000
            // (just to assure this is the OP/DA/IA used if button pressed on the middle of op execution)
            // request a cpu stop, without disabling interactive mode
            if (sim_is_running) {
                PR=AR* D4 + AR;
                cpanel_stop_flag = 2;
                // and set interactive mode
                cpanel_interactive=1; 
                return; 
            }
        }
        if (CP_Click.CId == IBM650.BTN_ComputerReset) {
            ACC[0] = ACC[1] = DIST = 0;
            OV = 0;
            AccNegativeZeroFlag = 0;
            DistNegativeZeroFlag = 0;
        }
    }
    if (CP_Click.CId == IBM650.BTN_AccumReset) {
        ACC[0] = ACC[1] = DIST = 0;
        OV = 0;
        AccNegativeZeroFlag = 0;
        DistNegativeZeroFlag = 0;
    }
    if (CP_Click.CId == IBM650.BTN_ErrorReset) {
        StopReason = 0; 
        StopIOError = 0;
        LastTapeSignal = 0;
        OV = 0;
    }
    // IBM650.BTN_ErrorSenseReset does nothing. 
}

// additional SCP commands to press/set switches

struct {
    int id;              // 0=button, >1 switch ident
    int * BTN_CId;       // addr of variable taht holds the control id number
    const char *name;    // name of button/switch to be used in scp command
} csw_btn_def[] = {
    { 0, &IBM650.BTN_Transfer,      "Transfer"},
    { 0, &IBM650.BTN_ProgStart,     "Program start"},
    { 0, &IBM650.BTN_ProgStart,     "start"},
    { 0, &IBM650.BTN_ProgStop,      "Program stop"},
    { 0, &IBM650.BTN_ProgStop,      "stop"},
    { 0, &IBM650.BTN_ProgReset,     "Program reset"},
    { 0, &IBM650.BTN_ComputerReset, "Computer reset"},
    { 0, &IBM650.BTN_AccumReset,    "Accumulator reset"},
    { 0, &IBM650.BTN_AccumReset,    "Error reset"},
    { 1, NULL,                      "Storage entry"},     
    { 1, NULL,                      "Storage"},     
    { 1, NULL,                      "Entry"},     
    { 1, NULL,                      "Console"},     
    { 2, NULL,                      "Programmed"},
    { 3, NULL,                      "Half cycle"},
    { 4, NULL,                      "Address selection"},
    { 4, NULL,                      "Address"},
    { 5, NULL,                      "Control"}, 
    { 6, NULL,                      "Display"},
    { 7, NULL,                      "Overflow"},
    { 8, NULL,                      "RAMAC Unit"},
    { 9, NULL,                      "Access arm"},
    {-1}
};

// implements the following SCP commands
//    switch "name" to "label"
//    switch "name" to nnnn 
//    press "name"
//    depress "name"
t_stat set_csw_btn_cmd(int32 flag, CONST char *cptr)
{
    char gbuf[4*CBUFSIZE];
    int n, id, result, neg, IsNeg; 
    int BTN_CId = 0;
    const char * cptr2 = NULL; 
    t_int64 d; 

    extern CONST char * parse_sgn(int *neg, CONST char *cptr); 
    extern CONST char * parse_n(t_int64 *d, CONST char *cptr, int n);

    if (cpanel_on == 0) {
        fprintf(stderr, "Control Panel is not visible. Cannot execute command\r\n"); 
        return SCPE_ARG;
    }

    cptr = get_glyph_quoted (cptr, gbuf, 0);   // get next param (name of button/switch, quoted)
    if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote
    n=strlen(gbuf);
    if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0; // remove trailing quote

    // find the button/swith name in table
    for(n=0;;n++) {
        id = csw_btn_def[n].id;
        if (id<0) {
            return sim_messagef (SCPE_ARG, "Unknown button/switch name \"%s\"\r\n", gbuf); 
        }
        if (sim_strncasecmp(gbuf, csw_btn_def[n].name, 32) != 0) continue; // loop if not this name
        // found, check button/switch vs command
        if ((flag == 0) && (id > 0)) {
            return sim_messagef (SCPE_ARG, "Unknown button name \"%s\"\r\n", gbuf); // because pressing a switch
        } else if ((flag == 1) && (id == 0)) {
            return sim_messagef (SCPE_ARG, "Unknown switch name \"%s\"\r\n", gbuf); // because switching a button
        }
        // found, get the data 
        if (csw_btn_def[n].BTN_CId) BTN_CId = *(csw_btn_def[n].BTN_CId); 
        break;
    }

    if (flag == 0) {
        // press button
        DoClickEvent(BTN_CId);
        return SCPE_OK;
    }

    // is a set switch .. to "label"
    // get TO separator
    cptr = get_glyph (cptr, gbuf, 0); // get TO
    if (strcmp(gbuf, "TO") != 0) return sim_messagef (SCPE_ARG, "Missign TO \"label\"\n");;

    // get label
    cptr2=cptr; // save label start
    cptr = get_glyph_quoted (cptr, gbuf, 0);   // get next param (name of switch label, quoted)
    if (gbuf[0] == '"') memcpy(&gbuf[0], &gbuf[1], sizeof(gbuf)-1); // remove leading quote, if any
    n=strlen(gbuf);
    if ((n>0) && (gbuf[n-1]=='"')) gbuf[n-1] = 0; // remove trailing quote, if any

    // set switch
    result=SCPE_OK;  
    if (id==1) {
        // switch "Storage entry" to [+/-] NN NNNN NNNN [+/-]
        //         Storage
        //         entry
        //         console
        IsNeg=0;
        cptr2 = parse_sgn(&neg, cptr2);
        if (neg) IsNeg = 1;
        cptr2 = parse_n(&d, cptr2, 10);
        cptr = parse_sgn(&neg, cptr2);
        if (neg) IsNeg = 1;
        if (IsNeg) d=-d;
        CSW = d; 
    } else if (id==2) {
        // switch programmed to run|stop
        // CSWProgStop=0 -> Switch at 'RUN', =1 -> Switch at 'STOP' 
        if (sim_strncasecmp(gbuf, "RUN",  32) == 0) CSWProgStop=0; else
        if (sim_strncasecmp(gbuf, "STOP", 32) == 0) CSWProgStop=1; else
        result=SCPE_ARG; 
    } else if (id==3) {
        // switch "half cycle" to half|run
        // CSWHalfCycle -> Console run/half cycle set to 0 for normal run, =1 to execute 1 half-cycle and stop
        if (sim_strncasecmp(gbuf, "RUN",  32) == 0) CSWHalfCycle=0; else
        if (sim_strncasecmp(gbuf, "HALF", 32) == 0) CSWHalfCycle=1; else
        result=SCPE_ARG; 
    } else if (id==4) {
        // switch "address selection" to NNNN
        //         address 
        parse_n(&d, gbuf, 4);
        CSWAddress = d; 
    } else if (id==5) {
        // switch control to run|"manual operation"|"address stop"
        //                        manual            
        // CSWControl 0=Run, 1=Manual Operation, 2=Address Stop
        if (sim_strncasecmp(gbuf, "RUN",              32) == 0)   CSWControl=0; else
        if (sim_strncasecmp(gbuf, "MANUAL OPERATION", 32) == 0)   CSWControl=1; else
        if (sim_strncasecmp(gbuf, "MANUAL",           32) == 0)   CSWControl=1; else
        if (sim_strncasecmp(gbuf, "ADDRESS STOP",     32) == 0)   CSWControl=2; else
        result=SCPE_ARG; 
    } else if (id==6) {
        // switch display to "program register"|"read-out storage"|"read-in storage"|"lower accumulator"|"upper accumulator"|distributor
        //                    pr                 read-out           read-in           acclo               accup              dist
        //                                                                            lower               upper
        // CSWDisplay 0=PR, 1=Read-Out, 2=Read-In, 3=AccLo, 4=AccUp, 5=Distrib
        if (sim_strncasecmp(gbuf, "PROGRAM REGISTER",  32) == 0)   CSWDisplay=0; else
        if (sim_strncasecmp(gbuf, "PR",                32) == 0)   CSWDisplay=0; else
        if (sim_strncasecmp(gbuf, "READ-OUT STORAGE",  32) == 0)   CSWDisplay=1; else
        if (sim_strncasecmp(gbuf, "READ-OUT",          32) == 0)   CSWDisplay=1; else
        if (sim_strncasecmp(gbuf, "READ-IN STORAGE",   32) == 0)   CSWDisplay=2; else
        if (sim_strncasecmp(gbuf, "READ-IN        ",   32) == 0)   CSWDisplay=2; else
        if (sim_strncasecmp(gbuf, "LOWER ACCUMULATOR", 32) == 0)   CSWDisplay=3; else
        if (sim_strncasecmp(gbuf, "ACCLO",             32) == 0)   CSWDisplay=3; else
        if (sim_strncasecmp(gbuf, "LOWER",             32) == 0)   CSWDisplay=3; else
        if (sim_strncasecmp(gbuf, "UPPER ACCUMULATOR", 32) == 0)   CSWDisplay=4; else
        if (sim_strncasecmp(gbuf, "ACCUP",             32) == 0)   CSWDisplay=4; else
        if (sim_strncasecmp(gbuf, "UPPER",             32) == 0)   CSWDisplay=4; else
        if (sim_strncasecmp(gbuf, "DISTRIBUTOR",       32) == 0)   CSWDisplay=5; else
        if (sim_strncasecmp(gbuf, "DIST",              32) == 0)   CSWDisplay=5; else
        result=SCPE_ARG; 
    } else if (id==7) {
        // switch overflow to sense|stop
        // CSWOverflowStop =0 -> Switch at 'SENSE' =1 -> Switch at 'STOP' (pos 9), 
        if (sim_strncasecmp(gbuf, "SENSE", 32) == 0) CSWOverflowStop=0; else
        if (sim_strncasecmp(gbuf, "STOP",  32) == 0) CSWOverflowStop=1; else
        result=SCPE_ARG; 
    } else if (id==8) {
        // switch "ramac unit" to 0|1|2|3
        n = gbuf[0] - '0';
        if ((n>=0) && (n<=3)) CSWDisplayUnit=n; else 
        result=SCPE_ARG; 
    } else if (id==9) {
        // switch "Access arm" to 0|1|2
        n = gbuf[0] - '0';
        if ((n>=0) && (n<=2)) CSWDisplayArm=n; else 
        result=SCPE_ARG; 
    } else return sim_messagef (SCPE_ARG, "Unknown switch name\r\n"); 
    if (result != SCPE_OK) {
        return sim_messagef (result, "Unknown switch label name \"%s\"\r\n", gbuf); 
    }
    return result;
}


// Matches the ifdef(CPANEL) at beginning of file
#endif


