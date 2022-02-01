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
   Jan-21    RSV     IBM NORC control panel support

   This module implements the following control panels:

   IBM NORC Console
   IBM NORC Display Panel
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "NORC_defs.h"	        
#include "sim_tape.h"
#include <math.h>

// cpu state
extern int                 U;                           // Saved Program counter during opcode execution
extern int                 V;                           // Address to be accesed in CRT memory
extern int                 M4,M6,M8;                    // address modifier registers
extern t_int64             IREG;                        // instruction register
extern t_int64             REG1, REG2;                  // Storage registers
extern int                 OV;                          // Overflow flag 
extern int                 ZR;                          // Zero result flag (on float result operation) 
extern int                 IA;                          // index Adjust flag 
extern int                 PRT;                         // printer ready flag 
extern int                 TCHK;                        // tape check flag 
extern int                 TEOF;                        // tape end of file flag 
extern int                 PRT;                         // printer ready flag 
extern int                 NSUBOP;                      // indicates current machine suboperation to be executed, 0=exec whole instruction
extern int                 ESUBOP;                      // already executed suboperations bitamap 
extern t_int64             CRT_BUS;                     // last value in crt bus to be read/write to CRT          
extern uint8               StopReason;                  // saved stop reason to be displayed in control panel
extern int                 P,Q;                         // decoded instr fields
extern int                 TU;                          // currently/last selected tape unit (TU) for tape operation

extern int Console_CurrentSubOp;     // already executed suboperations
extern int Console_TMOV;             // tape moving flag 
extern int Console_PADV;             // program advancing flag 
extern int Console_LiOV, Console_LiIA, Console_LiZR, Console_LiTEOF, Console_LiTCHK; // condition check flagextern int Console_Li64;             // console lights to signal the instruction 64..68has been executed
extern int Console_Li64;             // console lights to signal the instruction 64..68has been executed
extern int Console_Li74;             // console lights to signal the instruction has been executed
extern int Console_Sw64[5];          // selection switch =0 -> set to proceed, =1 -> set to stop
extern int Console_Sw74[6];          // selection switch for opcodes 74..79: =0 -> set to stop, =1 -> set to transfer, =2 -> set to off
extern int Console_LiPrtChk;         // console lights for priter check flag (because writting to 0001-0007 while print cycle in progress)
extern int Console_LiAddrChk;            // console lights for address check flag 
extern int Console_LiIndexChk, Console_LiSignChk, Console_LiZeroDivChk, Console_LiTEOT;        // console lights for index/sign/zero div/end of tape check flag 
extern int Console_Sw_ProgCHKStop;   // selection switch for PROGRAM CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
extern int Console_Sw_CRTCHKStop;    // selection switch for CRT CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
extern int Console_Sw_PrinterCHKStop;// selection switch for PRINTER CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
extern int Console_PRT_Instr;        // Indicate last printer opcode
extern int Console_Sw_Write_Output;  // selection switch for WRITE OUTPUT =0 -> set to off, =1 -> set to on
extern int Console_Sw_Floated_Index; // selection switch for FLOATED INDEX =0 -> set to off, =1 -> set to on
extern int Console_LiManualRead;     // indicates a manual read operation in progress

extern int Console_Sw_Reg1_CRTAddr;  // selection switch for Register 1 CRT Address NNNN
extern int Console_Sw_Reg2_CRTAddr;  // selection switch for Register 2 CRT Address NNNN
extern int Console_Sw_VEntry_Addr;   // selection switch for V Entry Address NNNN
extern int Console_Sw_KeyboardEntry; // selection switch =1 -> digits typed on keyboard goes to reg1, =2 -> to reg 2, =0 -> to none
extern int Console_Sw_SourceOfInstr; // selection switch =0 -> source of instr V, =1 -> source U, =2 -> source IREG
extern int Console_Tape_Addr[12];    // tape addr selection addr
extern int Console_Sw_TapeUnitSelector; // tape unit select for read/read backwds/rew button on console
extern int Console_Stall;            // reason for cpu stalled (not tape moving, not prog advancing)

extern int     CpuSpeed_Acceleration;       // cpu speed multiplier
extern int     Measure_CpuSpeed(int);       // return measured cpu speed 

// mt tape state
extern tapedata * TapeData;           // holds words read/writen to/from tape

// printout state
extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
extern int    lptPrintOutCount;


CP_DEF NORC_cp[];			// declare forwards
void NORC_Init(void);
void NORC_Done(void);
void NORC_Reset(void);
void NORC_TickIntensityCount(void);
void NORC_Refresh(void);
void NORC_DropFile(int CId, char * FileName);
void NORC_OnClick_Sw(void);
void NORC_OnClick_Sw2(void);
void NORC_OnClick_BTN(void);
void NORC_OnClick_BTN2(void);

// cpanel types constants
#define IS_NORC	1	// define CP type contants. must start on 1

// control panel available types
CP_TYPE cp_types = {
    NORC_cp, 
    &NORC_Init, &NORC_Done, &NORC_Reset, 
    &NORC_Refresh, &NORC_TickIntensityCount, &NORC_DropFile
};

// struct to hold the GUI controls Ids
static struct {
   int CtrlInfoPanel;
   // display panel registers. 
   // One control for each group of digits
   int DispNumber; 
   int Reg_V, Reg_U, Reg_M8, Reg_M6, Reg_M4;
   int Reg_IREG_D17, Reg_IREG_D1613, Reg_IREG_D1209, Reg_IREG_D0805, Reg_IREG_D0401;
   int Reg_REG1_D1817, Reg_REG1_D1613, Reg_REG1_D1209, Reg_REG1_D0805, Reg_REG1_D0401;
   int Reg_REG2_D17, Reg_REG2_D1613, Reg_REG2_D1209, Reg_REG2_D0805, Reg_REG2_D0401;
   // Console
   // Check Stops section - operative-reset 
   int LI_printer_Operative;
   int BTN_printer_reset, BTN_CRT_reset, BTN_Reset_InterLock; 
   // Check Stops-process switches
   int SW_printer, SW_CRT, SW_Program;
   // Check Stops section - printer   
   int LI_Printer_Check, LI_Special_Function; 
   int LI_Printer_Instr_A, LI_Printer_Instr_B, BTN_Carriage_A, BTN_Carriage_B;
   // Check Stops section - CRT
   int LI_Word_Check, BTN_CRT_Check_Reset;
   // Check Stops section - Program
   int LI_Index_Check, LI_Sign_Check;
   int BTN_Index_Reset, BTN_Sign_Reset, BTN_ProgCheck_Reset;
   int LI_Operation_Code_Check, LI_EOT_Check, LI_NonZeroDiv_Check, LI_Tape_Oprv_Check, LI_Printer_Oprv_Check;
   // program lights and resets
   int LI_6468, BTN_6468, LI_7479, BTN_7479;
   // condition stops 
   int SW_Overflow, SW_AdjustIndex, SW_ZeroResult, SW_EndOfFile, SW_TapeCheck;
   int LI_OV, LI_IA, LI_ZR, LI_TEOF, LI_TCHK;
   int BTN_OV, BTN_IA, BTN_ZR, BTN_TEOF, BTN_TCHK;
   // Emergency Off
   int BTN_Power_Off;
   // Prog advance Tape moving
   int LI_PADV_TMOV, LI_PSTOP, LI_CHKSTOP; 
   int BTN_SubOp_Reset, LI_Manual_Read;
   int SW_Source_of_intructions; 
   int BTN_Start, BTN_Stop, BTN_Operation_Start, BTN_Tape_Stop, BTN_SubOp_Start, BTN_SubOp_Stop;
   // V Entry
   int BTN_V_to_U, BTN_V_entry_to_V, BTN_U_to_V, BTN_U1_to_V;
   int BTN_V_to_M4, BTN_V_to_M6, BTN_V_to_M8, BTN_REG1_to_IREG;
   int SW_V_Entry[5], SWNum_V_Entry[5];  // 5 because controls used are [1] to [4] 
   // option switch
   int SW_74[6], SW_Floated_index, SW_Write_Output; 
   // reg1
   int SW_Reg1_CRT_Addr[5], SWNum_Reg1_CRT_Addr[5];  
   int BTN_Reg1_to_CRT, BTN_Reg1_from_CRT, BTN_Reg1_from_REG2, BTN_Reg1_Reset;
   int SW_Keyboard_Entry;
   // reg2
   int SW_Reg2_CRT_Addr[5], SWNum_Reg2_CRT_Addr[5];  
   int BTN_Reg2_to_CRT, BTN_Reg2_from_CRT, BTN_Reg2_from_REG1, BTN_Reg2_Reset;
   // tape address
   int SW_Tape_Addr[13], SWNum_Tape_Addr[13];
   // tape units resets
   int BTN_Tape_Reset[9];
   // tape units selector
   int BTN_Read_Forward, BTN_Read_Backward, BTN_Rewind; 
   int SW_Tape_Unit_Selector[2], SWNum_Tape_Unit_Selector[2];  
   // Indicator Panel
   // program advacing tape moving checls conditions
   int LI_IND_PADV_TMOV, LI_IND_PSTOP, LI_IND_CHKSTOP;
   int LI_IND_Printer_Check, LI_IND_CRT_Check, LI_IND_Program_Check;
   int LI_IND_OV, LI_IND_IA, LI_IND_ZR, LI_IND_TEOF, LI_IND_TCHK;
   // alarms
   int LI_IND_3600_Spots, LI_IND_NOT_PADV_TMOV; 
   // subops
   int LI_IND_SUBOP; 
   // indicator lights for tapes
   int LI_TU_Power_On[9], LI_TU_Operative[9], LI_TU_Moving, LI_TU_Rewind[9], LI_TU_EOT[9];
   // bus bit digits
   int LI_BITS_D0801, LI_BITS_D1709;
   // tape checks
   int LI_IND_WordLen, LI_IND_BitCount, LI_IND_BlockLen, LI_IND_GtThan9, LI_IND_BlockNum; 
   // Tapes
   int MT_InfoPanel; 
   int MT_panel[9], MT[9], MT_DoorHandle[9], MT_BTN_DoorOpen[9];
   int MT_head[9], MT_head_actuator[9];
   int MT_L[9], MT_R[9], MT_L_VacCol[9], MT_R_VacCol[9];
   int MT_VacColumn, MT_VacColMedium; 
   int Drop_MT_File[9];
   // printer
   int Paper, PaperBackground, PrinterCharSet;
} NORC = {0}; // must be init to zero

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF NORC_cp[] = {
    { &NORC.CtrlInfoPanel,            "CtrlInfoPanel",                     NULL},
    { &NORC.DispNumber,               "DispNumber",                        NULL},
    { &NORC.Reg_V,                    "Reg_V",                             NULL},
    { &NORC.Reg_U,                    "Reg_U",                             NULL},
    { &NORC.Reg_M8,                   "Reg_M8",                            NULL},
    { &NORC.Reg_M6,                   "Reg_M6",                            NULL},
    { &NORC.Reg_M4,                   "Reg_M4",                            NULL},
    { &NORC.Reg_IREG_D17,             "Reg_IREG_D17",                      NULL},
    { &NORC.Reg_IREG_D1613,           "Reg_IREG_D1613",                    NULL},
    { &NORC.Reg_IREG_D1209,           "Reg_IREG_D1209",                    NULL},
    { &NORC.Reg_IREG_D0805,           "Reg_IREG_D0805",                    NULL},
    { &NORC.Reg_IREG_D0401,           "Reg_IREG_D0401",                    NULL},
    { &NORC.Reg_REG1_D1817,           "Reg_REG1_D1817",                    NULL},
    { &NORC.Reg_REG1_D1613,           "Reg_REG1_D1613",                    NULL},
    { &NORC.Reg_REG1_D1209,           "Reg_REG1_D1209",                    NULL},
    { &NORC.Reg_REG1_D0805,           "Reg_REG1_D0805",                    NULL},
    { &NORC.Reg_REG1_D0401,           "Reg_REG1_D0401",                    NULL},
    { &NORC.Reg_REG2_D17,             "Reg_REG2_D17",                      NULL},
    { &NORC.Reg_REG2_D1613,           "Reg_REG2_D1613",                    NULL},
    { &NORC.Reg_REG2_D1209,           "Reg_REG2_D1209",                    NULL},
    { &NORC.Reg_REG2_D0805,           "Reg_REG2_D0805",                    NULL},
    { &NORC.Reg_REG2_D0401,           "Reg_REG2_D0401",                    NULL},
    { &NORC.LI_printer_Operative,     "LI_printer_Operative",              NULL},
    { &NORC.BTN_printer_reset,        "BTN_printer_reset",                 &NORC_OnClick_BTN},
    { &NORC.BTN_CRT_reset,            "BTN_CRT_reset",                     &NORC_OnClick_BTN},
    { &NORC.BTN_Reset_InterLock,      "BTN_Reset_InterLock",               &NORC_OnClick_BTN},
    { &NORC.SW_printer,               "SW_printer",                        &NORC_OnClick_Sw},
    { &NORC.SW_CRT,                   "SW_CRT",                            &NORC_OnClick_Sw},
    { &NORC.SW_Program,               "SW_Program",                        &NORC_OnClick_Sw},
    { &NORC.LI_Printer_Check,         "LI_Printer_Check",                  NULL},
    { &NORC.LI_Special_Function,      "LI_Special_Function",               NULL},
    { &NORC.LI_Printer_Instr_A,       "LI_Printer_Instr_A",                NULL},
    { &NORC.LI_Printer_Instr_B,       "LI_Printer_Instr_B",                NULL},
    { &NORC.BTN_Carriage_A,           "BTN_Carriage_A",                    &NORC_OnClick_BTN},
    { &NORC.BTN_Carriage_B,           "BTN_Carriage_B",                    &NORC_OnClick_BTN},
    { &NORC.LI_Word_Check,            "LI_Word_Check",                     NULL},
    { &NORC.BTN_CRT_Check_Reset,      "BTN_CRT_Check_Reset",               &NORC_OnClick_BTN},
    { &NORC.LI_Index_Check,           "LI_Index_Check",                    NULL},
    { &NORC.LI_Sign_Check,            "LI_Sign_Check",                     NULL},
    { &NORC.BTN_Index_Reset,          "BTN_Index_Reset",                   &NORC_OnClick_BTN},
    { &NORC.BTN_Sign_Reset,           "BTN_Sign_Reset",                    &NORC_OnClick_BTN},
    { &NORC.LI_Operation_Code_Check,  "LI_Operation_Code_Check",           NULL},
    { &NORC.LI_EOT_Check,             "LI_EOT_Check",                      NULL},
    { &NORC.LI_NonZeroDiv_Check,      "LI_NonZeroDiv_Check",               NULL},
    { &NORC.LI_Tape_Oprv_Check,       "LI_Tape_Oprv_Check",                NULL},
    { &NORC.LI_Printer_Oprv_Check,    "LI_Printer_Oprv_Check",             NULL},
    { &NORC.BTN_ProgCheck_Reset,      "BTN_ProgCheck_Reset",               &NORC_OnClick_BTN},
    { &NORC.LI_6468,                  "LI_6468",                           NULL},
    { &NORC.BTN_6468,                 "BTN_6468",                          &NORC_OnClick_BTN},
    { &NORC.LI_7479,                  "LI_7479",                           NULL},
    { &NORC.BTN_7479,                 "BTN_7479",                          &NORC_OnClick_BTN},
    { &NORC.SW_Overflow,              "SW_Overflow",                       &NORC_OnClick_Sw},
    { &NORC.SW_AdjustIndex,           "SW_AdjustIndex",                    &NORC_OnClick_Sw},
    { &NORC.SW_ZeroResult,            "SW_ZeroResult",                     &NORC_OnClick_Sw},
    { &NORC.SW_EndOfFile,             "SW_EndOfFile",                      &NORC_OnClick_Sw},
    { &NORC.SW_TapeCheck,             "SW_TapeCheck",                      &NORC_OnClick_Sw},
    { &NORC.LI_OV,                    "LI_OV",                             NULL},
    { &NORC.LI_IA,                    "LI_IA",                             NULL},
    { &NORC.LI_ZR,                    "LI_ZR",                             NULL},
    { &NORC.LI_TEOF,                  "LI_TEOF",                           NULL},
    { &NORC.LI_TCHK,                  "LI_TCHK",                           NULL},
    { &NORC.BTN_OV,                   "BTN_OV",                            &NORC_OnClick_BTN},
    { &NORC.BTN_IA,                   "BTN_IA",                            &NORC_OnClick_BTN},
    { &NORC.BTN_ZR,                   "BTN_ZR",                            &NORC_OnClick_BTN},
    { &NORC.BTN_TEOF,                 "BTN_TEOF",                          &NORC_OnClick_BTN},
    { &NORC.BTN_TCHK,                 "BTN_TCHK",                          &NORC_OnClick_BTN},
    { &NORC.LI_PADV_TMOV,             "LI_PADV_TMOV",                      NULL},
    { &NORC.LI_PSTOP,                 "LI_PSTOP",                          NULL},
    { &NORC.LI_CHKSTOP,               "LI_CHKSTOP",                        NULL},
    { &NORC.BTN_SubOp_Reset,          "BTN_SubOp_Reset",                   &NORC_OnClick_BTN},
    { &NORC.LI_Manual_Read,           "LI_Manual_Read",                    NULL},
    { &NORC.SW_Source_of_intructions, "SW_Source_of_intructions",          &NORC_OnClick_Sw},
    { &NORC.BTN_Start,                "BTN_Start",                         &NORC_OnClick_BTN},
    { &NORC.BTN_Stop,                 "BTN_Stop",                          &NORC_OnClick_BTN},
    { &NORC.BTN_Operation_Start,      "BTN_Operation_Start",               &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Stop,            "BTN_Tape_Stop",                     &NORC_OnClick_BTN},
    { &NORC.BTN_SubOp_Start,          "BTN_SubOp_Start",                   &NORC_OnClick_BTN},
    { &NORC.BTN_SubOp_Stop,           "BTN_SubOp_Stop",                    &NORC_OnClick_BTN},
    { &NORC.BTN_V_to_U,               "BTN_V_to_U",                        &NORC_OnClick_BTN},
    { &NORC.BTN_V_entry_to_V,         "BTN_V_entry_to_V",                  &NORC_OnClick_BTN},
    { &NORC.BTN_U_to_V,               "BTN_U_to_V",                        &NORC_OnClick_BTN},
    { &NORC.BTN_U1_to_V,              "BTN_U1_to_V",                       &NORC_OnClick_BTN},
    { &NORC.BTN_V_to_M4,              "BTN_V_to_M4",                       &NORC_OnClick_BTN},
    { &NORC.BTN_V_to_M6,              "BTN_V_to_M6",                       &NORC_OnClick_BTN},
    { &NORC.BTN_V_to_M8,              "BTN_V_to_M8",                       &NORC_OnClick_BTN},
    { &NORC.BTN_REG1_to_IREG,         "BTN_REG1_to_IREG",                  &NORC_OnClick_BTN},
    { &NORC.SW_V_Entry[4],            "SW_V_Entry_D4",                     &NORC_OnClick_Sw2},
    { &NORC.SW_V_Entry[3],            "SW_V_Entry_D3",                     &NORC_OnClick_Sw2},
    { &NORC.SW_V_Entry[2],            "SW_V_Entry_D2",                     &NORC_OnClick_Sw2},
    { &NORC.SW_V_Entry[1],            "SW_V_Entry_D1",                     &NORC_OnClick_Sw2},
    { &NORC.SWNum_V_Entry[4],         "SWNum_V_Entry_D4",                  NULL},
    { &NORC.SWNum_V_Entry[3],         "SWNum_V_Entry_D3",                  NULL},
    { &NORC.SWNum_V_Entry[2],         "SWNum_V_Entry_D2",                  NULL},
    { &NORC.SWNum_V_Entry[1],         "SWNum_V_Entry_D1",                  NULL},
    { &NORC.SW_74[0],                 "SW_74",                             &NORC_OnClick_Sw},
    { &NORC.SW_74[1],                 "SW_75",                             &NORC_OnClick_Sw},
    { &NORC.SW_74[2],                 "SW_76",                             &NORC_OnClick_Sw},
    { &NORC.SW_74[3],                 "SW_77",                             &NORC_OnClick_Sw},
    { &NORC.SW_74[4],                 "SW_78",                             &NORC_OnClick_Sw},
    { &NORC.SW_74[5],                 "SW_79",                             &NORC_OnClick_Sw},
    { &NORC.SW_Floated_index,         "SW_Floated_index",                  &NORC_OnClick_Sw},
    { &NORC.SW_Write_Output,          "SW_Write_Output",                   &NORC_OnClick_Sw},
    { &NORC.SW_Reg1_CRT_Addr[4],      "SW_Reg1_CRT_Addr_D4",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg1_CRT_Addr[3],      "SW_Reg1_CRT_Addr_D3",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg1_CRT_Addr[2],      "SW_Reg1_CRT_Addr_D2",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg1_CRT_Addr[1],      "SW_Reg1_CRT_Addr_D1",               &NORC_OnClick_Sw2},
    { &NORC.SWNum_Reg1_CRT_Addr[4],   "SWNum_Reg1_CRT_Addr_D4",            NULL},
    { &NORC.SWNum_Reg1_CRT_Addr[3],   "SWNum_Reg1_CRT_Addr_D3",            NULL},
    { &NORC.SWNum_Reg1_CRT_Addr[2],   "SWNum_Reg1_CRT_Addr_D2",            NULL},
    { &NORC.SWNum_Reg1_CRT_Addr[1],   "SWNum_Reg1_CRT_Addr_D1",            NULL},
    { &NORC.BTN_Reg1_to_CRT,          "BTN_Reg1_to_CRT",                   &NORC_OnClick_BTN},
    { &NORC.BTN_Reg1_from_CRT,        "BTN_Reg1_from_CRT",                 &NORC_OnClick_BTN},
    { &NORC.BTN_Reg1_from_REG2,       "BTN_Reg1_from_REG2",                &NORC_OnClick_BTN},
    { &NORC.BTN_Reg1_Reset,           "BTN_Reg1_Reset",                    &NORC_OnClick_BTN},
    { &NORC.SW_Keyboard_Entry,        "SW_Keyboard_Entry",                 &NORC_OnClick_Sw},
    { &NORC.BTN_Reg2_to_CRT,          "BTN_Reg2_to_CRT",                   &NORC_OnClick_BTN},
    { &NORC.BTN_Reg2_from_CRT,        "BTN_Reg2_from_CRT",                 &NORC_OnClick_BTN},
    { &NORC.BTN_Reg2_from_REG1,       "BTN_Reg2_from_REG1",                &NORC_OnClick_BTN},
    { &NORC.BTN_Reg2_Reset,           "BTN_Reg2_Reset",                    &NORC_OnClick_BTN},
    { &NORC.SW_Reg2_CRT_Addr[4],      "SW_Reg2_CRT_Addr_D4",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg2_CRT_Addr[3],      "SW_Reg2_CRT_Addr_D3",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg2_CRT_Addr[2],      "SW_Reg2_CRT_Addr_D2",               &NORC_OnClick_Sw2},
    { &NORC.SW_Reg2_CRT_Addr[1],      "SW_Reg2_CRT_Addr_D1",               &NORC_OnClick_Sw2},
    { &NORC.SWNum_Reg2_CRT_Addr[4],   "SWNum_Reg2_CRT_Addr_D4",            NULL},
    { &NORC.SWNum_Reg2_CRT_Addr[3],   "SWNum_Reg2_CRT_Addr_D3",            NULL},
    { &NORC.SWNum_Reg2_CRT_Addr[2],   "SWNum_Reg2_CRT_Addr_D2",            NULL},
    { &NORC.SWNum_Reg2_CRT_Addr[1],   "SWNum_Reg2_CRT_Addr_D1",            NULL},
    { &NORC.SW_Tape_Addr[1],          "SW_Tape_Addr_01",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[2],          "SW_Tape_Addr_02",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[3],          "SW_Tape_Addr_03",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[4],          "SW_Tape_Addr_04",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[5],          "SW_Tape_Addr_05",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[6],          "SW_Tape_Addr_06",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[7],          "SW_Tape_Addr_07",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[8],          "SW_Tape_Addr_08",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[9],          "SW_Tape_Addr_09",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[10],         "SW_Tape_Addr_10",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[11],         "SW_Tape_Addr_11",                   &NORC_OnClick_Sw2},
    { &NORC.SW_Tape_Addr[12],         "SW_Tape_Addr_12",                   &NORC_OnClick_Sw2},
    { &NORC.SWNum_Tape_Addr[1],       "SWNum_Tape_Addr_01",                NULL},
    { &NORC.SWNum_Tape_Addr[2],       "SWNum_Tape_Addr_02",                NULL},
    { &NORC.SWNum_Tape_Addr[3],       "SWNum_Tape_Addr_03",                NULL},
    { &NORC.SWNum_Tape_Addr[4],       "SWNum_Tape_Addr_04",                NULL},
    { &NORC.SWNum_Tape_Addr[5],       "SWNum_Tape_Addr_05",                NULL},
    { &NORC.SWNum_Tape_Addr[6],       "SWNum_Tape_Addr_06",                NULL},
    { &NORC.SWNum_Tape_Addr[7],       "SWNum_Tape_Addr_07",                NULL},
    { &NORC.SWNum_Tape_Addr[8],       "SWNum_Tape_Addr_08",                NULL},
    { &NORC.SWNum_Tape_Addr[9],       "SWNum_Tape_Addr_09",                NULL},
    { &NORC.SWNum_Tape_Addr[10],      "SWNum_Tape_Addr_10",                NULL},
    { &NORC.SWNum_Tape_Addr[11],      "SWNum_Tape_Addr_11",                NULL},
    { &NORC.SWNum_Tape_Addr[12],      "SWNum_Tape_Addr_12",                NULL},
    { &NORC.BTN_Tape_Reset[1],        "BTN_Tape_1_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[2],        "BTN_Tape_2_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[3],        "BTN_Tape_3_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[4],        "BTN_Tape_4_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[5],        "BTN_Tape_5_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[6],        "BTN_Tape_6_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[7],        "BTN_Tape_7_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Tape_Reset[8],        "BTN_Tape_8_Reset",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Read_Forward,         "BTN_Read_Forward",                  &NORC_OnClick_BTN},
    { &NORC.BTN_Read_Backward,        "BTN_Read_Backward",                 &NORC_OnClick_BTN},
    { &NORC.BTN_Rewind,               "BTN_Rewind",                        &NORC_OnClick_BTN},
    { &NORC.SW_Tape_Unit_Selector[1], "SW_Tape_Unit_Selector",             &NORC_OnClick_Sw2},
    { &NORC.SWNum_Tape_Unit_Selector[1],   "SWNum_Tape_Unit_Selector",     NULL},
    { &NORC.BTN_Power_Off,            "BTN_Power_Off",                     &NORC_OnClick_BTN},    
    { &NORC.LI_IND_PADV_TMOV,         "LI_IND_PADV_TMOV",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_PSTOP,             "LI_IND_PSTOP",                      NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_CHKSTOP,           "LI_IND_CHKSTOP",                    NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_Printer_Check,     "LI_IND_Printer_Check",              NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_CRT_Check,         "LI_IND_CRT_Check",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_Program_Check,     "LI_IND_Program_Check",              NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_OV,                "LI_IND_OV",                         NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_IA,                "LI_IND_IA",                         NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_ZR,                "LI_IND_ZR",                         NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_TEOF,              "LI_IND_TEOF",                       NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_TCHK,              "LI_IND_TCHK",                       NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_3600_Spots,        "LI_IND_3600_Spots",                 NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_NOT_PADV_TMOV,     "LI_IND_NOT_PADV_TMOV",              NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_SUBOP,             "LI_IND_SUBOP",                      NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[1],        "LI_TU1_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[2],        "LI_TU2_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[3],        "LI_TU3_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[4],        "LI_TU4_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[5],        "LI_TU5_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[6],        "LI_TU6_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[7],        "LI_TU7_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Power_On[8],        "LI_TU8_Power_On",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[1],       "LI_TU1_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[2],       "LI_TU2_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[3],       "LI_TU3_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[4],       "LI_TU4_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[5],       "LI_TU5_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[6],       "LI_TU6_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[7],       "LI_TU7_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Operative[8],       "LI_TU8_Operative",                  NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Moving,             "LI_TU_Moving",                      NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[1],          "LI_TU1_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[2],          "LI_TU2_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[3],          "LI_TU3_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[4],          "LI_TU4_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[5],          "LI_TU5_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[6],          "LI_TU6_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[7],          "LI_TU7_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_Rewind[8],          "LI_TU8_Rewind",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[1],             "LI_TU1_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[2],             "LI_TU2_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[3],             "LI_TU3_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[4],             "LI_TU4_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[5],             "LI_TU5_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[6],             "LI_TU6_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[7],             "LI_TU7_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_TU_EOT[8],             "LI_TU8_EOT",                        NULL, "IndicatorPanel/1" },
    { &NORC.LI_BITS_D0801,            "LI_BITS_D0801",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_BITS_D1709,            "LI_BITS_D1709",                     NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_WordLen,           "LI_IND_WordLen",                    NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_BitCount,          "LI_IND_BitCount",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_BlockLen,          "LI_IND_BlockLen",                   NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_GtThan9,           "LI_IND_GtThan9",                    NULL, "IndicatorPanel/1" },
    { &NORC.LI_IND_BlockNum,          "LI_IND_BlockNum",                   NULL, "IndicatorPanel/1" },

    { &NORC.MT_InfoPanel,             "MT_InfoPanel",                      NULL, "tape/1"  },
    { &NORC.MT_panel[1],              "MT_1_panel",                        NULL, "tape/1"  },
    { &NORC.MT[1],                    "MT_1",                              NULL, "tape/1"  },
    { &NORC.MT_L[1],                  "MT_1_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[1],                  "MT_1_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[1],               "MT_1_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[1],      "MT_1_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[1],          "Drop_MT1_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[1],       "MT_1_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[1],       "MT_1_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[1],     "MT_1_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[1],   "MT_1_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[2],              "MT_2_panel",                        NULL, "tape/1"  },
    { &NORC.MT[2],                    "MT_2",                              NULL, "tape/1"  },
    { &NORC.MT_L[2],                  "MT_2_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[2],                  "MT_2_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[2],               "MT_2_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[2],      "MT_2_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[2],          "Drop_MT2_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[2],       "MT_2_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[2],       "MT_2_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[2],     "MT_2_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[2],   "MT_2_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[3],              "MT_3_panel",                        NULL, "tape/1"  },
    { &NORC.MT[3],                    "MT_3",                              NULL, "tape/1"  },
    { &NORC.MT_L[3],                  "MT_3_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[3],                  "MT_3_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[3],               "MT_3_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[3],      "MT_3_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[3],          "Drop_MT3_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[3],       "MT_3_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[3],       "MT_3_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[3],     "MT_3_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[3],   "MT_3_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[4],              "MT_4_panel",                        NULL, "tape/1"  },
    { &NORC.MT[4],                    "MT_4",                              NULL, "tape/1"  },
    { &NORC.MT_L[4],                  "MT_4_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[4],                  "MT_4_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[4],               "MT_4_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[4],      "MT_4_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[4],          "Drop_MT4_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[4],       "MT_4_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[4],       "MT_4_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[4],     "MT_4_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[4],   "MT_4_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[5],              "MT_5_panel",                        NULL, "tape/1"  },
    { &NORC.MT[5],                    "MT_5",                              NULL, "tape/1"  },
    { &NORC.MT_L[5],                  "MT_5_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[5],                  "MT_5_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[5],               "MT_5_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[5],      "MT_5_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[5],          "Drop_MT5_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[5],       "MT_5_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[5],       "MT_5_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[5],     "MT_5_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[5],   "MT_5_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[6],              "MT_6_panel",                        NULL, "tape/1"  },
    { &NORC.MT[6],                    "MT_6",                              NULL, "tape/1"  },
    { &NORC.MT_L[6],                  "MT_6_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[6],                  "MT_6_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[6],               "MT_6_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[6],      "MT_6_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[6],          "Drop_MT6_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[6],       "MT_6_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[6],       "MT_6_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[6],     "MT_6_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[6],   "MT_6_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[7],              "MT_7_panel",                        NULL, "tape/1"  },
    { &NORC.MT[7],                    "MT_7",                              NULL, "tape/1"  },
    { &NORC.MT_L[7],                  "MT_7_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[7],                  "MT_7_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[7],               "MT_7_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[7],      "MT_7_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[7],          "Drop_MT7_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[7],       "MT_7_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[7],       "MT_7_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[7],     "MT_7_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[7],   "MT_7_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_panel[8],              "MT_8_panel",                        NULL, "tape/1"  },
    { &NORC.MT[8],                    "MT_8",                              NULL, "tape/1"  },
    { &NORC.MT_L[8],                  "MT_8_L",                            NULL, "tape/1"  },
    { &NORC.MT_R[8],                  "MT_8_R",                            NULL, "tape/1"  },
    { &NORC.MT_head[8],               "MT_8_head",                         NULL, "tape/1"  },
    { &NORC.MT_head_actuator[8],      "MT_8_head_actuator",                NULL, "tape/1"  },
    { &NORC.Drop_MT_File[8],          "Drop_MT8_File",                     NULL, "tape/1"  },
        { &NORC.MT_L_VacCol[8],       "MT_8_L_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_R_VacCol[8],       "MT_8_R_VacCol",                     NULL, "tape/1"  },
        { &NORC.MT_DoorHandle[8],     "MT_8_DoorHandle",                   &NORC_OnClick_BTN2, "tape/1"  },
        { &NORC.MT_BTN_DoorOpen[8],   "MT_8_BTN_DoorOpen",                 &NORC_OnClick_BTN, "tape/1"  },
    { &NORC.MT_VacColumn,             "MT_VacColumn",                      NULL, "tape/1"  },
    { &NORC.MT_VacColMedium,          "MT_VacColMedium",                   NULL, "tape/1"  },

    { &NORC.Paper,                    "Paper",                             NULL, "printer/1"  },
    { &NORC.PaperBackground,          "PaperBackground",                   NULL, "printer/1"  },
    { &NORC.PrinterCharSet,           "PrinterCharSet",                    NULL, "printer/1"  },

    { NULL }  
};

int bIndicatorPanel;     // indicator panel visible
int bTapesVisible;       // tape visible
int bPrintOutVisible;   // printout visible

// animation state vars (for dynamic state draw)
int digit_occurrences[8][18][10]; // number of occurrences of 0..9 on each digit of register
int * digits_rgb = NULL; // unpacked rgb components of pixels of display panel digits 0..9
int * digit_rgb = NULL; // unpacked rgb components of pixels of one display panel digits
int digit_npixels = 0; // number of pixels in each digit control image bitmap
int digit_ww = 0;     // wifth in pixels of digit control image bitmap

// for main cpu
int CpuSpeed_Acceleration_save = 0; // save value during HotKey ^F Max Speed
int bShowInfo                  = 0; // flag to show info for ^I 
uint32 ShowInfoTm0             = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                 = 0; // instr executed on ShowInfoTm0
int FramesCount0               = 0; // num of frames on ShowInfoTm0

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
      int MT_Reel_Amount, L_ang_inc, R_ang_inc, MT_head_actuator, L_VacCol_inc, R_VacCol_inc; 
   } seq[MT_anim_sequence_len];
} mtcab[9];
int bTapeAnimInProgress;                    // signals tape medium/reels are moving

// for printer printout
int lptPrintOutDoneCount       = -1; // number of lines already printed on cpanel paper
int hPaperBackgroundOffset;          // offset of background image on paper image

// load digits_rgb with unpacked rgb components of pixels of display panel digits 0..9
void load_digits_rgb(void)
{
    uint32 * surface; 
    int ww,hh,x,y,r,g,b, d; // digit width and height
    uint32 col; 
    int * np; 
    
    GetControlSurface(NORC.DispNumber, 0, &ww, &hh); // get digit size

    // size for unpacked pixels: ww*hh pixels x 11 digits (0..9 + blank) x 3 (red, blue, green)
    digits_rgb = (int *)malloc (ww*hh*11*3*sizeof(*digits_rgb)); 
    // size for 1 unpacked digit pixels: ww*hh pixels x 1 digit x 3 (red, blue, green)
    digit_rgb  = (int *)malloc (ww*hh* 1*3*sizeof(*digits_rgb)); 
    digit_npixels = ww*hh; 
    digit_ww = ww; 

    // load pixels
    np=digits_rgb;
    for (d=0; d<11; d++)  {
        surface = GetControlSurface(NORC.DispNumber, d, &ww, &hh); // get digit surface
        for (y=0; y<hh;y++) {
            for (x=0; x<ww;x++) {
                col = surface[x + y * ww];
                get_surface_rgb_color(col, &r,&g, &b); 
                *np++ = r;  // store individual color components of pixel
                *np++ = g; 
                *np++ = b; 
            }
        }
    }
}



// Control Panel callbacks
void NORC_Init(void)
{

    bIndicatorPanel = bTapesVisible = bPrintOutVisible = 0;

    if (IsOption("IndicatorPanel")) {
        bIndicatorPanel = 1; 
    }
    if (IsOption("Tape")) {
        bTapesVisible = 1; 
    }
    if (IsOption("Printer")) {
        bPrintOutVisible = 1; 
    }

    memset(digit_occurrences, 0, sizeof(digit_occurrences));
    // load digits_rgb with unpacked rgb components of pixels of display panel digits 0..9
    load_digits_rgb(); 

    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }
    SetTapeData(0, 0); // Init TapeData

    memset (mtcab, 0, sizeof mtcab);
    bTapeAnimInProgress=0;

    lptPrintOutDoneCount=-1; // number of lines already printed on cpanel paper
}

void NORC_Done(void)
{
    if (digits_rgb) free(digits_rgb); 
    if (digit_rgb)  free(digit_rgb); 
    SetTapeData(1, 0); // done TapeData
}

void NORC_Reset(void)
{
}

// count number of occurrences of 0..9 on each digit of register
void count_digit_occurrences(int nreg, t_int64 reg, int nDigits)
{
    int n, nd, d, bitcount; 

    bitcount = 0;
    nd = nDigits; if (nd>16) nd=16; 
    for (n=0; n<nd; n++) {
        d = reg % 10; reg = reg / 10; 
        digit_occurrences[nreg][n][d] += 1;
        bitcount += (d & 1) + ((d >> 1) & 1) + ((d >> 2) & 1) + ((d >> 3) & 1);
        if (nDigits == 17) {
            // simulate more or less the shifting of digits thru reg1
            d = (GlobalTicksCount & 3);
            digit_occurrences[nreg][16][d] += 1;
            digit_occurrences[nreg][17][0] += 1;
        }
    }
    if (nDigits==16) {
        d = 3 - (bitcount & 3);
        digit_occurrences[nreg][16][d] += 1;
    } else if (nDigits==17) {
        d = 3 - (bitcount & 3);
        digit_occurrences[nreg][16][d] += 16;
        digit_occurrences[nreg][17][d] += 16; // so the final bitcount on digit 17 is not overprinted with zeroes
    }
}

// lights for program advancing and tape moving
// bit0 = PMOV, bit1=TADV, bit2=Starting Operation, bit3=Ending operation
// if sim_is_running -> PADV or TMOV lit (but not both), starting/ending operation lit
//        is NOT running -> PADV/TMOV NOT lit, starting lit if decode in progress, end lit if terminate subop done
//                                             if starting remaints lit means instr stalled because printer cycle/tape busy
void Get_PADV_TMOV_lights(int * n)
{
    *n = (((Console_PADV) && (sim_is_running) && (Console_TMOV==0)) ? 1:0) + 
         (((Console_TMOV) && (sim_is_running)) ? 2:0) + 
         (((NSUBOP == 2) || (sim_is_running)) ? 4:0) +    // starting Operation = executing decode subop
         (((NSUBOP == 1) || (sim_is_running)) ? 8:0);     // ending Operation = executing terminate subop
}

// get the register values while data is being trasferred to/from tape
void Get_TapeData(t_int64 * reg1, t_int64 * reg2, int * v)
{
    int i; 
    
    *reg1 = REG1; // default values
    *reg2 = REG2; 
    *v=V; 

    if (TapeData==NULL) return; 
    i = TapeData->index; 
    if ((i>=TapeData->Count) || (i>=MAX_TAPEDATA)) return; 
    while (1) {
        if (GlobalTicksCount <= TapeData->TicksCount[i]) {
            *reg1 = TapeData->REG1[i]; 
            *reg2 = TapeData->REG2[i]; 
            *v = TapeData->V[i]; 
            return; 
        }
        TapeData->index += 1; 
        i = TapeData->index; 
        if ((i>=TapeData->Count) || (i>=MAX_TAPEDATA)) return; 
    }

}

// get the bus values while data is being trasferred to/from CRT
void Get_BusData(t_int64 * d0108, t_int64 * d1709)
{
    t_int64 data, bits;
    int i, d, bitcount; 

    data = CRT_BUS; 
    bits=0;
    bitcount=0;
    for (i=0; i<8; i++) {
        d = data % 10; data = data / 10; 
        bits = bits + (d << (4*i)); 
        bitcount += (d & 1) + ((d >> 1) & 1) + ((d >> 2) & 1) + ((d >> 3) & 1);
    }
    *d0108=bits;
    bits=0;
    for (i=0; i<8; i++) {
        d = data % 10; data = data / 10; 
        bits = bits + (d << (4*i)); 
        bitcount += (d & 1) + ((d >> 1) & 1) + ((d >> 2) & 1) + ((d >> 3) & 1);
    }
    data = 3 - (bitcount & 3);
    bits = bits + (data << 32); 
    *d1709 = bits; 
}

// get the tape moving lights
void Get_TapeMoving(t_int64 *tu)
{
    *tu = (Console_TMOV) ? (1 << (TU-1)) : 0;
}

void NORC_TickIntensityCount(void)
{
    int n, v; 
    t_int64 reg1, reg2; 

    // count for each register, each digit value so we can draw a mixed digits image on refresh

    // count number of occurrences of 0..9 on each digit of register to be able to calc on 
    // refresh callback the intensity on light on each bits
    count_digit_occurrences(0, IREG, 16);          
    if (Console_TMOV) {
        // if tape moving, get the register values at current point of time
        // when tape is moving, REG1 shows an additional 17th digit
        Get_TapeData(&reg1, &reg2, &v); 
        CRT_BUS=reg1;  
        count_digit_occurrences(1, reg1, 17);
        count_digit_occurrences(2, reg2, 16);
        count_digit_occurrences(3, v, 4);
    } else {
        count_digit_occurrences(1, REG1, 16);
        count_digit_occurrences(2, REG2, 16);
        count_digit_occurrences(3, V, 4);
    }
    count_digit_occurrences(4, U, 4);
    count_digit_occurrences(5, M4, 4);
    count_digit_occurrences(6, M6, 4);
    count_digit_occurrences(7, M8, 4);

    // lights for program advancing and tape moving
    Get_PADV_TMOV_lights(&n);
    TickCount(NORC.LI_PADV_TMOV, n);
    if (bIndicatorPanel) {
        TickCount(NORC.LI_IND_PADV_TMOV, n);
        // program not advancing and tape not moving
        TickCount(NORC.LI_IND_NOT_PADV_TMOV, Console_Stall ? 1:0);
        // subops
        TickCount(NORC.LI_IND_SUBOP, Console_CurrentSubOp + (sim_is_running ? 1:0));
        // parallel transmission bus bits light
        Get_BusData(&reg1, &reg2);
        TickCount(NORC.LI_BITS_D0801, reg1);
        TickCount(NORC.LI_BITS_D1709, reg2);
        // tape moving
        Get_TapeMoving(&reg1);
        TickCount(NORC.LI_TU_Moving, reg1); 
    }

}

// draw a CPU register in display panel
// CId = control where digits (1 to 4) are to be stored
// n1, n2 = start and end digits from CPU reg to be draw (1..16). Note that d2-d1+1 <= 4
// nreg = entry in digit_occurrences array to be used
void DrawReg(int CId, int n1, int n2, int nreg)
{
    int n, tocc, occ, d; 
    int x,y,ww,hh, r, g, b, c;
    int p;
    uint32 * surface; 
    int * np; 

    for (n=n1; n<=n2; n++) {
         // clear unpacked rgb componentes of digit to draw
         memset(digit_rgb, 0, digit_npixels * 3*sizeof(*digits_rgb)); 
         // accumulate on each pixel componenet color depending of number of ocurrences of 0..9
         tocc = 0;
         for (d=0;d<10; d++) {
             occ = digit_occurrences[nreg][n-1][d];
             if (occ == 0) continue; // because no ocurrences of digit d (0..9) in position n (1..16)
             tocc += occ;
             np = &digits_rgb[digit_npixels * d * 3]; 
             for (p=0;p<digit_npixels*3;p++) {
                 c = *np++; // get r/g/b of pixel for digit d (0..9)
                 digit_rgb[p] += c * occ;   // add to resulting digit to be draw
             }
         }
         if (tocc > 1) {
            // normalize color values
             for (p=0;p<digit_npixels*3;p++) {
                 c = digit_rgb[p]; 
                 digit_rgb[p] = c / tocc;   
             }
         } if (tocc == 0) {
             // no ocurrences. use blank
             d = 10;
             np = &digits_rgb[digit_npixels * d * 3]; 
             for (p=0;p<digit_npixels*3;p++) digit_rgb[p] = *np++;   // copy digit
         }
         // now draw to control surface
         surface = GetControlSurface(CId, 0, &ww, &hh); // get surface to draw to
         np = digit_rgb; 
         for (y=0; y<hh; y++) {
             p  = y * ww + digit_ww * (3-(n-n1));  // pixel destination
             for (x=0; x<digit_ww; x++) {
                 r = *np++; 
                 g = *np++; 
                 b = *np++; 
                 surface[p++] = surface_rgb_color(r, g, b);
             }
         }
    }

    cpanel_ControlRedrawNeeded=1; 
    SetState(CId, 0);      
}

// handle rotating swtich for multi-selection
// Mode = 1 -> to be used when mouse button pressed -> start switch rotation movement
//      = 2 -> to be used when mouse button released -> set end switch rotation and set varible with new value
//      = 3 -> to be used in refresh -> read varible with current value and set switch position 
// swCId -> is the switch Control Id 
// CSWvar = pointer to variable that holds value corresponding to switches. Cannot be NULL
// sAllowedPos = postions of switch allowed (e.j witch is "UL" -> when switch at pos "Up" then CSWVar set to 0, pos "Left" then CSWVar set to 1
void SetSwitch(int Mode, int swCId, int * CSWvar, char * sAllowedPos) 
{
    int n; 
    char c; 

    if (Mode == 1) {
        // press mouse button -> start switch rotation movement
        n = (int) GetState(swCId) + 1;
        SetState(swCId, n);
        return;
    } 
    if (Mode == 2) {
        // release mouse button -> select next position in switch
        *CSWvar=(*CSWvar)+1;
        if (*CSWvar >= (int) (strlen(sAllowedPos)) )  *CSWvar=0; // last allowed pos passed, return to first allowed pos
        SetState(swCId, 0); // to allow pass (n % 5 != 0) check and set pos of switch
    }
    // refresh switches based on current CSWvar value        
    n = (int) GetState(swCId);
    if (n % 5 != 0) return; // switch in moving -> do not refresh
    c = sAllowedPos[*CSWvar]; // positions for Up, Down, Left, Right
    if (c=='U') n=0; else
    if (c=='R') n=5; else
    if (c=='D') n=10; else n=15; 
    SetState(swCId, n);
}

// replace in d1 digit at <pos> pos with new digit <digit>
t_int64 SetDigit(t_int64 d1, int pos, int digit) 
{
    t_int64 d2 = 0;
    int n,i;

    for(i=16;i>0;i--) {
        n = (int) Shift_Digits(&d1, 1);
        if (i==pos) n=digit;
        d2 = d2*10+n;
    }
    return d2;
}

// handle rotating swtich for 0-9 digit
// Mode = 1 -> to be used when mouse button pressed -> start switch rotation movement
//      = 2 -> to be used when mouse button released -> set end switch rotation and set varible with new value
//      = 3 -> to be used in refresh -> read varible with current value and set switch position 
// swCIdArray, swCIdArrayNum -> is a pointer to array of Control Ids with switches/Switch Number Window
// CSWvar = pointer to variable that holds value corresponding to switches. Can be NULL
// nDigits = number of digits (number of switch controls on control array), max 9 digits
void SetSwitchNum(int Mode, int * swCIdArray, int * swCIdArrayNum, int  * CSWvar, int nDigits) 
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
            if (n & 1) return; 
            // set state to initiate rotation movement
            n=n+1;
            SetState(CP_Click.CId, n);
            SetState(swCIdArrayNum[i], n / 2 + 10);
            return;
        }
    } else if (Mode == 2) {
        // release mouse button -> end switch rotation movement
        for (i=1; i<=nDigits;i++) {
            if (CP_Click.CId != swCIdArray[i]) continue; 
            n = (int) GetState(CP_Click.CId); 
            // increment digit selected on switch
            n++;
            if (n>19) n=0;
            // set new state acording to digit selected
            SetState(CP_Click.CId, n);
            // set number at little window on top of switch
            n = n / 2; // now n is the digit selected
            SetState(swCIdArrayNum[i], n);
            // set digits in variable
            if (CSWvar) {
                d = SetDigit(*CSWvar, i, n); 
                *CSWvar= (int) d; 
            }
            return;
        }
    } else if (Mode == 3) {
        // refresh switches based on current CSWvar value
        if (CSWvar == NULL) return;
        d=*CSWvar; 
        for (i=1; i<=nDigits;i++) {
            CId = swCIdArray[i];
            n = (int) GetState(CId);
            if ((n & 1)==0) { // if movement not initiated, do refresh switch and number on top
               n = d % 10;
               SetState(CId, n*2);
               SetState(swCIdArrayNum[i], n);
            }
            d = d / 10; 
        }
    }
}



void Refresh_Console(void)
{
    UNIT * uptr;
    int n;
    t_int64 reg1, reg2; 

    // set register contents in dispplay panel
    // draw each digit registered in tickcount
    NORC_TickIntensityCount(); 

    DrawReg(NORC.Reg_V,  1, 4, 3);
    DrawReg(NORC.Reg_U,  1, 4, 4);
    DrawReg(NORC.Reg_M4, 1, 4, 5);
    DrawReg(NORC.Reg_M6, 1, 4, 6);
    DrawReg(NORC.Reg_M8, 1, 4, 7);

    DrawReg(NORC.Reg_IREG_D0401, 1,  4, 0);
    DrawReg(NORC.Reg_IREG_D0805, 5,  8, 0);
    DrawReg(NORC.Reg_IREG_D1209, 9, 12, 0);
    DrawReg(NORC.Reg_IREG_D1613,13, 16, 0);
    DrawReg(NORC.Reg_IREG_D17,  17, 17, 0);

    DrawReg(NORC.Reg_REG1_D0401, 1,  4, 1);
    DrawReg(NORC.Reg_REG1_D0805, 5,  8, 1);
    DrawReg(NORC.Reg_REG1_D1209, 9, 12, 1);
    DrawReg(NORC.Reg_REG1_D1613,13, 16, 1);
    DrawReg(NORC.Reg_REG1_D1817,17, 18, 1);

    DrawReg(NORC.Reg_REG2_D0401, 1,  4, 2);
    DrawReg(NORC.Reg_REG2_D0805, 5,  8, 2);
    DrawReg(NORC.Reg_REG2_D1209, 9, 12, 2);
    DrawReg(NORC.Reg_REG2_D1613,13, 16, 2);
    DrawReg(NORC.Reg_REG2_D17,  17, 17, 2);

    memset(digit_occurrences, 0, sizeof(digit_occurrences));

    // lights in Check Stops section - operative-reset
    n = ((lp_unit[1].flags & UNIT_DIS) || (lp_unit[2].flags & UNIT_DIS)) ? 0:1;
    SetState(NORC.LI_printer_Operative, n); 
    // light on  if both LP[1] and LP[2] enabled
    //       off if any LP[1] or LP[2] disabled
    // to change state, use SPp commands to enable/disable printes

    // set rotating switches for check stops
    SetSwitch(3,  NORC.SW_printer, &Console_Sw_PrinterCHKStop, "UL"); 
    SetSwitch(3, NORC.SW_CRT, &Console_Sw_CRTCHKStop, "UL"); 
    SetSwitch(3, NORC.SW_Program, &Console_Sw_ProgCHKStop, "UL"); 

    // lights in Check Stops section - printer 
    SetState(NORC.LI_Printer_Check, Console_LiPrtChk); 
    if (bIndicatorPanel) SetState(NORC.LI_IND_Printer_Check, Console_LiPrtChk); 
    // light on if carriage button pressed but printer returns an error (i.e printer disabled)
    //          if writting to 0001-0007 while print cycle in progress (also set addr check )
    //          if print instr 81-84 executed with printer disabled 
    // button Printer reset clears the light
    if (PRT) {
        SetState(NORC.LI_Special_Function, 0); // printer ready, all lights off
        SetState(NORC.LI_Printer_Instr_A, 0);
        SetState(NORC.LI_Printer_Instr_B, 0);
    } else {
        n = Console_PRT_Instr-80; // printer operating, show lights
        SetState(NORC.LI_Special_Function, (n >= 3) ? 1:0);
        SetState(NORC.LI_Printer_Instr_A, (n & 1) ? 1:0);
        SetState(NORC.LI_Printer_Instr_B, (n & 1) ? 0:1);
    }

    // lights in Check Stops section - CRT
    SetState(NORC.LI_Word_Check, Console_LiAddrChk);
    if (bIndicatorPanel) SetState(NORC.LI_IND_CRT_Check, Console_LiAddrChk);
    // light on if tape instr 9X executed but P field is zero
    //                                    but P field has not the corresponding tape address set
    //                                    write to addr 0001-0007 during printing cycle
    //                                    Fetching an opcode from an invalid addr (from V or U address depenging on switch Source of instrctions)
    //                                    read from/write to invalid addr during tape write/read
    //             regular instruction reads/write (with modifiers) from/to invalid addr
    //             write to CRT/read from CRT using the console buttons with invalid address
    // button CRT Check reset clears the light

    // lights for Check Stops section - Program
    // lit when program stops
    SetState(NORC.LI_Index_Check, Console_LiIndexChk);
    SetState(NORC.LI_Sign_Check, Console_LiSignChk);
    SetState(NORC.LI_Operation_Code_Check, StopReason == STOP_OPCODE);
    // light on if invalid opcode, or instr 58 with P not 00, 04, 06 or 08
    SetState(NORC.LI_NonZeroDiv_Check, Console_LiZeroDivChk);
    SetState(NORC.LI_EOT_Check, Console_LiTEOT);
    SetState(NORC.LI_Printer_Oprv_Check, StopReason == STOP_PRINTER); 
    // light on if print instr 81-84 executed with printer disabled and printer check switch set to stop
    // button Prog Check reset clears the light
    SetState(NORC.LI_Tape_Oprv_Check, StopReason == STOP_IOERROR);   
    if (bIndicatorPanel) {
        n = ( (Console_LiIndexChk) || (Console_LiSignChk) ||
              (StopReason == STOP_OPCODE) || (Console_LiZeroDivChk) || (Console_LiTEOT) ||
              (StopReason == STOP_PRINTER) || (StopReason == STOP_IOERROR) ) ? 1 : 0;
        SetState(NORC.LI_IND_Program_Check, n);
    }

    // console lights to signal the instruction has been executed
    SetState(NORC.LI_6468, Console_Li64);
    SetState(NORC.LI_7479, Console_Li74);

    // set rotating switches for condition stop
    SetSwitch(3, NORC.SW_Overflow, &Console_Sw64[0], "UL"); 
    SetSwitch(3, NORC.SW_AdjustIndex, &Console_Sw64[1], "UL"); 
    SetSwitch(3, NORC.SW_ZeroResult, &Console_Sw64[2], "UL"); 
    SetSwitch(3, NORC.SW_EndOfFile, &Console_Sw64[3], "UL"); 
    SetSwitch(3, NORC.SW_TapeCheck, &Console_Sw64[4], "UL"); 

    // console light to signal a condition code has been checked
    // example:
    // if float operation gives OV=1 (overflow), then oveflow light set, then ...
    // ... if SW set to stop -> program stop
    // ... if SW set to proceed -> program continues with OV=1
    // if instr 64 is exec, light for 64 set, then ...
    // ... if condition OV=1 then OV reset to zero, and console light OV keept as set until manually reset
    // ... 
    SetState(NORC.LI_OV, Console_LiOV);     if (bIndicatorPanel) SetState(NORC.LI_IND_OV, Console_LiOV);
    SetState(NORC.LI_IA, Console_LiIA);     if (bIndicatorPanel) SetState(NORC.LI_IND_IA, Console_LiIA);
    SetState(NORC.LI_ZR, Console_LiZR);     if (bIndicatorPanel) SetState(NORC.LI_IND_ZR, Console_LiZR);
    SetState(NORC.LI_TEOF, Console_LiTEOF); if (bIndicatorPanel) SetState(NORC.LI_IND_TEOF, Console_LiTEOF);
    SetState(NORC.LI_TCHK, Console_LiTCHK); if (bIndicatorPanel) SetState(NORC.LI_IND_TCHK, Console_LiTCHK);

    // lights for program advancing and tape moving
    Get_PADV_TMOV_lights(&n);
    SetStateWithIntensity(NORC.LI_PADV_TMOV, n);
    if (bIndicatorPanel) {
        SetStateWithIntensity(NORC.LI_IND_PADV_TMOV, n);
        // program not advancing and tape not moving
        SetStateWithIntensity(NORC.LI_IND_NOT_PADV_TMOV, Console_Stall ? 1:0);
        // subops
        SetStateWithIntensity(NORC.LI_IND_SUBOP, Console_CurrentSubOp + (sim_is_running ? 1:0));
        // bus bit digits
        Get_BusData(&reg1, &reg2);
        SetStateWithIntensity(NORC.LI_BITS_D0801, reg1);
        SetStateWithIntensity(NORC.LI_BITS_D1709, reg2);
    }

    // n=1 if check stop
    n = ((StopReason > 0) && (StopReason < STOP_PROG) && (sim_is_running==0)) ? 1:0; 
    SetState(NORC.LI_CHKSTOP, n);
    if (bIndicatorPanel) SetState(NORC.LI_IND_CHKSTOP, n);
    n = ((sim_is_running==0) && (n==0)) ? 1:0; 
    SetState(NORC.LI_PSTOP, n);
    if (bIndicatorPanel) SetState(NORC.LI_IND_PSTOP, n);

    SetState(NORC.LI_Manual_Read, Console_LiManualRead); 

    if (bIndicatorPanel) {
        SetState(NORC.LI_IND_3600_Spots, MEM2K ? 0:1); 
        // tapes
        for(n=1;n<=8;n++) {
            uptr = &mt_unit[n]; 
            // tape unit disabled -> do not lit power on, all tape unit light are off
            if (uptr->flags & UNIT_DIS) {
                SetState(NORC.LI_TU_Power_On[n],0);
                SetState(NORC.LI_TU_Operative[n],0);
                SetState(NORC.LI_TU_Rewind[n],0);
                SetState(NORC.LI_TU_EOT[n],0);
                continue; 
            }
            // tape unit enabled -> lit power on
            SetState(NORC.LI_TU_Power_On[n],1);
            // no file attached -> do not lit operative
            if ((uptr->flags & UNIT_ATT) == 0) {
                SetState(NORC.LI_TU_Operative[n],0);
                SetState(NORC.LI_TU_Rewind[n],0);
                SetState(NORC.LI_TU_EOT[n],0);
                continue; 
            }
            // file attached -> lit operative
            SetState(NORC.LI_TU_Operative[n], 1); 
            // tape rewinding?
            SetState(NORC.LI_TU_Rewind[n], 
                ( (mt_ready(n) == 0) && (mt_get_last_cmd(n) == OP_REWIND) ) ? 1:0); 
            // tape at eot?
            SetState(NORC.LI_TU_EOT[n], 
                ( (mt_info[n].result1==STOP_EOT) || (uptr->u3 > uptr->u4*1000) ) ? 1:0); 
        }
        // tape moving?
        Get_TapeMoving(&reg1);
        SetStateWithIntensity(NORC.LI_TU_Moving, reg1); 
        // tape checks
        if (Console_LiTCHK==0) {
            SetState(NORC.LI_IND_WordLen, 0);
            SetState(NORC.LI_IND_BitCount, 0);
            SetState(NORC.LI_IND_BlockLen, 0);
            SetState(NORC.LI_IND_GtThan9, 0);
            SetState(NORC.LI_IND_BlockNum, 0);
        } else {
            SetState(NORC.LI_IND_WordLen, (LastTapeCheck == MT_IND_WORDLEN) ? 1:0);
            SetState(NORC.LI_IND_BitCount, (LastTapeCheck == MT_IND_RDERR) ? 1:0);
            SetState(NORC.LI_IND_BlockLen, (LastTapeCheck == MT_IND_BLKLEN) ? 1:0);
            SetState(NORC.LI_IND_GtThan9, (LastTapeCheck == MT_IND_BADCHAR) ? 1:0);
            SetState(NORC.LI_IND_BlockNum, (LastTapeCheck == MT_IND_BLKNUM) ? 1:0);
        }
    }    

    SetSwitch(3, NORC.SW_Source_of_intructions, &Console_Sw_SourceOfInstr, "LUR"); 

    // display v entry switches setting
    SetSwitchNum(3, &NORC.SW_V_Entry[0], &NORC.SWNum_V_Entry[0], &Console_Sw_VEntry_Addr, 4);

    // option switches
    for (n=0; n<6;n++) {
        SetSwitch(3, NORC.SW_74[n], &Console_Sw74[n], "LUR"); 
    }
    SetSwitch(3, NORC.SW_Floated_index, &Console_Sw_Floated_Index, "LU"); 
    SetSwitch(3, NORC.SW_Write_Output, &Console_Sw_Write_Output, "LU"); 

    // register crt address
    SetSwitchNum(3, &NORC.SW_Reg1_CRT_Addr[0], &NORC.SWNum_Reg1_CRT_Addr[0], &Console_Sw_Reg1_CRTAddr, 4);
    SetSwitchNum(3, &NORC.SW_Reg2_CRT_Addr[0], &NORC.SWNum_Reg2_CRT_Addr[0], &Console_Sw_Reg2_CRTAddr, 4);

    n=Console_Sw_KeyboardEntry; 
    SetSwitch(3, NORC.SW_Keyboard_Entry, &Console_Sw_KeyboardEntry, "LUR"); 
    // if KeyboardEnntry switch changes, then clears the input buffer
    if (n!=Console_Sw_KeyboardEntry) cpvid[0].keyb_buf_len; 

    // tape address
    for (n=1; n<=12; n++) {
        SetSwitchNum(3, &NORC.SW_Tape_Addr[n-1], &NORC.SWNum_Tape_Addr[n-1], &Console_Tape_Addr[n-1], 1);
    }

    // tape+ unit selector
    SetSwitchNum(3, &NORC.SW_Tape_Unit_Selector[0], &NORC.SWNum_Tape_Unit_Selector[0], &Console_Sw_TapeUnitSelector, 1);
}

#define     MT_is_loading_tape      1
#define     MT_is_rewinding         2
#define     MT_is_unloading_tape    3

int PARAM_MaxSlice_msec  =   100;  // max time considered for tape hop calculations
int PARAM_Reel_Diameter  =   267;  // reel diameter in mm 
int PARAM_RPM            =  1600;  // reel forward/backwards motor revolutions per minute
int PARAM_VacCol_h_Low   = 19200;  // upper vacuum colum sensor (inches x 1000) triggers reel load medium on colum 
int PARAM_VacCol_h_Hi    = 32000;  // lower vacuum colum sensor (inches x 1000) triggers reel take medium from colum 
int PARAM_RWSpeed        =   140;  // tape head operates at 140 inches per sec
int PARAM_AccelTime      =    95;  // accel time in msec. clutch must obtain 2/3 of maximum speed in 0.135-0.150 seconds with a full reel of tape
int PARAM_DecelTime      =   105;  // decel time in msec. the stop clutch must stop a full reel of tape from full speed in 0.145-0.160 sec
                                   // note: the accel/decel time increases when tape unit
                                   // get used. Lower values makes jerky spins with thigh 
                                   // oscilation of tape loop in vacuum columns. Higher values
                                   // makes smother spins, with more ample oscilations of tape loop
int PARAM_HeadOpenTime   =  1000;  // time in msec needed by r/w tape head to fully open or close
int PARAM_TakeMotor_RPM  =    40;  // take motor revolutions per minute


#define     MT_anim_step_nop       -1             // do nothing, just keeps current MT state values
#define     MT_anim_step_inc        0             // incremental animation step
#define     MT_anim_step_rw         1             // read/write tape animation step 
#define     MT_anim_finished       99             // this is the final step that signals the animation sequence has finished
#define     MT_anim_no_change    (1<<30)          // no change the current value.

void mt_reels_mov(int unit, int cmd, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot, 
                  int * MT_Reel_Amount, int * MT_head_actuator);

// execute animation step. return 1 if animation terminated
int mt_do_animation_seq(int unit, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot, 
                  int * MT_Reel_Amount, int * MT_head_actuator)
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
            *MT_Reel_Amount = (int) GetState(NORC.MT[unit]); 
            *MT_L_Rot = (int) GetState(NORC.MT_L[unit]); // keeps blur
            *MT_R_Rot = (int) GetState(NORC.MT_R[unit]);                           
            *MT_head_actuator =  (int) GetState(NORC.MT_head_actuator[unit]);  
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
            if (MT_anim_no_change & mtcab[unit].seq[nseq].MT_Reel_Amount) *MT_Reel_Amount = (int) GetState(NORC.MT[unit]); 
            else *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            *MT_L_Rot = mtcab[unit].reel[0].n = (mtcab[unit].reel[0].ang % 60) * 12 / 60;
            *MT_R_Rot = mtcab[unit].reel[1].n = (mtcab[unit].reel[1].ang % 60) * 12 / 60;
            *MT_head_actuator  = mtcab[unit].seq[nseq].MT_head_actuator; 
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
            // this step is regular tape movement made by the r/w header, either stoped, backwards
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
            n=mtcab[unit].seq[nseq].MT_head_actuator; 
            recsize  = n * time * PARAM_RWSpeed; // inches x1000, 
            recsize = recsize / 2; 
            mtcab[unit].reel[0].VacCol_h -= recsize;
            mtcab[unit].reel[1].VacCol_h += recsize;
            mt_reels_mov(unit, 0, 
                     L_VacColMedium_h, R_VacColMedium_h, 
                     MT_L_Rot, MT_R_Rot, 
                     MT_Reel_Amount, MT_head_actuator);
            if (u3) {
                if (mtcab[unit].seq[nseq].R_VacCol_inc==0) {
                    // update mtcab[unit].rew_u3 to signal progress of rewind
                    p = 1000 - 1000*m / msec; 
                    if (p>1000) p=1000; if (p<0) p=0; // m=0..1000 is % x10 of time of step remaining
                    // calculate rew_u3 = how much medium has been rew 
                    mtcab[unit].rew_u3 = (int) (((t_int64) (u3)) * ((t_int64) (p)) / 1000);
                    *MT_Reel_Amount = 1 + (int) (23 * (mtcab[unit].rew_u3 / (mt_unit[unit].u4*1000.0))); 
                    if (*MT_Reel_Amount > 24) *MT_Reel_Amount = 24;
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
            mtcab[unit].seq[i].MT_head_actuator);
    }
}

// add animation sequence step 
void AddSeq(int unit, int msec, int hint, 
            int L_VacCol_inc, int R_VacCol_inc, 
            int L_ang_inc, int R_ang_inc,
            int MT_Reel_Amount, int MT_head_actuator)
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
    mtcab[unit].seq[i].MT_head_actuator = MT_head_actuator;
}

// add to the current animation sequence the load animation:
//    - close r/w head
//    - spin reels slowly to load medium on vacuum columns
void mt_add_load_seq(int unit)
{
    int MT_Reel_Amount,  MT_head_actuator; // state of controls at end of of redraw
    int i, L_h, R_h, L_inc_h, R_inc_h, msec, ang_inc, r1, r2;

    MT_Reel_Amount   = 1;    // all tape medium on L tape
    MT_head_actuator = 19;   // head full open

    // user needs time to move head actuator from right to left to close the r/w head
    // prepare animation sequence each given msec
    msec= PARAM_HeadOpenTime / MT_head_actuator; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, MT_head_actuator);       
        if (MT_head_actuator==0) break; 
        MT_head_actuator--;
    }

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
                MT_Reel_Amount, 0 /* MT_head_actuator */ );       
        L_h += L_inc_h; 
        R_h += R_inc_h; 
        if ((R_h > PARAM_VacCol_h_Low) && (L_h > PARAM_VacCol_h_Low)) break; 
    }

    // make a small read backwards so left reel starts to move and remove tape medium from column
    msec = 30;
    AddSeq(unit, msec, MT_anim_step_rw,  0,0,0,0,0, 
                                         1 /* read forward */); 

}

// add to the current animation sequence the load animation:
//    - spin reels slowly to unload medium from vacuum columns
//    - open r/w head
void mt_add_unload_seq(int unit)
{
    int u3 = mt_info[unit].recsize;  // use recsize to get u3 value on rewind start 
    int i, L_h, R_h, L_inc_h, R_inc_h, msec, ang_inc, r1, r2;
    int MT_head_actuator = 19;   

    // rewind tape if necessary
    if (u3 > 0) {

        mtcab[unit].rew_u3 = u3; // amount on tape medium (inch x1000) in reel R that should be rewinded
                                 // original mt_unit[].u3 is set to 0 on OP_REWIND command start in mt_cmd()
        msec = u3 / PARAM_RWSpeed;              // time to rewind tape in msec
        AddSeq(unit, msec, MT_anim_step_rw,  u3 /* update recsize */,0,0,0,0, 
                                             -1 /* read backwards */); 
    }

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
                1 /* MT_Reel_Amount */, 0 /* MT_head_actuator */ );       
        L_h -= L_inc_h; 
        R_h -= R_inc_h; 
        if ((R_h < 0) && (L_h < 0)) break; 
    }

    MT_head_actuator = 0;   // head closed

    // user needs time to move head actuator from left to right to open the r/w head
    // prepare animation sequence each given msec
    msec= PARAM_HeadOpenTime / 19; // time for animation step 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                1 /* MT_Reel_Amount */, MT_head_actuator);       
        if (MT_head_actuator==19) break; 
        MT_head_actuator++;
    }

    // remove reels
    AddSeq(unit, 500, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0 /* MT_Reel_Amount */, 18);       

    // move again head actuator from right to left to close again r/w head
    MT_head_actuator=17; 
    for(i=0;;i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0 /* MT_Reel_Amount */, MT_head_actuator);       
        if (MT_head_actuator==0) break; 
        MT_head_actuator--;
    }

}

// calculate and store in animation array the animation sequence for rewind
void mt_set_rew_seq(int unit, int bStartEndSeqFlag)
{
    int MT_Reel_Amount;    // state of controls at end of of redraw
    int u3 = mt_info[unit].recsize;  // use recsize to get u3 value on rewind start 
    int msec, time; 

    MT_Reel_Amount = 1 + (int) (23 * (u3 / (mt_unit[unit].u4*1000.0))); 
    if (MT_Reel_Amount > 24) MT_Reel_Amount = 24;

    if (bStartEndSeqFlag) {
        mtcab[unit].nseq=0;                 // init sequence
        mtcab[unit].nseq_tm0=Refresh_tnow;  // when animation starts
    } else if (u3==0) {
        // nothing to rewind, so exit
        return; 
    }

    mtcab[unit].rew_u3 = u3; // amount on tape medium (inch x1000) in reel R that should be rewinded
                             // original mt_unit[].u3 is set to 0 on OP_RWD command start in mt_cmd()
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

// calculate and store in animation array the load animation when a reel 
// if cmode = 'L' -> load animation: tape is mounted (attached) to tape unit. 
// If cmode = 'F' or 'B' load animation will be move all medium forwards/backwards
// if cmode = 'U' -> unload animation: tape is dismounted (detached) to tape unit. 
// during animations, from simulated cpu point of wiew: the tape is at its final state
//                                                      animation can be interrupted at any moment by tape command
void mt_set_anim_seq(int unit, char cmode)
{
    int n, msec, r, R_h;
    int tm0, tm1; 

    // on load, user places the medium over vacuum columns, and manually moves the 
    // head actuator handle from right to left. Then the reels turns slowly to
    // feed medium into columns. Then goes backwards to read read record backwards 
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
    for (n=1;n<=8;n++) {
        if (n==unit) continue; 
        if (((mtcab[n].mt_is == MT_is_loading_tape) || (mtcab[n].mt_is == MT_is_unloading_tape)) &&
             (mtcab[n].seq[0].MT_head_actuator == 0)) {
            // load/unload anim in progress in unit n. Let's know when it started
            tm1=mtcab[n].nseq_tm0; 
            if (mtcab[n].seq[0].MT_head_actuator==0) tm1 += mtcab[n].seq[0].msec; // add initial wait if any
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
                (cmode == 'U') ? MT_anim_no_change:0 /* MT_Reel_Amount */, 
                0 /* MT_head-actuator */ ); // no need to diferentiate on load/unload: in both cases, actuator starts on left


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
    //    - close r/w head
    //    - spin reels slowly to load medium on vacuum columns
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
                  int * MT_Reel_Amount, int * MT_head_actuator)
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
        // if no r/w op being animated (rw_tm0=0), and now a r/w operation is being done by tape
        mt_info[unit].cmd_tm0=0;   // mark this as being animated (to avoid animating it twice)
        mtcab[unit].rw_tm0     = tnow; 
        mtcab[unit].rw_msec    = (mt_info[unit].cmd_usec1 + mt_info[unit].cmd_usec2)  / 1000;; // microsecs needed to complete tape command
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
    *MT_Reel_Amount = 1 + (int) (23 * (mt_unit[unit].u3 / (mt_unit[unit].u4*1000.0))); 
    if (*MT_Reel_Amount > 24) *MT_Reel_Amount = 24;
    // tape head on closed position
    *MT_head_actuator = 0; 

    // save last tnow used to know how much time elapsed since last refresh
    old_tnow = tnow;  
}

// dynamic draw of tape medium into vaccum column control CId_VacCol
// if VacColMedium_h = PARAM_VacCol_h_Low -> tape medium loop positioned over upper vacuum colum sensor
// if VacColMedium_h = PARAM_VacCol_h_Hi  -> tape medium loop positioned over lower vacuum colum sensor
void mt_VacColSetDynamicState(int VacColMedium_h, int * VacColMedium_h0, int CId_VacCol, int bVacColVisible)
{
    int h, y;

    if (VacColMedium_h == -1) {
        // force redraw
        VacColMedium_h = *VacColMedium_h0;
    } else {
        if (VacColMedium_h == *VacColMedium_h0) return; // no changes in height. nothing to draw
        *VacColMedium_h0 = VacColMedium_h; 
    }

    // tape medium control is 243 pixels height
    // on vac col h=93 -> upper sensor, h=153 -> lower sensor (h=0 -> top) 
    h= (243-153) + ((153-93) * (VacColMedium_h-PARAM_VacCol_h_Low) / (PARAM_VacCol_h_Hi-PARAM_VacCol_h_Low));
    if (h<0) h=0; 
    // h is vertical position of tape loop base, convert to y coord
    // y=243 <-> VacColMedium_h = 0 (medium outside vac col)
    // y=153 <-> VacColMedium_h = PARAM_VacCol_h_Low (medium over upper sensor)
    // y=93 <-> VacColMedium_h = PARAM_VacCol_h_Hi  (medium over lower sensor)
    y=243-h; if (y<0) y=0; 

    // set height of draw medium
    h = (bVacColVisible ? 0 : 68);
    
    // Dynamically generate State 0 for CId_VacCol (=NORC.MT_L|R_VacCol[unit])

    //  - Copy State 0 from control MT_VacColumn (background) to state 0 of CId (state displayed) 
    CopyControlImage(NORC.MT_VacColumn, bVacColVisible, 0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol, 0,                0, 0);       // ToCId, ToState,     x1, y1
    //  - Copy State 0 from MT_VacColMedium (height dependes on medium position) on State 0 -> now vaccol with tape medium
    CopyControlImage(NORC.MT_VacColMedium, 0,    0, y, 0, h,  // FromCId, FromState, x0, y0, w, h, 
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
    if ((cmd ==  OP_WRITE) || (cmd ==  OP_WRI_OUTPUT) || 
        (cmd ==  OP_DELETE) || (cmd ==  OP_DEL_OUTPUT) ||
        (cmd ==  OP_READ) || (cmd ==  OP_READ_BWRD) ||
        (cmd ==  OP_VERIFY) || (cmd ==  OP_VER_BWRD)) {
        return 1; // tape read/write operation
    }
    return 0;
}

// intruct the control to be redraw on refresh
void set_redraw_needed(int CId)
{
    int n = (int) GetState(CId);    
    cpanel_ControlRedrawNeeded = 1;
    SetState(CId, n);    
}

// redraw full tape cabinet
void set_mt_cabinet_redraw_needed(int unit)
{
    int door_open = (int) GetState(NORC.MT_panel[unit]); 
    cpanel_ControlRedrawNeeded = 1;
    SetState(NORC.MT_DoorHandle[unit], door_open);    
    mt_VacColSetDynamicState(-1, &mtcab[unit].L_VacColMedium_h0, NORC.MT_L_VacCol[unit], door_open);
    mt_VacColSetDynamicState(-1, &mtcab[unit].R_VacColMedium_h0, NORC.MT_R_VacCol[unit], door_open);
    set_redraw_needed(NORC.MT_panel[unit]);
    set_redraw_needed(NORC.MT[unit]);
    set_redraw_needed(NORC.MT_head[unit]);
    set_redraw_needed(NORC.MT_head_actuator[unit]);
    set_redraw_needed(NORC.MT_L[unit]);
    set_redraw_needed(NORC.MT_R[unit]);
    set_redraw_needed(NORC.MT_L_VacCol[unit]);
    set_redraw_needed(NORC.MT_R_VacCol[unit]);
    // if setting MT_panel, force redraw of printer paper, if any, to avoid being hidden under tape cabinet
    if (bPrintOutVisible) { 
        cpanel_ControlRedrawNeeded = 1; 
        SetState(NORC.Paper, 0); 
    }
}

void Refresh_MagTape(void)
{
    int unit, n;
    UNIT *uptr;

    int MT_Reel_Amount0, MT_L_Rot0, MT_R_Rot0, MT_head0, MT_head_actuator0; // state of controls at beggining of redraw
    int MT_Reel_Amount,  MT_L_Rot,  MT_R_Rot,  MT_head, MT_head_actuator; // state of controls at end of of redraw
    int L_VacColMedium_h, R_VacColMedium_h; // annount of tape medium in vaccol
    int cmd, mt_is, bVacColVisible;

    bTapeAnimInProgress=0;
    for(unit=1;unit<=8;unit++) {
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
                if (GetState(NORC.MT[unit])==0) continue; 
                SetState(NORC.MT[unit], 0);          // no magnetic medium on reels
                SetState(NORC.MT_head[unit], 0);     // head transparent (seen as closed)
                SetState(NORC.MT_L[unit], GetState(NORC.MT_L[unit]) % 12); // keep the angular position of reel          
                SetState(NORC.MT_R[unit], GetState(NORC.MT_R[unit]) % 12);          
                mtcab[unit].L_VacColMedium_h0=0;
                mtcab[unit].R_VacColMedium_h0=0;
                SetState(NORC.MT_head_actuator[unit], 0); // tape head actuator set to left
                set_mt_cabinet_redraw_needed(unit);
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
                // syntax: option=mt1/V  <- tape 1 cabinet has the front panel removed so the vacuum columns are visible
                c = *IsOptionParam++; 
                c = sim_toupper(c);
                n =  (c == 'V') ? 1:0;    // state 1 set tape cabinet panel removed so vac col are visible
                SetState(NORC.MT_panel[unit], n); 
                set_mt_cabinet_redraw_needed(unit);
                // if setting MT_panel, force redraw of printer paper, if any, to avoid being hidden under tape cabinet
                if (bPrintOutVisible) { cpanel_ControlRedrawNeeded = 1; SetState(NORC.Paper, 0); }
                if ((c == 'V') || (c == 'C')) {
                    c = *IsOptionParam++; 
                    c = sim_toupper(c);
                }
                if (c == '*') {
                    c = *IsOptionParam++; 
                    c = sim_toupper(c);
                    if ((c == 'R') || (c == 'F')) {
                        cmode = c; // load animation will be Forward/backwards all reel, or *R
                    }
                    RemoveOption(MT_cab_opt); // remove this option, as it is being executed now so will not apply on next attach of tape
                }
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
                // normal load animation, *F 
                 mt_set_anim_seq(unit, cmode);
            } else {
                // *R rewind animation 
                // start rewind. Whis is nice!
                mt_info[unit].recsize = mt_unit[unit].u4 * 1000;
                mt_set_rew_seq(unit,1);
            }
        }
        // tape is about to be painted on cpanel
        // MT state holds the amount of tape medium in each reel. 
        //   =1  -> all tape on L reel, 
        //   =24 -> all tape on R reel, 
        //   =0  -> no medium on tape reel 
        // MT_L/MT_R state holds the rotational position of reel, the speed ot reel, and the reel's colour
        //    0..11 -> reel rotated 0gr, 5gr, 10gr, ... 55gr. To be used when reel is not moving
        //   12..13 -> same reel rotated 0gr, 90gr ... but with some spin blur. To be used when reel is moving
        //   14..15 -> reel rotalted 0gr, 90gr, ... whith greater spin blur. To be used when reel is rewinding at fast pace
        // MT_head holds the position of r/w head
        //   =0  -> transparent (so head closed)
        //   =1  -> head open, prepared to manualy remove tape medium
        //   =12 -> closed head, prepared to read or write tape medium
        // MT_head actuator holds the position of handle actuator to open/close r/w head 
        //   =0  -> actuator on left (head closed)
        //   =10 -> actuator up
        //   =19 -> actuator on right (head full open)

        // get the current tape control state. If this state changes, then should redraw
        MT_Reel_Amount0   = (int) GetState(NORC.MT[unit]); 
        MT_L_Rot0         = (int) GetState(NORC.MT_L[unit]);
        MT_R_Rot0         = (int) GetState(NORC.MT_R[unit]);
        MT_head0          = (int) GetState(NORC.MT_head[unit]);
        MT_head_actuator0 = (int) GetState(NORC.MT_head_actuator[unit]);

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
                         &MT_Reel_Amount, &MT_head_actuator);
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
                mt_is = 0; // set to zero to allow normal tape processing
            }
        }

        // if no animation, simulate reel/vacuum col movement
        if (mt_is == 0) {
            // if tape at end of file, do not perform cmd 
            if ((mt_info[unit].result1==STOP_EOT) || (uptr->u3 > uptr->u4*1000)) cmd = 0;
            mt_reels_mov(unit, cmd, 
                         &L_VacColMedium_h, &R_VacColMedium_h, 
                         &MT_L_Rot, &MT_R_Rot, 
                         &MT_Reel_Amount, &MT_head_actuator);
        }

        // set head position based on head actuator position 
        MT_head = 12 - 12 * MT_head_actuator / 20;
        // update tape control states
        if ((MT_Reel_Amount0 != MT_Reel_Amount) || 
            (MT_L_Rot0 != MT_L_Rot) || (MT_R_Rot0 != MT_R_Rot) || 
            (MT_head0 != MT_head) || (MT_head_actuator0 != MT_head_actuator)) {
            // if some state has changed, redraw all controls
            cpanel_ControlRedrawNeeded = 1;
            SetState(NORC.MT[unit], MT_Reel_Amount);
            cpanel_ControlRedrawNeeded = 1;
            SetState(NORC.MT_L[unit], MT_L_Rot);
            cpanel_ControlRedrawNeeded = 1;
            SetState(NORC.MT_R[unit], MT_R_Rot);
            cpanel_ControlRedrawNeeded = 1;
            if ((MT_head0 != MT_head) || (MT_head_actuator0 != MT_head_actuator)) {
                cpanel_ControlRedrawNeeded = 1;
                SetState(NORC.MT_head[unit], MT_head); 
                cpanel_ControlRedrawNeeded = 1;
                SetState(NORC.MT_head_actuator[unit], MT_head_actuator); 
            }
        }
        // set dynamic state for vacCol controls 
        bVacColVisible = (int) GetState(NORC.MT_panel[unit]);
        mt_VacColSetDynamicState(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, NORC.MT_L_VacCol[unit], bVacColVisible);
        mt_VacColSetDynamicState(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, NORC.MT_R_VacCol[unit], bVacColVisible);
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
    
    surface0=GetControlSurface(NORC.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(NORC.PaperBackground, 0, &bg_ww, &bg_hh); // paper backgtound should have same width that paper control

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

    surface0=GetControlSurface(NORC.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(NORC.PrinterCharSet, 0, &char_ww, &char_hh); 

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

    surface0=GetControlSurface(NORC.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(NORC.PaperBackground, 0, &bg_ww, &bg_hh); // paper background should have same width that paper control

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
    SetState(NORC.Paper, 0);
}

void process_HotKeys(void)
{
    char c; 

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
            SetState(NORC.CtrlInfoPanel, 0); 
            if (bTapesVisible) SetState(NORC.MT_InfoPanel, 0); 
        } else {
            // init IPS measurement
            ShowInfoTm0 = 0;
        }
    } else if ((((c=cpvid_getkey(0)) >= '0') && (c <=  '9')) || (c==8)) {
        // digits 0..9, BackSpace -> NORC console keyboard keys
        // use ncp=0 on cpvid_getkey as norc has only one control panel
        // used getkey instead of LastKeyPress because LastKeyPress returns ScanCode for KeyPad keys
        // instead of the keypad digit
        // processed if cpu not running and Keyboard entry switch set to REG1 or REG2
        if ((sim_is_running==0) && (Console_Sw_KeyboardEntry)) {
            t_int64 d; 
            d = (Console_Sw_KeyboardEntry == 1) ? REG1 : REG2; 
            if (c==8) {
                Shift_Digits(&d, -1); // BackSpace shift one digit to right
            } else {
                Shift_Digits(&d, 1);
                d = d + c - '0';
            }
            if (Console_Sw_KeyboardEntry == 1) REG1=d; else REG2=d; 
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
    // if tape operation in progress (cpu resumes exec when operation finishes)
    //    FPS: nnn CPU SPEED: xn.nn TMOV: nnnn
    //       TMOV = simulated time (milliseconds) remaining for tape operation to finish
    // if there is a printing cycle in progress (cpu still running, but if a tape or printer instr will be stalled)
    //    FPS: nnn CPU SPEED: xn.nn PRT: nnnn
    //       PRT = simulated time (milliseconds) remaining for printing cycle to finish
    // if cpu is stalled (cpu not running)
    //    FPS: nnn STALL: aaaaa (nnn)
    //       cpu is waiting, stall reason:
    //          WAIT PRT READY          Print instr stalled because printer cycle not finished
    //          TAPE WAIT PRT READY     Tape instr stalled because printer cycle not finished
    //          TAPE UNIT NOT RDY       Tape instr stalled because tape unit not ready
    //       shows nnn number of msec remaining to resume exec, If =0 -> will never resume
    if (Console_Stall) {
        msec=-1;
        if (Console_Stall <= 2) {
            n=sim_activate_time(&lp_unit[1]) + sim_activate_time(&lp_unit[2]);
            msec=n / 1000; 
        } else if (Console_Stall == 3) {
            n=sim_activate_time(&mt_unit[TU]);
            msec=n / 1000; 
        }
        sprintf(buf, "FPS: %d STALL: %s (%d)\n", 
            fps, 
            (Console_Stall == 1) ? "WAIT PRT READY"      : // Print instr stalled because printer cycle not finished
            (Console_Stall == 2) ? "TAPE WAIT PRT READY" : // Tape instr stalled because printer cycle not finished
            (Console_Stall == 3) ? "TAPE UNIT NOT RDY"   : // Tape instr stalled because tape unit not ready
                                   "???", 
             msec);
    } else if (Console_TMOV) {
        sprintf(buf, "FPS: %d Cpu Speed: x%0.2f TMOV: %d\n", 
            fps, 
            TickCountPerSec / 1000000.0,  Console_TMOV / 1000);
    } else if (PRT==0) {
        n=sim_activate_time(&lp_unit[1]) + sim_activate_time(&lp_unit[2]);
        msec=n / 1000; 
        sprintf(buf, "FPS: %d Cpu Speed: x%0.2f PRT: %d\n", 
            fps, 
            TickCountPerSec / 1000000.0, msec);
    } else {
        sprintf(buf, "FPS: %d Cpu Speed: x%0.2f (%0.1f %s)\n",
            fps, 
            TickCountPerSec / 1000000.0,  
            ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS");
    }

    ShowInfo_DrawText(NORC.CtrlInfoPanel, buf);

    if (bTapesVisible) {
        // set tape info panel
        double p_MT_used[9]; char c_MT[9];
        int i, n;
        char c; 
        for(i=1;i<=8;i++) { 
            c = ' ';
            if (((mt_unit[i].flags & UNIT_DIS) || ((mt_unit[i].flags & UNIT_ATT) == 0)) &&
                 (mtcab[i].mt_is != MT_is_unloading_tape)) {
                n = 0;
            } else if ((mtcab[i].mt_is == MT_is_rewinding) || 
                       ((mtcab[i].mt_is == MT_is_loading_tape) && (mtcab[i].rew_u3 > 0)) ||
                       ((mtcab[i].mt_is == MT_is_unloading_tape) && (mtcab[i].rew_u3 > 0))  ) {
                // if rewinding then the ammount of tape medium used is in variable recsize, not in u3
                // if loading tape AND rew_u3 > 0 then the tape is showing a nice rew animation started with *R
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

        sprintf(buf, "            MT1     MT2     MT3     MT4     MT5     MT6     MT7     MT8 \n"
                     " record   %5d   %5d   %5d   %5d   %5d   %5d   %5d   %5d \n"
                     " %% unwind   %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c  %5.1f%c",
            mt_info[1].numrec, mt_info[2].numrec, mt_info[3].numrec, mt_info[4].numrec, mt_info[5].numrec, mt_info[6].numrec, mt_info[7].numrec, mt_info[8].numrec, 
            p_MT_used[1], c_MT[1], p_MT_used[2], c_MT[2], p_MT_used[3], c_MT[3], p_MT_used[4], c_MT[4], 
            p_MT_used[5], c_MT[5], p_MT_used[6], c_MT[6], p_MT_used[7], c_MT[7], p_MT_used[8], c_MT[8]
        );
        ShowInfo_DrawText(NORC.MT_InfoPanel, buf);

    }
}

void NORC_Refresh(void)
{
    // refresh console
    Refresh_Console();

    // tapes
    if (bTapesVisible) {
        Refresh_MagTape();
    }
    // printer
    if (bPrintOutVisible) {
        Refresh_PrintOut();
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
void NORC_DropFile(int CId, char * FileName)
{
    extern t_stat   mt_attach(UNIT * uptr, CONST char *file);
    int32  sv; 
    int n; 

    for (n=1;n<=8;n++) if (CId == NORC.Drop_MT_File[n]) {
        // drag and drop a file on tape -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        mt_attach(&mt_unit[n], FileName);
        sim_switches=sv; 
    }
}

// buttons for tape detach
void NORC_OnClick_BTN2(void)
{
    extern t_stat   mt_detach(UNIT * uptr);
    int n; 
    int32  sv; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press door handle
        // return now because Door handle has no states to set 
        return;
    }

    // to detach a tape, must click unload button
    for (n=1;n<=8;n++) {
        if (CP_Click.CId == NORC.MT_DoorHandle[n]) {
            if ((mt_unit[n].flags & UNIT_ATT) == 0) {
                // already detached. Do not detach again
                return; 
            }
            // click to detach tape
            sv=sim_switches; // save current switches
            sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
            mt_detach(&mt_unit[n]);
            sim_switches=sv; 
        }
    }
}

void NORC_OnClick_Sw(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> start switch rotation movement n-postion switches
        SetSwitch(1, CP_Click.CId, NULL, NULL) ;
        return;
    } 

    // release mouse button -> do the action
    // selection switch set to STOP on condition
    //    =0 -> set to proceed (Up) 
    //    =1 -> set to stop (Left)
    if (CP_Click.CId == NORC.SW_printer) {
        SetSwitch(2,  NORC.SW_printer, &Console_Sw_PrinterCHKStop, "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_CRT) {
        // selection switch for CRT CHECK STOPS: =0 -> set to proceed, =1 -> set to stop
        SetSwitch(2, NORC.SW_CRT, &Console_Sw_CRTCHKStop, "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_Program) {
        SetSwitch(2, NORC.SW_Program, &Console_Sw_ProgCHKStop, "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_Overflow) {
        SetSwitch(2, NORC.SW_Overflow, &Console_Sw64[0], "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_AdjustIndex) {
        SetSwitch(2, NORC.SW_AdjustIndex, &Console_Sw64[1], "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_ZeroResult) {
        SetSwitch(2, NORC.SW_ZeroResult, &Console_Sw64[2], "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_EndOfFile) {
        SetSwitch(2, NORC.SW_EndOfFile, &Console_Sw64[3], "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_TapeCheck) {
        SetSwitch(2, NORC.SW_TapeCheck, &Console_Sw64[4], "UL"); 
        return;
    } else if (CP_Click.CId == NORC.SW_Source_of_intructions) {
        SetSwitch(2, NORC.SW_Source_of_intructions, &Console_Sw_SourceOfInstr, "LUR"); 
        return;
    } else if (CP_Click.CId == NORC.SW_Floated_index) {
        SetSwitch(2, NORC.SW_Floated_index, &Console_Sw_Floated_Index, "LU"); 
        return;
    } else if (CP_Click.CId == NORC.SW_Write_Output) {
        SetSwitch(2, NORC.SW_Write_Output, &Console_Sw_Write_Output, "LU"); 
        return;
    }

    for (n=0;n<6;n++) {
        if (CP_Click.CId == NORC.SW_74[n]) {
            SetSwitch(2, NORC.SW_74[n], &Console_Sw74[n], "LUR"); 
            return;
        }
    }

    if (CP_Click.CId == NORC.SW_Keyboard_Entry) {
        SetSwitch(2, NORC.SW_Keyboard_Entry, &Console_Sw_KeyboardEntry, "LUR"); 
        return;
    }

}

void NORC_OnClick_Sw2(void)
{
    int m, n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {        
        m=1; // press mouse button -> start switch rotation movement n-postion switches
    } else {
        m=2; // release mouse button -> do the action
    } 
    SetSwitchNum(m, &NORC.SW_V_Entry[0], &NORC.SWNum_V_Entry[0], &Console_Sw_VEntry_Addr, 4);
    SetSwitchNum(m, &NORC.SW_Reg1_CRT_Addr[0], &NORC.SWNum_Reg1_CRT_Addr[0], &Console_Sw_Reg1_CRTAddr, 4);
    SetSwitchNum(m, &NORC.SW_Reg2_CRT_Addr[0], &NORC.SWNum_Reg2_CRT_Addr[0], &Console_Sw_Reg2_CRTAddr, 4);
    for (n=1; n<=12; n++) {
        SetSwitchNum(m, &NORC.SW_Tape_Addr[n-1], &NORC.SWNum_Tape_Addr[n-1], &Console_Tape_Addr[n-1], 1);
    }
    SetSwitchNum(m, &NORC.SW_Tape_Unit_Selector[0], &NORC.SWNum_Tape_Unit_Selector[0], &Console_Sw_TapeUnitSelector, 1);
}

void NORC_OnClick_BTN(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        return;
    }
    // set button to unpressed state (except Button Switch power, that remains pressed)
    if (CP_Click.CId != NORC.BTN_Power_Off) {
        SetState(CP_Click.CId, 0);
    }

    if (CP_Click.CId == NORC.BTN_Power_Off) {
       // DoSCP stops the cpu to allow command to be executed by scp
       // if "set cpanel off" is sent via DoSCP, then no more commands can be stacked after.
       // this is because being cpanel off, pendign scp commands issued by cpanel are not processed       
       cpanel_stop_flag = 4; // request cpanel off and quit
       return;
    } 
    if (CP_Click.CId == NORC.BTN_printer_reset) {
        // This button clears Printer check light, terminate current print cycle (so set PRT=1)
        Console_LiPrtChk=0;
        sim_cancel(&lp_unit[1]);
        sim_cancel(&lp_unit[2]);
        PRT=1;
        return;
    }
    if (CP_Click.CId == NORC.BTN_CRT_reset) {
        // This button clears CRT memory, symbolic mem, set V=0
        if (sim_is_running==0) crt_reset();
        return;
    }
    if (CP_Click.CId == NORC.BTN_Reset_InterLock) {
        ClearInterlocks();
        return;
    }
    if ((CP_Click.CId == NORC.BTN_Carriage_A) || (CP_Click.CId == NORC.BTN_Carriage_B)) {
        n = (CP_Click.CId == NORC.BTN_Carriage_A) ? 1:2;
        n=lpt_printline(&lp_unit[n], "");
        if (n<0) Console_LiPrtChk=1; // because carriage button pressed but printer returns an error
        return; 
    }
    if (CP_Click.CId == NORC.BTN_CRT_Check_Reset) {
        // This button clears Addr Check (word check light)
        Console_LiAddrChk=0;
        return; 
    }
    if (CP_Click.CId == NORC.BTN_ProgCheck_Reset) {
        // This button clears all program Checks and indicators
        if ((StopReason==STOP_OPCODE) ||
            (StopReason==STOP_IOERROR) ||
            (StopReason==STOP_PRINTER)) {
            StopReason=0; 
        }
        cpu_reset_indicator_lights();
        return;
    }
    if (CP_Click.CId == NORC.BTN_Index_Reset) { Console_LiIndexChk=0; return; }
    if (CP_Click.CId == NORC.BTN_Sign_Reset) { Console_LiSignChk=0; return; }

    if (CP_Click.CArrayId == NORC.BTN_6468) {
        n = CP_Click.CArrayItem; // =0 if pressed btn_64, =1 if pressed btn_65 ...
        Console_Li64 &= ~(1 << n); // remove bit to turn of the light
        return;
    }
    if (CP_Click.CArrayId == NORC.BTN_7479) {
        n = CP_Click.CArrayItem; // =0 if pressed btn_74, =1 if pressed btn_75 ...
        Console_Li74 &= ~(1 << n); // remove bit to turn of the light
        return;
    }
    if (CP_Click.CId == NORC.BTN_OV) { Console_LiOV=0; if (sim_is_running==0) OV=0; return; }
    if (CP_Click.CId == NORC.BTN_IA) { Console_LiIA=0; if (sim_is_running==0) IA=0; return; }
    if (CP_Click.CId == NORC.BTN_ZR) { Console_LiZR=0; if (sim_is_running==0) ZR=0; return; }
    if (CP_Click.CId == NORC.BTN_TEOF) { Console_LiTEOF=0; if (sim_is_running==0) TEOF=0; return; }
    if (CP_Click.CId == NORC.BTN_TCHK) { Console_LiTCHK=0; if (sim_is_running==0) TCHK=0; return; }
    // if pressed, reset the console condition ligth
    // if cpu is stopped, also clears the flag

    if (CP_Click.CId == NORC.BTN_SubOp_Reset) {
       // only works when cpu is stopped. Reset the suboperation counter
       if (sim_is_running) return; 
       Console_CurrentSubOp = NSUBOP = ESUBOP = 0;
       // also set V to U value, so start will restart current instr
       // (the scp cpmmand presse "SubOp reset" does not changes V)
        V=U; 
       return; 
    }

    if (CP_Click.CId == NORC.BTN_Start) {
       // only works when cpu is stopped. do not reset subop, Starts execution
       if (sim_is_running) return;
       cpu_unit.flags &= ~(OPTION_STEP_SUBOP); // remove step subop flag
       DoSCP("go");
       return;
    }
    if ((CP_Click.CId == NORC.BTN_Operation_Start) || 
        (CP_Click.CId == NORC.BTN_SubOp_Start)) {
       // only works when cpu is stopped. 
       if (sim_is_running) return;
       if (CP_Click.CId == NORC.BTN_Operation_Start) {
           // if operation start then Reset suboperation to step one instruction 
           cpu_unit.flags &= ~(OPTION_STEP_SUBOP); // remove step subop flag
       } else {
           // if sub operation start then do NOT Reset suboperation to step one suboperation
           cpu_unit.flags |= (OPTION_STEP_SUBOP); // set step subop flag
       }
       DoSCP("step");
       return;
    }
    if ((CP_Click.CId == NORC.BTN_Stop) ||
        (CP_Click.CId == NORC.BTN_SubOp_Stop)) {
        // only works when cpu is running
        if (sim_is_running) { 
            // stops at end of instruction/suboperation
            if (CP_Click.CId == NORC.BTN_Stop) {
                cpu_unit.flags &= ~(OPTION_STEP_SUBOP); // remove step subop flag
            } else {
                cpu_unit.flags |= (OPTION_STEP_SUBOP); // set step subop flag
            }
            // request a cpu stop, without disabling interactive mode
            cpanel_stop_flag = 2;
            // and set interactive mode
            cpanel_interactive=1; 
        }
        return; 
    }

    if (CP_Click.CId == NORC.BTN_Tape_Stop) {
        // stop all tape sched events (so the tape unit will not get ready again it was busy)
        for (n=1;n<=8;n++) sim_cancel(&mt_unit[n]);
        if (Console_TMOV==0) return; 
        // if a tape transfer in progress, terminate it, and lit tape check/flag
        Console_TMOV=0;
        Console_LiTCHK=TCHK=1; 
        return;
    }

    if (sim_is_running==0) {
        // buttons that are operative only if cpu is stopped
        t_int64 reg; 
        if (CP_Click.CId == NORC.BTN_V_to_U) {U=V; return; }
        if (CP_Click.CId == NORC.BTN_V_entry_to_V) { V=Console_Sw_VEntry_Addr; return; }
        if (CP_Click.CId == NORC.BTN_U_to_V) {V=U; return; }
        if (CP_Click.CId == NORC.BTN_U1_to_V) {V=U+1; return; }
        if (CP_Click.CId == NORC.BTN_V_to_M4) {M4=V; return; }
        if (CP_Click.CId == NORC.BTN_V_to_M6) {M6=V; return; }
        if (CP_Click.CId == NORC.BTN_V_to_M8) {M8=V; return; }
        if (CP_Click.CId == NORC.BTN_REG1_to_IREG) {IREG=REG1; return; }

        if ((CP_Click.CId == NORC.BTN_Reg1_to_CRT)   || (CP_Click.CId == NORC.BTN_Reg2_to_CRT) ||
            (CP_Click.CId == NORC.BTN_Reg1_from_CRT) || (CP_Click.CId == NORC.BTN_Reg2_from_CRT)) {
            V = ((CP_Click.CId == NORC.BTN_Reg1_from_CRT) || 
                 (CP_Click.CId == NORC.BTN_Reg1_to_CRT)) 
                ? Console_Sw_Reg1_CRTAddr : Console_Sw_Reg2_CRTAddr;
            if (V >= MemSize()) {
                Console_LiAddrChk=1; 
                return; 
            }
        }
        if ((CP_Click.CId == NORC.BTN_Reg1_to_CRT) || (CP_Click.CId == NORC.BTN_Reg2_to_CRT)) {
            // store REG1/REG2 in CRT mem at selected addr. Work only if cpu stopped
            // if addr selected >= mem size (2000 or 3600) then will lit address error
            // the selected addr will be stored in V register
            reg = (CP_Click.CId == NORC.BTN_Reg1_to_CRT) ? REG1 : REG2;
            CRT[V] = CRT_BUS = reg; 
            return;
        }
        if ((CP_Click.CId == NORC.BTN_Reg1_from_CRT) || (CP_Click.CId == NORC.BTN_Reg2_from_CRT)) {
            // read REG1/REG2 from CRT mem at selected addr. Work only if cpu stopped
            // if addr selected >= mem size (2000 or 3600) then will lit address error
            // the selected addr will be stored in V register
            reg = CRT_BUS = CRT[V];
            if (CP_Click.CId == NORC.BTN_Reg1_from_CRT) {
                REG1 = reg; 
            } else {
                REG2 = reg; 
            }
            return;
        }
        if (CP_Click.CId == NORC.BTN_Reg1_from_REG2) { REG1=REG2; return; }
        if (CP_Click.CId == NORC.BTN_Reg2_from_REG1) { REG2=REG1; return; }
        if (CP_Click.CId == NORC.BTN_Reg1_Reset) { REG1=0; return; }
        if (CP_Click.CId == NORC.BTN_Reg2_Reset) { REG2=0; return; }

        if ((CP_Click.CId == NORC.BTN_Read_Forward) ||
            (CP_Click.CId == NORC.BTN_Read_Backward) ||
            (CP_Click.CId == NORC.BTN_Rewind)) { 
            // manual read operations
            char sBuf[40];

            if ((Console_Sw_TapeUnitSelector<1) || (Console_Sw_TapeUnitSelector>8)) return; 
            sprintf(sBuf, "press \"%s\" MT%d", 
                (CP_Click.CId == NORC.BTN_Read_Forward)  ? "Read Forward"  : 
                (CP_Click.CId == NORC.BTN_Read_Backward) ? "Read backward" : "Rewind", 
                Console_Sw_TapeUnitSelector);
           DoSCP(sBuf);
           return; 
        }

    }
    for (n=1;n<=8;n++) {
        if (CP_Click.CId == NORC.BTN_Tape_Reset[n]) {
            // reset tapes unit -> set ready flag on
            mt_unit[n].u5 |= MT_RDY; // set tape ready 
            return;
        } else if (CP_Click.CId == NORC.MT_BTN_DoorOpen[n]) {
            // open/close tape cabinet door to show the vaccum columns operating
            int door_open = (int) GetState(NORC.MT_panel[n]); 
            door_open = 1-door_open; 
            SetState(NORC.MT_panel[n], door_open); 
            set_mt_cabinet_redraw_needed(n);
        }
    }
}

// Matches the ifdef(CPANEL) at beginning of file
#endif


