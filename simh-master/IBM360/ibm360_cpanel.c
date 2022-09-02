/* cpanel.c: simulator control panel simulation

   Copyright (c) 2022, Roberto Sancho

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
   Jan-22    RSV     IBM 360/370 control panel support

   This module implements the following control panels:

   IBM 360 Model 30/40/50/65 Cpu Panel 
   IBM 370 Model 145/148 Cpu Panel 
   IBM 1052/3210 Keyboard-printer console
   IBM 2540/3525 Card Read Punch
   IBM 1403/3203 Printer
   IBM 2401/2415/3420 Magnetic Tape
   IBM 2314/3330 DASD
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "ibm360_defs.h"	        
#include "sim_tape.h"
#include "sim_card.h"
#include <math.h>

// cpu registers
extern uint32       regs[16];             /* CPU Registers */
extern uint32       fpregs[8];            /* Floating point registers */
extern uint32       PC;                   /* Program counter */
extern uint8        flags;                /* Misc flags */
extern uint32       cpu_mem_addr;         // save last accessed main storage addr accessed to be shown on control panel
extern uint32       cpu_mem_data;         // save last data read/write from/to main storage to be shown on control panel 
extern addrcomp_rec cpu_mem_comp;         // compare data to breakpoint execution if addr hit
extern int          cpu_psw_restart;      // =1 when operator requested a psw restart
extern int          InstrExec;            // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
extern int          InstrCycles;          // count of intructions loops executed so far (if cpu waiting, do get incremented)
extern uint32       rtc_activate_usecs;   // usecs remaining to activate rtc_srv, set by cpanel TickCount
extern int          ReadByte(uint32 addr, uint32 *data);
extern int          WriteByte(uint32 addr, uint32 data);
#define WAIT        0x02                  // wait mask in flags

// channel state
extern int     get_chan_info(int nChan, int mode); 
extern UNIT *  unit_name_at_addr(uint32 addr); 
extern uint8   dev_status[MAX_CHAN * 256]; 

// card reader punch state
extern UNIT         cdr_unit[];
extern UNIT         cdp_unit[];
extern uint32 tm0CardInReadStacker;
extern uint32 tm0CardInPunchStacker;
extern int bCardReadEOFset; 
#define CMD    u3
#define CDR_ERR        0x400      /* Last card had an error */

// mt tape state
extern UNIT         mta_unit[];
extern void cpanel_tape_cmd_info(UNIT * uptr, int * addr, int * cmd);

// printout state
extern UNIT   lpr_unit[];
extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
extern int    lptPrintOutCount;

// console printout state
extern UNIT   con_unit[];
extern char   ConsPrintOut[CONS_LPT_COLUMNS * ConsPrintOutMAX];
extern int    ConsPrintOutCount; // number of lines printed
extern int    ConsCharsOutCount;  // number of chars printed in current line
extern int    ConsSomethingPrinted; // instruct refresh to update the console printout control
extern int    bProceed; // status of proceed light
extern int    bRequestPending; // request button pressed, but pending to be serviced 
extern char   ConsKeybBuffer[128]; // keyb buffer of chars sent from cpanel console
extern int    ConsGolfBallAction; // action Golf ball printing head should animate
extern int    ConsGolfBallPosition; // column Golf ball is doing action
extern uint32 tm0ConsGolfBallAction; // when action was started

#define CON_REQ         0x400   /* Request key pressed */
#define CON_CANCEL      0x1000  /* Control C pressed to cancel console input line */

CP_DEF IBM360_cp[];			// declare forwards
void IBM360_Init(void);
void IBM360_Done(void);
void IBM360_Reset(void);
void IBM360_Refresh(void);
void IBM360_TickIntensityCount(void);
void IBM360_DropFile(int CId, char * FileName);
void IBM360_OnClick_Sw(void);
void IBM360_OnClick_Sw2(void);
void IBM360_OnClick_Sw3(void);
void IBM360_OnClick_BTN(void);
void IBM360_OnClick_BTN2(void);

// control panel callbacks
CP_TYPE cp_types = {
      IBM360_cp, 
      &IBM360_Init, &IBM360_Done, &IBM360_Reset, 
      &IBM360_Refresh, &IBM360_TickIntensityCount, &IBM360_DropFile
 };

// struct to hold the GUI controls Ids
static struct {
   int CtrlInfoPanel;
   int MT_DASD_InfoPanel1, MT_DASD_InfoPanel2;
   // IBM 360 Model 30 cpu panel
   int BoxLight;
   int Reg_CN_X, Reg_SA_CK, Reg_CR_CS, Reg_Count;
   int Reg_CH_Data[3], Reg_CH_Flags[3];
   int Reg_CH_MPX;
   int Reg_SAR, Reg_SDR;
   int Reg_ALU, Reg_BA, Reg_Status_Checks;
   // IBM 360 Model 40 cpu panel
   int Reg_Cons_Stats, Reg_ALU_Sel, Reg_LSAR_ROAR;
   int Reg_Roller1_16b, Reg_Roller2_16b; 
   int Reg_Data_16b, Reg_Addr_16b; 
   int Label_Roller1_L, Label_Roller2_L;
   int Label_Roller1_R, Label_Roller2_R;
   int BTN_Roller1, BTN_Roller2;
   // IBM 360 Model 50 cpu panel
   int Reg_Volt; 
   int Reg_Roller1_L, Reg_Roller2_L, Reg_Roller3_L, Reg_Roller4_L;
   int Reg_Roller1_R, Reg_Roller2_R, Reg_Roller3_R, Reg_Roller4_R;
   int Label_Roller3_L, Label_Roller4_L;
   int Label_Roller3_R, Label_Roller4_R;
   int BTN_Roller3, BTN_Roller4;
   int Reg_CPU_L, Reg_CPU_R, Reg_SDR_L, Reg_SDR_R, Reg_IAR_L, Reg_IAR_R; 
   int BTN_PSW_Restart; 
   int SW_Lamp_Test, Shdw_Lamp_Test; 
   // IBM 360 Model 65 cpu panel
   int Reg_Roller5_L, Reg_Roller6_L;
   int Reg_Roller5_R, Reg_Roller6_R;
   int Label_Roller5_L, Label_Roller6_L;
   int Label_Roller5_R, Label_Roller6_R;
   int BTN_Roller5, BTN_Roller6;
   int Reg_Stor_Proc_CHK;
   // IBM 370 Model 145 cpu panel
   int SwitchHandle; 
   int Reg_SysChk_L, Reg_SysChk_R, Reg_Cpu_Status, Reg_Cons_File;
   int SW_Storage_Select; 
   int BTN_Restart; 
   int SW_Addr_Compare_Cntrl; 
   // IBM 370 Model 148 cpu panel
   int Reg_Sys_Status; 
   // odometer
   int Odo[7], OdoDigitWhite;
   // ocp (operator control panel) lights
   int LI_System, LI_Manual, LI_Wait, LI_Test, LI_Load;
   // Rotary switches
   int SW_A, SW_B, SW_C, SW_D, SW_E, SW_F, SW_G, SW_H, SW_J;
   int SW_Rate, SW_Addr_Compare;
   // Buttons OCP panel;
   int BTN_Power_Off, BTN_Emergency_Off, Shdw_Emergency_Off;
   int BTN_Interrupt, BTN_Load;
   int BTN_System_Reset, BTN_Set_IC, BTN_Lamp_Test;
   int BTN_Start, BTN_Stop, BTN_Display, BTN_Store;
   // IBM 2540 Card Read Punch
   int ReadHopper, InputDeckBody, InputDeckTop;
   int ReadStacker, ReadStackerBack1, ReadStackerBack2, ReadStackerSeparator, CardInReadStacker;
   int PunchStacker, PunchStackerBack1, PunchStackerBack2, PunchStackerSeparator, CardInPunchStacker;
   int LI_Ready_CardReader, LI_EOF_CardReader, LI_Check_CardReader;
   int LI_Ready_CardPunch, LI_Check_CardPunch;
   int Drop_InputDeckFile; 
   // IBM 3525 Card Punch
   int PunchStacker1L, PunchStacker2L, PunchStacker1R, PunchStacker2R; 
   int CardDeckInStacker, LStackerBackground;
   // IBM 1403/3203 printer
   int Paper, PaperBackground, PrinterCharSet;
   int LI_Print_Ready, LI_Print_Check; 
   // IBM 1052 Printer Keyboard Console
   int ConsPaper, ConsPaperBackground, ConsPrinterCharSet, ConsGolfBall;
   int LI_Cons_Proceed, BTN_Cons_Request, BTN_Cons_NoFunction; 
   // IBM 3210 Printer Keyboard Console
   int LI_Cons_Req_Pending; 
   int BTN_Cons_Cancel, BTN_Cons_End;
   // IBM 2314 DASD
   int DASD_addr_digit[8][4], DASD_LI_Ready[8], DASD_SW_StartStop[8];
   // IBM 2401 Tape
   int MT_head, MT_VacColumn, MT_VacColMedium;
   int MT_reel_amount, MT_blue_reel, MT_red_reel, MT_green_reel;
   int MT_panel[8], MT[8], MT_addr_digit[8][4]; 
   int MT_LI_Select[8], MT_LI_Ready[8], MT_LI_File_protect[8];
   int MT_BTN_LoadRew[8], MT_BTN_Start[8], MT_BTN_UnLoad[8], MT_BTN_Reset[8];
   int MT_L_VacCol[8], MT_R_VacCol[8];
   int MT_BTN_DoorOpen[8];
   // IBM 2415 Tape
   int MT_Head_Medium[8], MT_Head_pos[8]; 
   // IBM 3420 Tape
   int MT_ring, MT_ring_anchor, MT_GlassSlidingDoor, MT_no_reel; 
   int MT_Take_reel_amount;
   int MT_Take[8], MT_GlassDoor[8], MT_Drop_TapeFile[8]; 
} S360 = {0}; // must be init to zero
// S360 is for System/360. Cannot use IBM360 because this symbol is already defined

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF IBM360_cp[] = {
    { &S360.CtrlInfoPanel,         "CtrlInfoPanel",                     NULL},
    { &S360.MT_DASD_InfoPanel1,    "MT_DASD_InfoPanel1",                NULL},
    { &S360.MT_DASD_InfoPanel2,    "MT_DASD_InfoPanel2",                NULL},
    // IBM 360 Model 30 cpu panel
    { &S360.BoxLight,              "BoxLight",                          NULL, "CpuType/2030"}, // back Light for indicators in model 30 panel
    { &S360.Reg_CN_X,              "Reg_CN_X",                          NULL, "CpuType/2030"}, // CN ADR W X register lights
    { &S360.Reg_SA_CK,             "Reg_SA_CK",                         NULL, "CpuType/2030"}, // SA CH CL CA CB CM CU CK register lights
    { &S360.Reg_CR_CS,             "Reg_CR_CS",                         NULL, "CpuType/2030"}, // CR CD CF CG CV CC CS register lights
    { &S360.Reg_Count,             "Reg_Count",                         NULL, "CpuType/2030"}, 
    { &S360.Reg_CH_Data[1],        "Reg_CH1_Data",                      NULL, "CpuType/2030"}, // Data Key Command for channel 1 lights
    { &S360.Reg_CH_Flags[1],       "Reg_CH1_Flags",                     NULL, "CpuType/2030"}, // Flags Tags Checks for channel 1 lights
    { &S360.Reg_CH_Data[2],        "Reg_CH2_Data",                      NULL, "CpuType/2030"}, // Data Key Command for channel 2 lights
    { &S360.Reg_CH_Flags[2],       "Reg_CH2_Flags",                     NULL, "CpuType/2030"}, // Flags Tags Checks for channel 2 lights
    { &S360.Reg_CH_MPX,            "Reg_CH_MPX",                        NULL, "CpuType/2030"}, // Tags Bus-out for mpx channel lights
    { &S360.Reg_SAR,               "Reg_SAR",                           NULL, "CpuType/2030"}, // Main Storage Address Register lights
    { &S360.Reg_SDR,               "Reg_SDR",                           NULL, "CpuType/2030"}, // Main Storage Data Register lights
    { &S360.Reg_ALU,               "Reg_ALU",                           NULL, "CpuType/2030"}, 
    { &S360.Reg_BA,                "Reg_BA",                            NULL, "CpuType/2030"}, // A register and B register lights
    { &S360.Reg_Status_Checks,     "Reg_Status_Checks",                 NULL, "CpuType/2030"}, // CPU STatus and CPU checks lights
    // IBM 360 Model 40 cpu panel
    { &S360.Reg_Cons_Stats,        "Reg_Cons_Stats",                    NULL, "CpuType/2040"}, 
    { &S360.Reg_ALU_Sel,           "Reg_ALU_Sel",                       NULL, "CpuType/2040"}, 
    { &S360.Reg_LSAR_ROAR,         "Reg_LSAR_ROAR",                     NULL, "CpuType/2040"}, 
    { &S360.Reg_Roller1_16b,       "Reg_Roller1_16b",                   NULL, "CpuType/2040"}, 
    { &S360.Reg_Roller2_16b,       "Reg_Roller2_16b",                   NULL, "CpuType/2040"}, 
    { &S360.Reg_Data_16b,          "Reg_Data_16b",                      NULL, "CpuType/2040"}, 
    { &S360.Reg_Addr_16b,          "Reg_Addr_16b",                      NULL, "CpuType/2040"}, 
    { &S360.Label_Roller1_L,       "Label_Roller1_L",                   NULL, "CpuType/2040"}, 
    { &S360.Label_Roller2_L,       "Label_Roller2_L",                   NULL, "CpuType/2040"}, 
    { &S360.Label_Roller1_R,       "Label_Roller1_R",                   NULL, "CpuType/2040"}, 
    { &S360.Label_Roller2_R,       "Label_Roller2_R",                   NULL, "CpuType/2040"}, 
    { &S360.BTN_Roller1,           "BTN_Roller1",                       &IBM360_OnClick_BTN, "CpuType/2040"},
    { &S360.BTN_Roller2,           "BTN_Roller2",                       &IBM360_OnClick_BTN, "CpuType/2040"},
    // IBM 360 Model 50 cpu panel
    { &S360.Reg_Volt,              "Reg_Volt",                          NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller1_L,         "Reg_Roller1_L",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller2_L,         "Reg_Roller2_L",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller3_L,         "Reg_Roller3_L",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller4_L,         "Reg_Roller4_L",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller1_R,         "Reg_Roller1_R",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller2_R,         "Reg_Roller2_R",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller3_R,         "Reg_Roller3_R",                     NULL, "CpuType/2050"}, 
    { &S360.Reg_Roller4_R,         "Reg_Roller4_R",                     NULL, "CpuType/2050"}, 
    { &S360.Label_Roller1_L,       "Label_Roller1_L",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller2_L,       "Label_Roller2_L",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller3_L,       "Label_Roller3_L",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller4_L,       "Label_Roller4_L",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller1_R,       "Label_Roller1_R",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller2_R,       "Label_Roller2_R",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller3_R,       "Label_Roller3_R",                   NULL, "CpuType/2050"}, 
    { &S360.Label_Roller4_R,       "Label_Roller4_R",                   NULL, "CpuType/2050"}, 
    { &S360.BTN_Roller1,           "BTN_Roller1",                       &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Roller2,           "BTN_Roller2",                       &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Roller3,           "BTN_Roller3",                       &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Roller4,           "BTN_Roller4",                       &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.Reg_CPU_L,             "Reg_CPU_L",                         NULL, "CpuType/2050"}, 
    { &S360.Reg_CPU_R,             "Reg_CPU_R",                         NULL, "CpuType/2050"}, 
    { &S360.Reg_SDR_L,             "Reg_SDR_L",                         NULL, "CpuType/2050"}, 
    { &S360.Reg_SDR_R,             "Reg_SDR_R",                         NULL, "CpuType/2050"}, 
    { &S360.Reg_IAR_L,             "Reg_IAR_L",                         NULL, "CpuType/2050"}, 
    { &S360.Reg_IAR_R,             "Reg_IAR_R",                         NULL, "CpuType/2050"}, 
    // IBM 360 Model 65 cpu panel
    { &S360.Reg_Volt,              "Reg_Volt",                          NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller1_L,         "Reg_Roller1_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller2_L,         "Reg_Roller2_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller3_L,         "Reg_Roller3_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller4_L,         "Reg_Roller4_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller5_L,         "Reg_Roller5_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller6_L,         "Reg_Roller6_L",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller1_R,         "Reg_Roller1_R",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller2_R,         "Reg_Roller2_R",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller3_R,         "Reg_Roller3_R",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller4_R,         "Reg_Roller4_R",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller5_R,         "Reg_Roller5_R",                     NULL, "CpuType/2065"}, 
    { &S360.Reg_Roller6_R,         "Reg_Roller6_R",                     NULL, "CpuType/2065"}, 
    { &S360.Label_Roller1_L,       "Label_Roller1_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller2_L,       "Label_Roller2_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller3_L,       "Label_Roller3_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller4_L,       "Label_Roller4_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller5_L,       "Label_Roller5_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller6_L,       "Label_Roller6_L",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller1_R,       "Label_Roller1_R",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller2_R,       "Label_Roller2_R",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller3_R,       "Label_Roller3_R",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller4_R,       "Label_Roller4_R",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller5_R,       "Label_Roller5_R",                   NULL, "CpuType/2065"}, 
    { &S360.Label_Roller6_R,       "Label_Roller6_R",                   NULL, "CpuType/2065"}, 
    { &S360.BTN_Roller1,           "BTN_Roller1",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.BTN_Roller2,           "BTN_Roller2",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.BTN_Roller3,           "BTN_Roller3",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.BTN_Roller4,           "BTN_Roller4",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.BTN_Roller5,           "BTN_Roller5",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.BTN_Roller6,           "BTN_Roller6",                       &IBM360_OnClick_BTN, "CpuType/2065"},
    { &S360.Reg_Stor_Proc_CHK,     "Reg_Stor_Proc_CHK",                 NULL, "CpuType/2065"}, 
    // IBM 370 Model 145 cpu panel
    { &S360.SwitchHandle,          "SwitchHandle",                      NULL, "CpuType/3145"}, 
    { &S360.Reg_SysChk_L,          "Reg_SysChk_L",                      NULL, "CpuType/3145"}, 
    { &S360.Reg_SysChk_R,          "Reg_SysChk_R",                      NULL, "CpuType/3145"}, 
    { &S360.Reg_Cpu_Status,        "Reg_Cpu_Status",                    NULL, "CpuType/3145"}, 
    { &S360.Reg_Cons_File,         "Reg_Cons_File",                     NULL, "CpuType/3145"}, 
    { &S360.Reg_Roller1_L,         "Reg_Roller1_L",                     NULL, "CpuType/3145"}, 
    { &S360.Reg_Roller1_R,         "Reg_Roller1_R",                     NULL, "CpuType/3145"}, 
    { &S360.Reg_Roller2_L,         "Reg_Roller2_L",                     NULL, "CpuType/3145"}, 
    { &S360.Reg_Roller2_R,         "Reg_Roller2_R",                     NULL, "CpuType/3145"}, 
    { &S360.Label_Roller1_L,       "Label_Roller1_L",                   NULL, "CpuType/3145"}, 
    { &S360.Label_Roller1_R,       "Label_Roller1_R",                   NULL, "CpuType/3145"}, 
    { &S360.Label_Roller2_L,       "Label_Roller2_L",                   NULL, "CpuType/3145"}, 
    { &S360.Label_Roller2_R,       "Label_Roller2_R",                   NULL, "CpuType/3145"}, 
    { &S360.BTN_Roller1,           "BTN_Roller1",                       &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.BTN_Roller2,           "BTN_Roller2",                       &IBM360_OnClick_BTN, "CpuType/3145"},
    // IBM 370 Model 148 cpu panel
    { &S360.SwitchHandle,          "SwitchHandle",                      NULL, "CpuType/3148"}, 
    { &S360.Reg_Sys_Status,        "Reg_Sys_Status",                    NULL, "CpuType/3148"}, 
    { &S360.Reg_Roller1_L,         "Reg_Roller1_L",                     NULL, "CpuType/3148"}, 
    { &S360.Reg_Roller1_R,         "Reg_Roller1_R",                     NULL, "CpuType/3148"}, 
    { &S360.Reg_Roller2_L,         "Reg_Roller2_L",                     NULL, "CpuType/3148"}, 
    { &S360.Reg_Roller2_R,         "Reg_Roller2_R",                     NULL, "CpuType/3148"}, 
    { &S360.Label_Roller1_L,       "Label_Roller1_L",                   NULL, "CpuType/3148"}, 
    { &S360.Label_Roller1_R,       "Label_Roller1_R",                   NULL, "CpuType/3148"}, 
    { &S360.BTN_Roller1,           "BTN_Roller1",                       &IBM360_OnClick_BTN, "CpuType/3148"},
    // odometer
    { &S360.Odo[1],                "Odo1",                              NULL}, 
    { &S360.Odo[2],                "Odo2",                              NULL}, 
    { &S360.Odo[3],                "Odo3",                              NULL}, 
    { &S360.Odo[4],                "Odo4",                              NULL}, 
    { &S360.Odo[5],                "Odo5",                              NULL}, 
    { &S360.Odo[6],                "Odo6",                              NULL}, 
    { &S360.OdoDigitWhite,         "OdoDigitWhite",                     NULL}, // holds all the white digitis in a continuous ribbon
    // ocp (operator control panel) lights
    { &S360.LI_System,             "LI_System",                         NULL}, 
    { &S360.LI_Manual,             "LI_Manual",                         NULL}, 
    { &S360.LI_Wait,               "LI_Wait",                           NULL}, 
    { &S360.LI_Test,               "LI_Test",                           NULL}, 
    { &S360.LI_Load,               "LI_Load",                           NULL}, 
    // Rotary switches
    { &S360.SW_A,                  "SW_A",                              &IBM360_OnClick_Sw},
    { &S360.SW_B,                  "SW_B",                              &IBM360_OnClick_Sw},
    { &S360.SW_C,                  "SW_C",                              &IBM360_OnClick_Sw},
    { &S360.SW_D,                  "SW_D",                              &IBM360_OnClick_Sw, "CpuType/2030"},
    { &S360.SW_F,                  "SW_F",                              &IBM360_OnClick_Sw, "CpuType/2030"},
    { &S360.SW_G,                  "SW_G",                              &IBM360_OnClick_Sw, "CpuType/2030"},
    { &S360.SW_H,                  "SW_H",                              &IBM360_OnClick_Sw, "CpuType/2030"},
    { &S360.SW_J,                  "SW_J",                              &IBM360_OnClick_Sw, "CpuType/2030"},
    { &S360.SW_D,                  "SW_D",                              &IBM360_OnClick_Sw, "CpuType/3145"},
    { &S360.SW_E,                  "SW_E",                              &IBM360_OnClick_Sw, "CpuType/3145"},
    { &S360.SW_F,                  "SW_F",                              &IBM360_OnClick_Sw, "CpuType/3145"},
    { &S360.SW_G,                  "SW_G",                              &IBM360_OnClick_Sw, "CpuType/3145"},
    { &S360.SW_H,                  "SW_H",                              &IBM360_OnClick_Sw, "CpuType/3145"},
    { &S360.SW_D,                  "SW_D",                              &IBM360_OnClick_Sw, "CpuType/3148"},
    { &S360.SW_E,                  "SW_E",                              &IBM360_OnClick_Sw, "CpuType/3148"},
    { &S360.SW_F,                  "SW_F",                              &IBM360_OnClick_Sw, "CpuType/3148"},
    { &S360.SW_G,                  "SW_G",                              &IBM360_OnClick_Sw, "CpuType/3148"},
    { &S360.SW_H,                  "SW_H",                              &IBM360_OnClick_Sw, "CpuType/3148"},
    { &S360.SW_Rate,               "SW_Rate",                           &IBM360_OnClick_Sw2},
    { &S360.SW_Rate,               "SW_Rate",                           &IBM360_OnClick_Sw3, "CpuType/3145"}, // redefine click event handler for 3145 cpu
    { &S360.SW_Addr_Compare,       "SW_Addr_Compare",                   &IBM360_OnClick_Sw2, "CpuType/2030"},
    { &S360.SW_Addr_Compare,       "SW_Addr_Compare",                   &IBM360_OnClick_Sw3, "CpuType/3145"},
    { &S360.SW_Addr_Compare_Cntrl, "SW_Addr_Compare_Cntrl",             &IBM360_OnClick_Sw2, "CpuType/3145"},
    { &S360.SW_Storage_Select,     "SW_Storage_Select",                 &IBM360_OnClick_Sw3, "CpuType/3145"},
    { &S360.SW_Addr_Compare,       "SW_Addr_Compare",                   &IBM360_OnClick_Sw3, "CpuType/3148"},
    { &S360.SW_Addr_Compare_Cntrl, "SW_Addr_Compare_Cntrl",             &IBM360_OnClick_Sw2, "CpuType/3148"},
    { &S360.SW_Storage_Select,     "SW_Storage_Select",                 &IBM360_OnClick_Sw2, "CpuType/3148"},
    // Buttons 
    { &S360.BTN_Power_Off,         "BTN_Power_Off",                     &IBM360_OnClick_BTN},
    { &S360.Shdw_Emergency_Off,    "Shdw_Emergency_Power_Off",          NULL               , "CpuType/2030"}, 
    { &S360.BTN_Emergency_Off,     "BTN_Emergency_Power_Off",           &IBM360_OnClick_BTN},
    { &S360.BTN_Interrupt,         "BTN_Interrupt",                     &IBM360_OnClick_BTN},
    { &S360.BTN_Load,              "BTN_Load",                          &IBM360_OnClick_BTN},
    { &S360.BTN_System_Reset,      "BTN_System_Reset",                  &IBM360_OnClick_BTN},
    { &S360.BTN_Start,             "BTN_Start",                         &IBM360_OnClick_BTN},
    { &S360.BTN_Stop,              "BTN_Stop",                          &IBM360_OnClick_BTN},
    { &S360.BTN_Set_IC,            "BTN_Set_IC",                        &IBM360_OnClick_BTN, "CpuType/2030"},
//    { &S360.BTN_Set_IC,            "BTN_Set_IC",                        &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Store,             "BTN_Store",                         &IBM360_OnClick_BTN, "CpuType/2030"},
//    { &S360.BTN_Store,             "BTN_Store",                         &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Display,           "BTN_Display",                       &IBM360_OnClick_BTN, "CpuType/2030"},
//    { &S360.BTN_Display,           "BTN_Display",                       &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.BTN_Lamp_Test,         "BTN_Lamp_Test",                     &IBM360_OnClick_BTN, "CpuType/2030"},
//    { &S360.BTN_PSW_Restart,       "BTN_PSW_Restart",                   &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.SW_Lamp_Test,          "SW_Lamp_Test",                      &IBM360_OnClick_BTN, "CpuType/2050"},
    { &S360.Shdw_Lamp_Test,        "Shdw_Lamp_Test",                    NULL               , "CpuType/2050"}, 
    { &S360.BTN_Restart,           "BTN_Restart",                       &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.BTN_Set_IC,            "BTN_Set_IC",                        &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.BTN_Store,             "BTN_Store",                         &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.BTN_Display,           "BTN_Display",                       &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.SW_Lamp_Test,          "SW_Lamp_Test",                      &IBM360_OnClick_BTN, "CpuType/3145"},
    { &S360.SW_Lamp_Test,          "SW_Lamp_Test",                      &IBM360_OnClick_BTN, "CpuType/3148"},
    { &S360.BTN_Restart,           "BTN_Restart",                       &IBM360_OnClick_BTN, "CpuType/3148"},
    { &S360.BTN_Set_IC,            "BTN_Set_IC",                        &IBM360_OnClick_BTN, "CpuType/3148"},
    { &S360.BTN_Store,             "BTN_Store",                         &IBM360_OnClick_BTN, "CpuType/3148"},
    { &S360.BTN_Display,           "BTN_Display",                       &IBM360_OnClick_BTN, "CpuType/3148"},
    // IBM 2540 Card Read Punch
    { &S360.ReadHopper,            "ReadHopper",                        NULL,     "Card/2540" },
    { &S360.InputDeckBody,         "InputDeckBody",                     NULL,     "Card/2540" },
    { &S360.InputDeckTop,          "InputDeckTop",                      NULL,     "Card/2540" },
    { &S360.ReadStacker,           "ReadStacker",                       NULL,     "Card/2540" },
    { &S360.ReadStackerBack1,      "ReadStackerBack1",                  NULL,     "Card/2540" },
    { &S360.ReadStackerBack2,      "ReadStackerBack2",                  NULL,     "Card/2540" },
    { &S360.ReadStackerSeparator,  "ReadStackerSeparator",              NULL,     "Card/2540" },
    { &S360.CardInReadStacker,     "CardInReadStacker",                 NULL,     "Card/2540" },
    { &S360.PunchStacker,          "PunchStacker",                      NULL,     "Card/2540" },
    { &S360.PunchStackerBack1,     "PunchStackerBack1",                 NULL,     "Card/2540" },
    { &S360.PunchStackerBack2,     "PunchStackerBack2",                 NULL,     "Card/2540" },
    { &S360.PunchStackerSeparator, "PunchStackerSeparator",             NULL,     "Card/2540" },
    { &S360.CardInPunchStacker,    "CardInPunchStacker",                NULL,     "Card/2540" },
    { &S360.LI_Ready_CardReader,   "LI_Ready_CardReader",               NULL,     "Card/2540" },
    { &S360.LI_EOF_CardReader,     "LI_EOF_CardReader",                 NULL,     "Card/2540" },
    { &S360.LI_Check_CardReader,   "LI_Check_CardReader",               NULL,     "Card/2540" },
    { &S360.LI_Ready_CardPunch,    "LI_Ready_CardPunch",                NULL,     "Card/2540" },
    { &S360.LI_Check_CardPunch,    "LI_Check_CardPunch",                NULL,     "Card/2540" },
    { &S360.Drop_InputDeckFile,    "Drop_InputDeckFile",                NULL,     "Card/2540" },
    // IBM 3525 Card Punch
    { &S360.PunchStacker,          "PunchStacker",                      NULL,     "Card/3525" },
    { &S360.PunchStacker1L,        "PunchStacker1L",                    NULL,     "Card/3525" },
    { &S360.PunchStacker2L,        "PunchStacker2L",                    NULL,     "Card/3525" },
    { &S360.PunchStackerSeparator, "PunchStackerSeparator",             NULL,     "Card/3525" },
    { &S360.PunchStacker1R,        "PunchStacker1R",                    NULL,     "Card/3525" },
    { &S360.PunchStacker2R,        "PunchStacker2R",                    NULL,     "Card/3525" },
    { &S360.CardInPunchStacker,    "CardInPunchStacker",                NULL,     "Card/3525" },
    { &S360.CardDeckInStacker,     "CardDeckInStacker",                 NULL,     "Card/3525" },
    { &S360.LStackerBackground,    "LStackerBackground",                NULL,     "Card/3525" },
    { &S360.LI_Ready_CardPunch,    "LI_Ready_CardPunch",                NULL,     "Card/3525" },
    { &S360.LI_EOF_CardReader,     "LI_EOF_CardReader",                 NULL,     "Card/3525" },
    { &S360.Drop_InputDeckFile,    "Drop_InputDeckFile",                NULL,     "Card/3525" },
    // IBM 1403/3203 Printer
    { &S360.Paper,                 "Paper",                             NULL,     "cardprinter/1" },
    { &S360.PaperBackground,       "PaperBackground",                   NULL,     "cardprinter/1" },
    { &S360.PrinterCharSet,        "PrinterCharSet",                    NULL,     "cardprinter/1" },
    { &S360.LI_Print_Ready,        "LI_Print_Ready",                    NULL,     "cardprinter/1" },
    { &S360.LI_Print_Check,        "LI_Print_Check",                    NULL,     "cardprinter/1" },
    // IBM 1052 Printer Keyboard Console
    { &S360.ConsPaper,             "ConsPaper",                         NULL,     "console/1" },  // console/1 are control
    { &S360.ConsPaperBackground,   "ConsPaperBackground",               NULL,     "console/1" },  // common to both consoles
    { &S360.ConsPrinterCharSet,    "ConsPrinterCharSet",                NULL,     "console/1" },  // ConsType/nnnn are controls
    { &S360.ConsGolfBall,          "ConsGolfBall",                      NULL,     "console/1" },  // specific of that console 
    { &S360.LI_Cons_Proceed,       "LI_Cons_Proceed",                   NULL,     "console/1" },  // type 
    { &S360.BTN_Cons_Request,      "BTN_Cons_Request",                  &IBM360_OnClick_BTN,   "console/1" },
    { &S360.BTN_Cons_NoFunction,   "BTN_Cons_NoFunction",               &IBM360_OnClick_BTN,   "ConsType/1052"},
    // IBM 3210 Printer Keyboard Console
    { &S360.LI_Cons_Req_Pending,   "LI_Cons_Req_Pending",               NULL,     "ConsType/3210"},
    { &S360.BTN_Cons_Cancel,       "BTN_Cons_Cancel",                   &IBM360_OnClick_BTN,   "ConsType/3210"},
    { &S360.BTN_Cons_End,          "BTN_Cons_End",                      &IBM360_OnClick_BTN,   "ConsType/3210"},
    // IBM 2314 DASD 
    { &S360.DASD_addr_digit[0][1], "DASD_diskA_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[0][2], "DASD_diskA_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[0][3], "DASD_diskA_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[0],  "DASD_diskA_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[0],      "DASD_diskA_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[1][1], "DASD_diskB_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[1][2], "DASD_diskB_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[1][3], "DASD_diskB_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[1],  "DASD_diskB_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[1],      "DASD_diskB_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[2][1], "DASD_diskC_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[2][2], "DASD_diskC_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[2][3], "DASD_diskC_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[2],  "DASD_diskC_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[2],      "DASD_diskC_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[3][1], "DASD_diskD_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[3][2], "DASD_diskD_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[3][3], "DASD_diskD_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[3],  "DASD_diskD_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[3],      "DASD_diskD_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[4][1], "DASD_diskE_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[4][2], "DASD_diskE_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[4][3], "DASD_diskE_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[4],  "DASD_diskE_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[4],      "DASD_diskE_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[5][1], "DASD_diskF_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[5][2], "DASD_diskF_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[5][3], "DASD_diskF_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[5],  "DASD_diskF_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[5],      "DASD_diskF_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[6][1], "DASD_diskG_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[6][2], "DASD_diskG_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[6][3], "DASD_diskG_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[6],  "DASD_diskG_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[6],      "DASD_diskG_LI_Ready",            NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[7][1], "DASD_diskH_digit1",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[7][2], "DASD_diskH_digit2",                 NULL,     "dasd/1" },
    { &S360.DASD_addr_digit[7][3], "DASD_diskH_digit3",                 NULL,     "dasd/1" },
       { &S360.DASD_SW_StartStop[7],  "DASD_diskH_SW_StartStop",        NULL,     "dasd/1" },
       { &S360.DASD_LI_Ready[7],      "DASD_diskH_LI_Ready",            NULL,     "dasd/1" },
    // IBM 2401, 2415, 3420 Tape
    { &S360.MT_head,               "MT_head",                           NULL,     "tapecab/1" },
    { &S360.MT_VacColumn,          "MT_VacColumn",                      NULL,     "tapecab/1" },
    { &S360.MT_VacColMedium,       "MT_VacColMedium",                   NULL,     "tapecab/1" },
    { &S360.MT_reel_amount,        "MT_reel_amount",                    NULL,     "tapecab/1" },
    { &S360.MT_Take_reel_amount,   "MT_Take_reel_amount",               NULL,     "tapecab/1", "TapeType/3420" },
    { &S360.MT_blue_reel,          "MT_blue_reel",                      NULL,     "tapecab/1" },
    { &S360.MT_red_reel,           "MT_red_reel",                       NULL,     "tapecab/1" },
    { &S360.MT_green_reel,         "MT_green_reel",                     NULL,     "tapecab/1" },
    { &S360.MT_ring,               "MT_ring",                           NULL,     "tapecab/1", "TapeType/3420" },
    { &S360.MT_no_reel,            "MT_no_reel",                        NULL,     "tapecab/1", "TapeType/3420" },
    { &S360.MT_ring_anchor,        "MT_ring_anchor",                    NULL,     "tapecab/1", "TapeType/3420" },
    { &S360.MT_GlassSlidingDoor,   "MT_GlassSlidingDoor",               NULL,     "tapecab/1", "TapeType/3420" },
    { &S360.MT_panel[0],           "MT_0_panel",                        NULL,     "tape0/1" },
       { &S360.MT_addr_digit[0][1],   "MT_0_digit1",                    NULL,     "tape0/1" },
       { &S360.MT_addr_digit[0][2],   "MT_0_digit2",                    NULL,     "tape0/1" },
       { &S360.MT_addr_digit[0][3],   "MT_0_digit3",                    NULL,     "tape0/1" },
       { &S360.MT_LI_Select[0],       "MT_0_LI_Select",                 NULL,     "tape0/1" },
       { &S360.MT_LI_Ready[0],        "MT_0_LI_Ready",                  NULL,     "tape0/1" },
       { &S360.MT_LI_File_protect[0], "MT_0_LI_File_protect",           NULL,     "tape0/1" },
       { &S360.MT_Take[0],            "MT_0_Take",                      NULL,     "tape0/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[0],       "MT_0_GlassSlidingDoor",          NULL,     "tape0/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[0],   "MT_0_Drop_TapeFile",             NULL,     "tape0/1", "TapeType/3420" },
          { &S360.MT[0],              "MT_0",                           NULL,     "tape0/1" },
          { &S360.MT_L_VacCol[0],     "MT_0_L_VacCol",                  NULL,     "tape0/1" },
          { &S360.MT_R_VacCol[0],     "MT_0_R_VacCol",                  NULL,     "tape0/1" },
       { &S360.MT_BTN_LoadRew[0],     "MT_0_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape0/1"  },
       { &S360.MT_BTN_Start[0],       "MT_0_BTN_Start",                 &IBM360_OnClick_BTN2, "tape0/1"  },
       { &S360.MT_BTN_Reset[0],       "MT_0_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape0/1"  },
       { &S360.MT_BTN_DoorOpen[0],    "MT_0_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape0/1"  },
       { &S360.MT_Head_Medium[0],     "MT_0_Head_Medium",               NULL,     "tape0/1","TapeType/2415" },
       { &S360.MT_Head_pos[0],        "MT_0_Head",                      NULL,     "tape0/1","TapeType/2415" },
    { &S360.MT_panel[1],           "MT_1_panel",                        NULL,     "tape1/1" },
       { &S360.MT_addr_digit[1][1],   "MT_1_digit1",                    NULL,     "tape1/1" },
       { &S360.MT_addr_digit[1][2],   "MT_1_digit2",                    NULL,     "tape1/1" },
       { &S360.MT_addr_digit[1][3],   "MT_1_digit3",                    NULL,     "tape1/1" },
       { &S360.MT_LI_Select[1],       "MT_1_LI_Select",                 NULL,     "tape1/1" },
       { &S360.MT_LI_Ready[1],        "MT_1_LI_Ready",                  NULL,     "tape1/1" },
       { &S360.MT_LI_File_protect[1], "MT_1_LI_File_protect",           NULL,     "tape1/1" },
       { &S360.MT_Take[1],            "MT_1_Take",                      NULL,     "tape1/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[1],       "MT_1_GlassSlidingDoor",          NULL,     "tape1/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[1],   "MT_1_Drop_TapeFile",             NULL,     "tape1/1", "TapeType/3420" },
       { &S360.MT[1],                 "MT_1",                           NULL,     "tape1/1" },
          { &S360.MT_L_VacCol[1],     "MT_1_L_VacCol",                  NULL,     "tape1/1" },
          { &S360.MT_R_VacCol[1],     "MT_1_R_VacCol",                  NULL,     "tape1/1" },
       { &S360.MT_BTN_LoadRew[1],     "MT_1_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape1/1"  },
       { &S360.MT_BTN_Start[1],       "MT_1_BTN_Start",                 &IBM360_OnClick_BTN2, "tape1/1"  },
       { &S360.MT_BTN_Reset[1],       "MT_1_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape1/1"  },
       { &S360.MT_BTN_DoorOpen[1],    "MT_1_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape1/1"  },
       { &S360.MT_Head_Medium[1],     "MT_1_Head_Medium",               NULL,     "tape1/1", "TapeType/2415" },
       { &S360.MT_Head_pos[1],        "MT_1_Head",                      NULL,     "tape1/1", "TapeType/2415" },
    { &S360.MT_panel[2],           "MT_2_panel",                        NULL,     "tape23/1" },
       { &S360.MT_addr_digit[2][1],   "MT_2_digit1",                    NULL,     "tape23/1" },
       { &S360.MT_addr_digit[2][2],   "MT_2_digit2",                    NULL,     "tape23/1" },
       { &S360.MT_addr_digit[2][3],   "MT_2_digit3",                    NULL,     "tape23/1" },
       { &S360.MT_LI_Select[2],       "MT_2_LI_Select",                 NULL,     "tape23/1" },
       { &S360.MT_LI_Ready[2],        "MT_2_LI_Ready",                  NULL,     "tape23/1" },
       { &S360.MT_LI_File_protect[2], "MT_2_LI_File_protect",           NULL,     "tape23/1" },
       { &S360.MT_Take[2],            "MT_2_Take",                      NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[2],       "MT_2_GlassSlidingDoor",          NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[2],   "MT_2_Drop_TapeFile",             NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT[2],                 "MT_2",                           NULL,     "tape23/1" },
          { &S360.MT_L_VacCol[2],     "MT_2_L_VacCol",                  NULL,     "tape23/1" },
          { &S360.MT_R_VacCol[2],     "MT_2_R_VacCol",                  NULL,     "tape23/1" },
       { &S360.MT_BTN_LoadRew[2],     "MT_2_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_Start[2],       "MT_2_BTN_Start",                 &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_Reset[2],       "MT_2_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_DoorOpen[2],    "MT_2_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_Head_Medium[2],     "MT_2_Head_Medium",               NULL,     "tape23/1", "TapeType/2415" },
       { &S360.MT_Head_pos[2],        "MT_2_Head",                      NULL,     "tape23/1", "TapeType/2415" },
    { &S360.MT_panel[3],           "MT_3_panel",                        NULL,     "tape23/1" },
       { &S360.MT_addr_digit[3][1],   "MT_3_digit1",                    NULL,     "tape23/1" },
       { &S360.MT_addr_digit[3][2],   "MT_3_digit2",                    NULL,     "tape23/1" },
       { &S360.MT_addr_digit[3][3],   "MT_3_digit3",                    NULL,     "tape23/1" },
       { &S360.MT_LI_Select[3],       "MT_3_LI_Select",                 NULL,     "tape23/1" },
       { &S360.MT_LI_Ready[3],        "MT_3_LI_Ready",                  NULL,     "tape23/1" },
       { &S360.MT_LI_File_protect[3], "MT_3_LI_File_protect",           NULL,     "tape23/1" },
       { &S360.MT_Take[3],            "MT_3_Take",                      NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[3],       "MT_3_GlassSlidingDoor",          NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[3],   "MT_3_Drop_TapeFile",             NULL,     "tape23/1", "TapeType/3420" },
       { &S360.MT[3],                 "MT_3",                           NULL,     "tape23/1" },
          { &S360.MT_L_VacCol[3],     "MT_3_L_VacCol",                  NULL,     "tape23/1" },
          { &S360.MT_R_VacCol[3],     "MT_3_R_VacCol",                  NULL,     "tape23/1" },
       { &S360.MT_BTN_LoadRew[3],     "MT_3_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_Start[3],       "MT_3_BTN_Start",                 &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_Reset[3],       "MT_3_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_BTN_DoorOpen[3],    "MT_3_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape23/1"  },
       { &S360.MT_Head_Medium[3],     "MT_3_Head_Medium",               NULL,     "tape23/1", "TapeType/2415" },
       { &S360.MT_Head_pos[3],        "MT_3_Head",                      NULL,     "tape23/1", "TapeType/2415" },
    { &S360.MT_panel[4],           "MT_4_panel",                        NULL,     "tape45/1" },
       { &S360.MT_addr_digit[4][1],   "MT_4_digit1",                    NULL,     "tape45/1" },
       { &S360.MT_addr_digit[4][2],   "MT_4_digit2",                    NULL,     "tape45/1" },
       { &S360.MT_addr_digit[4][3],   "MT_4_digit3",                    NULL,     "tape45/1" },
       { &S360.MT_LI_Select[4],       "MT_4_LI_Select",                 NULL,     "tape45/1" },
       { &S360.MT_LI_Ready[4],        "MT_4_LI_Ready",                  NULL,     "tape45/1" },
       { &S360.MT_LI_File_protect[4], "MT_4_LI_File_protect",           NULL,     "tape45/1" },
       { &S360.MT_Take[4],            "MT_4_Take",                      NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[4],       "MT_4_GlassSlidingDoor",          NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[4],   "MT_4_Drop_TapeFile",             NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT[4],                 "MT_4",                           NULL,     "tape45/1" },
          { &S360.MT_L_VacCol[4],     "MT_4_L_VacCol",                  NULL,     "tape45/1" },
          { &S360.MT_R_VacCol[4],     "MT_4_R_VacCol",                  NULL,     "tape45/1" },
       { &S360.MT_BTN_LoadRew[4],     "MT_4_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_Start[4],       "MT_4_BTN_Start",                 &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_Reset[4],       "MT_4_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_DoorOpen[4],    "MT_4_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_Head_Medium[4],     "MT_4_Head_Medium",               NULL,     "tape45/1", "TapeType/2415" },
       { &S360.MT_Head_pos[4],        "MT_4_Head",                      NULL,     "tape45/1", "TapeType/2415" },
    { &S360.MT_panel[5],           "MT_5_panel",                        NULL,     "tape45/1" },
       { &S360.MT_addr_digit[5][1],   "MT_5_digit1",                    NULL,     "tape45/1" },
       { &S360.MT_addr_digit[5][2],   "MT_5_digit2",                    NULL,     "tape45/1" },
       { &S360.MT_addr_digit[5][3],   "MT_5_digit3",                    NULL,     "tape45/1" },
       { &S360.MT_LI_Select[5],       "MT_5_LI_Select",                 NULL,     "tape45/1" },
       { &S360.MT_LI_Ready[5],        "MT_5_LI_Ready",                  NULL,     "tape45/1" },
       { &S360.MT_LI_File_protect[5], "MT_5_LI_File_protect",           NULL,     "tape45/1" },
       { &S360.MT_Take[5],            "MT_5_Take",                      NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT_GlassDoor[5],       "MT_5_GlassSlidingDoor",          NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT_Drop_TapeFile[5],   "MT_5_Drop_TapeFile",             NULL,     "tape45/1", "TapeType/3420" },
       { &S360.MT[5],                 "MT_5",                           NULL,     "tape45/1" },
          { &S360.MT_L_VacCol[5],     "MT_5_L_VacCol",                  NULL,     "tape45/1" },
          { &S360.MT_R_VacCol[5],     "MT_5_R_VacCol",                  NULL,     "tape45/1" },
       { &S360.MT_BTN_LoadRew[5],     "MT_5_BTN_LoadRew",               &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_Start[5],       "MT_5_BTN_Start",                 &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_Reset[5],       "MT_5_BTN_Reset",                 &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_BTN_DoorOpen[5],    "MT_5_BTN_DoorOpen",              &IBM360_OnClick_BTN2, "tape45/1"  },
       { &S360.MT_Head_Medium[5],     "MT_5_Head_Medium",               NULL,     "tape45/1", "TapeType/2415" },
       { &S360.MT_Head_pos[5],        "MT_5_Head",                      NULL,     "tape45/1", "TapeType/2415" },
       
    { NULL }  
};

int bCardReadPunchVisible;                    // ibm 2450 is visible
int bPrintOutVisible;                         // printout visible
int bTapesVisible;                            // tape visible
int bDasdVisible;                             // dasd visible

// animation state vars (for dynamic state draw)
// for card reader
int nCardsInReadHopper               = 0; // number of cards alredy draw in input hopper
int nCardsInReadStacker              = 0; // number of cards alredy draw in read stacker 
int nCardsInPunchStacker             = 0; // number of cards alredy draw in punch stacker 
int bCardReadHopperBackgroundSet     = 0; // flag to allow grabbing background behind read hopper

// for main cpu
int bCpuModelIs                = 2030; // cpu model being displayed in control panel gui. Can be 2030, 2040, 2050, 2065, 3145, 3148
int bSystemResetKeyPressed     = 0; // set when system reset key is pressed
int IPLaddr                    = 0; // IPL addr when channel starts IPL (triggered by boot scp command)
int bRegBA_display             = 0; // signal reg B A displays PC when stopped, =1 will display MSDR
struct {                            // odometer values
   int tnow;
   int msec;
   int sec;
   int hours;
   int hour_changed_flag; 
} OdoMeter = {0,0,0,0,0};
int bShowInfo                  = 0; // flag to show info for ^I 
uint32 ShowInfoTm0             = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                 = 0; // instr executed on ShowInfoTm0
int InstrCycles0               = 0; // instr cycles executed on ShowInfoTm0
int FramesCount0               = 0; // num of frames on ShowInfoTm0

// for tape cabinet
int bTapeIs                    = 2401;      // bTapeIs can be 2401, 2415, 3420
#define MT_anim_sequence_len     1500       // max number of animation sequence steps for tape
struct mtcabrec {                           // mtcab[0..7].reel[0..1] is a record that holds the tape reel states for MTA0..7 units
   int mt_is;                               // current visual animation being done
   int ResetPressed;                        // 1=reset button has been pressed on tape control panel 
   // state of tape elements
   int rew_u3;                              // amount of tape medium on R reel during rewind operation (used to show info with ^I)
   int rw_tm0, rw_msec, rw_dir;             // r/w operation start (sim_os_msec time), duration left (msec), tape direction (1->fwd, -1->backwrd)
   struct mtreelrec {               
       int color;                           // reel color
       int VacCol_h;                        // ammount of medium (x1000 inch) in vacuum column. 0=no medium, >0 medium loop going down into the vaccol in direction of upper sensor
       double rpm;                          // reel current revolutions per second (0=stopped, <0=backwards)
       int ang;                             // reel current angular position (but NOT normalizaed to 0..360 degrees! normalize before using)
       int motor;                           // reel motor operation in progress (-1=accelerate backwards, 0=decelerate, 1=accelerate forward)
       int tm0;                             // timestamp when reel motor start accelerating/decelerating (in sim_os_msec time). =0 when motor stoped
       double rpm0;                         // revolutions per second (0=stopped, <0=backwards) when reel motor start accelerating/decelerating 
       int ang0;                            // reel angular position (0..360 degrees) when reel motor start accelerating/decelerating
       double revs;                         // reel revoltions done after motor start accel/decel (1 revolution = 360 gr)
       char debug_text[100];                // debug text (if any) to be draw in reels hub on refresh
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
   struct {
       int MT_L_Rot, MT_R_Rot;  // holds last state of control dynamically draw
       int MT_Head;             
       int MT_Reel_Ammount; 
       int MT_DoorOpen;         // 1= tape cabinet door open
   } state;
} mtcab[8] = {0};
int bTapeAnimInProgress;                    // signals tape medium/reels are moving

// for dasd disk-packs cabinets
struct dasdcabrec {                         // dasdcab[0..7].uptr points to dasd unit in the cabinet
    UNIT * uptr;                            // to be displayed in dasd storage facility as disk-packs A to H
} dasdcab[8] = {NULL};

// for printer printout
int lptPrintOutDoneCount       = -1; // number of lines already printed on cpanel paper
int hPaperBackgroundOffset;          // offset of background image on paper image

// for console printout in its own window 
int bConsTypeIs                = 1052; // console model being displayed in control panel gui
int ncp_console = -1;                // the console window 
int ConsPrintOutDoneCount      = -1; // number of lines already printed on console paper
int hConsPaperBackgroundOffset;      // offset of background image on paper image


void SetBoxLights(int CArrayId)
{
    int nItems, nItem, nState; 
    int CId; 
    uint32 * surface0; 
    uint32 * surface1;
    uint32 * surface2;
    uint32 col; 
    int w0, h0, ww, hh, w1, h1; 
    int x,y,p1,x0,y0, IsRed; 
    int rr,gg,bb, alpha, rr_bg,gg_bg,bb_bg;

    // get the boxlight surface
    surface0 = GetControlSurface(S360.BoxLight, 0, &ww, &hh);
    w0=ww; h0=hh;

    // iterate on control items to select each individual light of array given as param
    nItems=GetControlInfo(CArrayId, CINFO_NITEMS);
    for (nItem=0; nItem<nItems; nItem++) {
       // get individual control 
       CId=GetCArrayCId(CArrayId, nItem);
       for (nState=2; nState<16; nState++) {
           // put states 2..32 on 2..16
           CopyControlImage(CId, nState*2,   0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                            CId, nState,     0, 0);       // ToCId, ToState,     x1, y1
       }
       // get control state 1 surface
       surface1 = GetControlSurface(CId, 1, &ww, &hh);
       w1=ww; h1=hh;
       // Detect red labels on controls state image
       IsRed=0;
       for(y=0;y<h1;y++) for (x=0;x<w1;x++) {
           col = surface1[y*w1+x];
           get_surface_rgb_color(col,&rr,&gg,&bb);      // get control state pixel 
           if ((rr > 150) && (rr > (int)(gg * 1.8)) && (rr > (int)(bb * 1.8))) {
               IsRed=1; break; 
           }
       }
       // blend image of control states 1 with BoxLight surface to generate states 16..31, 1
       x0=(w0-w1)/2; 
       y0=(h0-h1)/2; 
       for (nState=16; nState<=32; nState++) {
           if (nState<16) break; 
           alpha = 32-nState; // state 16..31,1 -> alpha 16..0
           alpha = alpha * 16; 
           if (nState==32) nState=1; 
           surface2 = GetControlSurface(CId, nState, &ww, &hh);
           for(y=0;y<h1;y++) {
               if (y+y0<0) continue; if (y+y0>h0) break; 
               for (x=0;x<w1;x++) {
                  if (x+x0<0) continue; if (x+x0>w0) break; 
                  col = surface1[p1=y*w1+x];
                  get_surface_rgb_color(col,&rr,&gg,&bb);      // get control state pixel 
                  get_surface_rgb_color(surface0[(y+y0)*w0+x+x0],&rr_bg,&gg_bg,&bb_bg); // get BoxLight pixel
                  if (IsRed) { // if label is red, turn BoxLight pixel to red
                      rr_bg = rr_bg * 100/125;
                      gg_bg = gg_bg * 100/175;
                      bb_bg = bb_bg * 100/175;
                  }
                  // if control state pixel more illuminated than the BoxLight one, keep control state pixel
                  if (rr + gg + bb > rr_bg + gg_bg + bb_bg) continue; 
                  // if alpha=255 -> color is rr. If alfa=0 -> color is rr_bg
                  rr = rr_bg + (alpha * (rr-rr_bg)) / 256; 
                  gg = gg_bg + (alpha * (gg-gg_bg)) / 256; 
                  bb = bb_bg + (alpha * (bb-bb_bg)) / 256; 
                  col = surface_rgb_color(rr,gg,bb);
                  surface1[p1]=col;
               }
           }
       }
    }
}

void Rotate(int CId, int State0, int Rot, int xctr, int yctr, int State1)
{
    // will copy state0 image from control CId to State1 image on same control CId
    // State0 and State1 cannot be the same
    // rotating pixes 90, 180 or 270gr. Center of rotation is xctr, yctr;

    int x,y,ww,hh,x0,y0,x1,y1;
    uint32 * surface0; 
    uint32 * surface1; 
    uint32 col; 

    surface0 = GetControlSurface(CId, State0, &ww, &hh);
    surface1 = GetControlSurface(CId, State1, &ww, &hh);

    for(y=-hh;y<hh;y++) {
       for(x=-ww;x<ww;x++) {
           x0=x+xctr; y0=y+yctr; 
           if ((x0<0) || (x0>=ww)) continue;  // safety
           if ((y0<0) || (y0>=hh)) continue;  
           col=surface0[x0 + y0 * ww]; // source pixel
           if (Rot== 90) { x1=xctr-y; y1=yctr+x; } else
           if (Rot==180) { x1=xctr-x; y1=yctr-y; } else
           {              x1=xctr+y; y1=yctr-x; }
           if ((x1<0) || (x1>=ww)) continue;  // safety
           if ((y1<0) || (y1>=hh)) continue;  
           surface1[x1 + y1 * ww]=col; // source pixel          
       }
    }
}

// horizontal skew states in CId based on nSkew. add switch handle with given state image
void SetRotatingSwitchSkew(int CId, int nSkew, int nSwitchHandle)
{
    int nState, nStates, ww,hh,n,x,y, rr,gg,bb, rr0,gg0,bb0, rr1,bb1,gg1; 
    int dir, xint, xfrac, x_org;
    int skew100; 
    uint32 * surface0; 
    uint32 col; 
    uint32 xline[1000];

    nStates=GetControlInfo(CId, CINFO_NSTATES);
    dir = (nSkew > 0) ? 1:-1; // direction of skew

    // for each state image in rotationg switch control
    for (nState=0; nState<nStates; nState++) {
        // add image for rotating switch handle 
        CopyControlImage(S360.SwitchHandle, nSwitchHandle,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId, nState,  0,0);                                  // ToCId, ToState,     x1, y1
        // set image skew
        if (nSkew==0) continue; // no skew

        // nSkew=-100 -> skew 5 pixels to right on top of image
        // nSkew=+100 -> skew 5 pixels to left on top of image
        skew100=6; 
        surface0 = GetControlSurface(CId, nState, &ww, &hh);
        for(y=0;y<hh;y++) {
            n=nSkew * (hh-y) / hh;    // skew depending on y pos
            xint  = (n * skew100) / 100; // integer part of skew: number of pixels
            xfrac = abs((n * skew100) - xint * 100); // frational (0..100) part of skew
            //if (dir > 0) xfrac=100-xfrac;
            // calc skewed xline 
            for (x=0;x<ww;x++) {
                xline[x]=0;
                x_org=x - xint; 
                if ((x_org < 0) || (x_org+dir < 0)) continue; 
                if ((x_org >= ww) || (x_org+dir >= ww)) continue; 
                col = surface0[x_org + y*ww];
                if (col==0) continue;                           // keep transparent pixels
                get_surface_rgb_color(col,&rr0,&gg0,&bb0);      // get first pixel to skew
                col = surface0[x_org + dir + y*ww];
                if (col==0) continue;                           // keep transparent pixels
                get_surface_rgb_color(col,&rr1,&gg1,&bb1);      // get second pixel to skew
                rr = rr1 + xfrac * (rr0-rr1) / 100;             // blend both pixels according to xfrac
                gg = gg1 + xfrac * (gg0-gg1) / 100;
                bb = bb1 + xfrac * (bb0-bb1) / 100;
                xline[x]=surface_rgb_color(rr,gg,bb);
            }
            // put xline on control image
            for (x=0;x<ww;x++) surface0[x + y*ww]=xline[x];
        }
    }
}

// Control Panel callbacks
void IBM360_Init(void)
{
    int unit; 

    if (IsOption("CpuType/3145")) {
       bCpuModelIs=3145; 
       // process the rotating switches to add central handle and skew as needed
       SetRotatingSwitchSkew(S360.SW_Addr_Compare,   0, 0); 
       SetRotatingSwitchSkew(S360.SW_Storage_Select, 0, 2); 
       SetRotatingSwitchSkew(S360.SW_Rate,           0, 4); 
       SetRotatingSwitchSkew(S360.SW_A,   50, 3); 
       SetRotatingSwitchSkew(S360.SW_B,   30, 4); 
       SetRotatingSwitchSkew(S360.SW_C,   10, 4); 
       SetRotatingSwitchSkew(S360.SW_D,  -10, 5); 
       SetRotatingSwitchSkew(S360.SW_E,  -30, 5); 
       SetRotatingSwitchSkew(S360.SW_F,  -50, 6); 
       SetRotatingSwitchSkew(S360.SW_G,  -70, 7); 
       SetRotatingSwitchSkew(S360.SW_H,  -90, 8); 
    } else if (IsOption("CpuType/3148")) {
       bCpuModelIs=3148; 
       // process the rotating switches to add central handle and skew as needed
       SetRotatingSwitchSkew(S360.SW_Addr_Compare,   0, 2); 
       SetRotatingSwitchSkew(S360.SW_A,   50, 3); 
       SetRotatingSwitchSkew(S360.SW_B,   30, 4); 
       SetRotatingSwitchSkew(S360.SW_C,   10, 4); 
       SetRotatingSwitchSkew(S360.SW_D,  -10, 5); 
       SetRotatingSwitchSkew(S360.SW_E,  -30, 5); 
       SetRotatingSwitchSkew(S360.SW_F,  -50, 6); 
       SetRotatingSwitchSkew(S360.SW_G,  -70, 7); 
       SetRotatingSwitchSkew(S360.SW_H,  -90, 8); 
    } else if (IsOption("CpuType/2065")) {
       bCpuModelIs=2065; 
    } else if (IsOption("CpuType/2050")) {
       bCpuModelIs=2050; 
    } else if (IsOption("CpuType/2040")) {
       bCpuModelIs=2040; 
    } else {
       // process the control panel lights to add backlight when state is > 15
       bCpuModelIs=2030; 
       SetBoxLights(S360.Reg_CN_X);
       SetBoxLights(S360.Reg_SA_CK);
       SetBoxLights(S360.Reg_CR_CS);
       SetBoxLights(S360.Reg_Count);
       SetBoxLights(S360.Reg_CH_Data[1]);
       SetBoxLights(S360.Reg_CH_Flags[1]);
       SetBoxLights(S360.Reg_CH_Data[2]);
       SetBoxLights(S360.Reg_CH_Flags[2]);
       SetBoxLights(S360.Reg_CH_MPX);
       SetBoxLights(S360.Reg_SAR);
       SetBoxLights(S360.Reg_SDR);
       SetBoxLights(S360.Reg_ALU);
       SetBoxLights(S360.Reg_BA);
       SetBoxLights(S360.Reg_Status_Checks);
    }

    IPLaddr=0;
    bCardReadPunchVisible = 0;
    bPrintOutVisible = 0;
    bDasdVisible = 0;
    bTapesVisible = 0;

    nCardsInReadHopper=nCardsInReadStacker=nCardsInPunchStacker=0; // no cards draw in cdr/cdp
    bCardReadHopperBackgroundSet=0; // must grab background behind read hopper guide
    lptPrintOutDoneCount=-1; // to force redraw of printer paper
    memset(lptPrintOut, 0, sizeof(lptPrintOut));

    if (IsOption("CpuOnly")) {
    } else if (IsOption("CpuCardPrinter")) {
        bCardReadPunchVisible = 1;
        bPrintOutVisible = 1;
    } else {
        bCardReadPunchVisible = 1;
        bPrintOutVisible = 1;
        bDasdVisible = 1;
    }
    if (IsOption("tapecab/1")) {
        extern int PARAM_SensorsPerVacCol; 
        extern int PARAM_RWSpeed;
        extern int PARAM_AccelTime;
        extern int PARAM_DecelTime;
        extern int PARAM_HeadOpenTime; 
        extern int PARAM_HiSpeedRwdTrigger;
        extern int PARAM_HiSpeedRwdEnd;
        bTapesVisible=1;
        if (bCpuModelIs > 3000) {
            // ibm 360 tapes
            // IBM 3420 Model 5 tape
            // 4 sensor in each vacuum column 
            // tape speed: 125 inches per second
            // 1600 bits per inch
            bTapeIs=3420;  // using IBM 3420 tapes on cpanel gui
            PARAM_SensorsPerVacCol=2; 
            PARAM_RWSpeed=125; 
            PARAM_AccelTime=110; // guessed value
            PARAM_DecelTime=80;  // guessed value
            PARAM_HeadOpenTime=1200; 
            PARAM_HiSpeedRwdTrigger =  400; // guessed value
            PARAM_HiSpeedRwdEnd     =  200; // guessed value
        } else {
            // ibm 360 tapes
            if (IsOption("TapeType/2415")) {
                // IBM 2415 tape
                // 1 sensor in each vacuum column 
                // tape speed: 18.75 inches per second
                // 1600 bits per inch
                bTapeIs=2415;  // using IBM 2415 tapes on cpanel gui
                PARAM_SensorsPerVacCol=1; 
                PARAM_RWSpeed=19;
                PARAM_AccelTime=225*3; // guessed value
                PARAM_DecelTime=180; // guessed value
                PARAM_HeadOpenTime=700;  // guessed value
                PARAM_HiSpeedRwdTrigger = 350; // guessed value
                PARAM_HiSpeedRwdEnd     = 100; // guessed value
            } else {
                // IBM 2401 Model 5 tape
                // 2 sensor in each vacuum column 
                // tape speed: 75 inches per second
                // 1600 bits per inch
                bTapeIs=2401;  // using IBM 2401 tapes on cpanel gui
                PARAM_SensorsPerVacCol=2; 
                PARAM_RWSpeed=75; 
                PARAM_AccelTime=225;
                PARAM_DecelTime=160;
                PARAM_HeadOpenTime=700; 
                PARAM_HiSpeedRwdTrigger = 1500;
                PARAM_HiSpeedRwdEnd     =  400;
            }
        }
    }

    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }

    memset (mtcab, 0, sizeof mtcab);
    bTapeAnimInProgress=0;
    for (unit=0; unit<8; unit++) {
        mtcab[unit].L_VacColMedium_h0 = -1; // init to -1 last state to force redraw 
        mtcab[unit].R_VacColMedium_h0 = -1; 
        mtcab[unit].state.MT_Head=-1; 
    }
    if (bTapesVisible) {
        int n;
        uint32 col0a, col0b, col1; 
        // set reel amount: medium winded on tape reel
        col0a = surface_rgb_color(208, 209, 201); // fixation 1
        col0b = surface_rgb_color(228, 219, 211); // fixation 2
        col1 = surface_rgb_color(55, 55, 55); // medium color
        for (n=1; n<=67; n++) {
            // copy reel empty
            CopyControlImage(S360.MT_reel_amount, 0,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.MT_reel_amount, n,  0,0);        // ToCId, ToState,     x1, y1
            // add medium circle
            AddCircle(S360.MT_reel_amount, n, 110+n*2, 110+n*2, col1, 149,149);
            // add fixation circle1
            AddCircle(S360.MT_reel_amount, n, 110, 110, col0a, 149,149);
            // add fixation circle1
            AddCircle(S360.MT_reel_amount, n,  80,  80, col0b, 149,149);
        }
        // fill tape reel state images rotated 90, 180 and 270 gr
        // no blur images
        for (n=0; n<9; n++) {
            Rotate(S360.MT_blue_reel, n,  90, 149,149, n+ 9); // rotated  90gr -> states 9..17
            Rotate(S360.MT_blue_reel, n, 180, 149,149, n+18); // rotated 180gr -> states 18..26
            Rotate(S360.MT_blue_reel, n, 270, 149,149, n+27); // rotated 270gr -> states 27..35
            Rotate(S360.MT_red_reel, n,  90, 149,149, n+ 9); // rotated  90gr -> states 9..17
            Rotate(S360.MT_red_reel, n, 180, 149,149, n+18); // rotated 180gr -> states 18..26
            Rotate(S360.MT_red_reel, n, 270, 149,149, n+27); // rotated 270gr -> states 27..35
            Rotate(S360.MT_green_reel, n,  90, 149,149, n+ 9); // rotated  90gr -> states 9..17
            Rotate(S360.MT_green_reel, n, 180, 149,149, n+18); // rotated 180gr -> states 18..26
            Rotate(S360.MT_green_reel, n, 270, 149,149, n+27); // rotated 270gr -> states 27..35
        }
        // slow blur images
        for (n=0; n<6; n++) {
            Rotate(S360.MT_blue_reel, n+36,  90, 149,149, n+36+ 6); 
            Rotate(S360.MT_blue_reel, n+36, 180, 149,149, n+36+12); 
            Rotate(S360.MT_blue_reel, n+36, 270, 149,149, n+36+18); 
            Rotate(S360.MT_red_reel, n+36,  90, 149,149, n+36+ 6); 
            Rotate(S360.MT_red_reel, n+36, 180, 149,149, n+36+12); 
            Rotate(S360.MT_red_reel, n+36, 270, 149,149, n+36+18); 
            Rotate(S360.MT_green_reel, n+36,  90, 149,149, n+36+ 6);
            Rotate(S360.MT_green_reel, n+36, 180, 149,149, n+36+12);
            Rotate(S360.MT_green_reel, n+36, 270, 149,149, n+36+18);
        }
        // fast blur images
        for (n=0; n<2; n++) {
            Rotate(S360.MT_blue_reel, n+60,  90, 149,149, n+60+ 2); 
            Rotate(S360.MT_blue_reel, n+60, 180, 149,149, n+60+ 4); 
            Rotate(S360.MT_blue_reel, n+60, 270, 149,149, n+60+ 6); 
            Rotate(S360.MT_red_reel, n+60,  90, 149,149, n+60+ 2); 
            Rotate(S360.MT_red_reel, n+60, 180, 149,149, n+60+ 4); 
            Rotate(S360.MT_red_reel, n+60, 270, 149,149, n+60+ 6); 
            Rotate(S360.MT_green_reel, n+60,  90, 149,149, n+60+ 2); 
            Rotate(S360.MT_green_reel, n+60, 180, 149,149, n+60+ 4); 
            Rotate(S360.MT_green_reel, n+60, 270, 149,149, n+60+ 6); 
        }
        if (bTapeIs == 3420) {
            // for 3420 tapes, do the follwing actions:
            // 1- set reel hub on control images
            for (n=0; n<68; n++) {
                CopyControlImage(S360.MT_Take_reel_amount, 70,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.MT_blue_reel, n,  0,0);                // ToCId, ToState,     x1, y1
                CopyControlImage(S360.MT_Take_reel_amount, 70,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.MT_red_reel, n,  0,0);                 // ToCId, ToState,     x1, y1
                CopyControlImage(S360.MT_Take_reel_amount, 70,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.MT_green_reel, n,  0,0);               // ToCId, ToState,     x1, y1
            }
            // 2- set the reel ammount for take reel
            col1 = surface_rgb_color(55, 55, 55); // medium color
            for (n=1; n<=67; n++) {
                // copy empty take reel 
                CopyControlImage(S360.MT_Take_reel_amount, 0, 0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.MT_Take_reel_amount, n,  0,0);        // ToCId, ToState,     x1, y1
                // add medium circle
                AddCircle(S360.MT_Take_reel_amount, n, (int)(159+n*1.5), (int)(159+n*1.5), col1, 149,149);
                // copy take reel central hub
                CopyControlImage(S360.MT_Take_reel_amount, 68, 0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.MT_Take_reel_amount, n,  0,0);        // ToCId, ToState,     x1, y1
            }
        }
    }
    
    memset (dasdcab, 0, sizeof dasdcab);

    lptPrintOutDoneCount=-1; // number of lines already printed on main cpanel printer paper

    if (IsOption("console/1")) {
        extern int PARAM_GolfBall_char_ww2;  // Console printer horizontal spacing between first pixel of a char and first pixel of next char in next char same line
        // console window available. get its control panel window number
        // to get it, just ask to a control
        ncp_console=GetControlInfo(S360.ConsPaper, CINFO_NCP); 
        // determine the hoeiz spacinf between chars depending on console type
        
        if (IsOption("ConsType/3210")) {
            bConsTypeIs=3210;
            PARAM_GolfBall_char_ww2 = 12; 
        } else {
            bConsTypeIs=1052;
            PARAM_GolfBall_char_ww2 = 11;  
        }

    } else {
        ncp_console=-1; // no console window
    }
    ConsPrintOutDoneCount = -1; // number of lines already printed on console cpanel paper. -1 to force redraw of paper
    memset(ConsPrintOut, 0, sizeof(ConsPrintOut));
    ConsCharsOutCount = ConsSomethingPrinted = 0; 
    ConsGolfBallAction = ConsGolfBallPosition = 0; 

    if (bDasdVisible) {
        // determine the dasd units to be displayed in dasd storage facility as disk-packs A to H
        // on real hw, storage facilities comes in n cabinets, each one with two (upper one, lower one) disk-pack drawer
        // n can be 1, 2,3 or 4 attached to a control unit (DA, DB, DC, DD on SimH). 
        // so on real HW, if the upper disk-pack is DA0, the lower one should be DAn, n=0..7
        // The cpanel removes this limitation. Each disk-pack A to H can be tied to any control unit/unit. Upper 
        // disk-pack can be e.g DA2, the lower one can be say DC5
        // yoy can configure what is the DASD unit in SimH tied to each disk-pack with
        //
        //     SET OPTION CPANEL=DiskPack/xxx
        //
        // The default asignement is:
        //
        //     SET OPTION CPANEL=DiskPack/DA0-DA1-DB0-DB1-DC0-DC1-DD0-DD1
        //
        UNIT * uptr; 
        char buf[80], c, dev;
        int i, disk_pack, unit; 
        memset(buf, 32, sizeof buf); buf[sizeof(buf)-1]=0;
        if (IsOption("DiskPack") && (IsOptionParam) && (*IsOptionParam++ == '/')) {
            // get option to set the dasd units to be displayed in disk pack cabinets
            for (i=0;i<31;i++) {
                c = *IsOptionParam++; 
                if ((c==0) || (c==',')) break; // end of option params
                buf[i]=sim_toupper(c); 
            }
        } else {
            // default arrangement: each cabinet show first two units of each controller 
            sprintf(buf, "DA0-DA1-DB0-DB1-DC0-DC1-DD0-DD1"); 
        }
        // parse units to display
        for (disk_pack=0; disk_pack<8; disk_pack++) {
            i=disk_pack*4; //i=points to disk pack unit name. Check if valid one
            if ((buf[i] != 'D') || (buf[i+1] < 'A') || (buf[i+1] > 'D') || (buf[i+2] < '0') || (buf[i+2] > '7')) {
                sim_printf("set cpanel option DiskPack: %c%c%c not a valid dasd unit\r\n", buf[i], buf[i+1], buf[i+2]);
                break; 
            }
            if ((buf[i+3] != 0) && (buf[i+3] != 32) && (buf[i+3] != '-')) {
                sim_printf("set cpanel option DiskPack: invalid separator %c\r\n", buf[i+3]);
                break; 
            }
            dev=buf[i+1]; 
            unit=buf[i+2]-'0'; uptr=NULL; 
            if (dev=='A') {
                extern UNIT dda_unit[]; 
                uptr=&dda_unit[unit]; 
            } else if ((NUM_DEVS_DASD>1) && (dev=='B')) {
                extern UNIT ddb_unit[]; 
                uptr=&ddb_unit[unit]; 
            } else if ((NUM_DEVS_DASD>2) && (dev=='C')) {
               extern UNIT ddc_unit[]; 
               uptr=&ddc_unit[unit]; 
            } else if ((NUM_DEVS_DASD>3) && (dev=='D')) {
               extern UNIT ddd_unit[]; 
               uptr=&ddd_unit[unit]; 
            } else {
                sim_printf("set cpanel option DiskPack: unit not available dev %c, unit %d\r\n", dev, unit);
                break; 
            }
            dasdcab[disk_pack].uptr=uptr; 
        }

    }
}

void IBM360_Done(void)
{
}

void IBM360_Reset(void)
{
}

int calculateParity(uint32 n)
{
    uint8 x = 0;
    int i;
    for(i = 0; i < 4; i++){
        x ^= n & 0xFF; n = n >> 8;
    }
    x ^= x >> 4;
    x ^= x >> 2;
    x ^= x >> 1;
    return x & 1;
}

void SetOCPPanelLights(int mode, 
                  int TestState, 
                  int SW_IPL1, int SW_IPL2, int SW_IPL3)
{
    int n; 

    n= (((flags & WAIT)!=0) && (sim_is_running!=0)) ? 1:0;
    if (mode==0) TickCount(S360.LI_Wait, n); 
    else SetStateWithIntensity(S360.LI_Wait, n); 
    if (mode) {
        // set light on refresh, but no need to set them on TickCount
        SetState(S360.LI_System, (sim_is_running) ? 1:0); 
        SetState(S360.LI_Manual, (sim_is_running) ? 0:1); 
        SetState(S360.LI_Test, (TestState) ? 1:0); 
        SetState(S360.LI_Load, (loading) ? 1:0);
        if (IPLaddr) {
            // scp boot command has been issued
            // Set the rotating switches according to booted addr from
            SetState(SW_IPL3, IPLaddr & 0xF); IPLaddr=IPLaddr>>4; 
            SetState(SW_IPL2, IPLaddr & 0xF); IPLaddr=IPLaddr>>4; 
            SetState(SW_IPL1, IPLaddr & 0x7); IPLaddr=0;
        }
        if ((GetState(S360.SW_Rate)!=0) && (sim_is_running) && (cpanel_stop_flag != 2)) {
            // rate sw is set to instr step, but cpu is running whiout stop flag set -> 
            // we are in regurlar run mode -> stop the cpu
            cpanel_stop_flag = 2;
            // and set interactive mode
            cpanel_interactive=1; 
        }
    }
}                  


// set cpu panel lights for Model 30
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
//         2 set al lights to 1 (while lamp test button is pressed)
void SetCpuPanelLights_M30(int mode)
{
    t_uint64 AllOnes = (t_uint64)(-1);
    int bSelChan, bMpxChan, bAuxStorage;
    static int flip_flop = 0;

    uint32 n, n1,n2,n3,p1,p2,p3, nChan; 

    if (mode==2) {
        // set all to ones, is lamp test
        // this button is only available for Model 30
        SetState(S360.Reg_CN_X, AllOnes);
        SetState(S360.Reg_SA_CK, AllOnes);
        SetState(S360.Reg_CR_CS, AllOnes);
        SetState(S360.Reg_Count, AllOnes); 
        SetState(S360.Reg_CH_Data[1], AllOnes); 
        SetState(S360.Reg_CH_Flags[1], AllOnes); 
        SetState(S360.Reg_CH_Data[2], AllOnes); 
        SetState(S360.Reg_CH_Flags[2], AllOnes); 
        SetState(S360.Reg_CH_MPX, AllOnes); 
        SetState(S360.Reg_SAR, AllOnes); 
        SetState(S360.Reg_SDR, AllOnes); 
        SetState(S360.Reg_ALU, AllOnes); 
        SetState(S360.Reg_BA, AllOnes); 
        SetState(S360.Reg_Status_Checks, AllOnes); 
        SetState(S360.Reg_Status_Checks, AllOnes); 
        SetState(S360.LI_System, 1); 
        SetState(S360.LI_Manual, 1); 
        SetState(S360.LI_Wait, 1); 
        SetState(S360.LI_Test, 1); 
        SetState(S360.LI_Load, 1); 
        return; 
    }

    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=((int)GetState(S360.SW_Rate) ? 1:0) || ((int)GetState(S360.SW_Addr_Compare) ? 1:0); // test state
    SetOCPPanelLights(mode, n, S360.SW_G, S360.SW_H, S360.SW_J); 

    // as microarch is not simulated, lights are set with general
    // purpose register contents. This, even if not acurate, will provide a 
    // blinkenlight flashing that looks like the original one
    if (bSystemResetKeyPressed==2) {
        // all to 1 to give user feedback that microprogram test has been executed
        n1=0x3F; n2=0x9F; n3=0xFF; 
    } else {
        n1=(((PC >> 1) ^ (PC >> 7)) ^ (sim_interval)) & 0x3F ; // reg CN <- = ros pc
        n2=((PC >> 3) ^ (sim_interval)) & 0x1F;      // reg W <-WX=ros address register
        n3=((PC >> 1) ^ (sim_interval - 1)) & 0xFF;  // reg X
    }
    p1=calculateParity(n1); p2=calculateParity(n2); p3=calculateParity(n3); // calc parity 
    n = (p1 << 23) | (n1 << 17) | (p2 << 14) | (n2 << 9) | (p3 << 8) | (n3); // assemble result. 
    if (mode==0) TickCount(S360.Reg_CN_X, n); 
    else SetStateWithIntensity(S360.Reg_CN_X, n); 

    if (bSystemResetKeyPressed==2) {
        // all to 1 to give user feedback that microprogram test has been executed
        n1=0xFF; n2=0x1FFF; n3=0x0F; 
    } else {
        // n = (regs[2] ^ (regs[3] << 6) ^ (regs[4] << 12) ^ (regs[5] << 18) ^ (regs[6] << 24)); // ALU regiter is not realistic. 
        // n = n ^ PC ^ cpu_mem_data; 
        n = sim_rand() | sim_rand();
        n1 = n & 0xFF; n = n >> 8;     // reg CH-CL 
        n2 = n & 0x1FFF; n = n >> 13;  // reg CA, CB, CM, CU, A bit of CK
        n3 = n & 0x0F;                 // reg CK
    }
    p1=calculateParity(n1); p3=calculateParity(n3); // calc parity 
    n = (p1 << 26) | (n1 << 18) | (n2 << 5) | (p3 << 4) | (n3); // assemble result. 
    if (mode==0) TickCount(S360.Reg_SA_CK, n); 
    else SetStateWithIntensity(S360.Reg_SA_CK, n); 

    if (bSystemResetKeyPressed==2) {
        // all light set to 1 (=illuminated) to give user feedback that microprogram test has been executed
        n1=0x0F; n2=0x1F; n3=0x3FF; 
    } else {
        // n = (regs[7] ^ (regs[8] << 6) ^ (regs[9] << 12) ^ (regs[10] << 18) ^ (regs[11] << 24)); // ALU regiter is not realistic. 
        // n = n ^ PC ^ cpu_mem_data; 
        n = sim_rand() | sim_rand();
        n1 = n & 0x0F; n = n >> 4;  // reg CR, CD
        n2 = n & 0x1F; n = n >> 5;  // reg CF, CG
        n3 = n & 0x3FF;             // reg CV, CC, CS
    }
    p1=calculateParity(n1); // calc parity 
    n = (p1 << 19) | (n1 << 15) | (n2 << 10) | (n3); // assemble result. 
    if (mode==0) TickCount(S360.Reg_CR_CS, n); 
    else SetStateWithIntensity(S360.Reg_CR_CS, n); 
    
    // byte count in channel 1. this is realistic. The point is you cannot select to display channel 2 on 
    // count register, because rotary switch E is not implemented on control panel 
    n = get_chan_info(1, 0); // get channel count
    n1 = n >> 8; n2 = n & 0xFF; // get hi/low part
    p1 = calculateParity(n1); p2 = calculateParity(n2); // calc parity for hi/low part
    n = (p1 << 17) | (n1 << 9) | (p2 << 8) | (n2); // assemble result
    if (mode==0) TickCount(S360.Reg_Count, n); 
    else SetStateWithIntensity(S360.Reg_Count, n); 

    // info for channels 1&2 is realistic 
    bSelChan=0;
    for (nChan=1; nChan<=2; nChan++) {
        n  = get_chan_info(nChan, 1); // get channel data/key/command for channel 1
        n1 = (n >> 16) & 0xFF; n2 = (n >> 8) & 0x0F; n3 = n & 0x0F; 
        if (n3) bSelChan=1; // selector channel active processing command n3
        p1 = calculateParity(n1); p2 = calculateParity(n2); 
        n = (p1 << 17) | (n1 << 9) | (p2 << 8) | (n2 << 4) | (n3);
        if (mode==0) TickCount(S360.Reg_CH_Data[nChan], n); 
        else SetStateWithIntensity(S360.Reg_CH_Data[nChan], n); 

        n  = get_chan_info(nChan, 2); // get channel flags/checks for channel 1
        n1 = ((n >> 16) >> 11) & 0x1F; n3 = n & 0x3F; // flags/check
        //XXX falta en n3 los tags
        n = (n1 << (9+6)) | (n2 << 6) | n3;
        if (mode==0) TickCount(S360.Reg_CH_Flags[nChan], n); 
        else SetStateWithIntensity(S360.Reg_CH_Flags[nChan], n); 
    }

    bMpxChan=0;
    n  = get_chan_info(0, 1); // get channel data/key/command for channel 0 (Multiplexed chan)
    n1 = (n >> 16) & 0xFF; n3 = n & 0x0F; 
    if (n3) bMpxChan=1; // selector channel active processing command n3
    //XXX faltan los tags
    p1 = calculateParity(n1); 
    n = (p1 << 8) | (n1);
    if (mode==0) TickCount(S360.Reg_CH_MPX, n); 
    else SetStateWithIntensity(S360.Reg_CH_MPX, n); 

    flip_flop = 1-flip_flop; 
    n1 = (((flags & WAIT)==0) && (sim_is_running!=0)) ? flip_flop:0; // EX status
    n2 = (cpu_mem_comp.hit) ? 1:0; // MATCH status
    if (cpu_mem_data & (1<<31)) { // memory access
        n3=flip_flop; // ALLOW WRITE status
        cpu_mem_data &= ~(1<<31);
        bAuxStorage=2; // MAIN STORAGE
    } else {
        n3=0;
        bAuxStorage=1; // AUX STORAGE
    }
    n = (bSelChan << 1) | (bMpxChan << 2) | (bRequestPending << 3) | (n3 << 5) | (n2 << 6) | (n1 << 7);
    n = n << 8; // to skip check bits
    if (mode==0) TickCount(S360.Reg_Status_Checks, n); 
    else SetStateWithIntensity(S360.Reg_Status_Checks, n); 

    n = cpu_mem_addr; // get last accessed main storage addr accessed to be shown on control panel
    n1 = (n >> 8) & 0xFF; n2 = n & 0xFF; // get hi/low part
    p1 = calculateParity(n1); p2 = calculateParity(n2); // calc parity for hi/low part
    n = (p1 << 19) | (n1 << 11) | (p2 << 10) | (n2 << 2) | bAuxStorage; // assemble result
    if (mode==0) TickCount(S360.Reg_SAR, n); 
    else SetStateWithIntensity(S360.Reg_SAR, n); 

    n = cpu_mem_data & 0xFF; // get last data read/write from/to main storage 
    p1 = calculateParity(n); 
    n = (p1 << 8) | (n); 
    if (mode==0) TickCount(S360.Reg_SDR, n); 
    else SetStateWithIntensity(S360.Reg_SDR, n); 

    n = (regs[5] ^ (regs[6] << 3) ^ (regs[7] << 6) ^ (regs[8] << 9) ^ (regs[9] << 12)) & 0xFF; // ALU regiter is not realistic. 
    n = n ^ PC ^ cpu_mem_data; 
    p1 = calculateParity(n); 
    n = (p1 << 8) | (n); 
    if (mode==0) TickCount(S360.Reg_ALU, n); 
    else SetStateWithIntensity(S360.Reg_ALU, n); 

    // AB register is not realistic when CPU runs. But when CPU is stopped or in wait condition
    // it will display the inctruction counter as stated in manual. This is realistic
    if ((sim_is_running==0) || (flags & WAIT)) {
        if (bRegBA_display == 1) {// signal reg B A displays PC when stopped, =1 will display MSDR
            n = cpu_mem_data << 8 | cpu_mem_data; 
        } else {
            n = PC; 
        }
    } else {
        n = (regs[9] ^ (regs[10] << 3) ^ (regs[11] << 6) ^ (regs[2] << 9) ^ (regs[3] << 12)); // A regiter and B register not realistic. 
        n = n ^ PC; 
        n = n ^ cpu_mem_data; 
        bRegBA_display=0; // signal reg B A displays PC when stopped
    }
    n1 = n >> 8; n2 = n & 0xFF; // get hi/low part
    p1 = calculateParity(n1); p2 = calculateParity(n2); // calc parity for hi/low part
    n = (p1 << 17) | (n1 << 9) | (p2 << 8) | n2; // assemble result
    if (mode==0) TickCount(S360.Reg_BA, n); 
    else SetStateWithIntensity(S360.Reg_BA, n); 
}

// set cpu panel lights for Model 40
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
void SetCpuPanelLights_M40(int mode)
{
    uint32 n, n1,n2,n3,p1,p2,p3; 

    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=(int)GetState(S360.SW_Rate);
    SetOCPPanelLights(mode, n, S360.SW_A, S360.SW_B, S360.SW_C); 

    // as microarch is not simulated, lights are set with general
    // purpose register contents. This, even if not acurate, will provide a 
    // blinkenlight flashing that looks like the original one
    n1=(((PC >> 1) ^ (PC >> 7)) ^ (sim_interval)) & 0xFFF; // Reg_Cons_Stats has 21 bits
    n2=((PC >> 3) ^ (sim_interval - 1)) & 0x1FF;      
    n = (n1 << 12) | (n2); 
    if (mode==0) TickCount(S360.Reg_Cons_Stats, n); 
    else SetStateWithIntensity(S360.Reg_Cons_Stats, n); 

    n = sim_rand() | sim_rand();
    n1 = n & 0xF; n = n >> 4;     
    n2 = n & 0xFF; n = n >> 8; 
    n3 = n & 0xFF; n = n >> 8; 
    p1=calculateParity(n1); p2=calculateParity(n2); p3=calculateParity(n3); // calc parity 
    n = (p1 << 22) | (n1 << 18) | ((n & 1) << 17) | (p2 << 16) | (n2 << 9) | (p3 << 8) | (n3); // assemble result. 
    if (mode==0) TickCount(S360.Reg_ALU_Sel, n); 
    else SetStateWithIntensity(S360.Reg_ALU_Sel, n); 

    n = sim_rand() | sim_rand();
    n1 = n & 0xFF; n = n >> 8;     
    n2 = n & 0x1FFF; n = n >> 13; 
    p1=calculateParity(n1); p2=calculateParity(n2); 
    n = (p1 << 22) | (n1 << 14) | (p2 << 13) | (n2); // assemble result. 
    if (mode==0) TickCount(S360.Reg_LSAR_ROAR, n); 
    else SetStateWithIntensity(S360.Reg_LSAR_ROAR, n); 

    // roller gets real data, even if it is not the microarch ones
    switch (1+((int) GetState(S360.Label_Roller1_L) & 7)) {
       case 1: n = get_chan_info(1, 0); break; // return count bytes in channel 1 to be displayed in control panel
       case 2: n = get_chan_info(1, 1); break; // last data byte read/write, key and command
       case 3: n = get_chan_info(1, 2); break; // flags/status
       case 4: n = get_chan_info(2, 0); break; // same for channel 2
       case 5: n = get_chan_info(2, 1); break; // 
       case 6: n = get_chan_info(2, 2); break; // 
       case 7: n=InstrCycles; break; // count of intructions loops executed so far (if cpu waiting, do get incremented)
       case 8: n=InstrExec; break; // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
    }
    n1 = n & 0x1FFFFF; 
    p1=calculateParity(n1);  
    n = (p1 << 21) | (n1); // assemble result. 
    if (mode==0) TickCount(S360.Reg_Roller1_16b, n); 
    else SetStateWithIntensity(S360.Reg_Roller1_16b, n); 

    switch (1+((int) GetState(S360.Label_Roller2_L) & 7)) {
       case 1: n=PC; break; 
       case 2: n=regs[0]; break; 
       case 3: n=regs[0] >> 16; break; 
       case 4: n=regs[1]; break; 
       case 5: n=regs[1] >> 16; break; 
       case 6: n = get_chan_info(0, 0); break; // same for channel 0 (mpx channel)
       case 7: n = get_chan_info(0, 1); break; // 
       case 8: n = get_chan_info(0, 2); break; // 
       default: n=0; break; 
    }
    n1 = n & 0x1FFFFF; 
    p1=calculateParity(n1);  
    n = (p1 << 21) | (n1); // assemble result. 
    if (mode==0) TickCount(S360.Reg_Roller2_16b, n); 
    else SetStateWithIntensity(S360.Reg_Roller2_16b, n); 

    // show lower 16b of address and data read from mem. This is realistic
    n=cpu_mem_data;         // save last data read/write from/to main storage to be shown on control panel 
    n1 = n & 0xFF; n = n >> 8;     
    n2 = n & 0xFF; n = n >> 8; 
    p1=calculateParity(n1); p2=calculateParity(n2); 
    n = (p1 << 17) | (n1 << 9) | (p2 << 8) | (n2); // assemble result. 
    if (mode==0) TickCount(S360.Reg_Data_16b, n); 
    else SetStateWithIntensity(S360.Reg_Data_16b, n); 

    n=cpu_mem_addr;         // save last accessed main storage addr accessed to be shown on control panel
    n1 = n & 0x7FFFF; 
    p1=calculateParity(n1);  
    n = (p1 << 19) | (n1); // assemble result. 
    if (mode==0) TickCount(S360.Reg_Addr_16b, n); 
    else SetStateWithIntensity(S360.Reg_Addr_16b, n); 
}

// set cpu panel lights for Model 50
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
//         2 set al lights to 1 (while lamp test button is pressed)
void SetCpuPanelLights_M50(int mode)
{
    t_uint64 AllOnes = (t_uint64)(-1);
    static int flip_flop = 0;

    uint32 n, n1,n2,n3,p1,p2,p3, nHi, nLo; 
    int bStorage;

    if (mode==2) {
        // set all to ones, is lamp test
        // this switch is only available for Model 50
        SetState(S360.Reg_Volt, AllOnes);
        SetState(S360.Reg_Roller1_L, AllOnes);
        SetState(S360.Reg_Roller1_R, AllOnes);
        SetState(S360.Reg_Roller2_L, AllOnes);
        SetState(S360.Reg_Roller2_R, AllOnes);
        SetState(S360.Reg_Roller3_L, AllOnes);
        SetState(S360.Reg_Roller3_R, AllOnes);
        SetState(S360.Reg_Roller4_L, AllOnes);
        SetState(S360.Reg_Roller4_R, AllOnes);
        SetState(S360.Reg_CPU_L, AllOnes);
        SetState(S360.Reg_CPU_R, AllOnes); 
        SetState(S360.Reg_SDR_L, AllOnes); 
        SetState(S360.Reg_SDR_R, AllOnes); 
        SetState(S360.Reg_IAR_L, AllOnes); 
        SetState(S360.Reg_IAR_R, AllOnes); 
        SetState(S360.LI_System, 1); 
        SetState(S360.LI_Manual, 1); 
        SetState(S360.LI_Wait, 1); 
        SetState(S360.LI_Test, 1); 
        SetState(S360.LI_Load, 1); 
        return; 
    }
    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=(int)GetState(S360.SW_Rate);
    SetOCPPanelLights(mode, n, S360.SW_A, S360.SW_B, S360.SW_C); 

    // clear voltage check
    if (mode) SetState(S360.Reg_Volt, 0);

    // Storage Data register -> contents of data read from mem when running
    if (cpu_mem_data & (1<<31)) { // memory access
        cpu_mem_data &= ~(1<<31);
        bStorage=2; // MAIN STORAGE
    } else {
        bStorage=1; // ROS STORAGE
    }
    n = cpu_mem_data; 
    n1 = n & 0xFF; n = n >> 8; // Low PC byte
    n2 = n & 0xFF; n = n >> 8; 
    p1=calculateParity(n1); p2=calculateParity(n2); 
    nLo = (p2 << 17) | (n2 << 9) | (p1 << 8) | (n1); // assemble result (low part, displayed on SDR_R)
    n1 = n & 0xFF; n = n >> 8; // 
    n2 = n & 0xFF; n = n >> 8; 
    p1=calculateParity(n1); p2=calculateParity(n2); 
    nHi  = (p2 << 17) | (n2 << 9) | (p1 << 8) | (n1); // assemble result (hi part, displayed on SDR_L)
    if (mode==0) { TickCount(S360.Reg_SDR_L, nHi); TickCount(S360.Reg_SDR_R, nLo); }
    else { SetStateWithIntensity(S360.Reg_SDR_L, nHi); SetStateWithIntensity(S360.Reg_SDR_R, nLo); }

    // Instruction Counter is allways displayed on IAR reg
    n = PC; 
    n1 = n & 0xFF; n = n >> 8; // Low PC byte
    n2 = n & 0xFF; n = n >> 8; 
    n3 = n & 0xFF; n = n >> 8; 
    p1=calculateParity(n1); p2=calculateParity(n2); p3=calculateParity(n3); // calc parity 
    nHi = (p3 << 8) | (n3); // Hi Part
    nLo = (p2 << 17) | (n2 << 9) | (p1 << 8) | (n1); // Lo part
    if (mode==0) { TickCount(S360.Reg_IAR_L, nHi); TickCount(S360.Reg_IAR_R, nLo); }
    else { SetStateWithIntensity(S360.Reg_IAR_L, nHi); SetStateWithIntensity(S360.Reg_IAR_R, nLo); }
    
    // CPU ligths (from p183 of http://www.bitsavers.org/pdf/ibm/360/fe/2050/SY22-2832-4_360-50Maint.pdf)
    // FLT on control panel is Fault Loading Test 
    // MODE ROS -> lit when normal instr execution
    // All clock lit -> has clock signal
    n = (15 << 4) | (1 << 13); 
    if (mode) {
        SetState(S360.Reg_CPU_L, 0);
        SetState(S360.Reg_CPU_R, n); 
    }

    // roller data is described in detail again in 
    // http://www.bitsavers.org/pdf/ibm/360/fe/2050/SY22-2832-4_360-50Maint.pdf, page 162 and following

    // roller gets real data, even if it is not the microarch ones
    // roller 1 is channel info
    switch (1+((int) GetState(S360.Label_Roller1_L) & 7)) {
       case 1: n = get_chan_info(1, 0) << 16; break; // return count bytes in channel 1 to be displayed in control panel
       case 2: n = get_chan_info(1, 1) << 8; break; // last data byte read/write, key and command
       case 3: n = get_chan_info(1, 2); break; // flags/status
       case 4: n = get_chan_info(2, 0) << 16; break; // same for channel 2
       case 5: n = get_chan_info(2, 1) << 8; break; // 
       case 6: n = get_chan_info(2, 2); break; // 
       case 7: n=0; break; 
       case 8: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller1_L, n >> 18); TickCount(S360.Reg_Roller1_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller1_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller1_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller2_L) & 7)) {
       case 1: n=regs[0]; break; 
       case 2: n=regs[1]; break; 
       case 3: n=regs[2]; break; 
       case 4: n=regs[3]; break; 
       case 5: n=regs[4]; break; 
       case 6: n=regs[5]; break; 
       case 7: n=0; break; 
       case 8: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller2_L, n >> 18); TickCount(S360.Reg_Roller2_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller2_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller2_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller3_L) & 7)) {
       case 1: n=regs[6]; break; 
       case 2: n=regs[7]; break; 
       case 3: n=regs[8]; break; 
       case 4: n=regs[9]; break; 
       case 5: n=regs[10]; break; 
       case 6: n=regs[11]; break; 
       case 7: n=0; break; 
       case 8: n=cpu_mem_addr; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller3_L, n >> 18); TickCount(S360.Reg_Roller3_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller3_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller3_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller4_L) & 7)) {
       case 1: n=regs[12]; break; 
       case 2: n=regs[13]; break; 
       case 3: n=regs[14]; break; 
       case 4: n=regs[15]; break; 
       case 5: n=InstrCycles << 4; break; // count of intructions loops executed so far (if cpu waiting, do get incremented)
       case 6: n=InstrExec << 9; break; // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
       case 7: n=(sim_rand() & 0x1FFF) << 18; break; // ROS addr
       case 8: n=(sim_rand() & 0x1FFF) << 18; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller4_L, n >> 18); TickCount(S360.Reg_Roller4_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller4_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller4_R, n & 0x3FFFF); }
}

// set cpu panel lights for Model 65
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
//         2 set al lights to 1 (while lamp test button is pressed)
void SetCpuPanelLights_M65(int mode)
{
    t_uint64 AllOnes = (t_uint64)(-1);
    static int flip_flop = 0;

    uint32 n;  

    if (mode==2) {
        // set all to ones, is lamp test
        // this switch is only available for Model 65
        SetState(S360.Reg_Volt, AllOnes);
        SetState(S360.Reg_Roller1_L, AllOnes);
        SetState(S360.Reg_Roller1_R, AllOnes);
        SetState(S360.Reg_Roller2_L, AllOnes);
        SetState(S360.Reg_Roller2_R, AllOnes);
        SetState(S360.Reg_Roller3_L, AllOnes);
        SetState(S360.Reg_Roller3_R, AllOnes);
        SetState(S360.Reg_Roller4_L, AllOnes);
        SetState(S360.Reg_Roller4_R, AllOnes);
        SetState(S360.Reg_Roller5_L, AllOnes);
        SetState(S360.Reg_Roller5_R, AllOnes);
        SetState(S360.Reg_Roller6_L, AllOnes);
        SetState(S360.Reg_Roller6_R, AllOnes);
        SetState(S360.Reg_Stor_Proc_CHK, AllOnes);
        SetState(S360.LI_System, 1); 
        SetState(S360.LI_Manual, 1); 
        SetState(S360.LI_Wait, 1); 
        SetState(S360.LI_Test, 1); 
        SetState(S360.LI_Load, 1); 
        return; 
    }
    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=(int)GetState(S360.SW_Rate);
    SetOCPPanelLights(mode, n, S360.SW_A, S360.SW_B, S360.SW_C); 

    if (mode) SetState(S360.Reg_Volt, 0);
    if (mode) SetState(S360.Reg_Stor_Proc_CHK, 0);

    // roller data is sumarized in 
    // http://www.bitsavers.org/pdf/ibm/360/fe/2065/Y25-0501-2_System_360_Model_65_Field_Engineering_Handbook_Oct1969.pdf, 
    // page 17 and following

    // roller gets real data, even if it is not the microarch ones
    // roller 1 is channel info
    switch (1+((int) GetState(S360.Label_Roller1_L) & 7)) {
       case 1: n = get_chan_info(1, 0) << 16; break; // return count bytes in channel 1 to be displayed in control panel
       case 2: n = get_chan_info(1, 1) << 8; break; // last data byte read/write, key and command
       case 3: n = get_chan_info(1, 2); break; // flags/status
       case 4: n = get_chan_info(2, 0) << 16; break; // same for channel 2
       case 5: n = get_chan_info(2, 1) << 8; break; // 
       case 6: n = get_chan_info(2, 2); break; // 
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller1_L, n >> 18); TickCount(S360.Reg_Roller1_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller1_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller1_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller2_L) & 7)) {
       case 1: n=regs[0]; break; 
       case 2: n=regs[1]; break; 
       case 3: n=regs[2]; break; 
       case 4: n=regs[3]; break; 
       case 5: n=regs[4]; break; 
       case 6: n=regs[5]; break; 
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller2_L, n >> 18); TickCount(S360.Reg_Roller2_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller2_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller2_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller3_L) & 7)) {
       case 1: n=regs[6]; break; 
       case 2: n=regs[7]; break; 
       case 3: n=regs[8]; break; 
       case 4: n=regs[9]; break; 
       case 5: n=regs[10]; break; 
       case 6: n=regs[11]; break; 
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller3_L, n >> 18); TickCount(S360.Reg_Roller3_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller3_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller3_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller4_L) & 7)) {
       case 1: n=cpu_mem_addr; break; 
       case 2: n=regs[12]; break; 
       case 3: n=regs[13]; break; 
       case 4: n=regs[14]; break; 
       case 5: n=regs[15]; break; 
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller4_L, n >> 18); TickCount(S360.Reg_Roller4_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller4_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller4_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller5_L) & 7)) {
       case 1: n=cpu_mem_data; break; 
       case 2: n = InstrCycles << 4; break; // count of intructions loops executed so far (if cpu waiting, do get incremented)
       case 3: n = InstrExec << 9; break; // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller5_L, n >> 18); TickCount(S360.Reg_Roller5_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller5_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller5_R, n & 0x3FFFF); }

    switch (1+((int) GetState(S360.Label_Roller6_L) & 7)) {
       case 1: n=PC; break; 
       case 2: n = get_chan_info(0, 0) << 16; break; // mpx channel 0
       case 3: n = get_chan_info(0, 1) << 8; break; // 
       case 4: n = get_chan_info(0, 2); break; // 
       case 5: n=0; break; 
       case 6: n=(int) AllOnes; break; // lamp test
       default: n=0; break; 
    }
    if (mode==0) { TickCount(S360.Reg_Roller6_L, n >> 18); TickCount(S360.Reg_Roller6_R, n & 0x3FFFF); }
    else { SetStateWithIntensity(S360.Reg_Roller6_L, n >> 18); SetStateWithIntensity(S360.Reg_Roller6_R, n & 0x3FFFF); }
}

// set cpu panel lights for 370 Model 145
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
//         2 set al lights to 1 (while lamp test button is pressed)
void SetCpuPanelLights_M145(int mode)
{
    extern uint8        dat_en;           /* Translate addresses */
    extern uint8        ec_mode;              /* EC mode PSW */
    t_uint64 AllOnes = (t_uint64)(-1);
    static int flip_flop = 0;

    uint32 n;  
    int n1, n2, n3, n4;

    if (mode==2) {
        // set all to ones, is lamp test
        // this switch is only available for Model 145/148
        SetState(S360.Reg_SysChk_L, AllOnes);
        SetState(S360.Reg_SysChk_R, AllOnes);
        SetState(S360.Reg_Cpu_Status, AllOnes);
        SetState(S360.Reg_Cons_File, AllOnes);

        SetState(S360.Reg_Roller1_L, AllOnes);
        SetState(S360.Reg_Roller1_R, AllOnes);
        SetState(S360.Reg_Roller2_L, AllOnes);
        SetState(S360.Reg_Roller2_R, AllOnes);

        SetState(S360.LI_System, 1); 
        SetState(S360.LI_Manual, 1); 
        SetState(S360.LI_Wait, 1); 
        SetState(S360.LI_Test, 1); 
        SetState(S360.LI_Load, 1); 
        return; 
    }
    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=((int)GetState(S360.SW_Rate) ? 1:0) || ((int)GetState(S360.SW_Addr_Compare_Cntrl) ? 1:0);
    SetOCPPanelLights(mode, n, S360.SW_F, S360.SW_G, S360.SW_H); 

    SetState(S360.Reg_SysChk_L, 0);
    SetState(S360.Reg_SysChk_R, 0);
    SetState(S360.Reg_Cons_File, 0); 
    
    // EXE CPLT  Indicates CPU stopped state reached as result of:
    //              1. Pressing the STOP key.
    //              2. The RATE switch In the INSTRUCTION STEP position.
    //              3. A match being detected while In the address match mode of operation.
    // ADR COMP MATCH Indicates that the address (real or logical) being used to access
    //              main storage matches the address set into switches CDEFGH during an 
    //              address match operation. The action taken by the CPU
    //              as a result of the match condition is controlled by the setting of
    //              the ADDRESS COMPARE CONTROL switch.    
    // ADR X-LATE MODE Indicates dat features active
    // EC MODE      Indicates Enhanced control active (ir 370 mode, vs BC - Basic Control mode aka 360 mode)
    n= (sim_is_running ? 0:(1<<17)) +      // EXEC CPLT
       ((cpu_mem_comp.hit) ? (1<<16):0) +  // MATCH status
       (dat_en ? (1<<5):0) +               // ADR X-LATE
       (ec_mode ? (1<<7):0);               // EC-MODE

    // STOR I lits on each Instr Cycle (flip flop), I lits on instr, 
    // TRAP 1 list on irq pending
    flip_flop = 1-flip_flop; 
    n= n + (flip_flop << 13) + (irq_pend ? (1<<12):0) + ((InstrExec & 1) << 10) ; 

    if (mode==0) TickCount(S360.Reg_Cpu_Status, n); 
    else SetStateWithIntensity(S360.Reg_Cpu_Status, n); 

    // control panel info is described in 
    // http://bitsavers.informatik.uni-stuttgart.de/pdf/ibm/370/model145/GC38-0015-2_370_145_Operating_Procedures_sep72.pdf
    // in the doc, in p13, it is said that roller 2 (A-Register) get on position 1 data from "manual store/display"
    // in p61, it is said that roller 1 (dply asmblr) get on position 2 data from store/display
    // is it not clear with one is the correct one. There we implement what is said in p61

    // roller gets real data, even if it is not the microarch ones
    // roller 1 is DPLY ASSEMBLER OUT
    switch (1+((int) GetState(S360.Label_Roller1_L) & 7)) {
       case 1: n=cpu_mem_addr; break; // this is realistic
       case 2: n=cpu_mem_data; break; // this is realistic
       case 3: n=regs[0]; break; 
       case 4: n=regs[1]; break; 
       case 5: n=regs[2]; break; 
       case 6: n=regs[3]; break; 
       case 7: n = InstrCycles << 4; break; // count of intructions loops executed so far (if cpu waiting, do get incremented)
       case 8: n = InstrExec << 9; break; // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
       default: n=0; break; 
    }
    n1= n & 0xFF; n1 |= (calculateParity(n1) << 8); n=n>>8; 
    n2= n & 0xFF; n2 |= (calculateParity(n2) << 8); n=n>>8; 
    n3= n & 0xFF; n3 |= (calculateParity(n3) << 8); n=n>>8; 
    n4= n & 0xFF; n4 |= (calculateParity(n4) << 8); n=n>>8; 
    n4=(n4 << 9) | n3; n2=(n2 << 9) | n1; 

    if (mode==0) { TickCount(S360.Reg_Roller1_L, n4); TickCount(S360.Reg_Roller1_R, n2); }
    else { SetStateWithIntensity(S360.Reg_Roller1_L, n4); SetStateWithIntensity(S360.Reg_Roller1_R, n2); }

    // roller 2 is A REGISTER DISPLAY 
    switch (1+((int) GetState(S360.Label_Roller2_L) & 7)) {
       case 1: n=PC; break; // this is realistic
       case 2: n=regs[4]; break; 
       case 3: n = get_chan_info(1, 0) << 16; break; // return count bytes in channel 1 to be displayed in control panel
       case 4: n = get_chan_info(1, 1) << 8; break; // last data byte read/write, key and command
       case 5: n = get_chan_info(1, 2); break; // flags/status
       case 6: n = get_chan_info(2, 0) << 16; break; // same for channel 2
       case 7: n = get_chan_info(2, 1) << 8; break; // 
       case 8: n = get_chan_info(2, 2); break; // 
       default: n=0; break; 
    }
    n1= n & 0xFF; n1 |= (calculateParity(n1) << 8); n=n>>8; 
    n2= n & 0xFF; n2 |= (calculateParity(n2) << 8); n=n>>8; 
    n3= n & 0xFF; n3 |= (calculateParity(n3) << 8); n=n>>8; 
    n4= n & 0xFF; n4 |= (calculateParity(n4) << 8); n=n>>8; 
    n4=(n4 << 9) | n3; n2=(n2 << 9) | n1; 

    if (mode==0) { TickCount(S360.Reg_Roller2_L, n4); TickCount(S360.Reg_Roller2_R, n2); }
    else { SetStateWithIntensity(S360.Reg_Roller2_L, n4); SetStateWithIntensity(S360.Reg_Roller2_R, n2); }
}


// set cpu panel lights for 370 Model 148
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
//         2 set al lights to 1 (while lamp test button is pressed)
void SetCpuPanelLights_M148(int mode)
{
    t_uint64 AllOnes = (t_uint64)(-1);

    uint32 n;  
    int n1, n2, n3, n4;

    if (mode==2) {
        // set all to ones, is lamp test
        // this switch is only available for Model 145/148
        SetState(S360.Reg_Sys_Status, AllOnes);

        SetState(S360.Reg_Roller1_L, AllOnes);
        SetState(S360.Reg_Roller1_R, AllOnes);
        SetState(S360.Reg_Roller2_L, AllOnes);
        SetState(S360.Reg_Roller2_R, AllOnes);

        SetState(S360.LI_System, 1); 
        SetState(S360.LI_Manual, 1); 
        SetState(S360.LI_Wait, 1); 
        SetState(S360.LI_Test, 1); 
        SetState(S360.LI_Load, 1); 
        return; 
    }
    // Inside Cpu Panel, Operator Console Panel (ocp) lights are realistic
    n=((int)GetState(S360.SW_Rate) ? 1:0) || ((int)GetState(S360.SW_Addr_Compare_Cntrl) ? 1:0);
    SetOCPPanelLights(mode, n, S360.SW_F, S360.SW_G, S360.SW_H); 
    
    // EXE CPLT  Indicates CPU stopped state reached as result of:
    //              1. Pressing the STOP key.
    //              2. The RATE switch In the INSTRUCTION STEP position.
    //              3. A match being detected while In the address match mode of operation.
    // ADR COMP MATCH Indicates that the address (real or logical) being used to access
    //              main storage matches the address set into switches CDEFGH during an 
    //              address match operation. The action taken by the CPU
    //              as a result of the match condition is controlled by the setting of
    //              the ADDRESS COMPARE CONTROL switch.    
    n= (sim_is_running ? 0:(1<<2)) +       // EXEC CPLT
       ((cpu_mem_comp.hit) ? (1<<1):0);    // MATCH status

    if (mode==0) TickCount(S360.Reg_Sys_Status, n); 
    else SetStateWithIntensity(S360.Reg_Sys_Status, n); 

    // no control panel info is available. Modeled after Model 145
    // both machines seems to share the same (may be evolved) microarchitecture
    // in real hw, most of light of Model 145 are hiden inside to panel, not shown in cpanel impementation
    // into this top panel there is the roller handle for roller 2 (A register). In main panel only light
    // are visible; roller and roller handle are hidden into top panel

    // roller gets real data, even if it is not the microarch ones
    // roller 1 is DPLY ASSEMBLER OUT
    switch (1+((int) GetState(S360.Label_Roller1_L) & 7)) {
       case 1: n=cpu_mem_addr; break; // this is realistic
       case 2: n=cpu_mem_data; break; // this is realistic
       case 3: n=regs[0]; break; 
       case 4: n=regs[1]; break; 
       case 5: n=regs[2]; break; 
       case 6: n=PC; break; 
       case 7: n = InstrCycles << 4; break; // count of intructions loops executed so far (if cpu waiting, do get incremented)
       case 8: n = InstrExec << 9; break; // count of intructions executed so far (real instr, if cpu waiting, InstrExec do NO get incremented)
       default: n=0; break; 
    }
    n1= n & 0xFF; n1 |= (calculateParity(n1) << 8); n=n>>8; 
    n2= n & 0xFF; n2 |= (calculateParity(n2) << 8); n=n>>8; 
    n3= n & 0xFF; n3 |= (calculateParity(n3) << 8); n=n>>8; 
    n4= n & 0xFF; n4 |= (calculateParity(n4) << 8); n=n>>8; 
    n4=(n4 << 9) | n3; n2=(n2 << 9) | n1; 

    if (mode==0) { TickCount(S360.Reg_Roller1_L, n4); TickCount(S360.Reg_Roller1_R, n2); }
    else { SetStateWithIntensity(S360.Reg_Roller1_L, n4); SetStateWithIntensity(S360.Reg_Roller1_R, n2); }

    // roller 2 is unlabeled, and placed on top of roller 1 lights
    // as roller 2 handle is hidden in top panel, the lights are set fixed to PC value
    n=PC; // this is realistic

    n1= n & 0xFF; n1 |= (calculateParity(n1) << 8); n=n>>8; 
    n2= n & 0xFF; n2 |= (calculateParity(n2) << 8); n=n>>8; 
    n3= n & 0xFF; n3 |= (calculateParity(n3) << 8); n=n>>8; 
    n4= n & 0xFF; n4 |= (calculateParity(n4) << 8); n=n>>8; 
    n4=(n4 << 9) | n3; n2=(n2 << 9) | n1; 

    if (mode==0) { TickCount(S360.Reg_Roller2_L, n4); TickCount(S360.Reg_Roller2_R, n2); }
    else { SetStateWithIntensity(S360.Reg_Roller2_L, n4); SetStateWithIntensity(S360.Reg_Roller2_R, n2); }
}


#define     MT_is_loading_tape      1
#define     MT_is_rewinding         2
#define     MT_is_unloading_tape    3

int PARAM_MaxSlice_msec  =    50;  // max time considered for tape hop calculations
int PARAM_Reel_Diameter  =   267;  // reel diameter in mm 
int PARAM_RPM            =  1600;  // reel forward/backwards motor revolutions per minute
int PARAM_VacCol_h_Low   = 16200;  // upper vacuum colum sensor (inches x 1000) triggers reel load medium on colum 
int PARAM_VacCol_h_Hi    = 32000;  // lower vacuum colum sensor (inches x 1000) triggers reel take medium from colum 
int PARAM_RWSpeed        =    75;  // tape head operates at 75 inches per sec
int PARAM_AccelTime      =   225;  // accel time in msec. clutch must obtain 2/3 of maximum speed in 0.135-0.150 seconds with a full reel of tape
int PARAM_DecelTime      =   160;  // decel time in msec. the stop clutch must stop a full reel of tape from full speed in 0.145-0.160 sec
                                   // note: the accel/decel time increases when tape unit
                                   // get used. Lower values makes jerky spins with thigh 
                                   // oscilation of tape loop in vacuum columns. Higher values
                                   // makes smother spins, with more ample oscilations of tape loop
int PARAM_HeadOpenTime      = 700; // time in msec needed by r/w tape head to fully open or close
int PARAM_TakeMotor_RPM     =  40; // take motor revolutions per minute
int PARAM_HiSpeedRwdSpeed   = 500; // High Speed rewind at 500 inches/sec 
int PARAM_HiSpeedRwdTrigger = 1500;// High Speed rewind triggered if more than 1500 inches unwound 
int PARAM_HiSpeedRwdEnd     =  400;// High Speed terminates when less than 400 inches remains to be unwound 
int PARAM_SensorsPerVacCol  =   2; // number of tape medium sensors in each vacuum column. 
                                   // IBM 2401 Model 2,3,5,6 or 8, and IBM 2420 have 2 sensors per column; 
                                   // IBM 2401 Model 1 or 4, and IBM 2415 have only one sensor per column
                                   // IBM 2420 has a RWSpeed of 200 ips, HiSpeedRwd trigger is 60 feet

#define     MT_anim_step_nop       -1             // do nothing, just keeps current MT state values
#define     MT_anim_step_inc        0             // incremental animation step
#define     MT_anim_step_rw         1             // read/write tape animation step 
#define     MT_anim_step_HiRew      2             // High Speed Rewind animation step 
#define     MT_anim_finished       99             // this is the final step that signals the animation sequence has finished
#define     MT_anim_no_change    (1<<30)          // no change the current value.

void mt_reels_mov(int unit, int cmd, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot,
                  int * MT_Reel_Amount, int * MT_head, int bEnableRW);

// execute animation step. return 1 if animation terminated
int mt_do_animation_seq(int unit, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot,
                  int * MT_Reel_Amount, int * MT_head)
{
    int time, nseq, msec, hint, recsize, ang, n, m, n1, n2, u3, u3_dec, p, accel, dir; 
    int L_inc, R_inc, L_into_col, R_into_col, ireel; 
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
            *MT_Reel_Amount = mtcab[unit].state.MT_Reel_Ammount; 
            *MT_L_Rot =       mtcab[unit].state.MT_L_Rot; // this state includes blur
            *MT_R_Rot =       mtcab[unit].state.MT_R_Rot; 
            *MT_head =        mtcab[unit].state.MT_Head; 
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
            if (MT_anim_no_change & mtcab[unit].seq[nseq].MT_Reel_Amount) *MT_Reel_Amount = mtcab[unit].state.MT_Reel_Ammount; 
            else *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            for(ireel=0; ireel<2; ireel++) {
                ang=mtcab[unit].reel[ireel].ang; 
                switch(mtcab[unit].reel[ireel].color) {
                    case 1: n = 36 * ang / 360; break; // red reel  
                    case 2: n = 36 * ang / 360; break; // green reel  
                   default: n = 36 * ang / 360; break; // blue reel
                }
                if (ireel==0) *MT_L_Rot=n; else *MT_R_Rot =n; 
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
            mtcab[unit].rw_msec = mtcab[unit].rw_dir = 0;
            // incremental animation step done. Now check time variable. if time < 0 
            // then this is the current amimation step, so return 
            // on the contrary, it time >= 0 this is a pending incremental step that has
            // been executed. continue the main loop execute any other pending incremental
            // step until we arrive at the current animantion step
            if (time < 0) return 0; // exit after step execution. tnow is in this step
        } else if ((hint == MT_anim_step_rw) && (time < 0)) {
            // this step is regular tape mevement made by the r/w header, either stoped, backwards
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
            if (mtcab[unit].seq[nseq].MT_Reel_Amount == 0) {
                time=0; // first execution of this step
            } else {
                time = tnow - mtcab[unit].seq[nseq].MT_Reel_Amount;
                // max time considered between refreshes 
                if ((time < 0) || (time > PARAM_MaxSlice_msec)) time=PARAM_MaxSlice_msec; 
            }
            mtcab[unit].seq[nseq].MT_Reel_Amount = tnow;
            dir=mtcab[unit].seq[nseq].MT_head; // 1-> fwd, -1->bckwrds
            recsize  = dir * time * PARAM_RWSpeed; // inches x1000, 
            recsize = recsize / 2; 
            mtcab[unit].reel[0].VacCol_h -= recsize;
            mtcab[unit].reel[1].VacCol_h += recsize;
            mtcab[unit].rw_dir = dir;
            mt_reels_mov(unit, 0, 
                     L_VacColMedium_h, R_VacColMedium_h, 
                     MT_L_Rot, MT_R_Rot, 
                     MT_Reel_Amount, MT_head, 0);
            if (u3) {
                if (mtcab[unit].seq[nseq].R_VacCol_inc==0) {
                    // update mtcab[unit].rew_u3 to signal progress of rewind
                    p = 1000 - 1000*m / msec; 
                    if (p>1000) p=1000; if (p<0) p=0; // m=0..1000 is % x10 of time of step remaining
                    // calculate rew_u3 = how much medium has been rew 
                    mtcab[unit].rew_u3 = (int) (((t_int64) (u3)) * ((t_int64) (p)) / 1000);
                    *MT_Reel_Amount = 1 + (int) (99 * (mtcab[unit].rew_u3 / (mt_info[unit].MediumLen*1000.0))); 
                    if (*MT_Reel_Amount > 99) *MT_Reel_Amount = 99;
                } else {
                    // update to signal progress of r/w
                    mt_info[unit].MediumPos = (int) (1.0 * u3 * (dir < 0 ? msec-m:m) / msec); // to avoid int overflow
                    *MT_Reel_Amount = 1 + (int) (99 * (mt_info[unit].MediumPos / (mt_info[unit].MediumLen*1000.0))); 
                    if (*MT_Reel_Amount > 99) *MT_Reel_Amount = 99;
                }
                // check fast mode
                if (bFastMode) {
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
            //        is the amount of medium to rew in this step
            //        update mtcab[unit].rew_u3 with the amount of tape remaining to be rew
            u3     = mtcab[unit].seq[nseq].L_VacCol_inc;
            u3_dec = mtcab[unit].seq[nseq].R_VacCol_inc;
            accel=mtcab[unit].seq[nseq].MT_head; //1 (acceelerate), -1 (decelerate),
            *MT_Reel_Amount = mtcab[unit].seq[nseq].MT_Reel_Amount; 
            n1 = (int) mtcab[unit].state.MT_L_Rot; 
            n2 = (int) mtcab[unit].state.MT_R_Rot; 
            // msec+time = msecs elapsed of this animation step
            // -time     = msecs remaining to be done on this animation step until it ends
            // use it on acceleration/deceleration 
            m = msec+time; 
            if (accel) { // acceleration/deceleration
                if (accel<0) m = msec-m; // deceleration -> reverse m time used
                for(ireel=0; ireel<2; ireel++) {
                    n= (ireel == 0) ? n1:n2; 
                    switch(mtcab[unit].reel[ireel].color) {
                        case 1: n -= ((m<msec/3) ? 2 : (m<2*msec/3) ? 3 : 5); // red reel  
                                while (n<36) n+=24; while(n>60) n-=24;
                                ang=(n-36) * 15; 
                                break;
                        case 2: n -= ((m<msec/4) ? 1 : (m<2*msec/4) ? 2 : (m<3*msec/4) ? 3 : 5); // green reel  
                                while (n<36) n+=24; while(n>60) n-=24;
                                ang=(n-36) * 15; 
                                break;
                        default: n -= ((m<msec/3) ? 2 : (m<2*msec/3) ? 3 : 5); // blue reel
                                while (n<36) n+=24; while(n>60) n-=24;
                                ang=(n-36) * 15; 
                                break;
                    }
                    if (ireel==0) {n1=n; } else {n2=n; }
                    mtcab[unit].reel[ireel].ang=ang;
                }
            } else { // full speed
                for(ireel=0; ireel<2; ireel++) {
                    n= (ireel == 0) ? n1:n2; 
                    switch(mtcab[unit].reel[ireel].color) {
                        case 1: n -= 3; // red reel  
                                while (n<60) n+=8; while(n>67) n-=8; 
                                break; 
                        case 2: n -= 3; // green reel  
                                while (n<60) n+=8; while(n>67) n-=8; 
                                break;
                        default: n -= 3; // blue reel
                                while (n<60) n+=8; while(n>67) n-=8; 
                                break; 
                    }
                    if (ireel==0) n1=n; else n2=n; 
                }
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
            if (bFastMode) {
                // if Key Control-F (^F, Ctrl F) being pressed then set duration to 180 msec
                mtcab[unit].seq[nseq].msec = 180;
                if (bTapeIs != 3420) n1=1000; // make L reel transparent for 24XX tapes
            }
            if (bTapeIs==3420) {
                // 3420 tapes do in column rewind
                 // so glass door closed, tape medium in columns
                *MT_head  = 1;               
                // oscilate tape medium loop in columns each n msec
                p=mtcab[unit].rew_u3 / (mt_info[unit].MediumLen*10); 
                if (p<0) p=0; if (p>100) p=100; 
                m= msec+time; // time elapsed in this step
                n=100 + (100-p) *2/3; 
                n=abs((m % n) * 200 / n - 100) - 50; // tape loop oscilation amplitude -50..+50
                *L_VacColMedium_h = (int) (PARAM_VacCol_h_Hi * (1.0 + n*1.2/1000.0)); 
                n=100 + (p) *2/3; 
                n=abs((m % n) * 200 / n - 100) - 50; // tape loop oscilation amplitude -50..+50
                *R_VacColMedium_h = (int) (PARAM_VacCol_h_Low * (1.0 + n*1.2/1000.0)); 
                // keep the values in vaccol when quitting HiRew animation
                mtcab[unit].reel[0].VacCol_h = *L_VacColMedium_h; 
                mtcab[unit].reel[1].VacCol_h = *R_VacColMedium_h; 
            } else {
                *MT_head  = 16;               // head wide open
                *L_VacColMedium_h = 0;        // vac col empty
                *R_VacColMedium_h = 0;        // vac col empty
            }
            *MT_L_Rot = n1;
            *MT_R_Rot = n2;
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
    MT_head          = 16;   // head full open

    // head needs 1800 msec to go from wide open to close
    // reels rotates 360 + 90 + 45 gr during this time (take motor goes at 46 rpm)
    // prepare animation sequence each given msec
    msec=33;                                   // time for animation step 
    nHead   = PARAM_HeadOpenTime / msec;      // number of steps in animation of head closing
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

    L_h = 0;  // amount of tape medium in each column
    R_h = 0;

    // on 3420, 
    if (bTapeIs == 3420) {
        // load sequence for 3420
        // MT_Head=1 and MT_Reel_Amount=0 -> closed glass door, no data reel/ring is visible 
        // MT_Head=2..15 and MT_Reel_Amount=0 -> glass door opening, no data reel/ring is visible 
        // MT_Head=16 and MT_Reel_Amount=0 -> glass door full open, no data reel/ring is visible 
        // MT_Head=16 and MT_Reel_Amount=1 -> glass door full open, data reel visible, reel ring visible and closed
        // MT_Head=15..2 and MT_Reel_Amount=1 -> glass closing, data reel visible, reel ring opening
        // MT_Head=1 and MT_Reel_Amount=1 -> glass door closed, data reel visible, reel ring full open

        int msec = PARAM_HeadOpenTime / 16; 
        MT_head=1; 
        for(;;) {
            AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0, MT_head);     
            if (MT_head==16) break; 
            MT_head++;
        }

        // pause 0.8 sec
        AddSeq(unit, 800 /* msec */, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            0 /* L_ang_inc */, 0 /* R_ang_inc */, 
            0 /* MT_Reel_Amount */, 16 /* MT_head */);       
        // Reel Amount=0 and MT_Head=16 -> head is closed with reel amount = 1, thas is, reel visible, seal ring is opened and glass door closes

        MT_head=16; // open head
        for(;;) {
            AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                1 /* MT_Reel_Amount */, MT_head);     // all tape medium on data reel
            if (MT_head==1) break; 
            MT_head--;
        }
        nHead=0; // set to zero to inhibit MT_Head change during medium fed in columns
    }

    if (PARAM_SensorsPerVacCol == 2) {
        // take motor moves both reels at same low angular speed. tape medium enters in col 
        // depending on reel diameter (L reel feeds faster beacuse it is full, R reel feeds slow beause
        // near empty). medium is fed until both tape loops pases over upper vaccol sensor
        for(i=0;;i++) {
            // head position 
            if (nHead > 0) {
                MT_head = 16 * (nHead-i) / nHead; 
                if (MT_head<1) MT_head=1;
            }
            // reel rotation
            AddSeq(unit, msec, MT_anim_step_inc, 
                    L_inc_h  /* L_VacCol_inc */, R_inc_h /* R_VacCol_inc */, 
                    +ang_inc /* L_ang_inc */, -ang_inc /* R_ang_inc */, 
                    MT_Reel_Amount /* MT_Reel_Amount */, MT_head /* MT_head */ );       
            L_h += L_inc_h; 
            R_h += R_inc_h; 
            if ((R_h > PARAM_VacCol_h_Low) && (L_h > PARAM_VacCol_h_Low)) break; 
        }
    } else {
        // with one sensor per vaccol, medium is fed until left tape loops pases over upper vaccol sensor
        for(i=0;;i++) {
            // head position 
            MT_head = 16 * (nHead-i) / nHead; 
            if (MT_head<1) MT_head=1;
            // reel rotation
            AddSeq(unit, msec, MT_anim_step_inc, 
                    L_inc_h  /* L_VacCol_inc */, R_inc_h /* R_VacCol_inc */, 
                    +ang_inc /* L_ang_inc */, -ang_inc /* R_ang_inc */, 
                    MT_Reel_Amount /* MT_Reel_Amount */, MT_head /* MT_head */ );       
            L_h += L_inc_h; 
            R_h += R_inc_h; 
            if (L_h > PARAM_VacCol_h_Low) break; 
        }
    }
}

// add to the current animation sequence the unload animation:
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
    p = (int) ((u3 / (mt_info[unit].MediumPos*1000.0))*100); 
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

    if (PARAM_SensorsPerVacCol == 2) {
        L_h = R_h = (int) (PARAM_VacCol_h_Hi * 1.2); // maximum teorical value of tape medium in vacCol + 20% of safety margin
    } else {
        L_h = R_h = (int) (PARAM_VacCol_h_Low * 1.2); // maximum teorical value of tape medium in vacCol + 20% of safety margin
    }

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
    msec = PARAM_HeadOpenTime / 16; 
    for(i=1;i<16; i++) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_anim_no_change, MT_head);        
        MT_head++;
    }

    // check if unload terminated with r/w open but keeping the reel mounted 
    // (this is an unfload for rewind on 24XX)
    if (bUnLoadReel==0) return; 

    // pause 0.5 sec to remove reels
    msec=500;
    AddSeq(unit, msec, MT_anim_step_inc, 
            0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
            25 /* L_ang_inc */, +15 /* R_ang_inc */, 
            0, 16 /* MT_head open */ );       

    // close r/w head
    msec = PARAM_HeadOpenTime / 16; 
    MT_head=16; // open head
    for(;;) {
        AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                0, MT_head);     // no tape medium on reels   
        if (MT_head==1) break; 
        MT_head--;
    }
}

// calculate and store in animation array the animation sequence for rewind
void mt_set_rew_seq(int unit, int bStartEndSeqFlag)
{
    int MT_Reel_Amount,  MT_head;    // state of controls at end of of redraw
    int u3 = mt_info[unit].recsize;  // use recsize to get MediumPos value on rewind start 
    int msec, time, bHi, u3_dec, msec_accel, msec_decel; 

    MT_Reel_Amount = 1 + (int) (99 * (u3 / (mt_info[unit].MediumLen*1000.0))); 
    if (MT_Reel_Amount > 99) MT_Reel_Amount = 99;
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

    // check if rew at high speed. Hi Speed rew is done when at least 125 feet (1500 inch) in take reel
    bHi=0;
    if (u3 > PARAM_HiSpeedRwdTrigger * 1000) { //n has inches x1000 of tape medium used
        // yes, do high speed rew
        bHi=1; 

        if (bTapeIs==3420) {
            // 3420 tapes do in column rewind
            // so no need to unload tape
            msec_accel = 500; // time needed to accel to hi speed
            msec_decel = 500; // time to decel
        } else {
            // for 24XX tapes
            // add the unload animation:
            //    - spin reels slowly to unload medium on vacuum columns
            //    - open r/w head
            mt_add_unload_seq(unit, u3);
            MT_head        = 16;    // head open
                
            // pause 0.5 sec
            msec=500;
            AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, MT_head); 
            msec_accel = 1000; 
            msec_decel = 3000; 
        }

        // now start hi speed rew
        // rewind at 500 inch/sec (average) until 400 inches left
        u3 -= PARAM_HiSpeedRwdEnd*1000;     // now u3 in inches x1000 = ammount of medium inches to rewind at hi speed
        time= u3 / PARAM_HiSpeedRwdSpeed;   // hi speed rew total time in msec

        // hi rew with at least 1500 inches (leaves 400 for low speed res)
        // so min amount of reel rew at hi speed is 5000
        // this means 10 sec at 500 inc/sec
        // reduce this time with hi speed rew acceleration deceleration time
        time = time - msec_accel - msec_decel;

        // 1000 msec to accelerate. This is an educated guess
        msec = msec_accel; 
        AddSeq(unit, msec, MT_anim_step_HiRew, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, 1 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        

        // full speed rewind
        // calculate time of each step and how much mediun is rewinded on each step (u3_dec)
        if (MT_Reel_Amount < 2) {
            MT_Reel_Amount=2;               // safety
            msec = time; 
            u3_dec = u3;  // amount of medium rewinded on each animation step
        } else {
            msec = time / (MT_Reel_Amount-2 +1);   // time to decrease reel input by one
            u3_dec = u3 / (MT_Reel_Amount-2 +1); 
        }
        if (bFastMode == -1) msec = 100; // if ^F only 100 msec per decr of reel_amount

        u3 = mtcab[unit].rew_u3; // position of medium at beginning of hi speed rew;
                                 // at end of hi speeed rew this should have the value of 400x1000
        for(;;) {
            AddSeq(unit, msec, MT_anim_step_HiRew, 
                u3 /* L_VacCol_inc */, u3_dec /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, 0 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        
            u3 -= u3_dec;
            if (MT_Reel_Amount==2) break;
            MT_Reel_Amount--;
        }

        // 3000 msec to decelerate. This is an educated guess
        msec=msec_decel;
        AddSeq(unit, msec, MT_anim_step_HiRew, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, -1 /* acceleration flag: -1 decel, 0=full speed, -1=decelerate */ );        

        if (bTapeIs==3420) {
            // 3420 tapes do in column rewind
            // so no need to reload tape
        } else {
            // pause 0.5 sec
            msec=500;
            AddSeq(unit, msec, MT_anim_step_inc, 
                0 /* L_VacCol_inc */, 0 /* R_VacCol_inc */, 
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                MT_Reel_Amount, MT_head);        

            // load tape animation
            mt_add_load_seq(unit);
        }
        MT_head        = 1;    // head closed
        u3 = PARAM_HiSpeedRwdEnd*1000; // after hi speed rew, this is the amount of tape medium in reel
    }
    // finish rewinding at low speed
    msec = u3 / PARAM_RWSpeed;              // time to rewind remaining of tape at low speed (at r/w speed)
    AddSeq(unit, msec, MT_anim_step_rw,  u3 /* update recsize */,0,0,0,0, -1 /* read backwards */); 

    if (bStartEndSeqFlag) {
        // end sequence
        AddSeq(unit, 0,  MT_anim_finished, 0,0,0,0,0,0); 
    }

    // log on debug precise rew time 
    {
        DEVICE *dptr = find_dev_from_unit(&mta_unit[unit]);
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
// if cmode = 'L' or ' ' -> load animation: tape is mounted (attached) to tape unit. 
// If cmode = 'F' or 'B' load animation will be move all medium forwards/backwards
// if cmode = 'U' -> unload animation: tape is dismounted (detached) to tape unit. 
// during animations, from simulated cpu point of wiew: the tape is at its final state
//                                                      animation can be interrupted at any moment by tape command
void mt_set_anim_seq(int unit, char cmode)
{
    int n, msec, r, R_h, MT_head;
    int tm0, tm1; 

    // on load, reels rotates at low speed to feed vacuum columns with tape medium
    // while r/w head is closing. Then goes backwards to read read record backwards 
    // (locate the load point)

    mtcab[unit].nseq=0;                 // init sequence
    mtcab[unit].nseq_tm0=Refresh_tnow; // when animation starts

    if ((cmode == 'F') || (cmode == 'B')) {
        // forward all tape sequence
        msec = mt_info[unit].MediumLen * 1000 / PARAM_RWSpeed;
        if (cmode == 'B') {
            mt_info[unit].MediumPos = mt_info[unit].MediumLen * 1000;
            n=-1; 
        } else {
            n= 1;
        }
        AddSeq(unit, msec, MT_anim_step_rw,  mt_info[unit].MediumLen * 1000 ,1,
                                             0,0,0, n /* read fwd/bckwrd */); 
        // end sequence
        AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 
        return; 
    }

    // start delay depends on other loads/unloads already in progress 
    // This is done to avoid animations to be synchonized
    // determine when last animation started

    tm0=0;
    for (n=0;n<8;n++) {
        if (n==unit) continue; 
        if ((mtcab[n].mt_is == MT_is_loading_tape) || (mtcab[n].mt_is == MT_is_unloading_tape)) {
            // load/unload anim in progress in unit n. Let's know when it started
            tm1=mtcab[n].nseq_tm0; 
            if ((mtcab[n].seq[0].L_VacCol_inc==-1) && (mtcab[n].seq[0].R_VacCol_inc==-1)) {
                tm1 += mtcab[n].seq[0].msec; // add initial wait if any
            }
            if (tm0 < tm1) {
                tm0 = tm1; // time when last anim started (after any initial wait time)
            }
        }
    }
    // determine wait time to start
    msec=0; 
    if (tm0) {
        // there is an ongoing animation started (after initial wait) at tm0
        tm0 += PARAM_HeadOpenTime / 2; // add delay to anim to not be sync with previous one 
        if (tm0 > mtcab[unit].nseq_tm0) {
            // should wait 
            msec = tm0 - mtcab[unit].nseq_tm0; 
        }
    }
    if (msec == 0) msec=10; // min waiting time. Needed to identify it is an unload/load waiting
    
    if (bTapeIs == 3420) {
        // on 3420, glass door is allways closed, at start of load and at start of UnLoad
        MT_head=1; 
    } else {
        // on 24XX, head is closed on start of unload; head is open on start of load
        MT_head=(cmode == 'U') ? 1:16; // MT_head full closed:open 
    }

    AddSeq(unit, msec, MT_anim_step_inc, 
                -1 /* L_VacCol_inc */, -1 /* R_VacCol_inc */,  // to signal inital wait
                0 /* L_ang_inc */, 0 /* R_ang_inc */, 
                (cmode == 'U') ? MT_anim_no_change:0  /* MT_Reel_Amount */, 
                MT_head); 

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
    if (bTapeIs == 3420) {
        // On 3420, just go backwards 200gr fast, them 45 forward slow
        msec = 400;
        AddSeq(unit, msec, MT_anim_step_rw,  0,0,0,0,0, 
                                             -1 /* read backwards */); 
    } else {
        // on 24XXX, Duration of it depends on how much the user has spinned reel R when 
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
    }
    msec = 400;
    AddSeq(unit, msec, MT_anim_step_rw,  0,0,0,0,0, 
                                         1 /* read forward */); 

    // end sequence
    AddSeq(unit,    0, MT_anim_finished, 0,0,0,0,0,0); 

    // mt_dump_animation_seq(unit);

}

// tape r/w head & reels & vacuum columns simulation
// update tape controls for loop position on VacCols based on reels movement, loop 
// past the vaccols upper/lower sensors, r/w head reading forwards or backwards
void mt_reels_mov(int unit, int cmd, 
                  int * L_VacColMedium_h, int * R_VacColMedium_h, 
                  int * MT_L_Rot, int * MT_R_Rot,
                  int * MT_Reel_Amount, int * MT_head, int bEnableRW)
{
    uint32 tnow = Refresh_tnow;   
    static uint32 old_tnow = 0; 

    int time, time1, recsize, ireel, r, r1, r2, p, h, dir, 
        ang_inc, ang, n, tm0, msec, motor, old_motor[2], new_motor[2];
    double rpm, rpm0, rpmMax, rpsMax, a, am, revs; 
    
    struct oldrec {
        int h, ang, ang_inc;
        double rpm, revs;  
    } old[2]; 

    // save state of reels
    for(ireel=0;ireel<2;ireel++) {  
        old[ireel].h   = mtcab[unit].reel[ireel].VacCol_h;
        old[ireel].ang = mtcab[unit].reel[ireel].ang;
        old[ireel].ang_inc = 0;
        old[ireel].rpm = mtcab[unit].reel[ireel].rpm;
        old[ireel].revs = mtcab[unit].reel[ireel].revs;
    }

    // calc percent 0..100 of reel full
    p = (int) ((mt_info[unit].MediumPos / (mt_info[unit].MediumLen*1000.0))*100); // r=0 -> reel L full, p=100 -> reel R rull
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
    if (bEnableRW) {
        if (mtcab[unit].rw_msec) { 
            // calc time elapsed on r/w cmd
            time = tnow - mtcab[unit].rw_tm0;
            if (time < 0) 
                time=PARAM_MaxSlice_msec; 
            // calc medium (in inches x1000) that has been moved under r/w head
            recsize = PARAM_RWSpeed * time * mtcab[unit].rw_dir; 
            mtcab[unit].rw_tm0      = tnow;
            mtcab[unit].rw_msec    -= time; 
            if (mtcab[unit].rw_msec < 0) mtcab[unit].rw_msec=0;
            // take/give medium to vacuum columns
            // the maximun time slice allowed to avoid too big hops on tape medium is PARAM_MaxSlice_msec
            // PARAM_RWSpeed is the inches per second tape head moves the medium when reading/writing
            // this means the maximum linear increment reading or writing (in inches x1000) is
            n=PARAM_RWSpeed * PARAM_MaxSlice_msec;
            if (recsize >  n) recsize =  n;
            if (recsize < -n) recsize = -n;
            recsize = recsize / 2; // when adding/removing medium, loop position in vacuum colum moves half distance
            mtcab[unit].reel[0].VacCol_h -= recsize;
            mtcab[unit].reel[1].VacCol_h += recsize;
            bTapeAnimInProgress=1; // signal tape medium movement in progress 
        } else {
            mtcab[unit].rw_dir=0; // r/w head not moving tape medium
        }
        // check if a new r/w operation can be schedulled 
        if (mt_info[unit].cmd_tm0) { 
            // if no r/w op being animated (rw_msec=0), and now a r/w operation is being done by tape (cmd_tm0 > 0)
            mtcab[unit].rw_tm0     = tnow; 
            mtcab[unit].rw_msec    = mt_info[unit].cmd_msec;
            mtcab[unit].rw_dir     = mt_info[unit].TapeDir; // r/w head moving tape medium fwd (=1) or backward(=-1) 
            mt_info[unit].cmd_tm0=0;   // mark this as being animated (to avoid animating it twice)
            mt_info[unit].cmd_msec=0;
            bTapeAnimInProgress=1; // signal tape medium movement started
        }
    }
    
    // calc reel algular position base on motor accelerating or decelerating the reel
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
        if (ang_inc >  ang) ang_inc =  ang; 
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
        old_motor[ireel] = mtcab[unit].reel[ireel].motor;
        // get heigh of medium in vac col (0 = no medium loaded, nnn = these inches of medium loaded
        h = mtcab[unit].reel[ireel].VacCol_h;
        // ammount of medium in vac col is measured from top of column to tape medium loop
        // when no much medium is in vac col, h is low, when colum is full of medium then h is high
        if (PARAM_SensorsPerVacCol == 2) { 
            // two sensors per vacuum column 
            if (h < PARAM_VacCol_h_Low) {              // Upper column sensor h position
                motor=1; // accelerate forward, should load medium in colum
            } else if (h > PARAM_VacCol_h_Hi) {        // Lower column sensor h postion
                motor=-1; // accelerate backwards, should take medium from column
            } else {
                motor=0;   // decelerate motor (break reel)
            }
        } else {
            // one sensors per vacuum column 
            motor=0;
            if (mtcab[unit].rw_dir > 0) {              // tape going forward and ...
                if (h < PARAM_VacCol_h_Low) {          // ... tape loop between top of column and sensor 
                    if (ireel == 0) motor=1;           // then accelerate reel forward, should load medium in left colum
                } else {
                    if (ireel == 1) motor=-1;          
                }
            } else if (mtcab[unit].rw_dir < 0) {       // tape going backward 
                if (h < PARAM_VacCol_h_Low) {          
                    if (ireel == 1) motor=1;           
                } else {
                    if (ireel == 0) motor=-1;          
                }
            }
        }
        new_motor[ireel]=motor; // this is the new motor state
    }

    // starts/stop motors
    for(ireel=0;ireel<2;ireel++) {  
        // has motor changed its state?
        if (old_motor[ireel] == new_motor[ireel]) continue; // no, nothing to change
        // yes!, motor start (accelerate) or motor brake (decelerate). 
        time = 0;
        h = mtcab[unit].reel[ireel].VacCol_h;
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
        mtcab[unit].reel[ireel].motor = new_motor[ireel];
        // save time (tnow) when motor starts accelerating/decelerating
        mtcab[unit].reel[ireel].tm0   = tnow - time; 
        // init number of revolutions done
        mtcab[unit].reel[ireel].revs = 0; 
        // save reel initial angular position when motor starts accelerating/decelerating
        mtcab[unit].reel[ireel].ang0 = mtcab[unit].reel[ireel].ang;
        // save reel initial reel rpm speed when motor starts accelerating/decelerating
        mtcab[unit].reel[ireel].rpm0 = mtcab[unit].reel[ireel].rpm;             
    }

    // set reel/vacuum columns control state on cpanel
    for(ireel=0;ireel<2;ireel++) {       
        // get heigh of medium in vac col (0 = no medium loaded, nnn = these inches of medium loaded
        h = mtcab[unit].reel[ireel].VacCol_h;  
        // set height of medium 
        if (ireel) *R_VacColMedium_h = h; else *L_VacColMedium_h = h;
        // get angluar position of reel, and rotating direction (1-> ang incrments, -1 -> ang decrements)
        mtcab[unit].reel[ireel].debug_text[0]=0;
        ang = mtcab[unit].reel[ireel].ang; 
        dir = (old[ireel].ang_inc > 0) ? 1:-1; 

        // calc state that match angular position of reel and blur
        {
            static struct {
                int ang_inc_high_blur, state0_high_blur, nStates_high_blur;
                int ang_inc_slow_blur, state0_slow_blur, nStates_slow_blur;
                int nStates_no_blur; 
            } rcol[3] = {
                {2*45, 60, 8,      // color 0 -> blue reel
                 2*15, 36, 24, 36}, 
                {2*45, 60, 8,      // color 1 -> red reel 
                 2*15, 36, 24, 36}, 
                {2*45, 60, 8,      // color 2 -> green reel 
                 2*15, 36, 24, 36}, 
            };
            int c=mtcab[unit].reel[ireel].color; 
            // check if blur 
            if (abs(old[ireel].ang_inc) >= rcol[c].ang_inc_high_blur) {
                // high blur, set reel control at previous reel blur angular position respect rotation direction. 
                ang = ang - old[ireel].ang_inc / 2; 
                while (ang < 360) ang += 360; while (ang >= 360) ang -= 360; 
                n = rcol[c].state0_high_blur + rcol[c].nStates_high_blur * ang / 360; 
                bTapeAnimInProgress=1;  // signal reel blur started 
            } else if (abs(old[ireel].ang_inc) >= rcol[c].ang_inc_slow_blur) {
                // slow blur, set reel control at previous reel blur angular position respect rotation direction. 
                ang = ang - old[ireel].ang_inc / 2; 
                while (ang < 360) ang += 360; while (ang >= 360) ang -= 360; 
                n = rcol[c].state0_slow_blur + rcol[c].nStates_slow_blur * ang / 360; 
                bTapeAnimInProgress=1;  // signal reel blur started 
            } else {
                // no blur
                n = rcol[c].nStates_no_blur * ang / 360;
            } 
            // uncomment to display a debug message overprinted on reel
            //
            // sprintf(mtcab[unit].reel[ireel].debug_text, 
            //    "ang0 %d, \n ang_inc %d, \n ang %d, \n blur %d, \n n%d",
            //    mtcab[unit].reel[ireel].ang, old[ireel].ang_inc, ang, mtcab[unit].reel[ireel].hasblur, n);

        } 
        // set angular position state
        if (ireel) *MT_R_Rot = n; else *MT_L_Rot = n;
    }
    // set head/reel amount control state on cpanel
    // % of tape medium used
    *MT_Reel_Amount = 1 + (int) (99 * (mt_info[unit].MediumPos / (mt_info[unit].MediumLen*1000.0))); 
    if (*MT_Reel_Amount > 99) *MT_Reel_Amount = 99;
    // tape head on closed position
    *MT_head = 1;  

    // save last tnow used to know how much time elapsed since last refresh
    old_tnow = tnow;  
}

// dynamic draw of tape medium into vaccum column control CId_VacCol
// if VacColMedium_h = 15000 = PARAM_VacCol_h_Low -> tape medium loop positioned over upper vacuum colum sensor
// if VacColMedium_h = 40000 = PARAM_VacCol_h_Hi  -> tape medium loop positioned over lower vacuum colum sensor
void mt_VacColSetDynamicState(int VacColMedium_h, int * VacColMedium_h0, int CId_VacCol, int bDoorOpen)
{
    int h, y, y_top, h0, y0;
    if (VacColMedium_h == *VacColMedium_h0) return; // no changes in height. nothing to draw

    // calculate h
    // control has upper sensor on h=138, lower sensor on h=510
    h= 138 + ((510-138) * (VacColMedium_h-PARAM_VacCol_h_Low) / (PARAM_VacCol_h_Hi-PARAM_VacCol_h_Low));
    if (h<0) h=0; 
    // h is vertical position of tape loop base, converto to y coord
    y=730-h; if (y<0) y=0; 

    // calc also values for last VacColMedium_h0
    h0= 138 + ((510-138) * (*VacColMedium_h0-PARAM_VacCol_h_Low) / (PARAM_VacCol_h_Hi-PARAM_VacCol_h_Low));
    if (h0<0) h0=0; 
    y0=730-h0; if (y0<0) y0=0; 

    // Dynamically generate State 0 for CId_VacCol (=IBM360.MT_L|R_VacCol[unit])
    // control if door open

    if ((*VacColMedium_h0 == -1) || (VacColMedium_h == 0)) {
        // do a full redraw of backgound
        //  - Copy State 0 from control MT_VacColumn (background) to state 0 of CId (state displayed) 
        CopyControlImage(S360.MT_VacColumn, bDoorOpen,  0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_VacCol, 0,                      0, 0);       // ToCId, ToState,     x1, y1
    } else if (h0 < h) {
        // tape loop is down in vaccol respect previous draw. Just remove previous image tape loop
        CopyControlImage(S360.MT_VacColumn, bDoorOpen,  0, h0-40, 0, 50,  // FromCId, FromState, x0, y0, w, h, 
                         CId_VacCol, 0,                      0, h0-40);       // ToCId, ToState,     x1, y1
    } else {
        // tape loop is up in vaccol respect previous draw. Just remove image form new uppet loop to old tape loop
        CopyControlImage(S360.MT_VacColumn, bDoorOpen,  0, h-40, 0, (h0+50)-(h-40),  // FromCId, FromState, x0, y0, w, h, 
                         CId_VacCol, 0,                      0, h-40);       // ToCId, ToState,     x1, y1
    }

    //  - Copy State 0 from MT_VacColMedium (height dependes on medium position) on State 0 -> now vaccol with tape medium
    y_top= (bDoorOpen) ? 0:135; // upper top of VacCol is hidden under tape cabinet door if door is closed
    if (bTapeIs==3420) y_top=0; // on 3420, medium is visible from this position

    if (VacColMedium_h) {
        CopyControlImage(S360.MT_VacColMedium, 0,    0, y_top+y, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_VacCol, 0,                   0, y_top+0);       // ToCId, ToState,     x1, y1
    }
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId_VacCol, 0);

    *VacColMedium_h0 = VacColMedium_h; 
}

// dynamic draw of tape medium into vaccum column control CId_VacCol
// if VacColMedium_h = 15000 = PARAM_VacCol_h_Low -> tape medium loop positioned over upper vacuum colum sensor
// ireel=0 for left vaccol, =1 for right vaccol
void mt_VacColSetDynamicState_2415(int VacColMedium_h, int * VacColMedium_h0, int CId_VacCol, int ireel)
{
    int d, x_base, x0, x1, w;

    // vacuum column interior visible!. Metal cover removed. 
    // tape medium to draw
    if ((VacColMedium_h == *VacColMedium_h0) && (*VacColMedium_h0 != -1)) {
        // no changes. nothing to draw. Just set to draw the already generated dynamic state
        // as nothing draw, no need to force its redar with cpanel_ControlRedrawNeeded
        SetState(CId_VacCol, 4);
        return; 
    }

    // MT_VacColumn control covers both vaccols (width 590). State 0=VacCol not visible, State>=1 visible
    // right vaccol starts at +295. Each vaccol has a width of 295
    x_base = (ireel==0) ? 0:295; 

    if (VacColMedium_h <= 0) {
        // copy state 3 (vaccol visible, tape medium in border of column) to state 4 (dymanic draw)
        CopyControlImage(CId_VacCol, 3,    x_base, 0, 295, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_VacCol, 4,    x_base, 0);         // ToCId, ToState,     x1, y1
        goto EndOfDraw;
    } 

    // calculate d (horizontal displacement from vaccol entry point to inner column)
    // convert d to x coord for vaccolmedium and vaccol
    if (ireel == 0) {
        // left column
        d= 130 * VacColMedium_h / PARAM_VacCol_h_Low;
        x0=290-d;  // start of tape medium at d
        w=0;  // draw until tape loop
        x1=30; // start draw on vaccol 
    } else {
        // right column
        d= 130 * VacColMedium_h / PARAM_VacCol_h_Low;
        x0=10; 
        w=45+d;
        x1=x_base+220-d; 
    }

    // Dynamically generate State 0 for CId_VacCol (=IBM360.MT_L|R_VacCol[unit])

    // redraw of open vaccol backgound
    //  - Copy State 1 from control MT_VacColumn (vaccol visible background) to state 0 of CId (state displayed) 
    CopyControlImage(CId_VacCol, 2,    x_base, 0, 295, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol, 4,    x_base, 0);         // ToCId, ToState,     x1, y1

    //  - Copy State 0 from MT_VacColMedium on State 0 -> now vaccol with tape medium
    CopyControlImage(S360.MT_VacColMedium, ireel, x0,  0, w, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId_VacCol, 4,                    x1, 19);       // ToCId, ToState,     x1, y1

  EndOfDraw:
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId_VacCol, 4);

    *VacColMedium_h0 = VacColMedium_h; 
}

// dynamic draw of tape medium into L and R vaccum columns for given unit 
void mt_VacColSet(int unit, int L_VacColMedium_h, int R_VacColMedium_h)
{
    // when door open changes its state, the caller must also set L|R_VacColMedium_h0 to -1 to force redraw
    if (bTapeIs==3420) {
        // on 3420, Left reel is take reel (on 24XX, take reel is right reel)
        // so we draw columns switched: Left column is draw on MT_R_VacCol control
        mt_VacColSetDynamicState(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, S360.MT_R_VacCol[unit], 0);
        mt_VacColSetDynamicState(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, S360.MT_L_VacCol[unit], 0);
    } else if (bTapeIs==2415) {
        int CId_VacCol; 
        if (mtcab[unit].state.MT_DoorOpen == 0) {
            // vacuum column interior not visible. Metal cover not removed. Tape loop not visible :-(
            SetState(S360.MT_L_VacCol[unit], 0); 
            EndOfDraw:
            mtcab[unit].L_VacColMedium_h0=L_VacColMedium_h;
            mtcab[unit].R_VacColMedium_h0=R_VacColMedium_h;
            return; 
        }
        // vacuum column interior visible!. Metal cover removed. 
        // check is tape medium loaded in r/w head
        if (mtcab[unit].state.MT_Reel_Ammount == 0) {
            // no medium loaded -> no tape loop to draw
            SetState(S360.MT_L_VacCol[unit], 1); 
            goto EndOfDraw;
        }
        // only one horizontal column (covers Left and Roght reel), set on S360.MT_L_VacCol[unit] control
        // S360.MT_R_VacCol[unit] is not used in 2415
        CId_VacCol=S360.MT_L_VacCol[unit]; 
        mt_VacColSetDynamicState_2415(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, CId_VacCol, 0);
        mt_VacColSetDynamicState_2415(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, CId_VacCol, 1);
    } else {
        mt_VacColSetDynamicState(L_VacColMedium_h, &mtcab[unit].L_VacColMedium_h0, S360.MT_L_VacCol[unit], mtcab[unit].state.MT_DoorOpen);
        mt_VacColSetDynamicState(R_VacColMedium_h, &mtcab[unit].R_VacColMedium_h0, S360.MT_R_VacCol[unit], mtcab[unit].state.MT_DoorOpen);
    }
}

// open/close tape cabinet door. bDoorOpen=1 -> tape cabient open
void cpanel_set_mt_door(int unit, int bDoorOpen)
{
    // set cabinet state to open/closed
    if (bTapeIs==3420) return; // 3420 door open not available
    mtcab[unit].state.MT_DoorOpen=bDoorOpen; 
    if (bTapeIs==2415) {
        // set state of vacuum colum metal cover. 0=metal cover visible (vac col interior not visible, tape loop not visible) 
        // 1=metal cover removed, vac col interior visible (but no tape loop draw)
        // next refresh will fill the interior with tape loop
        SetState(S360.MT_L_VacCol[unit], bDoorOpen);
    } else {
        SetState(S360.MT_panel[unit], bDoorOpen);
    }
    // now should invalidate all controls that needed to be repainted as MT_panel will be draw over them
    // invalidate vaccol and head last value to force redraw 
    mtcab[unit].L_VacColMedium_h0=-1;
    mtcab[unit].R_VacColMedium_h0=-1;
    mtcab[unit].state.MT_Head=-1; 
    // invalidate digits to force redraw
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_addr_digit[unit][1],GetState(S360.MT_addr_digit[unit][1]));
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_addr_digit[unit][2],GetState(S360.MT_addr_digit[unit][2]));
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_addr_digit[unit][3],GetState(S360.MT_addr_digit[unit][3]));
    // invalidate lights
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_LI_Ready[unit],GetState(S360.MT_LI_Ready[unit]));
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_LI_Select[unit],GetState(S360.MT_LI_Select[unit]));
    cpanel_ControlRedrawNeeded=1; SetState(S360.MT_LI_File_protect[unit],GetState(S360.MT_LI_File_protect[unit]));
}

// return 1 if a rewin animation is in progress
int cpanel_is_rewinding(int unit)
{
    if (mtcab[unit].mt_is == MT_is_rewinding) return 1;
    return 0; 
}

// dynamic draw tape medium from x0,y0, right direction (if len >0), or left direction (if len < 0) 
// for abs(len) pixels, with given slope, until hit a white pixel (of capstain)
void mt_TapeMediumToRwHeadDynamicState(uint32 * surface, int pitch, int x0, int y0, int len, uint32 col, int slope)
{
    int x,y,p,dy,dyy, rr,gg,bb, sgn, bStopAtCapstain;

    if (len >= 0) {
        sgn=1; 
    } else {
        sgn=-1; len=-len; 
    }
    bStopAtCapstain = (slope > 0) ? 1:0;
    dy=0;
    if (slope==0) dyy=0; else dyy = 1000 * (slope+1) / len; 
    // draw line (5 pixels wide)
    for (x=0;x<len;x++) {
        p=x0 + pitch * y0; 
        if (sgn > 0) p=p+x; else p=p-x; 
        dy += dyy; 
        if (dy > 1000) { dy -= 1000; p+=pitch; y0++; }
        p+=5*pitch;
        for (y=0;y<5;y++) {
            get_surface_rgb_color(surface[p], &rr, &gg, &bb);
            if ((bStopAtCapstain) && (rr > 128) && (gg > 128) && (bb > 128)) break; 
            surface[p] = col;
            p-=pitch; 
        }
        if (y==0) break; // hit capstain with lower border of tape
    }
}

// dynamic draw tape medium from external vaccol capstain to left reel (if ireel=0), or right reel (if ireel=1) 
// draw until hit reel with ammount of medium given by Reel_Amount (1..67)
void mt_TapeMediumToReelDynamicState(uint32 * surface, int pitch, int ireel, int Reel_Amount, uint32 col)
{
    // tangent coords relative to reel centre
    static int xytg[4][2*6] = { // for 2401 tape 
                               0, 0,    45, 33,    55, 45,    70, 52,     75, 72,    81, 91,
                                // for 2415 tape
                               0,  0,  -53, 20,   -67, 25,   -82, 30,   -101, 24,  -121, 19,
                                // for 3420 tape. take reel (when ireel==0)
                               0,  0,  -60, 42,   -61,  60,  -68,  69,  -74,  80,  -60, 107, 
                                // for 3420 tape. data reel (when ireel==1)
                               0,  0,  -45, 30,   -57,  40,  -74,  45,  -80,  65,  -70,  95 };
    static int xycapstain[4][4]   = {115, 288, 434, 288,      // for 2401 tape
                                       0,   0, 575,   0,      // for 2415 tape
                                     160, 288,   0,   0,      // for 3420 tape, left (take) reel
                                     0,   0,  120, 294};      // for 3420 tape, right (data) reel
    static int xyreelcentre[4][4] = {125, 123, 137+285, 123,  // for 2401 tape
                                     145, 157, 145+285, 157,  // for 2415 tape
                                     145, 145,       0,   0,  // for 3420 tape, left (take) reel
                                       0,   0,     145, 145}; // for 3420 tape, right (data) reel
    int p, x0, y0, x1, y1, x, y, yH, dd, dx, dy, dxx, dyy, len, index, thick; 
    uint32 col_reel, col_reel2, c; 
    int base; 

    if (bTapeIs == 3420) base=2 + ireel; else // for 3420 there are two sets, because reels are not symetric
    if (bTapeIs == 2415) base=1; else 
    base=0; // for 2401

    // Reel_Amount:   0 | 1 .. 15 | 16 .. 31 |32 ...47 | 48 ..  67
    // index in xytg: 0   1          2         3          4      5
    if (Reel_Amount <= 16) {index=2; p=100*(Reel_Amount-1)/15; } else
    if (Reel_Amount <= 32) {index=3; p=100*(Reel_Amount-16)/16; } else
    if (Reel_Amount <= 48) {index=4; p=100*(Reel_Amount-32)/16; } else {index=5; p=100*(Reel_Amount-48)/19; }

    x0=  xycapstain[base][ireel*2]; 
    y0=  xycapstain[base][ireel*2+1];  
    x1=xytg[base][index*2-2  ] + p*(xytg[base][index*2  ] - xytg[base][index*2-2])/100;
    y1=xytg[base][index*2-2+1] + p*(xytg[base][index*2+1] - xytg[base][index*2-2+1])/100;

    x1=xyreelcentre[base][ireel*2]  +x1 * ( (ireel == 0) ? 1:-1);
    y1=xyreelcentre[base][ireel*2+1]+y1;

    yH=max(y0, y1); 
    // colors to not overdraw
    col_reel= surface_rgb_color(55, 55, 55) ;  // color for tape medium winded on reel 
    if (bTapeIs == 3420) {
        col_reel2= surface_rgb_color(0, 0, 0) ; // color for central reel hub
    } else {
        col_reel2= surface_rgb_color(208, 209, 201) ; // color for reel fixation 1
    }
        
    dx=x1-x0; 
    dy=y1-y0; 

    // draw line (10 pixels wide)
    thick=10; 

    if (abs(dx)>=abs(dy)) {
        // iterate on x
        len=abs(dx); 
        dyy = 1000 * dy / len; dd=0;
        for (x=0;x<len;x++) {
            p=x0 + pitch * y0; 
            for (y=0;y<thick;y++) {
                if (y0+y > yH) break;
                c=surface[p];
                // do not overprint reel fixation nor reel winded medium
                if ((col_reel != c) && (col_reel2 != c)) surface[p] = col;
                p+=pitch; 
            }
            if (dx > 0) x0++; else x0--; 
            dd += dyy; 
            if (dd < -1000) { dd += 1000; y0--; } else 
            if (dd >  1000) { dd -= 1000; y0++; }  
        }
    } else {
        // iterate on y
        len=abs(dy); 
        dxx = 1000 * dx / len; dd=0;
        for (y=0;y<len;y++) {
            p=x0 + pitch * y0; 
            for (x=0;x<thick;x++) {
                c=surface[p];
                // do not overprint reel fixation nor reel winded medium
                if ((col_reel != c) && (col_reel2 != c)) surface[p] = col;
                if (dx > 0) p++; else p--;
            }
            if (dy > 0) y0++; else y0--; 
            dd += dxx; 
            if (dd < -1000) { dd += 1000; x0--; } else
            if (dd >  1000) { dd -= 1000; x0++; }
        }
    }
}

// dynamic draw reels and head and tape medium for 24XX tapes
void mt_ReelsSetDynamicState(int unit, int MT_head, int MT_Reel_Amount, int MT_L_Rot, int MT_R_Rot, 
                             int L_VacCol_empty, int R_VacCol_empty, int CId_MT)
{
    uint32 * surface;       // reels, head, medium dynamicaly generated into MT control
    int x0, y0, ww, hh, x_reel, y_reel;     // MT control pos and size
    int nR, nL, ireel, CId_Reel, x,len; 
    int bRedrawL, bRedrawR, bRedraw; 
    uint32 col; 

    surface=GetControlSurface(CId_MT, 0, &ww, &hh); 
    x0 = GetControlInfo(S360.MT[0], CINFO_X) - GetControlInfo(S360.MT_panel[0], CINFO_X);
    y0 = GetControlInfo(S360.MT[0], CINFO_Y) - GetControlInfo(S360.MT_panel[0], CINFO_Y);
    col = surface_rgb_color(20, 20, 20) ; // for 24XX tape medium color is near black

    bRedrawL= (mtcab[unit].state.MT_L_Rot != MT_L_Rot) ? 1:0; 
    bRedrawR= (mtcab[unit].state.MT_R_Rot != MT_R_Rot) ? 1:0; 
    // check last state to determine what needs to be redraw
    if ((mtcab[unit].state.MT_Head != MT_head) || (mtcab[unit].state.MT_Reel_Ammount != MT_Reel_Amount))  {        
        bRedraw=1; bRedrawL=bRedrawR=1; // redraw all
    } else {
        bRedraw=0;
        if ((bRedrawL==0) && (bRedrawR==0)) return; // nothing to update
    }

    mtcab[unit].state.MT_Head=MT_head; 
    mtcab[unit].state.MT_Reel_Ammount=MT_Reel_Amount;
    mtcab[unit].state.MT_L_Rot=MT_L_Rot;
    mtcab[unit].state.MT_R_Rot=MT_R_Rot; 

    // Dynamically generate State 0 for CId_MT (=IBM360.MT[unit])
    if (bRedraw) {
        //  - Copy State 0 from control MT_Panel (background) to state 0 of CId_MT (state displayed) 
        CopyControlImage(S360.MT_panel[0], 0,          x0, y0, ww, hh,   // FromCId, FromState, x0, y0, w, h, 
                         CId_MT, 0,                         0,  0);           // ToCId, ToState,     x1, y1
                         
        if (MT_head < 1) MT_head=1; // closed head 
        if (bTapeIs==2415) {
            // set head state
            if (MT_head > 8) MT_head=8; // open head 
            SetState(S360.MT_Head_pos[unit], MT_head);
        } else {
            // - Copy head image
            CopyControlImage(S360.MT_head, MT_head,        50, 10, 260, 115,  // FromCId, FromState, x0, y0, w, h, 
                             CId_MT, 0,                         291-x0, 517-y0);             // ToCId, ToState,     x1, y1      
        }
        if (bTapeIs==2415) {
            // - Set state to show tape medium lines from vacuum column top capstains to r/w head
            SetState(S360.MT_Head_Medium[unit], (MT_Reel_Amount > 0) ? 1:0);
        } else {
            // - Draw tape medium lines from vacuum column internal capstain to r/w head
            if (MT_Reel_Amount > 0) {
                x0=165; y0=328; 
                if (L_VacCol_empty) { // vac col empty -> medium visible on top of column
                   len=42;  mt_TapeMediumToRwHeadDynamicState(surface, ww, x0-len, y0, len, col,0);
                }
                if (R_VacCol_empty) { // vac col empty -> medium visible on top of column
                   len=42;  mt_TapeMediumToRwHeadDynamicState(surface, ww, x0+260, y0, -len, col,0);
                }
                if (MT_head > 10) { // head wide open -> medium visible crossing r/w head
                   len=220; 
                   mt_TapeMediumToRwHeadDynamicState(surface, ww, x0, y0, len, col,0);
                } else if (MT_head > 0) { // tape medium being fixed under head capstains
                   len=40;
                   mt_TapeMediumToRwHeadDynamicState(surface, ww, x0,     y0+2,  len, col, (11-MT_head));
                   mt_TapeMediumToRwHeadDynamicState(surface, ww, x0+220, y0+2, -len, col, (11-MT_head));
                   if (MT_head > 1) { 
                       mt_TapeMediumToRwHeadDynamicState(surface, ww, x0+150, y0+2,  len, col, (11-MT_head));
                       mt_TapeMediumToRwHeadDynamicState(surface, ww, x0+60,  y0+2, -len, col, (11-MT_head));
                   }
                }
            }
        }
    }

    // position of reel in CId_MT control
    if (bTapeIs==2415) {
        x_reel=-4; y_reel=9; 
    } else {
        x_reel=-18; y_reel=-18;
    }

    //  - Copy tape medium ammount winded in Left and Right reel 
    if (MT_Reel_Amount < 1) {
        nL=0; nR=0;
    } else {
        // MT_Reel_Amount=1 -> all medium on left reel
        // state: 0=empty, 1=few medium on reel, 99=reel full
        nL=100-MT_Reel_Amount;    
        nR=MT_Reel_Amount;       
        // map MT_Reel_Amount value 1..99 into 1..79 available states
        // the mapping is non-linear, as radius grows fast when reel has not much medium (radius is small)
        // and then radius grows slow when reel gets full (radius is big)
        if (nL < 25) {nL=nL*2;} else if (nL < 50) {nL=nL+25;} else {nL = 75 + (nL-50) / 2;} nL=67*nL/100; 
        if (nR < 25) {nR=nR*2;} else if (nR < 50) {nR=nR+25;} else {nR = 75 + (nR-50) / 2;} nR=67*nR/100;  
        // safety
        if (nL<1) nL=1; if (nL>67) nL=67; 
        if (nR<1) nR=1; if (nR>67) nR=67; 
    }
    // if tape is 2415 and vaccol not visible -> do not draw tape up to top (would overdraw vaccol)
    if (bRedrawL) {
        // draw tape medium for left reel
        CopyControlImage(S360.MT_reel_amount, nL,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_MT, 0,                   x_reel, y_reel);       // ToCId, ToState,     x1, y1
        if (nL) mt_TapeMediumToReelDynamicState(surface, ww,  0, nL, col);      
    }
    if (bRedrawR) {
        // draw tape medium for right reel
        CopyControlImage(S360.MT_reel_amount, nR,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_MT, 0,                     285+x_reel, y_reel);       // ToCId, ToState,     x1, y1
        if (nR) mt_TapeMediumToReelDynamicState(surface, ww, 1, nR, col);
    }
                            
    for (ireel=0; ireel<2; ireel++) {
        if (((ireel==0) ? bRedrawL:bRedrawR)==0) continue; 
        // - Draw tape medium lines from vaccum column external capstain to reel 
        // - Copy Left and Right reel and its hub
        // nL = state for reel
        if (ireel==0) {
            nL=MT_L_Rot; x=0; 
            if (nL==1000) continue; // state 1000 -> reel transparent
        } else {
            nL=MT_R_Rot; x=285;
        }
        switch(mtcab[unit].reel[ireel].color) {
            case 1: CId_Reel = S360.MT_red_reel; break; 
            case 2: CId_Reel = S360.MT_green_reel; break; 
           default: CId_Reel = S360.MT_blue_reel; break; 
        }
        // draw reel on top of medium

        if (0) { 
            // debug mode -> will display one reel state at the time, cycling from 0 up to max number of states
            // the current state for reel image show into the hub
            // press Z/X key to incr/decr state number
            char buf[10];
            static int current_reel_state = 0; 
            if (vid_keyb.LastKeyPress == 'z') {
                vid_keyb.LastKeyPress = 0; current_reel_state++;
            } else if (vid_keyb.LastKeyPress == 'x') {
                vid_keyb.LastKeyPress = 0; current_reel_state--;
            }
            if (current_reel_state >= GetControlInfo(CId_Reel, CINFO_NSTATES)) current_reel_state=0;
            else if (current_reel_state < 0) current_reel_state = GetControlInfo(CId_Reel, CINFO_NSTATES)-1; 
            nL=current_reel_state; 
            sprintf(buf, "%d", nL);
            DrawTextImage   (CId_Reel, nL, 140,140, 0, 0,     // ToCId, ToState, x0, y0, w0, h0: where to draw text
                               0,0,0,       // r, g, b: text color
                               buf, 1);       // str to draw, char size

        }
        CopyControlImage(CId_Reel, nL,     0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_MT, 0,             x+x_reel, y_reel);  // ToCId, ToState,     x1, y1
        // overprint debug text in reel if any
        if (mtcab[unit].reel[ireel].debug_text[0]) {
            DrawTextImage   (CId_MT, 0, x+5,100, 0, 0,     // ToCId, ToState, x0, y0, w0, h0: where to draw text
                               255,255,255,       // r, g, b: text color
                               mtcab[unit].reel[ireel].debug_text, 2);       // str to draw, char size

        }
    }
    //  - Set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId_MT, 0);
}

// dynamic draw reels and head and tape medium for 3420 tapes
void mt_ReelsSetDynamicState_3420(int unit, int MT_head, int MT_Reel_Amount, int MT_Data_Rot, 
                                  int Data_VacCol_empty, int Take_VacCol_empty, 
                                  int CId_MT_Data, int CId_MT_Take)
{
    uint32 * surface;       // reels, head, medium dynamicaly generated into MT control
    int x0, y0, ww, hh, h;  // MT control pos and size
    int nR, nL, CId_Reel, TapeWinded; 
    int bRedrawData, bRedrawTake, bRedrawGlassDoor; 
    uint32 col; 

    // On 3420, Right reel is the mounted data reel     
    // Left reel is the take reel
    // outside this routine MT_L is data reel
    
    // lets draw the data reel
    surface=GetControlSurface(CId_MT_Data, 0, &ww, &hh); 
    col = surface_rgb_color(55, 55, 55) ; // tape medium color 

    // MT_Head holds the state of reel seal & state of glass door
    bRedrawTake=0;
    bRedrawData=(mtcab[unit].state.MT_L_Rot != MT_Data_Rot) ? 1:0; 
    if ((mtcab[unit].state.MT_Reel_Ammount != MT_Reel_Amount) || 
        (mtcab[unit].state.MT_Reel_Ammount < 0) || 
        (mtcab[unit].state.MT_Head != MT_head) ||
        (mtcab[unit].state.MT_Head < 0)) {        
        bRedrawData=bRedrawTake=1; // redraw all
    } else {
        if (bRedrawData==0) return; // nothing to update
    }
    // is glass door moving?
    bRedrawGlassDoor=((mtcab[unit].state.MT_Head != MT_head) || (mtcab[unit].state.MT_Head < 0)) ? 1:0; 

    // is tape medium winded on take reel?
    if (Data_VacCol_empty && Take_VacCol_empty) TapeWinded=0; else TapeWinded=1; 

    if (mtcab[unit].state.MT_R_Rot != TapeWinded) {
        bRedrawData=bRedrawTake=1; // redraw 
    }

    mtcab[unit].state.MT_Head=MT_head; 
    mtcab[unit].state.MT_Reel_Ammount=MT_Reel_Amount;
    mtcab[unit].state.MT_L_Rot=MT_Data_Rot;
    mtcab[unit].state.MT_R_Rot=TapeWinded; // save the tape winded state here

    // Dynamically generate State 0 for CId_MT_Data (=IBM360.MT[unit])
    if (bRedrawData) {
        if (MT_Reel_Amount == 0) {
            // no tape medium -> no reels visible
            //  - Copy State 0 from control MT_no_reel to state 0 of CId_MT (state displayed) 
            CopyControlImage(S360.MT_no_reel, 0,          0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Data, 0,                    0, 0);  // ToCId, ToState,     x1, y1
        } else {
            //  - Copy State 0 from control MT_Panel (background) to state 0 of CId_MT (state displayed) 
            x0 = GetControlInfo(S360.MT[0], CINFO_X) - GetControlInfo(S360.MT_panel[0], CINFO_X);
            y0 = GetControlInfo(S360.MT[0], CINFO_Y) - GetControlInfo(S360.MT_panel[0], CINFO_Y);
            CopyControlImage(S360.MT_panel[0], 0,          x0, y0, ww, hh,   // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Data, 0,                       0,  0);           // ToCId, ToState,     x1, y1
                         
            // MT_head 1..9 -> sets MT_ring/MT_ring_anchor 1..9
            //         1..15 -> sets MT_GlassDoor
            if (MT_head <= 1) { 
                // closed head -> tape medium mounted -> open reel seal ring -> MT_ring state=1
                nL=1; 
            } else if (MT_head >= 9) {
                // open head  -> tape medium NOT mounted -> closed reel seal ring -> MT_ring state=9
                nL=9;
            } else {
                nL=MT_head; 
            }
            // - Copy seal ring image
            CopyControlImage(S360.MT_ring, nL,                0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Data,  0,                     6, 182);             // ToCId, ToState,     x1, y1      
            // - Copy seal anchor image
            CopyControlImage(S360.MT_ring_anchor, nL,         0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Data,  0,                      -9, 42);             // ToCId, ToState,     x1, y1      
        }
    }

    if (MT_Reel_Amount < 1) {
        nL=0; nR=0;
    } else {
        // MT_Reel_Amount=1 -> all medium on Data reel (nL)
        // state: 0=empty, 1=few medium on reel, 99=reel full
        nL=100-MT_Reel_Amount;    
        nR=MT_Reel_Amount;       
        // map MT_Reel_Amount value 1..99 into 1..79 available states
        // the mapping is non-linear, as radius grows fast when reel has not much medium (radius is small)
        // and then radius grows slow when reel gets full (radius is big)
        if (nL < 25) {nL=nL*2;} else if (nL < 50) {nL=nL+25;} else {nL = 75 + (nL-50) / 2;} nL=67*nL/100; 
        if (nR < 25) {nR=nR*2;} else if (nR < 50) {nR=nR+25;} else {nR = 75 + (nR-50) / 2;} nR=67*nR/100;  
        // safety
        if (nL<1) nL=1; if (nL>67) nL=67; 
        if (nR<1) nR=1; if (nR>67) nR=67; 
    }
    if ((bRedrawData) && (nL)) {
        // draw tape medium for Data reel
        CopyControlImage(S360.MT_reel_amount, nL,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_MT_Data, 0,                   2, -2);       // ToCId, ToState,     x1, y1
        if (TapeWinded) {
            mt_TapeMediumToReelDynamicState(surface, ww,  1, nL, col);   
            // redraw lower ring, as previous routine has overwritten it
            CopyControlImage(S360.MT_ring, 10,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Data, 0,         112,250);       // ToCId, ToState,     x1, y1
        }
    }
    if (bRedrawTake) {
        //  - Copy State 0 from control MT_Panel (background) to state 0 of CId_MT_Take (state displayed) 
        surface=GetControlSurface(CId_MT_Take, 0, &ww, &hh); 
        x0 = GetControlInfo(S360.MT_Take[0], CINFO_X) - GetControlInfo(S360.MT_panel[0], CINFO_X);
        y0 = GetControlInfo(S360.MT_Take[0], CINFO_Y) - GetControlInfo(S360.MT_panel[0], CINFO_Y);
        CopyControlImage(S360.MT_panel[0], 0,          x0, y0, ww, hh,   // FromCId, FromState, x0, y0, w, h, 
                         CId_MT_Take, 0,                       0,  0);           // ToCId, ToState,     x1, y1
        if ((TapeWinded) && (nR)) {
            CopyControlImage(S360.MT_Take_reel_amount, nR,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             CId_MT_Take, 0,                       -3,-2);       // ToCId, ToState,     x1, y1
            mt_TapeMediumToReelDynamicState(surface, ww, 0, nR, col);
        }
    }
    if ((bRedrawData) && (MT_Reel_Amount)) {
        // - Copy Left and Right reel and its hub
        // nL = state for reel
        nL=MT_Data_Rot;
        switch(mtcab[unit].reel[1].color) {
            case 1: CId_Reel = S360.MT_red_reel; break; 
            case 2: CId_Reel = S360.MT_green_reel; break; 
           default: CId_Reel = S360.MT_blue_reel; break; 
        }
        // draw reel on top of medium
        if (0) { 
            // debug mode -> will display one reel state at the time, cycling from 0 up to max number of states
            // the current state for reel image show into the hub
            // press Z/X key to incr/decr state number
            char buf[10];
            static int current_reel_state = 0; 
            if (vid_keyb.LastKeyPress == 'z') {
                vid_keyb.LastKeyPress = 0; current_reel_state++;
            } else if (vid_keyb.LastKeyPress == 'x') {
                vid_keyb.LastKeyPress = 0; current_reel_state--;
            }
            if (current_reel_state >= GetControlInfo(CId_Reel, CINFO_NSTATES)) current_reel_state=0;
            else if (current_reel_state < 0) current_reel_state = GetControlInfo(CId_Reel, CINFO_NSTATES)-1; 
            nL=current_reel_state; 
            sprintf(buf, "%d", nL);
            DrawTextImage   (CId_Reel, nL, 140,140, 0, 0,     // ToCId, ToState, x0, y0, w0, h0: where to draw text
                               0,0,0,       // r, g, b: text color
                               buf, 1);       // str to draw, char size

        }
        CopyControlImage(CId_Reel, nL,     0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         CId_MT_Data, 0,         2, -2);  // ToCId, ToState,     x1, y1
        // overprint debug text in reel if any
        if (mtcab[unit].reel[0].debug_text[0]) {
            DrawTextImage   (CId_MT_Data, 0, 5,100, 0, 0,     // ToCId, ToState, x0, y0, w0, h0: where to draw text
                               255,255,255,       // r, g, b: text color
                               mtcab[unit].reel[0].debug_text, 2);       // str to draw, char size

        }
    }

    if (bRedrawGlassDoor) {
        // glass control overlaps both reels. Upper part of vaccols overlaps glass
        // So reels should be drawn again on glass, and vaccols should be invalidated to be redrawn by refresh routine
        cpanel_ControlRedrawNeeded=1; SetState(S360.MT_L_VacCol[unit], GetState(S360.MT_L_VacCol[unit])); 
        cpanel_ControlRedrawNeeded=1; SetState(S360.MT_R_VacCol[unit], GetState(S360.MT_R_VacCol[unit])); 
        // draw glass door background: upper part: reels background
        surface=GetControlSurface(S360.MT_GlassDoor[unit], 0, &ww, &hh); 
        x0 = GetControlInfo(S360.MT_GlassDoor[0], CINFO_X) - GetControlInfo(S360.MT_panel[0], CINFO_X);
        y0 = GetControlInfo(S360.MT_GlassDoor[0], CINFO_Y) - GetControlInfo(S360.MT_panel[0], CINFO_Y);
        CopyControlImage(S360.MT_panel[0], 0,          x0, y0, ww, 356,   // FromCId, FromState, x0, y0, w, h, 
                         S360.MT_GlassDoor[unit], 0,            0,   0);           // ToCId, ToState,     x1, y1
        // redraw reels as glass background will overprint them
        x0 = GetControlInfo(S360.MT_Take[0], CINFO_X) - GetControlInfo(S360.MT_GlassDoor[0], CINFO_X);
        y0 = GetControlInfo(S360.MT_Take[0], CINFO_Y) - GetControlInfo(S360.MT_GlassDoor[0], CINFO_Y);
        CopyControlImage(CId_MT_Take, 0,                0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                         S360.MT_GlassDoor[unit], 0,        x0, y0);           // ToCId, ToState,     x1, y1
        x0 = GetControlInfo(S360.MT[0], CINFO_X) - GetControlInfo(S360.MT_GlassDoor[0], CINFO_X);
        y0 = GetControlInfo(S360.MT[0], CINFO_Y) - GetControlInfo(S360.MT_GlassDoor[0], CINFO_Y);
        CopyControlImage(CId_MT_Data, 0,                0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                         S360.MT_GlassDoor[unit], 0,        x0, y0);           // ToCId, ToState,     x1, y1
        if (MT_head <= 1) {
            // Copy glass door closed
            CopyControlImage(S360.MT_GlassSlidingDoor, 0,         0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.MT_GlassDoor[unit],  0,         0, 356);       // ToCId, ToState,     x1, y1      
        } else {
            // draw glass door background: lower part: vaccol top
            CopyControlImage(S360.MT_GlassSlidingDoor, 1,         0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.MT_GlassDoor[unit],  0,         0, 356);       // ToCId, ToState,     x1, y1      
            // copy glass door sides at current position (given by MT_head)
            // MT_head == 16 -> glass door fully open -> h=full height
            // MT_head == 1  -> glass door closed -> h=380
            h= 380 + (MT_head) * (hh-380) / 14; 
            if (h>hh-45) h=hh-45; 
            y0=hh-h; 
            CopyControlImage(S360.MT_GlassSlidingDoor, 2,         0, y0, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                             S360.MT_GlassDoor[unit],  0,         0, 380);       // ToCId, ToState,     x1, y1      
            // draw blue frontal 
            CopyControlImage(S360.MT_GlassSlidingDoor, 0,       194, 0, 354, 90,  // FromCId, FromState, x0, y0, w, h, 
                             S360.MT_GlassDoor[unit],  0,       194, h-64);       // ToCId, ToState,     x1, y1      
            // draw glass door top edge
            y0=h-380;
            col = surface_rgb_color(55, 55, 55) ; // top border
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
            y0++;
            col = surface_rgb_color(230, 230, 230); 
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
            y0++;
            col = surface_rgb_color(128, 128, 128); 
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
            y0++;
            col = surface_rgb_color(64, 64, 64); 
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
            y0++;
            col = surface_rgb_color(32, 32, 32); 
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
            y0++;
            col = surface_rgb_color(16, 16, 16); 
            for (x0=0; x0<ww;x0++) surface[x0 + ww*y0]=col; 
        }
    }


    //  - Set dynamically generated image to be redraw 
    if (bRedrawData) {
       cpanel_ControlRedrawNeeded=1; 
       SetState(CId_MT_Data, 0);
    }
    if (bRedrawTake) {
       cpanel_ControlRedrawNeeded=1; 
       SetState(CId_MT_Take, 0);
    }
    if (bRedrawGlassDoor) {
       cpanel_ControlRedrawNeeded=1; 
       SetState(S360.MT_GlassDoor[unit], 0);
    }
}

// dynamic draw of L and R Reels, medium and head for given unit 
void mt_ReelsSet(int unit, int MT_head, int MT_Reel_Amount, int MT_L_Rot, int MT_R_Rot, 
                           int L_VacColMedium_h, int R_VacColMedium_h)
{
    if (bTapeIs==3420) {
        mt_ReelsSetDynamicState_3420(unit, MT_head, MT_Reel_Amount, MT_L_Rot, 
                                     (L_VacColMedium_h > 0) ? 0:1, (R_VacColMedium_h > 0) ? 0:1, 
                                     S360.MT[unit], S360.MT_Take[unit]);
    } else {
        mt_ReelsSetDynamicState(unit, MT_head, MT_Reel_Amount, MT_L_Rot, MT_R_Rot, 
                                (L_VacColMedium_h > 0) ? 0:1, (R_VacColMedium_h > 0) ? 0:1, 
                                S360.MT[unit]);
    }
}

void IBM360_SetTapesSelectLight(int mode)
{
    int unit, cmd, cmd_rw, ready; 

    for (unit=0; unit<8; unit++) {
        if (S360.MT[unit]>0) {
            // if tape unit visible
            cmd = mta_unit[unit].u3; 
            //    MT_CMDREW              0x10000       /* Rewind being done */
            if (cmd & 0x10000) {
                cmd = 0x07; // MT_REW              0x07       /* Rewind command */
                cmd_rw = 0;
            } else {
                cmd = cmd & 0x3f; 
                // set select light: tape unit selected by channel for data read or write
                //    case 0x1:              /* Write command */
                //    case 0x2:              /* Read command */
                //    case 0xc:              /* Read backward */
                cmd_rw = ((cmd==1) || (cmd==2) || (cmd==0xc)) ? 1:0;
            }
            if (mode==0) TickCount(S360.MT_LI_Select[unit], cmd_rw); 
            else SetStateWithIntensity(S360.MT_LI_Select[unit], cmd_rw); 
            // set ready light. 
            ready = cmd ? 0:1; // ready if no tape command execution
            if ((mta_unit[unit].flags & UNIT_DIS) ||                  // if unit disabled, 
                ((mta_unit[unit].flags & UNIT_ATT) == 0) ||           // no tape file attached, 
                (mtcab[unit].ResetPressed) ||                         // reset button has been pressed
                (mtcab[unit].mt_is==MT_is_loading_tape)) ready=0;     // or is loading in progress -> not ready
            if (mode == 0) TickCount(S360.MT_LI_Ready[unit], ready);
            else SetStateWithIntensity(S360.MT_LI_Ready[unit], ready);
        }
    }
}

// return 0 if tape is idle (not moving the medium), 1 if tape is doing read/write, -1 if rew
int get_mt_last_command(int unit)
{
    int cmd, current_cmd, cmd_0x3f, addr;

    cpanel_tape_cmd_info(&mta_unit[unit], &addr, &current_cmd); // get addr and current cmd in execution
    cmd = mt_info[unit].last_cmd; 
    cmd_0x3f = cmd & 0x3f; 
    if (current_cmd == 0) {
        mt_info[unit].last_cmd=0; // tape idle -> clear last_cmd as it is already copied to cmd
    } else if (current_cmd == 0x07) {
        // MT_REW  -> Rewind command in progress 
        extern uint32 TapeCmd_msec[8]; 
        if (TapeCmd_msec[unit]==0) return 0; // rew durantion zero -> no rew to do
        return -1;  
    }
    if ((cmd_0x3f == 0) || (cmd_0x3f == 0x04) || (cmd_0x3f == 0x03)) {
        // MT_SENSE            0x04       /* Sense command */
        // MT_MODE             0x03       /* Mode command */
        return 0; 
    } 
    return 1; // tape read/write/tape motion operation
}

void Refresh_MagTape(void)
{
    int unit, n;
    UNIT *uptr;

    int MT_Reel_Amount,  MT_L_Rot,  MT_R_Rot,  MT_head; // state of controls at end of of redraw
    int L_VacColMedium_h, R_VacColMedium_h; // annount of tape medium in vaccol
    int cmd, mt_is, addr;

    bTapeAnimInProgress=0;
    for(unit=0;unit<8;unit++) {
        if (S360.MT_panel[unit]<1) continue; // non visible tape unit
        uptr=&mta_unit[unit];
        cpanel_tape_cmd_info(uptr, &addr, &cmd); // get addr for tape unit
        if (uptr->flags & UNIT_DIS) {
            SetState(S360.MT_addr_digit[unit][1], 16); // no digits shown
            SetState(S360.MT_addr_digit[unit][2], 16); // no digits shown
            SetState(S360.MT_addr_digit[unit][3], 16); // no digits shown
        } else {
            // set unit addr
            SetState(S360.MT_addr_digit[unit][1], (addr >> 8) & 15); 
            SetState(S360.MT_addr_digit[unit][2], (addr >> 4) & 15); 
            SetState(S360.MT_addr_digit[unit][3], addr & 15); 
        }
        // check if unit disabled/no file attached
        if ((uptr->flags & UNIT_DIS) || ((uptr->flags & UNIT_ATT) == 0)) {
            if (((uptr->flags & UNIT_ATT) == 0) && (mt_info[unit].justdetached==1)) {  // 1 -> just detached -> should start unload animation
                if (mtcab[unit].mt_is == MT_is_unloading_tape) {
                    // unload amination in progress, continue
                } else {
                    // start unload amination
                    mtcab[unit].mt_is = MT_is_unloading_tape; 
                    mt_set_anim_seq(unit, 'U'); 
                }
            } else {
                if (mtcab[unit].state.MT_Head == 0) continue; 
                //draw on MT: two blue reels, head closed, no tape medium
                mtcab[unit].reel[0].color = 0; /* Left reel has been removed, a default blue has been set */
                mtcab[unit].state.MT_Head=-1; 
                mt_ReelsSet(unit, 0 /* MT_Head */, 0 /*  MT_Reel_Amount empty */, 
                                  mtcab[unit].state.MT_L_Rot, mtcab[unit].state.MT_R_Rot,1,1); 
                // empty vac cols
                mtcab[unit].L_VacColMedium_h0=-1;
                mtcab[unit].R_VacColMedium_h0=-1;
                mt_VacColSet(unit, 0, 0);
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
            int bDoorOpen; 

            bDoorOpen=mtcab[unit].state.MT_DoorOpen; // save door state
            memset (&mtcab[unit], 0, sizeof (mtcab[0]) ); // clear all
            mtcab[unit].state.MT_DoorOpen=bDoorOpen; // restore door state
            mtcab[unit].reel[0].color = 0; /* Left reel defaults to blue */
            mtcab[unit].reel[1].color = 0; /* Right reel defaults to bñue */
            cmode = ' ';

            MT_cab_opt[2] = '0' + unit;
            if ((IsOption(MT_cab_opt)) && (IsOptionParam) && (*IsOptionParam++ == '/')) {
                // syntax: option=mt1/WG <- tape 1 is set have reels color White(Left) and Gray (right). 
                //                          the third possible color is D (dark gray)                
                c = *IsOptionParam++; // get left reel color
                c = sim_toupper(c);
                mtcab[unit].reel[0].color = (c == 'R') ? 1 /* red reel */ : (c == 'G') ? 2 /* green reel */ : 0; /* else defaults to blue */
                c = *IsOptionParam++; 
                c = sim_toupper(c);
                mtcab[unit].reel[1].color = (c == 'R') ? 1 /* red reel */ : (c == 'G') ? 2 /* green reel */ : 0; /* else defaults to blue */
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
            mtcab[unit].state.MT_Reel_Ammount=-1; // init to -1 to force redraw of reels
            // ... signal the load animation can begin
            mtcab[unit].mt_is = MT_is_loading_tape; 
            if (cmode != 'R') {
                // normal load animation, *F or *B 
                 mt_set_anim_seq(unit, cmode);
            } else {
                // *R rewind animation 
                // position of medium on vaccol as it should be at end of write forward
                mtcab[unit].reel[0].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
                mtcab[unit].reel[1].VacCol_h = (PARAM_SensorsPerVacCol == 2) ? (int) (PARAM_VacCol_h_Hi  * 0.9):
                                                                               (int) (PARAM_VacCol_h_Low  * 0.9); 
                // put all medium on right reel
                mt_info[unit].MediumPos = mt_info[unit].MediumLen * 1000;
                // start hi speed rewind. This is nice!
                mt_info[unit].recsize = mt_info[unit].MediumLen * 1000;
                mt_set_rew_seq(unit, 1);
            }
        }
        // tape is about to be painted on cpanel
        // MT if the dynamic ocntrol that get image from generic control as
        // MT_reel_amount sets the quantity of tape medium into the reel
        //    =0  -> no tape medium winded on reel, reel empt
        //    =1 -> reel just start entering the reel 
        //    =67 -> reel full of tape medium 
        //    =1 -> all tape on Left reel, 99 -> all tape on Right reel
        // MT_blue_reel, MT_red_reel, MT_green_reel holds the rotational position of reel, the speed of reel, 
        // and the reel's colour
        //          0 ..35 (36 states) -> reel rotated 0gr, 10gr, ... 350gr. To be used when reel is not spinning
        //          36..60 (24 states) -> reel rotated 0gr, 15gr ... 345gr but with some spin blur. To be used when reel is moving
        //          60..67 ( 8 states) -> reel rotated 0gr, 45gr ... 315gr whith greater spin blur. To be used when reel is rewinding at high Speed
        // MT_head holds the position of r/w head
        //    =0  -> no head image
        //    =1  -> head closed, prepared to read or write tape medium
        //    =16 -> open head, prepared to manualy remove tape medium 

        // what tape is doing now?
        cmd = get_mt_last_command(unit); // last tape cmd executed: -1=rew, 0=idle, 1=read/write
        mt_is =  mtcab[unit].mt_is;         // the current animation being done (visual state of tape)

        // check if load/unload/rew animation in progress should be aborted
        if (mt_is == MT_is_loading_tape) {
            if (mt_info[unit].numrw > 0) {
                // if any mt r/w command issued by cpu then abort any load animation in progress
                mtcab[unit].mt_is = mt_is = 0;
                // position of medium on vaccol as it should be at end of load
                mtcab[unit].reel[0].VacCol_h = (PARAM_SensorsPerVacCol == 2) ? (int) (PARAM_VacCol_h_Hi  * 0.9):
                                                                               (int) (PARAM_VacCol_h_Low * 0.9); 
                mtcab[unit].reel[1].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
            }
        } else if (mt_is == MT_is_unloading_tape) {
            // no reason to abort unload anim. if tape attached, mt_is will be set to MT_is_loading
        } else if (mt_is == MT_is_rewinding) {
            if (cmd != -1) {
                // last command is not rew -> any mt command in progress terminates any rew animation in progress
                mtcab[unit].mt_is = mt_is = 0;
                // position of medium on vaccol as it should be at end of load
                mtcab[unit].reel[0].VacCol_h = (PARAM_SensorsPerVacCol == 2) ? (int) (PARAM_VacCol_h_Hi  * 0.9):
                                                                               (int) (PARAM_VacCol_h_Low * 0.9); 
                mtcab[unit].reel[1].VacCol_h = (int) (PARAM_VacCol_h_Low * 1.1); 
            }
        }
        
        // check if should start rew animation
        if ((mt_is == 0) && (cmd < 0)) {
               // if (no animation in progress) and (last tape cmd is rew) 
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
                    // if rew terminates
                    // set time to zero so mt_svr will terminate the command
                    extern uint32 TapeCmd_msec[8];           // duration of device operation
                    TapeCmd_msec[unit]=0; 
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
            mt_reels_mov(unit, cmd, 
                         &L_VacColMedium_h, &R_VacColMedium_h, 
                         &MT_L_Rot, &MT_R_Rot, 
                         &MT_Reel_Amount, &MT_head, 1);
        }

        // set dynamic state for reels 
        mt_ReelsSet(unit, MT_head, MT_Reel_Amount, MT_L_Rot, MT_R_Rot, L_VacColMedium_h, R_VacColMedium_h); 
        // set dynamic state for vacCol controls 
        mt_VacColSet(unit, L_VacColMedium_h, R_VacColMedium_h);
        // set tape lights 
        SetState(S360.MT_LI_File_protect[unit], sim_tape_wrp(uptr) ? 1 : 0);
    }
    IBM360_SetTapesSelectLight(1);
}


int PARAM_LPT_char_ww2   =    13;  // Printer horizontal spacing between first pixel of a char and first pixel of next char in next char same line
int PARAM_char_hh2       =    23;  // height between top line of one char and top line of char in next text line
int PARAM_xPaperMargin   =    52;  // left and right margin on printed paper
int PARAM_ink0           =   255;  // ammount of ink on top of char (0..255, 0=no ink, 255=full black)
int PARAM_ink1           =   150;  // ammount of ink on bottom top of char 

// draw paper background on paper from y0 to y1-1
void lpt_set_paper_background(int paper_y0, int paper_y1, 
                              int CId_Paper, int CId_PaperBackground, int hPaperBackgroundOffset)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int x,y,bg_x,bg_y;
    
    surface0=GetControlSurface(CId_Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(CId_PaperBackground, 0, &bg_ww, &bg_hh); // paper backgtound should have same width that paper control

    // pad background on paper, taking into account offset
    for (y=paper_y0; y<paper_y1; y++) {
        bg_y = (y + hPaperBackgroundOffset) % bg_hh; 
        for (x=0; x<paper_ww; x++) {
            bg_x = x % bg_ww;
            surface0[x + y * paper_ww] = surface1[bg_x + bg_y * bg_ww];
        }
    }
}

void lpt_print_line(int y0, char * sLin, 
                    int CId_Paper, int CId_PrinterCharSet, int char_ww2)
{
    static char sCharSet[128] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ,.()=*+-/$@&?!'\";:%\\|#[]<>_abcdefghijklmnopqrstuvwxyz";
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // character to be printed on paper
    int paper_ww, paper_hh; // printed paper control size
    int char_ww, char_hh;   // character to be printed size
    int x,y, x0, nChar, i,ic, n, ink, p, r, g, b, ink0, ink1;
    char c; 

    surface0=GetControlSurface(CId_Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(CId_PrinterCharSet, 0, &char_ww, &char_hh); 

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
        x0 += char_ww2; // char_ww2 = horizontal pixels of horizontal spacing between chars (top left corner to next char top left corner)
        if (x0 >= paper_ww - PARAM_xPaperMargin) break; // check if outside paper margins
    }
}

// print on CId_Paper dynamic state 0, with CId_PaperBackground paper background and CId_PrinterCharSet font
// lptPrintOutDoneCount holds lines already printed (do not modify, just init once to -1 to force redraw of all paper background)
// lptPrintOutCount holds count of total lines printed in lptPrintOut array
// lptPrintOut is a circular buffer that holds lptPrintOutMax lines, ecah one with lptColumns columns
// char_mode: if =0 -> print lines at once. =1 print char by char
// char_ww2: horizontal pixels of horizontal spacing between chars (top left corner to next char top left corner)
void lpt_print_page(int CId_Paper, int CId_PaperBackground, int CId_PrinterCharSet, 
                    int * lptPrintOutDoneCount, int * lptPrintOutCount, int * hPaperBackgroundOffset, 
                    char * lptPrintOut, int lptPrintOutMax, int lptColumns, int char_mode, int char_ww2)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int y, nLin, nLinMax, iLin, n;
    char sLin[LPT_COLUMNS+1];

    surface0=GetControlSurface(CId_Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(CId_PaperBackground, 0, &bg_ww, &bg_hh); // paper background should have same width that paper control

    nLin = *lptPrintOutCount - *lptPrintOutDoneCount, // nLin = number of char lines to print
    nLinMax = paper_hh  / PARAM_char_hh2;           // calc how much char lines fits in printed page visible

    if (*lptPrintOutDoneCount < 0) {
        // first page setup. copy background image
        *hPaperBackgroundOffset = 6; 
        lpt_set_paper_background(0, nLinMax * PARAM_char_hh2, 
                                 CId_Paper, CId_PaperBackground, *hPaperBackgroundOffset);
        *lptPrintOutDoneCount=0;
    }
    if (nLin<0) {
        // nLin < 0 when lptPrintOutCount has been set to zero -> new printing 
        *hPaperBackgroundOffset = 6; // hOffset is vertical alignement of line into paper (in vertical pixels)
    } else {
        *hPaperBackgroundOffset = (*hPaperBackgroundOffset + nLin * PARAM_char_hh2) % bg_hh;
        if (*hPaperBackgroundOffset<0) (*hPaperBackgroundOffset) += bg_hh; 
    }

    // calc nLin = lines to redraw (starting from bottom=last line printed)
    if ((nLin >= nLinMax) || (nLin < 0)) {
        nLin = nLinMax; 
    } else if (nLin==0) {
        // do not scroll, no additional lines printed
        if (char_mode) nLin=1;   // char mode: set nLin to 1 to redraw only last line 
        else nLin = nLinMax;     // line mode: redraw all
    } else {
        // scroll up paper image
        y = nLin * PARAM_char_hh2; // first image line of char line that goes to the top of screen
        if (char_mode) {
            // if charmode, do not scroll up last line (will be redrawn allways)
            memcpy(&surface0[0], 
               &surface0[y * paper_ww], 
               sizeof(*surface0) * paper_ww 
                                 * (paper_hh - y - PARAM_char_hh2)); 
            nLin++; // char mode: redraw also line above last, just in case it go extra chars 
            if (nLin >= nLinMax) nLin = nLinMax;
        } else {
            memcpy(&surface0[0], 
               &surface0[y * paper_ww], 
               sizeof(*surface0) * paper_ww * (paper_hh - y)); 
        }
    }

    if (*lptPrintOutCount <= 0) {
        iLin = 0;
    } else {
        iLin = (*lptPrintOutCount - 1) % lptPrintOutMax; // last line printed, the one at bottom of paper
    }
    // draw nLin lines, starting at bottom of paper and goindg upwards
    // chars to print comes from lptPrintOut[iLin]
    n = nLinMax-1;
    while (nLin-- > 0) {
        memcpy(sLin, &lptPrintOut[iLin * lptColumns], lptColumns); sLin[lptColumns] = 0;
        iLin = (iLin-1) % lptPrintOutMax;
        if (iLin<0) iLin += lptPrintOutMax; 
        lpt_set_paper_background(n*PARAM_char_hh2, (n+1)*PARAM_char_hh2,
                                 CId_Paper, CId_PaperBackground, *hPaperBackgroundOffset);
        lpt_print_line(n*PARAM_char_hh2, sLin, CId_Paper, CId_PrinterCharSet, char_ww2);
        n--;
    }

    *lptPrintOutDoneCount=*lptPrintOutCount;
    cpanel_ControlRedrawNeeded = 1;
    SetState(CId_Paper, 0);
}

void Refresh_PrintOut(void)
{
    SetState(S360.LI_Print_Ready, ((lpr_unit[0].flags & UNIT_DIS) || ((lpr_unit[0].flags & UNIT_ATT) == 0)) ? 0:1); // Ready light on -> device ready to process i/o commands

    if (lptPrintOutCount == lptPrintOutDoneCount)  return; // nothing to update

    lpt_print_page(S360.Paper, S360.PaperBackground, S360.PrinterCharSet, // printer controls
                   &lptPrintOutDoneCount, &lptPrintOutCount, &hPaperBackgroundOffset,    // current state
                   lptPrintOut, lptPrintOutMAX, LPT_COLUMNS, 0, PARAM_LPT_char_ww2);     // array to print

}

int PARAM_GolfBall_char_ww2       = 11;  // Console horizontal spacing between first pixel of a char and first pixel of next char in next char same line
int PARAM_GolfBall_LeftOffset     = -40;     // Offset to set the printing head at first leftmost col in printout paper
int PARAM_GolfBall_PrintChar_msec = 168;     // Console printer goes at 14.8 chars per sec -> 67.5 msec per char. Set to 168 to allow time to see the golf ball anim
int PARAM_GolfBall_CR_msec        = 473;     // Carriage return takes 7 character time from last rightmost column

void Refresh_PrinterConsole(void)
{
    int x, nCol, nState, n;
    uint32 msec; 

    SetState(S360.LI_Cons_Proceed, bProceed); // console proceed light 
    if (bConsTypeIs==3210) {
        SetState(S360.LI_Cons_Req_Pending, bRequestPending); // console request pending light 
    }

    // if ConsPrintOutDoneCount==-1, it is just the first refresh of a just created windoe
    // so set ConsSomethingPrinted=1 to guarantee the controls are refreshed
    if (ConsPrintOutDoneCount==-1) ConsSomethingPrinted=1;
    if (ConsGolfBallAction) ConsSomethingPrinted=1; // if will animate golf ball, then redraw printout paper

    // generate dynamic state for console printer paper
    // on cmd.exe console, there is room for 78 columns (80 minus "I " prompt)
    //    ibm 1052 console cpanel window there is room for 95 columns
    //    on memory: room for CONS_LPT_COLUMNS = 100 columns
    //    real ibm 1052 console: 126 columns
    if (ConsSomethingPrinted) {
        lpt_print_page(S360.ConsPaper, S360.ConsPaperBackground, S360.ConsPrinterCharSet, // printer controls
                   &ConsPrintOutDoneCount, &ConsPrintOutCount, &hConsPaperBackgroundOffset,              // current state
                   ConsPrintOut, ConsPrintOutMAX, CONS_LPT_COLUMNS, 1, PARAM_GolfBall_char_ww2);         // array to print
    }

    // animate golf ball printing head
    if (ConsGolfBallAction==4) {
        // Golf ball printing head is stoped at colum ConsCharsOutCount
      Set_GolfBall_PrinterHead:
        nState=0; // Golf ball stoped
        nCol = ConsCharsOutCount;
        ConsGolfBallAction=0;
      Draw_GolfBall_PrinterHead:
        // copy golf ball image to last line of paper at column ncol with state nState
        x = PARAM_GolfBall_LeftOffset + nCol * PARAM_GolfBall_char_ww2;
        //  - Copy State 0 from control GolfBall to state 0 of Paper (state displayed) 
        if (nState==3) n=6; else if (nState==2) n=12; else n=0;
        CopyControlImage(S360.ConsGolfBall, nState,   0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.ConsPaper, 0,           x, 215+n);     // ToCId, ToState,     x1, y1
        cpanel_ControlRedrawNeeded = 1;
        SetState(S360.ConsPaper, 0);
    } else if (ConsGolfBallAction==3) {
        // Golf ball printing head is being positioned at colum ConsCharsOutCount
        nState=1; // Golf ball moving, horizontal blur
        nCol = ConsCharsOutCount;
        ConsGolfBallAction=4; // next refresh, golf ball will be stoped
        goto Draw_GolfBall_PrinterHead;
    } else if (ConsGolfBallAction==2) {
        // Golf ball printing head is printing a char at column ConsGolfBallPosition
        msec=sim_os_msec() - tm0ConsGolfBallAction; 
        if (msec > 1000) msec=1000; // safety
        if (msec >= (uint32) PARAM_GolfBall_PrintChar_msec) goto Set_GolfBall_PrinterHead; // Printing finished, set head at its definitive position            
        nState = 2 + 5 * msec / PARAM_GolfBall_PrintChar_msec; // select state 2..6 
        nCol = ConsGolfBallPosition;
        goto Draw_GolfBall_PrinterHead;
    } else if (ConsGolfBallAction==1) {
        // Golf ball printing head is executing a carriage return from column ConsGolfBallPosition
        msec=sim_os_msec() - tm0ConsGolfBallAction; 
        if (msec > 1000) msec=1000; // safety
        if (msec >= (uint32) PARAM_GolfBall_CR_msec) goto Set_GolfBall_PrinterHead; // Printing finished, set head at its definitive position            
        nState=1; //  moving horizontal direction
        n = 80 * msec / PARAM_GolfBall_CR_msec; // cols advanced
        nCol = ConsGolfBallPosition-n; 
        if (nCol > 5) goto Draw_GolfBall_PrinterHead;
        goto Set_GolfBall_PrinterHead;        
    }
    ConsSomethingPrinted=0;
}


void IBM360_SetDasdReadyLights(int mode)
{
    extern UNIT dda_unit[]; 
    extern void cpanel_dasd_cmd_info(UNIT * uptr, int * addr, int * cmd, int * cyl, int * hd, int * trk);
    UNIT * uptr; 
    int disk_pack, dummy, cmd_seek, addr; 

    for (disk_pack=0; disk_pack<8; disk_pack++) {
        uptr=dasdcab[disk_pack].uptr; 
        if ((uptr==NULL) || (uptr->flags & UNIT_ATT) == 0) continue; 
        switch (uptr->CMD & 0x7f) {
            case 0x07: /* SK_SEEK */ case 0x0B: /* DK_SEEKCYL */ case 0x1B: /* DK_SEEKHD */
            case 0x39: /* DK_SRCH_HAEQ */ case 0x31: /* DK_SRCH_IDEQ */ case 0x51: /* DK_SRCH_IDGT */ case 0x71: /* DK_SRCH_IDGE */ 
            case 0x29: /* DK_SRCH_KYEQ */ case 0x49: /* DK_SRCH_KYGT */ case 0x69: /* DK_SRCH_KYGE */ 
                    cmd_seek=1; break; 
            default: cmd_seek=0; 
        }            
        // ready light is off on seek commands
        // cpanels assumes search dasd commands are also seek. This may not be realistic, but the manual
        // si not clear about what exactly is "ready light is off on seek commands"
        if (mode==0) TickCount(S360.DASD_LI_Ready[disk_pack], (cmd_seek==1) ? 0:1); 
        else SetStateWithIntensity(S360.DASD_LI_Ready[disk_pack], (cmd_seek==1) ? 0:1); 
    }
    // set disk pack addr
    if (mode==1) for (disk_pack=0; disk_pack<8; disk_pack++) {
        uptr=dasdcab[disk_pack].uptr; 
        if ((uptr==NULL) || (uptr->flags & UNIT_ATT) == 0) {
            // no file attached (no disk pack) -> ready light off, Start/Stop switch in stop position, no address
            SetState(S360.DASD_LI_Ready[disk_pack], 0); 
            SetState(S360.DASD_SW_StartStop[disk_pack], 0); 
        } else {
            // LI_Ready already set as TickCount
            SetState(S360.DASD_SW_StartStop[disk_pack], 1); // Start/Stop switch in stop position
        }
        if (uptr==NULL) {
            SetState(S360.DASD_addr_digit[disk_pack][1], 16); // no digits shown
            SetState(S360.DASD_addr_digit[disk_pack][2], 16); // no digits shown
            SetState(S360.DASD_addr_digit[disk_pack][3], 16); // no digits shown
        } else {
            // get unit addr
            cpanel_dasd_cmd_info(uptr, &addr, &dummy, &dummy, &dummy, &dummy);
            SetState(S360.DASD_addr_digit[disk_pack][1], (addr >> 8) & 15); 
            SetState(S360.DASD_addr_digit[disk_pack][2], (addr >> 4) & 15); 
            SetState(S360.DASD_addr_digit[disk_pack][3], addr & 15); 
        }
    }
}

void Refresh_Dasd(void)
{
    IBM360_SetDasdReadyLights(1); 
}

// handle RTC timer using TickCount 
void SetRtcTimer()
{
    static uint32 rtc_activate_msec = 0; 
    static int    catchup_usec = 0; 
    static int    catchup_Instr = 0;
    uint32 n; 

    // this function is called at TickCount rate -> 30K per second -> each 33.33 usec
    // but if set cpu realtime is active, time will be yied to host with sim_os_ms_sleep()
    // so the frequence of calls to ticckount will decrease
    // this is not a problem as rtc_svr will be called based on corrent msec value change
    // if more than one msec has elapsed, the lost rtc_svr calls will be catch-up 

    // lets calc rtc_activate_usecs = usecs remaining to activate rtc_srv
    rtc_activate_usecs = (rtc_activate_usecs & 0xffff) - 33; 
    if (rtc_activate_usecs > 10000) rtc_activate_usecs = 3333; // reset counter

    // clac n= number of msec elapsed
    n=Refresh_tnow - rtc_activate_msec; 
    rtc_activate_msec=Refresh_tnow;

    if (n==0) {
        // nothing new to catch-up
    } else if (n < 1000) {
        // allow to catchup up to 1000 msec 
        catchup_usec += n * 1000; 
        if (catchup_usec > 10 * 1000 * 1000) catchup_usec=0; // too much to catchup. Forget it
    } else { 
        // too much to catchup. Forget it
        catchup_usec=0;
    }
    // call rtc_svr to catchup (each 1000 instr cycles)
    if ((catchup_usec > 0) && ((uint32)(InstrCycles - catchup_Instr) > 1000)) {
        catchup_usec -= 3333; 
        rtc_activate_usecs = 3333; 
        // schedulle catchup rtc_svr call
        sim_cancel(&cpu_unit[0]);
        sim_activate(&cpu_unit[0], 1);
        catchup_Instr=InstrCycles; 
    }
    // notify that a catchup is in progress, so cpu cannot sleep
    if (catchup_usec > 0) rtc_activate_usecs |= 0x10000;
}


void IBM360_TickIntensityCount(void)
{
    if (bCpuModelIs == 3148) {
        SetCpuPanelLights_M148(0); // set tickcount
    } else if (bCpuModelIs == 3145) {
        SetCpuPanelLights_M145(0); // set tickcount
    } else if (bCpuModelIs == 2065) {
        SetCpuPanelLights_M65(0); // set tickcount
    } else if (bCpuModelIs == 2050) {
        SetCpuPanelLights_M50(0); // set tickcount
    } else if (bCpuModelIs == 2040) {
        SetCpuPanelLights_M40(0); // set tickcount
    } else {
        SetCpuPanelLights_M30(0); // set tickcount
    }
    if (bDasdVisible) {
        IBM360_SetDasdReadyLights(0); 
    }
    if (bTapesVisible) {
        IBM360_SetTapesSelectLight(0); 
    }
    SetRtcTimer(); 
}

void IBM360_SetOdoMeter(void)
{
    int n, msec; 
    int CId, y0; 

    if (OdoMeter.tnow ==0) {
       OdoMeter.tnow = Refresh_tnow;                                   
       goto drawOdo;
    }
    msec = Refresh_tnow - OdoMeter.tnow; if ((msec<0) || (msec > 1000)) msec=1000;
    OdoMeter.tnow = Refresh_tnow;                                   
    if (sim_is_running == 0) return; // if not running, do not update meter
    OdoMeter.msec += msec;
    if (OdoMeter.msec < 1000) return; // not yet a second measured
    OdoMeter.msec -= 1000;
    // a second is measured and accumulated
    OdoMeter.sec += 1;
    // on real hw, odometer is updated only when cpu is executing intructions. When cpu is
    // waiting the odometer is not incremented
    // cpanel update odometer when cpu is executing intructions, AND also when cpu is waiting
    // this allows odometer to measure the time a spript needs to run  
    if ((OdoMeter.sec > 3600 - 18) && (OdoMeter.hour_changed_flag==0)) {
        // an hour is measured and accumulated
        OdoMeter.hour_changed_flag=1; 
        OdoMeter.hours++;
        if (OdoMeter.hours > 999) OdoMeter.hours=0;
    }
    if (OdoMeter.sec > 3600) {
        OdoMeter.sec -= 3600;
        OdoMeter.hour_changed_flag=0; 
    }
drawOdo:

    // set the odometer for black digits (hours)
    SetState(S360.Odo[2], OdoMeter.hours / 100);
    SetState(S360.Odo[3], (OdoMeter.hours / 10) % 10);
    SetState(S360.Odo[4], OdoMeter.hours % 10);
    // set the odometer for white digits (fraction of hour)

    n = OdoMeter.sec; 
    n = n / 36; // fraction of hour 0..99

    // Dynamically generate State 0 for Odo5 and Odo6 controls
    CId = S360.Odo[5]; 
    y0 = 3 + 16 * (n / 10); 
    if (n % 10 == 9)  {
        y0 += ((OdoMeter.sec % 36) * 16) / 36; 
    }
    //  - Copy State 0 from control OdoWhiteDigit to state 0 of CId (state displayed) 
    CopyControlImage(S360.OdoDigitWhite, 0,   0, y0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId, 0,                       0, 0);       // ToCId, ToState,     x1, y1
    // apply alpha mask to generated state
    CopyControlImage(CId, 3,                       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId, 0,                       0, 0);       // ToCId, ToState,     x1, y1
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId, 0);

    // for last digit, make continuous movement
    CId = S360.Odo[6]; 
    y0 = 3 + 16 * (n % 10) + ((OdoMeter.sec % 36) * 16) / 36; 
    //  - Copy State 0 from control OdoWhiteDigit to state 0 of CId (state displayed) 
    CopyControlImage(S360.OdoDigitWhite, 0,   0, y0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId, 0,                       0, 0);       // ToCId, ToState,     x1, y1
    // apply alpha mask to generated state
    CopyControlImage(CId, 3,                       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                     CId, 0,                       0, 0);       // ToCId, ToState,     x1, y1
    //  - set dynamically generated image to be redraw 
    //  - set dynamically generated image to be redraw 
    cpanel_ControlRedrawNeeded=1; 
    SetState(CId, 0);
}


int PARAM_nCardInReadHopperMax   = 1500; // Should be 3100, not 1500. Not so realistic, but allow nicer looking
int PARAM_nCardInStackerMax      = 700;  // Should be 1350, not 700. Not so realistic, but allow nicer looking
int PARAM_CardDropTime_msec      = 200;  // educated guess. Time in msec needed for a card to drop on stacker

void Refresh_CardReadPunch_2540(void)
{
    int CId, nState, msec;
    int x1, y1, x0, y0, x, y, h, n, nCards, ncp; 

    SetState(S360.LI_Ready_CardReader, ((cdr_unit[0].flags & UNIT_DIS) || ((cdr_unit[0].flags & UNIT_ATT) == 0)) ? 0:1); // Ready light on -> device ready to process i/o commands
    SetState(S360.LI_EOF_CardReader, bCardReadEOFset ? 1:0); 
    SetState(S360.LI_Check_CardReader, (cdr_unit[0].CMD & CDR_ERR) ? 1:0);
    SetState(S360.LI_Ready_CardPunch, ((cdp_unit[0].flags & UNIT_DIS) || ((cdp_unit[0].flags & UNIT_ATT) == 0)) ? 0:1); // Ready light on -> device ready to process i/o commands

    // from bitsavers A21-9033-1_2540_Card_Punch_Component_Description_1965.pdf
    // The 2540 has separate read and punch feeds, with maximum reading speed of 1000 cards per minute 
    // and maximum punching speed of 300 cards per minute. The punch hopper holds approximately 1350 
    // cards; the read hopper and file-feed magazine hold approximately 3100 cards. 

    // this means a card read takes 60 msec, a card punch takes 200 msec

    // dynamic draw of inclinated input card deck

    // if this is the first time card reader refresh, then get the background 
    // image behind the card reader and set it as state 0 on ReadHopper control
    if (bCardReadHopperBackgroundSet==0) { 
        // this is the first refresh of card reader. return to terminate the refresh and be able to get the backgound
        bCardReadHopperBackgroundSet=1; 
        return; 
    } else if (bCardReadHopperBackgroundSet==1) { 
        // first refresh already done. Now we can grab the background behing read hopprer guide to state 0
        x=GetControlInfo(S360.ReadHopper, CINFO_X);
        y=GetControlInfo(S360.ReadHopper, CINFO_Y);
        ncp=GetControlInfo(S360.ReadHopper, CINFO_NCP);
        CopyControlImage(0, ncp,                      x, y, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.ReadHopper, 0,           0, 0); // ToCId, ToState,     x1, y1
        bCardReadHopperBackgroundSet=2; 
    }

    // draw input column of n cards for cdr[0]
    nCards = sim_card_input_hopper_count(&cdr_unit[0]); 
    // break big deck in clunches
    if (nCards > PARAM_nCardInReadHopperMax) {
        nCards = nCards % (PARAM_nCardInReadHopperMax / 3) + (PARAM_nCardInReadHopperMax / 2) ;
    }

    if (nCardsInReadHopper == nCards) {
        // input deck is already draw with needed number of cards. No need to generate a new dynamic state image
    } else { 
        //  - Copy State 0 (background) to state 1 (state displayed) to start with an empty image
        CopyControlImage(S360.ReadHopper, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.ReadHopper, 1,       0, 0);       // ToCId, ToState,     x1, y1

        if (nCards<=0) {
            // no cards to draw
            nCards=0;
        } else {
            if (nCards > PARAM_nCardInReadHopperMax) nCards=PARAM_nCardInReadHopperMax; // safety
            // just one card visible (input hopper near empty) -> n=1095; 
            // input hopper magazine at its maximum capacity (3100 cards) -> n=0;
            n = (PARAM_nCardInReadHopperMax-nCards) * 1095 / PARAM_nCardInReadHopperMax; 
            // draw input deck into magazine
            x0=15; y0=47; 
            x1 = x0 + 165; y1 = y0 + 399; // 165,399=last position inclinated deck visible
            x = x0 + (x1 - x0) * n / 1000; 
            y = y0 + (y1 - y0) * n / 1000; 
             
            CopyControlImage(S360.InputDeckBody, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadHopper, 1,       x, y);       // ToCId, ToState,     x1, y1

            // draw top of column taking into account perspective of last card
            n = (PARAM_nCardInReadHopperMax-nCards) * 6 / PARAM_nCardInReadHopperMax; if (n>5) n=5; 
            x = x - 11; y = y-39; 
            CopyControlImage(S360.InputDeckTop, n,     0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadHopper, 1,       x, y);       // ToCId, ToState,     x1, y1
        }
        // to force redraw of control 
        cpanel_ControlRedrawNeeded=1; 
        SetState(S360.ReadHopper, 1);
        nCardsInReadHopper = nCards;
    }

    // animation sequence: card going out to already-read-card-deck on rightmost read stacker 
    // duration of card animation: 200 msec this value is a guess; used because it looks good

    // dynamic draw of read cards take stacker (rightmost one)
    nCards = sim_card_output_hopper_count(&cdr_unit[0]) - 1; // minus one because last card still into the read station, not yet out

    if (nCards <0) nCards=0;
    // break big deck in clunches
    if (nCards > PARAM_nCardInStackerMax) {
        nCards = nCards % PARAM_nCardInStackerMax;
    }

    if ((nCardsInReadStacker == nCards) && (tm0CardInReadStacker==0)) {
        // deck is already draw with needed number of cards. 
        // and no card animation in progress
        // so no need to generate a new dynamic state image
    } else { 
        // draw animation of card falling into stacker
        //  - Copy State 0 (background) to state 1 (state displayed) to start with an empty image
        CopyControlImage(S360.ReadStacker, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.ReadStacker, 1,       0, 0);       // ToCId, ToState,     x1, y1

        if (tm0CardInReadStacker) {
            // card dropping to stacker animation in progress
            CId=nState=0;
            msec = Refresh_tnow - tm0CardInReadStacker; 
            if ((msec < 0) || (msec >= PARAM_CardDropTime_msec)) {
               // animation ended
               tm0CardInReadStacker=0;
            } else if (nCards==1) {
                // drop first card
                CId    = S360.ReadStackerBack1; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else if (nCards - nCardsInReadStacker <= 1) {
                // drop one more card
                CId    = S360.ReadStackerBack2; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else {
                // drop multiple cards at fast pace
                CId    = S360.ReadStackerBack2; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
                // change states 0,1,2,3 to 0,4,1,5
                { static int st[4] = {0,4,1,5};  nState = st[nState]; }
            }
            // draw card dropping to stacker
            if (CId) {
                CopyControlImage(CId, nState,                    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.ReadStacker, 1,       0, 0);       // ToCId, ToState,     x1, y1

            }
        }
        // now, draw the deck of processed cards sitting into the stacker
        if (nCards<=0) {
            // no cards to draw, empty stacker
            nCards=0;
        } else {
            if (nCards > PARAM_nCardInStackerMax) nCards=PARAM_nCardInStackerMax; // safety
            // stacker at its maximum capacity -> n=1000;
            n = nCards * 1000 / PARAM_nCardInStackerMax; 
            // draw output deck into rightmost output stacker

            // draw output deck into stacker (top part)
            y0=30; h = 8 + 100 * n / 1000; 
            CopyControlImage(S360.ReadStackerSeparator, 1,    0, y0, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadStacker, 1,             0, y0);        // ToCId, ToState,     x1, y1

            // draw output deck into stacker (left side)
            y0=30; h = 120 + 100 * n / 1000; 
            CopyControlImage(S360.ReadStackerSeparator, 1,    0, y0, 30, h,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadStacker, 1,             0, y0);        // ToCId, ToState,     x1, y1

            // draw last card of deck (topmost one)
            x0=-9; y0=29; 
            x1 = x0 + 23; y1 = y0 + 100; // position tomost card visible when stacker is full
            x = x0 + (x1 - x0) * n / 1000; 
            y = y0 + (y1 - y0) * n / 1000; 
            nState= n * 9 / 1000; if (nState > 8) nState=8;            
            CopyControlImage(S360.CardInReadStacker, nState,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadStacker, 1,       x, y);       // ToCId, ToState,     x1, y1

            // draw stacker separator over output deck 
            CopyControlImage(S360.ReadStackerSeparator, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.ReadStacker, 1,             0, 0);       // ToCId, ToState,     x1, y1

        }

        // to force redraw of control 
        cpanel_ControlRedrawNeeded=1; 
        SetState(S360.ReadStacker, 1);
        nCardsInReadStacker = nCards;
    }

    // dynamic draw of punched cards take stacker (leftmost one)
    nCards = sim_card_output_hopper_count(&cdp_unit[0]); 

    // break big deck in clunches
    if (nCards > PARAM_nCardInStackerMax) {
        nCards = nCards % PARAM_nCardInStackerMax;
    }

    if ((nCardsInPunchStacker == nCards) && (tm0CardInPunchStacker==0)) {
        // deck is already draw with needed number of cards. 
        // and no card animation in progress
        // so no need to generate a new dynamic state image
    } else { 
        // draw animation of card falling into stacker
        //  - Copy State 0 (background) to state 1 (state displayed) to start with an empty image
        CopyControlImage(S360.PunchStacker, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.PunchStacker, 1,       0, 0);       // ToCId, ToState,     x1, y1

        if (tm0CardInPunchStacker) {
            // card dropping to stacker animation in progress
            CId=nState=0;
            msec = Refresh_tnow - tm0CardInPunchStacker; 
            if ((msec < 0) || (msec >= PARAM_CardDropTime_msec)) {
               // animation ended
               tm0CardInPunchStacker=0;
            } else if (nCards==1) {
                // drop first card
                CId    = S360.PunchStackerBack1; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else if (nCards - nCardsInPunchStacker <= 1) {
                // drop one more card
                CId    = S360.PunchStackerBack2; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else {
                // drop multiple cards at fast pace
                CId    = S360.PunchStackerBack2; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
                // change states 0,1,2,3 to 0,4,1,5
                { static int st[4] = {0,4,1,5};  nState = st[nState]; }
            }
            // draw card dropping to stacker
            if (CId) { 
                CopyControlImage(CId, nState,                0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.PunchStacker, 1,      50, 0);       // ToCId, ToState,     x1, y1

            }
        }

        // now, draw the deck of processed cards sitting into the stacker
        if (nCards<=0) {
            // no cards to draw, empty stacker
            nCards=0;
        } else {
            if (nCards > PARAM_nCardInStackerMax) nCards=PARAM_nCardInStackerMax; // safety
            // stacker at its maximum capacity -> n=1000;
            n = nCards * 1000 / PARAM_nCardInStackerMax; 
            // draw output deck into rightmost output stacker

            // draw output deck into stacker (top part)
            y0=30; h = 8 + 100 * n / 1000; 
            CopyControlImage(S360.PunchStackerSeparator, 1,    0, y0, 0, h,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,             0, y0);        // ToCId, ToState,     x1, y1

            // draw output deck into stacker (right side)
            y0=30; h = 120 + 100 * n / 1000; 
            CopyControlImage(S360.PunchStackerSeparator, 1,  100, y0,  0, h,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,           100, y0);        // ToCId, ToState,     x1, y1

            // draw last card of deck (topmost one)
            x0=42; y0=29; 
            x1 = x0 - 40; y1 = y0 + 100; // position tomost card visible when stacker is full
            x = x0 + (x1 - x0) * n / 1000; 
            y = y0 + (y1 - y0) * n / 1000; 
            nState= n * 9 / 1000; if (nState > 8) nState=8;            
            CopyControlImage(S360.CardInPunchStacker, nState,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,       x, y);       // ToCId, ToState,     x1, y1
            // draw stacker separator over output deck 
            CopyControlImage(S360.PunchStackerSeparator, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,            50, 0);       // ToCId, ToState,     x1, y1

        }

        // to force redraw of control 
        cpanel_ControlRedrawNeeded=1; 
        SetState(S360.PunchStacker, 1);
        nCardsInPunchStacker = nCards;
    }
}


void Refresh_CardReadPunch_3525(void)
{
    int CId, nState, msec;
    int x1, y1, x0, y0, x, y, h, n, nCardsL, nCardsR; 

    // Ready light on -> device ready to process i/o commands (either cdr, cdp or both)
    n= (((cdr_unit[0].flags & UNIT_DIS) || ((cdr_unit[0].flags & UNIT_ATT) == 0)) ? 0:1) |
       (((cdp_unit[0].flags & UNIT_DIS) || ((cdp_unit[0].flags & UNIT_ATT) == 0)) ? 0:1);
    SetState(S360.LI_Ready_CardPunch, n);
    SetState(S360.LI_EOF_CardReader, bCardReadEOFset ? 1:0); 

    // from bitsavers GA21-9124-1_3505_Card_Reader_and_3525_Card_Punch_Subsystem_Mar71
    // The 3525 [...] each stacker [...] holds about 1200 cards
    // card read feature provides the punch with an optical-punched hole sensing station
  
    // in cpanel, left stacker receives punched cards, right stacker receives read cards

    // animation sequence: as right stacker hides partially left stacker, a card arriving to any 
    // stacker makes both to be redraw
    // duration of card animation: 200 msec this value is a guess; used because it looks good

    // right stacker = cards read
    nCardsR = sim_card_output_hopper_count(&cdr_unit[0]) - 1; // minus one because last card still into the read station, not yet out
    if (nCardsR <0) nCardsR=0;
    // break big deck in clunches
    if (nCardsR > PARAM_nCardInStackerMax) {
        nCardsR = nCardsR % PARAM_nCardInStackerMax;
    }
    // left stacker = punched cards 
    nCardsL = sim_card_output_hopper_count(&cdp_unit[0]); 
    // break big deck in clunches
    if (nCardsL > PARAM_nCardInStackerMax) {
        nCardsL = nCardsL % PARAM_nCardInStackerMax;
    }

    if ((nCardsInReadStacker == nCardsR) && (tm0CardInReadStacker==0) &&
        (nCardsInPunchStacker == nCardsL) && (tm0CardInPunchStacker==0) ) {
        // both L and R decks are already draw with needed number of cards. 
        // and no card animation in progress
        // so no need to generate a new dynamic state image
    } else { 
        // draw animation of card falling into LEFT stacker
        //  - Copy State 0 (background) to state 1 (state displayed) to start with an empty image
        CopyControlImage(S360.PunchStacker, 0,       0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                         S360.PunchStacker, 1,       0, 0);       // ToCId, ToState,     x1, y1

        if (nCardsL>0) {
            // hide left card stick
            CopyControlImage(S360.LStackerBackground, 0,   0,  0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,       113, 24);       // ToCId, ToState,     x1, y1

        }

        // first draw Left stacker (with punched cards)
        if (tm0CardInPunchStacker) {
            // card dropping to stacker animation in progress
            CId=nState=0;
            msec = Refresh_tnow - tm0CardInPunchStacker; 
            if ((msec < 0) || (msec >= PARAM_CardDropTime_msec)) {
               // animation ended
               tm0CardInPunchStacker=0;
            } else if (nCardsL==1) {
                // drop first card
                CId    = S360.PunchStacker1L; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else if (nCardsL - nCardsInPunchStacker <= 1) {
                // drop one more card
                CId    = S360.PunchStacker2L; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else {
                // drop multiple cards at fast pace
                CId    = S360.PunchStacker2L; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
                // change states 0,1,2,3 to 0,4,1,5
                { static int st[4] = {0,4,1,5};  nState = st[nState]; }
            }
            // draw card dropping to stacker
            if (CId) { 
                CopyControlImage(CId, nState,                     0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.PunchStacker, 1,            0, 0);       // ToCId, ToState,     x1, y1
            }
        }
        // now, draw the deck of processed cards sitting into LEFT stacker
        if (nCardsL<=0) {
            // no cards to draw, empty stacker
            nCardsL=0;
        } else {
            if (nCardsL > PARAM_nCardInStackerMax) nCardsL=PARAM_nCardInStackerMax; // safety
            // stacker at its maximum capacity -> n=1000;
            n = nCardsL * 1000 / PARAM_nCardInStackerMax; 
            // draw output deck into leftmost output stacker
            h = 31 * n / 1000; 
            CopyControlImage(S360.CardDeckInStacker, 0,     21-h, 0, 0, 0,      // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,          41-h, 73);        // ToCId, ToState,     x1, y1
            // draw last card of deck (topmost one)
            x0=30; y0=71; // position tomost card visible when stacker is empty 
            x1= 0; y1=77; // position tomost card visible when stacker is full
            x = x0 + (x1 - x0) * n / 1000; 
            y = y0 + (y1 - y0) * n / 1000; 
            CopyControlImage(S360.CardInPunchStacker, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,          x, y);       // ToCId, ToState,     x1, y1
            // draw stacker separator between Left and right stacker
            CopyControlImage(S360.PunchStackerSeparator, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,             0, 0);       // ToCId, ToState,     x1, y1

        }
        // then draw RIGHT stacker (with read cards)
        if (tm0CardInReadStacker) {
            // card dropping to Right stacker animation in progress
            CId=nState=0;
            msec = Refresh_tnow - tm0CardInReadStacker; 
            if ((msec < 0) || (msec >= PARAM_CardDropTime_msec)) {
               // animation ended
               tm0CardInReadStacker=0;
            } else if (nCardsR==1) {
                // drop first card
                CId    = S360.PunchStacker1R; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else if (nCardsR - nCardsInReadStacker <= 1) {
                // drop one more card
                CId    = S360.PunchStacker2R; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
            } else {
                // drop multiple cards at fast pace
                CId    = S360.PunchStacker2R; 
                nState = 4 * msec / PARAM_CardDropTime_msec;
                // change states 0,1,2,3 to 0,4,1,5
                { static int st[4] = {0,4,1,5};  nState = st[nState]; }
            }
            // draw card dropping to stacker
            if (CId) {
                CopyControlImage(CId, nState,               0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                                 S360.PunchStacker, 1,     139, 0);       // ToCId, ToState,     x1, y1

            }
        }
        // draw the deck of processed cards sitting into RIGHT stacker
        if (nCardsR<=0) {
            // no cards to draw, empty stacker
            nCardsR=0;
        } else {
            if (nCardsR > PARAM_nCardInStackerMax) nCardsR=PARAM_nCardInStackerMax; // safety
            // stacker at its maximum capacity -> n=1000;
            n = nCardsR * 1000 / PARAM_nCardInStackerMax; 
            // draw output deck into leftmost output stacker
            h = 31 * n / 1000; 
            CopyControlImage(S360.CardDeckInStacker, 0,     26-h, 0, 0, 0,      // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,            135-h, 73);        // ToCId, ToState,     x1, y1

            // draw last card of deck (topmost one)
            x0=124; y0=71; // position tomost card visible when stacker is empty 
            x1= 93; y1=77; // position tomost card visible when stacker is full
            x = x0 + (x1 - x0) * n / 1000; 
            y = y0 + (y1 - y0) * n / 1000; 
            CopyControlImage(S360.CardInPunchStacker, 0,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,          x, y);       // ToCId, ToState,     x1, y1
            // draw stacker separator between right stacker and cabinet
            CopyControlImage(S360.PunchStackerSeparator, 1,    0, 0, 0, 0,  // FromCId, FromState, x0, y0, w, h, 
                             S360.PunchStacker, 1,             0, 0);       // ToCId, ToState,     x1, y1

        }
        // to force redraw of control 
        cpanel_ControlRedrawNeeded=1; 
        SetState(S360.PunchStacker, 1);
        nCardsInPunchStacker = nCardsL;   
        nCardsInReadStacker = nCardsR;
    }
}


void Refresh_CpuPanel(void)
{
    if (bCpuModelIs == 3148) {
        if (GetState(S360.SW_Lamp_Test)==1) {
            // lamp test on -> lit everything!
            SetCpuPanelLights_M148(2); 
        } else {
            // set register light in cpu panel
            SetCpuPanelLights_M148(1); 
        }  
    } else if (bCpuModelIs == 3145) {
        if (GetState(S360.SW_Lamp_Test)==1) {
            // lamp test on -> lit everything!
            SetCpuPanelLights_M145(2); 
        } else {
            // set register light in cpu panel
            SetCpuPanelLights_M145(1); 
        }  
    } else if (bCpuModelIs == 2065) {
        if ((1+(int) GetState(S360.Label_Roller6_L)) == 6) {
            // lamp test on -> lit everything!
            SetCpuPanelLights_M65(2); 
        } else {
            // set register light in cpu panel
            SetCpuPanelLights_M65(1); 
        }  
    } else if (bCpuModelIs == 2050) {
        if (GetState(S360.SW_Lamp_Test)==1) {
            // lamp test on -> lit everything!
            SetCpuPanelLights_M50(2); 
        } else {
            // set register light in cpu panel
            SetCpuPanelLights_M50(1); 
        }  
    } else if (bCpuModelIs == 2040) {
        SetCpuPanelLights_M40(1); 
    } else {
        if (GetState(S360.BTN_Lamp_Test)==1) {
            // lamp test on -> lit everything!
            SetCpuPanelLights_M30(2); 
        } else {
            // set register light in cpu panel
            SetCpuPanelLights_M30(1); 
        }  
    } 
    // calculate time elapsed for odometer
    IBM360_SetOdoMeter();
}

void process_HotKeys(void)
{
    int n; 
    char c; 

    if (vid_keyb.KeyPress == 'f'-'a'+1) { // Control-F (^F) is being pressed
        if (bFastMode == 0) { 
            // accelerate to max speed max while keydown
            bFastMode=1;        // do not wait for i/o operations to terminate
            ShowInfoTm0 = 0; // reset speed measurement because ^F pressed
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F pressed\n");
        }
    } else if (bFastMode==1) {
        // return real i/o speed
        bFastMode=0; 
        ShowInfoTm0 = 0; // reset speed measurement because ^F released
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F released\n");
    }
    if (vid_keyb.LastKeyPress ==  ('i'-'a'+1)) { // Tab/Control-I (^I) has been pressed
        // toggle show Info on GUI 
        vid_keyb.LastKeyPress = 0; // clear key as it is processed
        bShowInfo = (bShowInfo) ? 0:1; // toggle value
        if (bShowInfo==0) {
           // put again background thus removing info from gui
           SetState(S360.CtrlInfoPanel, 0); 
           if (((int) GetState(S360.MT_DASD_InfoPanel1)) || ((int) GetState(S360.MT_DASD_InfoPanel2))) {
               SetState(S360.MT_DASD_InfoPanel1, 0); 
               SetState(S360.MT_DASD_InfoPanel2, 0); 
           }
        } else {
           // init showinfo measurement
           ShowInfoTm0 = 0;
       }
    }
    // get keys sent to console, if console visible
    if (cpanel_visible(ncp_console)) {
        while (1) {
           c=cpvid_getkey(ncp_console); // get last key sent to console window
           if (c==0) break; // no remaining keys pressed
           // key pressed -> should send it to console
           if (bProceed==0) {
               // console keyboard locked (proceed light off) -> discard key typed
               cpvid[ncp_console].keyb_buf_len=0; // empty buffer
               break; 
           }
           // filter cpanel hotkeys ^I (tab) ^T ^Y ^F 
           if ((c==9) /* ^I tab */ || (c==20) /* ^T */ || (c==25) /* ^Y */ || (c==6) /* ^F */ ) {
               break;
           }
           // add key to console keyb buffer
           n=strlen(ConsKeybBuffer); 
           if (n >= sizeof(ConsKeybBuffer)-2) break; // console keyb buf full
           if (bConsTypeIs==1052) c=sim_toupper(c);  // 1052 only uppercase
           ConsKeybBuffer[n]=c; 
           ConsKeybBuffer[n+1]=0; 
        }
    }
}


// draw text on given Control Id infopanel 
void ShowInfo_DrawText(int CId, char * buf, int bNoScale)
{
    int scale, ww,hh,ww2,hh2, chrsz;
    int ncp; 

    GetControlSurface(CId, 1, &ww, &hh); // get size of control surface where to draw info
    if (bNoScale) {
        ww2=ww; hh2=hh; chrsz=1; // use full panel, char size x1
    } else {
        ncp=GetControlInfo(CId, CINFO_NCP);  // control panel window number where info is displayed
        scale=cpanel_scale(ncp,0);           // get scale of control panel window 
        if (scale >= 100) {
            ww2 = ww/2; hh2=hh/2; chrsz=1; // use half panel
        } else {
            ww2=ww; hh2=hh; chrsz=2; // use full panel, char size x2
        }
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
    char buf[10*100];
    int InstrExecPerSec, InstrCyclesPerSec, fps, n=0; 
    int MT_DASD_InfoPanel_Size=0; // size of MT_DASD infopanel to display. 0=not show, 1=short one, 2=long one
    int MT_InfoPanel_visible, DASD_InfoPanel_visible; 
    int i, unit; 
    int ips_unit; double ips; 
    double RealHwIPS; 
    uint32 msec; 

    // set dynamic contents of CtrlInfoPanel and MT_DASD_InfoPanel

    if (bShowInfo>1) {
        InstrExecPerSec = InstrCyclesPerSec = fps = 0;
    } else {
        InstrExecPerSec = InstrCyclesPerSec = fps = 0;
        msec = Refresh_tnow - ShowInfoTm0; 
        if ((msec > 0) && (msec < 1000)) {
            n = InstrExec - InstrExec0;
            if (n<0) n=0;
            InstrExecPerSec = n * 1000 / msec;
            n = InstrCycles - InstrCycles0;
            if (n<0) n=0;
            InstrCyclesPerSec = n * 1000 / msec;
            n = Refresh_Frames_Count  - FramesCount0;
            fps             = n * 1000 / msec;
            if (fps>60) fps=60;
            if (fps<0) fps=0;
        }
        ShowInfoTm0  = Refresh_tnow;
        InstrExec0   = InstrExec; 
        InstrCycles0 = InstrCycles; 
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
    //                           real hardware speed for IBM 360 Model 30 is 34500 Instructions Cycles per second
    //       IPS = Instructions Per Second -> Instr executed each sec (when waiting, does not count on IPS)

    // set the aprox speed of real hardware model 30. 40, 50, 65 to show the relative speed
    if (bCpuModelIs == 3148) {
        RealHwIPS = 34500.0 * 3 * 4 * 1.25; // model 148 performance is 1.25 times of a system / 370 model 145
    } else if (bCpuModelIs == 3145) {
        RealHwIPS = 34500.0 * 3 * 4; // model 145 is five times faster than the IBM System/360 Model 40
    } else if (bCpuModelIs == 2065) {
        RealHwIPS = 34500.0 * 10 * 3.5; // model 65 has more than 3 times performance of model 50
    } else if (bCpuModelIs == 2050) {
        RealHwIPS = 34500.0 * 10; // model 50 has aprox 10 times performance of model 30
    } else if (bCpuModelIs == 2040) {
        RealHwIPS = 34500.0 * 3; // Model 40 has aprox 3 times the internal power of the Model 30 when performing a mix of computations
    } else {
        RealHwIPS = 34500.0; // model 30
    }

    sprintf(buf, "FPS: %d Cpu Speed: x%0.1f (%0.1f %s)\n"
                 "Cards in read hopper: %d\n"
                 "Cards punched: %d, Lines Printed: %d",
            fps, InstrCyclesPerSec / RealHwIPS, 
            ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS",
            sim_card_input_hopper_count(&cdr_unit[0]), 
            sim_card_output_hopper_count(&cdp_unit[0]), 
            lptPrintOutCount); 
  
    ShowInfo_DrawText(S360.CtrlInfoPanel, buf, 0);

    // prepare panel for MT/DASD
    buf[0]=0; 
    MT_DASD_InfoPanel_Size=0;
    MT_InfoPanel_visible=DASD_InfoPanel_visible=0;

    if (bCardReadPunchVisible) {
        // panel visible only if Card Reader and printer visible (Panel is placed on top of card reader)
        // determine if at least one tape attached in MTA0..MTA5
        UNIT * uptr; 
        for(i=0;i<6;i++) { 
            if ((mta_unit[i].flags & UNIT_DIS) || (((mta_unit[i].flags & UNIT_ATT) == 0) &&
                (mtcab[i].mt_is != MT_is_unloading_tape))) continue; 
            MT_InfoPanel_visible=1;
            break; 
        }
        // determine if at least one dasd attached
        for (unit=0; unit<8*NUM_DEVS_DASD; unit++) {
            if (unit < 8) {
                extern UNIT dda_unit[]; 
                uptr=&dda_unit[unit]; 
            } else if ((NUM_DEVS_DASD>1) && ((unit >> 3) == 1)) {
                extern UNIT ddb_unit[]; 
                uptr=&ddb_unit[unit & 7]; 
            } else if ((NUM_DEVS_DASD>2) && ((unit >> 3) == 2)) {
                extern UNIT ddc_unit[]; 
                uptr=&ddc_unit[unit & 7]; 
            } else if ((NUM_DEVS_DASD>3) && ((unit >> 3) == 3)) {
                extern UNIT ddd_unit[]; 
                uptr=&ddd_unit[unit & 7]; 
            }
            if ((uptr->flags & UNIT_ATT) == 0) continue; 
            DASD_InfoPanel_visible=1; 
            break;
        }
    }


    if (MT_InfoPanel_visible) {
        // set tape info panel
        double p; 
        char sblk[6][15]; // only mta0..mta5 visible in InfoPanel
        char sunwind[6][15]; 
        const char * scmd[6]; 
        char scmdtxt[2][15] = {"", "REW"};
        int n, addr, cmd;
        char c; 
        for(i=0;i<6;i++) { 
            c = ' ';
            if ((mta_unit[i].flags & UNIT_DIS) || (((mta_unit[i].flags & UNIT_ATT) == 0) &&
                (mtcab[i].mt_is != MT_is_unloading_tape))) {
                n = 0;
                sblk[i][0]=sunwind[i][0]=0;
                scmd[i]=scmdtxt[0]; // empty string
                continue; 
            } else if ((mtcab[i].mt_is == MT_is_rewinding) || 
                       ((mtcab[i].mt_is == MT_is_loading_tape) && (mtcab[i].rew_u3 > 0)) ||
                       ((mtcab[i].mt_is == MT_is_unloading_tape) && (mtcab[i].rew_u3 > 0))  ) {
                // if rewinding then the ammount of tape medium used is in variable recsize, not in u3
                // if loading tape AND rew_u3 > 0 then the tape is showing a nice rew animation started with R
                n = mtcab[i].rew_u3; 
                scmd[i]=scmdtxt[1]; // rewind string
                c = 'R';
            } else {
                // use regular value from mt_into to get the amount of medium used 
                n = mt_info[i].MediumPos; // amount of reel used in inches x1000
                scmd[i]=((mt_info[i].last_cmd_name) ? mt_info[i].last_cmd_name : scmdtxt[0]);
            }
            p = n / (mt_info[i].MediumLen * 10.0); 
            sprintf(sblk[i], "%7d", mt_info[i].numrec); 
            sprintf(sunwind[i], "%5.1f%c", p, c); 
        }
        cpanel_tape_cmd_info(&mta_unit[0], &addr, &cmd); // get addr for first tape

        MT_DASD_InfoPanel_Size++;
        sprintf(buf, "         %3x-MTA0  %3x-MTA1  %3x-MTA2  %3x-MTA3  %3x-MTA4  %3x-MTA5 \n"  // 68 chars long
                     " cmd      %7s   %7s   %7s   %7s   %7s   %7s \n"
                     " block    %7s   %7s   %7s   %7s   %7s   %7s \n"
                     " %% unwind   %6s    %6s    %6s    %6s    %6s    %6s ",
            addr, addr+1, addr+2, addr+3, addr+4, addr+5, 
            scmd[0], scmd[1], scmd[2], scmd[3], scmd[4], scmd[5],
            sblk[0], sblk[1], sblk[2], sblk[3], sblk[4], sblk[5], 
            sunwind[0], sunwind[1], sunwind[2], sunwind[3], sunwind[4], sunwind[5]
        );
        for(i=0;i<8;i++) { 
            // last tape command has been set in infopanel. 
            // If command has finished, clear last tape command variable so it is not shown again in next refresh
            if ((mta_unit[i].u3 & 0x3f) == 0) mt_info[i].last_cmd_name = NULL;
        }
    }

    if (DASD_InfoPanel_visible) {
        // set dasd info panel
        #define DASD_STR(i,s)            (((dasd_addr_name[i]) && (dasd_addr_name[i][0]) && (s) && (s[0])) ? s:"")
        extern UNIT dda_unit[]; 
        extern void cpanel_dasd_cmd_info(UNIT * uptr, int * addr, int * cmd, int * cyl, int * hd, int * trk);
        extern const char * cpanel_dasd_last_cmd_name(UNIT * uptr, int bClear);
        UNIT * uptr; 
        int addr, cmd, cyl, hd, trk;
        const char * cmd_name[6] = {0}; 
        char dasd_addr_name[6][15]; 
        char scyl[6][15]; // max 6 dasd visible in InfoPanel
        char shead[6][15]; 

        for (i=0; i<6; i++) dasd_addr_name[i][0]=0; 
        i=0; 
        for (unit=0; unit<8*NUM_DEVS_DASD; unit++) {
            if (unit < 8) {
                uptr=&dda_unit[unit]; 
            } else if ((NUM_DEVS_DASD>1) && ((unit >> 3) == 1)) {
                extern UNIT ddb_unit[]; 
                uptr=&ddb_unit[unit & 7]; 
            } else if ((NUM_DEVS_DASD>2) && ((unit >> 3) == 2)) {
                extern UNIT ddc_unit[]; 
                uptr=&ddc_unit[unit & 7]; 
            } else if ((NUM_DEVS_DASD>3) && ((unit >> 3) == 3)) {
                extern UNIT ddd_unit[]; 
                uptr=&ddd_unit[unit & 7]; 
            }
            if ((uptr->flags & UNIT_ATT) == 0) continue; 
            // get current state of dasd
            cpanel_dasd_cmd_info(uptr, &addr, &cmd, &cyl, &hd, &trk);
            // get name of last cmd executed, and also clear it if command has finished
            cmd_name[i] = cpanel_dasd_last_cmd_name(uptr, cmd ? 0:1);
            sprintf(dasd_addr_name[i], "%3x-%3s", addr, uptr->uname); 
            sprintf(scyl[i], "%7d", cyl); 
            sprintf(shead[i], "%7d", hd); 
            i++; 
            if (i==6) break; 
        }
        if (MT_InfoPanel_visible) {
            // if dasd and tapes visible, add a separation blank line
            sprintf(&buf[strlen(buf)], "\n\n");
        }
        sprintf(&buf[strlen(buf)], 
                     "          %7s   %7s   %7s   %7s   %7s   %7s \n"
                     " cmd      %7s   %7s   %7s   %7s   %7s   %7s \n"
                     " cyl      %7s   %7s   %7s   %7s   %7s   %7s \n"
                     " head     %7s   %7s   %7s   %7s   %7s   %7s ", 
            dasd_addr_name[0], dasd_addr_name[1], dasd_addr_name[2], dasd_addr_name[3], dasd_addr_name[4], dasd_addr_name[5], 
            DASD_STR(0, cmd_name[0]), DASD_STR(1, cmd_name[1]), DASD_STR(2, cmd_name[2]), DASD_STR(3, cmd_name[3]), DASD_STR(4, cmd_name[4]), DASD_STR(5, cmd_name[5]),
            DASD_STR(0, scyl[0]),     DASD_STR(1, scyl[1]),     DASD_STR(2, scyl[2]),     DASD_STR(3, scyl[3]),     DASD_STR(4, scyl[4]),     DASD_STR(5, scyl[5]), 
            DASD_STR(0, shead[0]),    DASD_STR(1, shead[1]),    DASD_STR(2, shead[2]),    DASD_STR(3, shead[3]),    DASD_STR(4, shead[4]),    DASD_STR(5, shead[5])
        );
        MT_DASD_InfoPanel_Size++;  

    }

    if (MT_DASD_InfoPanel_Size==0) {
        // Nothing to show in infopanel: no tapes attached, no dasd attached
        sprintf(buf, "\n (No tapes attached, no dasd attached)");
        ShowInfo_DrawText(S360.MT_DASD_InfoPanel1, buf, 1);
        SetState(S360.MT_DASD_InfoPanel2, 0); 
    } else {
        // add fastmode condition
        if (bFastMode) {
            const char s[10] = "(Fast)";
            int i; 
            for (i=0; s[i]; i++) { buf[i]=s[i]; }
        }
        if (MT_DASD_InfoPanel_Size==1) {
            // only tapes or dasd visible, but no both -> use short panel
            ShowInfo_DrawText(S360.MT_DASD_InfoPanel1, buf, 1);
            SetState(S360.MT_DASD_InfoPanel2, 0); 
        } else {
            // tapes and dasd visible -> use long panel
            ShowInfo_DrawText(S360.MT_DASD_InfoPanel2, buf, 1);
        }
    }

}

void IBM360_Refresh(void)
{
    // refresh Cpu Panel
    Refresh_CpuPanel();

    // IBM 2540
    if (bCardReadPunchVisible) {
        if (bCpuModelIs > 3000) {
            Refresh_CardReadPunch_3525(); // IBM 370 uses 3525
        } else {
            Refresh_CardReadPunch_2540(); // IBM 360 uses 2540
        }
    }
    // IBM 1403
    if (bPrintOutVisible) {
        Refresh_PrintOut();
    }
    // tapes
    if (bTapesVisible) {
        Refresh_MagTape();
    }
    // IBM 2314
    if (bDasdVisible) {
        Refresh_Dasd();
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

    // IBM 1052 
    if (cpanel_visible(ncp_console)) {
        Refresh_PrinterConsole();
    }
}

// drag and drop a file handling. Called when a file of given filename is droped on given CId control 
void IBM360_DropFile(int CId, char * FileName)
{
    extern t_stat   cdr_attach(UNIT *, CONST char *);
    extern t_stat   mt_attach(UNIT * uptr, CONST char *file);
    int32  sv; 
    int n; 

    if (CId ==S360.Drop_InputDeckFile) {
        // drag and drop a file on card reader -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q') | SWMASK ('E'); // set switch -Q (quiet) -E (eof)
        // if already cards in reader, stack the new ones on the top
        if (sim_card_input_hopper_count(&cdr_unit[0]) > 0) sim_switches |= SWMASK ('S'); 
        cdr_attach(&cdr_unit[0], FileName);
        sim_switches=sv; 
    } else for (n=0;n<8;n++) if ((CId == S360.MT[n]) || (CId == S360.MT_Drop_TapeFile[n])) {
        // drag and drop a file on tape -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        if (vid_keyb.RightButton) {
            sim_switches |= SWMASK ('R'); // set switch -R (read only) 
        }
        mt_attach(&mta_unit[n], FileName);
        sim_switches=sv; 
    }
}

// buttons for tape detach
void IBM360_OnClick_BTN2(void)
{
    extern t_stat   mt_detach(UNIT * uptr);
    int n; 
    int32  sv; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        return;
    }
    // set button to unpressed state 
    SetState(CP_Click.CId, 0);

    if (cpinput.mouse.drag_flag==2) {
        // key release for a windows drag -> do not execute button operation
        return; 
    }

    // to detach a tape, must click on reset button, then on load-rew
    for (n=0;n<8;n++) {
        if (CP_Click.CId == S360.MT_BTN_Reset[n]) {
            // click on tape reset 
            mtcab[n].ResetPressed=1;
        } else if (CP_Click.CId == S360.MT_BTN_Start[n]) {
            // click on tape start 
            mtcab[n].ResetPressed=0;
        } else if (CP_Click.CId == S360.MT_BTN_LoadRew[n]) {
            // click to detach tape
            // reset not pressed. Must press reset before load-rew to unload a tape
            if (mtcab[n].ResetPressed==0) return;
            mtcab[n].ResetPressed=0; // clear reset flag
            // already detached. Do not detach again
            if ((mta_unit[n].flags & UNIT_ATT) == 0) return; 
            sv=sim_switches; // save current switches
            sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
            mt_detach(&mta_unit[n]);
            sim_switches=sv; 
        } else if (CP_Click.CId == S360.MT_BTN_DoorOpen[n]) {
            // click to open/close door          
            sv=mtcab[n].state.MT_DoorOpen;
            sv= (sv ? 0:1);
            cpanel_set_mt_door(n,sv); 
        }
    }
}

int GetSwitchNum2(int SW_A, int SW_B) 
{
    int n=0; 
    if (SW_A) n+=((int) (GetState(SW_A)) & 0xF) * 16;
    if (SW_B) n+=((int) (GetState(SW_B)) & 0xF);
    return n; 
}

int GetSwitchNum4(int SW_A, int SW_B, int SW_C, int SW_D) 
{
    return GetSwitchNum2(SW_A, SW_B) * 256 + GetSwitchNum2(SW_C, SW_D); 
}

int GetSwitchNum6(int SW_A, int SW_B, int SW_C, int SW_D, int SW_E, int SW_F) 
{
    return GetSwitchNum2(SW_A, SW_B) * 256 * 256 + GetSwitchNum4(SW_C, SW_D, SW_E, SW_F); 
}

void SetAddrBreakPoint(void)
{
    cpu_mem_comp.match = 0; // disable match addr breakpoint
    if (bCpuModelIs == 2030) {
        if (GetState(S360.SW_Addr_Compare)) {
            cpu_mem_comp.addr = GetSwitchNum4(S360.SW_A, S360.SW_B, S360.SW_C, S360.SW_D); 
            cpu_mem_comp.match = 3; // Addr compare SAR -> compare any read/write addr
        }
    } else if ((bCpuModelIs == 3145) || (bCpuModelIs == 3148))  {
        if (GetState(S360.SW_Addr_Compare_Cntrl)) {
            cpu_mem_comp.addr = GetSwitchNum6(S360.SW_C, S360.SW_D, S360.SW_E, S360.SW_F, S360.SW_G, S360.SW_H); 
            cpu_mem_comp.matchbyte = GetSwitchNum2(S360.SW_A, S360.SW_B);
            switch (GetState(S360.SW_Addr_Compare)) {
               case 0: cpu_mem_comp.match = 3; break; // Addr compare ANY real -> compare read/write addr
               case 1: cpu_mem_comp.match = 1; break; // Addr compare Data Store real -> compare write addr
               case 2: cpu_mem_comp.match = 4; break; // Addr compare I/O real -> compare on chan r/w addr
               case 3: cpu_mem_comp.match = 8; break; // Addr compare I counter real -> compare on PC addr at fetch instr
               case 4: cpu_mem_comp.match = 9; break; // data comp trap -> compare writting byte to real mem 
               case 8: cpu_mem_comp.match = 8; break; // Addr compare I counter virtual -> compare on PC addr at fetch instr
               case 9: cpu_mem_comp.match = 16+3; break; // Addr compare ANY virtual -> compare virtual read/write addr
            }
            // note: addr compare on PC allways is on virtual addres, not in real address
            // when addr is matched, the breackpoint occurs after the current inst is exec, as in real HW
        }
    }
}

// handle Rotating Switch 0-F
void IBM360_OnClick_Sw(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press button -> start switch rotation motion
        if (GetControlInfo(CP_Click.CId, CINFO_NSTATES)==16) {
            n = (int) (GetState(CP_Click.CId));
            n = (n & 7) + 8;
        } else {
            n = (int) (GetState(CP_Click.CId));
            n = (n & 0xF) + 16;
        }
        SetState(CP_Click.CId, n);
        return;
    }
    // set new position
    if (GetControlInfo(CP_Click.CId, CINFO_NSTATES)==16) {
        n = (int) (GetState(CP_Click.CId));
        n = (n & 7) + 1; if (n==8) n=0;
    } else {
        n = (int) (GetState(CP_Click.CId));
        n = (n & 0xF) + 1; if (n==16) n=0;
    }
    SetState(CP_Click.CId, n);
    if (bCpuModelIs==2030) {
        SetAddrBreakPoint();
    } else if ((bCpuModelIs == 3145) || (bCpuModelIs == 3148))  {
        SetAddrBreakPoint();
    }
}


// handle Rotating Switch 0-9 for Rate/Addr compare/Storage select on model 370 cpu's
void IBM360_OnClick_Sw3(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press button -> start switch rotation motion
        n = (int) (GetState(CP_Click.CId));
        n = (n % 10) + 10;
        SetState(CP_Click.CId, n);
        return;
    }
    // set new position
    n = (int) (GetState(CP_Click.CId));
    n = (n % 10) + 1; if (n==10) n=0;
    SetState(CP_Click.CId, n);

    if (CP_Click.CId == S360.SW_Rate) {
        if (GetState(CP_Click.CId) == 0) return; //  rate is  at "proceed" -> nothing to do
        SetState(CP_Click.CId, 9); // only states 0 (proceed) and 9 (inst step) allowed 
        if (sim_is_running == 0) return; // cpu is not running -> nothing to do
        // set rate to instr while cpu is running -> stop cpu
        cpanel_stop_flag = 2;
        // and set interactive mode
        cpanel_interactive=1; 
        return;
    } else if (CP_Click.CId == S360.SW_Addr_Compare) {
        n=(int)GetState(CP_Click.CId);
        if ((n > 4) && (n < 8)) SetState(CP_Click.CId, 8); // skip non-implemented states 5..7
        SetAddrBreakPoint(); // this is addr compare handling for 3145 (model 145) and 3148 (model 148)
    } else if (CP_Click.CId == S360.SW_Storage_Select) {
        if (GetState(CP_Click.CId) < 2) SetState(CP_Click.CId, 2); 
        else SetState(CP_Click.CId, 0); // only states 0 (main storage) and 2 (local storage) allowed 
    }
}

// handle Rotating Switch 0-1 (with sprin initiated states states 2 and 3)
// and handle Up/Down switch 0-1 (with no intermediate states)
void IBM360_OnClick_Sw2(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // if is a up/down switch, ignore keypress; processing is done on keyrelease
        if (GetControlInfo(CP_Click.CId, CINFO_NSTATES) == 2) return; 
        // start switch rotation motion
        n = (int) (GetState(CP_Click.CId));
        n = (n & 1) + 2;
        SetState(CP_Click.CId, n);
        return;
    }
    // set new position
    n = (int) (GetState(CP_Click.CId));
    n = (n & 1) + 1; if (n==2) n=0;
    SetState(CP_Click.CId, n);

    if (CP_Click.CId == S360.SW_Rate) {
        if (GetState(CP_Click.CId) != 1) return; // rate set to proceed -> nothing to do
        if (sim_is_running == 0) return; // cpu is not running -> nothing to do
        // set rate to instr while cpu is running -> stop cpu
        cpanel_stop_flag = 2;
        // and set interactive mode
        cpanel_interactive=1; 
        return;
    } else if (CP_Click.CId == S360.SW_Addr_Compare) {
        SetAddrBreakPoint(); // this is addr compare handling for 2030 model 30 
    } else if (CP_Click.CId == S360.SW_Addr_Compare_Cntrl) {
        SetAddrBreakPoint();
    } else if (CP_Click.CId == S360.SW_Storage_Select) {
        // nothing more to do. State has changed, that's all       
    }
}

// Handle Button
void IBM360_OnClick_BTN(void)
{
    uint32 addr, data, n; 
    UNIT * uptr; 
    char buf[80];

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        SetState(CP_Click.CId, 1);
        if (CP_Click.CId == S360.BTN_Emergency_Off) {
            // if Emergency button pressed, set also the state of button shadow
            SetState(S360.Shdw_Emergency_Off, 1);
        } else if (CP_Click.CId == S360.SW_Lamp_Test) {
            // if Switch Lamp test is pressend (Model 50 only), set also the state of switch shadow
            SetState(S360.Shdw_Lamp_Test, 1);
        }
        return;
    }
    // set button to unpressed state (except Power Button, that remains pressed)
    if ((CP_Click.CId != S360.BTN_Power_Off) && (CP_Click.CId != S360.BTN_Emergency_Off)) {
        SetState(CP_Click.CId, 0);
        if (CP_Click.CId == S360.SW_Lamp_Test) {
            // reset state of switch shadow
            SetState(S360.Shdw_Lamp_Test, 0);
        }
    }
    if (cpinput.mouse.drag_flag==2) {
        // key release for a windows drag -> do not execute opertion
        return; 
    }

    if ((CP_Click.CId == S360.BTN_Power_Off) || (CP_Click.CId == S360.BTN_Emergency_Off)) {
        cpanel_stop_flag = 4; // request cpanel off and quit
        return;
    } else if (CP_Click.CId == S360.BTN_System_Reset) {
        // can be pressed at any moment
        if (sim_is_running) { // if cpu is running then request a cpu stop, without disabling interactive mode
            cpanel_stop_flag = 2;
            // and set interactive mode
            cpanel_interactive=1; 
        }
        if (bCpuModelIs==2030) bSystemResetKeyPressed=1; // signal system reset pressed
        DoSCP("reset cpu"); 
        return;
    } else if (CP_Click.CId == S360.BTN_Start) {
        if (sim_is_running != 0) {
            // Start while cpu running -> ignore
        } else if (bSystemResetKeyPressed == 1) {
            // Start pressed after system reset 
            // now cpu runs the microprogram tests (not simulated) and stops
            bSystemResetKeyPressed=2; 
        } else if (bSystemResetKeyPressed == 3) {
            // on 2030 (model 30)
            // Start pressed again after system reset -> psw restart ->the PSW located 
            // at address 0000 is loaded into circuitry, and processing starts
            bSystemResetKeyPressed=0; 
            cpu_psw_restart=1;
            // send command to scp
            DoSCP("go"); 
        } else if (GetState(S360.SW_Rate)) {
            // Rate Switch is set to INSTR STEP
            cpanel_interactive=1;  // set interactive mode
            // send command to scp
            DoSCP("step"); 
        } else {
            // run the program 
            // disable interactive mode so when 
            // terminating prog the script can continue
            cpanel_interactive=0; 
            // send command to scp
            DoSCP("go"); 
        }
        // check switches to see if addr compare set. if so, set al cpanel_interactive to 1 
        // so if breakpoint reached, control will return to user via cpanel 
        SetAddrBreakPoint();
        return; 
    }
    bSystemResetKeyPressed=0; // clear signal system reset pressed flag
    bRegBA_display=0; // set Reg BA will display PC when stopped (default behaviour) 
    if (CP_Click.CId == S360.BTN_Stop) {
        if (sim_is_running == 0) return; // stop while cpu is not running is ignored
        // request a cpu stop, without disabling interactive mode
        cpanel_stop_flag = 2;
        // and set interactive mode
        cpanel_interactive=1; 
    } else if (CP_Click.CId == S360.BTN_Restart) {
        extern void storepsw(uint32 addr, uint16 ircode);
        storepsw(ICCW1, 0); // doc does not details the irqcode stpred in PSW at addr 0x8
        cpu_psw_restart=1;
        // restars key is active when cpu is stoped or running
        // if stoped, issue a go command to start exec from psw at addr zerp
        if (sim_is_running == 0) DoSCP("go"); 
    } else if (CP_Click.CId == S360.BTN_Store) {
        if (sim_is_running != 0) return; // store while cpu is running is ignored
        if (bCpuModelIs==2030) {
            // Rotating switch E (storage select) is not simulated; store key allways write data into Main Storage
           addr = GetSwitchNum4(S360.SW_A, S360.SW_B, S360.SW_C, S360.SW_D);
           data = GetSwitchNum2(S360.SW_H, S360.SW_J);
           WriteByte(addr, data);
           cpu_mem_data=data; 
           cpu_mem_addr=addr; 
           bRegBA_display=1; // set Reg BA will display data stored
        } else if ((bCpuModelIs==3145) ||(bCpuModelIs==3148)) {
            data = GetSwitchNum2(S360.SW_A, S360.SW_B);
            if (GetState(S360.SW_Storage_Select)) {
                // local storage selected. retrieve reg 32bit contents
                addr = GetSwitchNum2(S360.SW_F, S360.SW_G);
                if (addr < 16) n=regs[addr]; // get 32bit value from reg
                else if ((addr >= 0x30) && (addr <= 0x37)) n=fpregs[addr - 0x30]; else n=0;
                // alter selected byte in switch H
                switch(GetState(S360.SW_F) & 3) {
                    case 0: n=(n & (~0x000000ff)) | data; break; 
                    case 1: n=(n & (~0x0000ff00)) | (data << 8); break; 
                    case 2: n=(n & (~0x00ff0000)) | (data << 16); break; 
                    case 3: n=(n & (~0xff000000)) | (data << 24); break; 
                }
                if (addr < 16) regs[addr]=n; // set back 32bit result into reg
                else if ((addr >= 0x30) && (addr <= 0x37)) fpregs[addr - 0x30]=n; else addr=-1; // invalid value selected in sw F G 
                if (addr >=0) {
                    cpu_mem_data=n; 
                    cpu_mem_addr=addr; 
                }
            } else {
                // main storage selected
                addr = GetSwitchNum6(S360.SW_C, S360.SW_D, S360.SW_E, S360.SW_F, S360.SW_G, S360.SW_H);
                WriteByte(addr, data);
                cpu_mem_data=data; 
                cpu_mem_addr=addr; 
            }
        }
    } else if (CP_Click.CId == S360.BTN_Display) {
        if (sim_is_running != 0) return; // display while cpu is running is ignored
        if (bCpuModelIs==2030) {
            // Rotating switch E (storage select) is not simulated; store key allways write data into Main Storage
            addr = GetSwitchNum4(S360.SW_A, S360.SW_B, S360.SW_C, S360.SW_D);
            ReadByte(addr, &data);
            cpu_mem_data=data; 
            cpu_mem_addr=addr; 
        } else if ((bCpuModelIs==3145) ||(bCpuModelIs==3148)) {
            if (GetState(S360.SW_Storage_Select)) {
                // local storage selected. retrieve reg 32bit contents
                addr = GetSwitchNum2(S360.SW_F, S360.SW_G);
                if (addr < 16) n=regs[addr]; // get 32bit value from reg
                else if ((addr >= 0x30) && (addr <= 0x37)) n=fpregs[addr - 0x30]; else addr=-1; // invalid value selected in sw F G 
                if (addr >=0) {
                    cpu_mem_data=n; 
                    cpu_mem_addr=addr; 
                }
            } else {
                // main storage selected
                addr = GetSwitchNum6(S360.SW_C, S360.SW_D, S360.SW_E, S360.SW_F, S360.SW_G, S360.SW_H);
                ReadByte(addr, &data);
                cpu_mem_data=data; 
                cpu_mem_addr=addr; 
            }
        }
    } else if (CP_Click.CId == S360.BTN_Set_IC) {
        if (sim_is_running != 0) return; // if cpu is running key is ignored
        if (bCpuModelIs == 2030) {
            PC = GetSwitchNum4(S360.SW_F, S360.SW_G, S360.SW_H, S360.SW_J);
        } else if ((bCpuModelIs==3145) ||(bCpuModelIs==3148)) {
            PC = GetSwitchNum6(S360.SW_C, S360.SW_D, S360.SW_E, S360.SW_F, S360.SW_G, S360.SW_H);
        }
    } else if (CP_Click.CId == S360.BTN_Interrupt) {
        // this key is available when cpu is running or stopped
        post_extirq();
    } else if (CP_Click.CId == S360.BTN_Load) {
        if (sim_is_running != 0) return; // if cpu is running key is ignored
        if ((bCpuModelIs==3145) ||(bCpuModelIs==3148)) {
            // Model 145 gets load addr from rotating switches F G H 
           addr = GetSwitchNum4(0, S360.SW_F, S360.SW_G, S360.SW_H);
        } else if ((bCpuModelIs==2040) || (bCpuModelIs==2050) || (bCpuModelIs==2065)) {
            // Model 40, 50, 65 gets load addr from rotating switches A B C 
           addr = GetSwitchNum4(0, S360.SW_A, S360.SW_B, S360.SW_C);
        } else if (bCpuModelIs==2030) {
            // Model 30 gets load addr from rotating switches G H J 
           addr = GetSwitchNum4(0, S360.SW_G, S360.SW_H, S360.SW_J);
        }
        uptr = unit_name_at_addr(addr);
        if (uptr == NULL) {
            printf ("No unit defined at address %03x\r\n", addr);                              
            return; 
        }
        SetAddrBreakPoint();
        sprintf(buf, "boot %s", uptr->uname);
        DoSCP(buf); 
    } else if (CP_Click.CId == S360.BTN_Cons_Request) {
        // request button on 1052/3210 console
        if ((con_unit[0].CMD & 0x0f) != 0x0a) {
            con_unit[0].CMD |= CON_REQ; 
        }
    } else if (CP_Click.CId == S360.BTN_Cons_Cancel) {
        // Cancel button on 3210 console
        if (bProceed==0) return; // console keyboard locked (proceed light off) -> key disabled
        ConsKeybBuffer[0]=0x03; // put ^C char as first char in keyboard buffer
        ConsKeybBuffer[1]=0;    // and clear any pending char in keyb buffer
    } else if (CP_Click.CId == S360.BTN_Cons_End) {
        // End (EOB - End of block) button on 3210 console
        // add ^B char as last char in keyboard buffer
        if (bProceed==0) return; // console keyboard locked (proceed light off) -> key disabled
        n=strlen(ConsKeybBuffer); 
        if (n >= sizeof(ConsKeybBuffer)-2) return; // console keyb buf full
        ConsKeybBuffer[n]=0x02; 
        ConsKeybBuffer[n+1]=0; 
    } else if (CP_Click.CId == S360.BTN_Roller1) {
        // click on roller
        data=(int) GetState(S360.Label_Roller1_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((bCpuModelIs == 2065) && ((data & 7) > 5)) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller1_L, data);
        SetState(S360.Label_Roller1_R, data);
    } else if (CP_Click.CId == S360.BTN_Roller2) {
        // click on roller
        data=(int) GetState(S360.Label_Roller2_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((bCpuModelIs == 2065) && ((data & 7) > 5)) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller2_L, data);
        SetState(S360.Label_Roller2_R, data);
    } else if (CP_Click.CId == S360.BTN_Roller3) {
        data=(int) GetState(S360.Label_Roller3_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((bCpuModelIs == 2065) && ((data & 7) > 5)) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller3_L, data);
        SetState(S360.Label_Roller3_R, data);
    } else if (CP_Click.CId == S360.BTN_Roller4) {
        data=(int) GetState(S360.Label_Roller4_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((bCpuModelIs == 2065) && ((data & 7) > 5)) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller4_L, data);
        SetState(S360.Label_Roller4_R, data);
    } else if (CP_Click.CId == S360.BTN_Roller5) {
        data=(int) GetState(S360.Label_Roller5_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((data & 7) > 5) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller5_L, data);
        SetState(S360.Label_Roller5_R, data);
    } else if (CP_Click.CId == S360.BTN_Roller6) {
        data=(int) GetState(S360.Label_Roller6_L);
        data = (data & 0xF8) | (((data & 7) + 1) & 7) ; 
        if ((data & 7) > 5) data = data & (~7); // 2065 roller only has 6 positions
        SetState(S360.Label_Roller6_L, data);
        SetState(S360.Label_Roller6_R, data);
    }
}

// Matches the ifdef(CPANEL) at beginning of file
#endif

