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
   Sep-22    RSV     HP 2100 control panel support

   This module implements the following control panels:

   HP 2116/5/4 Cpu Panel
   HP 2100 Cpu Panel
   HP 21MX/1000 Cpu Panel
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "hp2100_defs.h"	        
#include <math.h>

// cpu registers
#include "hp2100_cpu.h"	        
extern int          InstrExec;            // count of intructions executed so far 
extern FLIP_FLOP    ion;                  // cpu interrupt enable 


CP_DEF HP2100_cp[];			// declare forwards
void HP2100_Init(void);
void HP2100_Done(void);
void HP2100_Reset(void);
void HP2100_Refresh(void);
void HP2100_TickIntensityCount(void);

void HP211X_OnClick_BTN(void); 
void HP211X_OnClick_SW_Reg(void); 

void HP2100A_OnClick_Reg_D(void); 
void HP2100A_OnClick_BTN(void); 

void HP1000_OnClick_Reg_D(void); 
void HP1000_OnClick_BTN(void); 

// control panel callbacks
CP_TYPE cp_types = {
      HP2100_cp, 
      &HP2100_Init, &HP2100_Done, &HP2100_Reset, 
      &HP2100_Refresh, &HP2100_TickIntensityCount, NULL
 };

// struct to hold the GUI controls Ids
static struct {
    int CtrlInfoPanel;
    // for HP211X
    int Reg_T, Reg_P, Reg_M, Reg_A, Reg_B;
	int IND_Extend, IND_Overflow, 
        IND_Fetch, IND_Indirect, IND_Execute, IND_Interrupt; 
	int SW_Loader, SW_Reg;
	int BTN_Preset,	BTN_Run, BTN_Halt, 
		BTN_Load_Mem, BTN_Load_A, BTN_Load_B, BTN_Load_Addr, 
        BTN_Disp_Mem, BTN_Single_Cycle,
		BTN_Power;

    // for HP2114
	int BTN_Load_Ptr, BTN_Clear_SW_Reg;				// buttons only for HP2114

	// HP2100A
	int Reg_D;										// also used by HP1000
	int BTN_Loader, BTN_Internal_Preset, BTN_External_Preset, IND_Parity;
	int BTN_Reg_A, BTN_Reg_B, BTN_Reg_S, BTN_Reg_P;
	int BTN_Reg_M, BTN_Memory_Data;
	int BTN_Dec_M, BTN_Inc_M;						// also used by HP1000
	int BTN_Step;									// also used by HP1000
	// Interrupt_System, Extend and Overflow are mapped to IND_Interrupt_System, IND_Extend and IND_Overflow
	int BTN_Clear_Display;							// also used by HP1000

	// HP1000
	int IND_Run, IND_Interrupt_System;
	int IND_Reg_A, IND_Reg_B, IND_Reg_M, IND_Reg_T, IND_Reg_P, IND_Reg_S; 
	int Sw_Reg_D, BTN1_Reg_D, BTN0_Reg_D;
	int Sw_Run_Halt, Sw_Preset_IBL, Sw_Step_Clear, Sw_Inc_Dec, Sw_Store_Mode, Sw_Left_Right;
	int BTN_IBL, BTN_Store, BTN_Mode, BTN_Left, BTN_Right;
} HP2100 = {0}; // must be init to zero

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF HP2100_cp[] = {
    { &HP2100.CtrlInfoPanel,        "CtrlInfoPanel",        NULL},
    // HP 2116/2115/2114 cpu panel
	{ &HP2100.Reg_T,				"Reg_T",				NULL,                   "CpuFamily/211X"},
	{ &HP2100.Reg_P,				"Reg_P",				NULL,                   "CpuFamily/211X"},
	{ &HP2100.Reg_M,				"Reg_M",				NULL,                   "CpuFamily/211X"},
	{ &HP2100.Reg_A,				"Reg_A",				NULL,                   "CpuFamily/211X"},
	{ &HP2100.Reg_B,				"Reg_B",				NULL,                   "CpuFamily/211X"},
	{ &HP2100.SW_Loader,			"SW_Loader",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Preset,			"BTN_Preset",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Run,				"BTN_Run",				&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Halt,				"BTN_Halt",				&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Load_Mem,			"BTN_Load_Mem",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Load_A,			"BTN_Load_A",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Load_B,			"BTN_Load_B",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Load_Addr,		"BTN_Load_Addr",		&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Disp_Mem,			"BTN_Disp_Mem",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Single_Cycle,		"BTN_Single_Cycle",		&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.BTN_Power,			"BTN_Power",			&HP211X_OnClick_BTN,    "CpuFamily/211X"},
	{ &HP2100.SW_Reg,				"SW_Reg",				&HP211X_OnClick_SW_Reg, "CpuFamily/211X"},
	{ &HP2100.IND_Extend,           "IND_Extend",           NULL,                   "CpuFamily/211X"},
	{ &HP2100.IND_Overflow,			"IND_Overflow",         NULL,                   "CpuFamily/211X"},
	{ &HP2100.IND_Fetch,			"IND_Fetch",			NULL,                   "CpuFamily/211X"},
	{ &HP2100.IND_Indirect,			"IND_Indirect",         NULL,                   "CpuFamily/211X"},
	{ &HP2100.IND_Execute,			"IND_Execute",			NULL,                   "CpuFamily/211X"},
	{ &HP2100.IND_Interrupt,		"IND_Interrupt",		NULL,                   "CpuFamily/211X"},
    // HP 2114 only
	{ &HP2100.BTN_Load_Ptr,			"BTN_Load_Ptr",			&HP211X_OnClick_BTN,    "CpuType/2114A"},
	{ &HP2100.BTN_Load_Ptr,			"BTN_Load_Ptr",			&HP211X_OnClick_BTN,    "CpuType/2114B"},
	{ &HP2100.BTN_Clear_SW_Reg,		"BTN_Clear_SW_Reg",		&HP211X_OnClick_BTN,    "CpuType/2114A"},
	{ &HP2100.BTN_Clear_SW_Reg,		"BTN_Clear_SW_Reg",		&HP211X_OnClick_BTN,    "CpuType/2114B"},
    // HP 2100 
	{ &HP2100.Reg_D,				"Reg_D",				&HP2100A_OnClick_Reg_D, "CpuFamily/2100"},
	{ &HP2100.BTN_Loader,			"BTN_Loader",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Internal_Preset,	"BTN_Internal_Preset",	&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_External_Preset,	"BTN_External_Preset",	&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.IND_Fetch,			"IND_Fetch",			NULL,                   "CpuFamily/2100"},
	{ &HP2100.IND_Indirect,			"IND_Indirect",			NULL,                   "CpuFamily/2100"},
	{ &HP2100.IND_Execute,			"IND_Execute",			NULL,                   "CpuFamily/2100"},
	{ &HP2100.IND_Parity,			"IND_Parity",			NULL,                   "CpuFamily/2100"},
	{ &HP2100.BTN_Reg_A,			"BTN_Reg_A",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Reg_B,			"BTN_Reg_B",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Reg_M,			"BTN_Reg_M",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Memory_Data,		"BTN_Memory_Data",		&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Run,				"BTN_Run",				&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Halt,				"BTN_Halt",				&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Step,				"BTN_Step",				&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.IND_Interrupt_System, "BTN_Interrupt_System", &HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.IND_Extend,			"BTN_Extend",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.IND_Overflow,			"BTN_Overflow",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Clear_Display,	"BTN_Clear_Display",	&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Reg_S,			"BTN_Reg_S",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Reg_P,			"BTN_Reg_P",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Dec_M,			"BTN_Dec_M",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Inc_M,			"BTN_Inc_M",			&HP2100A_OnClick_BTN,   "CpuFamily/2100"},
	{ &HP2100.BTN_Power,			"BTN_Power",			&HP2100A_OnClick_BTN,   "CpuType/2100S"},
    // HP 1000
	{ &HP2100.Reg_D,				"Reg_D",				NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Reg_D,             "Sw_Reg_D",				NULL,                   "CpuFamily/1000"},
	{ &HP2100.BTN0_Reg_D,           "BTN0_Reg_D",			&HP1000_OnClick_Reg_D,  "CpuFamily/1000"},
	{ &HP2100.BTN1_Reg_D,           "BTN1_Reg_D",			&HP1000_OnClick_Reg_D,  "CpuFamily/1000"},
	{ &HP2100.BTN_Clear_Display,    "BTN_Clear_Display",	&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Dec_M,            "BTN_Dec_M",			&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Halt,             "BTN_Halt",				&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_IBL,              "BTN_IBL",				&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Inc_M,            "BTN_Inc_M",			&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Left,             "BTN_Left",			    &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Mode,             "BTN_Mode",				&HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Preset,           "BTN_Preset",           &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Right,            "BTN_Right",            &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Run,              "BTN_Run",              &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Step,             "BTN_Step",             &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.BTN_Store,            "BTN_Store",            &HP1000_OnClick_BTN,    "CpuFamily/1000"},
	{ &HP2100.IND_Extend,           "IND_Extend",           NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Interrupt_System, "IND_Interrupt_System", NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Overflow,         "IND_Overflow",         NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_A,            "IND_Reg_A",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_B,            "IND_Reg_B",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_M,            "IND_Reg_M",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_P,            "IND_Reg_P",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_S,            "IND_Reg_S",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Reg_T,            "IND_Reg_T",            NULL,                   "CpuFamily/1000"},
	{ &HP2100.IND_Run,              "IND_Run",              NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Inc_Dec,           "Sw_Inc_Dec",           NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Left_Right,        "Sw_Left_Right",        NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Preset_IBL,        "Sw_Preset_IBL",        NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Run_Halt,          "Sw_Run_Halt",          NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Step_Clear,        "Sw_Step_Clear",        NULL,                   "CpuFamily/1000"},
	{ &HP2100.Sw_Store_Mode,        "Sw_Store_Mode",        NULL,                   "CpuFamily/1000"},
	{ &HP2100.BTN_Power,			"BTN_Power",			&HP1000_OnClick_BTN,    "CpuFamily/1000"},

    { NULL }  
};

// animation state vars 
// for HP2116 panel
int bInterruptIndicator = 0;    // to show on cpanel an interrupt has been requested to CPU

// for HP2100 panel
int Reg_D = 0;                  // display register
int Red_D_selected = 0;         // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
int Red_D_dirty = 0;            // =1 if display register has been changed by user and thus should be saved back to the register it maps

// for HP1000 panel
int mR = 0;                     // memory map register to show/alter
int bSpecialRegistersEnabled=0; // flag to allow access special registers from control panel

// for main cpu
int bCpuModelIs                = 1000; // cpu model being displayed in control panel gui. 
int bShowInfo                  = 0; // flag to show info for ^I 
uint32 ShowInfoTm0             = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                 = 0; // instr executed on ShowInfoTm0
int FramesCount0               = 0; // num of frames on ShowInfoTm0
int MemCycle_nsec              = 0; // memory cycle cpanel cpu model (ised to calc IPS in ShowInfo panel)

// for ptr into ShowInfo panel
char PtrFileName[MAX_PATH+1]   = {0};   // filename.ext of paper tape file currently attached
int32 PtrFileSize = -1;                 // filesize of paper tape file currently being read
int32 PtrFilePos  = -1;                 // filepos into paper tape file currently being read

// Control Panel callbacks
void HP2100_Init(void)
{
    if (IsOption("CpuType/2116A") || IsOption("CpuType/2116B") || IsOption("CpuType/2116C") || 
        IsOption("CpuType/2115A") )  {
        // all HP2116A, HP2116B, HP2116B and HP2115A have different visual in cpanel
        // but has exactly the same lights & switches
        bCpuModelIs=2116; 
        if (IsOption("CpuType/2115A")) {
            MemCycle_nsec=2000; // 2115 has a memory cycle of 2 micro seconds (2000 nano seconds)
        } else {
            MemCycle_nsec=1600; // 2116 has a memory cycle of 1.6 micro seconds (1600 nano seconds)
        }
    } else if (IsOption("CpuType/2114A") || IsOption("CpuType/2114B")) {
       bCpuModelIs=2114; 
       MemCycle_nsec=2000; // 2114 has a memory cycle of 2 micro seconds (2000 nano seconds)
    } else if (IsOption("CpuType/2100A") || IsOption("CpuType/2100S")) {
       bCpuModelIs=2100; 
       // panel init sets SR mapped to Display Register
       Reg_D = SR; 
       Red_D_selected = 5;         // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
       Red_D_dirty = 0;
       MemCycle_nsec=980; // 2100 has a memory cycle of 980 nano seconds
    } else {
       bCpuModelIs=1000; 
       // panel init sets SR mapped to Display Register
       Reg_D = SR; 
       Red_D_selected = 5;         // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
       Red_D_dirty = 0;
       if (IsOption("SpecialReg/1")) {
          bSpecialRegistersEnabled=1; 
       } else {
          bSpecialRegistersEnabled=0; 
       }
       if (IsOption("CpuType/21MXE") || IsOption("CpuType/1000M")) {
          MemCycle_nsec=650; // 21MX and 1000-M have a memory cycle of 650 nano seconds
       } else {
          MemCycle_nsec=350; // 1000-E and 1000-F have a memory cycle of 350 nano seconds
       }
    }

    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }

}

void HP2100_Done(void)
{
}

void HP2100_Reset(void)
{
}

void SetCpuPanelLights_HP2100(int mode); // fwrd ref
void SetCpuPanelLights_HP211X(int mode); // fwrd ref 
void SetCpuPanelLights_HP1000(int mode); // fwrd ref 

void HP2100_TickIntensityCount(void)
{
    if ((bCpuModelIs == 2116) || (bCpuModelIs == 2114)) {
        SetCpuPanelLights_HP211X(0); // set tickcount
    } else if (bCpuModelIs == 2100) {
        SetCpuPanelLights_HP2100(0); // set tickcount
    } else {
        SetCpuPanelLights_HP1000(0); // set tickcount
    }
}

void Refresh_CpuPanel(void)
{
    if ((bCpuModelIs == 2116) || (bCpuModelIs == 2114)) {
        // set register light in cpu panel
        SetCpuPanelLights_HP211X(1); 
    } else if (bCpuModelIs == 2100) {
        // set register light in cpu panel
        SetCpuPanelLights_HP2100(1); 
    } else {
        // set register light in cpu panel
        SetCpuPanelLights_HP1000(1); 
    } 
}

void process_HotKeys(void)
{
    int ncp; 
    char c; 

    // get vptr for current window
    ncp=GetControlInfo(HP2100.CtrlInfoPanel, CINFO_NCP);
    if (cpvid[ncp].vptr_cp != vid_keyb.vptr) return; // key not for this window
    c = vid_keyb.LastKeyPress; // key currently being pressed

    if (c ==  ('i'-'a'+1)) { // Tab/Control-I (^I) has been pressed
        // toggle show Info on GUI 
        vid_keyb.LastKeyPress = 0; // clear key as it is processed
        bShowInfo = (bShowInfo) ? 0:1; // toggle value
        if (bShowInfo==0) {
           // put again background thus removing info from gui
           SetState(HP2100.CtrlInfoPanel, 0); 
        } else {
           // init showinfo measurement
           ShowInfoTm0 = 0;
       }
    }
}


// draw text on given Control Id infopanel 
void ShowInfo_DrawText(int CId, char * buf)
{
    int scale, ww,hh,ww2,hh2, chrsz;
    int ncp; 

    GetControlSurface(CId, 1, &ww, &hh); // get size of control surface where to draw info
    if (GetControlInfo(CId, CINFO_NOSCALE)) {
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
    //  - Copy State 2 (black background) to state 1 (state displayed), creating black box where text will be placed
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

void Refresh_ShowInfo(void) 
{
    char buf[10*100];
    int InstrExecPerSec, fps, n=0; 
    int ips_unit; double ips; 
    double RealHwIPS; 
    uint32 msec; 

    // set dynamic contents of CtrlInfoPanel 

    if (bShowInfo>1) {
        InstrExecPerSec = fps = 0;
    } else {
        InstrExecPerSec = fps = 0;
        msec = Refresh_tnow - ShowInfoTm0; 
        if ((msec > 0) && (msec < 1000)) {
            n = InstrExec - InstrExec0;
            if (n<0) n=0;
            InstrExecPerSec = n * 1000 / msec;
            n = Refresh_Frames_Count  - FramesCount0;
            fps             = n * 1000 / msec;
            if (fps>60) fps=60;
            if (fps<0) fps=0;
        }
        ShowInfoTm0  = Refresh_tnow;
        InstrExec0   = InstrExec; 
        FramesCount0 = Refresh_Frames_Count ; 
    }

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
    //       IPS = Instructions Per Second -> Instr executed each sec (when waiting, does not count on IPS)

    // set the aprox speed of real hardware according to memory cycle time
    if (MemCycle_nsec==0) {
        RealHwIPS = 0;
    } else {
        RealHwIPS = (1000*1000*1000 / MemCycle_nsec) * 2 / 5; // say 2.5 cycles per instr on average
    }

    sprintf(buf, "FPS: %d, Cpu Speed: x%0.1f (%0.1f %s)\n", 
            fps, InstrExecPerSec / RealHwIPS, 
            ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS"); 

    if (PtrFileName[0]) {
        // a paper tape file is attached
        if ((PtrFileSize > 0) && (PtrFilePos >= 0) && (PtrFilePos < PtrFileSize)) {
            sprintf(&buf[strlen(buf)], "PTR: %s (%d%%)\n", 
                PtrFileName, 100 * PtrFilePos / PtrFileSize);
        } else {
            sprintf(&buf[strlen(buf)], "PTR: %s\n", 
                PtrFileName);
        }
    }
    
    ShowInfo_DrawText(HP2100.CtrlInfoPanel, buf);

}

void HP2100_Refresh(void)
{
    // refresh Cpu Panel
    Refresh_CpuPanel();

    // show info
    if (  ((bShowInfo) && ((Refresh_Frames_Count % 8)==0)) || (bShowInfo==2)  ) {
        // show info for ^I (Ctrl I) at 60 FPS / 8 -> 8 FPS
        // if (bShowInfo==2) ips will be shown each frame
        Refresh_ShowInfo();
    }
    // process HotKeys on Control Panel GUI window 
    process_HotKeys();
}

// ========== HP 211X Family Control Panel

int CurrentInstHasIndirectAddressing(void)
{
	uint32 IR; 
	int ind;

	// determine if current instruction has indirect mem reference
	// 15 14 13 12 11 10 09 08      instruction
    //  x <-!= 0->  x  x  x  x      memory reference
	ind = 0;
    IR = ReadW(PR);                                
	if (((IR & 0x7000) != 0) && ((IR & 0x8000) != 0) ) ind = 1;
	return ind;
}


// set cpu panel lights for HP 211X
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
void SetCpuPanelLights_HP211X(int mode)
{
    if (mode==0) {
        TickCount(HP2100.Reg_T, TR);
        TickCount(HP2100.Reg_M, MR);
        if (bCpuModelIs != 2114) {
            TickCount(HP2100.Reg_P, PR);
            TickCount(HP2100.Reg_A, AR);
            TickCount(HP2100.Reg_B, BR);
        }

        TickCount(HP2100.IND_Extend,   E ? 1:0);
        TickCount(HP2100.IND_Overflow, O ? 1:0);
		TickCount(HP2100.IND_Indirect, CurrentInstHasIndirectAddressing());
		TickCount(HP2100.IND_Interrupt, bInterruptIndicator);
        
        return; 
    } 

    SetStateWithIntensity(HP2100.Reg_T, TR);
    SetStateWithIntensity(HP2100.Reg_M, MR);

    if (bCpuModelIs != 2114) {
        SetStateWithIntensity(HP2100.Reg_P, PR);
        SetStateWithIntensity(HP2100.Reg_A, AR);
        SetStateWithIntensity(HP2100.Reg_B, BR);
    }

    SetStateWithIntensity(HP2100.IND_Extend,   E ? 1:0);
    SetStateWithIntensity(HP2100.IND_Overflow, O ? 1:0);

	if (GetState(HP2100.BTN_Preset)) {
		// preset button lighted (has been pressed while cpu halted -> reset requested by user)
        // clear run & halt buttons
		SetState(HP2100.BTN_Run,  0);
		SetState(HP2100.BTN_Halt, 0);
		// clear indicators
		SetState(HP2100.IND_Fetch, 0);
		SetState(HP2100.IND_Indirect, 0);
		SetState(HP2100.IND_Execute, 0);
		SetState(HP2100.IND_Interrupt, 0);

    } else {
    	// set Run/Halt button light depending on CPU running or not
    	SetState(HP2100.BTN_Run,  (sim_is_running) ? 1 : 0);
	    SetState(HP2100.BTN_Halt, (sim_is_running) ? 0 : 1);
    
        // set fetch-indirect-execute instruction pahse indicators
	    if (sim_is_running) {
		    // indicator at half intensity
    		SetState(HP2100.IND_Fetch, 24);
	    	SetStateWithIntensity(HP2100.IND_Indirect, CurrentInstHasIndirectAddressing());
		    SetStateWithIntensity(HP2100.IND_Interrupt, bInterruptIndicator);
		    SetState(HP2100.IND_Execute, 24);
    	} else {
	    	// if cpu halted, lit fetch at full intensity
		    SetState(HP2100.IND_Fetch, 1);
    		SetState(HP2100.IND_Indirect, 0);
	    	SetState(HP2100.IND_Execute, 0);
        }
	}

    // set the switchs according to current Switch Reg contents
	SetState(HP2100.SW_Reg, SR);
}

// handle HP211X Switchs 
void HP211X_OnClick_SW_Reg(void)
{
	int n;
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> toggle switch 
    	// get sw clicked bit number
    	n = CP_Click.CArrayItem;	
	    // update the switch register
	    SR = SR ^ (1 << n);
    }
}

// Handle Button
void HP211X_OnClick_BTN(void)
{
	int buttons[] = {HP2100.BTN_Preset, HP2100.BTN_Run, HP2100.BTN_Halt, 
                     HP2100.BTN_Load_Mem, HP2100.BTN_Load_A, HP2100.BTN_Load_B, HP2100.BTN_Load_Addr, 
                     HP2100.BTN_Disp_Mem, HP2100.BTN_Single_Cycle,
                     HP2100.BTN_Load_Ptr, HP2100.BTN_Clear_SW_Reg,
					 -1};
	int btn, n;

    extern DEVICE cpu_dev; 
    t_value val; 

    if (CP_Click.CId == HP2100.BTN_Power) {
        if (CP_Click.KeyPress_KeyRelease == 1) {    // press the mouse button ...
            SetState(CP_Click.CId, 0);              // ... to press the power button/switch
        } else {
            cpanel_stop_flag = 4; // request cpanel off and quit
        }
        return;
    }

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
	    if (CP_Click.CId == HP2100.SW_Loader) {
		    // toggle the loader enabled/protected switch. Does nothing (not implemented)
	    	SetState(CP_Click.CId, 1-GetState(CP_Click.CId));
    	} else {
        	// on CPU halt: lit pressed button, turn off the others
	        // on CPU running: only active button is halt: lit when pressed, turn off the others
	        if ((sim_is_running==0) || 
		        ((sim_is_running) && (CP_Click.CId == HP2100.BTN_Halt) ) ) {
		        for (n=0;buttons[n]>=0;n++) {
			        btn=buttons[n];
			        SetState(btn, (CP_Click.CId == btn) ? 1 : 0);
		        }
	        }
        }
        return;
    }
    if (CP_Click.CId == HP2100.BTN_Halt) {
        if (sim_is_running == 0) return; // halt while cpu is not running is ignored
        // request a cpu stop, without disabling interactive mode
        cpanel_stop_flag = 2;
        // and set interactive mode
        cpanel_interactive=1; 
	} else if (CP_Click.CId == HP2100.BTN_Load_Ptr) {
		// this button only exists on HP2114
		DoSCP("boot ptr");		
	} else if (CP_Click.CId == HP2100.BTN_Preset) {
        if (sim_is_running) return; // button ignored while cpu is running 
        // issue scp reset
		DoSCP("reset");   
	} else if (CP_Click.CId == HP2100.BTN_Run) {
        if (sim_is_running) return; // button ignored while cpu is running 
        // clear interactive mode
        cpanel_interactive=0; 
		DoSCP("go");   
	} else if (CP_Click.CId == HP2100.BTN_Single_Cycle) {
        if (sim_is_running) return; // button ignored while cpu is running 
		DoSCP("step");
	} else if (CP_Click.CId == HP2100.BTN_Clear_SW_Reg) {
		// this button only exists on HP2114
		SR = 0;
	} else if (CP_Click.CId == HP2100.BTN_Load_A) {
        if (sim_is_running) return; // ignored if cpu is running 
		AR = SR;
	} else if (CP_Click.CId == HP2100.BTN_Load_B) {
        if (sim_is_running) return; // ignored if cpu is running 
		BR = SR;
	} else if (CP_Click.CId == HP2100.BTN_Load_Addr) {
        if (sim_is_running) return; // ignored if cpu is running 
		PR = MR = SR;
	} else if (CP_Click.CId == HP2100.BTN_Load_Mem) {
		//save TR in mem addr given by MR
        if (sim_is_running) return; // ignored if cpu is running 
		TR = SR;
		cpu_dev.deposit(TR,MR,cpu_dev.units,0);
	} else if (CP_Click.CId == HP2100.BTN_Disp_Mem) {
		//read addr MR contenst, save it in TR
        if (sim_is_running) return; // ignored if cpu is running 
		cpu_dev.examine(&val, MR, cpu_dev.units,0);
		TR = val;
	} else if (CP_Click.CId == HP2100.BTN_Load_Ptr) {
		// this button only exists on HP2114
        if (sim_is_running) return; // button ignored while cpu is running 
		DoSCP("boot ptr");		
	} else if (CP_Click.CId == HP2100.BTN_Clear_SW_Reg) {
		// this button only exists on HP2114
        if (sim_is_running) return; // button ignored while cpu is running 
		SR = 0;
	} 
}

// ========== HP 2100 Control Panel

// set cpu panel lights for HP 2100
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
void SetCpuPanelLights_HP2100(int mode)
{
    int n;
    int buttons[] = {HP2100.BTN_Reg_A, HP2100.BTN_Reg_B, HP2100.BTN_Reg_M, HP2100.BTN_Memory_Data, 
                     HP2100.BTN_Reg_S, HP2100.BTN_Reg_P, 
                     -1};

    if (mode==0) {
        // no blinkenlights regs, just fetch/Indirect/Execute, ovf, extend and Interrupt :-(
        TickCount(HP2100.IND_Extend,   E ? 1:0);
        TickCount(HP2100.IND_Overflow, O ? 1:0);
		TickCount(HP2100.IND_Indirect, CurrentInstHasIndirectAddressing());
        TickCount(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);
        return; 
    } 
    
    if (sim_is_running) {
        SetStateWithIntensity(HP2100.IND_Extend,   E ? 1:0);
        SetStateWithIntensity(HP2100.IND_Overflow, O ? 1:0);
		SetStateWithIntensity(HP2100.IND_Indirect, CurrentInstHasIndirectAddressing());
        SetStateWithIntensity(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);

        // phase indicator at half intensity
    	SetState(HP2100.IND_Fetch, 24);
	    SetState(HP2100.IND_Execute, 24);

        // set Run/Halt button light depending on CPU running or not
        SetState(HP2100.BTN_Run,  1);
	    SetState(HP2100.BTN_Halt, 0);

        // set SR as register maped to Display Register
        for (n=0;buttons[n]>=0;n++) SetState(buttons[n], 0);    // clear all indicators ...
        SetState(HP2100.BTN_Reg_S, 1);                          // ... and set Reg_S
        Reg_D=SR; 
        Red_D_dirty=0;
    } else {
        SetState(HP2100.IND_Extend,   E ? 1:0);
        SetState(HP2100.IND_Overflow, O ? 1:0);
		SetState(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);

        if (GetState(HP2100.BTN_Run) == 1) {
            // run indicator was set -> cpu was running on previous refresh ...
            // ... but in this refresh sim_is_running is 0 do it has just been halted
            // set phase indicators for stopped cpu
            SetState(HP2100.IND_Fetch, 1);
	        SetState(HP2100.IND_Indirect, 0);
	        SetState(HP2100.IND_Execute, 0);
            // clear load enable light
            SetState(HP2100.BTN_Loader, 0); 
            // Display Register mapped to TR
            Red_D_selected=3;
            Reg_D= TR;
            Red_D_dirty=0;
        }

        // set Run/Halt button light for CPU not running 
        SetState(HP2100.BTN_Run,  0);
        // if Halt light is off, set to on. 
        // If Halt light is dimmed, increase its intensity (it is flashing)
        if ((n=(int)GetState(HP2100.BTN_Halt))==0) {
            SetState(HP2100.BTN_Halt, 1);
        } else if (n > 1) {
            n=n+10; if (n>31) n=1; 
            SetState(HP2100.BTN_Halt, n);
        }

        // set selected register mapped to Display Register
        for (n=0;buttons[n]>=0;n++) SetState(buttons[n], 0);    // clear all indicators ...
        switch (Red_D_selected) {
            // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
            case 0: {n=HP2100.BTN_Reg_A; break; }
            case 1: {n=HP2100.BTN_Reg_B; break; }
            case 2: {n=HP2100.BTN_Reg_M; break; }
            case 3: {n=HP2100.BTN_Memory_Data; break; }
            case 4: {n=HP2100.BTN_Reg_P; break; }
            case 5: {n=HP2100.BTN_Reg_S; break; }
        }
        SetState(n, 1);                                         // ... and set selected one     
    }
    SetState(HP2100.Reg_D, Reg_D); 

    // if Inc/Dec M illuminated, dim it but not too fast to allow the user to see it
    if ((n=(int)GetState(HP2100.BTN_Inc_M)) > 0) { if (n==1) n=31; n=n-10; if (n<2) n=0; SetState(HP2100.BTN_Inc_M, n); }; 
    if ((n=(int)GetState(HP2100.BTN_Dec_M)) > 0) { if (n==1) n=31; n=n-10; if (n<2) n=0; SetState(HP2100.BTN_Dec_M, n); }; 
}

void HP2100A_OnClick_Reg_D(void) 
{
	int n;
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> toggle display register button light 
    	n = CP_Click.CArrayItem;	
	    // update the display register
        if (sim_is_running) {
            // if cpu is running, Display Register is mapped to SR reg
	        SR = SR ^ (1 << n);
            Reg_D = SR;
            Red_D_dirty=0; // set to zero because Reg_D has just been updated with new SR value
        } else {
	        Reg_D = Reg_D ^ (1 << n);
            Red_D_dirty=1; // Reg_D has been changed, and should be saved back to mapped register
        }
    }
    // unlit clear display button in case it was illuminated
    // not stated in 2100A reference Manual 02100-90001, but seems logical
    SetState(HP2100.BTN_Clear_Display, 0); 
}

// HP2100: save Display Register to selected register 
void Save_Reg_D(void)
{
    extern DEVICE cpu_dev; 
    if (Red_D_dirty == 0) return; // Red_D has not been changed, so nothing to save
    switch (Red_D_selected) {
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
        case 0: {AR=Reg_D; break;}
        case 1: {BR=Reg_D; break;}
        case 2: {MR=Reg_D; break;}
        case 3: {
            // saving to TR also writes data to address MR
            TR=Reg_D; 
            cpu_dev.deposit(TR,MR,cpu_dev.units,0);
            break;
        }
        case 4: {PR=Reg_D; break;}
        case 5: {SR=Reg_D; break;}
    }
    Red_D_dirty=0; // Reg_D saved into mapped Reg
}

// HP2100: set selected register as mapped to Display Register 
void Select_Reg_for_Display(int sel)
{
    t_addr val; 

    if (sim_is_running) return; // button ignored while cpu is running
    if (Red_D_selected != sel) {
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
        // Reg D is not the register to be selected ...
        Save_Reg_D();             // ... so save current display reg value in current selected register ...
        Red_D_selected = sel;   // ... then select the new reg ...
    }
    switch (Red_D_selected) {
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR
        case 0: {Reg_D=AR; break;}
        case 1: {Reg_D=BR; break;}
        case 2: {Reg_D=MR; break;}
        case 3: {
            // reading TR also read data from address MR
    		cpu_dev.examine(&val, MR, cpu_dev.units,0);
	    	Reg_D = TR = val;
            break;
        }
        case 4: {Reg_D=PR; break;}
        case 5: {Reg_D=SR; break;}
    }
    Red_D_dirty=0; // Reg_D has the mapped reg value
}

void HP2100A_OnClick_BTN(void) 
{
    int n, btn; 
	int buttons[] = {HP2100.BTN_Internal_Preset,	HP2100.BTN_External_Preset,	
					 HP2100.BTN_Run, HP2100.BTN_Halt, HP2100.BTN_Step, HP2100.BTN_Clear_Display, 
                     HP2100.BTN_Dec_M, HP2100.BTN_Inc_M,
					 -1};

    if (CP_Click.CId == HP2100.BTN_Power) {
        if (CP_Click.KeyPress_KeyRelease == 1) {    // press the mouse button ...
            SetState(CP_Click.CId, 0);              // ... to turn the front panel key lock
        } else {
            cpanel_stop_flag = 4; // request cpanel off and quit
        }
        return;
    }

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> press button -> set state to 1
        if (CP_Click.CId == HP2100.BTN_Clear_Display) {
            // lit button pressed. This button is allways active
            SetState(CP_Click.CId, 1); 
        } else if (CP_Click.CId == HP2100.BTN_Loader) {
            // toggle button if pressed
            if (sim_is_running) return; // button ignored while cpu is running 
            SetState(CP_Click.CId, 1-GetState(CP_Click.CId)); 
        } else {
          	// on CPU halt: lit pressed button
            // on CPU running: only active button are halt (lit when pressed, turn off the others), also
            //                 display register (toggle when pressed), and clear display (lit while pressed) 
            if ((sim_is_running==0) || 
	            ((sim_is_running) && (CP_Click.CId == HP2100.BTN_Halt) ) ) {
                // clear operating controls pushbutton except the pressed one
		        for (n=0;buttons[n]>=0;n++) {
			        btn=buttons[n];
			        SetState(btn, (CP_Click.CId == btn) ? 1 : 0);
		        }
            }
            return;
        }
    }

    if (CP_Click.CId == HP2100.BTN_Power) {
        cpanel_stop_flag = 4; // request cpanel off and quit
    } else if (CP_Click.CId == HP2100.BTN_Loader) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        // does nothing
    } else if (CP_Click.CId == HP2100.BTN_Internal_Preset) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        SetState(HP2100.IND_Parity, 0); // clear parity indicator
        O=0;                            // clear overflow
   		mp_control = CLEAR;				// disable memory protect
		ion = CLEAR;	                // disable interrupts
        // clear all phase lights
        SetState(HP2100.IND_Fetch, 0);  
        SetState(HP2100.IND_Extend, 0);  
        SetState(HP2100.IND_Execute, 0);  
    } else if (CP_Click.CId == HP2100.BTN_External_Preset) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
		DoSCP("reset");
        // clear all phase lights
        SetState(HP2100.IND_Fetch, 0);  
        SetState(HP2100.IND_Extend, 0);  
        SetState(HP2100.IND_Execute, 0);  
    } else if (CP_Click.CId == HP2100.IND_Interrupt_System) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        // toggle cpu_interrupt_enable
        ion = (ion == SET) ? CLEAR:SET;
    } else if (CP_Click.CId == HP2100.IND_Extend) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        // toggle E flag
        E = E ? 0:1;
    } else if (CP_Click.CId == HP2100.IND_Overflow) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        // toggle O flag
        O = O ? 0:1;
    } else if (CP_Click.CId == HP2100.BTN_Clear_Display) {
        // Clear Display button is allways active
        // if cpu is running, clears S reg (as while cpu is running, Display Reg maps to SR)
        // If cpu is stopped, clears Display Register
        if (sim_is_running) { 
            SR=0;
        } else if (Reg_D) {
            Reg_D=0; 
            Red_D_dirty=1; 
        }
    } else if (CP_Click.CId == HP2100.BTN_Reg_A) {
        // select AR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(0);
    } else if (CP_Click.CId == HP2100.BTN_Reg_B) {
        // select BR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(1);
    } else if (CP_Click.CId == HP2100.BTN_Reg_P) {
        // select PR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(4);
        if (sim_is_running==0) {
            // set fetch phase 
            SetState(HP2100.IND_Fetch, 1);  
            SetState(HP2100.IND_Extend, 0);  
            SetState(HP2100.IND_Execute, 0);  
        }
    } else if (CP_Click.CId == HP2100.BTN_Reg_S) {
        // select SR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(5);
    } else if (CP_Click.CId == HP2100.BTN_Reg_M) {
        // select MR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(2);
    } else if (CP_Click.CId == HP2100.BTN_Memory_Data) {
        // select TR to be mapped in Display register 
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        Select_Reg_for_Display(3);
    } else if (CP_Click.CId == HP2100.BTN_Dec_M) {
        n=-1; 
        goto Inc_Dec_M;
    } else if (CP_Click.CId == HP2100.BTN_Inc_M) {
        n=1; 
       Inc_Dec_M:
        if (sim_is_running) return; // button ignored while cpu is running
        Save_Reg_D(); 
        MR = (MR + n) & 0xffff; // incr/decr current address
        Select_Reg_for_Display(Red_D_selected);
    } else if (CP_Click.CId == HP2100.BTN_Halt) {
        if (sim_is_running) {
            // halt cpu while running
            // request a cpu stop, without disabling interactive mode
            cpanel_stop_flag = 2;
            // and set interactive mode
            cpanel_interactive=1; 
        } else {
            // halt pressed while cpu stopped -> act as cycle button
            Save_Reg_D(); // save current display reg value in selected register
            // advance one phase
            if (GetState(HP2100.IND_Execute) == 1) {
                // next phase is exec -> step the instruction
    	    	DoSCP("step");
                // set next phase as fetch
                SetState(HP2100.IND_Fetch, 1);  
                SetState(HP2100.IND_Extend, 0);  
                SetState(HP2100.IND_Execute, 0);  
            } else {
                // fetch the current instr in TR reg 
                TR = ReadW (PR);                                /* fetch the instruction into TR reg*/
                // and set next phase as exec
                SetState(HP2100.IND_Fetch, 0);  
                SetState(HP2100.IND_Extend, 0);  
                SetState(HP2100.IND_Execute, 1);  
            }
            // load display register with TR 
            Select_Reg_for_Display(3);
            // dim the button intensity to flsh it to user
            SetState(HP2100.BTN_Halt, 10);
        }
    } else if (CP_Click.CId == HP2100.BTN_Run) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
        // clear interactive mode
        cpanel_interactive=0; 
  		DoSCP("go");
    } else if (CP_Click.CId == HP2100.BTN_Step) {
        if (sim_is_running) return; // button ignored while cpu is running 
        Save_Reg_D(); // save current display reg value in selected register
  		DoSCP("step");
        // set next phase as fetch
        SetState(HP2100.IND_Fetch, 1);  
        SetState(HP2100.IND_Extend, 0);  
        SetState(HP2100.IND_Execute, 0);  
        // get instr to be exec in TR
        TR = ReadW (PR);                  
        // and load display register with TR, thus showing instr just stepped 
        Select_Reg_for_Display(3);
    }
}

// ========== HP 1000 Control Panel

// set cpu panel lights for HP 1000
// if mode=0 set TickCount (for tickcount callback)
//         1 set SetStateWithIntensity (for refresh callback)
void SetCpuPanelLights_HP1000(int mode)
{
    int n;
    int indicators[] = {HP2100.IND_Reg_A, HP2100.IND_Reg_B, HP2100.IND_Reg_M, 
                        HP2100.IND_Reg_T, HP2100.IND_Reg_P, HP2100.IND_Reg_S, 
                     -1};

    if (mode==0) {
        // no blinkenlights regs, just fetch/Indirect/Execute, ovf, extend and Interrupt :-(
        TickCount(HP2100.IND_Extend,   E ? 1:0);
        TickCount(HP2100.IND_Overflow, O ? 1:0);
        TickCount(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);
        return; 
    } 
    
    if (sim_is_running) {
        SetStateWithIntensity(HP2100.IND_Extend,   E ? 1:0);
        SetStateWithIntensity(HP2100.IND_Overflow, O ? 1:0);
        SetStateWithIntensity(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);

        // set Run light 
        SetState(HP2100.IND_Run,  1);

        // set SR as register maped to Display Register
        for (n=0;indicators[n]>=0;n++) SetState(indicators[n], 0);  // clear all indicators ...
        SetState(HP2100.IND_Reg_S, 1);                              // ... and set Reg_S
        Reg_D=SR; 
    } else {
        SetState(HP2100.IND_Extend,   E ? 1:0);
        SetState(HP2100.IND_Overflow, O ? 1:0);
		SetState(HP2100.IND_Interrupt_System, (ion == SET) ? 1:0);

        if (GetState(HP2100.IND_Run) == 1) {
            // run indicator was set -> cpu was running on previous refresh ...
            // ... but has just been halted
            // Display Register mapped to TR
            Red_D_selected=3;
            Reg_D=TR;
        }

        // set Run light for CPU not running 
        SetState(HP2100.IND_Run,  0);

        // set selected register mapped to Display Register
        if (Red_D_selected <= 5) {
            // normal register mode
            for (n=0;indicators[n]>=0;n++) SetState(indicators[n], 0);  // clear all indicators ...
        } else {
            // special register mode
            for (n=0;indicators[n]>=0;n++) SetState(indicators[n], 1);  // set all indicators ...
        }
        switch (Red_D_selected % 6) {
            // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
            //                                                      6=XR, 7=YR, 8=m, 9=t, 10=f, 11=status
            case 0: {n=HP2100.IND_Reg_A; break; }
            case 1: {n=HP2100.IND_Reg_B; break; }
            case 2: {n=HP2100.IND_Reg_M; break; }
            case 3: {n=HP2100.IND_Reg_T; break; }
            case 4: {n=HP2100.IND_Reg_P; break; }
            case 5: {n=HP2100.IND_Reg_S; break; }
        }
        if (Red_D_selected <= 5) { SetState(n, 1); } // normal register mode: lit selected one     
        else { SetState(n, 0); } // special register mode: unlit selected one     
    }
    SetState(HP2100.Reg_D, Reg_D); 
}

void HP1000_OnClick_Reg_D(void) 
{
	int n, CId, setbit;

    // CId=control id for clicked switch, n=bit number (0..15)
    n = CP_Click.CArrayItem;	
    CId = GetCArrayCId(HP2100.Sw_Reg_D, n); 

    if (CP_Click.KeyPress_KeyRelease == 2) {
		// release button
		SetState(CId, 0);					// ... set the switch state to neutral
		return;
	}
    // press button
	if (CP_Click.CArrayId == HP2100.BTN1_Reg_D) {
		// click switch to set bit to 1
		SetState(CId, 1);					// ... set the swtich state to "pressed up"
        setbit=1; // we are setting the bit to 1
    } else {
		// click switch to set bit to 0
		SetState(CId, 2);					// ... set the swtich state to "pressed down"
        setbit=0; // we are setting the bit to 0
    }
    // update the display register
    if (sim_is_running) {
        // if cpu is running, Display Register is mapped to SR reg
        SR = SR | (1 << n);                     // set the bit ...
        if (setbit==0) { SR = SR ^ (1 << n); }  // ... and toggle if should be cleared
        Reg_D = SR;
    } else {
        Reg_D = Reg_D | (1 << n);                     // set the bit ...
        if (setbit==0) { Reg_D = Reg_D ^ (1 << n); }  // ... and toggle if should be cleared
    }
}

// HP1000: Store Display Register value into selected register 
void Store_Display_Reg(void)
{
    switch (Red_D_selected) {
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        //                                                      6=XR, 7=YR, 8=m, 9=t, 10=f, 11=status
        case 0: {AR=Reg_D; break;}
        case 1: {BR=Reg_D; break;}
        case 2: {MR=Reg_D; break;}
        case 3: {
            // saving to TR also writes data to address MR
            TR=Reg_D; 
            cpu_dev.deposit(TR,MR,cpu_dev.units,0);
            break;
        }
        case 4: {PR=Reg_D; break;}
        case 5: {SR=Reg_D; break;}
        case 6: {XR=Reg_D; break;}
        case 7: {YR=Reg_D; break;}
        case 8: {
            // DMS MAP: m=current memory map register number (7 bits)
            mR=Reg_D & 127; 
            break;
        } 
        case 9: {
            // DMS MAP: t=current memory map register number
            dms_wmap(mR, Reg_D); 
            break; 
        } 
        case 10: {
            // DMS MAP: f=Memory expansion Module status register
            dms_enb = (Reg_D & MST_ENB) ? 1:0;
            dms_ump = (Reg_D & MST_UMP) ? UMAP:SMAP;
            mp_control = (Reg_D & MST_PRO) ? SET:CLEAR;
            dms_upd_sr();
            break; 
        } 
        case 11: {
            O = (Reg_D & (1 << 15)) ? 1:0; 
            E = (Reg_D & (1 << 14)) ? 1:0; 
			ion = (Reg_D & (1 << 13)) ? SET:CLEAR;
        }
    }
}

// HP1000: Load selected register to Display Register 
void Load_Display_Reg(int sel)
{
    t_addr val; 

    Red_D_selected = sel;       // select the new reg ...
    switch (Red_D_selected) {
        // register mapped to display register when cpu halted. 0=AR, 1=BR, 2=MR, 3=TR, 4=PR, 5=SR, 
        //                                                      6=XR, 7=YR, 8=m, 9=t, 10=f, 11=status
        case 0: {Reg_D=AR; break;}
        case 1: {Reg_D=BR; break;}
        case 2: {Reg_D=MR; break;}
        case 3: {
            // reading TR also read data from address MR
    		cpu_dev.examine(&val, MR, cpu_dev.units,0);
	    	Reg_D = TR = val;
            break;
        }
        case 4: {Reg_D=PR; break;}
        case 5: {Reg_D=SR; break;}
        case 6: {Reg_D=XR; break;}
        case 7: {Reg_D=YR; break;}
        case 8: {
            // DMS MAP: m=current memory map register number (7 bits)
            mR &= 127; 
            Reg_D=mR; 
            break;
        } 
        case 9: {
            // DMS MAP: t=current memory map register number
            Reg_D = (int) dms_rmap(mR);
            break; 
        } 
        case 10: {
            // DMS MAP: f=Memory expansion Module status register
            Reg_D = (int) dms_upd_sr();
            break; 
        }   
        case 11: {
            Reg_D = (O << 15) | (E << 14);
			if (ion == SET) { Reg_D = Reg_D | (1 << 13); }
			Reg_D = Reg_D | (intaddr & 077);                        // intaddr is the CIR register
        }
    }
}

void HP1000_OnClick_BTN(void) 
{
	int sw, n;

    if (CP_Click.CId == HP2100.BTN_Power) {
        if (CP_Click.KeyPress_KeyRelease == 1) {    // press the mouse button ...
            SetState(CP_Click.CId, 1);              // ... to turn the front panel key lock
        } else {
            cpanel_stop_flag = 4; // request cpanel off and quit
        }
        return;
    }

	// switches can be pressed at any time. Allways do the switch animation 
	if (CP_Click.CId == HP2100.BTN_Run)				{ sw = HP2100.Sw_Run_Halt;		n = 1; } else
	if (CP_Click.CId == HP2100.BTN_Halt)			{ sw = HP2100.Sw_Run_Halt;		n = 2; } else 
	if (CP_Click.CId == HP2100.BTN_Preset)			{ sw = HP2100.Sw_Preset_IBL;	n = 1; } else 
	if (CP_Click.CId == HP2100.BTN_IBL)				{ sw = HP2100.Sw_Preset_IBL;	n = 2; } else
	if (CP_Click.CId == HP2100.BTN_Left)			{ sw = HP2100.Sw_Left_Right;	n = 1; } else 
	if (CP_Click.CId == HP2100.BTN_Right)			{ sw = HP2100.Sw_Left_Right;	n = 2; } else
	if (CP_Click.CId == HP2100.BTN_Step)			{ sw = HP2100.Sw_Step_Clear;	n = 1; } else 
	if (CP_Click.CId == HP2100.BTN_Clear_Display)	{ sw = HP2100.Sw_Step_Clear;	n = 2; } else
	if (CP_Click.CId == HP2100.BTN_Inc_M)			{ sw = HP2100.Sw_Inc_Dec;		n = 1; } else 
	if (CP_Click.CId == HP2100.BTN_Dec_M)			{ sw = HP2100.Sw_Inc_Dec;		n = 2; } else
	if (CP_Click.CId == HP2100.BTN_Store)			{ sw = HP2100.Sw_Store_Mode;	n = 1; } else 
	if (CP_Click.CId == HP2100.BTN_Mode)			{ sw = HP2100.Sw_Store_Mode;	n = 2; } 
    else return; // unknown button pressed
    if (CP_Click.KeyPress_KeyRelease == 2) {
		// release the button
		SetState(sw, 0);                    // ... set the switch state to neutral
		return;
	}
	// set switch as pushed on lower or upper half 
	SetState(sw, n);				

    if (CP_Click.CId == HP2100.BTN_Clear_Display) { 
        // Clear Display button is allways active
        // if cpu is running, clears S reg (as while cpu is running, Display Reg maps to SR)
        // If cpu is stopped, clears Display Register
        if (sim_is_running) { SR=0; } else { Reg_D=0; }
	} else if (CP_Click.CId == HP2100.BTN_Run)	{ 
        if (sim_is_running) return; // button ignored while cpu is running 
        // clear interactive mode
        cpanel_interactive=0; 
  		DoSCP("go");
    } else if (CP_Click.CId == HP2100.BTN_Halt) { 
        if (sim_is_running==0) return; // button ignored if cpu is NOT running 
        // halt cpu while running
        // request a cpu stop, without disabling interactive mode
        cpanel_stop_flag = 2;
        // and set interactive mode
        cpanel_interactive=1; 
    } else if (CP_Click.CId == HP2100.BTN_Preset) { 
        if (sim_is_running) return; // button ignored while cpu is running 
        SetState(HP2100.IND_Parity, 0); // clear parity indicator
        O=0;                            // clear overflow
		ion = CLEAR;	// disable interrupts
		DoSCP("reset");       
    } else if (CP_Click.CId == HP2100.BTN_IBL)	{ 
        // IBL/TEST load the initial binary load code from rom to ram
        // SR register holds the select code of device to be used by IBL routine
		if (cpu_dev.boot(0, NULL) != SCPE_OK) {
			O = 1; // error loading IBL from rom. Set overflow to notify
        } else { O = 0; }
    } else if (CP_Click.CId == HP2100.BTN_Step)	{ 
        if (sim_is_running) return; // button ignored while cpu is running 
  		DoSCP("step");
        // get instr to be exec in TR
        TR = ReadW (PR);                  
        // and load display register with TR, thus showing instr just stepped 
        Load_Display_Reg(3);
    } else if (CP_Click.CId == HP2100.BTN_Left)	{ 
        if (sim_is_running) return; // button ignored while cpu is running 
        if (Red_D_selected == 0) { Red_D_selected = 5; } 
        else if (Red_D_selected == 6) { Red_D_selected = 11; } 
        else {Red_D_selected--;}
        Load_Display_Reg(Red_D_selected);
    } else if (CP_Click.CId == HP2100.BTN_Right) { 
        if (sim_is_running) return; // button ignored while cpu is running 
        if (Red_D_selected == 5) { Red_D_selected = 0; } 
        else if (Red_D_selected == 11) { Red_D_selected = 6; } 
        else {Red_D_selected++;}
        Load_Display_Reg(Red_D_selected);
    } else if (CP_Click.CId == HP2100.BTN_Mode) { 
        if (sim_is_running) return; // button ignored while cpu is running 
        if (bSpecialRegistersEnabled) {
            // toggle between Special and normal registers
            if (Red_D_selected <= 5) { Red_D_selected += 6; } else
            if (Red_D_selected >= 6) { Red_D_selected -= 6; } 
        }
        // then reload current selected reg
        Load_Display_Reg(Red_D_selected);
    } else if (CP_Click.CId == HP2100.BTN_Store) { 
        if (sim_is_running) return; // button ignored while cpu is running 
        Store_Display_Reg();
    } else if (CP_Click.CId == HP2100.BTN_Dec_M) { 
        n=-1; 
        goto Inc_Dec_M2;
    } else if (CP_Click.CId == HP2100.BTN_Inc_M) { 
        n=1; 
       Inc_Dec_M2:
        if (sim_is_running) return; // button ignored while cpu is running
        if (Red_D_selected <= 5) { 
            MR = (MR + n) & 0xffff; // incr/decr current address
        } else {
            mR = (mR + n) & 127; // incr/decr current mem map to work with
        }
        Load_Display_Reg(Red_D_selected);
    } 

}


// Matches the ifdef(CPANEL) at beginning of file
#endif

