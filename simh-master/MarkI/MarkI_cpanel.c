/* MarkI_cpanel.c: Ferranti Mark I computer control panel simulation

   Copyright (c) 2023, Roberto Sancho

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

   Jan-23    RSV     Ferranti Mark I control panel support

   This module implements the following control panels:

   Console 
   Printer
   Perforator
   Paper tape Reader
  
*/

// xxx_cpanel.c needs the preprocessor symbol CPANEL to be defined
#if defined(CPANEL)

#include "cpanel.h"
#include "MarkI_defs.h"	        

// cpu registers
extern int                 C;                           // Instruction Number register. 10 bits (also know as register C). Holds 20 bits, but only lower 10 are used as Prog Counter
extern int                 PI;                          // Present Instruction register. 20 bits (also known as Actual Instruction)
extern t_int64             AL;                          // Accumulator Least Significat bits: 40 bits
extern t_int64             AM;                          // Accumulator Most Significat bits: 40 bits
extern t_int64             D;                           // Multiplicand register: 40 bits
extern int                 Dsigned;                     // =1 -> D register is signed value, =0 -> D register is unsigned
extern int                 S;                           // Memory select register: 20 bits
extern int                 B[8];                        // 8 Index register: 20 bits   
extern int                 Qneg;                        // =1 -> B has become negative
extern int                 H;                           // value Hand Switches (20 bits)
extern int                 NCYCLE;                      // indicates current machine cycle to be executed. 

extern uint8               StopReason;                  // saved stop reason to be displayed in control panel
extern int                 CRT_line65[8];               // the 65th line of CRT (Marker Line)

// console ligths and switches that are checked during cpu instruction execution
extern int Console_Sw_DummyStop;                        // console /G (bit 0), /L (bit 1) stop active
extern int Console_Sw_ManualMode;                       // =1 -> manual mode active
extern int Console_Sw_Instruction;                      // instruction set in console switches
extern int Console_Sw_KeyClear;                         // bit0=KAC switch on, bit1=KBC, bit2=KCC, bit3=KDC, bit7=KEC
extern int Console_Sw_PrepulseSpeed;                    // =0 -> full speed, =1 -> execute at a 50 instr/sec rate, =2 -> single prepulse   
extern int Console_Sw_ManKbdPtrToLpt;                   // =0 -> Printer input from computer, =1 -> from Ptr, =2 -> from its own keyboard


extern int     CpuSpeed_Acceleration;       // cpu speed multiplier
extern int     Measure_CpuSpeed(int);       // return measured cpu speed 
extern t_stat  CharIoInstr(int func, int * nBeats); // send a char to printer

// printout state
extern char   lptPrintOut[LPT_COLUMNS * lptPrintOutMAX];
extern int    lptPrintOutCount;
extern int    lptPrintOutChCount; 

CP_DEF Mark1_cp[];			// declare forwards
void Mark1_Init(void);
void Mark1_Done(void);
void Mark1_Reset(void);
void Mark1_Refresh(void);
void Mark1_TickIntensityCount(void);
void Mark1_DropFile(int CId, char * FileName);
void Mark1_OnClick_Sw(void);
void Mark1_OnClick_Tube(void);
void Mark1_OnClick_BTN(void);
void Mark1_OnClick_DarkMode(void); 

// control panel available types
CP_TYPE cp_types = {
     Mark1_cp, 
     &Mark1_Init, &Mark1_Done, &Mark1_Reset, 
     &Mark1_Refresh, &Mark1_TickIntensityCount, &Mark1_DropFile
};

// struct to hold the GUI controls Ids
static struct {
   int CtrlInfoPanel; 
   // controls for photorealistic cpanel
   int ConsolePanel, Background1, TopSwitch, TubesBackground, DarkMode, TypeWriter_Button;
   // Mark1 Console
   int Tube_bit_spot, Tube_bit_spot_spacing, TubeReg[4], TubeStore[2]; 
   // Left and right SW to control Storage Contents to display in left and right tube
   int SW_Storage_L, SW_Storage_R, TubeLbkg, TubeRbkg; 
   // Lower part to console: KEC=Key Everything Clear
   int SW_KEC, SW_MAN_AUTO, SW_Main_Prepulse;
   int SW_Erase_Insert;
   int SW_HAND, SW_ManInstr, BTN_TypeWriter; 
   // Rightmost panel switches (KLC=Key Store Clear; KCS=Key Completition Signal);
   int SW_KAC, SW_KBC, SW_KCC, SW_KDC, SW_KLC, SW_KCS; 
   // switches on right vertical panel
   int SW_HT, SW_WriteCurrent;
    // neon lights
   int LI_Sign_A, LI_Sign_Q, LI_Sign_D, LI_Stop; 
   // printer/punch control
   int SW_Print, SW_TapeStop, SW_PtpLptActive, SW_ManualKbdPtrToLpt; 
   // execution control 
   int SW_PrepulseSpeed, SW_SinglePrepulse, SW_Stop_L, SW_Stop_G; 
   // Creed 7B printer
   int Paper, PaperBackground, PrinterCharSet, PaperSheet, PaperDrum;
   // Creed 6S/7P Perforator
   int ProgSheet, PerforatorWall, PerforatorPanel, Drop_ProgFile;
   // PaperTape reader
   int PaperTape, ReaderTable, ReaderPanel, ReaderTop; 
} Mark1 = {0}; // must be init to zero

// mapping variables that holds the control Id with control name and event handler
// the name identifies the control in the definition file
CP_DEF Mark1_cp[] = {
    // Ferranti Mark 1
    { &Mark1.CtrlInfoPanel,              "CtrlInfoPanel",               NULL},
    { &Mark1.ConsolePanel,               "ConsolePanel",                NULL,   "PhotoConsole/1"},    // Main console placeholder
    { &Mark1.Background1,                "Background1",                 NULL,   "PhotoConsole/1"},    // Main console image
    { &Mark1.TopSwitch,                  "TopSwitch",                   NULL,   "PhotoConsole/1"},    // TopSwitch bitmap, used to draw realistic switch control
    { &Mark1.TypeWriter_Button,          "TypeWriter_Button",           NULL,   "PhotoConsole/1"},
    { &Mark1.TubesBackground,            "TubesBackground",             NULL,   "PhotoConsole/1"},    // Array with Tubes Background for dark mode
    { &Mark1.DarkMode,                   "DarkMode",                    &Mark1_OnClick_DarkMode,   "PhotoConsole/1"},    // click area for dark mode
    { &Mark1.Tube_bit_spot,              "Tube_bit_spot",               NULL},
    { &Mark1.Tube_bit_spot_spacing,      "Tube_bit_spot_spacing",       NULL},
    { &Mark1.TubeReg[0],                 "Tube0",                       NULL},
    { &Mark1.TubeReg[1],                 "Tube1",                       NULL},
    { &Mark1.TubeReg[2],                 "Tube2",                       NULL},
    { &Mark1.TubeReg[3],                 "Tube3",                       NULL},
    { &Mark1.TubeStore[0],               "TubeL",                       NULL},
    { &Mark1.TubeStore[1],               "TubeR",                       NULL},
    { &Mark1.TubeLbkg,                   "TubeLbkg",                    &Mark1_OnClick_Tube},
    { &Mark1.TubeRbkg,                   "TubeRbkg",                    &Mark1_OnClick_Tube},
    { &Mark1.SW_Storage_L,               "SW_Storage_L",                &Mark1_OnClick_Sw}, // this is the ControlArray
    { &Mark1.SW_Storage_R,               "SW_Storage_R",                &Mark1_OnClick_Sw}, // this is the ControlArray
    { &Mark1.SW_KEC,                     "SW_KEC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_MAN_AUTO,                "SW_MAN_AUTO",                 &Mark1_OnClick_Sw},
    { &Mark1.SW_Main_Prepulse,           "SW_Main_Prepulse",            &Mark1_OnClick_Sw},
    { &Mark1.SW_Erase_Insert,            "SW_Erase_Insert",             &Mark1_OnClick_Sw},
    { &Mark1.SW_HAND,                    "SW_HAND",                     &Mark1_OnClick_Sw},
    { &Mark1.SW_ManInstr,                "SW_ManInstr",                 &Mark1_OnClick_Sw},
    { &Mark1.BTN_TypeWriter,             "BTN_TypeWriter",              &Mark1_OnClick_BTN},
    { &Mark1.SW_KAC,                     "SW_KAC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_KBC,                     "SW_KBC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_KCC,                     "SW_KCC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_KDC,                     "SW_KDC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_KLC,                     "SW_KLC",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_KCS,                     "SW_KCS",                      &Mark1_OnClick_Sw},
    { &Mark1.SW_HT,                      "SW_HT",                       NULL}, // not emulated
    { &Mark1.SW_WriteCurrent,            "SW_WriteCurrent",             NULL}, // not emulated
    { &Mark1.SW_TapeStop,                "SW_TapeStop",                 NULL}, // not emulated
    { &Mark1.SW_ManualKbdPtrToLpt,       "SW_ManualKbdPtrToLpt",        &Mark1_OnClick_Sw}, 
    { &Mark1.LI_Sign_A,                  "LI_Sign_A",                   NULL},
    { &Mark1.LI_Sign_Q,                  "LI_Sign_Q",                   NULL},
    { &Mark1.LI_Sign_D,                  "LI_Sign_D",                   NULL},
    { &Mark1.LI_Stop,                    "LI_Stop",                     NULL},
    { &Mark1.SW_Print,                   "SW_Print",                    &Mark1_OnClick_Sw},
    { &Mark1.SW_PtpLptActive,            "SW_PtpLptActive",             &Mark1_OnClick_Sw},
    { &Mark1.SW_PrepulseSpeed,           "SW_PrepulseSpeed",            &Mark1_OnClick_Sw},
    { &Mark1.SW_SinglePrepulse,          "SW_SinglePrepulse",           &Mark1_OnClick_Sw},
    { &Mark1.SW_Stop_L,                  "SW_Stop_L",                   &Mark1_OnClick_Sw},
    { &Mark1.SW_Stop_G,                  "SW_Stop_G",                   &Mark1_OnClick_Sw},
    // Creed 7B Printer
    { &Mark1.Paper,                     "Paper",                        NULL, "Printer/1"  },
    { &Mark1.PaperBackground,           "PaperBackground",              NULL, "Printer/1"  },
    { &Mark1.PrinterCharSet,            "PrinterCharSet",               NULL, "Printer/1"  },
    { &Mark1.PaperSheet,                "PaperSheet",                   NULL, "Printer/1"  },
    { &Mark1.PaperDrum,                 "PaperDrum",                    NULL, "Printer/1"  },
    // Creed 6S/7P Perforator
    { &Mark1.PerforatorWall,           "PerforatorWall",                NULL, "Reader/1" },
    { &Mark1.PerforatorPanel,          "PerforatorPanel",               NULL, "Reader/1" },
    { &Mark1.ProgSheet,                 "ProgSheet",                    NULL, "Reader/1" },
    { &Mark1.Drop_ProgFile,             "Drop_ProgFile",                NULL, "Reader/1" },
    // PaperTape reader
    { &Mark1.ReaderTable,               "ReaderTable",                  NULL, "Reader/1" },
    { &Mark1.ReaderPanel,               "ReaderPanel",                  NULL, "Reader/1" },
    { &Mark1.PaperTape,                 "PaperTape",                    NULL, "Reader/1" },
    { &Mark1.ReaderTop,                 "ReaderTop",                    Mark1_OnClick_Sw, "Reader/1" },
    { NULL }  
};

int bPrintOutVisible;                   // printer visible
int bReaderPerforatorVisible;           // paper tape reader and perforators visible
int bDarkMode;                          // =1 -> ceiling lights turned off
int bAutoCodePerforator;                // =1 -> using Creed7 as brooker autocode perforator

// animation state vars (for dynamic state draw)

// for main cpu
int CpuSpeed_Acceleration_save    = 0; // save value during HotKey ^F Max Speed
int bShowInfo                     = 0; // flag to show info for ^I 
uint32 ShowInfoTm0                = 0; // last sim_os_msec() of ShowInfo display
int InstrExec0                    = 0; // instr executed on ShowInfoTm0
t_int64 BeatsExec0                = 0; // instr executed on ShowInfoTm0
int FramesCount0                  = 0; // num of frames on ShowInfoTm0
int bMainPrepulseToOff            = 0; // indicates that user has operated Main Prepulse from active to inactive

int Reg_H                         =-1; // cache shown value for H in cpanel
int Reg_LVisibleStoreTube         = 0; // by default, left tube displays Storage S0
int Reg_RVisibleStoreTube         = 4; // by default, left tube displays Storage S4 


// for printer printout
int lptPrintOutDoneCount       = -1; // number of lines already printed on cpanel paper
int hPaperBackgroundOffset;          // offset of background image on paper image
void Refresh_PrinterSheet(int bShouldUpdatePrintedPage); 

// draw switch taking care of trasparent pixels and antialiasing
// Mode will be =1 for first and last CopyControlImage call, =0 on the remaining calls
// nd = float value from 0 to 0.99999 = subpixel x value
void CopyControlImage2(int FromCId, int FromState, int x0, int y0, int w, int h, 
                       int ToCId,   int ToState,   int x1, int y1, 
                       double nd, int Mode)
{
    uint32 * surface0;
    uint32 * surface1;
    uint32 col,col1,col2; 
    int p0,p1,x,y,h0,w0,h1,w1;
    int rr,gg,bb,r1,g1,b1,r2,g2,b2;
    surface0 = GetControlSurface(FromCId, FromState, &w0, &h0);
    surface1 = GetControlSurface(ToCId, ToState, &w1, &h1);

    for(y=0;y<h;y++) {
        if (y0+y <  0) {y=-y0-1; continue;}
        if (y0+y >=h0) break;
        if (y1+y <  0) {y=-y1-1; continue;}
        if (y1+y >=h1) break;
        p0=x0+(y0+y)*w0;
        p1=x1+(y1+y)*w1;


        for (x=1;x<w-1;x++) {
           if (x0+x <  0) {x=-x0-1; continue;}
           if (x0+x >=w0) break;
           if (x1+x <  0) {x=-x1-1; continue;}
           if (x1+x >=w1) break;
           col1 = surface0[p0+x-1]; 
           col2 = surface0[p0+x  ]; 
           if ((col1==0) && (col2==0)) continue; // source pixel color is transparent
           if (Mode==1) { // processing first or last image
           } else {
               // procesing remaining images. Only allow antialiasing with background on leftmost and rightmost border of image
               if ((col1==0) && (x!=1)) continue; 
           }
           if (col1==0) col1=surface1[p1+x-1]; 
           if (col2==0) col2=surface1[p1+x  ]; 
           get_surface_rgb_color(col1, &r1, &g1, &b1);   
           get_surface_rgb_color(col2, &r2, &g2, &b2);   
           rr = (int) ((r1 - r2) * nd + r2);             // calc the color for antialiased pixel
           gg = (int) ((g1 - g2) * nd + g2);
           bb = (int) ((b1 - b2) * nd + b2);
           col = surface_rgb_color(rr, gg, bb);
           surface1[p1+x]=col;
        }
    }
}

void Draw_TypeWriter(int CId, int number)
{
    double dx0,dx,xx; 
    int ww,hh,x0,y0,y1,xbase; 
    int nState; 
    if (CId < 1) return; // safety: invalid control id

    xbase=GetControlInfo(Mark1.ConsolePanel, CINFO_X); // where to draw console panel bitmap
    x0=GetControlInfo(CId, CINFO_X);  
    y0=GetControlInfo(CId, CINFO_Y); 

    // x range from 100 to 1500: if x=100 -> x1=-4, if x=800 -> x1=0, if x=1500 -> x1=+4
    dx0=2.0; 
    dx0=dx0*(x0-800-xbase)/700.0; 
    ww=GetControlInfo(CId, CINFO_W); 
    hh=GetControlInfo(CId, CINFO_H); 
    for (nState=0; nState<6; nState++) {
        // copy background
        CopyControlImage(Mark1.Background1, 0,   x0-xbase, y0, 0, 0,         // FromCId, FromState, x0, y0, w, h, 
                         CId, nState,    0, 0);        // ToCId, ToState,     x1, y1
    }

    for (nState=0; nState<2; nState++) {

        x0=3; 
        if (nState==0) {y0=5; y1=0; }
        else           {y0=9; y1=4; }
        xx=x0; dx = dx0 / (abs(y1-y0));  
        if (nState==0) xx=xx+dx0; 

        while (1) {
            CopyControlImage(Mark1.TypeWriter_Button, number,   1, 0, 29, 50,  // FromCId, FromState, x0, y0, w, h, nd
                             CId, nState,                     (int)(xx), y0);  // ToCId, ToState,     x1, y1
            if (y0==y1) break;
            if (y0<y1) y0++; else y0--; 
            xx=xx+dx;
        }
    }
}

#define SW_Color_Black   0
#define SW_Color_Red     1
#define SW_Color_Grey    2
#define SW_Color_Yellow  3
#define SW_Color_Green   4
#define SW_Color_Blue    5

// draw colored switch on given control CId for photorealistic panel
void Draw_Switch(int CId, int SwType, int SW_Col)
{
    double dx0,dx,xx; 
    int ww,hh,x0,y0,y1,n,flag,xbase; 
    int nState, FromState1; 
    int FromState, Mode;             
                              // 0 1 2 3 4 <- states. State 5 gets same values as state 3
                              // down .. up      // the number indicates the top switch bitmap to use for each state 0..5
    int nSwitchTopState[]    = { 0,0,0,2,3,      // lower console switch, two positions (0=down, 4=up)
                                 3,2,0,1,0,      // upper console switch, two positions (0=down, 4=up)
                                 3,2,0,1,3,      // upper console switch, three positions (0=down, 2=center, 4=up)
                                 0,0,3,6,7       // lower console switch, three positions (0=down, 2=center, 4=up)
                               }; 

                              // down .. up        // the number indicates the y pos of top switch bitmap in each state 0..5
    int y1SwitchTop[] =        { 50,45,35,25,15,             
                                 70,60,40,50,40, 
                                 70,60,40,30,10,
                                 52,47,17,10,4
                               };
                              // down .. up        // the number indicates the y pos of base switch bitmap in each state 0..5
    int y0Switchbase[] =       { 50,48,44,40,36,
                                 50,45,40,45,40,
                                 50,45,40,35,30,
                                 52,50,38,34,32
                               };
                                 

    if (CId < 1) return; // safety: invalid control id
    flag=(SwType >> 8); // extract flags -> hadle special drawings: 1 & 2 for visible tube selection switches that has the light on left/right
    SwType &= 255;      //                                          3 for Auto/man switch that is a 3-pos switch, with pos 3 being drawn as pos 2

    xbase=GetControlInfo(Mark1.ConsolePanel, CINFO_X); // where to draw console panel bitmap
    // copy background under switch to switch state bitmap
    x0=GetControlInfo(CId, CINFO_X);  
    y0=GetControlInfo(CId, CINFO_Y); 
    // x range from 100 to 1500: if x=100 -> x1=-4, if x=800 -> x1=0, if x=1500 -> x1=+4
    dx0=4.0; 
    dx0=dx0*(x0-800-xbase)/700.0; 
    ww=GetControlInfo(CId, CINFO_W); 
    hh=GetControlInfo(CId, CINFO_H); 
    for (nState=0; nState<6; nState++) {
        // control 0, state 0 -> is the main window background, switch lights off
        // control 0, state 1 -> is the main window background, switch lights on
        n=0; 
        if (SwType>=2) {                            // 3-pos switches
            if (nState == 0) n=1; else              // state 0 (switch down) has lower light set 
            if (nState == 4) n=2;                   // state 4 (switch up) has upper switch light 
            if ((flag==3) && (nState == 2)) n=2;    // flag=2 -> lit upper light on state 2
        } else {                                    // 2-pos switches:
            if (nState == 0) n=2;                   // state 0 has upper switch light set (hence n=1 to take state 1 from background = Img1)
        }
        // copy switch background
        CopyControlImage(Mark1.Background1, 0,   x0-xbase, y0, 0, 0,         // FromCId, FromState, x0, y0, w, h, 
                         CId, nState,    0, 0);        // ToCId, ToState,     x1, y1
        if (n==1) { // if needed, copy lower light from background state 1
            CopyControlImage(Mark1.Background1, 1,   x0-xbase, y0+hh/2, ww, hh/2,    // FromCId, FromState, x0, y0, w, h, 
                             CId, nState,          0, hh/2);   // ToCId, ToState,     x1, y1
        } else if (n==2) { // if needed, copy upper light from background state 1
            CopyControlImage(Mark1.Background1, 1,   x0-xbase, y0, ww, hh/2,         // FromCId, FromState, x0, y0, w, h, 
                             CId, nState,      0,    0);       // ToCId, ToState,     x1, y1
        }
    }

    FromState1 = SW_Col * 8; // select switch top according to color

    for (nState=0; nState<6; nState++) {
        n=(nState==5) ? 3:nState; 
        FromState = FromState1 + nSwitchTopState[SwType * 5 + n]; 
        y1=y1SwitchTop[SwType * 5 + n]; 
        y0=y0Switchbase[SwType * 5 + n]; 
        if (SwType==0) x0=8; else x0=6; 
        if (flag==1) {y0 -= 40; y1 -= 40;}         // flag = 1 -> move switch 40 pixels up
        if (flag==2) {y0 -= 40; y1 -= 40; x0=27; } // flag = 2 -> move switch 40 pixels up

        if (y1==y0) y0++; 
        xx=x0; dx = dx0 / (abs(y1-y0));  

        Mode=1; // will be =1 for first and last CopyControlImage call, =0 on the remaining calls
        while (1) {
            if (y0==y1) Mode=1; 
            CopyControlImage2(Mark1.TopSwitch, FromState, 0, 0, 28, 22, // FromCId, FromState, x0, y0, w, h,
                                   CId, nState,                (int)(xx), y0,
                                   xx-((int)xx), Mode);  // ToCId, ToState,     x1, y1
            if (y0==y1) break;
            if (y0<y1) y0++; else y0--; 
            xx=xx+dx;
            Mode=0; 
        }
    }
    if (flag==3) {
        // flag 3 -> copy switch state 2->4, 1->3, 1->5
        CopyControlImage(CId, 2, 0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                         CId, 4,       0, 0);  // ToCId, ToState,     x1, y1
        CopyControlImage(CId, 1, 0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                         CId, 3,       0, 0);  // ToCId, ToState,     x1, y1
        CopyControlImage(CId, 1, 0, 0, 0, 0,   // FromCId, FromState, x0, y0, w, h, 
                         CId, 5,       0, 0);  // ToCId, ToState,     x1, y1
    }
}

void Draw_Switchs(void) 
{
    int n; 

    Draw_Switch(Mark1.SW_PrepulseSpeed, 1,  SW_Color_Black); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_SinglePrepulse, 1, SW_Color_Grey); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_Stop_L, 1,         SW_Color_Black); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_Stop_G, 1,         SW_Color_Black); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_HT, 1,             SW_Color_Black); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_WriteCurrent, 1,   SW_Color_Black); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_Print, 1,          SW_Color_Grey); // upper console 2 pos switch
    Draw_Switch(Mark1.SW_PtpLptActive, 2,   SW_Color_Grey); // upper console 3-pos switch
    Draw_Switch(Mark1.SW_TapeStop, 1,       SW_Color_Grey); 
    Draw_Switch(Mark1.SW_ManualKbdPtrToLpt, 2, SW_Color_Grey); // upper console 3-pos switch

    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_L, 0), (1<<8) + 1, SW_Color_Green);  // upper console, 2-pos switch 0 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_L, 1), (1<<8) + 1, SW_Color_Yellow);  // upper console, 2-pos switch 1 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_L, 2), (1<<8) + 1, SW_Color_Blue);  // upper console, 2-pos switch 2 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_R, 0), (2<<8) + 1, SW_Color_Green);  // upper console, 2-pos switch 0 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_R, 1), (2<<8) + 1, SW_Color_Yellow);  // upper console, 2-pos switch 1 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_Storage_R, 2), (2<<8) + 1, SW_Color_Blue);  // upper console, 2-pos switch 2 of Array

    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 0), 0, SW_Color_Yellow);  // lower console, 2-pos switch 0 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 1), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 2), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 3), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 4), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 5), 0, SW_Color_Red);  // lower console, 2-pos switch 5 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 6), 0, SW_Color_Red);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 7), 0, SW_Color_Red);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 8), 0, SW_Color_Red);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND, 9), 0, SW_Color_Red);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,10), 0, SW_Color_Green);  // lower console, 2-pos switch 5 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,11), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,12), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,13), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,14), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,15), 0, SW_Color_Blue);  // lower console, 2-pos switch 5 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,16), 0, SW_Color_Blue);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,17), 0, SW_Color_Blue);  
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,18), 0, SW_Color_Blue);
    Draw_Switch( GetCArrayCId(Mark1.SW_HAND,19), 0, SW_Color_Blue);  

    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 0), 0, SW_Color_Green);  // lower console, 2-pos switch 0 of Array
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 1), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 2), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 3), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 4), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 5), 0, SW_Color_Green);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 6), 0, SW_Color_Black);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 7), 0, SW_Color_Black);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 8), 0, SW_Color_Black);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr, 9), 0, SW_Color_Black);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,10), 0, SW_Color_Blue);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,11), 0, SW_Color_Blue);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,12), 0, SW_Color_Blue);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,13), 0, SW_Color_Black);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,14), 0, SW_Color_Yellow); 
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,15), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,16), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,17), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,18), 0, SW_Color_Yellow);  
    Draw_Switch( GetCArrayCId(Mark1.SW_ManInstr,19), 0, SW_Color_Yellow);  

    Draw_Switch(Mark1.SW_KEC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_MAN_AUTO, (3<<8) + 3, SW_Color_Black);  // lower console, 3-pos switch, black colored
    Draw_Switch(Mark1.SW_Main_Prepulse, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_Erase_Insert, 3, SW_Color_Red);  // lower console, 3-pos switch, red colored
    Draw_Switch(Mark1.SW_KAC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_KBC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_KCC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_KDC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_KLC, 0, SW_Color_Red);  // red
    Draw_Switch(Mark1.SW_KCS, 0, SW_Color_Red);  // red

    for(n=0; n<20; n++) {
        Draw_TypeWriter(GetCArrayCId(Mark1.BTN_TypeWriter, n), n);
    }
}


void SetDarkMode(int n)
{
    int ww, hh,xbase; 

    // Copy Main Console Bitmap to Background (control CId=0)
    bDarkMode=n; 
    ww=GetControlInfo(Mark1.Background1, CINFO_W); 
    hh=GetControlInfo(Mark1.Background1, CINFO_H); 
    xbase=GetControlInfo(Mark1.ConsolePanel, CINFO_X); // where to draw console panel bitmap
    CopyControlImage(Mark1.Background1, 0,   0,0,ww,hh,    // FromCId, FromState, x0, y0, w, h, 
                     0, 0,         xbase, 0);     // ToCId, ToState,     x1, y1
    // if Dark Mode set, ... 
    if (n==1) {
        // ... copy dark wall/table landscape from background1, state 1, left side
        CopyControlImage(Mark1.Background1, 1,   0,0,70,hh,       // FromCId, FromState, x0, y0, w, h, 
                         0, 0,          xbase, 0);      // ToCId, ToState,     x1, y1
        // ... copy dark wall/table landscape from background1, state 1, right side
        CopyControlImage(Mark1.Background1, 1,   1620,0,80,hh,    // FromCId, FromState, x0, y0, w, h, 
                         0, 0,          xbase+1620, 0); // ToCId, ToState,     x1, y1
        // ... copy dark wall/table landscape from background1, state 1, table base
        CopyControlImage(Mark1.Background1, 1,   0,1130,1700,100, // FromCId, FromState, x0, y0, w, h, 
                         0, 0,           xbase,0+1130);  // ToCId, ToState,     x1, y1
        // ... copy tubes black support
        CopyControlImage(Mark1.Background1, 1, 190,130,970,640,   // FromCId, FromState, x0, y0, w, h, 
                         0, 0,       xbase+190,0+130); // ToCId, ToState,     x1, y1
    }
    // if printer visible, extend wall & table color 
    if (bPrintOutVisible) {
        uint32 * surface0; 
        uint32 col; 
        int x,y,w0,h0; 
        surface0=GetControlSurface(0, 0, &w0, &h0);
        for(y=0;y<hh;y++) {
            for(x=xbase+ww-1;x<w0;x++) {
                col=surface0[xbase+1 + y * w0];  // copy color of leftmost pixel of gui
                surface0[x + y * w0]=col;  // to background under printer
            }
        }
        for(y=0;y<hh;y++) {
            for(x=0;x<=xbase;x++) {
                col=surface0[xbase+1 + y * w0];  // copy color of leftmost pixel of gui
                surface0[x + y * w0]=col;  // to background under printer
            }
        }
        Refresh_PrinterSheet(1); // force draw paper sheet with new colors
    }
    if (bReaderPerforatorVisible) {
       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.PerforatorWall, bDarkMode);        
       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.PerforatorPanel, 0);        
       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.ProgSheet, GetState(Mark1.ProgSheet)); 

       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.ReaderTable, bDarkMode);        
       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.ReaderPanel , 0);        
       cpanel_ControlRedrawNeeded=1; 
       SetState(Mark1.PaperTape, GetState(Mark1.PaperTape)); 
    }
    // set tube glass image (with or without reflections)
    SetState(Mark1.TubeLbkg, n); 
    SetState(Mark1.TubeRbkg, n); 
    AllControlRedrawNeeded(0); 

    return;
}

// Control Panel callbacks
void Mark1_Init(void)
{
    bDarkMode=0; 
    bPrintOutVisible=0; 
    bReaderPerforatorVisible=0;
    bAutoCodePerforator=0; 

    if (IsOption("Perforator/Creed7")) {
        bAutoCodePerforator = 1;
    }
    if (IsOption("Printer/1")) {
        bPrintOutVisible = 1;
    }
    if (IsOption("Reader/1")) {
        bReaderPerforatorVisible = 1;
    }
    if (IsOption("PhotoConsole/1")) {
        Draw_Switchs(); 
        if (IsOption("Dark")) {
            SetDarkMode(1); // should be called after setting bPrintOutVisible
        } else {
            SetDarkMode(0); 
        }
    }; 
    if (IsOption("ShowInfo")) {
        bShowInfo=1;
        ShowInfoTm0 = 0;
    } else {
        bShowInfo=0;
    }    
}

void Mark1_Done(void)
{
}

void Mark1_Reset(void)
{
}

// refresh upper console small tubes 0..3, with register data: up to 8 20-bits value 
// -1 indicates end of values, 1<<30 indicates an vertical space
void Refresh_TubeReg(int nTube, int * data)
{
    static int current_data[4*16]    = {0};    // current values displayed 
    static int PARAM_SpotW = 3; 
    static int PARAM_SpotH = 3; 
    static int PARAM_WSpace = 5; // horizontal separation between bits
    static int PARAM_HSpace = 5; // vertical separation between lines
    static uint32 * surfaceBit0 = NULL; 
    static uint32 * surfaceBit1 = NULL; 
    static int initFlag = 0; 
    uint32 * surface; 
    int ww, hh, x, y, n, d, ncp, nLin0, nLin1, nLin, rr,gg,bb, nSep; 
    int * cur_data; 
    uint32 * surfaceBit; 

    // init if first call
    if (initFlag == 0) {
        initFlag=1; 
        for (n=0;n<4*16;n++) current_data[n]=-1; 
        // get tube bit spot bitmap
        surfaceBit0=GetControlSurface(Mark1.Tube_bit_spot, 0, &PARAM_SpotW, &PARAM_SpotH); 
        surfaceBit1=GetControlSurface(Mark1.Tube_bit_spot, 1, &PARAM_SpotW, &PARAM_SpotH); 
        // get spacing
        PARAM_WSpace=GetControlInfo(Mark1.Tube_bit_spot_spacing, CINFO_W);
        PARAM_HSpace=GetControlInfo(Mark1.Tube_bit_spot_spacing, CINFO_H);
    }
    // get surface of cpanel
    ncp=GetControlInfo(Mark1.TubeReg[nTube], CINFO_NCP);
    surface=get_surface(ncp, &ww, &hh);
    // get pos of tube image on cpanel surface
    // we will not draw on any control. We will draw directly on cpanel surface
    x=GetControlInfo(Mark1.TubeReg[nTube], CINFO_X);
    y=GetControlInfo(Mark1.TubeReg[nTube], CINFO_Y);
    surface=surface + x + y*ww; 
    // if first pixel of surface at top-left pos of tube control is white this means 
    // background has been redraw. thus should init current data so all pixels will also be redraw
    get_surface_rgb_color(surface[0], &rr, &gg, &bb); 
    if ((rr==255) && (gg==255) && (bb==255)) {
        for (n=0;n<4*16;n++) current_data[n]=-1; 
    }
    // point to current data displayed
    cur_data = &current_data[nTube * 16];
    nLin0=-1; nLin1=-1; nLin=0; nSep=0; 
    while(*data >= 0) {
        if (*data == (1<<30)) {
            // leeave blank line as vertical separation (half line space)
            surface -= ww*(PARAM_HSpace/2);
            nSep++; 
        } else if (*data == *cur_data) {
            // this 20bit line has not changed
        } else {
            // this 20bit line has changed -> must redraw
            if (nLin0 < 0) nLin0=nLin; 
            nLin1=nLin; 
            // draw bits
            d=*cur_data=*data; 
            for (n=0; n<20; n++) {
                surfaceBit = (d & 1) ? surfaceBit1 : surfaceBit0; 
                for (x=0; x<PARAM_SpotW; x++) for (y=0; y<PARAM_SpotH; y++) {
                    surface[x + (n + n/5) * PARAM_WSpace + y * ww]=surfaceBit[x + y*PARAM_SpotW]; 
                }
                d=d>>1; 
            }
        }
        // next short line
        nLin++; cur_data++; data++; surface += ww*PARAM_HSpace;
    }
    // add rectangle so drawn pixels are refreshed on GUI
    if (nLin0<0) {
        // nothing modified. Just exit
    } else {
        int x,y,ww,hh; 
        x=GetControlInfo(Mark1.TubeReg[nTube], CINFO_X);
        y=GetControlInfo(Mark1.TubeReg[nTube], CINFO_Y) + nLin0 * PARAM_HSpace;
        ww=24*PARAM_WSpace; 
        hh = (nLin1-nLin0+1) * PARAM_HSpace + nSep * (PARAM_HSpace/2); 
        cpvid_AddRectList(ncp, x,y,ww,hh,0); 
    }
}

// refresh console big tubes 0..1, with storage mem data 0..7
void Refresh_TubeStore(int nTube, int nStore)
{
    static int current_data[2*65]    = {0};    // current values displayed 
    static int PARAM_SpotW = 3; 
    static int PARAM_SpotH = 3; 
    static int PARAM_WSpace = 5; // horizontal separation between bits
    static int PARAM_HSpace = 5; // vertical separation between lines
    static uint32 * surfaceBit0 = NULL; 
    static uint32 * surfaceBit1 = NULL; 
    static int initFlag = 0; 
    uint32 * surface; 
    uint32 * surfaceBase; 
    int * cur_data;
    int ww, hh, x, y, n, d, ncp, y0, y1, nLin, rr,gg,bb, nCol, data; 
    uint32 * surfaceBit; 

    // init if first call
    if (initFlag == 0) {
        initFlag=1; 
        for (n=0;n<2*65;n++) current_data[n]=-1; 
        // get tube bit spot bitmap
        surfaceBit0=GetControlSurface(Mark1.Tube_bit_spot, 0, &PARAM_SpotW, &PARAM_SpotH); 
        surfaceBit1=GetControlSurface(Mark1.Tube_bit_spot, 1, &PARAM_SpotW, &PARAM_SpotH); 
        // get spacing
        PARAM_WSpace=GetControlInfo(Mark1.Tube_bit_spot_spacing, CINFO_W);
        PARAM_HSpace=GetControlInfo(Mark1.Tube_bit_spot_spacing, CINFO_H);
    }
    // get surface of cpanel
    ncp=GetControlInfo(Mark1.TubeStore[nTube], CINFO_NCP);
    surface=get_surface(ncp, &ww, &hh);
    // get pos of tube image on cpanel surface
    // we will not draw on any control. We will draw directly on cpanel surface
    x=GetControlInfo(Mark1.TubeStore[nTube], CINFO_X);
    y=GetControlInfo(Mark1.TubeStore[nTube], CINFO_Y);
    surface=surface + x + y*ww; 
    // if first pixel of surface at top-left pos of tube control is white this means 
    // background has been redraw. thus should init current data so all pixels will also be redraw
    get_surface_rgb_color(surface[0], &rr, &gg, &bb); 
    if ((rr==255) && (gg==255) && (bb==255)) {
        for (n=0;n<2*65;n++) current_data[n]=-1; 
    }
    // point to current data displayed
    surfaceBase = surface; 
    for (nCol=0; nCol<2; nCol++) {
        y0=-1; y1=-1;
        surface = surfaceBase; 
        for (nLin=-1; nLin<32; nLin++) {
            if (nLin < 0) {
                if (nCol==0) {
                    data=CRT_line65[nStore]; // 65th line
                    cur_data = &current_data[nTube * 65 + 64]; 
                } else data = -1; 
            } else {
                data=CRT[nStore * 64 + nCol * 32 + nLin]; 
                cur_data = &current_data[nTube * 65 + nCol * 32 + nLin]; 
            }
            if ((data < 0) || (data == *cur_data)) {
                // this 20bit line has not changed
            } else {
                // this 20bit line has changed -> must redraw
                if (y0 < 0) y0=(surface - surfaceBase) / ww; 
                // draw bits
                d=*cur_data=data; 
                for (n=0; n<20; n++) {
                    surfaceBit = (d & 1) ? surfaceBit1 : surfaceBit0; 
                    for (x=0; x<PARAM_SpotW; x++) for (y=0; y<PARAM_SpotH; y++) {
                        surface[x + (n + n/5) * PARAM_WSpace + y * ww]=surfaceBit[x + y*PARAM_SpotW]; 
                    }
                    d=d>>1; 
                }
            }
            // next short line
            surface += ww*PARAM_HSpace;
            if (nLin >=0) {
                if ((nLin & 3)==3) surface += ww*(PARAM_HSpace/3);
                if ((nLin & 15)==15) surface += ww*(PARAM_HSpace/2);
            }
            y1=(surface - surfaceBase) / ww; 
        }
        // add rectangle so drawn pixels are refreshed on GUI
        if (y0<0) {
            // nothing modified. Just exit
        } else {
            int x,y,ww,hh; 
            x=GetControlInfo(Mark1.TubeStore[nTube], CINFO_X) + nCol * 25*PARAM_WSpace;
            y=GetControlInfo(Mark1.TubeStore[nTube], CINFO_Y) + y0; 
            ww=24*PARAM_WSpace; 
            hh = (y1-y0+1); 
            cpvid_AddRectList(ncp, x,y,ww,hh,0); 
        }
        surfaceBase += 25*PARAM_WSpace; 
    }
}


void Mark1_TickIntensityCount(void)
{
    int n; 
    // refresh callback the intensity on light on each bits
    TickCount(Mark1.LI_Sign_A, ( AM >> 39) ? 1:0); // lit if Accumulator is negative
    TickCount(Mark1.LI_Sign_Q, (Qneg) ? 1:0); // lit if Q bit set, i.r last operation on B index generates a negative number
    TickCount(Mark1.LI_Sign_D, ((Dsigned) && (D >> 39)) ? 1:0); // lit Dsigned set and D is negative
    n = ((PI >> 14) & 63); 
    TickCount(Mark1.LI_Stop, ((n == OP_STOP_L) || (n == OP_STOP_G)) ? 1:0);
}

void Refresh_CpuPanel(void)
{
    int n, n1, n2; 
    int data[16]; 
    static int nMainPrepulseToOnCount = 0; 

    SetStateWithIntensity(Mark1.LI_Sign_A, ( AM >> 39) ? 1:0); // lit if Accumulator is negative
    SetStateWithIntensity(Mark1.LI_Sign_Q, (Qneg) ? 1:0); // lit if Q bit set, i.r last operation on B index generates a negative number
    SetStateWithIntensity(Mark1.LI_Sign_D, ((Dsigned) && (D >> 39)) ? 1:0); // lit Dsigned set and D is negative
    n = ((PI >> 14) & 63); // get the current instr opcode
    SetStateWithIntensity(Mark1.LI_Stop, ((n == OP_STOP_L) || (n == OP_STOP_G)) ? 1:0);

    // sync the switches according to cpu variables just in case thses vars are modified via
    // scp commands instead of using cpanel

    // if cpu is executing instr this implies the main prepulse switch should be set to active (i.e in down position)
    // if user operates Main prepulse switch to move it to inactive position then the cpu will be
    // stoped. The stop occurs at beginning of inst execution, not in middle of cpu execution
    if (bMainPrepulseToOff) {
       // Main Prepulse = 0 -> Switch Up -> main prepulse inactive
       if ((sim_is_running) && (NCYCLE==0)) {
          // prepulse NOT set and cpu running -> stop the cpu at beginning of instr
          // request a cpu stop, without disabling interactive mode
          cpanel_stop_flag = 2;
          // and set interactive mode
          cpanel_interactive=1; 
          // clears the flag because stop handled
          bMainPrepulseToOff=0; 
          nMainPrepulseToOnCount=0; 
       }
    } else if ((GetState(Mark1.SW_Main_Prepulse)) && (sim_is_running)) {
        // the cpu is running (because a scp GO command has been issued in console, for example)
        // so set the prepulse switch to down (active) position. But this is done
        // only at second refresh, so a step on instr will not set prepulse
        nMainPrepulseToOnCount++;
        if (nMainPrepulseToOnCount > 1) {
            SetState(Mark1.SW_Main_Prepulse, 0); 
            bMainPrepulseToOff=0; 
        }
    }
    if (sim_is_running == 0) nMainPrepulseToOnCount=0; // reset counter
    // sync stop switches
    n1=(int) GetState(Mark1.SW_Stop_G);
    n2=(int) GetState(Mark1.SW_Stop_L);
    if ( ((n1==0) || (n1==4)) && ((n2==0) || (n2==4)) ) {
        // switches at its stationary positions (no movement in progress) 
        // set switches according to var
        // console G (bit 0) and L (bit 1) not zero -> dummy stop active
        // dummy stop active -> switch down -> switch state = 0
        // dummy stop not active -> switch up -> switch state = 4
        SetState(Mark1.SW_Stop_G, (Console_Sw_DummyStop & 1) ? 0:4); 
        SetState(Mark1.SW_Stop_L, (Console_Sw_DummyStop & 2) ? 0:4); 
    }
    // sync punch/printer active switches
    n1=(int) GetState(Mark1.SW_Print);
    n2=(int) GetState(Mark1.SW_PtpLptActive);
    if ( ((n1==0) || (n1==4)) && ((n2==0) || (n2==2) || (n2==4)) ) {
        int lpt, ptp; 
        // switches at its stationary positions (no movement in progress) 
        // set switches according to var
        // bit0=lpt active, bit1=ptp active
        // print switch in auto position -> switch up -> switch state = 4
        // print switch in off position -> switch down -> switch state = 0
        // print/punch/both switch in print position -> switch up -> switch state = 4
        // print/punch/both switch in punch position -> switch centered -> switch state = 2
        // print/punch/both switch in both position -> switch down -> switch state = 0
        // get lpt/ptp active according to switches
        lpt=ptp=0; 
        if ((n2==0) || (n2==2)) ptp=1; 
        if ((n2==0) || (n2==4)) lpt=1; 
        if (n1==0) lpt=0; 
        // is lpt/ptp active according to switches same as set in devices
        if (  (LPT_ON == lpt) &&
              (PTP_ON == ptp) ) {
            // yes -> no switch to update
        } else {
            // no! -> update switches to reflect correct lpt/ptp being active
            SetState(Mark1.SW_Print, LPT_ON ? 4:0 );             
            SetState(Mark1.SW_PtpLptActive, (LPT_ON+PTP_ON==2) ? 0 : (PTP_ON==1)? 2:4 );
        }
    }
    // sync KAC state
    if (Console_Sw_KeyClear & 64) {
        // a "set cpu kac" scp command in progress has requested to update the KAC key state
        SetState(Mark1.SW_KAC, (Console_Sw_KeyClear & 1) ? 0:4); // set KAC switch up (KAC off) or down (KAC active)
        Console_Sw_KeyClear &= ~(64); // remove the "update KAC switch" bit
    }    
    // sync H value
    if (Reg_H != H) {
        // H value in control panel (Reg_H) does not match H variable -> should sync cpanel
        // Hand Switch up (state = 4) if corresponding bit is 0, down (state = 0) if bit is 1
        Reg_H=H; 
        for (n=0; n<20; n++) {
            SetState(GetCArrayCId(Mark1.SW_HAND, n), (Reg_H & 1) ? 0:4); // set the switch
            Reg_H = (Reg_H >> 1); 
        }
        Reg_H=H; // restore, now switches states and Reg_H matches value in H
    }
    // display small upper tubes 
    // doc says tubes shows regiters A, B, C and D. Does not details order of tubes, but
    // it seems implied that this is the order. Nevertheless, the console shots on FERUT 
    // shows the tubes with the following order:
    //   
    //    Tube 0     1     2     3
    //       [ B ] [ C ] [ A ] [ D ]
    // 
    // We implemented this order
    //
    // will show AL/AM
    data[0]=(int) (AL & MASK_20BITS);   data[1]=(int)(AL >> 20);   data[2]=(1<<30); // separator
    data[3]=(int) (AM & MASK_20BITS);   data[4]=(int)(AM >> 20);   data[5]=-1;      // end of data
    Refresh_TubeReg(2, data); 
    // will show Bindex of leftmost tube
    for (n=0; n<4; n++) data[n  ]=B[n];     data[4]=(1<<30); // separator
    for (n=0; n<4; n++) data[n+5]=B[n+4];   data[9]=-1; 
    Refresh_TubeReg(0, data); 
    // will show C/PI
    data[0]=C;  data[1]=(1<<30); // separator
    data[2]=PI; data[3]=-1; 
    Refresh_TubeReg(1, data); 
    // will show D multiplicand
    data[0]=(int)(D & MASK_20BITS); 
    data[1]=(int)(D >> 20);    
    data[2]=-1; 
    Refresh_TubeReg(3, data); 
    // display big lower tubes 
    Refresh_TubeStore(0, Reg_LVisibleStoreTube); 
    Refresh_TubeStore(1, Reg_RVisibleStoreTube); 
    // sync visible tube selection tubes with Reg_LVisibleStoreTube/Reg_RVisibleStoreTube
    // check if all switchs are in stationary position
    n2=1; for(n=0;n<3;n++) {
        n1=(int) GetState(GetCArrayCId(Mark1.SW_Storage_L, n));
        if ((n1==0) || (n1==4)) { /* ok, switch at stationary pos */ } else {n2=0; break;} // the switch is being moved
        n1=(int) GetState(GetCArrayCId(Mark1.SW_Storage_R, n));
        if ((n1==0) || (n1==4)) { /* ok, switch at stationary pos */ } else {n2=0; break;} // the switch is being moved
    }
    if (n2) { // switches in stationary positions -> sync with variable
        n1=Reg_LVisibleStoreTube; 
        n2=Reg_RVisibleStoreTube; 
        for(n=0;n<3;n++) {
            SetState(GetCArrayCId(Mark1.SW_Storage_L, n), (n1 & 1) ? 0:4);
            SetState(GetCArrayCId(Mark1.SW_Storage_R, n), (n2 & 1) ? 0:4);
            n1=n1 >> 1; n2=n2 >> 1; 
        }
    }
}

int PARAM_char_ww2       =    12;  // horizontal spacing between first pixel of a char and first pixel of next char in next char same line
int PARAM_char_hh2       =    24;  // height between top line of one char and top line of char in next text line
int PARAM_xPaperMargin   =   100;  // left and right margin on printed paper
int PARAM_ink0           =   120;  // ammount of ink on top of char (0..255, 0=no ink, 255=full black)
int PARAM_ink1           =   255;  // ammount of ink on bottom top of char 

// draw paper background on printout paper from y0 to y1-1
// draw paper background on outgoing papersheet 
void lpt_set_paper_background(int paper_y0, int paper_y1)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    uint32 * surface2;      // outgoing papersheet from printer
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int x,y,bg_x,bg_y;
    int sheet_ww, sheet_hh, yh0;
    
    surface0=GetControlSurface(Mark1.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(Mark1.PaperBackground, 0, &bg_ww, &bg_hh); // paper backgtound should have same width as paper control
    surface2=GetControlSurface(Mark1.PaperSheet, 0, &sheet_ww, &sheet_hh); 

    // pad background on paper, taking into account offset
    for (y=paper_y0; y<paper_y1; y++) {
        bg_y = (y + hPaperBackgroundOffset) % bg_hh; 
        for (x=0; x<paper_ww; x++) {
            bg_x = x % bg_ww;
            surface0[x + y * paper_ww] = surface1[bg_x + bg_y * bg_ww];
        }
    }
    // pad paper sheet: it is 33% of printed out size
    // calc position over printer
    yh0=sheet_hh-paper_hh/3;
    for (y=paper_y0/3; y<paper_y1/3; y++) {
        if (y+yh0 >= sheet_hh) break; // safety
        for (x=0; x<paper_ww/3; x++) {
            surface2[x + (y+yh0) * sheet_ww] = surface0[x*3 + y*3 * paper_ww];
        }
    }
}

void lpt_print_line(int y0, char * sLin)
{
    static char sCharSet[50] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$.:\"=+-/#@"; // # prints one-half char, $ prints pound char
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // character to be printed on paper
    uint32 * surface2;      // outgoing papersheet from printer
    int paper_ww, paper_hh; // printed paper control size
    int char_ww, char_hh;   // character to be printed size
    int x,y, x0, nChar, i,ic, n, ink, p, r, g, b, ink0, ink1;
    char c; 
    int sheet_ww, sheet_hh, yh0;
    uint32 col; 
    int r1,g1,b1,rr,gg,bb,xx,yy; 

    surface0=GetControlSurface(Mark1.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(Mark1.PrinterCharSet, 0, &char_ww, &char_hh); 
    surface2=GetControlSurface(Mark1.PaperSheet, 0, &sheet_ww, &sheet_hh); 

    nChar = 0;
    x0    = PARAM_xPaperMargin;
    while(1) {
        c = sLin[nChar++];
        if (c==1) goto advchar; // char 1 -> advance without printing
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
       advchar:
        x0 += PARAM_char_ww2; 
        if (x0 >= paper_ww - PARAM_xPaperMargin) break; // check if outside paper margins
    }
    // copy line from PrintOut scontrol to PaperSheet control
    // reduce size to 33%, antialiasing
    yh0=sheet_hh-paper_hh/3;
    for (y=y0/3; y<(y0+char_hh)/3; y++) {
        if (y+yh0 >= sheet_hh) break; // safety
        for (x=0; x<paper_ww/3; x++) {
            // antialiase 3x3 pixels box form surface0
            rr=gg=bb=0; 
            for (xx=0;xx<3;xx++) {
                for (yy=0;yy<3;yy++) {
                    col=surface0[x*3+xx + (y*3+yy) * paper_ww]; 
                    get_surface_rgb_color(col, &r1, &g1, &b1);   
                    rr+=r1; gg+=g1;bb+=b1;
                }
            }
            rr=rr/9; gg=gg/9; bb=bb/9; 
            col = surface_rgb_color(rr, gg, bb);
            surface2[x + (y+yh0) * sheet_ww] = col;
        }
    }
}

// return 1 PrintOut image has been refreshed
int Refresh_PrintOut(void)
{
    uint32 * surface0;      // printed paper dynamicaly generated 
    uint32 * surface1;      // background paper sheet image
    uint32 * surface2;      // paper sheet image out going from printer carriage
    int paper_ww, paper_hh; // printed paper control size
    int bg_ww, bg_hh;       // printed paper background control size
    int sheet_ww, sheet_hh; 
    int y, nLin, nLinMax, iLin, n;
    char sLin[LPT_COLUMNS+1];
    static int lptPrintOutDoneChCount = 0; 
    int c1, y0;

    if ((lptPrintOutCount   == lptPrintOutDoneCount) &&
        (lptPrintOutChCount == lptPrintOutDoneChCount))  return 0; // nothing to update

    surface0=GetControlSurface(Mark1.Paper, 0, &paper_ww, &paper_hh); 
    surface1=GetControlSurface(Mark1.PaperBackground, 0, &bg_ww, &bg_hh); // paper background should have same width that paper control
    surface2=GetControlSurface(Mark1.PaperSheet, 0, &sheet_ww, &sheet_hh); // printer outgoing paper sheet 

    nLin = lptPrintOutCount - lptPrintOutDoneCount; // nLin = number of lines to print
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

    if ((nLin >= nLinMax-1) || (nLin < 0)) {
        nLin = nLinMax-1; 
    } else if (nLin == 0) {
        // do not scroll, just print some chars in last line
    } else if (nLin > 0) {
        // scroll up paper image nLin lines 
        y = nLin * PARAM_char_hh2; // first image line of char line that goes to the top of screen
        memcpy(&surface0[0], 
               &surface0[y * paper_ww], 
               sizeof(*surface0) * paper_ww * (paper_hh - y));
        // scroll up paper sheet image 
        y = nLin * PARAM_char_hh2 / 3; // first image line of char line that goes to the top of screen
        for (y0=y; y0<sheet_hh; y0++) {
            memcpy(&surface2[(y0-y)*sheet_ww], 
               &surface2[y0 * sheet_ww], 
               sizeof(*surface2) * (paper_ww/3) );
        }
    }

    if (lptPrintOutCount <= 0) {
        iLin = 0;
    } else {
        iLin = (lptPrintOutCount - 1) % lptPrintOutMAX; // last full line printed
    }
    // prepare first ans last chars to print in last line
    if (nLin>0) {
        c1=-1; // print last line complete
    } else {
        c1=lptPrintOutDoneChCount; // first colmns to print in last line
    }
    // draw nLin lines, starting at bottom of paper and goindg upwards. 
    // chars to print comes from lptPrintOut[iLin]
    n = nLinMax-2; // -2 to leave last line blank
    while (nLin-- > 0) {
        memcpy(sLin, &lptPrintOut[iLin * LPT_COLUMNS], LPT_COLUMNS); sLin[LPT_COLUMNS] = 0;
        iLin = (iLin-1) % lptPrintOutMAX;
        if (iLin<0) iLin += lptPrintOutMAX; 
        lpt_set_paper_background(n*PARAM_char_hh2, (n+1)*PARAM_char_hh2);
        lpt_print_line(n*PARAM_char_hh2, sLin);
        n--;
    }
    // draw chars in last line
    n = nLinMax-1;
    iLin = (lptPrintOutCount) % lptPrintOutMAX; // line currently being printed, the one at bottom of paper
    memcpy(sLin, &lptPrintOut[iLin * LPT_COLUMNS], LPT_COLUMNS); sLin[LPT_COLUMNS] = 0;
    sLin[lptPrintOutChCount]=0; // end of string
    if (c1 >=0) {
        while (c1>0) {c1--; sLin[c1]=1;}; // mark chars to be skiped instead of printed (it was already printed on previous refresh)
    } else {
        // erase the last line
        lpt_set_paper_background(n*PARAM_char_hh2, (n+1)*PARAM_char_hh2);
    }
    lpt_print_line(n*PARAM_char_hh2, sLin);

    lptPrintOutDoneCount=lptPrintOutCount;
    lptPrintOutDoneChCount=lptPrintOutChCount;

    cpanel_ControlRedrawNeeded = 1;
    SetState(Mark1.Paper, 0);
    return 1; 
}

int PARAM_CR_Duration = 200;  // time in msec to move the printer carriage its full distance, 
                              // educated guess based on EDSAC videos of creed teleprinter operating 
                              // from last col to first one

// refresh printed paper sheet going out from printer
void Refresh_PrinterSheet(int bShouldUpdatePrintedPage)
{
    uint32 * surface0;      
    uint32 * surface2;      // outgoing papersheet from printer
    uint32 * surface3;      // outgoing papersheet placed at carriage position
    int w0, h0, sheet_ww, sheet_hh, xh0, yh0, dyh0, x,y, paper_ww, n, pos;
    uint32 col, msec; 
    static int vibrating=0; 

    paper_ww=GetControlInfo(Mark1.Paper, CINFO_W);
    surface2=GetControlSurface(Mark1.PaperSheet, 0, &sheet_ww, &sheet_hh); 
    surface3=GetControlSurface(Mark1.PaperSheet, 1, &sheet_ww, &sheet_hh); 
    
    paper_ww = paper_ww - PARAM_xPaperMargin * 2; // remove margins
    n=paper_ww / PARAM_char_ww2; // num of chars that the paper can hold in a line
    paper_ww=paper_ww * LPT_COLUMNS/n; // area effectively used on printing 
    paper_ww=paper_ww / 3; // scale down
    xh0=140; 

    pos=lpt_chariage_pos; // pos of carrigage, where to draw the paper sheet
    if (lpt_CR_start_tm0) {
        // carriage return in progress
        bShouldUpdatePrintedPage=2;
        msec = sim_os_msec()-lpt_CR_start_tm0; // time elapsed
        if (msec > (uint32) PARAM_CR_Duration) {
            lpt_CR_start_tm0=0; // elapsed
        } else {
            // calc pos of carriage taking into account the starting point
            n=LPT_COLUMNS * msec / PARAM_CR_Duration; // number of columns advanced in msec milliseconds
            pos=lpt_CR_from_pos-n; 
            if (pos <= 0) {
                pos=0; 
                lpt_CR_start_tm0=0; // carriage return finished 
            }
        }
    }

    // calc destination pos
    yh0=(int)(PARAM_char_hh2 * 2.5 / 3);
    xh0= xh0+ paper_ww * (LPT_COLUMNS-pos) / LPT_COLUMNS;
    dyh0=0;

    if ((bShouldUpdatePrintedPage==0) && (vibrating==0)) return; // nothing to update
    if ((bShouldUpdatePrintedPage==0) && (vibrating==1)) {
        // terminate vibration
        vibrating=0; 
    } else if (bShouldUpdatePrintedPage==2) {
        // <CR> in progress -> do not vibrate
        vibrating=0;
    } else {
        // apply some vibration (because of mechanical type hammer hitting the paper)
        yh0 += (dyh0=(sim_rand() & 3)-2); 
        xh0 += (sim_rand() & 3)-2; 
        vibrating=1;
    } 
    // get wall color
    surface0=GetControlSurface(0, 0, &w0, &h0);
    col=surface0[1];
    // fill in wall color
    for (y=0;y<sheet_hh;y++) {
        for (x=0;x<sheet_ww;x++) {
            surface3[x + y * sheet_ww]=col;
        }
    }
    // copy paper image to xh0, yh0
    for (y=0;y<sheet_hh;y++) {
        if (y + yh0 >= sheet_hh) break; // safety
        for (x=0;;x++) {
            if (x + xh0 >= sheet_ww) break; // safety
            col=surface2[x + y * sheet_ww];
            if (col==0) break; // transparent pixel marks end of page 
            surface3[xh0 + x + (y + yh0) * sheet_ww]=col;
        }
    }
    // put the paper drum bitmap
    CopyControlImage(Mark1.PaperDrum,  0,     0,0,0,0,
                     Mark1.PaperSheet, 1,     xh0-28,sheet_hh-50+dyh0);

    cpanel_ControlRedrawNeeded = 1;
    SetState(Mark1.PaperSheet, 1);
}

int PARAM_PTR_Read_Duration = 500;  // time in msec needed for paper tape to finish movement on read
int PARAM_Perforator_TypeIn_Duration = 1500; // time in msec the paper sheet is shown on perforator

// refresh Paper Tape Reader and Paper Tape Perforator
void Refresh_ReaderPerforator(void)
{
    int n; 
    uint32 msec; 

    // refresh perforator
    if (perforator_start_tm0) { // Creed perforator for machine language programs
        SetState(Mark1.ProgSheet, 1); // show a Machester univ program sheet attached to perforator
        msec = sim_os_msec() - perforator_start_tm0; // time in msec the sheet has been visible
        if (msec > (uint32) PARAM_Perforator_TypeIn_Duration) { // if time expired
            perforator_start_tm0=0; // remove programme sheet
            cpanel_ControlRedrawNeeded=1; 
            SetState(Mark1.PerforatorWall, bDarkMode);        
            cpanel_ControlRedrawNeeded=1; 
            SetState(Mark1.PerforatorPanel, 0);        
            cpanel_ControlRedrawNeeded=1; 
            SetState(Mark1.ProgSheet, 0); 
        }
    }

    // refresh paper tape reader
    if ( ((ptr_unit.flags & UNIT_ATT) == 0) &&  (ptr_buf[0]==0)) {
        // if not attached and no char feed to papertape then
        // set state = 0 -> no tape attached to reader
        if (GetState(Mark1.PaperTape)) { 
            cpanel_ControlRedrawNeeded=1; 
            SetState(Mark1.ReaderTable, bDarkMode);        
            SetState(Mark1.PaperTape, 0); 
        }
        return; 
    }
    n=(int) GetState(Mark1.PaperTape); // get current papertape state

    if (ptr_move_start_tm0==0) {
        // start movement timestamp = 0 -> no read in progress
        if (n!=1) {
            // set paper tape in default position
            cpanel_ControlRedrawNeeded=1; 
            SetState(Mark1.ReaderTable, bDarkMode);        
            SetState(Mark1.PaperTape, 1); 
        }
        return; 
    }
    // tape is being read, animate it
    n=n+1; 
    if (n>4) n=1; 
    SetState(Mark1.PaperTape, n); 
    cpanel_ControlRedrawNeeded=1; 
    SetState(Mark1.ReaderTable, bDarkMode);        

    // calc time elapsed from read start
    msec = sim_os_msec() - ptr_move_start_tm0; 
    if (msec > (uint32) PARAM_PTR_Read_Duration) {
        ptr_move_start_tm0=0; // terminate mvtent
    }
}

void process_HotKeys(void)
{
    int ncp; 
    char c; 

    // get vptr for current window
    ncp=GetControlInfo(Mark1.CtrlInfoPanel, CINFO_NCP);
    if (cpvid[ncp].vptr_cp != vid_keyb.vptr) return; // key not for this window
    c = vid_keyb.LastKeyPress;  // key currently being pressed

    if (vid_keyb.KeyPress == 'f'-'a'+1) { // Control-F (^F) is being pressed
        if (CpuSpeed_Acceleration != -1) { 
            // accelerate to max speed max while keydown
            CpuSpeed_Acceleration_save = CpuSpeed_Acceleration;
            CpuSpeed_Acceleration=-1;        // set cpu speed=max & set cpu fast
            Measure_CpuSpeed(0); ShowInfoTm0 = 0; // reset speed measurement because ^F pressed
            sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F pressed\n");
        }
    } else if (CpuSpeed_Acceleration == -1) {
        // return to previous cpu speed setting
        CpuSpeed_Acceleration = CpuSpeed_Acceleration_save;
        CpuSpeed_Acceleration_save=0; 
        // return to previous fast/realtime setting
        Measure_CpuSpeed(0); ShowInfoTm0 = 0; // reset speed measurement because ^F released
        sim_debug(DEBUG_DETAIL, &cpu_dev, "Measured speed: init because ^F released\n");
    }
    if (c ==  ('i'-'a'+1)) { // Tab/Control-I (^I) has been pressed
        // toggle show Info on GUI 
        vid_keyb.LastKeyPress = 0; // clear key as it is processed
        bShowInfo = (bShowInfo) ? 0:1; // toggle value
        if (bShowInfo==0) {
           // put again background thus removing info from gui
           SetState(Mark1.CtrlInfoPanel, 0); 
        } else {
           // init showinfo measurement
           ShowInfoTm0 = 0;
       }
    }
    if ( (Console_Sw_ManKbdPtrToLpt==2) && (c) &&                     // =2 -> Printer input from its own keyboard
         (sim_is_running == 0) &&                                     // and cpu stopped
         (((c >= 'a') && (c <='z')) || ((c >= 'A') && (c <='Z')) ||   // and char is letter 
           (c == ('l'-'a'+1)) || (c == ('z'-'a'+1)) ||                //     or ^L (letter shift) ^Z (figure shift)
           (c == 13) || 
           (strchr("0123456789. =/@:\"#$", c)))) {                    //     or number/special char
       // send the char to the printer
       // note: the main gui window is not defined as ControlPanelShortName=name,TextInput so c vhar var
       //       holds the ascii code without taking into consideration if shift is being pressed
       int nBeats; 
       if (c==13) {
            CharIoInstr(2 + (13 << 8), &nBeats); // send a <CR><LF> to printer
            CharIoInstr(2 + (10 << 8), &nBeats);
       } else {
            CharIoInstr(2 + (c << 8), &nBeats); // send the key pressed to printer
       }
       vid_keyb.LastKeyPress = 0; // clear key as it is processed
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
    int BeatsExecPerSec; 
    int InstrExecPerSec, fps, n=0; 
    int ips_unit; double ips; 
    uint32 msec; 

    // set dynamic contents of CtrlInfoPanel 

    if (bShowInfo>1) {
        BeatsExecPerSec = InstrExecPerSec = fps = 0;
    } else {
        InstrExecPerSec = fps = 0;
        msec = Refresh_tnow - ShowInfoTm0; 
        if ((msec > 0) && (msec < 1000)) {
            n = Measure_CpuSpeed(2)- InstrExec0;
            if (n<0) n=0;
            InstrExecPerSec = n * 1000 / msec;
            n = (int) (GlobalBeatsCount - BeatsExec0); 
            if (n<0) n=0;
            BeatsExecPerSec = n * 1000 / msec;
            n = Refresh_Frames_Count  - FramesCount0;
            fps             = n * 1000 / msec;
            if (fps>60) fps=60;
            if (fps<0) fps=0;
        }
        ShowInfoTm0  = Refresh_tnow;
        BeatsExec0   = GlobalBeatsCount; 
        InstrExec0   = Measure_CpuSpeed(2);  
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
    //                           real hardware speed for Ferranti Mark I: clock 100KHz. 
    //                           Instr duration is expressed in number of beats. 
    //                           One beat = 24 clock cycles -> 4166 Beats per second
    //       IPS = Instructions Per Second -> Instr executed each sec (when waiting, does not count on IPS)

    // set the aprox speed of real hardware according to memory cycle time

    sprintf(buf, "FPS: %d, Cpu Speed: x%0.1f (%0.1f %s)\n", 
            fps, BeatsExecPerSec / 4166.6, 
            ips, (ips_unit == 2) ? "MIPS" : (ips_unit == 1) ? "KIPS" : "IPS"); 

    if (ptr_FileName[0]) {
        // a paper tape file is attached
        if ((ptr_FileSize > 0) && ((int) ptr_unit.pos < ptr_FileSize-2)) { 
            // reading in progress, -2 so the last <CR><LF> is not taked into account
            sprintf(&buf[strlen(buf)], "PTR: %s (%d%%), ", 
                ptr_FileName, 100 * ptr_unit.pos / ptr_FileSize);
        } else {
            sprintf(&buf[strlen(buf)], "PTR: %s, ", 
                ptr_FileName);
        }
    }
    // show then number of storage tubes being displayed right now
    sprintf(&buf[strlen(buf)], "(S%d S%d)\n", Reg_LVisibleStoreTube, Reg_RVisibleStoreTube); 
    
    ShowInfo_DrawText(Mark1.CtrlInfoPanel, buf);
}


void Mark1_Refresh(void)
{
    // refresh Cpu Panel
    Refresh_CpuPanel();

    // refesh printer
    if (bPrintOutVisible) {
        int bShouldUpdatePrintedPage; 
        bShouldUpdatePrintedPage=Refresh_PrintOut();
        Refresh_PrinterSheet(bShouldUpdatePrintedPage); 
    }

    // show info
    if (  ((bShowInfo) && ((Refresh_Frames_Count % 8)==0)) || (bShowInfo==2)  ) {
        // show info for ^I (Ctrl I) at 60 FPS / 8 -> 8 FPS
        // if (bShowInfo==2) ips will be shown each frame
        Refresh_ShowInfo(0);
    }
    // process HotKeys on Control Panel GUI window 
    process_HotKeys();
    // Must be called after Refresh_ShowInfo. when ^F is pressed it resets speed measurement vars
    // so if Refresh_ShowInfo is called inmedatelly after, a no tickcount are yet executed will display a speed of zero 
    if ((bShowInfo==1) && (ShowInfoTm0==0)) Refresh_ShowInfo(1);

    // if Printer set to print PTR, and Manual mode set 
    if ( (Console_Sw_ManKbdPtrToLpt==1) &&                            // =1 -> Printer input from PTR
         (Console_Sw_ManualMode)) {                                   // =1 -> manual mode active
       // call CharToInstr to read 1 char from PTR and send it to LPT
       int nBeats; 
       CharIoInstr(1, &nBeats);
    }

    // refresh Paper tape reader and Paper tape perforators
    if (bReaderPerforatorVisible) {
        Refresh_ReaderPerforator(); 
    }

}


// drag and drop a file handling. Called when a file of given filename is droped on given CId control 
void Mark1_DropFile(int CId, char * FileName)
{
    extern t_stat   ptr_attach(UNIT *, CONST char *);
    extern t_stat perforator_typein_cmd(int32 flag, CONST char *cptr); 
    int32  sv; 
    char *c; 
    char fname[MAX_PATH+1];
    char s[MAX_PATH+100];

    if ((CId ==Mark1.ReaderPanel) || (CId ==Mark1.PaperTape) || (CId ==Mark1.ReaderTop)) {
        // drag and drop a file on paper tape reader -> attach it
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q'); // set switch -Q (quiet) 
        ptr_attach(&ptr_unit, FileName);
        sim_switches=sv; 
    } else if (CId ==Mark1.Drop_ProgFile) {
        // drag and drop a file on perforator 
        sv=sim_switches; // save current switches
        sim_switches = SWMASK ('Q') | SWMASK ('N'); // set switch -Q (quiet) -N (new)
        c = sim_filepath_parts (FileName, "pn"); // return c =path + name for droped file, but no extension
        sprintf(fname, "%s.pt.txt", c);          // add ".pt.txt" extension
        free (c);
        attach_unit(&perforator_unit, fname);       // attache new 
        sim_switches=sv; 
        if (bAutoCodePerforator==0) {
            sprintf(s, "typein -q routine1 %s s4", FileName); 
            DoSCP(s); 
            sprintf(s, "typein -q routine2 %s s5", FileName); 
            DoSCP(s); 
        } else {
            sprintf(s, "typein -q bautocode source %s", FileName); 
            DoSCP(s); 
        }
        sprintf(s, "detach perforator"); 
        DoSCP(s); 
    }

}


// activate/deactivate dark mode: tube background gets darker
void Mark1_OnClick_DarkMode(void)
{
    int n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // get current dark mode setting
        n=(int) GetState(Mark1.TubeLbkg);
        n=(1-n); // toggle
        SetDarkMode(n); 
    }
    
}

// click on TypeWriter buttons
void Mark1_OnClick_BTN(void)
{
    int addr, d, n; 

    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> set pressed button state
        SetState(CP_Click.CId, 1); 
        return; 
    }
    // release mouse -> set button back at its release position
    SetState(CP_Click.CId, 0); 

    // Set/reset bit indicated by typewriter button at address part of ManualInstr switches
    // but only if manual-auto switch set to manual
     if (Console_Sw_ManualMode==0) return; // we are in auto mode -> buttons inhibited
     addr=Console_Sw_Instruction & 1023; 
    // get the button pressed -> this is the bit number handled
    n=CP_Click.CArrayItem;  
    d=ReadAddr(addr, 0); // read mem
    if (GetState(Mark1.SW_Erase_Insert)==4)  {
        // Erase/Insert swith at Erase position
        d = d & (~(1<<n)); // remove the bit
    } else if (GetState(Mark1.SW_Erase_Insert)==0)  {
        // Erase/Insert swith at Insert position
        d = d ^ (1<<n); // toggle the bit
    }
    WriteAddr(addr, d, 0); // write back
}

// click on Left/Right lower big tube -> shows next store (S0 -> S1, S1 -> S2, ... S7->S0)
// this is not a real hw feature (obviously) but very convenient on emulator GUI
void Mark1_OnClick_Tube(void)
{
    if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button -> change de shown store 
        if (CP_Click.CId == Mark1.TubeLbkg) {
            // click on left tube background
            Reg_LVisibleStoreTube++;
            if (Reg_LVisibleStoreTube > 7) Reg_LVisibleStoreTube=0; 
        } else if (CP_Click.CId == Mark1.TubeRbkg) {
            // click on right tube background
            Reg_RVisibleStoreTube++;
            if (Reg_RVisibleStoreTube > 7) Reg_RVisibleStoreTube=0; 
        }
    }
}

void Mark1_OnClick_Sw(void)
{
    int n,n1,n2; 
    static int Ignore_release_mouse_button_for_CId = 0; 

    // Note: the first edition of programming manual, on section 12. console, states
    // the "control of completition signals" group of keys. The name of the keys is not the same
    // as seen in available images of real console, so some kind of rename should had been
    // happened over the time. 
    // even if we do not know if this is historicaly correct, due to lack of further information
    // we do this educated guess:
    //  - Completition Signals on-off switch CS     -> This is the switch labeled "Main Prepulse" on cpanel
    //  - Key for single completition signal KCS    -> This is the switch labeled "KCS" on cpanel
    //  - Slow comletition signal key               -> This is the switch labeled "SEMI CONT" on cpanel
    // There are not "Key Store clear" that clear only the storage tubes. The Stres are cleared with KEC
    //



    if ((CP_Click.KeyPress_KeyRelease == 1) && (CP_Click.CId == Mark1.ReaderTop)) {
        // if click on tapereader -> detach paper tape input file
        extern t_stat ptr_detach (UNIT *uptr);
        ptr_detach(&ptr_unit); 
        return; 
    }

    if ((CP_Click.CId==Mark1.SW_KAC) || (CP_Click.CId==Mark1.SW_KBC) || (CP_Click.CId==Mark1.SW_KCC) ||
        (CP_Click.CId==Mark1.SW_KDC) || (CP_Click.CId==Mark1.SW_KEC) || (CP_Click.CId==Mark1.SW_KLC) ||
        (CP_Click.CId==Mark1.SW_KCS)  ) {
        // These are Key Switches
        if (CP_Click.KeyPress_KeyRelease == 1) {
            // press mouse button -> if switch up or down then move switch down and do the action
            n=(int) GetState(CP_Click.CId); 
            if (n<2) n=4; else n=0; // set the stationary position
            SetState(CP_Click.CId,n); 
            // KAC..KEC will be kept pressed if Shift key pressed. This allows 
            // the user to simulate "click something else without releasing key-Switch"
            if ((vid_keyb.Shift) && 
                ((CP_Click.CId==Mark1.SW_KAC) || (CP_Click.CId==Mark1.SW_KBC) || (CP_Click.CId==Mark1.SW_KCC) ||
                 (CP_Click.CId==Mark1.SW_KDC) || (CP_Click.CId==Mark1.SW_KEC))) {
                // Shift pressed! -> signal to ignore mouse release event (so we don't move back the key switch
                // to released position)
                Ignore_release_mouse_button_for_CId = CP_Click.CId; 
            } else Ignore_release_mouse_button_for_CId = 0; 
        } else {
           // release mouse button -> move switch back to up (released) inactive stationary position
            if (Ignore_release_mouse_button_for_CId == CP_Click.CId) {
                // ignore, user want Key-Switch hold down
                Ignore_release_mouse_button_for_CId = 0; 
            } else {
                SetState(CP_Click.CId,4); // release key-switch
            }
        }
    } else if (CP_Click.KeyPress_KeyRelease == 1) {
        // press mouse button on 2-way or 3-way Switches -> initiate switch move
        n=(int) GetState(CP_Click.CId); 
        SetState(CP_Click.CId, n+1); 
        // regular switches does the action on mouse-button release, so here (we are handling
        // mouse-button-press) we just return; 
        return; 
    } else {   
        // release mouse button on 2-way or 3-way Switches -> set switch at its stationary position
        if ((CP_Click.CId == Mark1.SW_PtpLptActive) || 
            (CP_Click.CId == Mark1.SW_Erase_Insert) ||
            (CP_Click.CId == Mark1.SW_ManualKbdPtrToLpt)) {
            // these are 3-way switch. State 0->down, 2->center, 4->up. Other states -> switch is changing from one position to another
            n=(int) GetState(CP_Click.CId); 
            if (n<2) n=2; else if (n<4) n=4; else n=0;  // set the stationary position
            SetState(CP_Click.CId, n); 
        } else {
            // up/down switch. State 0->down, 4->up. Other states -> switch is changing from one position to another
            n=(int) GetState(CP_Click.CId); 
            if (n<2) n=4; else n=0; // set the stationary position
            SetState(CP_Click.CId, n); 
        }
    }

    if (CP_Click.CArrayId == Mark1.SW_Storage_L) {
        // click on switches to select the store to view in console's left tube, 
        // switch down -> state = 0 -> switch read as set
        Reg_LVisibleStoreTube=0;
        for(n=0;n<3;n++) {
            if (0==GetState(GetCArrayCId(Mark1.SW_Storage_L, n))) Reg_LVisibleStoreTube |= (1 << n); 
        }
    } else if (CP_Click.CArrayId == Mark1.SW_Storage_R) {
        // click on switches to select the store to view in console's right tube, 
        Reg_RVisibleStoreTube=0;
        for(n=0;n<3;n++) {
            if (0==GetState(GetCArrayCId(Mark1.SW_Storage_R, n))) Reg_RVisibleStoreTube |= (1 << n); 
        }
    } else if (CP_Click.CArrayId == Mark1.SW_ManInstr) {
        // click on switch on Manual Instruction group
        n = CP_Click.CArrayItem;
        if (n < 20) {
            if (GetState(CP_Click.CId)==0) {
                // switch is down -> bit is 1
                Console_Sw_Instruction |= (1 << n); 
            } else {
                // switch is up -> bit is 0
                Console_Sw_Instruction &= ~(1 << n); 
            }
        }
    } else if (CP_Click.CArrayId == Mark1.SW_HAND) {
        // click on switch on HAND-SWITCHES group
        // get the instr 
        n = CP_Click.CArrayItem;
        if (n < 20) {
            if (GetState(CP_Click.CId)==0) {
                // switch is down -> bit set to 1
                H=H | (1 << n); 
            } else {
                // switch is up -> bit is 0
                H=H & ~(1 << n); 
            }
        }
    } else if (CP_Click.CId == Mark1.SW_Stop_G)  {
        // console G (bit 0) dummy stop active
        // dummy stop active when switch is down (so switch state is 0)
        // dummy stop not active when switch is up (so switch state is 4)
        if (GetState(Mark1.SW_Stop_G)==0) {
            Console_Sw_DummyStop  |= 1; // set bit to signal dummy stop active
            cpanel_interactive=1;  // set interactive mode
        } else {
            Console_Sw_DummyStop  &= ~1; // reset bit to signal dummy stop not active
            // disable interactive mode so when prog ends the script can continue
            // if no dummy stop set, then disable interactive mode so when prog ends the script can continue
            if ((Console_Sw_DummyStop & 3)==0)  cpanel_interactive=0; 
            StopReason=0;
        }
    } else if (CP_Click.CId == Mark1.SW_Stop_L)  {
        // console L (bit 1) dummy stop active
        // dummy stop active when switch is down (so switch state is 0)
        // dummy stop not active when switch is up (so switch state is 4)
        if (GetState(Mark1.SW_Stop_L)==0) {
            Console_Sw_DummyStop  |= 2; // set bit to signal dummy stop active
            cpanel_interactive=1;  // set interactive mode
        } else {
            Console_Sw_DummyStop  &= ~2; // reset bit to signal dummy stop not active
            // if no dummy stop set, then disable interactive mode so when prog ends the script can continue
            if ((Console_Sw_DummyStop & 3)==0)  cpanel_interactive=0; 
            StopReason=0;
        }
    } else if ((CP_Click.CId == Mark1.SW_Print) || (CP_Click.CId == Mark1.SW_PtpLptActive)) {
        // set lpt/ptp active according to switches
        n1=(int) GetState(Mark1.SW_Print);
        n2=(int) GetState(Mark1.SW_PtpLptActive);
        if (n2==0) { // print/punch/both switch in both position -> switch down -> switch state = 0
            // ptp active, lpt active depending on print switch: if print switch in auto position -> switch 
            // up -> switch state = 4, lpt not active if print switch in off position -> switch 
            // down -> switch state = 0
            ptp_unit.flags &= (~UNIT_DIS); // enable PTP
            if (n1==0) { lpt_unit.flags |= UNIT_DIS; } // disable LPT
            else         lpt_unit.flags &= (~UNIT_DIS); // enable LPT
        } else if (n2==2) { // print/punch/both switch in punch position -> switch centered -> switch state = 2
            ptp_unit.flags &= (~UNIT_DIS); // enable PTP
            lpt_unit.flags |= UNIT_DIS;  // disable LPT
        } else if (n2==4) { // print/punch/both switch in print position -> switch up -> switch state = 4
            ptp_unit.flags |= UNIT_DIS; // disable PTP
            if (n1==0) { lpt_unit.flags |= UNIT_DIS; } // disable LPT
            else         lpt_unit.flags &= (~UNIT_DIS); // enable LPT
        }
    } else if (CP_Click.CId == Mark1.SW_ManualKbdPtrToLpt) {
        // state 4 -> K,P -> Printer Keyboard to Printer
        // state 2 -> A,P -> Automatic: printer controlled by computer
        // state 0 -> T,P -> paper tape reader to printer
        // set variable Console_Sw_ManKbdPtrToLpt  // =0 -> Printer input from computer, =1 -> from Ptr, =2 -> from its own keyboard
        n=(int) GetState(Mark1.SW_ManualKbdPtrToLpt);
        if (n==0) Console_Sw_ManKbdPtrToLpt=1; 
        if (n==2) Console_Sw_ManKbdPtrToLpt=0; 
        if (n==4) {Console_Sw_ManKbdPtrToLpt=2; vid_keyb.LastKeyPress = 0;} // clear any host keyboard key 
    } else if (CP_Click.CId == Mark1.SW_KEC) {
        // KEC=Key Everything clear
        // will clear everything: regs A, B, C, D, storage tube ...
        // works allways, even if cpu is executing instr
        if (GetState(Mark1.SW_KEC)==0) {
            cpu_reset(&cpu_dev);
            Console_Sw_KeyClear |=   128;  /* KEC switch is on when down (state=0) */
        } else {
            Console_Sw_KeyClear &= ~(128); /* KEC switch is off when up (state=4) */
        }
    } else if (CP_Click.CId == Mark1.SW_KAC) {
        // KAC=Key Accumulator clear
        if (GetState(Mark1.SW_KAC)==0) { Console_Sw_KeyClear |=   1 ; AM=AL=0; } 
        else {                           Console_Sw_KeyClear &= ~(1); } /* KAC switch is off */
    } else if (CP_Click.CId == Mark1.SW_KBC) {
        // KBC=Key B tube clear
        if (GetState(Mark1.SW_KBC)==0) { Console_Sw_KeyClear |=   2 ; B[0]=B[1]=B[2]=B[3]=B[4]=B[5]=B[6]=B[7]=0; } 
        else {                           Console_Sw_KeyClear &= ~(2); } /* KBC switch is off */
    } else if (CP_Click.CId == Mark1.SW_KCC) {
        // KBC=Key C (control) clear
        // we set NCYCLE = 3 so when pressing Main/Single prepulse (or issuing a go/step), cpu incrs C and instr to 
        // be exec is fetched from addr 0001. This is needed for SchemeC to start as stated in doc
        if (GetState(Mark1.SW_KCC)==0) { Console_Sw_KeyClear |=   4 ; C=0; PI=0; NCYCLE=3; } /* KCC switch is on */
        else {                           Console_Sw_KeyClear &= ~(4); } /* KCC switch is off */
    } else if (CP_Click.CId == Mark1.SW_KDC) {
        // KBC=Key D (muliplier) clear
        if (GetState(Mark1.SW_KDC)==0) { Console_Sw_KeyClear |=   8 ; D=0; } /* KDC switch is on */
        else {                           Console_Sw_KeyClear &= ~(8); } /* KDC switch is off */
    } else if (CP_Click.CId == Mark1.SW_KLC) {
        // KLC=Key Line clear. Clear line indicated by address part of ManualInstr switches
        // but only if manual-auto switch set to manual
        if ((GetState(Mark1.SW_KLC)==0) && (Console_Sw_ManualMode)) { 
            // line to clear is indicated by addr part of Manual Instruction set in switches
            n=Console_Sw_Instruction & 1023; 
            WriteAddr(n, 0, 0); 
        }
    } else if (CP_Click.CId == Mark1.SW_MAN_AUTO) {
        // AUTO mode (switch down, state 0) -> get instr to exec from addr indicated in reg C. Typewriters buttons and KLC inhibited
        // MAN mode (switch up, state 4) -> get instr to exec from Instruction Switches. Typewriters buttons and KLC active, allows to modify storage
        Console_Sw_ManualMode = GetState(Mark1.SW_MAN_AUTO) ? 1:0;  
    } else if ((CP_Click.CId == Mark1.SW_PrepulseSpeed) || 
               (CP_Click.CId == Mark1.SW_SinglePrepulse)) {
        // SEMI CONT mode (switch down, state 0) -> instr exec at 50 instr/sec rate whatever cpu_acceleration is
        // unlabeled (switch up, state 4) -> instr exec at full speed (or according to cpu_acceleration is set)
        // Single Prepulse (switch down, state 0) -> exec only one instr
        // Console_Sw_PrepulseSpeed  =0 -> full speed, 
        //                           =1 -> execute at a 50 instr/sec rate, 
        //                           =2 -> single prepulse   
        if (GetState(Mark1.SW_SinglePrepulse)==0) {
            Console_Sw_PrepulseSpeed = 2; // halt at ento of next instr
            cpanel_interactive=1;  // set interactive mode
        } else {
            Console_Sw_PrepulseSpeed = GetState(Mark1.SW_PrepulseSpeed) ? 0:1; 
        }
    } else if ((CP_Click.CId == Mark1.SW_KCS) || 
               (CP_Click.CId == Mark1.SW_Main_Prepulse) )  {
        // execution control with KCS (Key-Switch Completition Signal), Main and Single Prepulse
        int bKCS            = ((GetState(Mark1.SW_KCS) == 0)            ? 1:0); // bKCS=1 -> KCS active
        int bSinglePrepulse = ((GetState(Mark1.SW_SinglePrepulse) == 0) ? 1:0); // bSinglePrepulse=1 -> SinglePrepulse active
        int bMainPrepulse   = ((GetState(Mark1.SW_Main_Prepulse) == 0)  ? 1:0); // bMainPrepulse=1 -> MainPrepulse active
        int nAction         = 0;
        if (sim_is_running) {
            // cpu is already running 
            if (bMainPrepulse == 0) { 
                // if Main Prepulse set to off -> halt cpu.
                // this is handled in refresh routine to guarantee the cpu is stopped at the start 
                // of instr execution (i.e. when NCYCLE==0). Just request this using bMainPrepulseToOff flag
                bMainPrepulseToOff=1; 
            }
        } else {
            // cpu is stopped
            if ((Console_Sw_ManualMode) && ((bMainPrepulse) || (bKCS))) {
                // if cpu is stopped, in manual mode, and requesting to execute an instr 
                // then reset NCYCLE to exec the manual instr whatever the previous instr state is
                NCYCLE=0; 
            }
            if (Console_Sw_KeyClear & 4) {
                // if KCC switch is active, reset C, PI to zero at start of instr execution
                NCYCLE=0; 
            }
            if ((NCYCLE==0) || (NCYCLE==3)) {
                // we are at beggining of inst exec, as result of a cpu halt (because MainPrepulse set to off)
                // or as result of KEC or KCC operation. 
                // operating KCS or Main Prepulse (from off to on) will start the cpu
                if (bSinglePrepulse) nAction=1; else  // perform a scp step 
                if (bMainPrepulse)   nAction=2;       // perform a scp go
            } else {
                // we are in the middle of inst exec, as result of a instr stop (because /L or /G dummy stop
                // commands exec, or because no data in PTR reader, or a hoot stop or a dynamic stop, or any other
                // stop condition)
                // in these cases, the execution should be resumed with KCS (Key-Switch Completition Signal).
                //                 Main Prepulse switch will not resume the instr execution
                // if execution is resumed, if Main Prepulse switch is on then program continues. but if
                // Main Prepulse is off, then current instr is resumed and terminated, and cpu stops at start of
                // next instr. Then execution can be resumend with single or main prepulse, but NOT with KCS
                if (bKCS) {
                    if (bMainPrepulse) nAction=2; // perform a scp go
                    else nAction=1; // perform a scp step 
                }
            }
            // now do the action (if any)
            if (nAction==1) {
                cpanel_interactive=1;  // set interactive mode
                DoSCP("step"); // -> step an instruction
            } else if (nAction==2) {
                // disable interactive mode so when prog ends the script can continue
                if ((Console_Sw_DummyStop & 3)==0)  cpanel_interactive=0; 
                StopReason=0;
                // send command to scp
                DoSCP("go"); 
            }
        }
    }
}

// Matches the ifdef(CPANEL) at beginning of file
#endif

