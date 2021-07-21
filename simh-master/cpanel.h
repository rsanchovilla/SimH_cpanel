/* cpanel.c: simulator control panel simulation

   Copyright (c) 2017-2020, Roberto Sancho

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

   27-Jun-17    RSV     First version control panel support (on HP2100 family)
   10-Nov-17    RSV     control panel support for rcornwell sims IBM 7000 family
   14-May-20    RSV     Control panel support for IBM 650 use standard LIB PNG bitmaps
   27-Jan-21    RSV     Control panel support for IBM NORC mouse drag and zoom
   22-Jun-21    RSV     Control panel support for IBM 701 support for big panels

   This module implements the following routines to support control panel simulation.

   These functions are to be used on control panel implementation for computer xxx in source file 
   xxx_cpanel.c where xxx is the simulated machine (e.g. hp2100_cpanel.c)

   GetState                                (called from xxx_cpanel.c) Get single control/control array state
   SetState                                (called from xxx_cpanel.c) Set single control/control array state
   SetStateAutoNext                        (called from xxx_cpanel.c) Schedulle a control state change 
   GetCArrayCId                            (called from xxx_cpanel.c) Get n'th Control Id from a Control Array

   DoClickEvent                            (called from xxx_cpanel.c) Simulates a click event on control panel
   DoSCP                                   (called from xxx_cpanel.c) Add a SCP command to pending list

   These functions are used in scp.c and xxx_cpu.c to hook control panel system in SimH. 

   cpanel_fgets                            (called from scp.c) Returns next pending scp command issued from control panel, remove from pending list 
   cpanel_interval_refresh                 (called from xxx_cpu.c) Refresh panel from within cpu inst execution loop
   
   

*/

#include "sim_defs.h"
#include "sim_video.h"                                // just needed because it defines uint64
#include "cpanel_vid.h"


typedef void (*CP_CALLBACK)(void);                    // callback prototype for mouse click event/refresh 
typedef struct CP_DEF  CP_DEF;
typedef struct CP_TYPE CP_TYPE;
typedef struct CP_INTENSITY_COUNT CP_INTENSITY_COUNT;

#define FPS                     50                    // update control panel  windows at this Frames Per Second rate 

#define CP_DF           0x00000001                    // CPANEL debug flags: Definition File processing
#define CP_REFRESH      0x00000002                    // refresh events
#define CP_CMDS         0x00000010 
#define CP_SDL          0x00000020                    // events in SDL Thread


struct CP_DEF {
    int                 *IdVar;                       /* addr of int var where control id named Name will be stored */
    char                *Name;                        /* control Name to locate in definition file */
    CP_CALLBACK         CallBack;                     /* control panel event callback */
    int                 UsedByType;                   /* this control is bind if cpanel type match the given one (0=any) */
    char                *OptName;                     /* this control is bind if OptName option defined (by DefOpt= or by SET OPTION) */
}; 

struct CP_TYPE {
    char                *Name;                        /* control panel type name as stated in definition */
    int                 Id;                           /* and its corresponding integer id */
    CP_DEF              *cp_def;                      /* and its corresponfing control vars definition */
    CP_CALLBACK         Refresh_CallBack;             /* refresh callback */
    CP_CALLBACK         Init_CallBack;                /* initialization after definition file loading and processing */
    CP_CALLBACK         Reset_CallBack;               /* called on reset control panel device */
    CP_CALLBACK         TickIntensity_CallBack;       /* called on ControlPanel_Refresh_CPU_Running */
    CP_CALLBACK         Done_CallBack;                /* called on set cpanel off */
};

typedef struct CP_CLICK {                             // data of mouse clicked control on control panel
    int KeyPress_KeyRelease;                          // if KeyPress_KeyRelease =1 -> key press, =2 -> key release
    int X, Y;                                         // clicked coords relative to GUI Panel Window
    int CId, CArrayId, CArrayItem;                    // control, control array, array item clicked
    int OfsX, OfsY;                                   // clicked coords relative to control origin
} CP_CLICK;                                        

extern CP_CLICK CP_Click;                             // R/O. global variable with clicked control data 
extern char cpanel_default_filename[128];             // R/W. default control panel file name (without path). To be set by SET CPU scp command
extern int cpanel_ControlRedrawNeeded;                // R/W. if set to 1 -> asure that next SetState call will update the control on GUI screen even if state set is same that the control already had
extern int cpanel_State_has_changed;                  // R/O. SetState call sets this to 0 or 1. 1=the state set was not the same the control had
extern int cpanel_on;                                 // R/O. Control Panel GUI window. indicates if window created. 
                                                      //      Inits as =0 -> power off (no GUI window) 
                                                      //      if >0 -> control panel. cpanel_on holds the CPU model
extern int cpanel_stop_flag;                          // R/W. global variable to notify close icon clicked on GUI window
extern int cpanel_interactive;                        // R/W. Set to 1 to read scp commands from control panel 
extern int cpanel_measured_fps;                       // R/O. measured fps value
extern int Refresh_Frames_Count;                      // R/O. frames done counter. At 60 FPS. Wraps after 9942 hours
extern int Refresh_tnow;                              // R/O. value of sim_os_msec() at starting of last refresh. 


// manage control panel visuals at GUI from xxx_cpanel.c
extern t_uint64 SetState(int Id, t_uint64 State);
extern t_uint64 GetState(int Id);
extern int GetCArrayCId(int CArrayId, int n);
extern int SetStateAutoNext(int Id, int NextState, uint32 Duration_msec);
// manage dynamic states
extern uint32 * GetControlSurface(int CId, int State, int * ww, int * hh);
extern int CopyControlImage(int FromCId, int FromState, int x0, int y0, int w, int h, int ToCId, int ToState, int x1, int y1);
extern int DrawTextImage   (int ToCId, int ToState, int x0, int y0, int w0, int h0, int rr, int gg, int bb, char * buf, int chrsz); 
// notify to sim actions requested by user using control panel at xxx_cpanel.c
extern int DoClickEvent(int Id);
extern void DoSCP(char *Name);
// manage control panel in sim main decode inst / scp read line
extern t_stat cpanel_interval_refresh(void);
extern char * cpanel_fgets(const char *prompt, char *cptr, int32 size, FILE *stream);
// manage variable intensity lights based on bit ticks count
extern int TickCount(int Id, t_uint64 n);
extern int SetStateWithIntensity(int Id, t_uint64 n);
// manage options
extern int IsOption(char * Name); 
extern char * IsOptionParam; 
// others
extern int cpanel_scale(int scale); // get/set control panel scale
extern int DoClickEvent(int CId); 




