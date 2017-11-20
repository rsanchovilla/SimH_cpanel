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

   27-Jun-17    RSV     First version control panel support (on HP2100 family)
   10-Nov-17    RSV     control panel support for rcornwell sims IBM 7000 family

   These simulator commands controls the control panel

      SET CPANEL ON          --> Load the control panel definition file
                                 attached, process it, and open a
                                 GUI Similator Window for control panel.
                                 Controls defined are drawn on it.

      SET CPANEL OFF         --> Closes the control panel simulator GUI Window
                                 if this scp command is issued from control 
                                 panel, the interactive mode is turned off

      SET CPANEL OPTION = <NONE> | name
                             --> Defines an option with the given name that 
                                 can be tested for existence in DF with tags 
                                 ifdef, ifndef or elseif. Maximum len for Name 
                                 are 32 characters. If <NONE> is used, all 
                                 options are deleted. 
                                 
      SET CPANEL TICKCOUNT = 0|1
                             --> Set to 0 to disable the variable intensity 
                                 display of lights. Set to 1 to enable them.
                                 The default value 1 is set on attach.
                                 

      SET CPANEL INTERACTIVE --> Allows to operate the control panel in an 
                                 interactive way. This command returns when
                                 user click on GUI window close icon (or if
                                 control panel issues a set cpanel off 
                                 command) or user press the WRU key (default
                                 Control-E) on console 

      SET CPANEL PRESS= name --> Simulates mouse clicking on given control name.
                                 Must be a single control, cannot be a control
                                 array name. This command allows to script
                                 the operation of control panel 

      SET CPANEL STATE= { <ALL> | name } / { * | n }
                             --> Forces on GUI window the state of given 
                                 control to n (a number), so it can be seen on 
                                 this state. If a control array name is given, 
                                 all the individual controls belonging to 
                                 control array are set to the state n. If 
                                 <ALL> is given as name, all controls state
                                 are set. If an asterisk (*) is given instead 
                                 of a number, the control is reset to normal
                                 operation 

      SET CPANEL MARK=<ALL> | <NONE> | name 
                             --> Marks on GUI control panel window the given 
                                 control, so it can be identified. If a control 
                                 array name is given, all the individual controls 
                                 belonging to control array are marked. If 
                                 <ALL> is given as name, all controls that have 
                                 OnClick event are marked (thus showing the
                                 clickable controls). If <NONE> is given as name
                                 all marks are removed.

      SET CPANEL LIST=<ALL> | name 
                             --> List the properties defined in control panel
                                 definition file for the given sigle control
                                 name, or control array name. If <ALL> is given
                                 as name, then all controls defined will be 
                                 listed, the numbers of defined/free controls 
                                 are listed, and the defined options are listed

      SET CPANEL SHOWFPS=0|1 --> Enables or disables the displaying of FPS on 
                                 console

      SET CPANEL SCALE=n     --> Sets the GUI window scale. Default is 100 
                                 (GUI has same size as defined in control panel 
                                 definition file). Use 50 to have a GUI window 
                                 of half size. Use 200 to have GUI window at
                                 double size. n ranges from 10 to 200.
                                 This command must be issued when a panel is 
                                 displayed on GUI window. If not, has no effect.

      SET CPANEL POS=x/y     --> Sets the GUI window position at coord x,y

      ATT CPANEL <file>      --> Define the control panel definition file to be used.
                                 File is opened to check it can be read, and then is 
                                 closed, so it can be modified even if attached. It 
                                 is opened again, read and closed again on set cpanel on.
                                 Attach also resets the options defined. 

      DET CPANEL             --> Detach control panel definition file 

      defaults: SET CPANEL OFF, SHOWFPS=0, TICKCOUNT=1, SCALE=100, no file attached

      CPANEL cannot be disabled. If CPU is stopped, GUI window is not refreshed nor
      polled, so it will not react to mouse clicks. Refresh and clicks reactions
      occurs on next scp command execution. Issue a SET CPANEL ON at any moment 
      to refresh the GUI window and see the results.

      When CPU is executing code, clicking on the close button of GUI windows 
      stops the emulation (same as Ctrl-E on main simulator window). If a scp command
      set cpanel interactive was in execution, it terminates thus returning control 
      to scp prompt (or continuation the script with next scp command). If CPU is
      stopped, the GUI close button does nothing. In any case, the GUI close button
      does not closes the GUI window. To close the GUI window, use the SCP command
      set cpanel off.
 
*/


#include <math.h>
#include "display/ws.h"
#include "cpanel.h"
#include "nanojpeg.h"

// the control panel impementation for simulted computer
// provides this array with the needed callbacks
extern CP_TYPE cp_types[];

DEVICE cp_dev;                                          /* control panel device */

#define UNIT_V_POWERON  (UNIT_V_UF + 0)                 /* unit powered on */
#define UNIT_POWERON    (1 << UNIT_V_POWERON)

t_stat cp_refresh_svc   (UNIT *uptr);
t_stat cp_set_cpanel_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat cp_set_param     (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat cp_interactive   (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat cp_attach        (UNIT *uptr, CONST char *cptr);
t_stat cp_dettach       (UNIT *uptr);
t_stat cp_reset         (DEVICE *dptr);

DEBTAB sim_cp_debug[] = {
  {"DF",        CP_DF,             "Control Panel Definition File processing"},
  {"CMDS",      CP_CMDS,           "Control Panel Commands"},
  {"REFRESH",   CP_REFRESH,        "GUI Refresh events"},
  {"SDL",       SIM_VID_DBG_VIDEO, "GUI SDL Events processing"},
  {"DETAIL",    CP_DETAIL,         "devices display events detail"},
  {0}
};

UNIT cp_unit = {
    UDATA (NULL, UNIT_SEQ+UNIT_ATTABLE+UNIT_TEXT, 0)
    };

MTAB cp_mod[] = {
    { UNIT_POWERON, UNIT_POWERON,  "cpanel on",              "ON",           cp_set_cpanel_on },
    { UNIT_POWERON, 0,             "cpanel off",             "OFF",          cp_set_cpanel_on },
    { MTAB_XTD | MTAB_VDV, 0,       NULL,                    "SHOWFPS",      &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 1,       NULL,                    "PRESS",        &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 2,       NULL,                    "MARK",         &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 3,       NULL,                    "LIST",         &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 4,       NULL,                    "STATE",        &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 5,       NULL,                    "OPTION",       &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 6,       NULL,                    "TICKCOUNT",    &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 7,       NULL,                    "SCALE",        &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 8,       NULL,                    "POS",          &cp_set_param, NULL, &cp_dev },
    { MTAB_XTD | MTAB_VDV, 0,       NULL,                    "INTERACTIVE",  &cp_interactive, NULL, &cp_dev },
    { 0 }
    };

DEVICE cp_dev = {
    "CPANEL", &cp_unit, NULL /* cp_reg */, cp_mod,      // unit needed to attach the definition file
    1, 10, 31, 1, 8, 8,                                 //??? what are these numbers ???
    NULL, NULL, &cp_reset,
    NULL, &cp_attach, &cp_dettach,
    NULL /* &cp_dib */, DEV_DEBUG, 0, sim_cp_debug      /* unit allways enabled */
    };


#define MAX_CONTROLS            1000                    // max number of controls per control panel
#define MAX_CARRAYS              128                    // max number of controls arrays per control panel
#define MAX_CARRAYITEMS         1000                    // max number of controls in control arrays
#define MAX_PIXELS      16*1024*1024                    // max number of pixels on control panel
#define MAX_IMAGES                32                    // max number of images for controls that can be loaded at same time
#define MAX_OPTIONS             2048                    // max number of chars to store options defined for control panel
#define MAX_TICK_COUNT           128                    // max mumber of register tick count 

#define CARRAY_START       (MAX_CONTROLS+1)         // first Id for Control Arrays, so Ids 0..MAX_CONTROLS are controls
                                                    // and Ids > MAX_CONTROLS are control arrays. This allows to get the
                                                    // type (single control or control array) just from Id value
// global vars
char ControlPanel_Name[128] = "";                   // control panel name. Displayed on GUI window title
char ControlPanel_Default_filename[128] = "";       // default control panel file name (without path). To be set by SET CPU scp command
int cpanel_Press_and_Release = 0;                   // =0 -> only one call to OnClick event when click on screen, =1 -> on call for key press down (mouse down), another for key release (mouse button release)
int cpanel_ControlRedrawNeeded = 0;                 // if set to 1 -> asure that next SetState call will update the control on GUI screen even if state set is same that the control already had
int cpanel_State_has_changed = 0;                   // SetState call sets this to 0 or 1. 1=the state set was not the same the control had
int cpanel_on = 0;                                  // Control Panel GUI window. indicates if window created. 
                                                    // Inits as =0 -> power off (no GUI window) 
                                                    // if >0 -> control panel. cpanel_on holds the control panel type id
int cpanel_interactive = 0;                         // =1 to read scp commands from control panel 

int CP_ShowFPS = 0;                                 // =1 display fps via printf (ugly but effective) 
int32 cpanel_interval = 0;                          // cpanel is cheched for refresh/event each cpanel_interval cpu instructions (0= -> no check)
CP_CLICK CP_Click;                                  // global variable with clicked control data
int cp_stop_flag = 0;                               // global variable to notify close icon clicked on GUI window

// private vars
static struct {                                     // Control Panel main data
    int ControlItems;                               // total number of defined controls (= items in Control[]
    int CArrayItems;                                // total number of defined control arrays (= items in CArray[]
    int SurfaceItems;                               // total number of defined pixels in surface arrays (= items in surface[]
    int ImgItems;                                   // total number of loaded images
    int TickCountItems;                             // total number of tick count registers in use
    struct {
        int State, nStates;                         // current state of the control, max number of possible states (=0 if control has no pixels (transparent) and only senses the click event 
        char Name[32];         
        int X, Y, W, H;                             // x,y position and witfth height. relative to top left corner of window. 
                                                    // if x=-1, control not draw nor sensed
        int MaxH;                                   // if >0, draw upo to this height (must be < to H)
        CP_CALLBACK OnClick;                        // onclick event function
        int iPos_surface;                           // start of pixel raw data in surface array
        int ByRef;                                  // =1 -> controls SameAs this one points to this control pixels instead on getting a copy of them
        int LastState;                              // LastState drawn on GUI (needed to draw only changes)
        int Mark;                                   // =1 -> show the control marked with and B/W semitransparent overlay 
                                                    // if bit 8 set, force the state for Mark & $ff
        struct {
            int State;                              // the refresh routine automaticaly set this state on control ...
            uint32 Duration_msec;                   // ... after this time has expired in wallclock ...
            uint32 Time0_msec;                      // ... counting from this moment in time
        } AutoNext;
        int nTick;                                  // entry used in TickCount array (-1 if none used) 
    } Control[MAX_CONTROLS];                        // controls defined
    struct {
        int Items;                                  // items in control array 
        int CId0;                                   // 1st controls inside control array
        char Name[32];     
        CP_CALLBACK OnClick;                        // onclick event function
        int nTick;                                  // entry used in TickCount array (-1 if none used) 
    } CArray[MAX_CARRAYS];
    int CArrayCidItems[MAX_CARRAYITEMS];            // control id items inside CArray
    uint32 * surface;                               // raw pixel data for controls
    struct {
        char Name[32];                              // images loaded to be used for control's states
        uint32 * surface;                           // raw pixel data 
        int W,H;
    } Img[MAX_IMAGES];
    struct {
        int32 Num;                                  /* total number of ticks counted (-1 -> entry is free ) */
        int32 Bit[64];                              /* number of ticks counted on that bit */
    } TickCount[MAX_TICK_COUNT];
    char Options[MAX_OPTIONS];                      // options defined (separated by ;)
    int FullRedrawNeeded;                           // =1 if a full redraw is needed
    int EnableTickCount;                            // =0 -> do not use TickCount callback and disable SetStateWithIntensity
    int Scale;                                      // Scale factor for GUI window. 100 -> 100% -> GUI window with size as defined. 50% -> half size
    uint32 LastRefreshInit;                         /* sim_os_msec() at starting of last refresh. used to calc FPS */
    uint32 LastRefreshDone;                         /* sim_os_msec() at ending of last refresh. used to calc if refresh needed */
    CP_CALLBACK CP_Refresh;                         // Control Panel refresh Callback
    CP_CALLBACK CP_Reset;                           // Control Panel reset Callback. Called on cpanel reset
    CP_CALLBACK CP_TickIntensityCount;              // callback to increment the tick count to calculate lights/leds intensity
} CP = { 0, 0, 0, 0, 0 }; 

UNIT * cp_unit_ptr = NULL;                          // pointer to control panel unit and device
DEVICE * cp_dev_ptr = NULL;

#define MAX_SCP_CMDS        16                      // max number scp commnads can be queued by control panel
#define MAX_SCP_CMD_LEN     80                      // max len of a scp command
char SCP_cmd[MAX_SCP_CMDS * MAX_SCP_CMD_LEN];       // stores scp commnads list queued by control panel pending to be executed
int  SCP_cmd_count = 0;                             // number of commands in list

int cpanel_TypeId = 0;                              // control panel type id from definition file

// forward declararion for 
int DoActionByName(char * Name, int Action);

void display_keydown(int k)
{
    int i, bMark; 

    // check if ^E (WRU) pressed while GUI has focus
    if (k == sim_int_char) {
        // it is! simulate click on close gui window to stop cpu execution
        cp_stop_flag = 1;     
    } else if (k == ('T'-'A'+1)) {
        // Control-T -> toggle mark on GUI all clickable areas
        bMark = 0;
        for (i=0; i<CP.ControlItems;i++) if (CP.Control[i].Mark) bMark = 1;
        if (bMark == 0) {
            DoActionByName("<ALL>", 1);
        } else {
            DoActionByName("<NONE>", 1);
        }
    }
}

void display_keyup(int k)
{
}

int upcase(int c)
{
    if ((c >= 'a') && (c <= 'z')) return c - 'a' + 'A';
    return c;
}

// compare str up to len, case insensitive
int cmpstr(char * s1, char * s2, int len) 
{
    int i,c1,c2; 

    if ((s1 == NULL) || (s2 == NULL)) return 0;  // no strings -> not equals
    if ((s1[0] == 0) || (s2[0] == 0)) return 0;  // strings connot be empty
    for (i=0;;i++) {
        c1 = s1[i]; c2 = s2[i];
        if ((len > 0) && (i==len))  return 1;  // no diferences found in len chars -> str are equal
        if ((c1 == 0) && (c2 == 0)) return 1;  // strings have same len -> str are equal
        if (upcase(c2) != upcase(c1)) return 0;
    }
}

int GetByName(char *Name) 
{
    int i,CId;
    if ((Name == NULL) || (Name[0] == 0)) {
        return -1;
    }
    // search a Control called Name
    for (i=0;i<CP.ControlItems;i++) {
        CId = i;
        if (cmpstr(CP.Control[i].Name, Name, 32)) return CId;
    }
    // control not found, now search as control array
    for (i=0;i<CP.CArrayItems;i++) {
        if (cmpstr(CP.CArray[i].Name, Name, 32)) {
            CId = i + CARRAY_START;
            return CId;
        }
    }
    return -1;
}

// Add definition of a new control. 
int AddControl(char *Name, int nStates,
               int X, int Y, int W, int H)
{
    int CId, i, n;
    char c; 
    // Control Id
    // check for room and get the control id
    if (CP.ControlItems >= MAX_CONTROLS) {
        fprintf(stderr, "No room to add control %s\r\n", Name);
        return -1; 
    }
    if ((Name != NULL) && (Name[0] != 0) && (GetByName(Name) >= 0)) {
        fprintf(stderr, "Already exists a Control or Control Array with name %s\r\n", Name);
        return -1; 
    }
    CId = CP.ControlItems++;
    // add the data
    CP.Control[CId].X = X;
    CP.Control[CId].Y = Y;
    CP.Control[CId].W = W;
    CP.Control[CId].H = H;
    CP.Control[CId].MaxH = 0;
    CP.Control[CId].nStates = nStates;      // if nStates==0 -> control not drawn. Just it is used for click on its area
    CP.Control[CId].OnClick = NULL;
    CP.Control[CId].nTick = -1;             // no tickcount by defautt
    CP.Control[CId].ByRef = 0;              // =0 no ByReference on SameAs by default

    // init auto next state (automaticaly changes the state after msec time)
    CP.Control[CId].AutoNext.State = -1;    // no auto next 
    // copy the name up to 31 chars. zero terminated. 
    if (Name == NULL) {
        CP.Control[CId].Name[0] = 0;
    } else for (i=0;i<32;i++) {
        if (i==31) {c = 0;} else {c = Name[i]; }
        CP.Control[CId].Name[i] = c;
        if (c == 0) break;
    }
    // see if room for pixels of control, and get space for pixels
    CP.Control[CId].iPos_surface = CP.SurfaceItems;
    n = CP.SurfaceItems + nStates * W * H + 1;
    if (n >= MAX_PIXELS) return -1; // no room for pixels of control!
    for (i=CP.Control[CId].iPos_surface; i<n;i++) CP.surface[i] = 0;     // fill with transparent 
    CP.SurfaceItems = n; 
    // int state 
    CP.Control[CId].State = 0;
    CP.Control[CId].Mark = 0;
    CP.Control[CId].LastState = -1;                                      // To force refresh
    // finished
    return CId;
}

int CHK_CId(int CId)
{
    if ((CId < 0) || (CId > CP.ControlItems)) {
        fprintf(stderr, "CId %d out of range (Items %d)\r\n", CId, CP.ControlItems); 
        return -1; 
    }
    return 0;
}

#define CHK_CArrayId(CArrayId)  if ((CArrayId < CARRAY_START) || (CArrayId > CARRAY_START+CP.CArrayItems)) {fprintf(stderr, "CArrayId %d out of range (Items %d)\r\n", CArrayId, CP.CArrayItems); return -1; }
#define CHK_Item(CArrayId,n)    if ((n < 0) || (n >= CP.CArray[CArrayId-CARRAY_START].Items)) {fprintf(stderr, "Item %d out of range (CArrayId %d, Items %d)\r\n", n, CArrayId, CP.CArray[(CArrayId)-(CARRAY_START)].Items); return -1; }

void get_surface_rgb_color(uint32 color, 
               int *r_Color, int *g_Color, int *b_Color)
{
    if (sim_end) {
        color = color & 0x00FFFFFF;
        *r_Color = (color >> 16);
        *g_Color = ((color & 0x00FF00) >>  8);
        *b_Color = (color & 0x0000FF);
    } else {
        color = color >> 8;
        *r_Color = (color & 0x0000FF);
        *g_Color = ((color & 0x00FF00) >>  8);
        *b_Color = (color >> 16);
    }
}

uint32 surface_rgb_color(uint32 r, uint32 g, uint32 b)  /* r,g,b are 8bit! */
{
    uint32 color;

    // rgb to 16 bits 
    if (r < 0) r = 0; if (g < 0) g = 0; if (b < 0) b = 0;
    r = r << 8; g = g << 8; b = b << 8; 

    color = sim_end ? 
               (0xFF000000 | ((r & 0xFF00) << 8) | (g & 0xFF00) | ((b & 0xFF00) >> 8)) : 
               (0x000000FF | (r  & 0xFF00) | ((g & 0xFF00) << 8) | ((b & 0xFF00) << 16));
    return color;
}

// add pixels to controls for given state. 
// if col == 1 -> return pixel color instear of setting it
uint32 AddControlPixel(int CId, int State, int X, int Y, uint32 col)
{
    int W, H, n;
    if (CHK_CId(CId) < 0) return 0; 
    if (State >= CP.Control[CId].nStates) return 0;
    W = CP.Control[CId].W;
    H = CP.Control[CId].H;
    if ((X < 0) || (Y < 0) || (X >= W) || (Y >= H)) return 0;           // sanity check;
    // set the pixel
    n = CP.Control[CId].iPos_surface;                                   // start of control's pixels
    n = n + State * W * H;                                              // start of this state 
    n = n + Y * W + X;                                                  // pos of this pixel
    if (n >= MAX_PIXELS) return 0;                                      // sanity check
    if (col == 1) {
        return CP.surface[n]; 
    }
    CP.surface[n] = col;                                                // set the pixel
    return 0;
}

// copy pixels from one control to another for all states
int CopyControlPixels(int OrgCId, int DestCId)
{
    int i, iOrg, iDest, W,H,nStates;

    if (CHK_CId(OrgCId) < 0) return -1;                                  // sanity check
    if (CHK_CId(DestCId) < 0) return -1;                                // sanity check
    iOrg  = CP.Control[OrgCId].iPos_surface;                            // start of origine control's pixels
    iDest = CP.Control[DestCId].iPos_surface;                           // start of destination control's pixels
    W = CP.Control[OrgCId].W;
    H = CP.Control[OrgCId].H;
    nStates = CP.Control[OrgCId].nStates;
    if ( (W != CP.Control[DestCId].W) || 
         (H != CP.Control[DestCId].H) ||
         (nStates != CP.Control[DestCId].nStates) ) {
        fprintf(stderr, "Cannot copy pixels: Org and Dest controls have diferent geometry\r\n");
        return -1; 
    }
    for (i=0;i< nStates * W * H;i++) {
        CP.surface[iDest + i] = CP.surface[iOrg + i];
    }
    return 0;
}

// draw circle
int AddCircle(int CId, int State, int W, int H, uint32 col1)
{
    int i,x,y,r0,g0,b0,r1,g1,b1,rr,gg,bb, r, w2, h2;
    int bAntiAliased; 
    uint32 col0;
    double nd, xd, yd;

    if (CHK_CId(CId) < 0) return -1;                                // sanity check
    if (State >= CP.Control[CId].nStates) return -1;
    if (W > CP.Control[CId].W) return -1; 
    if (H > CP.Control[CId].H) return -1;
    if ((W < 2) || (H < 2)) return -1;
    col0 = AddControlPixel(CId, State, 0, 0, 1);                    // get the color under filled circle. Use control's pixel 0,0 
    if (col0 == 0) {
        bAntiAliased = 0;                                           // under circle no color (== 0 -> transparent) -> no antialiasing
    } else {
        get_surface_rgb_color(col0, &r0, &g0, &b0);                 // there is a color! convert it to rgb
        bAntiAliased = 1; 
    }
    w2 = W / 2; h2 = H / 2; 
    if (col1 == 0) {                                                // col==0 -> paint with transparent color
        bAntiAliased = 0; 
        r1 = g1 = b1 = 0;
    } else {
        get_surface_rgb_color(col1, &r1, &g1, &b1);                     // convert circle color to rgb
    }
    r = w2; 
    for(x=0;x<2*r*0.35+1;x++) {
        yd = sqrt((r-1)*(r-1)-x*x) * (double)(H) / (double)(W);
        y  = (int) yd;
        nd = (yd - y);                                              // antialiase to look great
        if ((nd > 0) && (y > 0) && (y < H-1) && (bAntiAliased)) {   // there is an antialiased pixel and room for it
            rr = (int) ((r1 - r0) * nd + r0);                       // calc the color for antialiased pixel
            gg = (int) ((g1 - g0) * nd + g0);
            bb = (int) ((b1 - b0) * nd + b0);
            col0 = surface_rgb_color(rr, gg, bb);
            AddControlPixel(CId, State, w2 + x, h2 + y + 1, col0);  // set antialiased pixel 
            AddControlPixel(CId, State, w2 + x, h2 - y - 1, col0); 
            AddControlPixel(CId, State, w2 - x, h2 + y + 1, col0); 
            AddControlPixel(CId, State, w2 - x, h2 - y - 1, col0); 
        }
        for (i=-y;i<=y;i++) { 
            AddControlPixel(CId, State, w2 + x, h2 + i, col1);      // set pixel color 
            AddControlPixel(CId, State, w2 - x, h2 + i, col1);      // set pixel color 
        }
    }
    r = h2; 
    for(y=0;y<2*r*0.35+1;y++) {
        xd = sqrt((r-1)*(r-1)-y*y) * (double)(W) / (double)(H);
        x  = (int) xd;
        nd = (xd - x);                                              // antialiase to look great
        if ((nd > 0) && (x > 0) && (x < W-1) && (bAntiAliased)) {   // there is an antialiased pixel and room for it
            rr = (int) ((r1 - r0) * nd + r0);                       // calc the color for antialiased pixel
            gg = (int) ((g1 - g0) * nd + g0);
            bb = (int) ((b1 - b0) * nd + b0);
            col0 = surface_rgb_color(rr, gg, bb);
            AddControlPixel(CId, State, w2 + x + 1, h2 + y, col0);  // set antialiased pixel 
            AddControlPixel(CId, State, w2 - x - 1, h2 + y, col0); 
            AddControlPixel(CId, State, w2 + x + 1, h2 - y, col0); 
            AddControlPixel(CId, State, w2 - x - 1, h2 - y, col0); 
        }
        for (i=-x;i<=x;i++) { 
            AddControlPixel(CId, State, w2 + i, h2 + y, col1);      // set pixel color 
            AddControlPixel(CId, State, w2 + i, h2 - y, col1);      // set pixel color 
        }
    }
    return 0;
}

// interpolate pixels for state nState to generate
// intermediate intensities mixing State0 (color for
// intensity 0%) and State1 (color for intensity 100%)
int InterpolateControlPixels(int CId, int nState)
{
    int n, iOrg, iOrg0, iOrg1, W,H,nStates;
    double r;
    int r0,g0,b0,r1,g1,b1,rr,gg,bb;
    uint32 col0, col1, col;

    if (CHK_CId(CId) < 0) return -1;                                // sanity check
    W = CP.Control[CId].W;
    H = CP.Control[CId].H;
    nStates = CP.Control[CId].nStates;
    if ((nState < 2) || (nState >= nStates)) return -1;
    iOrg0 = CP.Control[CId].iPos_surface;                           // start of origin control's pixels
    iOrg1 = iOrg0 + W * H;
    iOrg  = iOrg0 + nState * W * H;
    r = (nState-1) / (double) nStates;
    for (n=0;n<W*H;n++) {
        col0 = CP.surface[iOrg0 + n];
        col1 = CP.surface[iOrg1 + n];
        if ((col0 == 0) || (col1 == 0)) {
            col = 0;                                                // transparent pixels are kept
        } else {
            get_surface_rgb_color(col0, &r0, &g0, &b0);             // there is a color! convert it to rgb
            get_surface_rgb_color(col1, &r1, &g1, &b1);      
            rr = (int) ((r1 - r0) * r + r0);                        // calc the color for mixed pixel
            gg = (int) ((g1 - g0) * r + g0);
            bb = (int) ((b1 - b0) * r + b0);
            col = surface_rgb_color(rr, gg, bb);
        }
        CP.surface[iOrg + n] = col;
    }
    return 0;
}

// Add definition of a new control array. returns the control array id, or -1 if no room
int AddCArray(char *Name)
{
    int CArrayId, CId0, i;
    char c; 
    // Control Array Id
    // check for room and get the control array id
    if (CP.CArrayItems >= MAX_CARRAYS) {
        fprintf(stderr, "No room to add control array %s\r\n", Name);
        return -1;
    }
    if ((Name != NULL) && (Name[0] != 0) && (GetByName(Name) >= 0)) {
        fprintf(stderr, "Already exists a Control or Control Array with name %s\r\n", Name);
        return -1; 
    }
    CArrayId = CARRAY_START+CP.CArrayItems;
    if (CP.CArrayItems == 0) {
        CId0 = 0;
    } else {
        // next free item CId0 is next from previos CArray
        CId0 = CP.CArray[CArrayId-CARRAY_START-1].CId0 + CP.CArray[CArrayId-CARRAY_START-1].Items;
    }
    CP.CArray[CArrayId-CARRAY_START].CId0 = CId0;
    CP.CArray[CArrayId-CARRAY_START].Items = 0;
    CP.CArray[CArrayId-CARRAY_START].nTick = -1; // no tick count by default
    CP.CArrayItems++;
    // add the data
    CP.CArray[CArrayId-CARRAY_START].OnClick = NULL;
    // copy the name up to 31 chars. zero terminated. 
    if (Name == NULL) {
        CP.CArray[CArrayId-CARRAY_START].Name[0] = 0;
    } else for (i=0;i<32;i++) {
        if (i==31) {c = 0;} else {c = Name[i]; }
        CP.CArray[CArrayId-CARRAY_START].Name[i] = c;
        if (c == 0) break;
    }
    // finished
    return CArrayId;
}

int AddCArrayItem(int CArrayId, int CId)
{
    int CId0, n;
    if (CHK_CId(CId) < 0) return -1;                                // sanity check
    CHK_CArrayId(CArrayId)              
    if (CArrayId != CARRAY_START+CP.CArrayItems - 1) {
        fprintf(stderr, "Can only add controls to last defined CArray (Current CArrayId %d, Last defined CArrayId %d)\r\n", 
            CArrayId, CARRAY_START+CP.CArrayItems - 1);
        return -1;
    }
    n = CP.CArray[CArrayId-CARRAY_START].Items;
    CId0 = CP.CArray[CArrayId-CARRAY_START].CId0;
    if (CId0 + n >= MAX_CARRAYITEMS) {
        fprintf(stderr, "No room to add control %d to control array %d\r\n", CId, CArrayId);
        return -1;
    }
    CP.CArray[CArrayId-CARRAY_START].Items++;
    CP.CArrayCidItems[CId0 + n] = CId;
    return 0;
}

int AddJPEGImage(char * ImgName, char * FileName)
{
    int i,j, nImage, W,H, r,g,b;
    char* p0;
    int32 size,p1,n;
    char *buf;
    FILE *f;
    uint32 col;
    // get room in images array
    for (i=0;i<CP.ImgItems;i++) {
        if (cmpstr(CP.Img[i].Name, ImgName, -1) == 0) continue;
        // image already defined -> free it
        if (CP.Img[i].surface) {
            free(CP.Img[i].surface);
            CP.Img[i].surface = NULL; 
        }
        nImage = i;
        break;
    }
    if (i>=CP.ImgItems) {
        // new image
        if (CP.ImgItems >= MAX_IMAGES) {
            fprintf(stderr, "Too many Images loaded\r\n");
            return -1;
        }
        i=CP.ImgItems++;
        // set the name
        for (j=0;ImgName[j];j++) CP.Img[i].Name[j] = ImgName[j];
        CP.Img[i].Name[j] = 0;
        nImage = i;
    }
    // load whole JPEG image file into buf buffer
    f = sim_fopen(FileName, "rb");
    if (!f) {
        fprintf(stderr, "Error opening the input file %s\n", FileName);
        return -1;
    }
    sim_fseek(f, 0, SEEK_END);
    size = (int) sim_ftell(f);
    buf = (char*) malloc(size);
    if (buf == NULL) {
        fprintf(stderr, "Error creating JPEG buf for %d bytes\r\n", size);
        return -1;
    }
    sim_fseek(f, 0, SEEK_SET);
    size = (int) sim_fread(buf, 1, size, f);
    fclose(f);
    if (size == 0) {
        if (buf) free((void*)buf);
        fprintf(stderr, "Error loading JPEG image from %s\r\n", FileName);
        return -1;
    }
    // now process buf to decode image
    njInit();
    if (n=njDecode(buf, size)) {
        if (buf) free((void*)buf);
        fprintf(stderr, "Error %d decoding JPEG image from %s\r\n", n, FileName);
        return -1;
    }
    // free JPEG file buf. Image stored in internal nj buffer
    free((void*)buf);
    CP.Img[nImage].W = W = njGetWidth();
    CP.Img[nImage].H = H = njGetHeight();
    // prepare to receive the image in surface format
    CP.Img[nImage].surface = (uint32 *)malloc ((W * H)*sizeof(*CP.surface));
    if (CP.Img[nImage].surface) for (p1=0; p1<W*H;p1++) CP.Img[nImage].surface[p1] = 0; // fill with transparent color
    p1   = 0;
    p0   = njGetImage();
    size = njGetImageSize();
    // copy image with surface format
    for(;;) {
        if (CP.Img[nImage].surface == NULL) break;
        if (p1 >= W*H) break;
        if (size < 3) break;
        size -= 3;
        r = *p0++;
        g = *p0++;
        b = *p0++;
        col = surface_rgb_color(r,g,b);
        CP.Img[nImage].surface[p1++] = col;
    }
    // cleanup (free internal nj buffer) and exit
    njDone();
    return 0;
}

uint8 *Font = NULL;     // pointer to block of malloc mem that stores the internal font

static const char *FontBmp[] = {
    "xxxxx ..... ..x.. .x.x. .x.x. ..x.. ....x ..xx. ",
    "x...x ..... ..x.. .x.x. .x.x. .xxx. xx.x. .x..x ",
    "x...x ..... ..x.. .x.x. xxxxx x.x.. xx.x. ..x.. ",
    "x...x ..... ..x.. ..... .x.x. .xxx. ..x.. .x.x. ",
    "x...x ..... ..x.. ..... xxxxx ..x.x .x.xx x.... ",
    "x...x ..... ..... ..... .x.x. .xxx. .x.xx x...x ",
    "xxxxx ..... ..x.. ..... .x.x. ..x.. x.... .xxx. ",

    "..x.. ...x. .x... ..x.. ..x.. ..... ..... ..... ",
    "..x.. ..x.. ..x.. x.x.x ..x.. ..... ..... ..... ",
    "..x.. .x... ...x. .xxx. ..x.. ..... ..... ..... ",
    "..... .x... ...x. xxxxx xxxxx ..... xxxxx ..... ",
    "..... .x... ...x. .xxx. ..x.. ..x.. ..... ..... ",
    "..... ..x.. ..x.. x.x.x ..x.. ..x.. ..... .xx.. ",
    "..... ...x. .x... ..x.. ..x.. .x... ..... .xx.. ",

    "...x. .xxx. ..x.. .xxx. .xxx. ...x. xxxx. .xxx. ",
    "..x.. x..xx .xx.. x...x x...x ..xx. x.... x...x ",
    "..x.. x.x.x ..x.. ....x ....x .x.x. x.... x.... ",
    ".x... x.x.x ..x.. .xxx. .xxx. x..x. xxxx. xxxx. ",
    ".x... xx..x ..x.. x.... ....x xxxxx ....x x...x ",
    "x.... xx..x ..x.. x.... x...x ...x. x...x x...x ",
    "x.... .xxx. .xxx. xxxxx .xxx. ...x. .xxx. .xxx. ",

    "xxxxx .xxx. .xxx. ..... ..... ...x. ..... x.... ",
    "....x x...x x...x ..... ..... ..x.. ..... .x... ",
    "...x. x...x x...x .xx.. ..x.. .x... xxxxx ..x.. ",
    "..x.. .xxx. .xxxx .xx.. ..... x.... ..... ...x. ",
    "..x.. x...x ....x ..... ..x.. .x... xxxxx ..x.. ",
    "..x.. x...x x...x .xx.. ..x.. ..x.. ..... .x... ",
    "..x.. .xxx. .xxx. .xx.. .x... ...x. ..... x.... ",

    ".xxx. .xxx. ..x.. xxxx. .xxx. xxxx. xxxxx xxxxx ",
    "x...x x.xxx .x.x. x...x x...x x...x x.... x.... ",
    "....x x.x.x x...x x...x x.... x...x x.... x.... ",
    "...x. x.x.x xxxxx xxxx. x.... x...x xxx.. xxx.. ",
    "..x.. x.xx. x...x x...x x.... x...x x.... x.... ",
    "..... x...x x...x x...x x...x x...x x.... x.... ",
    "..x.. .xxx. x...x xxxx. .xxx. xxxx. xxxxx x.... ",

    ".xxx. x...x .xxx. ..xxx x...x x.... x...x x...x ",
    "x...x x...x ..x.. ...x. x...x x.... xx.xx xx..x ",
    "x.... x...x ..x.. ...x. x..x. x.... x.x.x x.x.x ",
    "x..xx xxxxx ..x.. ...x. xxx.. x.... x.x.x x..xx ",
    "x...x x...x ..x.. ...x. x..x. x.... x.x.x x...x ",
    "x...x x...x ..x.. x..x. x...x x.... x...x x...x ",
    ".xxx. x...x .xxx. .xx.. x...x xxxxx x...x x...x ",

    ".xxx. xxxx. .xxx. xxxx. .xxx. xxxxx x...x x...x ",
    "x...x x...x x...x x...x x...x ..x.. x...x x...x ",
    "x...x x...x x...x x...x x.... ..x.. x...x x...x ",
    "x...x xxxx. x...x xxxx. .xxx. ..x.. x...x x...x ",
    "x...x x.... x.x.x x.x.. ....x ..x.. x...x x...x ",
    "x...x x.... x..x. x..x. x...x ..x.. x...x .x.x. ",
    ".xxx. x.... .xx.x x...x .xxx. ..x.. .xxx. ..x.. ",

    "x...x x...x x...x xxxxx .xxxx x.... xxxx. ..x.. ",
    "x...x x...x x...x ....x .x... .x... ...x. .x.x. ",
    "x...x .x.x. .x.x. ...x. .x... .x... ...x. x...x ",
    "x...x ..x.. ..x.. ..x.. .x... ..x.. ...x. ..... ",
    "x.x.x .x.x. ..x.. .x... .x... ..x.. ...x. ..... ",
    "x.x.x x...x ..x.. x.... .x... ...x. ...x. ..... ",
    ".x.x. x...x ..x.. xxxxx .xxxx ...x. xxxx. ..... ",
     
    "..... .x... ...xx ..x.. xx... ..... ..... ..... ",
    "..... .x... ..x.. ..x.. ..x.. ..... ..... ..... ",
    "..... ..x.. ..x.. ..x.. ..x.. .x... ..... ..... ",
    "..... ..... xx... ..x.. ...xx x.x.x ..... ..... ",
    "..... ..... ..x.. ..x.. ..x.. ...x. ..... ..... ",
    "..... ..... ..x.. ..x.. ..x.. ..... ..... ..... ",
    "xxxxx ..... ...xx ..x.. xx... ..... ..... ..... ",

    "",
};

// load internal font (if bLoad==1), else unload internal font
void FontLoad(int bLoad) 
{
    int i,b,g,c,ix,iy,x,y;
    if (bLoad == 0) {
        if (Font) free(Font);
        Font = NULL;
        return;
    }
    Font = (uint8 *)malloc (129*7*sizeof(*Font));
    // clear all
    for (i=0;i<129*7;i++) Font[i]=0;
    b=0; ix=0; iy=0;
    while (b < 70) {
        for (y=0;y<7;y++) {
            g = 0;
            for (x=4;x>=0;x--) {
               c = FontBmp[iy*7+y][ix*6+x];
               g = (g << 1);
               if (c == 'x') g = g | 1;
            }
            Font[b*7+y] = g;
        }
        b++;
        ix++;
        if (ix>7) {
            ix=0;
            iy++;
        }
    }
}

char * IsOptionParam; // points to / of found OPT/PARAM, else NULL
int nIsOption; // char num in Options array of found OPT/PARAM, else 0
// return 1 if Name is in options buffer.
int IsOption(char * Name)
{
    int iOpt, iName;
    char cOpt, cName; 

    nIsOption = 0;
    IsOptionParam = NULL;
    if ((Name == NULL) || (Name[0] == 0)) return 0;
    iOpt = 0; iName = 0; 
    while(1) {
        cName = Name[iName];
        cOpt  = CP.Options[iOpt];
        if (((cName == 0) || (cName == '/')) && ( (cOpt == 0) || (cOpt == ',') || (cOpt == '/') ) ) {
            if (cOpt == '/') IsOptionParam = &CP.Options[iOpt];
            return 1; // Option found
        }
        if (upcase(cName) == upcase(cOpt)) {
            iName++; iOpt++; // Name partially found ... keep comparing
            continue;
        }
        // not found. Skip current option up to start of next one
        for (; (cOpt=CP.Options[iOpt]) && (cOpt != ','); iOpt++);
        if (cOpt == 0) return 0; // no more options
        iOpt++; iName = 0;
        nIsOption = iOpt;
    }
}

// add Name to CP.Options array if does not already in
// if Name = "<NONE>" clears the defined options
// return -1 if invalid name because options buffer is full, of becuase Name is empty
int AddOption(char * Name)
{
    int i,j,n,c;

    if ((Name == NULL) || (Name[0] == 0)) return -1;
    if (cmpstr(Name, "<NONE>",0)) {
        // empty options buffer
        CP.Options[0] = 0;
        return 0;
    } 
    if (IsOption(Name)) {
        // option already present, remove it
        for (i=nIsOption;; i++) { // i points to start of option found 
            c=CP.Options[i]; 
            if (c == 0) { // end of option, so the found one was the last
                CP.Options[nIsOption]=0;
                break;
            }
            if (c == ',') { // end of this option, i points to next one
                n = i+1;
                for (i=nIsOption;c;) c=CP.Options[i++]=CP.Options[n++]; // copy from i+1 up to end of screen
                break;
            }
        }
    }
    // check if room to add Name to options array
    for (i=0; CP.Options[i]; i++); // i points to end of string NULL
    for (n=0; Name[n]; n++); // n = chars length
    if (n > 32) return -1;   // option too long
    if (i+n+2 >= MAX_OPTIONS) 
        return -1;           // no room for option
    // if buffer no empty, add a separator
    if (i > 0) {
        CP.Options[i++] = ',';
        CP.Options[i] = 0;
    }
    // add Name to buffer
    for(j=0;j<n;j++) CP.Options[i++] = Name[j];
    CP.Options[i] = 0;
    return 0;
}

#define CPDF_TAG_COMMENT            1
#define CPDF_TAG_CPNAME             2
#define CPDF_TAG_CPTYPE             3
#define CPDF_TAG_LOAD_IMG           4
#define CPDF_TAG_CNAME              5
#define CPDF_TAG_BASEXY             6
#define CPDF_TAG_XYWH               11
#define CPDF_TAG_WH                 12
#define CPDF_TAG_NSTATES            13
#define CPDF_TAG_INITIALSTATE       14
#define CPDF_TAG_STATEIMG           20
#define CPDF_TAG_STATERGB           21
#define CPDF_TAG_STATERGB_CIRCLE    22
#define CPDF_TAG_STATERGB_AREA      23
#define CPDF_TAG_STATERGB_TEXT      24
#define CPDF_TAG_CARRAY             31
#define CPDF_TAG_CARRAYITEM         32
#define CPDF_TAG_SAME               33
#define CPDF_TAG_HEIGHT             34
#define CPDF_TAG_SAME_BYREF         35
#define CPDF_TAG_AUTOSTATES         36
#define CPDF_TAG_TRANSPARENT        37
#define CPDF_TAG_IFOPT              41
#define CPDF_TAG_IFNOPT             42
#define CPDF_TAG_ELSE               43
#define CPDF_TAG_ELSEIF             44
#define CPDF_TAG_ENDIF              45

struct {
    char *Name;
    int nTag;
} tags[] = {
    {";",                   CPDF_TAG_COMMENT}, 
    {"ControlPanelName",    CPDF_TAG_CPNAME},
    {"ControlPanelType",    CPDF_TAG_CPTYPE},
    {"ControlName",         CPDF_TAG_CNAME}, 
    {"LoadImage",           CPDF_TAG_LOAD_IMG},
    {"BaseXY",              CPDF_TAG_BASEXY},
    {"XYWH",                CPDF_TAG_XYWH}, 
    {"WH",                  CPDF_TAG_WH}, 
    {"nStates",             CPDF_TAG_NSTATES}, 
    {"InitialState",        CPDF_TAG_INITIALSTATE},
    {"StateImg",            CPDF_TAG_STATEIMG}, 
    {"AutoStates",          CPDF_TAG_AUTOSTATES}, 
    {"TransparentColor",    CPDF_TAG_TRANSPARENT}, 
    {"StateRgb",            CPDF_TAG_STATERGB}, 
    {"StateRgbCircle",      CPDF_TAG_STATERGB_CIRCLE}, 
    {"StateRgbArea",        CPDF_TAG_STATERGB_AREA}, 
    {"StateRgbText",        CPDF_TAG_STATERGB_TEXT}, 
    {"XYSameAs",            CPDF_TAG_SAME}, 
    {"Height",              CPDF_TAG_HEIGHT}, 
    {"SameAsByReference",   CPDF_TAG_SAME_BYREF}, 
    {"ControlArrayName",    CPDF_TAG_CARRAY}, 
    {"ControlArrayItems",   CPDF_TAG_CARRAYITEM}, 
    {"IfOpt",               CPDF_TAG_IFOPT}, 
    {"IfnOpt",              CPDF_TAG_IFNOPT}, 
    {"else",                CPDF_TAG_ELSE}, 
    {"elseif",              CPDF_TAG_ELSEIF}, 
    {"endif",               CPDF_TAG_ENDIF}, 
    {NULL}
};

// get len of str in control panel definition file
// string = chr(33)..chr(127) except "=" and ","
int tag_len(char * s) {
    int i; 
    for (i=0;;i++) {
        if ((s[i] < 33) || (s[i] > 127)) break;
        if ((s[i] == '=') || (s[i] == ',')) break;
    }
    return i;
}
// remove leading spaces, control chars and comments
void trim_str(char * s)
{
    int i;
    char * p;
    p=s;
    while ((p != NULL) && (*p != 0)) {
        if ((*p==';') && (p==s)) {*p=0;break;}                      // convert comment at start of line in end of line
        if ((*p==';') && (p!=s) && (*(p-1)==32)) {*p=0;break;}      // convert comment in middle of line in end of line
        if ((*p==10) || (*p==13)) {*p=0;break;}                     // convert start/cr/lf in end of line
        if (*p<32) *p=32;                                           // convert non printable control ascii char to space
        p++;
    }
    while ((s != NULL) && (s[0] == 32)) {
        for(i=0;s[i]>0;i++) s[i]=s[i+1];
    }
}
// trim s for everything up to char ch
void rtrim_str(char * s, int ch)
{
    int i, j;
    if ((s == NULL) || (*s == 0)) return;
    i=0;
    while(1) {
        if (s[i] == 0) break;
        if (s[i] == ch) break;
        i++;
    }
    if (s[i] == 0) { // ch not found -> return empty string
        s[0] = 0;
        return;
    }
    // ch found -> preserve rest of string
    j=0; i++; 
    while(1) {
        s[j]=s[i];
        if (s[i] == 0) break;
        i++;j++;
    }
}

// return the tag found, remove "tag=" it from s
int cpdf_tag(char * s) {

    int n,len;
    trim_str(s);
    len=tag_len(s);
    if (len < 1) return CPDF_TAG_COMMENT;
    for (n=0;;n++) {
        if (tags[n].Name == NULL) break;
        if (cmpstr(tags[n].Name, s, len)) {
            // tag found. skip tag string and trailing spaces up to =
            rtrim_str(s, '=');
            return tags[n].nTag;
        }
    }
    return 0;
}
// copy str to Name (max nSize-1 chars), remove "str," from s
void cpdf_str_param(char * s, char * Name, int nSize)
{
    int i,l;
    if ((Name == NULL) || (s == NULL) || (nSize < 2))  return;
    Name[0] = 0;
    trim_str(s);
    l = (nSize > 32) ? nSize: tag_len(s);
    for(i=0;i<l;i++) {
        if (i == nSize-1) break;
        if (s[i] == 0) {
            s[0] = 0; // all source s string used. Mark as empty
            break;
        }
        Name[i]=s[i];
    }
    Name[i] = 0;
    // remove trailing spaces from Name
    while (i > 0) {
        if (Name[i-1] != ' ') break;
        Name[--i] = 0;
    }
    // remove up to trailing , from command string
    rtrim_str(s, ',');
}

// return -1 if minus signs, 0 if no sign, +1 if plus sign
// removes the sign
int cpdf_num_sign(char * s) 
{
    int n = 0;
    if (s == NULL) return 0;
    if (s[0] == '+') {n = 1;} else if (s[0] == '-') {n = -1;}
    if (n == 0) return 0;
    s[0] = ' ';
    trim_str(s);
    return n;
}

// return value (0 to 99999) of str, remove "str," from s. 
int cpdf_num_param(char * s)
{
    char str[8];
    int32 n,i,c,ok;

    cpdf_str_param(s, str, 6);
    n = i = ok = 0;
    while (1) {
        c = str[i++];
        if ((c < '0') || (c > '9')) break;
        n = n * 10 + c - '0';
        ok = 1;
    }
    return ok ? n : -1;
}

// initialize intentisty ticks counter. 
void InitTickCount(int nTick)
{
    int i;
    for (i=0;i<64;i++) CP.TickCount[nTick].Bit[i] = 0;
    CP.TickCount[nTick].Num = 0;
}

// return free intentisty ticks counter (and init it)
int GetTickCountItem()
{
    int i;
    for (i=0;i<CP.TickCountItems;i++) {
        if (CP.TickCount[i].Num < 0) {
            InitTickCount(i);
            return i;
        }
    }
    if (CP.TickCountItems >= MAX_TICK_COUNT) return -1;
    i = CP.TickCountItems++;
    InitTickCount(i);
    return i;
}

// return 1 if file exists (i.e. can be opened), else return 0
int file_exists(char * filename)
{
    FILE *file;

    if (filename == NULL) return 0;
    file = sim_fopen (filename, "r");
    if (file == NULL)                          // open failed? 
        return 0;
    fclose (file);
    return 1;
}



#define DF_ERR_NLIN             fprintf(stderr, "Control Panel Definition File line %d: ", nLin)
#define DF_ERR(str)             DF_ERR_NLIN; fprintf(stderr, str); fprintf(stderr, "\r\n")
#define DF_ERR2(str,param)      DF_ERR_NLIN; fprintf(stderr, str, param); fprintf(stderr, "\r\n")
#define DF_ERR3(str,p1,p2)      DF_ERR_NLIN; fprintf(stderr, str, p1, p2); fprintf(stderr, "\r\n")
#define DF_ERR_INV_NUM(str)     DF_ERR2("invalid number in %s", str)

// load & process control panel definition file (using attached filename)
void ControlPanelLoad(CP_TYPE *cp_type, UNIT *uptr, DEVICE *dptr)
{
    FILE *file;
    char *DF_filename; 
    char lbuf[255];
    char Name[255];
    char Name2[32];
    int CId, CArrayId, OrgCId, save_SurfaceItems; 
    int nLin,X,Y,W,H,n,ix,iy,wx,wy,r,g,b,i,tag,nImg,nTick,ifopt, BaseX, BaseY; 
    uint32 col, TransparentColor, TransparentValue, rr,gg,bb;
    // clear CP struct to receive new control panel data
    CP.ControlItems     = 0;       // total number of defined controls (= items in Control[]
    CP.CArrayItems      = 0;       // total number of defined control arrays (= items in CArray[]
    CP.SurfaceItems     = 0;       // total number of defined pixels in surface arrays (= items in surface[]
    CP.ImgItems         = 0;
    CP.TickCountItems   = 0;
    cpanel_TypeId   = 0;       

    if (uptr->filename) {
        DF_filename = uptr->filename;
    } else {
        // Control Panel definition file not attached, check default name
        if ((!ControlPanel_Default_filename[0]) || (!file_exists(ControlPanel_Default_filename))) {
            fprintf(stderr, "Control Panel definition file not attached\r\n");
            return;
        }
        // use default name
        DF_filename = ControlPanel_Default_filename;
    }
    file = sim_fopen (DF_filename, "r");
    if (file == NULL) {                         // open failed? 
        fprintf(stderr, "Cannot open Control Panel definition file %s\r\n", uptr->filename);
        return;
    }
    sim_debug (CP_DF, dptr, "Control Panel definition file used: %s\n", uptr->filename);

    // load internal font
    FontLoad(1);
    // set CP.Surface to its max size
    CP.surface = (uint32 *)malloc ((MAX_PIXELS)*sizeof(*CP.surface));
    ControlPanel_Name[0] = 0;     // control panel's name
    CId = CArrayId = X = Y = W = H = n = col = wx = wy = r = g = b = i = -1; 
    nLin = 0; ifopt = 0; TransparentColor = 0;
    BaseX = BaseY = 0;
    while (fgets (lbuf, sizeof(lbuf)-1, file)) {
        nLin++;
        sim_debug (CP_DF, dptr, "Lin %d: %s\n", nLin, lbuf);
        lbuf[sizeof(lbuf)-1] = 0;    // paranoic end of string
        trim_str(lbuf);
        tag=cpdf_tag(lbuf);
        if (ifopt == 2) {      // skip until else or endif 
            if ((tag != CPDF_TAG_ELSE) && (tag != CPDF_TAG_ELSEIF) && (tag != CPDF_TAG_ENDIF)) {
                tag = CPDF_TAG_COMMENT;   // skip line by setting tag as it was comment
            }
        } else if (ifopt == 3) {      // skip until endif 
            if (tag != CPDF_TAG_ENDIF) {
                tag = CPDF_TAG_COMMENT;   // skip line by setting tag as it was comment
            }
        }
process_tag:
        switch( tag ) {
            case CPDF_TAG_COMMENT:
                // ; is the comment char
                lbuf[0] = 0;     // assure line is cleared
                break;
            case CPDF_TAG_CPNAME: 
                // ControlPanelName= name shown on windows title up to 127 chars
                if (ControlPanel_Name[0]) {
                    DF_ERR("control panel name redefinition");
                    break;
                }
                cpdf_str_param(lbuf, ControlPanel_Name, 128);
                sim_debug (CP_DF, dptr, "Control Panel Name: %s\n", ControlPanel_Name);
                break;
            case CPDF_TAG_CPTYPE: 
                // ControlPanelType= name of control panel type 
                if (cpanel_TypeId) {
                    DF_ERR("control panel type redefinition");
                    break;
                }
                cpdf_str_param(lbuf, Name, 32);
                sim_debug (CP_DF, dptr, "Control Panel Type: %s\n", Name);
                for (n=0;;n++) {
                    if (cp_type[n].Name == NULL) break;
                    if (cmpstr(cp_type[n].Name, Name, 32)) {
                        cpanel_TypeId = cp_type[n].Id;
                        break;
                    }
                } 
                if (cpanel_TypeId < 1) {
                    DF_ERR2("control panel type %s not defined", Name);
                    cpanel_TypeId = -1;
                }
                break;
            case CPDF_TAG_LOAD_IMG:
                // LoadImage=Name up to 31 chars, filename up to 255 chars
                // to be used with StateImg=
                cpdf_str_param(lbuf, Name2, 32);
                cpdf_str_param(lbuf, Name, 255);
                { // get path from definition file
                    char path[PATH_MAX+1];
                    int i,j;
                    for (i=0;(i<PATH_MAX-1) && (uptr->filename[i]);i++) path[i]=uptr->filename[i];
                    for (j=i-1;(j>=0) && (path[j] != '\\') && (path[j] != '/'); j--);
                    path[++j]=0;
                    // add path to name
                    for(i=0;(j<PATH_MAX-1) && (Name[i]);i++) path[j++] = Name[i];
                    path[j]=0;
                    sim_debug (CP_DF, dptr, "Load Image Name: %s, FullFileName %s\n", Name2, path);
                    if ((Name2[0]==0) || (Name[0]==0) || (AddJPEGImage(Name2, path) < 0)) {
                        DF_ERR2("invalid LoadImage %s", path);
                        break;
                    }
                }
                break;
            case CPDF_TAG_CNAME: 
                // ControlName=Name up to 31 chars 
                cpdf_str_param(lbuf, Name, 32);
                sim_debug (CP_DF, dptr, "Control Name: %s\n", Name);
                break;
            case CPDF_TAG_BASEXY:
                // BaseXY=value to be added to pos X of control, value to be added to pos Y of control
                // only affects XYWH= and XYSame= tags
                // if values have a sign +/-, sets the new BaseXY values relative to the previous set ones
                wx = cpdf_num_sign(lbuf);
                X = cpdf_num_param(lbuf);
                wy = cpdf_num_sign(lbuf);
                Y = cpdf_num_param(lbuf);
                sim_debug (CP_DF, dptr, "BaseX=%d (sign %d), BaseY=%d (sign %d)\n", X,wx,Y,wy);
                if ( (X<0) || (Y<0) ) {
                    DF_ERR_INV_NUM( (X<0) ? "X": (Y<0) ? "Y": "?");
                    break;
                }
                if ((wx == 0) && (wy == 0)) {
                    BaseX = X; BaseY = Y;
                } else {
                    if (wx == 0) wx = 1; if (wy == 0) wy = 1; 
                    BaseX += wx * X; BaseY += wy * Y;
                }
                break;
            case CPDF_TAG_XYWH: 
                // XYWH=pos x of control, pos y of control, width, heigth
                // in pixel coordinates. (origin top left). If user click in this arean, the OnClick event callback is called
                X = cpdf_num_param(lbuf);
                Y = cpdf_num_param(lbuf);
                W = cpdf_num_param(lbuf);
                H = cpdf_num_param(lbuf);
                sim_debug (CP_DF, dptr, "X=%d, Y=%d, W=%d, H=%d\n", X,Y,W,H);
                if ( (X<0) || (Y<0) || (W<0) || (H<0) ) {
                    DF_ERR_INV_NUM( (X<0) ? "X": (Y<0) ? "Y": (W<0) ? "W": (H<0) ? "H": "?");
                    break;
                }
                X += BaseX; Y += BaseY; // add BaseXY offset 
                break;
            case CPDF_TAG_WH: 
                // WH=width, heigth of control. X is not defined and the control is not draw nor sensed. It is used
                // to define generic controls to be used in SameAs
                X = -1;
                Y = -1;
                W = cpdf_num_param(lbuf);
                H = cpdf_num_param(lbuf);
                sim_debug (CP_DF, dptr, "W=%d, H=%d\n", W,H);
                if ( (W<0) || (H<0) ) {
                    DF_ERR_INV_NUM( (W<0) ? "W": (H<0) ? "H": "?");
                    break;
                }
                break;
            case CPDF_TAG_NSTATES: 
                // nStates= number of possible states for the control
                // Each state has a different image
                // is nState = 0 -> no image. Just the click area is sensed
                // this tag (and XYSameAs) effectivelly creates the control
                n = cpdf_num_param(lbuf);
                TransparentColor = 0; // reset transparet color
                sim_debug (CP_DF, dptr, "nStates=%d\n", n);
                if (( n<0 ) || (n > 255))  {
                    DF_ERR_INV_NUM("nStates");
                    break;
                }
                if (n<0) {
                    DF_ERR_INV_NUM( (n<0) ? "nStates": "?");
                    break;
                }
                CId = AddControl(Name, n, X,Y,W,H);
                sim_debug (CP_DF, dptr, "AddControl(Name %s, nStates=%d, X=%d, Y=%d, W=%d, H=%d) returns CId %d\n", Name, n, X,Y,W,H, CId);
                if (CId < 0) {
                    DF_ERR("control definition failed");
                    break;
                }
                sim_debug (CP_DF, dptr, "   Total controls defined %d (max %d), total pixels used %d (max %d)\n", 
                    CP.ControlItems, MAX_CONTROLS, CP.SurfaceItems, MAX_PIXELS);
                break;
            case CPDF_TAG_INITIALSTATE: 
                // InitialState=initial state at panel creation
                n = cpdf_num_param(lbuf);
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control");
                    break;
                } 
                sim_debug (CP_DF, dptr, "Control %d (Name %s): Initial State %d\n", 
                    CId,CP.Control[CId].Name, n);
                if (n<0) {
                    DF_ERR_INV_NUM( (n<0) ? "n": "?");
                    break;
                }
                if (n >= CP.Control[CId].nStates) {
                    DF_ERR3("invalid state %d (nStates %d)", n, CP.Control[CId].nStates);
                    break;
                }
                // set state
                SetState(CId, n);
                break;
            case CPDF_TAG_STATEIMG: 
                // StateImg=state number n, pos x, pos y, image name
                // sets the image for control state n. Pixels are taken from image name (previously loaded with LoadImage),
                // at pos X,Y for the defined Width and Heigh of control. If no image filename is given, last image 
                // filemane is used
                n = cpdf_num_param(lbuf);
                X = cpdf_num_param(lbuf);
                Y = cpdf_num_param(lbuf);
                cpdf_str_param(lbuf, Name, 32);
                nImg = -1;
                if (Name[0] == 0) {
                    DF_ERR("Missing image name");
                    break;
                } else {
                    for (i=0;i<CP.ImgItems;i++) {
                        if (cmpstr(CP.Img[i].Name, Name, -1) == 0) continue;
                        nImg = i;
                        break;
                    }
                    if (nImg < 0) {
                        DF_ERR2("cannot found image name %s", Name);
                        break;
                    }
                }
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control Id");
                    break;
                } 
                sim_debug (CP_DF, dptr, "Control %d (Name %s): Def State %d with Image %s using data from X=%d, Y=%d\n", 
                    CId,CP.Control[CId].Name, n, Name[0] ? Name : "(current)",X,Y);
                if ( (n<0) || (X<0) || (Y<0) ) {
                    DF_ERR_INV_NUM( (n<0) ? "n": (X<0) ? "X": (Y<0) ? "Y": "?");
                    break;
                }
                if (CP.Img[nImg].surface == NULL) {
                    DF_ERR("no image");
                    break;
                }
                // copy pixels
                W = CP.Control[CId].W;
                H = CP.Control[CId].H;
                wx = CP.Img[nImg].W;
                wy = CP.Img[nImg].H;
                for (ix=X;ix<X+W;ix++) {
                    for (iy=Y;iy<Y+H;iy++) {
                        if ((ix < wx) && (iy < wy)) {
                            col = CP.Img[nImg].surface[wx * iy + ix]; 
                            if (TransparentColor > 0) {
                                get_surface_rgb_color(col, &rr, &gg, &bb); // get rgb color of image pixel being copied
                                if (TransparentColor == 1) {
                                    // back color is transparent
                                    if ((rr < TransparentValue) && (gg < TransparentValue) && (bb < TransparentValue)) col = 0;
                                } else if (TransparentColor == 2) {
                                    // white
                                    if ((rr > TransparentValue) && (gg > TransparentValue) && (bb > TransparentValue)) col = 0;
                                } else if (TransparentColor == 3) {
                                    // red transparent
                                    if ((rr <= bb) || (rr <= gg)) {
                                        // red color not dominant -> keep pixel
                                    } else if ((bb < 20) && (gg < 20) && (rr < 20)) {
                                        // quasi black color -> keep pixel
                                    } else if ((bb == 0) && (gg == 0)) {
                                        // very dark pure red -> keep pixel 
                                        if (rr > 50) col = 0;
                                    } else if (rr > gg) {
                                        if (rr - gg < TransparentValue) {
                                            // not too much red -> keep pixel
                                        } else if ((gg > 80) && ( (((100*(rr-gg))/gg) < TransparentValue) )) {
                                            // not too much red % -> keep pixel
                                        } else {
                                            col = 0;
                                        }
                                    } else if (rr > bb) {
                                        if (rr - bb < TransparentValue) {
                                            // not too much red -> keep pixel
                                        } else if ((bb > 80) && ( (((100*(rr-bb))/bb) < TransparentValue) )) {
                                            // not too much red % -> keep pixel
                                        } else {
                                            col = 0;
                                        }
                                    }
                                }
                                if (col == 0) {
                                    continue;
                                }
                            }
                            AddControlPixel(CId, n, ix-X, iy-Y, col);
                        };
                    }
                }
                break;
            case CPDF_TAG_STATERGB_AREA: 
                // StateRgbArea=state number n, red color, green, blue, pos X, pos Y, Width, Heigh
                // sets the image for control state n. Pixels are set to given r,g,b color (values 0 up to 255)
                // in an area from X,Y for given Width and Heigh. Pixels outside control W and H are ignored
            case CPDF_TAG_STATERGB_CIRCLE: 
                // StateRgbCircle=state number n, red color, green, blue
                // sets the image for control state n. Draw a filled circle with given r,g,b color (values 0 up to 255)
                // up to the defined Width and Heigh of control. If state pixel 0,0 is 0 (transparent) no antialiasing is
                // used. Else, the colour at pixel 0,0 is used to calc the antialiasing colour for circle border
            case CPDF_TAG_STATERGB: 
                // StateRgb=state number n, red color, green, blue
                // sets the image for control state n. Pixels are set to given r,g,b color (values 0 up to 255)
                // to the defined Width and Heigh of control. 
                n = cpdf_num_param(lbuf);
                r = cpdf_num_param(lbuf);
                g = cpdf_num_param(lbuf);
                b = cpdf_num_param(lbuf);
                if (tag == CPDF_TAG_STATERGB_AREA) {
                    X = cpdf_num_param(lbuf);
                    Y = cpdf_num_param(lbuf);
                    W = cpdf_num_param(lbuf);
                    H = cpdf_num_param(lbuf);
                } else {
                    W = CP.Control[CId].W;
                    H = CP.Control[CId].H;
                    X = Y = 0;
                }
                if (r==999) {
                    col = 0;
                } else {
                    col = surface_rgb_color(r,g,b);
                }
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control Id");
                    break;
                } 
                sim_debug (CP_DF, dptr, "Control %d (Name %s): Def State %d with Rgb %d,%d,%d, X=%d, Y=%d, W=%d, H=%d\n", 
                    CId,CP.Control[CId].Name, n, r,g,b,X,Y,W,H);
                if ( (n<0) || (r<0) || (g<0) || (b<0) || (X<0) || (Y<0) || (W<0) || (H<0) ) {
                    DF_ERR_INV_NUM( 
                        (n<0) ? "n": (r<0) ? "r": (g<0) ? "g": (b<0) ? "b": (X<0) ? "X": (Y<0) ? "Y": (W<0) ? "W": (H<0) ? "H": "?");
                    break;
                }
                // set pixels
                if (tag==CPDF_TAG_STATERGB_CIRCLE) {
                    AddCircle(CId, n, W, H, col);
                } else {
                    for (ix=X;ix<X+W;ix++) {
                        for (iy=Y;iy<Y+H;iy++) {
                            AddControlPixel(CId, n, ix, iy, col);
                        }
                    }
                }
                break;
            case CPDF_TAG_STATERGB_TEXT: 
                // StateRgbText=state number n, red color, green, blue, pos X, pos Y, text
                // sets the image for control state n. Text are painted in 5x7 font with given r,g,b color (values 0 up to 255)
                // starting at X,Y (upper left corner of first char). 2 Pixels separation between chars
                if (Font == NULL) break;        // internal font not ready
                n = cpdf_num_param(lbuf);
                r = cpdf_num_param(lbuf);
                g = cpdf_num_param(lbuf);
                b = cpdf_num_param(lbuf);
                X = cpdf_num_param(lbuf);
                Y = cpdf_num_param(lbuf);
                cpdf_str_param(lbuf, Name, 255);
                col = surface_rgb_color(r,g,b);
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control Id");
                    break;
                } 
                sim_debug (CP_DF, dptr, "Control %d (Name %s): Def State %d with Rgb %d,%d,%d Text at X=%d, Y=%d: %s\n", 
                    CId,CP.Control[CId].Name, n, r,g,b,X,Y,Name);
                if ( (n<0) || (r<0) || (g<0) || (b<0) || (X<0) || (Y<0) || (W<0) || (H<0) ) {
                    DF_ERR_INV_NUM( 
                        (n<0) ? "n": (r<0) ? "r": (g<0) ? "g": (b<0) ? "b": (X<0) ? "X": (Y<0) ? "Y": "?");
                    break;
                }
                // draw chars 
                r = 0;
                while((b=Name[r++]) > 0) {
                    b = upcase(b);                   // convert to uppercase
                    if (b > 123) b=b-27;             // because lower case not defined in font
                    if ((b < 32) || (b > 127)) b=31; // sets undef char if any
                    b=b-31;
                    for (iy=0;iy<7;iy++) {
                        g = Font[b*7 + iy];
                        for (ix=0;ix<5;ix++) {
                            if (g & 1) AddControlPixel(CId, n, X+ix, Y+iy, col);
                            g = (g >> 1);
                        }
                    }
                    X += 7;
                }
                break;
            case CPDF_TAG_SAME_BYREF:
                // SameAsByReference   
                // Sets the current control as ByRef. A new control created by XYSameAs= tag based
                // on this one will bot get the pixels copies, but instead will get a reference to 
                // current control pixels. This is the way to have several SameAs controls sharing
                // a copy of pixels images on the generic control instead on having each one its own copy
                // this tag must be afterward nState=
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control Id");
                    break;
                } 
                CP.Control[CId].ByRef = 1;
                sim_debug (CP_DF, dptr, "Control %d (Name %s): Set ByRef flag\n", 
                    CId,CP.Control[CId].Name);
                break;
            case CPDF_TAG_SAME: 
                // XYSameAs=pos x of control, pos y of control, control name to copy H W nStates MaxH and each state pixels data from
                // sets the image for all control states to be the same as give control (must already extists). 
                // this tag effectivelly creates the control
                X = cpdf_num_param(lbuf);
                Y = cpdf_num_param(lbuf);
                cpdf_str_param(lbuf, Name2, 32);
                TransparentColor = 0; // reset transparet color
                sim_debug (CP_DF, dptr, "Same As X=%d, Y=%d, Control Name to copy %s\n", X,Y,Name2);
                if ( (X<0) || (Y<0) ) {
                    DF_ERR_INV_NUM( 
                        (X<0) ? "X": (Y<0) ? "Y": "?");
                    break;
                }
                OrgCId = GetByName(Name2);
                if (OrgCId < 0) {
                    DF_ERR("cannot found same control");
                    break;
                } else if (OrgCId >= CARRAY_START) {
                    DF_ERR("cannot use a control array");
                    break;
                } 
                X += BaseX; Y += BaseY; // add BaseXY offset 
                // create control like SameAs one. 
                n = CP.Control[OrgCId].nStates;
                W = CP.Control[OrgCId].W;
                H = CP.Control[OrgCId].H;
                nTick = CP.Control[OrgCId].nTick;
                save_SurfaceItems = CP.SurfaceItems;
                CId = AddControl(Name, n, X,Y,W,H);
                sim_debug (CP_DF, dptr, "AddControl(Name %s, nStates=%d, X=%d, Y=%d, W=%d, H=%d) returns CId %d\n", 
                    Name, n, X,Y,W,H, CId);
                if (CId < 0) {
                    DF_ERR("control definition failed");
                    break;
                } else {
                    // copy MaxH
                    CP.Control[CId].MaxH = CP.Control[OrgCId].MaxH;
                    if (CP.Control[OrgCId].ByRef) {             // OrgCId y ByRef -> do not copy pixels from it, but instead juts let CId_iPos point to OrgCId_iPos
                        CP.SurfaceItems = save_SurfaceItems;    // restore SurfaceItems to free pixels space 
                        CP.Control[CId].iPos_surface = CP.Control[OrgCId].iPos_surface;
                    }
                    sim_debug (CP_DF, dptr, "   Total controls defined %d (max %d), total pixels used %d (max %d)\n", 
                        CP.ControlItems, MAX_CONTROLS, CP.SurfaceItems, MAX_PIXELS);
                    if (CP.Control[OrgCId].ByRef) {    
                        // ByRef -> no pixels to copy
                    } else {
                        // copy pixels
                        n = CopyControlPixels(OrgCId, CId);
                        if (n < 0) {
                            DF_ERR("copy pixels failed");
                            break;
                        }
                    }
                    if (nTick >= 0) {
                        // origin control has a tick count. Get the destination own's control tick count
                        nTick = GetTickCountItem(); 
                        if (nTick < 0) {
                            DF_ERR("get tick count failed");
                            break;
                        }
                        CP.Control[CId].nTick = nTick;
                    }
                }
                break;
            case CPDF_TAG_HEIGHT:
                // Height = n
                // set the control height to given value

                H = cpdf_num_param(lbuf);
                sim_debug (CP_DF, dptr, "Height H=%d\n", H);
                if ( (H<0) ) {
                    DF_ERR_INV_NUM( 
                        (H<0) ? "H": "?");
                    break;
                }
                if (CId < 0) {
                    DF_ERR("Invalid Control Id");
                    break;
                } 
                CP.Control[CId].MaxH = H;
                break;
            case CPDF_TAG_TRANSPARENT:
                // TransparentColor = W | B | R , n
                // this tag allows subsequent StateImg(s) to replace White/black/red pixels by transparent color
                // if W, tranparent pixel is the one that has r,g,b > n,n,n 
                // if B, tranparent pixel is the one that has r,g,b < n,n,n
                // if R, tranparent pixel is the one that has r-g >n ,r-b > n
                // resets on nState or XYSameAs 
                cpdf_str_param(lbuf, Name, 32);
                n = cpdf_num_param(lbuf);
                if ((Name[0] == 'b') || (Name[0] == 'B')) {TransparentColor = 1;} else
                if ((Name[0] == 'w') || (Name[0] == 'W')) {TransparentColor = 2;} else 
                if ((Name[0] == 'r') || (Name[0] == 'R')) {TransparentColor = 3;} else {
                    DF_ERR("Transparent color needs to be set to B (black), W (white) or R (red)");
                    break;
                }
                if (n<0) {
                    DF_ERR_INV_NUM( 
                        (X<0) ? "X": "?");
                    break;
                }
                TransparentValue = n;
                sim_debug (CP_DF, dptr, "TransparentColor %s, TransparentValue=%d\n", Name, TransparentValue);
                break;
            case CPDF_TAG_AUTOSTATES:
                // AutoStates   requieres nStates == 2
                // Creates (nStates-2) states from State 2 to nStates -1 with pixels being of color 
                // at intermedite between state 0 and state 1. Used to create intermedite intensities
                // for register lights/leds. Gets a tickcount new entry
                // create control like SameAs one. 
                // this tag must be afterward nState=
                if ((CId < 0) || (CId >= MAX_CONTROLS)) {
                    DF_ERR("invalid Control Id");
                    break;
                } 
                n = CP.Control[CId].nStates;
                if (n <= 2) {
                    DF_ERR("AutoStates requires nStates > 2");
                    break;
                }
                for (i=2;i<n;i++) InterpolateControlPixels(CId, i);
                nTick = GetTickCountItem(); 
                if (nTick < 0) {
                    DF_ERR("get tick count failed");
                    break;
                }
                CP.Control[CId].nTick = nTick;
                break;
            case CPDF_TAG_CARRAY: 
                // ControlArrayName=name of control array
                cpdf_str_param(lbuf, Name, 32);
                CArrayId = AddCArray(Name);
                sim_debug (CP_DF, dptr, "AddControlArray(Name %s) returns CArrayId %d\n", Name, CArrayId);
                if (CArrayId < 0) {
                    DF_ERR("control array definition failed");
                    break;
                }
                break;
            case CPDF_TAG_CARRAYITEM: 
                // ControlArrayItems=name of control item 0 in array, name of control 1, ...
                // controls must be previously defined
                if (CArrayId < CARRAY_START) {
                    DF_ERR("control array not set");
                    break;
                } 
                while (1) {
                    cpdf_str_param(lbuf, Name, 32);
                    if (Name[0] == 0) break;
                    CId = GetByName(Name);
                    if (CId < 0) {
                        DF_ERR2("control %s not found", Name);
                        break;
                    }
                    if (CP.Control[CId].nTick >= 0) {
                        // single control in control array has tickcount set 
                        // whole control array (if not already set) tickcount
                        if (CP.CArray[CArrayId - CARRAY_START].nTick < 0) {
                            nTick = GetTickCountItem(); 
                            if (nTick < 0) {
                                DF_ERR("get tick count failed");
                                break;
                            }
                            CP.CArray[CArrayId - CARRAY_START].nTick = nTick;
                        }
                        // remove tickcount from single control within control array
                        nTick = CP.Control[CId].nTick;
                        CP.Control[CId].nTick = -1;
                        CP.TickCount[nTick].Num = -1;
                    }
                    AddCArrayItem(CArrayId, CId);
                    sim_debug (CP_DF, dptr, "AddControl %d (Name %s) to Control Array %d (Name %s)\n", 
                        CId, CP.Control[CId].Name, CArrayId, CP.CArray[CArrayId-CARRAY_START].Name );
                }
                sim_debug (CP_DF, dptr, "   Total items in control array %d\n", 
                    CP.CArray[CArrayId-CARRAY_START].Items);
                break;
            case CPDF_TAG_IFOPT:
            case CPDF_TAG_IFNOPT:
                // if[n]opt=name
                // continue if name is defined/not defiend in options array. if not, skip lines until else tag
                // ifopt and ifnopt cannot be nested
                cpdf_str_param(lbuf, Name, 32);
                if ((Name == NULL) || (Name[0] == 0)) {
                    DF_ERR("Missing option name in if[n]opt");
                    break;
                }
                if (ifopt > 0) {
                    DF_ERR("if[n]opt cannot be nested");
                    break;
                }
                n = IsOption(Name); 
                if ((tag == CPDF_TAG_IFOPT) && (n != 0)) {
                    ifopt = 1; // if evaluates to true ... execute, then skip on next elseif or else, up to endif
                    sim_debug (CP_DF, dptr, "ifopt|elseif %s evaluates to true (option set)\n", Name);
                } else if ((tag == CPDF_TAG_IFNOPT) && (n == 0)) {
                    ifopt = 1; // if evaluates to true ... execute, then skip on next elseif or else, up to endif
                    sim_debug (CP_DF, dptr, "ifnopt %s evaluates to true (option not set)\n", Name);
                } else {
                    ifopt = 2; // if evaulates to false ... skip until else|elseif|endif
                    sim_debug (CP_DF, dptr, "if[n]opt|elseif %s evaluates to false\n", Name);
                }
                break;
            case CPDF_TAG_ELSEIF:
                if (ifopt == 0) {     
                    DF_ERR("elseif without if");
                    break;
                } else if (ifopt == 1) {     // if true ... elseif -> then skip to next endif
                    ifopt = 3;        // skip until endif
                    lbuf[0] = 0;      // ignore rest of line
                    sim_debug (CP_DF, dptr, "elseif found: skip until endif");
                } else if (ifopt == 2) {     // if false ... elseif -> evaluate
                    ifopt = 0;
                    tag = CPDF_TAG_IFOPT;
                    sim_debug (CP_DF, dptr, "elseif found evaluate opt again");
                    goto process_tag;           // ugly but very handly
                }
                break;
            case CPDF_TAG_ELSE:
                if (ifopt == 0) {     
                    DF_ERR("else without if");
                    break;
                } else if (ifopt == 1) {     // if true ... else -> then skip to endif
                    ifopt = 3;        // skip until endif
                    lbuf[0] = 0;      // ignore rest of line
                    sim_debug (CP_DF, dptr, "else found: skip until endif");
                } else if (ifopt == 2) {     // if false ... else -> execute until elseif|else|endif
                    ifopt = 1; 
                }
                break;
            case CPDF_TAG_ENDIF:
                if (ifopt == 0) {
                    DF_ERR("endif without if");
                    break;
                }
                ifopt = 0;
                break;
            default: 
                DF_ERR2("unknown tag: %s", lbuf);
                lbuf[0] = 0; // clear line
                break;
        }
        if (lbuf[0]) {
            DF_ERR2("unexpected data: %s", lbuf);
        }
    }
    fclose (file);
    // adjust control panel type
    if (cpanel_TypeId < 0) {
        // given panel type not defined -> set to 0 to prevent GUI open
        cpanel_TypeId = 0;
    } else if (cpanel_TypeId == 0) {
        // no panel type is set in definition file, use 1 as default
        cpanel_TypeId = 1;
    }
    // unload internal font
    FontLoad(0);
    // resize CP.Surface to its actual size
    CP.surface = (uint32 *)realloc (CP.surface, (CP.SurfaceItems+1)*sizeof(*CP.surface));
    // free Images 
    for (i=0;i<CP.ImgItems;i++) 
        if (CP.Img[i].surface) {
            free(CP.Img[i].surface);
            CP.Img[i].surface = NULL; 
        }
}

// bind loaded control panel (or internal one) to SimH variables and event handling routines
void ControlPanel_Bind(CP_TYPE *cp_type, UNIT *uptr, DEVICE *dptr) 
{
    int i, n;
    int *Id;
    CP_DEF *cp_def;
    CP_CALLBACK CP_Init_CallBack = NULL;                        // Control Panel init Callback

    // set the unit and dev pointers
    cp_unit_ptr = uptr;                                         // pointer to control panel unit and device
    cp_dev_ptr = dptr;
    // load the resources from attached file and creates the controls & carrays
    ControlPanelLoad(cp_type, uptr, dptr);
    if (cpanel_TypeId < 1) return; 
    // locate the cp_def to use based on control panel type set in definition file
    cp_def = NULL;
    for (n=0;;n++) {
        if (cp_type[n].Name == NULL) break;
        if (cpanel_TypeId == cp_type[n].Id) {                   // control panel type found
            cp_def = cp_type[n].cp_def;                         // get the control mapping for this type
            CP.CP_Refresh = cp_type[n].Refresh_CallBack;        // set the refresh callback
            CP_Init_CallBack = cp_type[n].Init_CallBack;        // set the init callback
            CP.CP_Reset = cp_type[n].Reset_CallBack;            // set the reset callback
            CP.CP_TickIntensityCount = cp_type[n].TickIntensity_CallBack;
            break;
        }
    }
    if (cp_def == NULL) {
        cpanel_TypeId = 0;
        return; 
    }
    // bind the variables and callbacks given in cp_def with created controls
    i = -1;
    while(Id=cp_def[++i].IdVar) {
        n = cp_def[i].UsedByType;   // this control is used by this cpanel type
        if (n > 0) {                // = 0 -> used by any type. >0 -> must match cpanel_TypeId
            if ((cpanel_TypeId & n) == 0) {
                *Id = -1; // control not bind
                continue;
            }
        }
        *Id = GetByName(cp_def[i].Name);
        if (*Id < 0) {
            fprintf(stderr, "control %s not found. ControlPanel_Bind failed\r\n", cp_def[i].Name);
        } else if (*Id < CARRAY_START) {    
            // single control OnClick event
            CP.Control[*Id].OnClick = cp_def[i].CallBack;   
        } else {   
            // control array OnClick event
            CP.CArray[*Id - CARRAY_START].OnClick = cp_def[i].CallBack;    
        }
    };
    // call the init routine for the control panel loaded
    cpanel_Press_and_Release = 0; // default no differentiation on key press and key release con control panel
	// set cpanel_on to the cpanel type loaded before calling init (so Init can perform initialization
	// depending on loaded panel type
    cpanel_on = cpanel_TypeId; 
    if (CP_Init_CallBack) CP_Init_CallBack();
	// but then set again to zero. Will be set again is GUI window is created without errors
	cpanel_on = 0;
}

void ControlPanel_Refresh(void);
void ControlPanel_init(DEVICE *dptr)
{
    int wx, wy; 

    if (CP.ControlItems < 1) {
        fprintf(stderr, "No controls loaded. Cannot init Control Panel GUI window\r\n");
        return;
    }
    if (vid_active) {
        fprintf(stderr, "Cannot display Control Panel. GUI Window already in use\n");
        return;
    }
    // first control is backgound and sets the control panel size
    wx = CP.Control[0].W;
    wy = CP.Control[0].H;
    if (!ws_init(ControlPanel_Name, wx, wy, 1 /* ncolors */, dptr )) {
        fprintf(stderr, "Control Panel GUI window initialization failed\r\n");
        return;
    }
    cpanel_on = cpanel_TypeId;  
    cpanel_interactive = 0;
    cpanel_interval    = 10;  // activate cpanel refresh on Main instruction fetch/decode loop (t_stat sim_instr (void))
    CP.FullRedrawNeeded=1;
    ControlPanel_Refresh();
}

void ControlPanel_done(void)
{
    sim_os_ms_sleep (200); // this sleep is to asure SDL thread has processed any pending EVENT_REDRAW
    ws_shutdown();
    cpanel_on = 0; 
    cpanel_interval = 0;  // to not activate cpanel refresh on Main instruction fetch/decode loop (t_stat sim_instr (void))
    cpanel_interactive = 0;
    if (CP.surface) free(CP.surface);
    CP.surface = NULL; 
}
// load and pop-up GUI window
// power = 0 or 1 to create/destry the GUI window
void ControlPanel_Load(int power, 
                       CP_TYPE *cp_type,  
                       UNIT *uptr, DEVICE *dptr)
{
    if (power) { 
        // power on control panel. 
        if (cpanel_on) {
            ControlPanel_Refresh();
            return;  // cpanel already there. nothing to do. just refresh
        }
        ControlPanel_Bind(cp_type, uptr, dptr); 
        // if control panel type still undefined, do not load the control panel
        if (cpanel_TypeId == 0) 
            return; 
        ControlPanel_init(dptr);
        // ControlPanel_init sets cpanel_on variable to cpanel_TypeId on success (0 if fail)
    } else {
        // ControlPanel_done sets cpanel_on variable to 0
        if (cpanel_on) {
            ControlPanel_done();
        }
    }
}

// set/get state from single control/control array
// if Id < 0 return state. Else sets state
// for control arrays, sets up to 64 items as Set(item n, bit n from State)
// this allows to move at once lights or switches in a control array
// for control arrays, returns bit n at returnes State set if bit0 for item n set
t_uint64 SetGetState(int Id, t_uint64 State, char SetGetFlag);
t_uint64 GetState(int Id)                 { return (Id < 0) ? 0 : SetGetState(Id, 0,     'G');}
t_uint64 SetState(int Id, t_uint64 State) { return (Id < 0) ? 0 : SetGetState(Id, State, 'S');}
t_uint64 SetGetState(int Id, t_uint64 State, char SetGetFlag)
{
    int bGetFlag = (SetGetFlag == 'G');
    if (Id < 0) return 0;
	if (Id < CARRAY_START) {    
        // single control 
        if (CHK_CId(Id) < 0) return -1;                                // sanity check
        if (bGetFlag) {
            // get and return state
            return CP.Control[Id].State;
        } else {
            // set state
            if (State >= CP.Control[Id].nStates) return -1;
            if (CP.Control[Id].State != (int) State) {
                CP.Control[Id].State = (int) State;
                cpanel_State_has_changed = 1;                   // the state set was not the same the control had
            } else {
                cpanel_State_has_changed = 0;
            }
            if (cpanel_ControlRedrawNeeded)  {     
                // asure that next SetState call will update the control on GUI screen even if state set is same that the control already had
                CP.Control[Id].LastState = -1;
                cpanel_ControlRedrawNeeded = 0;
            }
            // clear auto next state
            CP.Control[Id].AutoNext.State = -1; 
            return 0;
        }
    } else {   
        // set control array 
        int i, CId, CId0;
        CHK_CArrayId(Id)
        if (bGetFlag) {
            State = 0;
            for (i=CP.CArray[Id - CARRAY_START].Items-1;i>=0;i--) {
                CId0 = CP.CArray[Id - CARRAY_START].CId0;
                CId = CP.CArrayCidItems[CId0 + i];
                State = (State << 1) | (CP.Control[CId].State & 1);
            }
            return State;
        } else {
            cpanel_State_has_changed = 0;
            for (i=0;i<CP.CArray[Id - CARRAY_START].Items;i++) {
                CId0 = CP.CArray[Id - CARRAY_START].CId0;
                CId = CP.CArrayCidItems[CId0 + i];
                if (CP.Control[CId].State != (int) (State & 1)) cpanel_State_has_changed = 1;
                CP.Control[CId].State = (int) State & 1;
                if (cpanel_ControlRedrawNeeded) CP.Control[CId].LastState = -1;
                State = State >> 1;
            }
            cpanel_ControlRedrawNeeded = 0;
            return 0;
        }
    }
}
// return the control id for n-th item in control array 
int GetCArrayCId(int CArrayId, int n) 
{
    int CId0, CId;
    CHK_CArrayId(CArrayId) 
    CHK_Item(CArrayId,n)
    CId0 = CP.CArray[CArrayId - CARRAY_START].CId0;
    CId = CP.CArrayCidItems[CId0 + n];
    return CId;
}

// schedulle state change for Id (array or single control) 
// state will be set automatically by refresh routine to NextState after Duration_msec has passed
int SetStateAutoNext(int Id, int NextState, uint32 Duration_msec)
{
    if (Id < 0) {
        fprintf(stderr, "Cannot Get AutoNext %d\r\n", Id);
        return -1;
    } else if (Id >= CARRAY_START) {    
        fprintf(stderr, "Cannot Set AutoNext for arrays %d\r\n", Id);
        return -1;
    }
    // single control 
    if (CHK_CId(Id) < 0) return -1;                                // sanity check
    if (NextState >= CP.Control[Id].nStates) return -1;
    // set next state
    CP.Control[Id].AutoNext.State = NextState;
    CP.Control[Id].AutoNext.Duration_msec = Duration_msec;
    CP.Control[Id].AutoNext.Time0_msec = sim_os_msec();
    return 0;
}

extern SDL_Window *vid_window;                                 /* window handle */
extern SDL_Renderer *vid_renderer;

// If Action = 0 -> calls the OnClick event of given single control/control array name
// If Action = 1 -> Signal with a mark on GUI the clickable area of given single control/control array name
// If Action = 2 -> printf info on given single control/control array name
// If Action = 3 -> set control, state
int DoActionByName(char * Name, int Action)
{
    int Id, i, CId0, CId, n, Mark; 

    if (Action == 0) {
        // calls the OnClick event of given single control
        Id = GetByName(Name);
        if (Id < 0) return -1; // name not found
        if (Id >= CARRAY_START) return -1; // cannot press on control arrays, only on single controls
        DoClickEvent(Id);
        return 0;
    }
    if (Action == 1) {
        // mark on GUI the clickable area of given single control/control array name
        if (cmpstr(Name, "<NONE>",0)) {
            // unmark all
            for (i=0; i<CP.ControlItems;i++) CP.Control[i].Mark = 0;
        } else if (cmpstr(Name, "<ALL>",0)) {
            // mark all single controls/control arrays with OnClick handler
            for (i=0; i<CP.ControlItems;i++) {
                if (i == 0) continue;                           // control zero is background. Skip
                if (CP.Control[i].OnClick == NULL) continue;    // no OnClick handler: cannot be clicked, so is no Marked
                CP.Control[i].Mark = 1;
            }
            // mark controls for all control arrays with OnClick handler
            for (i=0; i<CP.CArrayItems;i++) {
                if (CP.CArray[i].OnClick == NULL) continue;     // no OnClick handler: cannot be clicked, so is no Marked
                DoActionByName(CP.CArray[i].Name, 1);           // now mark all controls for this CArray
            }
        } else if ((Id = GetByName(Name)) < 0) {
            // name not found
            return -1;   
        } else if (Id < CARRAY_START) {
            // Name is a single control, Mark a single control
            CP.Control[Id].Mark = 1;
        } else {
            // Name is a control array, Mark each control in array
            CId0 = CP.CArray[Id - CARRAY_START].CId0;
            for (i=0;i<CP.CArray[Id - CARRAY_START].Items;i++) {
                CId = CP.CArrayCidItems[CId0 + i];
                CP.Control[CId].Mark = 1;
            }
        }
        CP.FullRedrawNeeded=1; // redraw the whole screen with updated marks
        if (cpanel_on) {
            ControlPanel_Refresh();
        }
        return 0;
    }
    if (Action == 2) {
        // printf info on given single control/control array name
        if (cmpstr(Name, "<ALL>",0)) {
            // print all 
            for (i=0; i<CP.ControlItems;i++) DoActionByName(CP.Control[i].Name, 2);
            for (i=0; i<CP.CArrayItems;i++)  DoActionByName(CP.CArray[i].Name, 2);
            // print used free
            sim_printf("Total Defined:\r\n");
            sim_printf("   Single Controls     : %d (Max %d)\r\n", CP.ControlItems, MAX_CONTROLS);
            sim_printf("   Control Arrays      : %d (Max %d)\r\n", CP.CArrayItems, MAX_CARRAYS);
            sim_printf("   Loaded Images       : %d (Max %d)\r\n", CP.ImgItems, MAX_IMAGES);
            sim_printf("   Control State Pixels: %d (Max %d)\r\n", CP.SurfaceItems, MAX_PIXELS);
            n = 0; for (i=0;i<CP.TickCountItems;i++) if (CP.TickCount[i].Num >= 0) n++;
            sim_printf("   Tick Count used     : %d (Max %d)\r\n", n, MAX_TICK_COUNT);
            sim_printf("Options Defined:\r\n");
            sim_printf("   %s\r\n", CP.Options);
        } else if ((Id = GetByName(Name)) < 0) {
            // name not found
            return -1;   
        } else if (Id < CARRAY_START) {
            // Name is a single control, printf definition
            sim_printf("ControlName=%s (Id=%d)\r\n", CP.Control[Id].Name, Id);
            if (CP.Control[Id].X < 0) {
                sim_printf("   WH=%d,%d\r\n", CP.Control[Id].W, CP.Control[Id].H);
            } else {
                sim_printf("   XYWH=%d,%d,%d,%d\r\n", CP.Control[Id].X, CP.Control[Id].Y, CP.Control[Id].W, CP.Control[Id].H);
            }
            if (CP.Control[Id].MaxH > 0) {
                sim_printf("   Heigth=%d\r\n", CP.Control[Id].MaxH);
            }
            sim_printf("   nStates=%d\r\n", CP.Control[Id].nStates);
            sim_printf("   ;   Current State: %d\r\n", CP.Control[Id].State);
            if (CP.Control[Id].OnClick) {
                sim_printf("   ;   OnClick Event defined\r\n");
            }
        } else {
            // Name is a control array, printf its control's names
            sim_printf("ControlArrayName=%s (Id=%d)\r\n", CP.CArray[Id - CARRAY_START].Name, Id);
            sim_printf("   ;   Number of Items: %d\r\n", CP.CArray[Id - CARRAY_START].Items);
            if (CP.CArray[Id - CARRAY_START].OnClick) {
                sim_printf("   ;   OnClick Event defined\r\n");
            }
            CId0 = CP.CArray[Id - CARRAY_START].CId0;
            for (i=0;i<CP.CArray[Id - CARRAY_START].Items;i++) {
                CId = CP.CArrayCidItems[CId0 + i];
                if ((i & 3) == 0) {
                    if (i>0) sim_printf("\r\n");
                    sim_printf("   ControlArrayItems=");
                } else {
                    sim_printf(", ");
                }
                sim_printf("%s", CP.Control[CId].Name);
            }
            sim_printf("\r\n");
        }
        return 0;
    }
    if (Action == 3) {
        // If Action = 3 -> set control name, state
        for(i=0;((Name[i] != '/') && (Name[i] != ' ') && (Name[i] != 0));i++);
        if (Name[i] == 0) return 0; // no state given
        Name[i] = 0;
        n = 0;
        for(i=i+1;Name[i];i++) {
            if ((Name[i] >= '0') && (Name[i] <= '9')) n = n * 10 + Name[i] - '0';
            if (Name[i] == '*') {n = -1; break;}
        }
        Mark = (n < 0) ? 0 : n | 256; // to force the state drawing via .Mark 
        if (cmpstr(Name, "<ALL>",0)) {
            for (Id=0; Id<CP.ControlItems;Id++) if (CP.Control[Id].nStates > n) CP.Control[Id].Mark = Mark;
        } else if ((Id = GetByName(Name)) < 0) {
            // name not found
            return -1;   
        } else if (Id < CARRAY_START) {
            // Name is a single control, Mark a single control
            if (CP.Control[Id].nStates > n) CP.Control[Id].Mark = Mark;
        } else {
            // Name is a control array, Mark each control in array
            CId0 = CP.CArray[Id - CARRAY_START].CId0;
            for (i=0;i<CP.CArray[Id - CARRAY_START].Items;i++) {
                CId = CP.CArrayCidItems[CId0 + i];
                if (CP.Control[CId].nStates > n) CP.Control[CId].Mark = Mark;
            }
        }
        CP.FullRedrawNeeded=1; // redraw the whole screen with updated atate(s) 
        if (cpanel_on) {
            ControlPanel_Refresh();
        }
        return 0;
    }
    return -1;  // unknow action
}


// calls the OnClick event of control/array on top at coords PosX,PosY
// if KeyPress_KeyRelease = 0 -> process press and release on same onclick call, =1 -> key press, =2 -> key release
void ProcessClickEventAtXY(int PosX, int PosY, int KeyPress_KeyRelease) 
{
    int i,j,X,Y,W,H, CId0, CId;
    // init clicked item data
    CP_Click.KeyPress_KeyRelease = KeyPress_KeyRelease; 
    CP_Click.X = PosX;          // click coords relative to GUI Panel Window
    CP_Click.Y = PosY;
    CP_Click.CId = -1;          // no control clicked
    CP_Click.CArrayId = -1;     // no array clicked
    CP_Click.CArrayItem = -1;   // no array item clicked
    CP_Click.OfsX = -1;         // click coords relative to control origin
    CP_Click.OfsY = -1;
    // determine the control/array clicked top (this is the last one)
    for(i=0;i<CP.ControlItems;i++) {
        X = CP.Control[i].X;    // get control boundaries
        Y = CP.Control[i].Y;
        W = CP.Control[i].W;
        H = CP.Control[i].H;
        if (X == -1) continue;   // if X == -1 control is not sensed
        if ((PosX < X) || (PosY < Y)) continue; // because click outside control
        X = PosX - X; 
        Y = PosY - Y; 
        if ((W < X) || (H < Y)) continue;  // because click outside control
        // control clicked. Set in OfsX/Y the click pos relative to controls origin
        CP_Click.CId = i;
        CP_Click.OfsX = X;
        CP_Click.OfsY = Y;
    }
    // determine if control clicked belongs to an array 
    CP_Click.CArrayId = -1;  // no array clicked
    CP_Click.CArrayItem = -1; // no array item clicked
    for(i=0;i<CP.CArrayItems;i++) {
        for (j=0;j<CP.CArray[i].Items;j++) {
            CId0 = CP.CArray[i].CId0;
            CId = CP.CArrayCidItems[CId0 + j];
            if (CP_Click.CId == CId) {
                CP_Click.CArrayItem = j;
                CP_Click.CArrayId = i + CARRAY_START;
                break;
            }
        }
        if (CP_Click.CArrayItem >= 0) break;
    }
   // if array clicked then call array event handler (if any)
   // else call the control event handler (if any)
   if ((CP_Click.CArrayId >= 0) && (CP.CArray[CP_Click.CArrayId-CARRAY_START].OnClick)) {
      CP.CArray[CP_Click.CArrayId-CARRAY_START].OnClick();
   } else if ((CP_Click.CId >= 0) && (CP.Control[CP_Click.CId].OnClick)) {
      CP.Control[CP_Click.CId].OnClick();
   }
}

// calls the OnClick event of given control
int DoClickEvent(int CId)
{
    int X,Y;

    X = CP.Control[CId].X;
    Y = CP.Control[CId].Y;
    ProcessClickEventAtXY(X, Y, 0);
    return 0;
}
int ButtonPressed = 0;
int Click_PosX, Click_PosY;

void ControlPanel_Refresh(void)
{
    int xp,yp,ix,iy,n,p,X,Y,W,H,i,State,c,Mark,mc1,mc2,nStates, nStart, MaxH;
    uint32 *surface = get_surface(&xp,&yp);
    uint32 t0, t1, RefreshInterval;

    // calculate RefreshInterval = wallclock time in msec pased from start on last refresh
    t0 = CP.LastRefreshInit;
    CP.LastRefreshInit = sim_os_msec();
    RefreshInterval = CP.LastRefreshInit - t0;
    if ((RefreshInterval > 1000000) || (RefreshInterval < 1)) RefreshInterval = 1000000;

    // call control panel refresh routine callback
    if (CP.CP_Refresh != NULL) CP.CP_Refresh();
    // update states autonext: states thats automatically changes after a given msec duration
    t1 = CP.LastRefreshInit;
    for(i=0;i<CP.ControlItems;i++) {
        if (CP.Control[i].nStates == 0) continue;  // if nStates==0 -> control not drawn. Just it is used for click sense on its area
        if (CP.Control[i].AutoNext.State == -1) continue; // no AutoNext in this control
        t0 = CP.Control[i].AutoNext.Time0_msec;
        if ((t0 > t1) || ((t1 - t0) > CP.Control[i].AutoNext.Duration_msec)) {
            // duration expired, change state to the one defined
            CP.Control[i].State = CP.Control[i].AutoNext.State;
            CP.Control[i].AutoNext.State = -1;   // disable AutoNext in this control
        }
    }
    mc1 = surface_rgb_color(255,  0,0); // mark color red
    mc2 = surface_rgb_color(  0,255,0); // mark color green
    // itearate on controls and draw the ones that changed / are marked
    for(i=0;i<CP.ControlItems;i++) {
        State = CP.Control[i].State;
        Mark = CP.Control[i].Mark;  
        nStates = CP.Control[i].nStates; 
        if ((nStates == 0) && (Mark != 1)) continue; // if nStates==0 -> control not drawn. Just it is used for click sense on its area
        X = CP.Control[i].X;                         // get control boundaries
        Y = CP.Control[i].Y;
        W = CP.Control[i].W;
        H = CP.Control[i].H;
        MaxH = CP.Control[i].MaxH;
        if (X == -1) continue;                       // if X == -1 -> control not drawn nor sensed
        if ((X+W <  0) || (Y+H <  0)) continue;        // sanity check
        if ((X   > xp) || (Y   > yp)) continue;        // sanity check
        if (Mark >= 256) State = Mark & 255;         // Mark > 255 -> holds the state to force redraw control with this state
        if ((CP.Control[i].LastState == State) && (CP.FullRedrawNeeded == 0) && (Mark == 0)) continue; // skip control, no need to redraw
        // control needs to be redrawn. Calc control pixels to draw for current state
        CP.Control[i].LastState = State;             // save the last state drawn on GUI
        nStart = CP.Control[i].iPos_surface          // start of control's pixels
                 + State * W * H;                    // start of this state 
        // set max height to be draw
        if ((MaxH == 0) || (MaxH > H)) MaxH = H;
        // copy control pixels to GUI surface
        for (iy=Y;iy<Y+MaxH;iy++) {
            if (iy < 0) continue;
            if (iy >= yp) break;
            p = iy*xp + X;
            n = nStart + (iy-Y) * W;
            for (ix=0;ix<W;ix++) {
                if (ix + X >= xp) break;
                if ((p >= xp * yp) || (n >= MAX_PIXELS)) break;
                if ((Mark == 1) && (((ix & 1)==0) && ((iy & 1)==0))) { // draw overlay mark on control ?
                    c = mc1;
                } else if ((Mark == 1) && (((ix & 1)==1) && ((iy & 1)==1))) { 
                    c = mc2;   
                } else if (nStates > 0) {
                    c = CP.surface[n];         // no, use its pixel color if has one
                } else {
                    c = 0;                     // control has no states -> set transparent
                }
                if ((ix + X >= 0) && (c))
                   surface[p] = c;         // c==0 -> transparent pixel, else draw pixel with color c on GUI window
                p++; n++;
            }
        }

    }

    //update GUI window
    if (cpanel_on) {
        ws_poll (NULL, 0);
        ws_sync ();
    }
    CP.FullRedrawNeeded=0;
    // check if button click (on mouse button release)
    if ((ws_lp_x >= 0) || (ws_lp_y >= 0)) {
        if (ButtonPressed == 0) {
            Click_PosX = ws_lp_x;
            Click_PosY = yp - 1 - ws_lp_y; 
            if ((CP.Scale != 100) && (CP.Scale >= 10)) { // if GUI scaled, adjust click position to match the unscaled control position
                Click_PosX = Click_PosX * 100 / CP.Scale;
                Click_PosY = Click_PosY * 100 / CP.Scale;
            }
            if (cpanel_Press_and_Release) {
                ProcessClickEventAtXY(Click_PosX, Click_PosY, 1); // event for keypress
            } else {
                ProcessClickEventAtXY(Click_PosX, Click_PosY, 0); // event for keypress & release
            }
            ButtonPressed = 1;
        }
    } else {
        if (ButtonPressed) {
            ButtonPressed = 0;
            if (cpanel_Press_and_Release) {
                ProcessClickEventAtXY(Click_PosX, Click_PosY, 2); // event for keyrelease
            }
        }
    }

    // finalize
    CP.LastRefreshDone = sim_os_msec();
    if (CP_ShowFPS) { // do not use sim_printf to avoid filling the log with this trace
        printf("refresh duration: %d msec, starting refresh each %d msec (FPS %d) \n",
            CP.LastRefreshDone - CP.LastRefreshInit, RefreshInterval, 1000 / RefreshInterval);
    }

}

int Refresh_needed(void)
{
    uint32 tnow; 

    // check if GUI needed refresh. Checks if wallclock msec for target FPS are passed from last
    // refresh termination (not refresh start). This assures that refresh() will not eat all the CPU time
    // if refresh() takes 0 msec: Achieved FPS = FPS
    // if refresh() takes n msec, Achieved FPS = 1000/(1000/FPS + n)
    // For target 30 FPS (33 msec per frame) and a long 50 msec refresh, we go to
    // Achieved FPS = 1000/(1000/30+50) = 12 frames per second. 
    tnow=sim_os_msec();
    if ((tnow < CP.LastRefreshDone) || ((tnow - CP.LastRefreshDone) > (1000 / FPS) )) {
        return 1; // needs refresh
    } else {
        return 0;
    }
}

void ControlPanel_Refresh_CPU_Halted(void)
{
    if (Refresh_needed()) {
        sim_debug (CP_REFRESH, cp_dev_ptr, "GUI Refresh (CPU halted)\n");
        ControlPanel_Refresh();   // do the GUI window refresh
    }
}

t_stat ControlPanel_Refresh_CPU_Running(void)
{
    if ((CP.TickCount) && (CP.CP_TickIntensityCount)) CP.CP_TickIntensityCount(); // call the tick intensity callback

    if (Refresh_needed()) {
        sim_debug (CP_REFRESH, cp_dev_ptr, "GUI Refresh (CPU running)\n");
        ControlPanel_Refresh();   // do the GUI window refresh
    }

    cpanel_interval = 101;  // it is desirable to be a prime number so the sampling rate for refresh check
                            // will not couple with any thigh loop 
    // check if GUI window close has been requested by user
    if (cp_stop_flag > 0) {
        if (cp_stop_flag == 1) cpanel_interactive = 0;
        cp_stop_flag = 0;        
        sim_debug (CP_REFRESH, cp_dev_ptr, "GUI Refresh on cp_stop_flag\n");
        return SCPE_STOP;                               // signal inst main loop that user asked to close the cpanel window 
    }
    if (ControlPanel_DoSCP_pending()) {
        // if SCP command issued while cpu running, stop cpu to allow scp command to execute from scp
        return SCPE_STOP;
    }
    return SCPE_OK;
}

void ControlPanel_GetSCP(void)
{
    t_stat reason; 

    vid_mouse_read_on_sim_running = TRUE;
    while (1) {
        reason = ControlPanel_Refresh_CPU_Running();
        if (reason != SCPE_OK) {
            break;
        }
        sim_os_ms_sleep (25);     // this sleep is to let the user press a button
        sim_poll_kbd();           // poll keyboard to see if ^E (WRU) pressed during set cpanel interactive scp command execution
        if (stop_cpu) {           // yes, it is
            stop_cpu = 0;         // clean up
            cp_stop_flag = 1;     // simulate click on close gui window to stop cpu execution
        }
    }
    vid_mouse_read_on_sim_running = FALSE;
}


// add scp command given as param to list in SCP_cmd array
void DoSCP(char *cmd)
{
    int i; 

    if ((cmd == NULL) || (cmd[0] == 0)) return;
    if (SCP_cmd_count >= MAX_SCP_CMDS) {
        // if list full, discard list
        SCP_cmd_count = 0;
    }
    // copy scp command to list at next free pos
    for (i=0;(cmd[i]>0) && (i < MAX_SCP_CMD_LEN-1);i++) SCP_cmd[SCP_cmd_count * MAX_SCP_CMD_LEN + i] = cmd[i];
    SCP_cmd[SCP_cmd_count * MAX_SCP_CMD_LEN + MAX_SCP_CMD_LEN-1] = 0; // paranoic preacution
    SCP_cmd_count++;
}

// return number of SCP commands issued by control panel pending to be executed
int ControlPanel_DoSCP_pending(void)
{
    return SCP_cmd_count;
}

// set cmd with next pending scp command, and remove if from list
// if called with cmd=NULL, just remove last cmd form list
void ControlPanel_GetSCP_pending(char *cmd, int nSize)
{
    int i,c;
    int bCmdOk = 1;

    if ((nSize < 1) || (cmd == NULL)) bCmdOk = 0;
    if (bCmdOk) cmd[0] = 0;          // clear retul scp cmd 
    if (SCP_cmd_count == 0) return;  // no scp command pending
    i = 0;
    // copy first command to result cmd
    while(bCmdOk) {
        c = SCP_cmd[i];
        cmd[i++] = c;
        if (c == 0) break;
        if (i >= nSize-1) {cmd[i]=0; break;}
    }
    // remove first command from lost
    for (i=MAX_SCP_CMD_LEN; i< MAX_SCP_CMD_LEN*MAX_SCP_CMDS; i++) SCP_cmd[i-MAX_SCP_CMD_LEN] = SCP_cmd[i];
    SCP_cmd_count--;
}

// analyze bits in param n. Increment the tick count for each bit to 1 found on n
int TickCount(int Id, t_uint64 n) 
{
    int i, nTick;
    if (Id < CARRAY_START) {
        // single control. Just count tick based on bit 0 
        if (CHK_CId(Id) < 0) return -1;                                // sanity check
        nTick = CP.Control[Id].nTick;
        if (n & 1) CP.TickCount[nTick].Bit[0]++;
        CP.TickCount[nTick].Num++;
    } else {
        // control array -> set tick count on each single control
        CHK_CArrayId(Id)
        nTick = CP.CArray[Id - CARRAY_START].nTick;
        i = 0;
        while (n) {
            if (n & 1) CP.TickCount[nTick].Bit[i]++;
            n = n >> 1;
            i++;
        }
        CP.TickCount[nTick].Num++;
    }
    return 0;
}

// return state that match the intensity of given bit number, based on ticks count
int GetStateForIntensity(int nTick, int nbit, int nStates) 
{
    int n1, n2;

    if ((nbit < 0) || (nbit > 63)) return 0;
    n1 = CP.TickCount[nTick].Bit[nbit];
    if (n1 < 1) return 0;  // 0 ticks counted -> 0 intensity
    n2 = CP.TickCount[nTick].Num;
    if (n2 < 1) return 0;  // tick count = 0 -> no data -> no intensity
    n1 = (nStates * n1) / n2;
    if (n1 < 1) return 0;  
    if (n1 >= nStates-1) return 1; // state = 1 = full intensity
    return n1+1;           // state = 2 .. nStates-2 = intermediate intensities
}

int SetStateWithIntensity(int Id, t_uint64 n) 
{
    int nStates = 0;
    int CId, CId0, i, sta, nTick;
    if (CP.EnableTickCount == 0) {
        SetState(Id, n);
        return 0;
    }
    if (sim_is_running == 0) {
        // if cpu not running or no intensity tick data just do a normal setstate and reset tick counter
        SetState(Id, n);
        if (Id < CARRAY_START) {
            if (CHK_CId(Id) < 0) return -1;                                // sanity check
            nTick = CP.Control[Id].nTick;
        } else {
            CHK_CArrayId(Id)
            nTick = CP.CArray[Id - CARRAY_START].nTick;
        }
        if (nTick >= 0) InitTickCount(nTick);
        return 0;
    }
    // cpu is running and tick data available: go on for setting the intensity in function
    // of number of times (tick count) a bit is set respect the total number of ticks counted
    if (Id < CARRAY_START) {
        // single control. Just calc intensity based on bit 0 and set it as control state
        if (CHK_CId(Id) < 0) return -1;                                // sanity check
        nTick = CP.Control[Id].nTick;
        if ((nTick < 0) || (CP.TickCount[nTick].Num < 2)) {
            SetState(Id, n);
        } else {
            nStates = CP.Control[Id].nStates;
            sta = GetStateForIntensity(nTick, 0, nStates);
            CP.Control[Id].State = sta;
        }
    } else {
        // control array -> set intensity on each single control
        CHK_CArrayId(Id)
        nTick = CP.CArray[Id - CARRAY_START].nTick;
        if ((nTick < 0) || (CP.TickCount[nTick].Num < 2)) {
            SetState(Id, n);
        } else {
            for (i=0;i<CP.CArray[Id - CARRAY_START].Items;i++) {
                CId0 = CP.CArray[Id - CARRAY_START].CId0;
                CId = CP.CArrayCidItems[CId0 + i];
                nStates = CP.Control[CId].nStates;
                sta = GetStateForIntensity(nTick, i, nStates);
                CP.Control[CId].State = sta;
            }
        }
    }
    if (nTick >= 0) InitTickCount(nTick);
    return 0;
}


t_stat cp_set_cpanel_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc) 
{
    int power = value ? 1:0;
    ControlPanel_Load(
        power, 
        cp_types, 
        &cp_unit, &cp_dev); 

    return SCPE_OK;
}

t_stat cp_set_param(UNIT *uptr, int32 value, CONST char *cptr, void *desc) { 

    DEVICE *dptr = (DEVICE *) desc;
    int32 num, xPos,yPos, xSize, ySize;
    CONST char *tptr;
    t_stat r;

    if (cptr == NULL) return SCPE_ARG;
    if (desc == NULL) return SCPE_IERR;

    if (value == 0) { // SET SHOWFPS=0|1
        /* =1 display fps via printf (ugly but effective) */
        num = (int32) get_uint (cptr, 10, 1, &r);
        if (r != SCPE_OK) return r;
        CP_ShowFPS = num; 
    } else if (value == 1) { // SET PRESS=name
        if (DoActionByName((char *) cptr,0) < 0) {
            fprintf(stderr, "Control %s not found\r\n", cptr);
        }
    } else if (value == 2) { // SET MARK=name|<ALL>|<NONE>
        if (DoActionByName((char *) cptr,1) < 0) {
            fprintf(stderr, "Control %s not found\r\n", cptr);
        }
    } else if (value == 3) { // SET LIST=name|<ALL>
        if (DoActionByName((char *) cptr,2) < 0) {
            fprintf(stderr, "Control %s not found\r\n", cptr);
        }
    } else if (value == 4) { // SET STATE=name|<ALL> / n|*
        if (DoActionByName((char *) cptr,3) < 0) {
            fprintf(stderr, "Control %s not found\r\n", cptr);
        }
    } else if (value == 5) { // SET OPTION=name|<NONE> 
        if (AddOption((char *) cptr) < 0) {
            fprintf(stderr, "Cannot set option\r\n");
        }
    } else if (value == 6) { // SET TICKCOUNT=0|1
        num = (int32) get_uint (cptr, 10, 1, &r);
        if (r != SCPE_OK) return r;
        CP.EnableTickCount = num; 
    } else if (value == 7) { // SET SCALE=10..200
        num = (int32) get_uint (cptr, 10, 200, &r);
        if (r != SCPE_OK) return r;
        if (num < 10) return SCPE_ARG;
        CP.Scale = num; 
        if (cpanel_on == 0) {
            fprintf(stderr, "No cpanel open\r\n");
            return SCPE_OK; // no panel opened
        }
        xSize = CP.Control[0].W * CP.Scale / 100;
        ySize = CP.Control[0].H * CP.Scale / 100;
        vid_SetWindowSizeAndPos(1, xSize, ySize, 0,0,0);
    } else if (value == 8) { // SET POS=X/Y
        xPos = (int) strtotv (cptr, &tptr, 10);
        if (cptr == tptr) return SCPE_ARG;
        if (*tptr != '/') return SCPE_ARG;
        yPos = (int) strtotv (++tptr, &tptr, 10);
        if (cptr == tptr) return SCPE_ARG;
        if (cpanel_on == 0) {
            fprintf(stderr, "No cpanel open\r\n");
            return SCPE_OK; // no panel opened
        }
        vid_SetWindowSizeAndPos(0,0,0, 1,xPos,yPos);
    } else {
        // undefined SET 
        return SCPE_IERR;
    }
    return SCPE_OK;
}

static void cp_quit_callback (void)
{
    cp_stop_flag = 1; // request a stop, disabling interactive mode
}

/* Reset routine */

t_stat cp_reset (DEVICE *dptr)
{
    sim_debug (CP_CMDS, &cp_dev, "Control Panel Reset\n");
    if (!(dptr->flags & DEV_DIS)) {
        vid_register_quit_callback (&cp_quit_callback);
    }
    if ((cpanel_on) && (CP.CP_Reset != NULL)) CP.CP_Reset();

    return SCPE_OK;
}

t_stat cp_interactive(UNIT *uptr, int32 value, CONST char *cptr, void *desc)     { cpanel_interactive = 1; return SCPE_OK; }
t_stat cp_attach (UNIT *uptr, CONST char *cptr) 
{ 
    if (uptr == NULL)  return SCPE_IERR;
    if (uptr->filename) free(uptr->filename);
    uptr->filename = (char *) calloc (CBUFSIZE, sizeof (char)); /* alloc name buf */
    if (uptr->filename == NULL) return SCPE_MEM;
    strncpy (uptr->filename, cptr, CBUFSIZE);               /* save name */
    if (!file_exists(uptr->filename)) {
        fprintf(stderr, "Cannot open Control Panel definition file %s\r\n", uptr->filename);
        free(uptr->filename);
        uptr->filename = NULL;
        return SCPE_OPENERR;
    }
    uptr->flags = uptr->flags | UNIT_ATT;
    uptr->pos = 0;
    uptr->fileref = 0; 
    CP.Options[0] = 0;
    CP.EnableTickCount = 1; // default value
    CP.Scale = 100; // set default scale to 100% 
    return SCPE_OK;
}
t_stat cp_dettach(UNIT *uptr)
{
    if (uptr == NULL)  return SCPE_IERR;
    if (uptr->filename) free(uptr->filename);
    if (!(uptr->flags & UNIT_ATT)) {                        /* not attached? */
        return SCPE_NOTATT;                                 /* complain */
    }
    uptr->flags = uptr->flags & ~(UNIT_ATT);
    return SCPE_OK;
}


