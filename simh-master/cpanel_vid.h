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

   14-May-20    RSV     Control panel video routines support 
                        Based on sim_ws.c module by Mark Pizzolato


*/

struct cpinputrec {                             
    struct {
        VID_DISPLAY * vptr;                     // windows that has generated the mouse event
        int ncp;                                // index in cpvid that signals the control panel who has the focus for mouse input (-1 if none)
        int b1;                                 // =1 if mouse button 1 is being pressed
        int X, Y;                               // mouse position on screen coordinates on current vptr GUI Window (0,0 is top left corner) of pressed button
        int drag_flag;                          // 1-> drag in progresss, 2=drag ended (b1 released)
    } mouse; 
    struct {
        int  ncp;                               // index in cpvid that signals the control panel who has the focus for keyboard
    } keyb;
    struct {
        int ncp;                                // index in cpvid that signals the control panel who got the file drag and dropped
    } DropFile; 
};
extern struct cpinputrec cpinput; 

#define MAX_CPVID_WINDOWS   6                            // max number of cpanel windows 
#define MAX_CPVID_KEYBUF  128                            // size of keyboard buffer (ascii chars)

struct cpvidrec {
    char short_name[32];                                // short name of the cpanel, to be used on SCP commands set cpanel name ...
    char long_name[128];                                // long name of the cpanel, as defined in definiton file .ini with tag ControlPanelName=
    int xpixels, ypixels;                               // size of cpanel (when created at 100% scale)
    int InitialScale;                                   // initial scale 10..200 to be used when window is created
    int InitialPosX, InitialPosY;                       //         x,y postion of window on screen to be used when window is created
    int bTextInput;                                     // if=0, window scales with +/-/^+/^-/^Y key, does not trasnlate natinal char to ascii. If =1, scales only eith ^+/^- but translates national keyboard
    int bCanBeClosed;                                   // if=1, the window can be closed and reopened, without closing the whole emulator
    uint32 *surface;                                    // control panel pixels array. This is the whole 100% scale panel image bitmat
    rectlist RectList;                                  // rectangle list: indicate rectangles in pixels array to update on screen at given scale    
    int keyb_buf[MAX_CPVID_KEYBUF];                     // keyboard buffer of ascii values/scancodes for non-ascii keys typed (if bit30 (1<<30) set then bits 29..0 contains the scancode
    int keyb_buf_len;                                   // number of keys in buffer
    VID_DISPLAY * vptr_cp;                              // pointer to cpanel window for sim_video. Note that xpixels, ypixels and scale values are mirrored in vptr->vid_width, vid_heigh, scale
};
extern struct cpvidrec cpvid[MAX_CPVID_WINDOWS];        // array of cpanels
extern int             cpvid_count;                     // number of entries in cpvid

extern uint32 * get_surface(int ncp, int *xp, int *yp); // return GUI surface (and size) to draw pixels on for control panel vwindow cpvid[ncp]

extern void cpvid_poll(void);                           // scan GUI and update cpinput struct
extern void cpvid_sync(void);                           // send surface rectable to GUI window
extern int  cpvid_checkredraw(void);                    // check if refresh in progress
extern int  cpvid_init(int ncp, const char *, int, int, void *, int, int, int); // open GUI window for given cpanel title, width and height
extern void cpvid_open(int ncp);                       // open a single cpanel windows named ncd. At least another panel should be visible (so DF file has been parsed)
extern void cpvid_close(int ncp);                      // close GUI window of given cpanel name
extern char cpvid_getkey(int ncp);
extern int  find_cpvid_by_vptr(VID_DISPLAY * ); 
extern void cpvid_AddRectList(int ncp, int x, int y, int w, int h, int flags); 
                                                   