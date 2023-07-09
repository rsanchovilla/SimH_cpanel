/* cpanel_vid.c: simulator control panel video routines

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
   ROBERT M SUPNIK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of Robert M Supnik shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from Robert M Supnik.

   14-May-20    RSV     Control panel video routines support 
                        Based on sim_ws.c module by Mark Pizzolato
                        
*/

// cpanel_vid.c needs the preprocessor symbol CPANEL to be defined

#if defined(CPANEL)

#include "sim_video.h"
#include "cpanel_vid.h"

#define CP_REFRESH      0x00000002                    // refresh events
extern DEVICE cp_dev;                                 /* control panel device */

struct cpinputrec cpinput;                            // data for cpanel windows. Applies to all windows
struct cpvidrec cpvid[MAX_CPVID_WINDOWS] = {0};   
int cpvid_count = -1;                                 // number of entries in cpvid. -1 -> not initialized
int DropFile_ncp = -1;                                // entry in cpvid that got the file droped

// initialize cpvid array. Called from cpvid_init or cp_attach, wich happedns first
void init_array_cpvid(void) 
{
    int ncp; 

    if (cpvid_count >= 0) return; // already initialized
    memset(cpvid, 0, sizeof(cpvid));
    for (ncp=0; ncp<MAX_CPVID_WINDOWS; ncp++) {
        cpvid[ncp].InitialScale = 100; // default scale=100%
        cpvid[ncp].InitialPosX  =  90; 
        cpvid[ncp].InitialPosY  =  50; 
    }
    cpvid_count=0; // mark as initialized
}

uint32 * get_surface(int ncp, int *xp, int *yp) 
{
    // return static file-scope surface pointer for direct manipulation outside this module
    // return static xpixels,ypixels (the surface size in pixels) as wx,wy param 
    // handle with care, do not go outside the surface
    if (ncp >= MAX_CPVID_WINDOWS) return NULL; //safety
    *xp = cpvid[ncp].xpixels; 
    *yp = cpvid[ncp].ypixels; 
    return cpvid[ncp].surface;
}

// find cpvid entry that matches vptr
// return index in cpvid array, -1 if not found
int find_cpvid_by_vptr(VID_DISPLAY *vptr_cp)
{
    int ncp; 

    for (ncp=0; ncp<cpvid_count; ncp++) {
        if (cpvid[ncp].vptr_cp == vptr_cp) {
            return ncp; 
            break; 
        }
    }
    return -1; 
}

// add a rectable to rectangle list for given cpvid entry
void cpvid_AddRectList(int ncp, int x, int y, int w, int h, int flags)
{
    int n; 

    if (ncp >= MAX_CPVID_WINDOWS) return; //safety
    n=cpvid[ncp].RectList.Count; 
    if (n==-1) {
       // rect table full
    } else if (n>=RECTLIST_MAX) {
       // set rect table as full
       cpvid[ncp].RectList.Count=-1; 
    } else {
       // room left on rectable table. can add entry
       cpvid[ncp].RectList.flags[n]=flags;
       cpvid[ncp].RectList.x[n]=x; 
       cpvid[ncp].RectList.y[n]=y; 
       cpvid[ncp].RectList.w[n]=w; 
       cpvid[ncp].RectList.h[n]=h; 
       cpvid[ncp].RectList.Count++;
    }
}


// create cpanel window at given cpvid entry. If short_name is empty, "MAIN" is used as default name
// initialize cpvid entry, alloc surface 
int cpvid_init(int ncp, const char *title, int xp, int yp, void *dptr, 
               int InitialScale, int InitialPosX, int InitialPosY)
{
    t_stat r; 
    VID_DISPLAY *vptr_cp; 

    // init cpvid if needed    
    if (cpvid_count < 0) init_array_cpvid(); 
    if (ncp<0) return -1; 
    cpvid[ncp].xpixels = xp;
    cpvid[ncp].ypixels = yp;
    // cpvid[ncp].bTextInput should already be set previous to call to cpvid_init
    // cpvid[ncp].short_name should already be set previous to call to cpvid_init
    // Scale already set to 100 on init_array_cpvid, can have been changed previously
    strncpy(cpvid[ncp].long_name, title, sizeof(cpvid[ncp].long_name));
    // init to zero the others record entries just in case we are reopening this cpanel
    memset(&cpvid[ncp].RectList, 0, sizeof(cpvid[ncp].RectList)); 
    // create the pixel array (called in cpanel surface), the bitmap contents of windo
    cpvid[ncp].surface = (uint32 *)malloc(xp*yp*sizeof(*cpvid[ncp].surface));
    if (!cpvid[ncp].surface) return -1;
    cpvid[ncp].keyb_buf_len=0; 
    // create the window on GUI, but hidden. So initial resize can be done without being visible
    r = vid_open_window_ex (&vptr_cp, (DEVICE *)dptr, title, xp, yp, 
                            SDL_WINDOW_HIDDEN | 
                            SIM_VID_FLAG_SCALE_PLUSMINUS | 
                            ((cpvid[ncp].bTextInput==1) ? SIM_VID_FLAG_ALLOW_TEXTINPUT : 0) |
                            ((cpvid[ncp].bTextInput==2) ? SIM_VID_FLAG_ALLOW_ALPHAINPUT : 0) |
                            SIM_VID_FLAG_ALLOW_TOOLTIP   | 
                            SIM_VID_FLAG_ALLOW_DRAG      , 
                            InitialScale, InitialPosX, InitialPosY);
    if (r) {
      fprintf(stderr, "vid_open_window error %d\n", r);
      return -1; 
    }
    cpvid[ncp].vptr_cp = vptr_cp; 
    return ncp;
}

// close cpanle window of given short_name. If ncp=-1, then close all windows
void cpvid_close(int ncp)
{
    if (vid_active==0) return; // video system not active
    if (ncp<0) {
        for (ncp=0; ncp<cpvid_count; ncp++) cpvid_close(ncp); 
    } else {
       if (cpvid[ncp].vptr_cp) vid_close_window(cpvid[ncp].vptr_cp);
       if (cpvid[ncp].surface) free(cpvid[ncp].surface);
       cpvid[ncp].surface = NULL;
       cpvid[ncp].vptr_cp = NULL;
    }
}

// check refresh 
// return 0 if a refresh is in progress
// return 1 if a refresh can be done
int cpvid_checkredraw(void)
{
    int ncp;
    VID_DISPLAY *vptr_cp;

    // check no cpvid has a refresh in progress
    for (ncp=0; ncp<cpvid_count; ncp++) {
        vptr_cp = cpvid[ncp].vptr_cp; 
        if (vptr_cp == NULL) continue; 
        if (vid_refresh_in_progress(vptr_cp)) {
           sim_debug (CP_REFRESH, &cp_dev, "GUI Refresh skipped because vid_refresh_in_progress\n");
           return 0;
        }
    }
    return 1; 
}

// do the refresh
void cpvid_sync(void) 
{
    // these are standard call for SDL. vid_draw malloc and memcpy all the surface data, 
    // refresh copies all the data to sdl_texture. 
    // 
    // vid_draw (0, 0, xpixels, ypixels, surface);
    // vid_refresh ();
    //
    // If cpanel is big (say 3000x1000 pixels), surface will have >10MB. To avoid these big
    // malloc and memcopy, a new surface refresh list is used, to update only changed surface zones
    // a list of changed rectangles is provided in RectList structure- Also this allows to scale
    // down image using antialiasing for a nice control panel looking

    uint32 * surface;  
    void   * RectList; 
    VID_DISPLAY *vptr_cp;
    int ncp; 

    // call refresh for each cpanel window in cpvid array
    for (ncp=0; ncp<cpvid_count; ncp++) {
        if (cpvid[ncp].RectList.Count==0) continue; // nothing to refresh
        vptr_cp = cpvid[ncp].vptr_cp; 
        if (vptr_cp == NULL) continue; 
        surface  = cpvid[ncp].surface;  
        RectList = &cpvid[ncp].RectList; 
        vid_refresh_ex(vptr_cp, surface, RectList); 
    }
}

// fills cpinput struct with current input states
void cpvid_poll(void)
{
    SIM_MOUSE_EVENT mev;
    SIM_KEY_EVENT kev;
    int n,ncp,key;

    if (SCPE_OK == vid_poll_mouse (&mev)) {
        // mouse coordinates are relative to window, and as if scale is 100%
        // x,y coord are relative to surface size, and vid_wifth, vid_height. Not relative to the
        // current scale of the window. The scale of window is transparent
        cpinput.mouse.b1 = mev.b1_state;
        cpinput.mouse.X = mev.x_pos; // position of mouse click/release event
        cpinput.mouse.Y = mev.y_pos; 
        cpinput.mouse.drag_flag=mev.drag_flag; 
        cpinput.mouse.vptr=mev.vptr; 
        cpinput.mouse.ncp=find_cpvid_by_vptr(mev.vptr); 
    }
    // get the ncp index for LeyPress/LastKeyPress
    if (vid_keyb.vptr) cpinput.keyb.ncp=cpinput.keyb.ncp=find_cpvid_by_vptr(vid_keyb.vptr); 
    // poll textinput keys
    while (vid_poll_kb (&kev) == SCPE_OK) {
        // get char in sim_video key event queue, and convert to ascii
        ncp=cpinput.keyb.ncp=find_cpvid_by_vptr(kev.vptr); 
        if (ncp>=0) {
            key=kev.key; 
            // now add key to cpvid input buffer
            n=cpvid[ncp].keyb_buf_len; 
            if (n>=MAX_CPVID_KEYBUF) {
               // keyboard buffer full, do not add char to buffer
            } else if (key) {
               // add key pressed to buffer (can be ascii valur or scancode depending on bit 30)
               cpvid[ncp].keyb_buf[n]=key; 
               cpvid[ncp].keyb_buf_len++;
            }
        }
    }
    // set RightButton var with the current state of mouse right button, even if
    // cpanel window has not the focus. This is used to detect droping a file
    // with the right button pressed
    {
        int n,x,y; 
        n=SDL_GetGlobalMouseState(&x,&y);            
        if (n & SDL_BUTTON(SDL_BUTTON_RIGHT)) {
            vid_keyb.RightButton = 5; // neet 5 polls to get to zero
        } else if (vid_keyb.RightButton > 0) {
            vid_keyb.RightButton--;
        }
        if (n & SDL_BUTTON(SDL_BUTTON_LEFT)) vid_keyb.RightButton=0;
    }
    if (DropFile_FileName[0]) {
        cpinput.DropFile.ncp=find_cpvid_by_vptr(DropFile_vptr); 
    }
}

// return next char in input buffer for ncp window
// if no char return 0
char cpvid_getkey(int ncp)
{
    int n,i; 
    char c; 

    if (ncp < 0) return 0;
    n=cpvid[ncp].keyb_buf_len;
    if (n==0) return 0; 
    c=cpvid[ncp].keyb_buf[0]; 
    if (n==1) {
        // keyb buffer only had one char, buffer is empty
        cpvid[ncp].keyb_buf_len=0;
    } else {
        // remove char from buffer
        for (i=0; i<n-1; i++) {
            cpvid[ncp].keyb_buf[i]=cpvid[ncp].keyb_buf[i+1];
        }
        cpvid[ncp].keyb_buf_len--;
    }
    return c; 
}


// Matches the ifdef(CPANEL) at beginning of file
#endif

  

