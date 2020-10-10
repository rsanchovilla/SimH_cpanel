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

struct cpvidrec {                             
    int mouse_b1;                           // =1 if mouse button 1 is being pressed
    int X, Y;                               // mouse position on GUI Panel Window (0,0 is top left corner) of pressed button
    char last_char;                         // last char pressed (ascii value). Set when key released. Never cleared (overwritten with next key pressed)
    uint32 kev_key;                         /* SDL key sym */
    uint32 kev_state;                       /* SDL key state change */
    int    kev_modifier;                    // bit0=shift pressed, bit1=cntrl pressed
};

extern struct cpvidrec cpvid;   

extern void get_surface_rgb_color(uint32 color, int *r_Color, int *g_Color, int *b_Color);
extern uint32 surface_rgb_color(uint32 r, uint32 g, uint32 b);  /* r,g,b are 8bit! */
extern uint32 * get_surface(int *xp, int *yp); // return GUI surface (and size) to draw pixels on

extern void cpvid_poll(void);                           // scan GUI and update cpvid struct
extern int  cpvid_sync(int);                            // send surface rectable to GUI window
extern int  cpvid_init(const char *, int, int, void *); // create and open GUI window
extern void cpvid_shutdown(void);                       // close GUI window

extern uint32 * read_png_file(char* file_name, int * WW, int * HH);
                                                    