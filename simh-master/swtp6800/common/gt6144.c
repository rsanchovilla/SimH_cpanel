/*  gt6144.c: GT-6144 Graphics Terminal + PPJ Analog Joystick Simulator 

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
        WILLIAM A BEECH BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
        IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
        CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

        Except as contained in this notice, the name of Roberto Sancho shall not
        be used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from Roberto Sancho .


*/

#include <stdio.h>
#include "sim_video.h"
#include "swtp_defs.h"

/* emulate a BW image  */

#define H_RESOL         64          // Horizontal resolution (similated pixels)     
#define V_RESOL         96          // Vertical resolution (similated pixels)   

#define X_MULT          12          // each simulated horiz pixel take 4 horiz GUI pixels on window
#define Y_MULT          6           // each simulated vertical pixel take 3 vertical GUI pixels on window

#define H_OFFSET        50          // horizontal offset on left/right of addressable image (GUI pixels on window)
#define V_OFFSET        30          // vertical offset on tob/bottom of addressable image (GUI pixels on window)

#define JOY_STEP        3           // default joystick movement each time arrow key is pressed (keypad 5 centers joy)

#define REFRESH_INTERVAL  1000      // for svr call 
/* function prototypes */

t_stat gt6144_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat ppj_set_sensitivity(UNIT *uptr, int32 val, CONST char *cptr, void *desc);

t_stat gt6144_svc (UNIT *uptr);
void gt6144_ClearScreen(void); 
void gt6144_Refresh (uint32 tnow); 

/* SS-50 I/O address space functions */

int32 gt6144_pia1(int32 io, int32 data);
int32 ppj_pia1(int32 io, int32 data);
int32 ppj_pia2(int32 io, int32 data);

/* Local Variables */
struct {
    int power;           // 0=terminar powered off=no GUI window, 1=GUI window visible
    uint32 * surface;    // GUI surface.  
    uint32 * blanked_surface; 
    uint32 color[2];     // black and white color
    int wx, wy;          // width and height of GUI window
    int refresh_needed;  // 1=if video ram has been modified and needs to be refreshed on GUI
    uint32 refresh_tnow; // value for sim_os_msec() on last GUI refresh
    int quit_requested;  // =1 is user closes the GUI window

    int data_holding;    // byte received from swtpc on data holding flip flops
    int blanked;         // =1 if graphic screen is blanked
    int inverted;        // =1 if graphic screen is inverted
    int joy_sensitivity; // joy position incr/decr when a cursor key is sensed
    int joy_x, joy_y;    // joystick current position: x=0..63, y=0..95
    int joy_seq;         // =0 -> return joy_x, =1 -> return joy_y
} gt6144 = {0}; 

DEVICE gt6144_dev; 

MTAB gt6144_mod[] = {
    { MTAB_XTD | MTAB_VDV, 1,       NULL,     "ON",          &gt6144_set_power_on, NULL, &gt6144_dev },
    { MTAB_XTD | MTAB_VDV, 0,       NULL,     "OFF",         &gt6144_set_power_on, NULL, &gt6144_dev },
    { MTAB_XTD | MTAB_VDV, 0, "PPJSEN", "PPJSENS", &ppj_set_sensitivity, NULL, NULL, NULL},
    { 0 }
};


UNIT gt6144_unit = { UDATA (&gt6144_svc, UNIT_DISABLE + UNIT_DIS, 0)
    };

DEVICE gt6144_dev = {
    "GT6144",                           //name
    &gt6144_unit,                       //units
    NULL,                               //registers
    gt6144_mod,                         //modifiers
    1,                                  //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    NULL,                               //examine
    NULL,                               //deposit
    NULL,                               //reset
    NULL,                               //boot
    NULL,                               //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    NULL,                               //debflags
    NULL,                               //msize
    NULL                                //lname
};

static void quit_callback (void)
{
    if (sim_is_running) {
        gt6144.quit_requested=1; 
    }
}

/* power on routine */

t_stat gt6144_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    extern struct idev dev_table[32]; 
    extern int32 nulldev(int32 io, int32 data);

    t_stat stat; 

    if (value == 1) {
        if (gt6144.power == 1) return SCPE_OK; // already powered on
        // create GUI window
        gt6144.wx=H_RESOL * X_MULT + 2 * H_OFFSET; 
        gt6144.wy=V_RESOL * Y_MULT + 2 * V_OFFSET; 
        gt6144.surface = (uint32 *)malloc (gt6144.wx * gt6144.wy * sizeof(uint32));
        gt6144.color[0]=0;
        gt6144.color[1]=(uint32) (-1);
        gt6144.blanked_surface = (uint32 *)malloc (gt6144.wx * gt6144.wy * sizeof(uint32));
        gt6144.blanked_surface[0]=1; // to force surface fill when blank set
        stat = vid_open (&gt6144_dev, "GT-6144 Graphic Terminal", gt6144.wx, gt6144.wy, 0);
        if (stat != SCPE_OK) return stat; 
        // init done
        gt6144.power=1; 
        // power on -  set I/O ports to point to Graphic Terminal One
        dev_table[0x0C].routine = &gt6144_pia1; 
        dev_table[0x0E].routine = &ppj_pia1; 
        dev_table[0x0F].routine = &ppj_pia2; 
        // set quit callback
        gt6144.quit_requested=0; 
        vid_register_quit_callback (&quit_callback);
        // init local vars
        gt6144.inverted=0; 
        gt6144.blanked=0; 
        gt6144.data_holding=0;
        gt6144.joy_x=64/2; gt6144.joy_y=96/2; // joy centered
        gt6144.joy_seq=0; 
        // clear screen 
        gt6144_ClearScreen(); 
        // force refresh
        gt6144_Refresh (0);
        sim_activate(&gt6144_unit, REFRESH_INTERVAL);
    } else if (value == 0) {
        if (gt6144.power == 0) return SCPE_OK; // already powered off
        // power off - restore I/O ports back to disk 
        dev_table[0x0C].routine = &nulldev; 
        dev_table[0x0E].routine = &nulldev; 
        dev_table[0x0F].routine = &nulldev; 
        // close GUI window
        vid_close();
        // init done
        gt6144.power=0; 
        if (gt6144.surface) free(gt6144.surface); 
        if (gt6144.blanked_surface) free(gt6144.blanked_surface); 
        sim_cancel(&gt6144_unit);
    }
    return SCPE_OK; 
}

void gt6144_Refresh (uint32 tnow)
{
    uint32 blank_col; 

    if (gt6144.blanked) {
        // determine color of blanked screen
        blank_col = gt6144.color[(gt6144.inverted) ? 1:0]; 
        // set this color if necessary
        if (gt6144.blanked_surface[0] != blank_col) {
            memset(gt6144.blanked_surface, blank_col, 
                  gt6144.wx * gt6144.wy * sizeof(uint32));        
        }
        vid_draw (0, 0, gt6144.wx, gt6144.wy, gt6144.blanked_surface);
    } else {
        vid_draw (0, 0, gt6144.wx, gt6144.wy, gt6144.surface);
    }
    vid_refresh ();
    gt6144.refresh_needed=0;
    if (tnow==0) tnow = sim_os_msec(); 
    gt6144.refresh_tnow = tnow; 
}

void gt6144_SetPixel(int xx, int yy, int col)
{
    int x,y; 

    if ((xx < 0) || (xx >= H_RESOL)) return; // check pixel off screen
    if ((yy < 0) || (yy >= V_RESOL)) return; 
    xx = xx * X_MULT + H_OFFSET; 
    yy = yy * Y_MULT + V_OFFSET; 
    for (y=0; y<Y_MULT; y++) {
        for (x=0;x<X_MULT;x++) { 
            if (gt6144.inverted) col=1-col; 
            gt6144.surface[xx + x + (yy + y) * gt6144.wx]=gt6144.color[col];
        }
    }
    gt6144.refresh_needed=1; 
}

void gt6144_ClearScreen (void)
{
    if (gt6144.power == 0) return; // powered off -> ignore command
    memset(gt6144.surface, gt6144.color[ (gt6144.inverted) ? 1:0 ], 
                  gt6144.wx * gt6144.wy * sizeof(uint32));
}

void gt6144_InvertScreen(void)
{
    int i; 
    for(i=0; i<gt6144.wx * gt6144.wy; i++) {
        gt6144.surface[i] = ~gt6144.surface[i]; 
    }
}

t_stat ppj_set_sensitivity(UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    num = (int32) get_uint (cptr, 10, 96, &r);
    if (r != SCPE_OK) return r;
    gt6144.joy_sensitivity=num; 
    return SCPE_OK; 
}


void ppj_joy_poll(void)
{
    extern UNIT sio_unit; 

    SIM_KEY_EVENT ev;
    t_stat r; 
    int c, step; 

    for(;;) {
        r=vid_poll_kb(&ev);
        if (r==SCPE_EOF) break; 
        if (ev.state == SIM_KEYPRESS_UP) continue; // ignore keyup event
        c=ev.key; // key pressed

        if ((c >= SIM_KEY_0) && (c <= SIM_KEY_9)) {
            // main keyboard numeric key pressed -> process them as if pressed 
            // on text terminal (then cmd.exe console)
            c='0' + c-SIM_KEY_0; 
            sio_unit.buf = c; 
            sio_unit.u3 |= 0x01;                // Set RXF flag
            sio_unit.pos++;                     // step character count
            return; 
        }
        step = gt6144.joy_sensitivity; 
        if (step == 0) step=JOY_STEP; // if joy sensitivity not set, use default

        if ((c == SIM_KEY_LEFT) || (c==SIM_KEY_KP_LEFT)) {
            // left cursor key/4 in numeric keypad
            gt6144.joy_x -= step; 
        } if ((c == SIM_KEY_RIGHT) || (c==SIM_KEY_KP_RIGHT)) {
            // right cursor key/6 in numeric keypad
            gt6144.joy_x += step; 
        } if ((c == SIM_KEY_UP) || (c==SIM_KEY_KP_UP)) {
            // up cursor key/8 in numeric keypad
            gt6144.joy_y -= step; 
        } if ((c == SIM_KEY_DOWN) || (c==SIM_KEY_KP_DOWN)) {
            // down cursor key/2 in numeric keypad
            gt6144.joy_y += step; 
        } if (c == SIM_KEY_KP_5) {
            // 5 in numeric keypad -> return to center position
            gt6144.joy_x = 64 / 2; 
            gt6144.joy_y = 96 / 2; 
        }
        if (gt6144.joy_x < 0) gt6144.joy_x=0; else
        if (gt6144.joy_x >= 64) gt6144.joy_x=64-1; else
        if (gt6144.joy_y < 0) gt6144.joy_y=0; else
        if (gt6144.joy_y >= 96) gt6144.joy_y=96-1; 
    }
}

t_stat gt6144_svc (UNIT *uptr)
{
    uint32 tnow; 
    uint32 msec; 

    if (gt6144.quit_requested) return SCPE_EXIT; 
    sim_activate(&gt6144_unit, REFRESH_INTERVAL);
    // scan keyboard to simulate joystick
    ppj_joy_poll();
    if (gt6144.refresh_needed==0) return SCPE_OK; // nothing to refresh on screen
    // check if 20 msec has elapsed from last refresh (20 msec -> 50 FPS)
    tnow = sim_os_msec(); 
    msec = tnow - gt6144.refresh_tnow; 
    if (msec < 20) return SCPE_OK; // do not refresh yet
    // should refresh as >20msec has elapsed from previous refresh
    gt6144_Refresh(tnow); 
    return SCPE_OK; 
}

//  I/O instruction handlers, called from the MP-B2 module 

int32 gt6144_pia1(int32 io, int32 data)
{
    int x,y,col,cmd; 

    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        // terminal allways ready
        return 0xFF; 
    } 
    // io=1 -> writing data to i/o register,
    if ((data & 0x80) == 0) {
        // bit7 = FF LOAD BIT/MEMORY LOAD BIT. IF =0 -> Bits 0..6 store in data holding flip flops
        gt6144.data_holding = data & 0x7F;
        return 0; 
    }
    if ((data & 0xE0) == 0xE0) {
        // bits 7..5 = 1 -> control command
        cmd = data & 7; 
        if (cmd == 0) { // set inverted screen
            if (gt6144.inverted==0) {
                gt6144.inverted=1; 
                gt6144_InvertScreen(); 
            }
        } else if (cmd == 1) { // set normal screen 
            if (gt6144.inverted) {
                gt6144.inverted=0; 
                gt6144_InvertScreen(); 
            }
        } else if (cmd == 4) { // enable screen
            gt6144.blanked=1; 
        } else if (cmd == 5) { // blank screen
            gt6144.blanked=0; 
        }
        gt6144.refresh_needed=1; 
        return 0; 
    }
    col= (gt6144.data_holding & 0x40) ? 1:0;
    x=gt6144.data_holding & 0x3F; 
    y=data & 0x7F; 
    gt6144_SetPixel(x,y,col); 
    return 0; 
}

int32 ppj_pia1(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        // first read return joy_x with bit7=1,  
        // second read return joy_y with bit7=0, and so on
        if (gt6144.joy_seq == 0) {
           data = (gt6144.joy_x & 0x7f); 
           data += 0x80; // bit7=1 to signal joy_x value is returned
           gt6144.joy_seq = 1; 
        } else {
           data = (gt6144.joy_y & 0x7f); 
           gt6144.joy_seq = 0; 
        }
        return data; 
    } 
    return 0; 
}

int32 ppj_pia2(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        // on reading return bit7 set (data ready)
        data=0x80; 
        return data; 
    } 
    // when writing, reset joy sequence
    gt6144.joy_seq=0; 
    return 0; 
}