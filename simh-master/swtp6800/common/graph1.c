/*  graph1.c: Graphics One subLogic Graphic terminal Simulator

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

    MODIFICATIONS:

    NOTES:

        The Graphic One is a 1976 graphic terminal designed by Bruec Artwitck
        It is raster B/W raster graphic terminal with a TTL based custom CPU
        Resolution is 200x200 pixels
        https://ia601902.us.archive.org/10/items/graphicsonetermi814artw/graphicsonetermi814artw.pdf

        Even if this doc describes the hw, there is no information on graphic commands handled by terminal
        from subLogic 3d basic package, we can make deductions on draw vector command
        https://ia903008.us.archive.org/19/items/Sublogic_3_Dimensional_Microcomputer_Graphics_-1/Sublogic_3_Dimensional_Microcomputer_Graphics_-1.pdf

        
        Address     Mode    Function
        -------     ----    --------

        0x8018      write   0x13, 0x59          clear pia
        0x8018      read    0x00                -> terminal busy , <>0 -> terminal ready
        0x8019      write   0x59,x0,y0,x1,y1    draw vector command
                 
*/

#include <stdio.h>
#include "sim_video.h"
#include "swtp_defs.h"

/* emulate a BW image  */

#define H_RESOL         200         // Horizontal resolution (similated pixels)     
#define V_RESOL         200         // Vertical resolution (similated pixels)   

#define X_MULT          4           // each simulated horiz pixel take 4 horiz GUI pixels on window
#define Y_MULT          3           // each simulated vertical pixel take 3 vertical GUI pixels on window

#define H_OFFSET        50          // horizontal offset on left/right of addressable image (GUI pixels on window)
#define V_OFFSET        30          // vertical offset on tob/bottom of addressable image (GUI pixels on window)


#define REFRESH_INTERVAL  1000      // for svr call 
/* 
Refresh strategy:

- If program makes any changes in videoram or in appearence of screen 
  (clear screen, draw a vector) 
  then set refresh_needed flag=1
- GraphicOne_svc() is shedulled with sim_activate() to be executed
  each REFRESH_INTERVAL cpu instructions     
- GraphicOne_svc() checks 
     - Screen should be refreshed? (ie. refresh_needed!=0 ?) -> no, exit
     - Has at least 20msec elapsed form frevious refresh? -> no, exit
     - Clears flag: refresh_needed=0
     - Send the whole SDL2 surface to GUI window for repainting
*/ 

t_stat GraphicOne_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat GraphicOne_ClearScreen (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat GraphicOne_svc (UNIT *uptr);

/* SS-50 I/O address space functions */

int32 GraphicOne_pia1(int32 io, int32 data);
int32 GraphicOne_pia2(int32 io, int32 data);

/* Local Variables */
struct {
    int power;           // 0=terminar powered off=no GUI window, 1=GUI window visible
    uint32 * surface;    // GUI surface
    uint32 color[2];     // black and white color
    int wx, wy;          // width and height of GUI window
    int refresh_needed;  // 1=if video ram has been modified and needs to be refreshed on GUI
    uint32 refresh_tnow; // value for sim_os_msec() on last GUI refresh
    int quit_requested;  // =1 is user closes the GUI window

    int cmd;             // command received from swtpc
    int x0,y0,x1,y1;     // command parameters; 
} GraphicOne = {0}; 

DEVICE GraphicOne_dev; 

MTAB GraphicOne_mod[] = {
    { MTAB_XTD | MTAB_VDV, 1,       NULL,     "ON",          &GraphicOne_set_power_on, NULL, &GraphicOne_dev },
    { MTAB_XTD | MTAB_VDV, 0,       NULL,     "OFF",         &GraphicOne_set_power_on, NULL, &GraphicOne_dev },
    { MTAB_XTD | MTAB_VDV, 0,       NULL,     "CLS",         &GraphicOne_ClearScreen,  NULL, &GraphicOne_dev },
    { 0 }
};


UNIT GraphicOne_unit = { UDATA (&GraphicOne_svc, UNIT_DISABLE + UNIT_DIS, 0)
    };

DEVICE GraphicOne_dev = {
    "GRAPH1",                           //name
    &GraphicOne_unit,                   //units
    NULL,                               //registers
    GraphicOne_mod,                     //modifiers
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
        GraphicOne.quit_requested=1; 
    }
}

/* power on routine */

t_stat GraphicOne_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    extern struct idev dev_table[32]; 
    extern int32 dc4_fdccmd(int32 io, int32 data);
    extern int32 dc4_fdctrk(int32 io, int32 data);

    t_stat stat; 

    if (value == 1) {
        if (GraphicOne.power == 1) return SCPE_OK; // already powered on
        // create GUI window
        GraphicOne.wx=H_RESOL * X_MULT + 2 * H_OFFSET; 
        GraphicOne.wy=V_RESOL * Y_MULT + 2 * V_OFFSET; 
        GraphicOne.surface = (uint32 *)malloc (GraphicOne.wx * GraphicOne.wy * sizeof(uint32));
        GraphicOne.color[0]=0;
        GraphicOne.color[1]=(uint32) (-1);
        stat = vid_open (&GraphicOne_dev, "Graphic One", GraphicOne.wx, GraphicOne.wy, 0);
        if (stat != SCPE_OK) return stat; 
        // init done
        GraphicOne.power=1; 
        // power on -  set I/O ports to point to Graphic Terminal One
        dev_table[0x18].routine = &GraphicOne_pia1; 
        dev_table[0x19].routine = &GraphicOne_pia2;
        // set quit callback
        GraphicOne.quit_requested=0; 
        vid_register_quit_callback (&quit_callback);
        // clear screen 
        GraphicOne_ClearScreen(uptr, 0, NULL, NULL); 
        sim_activate(&GraphicOne_unit, REFRESH_INTERVAL);
    } else if (value == 0) {
        if (GraphicOne.power == 0) return SCPE_OK; // already powered off
        // power off - restore I/O ports back to disk 
        dev_table[0x18].routine = &dc4_fdccmd; 
        dev_table[0x19].routine = &dc4_fdctrk;
        // close GUI window
        vid_close();
        // init done
        GraphicOne.power=0; 
        if (GraphicOne.surface) free(GraphicOne.surface); 
        sim_cancel(&GraphicOne_unit);
    }
    return SCPE_OK; 
}

void GraphicOne_Refresh (uint32 tnow)
{
    vid_draw (0, 0, GraphicOne.wx, GraphicOne.wy, GraphicOne.surface);
    vid_refresh ();
    GraphicOne.refresh_needed=0;
    if (tnow==0) tnow = sim_os_msec(); 
    GraphicOne.refresh_tnow = tnow; 
}

void GraphicOne_SetPixel(int xx, int yy)
{
    int x,y; 

    if ((xx < 0) || (xx >= H_RESOL)) return; // check pixel off screen
    if ((yy < 0) || (yy >= V_RESOL)) return; 
    xx = xx * X_MULT + H_OFFSET; 
    yy = yy * Y_MULT + V_OFFSET; 
    for (y=0; y<Y_MULT; y++) {
        for (x=0;x<X_MULT;x++) { 
            GraphicOne.surface[xx + x + (yy + y) * GraphicOne.wx]=GraphicOne.color[1];
        }
    }
}

t_stat GraphicOne_ClearScreen (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    int x, y; 

    if (GraphicOne.power == 0) return SCPE_OK; // powered off -> ignore command
    // clear the screen: all pixels set to black
    memset(GraphicOne.surface, 0, GraphicOne.wx * GraphicOne.wy * sizeof(uint32));
    // draw white frame on addressable area of screen
    for (y=0; y<V_RESOL; y++) {
        GraphicOne_SetPixel(0, y); 
        GraphicOne_SetPixel(H_RESOL-1, y); 
    }
    for (x=0; x<H_RESOL; x++) {
        GraphicOne_SetPixel(x, 0); 
        GraphicOne_SetPixel(x, V_RESOL-1); 
    }
    // force refresh
    GraphicOne_Refresh (0);
    return SCPE_OK; 
}

t_stat GraphicOne_svc (UNIT *uptr)
{
    uint32 tnow; 
    uint32 msec; 

    if (GraphicOne.quit_requested) return SCPE_EXIT; 
    sim_activate(&GraphicOne_unit, REFRESH_INTERVAL);
    if (GraphicOne.refresh_needed==0) return SCPE_OK; // nothing to refresh
    // check if 20 msec has elapsed from last refresh (20 msec -> 50 FPS)
    tnow = sim_os_msec(); 
    msec = tnow - GraphicOne.refresh_tnow; 
    if (msec < 20) return SCPE_OK; // do not refresh yet
    // should refresh as >20msec has elapsed from previous refresh
    GraphicOne_Refresh(tnow); 
    return SCPE_OK; 
}

void GraphicOne_vector(void)
{
    int s,m,n,dx,dy;
    int x1 = GraphicOne.x0; 
    int y1 = GraphicOne.y0; 
    int x2 = GraphicOne.x1; 
    int y2 = GraphicOne.y1; 

    s=0; m=1; n=1; 
    dx=x2-x1;
    if (dx<0) {m=-1; dx=-dx; }
    else if (dx==0) s=-1;
    dy=y2-y1;
    if (dy<0) {n=-1; dy=-dy; }
    for(;;) {
        GraphicOne_SetPixel(x1, y1); 
        if ((x1 == x2) && (y1==y2)) break; 
        if (s<0) {
            y1+=n; 
            s+=dx; 
        } else {
            x1+=m;
            s-=dy;
        }
    }
    GraphicOne.refresh_needed=1; 
}

/*  I/O instruction handlers, called from the MP-B2 module when a
   read or write occur to addresses 0x8018-0x8019. */

/* GraphicOne register */
// 0x8018      write   0x13, 0x59          clear pia
// 0x8018      read    0x00                -> terminal busy , <>0 -> terminal ready

int32 GraphicOne_pia1(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        // terminal allways ready
        return 0xFF; 
    } 
    // io=1 -> writing data to i/o register,
    // ignore wathever is written here, just init command sequence
    GraphicOne.cmd=0; 
    return 0; 
}

/* GraphicOne register */
// 0x8019      write   0x59,x0,y0,x1,y1    draw vector command

int32 GraphicOne_pia2(int32 io, int32 data)
{
    if (io==0) { 
        // io=0 when reading from io register (return data read from i/o device register)
        return 0; 
    } 
    // io=1 -> writing data to i/o register,
    switch(GraphicOne.cmd) {
        case 0: // no command in exectution. Init new command
            if (data == 89) {
                GraphicOne.cmd=1; // init draw vector command
            }
            break; 
        case 1: // draw vector, get x0 coord
            GraphicOne.x0=data; 
            GraphicOne.cmd++; 
            break; 
        case 2: // draw vector, get y0 coord
            GraphicOne.y0=data; 
            GraphicOne.cmd++; 
            break; 
        case 3: // draw vector, get x1 coord
            GraphicOne.x1=data; 
            GraphicOne.cmd++; 
            break; 
        case 4: // draw vector, get y1 coord, and execute command
            GraphicOne.y1=data; 
            GraphicOne.cmd=0; 
            GraphicOne_vector(); 
            break; 
        default: GraphicOne.cmd=0; 
    }
    return 0; 
}

