/* hp2100_tv.c: HP 2100 91200B TV Interface Kit Simulator

   Copyright (c) 2017-2022, Roberto Sancho

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

   Except as contained in this notice, the name of Robert M Supnik, Roberto 
   Sancho shall not be used in advertising or otherwise to promote the sale, use 
   or other dealings in this Software without prior written authorization 
   from Robert M Supnik or Roberto Sancho.

   TV           91200B TV Interface Card Kit 

   24-Mar-17    RSV     initial version 
   Oct-2022     RSV     updated to match current sim_video 

   Reference:
   - 91200B TV Interface Kit Programming and Operating Manual (91200-90006, Apr-1977)

   TV Interface HW usage

   HP ASM               
   opcode               TV Interface card responds with ...
   ====== ==========================================================================================
                                     TV interface
          Ready         Control      Mode control
          Flag          Flag         Flag               Comment
          ------------- ------------ -------------- -------------------------------------------------
   STF    sets to 1     
   CLF    sets to 0(*)               sets to 0      (*) if TV Control Flag was 1, it is set to 0, 
                                                        and then Ready Flag is set again to 1 when 
														operation execution completes on TV card
   STC    (*)           sets to 1                   (*) also writes in video ram a pixel in x-y coord 
                                                        or executes a requested bulk erase. Then 
                                                        Ready Flag is set again to 1 when operation 
														execution completes on TV card
   CLC                  sets to 0    sets to 1

   OTA    if Mode Control Flag = 1 -> data is stored in TV card as control word (CONWD)

   				bits 0, 2, 3 -> color to use on pixel draw
				bit 1 -> Inverse polarity when set (the TV card outputs to monitor the complementary 
				         color for the whole screen)

						 color table:

						 bits  3 2 1 0    color used for pixel draw
						       -------
							   0 0 X 0    WHITE
							   0 0 X 1    CYAN
							   0 1 X 0    MAGENTA
							   0 1 X 1    BLUE
							   1 0 X 0    YELLOW
                               1 0 X 1    GREEN
							   1 1 X 0    RED
							   1 1 X 1    BLACK

          if Mode Control Flag = 0 -> data is stored in TV card as x-y coord, and request a pixel 
		                              write on next STC

   				bits 0 to  7 -> X coord 0..255
   				bits 8 to 15 -> Y coord 0..255
				Pixel 0,0 at lower left corner of screen 

   LIA    Any input from TV card requests a bulk erase on next STC. Bulk erase will set all pixels 
          to black.
   ====== ==========================================================================================

   These simulator commands controls the TV interface

     SET TV ON      
	 SET TV POWERON     --> power on TV Monitor attached to TV interface card 
	                        (opens GUI Simulator Window for TV image display)
     SET TV OFF     
	 SET TV POWEROFF    --> power off Monitor attached to TV interface 
	                        (closes the TV Simulator GUI Window)

     SET TV AUTOON      --> first CPU instr for TV powers on the TV monitor
                            (AutoPowerOn). This is the default
     SET TV NOAUTOON    --> Must issue a SET TV ON to power the TV

     SET TV BW          --> set display mode to Black and White
     SET TV COLOR4      --> set display mode to 4 colors (Black, Red, Green, Yellow)
     SET TV GRAY4       --> set display mode to 4 level of gray (Black, 1/4, 1/2, 3/4 White)
     SET TV COLOR8      --> set display mode to 8 colors (Black, Blue, Cyan, Red, Green, Magenta, Yellow, White)
     SET TV GRAY8       --> set display mode to 8 level of gray (Black, 1/8 up to 7/8 White)
	  
	 defaults: SET TV DIS, OFF, COLOR8, AUTOON

	 If CPU is stopped, GUI window is not refreshed. During step by step debugging
	 of any HP program that sends commands to this device, nothing will be updated
	 on GUI window. Issue a SET TV ON at any moment to refresh the GUI window
	 and see the results.

	 When HP CPU is executing code, clicking on the close button of GUI windows 
	 closes the emulator. If CPU is stopped, the GUI close button does nothing. 
     To close the GUI window, use the SCP command SET TV OFF

     If Simulator is compiled with CPANEL symbol active, then the following SCP commands 
     are available

     SET TV SCALE=n          --> Sets the GUI window scale. Default is 100 
                                 Use 50 to have a GUI window of half size. 
                                 Use 200 to have GUI window at double size. 
                                 Scale value n ranges from 10 to 200.
                                 If issued before first SET TV ON, sets the 
                                 initial scale to be used on window creation

     SET TV POS=x/y          --> Sets the GUI window position at coord x,y
                                 (can be negative values)
                                 If ussed before first SET TV ON, sets the 
                                 initial scale to be used on window creation

     If Simulator is compiled with CPANEL symbol, then the following features are available:

     1) Hotkeys active when windows has the focus:

        +   -                    Zoom In/Zoom Out: increases/decreases window size by 10%
        Control +, Control -     Zoom In/Zoom Out: increases/decreases window size by 1%
        Control Y                Toggle zoom 100% <-> 50%

     2) Click on window image and drag mouse to move the window
     3) Right click mouse on window image to show a tooltip popup with image at 100% scale

   Usage notes:

	 To use the TV interface, these SCP commands must be issued

		set tv dev=nnn         <-- to set the Select Code for the interface to nnn octal (defaults to 45)
		set tv color8          <-- sets the monitor type. intead of color8, color4, gray8, gray4 or bw can be used
		set tv ena             <-- enables the interface, so HP CPU sees it
		set tv on              <-- makes visible the TV Interfave GUI Window
		...
		set tv off             <-- closes the GUI window

	  When simulated HP CPU is running executing code, clicking on the close button 
      of GUI windows closes the emulator. 
      To just close the device GUI window, use the SCP command SET TV OFF

      Case 1: Simulator compiled with CPANELS in windows platform:
	     If CPU is stopped, GUI window is refreshed even if simulator shows the 
         sim> prompt on console. The Window close button will close the emulator
         program.

      Case 2: Simulator compiled without CPANELS or in non-windows platform (linux, 
              mac, any other):
	     If CPU is stopped, GUI window is not refreshed. During step by step debugging
	     of any HP program that sends commands to this device, nothing will be updated
	     on GUI window. Issue a SET TV ON at any moment to refresh the GUI window
	     and see the results.

         If CPU is stopped, the GUI close button does nothing when clicked. It will
         close the simulation when cpu is started again.
*/

#include "hp2100_defs.h"
#include "sim_video.h"

#if defined(USE_SIM_VIDEO) && defined(HAVE_LIBSDL)
// ok, both preprocessor vars are defined
#else
#error hp2100_tv.c needs the preprocessor symbol USE_SIM_VIDEO and HAVE_LIBSDL to be defined
#endif

#define TV_IDLE               0x00000000
#define TV_BULK_ERASE         0x00010000
#define TV_SET_MODE           0x00020000
#define TV_SET_POINT          0x00040000

#define UNIT_V_POWERON (UNIT_V_UF + 0)                 /* unit powered off */
#define UNIT_POWERON   (1 << UNIT_V_POWERON)

struct {
    FLIP_FLOP control;          // control flip-flop 
    FLIP_FLOP flag;             // flag flip-flop
    FLIP_FLOP flagbuf;          // flag buffer flip-flop 
} tv = { CLEAR, CLEAR, CLEAR};

#define FPS				     30			// update plotter window at this Frames Per Second rate 
#define REFRESH_INTERVAL   1000         // check if refresh needed each these CPU instr 
/* 
Refresh strategy:

- If program makes any changes in videoram or in appearence of screen 
     - draw a pixel
     - change screen color deep/polarity
     - erase the screen
  then set refresh_needed flag=1
- tv_svc() is shedulled to simulate the time needed by TV hw to 
  execute a command, but does not handle any screen redraw/refresh
- refresh_svc() is shedulled with sim_activate() to be executed
  each REFRESH_INTERVAL cpu instructions
- refresh_svc() checks 
     - Screen should be refreshed? (ie. refresh_needed!=0 ?) -> no, exit
     - Has at least 1000/FPS msec elapsed from previous refresh? -> no, exit
     - Clears flag: refresh_needed=0
     - Send the whole SDL2 surface to GUI window for repainting

- if SCP command makes any changes in appearence of screen
     - SET POWERON to open/refresh the GUI window, 
     - SET COLOR to set color deep as B/W, 4 Colors, 8 Colors, 4 Gray Lev, 8 Gray Levels
     then Send the whole SDL2 surface to GUI window for repainting
*/ 

// internal refresh device & unit
// all declared as static so same declaration on all devices is possible (will not have confict names)
static t_stat tv_refresh_svc (UNIT *ptr);
static const char *tv_refresh_description (DEVICE *dptr) {
    return "TV GUI Window SDL2 Refresh facility";
}
static UNIT tv_refresh_unit = { UDATA (&tv_refresh_svc, UNIT_IDLE, 0) };
static DEVICE tv_refresh_dev = {
    "INT-REFRESH-TV", &tv_refresh_unit, NULL, NULL,     // the name INT-REFRESH-xxx
    1, 0, 0, 0, 0, 0,                                   // allows cpanel to call the 
    NULL, NULL, NULL, NULL, NULL, NULL,                 // device refresh routine refresh_svc
    NULL, DEV_NOSAVE, 0,                                // at sim> prompt (if windows os) or
    NULL, NULL, NULL, NULL, NULL, NULL,                 // while SET CPANEL INTERACTIVE
    tv_refresh_description
};

#define H_RESOL             256         // Horizontal resolution (simulated pixels)     
#define V_RESOL             256         // Vertical resolution (simulated pixels)   

#define X_MULT                3         // each simulated horiz pixel takes 3 horiz GUI pixels on window
#define Y_MULT                2         // each simulated vertical pixel takes 2 vertical GUI pixels on window

#define H_MARGIN             50         // horizontal margin on left/right of addressable image (GUI pixels on window)
#define V_MARGIN             30         // vertical margin on tob/bottom of addressable image (GUI pixels on window)

#define H_GRID_SIZE     ((H_RESOL + 15)>>4)              // number of cells of 16x16 pixels in
#define V_GRID_SIZE     ((V_RESOL + 15)>>4)              // VideoRAM array


#define TV_INSTR_TO_DRAW_A_POINT             4          /* time (expressed in CPU intructions executed) to draw a point */
#define TV_INSTR_TO_BULK_ERASE           10000          /* time to bulk erase < 40 ms.  */

#if defined(CPANEL)
#define INITIAL_SCALE                    100            // if using CPANELs, default scale for window creation
#endif

#define TV_COL_BLACK    0								 /* colors are ordered from black to higher luminosity */
#define TV_COL_BLUE     1                                /* first 8 are colors, then 8 gray scale              */
#define TV_COL_GREEN    2								 /* next 16 colors are set same at half bright         */
#define TV_COL_CYAN     3								 /* used for scanlines simulation                      */
#define TV_COL_RED      4
#define TV_COL_MAGENTA  5
#define TV_COL_YELLOW   6
#define TV_COL_WHITE    7

static const char *ColorName[] = {
	"BLACK", "BLUE", "GREEN", "CYAN", "RED", "MAGENTA", "YELLOW", "WHITE", 
    "Black", "Gray 1/8", "Gray 2/8", "Gray 3/8", "Gray 4/8", "Gray 5/8", "Gray 6/8", "Gray 7/8"};

static struct {					/* TV Interface internal state (internal memory, flip-flops and registers) */
    FLIP_FLOP mode_control;		/* not visible to HP2100 program */	
	int8 card_num;				/* indicated number of cards wired together. can be 1,2 or 3 */
    int AutoPowerOn;            /* will power the TV device on first OTA/LIA received from CPU */

    // tv current state
    int pixel_color;	
	int current_polarity;		/* =1 inverse polarity -> use complementary color */
	int8 gray_level_operation;	/* =0 -> simulate color monitor, =1 -> simulate gray scale monitor */
	int8 VideoRAM[256*256];                         // 256x256 array: stores the pixel colors
    uint8 RefreshGrid[H_GRID_SIZE][V_GRID_SIZE];    // 16x16 array: for each 16x16 pixel box in VideoRAM, indicates if has pixels pending to refresh
    // variables for rendering on GUI
    int power;                  // 0=tv powered off=no GUI window, 1=GUI window visible
    uint32 * surface;           // SDL2 GUI surface.  
    VID_DISPLAY *vptr;          // SDL2 window handled
    uint32 color_palette[32];   // TV interface Color palette 
    int wx, wy;                 // width and height of GUI window
    int FullRedrawNeeded;		// =1 to request the full redraw of GUI window on next refresh call (=0 refresh only changed pp rectangle, arm and changes state error light)
    int refresh_needed;         // 1=if video ram has been modified and needs to be refreshed on GUI
    uint32 refresh_tnow;        // value for sim_os_msec() on last GUI refresh
    int quit_requested;         // =1 is user closes the GUI window

    int InitialScale;           // initial scale 10..200 to be used when window is created
    int InitialPosX;            //         x,y postion of window on screen to be used when window is created
    int InitialPosY; 
} tv_state = { CLEAR, 3, 1, 0}; 

DEVICE tv_dev;

IOHANDLER tvio;

t_stat tv_svc (UNIT *uptr);
t_stat tv_reset (DEVICE *dptr);
t_stat tv_set_power_on    (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat tv_set_color       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat tv_set_param       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat tv_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat tv_show	          (FILE *st, UNIT *uptr, int32 num, CONST void *desc);
void tv_refresh(uint32 tnow);
void tv_draw_pixel(int x, int y);

/* TV data structures

   tv_dev      TV device descriptor
   tv_unit     TV unit descriptor
   tv_reg      TV register list
*/

DIB tv_dib = { &tvio, TV };

UNIT tv_unit = {
    UDATA (&tv_svc, UNIT_SEQ+UNIT_DISABLE, 0)
    };

REG tv_reg[] = {
    { ORDATA (BUF, tv_unit.buf, 7) },
    { FLDATA (CTL, tv.control, 0) },
    { FLDATA (FLG, tv.flag,    0) },
    { FLDATA (FBF, tv.flagbuf, 0) },
    { ORDATA (SC, tv_dib.select_code, 6), REG_HRO },
    { ORDATA (DEVNO, tv_dib.select_code, 6), REG_HRO },
    { NULL }
    };

#define DBG_FLAGS  0x00000004 
#define DBG_DATA   0x00000008 
#define DBG_CMDS   0x00000010   
#define DBG_DETAIL 0x00000020   
#define DBG_POINT  0x00000040   

DEBTAB sim_tv_debug[] = {
  {"FLAGS",    DBG_FLAGS,  "Flags status"},
  {"DATA",     DBG_DATA,   "Transmitted Data to TV card"},
  {"CONWD",    DBG_CMDS,   "Control Words with commands sent to TV card"},
  {"POINT",    DBG_POINT,  "Points to write on TV card"},
  {"DETAIL",   DBG_DETAIL, "Operations detail"},
  {0}
};

MTAB tv_mod[] = {
    { UNIT_POWERON, UNIT_POWERON, "monitor on",     "ON",       &tv_set_power_on },
    { UNIT_POWERON, 0,            "monitor off",    "OFF",      &tv_set_power_on },
    { UNIT_POWERON, UNIT_POWERON, NULL,             "POWERON",  &tv_set_power_on},
    { UNIT_POWERON, 0,            NULL,             "POWEROFF", &tv_set_power_on },

    { UNIT_POWERON, 0,            NULL,             "BW",       &tv_set_color },
    { UNIT_POWERON, 1,            NULL,             "COLOR4",   &tv_set_color },
    { UNIT_POWERON, 2,            NULL,             "COLOR8",   &tv_set_color },
    { UNIT_POWERON, 3,            NULL,             "GRAY4",    &tv_set_color },
    { UNIT_POWERON, 4,            NULL,             "GRAY8",    &tv_set_color },

    { UNIT_POWERON, 0,            NULL,             "NOAUTOON", &tv_set_AutoPowerOn },
    { UNIT_POWERON, 1,            NULL,             "AUTOON",   &tv_set_AutoPowerOn },

    { MTAB_XTD | MTAB_VDV,            0, "SC",      "SC",       &hp_setsc,  &hp_showsc,  &tv_dev },
    { MTAB_XTD | MTAB_VDV | MTAB_NMO, 0, "DEVNO",   "DEVNO",    &hp_setdev, &hp_showdev, &tv_dev },

#if defined(CPANEL)
    // important: SCALE should be defined AFTER "SC". If defined before, SET SC= is interpreted as
    // SET SCale (scale being abbreviated on its two first letter), so SC will never be called
    { MTAB_XTD | MTAB_VDV, 0,     NULL,             "POS",      &tv_set_param, NULL, &tv_dev },
    { MTAB_XTD | MTAB_VDV, 1,     NULL,             "SCALE",    &tv_set_param, NULL, &tv_dev }, // 
#endif

    { 0 }
    };

DEVICE tv_dev = {
    "TV", &tv_unit, tv_reg, tv_mod,
    1, 10, 31, 1, 8, 8,	// 1->num of units, 10->addr radix, 31->addr width, 1->addr incr, 8->data radix, 8->data width
    NULL, NULL, &tv_reset,
    NULL, NULL, NULL,
	&tv_dib, DEV_DISABLE | DEV_DIS | DEV_DEBUG, 0, sim_tv_debug
    };


static void tv_quit_callback (VID_DISPLAY * vptr_cp) 
{
    // note: quit callback is executed on sim_video thread, not in main emulator thread
    // should not use sim_debug (is not thread safe) 
    tv_state.quit_requested=1; 
}

uint32 tv_rgb_color(uint32 r, uint32 g, uint32 b) { /* r,g,b are 16bit! */
	uint32 color;
    color = sim_end ? 
		       (0xFF000000 | ((r & 0xFF00) << 8) | (g & 0xFF00) | ((b & 0xFF00) >> 8)) : 
	           (0x000000FF | (r  & 0xFF00) | ((g & 0xFF00) << 8) | ((b & 0xFF00) << 16));
    return color;
}

uint32 tv_gray(uint32 lvl) { /* gray level set fron 0 to 7 */
	lvl = ((256 / 8) * lvl) << 8; 
    return tv_rgb_color(lvl, lvl, lvl);
}


void tv_set_color_palette(void) {

	int i;

    tv_state.color_palette[TV_COL_WHITE]     = 0xFFFFFFFF;                              /* White   */
    tv_state.color_palette[TV_COL_BLACK]     = sim_end ? 0xFF000000 : 0x000000FF;       /* Black   */
    tv_state.color_palette[TV_COL_RED]       = tv_rgb_color(0xFF00,0,0);                /* Red     */
    tv_state.color_palette[TV_COL_GREEN]     = tv_rgb_color(0,0xFF00,0);                /* Green   */
    tv_state.color_palette[TV_COL_BLUE]      = tv_rgb_color(0,0,0xFF00);                /* Blue    */
    tv_state.color_palette[TV_COL_YELLOW]    = tv_rgb_color(0xFF00,0xFF00,0);           /* Yellow  = red+green  */
    tv_state.color_palette[TV_COL_MAGENTA]   = tv_rgb_color(0xFF00,0,0xFF00);           /* Magenta = red+blue   */
    tv_state.color_palette[TV_COL_CYAN]      = tv_rgb_color(0,0xFF00,0xFF00);           /* Gyan    = green+blue */

    tv_state.color_palette[8+TV_COL_BLACK]   = tv_gray(0);
    tv_state.color_palette[8+TV_COL_BLUE]    = tv_gray(1);
    tv_state.color_palette[8+TV_COL_GREEN]   = tv_gray(2);
    tv_state.color_palette[8+TV_COL_CYAN]    = tv_gray(3);
    tv_state.color_palette[8+TV_COL_RED]     = tv_gray(4);
    tv_state.color_palette[8+TV_COL_MAGENTA] = tv_gray(5);
    tv_state.color_palette[8+TV_COL_YELLOW]  = tv_gray(6);
	tv_state.color_palette[8+TV_COL_WHITE]   = tv_gray(7);

	/* generate palette with half right bright to simulate scan lines */
	for (i=16;i<32;i++) 
		tv_state.color_palette[i] = tv_state.color_palette[i-16] & 
			(sim_end ? 0xFF7F7F7F : 0x7F7F7FFF);
}


t_stat tv_init(void) 
{
    t_stat stat;
    int i; 
    static int first_poweron_done=0; 

    // populate wc, wy and allocate mem for surface
    tv_state.wx=H_RESOL * X_MULT + 2 * H_MARGIN; 
    tv_state.wy=V_RESOL * Y_MULT + 2 * V_MARGIN; 
    tv_state.surface = (uint32 *)malloc (tv_state.wx * tv_state.wy * sizeof(uint32));
    if (tv_state.surface == NULL) return SCPE_IERR;

    tv_set_color_palette();
    // open the GUI window ... not so simple. Depends on what we want
    // 1) The simplest: simulators with only one window open at any moment: 
    //       stat = vid_open (&tv_dev, "HP91200B TV Interface", wx, wy, 0);
    //       ... (later) ... vid_close();
    // 2) simulators that allows one or more windows open at same time (cpanel+tv, tv+plotter, cpanel+tv+plotter)
    //    but are compiled using github unmodified sim_video (i.e. without cpanels)
    //       stat = vid_open_window (&vptr, &tv_dev, "HP91200B TV Interface", wx, wy, 0);
    //       ... (later) ... vid_close_window(vptr);
    // 3) Same as 2) using sim_video compiled with CPANEL symbol active: we benefit from cpanel's 
    //    built in zoom/unzoom hotkeys (+/-, ^Y), tooltip, window drag (by click at any point in 
    //    windows client area), ans initial scale and postion settings
    //       stat = vid_open_window_ex (&vptr, &tv_dev, "HP91200B TV Interface", wx, wy, flags, 
    //                                  InitialScale, InitialPosX, InitialPosY);
    //       ... (later) ... vid_close_window(vptr);
#if defined(CPANEL)
    if (tv_state.InitialScale == 0) tv_state.InitialScale=INITIAL_SCALE; 
    if ((tv_state.InitialPosX == 0) && (tv_state.InitialPosY == 0)) {
        tv_state.InitialPosX=100; tv_state.InitialPosY=80; 
    }
    stat = vid_open_window_ex (&tv_state.vptr, &tv_dev, "HP91200B TV Interface", 
                     tv_state.wx, tv_state.wy, 
                     SIM_VID_FLAG_SCALE_PLUSMINUS | SIM_VID_FLAG_ALLOW_TOOLTIP  | SIM_VID_FLAG_ALLOW_DRAG,
                     tv_state.InitialScale, tv_state.InitialPosX, tv_state.InitialPosY);
#else
    stat = vid_open_window (&tv_state.vptr, &tv_dev, "HP91200B TV Interface", 
                            tv_state.wx, tv_state.wy, 0);
#endif
    if (stat != SCPE_OK) return stat; 

    memset(tv_state.RefreshGrid, 0, sizeof(tv_state.RefreshGrid)); 
    if (first_poweron_done==0) {
        first_poweron_done=1; 
        for (i=0; i<256*256; i++) tv_state.VideoRAM[i] = TV_COL_BLACK;	/* fill video ram with black color */
    }
    tv_state.FullRedrawNeeded = 1; // because tv init issued
    tv_state.refresh_tnow=0;
    return SCPE_OK;
}

void tv_done(void) 
{
	sim_os_ms_sleep (200); // this sleep is to asure SDL thread has processed any pending EVENT_REDRAW
    vid_close_window(tv_state.vptr);
    tv_state.vptr=NULL; 
    if (tv_state.surface) free(tv_state.surface); 
    tv_state.surface=NULL;
}

// Creates and destroy the GUI window 
// power = 0 -> power off (closes GUI Window), 
//       = 1 -> power on (creates and shows GUI Window), 
t_stat power_tv_monitor(int power)
{
    t_stat stat;

	if (power) {
        // power on tv/monitor
        if (tv_state.power == 1) {
            tv_refresh(0);   // force refresh: power on also refreshes the screen 
            return SCPE_OK;  // already powered on
        }
		// power on monitor -> create GUI window */
	    sim_debug (DBG_DETAIL, &tv_dev, "TV Init\n");
        // create GUI window
		stat=tv_init();
        if (stat) return stat; 
        // init done
        tv_state.power=1; 
        // set quit callback. Register it linked to close button of last created window
        tv_state.quit_requested=0; 
        vid_register_quit_callback (&tv_quit_callback);
        // init local vars
        // clear screen 
        memset(tv_state.surface, 0, tv_state.wx * tv_state.wy * sizeof(uint32));
        // force refresh
  	    tv_state.FullRedrawNeeded = 1; // first refresh after power on 
        tv_refresh (0);  // first refresh after power on 
        // start refresh_svc
        sim_register_internal_device (&tv_refresh_dev);
        sim_activate(&tv_refresh_unit, REFRESH_INTERVAL);
	} else if (power == 0) {
		/* power off monitor -> close GUI window */
		if (tv_state.power == 0) return SCPE_OK; /* monitor already off -> nothing to do*/
        // stop refresh_svc
        sim_cancel(&tv_refresh_unit);
        sim_cancel(&tv_unit);						
        // close GUI window
        tv_done();
        // power off done
        tv_state.power=0; 
	    sim_debug (DBG_DETAIL, &tv_dev, "TV Done\n");
    }
    return SCPE_OK; 
}

// power the TV if AutoPowerOn flag set and power is off
void tv_CheckAutoPowerOn(void)
{
    if (tv_state.AutoPowerOn == 0) return; 
    if (tv_state.power) return; 
    power_tv_monitor(1); 
}

/* refresh: draw simulated video mem on visible windows, and redraw on GUI */
#if defined(CPANEL)
rectlist tv_RectList; // rectangle list structure

static void SetRectList_using_RefreshGrid(void)
{
    typedef struct {           // grid to refresh, V cells vertical and H cells horizontal
        int V,H; 
    } Grid; 
    static Grid grid[H_GRID_SIZE][V_GRID_SIZE];
    int x,y,xx,yy,n,v,ww,hh; 

    // populate RectList using RefreshGrid info
    // let's now identify vertical rectagles in RefreshGrid. 
    memset(grid, 0, sizeof(grid)); 
    // scan vertical columns in RefreshGrid. 
    // Set grid[x,y].V cell value= number of contiguous vertival cells to refresh 
    for (x=0; x<H_GRID_SIZE; x++) for (y=0; y<V_GRID_SIZE; y++) {   // for each vertical column 
        if (tv_state.RefreshGrid[x][y]==0) continue; 
        n=0; // cell has pixels, calc n=height of sco_grid column
        for (yy=y+1;;yy++) { 
            if (yy>=V_GRID_SIZE) break; 
            if (tv_state.RefreshGrid[x][yy]==0) break; 
            n++; 
        } 
        grid[x][y].V=1+n; 
        y+=n; 
    }
    memset(tv_state.RefreshGrid, 0, sizeof(tv_state.RefreshGrid)); 
    // scan horizontal lines in grid. 
    // Set grid[x,y].H cell value= number of contiguous horizontal cells to refresh 
    for (y=0; y<V_GRID_SIZE; y++) for (x=0; x<H_GRID_SIZE; x++) { // for each line 
        if ((v=grid[x][y].V) == 0) continue; 
        n=0;  // calc n=width of rectangle
        for (xx=x+1;;xx++) { 
            if (xx>=H_GRID_SIZE) break; 
            if (grid[xx][y].V!=v) break; 
            grid[xx][y].V=0;
            n++; 
         } 
         grid[x][y].H=1+n; 
         x+=n; 
    }
    // grid array built. Now set up RectList
    tv_RectList.Count=0; 
    for (x=0; x<H_GRID_SIZE; x++) for (y=0; y<V_GRID_SIZE; y++) {   
        if (grid[x][y].H==0) continue; 
        // add entry to rectlist
        n=tv_RectList.Count; 
        if (n>=RECTLIST_MAX) {
            // set rect table as full
            tv_RectList.Count=-1; 
            return; 
        }
        // room left on rectable table. can add entry
        // calculate surface coordinates
        xx=H_MARGIN + (x<<4)*X_MULT; 
        yy=V_MARGIN + (y<<4)*Y_MULT; 
        ww=((grid[x][y].H)<<4)*X_MULT; 
        hh=((grid[x][y].V)<<4)*Y_MULT; 
        if (xx >= tv_state.wx) continue; // safety
        if (yy >= tv_state.wy) continue; // safety
        if (xx+ww > tv_state.wx) ww = tv_state.wx-xx; 
        if (yy+hh > tv_state.wy) hh = tv_state.wy-yy; 
        // set up rectlist entry
        tv_RectList.flags[n]=0;
        tv_RectList.x[n]=xx; tv_RectList.w[n]=ww; 
        tv_RectList.y[n]=yy; tv_RectList.h[n]=hh; 
        tv_RectList.Count++;
    }
}
#endif

void tv_refresh(uint32 tnow) {

	int x,y;
    int FullRedrawDone = 0; 

    if (tv_state.power == 0) return; 

	if 	(tv_state.FullRedrawNeeded) {
		tv_state.FullRedrawNeeded = 0;
        tv_state.refresh_needed = 1; 
        // clear grid
        memset(tv_state.RefreshGrid, 0, sizeof(tv_state.RefreshGrid)); 
		// generate surface according to VideoRAM 
        for (y=0;y<V_RESOL;y++) for (x=0;x<H_RESOL;x++) {
            tv_draw_pixel(x,y); 
        }
        FullRedrawDone=1; 
    }

    // now we send the surface to be updated in GUI screen
    // several ways to do, depending on what we want
    // 1) SimH is compiled without CPANELS: 
    //       we send rectangle(s) to redraw with vid_draw_window, then the update on screen
    //       is done by vid_refresh_window. Each call to vid_draw_window malloc the surface
    //       rectangle and copies it, can be slow on big rectangles. 
    // 2) SimH is compiled with CPANELS and we update the screen at once
    //       use vid_refresh_ex(tv_state.vptr, tv_state.surface, NULL); No memory copy, just send 
    //       a pointer to surface. 
    // 3) SimH is compiled with CPANELS and we update rectangle list
    //       use vid_refresh_ex(vptr, surface, RectList); to send a list
    //       of changed image rectangles to be redraw. 
	//update GUI window
#if defined(CPANEL)
    if (FullRedrawDone) {
        tv_RectList.Count=-1; // update full surface to GUI window
    } else {
        SetRectList_using_RefreshGrid(); // update only rectlist surface 
    }
    if (tv_RectList.Count!=0) {
        vid_refresh_ex(tv_state.vptr, tv_state.surface, &tv_RectList); 
    }
#else
    vid_draw_window (tv_state.vptr, 0, 0, tv_state.wx, tv_state.wy, tv_state.surface);
    vid_refresh_window (tv_state.vptr);
#endif
    tv_state.refresh_needed=0;
    if (tnow==0) tnow = sim_os_msec(); 
    tv_state.refresh_tnow = tnow; 

	sim_debug (DBG_DETAIL, &tv_dev, "Refresh done.\n");
}

void tv_refresh_if_needed(void)
{
    uint32 tnow; 
    uint32 msec; 

    if (tv_state.FullRedrawNeeded) tv_state.refresh_needed=1; 
    // check if 20 msec has elapsed from last refresh (20 msec -> 50 FPS)
    tnow = sim_os_msec(); 
    msec = tnow - tv_state.refresh_tnow; 
    if (msec > (1000/FPS)) {                                   // enought time has pased from previous refresh
        if (tv_state.refresh_needed) {                         // and there is something to draw on gui
            #if defined(CPANEL)
            if (vid_refresh_in_progress(tv_state.vptr) == 0) { // and no previous refresh in progress
                tv_refresh(tnow);                              // go ahead and redraw surface
            }
            #else
            tv_refresh(tnow);                              
            #endif
        }
    }
}

/* Simulate TV card graphics circuitery drawing on VideoRAM */

void tv_bulk_erase (void) 
{
    int i; 
    for (i=0; i<256*256; i++) tv_state.VideoRAM[i] = TV_COL_BLACK;	/* fill  video ram with black color */
    tv_state.FullRedrawNeeded=1; // because a a bulk erase issued
}

// draw a pixel from VideoRAM to surface  
void tv_draw_pixel(int x, int y)
{
    int color, c, Xn, Yn, ps; 

    color = tv_state.VideoRAM[x + y*H_RESOL];
	if (tv_state.current_polarity) color = 7-color;			/* invert polarity -> use complementary color */
	if (tv_state.card_num == 2) color = color & 0x0E;		/* two cards, color converted to black, red, blue, yellow */
	else if (tv_state.card_num == 1) color = color ? TV_COL_WHITE : TV_COL_BLACK;   /* one cards, color converted black, rest of colors white */
	if (tv_state.gray_level_operation) color = color + 8;	/* use gray level pallete */
    x = x * X_MULT + H_MARGIN; // convert x,y from videoram coords to surface coords
    y = y * Y_MULT + V_MARGIN; 
    for (Yn=0;Yn<Y_MULT;Yn++) {
    	c = color;
		if (Yn==0) c = color + 16;	// will draw a scanline -> use scanline pallete 
        ps = x + tv_state.wx * (y + Yn); 
	    for (Xn=0;Xn<X_MULT;Xn++) tv_state.surface[ps++] = tv_state.color_palette[c];
    }
}

// draw a color pixel into VideoRAM at coords given by HP2100 CPU (0..255, 0..255) 
void tv_set_point(int x_HP, int y_HP, int color)
{
    int x,y; 

    // convert CPU coordinates 0..255 to surface/VideoRAM coordinates
    // HP coords have (0,0) at bottom left corner
    x_HP &= 255; y_HP &= 255; 
    x = x_HP & 255;            // coordinates in video Ram. (0,0) is in top left corner
    y = 255 - (y_HP & 255); 

    // update video ram and surface
    tv_state.VideoRAM[x + y*H_RESOL] = color;
    tv_draw_pixel(x,y); 

    // pixel modified in VideoRAM, so must be refreshed in GUI
    tv_state.RefreshGrid[x >> 4][y >> 4]=1; 
    tv_state.refresh_needed=1; // because a point is set
}

void tv_draw (UNIT *uptr) {

   int32 buf, cmd, X, Y;
   uint32 color, polarity;

   buf = uptr->buf & 0x0000FFFF;
   cmd = uptr->buf & 0xFFFF0000;

   uptr->buf = buf;								/* remove command from buf */

   if (cmd == TV_IDLE) return;					/* no command -> exit */

   if (cmd & TV_BULK_ERASE) {
	  sim_debug (DBG_DETAIL, &tv_dev, "Bulk Erase done\n");      
      tv_bulk_erase(); 
   }

   if (cmd & TV_SET_MODE) {						/* TV interface decoding for MODE word  */
	   color = buf & 0x0D;	 					/*		bits 0, 2, 3 -> color to use on pixel draw */
	   polarity = (buf & 2) > 1;				/*		bit 1 -> Inverse polarity when set */
	   switch (color) {
		   case 0x00: color = TV_COL_WHITE;   break;
		   case 0x0D: color = TV_COL_BLACK;   break;
		   case 0x0C: color = TV_COL_RED;     break;
		   case 0x09: color = TV_COL_GREEN;   break;
		   case 0x05: color = TV_COL_BLUE;    break;
		   case 0x08: color = TV_COL_YELLOW;  break;
		   case 0x04: color = TV_COL_MAGENTA; break;
		   case 0x01: color = TV_COL_CYAN;    break;
	   }
	   tv_state.pixel_color = color;
	   sim_debug (DBG_DETAIL, &tv_dev, "Set Pixel Color to %s, Inverse Polarity %d\n",ColorName[color], polarity);
	   /* check polarity */
	   if ( tv_state.current_polarity != polarity) {   
	       sim_debug (DBG_DETAIL, &tv_dev, "Polarity Changed! from %d to %d\n", tv_state.current_polarity, polarity);
		   tv_state.current_polarity = polarity;
           tv_state.FullRedrawNeeded=1;  // because a polarity change */
	   }
   } 

   if (cmd & TV_SET_POINT) {
	   X = buf & 0xFF;
   	   Y = (buf >> 8); 
	   color = tv_state.pixel_color;
 	   sim_debug (DBG_DETAIL, &tv_dev, "Write Pixel at HP coords X=%d, Y=%d, Color %s\n",X,Y,ColorName[color]);
       // draw pixel in surface
       tv_set_point(X, Y, color); 
   }
}

static t_stat tv_refresh_svc (UNIT *uptr)
{
   if (tv_state.quit_requested) return SCPE_EXIT; 
   sim_activate(&tv_refresh_unit, REFRESH_INTERVAL);
   tv_refresh_if_needed(); 
   return SCPE_OK;
}

/* Unit service */

t_stat tv_svc (UNIT *uptr)
{
   if (tv_state.quit_requested) return SCPE_EXIT; 
   // it's an operation completition event -> set the flag to tell it to HP program */
   sim_debug (DBG_FLAGS, &tv_dev, "Operation done - Set Flag\n");
   tvio (&tv_dib, ioENF, 0);             /* set flag */
   return SCPE_OK;
}

/* I/O signal handler */

uint32 tvio (DIB *dibptr, IOCYCLE signal_set, uint32 stat_data)
{
IOSIGNAL signal;
IOCYCLE  working_set = IOADDSIR (signal_set);           /* add ioSIR if needed */
int32 duration;

while (working_set) {
    signal = IONEXT (working_set);                      /* isolate next signal */

    switch (signal) {                                   /* dispatch I/O signal */

        case ioCLF:                                     /* clear flag flip-flop */
            sim_debug (DBG_FLAGS, &tv_dev, "ioCLF\n");
            tv.flag = tv.flagbuf = CLEAR;
			if (tv_state.mode_control == SET) {
				tv_state.mode_control = CLEAR;
                if (sim_is_active(&tv_unit)==0) {
  				    sim_activate (&tv_unit, TV_INSTR_TO_DRAW_A_POINT);          
                }
			}
            break;


        case ioSTF:                                     /* set flag flip-flop */
		case ioENF:                                     /* enable flag */
			if (signal == ioSTF) {
				sim_debug (DBG_FLAGS, &tv_dev, "ioSTF\n");
			} else {
				sim_debug (DBG_FLAGS, &tv_dev, "ioENF\n");
			}
            tv.flag = tv.flagbuf = SET;                 // set "Ready flag"
            break; 


        case ioSFC:                                     /* skip if flag is clear */
            setstdSKF (tv);
            break;


        case ioSFS:                                     /* skip if flag is set */
            setstdSKF (tv);
            break;


        case ioIOI:                                     /* I/O data input from TV interface */
            /* On data input, a Bulk erase is initiated */
            tv_CheckAutoPowerOn(); 
			sim_debug (DBG_CMDS, &tv_dev, "Bulk erase requested\n");
            tv_unit.buf |= TV_BULK_ERASE;				/* clear buf, set BULK erase comand to be executed */
            stat_data = IORETURN (SCPE_OK, 0);          /* merge in return status */  
            break; 


        case ioIOO:										/* I/O data output to TV interface */
            tv_CheckAutoPowerOn(); 
            tv_unit.buf &= ~0xFFFF;  
			tv_unit.buf |= IODATA (stat_data) & 0xFFFF;	
			sim_debug (DBG_DATA, &tv_dev, "Output data $%04x to TV, Mode control flag is %d\n", tv_unit.buf, tv_state.mode_control);
            if (tv_state.mode_control == SET) {
            	tv_unit.buf |= TV_SET_MODE;  /* set mode = set color to use on pixel draw*/
   			    sim_debug (DBG_CMDS, &tv_dev, "Sent control word $%04x\n",tv_unit.buf & 0xFFFF );
			    tv_draw(&tv_unit);						  /* Decodes Control Word */
            } else {
            	tv_unit.buf |= TV_SET_POINT; 
   			    sim_debug (DBG_POINT, &tv_dev, "Set POINT coord $%04x\n",tv_unit.buf & 0xFFFF );
            }
            break;


        case ioPOPIO:                                   /* power-on preset to I/O */
            tv.flag = tv.flagbuf = SET;                 /* set flag and flag buffer */
            tv_unit.buf = 0;                            /* clear output buffer */
			tv_state.mode_control = SET;				
			tv_state.current_polarity = 0;
			tv_state.pixel_color = 0;
            break;

        case ioCRS:                                     /* control reset */
        case ioCLC:                                     /* clear control flip-flop */
			sim_debug (DBG_FLAGS, &tv_dev, "ioCLC - Set Mode control\n");
            tv.control = CLEAR;
			tv_state.mode_control = SET;
            break;


        case ioSTC:                                     /* set control flip-flop */
            tv_CheckAutoPowerOn(); 
			sim_debug (DBG_FLAGS, &tv_dev, "ioSTC Set control Flip Flop (exec operation)\n");
            tv.control = SET;
			if (tv_unit.buf & TV_BULK_ERASE) { 
				duration = TV_INSTR_TO_BULK_ERASE; 
			} else { 
				duration = TV_INSTR_TO_DRAW_A_POINT; 
			}
			sim_cancel(&tv_unit);						/* cancel pending events on queue */
            sim_activate (&tv_unit, duration);          /* schedule op to set the flag marking operation completition */
			tv_draw(&tv_unit);							/* draw on screen */
            break;


        case ioSIR:                                     /* set interrupt request */
            setstdPRL (tv);                             /* set standard PRL signal */
            setstdIRQ (tv);                             /* set standard IRQ signal */
            setstdSRQ (tv);                             /* set standard SRQ signal */
            break;


        case ioIAK:                                     /* interrupt acknowledge */
            tv.flagbuf = CLEAR;
            break;


        default:                                        /* all other signals */
            break;                                      /*   are ignored */
        }

    working_set = working_set & ~signal;                /* remove current signal from set */
    }

return stat_data;
}

// SCP functions

t_stat tv_reset (DEVICE *dptr)
{
    IOPRESET (&tv_dib);                                    /* PRESET device (does not use PON) */
    tv_state.mode_control = SET;
	tv_state.pixel_color = 0; 
	tv_state.current_polarity = 0; 
	sim_cancel (&tv_unit);                                 /* deactivate unit */
    // boot & run SCP commands issues an implicit reset on SCP. In this reset first cancels
    // any schedulled device event and then calls each device reset routine (this routine)
    // se here we must re-schedulle the refresh event
    if (tv_state.power) {
        sim_cancel(&tv_refresh_unit); 
        sim_activate(&tv_refresh_unit, REFRESH_INTERVAL);
    }
    return SCPE_OK;
} 

t_stat tv_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{	
	return power_tv_monitor(value ? 1:0);;
}

t_stat tv_set_color  (UNIT *uptr, int32 value, CONST char *cptr, void *desc) {

	// color value:	0			1			2			3			4
	//				B/W			4 Colors	8 Colors	4 Gray Lev	8 Gray Levels
	// card_num:	1			2			3			2			3
	// gray_level:	0			0			0			1			1

	tv_state.gray_level_operation = (value > 2 ) ? 1 : 0;
	if (value == 0) { tv_state.card_num = 1; }
	else if ((value == 1) || (value == 3)) { tv_state.card_num = 2; }
	else tv_state.card_num = 3; 
    tv_state.FullRedrawNeeded=1; 
	tv_refresh(0);  // because SET COLOR 
	return SCPE_OK; 
}

#if defined(CPANEL)
t_stat tv_set_param (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    DEVICE *dptr = (DEVICE *) desc;
    int32 num, xPos,yPos, Scale;
    CONST char *tptr;
    t_stat r;

    if (cptr == NULL) return SCPE_ARG;
    if (desc == NULL) return SCPE_IERR;

    if (value == 0) { // SET POS=X/Y 
        xPos = (int) strtotsv (cptr, &tptr, 0);
        if (cptr == tptr) return SCPE_ARG;
        if (*tptr != '/') return SCPE_ARG;
        yPos = (int) strtotsv (++tptr, &tptr, 0);
        if (cptr == tptr) return SCPE_ARG;
        if (tv_state.power == 0) {
            tv_state.InitialPosX=xPos; 
            tv_state.InitialPosY=yPos; 
        } else {
            vid_SetWindowSizeAndPos(tv_state.vptr, SIM_SETWIN_POS, xPos,yPos);
        }
        return SCPE_OK; // no panel opened
    } else if (value == 1) { // SET SCALE=10..200 
        num = (int32) get_uint (cptr, 10, 200, &r);
        if (r != SCPE_OK) return r;
        // resize window to set the new scale
        if (num <  10) {Scale= 10; } // max scale range allowed: 10%..200%
        else if (num > 200) {Scale=200; }
        else Scale=num; 
        if (tv_state.power == 0) {
            tv_state.InitialScale=Scale; 
        } else {
            vid_SetWindowSizeAndPos(tv_state.vptr, SIM_SETWIN_SCALE, Scale, 0); // resize GUI window
        }
   	    return SCPE_OK;
    }
	return SCPE_OK;
}
#endif

t_stat tv_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    tv_state.AutoPowerOn=value; 
	return SCPE_OK;
}

t_stat tv_show (FILE *st, UNIT *uptr, int32 num, CONST void *desc) {

	if      ((tv_state.card_num == 2) && (tv_state.gray_level_operation == 0)) {fprintf (st, "4 Colors, "); }
	else if ((tv_state.card_num == 3) && (tv_state.gray_level_operation == 0)) {fprintf (st, "8 Colors, "); }
	else if ((tv_state.card_num == 2) && (tv_state.gray_level_operation == 1)) {fprintf (st, "4 Grey Levels, "); }
	else if ((tv_state.card_num == 3) && (tv_state.gray_level_operation == 1)) {fprintf (st, "8 Grey Levels, "); }
	else {fprintf (st, "B/W, "); }

	return SCPE_OK;
}


