/* hp2100_plt.c: HP7210A plotter simulator

   Copyright (c) 2017-2022, Roberto Sancho. 

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
   THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the names of the authors shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from the authors.

   plt			HP7210A Flatbed Plotter attached to 17210A Interface Kit

   04-May-17    RSV     initial version
   Oct-2022     RSV     updated to match current sim_video. Added ^F and ^P hotkeys
                        added SET PLOTTER SAVE=<filename>. Optimized refresh to reduce 
                        cpu usage. Added plotter arm shadow. Some minor bug fixes

   References:
   - 7210A Technical Data (5952-2744, Oct-1972)
   - 7210A Interface Manual (07210-90002, Jul-1972)
   - 7210A Operators Manual (07210-90000, Mar-1972)

   Plotter simulated

      HP7210A: 
         4 color pen interchangeable (black, red, blue, green)
		 15x10 inch plot surface
   
   Plotter Interface HW usage
   
      OTA   Output a word to plotter buffer

      STC   Makes the plotter to process the word in buffer. 

            Commands are multiword. Each word transnission is calles a pass. Depending on
			command, 1 or 4 words (passes) are needed.
			If plotter is not ready, the STC command is ignored.

		pass	word meaning
			
		1	S000 0CMP hhhh hhhh		S=SYC bit. if 1 marks the beggining of a command 
			                        M=MVR (Maneuver). 0=Position comand, 1=Pen maneuver command
									P=PNC (Pen Command). 0=Pen Up, 1=Pen Down
	                                C=CDE (Code). 0=Position coordinates enconded in BDC, 1=encoded in Binary

								              if bit M=1, no more passes are required. A new command with 
									          bit S set is expected by plotter. 

									          if bit M=0, 3 more words are required for a total of
											  four passes

		                            hhhh hhhh=if bit M=0, this is high part of X coordinate
									          if bit C=0, it is encoded in BDC (two packed 4 bit digits)


		2	0000 0000 llll llll		llll llll=if bit M=0, this is low part of X coordinate
									          if bit C=0, encoded in BDC (two packed 4 bit digits)
			
		3	0000 0000 HHHH HHHH		HHHH HHHH=if bit M=0, this is high part of Y coordinate
									          if bit C=0, encoded in BDC (two packed 4 bit digits)

		4	00yx m000 LLLL LLLL		m=MDE (Mode). 0=Coordinates are Absolute, 1=are Relative to 
		                                          current pen position
                                    y=DXS (Delta X sign). 0=Positive, 1=Negative. 
                                    x=DYS (Delta Y sign). 0=Positive, 1=Negative. 
									
									          Only aplies if relative and BDC is used.
											  If binary enconding is used for relative coordinates, 
											  negative values are encoded in 2's complement
		
		                            LLLL LLLL=if bit M=0, this is low part of Y coordinate
									          if bit C=0, encoded in BDC (two packed 4 bit digits)

   How Plotter paper is simulated:

   HP Plotter has a 10.000 x 10.000 points resolution. As this resolution is too high and is squared, 
   it is simulated as:
	   4 HP_dots in horizontal axis as 1 dot in simulated paper sheet (pp array)
	   6 HP_dots in vertical axis as1 dot in simulated paper sheet (pp array)
   
   This means that we have a paper resolution of 10K/4 x 10K/6 = 2500 x 1667. This is the resolution
   of image saved in PNG image file. This resolution also match the aspect ratio of plotter's
   drawing surface (15x10 inch -> 1.5 aspect ratio. 2500x1667 is also 1.5 aspect ratio)

   GUI window has some little extra size for margins and background, arm, head, control panel light.

   These simulator commands controls the plotter

      SET PLOTTER ON 
	  SET PLOTTER POWERON   --> power on plotter  attached to interface 
							  	card (opens GUI Simulator Window 
								for plotter display). If the window
								is already open, refreshes its contents 
								to reflect any changes/pending GUI update               
      SET PLOTTER OFF        
	  SET PLOTTER POWEROFF  --> power off plotter attached to interface 
								card (closes the scope simulator GUI Window)
   
      SET PLOTTER AUTOON    --> first CPU instr for plotter powers on the device
                                (AutoPowerOn). This is the default
      SET PLOTTER NOAUTOON  --> Must issue a SET PLOTTER ON to power the device

	  SET PLOTTER BLANKSHEET --> Discard the current plotted image
	  SET PLOTTER NEWSHEET       Start a new blank image sheet

	  SET PLOTTER BLACKPEN      sets the plotter's pen color to black pen
	  SET PLOTTER BLUEPEN                                       blue 
	  SET PLOTTER REDPEN                                        red 
	  SET PLOTTER GREENPEN                                      green 

	  SET PLOTTER PENSIZE=n --> sets the plotter's pen width
								1 up to 6 = sizes of antialiased pen 
								    drawing on simulated paper (pen 
									size 6 is 30 paper dots wide)

      SET PLOTTER FAST=n    --> sets the speed operation of plotter
                                0 = Match real world HW speed
                                1 = As fast as possible. 
                                2 = Like above, but do not render 
	                                plotter's arm and head


     If Simulator is compiled with CPANEL symbol active, then the following SCP commands 
     are available

     SET PLOTTER SAVE=<filename>          --> Saves the plotted image on given filname 
	                                          Format: PNG, size: 2820x1907 pixels

     SET PLOTTER SCALE=n     --> Sets the GUI window scale. Default is 50
                                 Use 50 to have a GUI window of half size. 
                                 Use 200 to have GUI window at double size. 
                                 Scale value n ranges from 10 to 200.
                                 If issued before first SET PLOTTER ON, sets the 
                                 initial scale to be used on window creation

     SET PLOTTER POS=x/y     --> Sets the GUI window position at coord x,y
                                 (can be negative values)
                                 If used before first SET PLOTTER ON, sets 
                                 the initial scale to be used on window creation

     If Simulator is compiled with CPANEL symbol, then the following features are available:

     1) Hotkeys active when windows has the focus:

        +   -                    Zoom In/Zoom Out: increases/decreases window size by 10%
        Control +, Control -     Zoom In/Zoom Out: increases/decreases window size by 1%
        Control Y                Toggle zoom 100% <-> 50%
        Control F                While pressed down, accelerates plotter drawing to max speed
        Control P                Saves in current directory the plotter image into 
                                 file "Plotter_Image_NN.png"

     2) Click on window image and drag mouse to move the window
     3) Right click mouse on window image to show a tooltip popup with image at 100% scale

	  defaults: SET PLOTTER DIS, OFF, BLUEPEN, PENSIZE=2, AUTOON

	  there is a little unrealistic but handly hack that simulates paper sheet change
	  that can be issued from program. To do so, just
			     Put arm at position 1,2
				 move arm to relative position -3,-4
	  in real HW, the plotter arm does not moves, and error light goes on
	  as destination coords are out of range. On SimH plotter, the paper sheet
	  is cleared.

	  When simulated HP CPU is running executing code, clicking on the close button 
      of GUI windows closes the emulator. 
      To just close the device GUI window, use the SCP command SET PLOTTER OFF

      Case 1: Simulator compiled with CPANELS in windows platform:
	     If CPU is stopped, GUI window is refreshed even if simulator shows the 
         sim> prompt on console. The Window close button will close the emulator
         program.

      Case 2: Simulator compiled without CPANELS or in non-windows platform (linux, 
              mac, any other):
	     If CPU is stopped, GUI window is not refreshed. During step by step debugging
	     of any HP program that sends commands to this device, nothing will be updated
	     on GUI window. Issue a SET PLOTTER ON at any moment to refresh the GUI window
	     and see the results.

         If CPU is stopped, the GUI close button does nothing when clicked. It will
         close the simulation when cpu is started again.
*/

#include <math.h>
#include "hp2100_defs.h"
#include "sim_video.h" 

#if defined(USE_SIM_VIDEO) && defined(HAVE_LIBSDL)
// ok, both preprocessor vars are defined
#else
#error hp2100_plt.c needs the preprocessor symbol USE_SIM_VIDEO and HAVE_LIBSDL to be defined
#endif

#define UNIT_V_POWERON  (UNIT_V_UF + 0)                 /* unit powered on */
#define UNIT_POWERON    (1 << UNIT_V_POWERON)

struct {
    FLIP_FLOP control;                                  /* control flip-flop */
    FLIP_FLOP flag;                                     /* flag flip-flop */
    FLIP_FLOP flagbuf;                                  /* flag buffer flip-flop */
    } plt = { CLEAR, CLEAR, CLEAR };

#define FPS						 30			// update GUI window at this Frames Per Second rate 
#define REFRESH_INTERVAL       1000         // check if refresh needed each these CPU instr 
/* 
Refresh strategy:

- If program issues any command to plotter that makes visible changes on screen
     - change the color of pen mounted on ploter arm (will need to do fullrefresh to rebuild the arm)
     - invoke the hack command to programatically set a new sheet (will need to do fullrefresh to show new clean paper)
     - plotter arm moving
     - raise/lower pen   
     - drawing on simulated paper 
  then set refresh_needed flag=1
- plt_timer_svc() is shedulled with sim_activate() to be executed
  each REFRESH_INTERVAL cpu instructions
- plt_timer_svc() checks 
     - Screen should be refreshed? (ie. refresh_needed!=0 ?) -> no, exit
     - Has at least 1000/FPS msec elapsed from previous refresh? -> no, exit
     - Clears flag: refresh_needed=0
     - If using CPANELs, builds the rectagle list of surface areas to update
           if FullrefreshNeed -> will update the whole surface
           If not using CPANELs will update the whole surface allways
     - Send SDL2 surface to GUI window for repainting
     - Check user pressed hotkeys (and process it if any)

- if SCP command makes any changes in appearence of screen
     - SET POWERON to open/refresh the GUI window, 
     - SET PEN COLOR to red, green, blue, black -> changes the pen on arm head 
     - SET NEWSHEET -> sets a new blank sheet to start a new plotting
     - SET FAST=2 -> does not render the plotter arm
     then Send the whole SDL2 surface to GUI window for repainting

*/ 

// internal refresh device & unit
// all declared as static so same declaration on all devices is possible (will not have confict names)
static t_stat plt_refresh_svc (UNIT *ptr);
static const char *plt_refresh_description (DEVICE *dptr) {
    return "Plotter GUI Window SDL2 Refresh facility";
}
static UNIT plt_refresh_unit = { UDATA (&plt_refresh_svc, UNIT_IDLE, 0) };
static DEVICE plt_refresh_dev = {
    "INT-REFRESH-PLOTTER", &plt_refresh_unit, NULL, NULL,    // the name INT-REFRESH-xxx
    1, 0, 0, 0, 0, 0,                                        // allows cpanel to call the 
    NULL, NULL, NULL, NULL, NULL, NULL,                      // device refresh routine refresh_svc
    NULL, DEV_NOSAVE, 0,                                     // at sim> prompt (if windows os) or
    NULL, NULL, NULL, NULL, NULL, NULL,                      // while SET CPANEL INTERACTIVE
    plt_refresh_description
};


#define PLT_PEN_MOVE_MSEC		200			// time in msec needed for plotter to raise/lower pen
#define PLT_SPEED_MULT			1.0			// arm speed factor (2.0 -> arm speed x 2)
#define PLT_ARM_ACCEL_MSEC		100			// msec time needed for arm to accel/stop on each vector
#define PLT_LOW_SPEED_MULT		8.0			// calibration low speed factor (8.0 -> arm speed is / 8 )
#define PLT_NEW_SHEET_MSEC     1000			// new blank sheet hack command takes 1s to execute

#if defined(CPANEL)
#define INITIAL_SCALE                    40             // if using CPANELs, default scale for window creation
#endif

// command line bits position in data sent from computer program to plotter 
#define PLT_CL_SYC		0x8000			// SYC command line. Indicates the pass 1 in data transmission to plotter
#define PLT_CL_MVR		0x0200			// MVR (Maneuver). 0=Position, 1=Pen maneuver
#define PLT_CL_PNC		0x0100			// PNC (Pen Command). 0=Pen Up, 1=Pen Down
#define PLT_CL_CDE		0x0400			// CDE (Code). 0=BDC, 1=Binary
#define PLT_CL_MDE		0x0800			// MDE (Mode). 0=Absolute, 1=Relative
#define PLT_CL_DXS		0x1000			// DXS (Delta X sign). 0=Positive, 1=Negative. Only aplies if relative and BDC used
#define PLT_CL_DYS		0x2000			// CDE (Delta Y sign). 0=Positive, 1=Negative. Only aplies if relative and BDC used

// status lines bits position in data sent from plotter to computer program to 
#define PLT_SL_PEN		0x0001			// PEN status line. 1=Pen Down, 0=Pen Up

// actions to be done by plotter that takes time to execute
#define LOWER_PEN	1
#define RAISE_PEN	2
#define MOVE_ARM	3
#define NEW_SHEET	4

static struct {
	// plotter settings
	int PenColor;					/* 0=black, 1=blue, 2=red, 3=green */
	int PenSize;					/* 0=pixel exact, no antialiasing, 1..6 biggest pen size */
	int pp_nDotsPerPixel;   		/* =2 -> 2 pp dots per pixel, =3 -> 3 dots, =4 -> 4 dots, =6 -> 6 dots, =8 -> 8 dots */
    int InitialFastMode;            /* =1 -> fast mode set by user with SET PLOTTER FAST */
    int AutoPowerOn;                /* will power the plotter device on first OTA/STC received from CPU */
	// plotter current state
	int FastMode;					/* Fast Mode currently active =0 normal speed, =1 fast speed, =2 fast and do not render plotter arm */
	int PenX, PenY;					/* current coords of physical Pen. range 0..9999. (0,0) is lower left corner */
	int Pen;						/* current pen postion. can be LOWER_PEN or RAISE_PEN */
	int ErrorLight;					/* =0 turned off, =1 turned on */
	int CalibrationDone;			/* =1 the initial 1.2 sec clamping arm calibration done */
    int32 pp_xsize, pp_ysize;		/* size of simulated paper sheet pp */
    uint16 *pp;     				/* paper sheet: stores the simulated inked dots */
    // variables to track the state when receiving data in multiple passes
	int pass;						/* number of current pass for multiplexed data read forn computer */
	int bdc;						/* =0 -> data received from CPU is binary, =1 -> is BDC */
	int32 Xreq, Yreq;				/* coords X,Y requested by the computer program */
	int RelativeMode;				/* =0 -> absolute coords mode, =1 relative mode */
	// variables to track the plotter arm/pen movement
	int PlotterAction;				/* can be LOWER_PEN or RAISE_PEN or MOVE_ARM */
	int ArmIsMoving;				/* =0 -> no, =1 -> yes, pen/arm GUI animation in progress */
	int32 MoveDuration_msec;		/* total duration of movement. =0 -> finished, -1 -> no movement */
	uint32 MoveStart_msec;			/* starting time in milliseconds (wallclock) of the movement */
	int FromX,FromY,GotoX,GotoY;	/* origin and destination coords for full arm move */
	uint32 CalibrationRemains_msec; /* msec time remaining to finish initial calibration */
	// variables to hold the in-progress line draw state
	int LineSteps;					/* total steps to do the line (the longest dx or dy)*/ 
	double dx,dy;					/* coord increment per step (pp coords)*/
	int x1,y1;						/* line origin in pp coords */
	int NextStep;					/* next step to be done on drawing */ 

    // variables for rendering on GUI
    int power;                      // 0=plotter powered off=no GUI window, 1=GUI window visible
    uint32 * surface;               // SDL2 GUI surface.  
    VID_DISPLAY *vptr;              // SDL2 window handled
    uint32 paper_color_palette[256];	// color palette for paper ink (4x64 -> as there are 4 possible pen colors: black, blue, red, green each with 64 intensities (0=white, 63=full color))
    uint32 plotter_color_palette[16];	// color palette for rest of plotter renderings
    int wx, wy;                     // width and height of GUI window
    int refresh_needed;             // =1 if surface has been modified and needs to be refreshed on GUI
    int FullRedrawNeeded;		    // =1 to request the full redraw of GUI window on next refresh call (=0 refresh only changed pp rectangle, arm and changes state error light)
    uint32 refresh_tnow;            // value for sim_os_msec() on last GUI refresh
    int quit_requested;             // =1 is user closes the GUI window

    int InitialScale;               // initial scale 10..200 to be used when window is created
    int InitialPosX;                // x,y postion of window on screen to be used when window is created
    int InitialPosY; 
    
	int Inked_x1, Inked_x2, Inked_y1, Inked_y2;			/* defines the changed rectangle in pp array from last refresh */

} plt_state = { 1, 2, 4, 0, 1,
                0, 0, 0, RAISE_PEN, 0}; 

#define INK_TO_COL(n,c)     plt_state.paper_color_palette[((n) >> 2) + 64 * (c)]		// convert ink intensity 0..255 to pp color palete
#define COL_BLACK       0
#define COL_DARK_GRAY   1
#define COL_GRAY        2
#define COL_LIGHT_GRAY  3
#define COL_WHITE       4
#define COL_BLUE        5
#define COL_ORANGE      6
#define COL_RED         7
#define COL_BEIGE       8
#define COL_DARK_BEIGE  9
#define COL_DARK_YELLOW 10

#define H0_BORDER	30				// see what this constansts means in comments later
#define H1_BORDER	30
#define H2_BORDER	30
#define H3_BORDER	30

#define V3_BORDER	20
#define V2_BORDER	15
#define V1_BORDER	15
#define V0_BORDER	50

#define ERRLIGHT_H  (H0_BORDER+H1_BORDER+55) 
#define ERRLIGHT_V  32

uint8 PointBmp[64*64];				// holds the antialiased plotted point (circle) image in paper sheet
int   PointBmpRadius = 0;			// and its radius in pixels
uint32 *arm_bkg;					// array to save the SDL surface under the plotter arm
int32 arm_bkg_x1, arm_bkg_x2;
int32 arm_bkg_y1, arm_bkg_y2;

uint8 HeadAPenBmp[64*64];			// holds the antialiased circle for plotter head pen handler (exterior)
uint8 Head0PenBmp[64*64];			// holds the antialiased circle pen (pen raised)
uint8 Head1PenBmp[64*64];			// holds the antialiased circle pen (pen lowered)
int ErrorLightStatus = 0;			// indicates the state for control panel error light

/*	How Plotter paper is simulated:

We have a paper resolution of 10K/4 x 10K/6 = 2500 x 1667
We add a little horizontal and vertical margin (160 and 120 pp-size dots) so that image 
is not generated in the border of paper. This image is stored in pp array
	
*/
#define H_MARGIN			160
#define V_MARGIN			120
#define H_PP_SIZE			(H_MARGIN + 2500 + H_MARGIN)
#define V_PP_SIZE			(V_MARGIN + 1667 + V_MARGIN)

#define HP_TO_PP_X(x)       (x / 4)
#define HP_TO_PP_Y(y)       (y / 6)

/* quantity of ink drop on paper when plotting a point or a line. Value 0..255
   low value -> light color. Multiple passes darkens the color. 
   high value -> intense color. But Multiples passes does not changes much (as color goes to the max value 255)
   e.g. if value = 96, on first plot, pp point will have an intensity of 96, on second pass, 192 (96*2), on third 
   pass and beyond it saturates to 255. 
*/
#define INK_DROP			110			

DEVICE plt_dev;											/* Plotter device */

IOHANDLER pltio;

t_stat plt_svc (UNIT *uptr);
t_stat plt_reset (DEVICE *dptr);
t_stat plt_set_power_on    (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat plt_set_pen_color   (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat plt_set_sheet       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat plt_set_param       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat plt_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat plt_show	           (FILE *st, UNIT *uptr, int32 num, CONST void *desc);
void plotter_refresh(uint32 tnow);
#if defined(CPANEL)
void plotter_check_HotKeys(void);
#endif

/* PLT data structures

   plt_dev      PLT device descriptor
   plt_unit     PLT unit
   plt_reg      PLT register list
*/

DIB plt_dib = { &pltio, PLT };

UNIT plt_unit = {
	UDATA (&plt_svc, UNIT_SEQ+UNIT_DISABLE, 0)
    };

REG plt_reg[] = {
    { ORDATA (BUF, plt_unit.buf, 7) },
    { FLDATA (CTL, plt.control, 0) },
    { FLDATA (FLG, plt.flag,    0) },
    { FLDATA (FBF, plt.flagbuf, 0) },
    { ORDATA (SC, plt_dib.select_code, 6), REG_HRO },
    { ORDATA (DEVNO, plt_dib.select_code, 6), REG_HRO },
    { NULL }
    };

#define DBG_FLAGS    0x00000004 
#define DBG_DATA     0x00000008 
#define DBG_CMDS     0x00000010 
#define DBG_DETAIL   0x00000020   

DEBTAB sim_plt_debug[] = {
  {"FLAGS",     DBG_FLAGS,   "Flags status"},
  {"DATA",      DBG_DATA,    "Data sent to Plotter"},
  {"COMMAND",   DBG_CMDS,    "Commands sent to plotter"},
  {"DETAIL",    DBG_DETAIL,  "Operations detail"},
  {0}
};

MTAB plt_mod[] = {
    { UNIT_POWERON, UNIT_POWERON,  "plotter on",             "ON",           &plt_set_power_on },
    { UNIT_POWERON, 0,             "plotter off",            "OFF",          &plt_set_power_on },
    { UNIT_POWERON, UNIT_POWERON,   NULL,                    "POWERON",      &plt_set_power_on },
    { UNIT_POWERON, 0,              NULL,                    "POWEROFF",     &plt_set_power_on },

	{ MTAB_XTD | MTAB_VDV, 0,       NULL,                    "BLACKPEN",     &plt_set_pen_color },
	{ MTAB_XTD | MTAB_VDV, 1,       NULL,                    "BLUEPEN",      &plt_set_pen_color },
	{ MTAB_XTD | MTAB_VDV, 2,       NULL,                    "REDPEN",       &plt_set_pen_color },
	{ MTAB_XTD | MTAB_VDV, 3,       NULL,                    "GREENPEN",     &plt_set_pen_color },

	{ MTAB_XTD | MTAB_VDV, 0,       NULL,                    "BLANKSHEET",   &plt_set_sheet },
	{ MTAB_XTD | MTAB_VDV, 0,       NULL,                    "NEWSHEET",     &plt_set_sheet },
    { MTAB_XTD | MTAB_VDV, 1,       NULL,                    "SAVE",         &plt_set_sheet, NULL, &plt_dev },

    { UNIT_POWERON, 0,              NULL,                    "NOAUTOON",     &plt_set_AutoPowerOn },
    { UNIT_POWERON, 1,              NULL,                    "AUTOON",       &plt_set_AutoPowerOn },

	{ MTAB_XTD | MTAB_VDV,            0, "SC",       "SC",    &hp_setsc,  &hp_showsc,  &plt_dev },
    { MTAB_XTD | MTAB_VDV | MTAB_NMO, 0, "DEVNO",    "DEVNO", &hp_setdev, &hp_showdev, &plt_dev },

#if defined(CPANEL)
    // important: SCALE should be defined AFTER "SC". If defined before, SET SC= is interpreted as
    // SET SCale (scale being abbreviated on its two first letter), so SC will never be called
    { MTAB_XTD | MTAB_VDV, 0,       NULL,                    "POS",          &plt_set_param, NULL, &plt_dev },
    { MTAB_XTD | MTAB_VDV, 1,       NULL,                    "SCALE",        &plt_set_param, NULL, &plt_dev },
#endif
	{ MTAB_XTD | MTAB_VDV, 2,       NULL,                    "PENSIZE",      &plt_set_param, NULL, &plt_dev },
	{ MTAB_XTD | MTAB_VDV, 3,       NULL,                    "FAST",         &plt_set_param, NULL, &plt_dev },

    { 0 }
    };

DEVICE plt_dev = {
    "PLOTTER", &plt_unit, plt_reg, plt_mod,
    1, 10, 31, 1, 8, 8,	// 1->num of units, 10->addr radix, 31->addr width, 1->addr incr, 8->data radix, 8->data width
    NULL, NULL, &plt_reset,
    NULL, NULL, NULL,
	&plt_dib, DEV_DISABLE | DEV_DIS | DEV_DEBUG, 0, sim_plt_debug
    };

static void plotter_quit_callback (void) 
{
    // note: quit callback is executed on sim_video thread, not in main emulator thread
    // should not use sim_debug (is not thread safe) 
    plt_state.quit_requested=1; 
}

// decompose a surface uint32 to its rr,gg,bb components. return alpha channel
int plotter_get_surface_rgb_color(uint32 color, int *r_Color, int *g_Color, int *b_Color)
{
    int alpha; 
    if (sim_end) {
        alpha = color >> 24; 
        color = color & 0x00FFFFFF;
        *r_Color = (color >> 16);
        *g_Color = ((color & 0x00FF00) >>  8);
        *b_Color = (color & 0x0000FF);
    } else {
        alpha = color & 0xFF; 
        color = color >> 8;
        *r_Color = (color & 0x0000FF);
        *g_Color = ((color & 0x00FF00) >>  8);
        *b_Color = (color >> 16);
    }
    return alpha; 
}

uint32 plotter_rgb_color(uint32 r, uint32 g, uint32 b)  /* r,g,b are 8bit! */
{
	uint32 color;

	// rgb to 16 bits 
	r = (r & 255) << 8; g = (g & 255) << 8; b = (b & 255) << 8; 

    color = sim_end ? 
		       (0xFF000000 | ((r & 0xFF00) << 8) | (g & 0xFF00) | ((b & 0xFF00) >> 8)) : 
	           (0x000000FF | (r  & 0xFF00) | ((g & 0xFF00) << 8) | ((b & 0xFF00) << 16));
    return color;
}

// set palette color for nCol from full r,g,b value to darker 
void paper_palette_white_rgb_dark(int nCol, int r, int g, int b) 
{
	int r1, g1, b1;
	int n;
	for (n=0;n<48;n++) {
		r1 = 255-((255-r)*n)/48; 
		g1 = 255-((255-g)*n)/48;
		b1 = 255-((255-b)*n)/48;
		plt_state.paper_color_palette[n+nCol*64]=plotter_rgb_color(r1,g1,b1);
	}
	for (n=48;n<64;n++) {
		r1 = r-(r*(n-48))/24;
		g1 = g-(g*(n-48))/24;
		b1 = b-(b*(n-48))/24;
	    plt_state.paper_color_palette[n+nCol*64]=plotter_rgb_color(r1,g1,b1);
	}
}

void clear_paper_sheet(void)
{
	// clear/make white/blank/fresh new sheet = clear paper sheet where image is plotter
	if (plt_state.pp) {
		int n;
		for (n=0; n<plt_state.pp_xsize * plt_state.pp_ysize; n++) plt_state.pp[n]=0;
	}
	plt_state.CalibrationDone = 0; // new sheet implies removing it from plotter so new calibration is needed
}


int plotter_init(void) 
{
	int n, stat;
	// number of paper dots per gui pixels 
#if defined(CPANEL)
    n=plt_state.pp_nDotsPerPixel=2; // one pixel is 2x2 paper dots
#else
    n=plt_state.pp_nDotsPerPixel=4; // one pixel is 4x4 paper dots
#endif
    // populate wc, wy and allocate mem for surface
	plt_state.wx = 1 + (uint32)(H_PP_SIZE / n) + H0_BORDER + H1_BORDER + H2_BORDER + H3_BORDER;
	plt_state.wy = 1 + (uint32)(V_PP_SIZE / n) + V0_BORDER + V1_BORDER + V2_BORDER + V3_BORDER;
    plt_state.surface = (uint32 *)malloc (plt_state.wx * plt_state.wy * sizeof(uint32));
    if (plt_state.surface == NULL) return SCPE_IERR;

#if defined(CPANEL)
    if (plt_state.InitialScale == 0) plt_state.InitialScale=INITIAL_SCALE; 
    if ((plt_state.InitialPosX == 0) && (plt_state.InitialPosY == 0)) {
        plt_state.InitialPosX=60; plt_state.InitialPosY=40; 
    }
    stat = vid_open_window_ex (&plt_state.vptr, &plt_dev, "HP7210A Plotter", plt_state.wx, plt_state.wy, 
        SIM_VID_FLAG_SCALE_PLUSMINUS | SIM_VID_FLAG_ALLOW_TOOLTIP  | SIM_VID_FLAG_ALLOW_DRAG,
        plt_state.InitialScale, plt_state.InitialPosX, plt_state.InitialPosY);
    if (stat != SCPE_OK) return stat; 
#else
    stat = vid_open_window (&plt_state.vptr, &plt_dev, "HP7210A Plotter", plt_state.wx, plt_state.wy, 0);
    if (stat != SCPE_OK) return stat; 
#endif

	// ploter draws on a sheet of paper simulated pp array. Each item of array is the ammount of ink in the paper dot, that range
	// from 0 (white) up to 255 (full color). Intermediate values are diferent levels of grey as result of antialiased drawing
	plt_state.pp_xsize = H_PP_SIZE;
	plt_state.pp_ysize = V_PP_SIZE; 
	          // Plot surface is 38.1 x 25.4 cm for 10K x 10K points. Aspect ratio = 38.1/25.4= 1.5
	          // emulated paper sheet resolution is 10K/4 x 10K/6. This is choosen to ease the antialiasing processing,
	          // to keep the pp array at a reasonable size, and to keep the original aspect ratio of 1.5:
			  // 10K/4 = 2500, 10K/6=1667, 2500/1667=1.5
	if (plt_state.pp == NULL) {
		n = plt_state.pp_xsize * plt_state.pp_ysize;
		plt_state.pp = (uint16 *)malloc (n*sizeof(*plt_state.pp));
    	if (!plt_state.pp) {
	    	fprintf(stderr, "malloc fail on plotter init (paper sheet mem)\r\n");
		    return 0;
	    }
		clear_paper_sheet();
	}
	n = plt_state.wx * plt_state.wy;		// arm background is a copy of surface and has its same size
    if (arm_bkg == NULL) {
        arm_bkg = (uint32 *)malloc (n*sizeof(*arm_bkg));
	    if (!arm_bkg) {
		    fprintf(stderr, "malloc fail on plotter init (save arm background mem)\r\n");
		    return 0;
        }
	}
	// fill in the color palette for paper:
	// (entries 0..47) full white to rgb color gray/blue/red/green
	// (entries 48..63) full gray/blue/red to black/dark blue/dark red
    paper_palette_white_rgb_dark(0,  64,  64,  64); // black ink
    paper_palette_white_rgb_dark(1,   0,   0, 255); // blue
    paper_palette_white_rgb_dark(2, 255,   0,   0); // red 
    paper_palette_white_rgb_dark(3,  32, 176,  80); // green
	// fill in control panel color palete
	plt_state.plotter_color_palette[COL_BLACK]       = plotter_rgb_color(0,0,0);		
	plt_state.plotter_color_palette[COL_DARK_GRAY]   = plotter_rgb_color(64,64,64);		
	plt_state.plotter_color_palette[COL_GRAY]        = plotter_rgb_color(128,128,128);		
	plt_state.plotter_color_palette[COL_LIGHT_GRAY]  = plotter_rgb_color(224,224,224);		
	plt_state.plotter_color_palette[COL_WHITE]       = plotter_rgb_color(255,255,255);		
	plt_state.plotter_color_palette[COL_BLUE]        = plotter_rgb_color(0,0,255);		
	plt_state.plotter_color_palette[COL_ORANGE]      = plotter_rgb_color(255,192,64);		
	plt_state.plotter_color_palette[COL_RED]         = plotter_rgb_color(255,0,0);		
	plt_state.plotter_color_palette[COL_BEIGE]       = plotter_rgb_color(145,150,120);		
	plt_state.plotter_color_palette[COL_DARK_BEIGE]  = plotter_rgb_color(115,115,80);		
	plt_state.plotter_color_palette[COL_DARK_YELLOW] = plotter_rgb_color(64,64,0);		

	plt_state.FullRedrawNeeded = 1; // because plotter init issued
    plt_state.refresh_needed=1; // because plotter init issued
    plt_state.refresh_tnow=0; 
    plt_state.FastMode=plt_state.InitialFastMode; 

	// init inked box to detect where paper was inked (the dirty box) for next refresh
	plt_state.Inked_x1 = plt_state.pp_xsize+1;		plt_state.Inked_y1 = plt_state.pp_ysize+1;
	plt_state.Inked_x2 = -1;				        plt_state.Inked_y2 = -1;
    return SCPE_OK;
}

void plotter_done(void) 
{
	sim_os_ms_sleep (200); // this sleep is to asure SDL thread has processed any pending EVENT_REDRAW
	vid_close_window(plt_state.vptr);
    plt_state.vptr=NULL; 
    if (plt_state.surface) free(plt_state.surface); 
    plt_state.surface=NULL;
    if (plt_state.pp) free(plt_state.pp);
	plt_state.pp = NULL;
	if (arm_bkg) free(arm_bkg);
    arm_bkg=NULL; 
}

// Creates and destroy the GUI window 
// power = 0 -> power off (closes GUI Window), 
//       = 1 -> power on (creates and shows GUI Window), 
t_stat power_plotter(int power) {

    t_stat stat; 

	if (power > 0) {
		// power on plotter 
        if (plt_state.power == 1) {
            plotter_refresh(0);   // force refresh now: power on also refreshes the screen 
            return SCPE_OK;  // already powered on
        }
		sim_debug (DBG_DETAIL, &plt_dev, "Plotter Init\n");
        // create GUI window
		stat=plotter_init();
        if (stat) return stat; 
        // init done
        plt_state.power=1; 
        // set quit callback. Register it linked to close button of last created window
        plt_state.quit_requested=0; 
        vid_register_quit_callback (&plotter_quit_callback);
        // init local vars
        // clear screen 
        memset(plt_state.surface, 0, plt_state.wx * plt_state.wy * sizeof(uint32));
        // force refresh
        plt_state.FullRedrawNeeded = 1;	// first refresh after power on 
		plotter_refresh(0);  // first refresh after power on 
        // start refresh_svc
        sim_register_internal_device (&plt_refresh_dev);
        sim_activate(&plt_refresh_unit, REFRESH_INTERVAL);
    } else {
		/* power off plotter -> close GUI window */
		if (plt_state.power == 0) return SCPE_OK; /* plotter already off -> nothing to do*/
        // stop refresh_svc
        sim_cancel(&plt_refresh_unit);
        sim_cancel(&plt_unit);						
        // close GUI window
		plotter_done();
        // power off done
		plt_state.power = 0;
		sim_debug (DBG_DETAIL, &plt_dev, "Plotter Done\n");
	}
    return SCPE_OK; 
}

// power the plotter if AutoPowerOn flag set and power is off
void plotter_CheckAutoPowerOn(void)
{
    if (plt_state.AutoPowerOn == 0) return; 
    if (plt_state.power) return; 
    power_plotter(1); 
}


/* refresh: draw with curren pen ink color & size into simulated plotter paper, 
            draw moving arm & pen on visible windows, and redraw on GUI */

/* Description of graphic constants to render plotter's arms and head
   all are pixel coords. (0,0) at lower left corner


                      
                         Minimum H1_BORDER
                         |<->| 
                         
                             |<-------------------->| Minimum H2_BORDER                    
                             
                         |<--- Arm_Head_H_Width --->|
                      
                         .   .    |                 | 
                                  |                 | 
                         .   .    |                 | 
     Minimum V2_BORDER            |                 | 
            --- .  .  .  +--------+                 |  ---
             ^           |                          |   ^
             |           |   .                      |   |  Head_V_Heigh
             |           |   .                      |   |  
             v           |      Pen Pos             |   |
             -  .  .  .  |   +  x1,y1  .  .  .  .   | . |  ---
             ^           |                          |   |   ^
             |           |   .                      |   |   |  Pen_V_Offset
             v           |                          |   v   v
            --- .  .  .  +--------+                 |  -------
     Minimum V1_BORDER            |                 | 
                         .   .    |                 | 
                                  |                 | 
                         .   .    |                 | 
                         
                         .   |<--- Pen_H_Offset --->|
                                 
                         .      
                                  |<- Arm_H_Width ->|
                         .

                         |                          |
                         arm_x1                    arm_x2          



Coordinates correspondece (same for Vertical ones: Hn_xxxx) :

                                       0                   9999              = HP coordinates seen by HP program
                                       |  plotting   space    |
                            |          |                      |          |
                            | H_MARGIN |  <-- 2500 dots --->  | H_MARGIN |   = pp[] array
                            0                                            nnn = 2500 + LMARGIN + R_MARGIN
	                        |                                            |          
    | H0_BORDER + H1_BORDER |                                            | H2_BORDER + H3_BORDER |
	|                       |                                            |           .           |
    0           .           | <-- 2500 / 2 if scale2 = 1250 ------------>|   = GUI window pixels in SDL surface array  
	|                                  / 3 if scale3 = 833                                       |
	|                                  / 4 if scale4 = 625                                       |
	|           .                      / 6 if scale6 = 416                           .           |
	|                                  / 8 if scale8 = 312 = effective addressable pixels        |
	|           .                                                                    .           |
    |<-------------------------------  GUI windows width in pixels ----------------------------> |
                .                                                                    .
				|<-------- plotter arm and head is rendered in this space ---------->|
	|<--------->| = control panel lights go there
*/

static char *ErrorBmpStr[] = {
	"XXXX0  XXXX.  XXXX.  .XXX.  XXXX. ",
	"X.     X. .X  X. .X  X0 0X  X. .X ",
	"XXX    X. .X  X. .X  X   X  X. .X ",
	"X      XXX0   XXX0   X   X  XXX0  ",
	"X.     X .X   X .X   X0 0X  X .X  ",
	"XXXX0  X  .X  X  .X  .XXX.  X  .X ",
	"",
};

static char *ErrorBmpStr2[] = {
	"XXXXXXXX0     XXXXXXX.      XXXXXXX.       .XXXXXX.      XXXXXXX.    ",
	"XXXXXXXX0     XXXXXXXX.     XXXXXXXX.     .XXXXXXXX.     XXXXXXXX.   ",
	"XX.           XX.   .XX.    XX.   .XX.    XX0    0XX     XX.   .XX.  ",
	"XX.           XX     .XX    XX     .XX    XX      XX     XX     .XX  ",
	"XX.           XX.   .XX.    XX.   .XX.    XX      XX     XX.   .XX.  ",
	"XXXXXX0       XXXXXXXX.     XXXXXXXX.     XX      XX     XXXXXXXX.   ",
	"XXXXXX0       XXXXXXXX      XXXXXXXX      XX      XX     XXXXXXXX    ",
	"XX.           XX.  .XX.     XX.  .XX.     XX      XX     XX.  .XX.   ",
	"XX.           XX.   .XX.    XX.   .XX.    XX      XX     XX.   .XX.  ",
	"XX.           XX.   .XX.    XX.   .XX.    XX      XX     XX.   .XX.  ",
	"XXXXXXXX0     XX.    .XX.   XX.    .XX.   .XXXXXXXX.     XX.    .XX. ",
	"XXXXXXXX0     XX.    .XX.   XX.    .XX.    .XXXXXX.      XX.    .XX. ",
    "",
};

void Paint_bmp_in_plt_surface(int x0, int y0, uint8 *Bmp[])
{
	// draw bitmap starting at x0, y0 (lower left corner. y0 goes direction up)
	int n,h,c,ix,iy;
	// get height of bitmap
	for (h=0;Bmp[h][0]!=0;h++);
	for (iy=0;iy<h;iy++) {
		for (ix=0;;ix++) {
			c = Bmp[iy][ix];
			if (c == 0) {break;}
			else if (c == 'X') {n = COL_WHITE;}		
			else if (c == '0') {n = COL_GRAY;}	
			else if (c == '.') {n = COL_DARK_GRAY;}
			else continue;
            plt_state.surface[x0+ix + plt_state.wx * (y0+iy)] = plt_state.plotter_color_palette[n];
		}
	}
}

void Paint_box_in_plt_surface(int x0, int y0, int wx, int wy, int col)
{
	int iy, ix, c; 
	for (iy=0;iy<wy;iy++) {
		for (ix=0;ix<wx; ix++) {
			if ((ix == 0) || (iy == 0) || (ix == wx-1) || (iy == wy-1)) {
				c = COL_WHITE;
			} else { c=col; }
            plt_state.surface[x0+ix + plt_state.wx * (y0+iy)] = plt_state.plotter_color_palette[c];
		}
	}
}

void Paint_circle_in_plt_surface(uint8 Bmp[64*64], int r, int ink, int bFilled) 
// render in given char array Bmp[64x64] an antialiased circle, filled or not
{
	int x,y,i,n;
	double yd;
	if (r>31) r=31;									// max 31-1 because Bmp is 64x64
	for (x=0;x<64;x++) for (y=0;y<64;y++) {			// clear bmp previous contents
		Bmp[x*64+y]=0; 
	}
	ink=192; 
	for(x=0;x<2*r/3+3;x++) {
		if (x >= r-1) break;
		yd = sqrt((double)(r-1)*(r-1)-x*x);
		y = (int) yd;
		n = (int)((yd - y) * ink); // antialiase to look great
		Bmp[(32+x)*64 + 32+y]=ink-n; Bmp[(32+x)*64 + 32+y+1]=n;
		Bmp[(32-x)*64 + 32+y]=ink-n; Bmp[(32-x)*64 + 32+y+1]=n;
		Bmp[(32+x)*64 + 32-y]=ink-n; Bmp[(32+x)*64 + 32-y-1]=n;
		Bmp[(32-x)*64 + 32-y]=ink-n; Bmp[(32-x)*64 + 32-y-1]=n;
		Bmp[(32+y)*64 + 32+x]=ink-n; Bmp[(32+y+1)*64 + 32+x]=n; 
		Bmp[(32-y)*64 + 32+x]=ink-n; Bmp[(32-y-1)*64 + 32+x]=n;
		Bmp[(32+y)*64 + 32-x]=ink-n; Bmp[(32+y+1)*64 + 32-x]=n; 
		Bmp[(32-y)*64 + 32-x]=ink-n; Bmp[(32-y-1)*64 + 32-x]=n; 
		if (bFilled) {
			for(i=-y;i<y+1;i++) { Bmp[(32+x)*64 + 32+i]=ink; }
			for(i=-y;i<y+1;i++) { Bmp[(32+x)*64 + 32-i]=ink; }
			for(i=-y;i<y+1;i++) { Bmp[(32-x)*64 + 32+i]=ink; }
			for(i=-y;i<y+1;i++) { Bmp[(32-x)*64 + 32-i]=ink; }
			for(i=-y;i<y+1;i++) { Bmp[(32+i)*64 + 32+x]=ink; }
			for(i=-y;i<y+1;i++) { Bmp[(32+i)*64 + 32-x]=ink; }
		}
	}
	Bmp[(32-(r-1))*64 + 32]= ink;
}

#if defined(CPANEL)
rectlist plt_RectList; // rectangle list structure

int AddSurfaceDirtyRect(int Mode, int x1, int y1, int ww, int hh)
{
    int n, nNew, nOld; 

    if ((x1<0) || (y1 < 0) || (ww<0) || (hh<0)) {
        return -1; // safety
    }

    if (Mode<1) {
        // init rectlist Mode=0, mark rectlist as invalid (Mode=-1) will redraw the whole surface; 
        plt_RectList.Count=Mode; 
        return -1;  
    } 
    if (Mode == 2) {
        // Merge rect x1 with newly added rect y1
        nNew = y1; 
        nOld = x1; 
        if (plt_RectList.h[nOld] != plt_RectList.h[nNew]) return 0; // not same height
        if ((plt_RectList.x[nOld] == plt_RectList.x[nNew]) && (plt_RectList.w[nOld] == plt_RectList.w[nNew])) {
            // both rect are the same, discard last rect
            plt_RectList.Count--;
        } else if ((plt_RectList.x[nOld] < plt_RectList.x[nNew]) && 
                   (plt_RectList.x[nNew] <= plt_RectList.x[nOld] + plt_RectList.w[nOld])) {
            // new rect overlaps old  [Old   <New   ]   >
            // extend old rect ...    [Old              ]
            plt_RectList.w[nOld] = (plt_RectList.x[nNew] + plt_RectList.w[nNew]) - plt_RectList.x[nOld]; 
            // ... and discard new rect
            plt_RectList.Count--;
        } else if ((plt_RectList.x[nNew] < plt_RectList.x[nOld]) && 
                   (plt_RectList.x[nOld] <= plt_RectList.x[nNew] + plt_RectList.w[nNew])) {
            // new rect overlaps old  <New   [Old   >   ]
            // extend old rect ...    [Old              ]
            plt_RectList.w[nOld] = (plt_RectList.x[nOld] + plt_RectList.w[nOld]) - plt_RectList.x[nNew]; 
            plt_RectList.x[nOld] = plt_RectList.x[nNew];
            // ... and discard new rect
            plt_RectList.Count--;
        }
        return 0;
    }
    // add entry to rectlist
    n=plt_RectList.Count; 
    if (n==-1) return -1; // rect table full
    if (n>=RECTLIST_MAX) {
        // set rect table as full
        plt_RectList.Count=-1; 
        return -1; 
    }
    // room left on rectable table. can add entry
    plt_RectList.flags[n]=0;
    plt_RectList.x[n]=x1; plt_RectList.w[n]=ww; 
    plt_RectList.y[n]=y1; plt_RectList.h[n]=hh; 
    plt_RectList.Count++;
    return n;
}
#else
    // no CPANELS -> dummy placeholder to avoid compiling errors
    int AddSurfaceDirtyRect(int Mode, int x1, int y1, int ww, int hh)
    {
        // do nothing
        return -1; 
    }
#endif

void plotter_refresh(uint32 tnow)
{
	int Arm_Head_H_Width, Head_V_Height, Arm_H_Width, Pen_H_Offset, Pen_V_Offset; 
    int Arm_Shadow_H_Width, p, rr,gg,bb, Shadow_Ammout; 
	int xp,yp,x1,x2,y1,y2,ix,iy,n,nDots,wx,wy,bSomeInk,bx1,bx2,by1,by2,r;
	int arm_x1, arm_x2, pen_x, pen_y, arm_bkg_rect;
	int nInk, nCol[4], col;
	
	if (plt_state.power == 0) return;

    xp=plt_state.wx; //size in pixels of surface (for GUI window)
    yp=plt_state.wy;

	// copy paper sheet to GUI window
	// pp size is bigger than surface -> display mean pixel intensity to get a smaller antialiased image

	// number of paper dots per gui pixels 
    nDots = plt_state.pp_nDotsPerPixel; 
	// given the scale, set up size for arm and moving head
	Arm_H_Width      = (int) (150 / nDots);
	Pen_H_Offset     = (int) (Arm_H_Width * 1.5)-1;
	Arm_Head_H_Width = (int) (Arm_H_Width * 1.95);
	Head_V_Height    = Arm_H_Width;
	Pen_V_Offset     = (int) (Head_V_Height / 2);
    // init rectlist to keep track of modified surface rectangles
    AddSurfaceDirtyRect(0,0,0,0,0); 
    // Is full redraw requested ? 
	if 	(plt_state.FullRedrawNeeded) {
        // waning! plotter coords have (0,0) at lower left corner 
        // surface coords have (0,0) at upper left coner. This is a pain, a conversion
        // should allways be made: y_at_surface = yp - y_at_plotter
		plt_state.FullRedrawNeeded = 0;
        plt_state.refresh_needed = 1; // beacuse FullRedrawNeeded=1
		// make inked box the whole paper. -> is the modified zone
		plt_state.Inked_x1 = 0;			            plt_state.Inked_y1 = 0;
		plt_state.Inked_x2 = plt_state.pp_xsize;	plt_state.Inked_y2 = plt_state.pp_ysize;
        AddSurfaceDirtyRect(-1,0,0,0,0); // no rectlist. Will redraw all surface on screen
        // discard any background saved
		arm_bkg_x1 = -1;				
        // now render the graphic objects to be used later when drawing into paper sheet (pp array)
		// Render a point plot into PointBmp array 
		if (plt_state.PenSize > 0) {
			PointBmpRadius=((plt_state.PenSize+1)*(plt_state.PenSize+1))/2;	// radius as funtion of pixel size
			Paint_circle_in_plt_surface(PointBmp, PointBmpRadius, 192, 1);
		}
        // set up surface for display on GUI
		// fill in with plotter background. 
		for (iy=0;iy<yp;iy++) {
			if ((iy < V0_BORDER-3) || 
				(iy == (V0_BORDER-1)) ||
				(iy == (yp - V3_BORDER)) ){
				col = COL_BLACK;
			} else if (((iy > (yp - V3_BORDER - 3)) && (iy < (yp - V3_BORDER + 3))) || 
					   ((iy > (V0_BORDER - 4)) && (iy < (V0_BORDER + 2))) ){
				col = COL_DARK_BEIGE;
			} else {
				col = COL_BEIGE;
			}
			for (ix=0; ix<xp; ix++) {
				y2 = yp - 1 - iy;
				plt_state.surface[y2*xp + ix] = plt_state.plotter_color_palette[col];
			}
		}
		// draw error light
#if defined(CPANEL)
		Paint_bmp_in_plt_surface(ERRLIGHT_H+5+50,yp-ERRLIGHT_V+6, (uint8 **) ErrorBmpStr2);
#else
		Paint_bmp_in_plt_surface(ERRLIGHT_H+5+50,yp-ERRLIGHT_V+5, (uint8 **) ErrorBmpStr);
#endif
		Paint_box_in_plt_surface(ERRLIGHT_H, yp-ERRLIGHT_V, 43, 22, COL_DARK_YELLOW);
		ErrorLightStatus = 0;
		// prepare head/pen bitmaps
		r = (int) (Head_V_Height * 1.0 / 2);			
		Paint_circle_in_plt_surface(HeadAPenBmp, r, 255, 0);	
		r = (int) (r * 0.6);
		Paint_circle_in_plt_surface(Head0PenBmp, r, 255, 0);	// not filled
		Paint_circle_in_plt_surface(Head1PenBmp, r, 255, 1);	// fill
	}
	// restore gui pixels under the moving plotter arm from previously saved arm background
    // the saved area width includes the head 
	if ((arm_bkg_x1 < 0) || (Arm_Head_H_Width < 1) || (plt_state.FastMode == 2)) {
		// no background to restore
        arm_bkg_x1=-1; 
        arm_bkg_rect=-1; 
	} else {
        for (iy=arm_bkg_y1;iy<arm_bkg_y2;iy++) {
   		    for (ix=arm_bkg_x1; ix<arm_bkg_x2; ix++) {
				plt_state.surface[iy*xp + ix] = arm_bkg[iy*xp + ix];
			}
		}
        // Add to RectList so the restored pixels will be repainted 
        arm_bkg_rect=AddSurfaceDirtyRect(1,arm_bkg_x1,arm_bkg_y1,
                              arm_bkg_x2-arm_bkg_x1,arm_bkg_y2-arm_bkg_y1); // add restored background under arm to rectlist 
    }
	// calculate GUI windows size
	wx = 1 + (uint32)(H_PP_SIZE / nDots);			    
	wy = 1 + (uint32)(V_PP_SIZE / nDots);
	// calculate GUI windows zone to redraw based on inked box (the dirty rectangle)
	bx1 = (uint32)(plt_state.Inked_x1 / nDots);		if (bx1 < 0) bx1 = 0;
	by1 = (uint32)(plt_state.Inked_y1 / nDots);		if (by1 < 0) by1 = 0;			
	bx2 = (uint32)(plt_state.Inked_x2 / nDots)+1;	if (bx2 > wx) bx2 = wx;		
	by2 = (uint32)(plt_state.Inked_y2 / nDots)+1;	if (by2 > wy) by2 = wy;		
	// draw pixels into surface from pp inked box
    if ((bx1>bx2) || (by1>by2)) {
        // no ink box initialized (nothing drawn from last refresh). So nothing to draw on surface
    } else {
	    for (x1=bx1;x1<bx2;x1++) {
    		for (y1=by1;y1<by2;y1++) {
			    nInk = nCol[0] = nCol[1] = nCol[2] = nCol[3] = 0;
		    	x2 = x1 * nDots; 
	    		y2 = y1 * nDots; 
    			for (ix=x2;ix<x2+nDots;ix++) {
				    for (iy=y2;iy<y2+nDots;iy++) {
			    		if ((ix >= plt_state.pp_xsize) || (iy >= plt_state.pp_ysize)) continue;
		    			n = plt_state.pp[iy*plt_state.pp_xsize + ix];
	    				if (n > 0) {
    						col = ((n >> 8) & 3);
						    nInk += (n & 255);	    // accumulate ink
					    	nCol[col]++;			// count points of each color
				    	}
			    	}
		    	}
	    		bSomeInk = (nInk > 0);
    			// calc dominant color
			    if (nCol[0] > nCol[1]) { n=nCol[0]; col=0; } else { n=nCol[1]; col=1; } 
		    	if (nCol[2] > n      ) { n=nCol[2]; col=2; }
	    		if (nCol[3] > n      ) { col=3; }
    			// draw the pixel on surface
			    nInk = nInk / (nDots * nDots);
		    	x2 = x1 + H0_BORDER + H1_BORDER;
	    		y2 = yp - 1 - (y1 + V0_BORDER + V1_BORDER);
    			if (nInk > 255) nInk = 255; 
		    	plt_state.surface[y2*xp + x2] = INK_TO_COL(nInk, col); // use pen palette depending on pen color
	    	}
    	}
        // add inked box to rectlist 
        AddSurfaceDirtyRect(1,bx1+H0_BORDER+H1_BORDER,yp-1-(by2+V0_BORDER+V1_BORDER),
                              bx2-bx1,by2-by1+1); 
    }
	// prepare for drawing arm & pen & control panel
	// convert HP plotter coords (0..9999) to gui screen coords for current scale
	pen_x = (uint32)(((plt_state.PenX / 4) + H_MARGIN) / nDots) + H0_BORDER + H1_BORDER;		
	pen_y = (uint32)(((plt_state.PenY / 6) + V_MARGIN) / nDots) + V0_BORDER + V1_BORDER;		
	// calc gui x coords overlayed by plotter moving arm
	arm_x2=pen_x + Pen_H_Offset;		
	arm_x1=arm_x2-Arm_Head_H_Width;							
	// calc vertical space covered by plotter arm (i.e. exclude V0_BORDER and V3_BORDER)
	wy = 1 + (uint32)( V_PP_SIZE / nDots ) + V1_BORDER + V2_BORDER ;
	// save gui pixels under the moving plotter arm (arm background). Add one pixel column at 
    // each side as a precaution
	arm_bkg_x1 = arm_x1-5;
	arm_bkg_x2 = arm_x2+5;
    arm_bkg_x2 += (Arm_Shadow_H_Width = Arm_Head_H_Width/6);  // add shadow width
    arm_bkg_y2 = -1; 
	if ((Arm_Head_H_Width > 0) && (plt_state.FastMode < 2)) {
    	for (iy=V0_BORDER;iy<wy+V0_BORDER;iy++) {
			y2 = yp - 1 - iy;
            if (arm_bkg_y2==-1) arm_bkg_y2=y2; // save vertical positions
            arm_bkg_y1=y2;
	    	for (ix=arm_bkg_x1; ix<arm_bkg_x2; ix++) {
				arm_bkg[y2*xp + ix] = plt_state.surface[y2*xp + ix];
			}
		}
        arm_bkg_y2=arm_bkg_y2+1; // to include last line in rectangle
        // Add to RectList so the saved area (where plotter arm + head is drawn) will be repainted 
        // but previously check is rect can be merged with rect from restored background
        if (arm_bkg_rect < -1) { 
            // no rect to merge wiht}
        } else {
            // Add new rect
            n = AddSurfaceDirtyRect(1,arm_bkg_x1,arm_bkg_y1,
                                    arm_bkg_x2-arm_bkg_x1,arm_bkg_y2-arm_bkg_y1); // add restored background under arm to rectlist 
            // check if both rects can be merged
            AddSurfaceDirtyRect(2, arm_bkg_rect, n, 0, 0); 
        }
	}
	// draw arm + head over saved area
    Shadow_Ammout=95; // shadow ammount 100=no shadow, 90=light shadow; 50=dark shadow
	bx1=(int) (arm_x2-Arm_H_Width);
	bx2=(int) (arm_x2-Arm_H_Width * 0.15);
	x1 = (int) (Arm_H_Width / 8); x2 = (int) (x1 * 1.1);
	if (plt_state.FastMode < 2) {
        // draw arm shadow
		for (iy=V0_BORDER;iy<wy+V0_BORDER;iy++) {
			for (ix=bx2; ix<bx2+Arm_Shadow_H_Width; ix++) {
				y2 = yp - 1 - iy;
                plotter_get_surface_rgb_color(plt_state.surface[p=(y2*xp + ix)], &rr,&gg,&bb);
                rr=rr*Shadow_Ammout/100; gg=gg*Shadow_Ammout/100; bb=bb*Shadow_Ammout/100;
				plt_state.surface[p] = plotter_rgb_color(rr,gg,bb);
            }
        }
        // draw arm
		for (iy=V0_BORDER;iy<wy+V0_BORDER;iy++) {
			for (ix=bx1; ix<bx2; ix++) {
				if ((ix == bx1) || (ix == bx2-1)) {
					n = COL_BLACK;	// border pixel black
				} else if (((ix >= bx1+x1) && (ix <= bx1+x2)) || 
				           ((ix >= bx2-x1-1) && (ix <= bx2-x2-1))) {
					n = COL_DARK_GRAY;	// gray fence for moving head guidance
				} else {
					n = COL_LIGHT_GRAY;
				}
				y2 = yp - 1 - iy;
				plt_state.surface[y2*xp + ix] = plt_state.plotter_color_palette[n];
			}
		}
		// draw moving head
		{
            // in first place draw the head box
			int p, ph, col, head_radius = 0;
			for (iy=-32; iy<32; iy++) {
				for (ix=-32; ix<=0; ix++) {
					ph = (iy+32)*64+(ix+32);
					n = HeadAPenBmp[ph];
					if (n == 0) continue;						// outside circle -> continue searching first border dot
					x1 = pen_x + ix;							// hit circle 
					y1 = pen_y + iy;
					y2 = yp - 1 - y1;
					p = y2*xp + x1;
					plt_state.surface[p] = INK_TO_COL(n,0);				// draw it on gui (ink col=0 -> black)
					if (head_radius == 0) head_radius = -iy;	// get the radius of circle being drawn
					while (n=HeadAPenBmp[++ph]) {
						plt_state.surface[++p] = INK_TO_COL(n,0);			// copy rest of border
						ix++;
						if (ix==0) break;
					}
					col = ((head_radius == -iy) || (head_radius == iy)) ? COL_BLACK : COL_WHITE;
					while (ix++<Pen_H_Offset) {					// fill rest of line with color
						plt_state.surface[++p] = plt_state.plotter_color_palette[col];
					}
				}
			}
			// in second place draw vertical head end line
			for (iy=-head_radius; iy<head_radius+1; iy++) {
				x1=pen_x+Pen_H_Offset;
				y1=pen_y+iy;
				y2 = yp - 1 - y1;
				plt_state.surface[y2*xp + x1] = plt_state.plotter_color_palette[COL_BLACK];
			}
            // in third place draw head shadow
			for (iy=-head_radius; iy<head_radius+1; iy++) {
                for (ix=bx2+Arm_Shadow_H_Width;ix<bx2+Arm_Shadow_H_Width*3/2;ix++) {
				    y1=pen_y+iy;
				    y2 = yp - 1 - y1;
                    y2 += Arm_Shadow_H_Width/3; 
                    plotter_get_surface_rgb_color(plt_state.surface[p=(y2*xp + ix)], &rr,&gg,&bb);
                    rr=rr*Shadow_Ammout/100; gg=gg*Shadow_Ammout/100; bb=bb*Shadow_Ammout/100;
		   		    plt_state.surface[p] = plotter_rgb_color(rr,gg,bb);
                }
			}
		}
		// draw the raised/lowered pen
		for (iy=-32; iy<32; iy++) {
			for (ix=-32; ix<32; ix++) {
				x1 = pen_x + ix;
				y1 = pen_y + iy;
				y2 = yp - 1 - y1;
				if (plt_state.Pen == RAISE_PEN) {
					n = Head0PenBmp[(ix+32)*64+(iy+32)];
				} else {
					n = Head1PenBmp[(ix+32)*64+(iy+32)];
				}
				if (n > 0) plt_state.surface[y2*xp + x1] = INK_TO_COL(n,plt_state.PenColor);
			}
		}
		x1 = pen_x;	y1 = pen_y;
		y2 = yp - 1 - y1;
		if (plt_state.Pen == RAISE_PEN) { 
			// pen raised -> put a pen color's pixel at pen position 
			plt_state.surface[y2*xp + x1] = INK_TO_COL(255,plt_state.PenColor);
		} else {
			// pen down -> put a white pixel al pen position in the middle of pen color's circle
			plt_state.surface[y2*xp + x1] = plt_state.plotter_color_palette[COL_WHITE];
		}
	}
	// draw error light
	if (ErrorLightStatus != plt_state.ErrorLight) {
        int ww, hh; 

		ErrorLightStatus = plt_state.ErrorLight;
        x1=ERRLIGHT_H; y1=yp-ERRLIGHT_V; // position of light
        ww=43; hh=22; // size of light
		Paint_box_in_plt_surface(x1, y1, ww, hh, 
			ErrorLightStatus ? COL_ORANGE : COL_DARK_YELLOW); 
        AddSurfaceDirtyRect(1,x1,y1,
                              ww,hh); // add ploter error light to rectlist 

	}
	//update GUI window
#if defined(CPANEL)
    vid_refresh_ex(plt_state.vptr, plt_state.surface, &plt_RectList); 
#else
    vid_draw_window (plt_state.vptr, 0, 0, plt_state.wx, plt_state.wy, plt_state.surface);
    vid_refresh_window (plt_state.vptr);
#endif
	// init inked box to detect where paper was inked (the dirty box) for next refresh
	plt_state.Inked_x1 = plt_state.pp_xsize+1;		plt_state.Inked_y1 = plt_state.pp_ysize+1;
	plt_state.Inked_x2 = -1;				        plt_state.Inked_y2 = -1;

    plt_state.refresh_needed=0; // clear flag because refresh just been done
    if (tnow==0) tnow = sim_os_msec(); 
    plt_state.refresh_tnow = tnow; 

    sim_debug (DBG_DETAIL, &plt_dev, "Refresh done.\n");
}

void plotter_refresh_if_needed(uint32 tnow)
{
    uint32 msec; 

    if (plt_state.FullRedrawNeeded) plt_state.refresh_needed=1; 
    // check if 20 msec has elapsed from last refresh (20 msec -> 50 FPS)
    if (tnow==0) tnow = sim_os_msec(); 
    msec = tnow - plt_state.refresh_tnow; 
    if (msec > (1000/FPS)) {                                    // enought time has pased from previous refresh
        if (plt_state.refresh_needed) {                         // and there is something to draw on gui
            #if defined(CPANEL)
            if (vid_refresh_in_progress(plt_state.vptr) == 0) { // and no previous refresh in progress
                plotter_refresh(tnow);                          // go ahead and redraw surface
            }
            #else
            plotter_refresh(tnow);
            #endif
        }
#if defined(CPANEL)
        // check key pressed
        plotter_check_HotKeys(); 
#endif
    }
}

/* Simulate plotter drawing on paper */

int calc_ink_drop()
{
	// the ink dropped in each point depends on INK_DROP setting and the PenSize
	int ink;
	ink = INK_DROP + (int) ((plt_state.PenSize + 1)*(plt_state.PenSize+1)*2.1);
	if (ink > 255) ink = 255;
	return ink;
}

void ink_paper(int x,int y, int n)
{
	int i, c, n0;
	// add ink to paper at pp x,y coords (2500x1667)
	if ((x < 0) || (y < 0) || (x >= plt_state.pp_xsize) || (y >= plt_state.pp_ysize) || (n <= 0)) return;
	if (plt_state.pp == NULL) return; 
	// get current paper ink and ink color
	n0 = plt_state.pp[i=(y*plt_state.pp_xsize + x)];
	c  = n0 >> 8; n0 = n0 & 255;
	// if ink in paper diferent color from current used ink, enhance intensity to simulate the color blending
	if (c == plt_state.PenColor) {
		n = n0 + n;
	} else {
		n = n0 + (n * 3)/2;
	}
	if (n > 255) n = 255; // prevent too much ink! 
	// store color and ink quty in paper
	plt_state.pp[i] = n + (plt_state.PenColor << 8);
	// expand the inked box to include this inked point
	if (plt_state.Inked_x1 > x) plt_state.Inked_x1 = x;
	if (plt_state.Inked_y1 > y) plt_state.Inked_y1 = y;
	if (plt_state.Inked_x2 < x) plt_state.Inked_x2 = x;
	if (plt_state.Inked_y2 < y) plt_state.Inked_y2 = y;
}

void plot_point_at_paper()
{
	// plot a point in pp array

	int32 x1,y1,x,y,ix,iy,n,ink;

	x1 = HP_TO_PP_X(plt_state.PenX);
	y1 = HP_TO_PP_Y(plt_state.PenY);
	x = H_MARGIN + x1; y = V_MARGIN + y1;

	if (plt_state.PenSize < 1) {
		// no antialiase in PP
		ink_paper(x, y, 255);
	} else {
		ink = calc_ink_drop();
		for (iy=0;iy<64;iy++) {
			for (ix=0;ix<64;ix++) {
				n = PointBmp[ix*64 + iy];
				if (n==0) continue; 
				ink_paper(x-32+ix, y-32+iy, 
					      (ink * n)/256);
			}
		}
	}
	return;
}

void plot_line_slice_at_paper(int x1, int y1)
{
	// plot a line slice in pp array
	int32 x,y, ix, iy;
	int n,ink;

	x = H_MARGIN + x1; y = V_MARGIN + y1;

	if (plt_state.PenSize < 1) {
		// no antialiase
		ink_paper(x, y, 255);
	} else {
		ink = calc_ink_drop();
		for (iy=0;iy<64;iy++) {
			for (ix=0;ix<64;ix++) {
				n = PointBmp[ix*64 + iy];
				if (n==0) continue; 
				ink_paper(x-32+ix, y-32+iy, 
					      (ink*n)/(256*PointBmpRadius));
			}
		}
		return;
	}
}

void plot_line_init()
{
	double dx,dy;
	int x1,x2,y1,y2,adx,ady;

	x1 = HP_TO_PP_X(plt_state.FromX);
	y1 = HP_TO_PP_Y(plt_state.FromY);
	x2 = HP_TO_PP_X(plt_state.GotoX);
	y2 = HP_TO_PP_Y(plt_state.GotoY);

	dx = x2 - x1; adx = abs((int) dx);
	dy = y2 - y1; ady = abs((int) dy);
	plt_state.LineSteps = (adx > ady) ? adx : ady;
	plt_state.x1 = x1;
	plt_state.y1 = y1;
	plt_state.dx = dx / plt_state.LineSteps;
	plt_state.dy = dy / plt_state.LineSteps;
	plt_state.NextStep = 0; 
	plt_state.LineSteps++; // to make sure last point of line is also draw
}

void plot_line(int percent_elapsed) {

	double x,y;
	int LastStep,i; 

	if (percent_elapsed < 100) {
		LastStep = (int) (plt_state.LineSteps * percent_elapsed / 100.0);
	} else {
		LastStep = plt_state.LineSteps;
	}
	if (plt_state.NextStep >= LastStep) return;		// already plotted at this percent, nothing to do
	// do the line from start 
	x = plt_state.x1; 
	y = plt_state.y1; 
	for (i=0; i<LastStep;i++) {
		if (i < plt_state.NextStep) {
			// step already done ... just go to increment x,y
		} else {
			// uodate arm positio (in HP 0..9999 coords), from current pp coords (2500x1667)
			plt_state.PenX = (int) x * 4;
			plt_state.PenY = (int) y * 6;
			if (plt_state.Pen == LOWER_PEN) {
				// pen lowered -> line draw
				plot_line_slice_at_paper((int) x, (int) y);
			}
		}
		x += plt_state.dx; y += plt_state.dy;
	}
	plt_state.NextStep = LastStep; 
    plt_state.refresh_needed = 1;	// because plotting line
}

void plotter_update_arm_pos(uint32 t0, int bFinishAction)
{
	if (plt_state.PlotterAction == 0) return; // action done
	if (plt_state.PlotterAction == -1) return; // no action 
	if (plt_state.PlotterAction == NEW_SHEET) {
		clear_paper_sheet();
		plt_state.FullRedrawNeeded = 1;	  // because programatically received SET NEWSHEET
		plt_state.PlotterAction = 0;      // action done
	} else if ((plt_state.PlotterAction == RAISE_PEN) || (plt_state.PlotterAction == LOWER_PEN)) {
		// set the pen at requested position
		plt_state.Pen = plt_state.PlotterAction; 
		// if pen down, plot a point
        // if pen raise, plot also a point, because on moving pen vertically a bit of ink is set in paper
    	plot_point_at_paper();
		plt_state.PlotterAction = 0; // action done
        plt_state.refresh_needed = 1;	// because raise/lower pen
	} else if (plt_state.PlotterAction == MOVE_ARM) {
		int elapsed;
		// advance plotter arm 
		if (plt_state.LineSteps < 0) {
			// first call - calculate line drawing initial values
            plot_line_init();
		}
		// check % of advance at current t0=sim_os_msec()
		if ((bFinishAction == 0) || (plt_state.MoveStart_msec < t0)) {
			elapsed = t0 - plt_state.MoveStart_msec;					// elapsed time passed from action start
			if (elapsed > 100000) elapsed = 100000;						// sanity check
			elapsed = (uint32) (100 * elapsed / plt_state.MoveDuration_msec); // elapsed in % 0..100 integer
			if (elapsed >= 100) {
				elapsed = 100;
				bFinishAction = 1;
			}
		} else {
			elapsed = 100;
			bFinishAction = 1;
		}
		plot_line(elapsed);
		if (bFinishAction) {							// make sure arm it at line end position
			plt_state.PenX = plt_state.GotoX;
			plt_state.PenY = plt_state.GotoY;
			plt_state.PlotterAction = 0; // action done
		}
        plt_state.refresh_needed = 1;	// because move arm

	}
}

static t_stat plt_refresh_svc (UNIT *uptr) 
{
	uint32 tnow, msec;

    if (plt_state.quit_requested) return SCPE_EXIT; 
    sim_activate(&plt_refresh_unit, REFRESH_INTERVAL);

    tnow=sim_os_msec(); 
    sim_debug (DBG_DETAIL, &plt_dev, "Plotter Timer Tick\n");
	if (plt_state.MoveDuration_msec == 0) {
		sim_debug (DBG_DETAIL, &plt_dev, "Action Finished\n");
		if (plt_state.PlotterAction == MOVE_ARM) {
			// make sure arm is in its end position
			plotter_update_arm_pos(0, 1);
		}
		plt_state.ArmIsMoving = 0;
		// Plotter Action finished. raise the flag to signal plotter ready to HP program
		pltio (&plt_dib, ioENF, 0);	
		// set duration to -1 for as no plotter action
		plt_state.MoveDuration_msec = -1;
	    plt_state.refresh_needed = 1;	// because animation terminated, trailing refresh
	} else if (plt_state.MoveDuration_msec == -1) {
		// No plotter action
    } else {
        // plotter action in progress
	    plt_state.refresh_needed = 1;	// because plotter movement in progress
    	// execution time is simulated even if it is not draw and seen in GUI
        plotter_update_arm_pos(tnow, 0); // increment arm position/change pen state
        // check if action has terminated
        msec = tnow - plt_state.MoveStart_msec; 
        if (msec > 1000000) msec = 1000000; // safety
        if ((int32) msec > plt_state.MoveDuration_msec) plt_state.MoveDuration_msec=0; // action terminated
    }
    plotter_refresh_if_needed(tnow); 
    return SCPE_OK;
}


/* Unit service */

int32 get_from_bin_or_bdc(int d, int bHigh)
{
	int32 n, n1, n2;
	if (plt_state.bdc) {
		n1=d & 0x000F; 
		n2=(d & 0x00F0) >> 4;
		if ((n1 > 9) || (n2 > 9)) {
			// bad BDC digit, return error
			return -1;
		}
		n = n1 + 10 * (n2);
		if (bHigh) n = n * 100;
	} else {
		n = (d & 255);
		if (bHigh) n = n << 8;
	}
	return n;
}

uint32 calc_maneuver_duration(int PlotterAction) 
{
	uint32 d1, d2, duration_msec; 
	int FastMode = plt_state.FastMode;

	if ((PlotterAction == RAISE_PEN) || (PlotterAction == LOWER_PEN)) {
		duration_msec = PLT_PEN_MOVE_MSEC; 						// guessed pen upper/lower maneuever duration in msec
	} else if (PlotterAction == NEW_SHEET) {
		duration_msec = PLT_NEW_SHEET_MSEC;	// time to simulate the paper sheet change 
		FastMode = 0;						// this time is not accelerated by FastMode setting
	} else if (PlotterAction == MOVE_ARM) {
		// calculate vector draw duration (i.e drawing speed)
		// All this is educated guessing based on marketing specs!!!!!!!
		// speed is 17 -> 30.5 cm/sec, depeding on slope. Plot surface is 38.1 x 25.4 cm for 10K x 10K points. 
		// speed long axe: 38.1/30.5 = 1249 msec for 10K point X axis
		// speed short axe: 25.4/17 = 1494 msec for 10K point Y axis
		d1 = (uint32) (1000 * (38.1/(PLT_SPEED_MULT * 30.5)) * ((double)abs(plt_state.PenX - plt_state.GotoX)/10000.0));
		d2 = (uint32) (1000 * (25.4/(PLT_SPEED_MULT * 17  )) * ((double)abs(plt_state.PenY - plt_state.GotoY)/10000.0));
		// get the longest duration plus a fixed time for servos accel/decel and internal processing
		duration_msec = (d1 > d2) ? d1 : d2 + PLT_ARM_ACCEL_MSEC;
		// After paper standby/powerup the plotter works at low speed for 1.2 seconds while it is calibrating
		// the arm position (clamping)
		if (plt_state.CalibrationDone == 0) {
			if (plt_state.CalibrationRemains_msec == 0) {
				plt_state.CalibrationRemains_msec = 1200; 
			} 
			d1 = plt_state.CalibrationRemains_msec;
			d2 = (uint32) (duration_msec * PLT_LOW_SPEED_MULT);		// operation at low speed 
			if (d2 == 0) d2 = 1;						// sanity check
			if (d1 > d2) {
				// calibration time not expired
				plt_state.CalibrationRemains_msec -= d2;
				duration_msec = d2;
			} else {
				// calibration time expiring during vector draw. 
				plt_state.CalibrationRemains_msec = 0;
				duration_msec = d1 + (uint32) ((d2-d1) / PLT_LOW_SPEED_MULT);
				plt_state.CalibrationDone = 1;
			}
		}
	} else {
		return 0; // unknow action 
	}
	if (FastMode > 0) {
		// fast mode -> maneuvers stands for 1 msec, cancels any calibration in progress
		plt_state.CalibrationDone = 1;
		duration_msec=1;
	}
	if (duration_msec == 0) duration_msec=1;	// sanity check
	return duration_msec;
}

void plotter_init_maneuver(int PlotterAction, int PenGotoX, int PenGotoY)
{
	uint32 duration_msec; 

	if (plt_state.ArmIsMoving) {
		// there is a previous maneuver in progress not finished yet. Just ignore this new one
		return;
	}

	if (PlotterAction == NEW_SHEET) {
		sim_debug (DBG_CMDS, &plt_dev, "Put New Blank Sheet in Plotter Action\n");
		duration_msec = calc_maneuver_duration(NEW_SHEET);	
		plt_state.PlotterAction = NEW_SHEET;
	} else if (PlotterAction == LOWER_PEN) {
		if (plt_state.Pen == LOWER_PEN) {
			sim_debug (DBG_CMDS, &plt_dev, "Lower Pen (pen already down)\n");
			duration_msec = 0;
			plt_state.PlotterAction = -1;
		} else {
			sim_debug (DBG_CMDS, &plt_dev, "Lower Pen\n");
			duration_msec = calc_maneuver_duration(LOWER_PEN);	
			plt_state.PlotterAction = LOWER_PEN;
		}
	} else if (PlotterAction == RAISE_PEN) {
		if (plt_state.Pen == RAISE_PEN) {
			sim_debug (DBG_CMDS, &plt_dev, "Raise Pen (pen already up)\n");
			duration_msec = 0;
			plt_state.PlotterAction = -1;
		} else {
			sim_debug (DBG_CMDS, &plt_dev, "Raise Pen\n");
			duration_msec = calc_maneuver_duration(RAISE_PEN);	
			plt_state.PlotterAction = RAISE_PEN;
		}
	} else if (PlotterAction == MOVE_ARM) {
		if ((plt_state.PenX == PenGotoX) && (plt_state.PenY == PenGotoY)) {
			sim_debug (DBG_CMDS, &plt_dev, "Move from (%d,%d) to (%d,%d) (same)\n", plt_state.PenX, plt_state.PenY, PenGotoX, PenGotoY);
			plt_state.PlotterAction = -1;
			duration_msec = 0;
		} else {
			sim_debug (DBG_CMDS, &plt_dev, "Move from (%d,%d) to (%d,%d)\n", plt_state.PenX, plt_state.PenY, PenGotoX, PenGotoY);
			plt_state.FromX = plt_state.PenX;
			plt_state.FromY = plt_state.PenY;
			plt_state.GotoX = PenGotoX;
			plt_state.GotoY = PenGotoY;
			plt_state.LineSteps = -1;							// to signal need to calculate initial values
			duration_msec = calc_maneuver_duration(MOVE_ARM);	// needs values for PenGoto set in plt_state
			plt_state.PlotterAction = MOVE_ARM;
		}
	} else {
		sim_debug (DBG_CMDS, &plt_dev, "invalid plotter maneuver requested: %d\n", PlotterAction);
		fprintf(stderr, "invalid plotter maneuver requested\r\n");
		return;
	}
	sim_debug (DBG_CMDS, &plt_dev, "Action Duration %d msec\n", duration_msec);
    plt_state.ArmIsMoving = 1;
	plt_state.MoveStart_msec = sim_os_msec(); // set starting time (wallclock) for arm movement sim
	plt_state.MoveDuration_msec = duration_msec;
	// Start timer. timer is tied to the fact that arm is moving, not to the fact that GUI window is open (and needs refresh 
	// as it occurs in Scope emulation). 
}

t_stat plt_svc (UNIT *uptr) 
{
	int d = plt_unit.buf;
	int error = 0;
	int n;
	int bNewSheetHack = 0;
	int bSignalReady = 0;

   if (plt_state.quit_requested) return SCPE_EXIT; 

    // decode data sent to plotter
	if (plt_state.ArmIsMoving) {
		// ignore command. Plotter is busy
	} else if (d & PLT_CL_SYC) { // data is pass 1
		plt_state.pass = 1;
		plt_state.bdc = 0;
		plt_state.Xreq = 0;
		plt_state.Yreq = 0;
		if (d & PLT_CL_MVR) { 
			// Pen Maneuver
			plotter_init_maneuver( (d & PLT_CL_PNC) ? LOWER_PEN : RAISE_PEN, 0, 0);
			plt_state.pass = 0; // no more passes
			// no need to signal ready. It will be done al end of maneuver execution
		} else {
			// Position maneuver
			plt_state.bdc = (d & PLT_CL_CDE) ? 0 : 1; // data binary or bdc encoding?
			plt_state.Xreq = (n=get_from_bin_or_bdc(d, 1));	// Coord X, Hi part
			if (n < 0) {
				plt_state.Xreq = 0; error = 1;
				sim_debug (DBG_DATA, &plt_dev, "Error in coord\n");
				// no need to signal ready. It will be done al end of error manuever execution
			} else {
				plt_state.pass = 2;	// next data received is for pass 2
				bSignalReady = 1;	// ok, go on to receive next pass
			}
		}
	} else if (plt_state.pass == 2) {
		plt_state.Xreq += (n=get_from_bin_or_bdc(d, 0)); 	// Coord X, Lo part
		if (n < 0) {
			plt_state.Xreq = 0; error = 1;
			sim_debug (DBG_DATA, &plt_dev, "Error in coord\n");
			// no need to signal ready. It will be done al end of error manuever execution
		} else {
			plt_state.pass = 3;	// next data received is for pass 3
			bSignalReady = 1;	// ok, go on to receive next pass
		}
	} else if (plt_state.pass == 3) {
		plt_state.Yreq = (n=get_from_bin_or_bdc(d, 1));	// Coord Y, Hi part
		if (n < 0) {
			plt_state.Yreq = 0; error = 1;
			sim_debug (DBG_DATA, &plt_dev, "Error in coord\n");
		} else {
			plt_state.pass = 4;	// next data received is for pass 4
			bSignalReady = 1;	// ok, go on to receive next pass
		}
	} else if (plt_state.pass == 4) {
		plt_state.Yreq += (n=get_from_bin_or_bdc(d, 0)); 	// Coord Y, Lo part 
		if (n < 0) {
			plt_state.Yreq = 0; error = 1;
			sim_debug (DBG_DATA, &plt_dev, "Error in coord\n");
		} else {
			// process pass 4 flags
			if (d & PLT_CL_MDE) { // Mode: coords received are 0=Absolute or 1=Relative
				// mode relative. apply sign
				if (plt_state.bdc) {
					// mode relative and bdc. check sing indicator
					if (d & PLT_CL_DXS) plt_state.Xreq = -plt_state.Xreq;
					if (d & PLT_CL_DYS) plt_state.Yreq = -plt_state.Yreq;
				} else {
					// mode relative and binary -> 2's complement
					if (plt_state.Xreq & 0x8000) plt_state.Xreq = plt_state.Xreq - 65536;
					if (plt_state.Yreq & 0x8000) plt_state.Yreq = plt_state.Yreq - 65536;
				}
				// SimH Hack. Check if paper change
				if ((plt_state.PenX ==  1) && (plt_state.PenY == 2) && 
					(plt_state.Xreq == -3) && (plt_state.Yreq == -4)) {
					// Asking the arm to go to relative pos -3,-4 when in pos 1,2 simulates 
					// paper sheet change for a new one
					bNewSheetHack = 1;
					// setup for no move and refresh the whole paper sheet 
					plt_state.Xreq = plt_state.Yreq = 0;
					sim_debug (DBG_CMDS, &plt_dev, "New paper sheet hack\n");
				}
				// check relative coords are in valid range
				n = plt_state.PenX + plt_state.Xreq;
				if ((n < 0) || (n > 9999)) { error = 1; } else plt_state.Xreq = n; 
				n = plt_state.PenY + plt_state.Yreq;
				if ((n < 0) || (n > 9999)) { error = 1; } else plt_state.Yreq = n; 
				if (error) {
					sim_debug (DBG_DATA, &plt_dev, "Relative coord sets pen out of range\n");
				}
			} else {
				// absolute mode
				if ((plt_state.Xreq > 9999) || (plt_state.Yreq > 9999)) {
					error = 1; // invalid coord request
					sim_debug (DBG_DATA, &plt_dev, "Absolute coord out of range\n");
				}
			}
			if (bNewSheetHack) {
				plotter_init_maneuver(NEW_SHEET, 0, 0);	
			} else if (error == 0) {
				plotter_init_maneuver(MOVE_ARM, plt_state.Xreq, plt_state.Yreq);
			}
		}
		plt_state.pass = 0; // no more passes
	} else {
		// error received data out of pass sequence
		error = 1;
		sim_debug (DBG_DATA, &plt_dev, "Data out of pass sequence\n");
	}
	if (error) {
		// error in processing plotter data -> lit error in plotter front pannel, lift pen
		sim_debug (DBG_CMDS, &plt_dev, "Error Sequence\n");
		plotter_init_maneuver( RAISE_PEN, 0, 0);
		plt_state.pass = 0; // reset passes count
		//if not already done, turn on error light in plotter control panel, and force refresh to be sure it's drawn on GUI
	}
	plt_state.ErrorLight=error;

	/* set flag to signal Plotter ready to receive next pass data (and to raise an interrupt to signal it) 
	   if plotter arm is moving (executing a previous command) then do not send yet ioENF.
	   ioENF will be sent at end of plotter moving command execution  
	*/
	if (bSignalReady) {
		pltio (&plt_dib, ioENF, 0);	
	}

    return SCPE_OK;
}

/* I/O signal handler */

uint32 pltio (DIB *dibptr, IOCYCLE signal_set, uint32 stat_data)
{
IOSIGNAL signal;
IOCYCLE  working_set = IOADDSIR (signal_set);            /* add ioSIR if needed */

int d;

while (working_set) {
	signal = IONEXT (working_set);                       /* isolate next signal */

    switch (signal) {                                    /* dispatch I/O signal */

        case ioCLF:                                      /* clear flag flip-flop */
            plt.flag = plt.flagbuf = CLEAR;
			sim_debug (DBG_FLAGS, &plt_dev, "ioCLF\n");
            break;


        case ioSTF:                                      /* set flag flip-flop */
		case ioENF:                                      /* enable flag */
            plt.flag = plt.flagbuf = SET;
			sim_debug (DBG_FLAGS, &plt_dev, "ioENF\n");

            break;


        case ioSFC:                                      /* skip if flag is clear */
            setstdSKF (plt);
            break;


        case ioSFS:                                      /* skip if flag is set */
            setstdSKF (plt);
            break;


        case ioIOI:                                      /* I/O data input */
            d = (plt_state.Pen == LOWER_PEN) ? PLT_SL_PEN : 0;
            stat_data = IORETURN (SCPE_OK, d);           /* merge in return status */  
            break;


        case ioIOO:									 	/* I/O data output to interface to Plotter */
			plotter_CheckAutoPowerOn(); 
            plt_unit.buf = d = IODATA (stat_data) & (0177777);	
			sim_debug (DBG_DATA, &plt_dev, "Send data: $%02x %02x\n", d >> 8, d & 255);
            break;


        case ioPOPIO:                                    /* power-on preset to I/O */
            plt.flag = plt.flagbuf = SET;                /* set flag and flag buffer */
            plt_unit.buf = 0;                            /* clear output buffer */
            break;

        case ioCRS:                                      /* control reset */
        case ioCLC:                                      /* clear control flip-flop */
            plt.control = CLEAR;						 /* set DMA mode */
			sim_debug (DBG_FLAGS, &plt_dev, "ioCLC\n");
            break;


        case ioSTC:                                      /* set control flip-flop */
			plotter_CheckAutoPowerOn(); 
            plt.control = SET;							 /* set programmed IO or Interrupt IO */
			sim_debug (DBG_FLAGS, &plt_dev, "ioSTC\n");
			sim_activate (&plt_unit, 10);			     /* schedule op */
            break;


        case ioSIR:                                      /* set interrupt request */
            setstdPRL (plt);                             /* set standard PRL signal */
            setstdIRQ (plt);                             /* set standard IRQ signal */
            setstdSRQ (plt);                             /* set standard SRQ signal */
            break;


        case ioIAK:                                     /* interrupt acknowledge */
            plt.flagbuf = CLEAR;
            break;


        default:                                        /* all other signals */
            break;                                      /*   are ignored */
        }

    working_set = working_set & ~signal;                /* remove current signal from set */
    }

return stat_data;
}


#if defined(CPANEL)
t_stat plotter_save_image(char * filename) 
{
    int x,y,col,n,nInk; 
    uint32 * savepng_surface; 
    t_stat stat; 

    // save plotted paper sheet as a png using attached filename
    /* generate surface from pp data */
    savepng_surface = (uint32 *)malloc (plt_state.pp_xsize * plt_state.pp_ysize * sizeof(uint32));
    if (savepng_surface == NULL) return SCPE_IERR;

    for (x=0;x<plt_state.pp_xsize;x++) {
	    for(y=0;y<plt_state.pp_ysize;y++) {
		    n = plt_state.pp[y*plt_state.pp_xsize + x];						     // get pp data: 3 bit color + 8 bit intensity
            col = ((n >> 8) & 3);   // color
			nInk = (n & 255);	    // amount of ink
		    savepng_surface[(plt_state.pp_ysize - y - 1)*plt_state.pp_xsize + x] = INK_TO_COL(nInk, col);  // invert image: in the saved file (0,0) is upper left corner, but pp (0,0) is lower left corner
        }
	}
    stat = write_png_file(filename, plt_state.pp_xsize, plt_state.pp_ysize, savepng_surface);
    free(savepng_surface); 
    return stat; 
}

// Hotkey processing
void plotter_check_HotKeys(void)
{
    int c = vid_keyb.KeyPress; // key currently being pressed

    if (c==0) {
        // no key pressed on keyboard 
        // return initial fast mode selected by user
        plt_state.FastMode=plt_state.InitialFastMode; 
        return; 
    }
    if (vid_keyb.vptr!=plt_state.vptr) return; // key pressed but another window has the focus

    if (c == 'f'-'a'+1) { // Control-F (^F) is being pressed
        if (plt_state.FastMode == 0) { 
            // accelerate plotter to max speed while keydown
            plt_state.FastMode=1; 
        	// if fastmode set, speed up any in progress movement and any calibration done
			if (plt_state.MoveDuration_msec > 1) plt_state.MoveDuration_msec = 1; 
			plt_state.CalibrationDone = 1;
        }
    }
    c = vid_keyb.LastKeyPress; // last key that was pressed
    if (c == 'p'-'a'+1) { // Control-P (^P) was pressed
        // save the current sheet in file "Plotter_Image_NN.png"
        // check if a saved image file already exists
        int n; 
        FILE *f; 
        char fname[80];

        vid_keyb.LastKeyPress=0; // Clear it as being processed
        for (n=0; n<99; n++) {
            sprintf(fname, "Plotter_Image_%02d.png", n);
            f = sim_fopen (fname, "r");
            if (f) {
                fclose (f); // file already exists! close if and try next one
                fname[0]=0;   // invalidate filename
                continue; 
            }
            break; 
        } 
        if (fname[0]==0) {
            // no filename available
            return; 
        }
        // buf contains the filename to save image into
        plotter_save_image(fname);       
        return;
    }
}
#endif

// SCP functions

t_stat plt_reset (DEVICE *dptr)
{
    sim_debug (DBG_CMDS, &plt_dev, "Plotter Reset\n");
    IOPRESET (&plt_dib);                                    /* PRESET device (does not use PON) */
    plt_state.ErrorLight=0;
    plt_state.CalibrationDone=0;
    plt_state.pass=0;
    plt_state.ArmIsMoving=0;
    plt_state.MoveDuration_msec=0;
    plt_state.PlotterAction=0;
    sim_cancel (&plt_unit);             /* deactivate unit */
    // boot & run SCP commands issues an implicit reset on SCP. In this reset first cancels
    // any schedulled device event and then calls each device reset routine (this routine)
    // se here we must re-schedulle the refresh event
    if (plt_state.power) {
        sim_cancel(&plt_refresh_unit); 
        sim_activate(&plt_refresh_unit, REFRESH_INTERVAL);
    }
    return SCPE_OK;
}

t_stat plt_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc)  { 
    return power_plotter(value ? 1:0); 
}

t_stat plt_set_sheet(UNIT *uptr, int32 value, CONST char *cptr, void *desc) 
{ 
    if (value == 0) { // SET NEWSHEET
	    clear_paper_sheet();
	    plt_state.FullRedrawNeeded = 1;						// because SET NEWSHEET
	    plotter_refresh(0);									// refresh now, because SET NEWSHEET
    } else if (value == 1) { // SET SAVE "filename"
        // only available with cpanels
        if (cptr == NULL) return SCPE_ARG;
#if defined(CPANEL)
        return plotter_save_image((char *) cptr);
#else
        return sim_messagef(SCPE_NOFNC, "Only available with CPANELs\r\n");
#endif
    }
	return SCPE_OK; 
}

t_stat plt_set_pen_color(UNIT *uptr, int32 value, CONST char *cptr, void *desc) { 

	if ((value < 0) || (value > 3)) value = 1;
	plt_state.PenColor = value; 
    // no need a full refresh, regular refresh will suficce to update color of mounted pen in plotter arm
	plt_state.refresh_needed = 1;	// because SET PEN COLOR 
    plotter_refresh(0);				// refresh now, because SET PEN COLOR
	return SCPE_OK; 
}

t_stat plt_set_param(UNIT *uptr, int32 value, CONST char *cptr, void *desc) { 

	DEVICE *dptr = (DEVICE *) desc;
	int32 num;
	t_stat r;

	if (cptr == NULL) return SCPE_ARG;
	if (desc == NULL) return SCPE_IERR;

	if (value == 2) { // SET PENSIZE= uint 0..6
		/* 0=pixel exact, no antialiasing, 1..6 biggest pen size */
		num = (int32) get_uint (cptr, 10, 6, &r);
		if (r != SCPE_OK) return r;
      	plt_state.FullRedrawNeeded = 1;		// because leaving SET PENSIZE (pen bmp is build in fullrefresh)
		plt_state.PenSize = num; 
        return SCPE_OK;
	} else if (value == 3) { // SET FAST=0|1|2
		/* =0 normal speed, =1 fast speed, =2 fast and do not render plotter arm */
		num = (int32) get_uint (cptr, 10, 2, &r);
		if (r != SCPE_OK) return r;
        if (plt_state.InitialFastMode == num) return SCPE_OK; 
        if (plt_state.InitialFastMode == 2) {
        	plt_state.FullRedrawNeeded = 1;		// because leaving SET FAST=2
	        plotter_refresh(0);					// refresh now, because leaving SET FAST=2
        }
  		plt_state.FastMode =plt_state.InitialFastMode = num; 
        return SCPE_OK;
#if defined(CPANEL)
    } else if (value == 0) { // SET POS=X/Y 
        int xPos, yPos; 
        CONST char *tptr;
        xPos = (int) strtotsv (cptr, &tptr, 0);
        if (cptr == tptr) return SCPE_ARG;
        if (*tptr != '/') return SCPE_ARG;
        yPos = (int) strtotsv (++tptr, &tptr, 0);
        if (cptr == tptr) return SCPE_ARG;
        if (plt_state.power == 0) {
            plt_state.InitialPosX=xPos; 
            plt_state.InitialPosY=yPos; 
        } else {
            vid_SetWindowSizeAndPos(plt_state.vptr, SIM_SETWIN_POS ,xPos,yPos);
        }
        return SCPE_OK; // no panel opened
    } else if (value == 1) { // SET SCALE=10..200 
        int Scale; 
        num = (int32) get_uint (cptr, 10, 200, &r);
        if (r != SCPE_OK) return r;
        // resize window to set the new scale
        if (num <  10) {Scale= 10; } // max scale range allowed: 10%..200%
        else if (num > 200) {Scale=200; }
        else Scale=num; 
        if (plt_state.power == 0) {
            plt_state.InitialScale=Scale; 
        } else {
            vid_SetWindowSizeAndPos(plt_state.vptr, SIM_SETWIN_SCALE, Scale, 0); // resize GUI window
        }
        return SCPE_OK;
#endif
    } else {
		// undefined SET 
		return SCPE_IERR;
	}
	return SCPE_OK;
}

t_stat plt_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    plt_state.AutoPowerOn=value; 
	return SCPE_OK;
}

t_stat plt_show (FILE *st, UNIT *uptr, int32 num, CONST void *desc) {

	switch(plt_state.PenColor) {
		case 0: fprintf (st, "Black Pen, "); break;
		case 1: fprintf (st, "Blue Pen, "); break;
		case 2: fprintf (st, "Red Pen, "); break;
		case 3: fprintf (st, "Green Pen, "); break;
		default: fprintf (st, "Undef Pen Color, ");
	}
	fprintf (st, "Pen Size=%d, ", plt_state.PenSize);

	fprintf (st, "\n\r        ");
	fprintf (st, "Fast Mode=%d", plt_state.InitialFastMode);
	return SCPE_OK;
}


