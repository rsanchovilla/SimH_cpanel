/* hp2100_scope.c: HP2100 scope simulator

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

   scope			HP13XX Scope Display attached to either
                    - 12555 Digital to Analog Interface Card (DAC), 
                         or
                    - 12531C buffered teleprinter interface (rs-232-c interface) + 
                      HP1350A/HP1351A Graphics Translator

   Reference:
   - 12555B Operating and Service Manual (12555-90063, Mar-1973)
   - 12531C Buffered Teleprinter Interface Kit Operating and Service Manual (12531-90033, Nov-1972)
   - HP1350A Graphics Translator (01350-90908, Jan-1980)
   - HP1351A Graphics Translator (01351-90805, Oct-1981)

   Scope displays simulated

      HP1332 Display Scope, blue phosphor
      HP1300 Display Scope, green phosphor: 
         simulate scope connected to DAC interface card
         refreshing the scope image is done actively by HP2100 cpu.
         assume a target rate on scope of 50 images/sec to avoid flickering 
         this means 20 msec to draw the image on scope
         HP2116: 1,6 uS cpu cycle, transfer loop of 7 instructions  
		                           -> approx 1780 points can be sent in 20 msec
	     Using DMA (3 instructions per transfer) -> 4140 points can be 
		                           transfered in 20 msec

      HP1331 Storage Display, ambar phosphor
         simulate scope connected to DAC interface card
         Spots in sotrage tube does not disapeard, son no refresh from CPU is needed

      HP1304 non-storage display, green phosphor, 13 inch 
         simulate 1304 Analog Display connected to HP1350/HP1351 Graphics Translator, in turn connected 
         to RS232C card in HP2100

   
   DAC & Scope Interface HW usage
   
      CLF intrution trigger the display erase on storage devices 
	      also starts the 20 msec DAC internal timer 

      OTA   bits 0-7		X coord		draw a spot in scope at x,y
            bits 8-15		Y coord
		    (0,0) is lower left corner

      Flag is set by DAC Timer each 20 mesc for ask the computer for data to 
	  redraw display on non-storage scopes

   These simulator commands controls the interface

      SET SCOPE ON 
	  SET SCOPE POWERON     --> power on scope display (opens GUI Simulator 
                                Window for scope image display). If the window
								is already open, refreshes its contents 
								to reflect any changes (e.g. color setting
								changes)
      SET SCOPE OFF        
	  SET SCOPE POWEROFF    --> power off scope display (closes the scope 
                                simulator GUI Window)
   
      SET SCOPE AUTOON      --> first CPU instr for scope powers on the device
                                (AutoPowerOn). This is the default
      SET SCOPE NOAUTOON    --> Must issue a SET SCOPE ON to power the device

	  SET SCOPE STORAGE     --> sets the simulated display to storage scope
                                attached to a DAC interface card. 
	                            Should not be used under interrupts
								as CLF erases the scope screen (and does a 
								nice flash). 
                                Clears the scope image

      SET SCOPE NOSTORAGE   --> sets the simulated display to non-storage 
	                            scope attached to a DAC interface card. 
                                Clears the scope image

      SET SCOPE GRAPHICS_TRANSLATOR --> sets the simulated display to non-storage 
	                            scope attached to a HP1350A Graphics Translator
                                in turn attached to a RS232C interface card. 
                                Clears the scope image.

      SET SCOPE FOCUS=n     --> sets the beam spot size. Values from 0 to 63
	                            0  = minimal narrow beam spot
								63 = wide spot defocused

	  SET SCOPE INTENSITY=n --> sets the intensity of the scope beam. 
                                values from 0 to 255
	                            0  = minimal intensity (not visible)
								255 = max intensity (overburn)

      SET SCOPE PERSISTENCE=n 
							--> sets the scope phosphor persistance: the time
							    in milliseconds for a spot to 
                                disappear in screen or to stop decaying (
                                if SET STORAGE has been issued) 
                                values from 10 to 100000 (100 sec)

      SET SCOPE COLOR=r_BO/g_BO/b_BO/r_BK/g_BK/b_BK
	                        --> sets the scope colors for electron beam spot 
                                and unilluminated scope tube background. 
                                Values expected are red, green and blue. 
                                Values are in 0 to 255 range. Must
								provide up to 6 vales separated by /
								If less are given, a value of zero is assumed

      SET SCOPE HP1300      --> sets the simulated display to HP-1300 non-
	                            storage scope, dark green background and light 
								green beam. Set all parameter including persistance 
								and focus. Simulates scope attached to a DAC 
                                interface card, Clears the scope image. 

      SET SCOPE HP1331      --> sets the simulated display to HP-1331 storage
	                            scope, amber background and light orange beam.
								Set all parameter including persistance and 
								focus. Simulates storage scope attached to a DAC 
                                interface card. Clears the scope image. 

      SET SCOPE HP1332      --> sets the simulated display to HP-1332 non-
	                            storage scope, blue background and light blue 
								beam. Set all parameter including persistance 
								and focus. Simulates scope attached to a DAC 
                                interface card. Clears the scope image. 

      SET SCOPE HP1304      --> sets the simulated display to HP-1304 non-
	  SET SCOPE HP1350          storage display, black background and green 
      SET SCOPE HP1351          beam. Set all parameter including persistance 
								and focus. Simulates display attached to a 
                                HP1350A or HP1351 Graphics Translator, in 
                                turn attached to RS232C interface card. 
                                Clears the scope image. 

     If Simulator is compiled with CPANEL symbol active, then the following SCP commands 
     are available

     SET SCOPE SCALE=n       --> Sets the GUI window scale. Default is 100.
                                 Use 50 to have a GUI window of half size. 
                                 Use 200 to have GUI window at double size. 
                                 Scale value n ranges from 10 to 200.
                                 If issued before first SET SCOPE ON, sets the 
                                 initial scale to be used on window creation

     SET SCOPE POS=x/y       --> Sets the GUI window position at coord x,y
                                 (can be negative values)
                                 If ussed before first SET SCOPE ON, sets 
                                 the initial scale to be used on window creation

     If Simulator is compiled with CPANEL symbol, then the following features are available:

     1) Hotkeys active when windows has the focus:

        +   -                    Zoom In/Zoom Out: increases/decreases window size by 10%
        Control +, Control -     Zoom In/Zoom Out: increases/decreases window size by 1%
        Control Y                Toggle zoom 100% <-> 50%
        Control I                Toggle Information Panel (^I is same as TAB key)
        i   I                    'i' key (unshifted) Increases Intensity of simulated scope 
                                 electron beam. Shift-I key decreases Intensity
                                 It is equivalent to SET INTENSITY command
        f   F                    'f' key (unshifted) widens the focus of simulated scope 
                                 electron beam. Shift-F key narrows the focus
                                 It is equivalent to SET FOCUS command
        p   P                    'p' key (unshifted) increases the persistence time of 
                                 simulated scope. Shift-P key decreases 
                                 It is equivalent to SET PERSISTENCE command
        e                        'e' key only available on storage scope simulation. 
                                 flashes the tube to erase any visible pixels 

     2) Click on window image and drag mouse to move the window
     3) Right click mouse on window image to show a tooltip popup with image at 100% scale
     4) On right side of scope image, 3 knobs will be displayed: Intensity, Focus and Persistance
        These shows the current setting for these values. When knob mark is pointing to the left, 
        the value is at its allowed minimum. When knob mark is pointing to the right, the value 
        is at its allowed maximum. Click with mouse on left half of knob to turn it to left (and 
        decrease its value), Click with mouse on right half of knob to turn it to riht (and increase 
        its value)
     5) For storage scope tubes, on bottom-right side of scope image, an erase button will be
        displayed. Click it with mouse to flash the tube and erase any visible pixel

	  defaults: SET SCOPE DIS, OFF, NOSTORAGE, FOCUS=2, PERSISTENCE=100, AUTOON,
	                INTENSITY=128, COLOR=131/249/143/52/62/36

   Usage notes:

	 To use the scope, these SCP commands must be issued

		set scope dev=nnn      <-- to set the Select Code for the interface Card to nnn octal (defaults to 46)
		set scope nostorage    <-- sets scope whatever settings wanted
		...
		set scope ena          <-- enables the interface card (DAC or RS232C), so HP CPU can talk to it
		set scope on           <-- makes visible the Scope GUI Window
		...
		run hp program who displays nice stuff
		...
		set scope off          <-- closes the GUI window
		set scope dis      
		
	  When simulated HP CPU is running executing code, clicking on the close button 
      of GUI windows closes the emulator. 
      To just close the device GUI window, use the SCP command SET SCOPE OFF

      Case 1: Simulator compiled with CPANELS in windows platform:
	     If CPU is stopped, GUI window is refreshed even if simulator shows the 
         sim> prompt on console. The Window close button will close the emulator
         program.

      Case 2: Simulator compiled without CPANELS or in non-windows platform (linux, 
              mac, any other):
	     If CPU is stopped, GUI window is not refreshed. During step by step debugging
	     of any HP program that sends commands to this device, nothing will be updated
	     on GUI window. Issue a SET SCOPE ON at any moment to refresh the GUI window
	     and see the results.

         If CPU is stopped, the GUI close button does nothing when clicked. It will
         close the simulation when cpu is started again.

*/

#include <math.h>
#include "hp2100_defs.h"
#include "sim_video.h" 
#if defined(CPANEL)
#include "cpanel_vid.h"  // needed to sense mouse events in scope GUI window
#endif

#if defined(USE_SIM_VIDEO) && defined(HAVE_LIBSDL)
// ok, both preprocessor vars are defined
#else
#error hp2100_scope.c needs the preprocessor symbol USE_SIM_VIDEO and HAVE_LIBSDL to be defined
#endif

#define UNIT_V_POWERON  (UNIT_V_UF + 0)                 /* unit powered on */
#define UNIT_POWERON    (1 << UNIT_V_POWERON)

struct {
    FLIP_FLOP control;                                  /* control flip-flop */
    FLIP_FLOP flag;                                     /* flag flip-flop */
    FLIP_FLOP flagbuf;                                  /* flag buffer flip-flop */
    } scope = { CLEAR, CLEAR, CLEAR };

#define FPS				         30			// update GUI window at this Frames Per Second rate 
#define REFRESH_INTERVAL       1000         // check if refresh needed each these CPU instr 

#define DECAY_STEP_MSEC          10         // calculate decay each DECAY_STEP_MSEC milliseconds
#define BULK_ERASE_MSEC         500         // msec needed to perform a bulk erase
/* 
Refresh strategy:

- If program spots a point into the scope, or issue a bulk erase
  then set refresh_needed flag=1
- refresh_svc() is shedulled with sim_activate() to be executed
  each REFRESH_INTERVAL cpu instructions
- refresh_svc() checks 
     - Has at least decay_step_msec elapsed from previous call to decay routine? 
           yes, call decay_image_in_scope()
     - is DAC timer active and has 20msec elapsed? 
           yes, generate a DAC interrupt. Note: as 20msec are wallclock measured, 
           the HP program will perform at same rate as in real hw (just less flicker)
     - Screen should be refreshed? (ie. refresh_needed!=0 ?) -> no, exit
     - Has at least 1000/FPS msec elapsed from previous refresh? -> no, exit
     - Clears flag: refresh_needed=0
     - If using CPANELs, builds the rectagle list of surface areas to update
           if FullrefreshNeed -> will update the whole surface
           If not using CPANELs will update the whole surface allways
     - Send SDL2 surface to GUI window for repainting
     - Check user pressed hotkeys (and process it if any. Also sense mouse clicks)

- if SCP command makes any changes in appearence of screen
     - SET POWERON to open/refresh the GUI window, 
     - SET COLOR to change color of scope beam and backgound
     - SET MODEL to select eiher HP1300A x-y scope display, HP1331 or HP-1332 storage scope
     then Send the whole SDL2 surface to GUI window for repainting
*/ 

// internal refresh device & unit
// all declared as static so same declaration on all devices is possible (will not have confict names)
static t_stat scope_refresh_svc (UNIT *ptr);
static const char *scope_refresh_description (DEVICE *dptr) {
    return "Scope GUI Window SDL2 Refresh facility";
}
static UNIT scope_refresh_unit = { UDATA (&scope_refresh_svc, UNIT_IDLE, 0) };
static DEVICE scope_refresh_dev = {
    "INT-REFRESH-SCOPE", &scope_refresh_unit, NULL, NULL,  // the name INT-REFRESH-xxx
    1, 0, 0, 0, 0, 0,                                      // allows cpanel to call the 
    NULL, NULL, NULL, NULL, NULL, NULL,                    // device refresh routine refresh_svc
    NULL, DEV_NOSAVE, 0,                                   // at sim> prompt (if windows os) or
    NULL, NULL, NULL, NULL, NULL, NULL,                    // while SET CPANEL INTERACTIVE
    scope_refresh_description
};

#define H_SCO_SIZE      800     // the image in scope is simulated using a 800x600 bitmap image
#define V_SCO_SIZE		600     

#define V_MARGIN		50      // horizontal and vertical margins of image in GUI 
#define H_MARGIN		60      // window (just to looks better)

#if defined(CPANEL)
#define H_KNOB          120     // width of control knob area
#else
#define H_KNOB          H_MARGIN     // No knob area, just same margin as on left side
#endif

#define H_SIZE			(H_MARGIN + H_SCO_SIZE + H_KNOB)    // actual size of GUI window, including
#define V_SIZE			(V_MARGIN + V_SCO_SIZE + V_MARGIN)  // margins

#define H_GRID_SIZE     ((H_SCO_SIZE + 15)>>4)              // number of cells of 16x16 pixels in
#define V_GRID_SIZE     ((V_SCO_SIZE + 15)>>4)              // sco_energy arrays

#if defined(CPANEL)
#define INITIAL_SCALE           100         // if using CPANELs, default scale for window creation
#endif

#define H_SHOWINFO_COLS         80          // columns in info panel
#define H_SHOWINFO_LINES         3          // lines in info panel


#define SCO_TYPE_IS_NOSTORAGE   0
#define SCO_TYPE_IS_STORAGE     1
#define SCO_TYPE_IS_VECTOR      2           // Graphics Translator on non-storage scope (HP1350A or HP1351A)

extern int HP1351_flag;    

static struct {
	// scope settings
	int type;                               // =0 -> non storage device, need refresh each frame. =1 -> storage scope (keeps each pixel at nondecay energy level), =2 -> vector device HP1350/1351
	int Intensity; 		                    // beam intensity
	int Focus; 		                        // beam focus
	int Persistence_msec;	                // beam spot persistence in millisec 
    int x_HP_Resolution, y_HP_Resolution;   // scope image resolution seen by HP program (256x256 on DAC scope, 1021x1021 on HP1350/1351)
    int AutoPowerOn;                        // will power the scope device on first OTA/LIA received from CPU 
    int BeamColor_R, BeamColor_G, BeamColor_B;      // RGB color for beam ( = illuminated point)
    int BkgColor_R,  BkgColor_G,  BkgColor_B;       // RGB color for background (= tube screen color unilluminated)
	// scope current state
    uint8 *sco_energy_nodecay;		        // 800x600 array: stores the lowest energy for each simulated Phosphor point of STORAGE scope screen 
    uint8 *sco_energy; 				        // 800x600 array: stores the current excitement energy for each simulated Phosphor point of scope screen
    int   *sco_decay_msec;  		        // 800x600 array: stores the excitement energy for each simulated Phosphor point of scope screen
    uint8 DecayGrid[H_GRID_SIZE][V_GRID_SIZE];      // 50x37 array: for each 16x16 pixel box in sco_energy, indicates if has pixels to decay
    uint8 RefreshGrid[H_GRID_SIZE][V_GRID_SIZE];    // 50x37 array: for each 16x16 pixel box in sco_energy, indicates if has pixels pending to refresh
    // scope internal state
    uint32 DAC_timer_tnow;                  // =0 -> DAC timer inactive, <>0 value for sim_os_msec() when DAC 20msec timer starts counting 
    uint32 Erase_tnow;                      // =0 -> no bulk erase in progress, <>0 value for sim_os_msec() when erase starts 
    // variables for rendering on GUI
    int power;                              // 0=scope powered off=no GUI window, 1=GUI window visible
    uint32 * surface;                       // SDL2 GUI surface.  
    VID_DISPLAY *vptr;                      // SDL2 window handled
    uint32 beam_color_palette[256];	        // color palette for beam-excited pixel
    int decay_table[256];                   // energy decay table. time needed to decay to energy level (n-1). Can be 0 (decay inmediatelly)
    int decay_energy_time_table[256][51][2];// energy of pixel after decaying msec (0..50) time from energy e (0..255); 
    uint32 decay_tnow;                      // value for sim_os_msec() on last decay calculation
    int wx, wy;                             // width and height of GUI window
    int refresh_needed;                     // =1 if surface has been modified and needs to be refreshed on GUI, bit 1=control knobs
    int FullRedrawNeeded;		            // =1 to request the full redraw of GUI window on next refresh call (=0 refresh only changes according to sco_grid, =2 -> regenerate beam spot bitmap)
    uint32 refresh_tnow;                    // value for sim_os_msec() on last GUI refresh
    int quit_requested;                     // =1 is user closes the GUI window
    int InitialScale;                       // initial scale 10..200 to be used when window is created
    int InitialPosX;                        // x,y postion of window on screen to be used when window is created
    int InitialPosY; 
    // show info variables
    int bShowInfo; 
    int Refresh_Frames_Count; 
    struct {
        int nDecayingPixels; // number of pixels with energy>0 and decaying
        int msec_into_decay; // time in msec to execute for decay_image_in_scope() 
        int msec_into_draw;  // time in msec to execute HP1350_draw_vectors()
    } info;
    
} scope_state = {SCO_TYPE_IS_NOSTORAGE,160,2,100, 
                 256,256,  1,   // resolution seen by program
                 131,249,143,   // beam spot color (green)
                  52, 62, 36,   // scope background color 
                 0}; 

#define BEAM_SPOT_SIZE  16                              // max size of beam spot. Should be even (e.g. 16x16)
uint8 BeamBmp[BEAM_SPOT_SIZE*BEAM_SPOT_SIZE];	        // holds the beam spot bitmap for decaying pixels
uint8 BeamBmp_nodecay[BEAM_SPOT_SIZE*BEAM_SPOT_SIZE];	// holds the beam spot bitmap for storage scope pixels

static struct {                         // holds the position, size and hotkeys for knobs 1..4
    int x, y;                           // knob top left coords on surface
    int sz;                             // knob side size (it is squared)     
    char IncKey, DecKey;                // Keyboard key that does the knob action
} Knob[5];

/*	How Scope is simulated:

HP scope does not draw vectors using analog signals. Instead, it draws spots at x,y coordinates
This eases a lot the simulation.

As scope is an analog device, there is no resolution of image. HP will send spots in a 256x256 grid
we map the visible scope image in a 800x600 SDL2 surface. There are also a sco array that stores the
energy excitement level of each pixel and the time in msec this energy level needs to decay. 
For each energy level there is a color to be shown on screen on SDL2 surface. The refresh routine 
reduces the remaining decay time of each pixel. When this time is exhasuted, 
the energy level is reduced by 1 and a new decay time is set according to decay table.
When the computer draws a spot, the draw routine rises the energy level of pixels at coords x,y

We add a little horizontal and vertical margin so that image is not generated in the border of window.

*/

DEVICE scope_dev;											/* Scope device */

IOHANDLER scopeio;

t_stat scope_svc (UNIT *uptr);
t_stat scope_reset (DEVICE *dptr);
t_stat scope_set_power_on    (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat scope_set_model       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat scope_set_param       (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat scope_set_type        (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat scope_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat scope_show            (FILE *st, UNIT *uptr, int32 num, CONST void *desc);
void scope_refresh(uint32 tnow);
void draw_spot_in_scope(int, int);
#if defined(CPANEL)
void scope_check_HotKeys(void);
#endif

/* Scope data structures

   scope_dev    SCOPE device descriptor
   scope_unit   SCOPE unit
   scope_reg    SCOPE register list
*/

DIB scope_dib = { &scopeio, SCOPE };

UNIT scope_unit = {
	UDATA (&scope_svc, UNIT_SEQ+UNIT_DISABLE, 0),
};

REG scope_reg[] = {
    { FLDATA (CTL, scope.control, 0) },
    { FLDATA (FLG, scope.flag,    0) },
    { FLDATA (FBF, scope.flagbuf, 0) },
    { ORDATA (SC, scope_dib.select_code, 6), REG_HRO },
    { ORDATA (DEVNO, scope_dib.select_code, 6), REG_HRO },
    { NULL }
    };

#define DBG_FLAGS       0x00000004 
#define DBG_DATA        0x00000008 
#define DBG_CMDS        0x00000010 
#define DBG_DETAIL      0x00000020   
#define HP1350_DETAIL   0x00000100   

DEBTAB sim_scope_debug[] = {
  {"FLAGS",     DBG_FLAGS,      "Flags status"},
  {"DATA",      DBG_DATA,       "Data sent to Scope"},
  {"COMMAND",   DBG_CMDS,       "Commands sent to Scope"},
  {"DETAIL",    DBG_DETAIL,     "Operations detail"},
  {"HP1350",    HP1350_DETAIL,  "HP1350 Graphic Translator operations detail"},
  {0}
};

MTAB scope_mod[] = {
    { UNIT_POWERON, UNIT_POWERON,  "scope on",               "ON",           &scope_set_power_on },
    { UNIT_POWERON, 0,             "scope off",              "OFF",          &scope_set_power_on },
    { UNIT_POWERON, UNIT_POWERON,   NULL,                    "POWERON",      &scope_set_power_on },
    { UNIT_POWERON, 0,              NULL,                    "POWEROFF",     &scope_set_power_on },

    { MTAB_XTD | MTAB_VDV, 0,       NULL,                    "HP1300",       &scope_set_model },
    { MTAB_XTD | MTAB_VDV, 1,       NULL,                    "HP1331",       &scope_set_model },
    { MTAB_XTD | MTAB_VDV, 2,       NULL,                    "HP1332",       &scope_set_model },
    { MTAB_XTD | MTAB_VDV, 3,       NULL,                    "HP1304",       &scope_set_model },
    { MTAB_XTD | MTAB_VDV, 3,       NULL,                    "HP1350",       &scope_set_model },
    { MTAB_XTD | MTAB_VDV, 4,       NULL,                    "HP1351",       &scope_set_model },

    { MTAB_XTD | MTAB_VDV, SCO_TYPE_IS_NOSTORAGE,   NULL,    "NOSTORAGE",           &scope_set_type, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, SCO_TYPE_IS_STORAGE,     NULL,    "STORAGE",             &scope_set_type, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, SCO_TYPE_IS_VECTOR,      NULL,    "GRAPHICS_TRANSLATOR", &scope_set_type, NULL, &scope_dev },

    { UNIT_POWERON, 0,              NULL,                    "NOAUTOON",            &scope_set_AutoPowerOn },
    { UNIT_POWERON, 1,              NULL,                    "AUTOON",              &scope_set_AutoPowerOn },

    { MTAB_XTD | MTAB_VDV,            0, "SC",       "SC",    &hp_setsc,  &hp_showsc,  &scope_dev },
    { MTAB_XTD | MTAB_VDV | MTAB_NMO, 0, "DEVNO",    "DEVNO", &hp_setdev, &hp_showdev, &scope_dev },

#if defined(CPANEL)
    // important: SCALE should be defined AFTER "SC". If defined before, SET SC= is interpreted as
    // SET SCale (scale being abbreviated on its two first letter), so SC will never be called
    { MTAB_XTD | MTAB_VDV, 0,       NULL,                    "POS",          &scope_set_param, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, 1,       NULL,                    "SCALE",        &scope_set_param, NULL, &scope_dev },
#endif
    { MTAB_XTD | MTAB_VDV, 2,       NULL,                    "FOCUS",        &scope_set_param, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, 3,       NULL,                    "INTENSITY",    &scope_set_param, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, 4,       NULL,                    "PERSISTENCE",  &scope_set_param, NULL, &scope_dev },
    { MTAB_XTD | MTAB_VDV, 5,       NULL,                    "COLOR",        &scope_set_param, NULL, &scope_dev },

    { 0 }
    };

DEVICE scope_dev = {
    "SCOPE", &scope_unit, scope_reg, scope_mod,
    1, 10, 31, 1, 8, 8,	// 1->num of units, 10->addr radix, 31->addr width, 1->addr incr, 8->data radix, 8->data width
    NULL, NULL, &scope_reset,
    NULL, NULL, NULL,
	&scope_dib, DEV_DISABLE | DEV_DIS | DEV_DEBUG, 0, sim_scope_debug
    };

// RS-232-C card simulation

struct {
    FLIP_FLOP control;                                  /* control flip-flop */
    FLIP_FLOP flag;                                     /* flag flip-flop */
    FLIP_FLOP flagbuf;                                  /* flag buffer flip-flop */
} rs232c = { CLEAR, CLEAR, CLEAR };

static void scope_quit_callback (void) 
{
    // note: quit callback is executed on sim_video thread, not in main emulator thread
    // should not use sim_debug (is not thread safe) 
    scope_state.quit_requested=1; 
}

// decompose a surface uint32 to its rr,gg,bb components. return alpha channel
int scope_get_surface_rgb_color(uint32 color, int *r_Color, int *g_Color, int *b_Color)
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

uint32 scope_rgb_color(uint32 r, uint32 g, uint32 b)  /* r,g,b are 8bit! */
{
	uint32 color;

	// rgb to 16 bits 
	r = (r & 255) << 8; g = (g & 255) << 8; b = (b & 255) << 8; 

    color = sim_end ? 
		       (0xFF000000 | ((r & 0xFF00) << 8) | (g & 0xFF00) | ((b & 0xFF00) >> 8)) : 
	           (0x000000FF | (r  & 0xFF00) | ((g & 0xFF00) << 8) | ((b & 0xFF00) << 16));
    return color;
}


int scope_init(void) 
{
	int n, stat;

    // populate wc, wy and allocate mem for surface
	scope_state.wx = H_SIZE;
	scope_state.wy = V_SIZE;
    scope_state.surface = (uint32 *)malloc (scope_state.wx * scope_state.wy * sizeof(uint32));
    if (scope_state.surface == NULL) return SCPE_IERR;

#if defined(CPANEL)
    if (scope_state.InitialScale == 0) scope_state.InitialScale=INITIAL_SCALE; 
    if ((scope_state.InitialPosX == 0) && (scope_state.InitialPosY == 0)) {
        scope_state.InitialPosX=80; scope_state.InitialPosY=20; 
    }
    stat = vid_open_window_ex (&scope_state.vptr, &scope_dev, "X-Y Display", scope_state.wx, scope_state.wy, 
        SIM_VID_FLAG_SCALE_PLUSMINUS | SIM_VID_FLAG_ALLOW_TOOLTIP  | SIM_VID_FLAG_ALLOW_DRAG,
        scope_state.InitialScale, scope_state.InitialPosX, scope_state.InitialPosY);
    if (stat != SCPE_OK) return stat; 
#else
    stat = vid_open_window (&scope_state.vptr, &scope_dev, "X-Y Display", scope_state.wx, scope_state.wy, 0);
    if (stat != SCPE_OK) return stat; 
#endif
    // allocate mem for sco_energy, sco_energy_nodecay, sco_decay_msec and sco_grid array
    n = H_SCO_SIZE * V_SCO_SIZE;
	if (scope_state.sco_energy == NULL) {
		scope_state.sco_energy = (uint8 *)calloc (n, sizeof(*scope_state.sco_energy));
    	if (!scope_state.sco_energy) {
	    	fprintf(stderr, "calloc fail on scope energy calloc \r\n");
		    return 0;
        }
	}
	if (scope_state.sco_energy_nodecay == NULL) {
		scope_state.sco_energy_nodecay = (uint8 *)calloc (n, sizeof(*scope_state.sco_energy_nodecay));
    	if (!scope_state.sco_energy_nodecay) {
	    	fprintf(stderr, "calloc fail on scope_nodecay energy calloc \r\n");
		    return 0;
        }
	}
	if (scope_state.sco_decay_msec == NULL) {
		scope_state.sco_decay_msec = (int *)calloc (n, sizeof(*scope_state.sco_decay_msec));
    	if (!scope_state.sco_decay_msec) {
	    	fprintf(stderr, "calloc fail on scope energy calloc \r\n");
		    return 0;
        }
	}
    memset(scope_state.DecayGrid, 0, sizeof(scope_state.DecayGrid)); 
    memset(scope_state.RefreshGrid, 0, sizeof(scope_state.RefreshGrid)); 
    scope_state.FullRedrawNeeded = 1; // because scope init issued
    scope_state.refresh_needed=1;     // init scope
    scope_state.refresh_tnow=0;
    scope_state.decay_tnow=0;
    scope_state.DAC_timer_tnow=0;
    scope_state.Erase_tnow=0;
	return SCPE_OK;
}

void scope_done(void) 
{
	sim_os_ms_sleep (200); // this sleep is to asure SDL thread has processed any pending EVENT_REDRAW
	vid_close_window(scope_state.vptr);
    scope_state.vptr=NULL; 
    if (scope_state.surface) free(scope_state.surface); 
    scope_state.surface=NULL;
    if (scope_state.sco_energy) free(scope_state.sco_energy); 
    scope_state.sco_energy=NULL;
    if (scope_state.sco_energy_nodecay) free(scope_state.sco_energy_nodecay); 
    scope_state.sco_energy_nodecay=NULL;
    if (scope_state.sco_decay_msec) free(scope_state.sco_decay_msec); 
    scope_state.sco_decay_msec=NULL;
}

// Creates and destroy the GUI window 
// power = 0 -> power off (closes GUI Window), 
//       = 1 -> power on (creates and shows GUI Window), 
t_stat power_scope(int power) {

    t_stat stat; 

	if (power > 0) {
		// power on scope 
        if (scope_state.power == 1) {
            scope_refresh(0);   // force refresh now: power on also refreshes the screen 
            return SCPE_OK;     // already powered on
        }
		// power on scope -> create GUI window */
	    sim_debug (DBG_DETAIL, &scope_dev, "Scope Init\n");
        // create GUI window
		stat=scope_init();
        if (stat) return stat; 
        // init done
        scope_state.power=1; 
        // set quit callback. Register it linked to close button of last created window
        scope_state.quit_requested=0; 
        vid_register_quit_callback (&scope_quit_callback);
        // init local vars
        // clear screen 
        memset(scope_state.surface, 0, scope_state.wx * scope_state.wy * sizeof(uint32));
        // force refresh
        scope_state.FullRedrawNeeded = 1;	// first refresh after power on 
		scope_refresh(0);  // first refresh after power on 
        // start refresh_svc
        sim_register_internal_device (&scope_refresh_dev);
        sim_activate(&scope_refresh_unit, REFRESH_INTERVAL);
    } else {
		/* power off scope -> close GUI window */
		if (scope_state.power == 0) return SCPE_OK; /* scope already off -> nothing to do*/
        // stop refresh_svc
        sim_cancel(&scope_refresh_unit);
        sim_cancel(&scope_unit);	
        // close GUI window
		scope_done();
        // power off done
		scope_state.power = 0;
	    sim_debug (DBG_DETAIL, &scope_dev, "Scope Done\n");
	}
    return SCPE_OK; 
}

// power the scope if AutoPowerOn flag set and power is off
void scope_CheckAutoPowerOn(void)
{
    if (scope_state.AutoPowerOn == 0) return; 
    if (scope_state.power) return; 
    power_scope(1); 
}

t_stat scope_dac_timer_expired (void) 
{
	/* timer expired */
	sim_debug (DBG_FLAGS, &scope_dev, "DAC Timer Expired (Flag %d, Control Flag %d)\n", 
		                            (scope.flag == SET) ? 1:0, 
									(scope.control == SET) ? 1:0);
	if (scope.control == SET) {					
	    /* control flag set -> Interrupt or programed io mode -> if Flag Clear, then ENF raised on timer expiration */
		if (scope.flag == CLEAR) {
			scopeio (&scope_dib, ioENF, 0);             /* set flag */
		}
	}
    return SCPE_OK;
}

// If n=0,                 return color rgb0
// If n=max,               return color rgb1
// if n=max/2,             return color halfway between rgb0-rgb1
// if n=low value,         return color rgb0 with a bit of rgb1
// if n=near to max value, return color rgb1 with a bit of rgb0
uint32 InterpolateColor(int r0, int g0, int b0, 
                        int n, int max, 
                        int r1, int g1, int b1)
{
    int rr,gg,bb;

    rr = (int) (((r1 - r0) * n)/max + r0);  // calc the color for mixed pixel
    gg = (int) (((g1 - g0) * n)/max + g0);
    bb = (int) (((b1 - b0) * n)/max + b0);
    return scope_rgb_color(rr, gg, bb);
}                      


static const char *FocusBmpStr[] = {
	"XXXX0 .XXX.  .XXX.  X   X  .XXX. ",
	"X.    X0 0X  X0 .0  X   X  X0 .0 ",
	"XXX   X   X  X      X   X  .XXX. ",
	"X     X   X  X      X   X     .X ",
	"X     X0 0X  X0 .0  X0 0X  X. 0X ",
	"X     .XXX.  .XXX.  .XXX.  .XXX. ",
	"",
};

static const char *IntensityBmpStr[] = {
	"0X0  X.   X  0XXX0  XXXX0  X.   X  .XXX.  0X0  0XXX0  X   X ",
	" X   XX.  X    X    X.     XX.  X  X0 .0   X     X    X. .X ",
 	" X   X.X. X    X    XXX    X.X. X  .XXX.   X     X     X.X  ",
	" X   X .X.X    X    X      X .X.X     .X   X     X     .X.  ",
	" X   X  .XX    X    X.     X  .XX  X. 0X   X     X      X   ",
	"0X0  X    X    X    XXXX0  X    X  .XXX.  0X0    X      X   ",
	"",
};

static const char *PersistenceBmpStr[] = {
	"XXXX.  XXXX0  XXXX.  .XXX.  0X0  .XXX.  0XXX0  XXXX0  X.   X  .XXX.  XXXX0 ",  
	"X. .X  X.     X. .X  X0 .0   X   X0 .0    X    X.     XX.  X  X0 .0  X     ",
	"X. .X  XXX    X. .X  .XXX.   X   .XXX.    X    XXX    X.X. X  X      XXX   ",
	"XXXX.  X      XXX0      .X   X      .X    X    X      X .X.X  X      X     ",
	"X      X.     X .X   X. 0X   X   X. 0X    X    X.     X  .XX  X0 .0  X.    ",  
	"X      XXXX0  X  .X  .XXX.  0X0  .XXX.    X    XXXX0  X    X  .XXX.  XXXX0 ",
	"",
};

static const char *EraseBmpStr[] = {
	"XXXX0  XXXX.    X    .XXX.  XXXX0 ",  
	"X.     X. .X   X0X   X0 .0  X     ",
	"XXX    X. .X  X. .X  .XXX.  XXX   ",
	"X      XXX0   xXXXX     .X  X     ",
	"X.     X .X   X   X  X. 0X  X.    ",  
	"XXXX0  X  .X  X   X  .XXX.  XXXX0 ",
	"",
};

// draw text centered: x0,y0=surface coords of text center top
void Paint_text_in_scope_surface(int x0, int y0, const char * BmpLabel[])
{ 
    int len, lin, c, n; 
    uint32 col, colW1, colW2, colW3;
    
    // draw text bitmap
    colW1 = scope_rgb_color(255, 255, 255); // white
    colW2 = scope_rgb_color(128, 128, 128); // grey
    colW3 = scope_rgb_color( 64,  64,  64); // dark grey
    // calc width of text
    len=strlen(BmpLabel[0]); 
    x0=x0-len/2;
    lin=0; 
    while (1) {
        if(strlen(BmpLabel[lin])==0) break; // end of text label
        for(n=0;n<len;n++) {
            c=BmpLabel[lin][n]; 
            if (c==0)   { break; } else
            if (c=='X') { col=colW1; } else
            if (c=='0') { col=colW2; } else
            if (c=='.') { col=colW3; } else continue; 
            scope_state.surface[x0+n  +  H_SIZE * (y0+lin) ] = col; 
        }
        lin++;
    }
}

// draw a circular knob in surface. 
// nKnob -> index in Knob[] struct where to save the pos/size of painted knob
// v_pos = pixels down from V_MARGIN top
// val = current value of knob
// max = max allowed value of knob (when turned to right at max)
// BmpLabel = text label to draw below knob
void Paint_knob_in_scope_surface(int nKnob, int v_pos, 
                                 int val, int max_val, const char * BmpLabel[])
{
    int sz, x, y, x0, y0, n, c;
    double r, r1, r2, r3, angle, xd, yd;
    uint32 col, colKnob, colBkg;
    int Knob_r1, Knob_g1, Knob_b1, Knob_r2, Knob_g2, Knob_b2;
    double PI = 3.14159265358979323846;

    sz=(int) (H_KNOB * 0.6);            // knob size (it is squared)
    x0 = H_SIZE - H_KNOB/2;             // x0, y0 = center of knob, in surface coords
    y0 = V_MARGIN + v_pos + sz/2; 

    // save this 
    Knob[nKnob].x = x0-sz/2;    // knob top left coords on surface
    Knob[nKnob].y = y0-sz/2; 
    Knob[nKnob].sz = sz;        // knob side (it is squared)

    r=(int)((sz/2) * 0.8); 
    r3=r; 
    r2=r-2;  Knob_r2=128; Knob_g2=128; Knob_b2=128; // color for border of knob (from r1 to r2)
    r1=r-22; Knob_r1=150; Knob_g1=150; Knob_b1=150; // color for center of knob (up to r1)
    
    colKnob=scope_rgb_color(Knob_r1, Knob_g1, Knob_b1);       
    colBkg=scope_rgb_color(0, 0, 0);       

	for (x=0;x<sz/2;x++) for (y=0;y<sz/2;y++) {			
        // calc only one quadrant
        r=sqrt((double)(x*x+y*y));
        if (r < r1) {
            col=colKnob; 
        } else if (r < r2) {
            col=InterpolateColor(Knob_r2, Knob_g2, Knob_b2,    
                                 (int)((r2-r)*100), (int)((r2-r1)*100), 
                                 Knob_r1, Knob_g1, Knob_b1);    
        } else if (r < r3) {
            col=InterpolateColor(0, 0, 0,
                                 (int)((r3-r)*100), (int)((r3-r2)*100), 
                                 Knob_r2, Knob_g2, Knob_b2);    
        } else {
            col=colBkg; 
        }
        // and replicate this quadrant
        scope_state.surface[x0+x  +  H_SIZE * (y0+y) ] = col; 
        scope_state.surface[x0+x  +  H_SIZE * (y0-y) ] = col; 
        scope_state.surface[x0-x  +  H_SIZE * (y0+y) ] = col; 
        scope_state.surface[x0-x  +  H_SIZE * (y0-y) ] = col; 
    }

    // draw antialiased current knob rotational postion
    if (val > max_val) val = max_val;
    angle = 3*PI/4 + ((3*PI/2) * val / max_val); 
    for (n=1; n<16; n++) {
        r = r1 + 5 + (r2-r1-5)*n/16; 
        x=(int)(xd=cos(angle)*r); xd-=x; 
        y=(int)(yd=sin(angle)*r); yd-=y; 
        col=scope_state.surface[x0+x  +  H_SIZE * (y0+y) ]; 

        if ((val > max_val * 1/3) && (val < max_val * 2/3)) {
            if (xd < 0) {
                c = (int) (150 * (1+xd)); 
                scope_state.surface[x0+x-1  +  H_SIZE * (y0+y) ] = scope_rgb_color(c, c, c); 
                c = (int) (150 * (-xd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y) ] = scope_rgb_color(c, c, c); 
            } else {
                c = (int) (150 * (xd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y) ] = scope_rgb_color(c, c, c); 
                c = (int) (150 * (1-xd)); 
                scope_state.surface[x0+x+1  +  H_SIZE * (y0+y) ] = scope_rgb_color(c, c, c); 
            }
        } else {
            if (yd < 0) {
                c = (int) (150 * (1+yd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y-1) ] = scope_rgb_color(c, c, c); 
                c = (int) (150 * (-yd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y)   ] = scope_rgb_color(c, c, c); 
            } else {
                c = (int) (150 * (yd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y)   ] = scope_rgb_color(c, c, c); 
                c = (int) (150 * (1-yd)); 
                scope_state.surface[x0+x    +  H_SIZE * (y0+y+1) ] = scope_rgb_color(c, c, c); 
            }
        }

    }
    // draw knob label 
    Paint_text_in_scope_surface(x0, y0+sz/2, BmpLabel);
}

// draw a rectangular button in surface. 
// nKnob -> index in Knob[] struct where to save the pos/size of painted button
// v_pos = pixels down from V_MARGIN top
// BmpLabel = text label to draw below button
void Paint_btn_in_scope_surface(int nKnob, int v_pos, 
                                const char * BmpLabel[])
{
    int x, y, x0, y0, ww, hh;
    uint32 col, colBTN1, colBTN2, colBkg;

    ww=(int) (H_KNOB * 0.3);            // Buttun width
    hh=(int)(ww*0.6);
    x0 = H_SIZE - H_KNOB/2-ww/2;        // x0, y0 = yop left corner, in surface coords
    y0 = V_MARGIN + v_pos; 

    // save this 
    Knob[nKnob].x = x0;    // knob top left coords on surface
    Knob[nKnob].y = y0; 
    Knob[nKnob].sz = ww;   // button width 

    colBTN1=scope_rgb_color(150, 150, 150);       
    colBTN2=scope_rgb_color(128, 128, 128);       
    colBkg=scope_rgb_color(0, 0, 0);       

    for (x=x0;x<x0+ww;x++) for (y=y0;y<y0+hh;y++) {			
        if ((x==x0) || (y==x0) || (x==x0+ww-1) || (y==y0+hh-1)) {
       	   col=colBTN1; 
       	} else {
       	   col=colBTN2; 
       	}
        scope_state.surface[x +  H_SIZE * y ] = col; 
    }
    // draw button label 
    Paint_text_in_scope_surface(x0+ww/2, y0+hh+10, BmpLabel);
}

// draw control knobs
void set_control_knobs(int Mode)
{
#if defined(CPANEL)
    int n,x,y,w,h; 
    uint32 col; 

    if (scope_state.power==0) return; // no GUI, nothing to draw
    if (Mode == 0) {
        // doing a full redraw. Clear H_KNOB area
        x=H_SIZE-H_KNOB; w=H_KNOB; 
        y=V_MARGIN; h=V_SCO_SIZE; 
        col = scope_rgb_color(0, 0, 0); // black
        for (x=H_SIZE-H_KNOB; x<H_SIZE; x++) for (y=V_MARGIN; y<V_MARGIN+V_SCO_SIZE; y++) {
            scope_state.surface[x + H_SIZE * y] = col; 
        }
        memset(Knob, 0, sizeof(Knob)); 
        Knob[1].IncKey = 'i'; // intensity knob is number 1
        Knob[1].DecKey = 'I'; 
        Knob[2].IncKey = 'f'; // focus knob is number 2
        Knob[2].DecKey = 'F'; 
        Knob[3].IncKey = 'p'; // persistence knob is number 3
        Knob[3].DecKey = 'P'; 
        Knob[4].IncKey = Knob[4].DecKey = 'e'; // erase button
        // x0,y0,sz fiels of knob array are filled in Paint_circle_in_scope_surface calls
    }
    if ((Mode==0) || (Mode==1)) {
        Paint_knob_in_scope_surface(1, 0 /* v_pos */, 
                                      scope_state.Intensity, 255, IntensityBmpStr);

    }
    if ((Mode==0) || (Mode==2)) {
        Paint_knob_in_scope_surface(2, 100 /* v_pos */, 
                                      scope_state.Focus, 63, FocusBmpStr);

    }
    if ((Mode==0) || (Mode==3)) {
        // knob ranges for values of persistence 10 .. 100.000 
        // using logaritmic scale. 
        // n ranges from 20 (persistence = 10 msec -> log10(10)=1)
        // up to 100 ((persistence = 100.000 msec -> log10(100.000)=5)
        n=(int) (100 * log10((double)scope_state.Persistence_msec)/5);
        Paint_knob_in_scope_surface(3, 200 /* v_pos */, 
                                    n-20, 80, PersistenceBmpStr);

    }
    if ((Mode==0) && (scope_state.type==SCO_TYPE_IS_STORAGE)) {
        // erase button 
        Paint_btn_in_scope_surface(4, 330 /* v_pos */, EraseBmpStr);

    }
    scope_state.refresh_needed |=2; // because a control knobs is redraw
#else
    return; // no cpanels ... no knobs
#endif
}

// populate beam_color_palette array using RGB color 
// of scope beam and background
void set_beam_color_palette_rgb(void)
{
    // populate 256 values: [0] = BkgColor_R/G/B  = tube screen color unilluminated
    //                            ... interpolates to ...
    //                    [128] = BeamColor_R/G/B = color for electron beam ( = illuminated point)
    //                            ... interpolates to ...
    //                    [255] = full white = saturated phosphor spot

    int n, r0,g0,b0, r1,g1,b1;
    uint32 col; 

    // interpolate color from BkgColor to BeamColor
    r0=scope_state.BkgColor_R;   g0=scope_state.BkgColor_G;   b0=scope_state.BkgColor_B; 
    r1=scope_state.BeamColor_R;  g1=scope_state.BeamColor_G;  b1=scope_state.BeamColor_B; 
    for (n=0; n<128; n++) {
        col=InterpolateColor(r0, g0, b0,    n, 128,   r1, g1, b1); 
        scope_state.beam_color_palette[n]=col; 
    }
    // interpolate color form BeamColor to full white
    r0=scope_state.BeamColor_R;  g0=scope_state.BeamColor_G;  b0=scope_state.BeamColor_B; 
    r1=255;                      g1=255;                      b1=255; 
    for (n=0; n<128; n++) {
        col=InterpolateColor(r0, g0, b0,    n, 128,   r1, g1, b1); 
        scope_state.beam_color_palette[n+128]=col; 
    }
}

// populate decay table using Persistence_msec value and storage flag
void set_decay_table(void)
{
    int n, ni, toti; 
    double decay[256], d, tot, factor;
    int energy, decay_msec, msec; 

    // decay_table[n] = msec -> time needed to decay to lower energy level (n-1). Can be 0
    // create a base decay table
    tot=0;
    for (n=255; n; n--) {
        d=0.3 + ((256-n)/32.0); 
        decay[n]=d;  
        if (n>85) tot += d; // accumulate time (msec) needed to go from energy 256 to 85 (from 100% to 33% of energy)
    }    
    // calc factor to apply to get the wanted persistence time
    factor = scope_state.Persistence_msec / tot; 
    // apply factor to decay table
    for (n=255; n; n--) {
        decay[n] = decay[n] * factor; 
    }    
    // convert it to msec integer values, adjusting totals
    tot=0; toti=0; 
    for (n=255; n; n--) {
        d=decay[n];  
        scope_state.decay_table[n]=(int)d; 
        // accumulate real values
        tot += d; 
        // accumulate integer values
        toti=0; 
        for (ni=255;ni>=n; ni--) toti += scope_state.decay_table[ni]; 
        // adjust accumulated difference in current value
        scope_state.decay_table[n] += ((int)(tot)) - toti; 
    }

    scope_state.decay_table[0]=0; 

    // populate decay time table
    // decay_table[n] = msec -> time needed to decay to lower energy level (n-1). Can be 0
    // scope_state.decay_energy_time_table[e1][msec][0] -> energy of pixel after decaying msec time from energy e1; 
    // scope_state.decay_energy_time_table[e1][msec][1] -> decay time of pixel after decaying msec time from energy e; 
    for (n=0;n<256;n++) {
        for(msec=0; msec<50; msec++) {
            energy=n; 
            decay_msec = -msec; 
            while (decay_msec <= 0) {
                energy--;
                if (energy<=0) break; // spot has decayed to no energy level
                decay_msec += scope_state.decay_table[energy];
            }
            if (energy<=0) {
                scope_state.decay_energy_time_table[n][msec][0]=0; 
                scope_state.decay_energy_time_table[n][msec][1]=0; 
            } else {
                scope_state.decay_energy_time_table[n][msec][0]=energy; 
                scope_state.decay_energy_time_table[n][msec][1]=decay_msec; 
            }
        }
    }

}

void Paint_scope_spot_in_bmp(uint8 Bmp[], double r1, double r2, double r3, int energy1, int energy2) 
// render in given char array Bmp[16x16] an antialiased circle
// radius r1 has energy1 value
// radius r1-r2 goes from energy1 to energy2
// radius r2-r3 goes from energy2 to zero
{
    int sz, x, y, e; 
    double r; 

    sz=BEAM_SPOT_SIZE / 2;    // beam spot bmp is squared

	for (x=0;x<sz;x++) for (y=0;y<sz;y++) {			
        // calc only one quadrant
         r=sqrt((double)(x*x+y*y));
        if (r < r1) {
            e=energy1; 
        } else if (r < r2) {
            e= energy2 + (int) ( ((r2-r) / (r2-r1)) * (energy1-energy2) );
        } else if (r < r3) {
            e= (int) ( ((r3-r) / (r3-r2)) * energy2 );
            if (e<1) e=1; // so allows calculation of energy of surrounding spots
        } else e=0; 
        // and replicate this quadrant
        Bmp[sz+x  +BEAM_SPOT_SIZE*( sz+y ) ]=e; 
        Bmp[sz+x  +BEAM_SPOT_SIZE*( sz-y ) ]=e; 
        Bmp[sz-x  +BEAM_SPOT_SIZE*( sz+y ) ]=e; 
        Bmp[sz-x  +BEAM_SPOT_SIZE*( sz-y ) ]=e; 
    }
}

// paint the beam spot image bitmap 
void set_beam_spot_image(uint8 * Bmp, int bExtraEnergy)
{
    double r1, r2, r3, f;
    int energy1, energy2; 
    int rmax = (BEAM_SPOT_SIZE / 2); 
    int p; 

    energy1=scope_state.Intensity; 
    if (bExtraEnergy) { 
        energy1=energy1*2; 
        if (energy1>255) energy1=255; 
    }

    // set the low focus value to be pixel exact
    if (scope_state.Focus < 2) {
        for (p=0;p<BEAM_SPOT_SIZE*BEAM_SPOT_SIZE;p++) Bmp[p]=0; 	
        p=BEAM_SPOT_SIZE/2 + BEAM_SPOT_SIZE* (BEAM_SPOT_SIZE/2); // p=center of Bmp
        if (scope_state.Focus == 0) {
            energy1=energy1*2; 
            if (energy1>255) energy1=255; 
            Bmp[p]=energy1; // focus = 0 -> one sole pixel
        } else if (scope_state.Focus == 1) {
            Bmp[p]=energy1;                  Bmp[p+1]=energy1; // focus = 1 -> 4 pixels
            Bmp[p+BEAM_SPOT_SIZE]=energy1;   Bmp[p+BEAM_SPOT_SIZE+1]=energy1; 
        }
        return; 
    }

    // focus 0=minimal narrow spot, 63 wide beam spot size 
    // intensity 0=not visible, 255=overburn
    // if intensity if high, aparent spot size grows 

    f=(scope_state.Focus - 2) / 64.0; 
    if (scope_state.Intensity > 128) {
        f = f * (1 + 1.50 * (scope_state.Intensity-128)/128);
        if (f>1) f=1; 
    }

    r1=(rmax-4.0) * f; 
    r2=r1+2.0;
    r3=rmax; 

    energy2=energy1-136;
    if (energy2<0) energy2=0;

    Paint_scope_spot_in_bmp(Bmp, r1, r2, r3, energy1, energy2); 
}

// set the beam spot bitmap according to focus and intensity values
void set_beam_bmp(void)
{
    if (scope_state.type==SCO_TYPE_IS_STORAGE) {
        // spot for storage tubes: energy of each tube spot will
        // decay to BeamBmp_nodecay value
        set_beam_spot_image(BeamBmp, 1); 
        set_beam_spot_image(BeamBmp_nodecay, 0); 
    } else {
        // spot for non-storage tubes: energy of each tube spot will
        // decay to zero, so BeamBmp_nodecay se to zero
        set_beam_spot_image(BeamBmp, 0); 
        memset(BeamBmp_nodecay, 0, sizeof(BeamBmp_nodecay));
    }
}

/* Refresh: Simulate drawing on phosphor scope with an energized electron beam */

// erase storage tube (Bluk erase)
// if Mode=1 -> chek and set scope_state.Erase_tnow
void scope_bulk_erase (int Mode) 
{
    int n; 

    if ((Mode==1) && (scope_state.Erase_tnow)) {
       sim_debug (DBG_CMDS, &scope_dev, "Bulk Erase in progress\n");
       return; 
    }

	/* simulate the green flash on erasing storage tube. */
    sim_debug (DBG_CMDS, &scope_dev, "Bulk Erase\n");
    // clear all
    memset(scope_state.DecayGrid, 0, sizeof(scope_state.DecayGrid)); 
    memset(scope_state.RefreshGrid, 0, sizeof(scope_state.RefreshGrid)); 
    n = H_SCO_SIZE * V_SCO_SIZE;
    if (scope_state.sco_decay_msec) memset(scope_state.sco_decay_msec, 0, n*sizeof(*scope_state.sco_decay_msec)); 
    if (scope_state.sco_energy) memset(scope_state.sco_energy, 0, n*sizeof(*scope_state.sco_energy)); 
    if (scope_state.sco_energy_nodecay) memset(scope_state.sco_energy_nodecay, 0, n*sizeof(*scope_state.sco_energy_nodecay)); 
	if (Mode==1) scope_state.Erase_tnow=sim_os_msec(); 
}

// decay scope and draw decayed pixels in surface
void decay_image_in_scope(int decay_msec)
{
    int x_Grid, y_Grid, x,y,p, nPixels, ps, energy, eMin, decay, x_SCO, y_SCO; 
    int nDecayingPixels=0; // count of total pixels decayed

    for (x_Grid=0; x_Grid < H_GRID_SIZE;x_Grid++) {
        x_SCO=(x_Grid<<4); 
        for (y_Grid=0; y_Grid < V_GRID_SIZE;y_Grid++) {
            if (scope_state.DecayGrid[x_Grid][y_Grid]==0) continue; // no pixels to decay in this 16x16 box
            nPixels=nDecayingPixels; // count of pixels to decay
            y_SCO=(y_Grid<<4); 
            // decay 16x16 pixels in box
            for (y=0; y<16; y++) {
                if (y_SCO >= V_SCO_SIZE) break; 
                p = x_SCO + H_SCO_SIZE * y_SCO; 
                ps= H_MARGIN + x_SCO + H_SIZE * (V_MARGIN + y_SCO) ; 
                for (x=0; x<16; x++) {
                    if (x_SCO+x >= H_SCO_SIZE) break; 
                    // has pixel some energy to decay? 
                    if ( (energy=scope_state.sco_energy[p]) <= (eMin=scope_state.sco_energy_nodecay[p]) )  {
                        goto nextspot; 
                    }
                    // pixel is decaying
                    nDecayingPixels++;
                    // decrease pixel decay time 
                    if ( (decay=(scope_state.sco_decay_msec[p] - decay_msec)) > 0) {
                        // set remaining decay time left, spot keeps current energy level
                        scope_state.sco_decay_msec[p]=decay;
                        goto nextspot;
                    }
                    // decay time has expired!should decrease spot energy level
                    // pixel has decayed, set new lower energy level
                    // while (decay <= 0) {
                    //    energy--;
                    //    if (energy<=eMin) break; // spot has decayed to no energy level
                    //    decay += scope_state.decay_table[energy];
                    // }
                    while (decay < -50) {
                        energy=scope_state.decay_energy_time_table[energy][50][0];
                        if (energy <= eMin) { 
                            energy=eMin; decay = 0;
                            goto okspot;
                        }
                        decay = decay + 50 + scope_state.decay_energy_time_table[energy][50][1];
                    }
                    energy=scope_state.decay_energy_time_table[energy][-decay][0];
                    if (energy <= eMin) { 
                        energy=eMin; decay = 0;
                        goto okspot;
                    }
                    decay = scope_state.decay_energy_time_table[energy][-decay][1];
                    // set remaining decay time, and new color according to new energy level
                 okspot:
                    scope_state.sco_energy[p]=energy;
                    scope_state.sco_decay_msec[p]=decay;
                    if (scope_state.Erase_tnow) {
                        // if bulk erase in progress, do not update surface, just sco array
                    } else {
                        scope_state.surface[ps]=scope_state.beam_color_palette[energy]; 
                    }
                 nextspot:
                    p++; ps++;
                }
                y_SCO++; 
            }
            if (nPixels==nDecayingPixels) {
                // there where no pixel that have been decayed in this box 
                scope_state.DecayGrid[x_Grid][y_Grid]=0; 
            } else {
                // there where pixel that have been decayed in this box, so must be refreshed on GUI
                scope_state.RefreshGrid[x_Grid][y_Grid]=1; 
                scope_state.refresh_needed |= 1;	// because decaying spots in scope image 
            }
        }
    }
    // save stats for ShowInfo 
    if (nDecayingPixels > scope_state.info.nDecayingPixels) scope_state.info.nDecayingPixels=nDecayingPixels; 
}

// add a pixel from BeamBmp_decay (the param) into sco_energy and 
// Beam_nodecay into sco_energy_nodecay arrays at x,y (H/V_SCO_SIZE range = 800x600)
// also draws the pixel to surface
void draw_beam_pixel_in_sco_energy(uint8 * BeamBmp_decay, // beam bmp for 
                                   int x_SCO, int y_SCO,  // integer part of coords of pixel to draw
                                   int xd256, int yd256,  // fractional part of coords (for antialising) 0..255 correspomd to 0.0 .. 0.9999
                                   int x, int y,          // coord into BeamBmp, ranging from -BEAM_SPOT_SIZE/2 to BEAM_SPOT_SIZE/2-1; 
                                   int bExamineSurroundingSpots)
{
    int x_Grid, y_Grid; 
    int ty, p, pe, ps, e2, e1, e0, ee; 
    int eLUp, eUp, eL;
    int RaiseEnergy = 0; 
    uint8 * Beam; 
    uint8 * sco_energy; 

    x_SCO += x; 
    y_SCO += y; 

            x_Grid = (x_SCO)>>4; 
            y_Grid = (y_SCO)>>4; 
            // calculate beam energy transfered to scope tube spot at position p ...
            p=(x_SCO) + H_SCO_SIZE * (y_SCO); 
            // ... for ty=0 (current energy in scope spot) and 
            //     ty=1 (nin energy in spot, will not decay to lower energy)
            for(ty=(scope_state.type==SCO_TYPE_IS_STORAGE) ? 1:0;ty>=0;ty--) {
                // ty=0 -> scope with decay (non-storage scope), ty=1 -> non decay scope (storage scope
                if (ty==0) {
                   Beam=BeamBmp_decay; // these pixels will decay up to _nodecay energy level
                   sco_energy=scope_state.sco_energy;  
                } else {
                   Beam=BeamBmp_nodecay; // min energy, spot will not decay to a lower value
                   sco_energy=scope_state.sco_energy_nodecay;   
                }
                // get energy of beam spot (into ee)
                // to do so, we get 4 pixels to apply antializasing. 
                // We use beam pixels on Left and Top of current beam pixel 
                eLUp=eUp=eL=-1; 
                if (x==-BEAM_SPOT_SIZE/2) eLUp=eL=0;  // left up and left pixels are outside beam Bmp -> energy=0
                if (y==-BEAM_SPOT_SIZE/2) eLUp=eUp=0; // left up and up pixels are outside beam Bmp -> energy=0
                // read current pixels and Up & left neighbours pixels
                pe=BEAM_SPOT_SIZE/2+x   + BEAM_SPOT_SIZE*(BEAM_SPOT_SIZE/2+y);
                if (xd256==0) {
                    ee=              Beam[pe]; 
                    if (eUp<0)  eUp= Beam[pe-BEAM_SPOT_SIZE]; 
                    if (eUp + ee == 0) continue; 
                    // apply antialiasing based on yd -> result in ee
                    ee  = ee  + ((eUp-ee)   * yd256) / 256;       // ee=energy of electron beam spot 
                } else if (yd256==0) {
                    ee=              Beam[pe]; 
                    if (eL<0)   eL=  Beam[pe-1]; 
                    if (eL + ee == 0) continue; 
                    // apply antialiasing based on xd -> result in ee
                    ee  = ee  + ((eL-ee)    * xd256) / 256;
                } else {
                    ee=              Beam[pe]; 
                    if (eUp<0)  eUp= Beam[pe-BEAM_SPOT_SIZE]; 
                    if (eL<0)   eL=  Beam[pe-1]; 
                    if (eLUp<0) eLUp=Beam[pe-1-BEAM_SPOT_SIZE]; 
                    if (eLUp + eUp + eL + ee == 0) continue; 
                    // apply antialiasing based on xd and yd -> result in ee
                    ee  = ee  + ((eL-ee)    * xd256) / 256;
                    eUp = eUp + ((eLUp-eUp) * xd256) / 256;
                    ee  = ee  + ((eUp-ee)   * yd256) / 256;       // ee=energy of electron beam spot 
                }
                if (ee==0) continue; 
                // get energy of scope tube energy 
                // e1=current energy of pixel already in scope tube         
                e1=sco_energy[p];  
                // e2=mean energy counting surrounding spots
                if (bExamineSurroundingSpots == 0) {
                    // do not examine energy of surrounding spots
                } else if (scope_state.Intensity < 128) {
                    // no energy transfered. Not enought energy for secondary emission
                } else if ((x_SCO == 0) || (x_SCO == H_SCO_SIZE-1) ||
                           (y_SCO == 0) || (y_SCO == V_SCO_SIZE-1)) {
                    // we are in border. 
                } else {
                    e2=sco_energy[p-1-H_SCO_SIZE] + sco_energy[p-H_SCO_SIZE] + sco_energy[p+1-H_SCO_SIZE] +
                       sco_energy[p-1           ] + e1                       + sco_energy[p+1           ] +
                       sco_energy[p-1+H_SCO_SIZE] + sco_energy[p+H_SCO_SIZE] + sco_energy[p+1+H_SCO_SIZE];
                    e2=e2/9; 
                    // calculate amount of energy transfered from surrounding spots (e2) 
                    if (e2>e1) {
                        if (scope_state.Intensity < 128+64) {
                            // ramp up of secondary emission from 0% to 100% for Intensity = 128..192
                            e2 = e1 + (e2-e1) * (scope_state.Intensity - 128) / 64; 
                        } else {
                            // If Intensity > 192 -> 100% of energy transfered
                        }
                        // energy transfered from surrounding spots (e2) powers up beam energy (ee)
                        if (e2 > ee) ee=e2; 
                    }
                }
                // compare beam spot energy vs scope tube energy. Keep higher energy
                if (ee > e1) {
                    // beam energy is higher that tube energy -> raise tube energy ...
                    sco_energy[p]=ee;       
                    RaiseEnergy++; 
                    if (ty==0) {
                        // set the pixel energy in scope energy array to be decayed
                        // set decay time in msec miliseconds, ands decay flag in grid
                        scope_state.sco_decay_msec[p]=scope_state.decay_table[ee]; 
                        scope_state.DecayGrid[x_Grid][y_Grid]=1; 
                    } 
                }
            }
            // sco_energy holds current scope pixel energy
            // sco_energy_nodecay holds min pixel energy to decay to
            // on non-storage tubes, min energy is zero: the pixel fades out as its energy decays ans dissipates
            // on storage tubes, min energy is > 0: the pixels that remains visible, stored on tube
            if (RaiseEnergy ==0) return; // no energy transfered to scope
            if (scope_state.type==SCO_TYPE_IS_STORAGE) {
                e1=scope_state.sco_energy[p];  
                e0=scope_state.sco_energy_nodecay[p];   
                // keep the highest energy level
                if (e1+e0==0) return; // no energy pixel, proceed to next one
                if (e1>e0) {
                    ee=e1; // highest energy level is the decaying pixel
                } else {
                    ee=e0; // highest energy level is the stored pixel                   
                    scope_state.sco_energy[p]=0; // so can discard decaying pixel
                }
            } else {
                ee=scope_state.sco_energy[p];  // energy to show in surface
                if (ee==0) return; // no energy pixel, proceed to next one
            }
            // update the surface in GUI
            if (scope_state.Erase_tnow) {
               // if bulk erase in progress, do not update surface, just sco arrays
            } else {
               ps= H_MARGIN + x_SCO + H_SIZE * (V_MARGIN + y_SCO); 
               scope_state.surface[ps]=scope_state.beam_color_palette[ee]; 
               scope_state.RefreshGrid[x_Grid][y_Grid]=1; 
               scope_state.refresh_needed |= 1;	// because adding a spot to scope image 
            }
}

// draw a spot into scope phosphor at coords given by HP2100 CPU (0..255, 0..255)
void draw_spot_in_scope(int x_HP, int y_HP) 
{
    int x,y, x_SCO, y_SCO, sz; 
    double xd, yd; 
    int xd256, yd256; 

    sz = BEAM_SPOT_SIZE/2; 
    // convert CPU coordinates 0..255 to surface/sco coordinates
    // HP coords have (0,0) at bottom left corner
    // provide an offset so spot on corners fits in SCO_SIZE
    if ((x_HP < 0) || (x_HP >= scope_state.x_HP_Resolution)) return; 
    if ((y_HP < 0) || (y_HP >= scope_state.y_HP_Resolution)) return; 
    y_HP =(scope_state.y_HP_Resolution-1) - y_HP; 
    xd = (double)(sz+1)+(double)((H_SCO_SIZE-(sz+1)*2) * x_HP) / scope_state.x_HP_Resolution; 
    yd = (double)(sz+1)+(double)((V_SCO_SIZE-(sz+1)*2) * y_HP) / scope_state.y_HP_Resolution; 
    x_SCO = (int)xd; xd256=(int)((xd-x_SCO) * 256);     // coordinates in sco_energy array. (0,0) is in top left corner
    y_SCO = (int)yd; yd256=(int)((yd-y_SCO) * 256);

    // add the beam spot energy intensity to scope 
    for (x=-sz; x<sz;x++) {
        for (y=-sz; y<sz;y++) {
            draw_beam_pixel_in_sco_energy(BeamBmp, x_SCO, y_SCO, xd256, yd256, x,y,1); 
        }
    }
}

// draw a vector into scope phosphor at coords given by HP2100 CPU (0..255, 0..255)
// the vector is draw as decayed decay_vector msecs
// intensity 0=max intensity, 1=a bit lower intensity, 14=dimmed, 15=minimum intensity
void draw_vector_in_scope(int x1_HP, int y1_HP, int x2_HP, int y2_HP, int decay_vector, int intensity) 
{
    #define SWAPI(a,b)   {int t=a; a=b; b=t; }
    #define SWAPD(a,b)   {double t=a; a=b; b=t; }

    int x,y, x1_SCO, y1_SCO, x2_SCO, y2_SCO, sz, dx, dy, xlen, ylen; 
    int energy, p, decay; 
    double x1d, y1d, x2d, y2d, d, ddx, ddy; 
    int xd256, yd256, le; 
    static uint8 Cached_BeamBmp[BEAM_SPOT_SIZE*BEAM_SPOT_SIZE];	
    static int   Cached_BeamBmp_current_decay = -1; 
    static int   Cached_BeamBmp_intensity = -1; 
    uint8 * BeamBmp_decay; 

    sz = BEAM_SPOT_SIZE/2; 
    // convert CPU coordinates 0..255 to surface/sco coordinates
    // HP coords have (0,0) at bottom left corner
    // provide an offset so spot on corners fits in SCO_SIZE
    if ((x1_HP < 0) || (x1_HP >= scope_state.x_HP_Resolution)) return; 
    if ((y1_HP < 0) || (y1_HP >= scope_state.y_HP_Resolution)) return; 
    if ((x2_HP < 0) || (x2_HP >= scope_state.x_HP_Resolution)) return; 
    if ((y2_HP < 0) || (y2_HP >= scope_state.y_HP_Resolution)) return; 
    y1_HP =(scope_state.y_HP_Resolution-1) - y1_HP; 
    y2_HP =(scope_state.y_HP_Resolution-1) - y2_HP; 

    // decay BeamBmp to use it on drawing the vector
    if ((decay_vector==0) && (intensity==0)) {
        // no decay, max intensity
        BeamBmp_decay = BeamBmp; 
    } else if ((decay_vector == Cached_BeamBmp_current_decay) && (intensity == Cached_BeamBmp_intensity))  {
        // decay match with current Cached_BeamBmp. Use it
        BeamBmp_decay=Cached_BeamBmp; 
    } else {
        // decay BeamBmp, save it in Cached_BeamBmp
        for (p=0; p<BEAM_SPOT_SIZE*BEAM_SPOT_SIZE;p++) {
            energy=BeamBmp[p]; 
            if (energy==0) {
                Cached_BeamBmp[p]=0; 
                continue; 
            }
            if (intensity) {
                energy = (energy * (16-intensity)) / 16;
            }
            decay=-decay_vector; 
            // while (energy) {
            //   if (energy <= BeamBmp_nodecay[p]) break; 
            //    decay -= scope_state.decay_table[energy];
            //    if (decay<=0) break; 
            //    energy--;
            // }
            while (decay < -50) {
                energy=scope_state.decay_energy_time_table[energy][50][0];
                if (energy = 0) break; 
                decay = decay + 50 + scope_state.decay_energy_time_table[energy][50][1];
            }
            Cached_BeamBmp[p]=scope_state.decay_energy_time_table[energy][-decay][0];
        }
        BeamBmp_decay=Cached_BeamBmp; 
        Cached_BeamBmp_current_decay = decay_vector; 
        Cached_BeamBmp_intensity     = intensity; 
    }
    // calc le=effective size of BmpBeam
    le=sz-1;
    while ((le > 0) && (0==BeamBmp_decay[le + BEAM_SPOT_SIZE/2+ BEAM_SPOT_SIZE*(BEAM_SPOT_SIZE/2)])) le--; 
    le += 2; 
    if (le > sz) le=sz; 

    // calc coord of starting spot
    x1d = (double)(sz+1)+(double)((H_SCO_SIZE-(sz+1)*2) * x1_HP) / scope_state.x_HP_Resolution; 
    y1d = (double)(sz+1)+(double)((V_SCO_SIZE-(sz+1)*2) * y1_HP) / scope_state.y_HP_Resolution; 
    // calc coord of ending spot
    x2d = (double)(sz+1)+(double)((H_SCO_SIZE-(sz+1)*2) * x2_HP) / scope_state.x_HP_Resolution; 
    y2d = (double)(sz+1)+(double)((V_SCO_SIZE-(sz+1)*2) * y2_HP) / scope_state.y_HP_Resolution; 
    // distance
    ddx=x2d-x1d; 
    ddy=y2d-y1d; 

    // coordinates in sco_energy array. (0,0) is in top left corner. 
    // separate integer and factional part
    x1_SCO = (int)x1d; x1d-=x1_SCO;     
    y1_SCO = (int)y1d; y1d-=y1_SCO;  
    x2_SCO = (int)x2d; x2d-=x2_SCO;  
    y2_SCO = (int)y2d; y2d-=y2_SCO;  

    // determine if will iterate on x axis or in y axis
    dx=(int)ddx; xlen= (dx < 0) ? -dx:dx;  
    dy=(int)ddy; ylen= (dy < 0) ? -dy:dy;  

    if (xlen >= ylen) {
        // iterate on x
        // 1) if necessary, swap start-end vector points to guarantee 
        //    we draw from left to right (ascending x values)
        if (dx<0) {
            SWAPI(x1_SCO, x2_SCO); SWAPD(x1d, x2d); 
            SWAPI(y1_SCO, y2_SCO); SWAPD(y1d, y2d); 
            dx=-dx; dy=-dy; ddx=-ddx; ddy=-ddy; 
        }
        // 2) draw left half of starting vector spot
        xd256 = (int)(x1d*256); 
        yd256 = (int)(y1d*256); 
        for (x=-le; x<=0;x++) {
            for (y=-le; y<le;y++) {
                draw_beam_pixel_in_sco_energy(BeamBmp_decay, x1_SCO, y1_SCO, xd256, yd256, x,y,1); 
            }
        }
        // 3) draw vector iterating on x
        if (dx<1) {
            // vector length zero
        } else {
            // calc vertical increment per pixel
            d = ddy / ddx; 
            // calc y1d on first integer coord in x axis
            y1d += (1-x1d) * d;
            if (y1d >=1) { y1d=y1d-1; y1_SCO++; } else
            if (y1d < 0) { y1d=y1d+1; y1_SCO--; } 
            yd256=(int)(y1d * 256);
            // iterate on x axis
            while (1) {
                x1_SCO++; 
                for (y=-le; y<le;y++) {
                    draw_beam_pixel_in_sco_energy(BeamBmp_decay, x1_SCO, y1_SCO, 0, yd256, 0,y,0); 
                }
                dx--; if (dx<0) break; 
                y1d += d; 
                if (y1d >=1) { y1d=y1d-1; y1_SCO++; } else
                if (y1d < 0) { y1d=y1d+1; y1_SCO--; } 
                yd256=(int)(y1d * 256);
            }
        }
        // 4) draw right half of ending vector spot
        xd256 = (int)(x2d*256); 
        yd256 = (int)(y2d*256); 
        for (x=0; x<le;x++) {
            for (y=-le; y<le;y++) {
                draw_beam_pixel_in_sco_energy(BeamBmp_decay, x2_SCO, y2_SCO, xd256, yd256, x,y,1); 
            }
        }
    } else {
        // iterate on y
        // 1) if necessary, swap start-end vector points to guarantee 
        //    we draw from top to bottom (ascending y values)
        if (dy<0) { 
            SWAPI(x1_SCO, x2_SCO); SWAPD(x1d, x2d); 
            SWAPI(y1_SCO, y2_SCO); SWAPD(y1d, y2d); 
            dx=-dx; dy=-dy; ddx=-ddx; ddy=-ddy; 
        }
        // 2) draw upper half of starting vector spot
        xd256 = (int)(x1d*256); 
        yd256 = (int)(y1d*256); 
        for (x=-le; x<le;x++) {
            for (y=-le; y<=0;y++) {
                draw_beam_pixel_in_sco_energy(BeamBmp_decay, x1_SCO, y1_SCO, xd256, yd256, x,y,1); 
            }
        }
        // 3) draw vector iterating on y
        if (dy<1) {
            // vector length zero
        } else {
            // calc horizontal increment per pixel
            d = ddx / ddy; 
            // calc x1d on first integer coord in y axis
            x1d += (1-y1d) * d;
            if (x1d >=1) { x1d=x1d-1; x1_SCO++; } else
            if (x1d < 0) { x1d=x1d+1; x1_SCO--; } 
            xd256=(int)(x1d * 256);
            // iterate on y axis
            while (1) {
                y1_SCO++; 
                for (x=-le; x<le;x++) {
                   draw_beam_pixel_in_sco_energy(BeamBmp_decay, x1_SCO, y1_SCO, xd256, 0, x,0,0); 
                }
                dy--; if (dy<0) break; 
                x1d += d; 
                if (x1d >=1) { x1d=x1d-1; x1_SCO++; } else
                if (x1d < 0) { x1d=x1d+1; x1_SCO--; } 
                xd256=(int)(x1d * 256);
            }
        }
        // 4) draw lower half of ending vector spot
        xd256 = (int)(x2d*256); 
        yd256 = (int)(y2d*256); 
        for (x=-le; x<le;x++) {
            for (y=0; y<le;y++) {
                draw_beam_pixel_in_sco_energy(BeamBmp_decay, x2_SCO, y2_SCO, xd256, yd256, x,y,1); 
            }
        }
    }
}



#if defined(CPANEL)

void scope_Refresh_ShowInfo(int bClearInfo)
{
    static uint32 white_col = 0; 
    char buf[10*100];
    extern void DrawText_surface(uint32 * surface, int pitch, // surface addr to start writing to, surface width (the length of a row of pixels in bytes)
                      int w0, int h0,                         // box where text will be set (clipped to it). 0,0 if no box wanted
                      uint32 col,                             // color of text
                      char * buf, int chrsz);                 // string to print, char size: 1-> x1, 2-> x2

    buf[0]=26; buf[1]=0; // set to clear the info area
    if (bClearInfo) {
        // no data, just clear
    } else if (scope_state.type == SCO_TYPE_IS_VECTOR) {
        extern int hp1350_Draw_usec; // Draw_usec = time in microseconds to draw the full 2048/8192 mem locations of HP1350A/HP1351A
        extern int hp1350_nLines;    // nLines, nChars = number of lines and vectors draw each frame
        extern int hp1350_nChars;    // 
        extern int hp1350_nVectors;  // nVectors = nLines + lines needed to draw nChars = total numer of vectors effectively draw on scope

        sprintf(&buf[1], "Intensity: %d, Focus: %d, Persistence: %d msec, Pixels to decay: %d \n"
                 "HP135%cA: %d lines, %d chars (total %d vectors), %d msec to draw \n"
                 "SimH: %d msec to decay pixels, %d to draw vectors",                 
                 scope_state.Intensity, scope_state.Focus, scope_state.Persistence_msec, scope_state.info.nDecayingPixels, 
                 '0' + HP1351_flag,
                 hp1350_nLines, hp1350_nChars, hp1350_nVectors, hp1350_Draw_usec / 1000,
                 (scope_state.info.msec_into_decay * (1000/FPS))/DECAY_STEP_MSEC, 
                 (scope_state.info.msec_into_draw * (1000/FPS))/DECAY_STEP_MSEC); 
    } else {
        sprintf(&buf[1], "Intensity: %d, Focus: %d, Persistence: %d msec, Pixels to decay: %d \n"
                 "SimH: %d msec to decay pixels",
                 scope_state.Intensity, scope_state.Focus, scope_state.Persistence_msec, scope_state.info.nDecayingPixels, 
                 (scope_state.info.msec_into_decay * (1000/FPS))/DECAY_STEP_MSEC); 
    }
    memset(&scope_state.info, 0, sizeof(scope_state.info)); // init the info counters so they can grab new values
    if (white_col==0) white_col=scope_rgb_color(255,255,255); 
    DrawText_surface(&scope_state.surface[H_MARGIN + 10 * H_SIZE], H_SIZE, // surface addr to start writing to, surface width (the length of a row of pixels in bytes)
                     H_SHOWINFO_COLS * 8, H_SHOWINFO_LINES * 10, 
                     white_col,  // text color
                     buf, 1);
}


rectlist scope_RectList; // rectangle list structure

static void SetRectList_using_RefreshGrid(void)
{
    typedef struct {           // grid to refresh, V cells vertical and H cells horizontal
        int V,H; 
    } Grid; 
    static Grid grid[H_GRID_SIZE][V_GRID_SIZE];
    int x,y,xx,yy,n,v,ww,hh; 

    // populate RectList using sco_RefreshGrid info
    // let's now identify vertical rectagles in sco_RefreshGrid. 
    memset(grid, 0, sizeof(grid)); 
    // scan vertical columns in sco_RefreshGrid. 
    // Set grid[x,y].V cell value= number of contiguous vertival cells to refresh 
    for (x=0; x<H_GRID_SIZE; x++) for (y=0; y<V_GRID_SIZE; y++) {   // for each vertical column 
        if (scope_state.RefreshGrid[x][y]==0) continue; 
        n=0; // cell has pixels, calc n=height of sco_grid column
        for (yy=y+1;;yy++) { 
            if (yy>=V_GRID_SIZE) break; 
            if (scope_state.RefreshGrid[x][yy]==0) break; 
            n++; 
        } 
        grid[x][y].V=1+n; 
        y+=n; 
    }
    memset(scope_state.RefreshGrid, 0, sizeof(scope_state.RefreshGrid)); 
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
    scope_RectList.Count=0; 
    for (x=0; x<H_GRID_SIZE; x++) for (y=0; y<V_GRID_SIZE; y++) {   
        if (grid[x][y].H==0) continue; 
        // add entry to rectlist
        n=scope_RectList.Count; 
        if (n>=RECTLIST_MAX) {
            // set rect table as full
            scope_RectList.Count=-1; 
            return; 
        }
        // room left on rectable table. can add entry
        // calculate surface coordinates
        xx=(x<<4); yy=(y<<4); 
        ww=((grid[x][y].H)<<4); hh=((grid[x][y].V)<<4); 
        if (xx >= H_SCO_SIZE) continue; // safety
        if (yy >= V_SCO_SIZE) continue; // safety
        if (xx+ww > H_SCO_SIZE) ww = H_SCO_SIZE-xx; 
        if (yy+hh > V_SCO_SIZE) hh = V_SCO_SIZE-yy; 
        xx+=H_MARGIN; yy+=V_MARGIN; 
        // set up rectlist entry
        scope_RectList.flags[n]=0;
        scope_RectList.x[n]=xx; scope_RectList.w[n]=ww; 
        scope_RectList.y[n]=yy; scope_RectList.h[n]=hh; 
        scope_RectList.Count++;
    }
    if (scope_state.refresh_needed & 2) {
        // should refresh control knobs. Add them to rectancgle list
        n=scope_RectList.Count; 
        if (n>=RECTLIST_MAX) {
            // set rect table as full
            scope_RectList.Count=-1; 
            return; 
        }
        // room left on rectable table. can add entry
        scope_RectList.flags[n]=0;
        scope_RectList.x[n]=H_SIZE-H_KNOB; scope_RectList.w[n]=H_KNOB; 
        scope_RectList.y[n]=V_MARGIN; scope_RectList.h[n]=V_SCO_SIZE/2; 
        scope_RectList.Count++;
    }
    if (scope_state.refresh_needed & 4) {
        // should refresh the ShowInfo area. Add them to rectancgle list
        n=scope_RectList.Count; 
        if (n>=RECTLIST_MAX) {
            // set rect table as full
            scope_RectList.Count=-1; 
            return; 
        }
        // room left on rectable table. can add entry
        scope_RectList.flags[n]=0;
        scope_RectList.x[n]=H_MARGIN; scope_RectList.w[n]=H_SHOWINFO_COLS * 8; 
        scope_RectList.y[n]=10;       scope_RectList.h[n]=H_SHOWINFO_LINES * 10; 
        scope_RectList.Count++;
    }
}
#endif

void scope_refresh(uint32 tnow)
{
    int x,y,p, ps, energy, e0, e1, decay; 
    int FullRedrawDone=0; 
    int bulk_erase_energy=0; 

	if (scope_state.power == 0) return;
	// scope tube image to GUI window

    // show info
    #if defined(CPANEL)
    scope_state.Refresh_Frames_Count++;
    if (  (scope_state.bShowInfo) && (                      // if ShowInfo active AND ...
          (scope_state.refresh_needed & 2)                  // (knob modified OR ...
          || (scope_state.FullRedrawNeeded)                 //  ... FullRedrawNeeded
          || ((scope_state.Refresh_Frames_Count % 8)==0))   //  ... frame count match 8 FPS)
       ) {
        // show info for ^I (Ctrl I) at 60 FPS / 8 -> 8 FPS
        scope_Refresh_ShowInfo(0);
        // set refresh_needed so SetRectList_using_RefreshGrid will also add to rectlist the InfoPanel to be redraw
        scope_state.refresh_needed |= 4;                    
    } 
    #endif

    if (scope_state.Erase_tnow) {
        uint32 msec; 
        // bulk erase in progress
        msec = tnow - scope_state.Erase_tnow; 
        if (msec > BULK_ERASE_MSEC) {
            // bulk erase finished
            scope_state.Erase_tnow=0; 
        } else {
            // calc current energy level left in bulk erase flash
            bulk_erase_energy=240 * (BULK_ERASE_MSEC-msec) / BULK_ERASE_MSEC; 
        }
        // ask for full redraw to rebuild the scope image adding bulk erase flash effect
        scope_state.FullRedrawNeeded=1; // because bulk erase in progress
    }

	if 	(scope_state.FullRedrawNeeded) {
		scope_state.FullRedrawNeeded = 0;
        scope_state.refresh_needed = 1; 
        // draw control knobs
        set_control_knobs(0); 
        // set the objects to be used later when drawing into surface 
    	// fill in the color palette for scope beam
        set_beam_color_palette_rgb(); 
        // fill in decay table
        set_decay_table();
        // set beam sport according to intensity and focus
	    set_beam_bmp(); 
        // clear grids
        memset(scope_state.DecayGrid, 0, sizeof(scope_state.DecayGrid)); 
        memset(scope_state.RefreshGrid, 0, sizeof(scope_state.RefreshGrid)); 
		// generate surface accordind to energy value of each pixel in sco_energy and sco_energy_nodecay array 
        // also rebuild DecayGrid
        for (y=0; y < V_SCO_SIZE;y++) {
            p = H_SCO_SIZE * y; 
            ps= H_MARGIN + H_SIZE * (V_MARGIN + y) ; 
            for (x=0; x < H_SCO_SIZE;x++) {
                decay=scope_state.sco_decay_msec[p];    // decay remaining 
                e1=scope_state.sco_energy[p];           // energy of pixels that decay
                e0=scope_state.sco_energy_nodecay[p];   // energy of pixels that DO NOT decay
                energy = (e0 > e1) ? e0:e1;             // get the hightest energy
                energy = (energy > bulk_erase_energy) ? energy:bulk_erase_energy; 
                scope_state.surface[ps]=scope_state.beam_color_palette[energy]; 
                if ((e1>e0) && (decay >= 0)) scope_state.DecayGrid[x>>4][y>>4]=1; // this point should be decayed 
                p++; ps++; 
            }
        }
        FullRedrawDone=1; 
    }
	//update GUI window
#if defined(CPANEL)
    if (FullRedrawDone) {
        scope_RectList.Count=-1; // update full surface to GUI window
    } else {
        SetRectList_using_RefreshGrid(); // update only rectlist surface 
    }
    if (scope_RectList.Count!=0) {
        vid_refresh_ex(scope_state.vptr, scope_state.surface, &scope_RectList); 
    }
#else
    // will send the whole surface allways
    vid_draw_window (scope_state.vptr, 0, 0, scope_state.wx, scope_state.wy, scope_state.surface);
    vid_refresh_window (scope_state.vptr);
#endif
    scope_state.refresh_needed=0; // clear flag because refresh just been done
    if (tnow==0) tnow = sim_os_msec(); 
    scope_state.refresh_tnow = tnow; 

	sim_debug (DBG_DETAIL, &scope_dev, "Refresh done.\n");
}

void scope_refresh_if_needed(uint32 tnow)
{
    uint32 msec; 
    static int msec_into_decay_image; 

    // handle scope image decay
    if (tnow==0) tnow = sim_os_msec(); 
    msec = tnow - scope_state.decay_tnow; 
    if (msec < DECAY_STEP_MSEC) {
        // do not decay yet
    } else {
        // decay image in sco_energy array
        decay_image_in_scope(DECAY_STEP_MSEC); 
        scope_state.decay_tnow=tnow;
        // measure time spent into decay 
        msec = sim_os_msec() - tnow; if (msec > 1000) msec=1000;
        if (scope_state.info.msec_into_decay < (int) msec) scope_state.info.msec_into_decay = msec; 
        if (scope_state.type == SCO_TYPE_IS_VECTOR) {
            // simulate vector generation hw inside hp1350 
            extern void HP1350_draw_vectors(int drawing_msec); 
            uint32 tnow2 = sim_os_msec(); 
            HP1350_draw_vectors(DECAY_STEP_MSEC); 
            // measure time spent into draw routine
            msec = sim_os_msec() - tnow2; if (msec > 1000) msec=1000;
            if (scope_state.info.msec_into_draw < (int) msec) scope_state.info.msec_into_draw = msec; 
            // adjust decay starting time to exclude draw vector time
            if (msec < 1000) scope_state.decay_tnow += msec; 
        }
    }

    // handle DAC 20msec timer 
    if (scope_state.DAC_timer_tnow) {
        msec = tnow - scope_state.DAC_timer_tnow;
        if (msec < 20) {
            // DAC timer not expired
        } else {
            scope_dac_timer_expired(); 
            scope_state.DAC_timer_tnow=0; // stop the timer
        }
    }

    if (scope_state.FullRedrawNeeded) scope_state.refresh_needed=1; 
    if (scope_state.Erase_tnow) scope_state.refresh_needed=1; 
    // check if 20 msec has elapsed from last refresh (20 msec -> 50 FPS)
    if (tnow==0) tnow = sim_os_msec(); 
    msec = tnow - scope_state.refresh_tnow; 

    if (msec > (1000/FPS)) {                                      // enought time has pased from previous refresh
        if (scope_state.refresh_needed) {                         // and there is something to draw on gui
            #if defined(CPANEL)
            if (vid_refresh_in_progress(scope_state.vptr) == 0) { // and no previous refresh in progress
                scope_refresh(tnow);                              // go ahead and redraw surface
            }
            #else
            scope_refresh(tnow);
            #endif
        }
#if defined(CPANEL)
        // check key pressed
        scope_check_HotKeys(); 

#endif
    }   
}

/* Simulate DAC card & scope circuitery drawing on tube with electronic beam */

static t_stat scope_refresh_svc (UNIT *uptr)
{
    if (scope_state.quit_requested) return SCPE_EXIT; 
    sim_activate(&scope_refresh_unit, REFRESH_INTERVAL);

    scope_refresh_if_needed(0); 
    return SCPE_OK;
}

/* Unit service */

t_stat scope_svc (UNIT *uptr) 
{
    if (scope_state.quit_requested) return SCPE_EXIT; 
    scopeio (&scope_dib, ioENF, 0);							/* set flag to be processed on next instr */
    return SCPE_OK;
}

/* I/O signal handler */

// scope connected to DAC interface card
uint32 scope_dac (DIB *dibptr, IOCYCLE signal_set, uint32 stat_data)
{
IOSIGNAL signal;
IOCYCLE  working_set = IOADDSIR (signal_set);           /* add ioSIR if needed */

int x,y,d;

while (working_set) {
	signal = IONEXT (working_set);                      /* isolate next signal */

    switch (signal) {                                   /* dispatch I/O signal */

        case ioCLF:                                     /* clear flag flip-flop */
            scope_CheckAutoPowerOn(); 
            scope.flag = scope.flagbuf = CLEAR;
			sim_debug (DBG_FLAGS, &scope_dev, "ioCLF\n");
			if (scope_state.type==SCO_TYPE_IS_STORAGE) scope_bulk_erase(1);			/* it's and storage tube -> erases the screen */
			if (scope_state.DAC_timer_tnow==0) { /* CLF starts the DAC timer if it is off */
   				sim_debug (DBG_FLAGS, &scope_dev, "DAC Timer Started \n");
			    scope_state.DAC_timer_tnow=sim_os_msec();
			}
            break;

        case ioSTF:                                     /* set flag flip-flop */
		case ioENF:                                     /* enable flag */
            scope.flag = scope.flagbuf = SET;
			sim_debug (DBG_FLAGS, &scope_dev, "ioENF\n");
            break;

        case ioSFC:                                     /* skip if flag is clear */
            setstdSKF (scope);
            break;

        case ioSFS:                                     /* skip if flag is set */
            setstdSKF (scope);
            break;

        case ioIOI:                                     /* I/O data input */
            stat_data = IORETURN (SCPE_OK, 0);          /* merge in return status */  
            break;

        case ioIOO:										/* I/O data output to DAC interface to Scope Display */
            scope_CheckAutoPowerOn(); 
			scope_unit.buf = d = IODATA (stat_data) & (0177777);	
			x = d & 0xFF;
			y = (d >> 8) & 0xFF;
            if ((scope_state.x_HP_Resolution != 256) || (scope_state.y_HP_Resolution != 256) ||
                (scope_state.type==SCO_TYPE_IS_VECTOR) ) {
                // set the scope model to HP1300A
                scope_set_model(&scope_unit, 0, NULL, NULL); 
            }
			draw_spot_in_scope(x, y);
			if (scope.control == CLEAR) {	 				
				/* control clear -> DMA mode -> ENF raised after each transfer */
				sim_activate(&scope_unit, 1); 
			}
            break;

        case ioPOPIO:                                   /* power-on preset to I/O */
            scope.flag = scope.flagbuf = SET;           /* set flag and flag buffer */
            scope_unit.buf = 0;                         /* clear output buffer */
            break;

        case ioCRS:                                     /* control reset */
        case ioCLC:                                     /* clear control flip-flop */
            scope.control = CLEAR;						/* set DMA mode */
			sim_debug (DBG_FLAGS, &scope_dev, "ioCLC\n");
			sim_activate(&scope_unit, 1);  		        /* set flag on next instr */
            break;

        case ioSTC:                                     /* set control flip-flop */
            scope.control = SET;				        /* set programmed IO or Interrupt IO */
			sim_debug (DBG_FLAGS, &scope_dev, "ioSTC\n");
            break;

        case ioSIR:                                     /* set interrupt request */
            setstdPRL (scope);                          /* set standard PRL signal */
            setstdIRQ (scope);                          /* set standard IRQ signal */
            setstdSRQ (scope);                          /* set standard SRQ signal */
            break;

        case ioIAK:                                     /* interrupt acknowledge */
            scope.flagbuf = CLEAR;
            break;

        default:                                        /* all other signals */
            break;                                      /*   are ignored */
        }

    working_set = working_set & ~signal;                /* remove current signal from set */
    }

return stat_data;
}

uint32 scope_rs232c_GraphicsTranslator (DIB *dibptr, IOCYCLE signal_set, uint32 stat_data)
{
uint16 data;
IOSIGNAL signal;
IOCYCLE  working_set = IOADDSIR (signal_set);           /* add ioSIR if needed */

while (working_set) {
    signal = IONEXT (working_set);                      /* isolate next signal */

    switch (signal) {                                   /* dispatch I/O signal */

        case ioCLF:                                     /* clear flag flip-flop */
            rs232c.flag = rs232c.flagbuf = CLEAR;
            break;

        case ioSTF:                                     /* set flag flip-flop */
        case ioENF:                                     /* enable flag */
            rs232c.flag = rs232c.flagbuf = SET;
            break;

        case ioSFC:                                     /* skip if flag is clear */
            setstdSKF (rs232c);
            break;

        case ioSFS:                                     /* skip if flag is set */
            setstdSKF (rs232c);
            break;

        case ioIOI:                                     /* I/O data input */
            // HP1350 does not send data to CPU
            data = (uint16) 0;
            stat_data = IORETURN (SCPE_OK, data);       /* merge in return status */
            break;

        case ioIOO:                                     /* I/O data output */
            scope_CheckAutoPowerOn(); 
            data = IODATA (stat_data);                  /* clear supplied status */
            scope_unit.buf = data & 0377;
            break;

        case ioCRS:                                     /* control reset */
            rs232c.control = CLEAR;                     /* clear control */
            rs232c.flag = rs232c.flagbuf = SET;         /* set flag and flag buffer */
            break;

        case ioCLC:                                     /* clear control flip-flop */
            rs232c.control = CLEAR;
            break;

        case ioSTC:                                     /* set control flip-flop */
            scope_CheckAutoPowerOn(); 
            rs232c.control = SET;
            {   // send received char from cpu to HP1350 for decoding and processing
                extern void HP1350_process_char_from_cpu(int); 
                HP1350_process_char_from_cpu(scope_unit.buf);
            }
            sim_activate (&scope_unit, 200);
            break;

        case ioSIR:                                     /* set interrupt request */
            setstdPRL (rs232c);                         /* set standard PRL signal */
            setstdIRQ (rs232c);                         /* set standard IRQ signal */
            setstdSRQ (rs232c);                         /* set standard SRQ signal */
            break;

        case ioIAK:                                     /* interrupt acknowledge */
            rs232c.flagbuf = CLEAR;
            break;

        default:                                        /* all other signals */
            break;                                      /*   are ignored */
        }

    working_set = working_set & ~signal;                /* remove current signal from set */
    }

return stat_data;
}

uint32 scopeio (DIB *dibptr, IOCYCLE signal_set, uint32 stat_data)
{
    if (scope_state.type == SCO_TYPE_IS_VECTOR) {
        // CPU connected to rs232c interface card that send the data to HP1350 Graphics Translator
        // that in turns drives the display scope
        return scope_rs232c_GraphicsTranslator (dibptr, signal_set, stat_data); 
    } else {
        // CPU connected to DAC interface that directly drives the scope
        return scope_dac (dibptr, signal_set, stat_data); 
    }
}


#if defined(CPANEL)
// Hotkey processing
void scope_check_HotKeys(void)
{   
    int c = 0; 
    int x,y,n,sz,xx,yy;
    int incr=1; 
    
    // c is ascii code of key last pressed (if bit30=1 bits 29..0 contains scancode)
    // note: cannot map global cpanel hotkeys: ^T, '+', '-', ^E, ^+, ^-
    if (vid_keyb.vptr==scope_state.vptr) c = vid_keyb.LastKeyPress; // key currently being pressed
    
    // no key pressed and left mouse button is down on scope GUI window
    // note: mouse only available in scope window IF cpanel is on (cpanel refresh polls
    // mouse events and updates cpinput struct)
    if ((cpinput.mouse.vptr==scope_state.vptr) && (cpinput.mouse.b1)) {
        // check if clicking a knob
        c=0; 
        x=cpinput.mouse.X; y=cpinput.mouse.Y; 
        for (n=1;n<=4;n++) {
            sz=Knob[n].sz; // side size of knob
            xx=Knob[n].x; yy=Knob[n].y; // top left corner of knob
            if ((y > yy) && (y < yy + sz)) {
                if ((x > xx) && (x < xx + sz/2)) { 
                    // click on left side of knob -> decrease it (turn it to left)
                    c = Knob[n].DecKey; // get the key that does the action
                } else if ((x > xx+sz/2) && (x < xx + sz)) { 
                    // click on right side of knob -> increase it (turn it to right)
                    c = Knob[n].IncKey; // get the key that does the action
                }
            }
            if (c) { // knob clicked!
                cpinput.mouse.b1=0; // mark click as processed
                incr=5; // mouse click turns the knob faster that keyboard
                break; 
            }
        }
    }

    if (c==0) {
        // no key pressed on keyboard not knob clicked
        return; 
    }
    
    switch (c) {
        case 'i'-'a'+1:  // Control-I (^I) was pressed: ShowInfo 
            vid_keyb.LastKeyPress = 0; // clear key as it is processed           
            if (scope_state.bShowInfo==1) {
                scope_state.bShowInfo = 0; // toggle value
                // put again background thus removing info from gui
                scope_Refresh_ShowInfo(1); // erase 
                scope_state.refresh_needed |= 4;                    
            } else {
                scope_state.bShowInfo = 1; // instruct refresh to show the info
                memset(&scope_state.info, 0, sizeof(scope_state.info)); // clear the data
            }
            break; 
        case 'f': // F key pressed -> increase focus value    
            scope_state.Focus+=incr; 
            if (scope_state.Focus > 63) scope_state.Focus=63; 
     	    set_beam_bmp(); // because beam spot has changed (focus increase)
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(2);
            break; 
        case 'F': // Shift-F key pressed -> decrease
            scope_state.Focus-=incr; 
            if (scope_state.Focus < 0) scope_state.Focus=0; 
      	    set_beam_bmp(); // because beam spot has changed (focus decrease)
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(2);
            break; 
        case 'i': // I key pressed -> increase intensity value    
            scope_state.Intensity+=incr; 
            if (scope_state.Intensity > 255) scope_state.Intensity=255; 
      	    set_beam_bmp(); // because beam spot has changed (intensity increase)
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(1); // update intensity knob
            break; 
        case 'I': // Shift-I key pressed -> decrease
            scope_state.Intensity-=incr; 
            if (scope_state.Intensity < 10) scope_state.Intensity=10; 
      	    set_beam_bmp(); // because beam spot has changed (intensity decrease)
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(1); // update intensity knob
            break; 
        case 'p': // P key pressed -> increase Persistence value    
            scope_state.Persistence_msec =  1 + (scope_state.Persistence_msec * (100 + 10*incr))/100; // increment 10%+1
            if (scope_state.Persistence_msec > 100*1000) scope_state.Persistence_msec=100*1000; 
            set_decay_table();
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(3);
            break; 
        case 'P': // Shift-P key pressed -> decrease
            scope_state.Persistence_msec =  scope_state.Persistence_msec * 100 / (100 + 10*incr) - 1; 
            if (scope_state.Persistence_msec < 10) scope_state.Persistence_msec=10; 
            set_decay_table();
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            set_control_knobs(3);  
            break; 
        case 'e': // E key -> erase storage scope screen 
            scope_bulk_erase(1);
            vid_keyb.LastKeyPress=0; // Clear it as being processed
            break; 
    }
}
#endif

// SCP functions

t_stat scope_reset (DEVICE *dptr)
{
    sim_debug (DBG_CMDS, &scope_dev, "Scope Reset\n");
    IOPRESET (&scope_dib);                  /* PRESET device (does not use PON) */
    sim_cancel (&scope_unit);               /* deactivate unit */
    scope_state.DAC_timer_tnow=0;           // deactivate DAC 20msec timer
    // boot & run SCP commands issues an implicit reset on SCP. In this reset first cancels
    // any schedulled device event and then calls each device reset routine (this routine)
    // se here we must re-schedulle the refresh event
    if (scope_state.power) {
        sim_cancel(&scope_refresh_unit); 
        sim_activate(&scope_refresh_unit, REFRESH_INTERVAL);
    }
    return SCPE_OK;
}

t_stat scope_set_power_on (UNIT *uptr, int32 value, CONST char *cptr, void *desc)  { 
    return power_scope(value ? 1:0); 
}

t_stat scope_set_model(UNIT *uptr, int32 value, CONST char *cptr, void *desc) 
{ 
	if (value == 0) {
		// HP-1300A X-Y Display non storage scope, green phosphor
		scope_state.type = SCO_TYPE_IS_NOSTORAGE;                        // non storage scope + DAC interface card
        scope_state.Focus = 2;  
        scope_state.Intensity = 160; 
        scope_state.Persistence_msec = 100;                              // persistence 500 msec
        scope_state.x_HP_Resolution = 256; 
        scope_state.y_HP_Resolution = 256; 
        scope_state.BeamColor_R=131;    // bean light green
        scope_state.BeamColor_G=249;
        scope_state.BeamColor_B=143;
        scope_state.BkgColor_R = 52;    // background dark green/grey
        scope_state.BkgColor_G = 62;
        scope_state.BkgColor_B = 36;
	} else if (value == 1) {
		// HP-1331A storage scope, ambar phosphor
		scope_state.type = SCO_TYPE_IS_STORAGE;                         // storage scope + DAC interface card
        scope_state.Focus = 2;  
        scope_state.Intensity = 128; 
        scope_state.Persistence_msec = 600;                             // persistence for non pullup spots: 600 sec
        scope_state.x_HP_Resolution = 256; 
        scope_state.y_HP_Resolution = 256; 
        scope_state.BeamColor_R=255;    // bean light ambar
        scope_state.BeamColor_G=192;
        scope_state.BeamColor_B=128;
        scope_state.BkgColor_R = 24;    // background dark orange
        scope_state.BkgColor_G = 24;
        scope_state.BkgColor_B =  0;
    } else if (value == 2) {
		// HP-1332A X-Y Display non storage scope, blue phosphor
		scope_state.type = SCO_TYPE_IS_NOSTORAGE;                   // non storage scope + DAC interface card
        scope_state.Focus = 2;  
        scope_state.Intensity = 150; 
        scope_state.Persistence_msec = 500;                         // persistence 500 msec
        scope_state.x_HP_Resolution = 256; 
        scope_state.y_HP_Resolution = 256; 
        scope_state.BeamColor_R= 48;    // bean light blue
        scope_state.BeamColor_G=240;
        scope_state.BeamColor_B=255;
        scope_state.BkgColor_R =  8;    // background dark blue
        scope_state.BkgColor_G = 64;
        scope_state.BkgColor_B = 92;
    } else if ((value == 3) ||(value == 4)) {
		// HP1350/1351 + HP-1304A Analog Display non storage scope, green phosphor
        HP1351_flag = ((value == 3) ? 0:1); 
		scope_state.type = SCO_TYPE_IS_VECTOR;                      // non storage scope + HP1350A Graphics Translator + RS232C interface card
        scope_state.Focus = 1;  
        scope_state.Intensity = 150; 
        scope_state.Persistence_msec = 100;                         // persistence 100 msec
        scope_state.x_HP_Resolution = 1021; 
        scope_state.y_HP_Resolution = 1021; 
        scope_state.BeamColor_R= 76;    // bean light gren
        scope_state.BeamColor_G=215;
        scope_state.BeamColor_B=149;
        scope_state.BkgColor_R = 10;    // background dark blue
        scope_state.BkgColor_G = 15;
        scope_state.BkgColor_B = 10;
    } else {
        return SCPE_ARG; 
	}
    // model change -> clear sco_array and surface
    scope_bulk_erase(0); 
    scope_state.FullRedrawNeeded = 1;   // because model has changed
	return SCPE_OK;
}

t_stat get_uintN (const char *cptr, int32 num[], int nums, t_value max)
{
	// read up to nums integers separated by / and store them in num[] array
	// if less than nums values are read, missing values are set to zero
	t_value val;
	char *tptr;
	int i;

	for (i=0;i<nums;i++) num[i]=0;
	for (i=0;i<nums;i++) {
		val = strtotv ((CONST char *)cptr, &tptr, 10);
		if ((cptr == tptr) || (val > max)) return SCPE_ARG;
		num[i]=val;
		if (*tptr == 0) break;
		if (*tptr != '/') return SCPE_ARG;
		cptr = ++tptr;
	}
	return SCPE_OK;
}

t_stat scope_set_type(UNIT *uptr, int32 value, CONST char *cptr, void *desc) 
{ 
	scope_state.type = value; 
    if (scope_state.type == SCO_TYPE_IS_VECTOR) {
        scope_state.x_HP_Resolution = 1021; 
        scope_state.y_HP_Resolution = 1021; 
    } else {
        scope_state.x_HP_Resolution = 256; 
        scope_state.y_HP_Resolution = 256; 
    }
    // type change -> clear sco_array and surface
    scope_bulk_erase(0); 
    scope_state.FullRedrawNeeded = 1;   // because switching scope type -> must redraw knobs, beambmp, etc
	return SCPE_OK; 
}

t_stat scope_set_param(UNIT *uptr, int32 value, CONST char *cptr, void *desc) { 

	DEVICE *dptr = (DEVICE *) desc;
	int32 num;
	t_stat r;

	if (cptr == NULL) return SCPE_ARG;
	if (desc == NULL) return SCPE_IERR;

	if (value == 2) { // SET FOCUS= 0..63
		/* 0=minimal spot, 63 wide beam spot size */
		num = (int32) get_uint (cptr, 10, 63, &r);
		if (r != SCPE_OK) return r;
      	scope_state.Focus = num;
   	    set_beam_bmp(); // because beam spot has changed (focus change)
        set_control_knobs(2);
        return SCPE_OK;
	} else if (value == 3) { // SET INTENSITY= 10..255
		num = (int32) get_uint (cptr, 10, 255, &r);
		if (r != SCPE_OK) return r;
        if (num < 10) num=10; 
      	scope_state.Intensity = num;
   	    set_beam_bmp(); // because beam spot has changed (intensity change)
        set_control_knobs(1);
        return SCPE_OK;
	} else if (value == 4) { // SET PERSISTENCE= n 
        // beam spot persistence time in msec
        // range 10 ... 100000 
		num = (int32) get_uint (cptr, 10, 100*1000, &r);
		if (r != SCPE_OK) return r;
        if (num < 10) num=10; 
      	scope_state.Persistence_msec = num;
        set_decay_table();  // calc decay table according to new persistence
        set_control_knobs(3); // set on screen knob accorting to new persistence
        return SCPE_OK; 
	} else if (value == 5) { // SET COLOR=n/n/n/n/n/n     n are 0..255
        // RGB color for beam, RGB color for background
  	    int32 col[6];
		r = get_uintN (cptr, col, 6, 255);
        if (r != SCPE_OK) return r;
        scope_state.BeamColor_R=col[0];
        scope_state.BeamColor_G=col[1];
        scope_state.BeamColor_B=col[2];
        scope_state.BkgColor_R =col[3];
        scope_state.BkgColor_G =col[4];
        scope_state.BkgColor_B =col[5];
   	    scope_state.FullRedrawNeeded = 1; // because beam and backgound colors have changed
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
        if (scope_state.power == 0) {
            scope_state.InitialPosX=xPos; 
            scope_state.InitialPosY=yPos; 
        } else {
            vid_SetWindowSizeAndPos(scope_state.vptr, SIM_SETWIN_POS ,xPos,yPos);
        }
        return SCPE_OK; // no panel opened
    } else if (value == 1) { // SET SCALE=10..200 
        int Scale; 
        num = (int32) get_uint (cptr, 10, 200, &r);
        if (r != SCPE_OK) return r;
        // resize window to set the new scale
        if (num < 10) {Scale= 10; } // max scale range allowed: 10%..200%
        else if (num > 200) {Scale=200; }
        else Scale=num; 
        if (scope_state.power == 0) {
            scope_state.InitialScale=Scale; 
        } else {
            vid_SetWindowSizeAndPos(scope_state.vptr, SIM_SETWIN_SCALE, Scale, 0); // resize GUI window
        }
#endif
    } else {
		// undefined SET 
		return SCPE_IERR;
	}
	return SCPE_OK;
}

t_stat scope_set_AutoPowerOn (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    scope_state.AutoPowerOn=value; 
	return SCPE_OK;
}

t_stat scope_show (FILE *st, UNIT *uptr, int32 num, CONST void *desc) {

	switch (scope_state.type) {
        case SCO_TYPE_IS_NOSTORAGE: fprintf (st, "non-storage scope + DAC interface card, "); break; 
        case SCO_TYPE_IS_STORAGE:   fprintf (st, "storage scope + DAC interface card, "); break; 
        case SCO_TYPE_IS_VECTOR:    fprintf (st, "non-storage scope + Graphics Translator + RS232C card, "); break; 
        default: fprintf (st, "unknown type, "); 
	} 
	fprintf (st, "Focus=%d, Intensity=%d, ", scope_state.Focus, scope_state.Intensity);
	if (scope_state.Persistence_msec < 1000) {
		fprintf (st, "Persistence=%d msec, ", scope_state.Persistence_msec);
	} else {
		fprintf (st, "Persistence=%d msec (%.3f sec), ", scope_state.Persistence_msec, scope_state.Persistence_msec / 1000.0);
	}
    fprintf (st, "Resolution x=%d, y=%d ", scope_state.x_HP_Resolution, scope_state.y_HP_Resolution);
	return SCPE_OK;
}
